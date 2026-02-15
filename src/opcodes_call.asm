; opcodes_call.asm - Opcode handler for CALL (Python 3.12)
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack top pointer
;   r14 = co_consts tuple data pointer (&tuple.ob_item[0])
;   r15 = co_names tuple data pointer (&tuple.ob_item[0])
;
; ecx = opcode argument on entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "opcodes.inc"
%include "frame.inc"
%include "builtins.inc"

section .text

extern eval_dispatch
extern obj_dealloc
extern obj_decref
extern obj_incref
extern fatal_error
extern raise_exception
extern func_new
extern exc_TypeError_type

;; ============================================================================
;; op_call - Call a callable object
;;
;; Python 3.12 CALL opcode (171).
;;
;; Stack layout before CALL (bottom to top):
;;   ... | NULL_or_self | callable | arg0 | arg1 | ... | argN-1 |
;;                                                            ^ r13 (TOS)
;;
;; ecx = nargs (number of positional arguments)
;;
;; Addresses:
;;   args[0]      = [r13 - nargs*8]
;;   callable     = [r13 - (nargs+1)*8]
;;   null_or_self = [r13 - (nargs+2)*8]
;;
;; Method calls (null_or_self != NULL):
;;   self is inserted as the first argument by overwriting the callable's
;;   value stack slot, and nargs is incremented by 1.
;;
;; After the call:
;;   1. DECREF each argument (including self copy for method calls)
;;   2. For method calls: DECREF original self_or_null, DECREF saved callable
;;      For function calls: DECREF callable, DECREF null_or_self (NULL)
;;   3. Pop all consumed items from value stack
;;   4. Push return value
;;   5. Skip 3 CACHE entries (6 bytes): add rbx, 6
;;
;; Followed by 3 CACHE entries (6 bytes) that must be skipped.
;; ============================================================================
DEF_FUNC op_call, 48
    ; Locals:
    ;   [rbp-8]  = nargs (possibly incremented for method calls)
    ;   [rbp-16] = callable ptr (saved before overwriting stack slot)
    ;   [rbp-24] = return value from tp_call
    ;   [rbp-32] = is_method (0 or 1)
    ;   [rbp-40] = original nargs (before increment)

    mov [rbp-8], rcx                ; save nargs
    mov qword [rbp-32], 0          ; is_method = 0

    ; CPython 3.12 stack layout (bottom to top):
    ;   ... | func_or_null | callable_or_self | arg0 | ... | argN-1
    ; func_or_null = PEEK(nargs+2) — deeper slot
    ; callable_or_self = PEEK(nargs+1) — shallower slot
    ;
    ; Method call (func_or_null != NULL): callable=func_or_null, self=callable_or_self
    ; Function call (func_or_null == NULL): callable=callable_or_self

    ; Read func_or_null from deeper slot
    lea rax, [rcx + 2]
    neg rax
    mov rdi, [r13 + rax*8]

    test rdi, rdi
    jz .func_call

    ; === Method call: callable is in the deeper slot ===
    mov [rbp-16], rdi               ; callable = func_or_null
    mov qword [rbp-32], 1          ; is_method = 1
    jmp .setup_call

.func_call:
    ; === Function call: callable is in the shallower slot ===
    lea rax, [rcx + 1]
    neg rax
    mov rdi, [r13 + rax*8]
    mov [rbp-16], rdi               ; callable = callable_or_self

.setup_call:
    ; Get tp_call from the callable's type
    mov rdi, [rbp-16]              ; callable
    test rdi, rdi
    jz .not_callable               ; NULL check
    js .not_callable               ; SmallInt check (bit 63 set)
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .not_callable               ; no type (shouldn't happen)
    mov rcx, rax                    ; save type for dunder check
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jnz .have_tp_call

    ; tp_call NULL — try __call__ on heaptype
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .not_callable
    extern dunder_lookup
    extern dunder_call
    mov rdi, rcx              ; type
    lea rsi, [rel dunder_call]
    call dunder_lookup
    test rax, rax
    jz .not_callable
    ; Found __call__ — use its tp_call to dispatch
    mov rcx, rax              ; __call__ func
    mov rax, [rcx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_callable
    ; Rewrite callable: we need to call __call__(self, *args)
    ; The self is the original callable, prepend it to args
    ; Actually, we can just call dunder_func's tp_call with [callable, args...]
    ; But that requires restructuring the args. For simplicity, call it as
    ; tp_call(dunder_func, args_including_self, nargs+1)
    ; where args_including_self starts at the callable's slot on the value stack
    ; The callable is at [r13 - (nargs+1)*8], and args start at [r13 - nargs*8]
    ; We already have self in the callable's slot... actually this is tricky.
    ; Let's use a simpler approach: save dunder_func in [rbp-16], store original
    ; callable as first arg by shifting the args pointer back by 1
    mov [rbp-16], rcx         ; replace callable with __call__ func
    ; args_ptr should now include the original callable as self
    ; The original callable is already on the value stack at the right position
    ; We just need to point args_ptr one slot earlier and increment nargs
    ; Actually, let's just use the value stack directly:
    ; Stack: ... | NULL_or_self | original_callable | arg0 | arg1 | ...
    ; We want: tp_call(__call_func__, &[original_callable, arg0, ...], nargs+1)
    ; The original_callable is at [r13 - (nargs+1)*8], which is exactly where
    ; args_ptr - 8 would be. So we can just decrement args_ptr and inc nargs.
    ; But wait, we need to read nargs first...
    mov rcx, [rbp-8]
    mov rdx, rcx
    inc rdx                    ; nargs + 1 (include self/callable)
    inc rcx
    neg rcx
    lea rsi, [r13 + rcx*8]   ; args_ptr includes original callable as first arg
    mov rdi, [rbp-16]         ; __call__ func
    call rax
    mov [rbp-24], rax
    jmp .cleanup

.have_tp_call:
    ; Set up args: tp_call(callable, args_ptr, nargs)
    mov rcx, [rbp-8]               ; original nargs
    mov rdx, rcx                    ; rdx = nargs for tp_call
    neg rcx
    lea rsi, [r13 + rcx*8]        ; rsi = &arg0

    cmp qword [rbp-32], 0
    je .call_now

    ; Method call: include self (shallower slot) as first arg
    sub rsi, 8
    inc rdx

.call_now:
    mov rdi, [rbp-16]              ; callable
    call rax
    mov [rbp-24], rax               ; save return value

.cleanup:
    ; === Unified cleanup ===
    ; Pop nargs args and DECREF each
    mov rcx, [rbp-8]
    test rcx, rcx
    jz .args_done
.decref_args:
    sub r13, 8
    mov rdi, [r13]
    call obj_decref
    dec qword [rbp-8]
    cmp qword [rbp-8], 0
    jnz .decref_args
.args_done:

    ; Pop shallower slot (self for method, callable for function) and DECREF
    sub r13, 8
    mov rdi, [r13]
    call obj_decref

    ; Pop deeper slot (callable for method, NULL for function) and DECREF
    sub r13, 8
    mov rdi, [r13]
    call obj_decref
    ; Push return value onto value stack
    mov rax, [rbp-24]
    VPUSH rax

    ; Skip 3 CACHE entries (6 bytes)
    add rbx, 6

    leave
    DISPATCH

.not_callable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not callable"
    call raise_exception
    ; does not return
END_FUNC op_call

;; ============================================================================
;; op_make_function - Create a function from code object on TOS
;;
;; Python 3.12 MAKE_FUNCTION (opcode 132).
;; arg = flags: 0 = plain, 1 = defaults, 2 = kwdefaults, 4 = annotations, 8 = closure
;;
;; Stack order (when flags set, bottom to top):
;;   defaults tuple (if flag 0x01)
;;   kwdefaults dict (if flag 0x02)
;;   annotations (if flag 0x04) - ignored
;;   closure tuple (if flag 0x08)
;;   code_obj (always on top)
;; ============================================================================
DEF_FUNC op_make_function, 48
    ; [rbp-8]=flags, [rbp-16]=code, [rbp-24]=closure, [rbp-32]=defaults, [rbp-40]=kwdefaults

    mov [rbp-8], ecx               ; save flags
    mov qword [rbp-24], 0          ; closure = NULL default
    mov qword [rbp-32], 0          ; defaults = NULL default
    mov qword [rbp-40], 0          ; kwdefaults = NULL default

    ; Pop code object from value stack (always TOS)
    VPOP rdi
    mov [rbp-16], rdi

    ; Pop in CPython 3.12 order (reverse of push): 0x08, 0x04, 0x02, 0x01

    ; closure (0x08) - pop and save
    test ecx, MAKE_FUNC_CLOSURE
    jz .mf_no_closure
    VPOP rax
    mov [rbp-24], rax              ; save closure tuple
.mf_no_closure:

    ; annotations (0x04) - pop and discard
    test ecx, MAKE_FUNC_ANNOTATIONS
    jz .mf_no_annotations
    VPOP rdi
    DECREF_REG rdi
    mov ecx, [rbp-8]              ; reload flags (DECREF clobbers ecx)
.mf_no_annotations:

    ; kwdefaults (0x02) - pop and save (transfer ownership to func)
    test ecx, MAKE_FUNC_KWDEFAULTS
    jz .mf_no_kwdefaults
    VPOP rdi
    mov [rbp-40], rdi
.mf_no_kwdefaults:

    ; defaults (0x01) - pop and save (transfer ownership to func)
    test ecx, MAKE_FUNC_DEFAULTS
    jz .mf_no_defaults
    VPOP rdi
    mov [rbp-32], rdi
.mf_no_defaults:

    ; Create function: func_new(code, globals)
    mov rdi, [rbp-16]
    mov rsi, [r12 + PyFrame.globals]
    call func_new
    ; rax = new function object

    ; Set closure if present
    mov rcx, [rbp-24]
    mov [rax + PyFuncObject.func_closure], rcx

    ; Set defaults if present (transfer ownership, no INCREF needed)
    mov rcx, [rbp-32]
    mov [rax + PyFuncObject.func_defaults], rcx

    ; Set kwdefaults if present (transfer ownership, no INCREF needed)
    mov rcx, [rbp-40]
    mov [rax + PyFuncObject.func_kwdefaults], rcx

    ; Save func obj, DECREF the code object
    push rax
    mov rdi, [rbp-16]
    DECREF_REG rdi
    pop rax

    ; Push function onto value stack
    VPUSH rax
    leave
    DISPATCH
END_FUNC op_make_function

;; ============================================================================
;; op_call_function_ex - Call with *args and optional **kwargs
;;
;; Python 3.12 CALL_FUNCTION_EX (opcode 142).
;; arg & 1: kwargs dict is present on TOS
;;
;; Stack layout (bottom to top):
;;   ... | func | NULL | args_tuple | [kwargs_dict]
;;
;; After: ... | result
;; ============================================================================
extern tuple_type
extern dict_type

DEF_FUNC op_call_function_ex
    push rbx                        ; save (clobbered by eval convention save)
    push r12
    sub rsp, 48                     ; [rbp-32]=func, [rbp-40]=args, [rbp-48]=kwargs, [rbp-56]=result, [rbp-64]=oparg

    mov [rbp-64], ecx               ; save oparg

    ; Pop kwargs if present
    mov qword [rbp-48], 0
    test ecx, 1
    jz .cfex_no_kwargs
    VPOP rax
    mov [rbp-48], rax               ; kwargs dict
.cfex_no_kwargs:

    ; Pop args tuple
    VPOP rax
    mov [rbp-40], rax

    ; Pop func
    VPOP rax
    mov [rbp-32], rax

    ; Pop NULL (unused)
    sub r13, 8

    ; Get tp_call from func's type
    mov rdi, [rbp-32]
    test rdi, rdi
    jz .cfex_not_callable
    js .cfex_not_callable
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .cfex_not_callable

    ; Call tp_call(func, args_ptr, nargs)
    ; Determine args_ptr based on type (tuple vs list)
    ; Tuple: inline items at obj+32, List: pointer to items at [obj+32]
    mov rsi, [rbp-40]                  ; args sequence
    mov rcx, [rsi + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    je .cfex_tuple_args
    ; List (or other): ob_item is a pointer
    mov rsi, [rsi + PyListObject.ob_item]  ; dereference pointer
    jmp .cfex_args_ready
.cfex_tuple_args:
    lea rsi, [rsi + PyTupleObject.ob_item]  ; inline items
.cfex_args_ready:
    mov rdx, [rbp-40]
    mov rdx, [rdx + PyVarObject.ob_size]   ; nargs (ob_size at +16 for both)
    mov rdi, [rbp-32]                  ; callable
    call rax
    mov [rbp-56], rax                  ; save result

    ; DECREF args tuple
    mov rdi, [rbp-40]
    call obj_decref

    ; DECREF kwargs if present
    mov rdi, [rbp-48]
    test rdi, rdi
    jz .cfex_no_kwargs_decref
    call obj_decref
.cfex_no_kwargs_decref:

    ; DECREF func
    mov rdi, [rbp-32]
    call obj_decref

    ; Push result
    mov rax, [rbp-56]
    VPUSH rax

    add rsp, 48
    pop r12
    pop rbx
    pop rbp
    DISPATCH

.cfex_not_callable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not callable"
    call raise_exception
END_FUNC op_call_function_ex

;; ============================================================================
;; op_before_with - Set up context manager
;;
;; Python 3.12 BEFORE_WITH (opcode 53).
;;
;; Stack: ... | mgr  ->  ... | __exit__ | result_of___enter__()
;;
;; 1. Look up __exit__ on mgr, push it
;; 2. Look up __enter__ on mgr, call it
;; 3. Push result of __enter__
;; 4. DECREF mgr
;; ============================================================================
extern dict_get
extern str_from_cstr
extern exc_AttributeError_type

DEF_FUNC op_before_with
    push rbx
    push r12
    sub rsp, 32                     ; [rbp-32]=mgr, [rbp-40]=exit_method, [rbp-48]=enter_result

    ; Pop mgr
    VPOP rax
    mov [rbp-32], rax
    mov rbx, rax                    ; rbx = mgr

    ; Look up __exit__ on mgr's type
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jz .bw_no_exit

    ; Get "__exit__" from type dict
    mov rdi, rax
    lea rsi, [rel bw_str_exit]
    call str_from_cstr
    mov r12, rax                    ; r12 = exit name str
    mov rdi, [rbx + PyObject.ob_type]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    mov rsi, r12
    call dict_get
    test rax, rax
    jz .bw_no_exit_decref_name

    ; Got __exit__ function - INCREF it
    mov [rbp-40], rax
    mov rdi, rax
    call obj_incref

    ; DECREF exit name string
    mov rdi, r12
    call obj_decref

    ; Push __exit__ onto value stack (with self bound below)
    ; For method-style: push self first, then __exit__
    mov rax, [rbp-32]              ; mgr (self)
    INCREF rax
    VPUSH rax                      ; push self for __exit__
    mov rax, [rbp-40]
    VPUSH rax                      ; push __exit__ func

    ; Now look up __enter__ on mgr's type
    mov rdi, [rbx + PyObject.ob_type]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .bw_no_enter

    lea rdi, [rel bw_str_enter]
    call str_from_cstr
    mov r12, rax                    ; r12 = enter name str
    mov rdi, [rbx + PyObject.ob_type]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    mov rsi, r12
    call dict_get
    test rax, rax
    jz .bw_no_enter_decref_name

    ; Got __enter__ function - call it with mgr as self
    push rax                        ; save func
    mov rdi, r12
    call obj_decref                 ; DECREF enter name
    pop rax                         ; restore func

    ; Call __enter__(mgr): tp_call(enter_func, &mgr, 1)
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .bw_no_enter

    ; Set up call: push mgr as arg on stack temporarily
    push qword [rbp-32]            ; &mgr on machine stack
    mov rdi, rax                   ; callable = __enter__
    mov rsi, rsp                   ; args ptr = &mgr
    mov rdx, 1                     ; nargs = 1
    call rcx
    add rsp, 8                     ; pop mgr arg
    mov [rbp-48], rax              ; save __enter__ result

    ; DECREF mgr
    mov rdi, [rbp-32]
    call obj_decref

    ; Push __enter__ result
    mov rax, [rbp-48]
    VPUSH rax

    add rsp, 32
    pop r12
    pop rbx
    pop rbp
    DISPATCH

.bw_no_exit_decref_name:
    mov rdi, r12
    call obj_decref
.bw_no_exit:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "__exit__"
    call raise_exception

.bw_no_enter_decref_name:
    mov rdi, r12
    call obj_decref
.bw_no_enter:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "__enter__"
    call raise_exception
END_FUNC op_before_with

section .rodata
bw_str_exit:  db "__exit__", 0
bw_str_enter: db "__enter__", 0
section .text

;; ============================================================================
;; op_with_except_start - Call __exit__ with exception info
;;
;; Python 3.12 WITH_EXCEPT_START (opcode 49).
;;
;; Stack: ... | exit_self | exit_func | lasti | exc_or_none | val  ->
;;        ... | exit_self | exit_func | lasti | exc_or_none | val | result
;;
;; Calls exit_func(exit_self, exc_type, exc_val, exc_tb)
;; For now: call exit_func(exit_self, val, val, None)
;; Push result of __exit__ call.
;; ============================================================================
extern none_singleton

DEF_FUNC op_with_except_start, 48

    ; Stack layout (TOS is rightmost):
    ; PEEK(1) = val (exception)
    ; PEEK(2) = exc_or_none (lasti in some docs)
    ; PEEK(3) = lasti
    ; PEEK(4) = exit_func
    ; PEEK(5) = exit_self
    ; Actually the order is: exit_self | exit_func | lasti | exc_or_none | val
    ; val = [r13-8], exc_or_none = [r13-16], lasti = [r13-24], exit_func = [r13-32], exit_self = [r13-40]

    mov rax, [r13-32]               ; exit_func
    mov [rbp-8], rax
    mov rax, [r13-40]               ; exit_self
    mov [rbp-16], rax
    mov rax, [r13-8]                ; val (exception value)
    mov [rbp-24], rax

    ; Get tp_call on exit_func
    mov rdi, [rbp-8]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .wes_error

    ; Build args array on machine stack: [self, exc_type, exc_val, exc_tb]
    ; exc_type = type(val), exc_val = val, exc_tb = None
    ; For simplicity: [self, val, val, None]
    mov rcx, [rbp-24]               ; val
    sub rsp, 32                      ; 4 args
    mov rdx, [rbp-16]               ; self
    mov [rsp], rdx
    ; Get type of exception
    test rcx, rcx
    jz .wes_none_exc
    js .wes_none_exc
    mov rdx, [rcx + PyObject.ob_type]
    jmp .wes_set_args
.wes_none_exc:
    lea rdx, [rel none_singleton]
.wes_set_args:
    mov [rsp+8], rdx                 ; exc_type
    mov [rsp+16], rcx                ; exc_val
    lea rdx, [rel none_singleton]
    mov [rsp+24], rdx                ; exc_tb = None

    ; Call exit_func(self, exc_type, exc_val, exc_tb)
    mov rdi, [rbp-8]                 ; callable = exit_func
    mov rsi, rsp                     ; args ptr
    mov rdx, 4                       ; nargs
    call rax
    add rsp, 32
    mov [rbp-32], rax                ; save result

    ; Push result onto value stack
    mov rax, [rbp-32]
    VPUSH rax

    leave
    DISPATCH

.wes_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__exit__ is not callable"
    call raise_exception
END_FUNC op_with_except_start
