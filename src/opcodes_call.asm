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
    mov [rbp-40], rcx               ; save original nargs

    ; Extract callable: [r13 - (nargs+1)*8]
    lea rax, [rcx + 1]
    neg rax
    mov rdi, [r13 + rax*8]
    mov [rbp-16], rdi               ; save callable

    ; Extract self_or_null: [r13 - (nargs+2)*8]
    lea rax, [rcx + 2]
    neg rax
    mov r8, [r13 + rax*8]          ; r8 = self_or_null
    mov qword [rbp-32], 0          ; is_method = 0

    test r8, r8
    jz .setup_call

    ; === Method call: self_or_null is non-NULL ===
    mov qword [rbp-32], 1          ; is_method = 1

    ; INCREF self for the copy we're about to place in callable's slot
    mov rdi, r8
    call obj_incref

    ; Reload self from value stack (registers may be clobbered by call)
    mov rcx, [rbp-40]              ; original nargs
    lea rax, [rcx + 2]
    neg rax
    mov r8, [r13 + rax*8]          ; r8 = self (reload)

    ; Overwrite callable's value stack slot with self
    mov rcx, [rbp-8]
    lea rax, [rcx + 1]
    neg rax
    mov [r13 + rax*8], r8          ; callable slot now holds self

    ; Increment nargs (self becomes first arg)
    inc qword [rbp-8]

.setup_call:
    ; Get tp_call from the callable's type
    mov rdi, [rbp-16]              ; callable
    test rdi, rdi
    jz .not_callable               ; NULL check
    js .not_callable               ; SmallInt check (bit 63 set)
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .not_callable               ; no type (shouldn't happen)
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_callable

    ; Set up args: tp_call(callable, args_ptr, nargs)
    mov rcx, [rbp-8]
    mov rdx, rcx                    ; total nargs (may include self)
    neg rcx
    lea rsi, [r13 + rcx*8]         ; args_ptr

    mov rdi, [rbp-16]              ; callable
    call rax
    mov [rbp-24], rax               ; save return value

    ; === Cleanup ===
    ; Pop nargs items (may include self copy for method calls), DECREF each
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

    ; Branch based on whether this was a method call
    cmp qword [rbp-32], 0
    je .func_cleanup

    ; === Method cleanup ===
    ; Pop the original self_or_null slot and DECREF it
    sub r13, 8
    mov rdi, [r13]
    call obj_decref

    ; DECREF saved callable (its stack slot was overwritten with self)
    mov rdi, [rbp-16]
    call obj_decref
    jmp .push_result

.func_cleanup:
    ; === Function cleanup (original path) ===
    ; Pop callable from value stack and DECREF
    sub r13, 8
    mov rdi, [r13]
    call obj_decref

    ; Pop null_or_self (NULL, obj_decref is null-safe) and DECREF
    sub r13, 8
    mov rdi, [r13]
    call obj_decref

.push_result:
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
;;   closure tuple (if flag 0x08)
;;   annotations (if flag 0x04) - ignored
;;   kwdefaults dict (if flag 0x02) - ignored
;;   defaults tuple (if flag 0x01) - ignored
;;   code_obj (always on top)
;; ============================================================================
DEF_FUNC op_make_function, 32
    ; [rbp-8]=flags, [rbp-16]=code, [rbp-24]=closure

    mov [rbp-8], ecx               ; save flags
    mov qword [rbp-24], 0          ; closure = NULL default

    ; Pop code object from value stack (always TOS)
    VPOP rdi
    mov [rbp-16], rdi

    ; Check flags for closure (must pop in reverse order of push)
    ; Defaults (0x01) - pop and ignore for now
    test ecx, MAKE_FUNC_DEFAULTS
    jz .mf_no_defaults
    VPOP rdi
    DECREF_REG rdi                 ; discard defaults tuple
    mov ecx, [rbp-8]
.mf_no_defaults:

    ; kwdefaults (0x02) - pop and ignore for now
    test ecx, MAKE_FUNC_KWDEFAULTS
    jz .mf_no_kwdefaults
    VPOP rdi
    DECREF_REG rdi
    mov ecx, [rbp-8]
.mf_no_kwdefaults:

    ; annotations (0x04) - pop and ignore
    test ecx, MAKE_FUNC_ANNOTATIONS
    jz .mf_no_annotations
    VPOP rdi
    DECREF_REG rdi
    mov ecx, [rbp-8]
.mf_no_annotations:

    ; closure (0x08) - pop and save
    test ecx, MAKE_FUNC_CLOSURE
    jz .mf_no_closure
    VPOP rax
    mov [rbp-24], rax              ; save closure tuple
.mf_no_closure:

    ; Create function: func_new(code, globals)
    mov rdi, [rbp-16]
    mov rsi, [r12 + PyFrame.globals]
    call func_new
    ; rax = new function object

    ; Set closure if present
    mov rcx, [rbp-24]
    mov [rax + PyFuncObject.func_closure], rcx

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
