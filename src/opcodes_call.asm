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

section .note.GNU-stack noalloc noexec nowrite progbits

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
global op_call
op_call:
    push rbp
    mov rbp, rsp
    ; Locals:
    ;   [rbp-8]  = nargs (possibly incremented for method calls)
    ;   [rbp-16] = callable ptr (saved before overwriting stack slot)
    ;   [rbp-24] = return value from tp_call
    ;   [rbp-32] = is_method (0 or 1)
    ;   [rbp-40] = original nargs (before increment)
    sub rsp, 48                     ; allocate locals (48 keeps 16-byte alignment)

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

;; ============================================================================
;; op_make_function - Create a function from code object on TOS
;;
;; Python 3.12 MAKE_FUNCTION (opcode 132).
;; arg = flags: 0 = plain, 1 = defaults, 2 = kwdefaults, 4 = annotations, 8 = closure
;; Currently supports arg=0 only (no defaults/closures).
;;
;; Stack: ... | code_obj | -> ... | func_obj |
;; ============================================================================
global op_make_function
op_make_function:
    ; Pop code object from value stack
    VPOP rdi                   ; rdi = code object

    ; Get globals from current frame
    mov rsi, [r12 + PyFrame.globals]  ; rsi = globals dict

    ; Save code obj for DECREF after func_new INCREFs it
    push rdi

    ; Create function: func_new(code, globals)
    call func_new
    ; rax = new function object

    ; DECREF the code object (func_new INCREFed it)
    push rax                   ; save func obj on machine stack
    mov rdi, [rsp + 8]        ; code object (saved earlier)
    DECREF_REG rdi
    pop rax                    ; restore func obj
    add rsp, 8                 ; discard saved code object

    ; Push function onto value stack
    VPUSH rax
    DISPATCH
