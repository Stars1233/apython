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
extern fatal_error

;; ============================================================================
;; op_call - Call a callable object (Phase 4: builtin functions only)
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
;; After the call:
;;   1. DECREF each argument
;;   2. DECREF callable
;;   3. DECREF null_or_self (no-op if NULL via obj_decref)
;;   4. Pop all consumed items (nargs + 2) from value stack
;;   5. Push return value
;;   6. Skip 3 CACHE entries (6 bytes): add rbx, 6
;;
;; Followed by 3 CACHE entries (6 bytes) that must be skipped.
;; ============================================================================
global op_call
op_call:
    ; ecx = nargs
    ; We enter from dispatch with rsp at 8-mod-16 (misaligned for calls).
    ; Use a frame pointer to manage stack alignment and local storage.
    push rbp
    mov rbp, rsp
    ; rsp is now 16-byte aligned (8-mod-16 + push rbp = 0-mod-16)

    ; Save nargs in a callee-saved location (use the stack frame)
    ; We cannot use r8-r11 across calls (caller-saved).
    ; Use rbp-relative locals instead.
    ; [rbp-8]  = nargs
    ; [rbp-16] = callable ptr
    ; [rbp-24] = return value from tp_call
    sub rsp, 32                     ; allocate locals (32 keeps 16-byte alignment)

    mov [rbp-8], rcx                ; save nargs (zero-extended, qword)

    ; Compute callable address: [r13 - (nargs+1)*8]
    lea rax, [rcx + 1]
    neg rax
    mov rdi, [r13 + rax*8]         ; rdi = callable
    mov [rbp-16], rdi               ; save callable

    ; Get tp_call from the callable's type
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_callable

    ; Set up args for tp_call(callable, args_ptr, nargs)
    ;   rdi = callable (already set)
    ;   rsi = pointer to first arg = r13 - nargs*8
    ;   edx = nargs
    mov rcx, [rbp-8]               ; reload nargs
    mov rdx, rcx                    ; rdx = nargs
    neg rcx
    lea rsi, [r13 + rcx*8]         ; rsi = &args[0]

    ; rdi = callable (already set above)
    call rax                        ; call tp_call
    mov [rbp-24], rax               ; save return value

    ; --- Cleanup: DECREF all args, callable, and null_or_self ---

    ; DECREF each argument (from top of stack downward)
    mov rcx, [rbp-8]               ; rcx = nargs (loop counter)
    test rcx, rcx
    jz .args_done
.decref_args:
    sub r13, 8                      ; pop from value stack
    mov rdi, [r13]                  ; rdi = arg object
    call obj_decref
    dec qword [rbp-8]              ; decrement counter in memory
    mov rcx, [rbp-8]
    test rcx, rcx
    jnz .decref_args
.args_done:

    ; DECREF callable
    sub r13, 8                      ; pop callable from value stack
    mov rdi, [r13]
    call obj_decref

    ; DECREF null_or_self (obj_decref is NULL-safe)
    sub r13, 8                      ; pop null_or_self from value stack
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
    CSTRING rdi, "TypeError: object is not callable"
    call fatal_error
    ; does not return
