; comperr.asm - Error reporting for the Python source compiler
;
; The compiler must never call raise_exception.  That function tail-jumps into
; eval_exception_unwind, which calls fatal_error when there is no live
; interpreter frame -- and `./apython foo.py` compiles from main.asm before any
; frame exists.  So a compiler error is *recorded* here and the failing
; function returns 0/NULL; the recorded error is turned into a real exception
; once, by the driver, after every arena and buffer has been released.
;
; The first error wins.  The parser keeps running after one is recorded (in
; panic mode, where the token cursor reports ENDMARKER forever), so loops
; terminate without every call site needing an explicit check -- but the
; message the user sees must be the first thing that actually went wrong, not
; the last confused thing the parser said about it.

%include "macros.inc"
%include "object.inc"
%include "compiler.inc"

section .text

;; ============================================================================
;; comp_error(Comp *c, PyTypeObject *type, const char *msg, int lineno, int col)
;;   -> rax = 0, always, so callers can `jmp comp_error`-style tail into it and
;;      return the failure value in one go.
;; ============================================================================
DEF_FUNC_BARE comp_error
    cmp dword [rdi + Comp.err + CompErr.set], 0
    jne .already
    mov [rdi + Comp.err + CompErr.type], rsi
    mov [rdi + Comp.err + CompErr.msg], rdx
    mov [rdi + Comp.err + CompErr.lineno], ecx
    mov [rdi + Comp.err + CompErr.col], r8d
    mov dword [rdi + Comp.err + CompErr.set], 1
.already:
    xor eax, eax
    ret
END_FUNC comp_error

;; ============================================================================
;; comp_failed(Comp *c) -> rax = non-zero once an error has been recorded
;; ============================================================================
DEF_FUNC_BARE comp_failed
    mov eax, [rdi + Comp.err + CompErr.set]
    ret
END_FUNC comp_failed

ASM_INIT
