; error.asm - Error handling and reporting

%include "macros.inc"

section .text

extern fprintf
extern exit
extern fflush
extern stderr

; fatal_error(const char *msg)
; Prints message to stderr and exits with code 1. Never returns.
global fatal_error
fatal_error:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi            ; save msg

    ; fprintf(stderr, "Error: %s\n", msg)
    mov rax, [rel stderr wrt ..got]
    mov rdi, [rax]          ; FILE *stderr
    lea rsi, [rel err_fmt]
    mov rdx, rbx
    xor eax, eax            ; no float args
    call fprintf wrt ..plt

    ; fflush(stderr)
    mov rax, [rel stderr wrt ..got]
    mov rdi, [rax]
    call fflush wrt ..plt

    ; exit(1)
    mov edi, 1
    call exit wrt ..plt

; runtime_error(const char *msg)
; For now, same as fatal_error
global runtime_error
runtime_error:
    jmp fatal_error

; error_unimplemented_opcode(int opcode)
; Reports unimplemented bytecode opcode and exits
global error_unimplemented_opcode
error_unimplemented_opcode:
    push rbp
    mov rbp, rsp
    push rbx
    mov ebx, edi            ; save opcode

    ; fprintf(stderr, "Error: unimplemented opcode %d\n", opcode)
    mov rax, [rel stderr wrt ..got]
    mov rdi, [rax]          ; FILE *stderr
    lea rsi, [rel err_op_fmt]
    mov edx, ebx            ; opcode
    xor eax, eax
    call fprintf wrt ..plt

    ; fflush(stderr)
    mov rax, [rel stderr wrt ..got]
    mov rdi, [rax]
    call fflush wrt ..plt

    ; exit(1)
    mov edi, 1
    call exit wrt ..plt

section .rodata
err_fmt: db "Error: %s", 10, 0
err_op_fmt: db "Error: unimplemented opcode %d", 10, 0
