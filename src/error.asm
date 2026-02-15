; error.asm - Error handling and reporting
; Uses raw Linux syscalls instead of libc stdio

%include "macros.inc"


extern sys_write
extern sys_exit

; fatal_error(const char *msg)
; Prints "Error: <msg>\n" to stderr and exits with code 1. Never returns.
DEF_FUNC fatal_error
    push rbx
    mov rbx, rdi            ; save msg

    ; sys_write(2, "Error: ", 7)
    mov edi, 2
    lea rsi, [rel err_prefix]
    mov edx, 7
    call sys_write

    ; strlen(msg) inline
    mov rdi, rbx
    xor ecx, ecx
.strlen_loop:
    cmp byte [rdi + rcx], 0
    je .strlen_done
    inc rcx
    jmp .strlen_loop
.strlen_done:

    ; sys_write(2, msg, len)
    mov edi, 2
    mov rsi, rbx
    mov rdx, rcx
    call sys_write

    ; sys_write(2, "\n", 1)
    mov edi, 2
    lea rsi, [rel err_newline]
    mov edx, 1
    call sys_write

    ; sys_exit(1)
    mov edi, 1
    call sys_exit
END_FUNC fatal_error

; runtime_error(const char *msg)
; For now, same as fatal_error
DEF_FUNC_BARE runtime_error
    jmp fatal_error
END_FUNC runtime_error

; error_unimplemented_opcode(int opcode)
; Reports unimplemented bytecode opcode and exits
DEF_FUNC error_unimplemented_opcode, 32             ; space for decimal digits

    mov eax, edi            ; opcode value

    ; Convert opcode int to decimal string on stack
    lea rdi, [rbp - 1]     ; write digits right-to-left
    mov byte [rdi], 10      ; trailing newline
    lea rcx, [rbp - 1]     ; rcx = end (points at newline)
    mov r8d, 10

.digit_loop:
    xor edx, edx
    div r8d                 ; eax = quot, edx = rem
    dec rdi
    add dl, '0'
    mov [rdi], dl
    test eax, eax
    jnz .digit_loop

    ; rdi = start of digits, rcx = newline position
    ; length = rcx - rdi + 1 (include newline)
    mov r8, rcx
    sub r8, rdi
    inc r8                  ; r8 = length of digits + newline

    ; Save digit start and length
    mov rbx, rdi
    mov r9, r8

    ; sys_write(2, prefix, prefix_len)
    mov edi, 2
    lea rsi, [rel err_op_prefix]
    mov edx, err_op_prefix_len
    call sys_write

    ; sys_write(2, digits_and_newline, len)
    mov edi, 2
    mov rsi, rbx
    mov rdx, r9
    call sys_write

    ; sys_exit(1)
    mov edi, 1
    call sys_exit
END_FUNC error_unimplemented_opcode

section .rodata
err_prefix: db "Error: "
err_newline: db 10
err_op_prefix: db "Error: unimplemented opcode "
err_op_prefix_len equ $ - err_op_prefix
