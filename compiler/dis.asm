; dis.asm - Disassemble a code object
;
; Reached with `./apython --dis "<expression>"`.  Fidelity here is semantic
; rather than byte-for-byte, so the fastest way to localise a codegen bug is to
; put this output beside `python3 -m dis` and look at where the two diverge --
; the opcode sequence should match even where the exact opargs need not.
;
; Walks the instruction stream the way the interpreter does, skipping the
; CACHE words each opcode carries, so a wrong cache count shows up here as a
; stream that decodes into nonsense rather than as a runtime mystery.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ap_strlen
extern compile_source
extern current_exception
extern obj_decref
extern str_from_cstr_heap
extern opcode_names
extern op_meta
extern sys_write

; --- Named frame-layout constants ---
DS_CODE  equ 8
DS_OFF   equ 16
DS_ARG   equ 24
DS_NUM   equ 56          ; a 32-byte scratch buffer at [rbp - 56]
DS_FRAME equ 56          ; + 5 pushes = 96

section .text

;; ============================================================================
;; dis_puts(const char *s)
;; ============================================================================
DEF_FUNC dis_puts, 8
    push rbx
    mov rbx, rdi
    call ap_strlen
    mov rdx, rax
    mov edi, 1
    mov rsi, rbx
    call sys_write
    pop rbx
    leave
    ret
END_FUNC dis_puts

;; ============================================================================
;; dis_num(int64_t v, int width)
;; Right-aligned decimal, so the columns line up with CPython's dis output.
;; ============================================================================
DN_FRAME equ 40          ; + 1 push = 48
DEF_FUNC dis_num, DN_FRAME
    push rbx
    mov rbx, rsi                        ; width
    lea rsi, [rbp - 8]
    mov byte [rsi], ' '
    mov rax, rdi
    mov ecx, 10
    xor r8d, r8d
    test rax, rax
    jns .digits
    neg rax
    mov r8d, 1
.digits:
    dec rsi
    xor edx, edx
    div rcx
    add dl, '0'
    mov [rsi], dl
    inc rbx
    test rax, rax
    jnz .digits
    test r8d, r8d
    jz .pad
    dec rsi
    mov byte [rsi], '-'
    inc rbx
.pad:
    ; rbx counted digits; pad out to the requested width.
    lea rdx, [rbp - 7]
    sub rdx, rsi                        ; bytes produced, including the trailer
.padloop:
    cmp rdx, 12
    jae .emit
    dec rsi
    mov byte [rsi], ' '
    inc rdx
    jmp .padloop
.emit:
    mov edi, 1
    call sys_write
    pop rbx
    leave
    ret
END_FUNC dis_num

;; ============================================================================
;; code_disassemble(PyCodeObject *code)
;;
;; rbx = code, r12 = byte offset, r13 = length, r14 = co_code base,
;; r15 = accumulated EXTENDED_ARG value.
;; ============================================================================
DEF_FUNC code_disassemble, DS_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov rbx, rdi
    lea r14, [rbx + PyCodeObject.co_code]
    mov r13d, [rbx + PyCodeObject.co_code_len]
    xor r12d, r12d
    xor r15d, r15d

.loop:
    cmp r12, r13
    jae .done

    mov rdi, r12
    shr rdi, 1                          ; report offsets in code units
    mov esi, 4
    call dis_num
    CSTRING rdi, "  "
    call dis_puts

    movzx eax, byte [r14 + r12]
    movzx edx, byte [r14 + r12 + 1]
    mov [rbp - DS_ARG], rdx
    mov [rbp - DS_OFF], rax

    lea rcx, [rel opcode_names]
    mov rdi, [rcx + rax*8]
    call dis_puts

    ; EXTENDED_ARG accumulates into the next instruction's oparg.
    mov rax, [rbp - DS_OFF]
    cmp eax, OP_EXTENDED_ARG
    jne .real_op
    shl r15, 8
    or r15, [rbp - DS_ARG]
    CSTRING rdi, `\n`
    call dis_puts
    add r12, 2
    jmp .loop

.real_op:
    ; Only opcodes that take one print an argument.
    lea rcx, [rel op_meta]
    mov rax, [rbp - DS_OFF]
    shl rax, 2
    movzx edx, byte [rcx + rax + OpMeta.flags]
    test dl, OM_HASARG
    jz .no_arg
    shl r15, 8
    or r15, [rbp - DS_ARG]
    mov rdi, r15
    mov esi, 6
    call dis_num
.no_arg:
    xor r15d, r15d
    CSTRING rdi, `\n`
    call dis_puts

    ; Step over the instruction and the CACHE words it carries.
    add r12, 2
    lea rcx, [rel op_meta]
    mov rax, [rbp - DS_OFF]
    shl rax, 2
    movzx eax, byte [rcx + rax + OpMeta.cache]
    shl rax, 1
    add r12, rax
    jmp .loop

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC code_disassemble


;; ============================================================================
;; dis_main(const char *expr) -> rax = exit status
;; Compiles one expression and prints its bytecode.
;; ============================================================================
DEF_FUNC dis_main, 16           ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi
    call ap_strlen
    mov r12, rax
    lea rdi, [rel dis_filename]
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    mov ecx, [rel dis_mode]
    call compile_source
    mov rbx, rax
    pop rdi
    call obj_decref
    test rbx, rbx
    jz .failed
    mov rdi, rbx
    call code_disassemble
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
.failed:
    CSTRING rdi, `--dis: could not compile that expression\n`
    call dis_puts
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
END_FUNC dis_main

section .rodata
dis_filename: db "<dis>", 0

section .data
align 8
global dis_mode
dis_mode: dq CMODE_EVAL

ASM_INIT
