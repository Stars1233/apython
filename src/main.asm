; main.asm - Entry point for apython
; Parses argv, loads .pyc, creates frame, runs bytecode

%include "macros.inc"
%include "object.inc"
%include "frame.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern bool_init
extern builtins_init
extern methods_init
extern dict_new
extern dict_set
extern frame_new
extern frame_free
extern eval_frame
extern pyc_read_file
extern fatal_error
extern obj_decref
extern str_from_cstr

; main(int argc, char **argv) -> int
global main
main:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    ; Check argc >= 2
    cmp edi, 2
    jl .usage

    ; Save argv[1] (the .pyc filename)
    mov rbx, [rsi + 8]      ; rbx = argv[1]

    ; Initialize subsystems
    call bool_init

    ; Load .pyc file -> code object
    mov rdi, rbx
    call pyc_read_file
    test rax, rax
    jz .load_failed
    mov r12, rax             ; r12 = code object

    ; Create builtins dict
    call builtins_init
    mov r13, rax             ; r13 = builtins dict

    ; Initialize type methods (str, list, dict tp_dict)
    call methods_init

    ; Create globals dict
    call dict_new
    mov r14, rax             ; r14 = globals dict

    ; Set __name__ = "__main__" in globals
    lea rdi, [rel __name__cstr]
    call str_from_cstr
    push rax                 ; save key str
    lea rdi, [rel __main__cstr]
    call str_from_cstr
    mov rdx, rax             ; value = "__main__" str
    pop rsi                  ; key = "__name__" str
    mov rdi, r14             ; dict = globals
    call dict_set

    ; Create execution frame
    ; frame_new(code, globals, builtins, locals)
    ; For module-level code, locals == globals
    mov rdi, r12             ; code
    mov rsi, r14             ; globals
    mov rdx, r13             ; builtins
    mov rcx, r14             ; locals = globals
    call frame_new
    mov rbx, rax             ; rbx = frame

    ; Execute the bytecode
    mov rdi, rbx
    call eval_frame
    ; rax = return value (ignore for module-level code)

    ; Clean up
    mov rdi, rbx
    call frame_free

    ; Exit 0
    xor eax, eax
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.usage:
    CSTRING rdi, "Usage: apython <file.pyc>"
    call fatal_error

.load_failed:
    CSTRING rdi, "Error: failed to load .pyc file"
    call fatal_error

section .rodata
__name__cstr: db "__name__", 0
__main__cstr: db "__main__", 0
