; pyc.asm - Read and validate .pyc file headers
; Opens a .pyc file, validates the magic number, and reads the code object

%include "macros.inc"
%include "marshal.inc"

section .text

extern fopen
extern fclose
extern fseek
extern ftell
extern fread
extern ap_malloc
extern ap_free
extern fatal_error
extern marshal_read_object
extern marshal_init_refs

; Global marshal state (defined in marshal.asm)
extern marshal_buf
extern marshal_pos
extern marshal_len

; pyc_read_file(const char *filename) -> PyObject*
; Opens a .pyc file, reads it into memory, validates the header,
; and returns the code object via marshal_read_object.
global pyc_read_file
pyc_read_file:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; rbx = filename

    ; fopen(filename, "rb")
    lea rsi, [rel pyc_mode_rb]
    call fopen wrt ..plt
    test rax, rax
    jz pyc_open_failed
    mov r12, rax             ; r12 = FILE*

    ; fseek(fp, 0, SEEK_END)
    mov rdi, r12
    xor esi, esi
    mov edx, 2              ; SEEK_END
    call fseek wrt ..plt

    ; ftell(fp) -> file size
    mov rdi, r12
    call ftell wrt ..plt
    mov r13, rax             ; r13 = file size

    ; Validate minimum size
    cmp r13, PYC_HEADER_SIZE
    jl pyc_too_small

    ; fseek(fp, 0, SEEK_SET)
    mov rdi, r12
    xor esi, esi
    xor edx, edx            ; SEEK_SET = 0
    call fseek wrt ..plt

    ; ap_malloc(file_size)
    mov rdi, r13
    call ap_malloc
    mov rbx, rax             ; rbx = buffer

    ; fread(buffer, 1, file_size, fp)
    mov rdi, rbx
    mov esi, 1
    mov rdx, r13
    mov rcx, r12
    call fread wrt ..plt
    cmp rax, r13
    jne pyc_read_failed

    ; fclose(fp)
    mov rdi, r12
    call fclose wrt ..plt

    ; Validate magic number (first 4 bytes)
    mov eax, [rbx]
    cmp eax, PYC_MAGIC_3_12
    jne pyc_bad_magic

    ; Set up marshal read state
    mov [rel marshal_buf], rbx
    mov qword [rel marshal_pos], PYC_HEADER_SIZE  ; skip 16-byte header
    mov [rel marshal_len], r13

    ; Initialize marshal reference list
    call marshal_init_refs

    ; Call marshal_read_object to read the code object
    call marshal_read_object
    mov r12, rax             ; r12 = code object

    ; Free the file buffer
    mov rdi, rbx
    call ap_free

    ; Return the code object
    mov rax, r12
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

pyc_open_failed:
    lea rdi, [rel pyc_err_open]
    call fatal_error

pyc_too_small:
    ; Close file before error
    mov rdi, r12
    call fclose wrt ..plt
    lea rdi, [rel pyc_err_small]
    call fatal_error

pyc_read_failed:
    ; Buffer allocated, file open - close and free before error
    mov rdi, r12
    call fclose wrt ..plt
    mov rdi, rbx
    call ap_free
    lea rdi, [rel pyc_err_read]
    call fatal_error

pyc_bad_magic:
    ; Buffer allocated, file closed
    mov rdi, rbx
    call ap_free
    lea rdi, [rel pyc_err_magic]
    call fatal_error

section .rodata
pyc_mode_rb:   db "rb", 0
pyc_err_open:  db "pyc: cannot open file", 0
pyc_err_small: db "pyc: file too small for header", 0
pyc_err_read:  db "pyc: failed to read file", 0
pyc_err_magic: db "pyc: invalid magic number (expected Python 3.12)", 0
