; pyc.asm - Read and validate .pyc file headers
; Opens a .pyc file, validates the magic number, and reads the code object
; Uses raw Linux syscalls instead of libc stdio

%include "macros.inc"
%include "marshal.inc"


extern sys_open
extern sys_close
extern sys_fstat
extern sys_read
extern ap_malloc
extern ap_free
extern fatal_error
extern marshal_read_object
extern marshal_init_refs

; Global marshal state (defined in marshal.asm)
extern marshal_buf
extern marshal_pos
extern marshal_len

; struct stat offsets (x86-64 Linux)
STAT_SIZE       equ 144         ; sizeof(struct stat)
STAT_ST_SIZE    equ 48          ; offset of st_size

; open flags
O_RDONLY        equ 0

; pyc_read_file(const char *filename) -> PyObject*
; Opens a .pyc file, reads it into memory, validates the header,
; and returns the code object via marshal_read_object.
DEF_FUNC pyc_read_file
    push rbx
    push r12
    push r13
    sub rsp, STAT_SIZE + 8      ; stat buf + alignment

    mov rbx, rdi            ; rbx = filename

    ; sys_open(filename, O_RDONLY, 0)
    mov esi, O_RDONLY
    xor edx, edx
    call sys_open
    test rax, rax
    js pyc_open_failed      ; negative = error
    mov r12, rax             ; r12 = fd

    ; sys_fstat(fd, &stat_buf) to get file size
    mov rdi, r12
    lea rsi, [rbp - STAT_SIZE - 24]  ; stat buf on stack (after 3 pushes = 24 bytes)
    call sys_fstat
    test rax, rax
    js pyc_stat_failed

    ; Read st_size from stat struct
    mov r13, [rbp - STAT_SIZE - 24 + STAT_ST_SIZE]  ; r13 = file size

    ; Validate minimum size
    cmp r13, PYC_HEADER_SIZE
    jl pyc_too_small

    ; ap_malloc(file_size)
    mov rdi, r13
    call ap_malloc
    mov rbx, rax             ; rbx = buffer

    ; Read entire file: sys_read loop for partial reads
    xor r8d, r8d            ; r8 = total bytes read
.read_loop:
    mov rdi, r12            ; fd
    lea rsi, [rbx + r8]    ; buf + offset
    mov rdx, r13
    sub rdx, r8             ; remaining bytes
    call sys_read
    test rax, rax
    jle pyc_read_failed     ; 0 = EOF too early, negative = error
    add r8, rax
    cmp r8, r13
    jl .read_loop

    ; sys_close(fd)
    mov rdi, r12
    call sys_close

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
    add rsp, STAT_SIZE + 8
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC pyc_read_file

pyc_open_failed:
    lea rdi, [rel pyc_err_open]
    call fatal_error

pyc_stat_failed:
    ; Close fd before error
    mov rdi, r12
    call sys_close
    lea rdi, [rel pyc_err_stat]
    call fatal_error

pyc_too_small:
    ; Close fd before error
    mov rdi, r12
    call sys_close
    lea rdi, [rel pyc_err_small]
    call fatal_error

pyc_read_failed:
    ; Buffer allocated, file open - close and free before error
    mov rdi, r12
    call sys_close
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
pyc_err_open:  db "pyc: cannot open file", 0
pyc_err_stat:  db "pyc: cannot stat file", 0
pyc_err_small: db "pyc: file too small for header", 0
pyc_err_read:  db "pyc: failed to read file", 0
pyc_err_magic: db "pyc: invalid magic number (expected Python 3.12)", 0
