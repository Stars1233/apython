; syscall.asm - Thin wrappers around Linux x86-64 syscalls
; Eliminates libc stdio dependency for all I/O paths

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

; Syscall numbers (x86-64)
SYS_read        equ 0
SYS_write       equ 1
SYS_open        equ 2
SYS_close       equ 3
SYS_fstat       equ 5
SYS_lseek       equ 8
SYS_exit_group  equ 231

; sys_write(int fd, const void *buf, size_t len) -> ssize_t
global sys_write
sys_write:
    mov rax, SYS_write
    ; rdi=fd, rsi=buf, rdx=len already in place
    syscall
    ret

; sys_read(int fd, void *buf, size_t len) -> ssize_t
global sys_read
sys_read:
    mov rax, SYS_read
    ; rdi=fd, rsi=buf, rdx=len already in place
    syscall
    ret

; sys_open(const char *path, int flags, int mode) -> int fd
global sys_open
sys_open:
    mov rax, SYS_open
    ; rdi=path, rsi=flags, rdx=mode already in place
    syscall
    ret

; sys_close(int fd) -> int
global sys_close
sys_close:
    mov rax, SYS_close
    ; rdi=fd already in place
    syscall
    ret

; sys_fstat(int fd, struct stat *buf) -> int
global sys_fstat
sys_fstat:
    mov rax, SYS_fstat
    ; rdi=fd, rsi=buf already in place
    syscall
    ret

; sys_lseek(int fd, off_t offset, int whence) -> off_t
global sys_lseek
sys_lseek:
    mov rax, SYS_lseek
    ; rdi=fd, rsi=offset, rdx=whence already in place
    syscall
    ret

; sys_exit(int code) -> noreturn
global sys_exit
sys_exit:
    mov rax, SYS_exit_group
    ; rdi=code already in place
    syscall
    ; should never reach here
    hlt
