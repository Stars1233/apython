; syscall.asm - Thin wrappers around Linux x86-64 syscalls
; Eliminates libc stdio dependency for all I/O paths

%include "macros.inc"


; Syscall numbers (x86-64)
SYS_read            equ 0
SYS_write           equ 1
SYS_open            equ 2
SYS_close           equ 3
SYS_fstat           equ 5
SYS_mmap            equ 9
SYS_munmap          equ 11
SYS_lseek           equ 8
SYS_socket          equ 41
SYS_connect         equ 42
SYS_accept4         equ 288
SYS_sendto          equ 44
SYS_recvfrom        equ 45
SYS_bind            equ 49
SYS_listen          equ 50
SYS_getsockname     equ 51
SYS_setsockopt      equ 54
SYS_fcntl           equ 72
SYS_io_uring_setup  equ 425
SYS_io_uring_enter  equ 426
SYS_exit_group      equ 231

; sys_write(int fd, const void *buf, size_t len) -> ssize_t
DEF_FUNC_BARE sys_write
    mov rax, SYS_write
    ; rdi=fd, rsi=buf, rdx=len already in place
    syscall
    ret
END_FUNC sys_write

; sys_read(int fd, void *buf, size_t len) -> ssize_t
DEF_FUNC_BARE sys_read
    mov rax, SYS_read
    ; rdi=fd, rsi=buf, rdx=len already in place
    syscall
    ret
END_FUNC sys_read

; sys_open(const char *path, int flags, int mode) -> int fd
DEF_FUNC_BARE sys_open
    mov rax, SYS_open
    ; rdi=path, rsi=flags, rdx=mode already in place
    syscall
    ret
END_FUNC sys_open

; sys_close(int fd) -> int
DEF_FUNC_BARE sys_close
    mov rax, SYS_close
    ; rdi=fd already in place
    syscall
    ret
END_FUNC sys_close

; sys_fstat(int fd, struct stat *buf) -> int
DEF_FUNC_BARE sys_fstat
    mov rax, SYS_fstat
    ; rdi=fd, rsi=buf already in place
    syscall
    ret
END_FUNC sys_fstat

; sys_lseek(int fd, off_t offset, int whence) -> off_t
DEF_FUNC_BARE sys_lseek
    mov rax, SYS_lseek
    ; rdi=fd, rsi=offset, rdx=whence already in place
    syscall
    ret
END_FUNC sys_lseek

; sys_exit(int code) -> noreturn
DEF_FUNC_BARE sys_exit
    mov rax, SYS_exit_group
    ; rdi=code already in place
    syscall
    ; should never reach here
    hlt
END_FUNC sys_exit

; sys_mmap(addr, len, prot, flags, fd, offset) -> void*
DEF_FUNC_BARE sys_mmap
    mov rax, SYS_mmap
    mov r10, rcx               ; Linux syscall: 4th arg in r10, not rcx
    syscall
    ret
END_FUNC sys_mmap

; sys_munmap(addr, len) -> int
DEF_FUNC_BARE sys_munmap
    mov rax, SYS_munmap
    syscall
    ret
END_FUNC sys_munmap

; sys_io_uring_setup(entries, params*) -> int fd
DEF_FUNC_BARE sys_io_uring_setup
    mov rax, SYS_io_uring_setup
    syscall
    ret
END_FUNC sys_io_uring_setup

; sys_io_uring_enter(fd, to_submit, min_complete, flags, sig, sigsz) -> int
DEF_FUNC_BARE sys_io_uring_enter
    mov rax, SYS_io_uring_enter
    mov r10, rcx               ; 4th arg
    syscall
    ret
END_FUNC sys_io_uring_enter

; sys_socket(domain, type, protocol) -> int fd
DEF_FUNC_BARE sys_socket
    mov rax, SYS_socket
    syscall
    ret
END_FUNC sys_socket

; sys_bind(fd, addr*, addrlen) -> int
DEF_FUNC_BARE sys_bind
    mov rax, SYS_bind
    syscall
    ret
END_FUNC sys_bind

; sys_listen(fd, backlog) -> int
DEF_FUNC_BARE sys_listen
    mov rax, SYS_listen
    syscall
    ret
END_FUNC sys_listen

; sys_accept4(fd, addr*, addrlen*, flags) -> int
DEF_FUNC_BARE sys_accept4
    mov rax, SYS_accept4
    mov r10, rcx               ; 4th arg
    syscall
    ret
END_FUNC sys_accept4

; sys_connect(fd, addr*, addrlen) -> int
DEF_FUNC_BARE sys_connect
    mov rax, SYS_connect
    syscall
    ret
END_FUNC sys_connect

; sys_sendto(fd, buf, len, flags, dest_addr*, addrlen) -> ssize_t
DEF_FUNC_BARE sys_sendto
    mov rax, SYS_sendto
    mov r10, rcx               ; 4th arg
    syscall
    ret
END_FUNC sys_sendto

; sys_recvfrom(fd, buf, len, flags, src_addr*, addrlen*) -> ssize_t
DEF_FUNC_BARE sys_recvfrom
    mov rax, SYS_recvfrom
    mov r10, rcx               ; 4th arg
    syscall
    ret
END_FUNC sys_recvfrom

; sys_setsockopt(fd, level, optname, optval*, optlen) -> int
DEF_FUNC_BARE sys_setsockopt
    mov rax, SYS_setsockopt
    mov r10, rcx               ; 4th arg
    syscall
    ret
END_FUNC sys_setsockopt

; sys_getsockname(fd, addr*, addrlen*) -> int
DEF_FUNC_BARE sys_getsockname
    mov rax, SYS_getsockname
    syscall
    ret
END_FUNC sys_getsockname

; sys_fcntl(fd, cmd, arg) -> int
DEF_FUNC_BARE sys_fcntl
    mov rax, SYS_fcntl
    syscall
    ret
END_FUNC sys_fcntl
