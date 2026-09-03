; runtime.asm - The freestanding layer: syscalls, memory, strings, and dying
;
; Everything here is below the object model and cannot allocate a Python object
; or raise a Python exception.  It is the bottom of the call graph: ap_malloc
; calls fatal_error when it is out of memory, and fatal_error can only reach
; sys_write and sys_exit.  That chain used to span three directories and two
; source trees; it is one file now, and the externs between them are gone.
;
; These are PLT-free by design -- the memory and string operations are open
; coded rather than calling into libc, so a Python-level operation does not
; pay a PLT indirection per byte.

%include "macros.inc"
%include "object.inc"


;; ============================================================================
;; Linux x86-64 syscall wrappers
;; (was src/lib/syscall.asm)
;; ============================================================================

section .text

; Eliminates libc stdio dependency for all I/O paths

; Syscall numbers (x86-64)
SYS_read            equ 0
SYS_write           equ 1
SYS_open            equ 2
SYS_close           equ 3
SYS_fstat           equ 5
SYS_mmap            equ 9
SYS_munmap          equ 11
SYS_socket          equ 41
SYS_connect         equ 42
SYS_accept4         equ 288
SYS_sendto          equ 44
SYS_recvfrom        equ 45
SYS_bind            equ 49
SYS_listen          equ 50
SYS_setsockopt      equ 54
SYS_fcntl           equ 72
SYS_ioctl           equ 16
SYS_io_uring_setup  equ 425
SYS_io_uring_enter  equ 426
SYS_lseek           equ 8
SYS_stat            equ 4
SYS_lstat           equ 6
SYS_dup             equ 32
SYS_getpid          equ 39
SYS_wait4           equ 61
SYS_rename          equ 82
SYS_symlink equ 88
SYS_mkdir           equ 83
SYS_rmdir           equ 84
SYS_unlink          equ 87
SYS_readlink        equ 89
SYS_chmod           equ 90
SYS_getcwd          equ 79
SYS_getdents64      equ 217
SYS_pipe2           equ 293
SYS_getrandom       equ 318
SYS_ftruncate       equ 77
SYS_uname           equ 63
SYS_access          equ 21
SYS_umask           equ 95
SYS_exit_group      equ 231
SYS_chdir           equ 80
SYS_truncate        equ 76
SYS_link            equ 86
SYS_chown           equ 92
SYS_fchmod          equ 91
SYS_fsync           equ 74
SYS_dup2            equ 33
SYS_utimensat       equ 280

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



;; ---------------------------------------------------------------------------
;; The syscalls the posix module needs.  Each returns the kernel's own value:
;; the result, or -errno.  Nothing normalises to -1 -- posix_check reads the
;; negative directly to build the OSError.
;;
;; The fourth argument goes in r10, not rcx, and `syscall` itself clobbers rcx
;; and r11 -- so `mov r10, rcx` has to come before it, never after.
;; ---------------------------------------------------------------------------

; sys_stat(const char *path, struct stat *buf) -> int
DEF_FUNC_BARE sys_stat
    mov rax, SYS_stat
    syscall
    ret
END_FUNC sys_stat

; sys_lstat(const char *path, struct stat *buf) -> int
DEF_FUNC_BARE sys_lstat
    mov rax, SYS_lstat
    syscall
    ret
END_FUNC sys_lstat

; sys_lseek(int fd, off_t off, int whence) -> off_t
DEF_FUNC_BARE sys_lseek
    mov rax, SYS_lseek
    syscall
    ret
END_FUNC sys_lseek

; sys_dup(int fd) -> int
DEF_FUNC_BARE sys_dup
    mov rax, SYS_dup
    syscall
    ret
END_FUNC sys_dup

; sys_getpid(void) -> pid_t
DEF_FUNC_BARE sys_getpid
    mov rax, SYS_getpid
    syscall
    ret
END_FUNC sys_getpid

; sys_getcwd(char *buf, size_t size) -> long (the length including the NUL)
DEF_FUNC_BARE sys_getcwd
    mov rax, SYS_getcwd
    syscall
    ret
END_FUNC sys_getcwd

; sys_mkdir(const char *path, mode_t mode) -> int
DEF_FUNC_BARE sys_mkdir
    mov rax, SYS_mkdir
    syscall
    ret
END_FUNC sys_mkdir

; sys_rmdir(const char *path) -> int
DEF_FUNC_BARE sys_rmdir
    mov rax, SYS_rmdir
    syscall
    ret
END_FUNC sys_rmdir

; sys_unlink(const char *path) -> int
DEF_FUNC_BARE sys_unlink
    mov rax, SYS_unlink
    syscall
    ret
END_FUNC sys_unlink

; sys_rename(const char *old, const char *new) -> int
DEF_FUNC_BARE sys_rename
    mov rax, SYS_rename
    syscall
    ret
END_FUNC sys_rename

; sys_symlink(const char *target, const char *linkpath) -> int
global sys_symlink
DEF_FUNC_BARE sys_symlink
    mov rax, SYS_symlink
    syscall
    ret
END_FUNC sys_symlink

; sys_readlink(const char *path, char *buf, size_t size) -> ssize_t
DEF_FUNC_BARE sys_readlink
    mov rax, SYS_readlink
    syscall
    ret
END_FUNC sys_readlink

; sys_chmod(const char *path, mode_t mode) -> int
DEF_FUNC_BARE sys_chmod
    mov rax, SYS_chmod
    syscall
    ret
END_FUNC sys_chmod

; sys_access(const char *path, int mode) -> int
DEF_FUNC_BARE sys_access
    mov rax, SYS_access
    syscall
    ret
END_FUNC sys_access

; sys_umask(mode_t mask) -> mode_t (the previous one)
DEF_FUNC_BARE sys_umask
    mov rax, SYS_umask
    syscall
    ret
END_FUNC sys_umask

; sys_pipe2(int fds[2], int flags) -> int
DEF_FUNC_BARE sys_pipe2
    mov rax, SYS_pipe2
    syscall
    ret
END_FUNC sys_pipe2

; sys_getdents64(int fd, void *dirp, unsigned count) -> int bytes read
DEF_FUNC_BARE sys_getdents64
    mov rax, SYS_getdents64
    syscall
    ret
END_FUNC sys_getdents64

; sys_getrandom(void *buf, size_t len, unsigned flags) -> ssize_t
DEF_FUNC_BARE sys_getrandom
    mov rax, SYS_getrandom
    syscall
    ret
END_FUNC sys_getrandom

; sys_ftruncate(int fd, off_t length) -> int
DEF_FUNC_BARE sys_ftruncate
    mov rax, SYS_ftruncate
    syscall
    ret
END_FUNC sys_ftruncate

; The eight the posix module was short of.  Each is the bare syscall; the
; argument checking and the OSError live in src/posixmod.asm.
; sys_chdir(const char *path) -> int
global sys_chdir
DEF_FUNC_BARE sys_chdir
    mov rax, SYS_chdir
    syscall
    ret
END_FUNC sys_chdir

; sys_truncate(const char *path, off_t length) -> int
global sys_truncate
DEF_FUNC_BARE sys_truncate
    mov rax, SYS_truncate
    syscall
    ret
END_FUNC sys_truncate

; sys_link(const char *old, const char *new) -> int
global sys_link
DEF_FUNC_BARE sys_link
    mov rax, SYS_link
    syscall
    ret
END_FUNC sys_link

; sys_chown(const char *path, uid_t uid, gid_t gid) -> int
global sys_chown
DEF_FUNC_BARE sys_chown
    mov rax, SYS_chown
    syscall
    ret
END_FUNC sys_chown

; sys_fchmod(int fd, mode_t mode) -> int
global sys_fchmod
DEF_FUNC_BARE sys_fchmod
    mov rax, SYS_fchmod
    syscall
    ret
END_FUNC sys_fchmod

; sys_fsync(int fd) -> int
global sys_fsync
DEF_FUNC_BARE sys_fsync
    mov rax, SYS_fsync
    syscall
    ret
END_FUNC sys_fsync

; sys_dup2(int oldfd, int newfd) -> int
global sys_dup2
DEF_FUNC_BARE sys_dup2
    mov rax, SYS_dup2
    syscall
    ret
END_FUNC sys_dup2

; sys_utimensat(int dirfd, const char *path, const struct timespec times[2],
;               int flags) -> int
; utime(path, times) goes through this: utimensat is the only one of the
; family Linux still keeps, and AT_FDCWD with a NULL times means "now".
global sys_utimensat
DEF_FUNC_BARE sys_utimensat
    mov r10, rcx                ; the fourth syscall argument is r10, not rcx
    mov rax, SYS_utimensat
    syscall
    ret
END_FUNC sys_utimensat

; sys_uname(struct utsname *buf) -> int
DEF_FUNC_BARE sys_uname
    mov rax, SYS_uname
    syscall
    ret
END_FUNC sys_uname

; sys_wait4(pid_t pid, int *status, int options, struct rusage *ru) -> pid_t
DEF_FUNC_BARE sys_wait4
    mov rax, SYS_wait4
    mov r10, rcx               ; 4th arg -- and syscall clobbers rcx, so first
    syscall
    ret
END_FUNC sys_wait4

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


; sys_fcntl(fd, cmd, arg) -> int
DEF_FUNC_BARE sys_fcntl
    mov rax, SYS_fcntl
    syscall
    ret
END_FUNC sys_fcntl

; sys_ioctl(fd, request, arg) -> int
DEF_FUNC_BARE sys_ioctl
    mov rax, SYS_ioctl
    syscall
    ret
END_FUNC sys_ioctl

;; ============================================================================
;; Memory operations, PLT-free
;; (was src/lib/memops.asm)
;; ============================================================================

section .text

; Replaces libc memcpy and memset
; Uses rep movsb / rep stosb (optimal on ERMS-capable CPUs, Ivy Bridge+)

; ap_memcpy(void *dst, const void *src, size_t n) -> void *dst
DEF_FUNC_BARE ap_memcpy
    mov rax, rdi            ; save dst for return
    mov rcx, rdx            ; rcx = count
    rep movsb               ; rdi=dst, rsi=src already in place
    ret
END_FUNC ap_memcpy

; ap_memset(void *dst, int val, size_t n) -> void *dst
DEF_FUNC_BARE ap_memset
    mov r8, rdi             ; save dst for return
    mov al, sil             ; val (byte)
    mov rcx, rdx            ; rcx = count
    rep stosb               ; rdi=dst already in place
    mov rax, r8             ; return original dst
    ret
END_FUNC ap_memset

; ap_memmove(void *dst, const void *src, size_t n) -> void *dst
; Handles overlapping regions. n must be a multiple of 8.
; Forward: rep movsq (fast). Backward: manual qword loop (avoids std penalty).
DEF_FUNC_BARE ap_memmove
    mov rax, rdi            ; save dst for return
    mov rcx, rdx            ; rcx = byte count
    test rcx, rcx
    jz .memmove_done
    cmp rdi, rsi
    je .memmove_done        ; dst == src, nop
    jb .memmove_fwd         ; dst < src: forward safe
.memmove_bk:
    ; dst > src: copy backward to avoid overlap corruption
    ; Point rsi/rdi to last byte, set direction flag, copy bytes
    lea rsi, [rsi + rcx - 1]
    lea rdi, [rdi + rcx - 1]
    std
    rep movsb
    cld
    ret
.memmove_fwd:
    ; dst < src: forward copy — qwords then byte remainder
    push rdx                ; save original count
    shr rcx, 3
    rep movsq
    pop rcx
    and rcx, 7
    rep movsb
.memmove_done:
    ret
END_FUNC ap_memmove

; ap_memcmp(const void *s1, const void *s2, size_t n) -> int
; Returns 0 if equal, <0 if s1<s2, >0 if s1>s2
DEF_FUNC_BARE ap_memcmp
    mov rcx, rdx            ; rcx = count
    repe cmpsb              ; rdi=s1, rsi=s2
    je .memcmp_equal
    movzx eax, byte [rdi - 1]
    movzx ecx, byte [rsi - 1]
    sub eax, ecx
    ret
.memcmp_equal:
    xor eax, eax
    ret
END_FUNC ap_memcmp

;; ============================================================================
;; String operations, PLT-free
;; (was src/lib/string.asm)
;; ============================================================================

section .text

; Replaces libc strlen, strcmp, strstr

; ap_strlen(const char *s) -> size_t
; Uses repne scasb (fast on modern x86-64 with FAST_SHORT_REP)
DEF_FUNC_BARE ap_strlen
    mov rdi, rdi            ; s already in rdi
    xor eax, eax            ; search for NUL byte
    mov rcx, -1             ; max search length
    repne scasb
    not rcx
    dec rcx                 ; rcx = length (not counting NUL)
    mov rax, rcx
    ret
END_FUNC ap_strlen

; ap_strcmp(const char *a, const char *b) -> int
; 8-byte fast path with byte-at-a-time fallback, returns <0 / 0 / >0
;
; Safety: reading 8 bytes at a time is safe because all callers compare
; PyStrObject.data which is inline after the header. Object allocation
; always provides >=8 bytes past .data even for 1-char strings, due to
; minimum object size and alignment.
DEF_FUNC_BARE ap_strcmp
    ; rdi = a, rsi = b
.fast8:
    mov rax, [rdi]          ; load 8 bytes from a
    mov rdx, [rsi]          ; load 8 bytes from b
    cmp rax, rdx
    jne .byte_loop          ; mismatch -> fall back

    ; Check if NUL within these 8 bytes (Mycroft's trick)
    mov rcx, rax
    mov r8, 0x0101010101010101
    sub rcx, r8
    not rax
    and rcx, rax
    mov r8, 0x8080808080808080
    and rcx, r8
    jnz .equal              ; NUL found -> strings equal

    add rdi, 8
    add rsi, 8
    jmp .fast8

.equal:
    xor eax, eax
    ret

.byte_loop:
    movzx eax, byte [rdi]
    movzx ecx, byte [rsi]
    sub eax, ecx
    jnz .done               ; bytes differ
    test cl, cl
    jz .done                 ; both NUL
    inc rdi
    inc rsi
    jmp .byte_loop
.done:
    ret
END_FUNC ap_strcmp


;; ============================================================================
;; ap_memfind(rdi = hay, rsi = hlen, rdx = needle, rcx = nlen)
;;   -> rax = pointer to the first match, or 0
;;
;; ap_strstr's length-aware sibling.  A Python str is counted, not
;; NUL-terminated: "a\x00b" is three characters, and every search over it that
;; went through ap_strstr stopped at the NUL and reported the tail missing.
;;
;; An empty needle matches at hay, as it does in CPython.  A needle longer than
;; what is left cannot match, which is also the loop's termination condition.
;; The first byte is checked before the inner loop is entered, so a mismatching
;; position costs one compare rather than a call frame -- ap_strstr re-entered
;; its inner loop at every offset, which is what made str.replace quadratic.
;; ============================================================================
DEF_FUNC_BARE ap_memfind
    test rcx, rcx
    jz .amf_empty               ; the empty needle matches immediately
    mov r8, rsi
    sub r8, rcx                 ; r8 = last offset a match could start at
    js .amf_none                ; needle longer than haystack
    movzx r9d, byte [rdx]       ; r9b = the needle's first byte
    xor r10, r10                ; r10 = current offset
                                ; rsi is free from here: r8 is all it was for

.amf_outer:
    cmp r10, r8
    jg .amf_none
    cmp r9b, [rdi + r10]
    jne .amf_next
    ; First byte matches; compare the rest against the candidate.
    lea r11, [rdi + r10]
    mov rax, 1
.amf_inner:
    cmp rax, rcx
    jge .amf_hit
    mov sil, [rdx + rax]
    cmp sil, [r11 + rax]
    jne .amf_next
    inc rax
    jmp .amf_inner
.amf_next:
    inc r10
    jmp .amf_outer

.amf_hit:
    mov rax, r11
    ret
.amf_empty:
    mov rax, rdi
    ret
.amf_none:
    xor eax, eax
    ret
END_FUNC ap_memfind

;; ============================================================================
;; ap_memrfind(rdi = hay, rsi = hlen, rdx = needle, rcx = nlen)
;;   -> rax = pointer to the *last* match, or 0
;;
;; ap_memfind walking down instead of up, for rfind and rindex.  An empty
;; needle matches at the end of the range, which is what "the last place it
;; occurs" means and what CPython returns.
;; ============================================================================
DEF_FUNC_BARE ap_memrfind
    mov r8, rsi
    sub r8, rcx                 ; r8 = last offset a match could start at
    js .amr_none                ; needle longer than haystack
    test rcx, rcx
    jz .amr_empty               ; empty needle: matches at the far end
    movzx r9d, byte [rdx]
    mov r10, r8                 ; r10 = current offset, counting down

.amr_outer:
    cmp r10, 0
    jl .amr_none
    cmp r9b, [rdi + r10]
    jne .amr_next
    lea r11, [rdi + r10]
    mov rax, 1
.amr_inner:
    cmp rax, rcx
    jge .amr_hit
    mov sil, [rdx + rax]
    cmp sil, [r11 + rax]
    jne .amr_next
    inc rax
    jmp .amr_inner
.amr_next:
    dec r10
    jmp .amr_outer

.amr_hit:
    mov rax, r11
    ret
.amr_empty:
    lea rax, [rdi + r8]
    ret
.amr_none:
    xor eax, eax
    ret
END_FUNC ap_memrfind

;; ============================================================================
;; Allocation
;; (was src/memory.asm)
;; ============================================================================

section .text

; Wraps libc malloc/free/realloc with error checking

extern malloc
extern free
extern realloc
; ap_malloc(size_t size) -> void*
; Allocates memory, fatal error on failure
DEF_FUNC ap_malloc
    push rbx
    mov rbx, rdi            ; save size
    call malloc wrt ..plt
    test rax, rax
    jz .oom
    pop rbx
    leave
    ret
.oom:
    lea rdi, [rel mem_oom_msg]
    call fatal_error        ; never returns
END_FUNC ap_malloc

; ap_free(void *ptr)
; Frees memory; NULL-safe
DEF_FUNC_BARE ap_free
    test rdi, rdi
    jz .null
    jmp free wrt ..plt
.null:
    ret
END_FUNC ap_free

; ap_realloc(void *ptr, size_t size) -> void*
; Reallocates memory, fatal error on failure
DEF_FUNC ap_realloc
    push rbx
    mov rbx, rsi            ; save size for error case
    call realloc wrt ..plt
    test rax, rax
    jz .oom
    pop rbx
    leave
    ret
.oom:
    lea rdi, [rel mem_oom_msg]
    call fatal_error        ; never returns
END_FUNC ap_realloc

section .rodata
mem_oom_msg: db "Fatal: out of memory", 0

;; ============================================================================
;; Dying without an interpreter
;; (was src/error.asm)
;; ============================================================================

section .text

; Uses raw Linux syscalls instead of libc stdio

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


; error_unimplemented_opcode(int opcode)
; Reports unimplemented bytecode opcode and exits
EUO_END   equ 1              ; one past the digits: holds the newline
DEF_FUNC error_unimplemented_opcode, 32             ; space for decimal digits

    mov eax, edi            ; opcode value

    ; Convert opcode int to decimal string on stack
    lea rdi, [rbp - EUO_END]     ; write digits right-to-left
    mov byte [rdi], 10      ; trailing newline
    lea rcx, [rbp - EUO_END]     ; rcx = end (points at newline)
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

; list_sorting_error - raise ValueError when list is mutated during sort
; Called when ob_item == NULL (list is being sorted)
; Does not return - jumps to exception unwinder
DEF_FUNC_BARE list_sorting_error
    extern exc_ValueError_type
    lea rdi, [rel exc_ValueError_type]
    lea rsi, [rel list_modified_msg]
    extern raise_exception
    jmp raise_exception        ; tail call, doesn't return
END_FUNC list_sorting_error

section .rodata
err_prefix: db "Error: "
err_newline: db 10
err_op_prefix: db "Error: unimplemented opcode "
err_op_prefix_len equ $ - err_op_prefix
list_modified_msg: db "list modified during sort", 0
