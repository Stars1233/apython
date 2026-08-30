; srcfile.asm - Load a code object from a path, compiling it if it is source
;
; The one entry point the rest of the interpreter uses to turn a filename into
; a code object.  `./apython foo.py` and `import foo` both come through here,
; and both used to call pyc_read_file directly; the choice between reading
; marshal and running the compiler is made in one place so the two cannot
; drift.
;
; There is deliberately no .pyc writer.  Compiling is fast enough to do every
; time, and a cache would need mtime comparison, atomic replacement and a
; marshal writer -- none of which exist -- to be anything but a source of
; stale-bytecode bugs.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "compiler.inc"

extern ap_free
extern ap_malloc
extern ap_strlen
extern compile_source
extern obj_decref
extern pyc_read_file
extern str_from_cstr_heap
extern sys_close
extern sys_fstat
extern sys_open
extern sys_read

global code_from_path
global path_is_source
global src_read_file

STAT_SIZE    equ 144
STAT_ST_SIZE equ 48

section .text

;; ============================================================================
;; path_is_source(const char *path) -> rax = 1 if it ends in ".py", else 0
;; ".pyc" ends in "yc", so the test is exact rather than a prefix match.
;; ============================================================================
DEF_FUNC_BARE path_is_source
    push rbx
    mov rbx, rdi
    call ap_strlen
    xor ecx, ecx
    cmp rax, 3
    jb .done
    cmp byte [rbx + rax - 3], '.'
    jne .done
    cmp byte [rbx + rax - 2], 'p'
    jne .done
    cmp byte [rbx + rax - 1], 'y'
    jne .done
    mov ecx, 1
.done:
    mov eax, ecx
    pop rbx
    ret
END_FUNC path_is_source

;; ============================================================================
;; src_read_file(const char *path, int64_t *out_len) -> rax = buffer, or 0
;; The whole file in one ap_malloc'd block, NUL-terminated.  The caller frees.
;; ============================================================================
SR_PATH  equ 8
SR_OUT   equ 16
SR_FD    equ 24
SR_SIZE  equ 32
SR_BUF   equ 40
SR_GOT   equ 48
SR_STAT  equ 56 + STAT_SIZE
SR_FRAME equ ((SR_STAT + 15) / 16) * 16 + 8      ; + 1 push = 16-aligned
DEF_FUNC src_read_file, SR_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - SR_OUT], rsi

    mov rdi, rbx
    xor esi, esi                        ; O_RDONLY
    xor edx, edx
    call sys_open
    test rax, rax
    js .fail
    mov [rbp - SR_FD], rax

    mov rdi, rax
    lea rsi, [rbp - SR_STAT]
    call sys_fstat
    test rax, rax
    js .close_fail
    mov rax, [rbp - SR_STAT + STAT_ST_SIZE]
    mov [rbp - SR_SIZE], rax

    ; One extra byte for the NUL, so an empty file still gets a valid buffer.
    lea rdi, [rax + 1]
    call ap_malloc
    test rax, rax
    jz .close_fail
    mov [rbp - SR_BUF], rax
    mov qword [rbp - SR_GOT], 0

.read_loop:
    mov rax, [rbp - SR_GOT]
    cmp rax, [rbp - SR_SIZE]
    jae .read_done
    mov rdi, [rbp - SR_FD]
    mov rsi, [rbp - SR_BUF]
    add rsi, rax
    mov rdx, [rbp - SR_SIZE]
    sub rdx, rax
    call sys_read
    test rax, rax
    jle .read_short
    add [rbp - SR_GOT], rax
    jmp .read_loop
.read_short:
    ; A short read is the real length: /proc and other synthetic files report
    ; a size of 0 from fstat and still have contents, and a file that shrank
    ; between the fstat and the read is not an error either.
    jmp .read_done

.read_done:
    mov rdi, [rbp - SR_FD]
    call sys_close
    mov rax, [rbp - SR_BUF]
    mov rcx, [rbp - SR_GOT]
    mov byte [rax + rcx], 0
    mov rdx, [rbp - SR_OUT]
    test rdx, rdx
    jz .no_out
    mov [rdx], rcx
.no_out:
    pop rbx
    leave
    ret

.close_fail:
    mov rdi, [rbp - SR_FD]
    call sys_close
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC src_read_file

;; ============================================================================
;; code_from_path(const char *path) -> rax = PyCodeObject*, or 0
;; A .py is compiled; anything else is read as marshalled bytecode.  A failed
;; compile leaves its exception pending, exactly as pyc_read_file's callers
;; already expect.
;; ============================================================================
CP_PATH  equ 8
CP_SRC   equ 16
CP_LEN   equ 24
CP_FILE  equ 32
CP_FRAME equ 40           ; + 1 push = 48
DEF_FUNC code_from_path, CP_FRAME
    push rbx
    mov rbx, rdi
    call path_is_source
    test eax, eax
    jnz .source
    mov rdi, rbx
    call pyc_read_file
    pop rbx
    leave
    ret

.source:
    mov rdi, rbx
    lea rsi, [rbp - CP_LEN]
    call src_read_file
    test rax, rax
    jz .fail
    mov [rbp - CP_SRC], rax

    mov rdi, rbx
    call str_from_cstr_heap
    test rax, rax
    jz .free_src
    mov [rbp - CP_FILE], rax

    mov rdi, [rbp - CP_SRC]
    mov rsi, [rbp - CP_LEN]
    mov rdx, rax
    mov ecx, CMODE_EXEC
    call compile_source
    mov rbx, rax
    mov rdi, [rbp - CP_FILE]
    call obj_decref
    mov rdi, [rbp - CP_SRC]
    call ap_free
    mov rax, rbx
    pop rbx
    leave
    ret

.free_src:
    mov rdi, [rbp - CP_SRC]
    call ap_free
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC code_from_path

ASM_INIT
