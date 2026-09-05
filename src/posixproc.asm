; posixproc.asm - the process primitives: fork, exec, wait and signal
;
; fork, execv, _exit, kill, setsid and PEP 3143's fork hooks.  They came out
; of posixmod.asm, which crossed the size cap when they arrived, and they are
; a seam of their own: everything else in that file is a question about a
; FILE, and these are the only ones that make a second process.
;
; `_posixsubprocess.fork_exec` is written on top of them in lib/, which is
; where the argument marshalling and the error pipe belong -- CPython's is C
; only because it must run between fork and exec, where almost nothing is
; safe to call, and the same is true of a Python function that allocates
; nothing new.

%include "macros.inc"
%include "object.inc"
%include "value.inc"

extern sys_fork
extern sys_execve
extern sys_exit_now
extern sys_kill
extern sys_setsid
extern sys_close_range
extern posix_int_arg
extern posix_path_arg
extern posix_raise_missing
extern raise_oserror
extern none_singleton
extern list_new
extern list_append
extern list_type
extern dict_type
extern tuple_type
extern str_type
extern ap_strcmp
extern environ
extern kw_names_pending
extern exc_TypeError_type
extern exc_ValueError_type
extern raise_exception
extern eval_exception_unwind

global posix_fork
global posix_execv
global posix_exit_now
global posix_kill
global posix_setsid
global posix_close_range
global posix_register_at_fork
global pm_atfork_before
global pm_atfork_parent
global pm_atfork_child

;; POSIX_CHECK result, filename -- turn a negative errno into an OSError.
;; The same macro posixmod.asm uses; it is small and duplicating it is
;; cheaper than a header that only these two files would include.
%macro POSIX_CHECK 2            ; %1 = the result register, %2 = filename Value
    cmp %1, -4095
    jb %%ok
    mov rdi, %1
    neg rdi
    mov rsi, %2
    call raise_oserror          ; does not return
%%ok:
%endmacro

;; PM_MISSING func, arg, pos -- the three-argument form of the arity error.
%macro PM_MISSING 3
    CSTRING rdi, %1
    CSTRING rsi, %2
    mov edx, %3
    call posix_raise_missing
%endmacro

section .bss
; The PEP 3143 fork hooks, one list each.  They are module state rather than
; dict entries because posix_fork walks them without a name lookup.
pm_atfork_before: resq 1
pm_atfork_parent: resq 1
pm_atfork_child:  resq 1

section .text

;; ============================================================================
;; posix.fork() -> 0 in the child, the child's pid in the parent
;;
;; The four primitives a subprocess needs, and nothing more: fork, execv,
;; _exit and kill.  `_posixsubprocess.fork_exec` is written on top of them in
;; lib/, which is where the argument marshalling and the error pipe belong --
;; CPython's is C only because it must run between fork and exec, where
;; almost nothing is safe to call, and the same is true of a Python function
;; that allocates nothing new.
;;
;; PEP 3143's hooks run around it.  CPython registers its own in C so that
;; its locks are reacquired in the child; the only caller here is random.py,
;; which reseeds so a forked child does not repeat the parent's stream.
;; ============================================================================
PFK_PID   equ 8
PFK_FRAME equ 16            ; + 1 push = 24... one more word to land right
DEF_FUNC posix_fork, 24                 ; + 1 push = 32, 16-aligned
    push rbx
    ; `before` runs in reverse registration order, as CPython's does.
    mov rdi, [rel pm_atfork_before]
    mov esi, 1
    call posix_run_fork_hooks

    call sys_fork
    mov [rbp - PFK_PID], rax
    cmp rax, 0
    jl .pfk_failed

    mov rdi, [rel pm_atfork_parent]
    test rax, rax
    jnz .pfk_have_list
    mov rdi, [rel pm_atfork_child]
.pfk_have_list:
    xor esi, esi
    call posix_run_fork_hooks

    mov rax, [rbp - PFK_PID]
    mov edx, TAG_SMALLINT
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.pfk_failed:
    POSIX_CHECK rax, 0
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
END_FUNC posix_fork

;; ============================================================================
;; posix_run_fork_hooks(rdi = a list or 0, esi = 1 to walk it backwards)
;;   -> nothing; a hook that raises leaves its exception pending, as
;;      CPython's does
;; ============================================================================
PRH_LIST  equ 8
PRH_I     equ 16
PRH_REV   equ 24
PRH_FRAME equ 32            ; + 1 push = 40... one more word to land right
DEF_FUNC_LOCAL posix_run_fork_hooks, 40 ; + 1 push = 48, 16-aligned
    push rbx
    mov rbx, rdi
    mov [rbp - PRH_LIST], rdi
    mov [rbp - PRH_REV], rsi
    test rdi, rdi
    jz .prh_done
    mov rax, [rdi + PyListObject.ob_size]
    test rax, rax
    jz .prh_done
    mov qword [rbp - PRH_I], 0
.prh_loop:
    mov rcx, [rbp - PRH_I]
    mov rbx, [rbp - PRH_LIST]
    cmp rcx, [rbx + PyListObject.ob_size]
    jge .prh_done
    cmp qword [rbp - PRH_REV], 0
    je .prh_forward
    mov rax, [rbx + PyListObject.ob_size]
    sub rax, rcx
    dec rax
    mov rcx, rax
.prh_forward:
    mov rax, [rbx + PyListObject.ob_item]
    mov rdi, [rax + rcx*8]
    V_TEST_PTR rdi, rax
    ja .prh_next
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .prh_next
    xor esi, esi
    xor edx, edx
    call rax
.prh_next:
    inc qword [rbp - PRH_I]
    jmp .prh_loop
.prh_done:
    pop rbx
    leave
    ret
END_FUNC posix_run_fork_hooks

;; ============================================================================
;; posix.register_at_fork(*, before=None, after_in_parent=None,
;;                        after_in_child=None) -> None
;;
;; Keyword-only, as CPython's is, and at least one of the three is required.
;; ============================================================================
PRF_I     equ 8
PRF_NAMES equ 16
PRF_FRAME equ 32            ; + 2 pushes = 48, 16-aligned
DEF_FUNC posix_register_at_fork, PRF_FRAME
    push rbx
    push r12
    ; Everything arrives as a keyword: the positional count must be zero, and
    ; kw_names_pending names each one.
    extern kw_names_pending
    mov rbx, rdi
    mov r12, [rel kw_names_pending]
    mov qword [rel kw_names_pending], 0
    test r12, r12
    jz .prf_none
    cmp rsi, [r12 + PyTupleObject.ob_size]
    jne .prf_positional

    mov qword [rbp - PRF_I], 0
.prf_loop:
    mov rcx, [rbp - PRF_I]
    cmp rcx, [r12 + PyTupleObject.ob_size]
    jge .prf_ok
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rdi, [rax + rcx*8]              ; the keyword's name
    add rdi, PyStrObject.data
    CSTRING rsi, "before"
    call ap_strcmp
    mov rcx, [rbp - PRF_I]
    test eax, eax
    jz .prf_before

    mov rax, [r12 + PyTupleObject.ob_item]
    mov rdi, [rax + rcx*8]
    add rdi, PyStrObject.data
    CSTRING rsi, "after_in_parent"
    call ap_strcmp
    mov rcx, [rbp - PRF_I]
    test eax, eax
    jz .prf_parent

    mov rax, [r12 + PyTupleObject.ob_item]
    mov rdi, [rax + rcx*8]
    add rdi, PyStrObject.data
    CSTRING rsi, "after_in_child"
    call ap_strcmp
    mov rcx, [rbp - PRF_I]
    test eax, eax
    jnz .prf_unexpected
    mov rdi, [rel pm_atfork_child]
    jmp .prf_append
.prf_before:
    mov rdi, [rel pm_atfork_before]
    jmp .prf_append
.prf_parent:
    mov rdi, [rel pm_atfork_parent]
.prf_append:
    mov rsi, [rbx + rcx*8]              ; the callable
    lea rax, [rel none_singleton]
    cmp rsi, rax
    je .prf_next
    extern list_append
    call list_append
.prf_next:
    inc qword [rbp - PRF_I]
    jmp .prf_loop

.prf_ok:
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.prf_none:
    RAISE exc_TypeError_type, "At least one argument is required."
.prf_positional:
    RAISE exc_TypeError_type, \
        "register_at_fork() takes no positional arguments"
.prf_unexpected:
    RAISE exc_TypeError_type, \
        "register_at_fork() got an unexpected keyword argument"
END_FUNC posix_register_at_fork


;; ============================================================================
;; posix._exit(code) -> does not return
;;
;; The bare exit: no flush, no atexit handlers.  A forked child that failed to
;; exec has to call this rather than unwind, or it would run the parent's
;; cleanup a second time -- including flushing buffers the parent still owns.
;; ============================================================================
DEF_FUNC posix_exit_now
    test rsi, rsi
    jz .pen_zero
    mov rdi, [rdi]
    call posix_int_arg
    mov rdi, rax
    jmp .pen_go
.pen_zero:
    xor edi, edi
.pen_go:
    and edi, 0xff
    call sys_exit_now
    ud2
END_FUNC posix_exit_now


;; ============================================================================
;; posix.kill(pid, sig) -> None
;; ============================================================================
DEF_FUNC posix_kill, 16
    cmp rsi, 2
    jl .pk_argerr
    push rbx
    mov rbx, rdi
    mov rdi, [rbx]
    call posix_int_arg
    push rax
    push rax
    mov rdi, [rbx + 8]
    call posix_int_arg
    mov rsi, rax
    pop rdi
    pop rdi
    call sys_kill
    POSIX_CHECK rax, 0
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.pk_argerr:
    PM_MISSING "kill", "sig", 2
END_FUNC posix_kill


;; ============================================================================
;; posix.closerange(fd_low, fd_high) -> None
;;
;; Shuts every descriptor in [fd_low, fd_high), which is CPython's half-open
;; span -- the close_range syscall's is inclusive, so the top is stepped down
;; by one.  A forked child cannot safely walk /proc to find what it inherited,
;; and `subprocess` asks for close_fds by DEFAULT, so without this every open
;; file, socket and pipe in the parent leaks into the child.
;;
;; Errors are swallowed, as CPython's is: closing a descriptor that was never
;; open is the ordinary case, not a failure.
;; ============================================================================
DEF_FUNC posix_close_range, 16
    cmp rsi, 2
    jl .pcr_argerr
    push rbx
    mov rbx, rdi
    mov rdi, [rbx]
    call posix_int_arg
    push rax
    push rax
    mov rdi, [rbx + 8]
    call posix_int_arg
    mov rsi, rax
    pop rdi
    pop rdi
    dec rsi                             ; CPython's top is exclusive
    cmp rsi, rdi
    jl .pcr_empty
    xor edx, edx
    call sys_close_range
.pcr_empty:
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.pcr_argerr:
    PM_MISSING "closerange", "fd_high", 2
END_FUNC posix_close_range

;; ============================================================================
;; posix.setsid() -> the new session id
;; ============================================================================
DEF_FUNC posix_setsid
    call sys_setsid
    POSIX_CHECK rax, 0
    mov edx, TAG_SMALLINT
    leave
    V_PACK rax, rdx
    ret
END_FUNC posix_setsid

;; ============================================================================
;; posix.execv(path, args) -> does not return on success
;; posix.execve(path, args, env) -> likewise, with the environment given
;;
;; The argv array is built on the machine stack rather than the heap: this
;; runs in a freshly forked child, where the allocator's locks belong to a
;; thread that no longer exists.  There is one thread here, so that is
;; theatre -- but the array has to outlive nothing, and the stack is where it
;; naturally goes.
;;
;; Without a third argument the child inherits this process's environment,
;; which glibc already keeps as the NULL-terminated array execve wants.  With
;; one it gets exactly that list and nothing else -- which is the whole point
;; of `subprocess.run(env=...)`, and dropping it would hand the child every
;; variable the caller was trying to keep from it.
;; ============================================================================
PXV_PATH  equ 8
PXV_ARGS  equ 16
PXV_N     equ 24
PXV_NARGS equ 40
PXV_MAX   equ 256
; The vector grows UPWARD from its base, so its slot is the DEEPEST one --
; naming it 32 put the first pointer over PXV_ARGS and the second over
; PXV_PATH.
; Two vectors: argv, then envp above it.  Each grows UPWARD from its base, so
; each slot names the DEEPEST word -- naming argv's 32 put its first pointer
; over PXV_ARGS and its second over PXV_PATH.
PXV_VEC   equ 48 + (PXV_MAX + 1) * 8
PXV_ENV   equ PXV_VEC + (PXV_MAX + 1) * 8
PXV_FRAME equ ((PXV_ENV + 15) / 16) * 16
DEF_FUNC posix_execv, PXV_FRAME
    push rbx
    cmp rsi, 2
    jl .pxv_argerr
    mov [rbp - PXV_NARGS], rsi
    mov rbx, rdi

    mov rdi, [rbx]
    xor esi, esi
    mov edx, -1                         ; every kind posix_path_arg takes
    call posix_path_arg
    test rax, rax
    jz .pxv_fail
    mov [rbp - PXV_PATH], rax

    mov rdi, [rbx + 8]
    lea rsi, [rbp - PXV_VEC]
    call pxv_string_vector
    test rax, rax
    jz .pxv_fail

    ; The environment: the caller's when there is one, this process's when
    ; there is not.
    mov rdx, [rel environ]
    cmp qword [rbp - PXV_NARGS], 3
    jl .pxv_have_env
    mov rdi, [rbx + 16]
    lea rsi, [rbp - PXV_ENV]
    call pxv_env_vector
    test rax, rax
    jz .pxv_fail
    lea rdx, [rbp - PXV_ENV]
.pxv_have_env:
    mov rdi, [rbp - PXV_PATH]
    lea rsi, [rbp - PXV_VEC]
    call sys_execve
    ; Only ever reached on failure.
    POSIX_CHECK rax, 0
.pxv_fail:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
.pxv_seqerr:
    RAISE exc_TypeError_type, "execv() arg 2 must be a sequence of strings"
.pxv_toomany:
    RAISE exc_ValueError_type, "execv() arg 2 is too long"
.pxv_argerr:
    PM_MISSING "execv", "args", 2
END_FUNC posix_execv

;; ============================================================================
;; pxv_string_vector(rdi = a list or tuple of str, rsi = where to build it)
;;   -> rax = 1, or 0 with a TypeError or ValueError pending
;;
;; A NULL-terminated char*[] pointing INTO the strings, which is what execve
;; takes.  Nothing is copied: the strings outlive the call, because the call
;; either replaces the process or fails.
;; ============================================================================
PSV_SEQ   equ 8
PSV_OUT   equ 16
PSV_N     equ 24
PSV_FRAME equ 32            ; + 1 push = 40... one word more to land right
DEF_FUNC_LOCAL pxv_string_vector, 40    ; + 1 push = 48, 16-aligned
    push rbx
    mov [rbp - PSV_OUT], rsi
    mov rax, rdi
    V_TEST_PTR rax, rcx
    ja .psv_seqerr
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel list_type]
    cmp rcx, rdx
    je .psv_have_seq
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    jne .psv_seqerr
.psv_have_seq:
    mov [rbp - PSV_SEQ], rax
    mov rcx, [rax + PyTupleObject.ob_size]  ; list and tuple agree here
    cmp rcx, PXV_MAX
    jae .psv_toomany
    mov [rbp - PSV_N], rcx

    xor r10d, r10d
.psv_loop:
    cmp r10, [rbp - PSV_N]
    jge .psv_done
    mov rax, [rbp - PSV_SEQ]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rdi, [rax + r10*8]
    V_TEST_PTR rdi, rcx
    ja .psv_seqerr
    mov rcx, [rdi + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .psv_seqerr
    add rdi, PyStrObject.data
    mov rax, [rbp - PSV_OUT]
    mov [rax + r10*8], rdi
    inc r10
    jmp .psv_loop
.psv_done:
    mov rax, [rbp - PSV_OUT]
    mov qword [rax + r10*8], 0
    mov eax, 1
    pop rbx
    leave
    ret
.psv_seqerr:
    RAISE exc_TypeError_type, "execv() takes a sequence of strings"
.psv_toomany:
    RAISE exc_ValueError_type, "execv() was given too many strings"
END_FUNC pxv_string_vector

;; ============================================================================
;; pxv_env_vector(rdi = a mapping or a sequence of "K=V" strings,
;;                rsi = where to build it) -> rax = 1, or 0 with an exception
;;
;; CPython's execve takes a MAPPING, and the strings it hands the kernel are
;; "key=value" pairs it builds itself.  _posixsubprocess already has that list
;; form -- subprocess.py assembles it -- so both are accepted, and a dict is
;; flattened into a scratch buffer here.
;;
;; The buffer is fixed because this may run in a freshly forked child, where
;; allocating is the one thing a program is told not to do.  Overflowing it is
;; a refusal rather than a truncation: a child that ran with HALF an
;; environment is worse than one that did not run.
;; ============================================================================
PEV_MAP   equ 8
PEV_OUT   equ 16
PEV_N     equ 24
PEV_POS   equ 32
PEV_I     equ 40
PEV_FRAME equ 64            ; + 2 pushes = 80, 16-aligned
DEF_FUNC_LOCAL pxv_env_vector, PEV_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov [rbp - PEV_MAP], rdi
    mov [rbp - PEV_OUT], rsi

    V_TEST_PTR rbx, rax
    ja .pev_bad
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel dict_type]
    cmp rax, rcx
    jne .pev_sequence

    mov qword [rbp - PEV_POS], 0
    mov qword [rbp - PEV_I], 0
    mov rax, [rbx + PyDictObject.ob_size]
    cmp rax, PXV_MAX
    jae .pev_toomany
    xor r12d, r12d                      ; how many pointers are written
.pev_loop:
    mov rcx, [rbp - PEV_I]
    cmp rcx, [rbx + PyDictObject.capacity]
    jge .pev_done
    mov rax, [rbx + PyDictObject.entries]
    imul rdx, rcx, DictEntry_size
    mov rdi, [rax + rdx + DictEntry.key]
    test rdi, rdi
    jz .pev_next                        ; empty, or a tombstone
    mov rsi, [rax + rdx + DictEntry.value]

    V_TEST_PTR rdi, rcx
    ja .pev_bad
    mov rcx, [rdi + PyObject.ob_type]
    lea rax, [rel str_type]
    cmp rcx, rax
    jne .pev_bad
    V_TEST_PTR rsi, rcx
    ja .pev_bad
    mov rcx, [rsi + PyObject.ob_type]
    cmp rcx, rax
    jne .pev_bad

    ; This entry's text starts where the buffer has got to.
    mov rax, [rbp - PEV_OUT]
    mov rcx, [rbp - PEV_POS]
    lea rdx, [rel pxv_envbuf]
    add rdx, rcx
    mov [rax + r12*8], rdx
    inc r12

    push rsi
    add rdi, PyStrObject.data
    call pev_append
    pop rsi
    test eax, eax
    jz .pev_full
    mov rcx, [rbp - PEV_POS]
    lea rdx, [rel pxv_envbuf]
    mov byte [rdx + rcx], '='
    inc qword [rbp - PEV_POS]
    mov rdi, rsi
    add rdi, PyStrObject.data
    call pev_append
    test eax, eax
    jz .pev_full
    mov rcx, [rbp - PEV_POS]
    lea rdx, [rel pxv_envbuf]
    mov byte [rdx + rcx], 0
    inc qword [rbp - PEV_POS]
.pev_next:
    inc qword [rbp - PEV_I]
    jmp .pev_loop
.pev_done:
    mov rax, [rbp - PEV_OUT]
    mov qword [rax + r12*8], 0
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret

.pev_sequence:
    mov rdi, rbx
    mov rsi, [rbp - PEV_OUT]
    call pxv_string_vector
    pop r12
    pop rbx
    leave
    ret

.pev_bad:
    RAISE exc_TypeError_type, \
        "execve() arg 3 must be a mapping of str to str"
.pev_toomany:
.pev_full:
    RAISE exc_ValueError_type, "execve() environment is too large"

; Local: append a NUL-terminated string to the scratch buffer, advancing
; PEV_POS in this frame.  eax = 0 when it would not fit.
.pev_append_body:
pev_append:
    mov rcx, [rbp - PEV_POS]
    lea rdx, [rel pxv_envbuf]
.pea_loop:
    cmp rcx, PXV_ENVBUF - 2
    jae .pea_full
    movzx eax, byte [rdi]
    test al, al
    jz .pea_done
    mov [rdx + rcx], al
    inc rcx
    inc rdi
    jmp .pea_loop
.pea_done:
    mov [rbp - PEV_POS], rcx
    mov eax, 1
    ret
.pea_full:
    xor eax, eax
    ret
END_FUNC pxv_env_vector

PXV_ENVBUF equ 65536

section .bss
; The environment a dict is flattened into.  Fixed, because this may run in a
; freshly forked child where allocating is the one thing not to do.
pxv_envbuf: resb PXV_ENVBUF
