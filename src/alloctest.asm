; alloctest.asm - Self-test for the pool allocator
;
; Reachable via `./apython --selftest-alloc`.
;
; An allocator bug does not present as an allocator bug.  It presents as a
; string that reads back empty, a refcount that was decremented through the
; wrong object, or a crash three operations later in code that is correct --
; and the interpreter is too large a haystack to find it in.  So this runs
; against ap_malloc, ap_free and ap_realloc directly, with no objects, no
; eval loop and no allocation it did not make itself.
;
; What it asserts, and why each one is here:
;
;   1. A block is never NULL and is always 16-byte aligned.  gc_alloc's
;      sixteen-byte head is added to whatever comes back, so an 8-aligned
;      block would give every GC-tracked object an 8-aligned address.
;   2. ap_block_size is at least what was asked for, and less than a size
;      class more.  That is the carve arithmetic, checked from outside.
;   3. ap_block_size does not change while a block is alive.  A pool never
;      changes size class, and ap_realloc's "does it still fit" test depends
;      on that being true.
;   4. Every live block still holds the pattern written into it.  This is the
;      one that catches TWO BLOCKS OVERLAPPING, which is the failure that
;      would otherwise reach the interpreter as data corruption.
;   5. ap_realloc preserves min(old, new) bytes, and hands back the SAME
;      pointer whenever the new size still fits the block it has.
;
; alloc_selftest() -> rax = 0 on success, else a nonzero case id of the form
;                     (group_number * 1000 + slot + 1).

%include "macros.inc"
%include "object.inc"

extern sys_write
extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_block_size

;; The slot table.  One live block each, filled with a pattern derived from
;; the slot number, so that two live blocks sharing a byte is detectable.
AT_SLOTS    equ 512
AT_OPS      equ 400000
AT_MAXSIZE  equ 700         ; straddles the 512-byte small-object threshold

section .text

;; ============================================================================
;; at_rand() -> rax = the next value of a 64-bit xorshift
;;
;; Fixed seed, so a failure is reproducible from the case id alone.
;; ============================================================================
DEF_FUNC_BARE at_rand
    mov rax, [rel at_state]
    mov rcx, rax
    shl rcx, 13
    xor rax, rcx
    mov rcx, rax
    shr rcx, 7
    xor rax, rcx
    mov rcx, rax
    shl rcx, 17
    xor rax, rcx
    mov [rel at_state], rax
    ret
END_FUNC at_rand

;; ============================================================================
;; at_fill(rdi = ptr, rsi = nbytes, rdx = slot) -> void
;; Write the slot's pattern over the whole block.
;; ============================================================================
DEF_FUNC_BARE at_fill
    xor ecx, ecx
.af_loop:
    cmp rcx, rsi
    jae .af_done
    lea eax, [rdx + rcx]
    mov [rdi + rcx], al
    inc rcx
    jmp .af_loop
.af_done:
    ret
END_FUNC at_fill

;; ============================================================================
;; at_check(rdi = ptr, rsi = nbytes, rdx = slot) -> rax = 1 if intact, 0 if not
;; ============================================================================
DEF_FUNC_BARE at_check
    xor ecx, ecx
.ac_loop:
    cmp rcx, rsi
    jae .ac_ok
    lea eax, [rdx + rcx]
    cmp [rdi + rcx], al
    jne .ac_bad
    inc rcx
    jmp .ac_loop
.ac_ok:
    mov eax, 1
    ret
.ac_bad:
    xor eax, eax
    ret
END_FUNC at_check

;; ============================================================================
;; alloc_selftest() -> rax = 0, or group*1000 + slot + 1
;; ============================================================================
AT_SLOT   equ 8             ; the slot this operation is working on
AT_SIZE   equ 16            ; the size it asked for
AT_PTR    equ 24            ; the block
AT_BSIZE  equ 32            ; ap_block_size of it, when it was handed over
AT_I      equ 40            ; the operation counter
AT_FRAME  equ 48            ; + 4 pushes = 80, 16-aligned
global alloc_selftest
DEF_FUNC alloc_selftest, AT_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov rax, 0x243F6A8885A308D3
    mov [rel at_state], rax
    mov qword [rbp - AT_SLOT], 0

    ; --- the boundary sizes, all live at once ------------------------------
    ; 0 and 1 either side of the smallest class, the 15/16/17 and 511/512/513
    ; boundaries, and one over the threshold that must come from libc.
    xor r12d, r12d
.at_bl_loop:
    cmp r12d, at_bounds_n
    jae .at_bl_done
    mov [rbp - AT_SLOT], r12    ; so a failure here names the size it was on
    lea rax, [rel at_bounds]
    mov r13, [rax + r12*8]      ; the size
    mov rdi, r13
    call ap_malloc
    test rax, rax
    jz .at_fail_1
    test al, 15
    jnz .at_fail_2
    mov r14, rax
    lea rax, [rel at_bslots]
    mov [rax + r12*8], r14
    mov rdi, r14
    mov rsi, r13
    mov rdx, r12
    call at_fill
    inc r12d
    jmp .at_bl_loop
.at_bl_done:
    ; each of them still holds its own pattern, so none overlapped another
    xor r12d, r12d
.at_bv_loop:
    cmp r12d, at_bounds_n
    jae .at_bv_done
    mov [rbp - AT_SLOT], r12
    lea rax, [rel at_bounds]
    mov r13, [rax + r12*8]
    lea rax, [rel at_bslots]
    mov rdi, [rax + r12*8]
    mov rsi, r13
    mov rdx, r12
    call at_check
    test eax, eax
    jz .at_fail_4
    lea rax, [rel at_bslots]
    mov rdi, [rax + r12*8]
    call ap_free
    inc r12d
    jmp .at_bv_loop
.at_bv_done:

    ; --- the churn ---------------------------------------------------------
    mov qword [rbp - AT_I], 0
.at_loop:
    mov rax, [rbp - AT_I]
    cmp rax, AT_OPS
    jae .at_drain
    inc qword [rbp - AT_I]

    call at_rand
    mov rbx, rax
    mov rdx, rax
    shr rdx, 8
    and edx, AT_SLOTS - 1
    mov [rbp - AT_SLOT], rdx    ; slot

    lea rax, [rel at_ptrs]
    mov r12, [rax + rdx*8]      ; the block there, or 0
    lea rax, [rel at_sizes]
    mov r13, [rax + rdx*8]      ; and its size

    test r12, r12
    jz .at_alloc

    ; It is live: verify it, then free, realloc or leave it alone.
    mov rdi, r12
    mov rsi, r13
    mov rdx, [rbp - AT_SLOT]
    call at_check
    test eax, eax
    jz .at_fail_4

    ; ap_block_size must be the same answer it was at handout
    mov rdi, r12
    call ap_block_size
    lea rcx, [rel at_bsizes]
    mov rdx, [rbp - AT_SLOT]
    cmp rax, [rcx + rdx*8]
    jne .at_fail_3

    mov rax, rbx
    shr rax, 32
    and eax, 3
    cmp eax, 1
    jb .at_free
    cmp eax, 2
    jb .at_realloc
    jmp .at_loop                ; leave it; the verify above was the point

.at_free:
    mov rdi, r12
    call ap_free
    lea rax, [rel at_ptrs]
    mov rdx, [rbp - AT_SLOT]
    mov qword [rax + rdx*8], 0
    jmp .at_loop

.at_realloc:
    ; A new size either side of the old one, then check that what fitted in
    ; both is still there.
    mov rax, rbx
    shr rax, 40
    xor edx, edx
    mov ecx, AT_MAXSIZE
    div rcx
    lea r14, [rdx + 1]          ; the new size
    mov [rbp - AT_SIZE], r14

    mov rdi, r12
    call ap_block_size
    mov [rbp - AT_BSIZE], rax   ; 0 when the block came from libc

    mov rdi, r12
    mov rsi, r14
    call ap_realloc
    test rax, rax
    jz .at_fail_1
    test al, 15
    jnz .at_fail_2
    mov [rbp - AT_PTR], rax

    ; Same pointer whenever the new size still fits the block it had.  Only
    ; checked for OUR blocks; libc's realloc makes no such promise.
    mov rcx, [rbp - AT_BSIZE]
    test rcx, rcx
    jz .at_rl_content
    cmp r14, rcx
    ja .at_rl_content
    cmp rax, r12
    jne .at_fail_5

.at_rl_content:
    ; min(old, new) bytes survived
    mov rsi, r13
    cmp rsi, r14
    jbe .at_rl_min
    mov rsi, r14
.at_rl_min:
    mov rdi, [rbp - AT_PTR]
    mov rdx, [rbp - AT_SLOT]
    call at_check
    test eax, eax
    jz .at_fail_5

    ; refill at the new size and record it
    mov rdi, [rbp - AT_PTR]
    mov rsi, r14
    mov rdx, [rbp - AT_SLOT]
    call at_fill
    mov rdx, [rbp - AT_SLOT]
    lea rax, [rel at_ptrs]
    mov rcx, [rbp - AT_PTR]
    mov [rax + rdx*8], rcx
    lea rax, [rel at_sizes]
    mov [rax + rdx*8], r14
    mov rdi, rcx
    call ap_block_size
    mov rdx, [rbp - AT_SLOT]
    lea rcx, [rel at_bsizes]
    mov [rcx + rdx*8], rax
    jmp .at_loop

.at_alloc:
    mov rax, rbx
    shr rax, 40
    xor edx, edx
    mov ecx, AT_MAXSIZE
    div rcx
    lea r14, [rdx + 1]          ; 1..AT_MAXSIZE
    mov rdi, r14
    call ap_malloc
    test rax, rax
    jz .at_fail_1
    test al, 15
    jnz .at_fail_2
    mov r12, rax

    ; The block is at least the request, and less than a class more.
    mov rdi, r12
    call ap_block_size
    test rax, rax
    jz .at_al_record            ; a libc block; it does not answer
    cmp rax, r14
    jb .at_fail_3
    sub rax, r14
    cmp rax, 16
    jae .at_fail_3
    mov rdi, r12
    call ap_block_size
.at_al_record:
    mov rdx, [rbp - AT_SLOT]
    lea rcx, [rel at_bsizes]
    mov [rcx + rdx*8], rax
    lea rax, [rel at_ptrs]
    mov [rax + rdx*8], r12
    lea rax, [rel at_sizes]
    mov [rax + rdx*8], r14
    mov rdi, r12
    mov rsi, r14
    call at_fill
    jmp .at_loop

.at_drain:
    ; Everything still live holds its pattern, and everything frees.
    xor r12d, r12d
.at_dr_loop:
    cmp r12d, AT_SLOTS
    jae .at_ok
    lea rax, [rel at_ptrs]
    mov r13, [rax + r12*8]
    test r13, r13
    jz .at_dr_next
    lea rax, [rel at_sizes]
    mov rsi, [rax + r12*8]
    mov rdi, r13
    mov rdx, r12
    mov [rbp - AT_SLOT], rdx
    call at_check
    test eax, eax
    jz .at_fail_4
    lea rax, [rel at_ptrs]
    mov rdi, [rax + r12*8]
    call ap_free
    lea rax, [rel at_ptrs]
    mov qword [rax + r12*8], 0
.at_dr_next:
    inc r12d
    jmp .at_dr_loop

.at_ok:
    xor eax, eax
    jmp .at_out

.at_fail_1:
    mov eax, 1000
    jmp .at_fail
.at_fail_2:
    mov eax, 2000
    jmp .at_fail
.at_fail_3:
    mov eax, 3000
    jmp .at_fail
.at_fail_4:
    mov eax, 4000
    jmp .at_fail
.at_fail_5:
    mov eax, 5000
.at_fail:
    add rax, [rbp - AT_SLOT]
    inc rax
.at_out:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC alloc_selftest

;; ============================================================================
;; alloc_selftest_main() -> rax = the process exit status
;;
;; Runs alloc_selftest and reports the outcome on stdout, as valtest does.
;; ============================================================================
global alloc_selftest_main
DEF_FUNC alloc_selftest_main, 8            ; 1 push, so rsp is 16-aligned
    push rbx
    call alloc_selftest
    mov rbx, rax
    test rbx, rbx
    jnz .asm_fail

    mov edi, 1
    lea rsi, [rel at_msg_ok]
    mov edx, at_msg_ok_len
    call sys_write
    xor eax, eax
    pop rbx
    leave
    ret

.asm_fail:
    mov edi, 1
    lea rsi, [rel at_msg_fail]
    mov edx, at_msg_fail_len
    call sys_write

    lea rsi, [rel at_fail_buf + 31]
    mov byte [rsi], 10
    mov rax, rbx
    mov ecx, 10
.asm_digits:
    xor edx, edx
    div rcx
    add dl, '0'
    dec rsi
    mov [rsi], dl
    test rax, rax
    jnz .asm_digits

    lea rdx, [rel at_fail_buf + 32]
    sub rdx, rsi
    mov edi, 1
    call sys_write

    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC alloc_selftest_main

section .rodata
at_msg_ok:   db "alloc selftest: OK", 10
at_msg_ok_len equ $ - at_msg_ok
at_msg_fail: db "alloc selftest: FAILED case "
at_msg_fail_len equ $ - at_msg_fail

align 8
at_bounds:
    dq 0, 1, 8, 15, 16, 17, 31, 32, 33, 48, 64, 128
    dq 255, 256, 257, 495, 496, 511, 512, 513, 1024, 4096
at_bounds_n equ ($ - at_bounds) / 8

section .bss
align 8
at_state:    resq 1
at_ptrs:     resq AT_SLOTS
at_sizes:    resq AT_SLOTS
at_bsizes:   resq AT_SLOTS
at_bslots:   resq at_bounds_n
at_fail_buf: resb 32
