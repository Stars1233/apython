; valtest.asm - Self-test for the NaN-boxed Value encoding
;
; Reachable via `./apython --selftest-value`.  Encoding bugs in a NaN box are
; near-impossible to diagnose from Python-level symptoms (a mis-tagged value
; simply segfaults somewhere unrelated), so the boundary cases are checked
; directly against the encoder here.
;
; value_selftest() -> rax = 0 on success, else a nonzero case id of the form
;                     (group_number * 1000 + case_index + 1).

%include "macros.inc"
%include "object.inc"

extern sys_write
extern int_from_i64_gmp
extern int_to_i64
extern obj_dealloc
extern val_from_i64
extern val_unpack

section .rodata

; --- Group 1: integers.  (value, 1 = expect immediate / 0 = expect boxed)
align 16
int_cases:
    dq 0,                    1
    dq 1,                    1
    dq -1,                   1
    dq 0x0003FFFFFFFFFFFF,   1      ; 2^50 - 1  (largest immediate)
    dq 0xFFFC000000000000,   1      ; -2^50     (smallest immediate)
    dq 0x0004000000000000,   0      ; 2^50      (first boxed)
    dq 0xFFFBFFFFFFFFFFFF,   0      ; -2^50 - 1 (first boxed)
    dq 4611686018427387904,  0      ; 2^62
    dq -4611686018427387904, 0      ; -2^62
    dq 0x7FFFFFFFFFFFFFFF,   0      ; INT64_MAX
    dq 0x8000000000000000,   0      ; INT64_MIN
int_cases_end:
INT_CASE_COUNT equ (int_cases_end - int_cases) / 16

; --- Group 2: floats.  (raw double bits, expected decoded bits)
align 16
float_cases:
    dq 0x0000000000000000, 0x0000000000000000   ; +0.0
    dq 0x8000000000000000, 0x8000000000000000   ; -0.0
    dq 0x3FF0000000000000, 0x3FF0000000000000   ; 1.0
    dq 0xBFF0000000000000, 0xBFF0000000000000   ; -1.0
    dq 0x0000000000000001, 0x0000000000000001   ; smallest subnormal
    dq 0x800FFFFFFFFFFFFF, 0x800FFFFFFFFFFFFF   ; largest negative subnormal
    dq 0x7FEFFFFFFFFFFFFF, 0x7FEFFFFFFFFFFFFF   ; DBL_MAX
    dq 0xFFEFFFFFFFFFFFFF, 0xFFEFFFFFFFFFFFFF   ; -DBL_MAX
    dq 0x7FF0000000000000, 0x7FF0000000000000   ; +inf
    dq 0xFFF0000000000000, 0xFFF0000000000000   ; -inf
    dq 0x7FF8000000000000, 0x7FF8000000000000   ; canonical quiet NaN
    dq 0x7FFFFFFFFFFFFFFF, 0x7FFFFFFFFFFFFFFF   ; positive NaN, max payload
    dq 0xFFF0000000000001, 0xFFF0000000000001   ; negative NaN below V_NAN_LIM
    dq 0xFFF0FFFFFFFFFFFF, 0xFFF0FFFFFFFFFFFF   ; negative NaN, last safe one
    dq 0xFFF1000000000000, 0x7FF8000000000000   ; first purified NaN
    dq 0xFFF8000000000000, 0x7FF8000000000000   ; x86 default QNaN (inf - inf)
    dq 0xFFFFFFFFFFFFFFFF, 0x7FF8000000000000   ; all-ones NaN
float_cases_end:
FLOAT_CASE_COUNT equ (float_cases_end - float_cases) / 16

msg_ok:     db "value selftest: OK", 10
msg_ok_len  equ $ - msg_ok
msg_fail:   db "value selftest: FAILED case "
msg_fail_len equ $ - msg_fail

section .bss
fail_buf:   resb 32

section .text

;; ============================================================================
;; value_selftest() -> rax: 0 on success, else the failing case id
;; ============================================================================
DEF_FUNC value_selftest
    push rbx
    push r12
    push r13
    push r14
    push r15

    ; ---------------------------------------------------------------- group 1
    xor r12, r12                    ; case index
.int_loop:
    cmp r12, INT_CASE_COUNT
    jae .int_done

    mov rdx, r12
    shl rdx, 4                      ; 16 bytes per case
    lea rax, [rel int_cases]
    mov r13, [rax + rdx]            ; the int64 under test
    mov r14, [rax + rdx + 8]        ; 1 = expect immediate

    mov rdi, r13
    call val_from_i64
    mov rbx, rax                    ; rbx = encoded Value

    test r14, r14
    jz .int_expect_boxed

    ; Expect an immediate: must land in int space and decode exactly.
    cmp rbx, [rel v_int_lo]
    jb .int_fail
    mov rax, rbx
    V_TO_I64 rax
    cmp rax, r13
    jne .int_fail
    ; ...and val_unpack must agree.
    mov rdi, rbx
    call val_unpack
    cmp edx, TAG_SMALLINT
    jne .int_fail
    cmp rax, r13
    jne .int_fail
    jmp .int_next

.int_expect_boxed:
    ; Expect a heap PyIntObject: raw pointer space, non-NULL, exact value.
    test rbx, rbx
    jz .int_fail
    cmp rbx, [rel v_int_lo]
    jae .int_fail
    mov rax, rbx
    shr rax, 48
    jnz .int_fail                   ; must be a raw pointer (high16 == 0)
    mov rdi, rbx
    mov edx, TAG_PTR
    call int_to_i64
    cmp rax, r13
    jne .int_fail
    mov rdi, rbx
    call obj_dealloc

.int_next:
    inc r12
    jmp .int_loop
.int_done:

    ; ---------------------------------------------------------------- group 2
    xor r12, r12
.flt_loop:
    cmp r12, FLOAT_CASE_COUNT
    jae .flt_done

    mov rdx, r12
    shl rdx, 4                      ; 16 bytes per case
    lea rax, [rel float_cases]
    mov r13, [rax + rdx]            ; raw double bits
    mov r14, [rax + rdx + 8]        ; expected decoded bits

    mov rbx, r13
    V_FROM_F64 rbx, rcx             ; rbx = encoded Value

    ; Must classify as a float: strictly between pointer space and tag space.
    mov rax, rbx
    shr rax, 48
    jz .flt_fail                    ; would look like a pointer
    cmp rax, VH_F64_MAX
    ja .flt_fail                    ; would look like a sentinel or an int

    ; Must decode to the expected (possibly purified) bit pattern.
    mov rax, rbx
    V_TO_F64 rax
    cmp rax, r14
    jne .flt_fail

    ; val_unpack must agree.
    mov rdi, rbx
    call val_unpack
    cmp edx, TAG_FLOAT
    jne .flt_fail
    cmp rax, r14
    jne .flt_fail

    inc r12
    jmp .flt_loop
.flt_done:

    ; ---------------------------------------------------------------- group 3
    ; Pointer and NULL handling.
    xor r12, r12

    ; NULL: unpacks to TAG_NULL, and DECREF_V must not touch memory.
    xor rdi, rdi
    call val_unpack
    test edx, edx
    jnz .ptr_fail
    test rax, rax
    jnz .ptr_fail
    inc r12

    xor rax, rax
    DECREF_V rax, rcx               ; must be a no-op, not a NULL dereference
    inc r12

    ; A real object: INCREF_V / DECREF_V must move its refcount by one.
    lea rbx, [rel probe_obj]
    mov qword [rbx + PyObject.ob_refcnt], 7
    INCREF_V rbx, rcx
    cmp qword [rbx + PyObject.ob_refcnt], 8
    jne .ptr_fail
    inc r12
    DECREF_V rbx, rcx
    cmp qword [rbx + PyObject.ob_refcnt], 7
    jne .ptr_fail
    inc r12

    ; Immediates must never be refcounted.
    mov rdi, 12345
    call val_from_i64
    mov rbx, rax
    cmp rbx, [rel v_int_lo]
    jb .ptr_fail
    INCREF_V rbx, rcx               ; must be a no-op
    DECREF_V rbx, rcx               ; must be a no-op
    mov rax, rbx
    V_TO_I64 rax
    cmp rax, 12345
    jne .ptr_fail
    inc r12

    ; ---------------------------------------------------------------- success
    xor eax, eax
    jmp .done

.int_fail:
    lea rax, [r12 + 1]
    add rax, 1000
    jmp .done
.flt_fail:
    lea rax, [r12 + 1]
    add rax, 2000
    jmp .done
.ptr_fail:
    lea rax, [r12 + 1]
    add rax, 3000

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC value_selftest

;; ============================================================================
;; value_selftest_main() -> rax: process exit status
;;
;; Runs value_selftest and reports the outcome on stdout.
;; ============================================================================
DEF_FUNC value_selftest_main
    push rbx
    call value_selftest
    mov rbx, rax
    test rbx, rbx
    jnz .fail

    mov edi, 1
    lea rsi, [rel msg_ok]
    mov edx, msg_ok_len
    call sys_write
    xor eax, eax
    pop rbx
    leave
    ret

.fail:
    mov edi, 1
    lea rsi, [rel msg_fail]
    mov edx, msg_fail_len
    call sys_write

    ; Render the case id as decimal into fail_buf, backwards from the end.
    lea rsi, [rel fail_buf + 31]
    mov byte [rsi], 10              ; trailing newline
    mov rax, rbx
    mov ecx, 10
.digits:
    xor edx, edx
    div rcx
    add dl, '0'
    dec rsi
    mov [rsi], dl
    test rax, rax
    jnz .digits

    lea rdx, [rel fail_buf + 32]
    sub rdx, rsi                    ; length
    mov edi, 1
    call sys_write

    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC value_selftest_main

section .data
align 16
probe_obj:
    dq 1            ; ob_refcnt
    dq 0            ; ob_type
