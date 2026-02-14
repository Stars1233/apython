; slice.asm - Slice type implementation
;
; PySliceObject layout (from object.inc):
;   +0  ob_refcnt (8 bytes)
;   +8  ob_type   (8 bytes)
;   +16 start     (8 bytes: PyObject*)
;   +24 stop      (8 bytes: PyObject*)
;   +32 step      (8 bytes: PyObject*)
;   Total: 40 bytes

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern obj_incref
extern obj_dealloc
extern obj_decref
extern str_from_cstr
extern none_singleton
extern int_type

;; ============================================================================
;; slice_new(PyObject *start, PyObject *stop, PyObject *step) -> PySliceObject*
;; INCREFs all three args. Caller should pass none_singleton for missing values.
;; ============================================================================
global slice_new
slice_new:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, rdi           ; start
    mov r12, rsi           ; stop
    mov r13, rdx           ; step

    mov edi, PySliceObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel slice_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PySliceObject.start], rbx
    mov [rax + PySliceObject.stop], r12
    mov [rax + PySliceObject.step], r13

    ; INCREF all three
    push rax
    mov rdi, rbx
    call obj_incref
    mov rdi, r12
    call obj_incref
    mov rdi, r13
    call obj_incref
    pop rax

    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; slice_dealloc(PySliceObject *self)
;; ============================================================================
global slice_dealloc
slice_dealloc:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PySliceObject.start]
    call obj_decref
    mov rdi, [rbx + PySliceObject.stop]
    call obj_decref
    mov rdi, [rbx + PySliceObject.step]
    call obj_decref
    mov rdi, rbx
    call ap_free

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; slice_repr(PySliceObject *self) -> PyStrObject*
;; ============================================================================
global slice_repr
slice_repr:
    lea rdi, [rel slice_repr_str]
    jmp str_from_cstr

;; ============================================================================
;; pyobj_to_i64(PyObject *obj) -> int64 in rax
;; Converts SmallInt or GMP int to int64. For None, returns special sentinel.
;; ============================================================================
pyobj_to_i64:
    test rdi, rdi
    js .smallint
    ; Check for None
    lea rax, [rel none_singleton]
    cmp rdi, rax
    je .is_none
    ; GMP int: use mpz_get_si
    push rbp
    mov rbp, rsp
    lea rdi, [rdi + PyIntObject.mpz]
    extern __gmpz_get_si
    call __gmpz_get_si wrt ..plt
    pop rbp
    ret
.smallint:
    mov rax, rdi
    shl rax, 1
    sar rax, 1
    ret
.is_none:
    mov rax, 0x7FFFFFFFFFFFFFFF  ; sentinel for "not specified"
    ret

;; ============================================================================
;; slice_indices(PySliceObject *slice, int64 length)
;;   -> (start, stop, step) in rax, rdx, rcx
;; Resolves None values, handles negatives, clamps to bounds.
;; ============================================================================
global slice_indices
slice_indices:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8             ; align

    mov rbx, rdi           ; slice
    mov r14, rsi           ; length

    ; Sentinel constant for "None" from pyobj_to_i64
    ; (can't use cmp rax, imm64 — x86-64 doesn't support it)
%define NONE_SENTINEL 0x7FFFFFFFFFFFFFFF

    ; Get step (default 1)
    mov rdi, [rbx + PySliceObject.step]
    call pyobj_to_i64
    mov rcx, NONE_SENTINEL
    cmp rax, rcx
    jne .have_step
    mov rax, 1
.have_step:
    mov r15, rax           ; r15 = step

    ; Get start (default: 0 if step>0, length-1 if step<0)
    mov rdi, [rbx + PySliceObject.start]
    call pyobj_to_i64
    mov rcx, NONE_SENTINEL
    cmp rax, rcx
    jne .have_start
    test r15, r15
    js .start_neg_step
    xor eax, eax           ; start = 0
    jmp .have_start
.start_neg_step:
    mov rax, r14
    dec rax                ; start = length - 1
.have_start:
    ; Handle negative start
    test rax, rax
    jns .start_pos
    add rax, r14           ; start += length
    test rax, rax
    jns .start_pos
    xor eax, eax           ; clamp to 0
.start_pos:
    cmp rax, r14
    jl .start_ok
    mov rax, r14           ; clamp to length
.start_ok:
    mov r12, rax           ; r12 = start

    ; Get stop (default: length if step>0, -1 if step<0)
    mov rdi, [rbx + PySliceObject.stop]
    call pyobj_to_i64
    mov rcx, NONE_SENTINEL
    cmp rax, rcx
    jne .have_stop
    test r15, r15
    js .stop_neg_step
    mov rax, r14           ; stop = length
    jmp .have_stop
.stop_neg_step:
    mov rax, -1            ; stop = -1 (before start)
    jmp .stop_ok           ; default value is already correct, skip adjustment
.have_stop:
    ; Handle negative stop
    test rax, rax
    jns .stop_pos
    add rax, r14           ; stop += length
    test rax, rax
    jns .stop_pos
    ; For negative step, stop=-1 is valid (means "go to beginning")
    test r15, r15
    js .stop_ok
    xor eax, eax           ; clamp to 0 for positive step
    jmp .stop_ok
.stop_pos:
    cmp rax, r14
    jle .stop_ok
    mov rax, r14           ; clamp to length
.stop_ok:
    mov r13, rax           ; r13 = stop

    ; Return: rax=start, rdx=stop, rcx=step
    mov rax, r12
    mov rdx, r13
    mov rcx, r15

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; Data
;; ============================================================================
section .data

slice_name_str: db "slice", 0
slice_repr_str: db "slice(...)", 0

align 8
global slice_type
slice_type:
    dq 1                      ; ob_refcnt (immortal)
    dq 0                      ; ob_type
    dq slice_name_str         ; tp_name
    dq PySliceObject_size     ; tp_basicsize
    dq slice_dealloc          ; tp_dealloc
    dq slice_repr             ; tp_repr
    dq slice_repr             ; tp_str
    dq 0                      ; tp_hash
    dq 0                      ; tp_call
    dq 0                      ; tp_getattr
    dq 0                      ; tp_setattr
    dq 0                      ; tp_richcompare
    dq 0                      ; tp_iter
    dq 0                      ; tp_iternext
    dq 0                      ; tp_init
    dq 0                      ; tp_new
    dq 0                      ; tp_as_number
    dq 0                      ; tp_as_sequence
    dq 0                      ; tp_as_mapping
    dq 0                      ; tp_base
    dq 0                      ; tp_dict
    dq 0                      ; tp_mro
    dq 0                      ; tp_flags
    dq 0                      ; tp_bases
