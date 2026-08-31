; val.asm - NaN-boxed Value helpers and runtime constant pool
;
; Defines the rip-relative constant pool declared by include/value.inc, plus
; the conversion helpers used at the boundaries between the old Value64
; (payload, tag) world and the new one-word Value world during the migration.

%define VALUE_INC_NO_EXTERN
%include "macros.inc"
%include "object.inc"

extern none_singleton
extern bool_true
extern bool_false
extern obj_incref
extern int_from_i64_gmp

;; ============================================================================
;; Runtime constant pool
;;
;; x86-64 has no `cmp r64, imm64`, so the encoding constants live here and are
;; referenced as `[rel v_*]` from the macros in include/value.inc.  They are
;; read on every hot classification, so they stay resident in L1.
;; ============================================================================
section .rodata
align 64
global v_f64_off
global v_ptr_max_m1
global v_nan_lim
global v_canon_nan
global v_int_lo
global v_int_bias
global v_mask48
global v_sleep_lo
global v_iowait_lo

v_f64_off:      dq V_F64_OFF
v_ptr_max_m1:   dq V_PTR_MAX_M1
v_nan_lim:      dq V_NAN_LIM
v_canon_nan:    dq V_CANON_NAN
v_int_lo:       dq V_INT_LO
v_int_bias:     dq V_INT_BIAS
v_mask48:       dq V_MASK48
v_sleep_lo:     dq V_SLEEP_LO
v_iowait_lo:    dq V_IOWAIT_LO

section .text

;; ============================================================================
;; val_from_i64(rdi: int64) -> rax: Value
;;
;; Encode a signed 64-bit integer.  Values in [-2^50, 2^50) become immediates;
;; anything wider is boxed into a heap PyIntObject (owned reference).
;;
;; EVERY int64 -> Value conversion must go through this (or the V_FROM_I64
;; macro with an overflow branch).  The old SmallInt covered the full i64
;; range; the immediate range no longer does.
;; ============================================================================
DEF_FUNC val_from_i64
    mov rax, rdi
    V_FROM_I64 rax, rcx, .box
    leave
    ret
.box:
    call int_from_i64_gmp       ; rdi already holds the value; returns rax = ptr
    leave
    ret
END_FUNC val_from_i64

;; ============================================================================
;; val_from_i64_p(rdi: int64) -> rax: Value
;;
;; As val_from_i64, but preserves every register except rax so the V_PACK_I64
;; macro can call it from anywhere without knowing what is live.
;; ============================================================================
DEF_FUNC val_from_i64_p
    push rcx
    push rdx
    push rsi
    push rdi
    push r8
    push r9
    push r10
    push r11
    and rsp, -16                ; the callee may reach ap_malloc
    call val_from_i64           ; rdi already holds the value
    lea rsp, [rbp - 64]         ; undo the alignment; 8 pushes below rbp
    pop r11
    pop r10
    pop r9
    pop r8
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    leave
    ret
END_FUNC val_from_i64_p

;; ============================================================================
;; val_to_i64(rdi: Value) -> rax: int64, edx: 0 on success / 1 on failure
;;
;; Decode an integer Value (immediate or heap PyIntObject) to int64.
;; ============================================================================
extern int_to_i64
DEF_FUNC val_to_i64
    mov rax, rdi
    cmp rax, [rel v_int_lo]
    jb .heap
    V_TO_I64 rax
    xor edx, edx
    leave
    ret
.heap:
    ; Heap PyIntObject (or anything else the caller vouched for).
    mov edx, TAG_PTR
    call int_to_i64
    xor edx, edx
    leave
    ret
END_FUNC val_to_i64


;; ============================================================================
;; val_unpack(rdi: Value) -> rax: payload, edx: tag
;;
;; MIGRATION SHIM, the inverse of val_pack.  Ownership transfers 1:1.
;;
;; None, True and False come back as TAG_PTR: they are ordinary heap
;; singletons with no tag of their own.
;; ============================================================================
DEF_FUNC val_unpack
    mov rax, rdi
    test rax, rax
    jz .null

    mov rcx, rax
    shr rcx, 48
    jz .ptr                     ; high16 == 0: raw pointer

    cmp ecx, VH_INT_LO
    jae .int

    cmp ecx, VH_F64_MAX
    jbe .float                  ; high16 in [0x0001, 0xfff1]

    cmp ecx, VH_SLEEP
    je .sleep
    cmp ecx, VH_IOWAIT
    je .iowait

.null:
    xor eax, eax
    xor edx, edx
    leave
    ret

.ptr:
    mov edx, TAG_PTR
    leave
    ret

.int:
    V_TO_I64 rax
    mov edx, TAG_SMALLINT
    leave
    ret

.float:
    V_TO_F64 rax
    mov edx, TAG_FLOAT
    leave
    ret

.sleep:
    and rax, [rel v_mask48]
    mov edx, TAG_SLEEP
    leave
    ret

.iowait:
    and rax, [rel v_mask48]
    mov edx, TAG_IO_WAIT
    leave
    ret
END_FUNC val_unpack

