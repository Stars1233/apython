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
VFI_PUSHED equ 64           ; rbp less the eight caller-saved pushes
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
    lea rsp, [rbp - VFI_PUSHED] ; undo the alignment, back to the eight pushes
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


;; ============================================================================
;; val_unpack_cold(rax = Value) -> rax = payload, rdx = tag
;;
;; The cold arms of the V_UNPACK macro, hoisted out of ~950 expansions.  The
;; macro answers a real non-NULL pointer itself -- one lea, one compare, one
;; branch -- and calls here for everything else.  Accepts a pointer anyway, so
;; it is a complete unpack on its own and not a trap for a future caller.
;;
;; TWO INVARIANTS, and both are load-bearing:
;;
;;   * It clobbers rax and rdx and NOTHING ELSE.  That is exactly the macro's
;;     documented clobber set, which is why the call needs no register saves
;;     at the 496 sites whose operands are already (rax, rdx).  Keep it that
;;     way: no call, no push, no SSE, no memory but the rip-relative pool.
;;   * It is correct at EITHER rsp parity.  It makes no call, so alignment is
;;     meaningless to it -- which matters because a macro cannot know the
;;     parity of the site it expands at.
;; ============================================================================
DEF_FUNC_BARE val_unpack_cold
    mov rdx, rax
    shr rdx, 48
    jz .vuc_ptr                 ; high16 == 0: a pointer, or NULL

    cmp edx, VH_INT_LO          ; the shr left at most 16 bits, so edx is exact
    jae .vuc_int
    cmp edx, VH_F64_MAX
    jbe .vuc_f64
    cmp edx, VH_SLEEP
    je .vuc_slp
    cmp edx, VH_IOWAIT
    je .vuc_iow
    xor eax, eax                ; a reserved encoding: treat it as an empty slot
.vuc_null:
    xor edx, edx
    ret

.vuc_ptr:
    test rax, rax
    jz .vuc_null
    mov edx, TAG_PTR
    ret

.vuc_int:
    V_TO_I64 rax
    mov edx, TAG_SMALLINT
    ret

.vuc_f64:
    V_TO_F64 rax
    mov edx, TAG_FLOAT
    ret

.vuc_slp:
    and rax, [rel v_mask48]
    mov edx, TAG_SLEEP
    ret

.vuc_iow:
    and rax, [rel v_mask48]
    mov edx, TAG_IO_WAIT
    ret
END_FUNC val_unpack_cold

;; ============================================================================
;; val_pack_cold(rax = payload, rdx = tag) -> rax = Value
;;
;; The cold arms of V_PACK, hoisted out of ~1280 expansions.  TAG_PTR and
;; TAG_NULL never arrive -- the macro answers those in two instructions -- but
;; they are accepted and passed through, so this is a complete pack.
;;
;; Same two invariants as val_unpack_cold, and the boxing arm is why the
;; second one needs saying twice.  It reaches ap_malloc and GMP, but only
;; through val_from_i64_p, which realigns rsp itself and preserves every
;; register except rax.  The caller's rdi is parked in rdx across it rather
;; than pushed: rdx is the macro's scratch operand and therefore ours, so this
;; function touches the stack only for its own return address.
;; ============================================================================
DEF_FUNC_BARE val_pack_cold
    cmp rdx, TAG_SMALLINT
    je .vpc_si
    cmp rdx, TAG_FLOAT
    je .vpc_f64
    cmp rdx, TAG_SLEEP
    je .vpc_slp
    cmp rdx, TAG_IO_WAIT
    je .vpc_iow
    ret                         ; TAG_PTR, TAG_NULL, reserved: already a Value

.vpc_f64:
    V_FROM_F64 rax, rdx
    ret

.vpc_slp:
    or rax, [rel v_sleep_lo]
    ret

.vpc_iow:
    or rax, [rel v_iowait_lo]
    ret

.vpc_si:
    V_FROM_I64 rax, rdx, .vpc_box
    ret

.vpc_box:
    mov rdx, rdi                ; park the caller's rdi in our own scratch
    mov rdi, rax
    call val_from_i64_p         ; preserves rdx and rdi; answers in rax
    mov rdi, rdx
    ret
END_FUNC val_pack_cold
