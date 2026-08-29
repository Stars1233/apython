; value.inc - NaN-boxed 64-bit value representation ("Value")
;
; ==========================================================================
; ENCODING
; ==========================================================================
;
; A Value is ONE 64-bit word.  Let high16 = v >> 48.
;
;   high16            meaning                       decode
;   ----------------  ----------------------------  ------------------------
;   0x0000            PyObject*, RAW (v==0 => NULL) use as-is
;   0x0001 .. 0xFFF1  float64                       bits = v - V_F64_OFF
;   0xFFF2            async SLEEP sentinel          delay_ns = v & V_MASK48
;   0xFFF3            async IO_WAIT sentinel        fd|dir<<32 = v & V_MASK48
;   0xFFF4 .. 0xFFF7  reserved
;   0xFFF8 .. 0xFFFF  int immediate                 i = v - V_INT_BIAS
;
; Rationale:
;   * Linux x86-64 user virtual addresses are < 2^47, so every heap pointer
;     has high16 == 0.  Pointers are stored RAW: zero-cost dereference, and
;     NULL == 0 so `test reg, reg` still works as an error check.
;   * Non-NaN doubles span raw bits [0, 0xFFF0000000000000].  Adding 2^48
;     maps them to [0x0001000000000000, 0xFFF1000000000000], which collides
;     with neither pointer space nor tag space.
;   * NaN purification: raw double bits >= V_NAN_LIM (0xFFF1000000000000)
;     are negative NaNs whose encoding would land in tag space.  That set
;     includes x86's default QNaN (0xFFF8000000000000, produced by inf-inf,
;     0*inf, inf/inf).  They are canonicalized to V_CANON_NAN before
;     encoding.  CPython renders every NaN as "nan" regardless of sign or
;     payload, so this is unobservable from Python.
;   * Integers use a bias so that encode is one ADD and decode one SUB.
;     Immediate range is [-2^50, 2^50); anything wider becomes a heap
;     PyIntObject (Python ints are arbitrary precision anyway).
;
; None / True / False have NO immediate form: they are the immortal heap
; singletons none_singleton / bool_true / bool_false, stored as raw pointers.
;
; ==========================================================================

%ifndef VALUE_INC
%define VALUE_INC

; --- Encoding constants (assemble-time; see v_* symbols below for the
; --- runtime rip-relative copies, since x86-64 has no `cmp r64, imm64`.)
V_F64_OFF     equ 0x0001000000000000   ; float bias (2^48)
V_PTR_LIM     equ 0x0001000000000000   ; v < this  => pointer or NULL
V_PTR_MAX_M1  equ 0x0000FFFFFFFFFFFE   ; (v-1) <= this => non-NULL pointer
V_NAN_LIM     equ 0xFFF1000000000000   ; raw double bits >= this need purifying
V_CANON_NAN   equ 0x7FF8000000000000   ; canonical quiet NaN
V_SLEEP_LO    equ 0xFFF2000000000000   ; async sleep sentinel base
V_IOWAIT_LO   equ 0xFFF3000000000000   ; async io-wait sentinel base
V_INT_LO      equ 0xFFF8000000000000   ; v >= this => int immediate
V_INT_BIAS    equ 0xFFFC000000000000   ; i = v - bias, range [-2^50, 2^50)
V_MASK48      equ 0x0000FFFFFFFFFFFF
V_INT_SHIFT   equ 50                   ; log2 of the int immediate magnitude
V_NULL        equ 0

; high16 discriminators, for the `mov scratch, v / shr scratch, 48` form
VH_PTR        equ 0x0000
VH_F64_MAX    equ 0xFFF1
VH_SLEEP      equ 0xFFF2
VH_IOWAIT     equ 0xFFF3
VH_INT_LO     equ 0xFFF8

; Runtime constant pool, defined in src/val.asm.  A file that defines them
; sets VALUE_INC_NO_EXTERN before including this header.
%ifndef VALUE_INC_NO_EXTERN
extern v_f64_off
extern v_ptr_max_m1
extern v_nan_lim
extern v_canon_nan
extern v_int_lo
extern v_int_bias
extern v_mask48
%endif

; ==========================================================================
; CLASSIFICATION
; Each macro takes (value_reg, scratch_reg) and leaves flags set for the
; documented conditional jump.  Neither register may be the other.
; ==========================================================================

; ZF=1 (jz) if v is a pointer or NULL.
%macro V_IS_PTR 2               ; %1 = value, %2 = scratch (clobbered)
    mov %2, %1
    shr %2, 48
%endmacro

; CF=0 (jae) if v is an int immediate.
%macro V_IS_INT 2               ; %1 = value, %2 = scratch (clobbered)
    mov %2, %1
    shr %2, 48
    cmp %2, VH_INT_LO
%endmacro

; CF=1 (jb) if v is a float immediate.
; (high16 - 1) < 0xFFF1 holds exactly for high16 in [0x0001, 0xFFF1];
; pointers wrap to 0xFFFFFFFFFFFFFFFF and correctly fail the test.
%macro V_IS_FLOAT 2             ; %1 = value, %2 = scratch (clobbered)
    mov %2, %1
    shr %2, 48
    dec %2
    cmp %2, VH_F64_MAX
%endmacro

; Load high16 of a value into a scratch register (for a multi-way switch).
%macro V_HIGH16 2               ; %1 = dest, %2 = value
    mov %1, %2
    shr %1, 48
%endmacro

; ==========================================================================
; CONVERSION
; ==========================================================================

; Raw IEEE-754 double bits -> Value.  Purifies tag-space NaNs.
%macro V_FROM_F64 2             ; %1 = bits in/value out, %2 = scratch
    mov %2, [rel v_canon_nan]
    cmp %1, [rel v_nan_lim]
    cmovae %1, %2
    add %1, [rel v_f64_off]
%endmacro

; Value (known float) -> raw IEEE-754 double bits.
%macro V_TO_F64 1               ; %1 = value in / bits out
    sub %1, [rel v_f64_off]
%endmacro

; int64 -> Value, branching to %3 when the value does not fit the immediate
; range (the caller must then box it, e.g. via val_from_i64).
%macro V_FROM_I64 3             ; %1 = i64 in/value out, %2 = scratch, %3 = overflow label
    mov %2, %1
    sar %2, V_INT_SHIFT
    inc %2
    cmp %2, 2
    jae %3
    add %1, [rel v_int_bias]
%endmacro

; Value (known int immediate) -> int64.
%macro V_TO_I64 1               ; %1 = value in / int64 out
    sub %1, [rel v_int_bias]
%endmacro

; Immortal singletons.  The including file must declare the extern itself
; (none.asm / bool.asm define these symbols and must not also extern them).
%macro V_NONE 1                 ; %1 = dest
    lea %1, [rel none_singleton]
%endmacro

%macro V_TRUE 1                 ; %1 = dest
    lea %1, [rel bool_true]
%endmacro

%macro V_FALSE 1                ; %1 = dest
    lea %1, [rel bool_false]
%endmacro

; ==========================================================================
; REFERENCE COUNTING
; A Value participates in refcounting iff it is a non-NULL pointer, i.e.
; 1 <= v < 2^48.  `lea scratch,[v-1]` + one unsigned compare tests both
; halves of that at once.
; ==========================================================================

%macro INCREF_V 2               ; %1 = value, %2 = scratch (clobbered)
    lea %2, [%1 - 1]
    cmp %2, [rel v_ptr_max_m1]
    ja %%skip
    inc qword [%1 + PyObject.ob_refcnt]
    %%skip:
%endmacro

; Clobbers rdi and all caller-saved registers when the object is freed.
%macro DECREF_V 2               ; %1 = value, %2 = scratch (clobbered)
    lea %2, [%1 - 1]
    cmp %2, [rel v_ptr_max_m1]
    ja %%skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

; DECREF_V is already NULL-safe; XDECREF_V exists for call-site clarity.
%macro XDECREF_V 2              ; %1 = value, %2 = scratch (clobbered)
    DECREF_V %1, %2
%endmacro

%endif ; VALUE_INC
