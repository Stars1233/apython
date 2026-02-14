; opcodes_misc.asm - Opcode handlers for return, binary/unary ops, comparisons,
;                    conditional jumps, and unconditional jumps
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack top pointer
;   r14 = co_consts tuple data pointer (&tuple.ob_item[0])
;   r15 = co_names tuple data pointer (&tuple.ob_item[0])
;
; ecx = opcode argument on entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "opcodes.inc"
%include "frame.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern eval_dispatch
extern eval_return
extern obj_dealloc
extern obj_is_true
extern fatal_error
extern none_singleton
extern bool_true
extern bool_false
extern int_type
extern float_type
extern float_number_methods
extern cell_new
extern gen_new
extern raise_exception
extern exc_RuntimeError_type
extern exc_TypeError_type
extern obj_incref
extern obj_decref
extern tuple_new
extern list_type

;; ============================================================================
;; op_return_value - Return TOS from current frame
;;
;; Phase 4 (simple case): module-level code, no previous frame.
;; Pop return value and jump to eval_return.
;; ============================================================================
global op_return_value
op_return_value:
    VPOP rax                    ; rax = return value
    mov qword [r12 + PyFrame.instr_ptr], 0  ; mark frame as "returned" (not yielded)
    jmp eval_return

;; ============================================================================
;; op_return_const - Return co_consts[arg] without popping the stack
;;
;; Load constant, INCREF, and jump to eval_return.
;; ============================================================================
global op_return_const
op_return_const:
    ; ecx = arg (index into co_consts)
    mov rax, [r14 + rcx*8]     ; rax = co_consts[arg]
    INCREF rax
    mov qword [r12 + PyFrame.instr_ptr], 0  ; mark frame as "returned" (not yielded)
    jmp eval_return

;; ============================================================================
;; op_binary_op - Perform a binary operation
;;
;; ecx = NB_* argument (operation selector)
;; Pops right (b) then left (a), dispatches through type's tp_as_number.
;; Followed by 1 CACHE entry (2 bytes) that must be skipped.
;; ============================================================================
global op_binary_op
op_binary_op:
    ; ecx = NB_* op code
    ; Save the op index before pops (VPOP doesn't clobber ecx)
    VPOP rsi                   ; rsi = right operand (b)
    VPOP rdi                   ; rdi = left operand (a)

    ; Fast path: SmallInt add (NB_ADD=0, NB_INPLACE_ADD=13)
    cmp ecx, 0                 ; NB_ADD
    je .binop_try_smallint_add
    cmp ecx, 13                ; NB_INPLACE_ADD
    je .binop_try_smallint_add

    ; Fast path: SmallInt subtract (NB_SUBTRACT=10, NB_INPLACE_SUBTRACT=23)
    cmp ecx, 10                ; NB_SUBTRACT
    je .binop_try_smallint_sub
    cmp ecx, 23                ; NB_INPLACE_SUBTRACT
    je .binop_try_smallint_sub

.binop_generic:
    ; Save operands for DECREF after call (push on machine stack)
    push rdi                   ; save left
    push rsi                   ; save right

    ; Look up offset in binary_op_offsets table
    ; For inplace variants (13-25), map to same slot as non-inplace (0-12)
    ; The table already has entries for indices 0-25
    lea rax, [rel binary_op_offsets]
    mov r8, [rax + rcx*8]      ; r8 = offset into PyNumberMethods

    ; Float coercion: if either operand is float, use float methods
    ; This handles int+float, float+int, float+float
    test rdi, rdi
    js .check_right_float      ; SmallInt left → can't be float, check right
    lea rcx, [rel float_type]
    cmp [rdi + PyObject.ob_type], rcx
    je .use_float_methods
.check_right_float:
    test rsi, rsi
    js .no_float_coerce        ; SmallInt right → no float
    lea rcx, [rel float_type]
    cmp [rsi + PyObject.ob_type], rcx
    je .use_float_methods

.no_float_coerce:
    ; Get type's tp_as_number method table from left operand
    ; SmallInt check: bit 63 set means tagged int, use int_type directly
    test rdi, rdi
    js .binop_smallint_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .binop_have_type
.binop_smallint_type:
    lea rax, [rel int_type]
.binop_have_type:
    mov rax, [rax + PyTypeObject.tp_as_number]
    jmp .binop_call_method

.use_float_methods:
    lea rax, [rel float_number_methods]

.binop_call_method:
    ; Get the specific method function pointer
    mov rax, [rax + r8]

    ; Call the method: rdi=left (a), rsi=right (b) - already set
    call rax
    ; rax = result

    ; Save result, DECREF operands
    push rax                   ; save result on machine stack
    mov rdi, [rsp + 8]        ; rdi = right operand
    DECREF_REG rdi
    mov rdi, [rsp + 16]       ; rdi = left operand
    DECREF_REG rdi
    pop rax                    ; restore result
    add rsp, 16                ; discard saved operands

    ; Push result
    VPUSH rax

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

.binop_try_smallint_add:
    ; Check both SmallInt (bit 63 set on both)
    mov rax, rdi
    and rax, rsi
    jns .binop_generic         ; at least one is NOT SmallInt

    ; Both SmallInt: decode, add, check overflow
    mov rax, rdi
    shl rax, 1
    sar rax, 1                 ; decode left
    mov rdx, rsi
    shl rdx, 1
    sar rdx, 1                 ; decode right
    add rax, rdx
    jo .binop_generic          ; overflow → fall back to generic
    ; Encode as SmallInt
    bts rax, 63
    ; No DECREF needed (SmallInt are not refcounted)
    VPUSH rax
    add rbx, 2
    DISPATCH

.binop_try_smallint_sub:
    ; Check both SmallInt (bit 63 set on both)
    mov rax, rdi
    and rax, rsi
    jns .binop_generic         ; at least one is NOT SmallInt

    ; Both SmallInt: decode, subtract, check overflow
    mov rax, rdi
    shl rax, 1
    sar rax, 1                 ; decode left
    mov rdx, rsi
    shl rdx, 1
    sar rdx, 1                 ; decode right
    sub rax, rdx
    jo .binop_generic          ; overflow → fall back to generic
    ; Encode as SmallInt
    bts rax, 63
    ; No DECREF needed (SmallInt are not refcounted)
    VPUSH rax
    add rbx, 2
    DISPATCH

;; ============================================================================
;; op_compare_op - Rich comparison
;;
;; Python 3.12: comparison op = arg >> 4
;; ecx = arg, extract comparison op by shifting right 4.
;; Calls type's tp_richcompare(left, right, op).
;; Followed by 1 CACHE entry (2 bytes) that must be skipped.
;; ============================================================================
global op_compare_op
op_compare_op:
    ; ecx = arg; comparison op = arg >> 4
    shr ecx, 4                 ; ecx = PY_LT/LE/EQ/NE/GT/GE (0-5)

    VPOP rsi                   ; rsi = right operand
    VPOP rdi                   ; rdi = left operand

    ; Fast path: both SmallInt — inline compare, no type dispatch
    mov rax, rdi
    and rax, rsi
    jns .cmp_slow_path         ; at least one is NOT SmallInt

    ; Both SmallInt: decode and compare
    mov rax, rdi
    shl rax, 1
    sar rax, 1                 ; decode left
    mov rdx, rsi
    shl rdx, 1
    sar rdx, 1                 ; decode right
    cmp rax, rdx

    ; Save comparison result: r8d = -1/0/1
    mov r8d, 0
    jz .cmp_dispatch
    mov r8d, -1
    jl .cmp_dispatch
    mov r8d, 1

.cmp_dispatch:
    ; Dispatch on comparison op (ecx)
    cmp ecx, PY_LT
    je .cmp_do_lt
    cmp ecx, PY_LE
    je .cmp_do_le
    cmp ecx, PY_EQ
    je .cmp_do_eq
    cmp ecx, PY_NE
    je .cmp_do_ne
    cmp ecx, PY_GT
    je .cmp_do_gt
    ; else PY_GE
    test r8d, r8d
    jge .cmp_push_true
    jmp .cmp_push_false
.cmp_do_lt:
    test r8d, r8d
    js .cmp_push_true
    jmp .cmp_push_false
.cmp_do_le:
    test r8d, r8d
    jle .cmp_push_true
    jmp .cmp_push_false
.cmp_do_eq:
    test r8d, r8d
    jz .cmp_push_true
    jmp .cmp_push_false
.cmp_do_ne:
    test r8d, r8d
    jnz .cmp_push_true
    jmp .cmp_push_false
.cmp_do_gt:
    test r8d, r8d
    jg .cmp_push_true
    jmp .cmp_push_false

.cmp_push_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    VPUSH rax
    add rbx, 2
    DISPATCH

.cmp_push_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    VPUSH rax
    add rbx, 2
    DISPATCH

.cmp_slow_path:
    ; Save operands and comparison op
    push rdi                   ; save left
    push rsi                   ; save right

    ; Float coercion: if either operand is float, use float_compare
    test rdi, rdi
    js .cmp_check_right_float  ; SmallInt can't be float
    lea rax, [rel float_type]
    cmp [rdi + PyObject.ob_type], rax
    je .cmp_use_float
.cmp_check_right_float:
    test rsi, rsi
    js .cmp_no_float           ; SmallInt can't be float
    lea rax, [rel float_type]
    cmp [rsi + PyObject.ob_type], rax
    je .cmp_use_float

.cmp_no_float:
    ; Get type's tp_richcompare (SmallInt-aware)
    test rdi, rdi
    js .cmp_smallint_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .cmp_have_type
.cmp_smallint_type:
    lea rax, [rel int_type]
.cmp_have_type:
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .cmp_identity            ; no tp_richcompare → fall back to identity
    jmp .cmp_do_call

.cmp_use_float:
    extern float_compare
    lea rax, [rel float_compare]

.cmp_do_call:

    ; Call tp_richcompare(left, right, op)
    ; rdi = left, rsi = right (already set)
    mov edx, ecx               ; edx = comparison op
    call rax
    ; rax = result (a bool object)

    ; Save result, DECREF operands
    push rax                   ; save result on machine stack
    mov rdi, [rsp + 8]        ; rdi = right operand
    DECREF_REG rdi
    mov rdi, [rsp + 16]       ; rdi = left operand
    DECREF_REG rdi
    pop rax                    ; restore result
    add rsp, 16                ; discard saved operands

    ; Push result
    VPUSH rax

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

.cmp_identity:
    ; Fallback: identity comparison (pointer equality)
    ; Stack: [rsp]=right, [rsp+8]=left; ecx=comparison op
    pop rsi                    ; rsi = right
    pop rdi                    ; rdi = left
    cmp rdi, rsi
    ; For EQ: equal pointers → True. For NE: unequal → True.
    ; All other comparisons fall back to False for unsupported types.
    je .cmp_id_equal
    ; Not equal
    cmp ecx, PY_NE
    je .cmp_id_true
    jmp .cmp_id_false
.cmp_id_equal:
    cmp ecx, PY_EQ
    je .cmp_id_true
    cmp ecx, PY_LE
    je .cmp_id_true
    cmp ecx, PY_GE
    je .cmp_id_true
.cmp_id_false:
    ; DECREF both operands, push False
    push rsi
    mov rdi, rdi               ; left already in rdi
    DECREF_REG rdi
    pop rdi                    ; right
    DECREF_REG rdi
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    VPUSH rax
    add rbx, 2
    DISPATCH
.cmp_id_true:
    ; DECREF both operands, push True
    push rsi
    DECREF_REG rdi
    pop rdi
    DECREF_REG rdi
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    VPUSH rax
    add rbx, 2
    DISPATCH

;; ============================================================================
;; op_unary_negative - Negate TOS
;;
;; Calls type's nb_negative from tp_as_number.
;; ============================================================================
global op_unary_negative
op_unary_negative:
    VPOP rdi                   ; rdi = operand

    ; Save operand for DECREF after call
    push rdi

    ; Get nb_negative: type -> tp_as_number -> nb_negative (SmallInt-aware)
    test rdi, rdi
    js .neg_smallint_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .neg_have_type
.neg_smallint_type:
    lea rax, [rel int_type]
.neg_have_type:
    mov rax, [rax + PyTypeObject.tp_as_number]
    mov rax, [rax + PyNumberMethods.nb_negative]

    ; Call nb_negative(operand); rdi already set
    call rax
    ; rax = result

    ; DECREF old operand
    push rax                   ; save result on machine stack
    mov rdi, [rsp + 8]        ; rdi = old operand
    DECREF_REG rdi
    pop rax                    ; restore result
    add rsp, 8                 ; discard saved operand

    ; Push result
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_unary_invert - Bitwise NOT of TOS (~x)
;;
;; Calls type's nb_invert from tp_as_number.
;; ============================================================================
global op_unary_invert
op_unary_invert:
    VPOP rdi                   ; rdi = operand
    push rdi

    test rdi, rdi
    js .inv_smallint_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .inv_have_type
.inv_smallint_type:
    lea rax, [rel int_type]
.inv_have_type:
    mov rax, [rax + PyTypeObject.tp_as_number]
    mov rax, [rax + PyNumberMethods.nb_invert]

    ; Call nb_invert(operand, NULL) — binary op signature, second arg unused
    xor esi, esi
    call rax
    push rax
    mov rdi, [rsp + 8]
    DECREF_REG rdi
    pop rax
    add rsp, 8
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_unary_not - Logical NOT of TOS
;;
;; Calls obj_is_true, then pushes the inverted boolean.
;; ============================================================================
global op_unary_not
op_unary_not:
    VPOP rdi                   ; rdi = operand

    ; Save operand for DECREF
    push rdi

    ; Call obj_is_true(operand) -> 0 or 1
    call obj_is_true
    push rax                   ; save truthiness result

    ; DECREF operand
    mov rdi, [rsp + 8]        ; reload operand
    DECREF_REG rdi
    pop rax                    ; restore truthiness
    add rsp, 8                 ; discard saved operand

    ; NOT inverts: if truthy (1), push False; if falsy (0), push True
    test eax, eax
    jnz .push_false
    lea rax, [rel bool_true]
    jmp .push_bool
.push_false:
    lea rax, [rel bool_false]
.push_bool:
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_pop_jump_if_false - Pop TOS, jump if falsy
;;
;; Python 3.12: arg is the absolute target offset in instruction words
;; (2-byte units from start of co_code).
;; ============================================================================
global op_pop_jump_if_false
op_pop_jump_if_false:
    ; Save arg (target offset) before call
    push rcx                   ; save target offset on machine stack

    VPOP rdi                   ; rdi = value to test

    ; Save value for DECREF
    push rdi

    ; Call obj_is_true(value) -> 0 (false) or 1 (true)
    call obj_is_true
    push rax                   ; save truthiness on machine stack

    ; DECREF the popped value
    mov rdi, [rsp + 8]        ; reload value
    DECREF_REG rdi
    pop rax                    ; restore truthiness
    add rsp, 8                 ; discard saved value
    pop rcx                    ; restore target offset

    ; If false (result == 0), jump to target
    test eax, eax
    jnz .no_jump

    ; Jump: relative from current rbx (delta in instruction words)
    lea rbx, [rbx + rcx*2]

.no_jump:
    DISPATCH

;; ============================================================================
;; op_pop_jump_if_true - Pop TOS, jump if truthy
;; ============================================================================
global op_pop_jump_if_true
op_pop_jump_if_true:
    ; Save arg (delta in instruction words)
    push rcx                   ; save target offset on machine stack

    VPOP rdi

    ; Save value for DECREF
    push rdi

    ; Call obj_is_true(value)
    call obj_is_true
    push rax                   ; save truthiness on machine stack

    ; DECREF the popped value
    mov rdi, [rsp + 8]        ; reload value
    DECREF_REG rdi
    pop rax                    ; restore truthiness
    add rsp, 8                 ; discard saved value
    pop rcx                    ; restore target offset

    ; If true (result != 0), jump to target
    test eax, eax
    jz .no_jump

    ; Jump: relative from current rbx
    lea rbx, [rbx + rcx*2]

.no_jump:
    DISPATCH

;; ============================================================================
;; op_pop_jump_if_none - Pop TOS, jump if None
;; ============================================================================
global op_pop_jump_if_none
op_pop_jump_if_none:
    VPOP rax                   ; rax = value

    ; Compare with none_singleton
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    jne .not_none

    ; IS None: save jump offset, DECREF, jump
    push rcx                   ; save jump offset
    DECREF rax
    pop rcx                    ; restore jump offset
    lea rbx, [rbx + rcx*2]
    DISPATCH

.not_none:
    ; NOT None: just DECREF and continue
    DECREF rax
    DISPATCH

;; ============================================================================
;; op_pop_jump_if_not_none - Pop TOS, jump if NOT None
;; ============================================================================
global op_pop_jump_if_not_none
op_pop_jump_if_not_none:
    VPOP rax                   ; rax = value

    ; Compare with none_singleton
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    je .is_none

    ; NOT None: save jump offset, DECREF, jump
    push rcx                   ; save jump offset
    DECREF rax
    pop rcx                    ; restore jump offset
    lea rbx, [rbx + rcx*2]
    DISPATCH

.is_none:
    ; IS None: just DECREF and continue
    DECREF rax
    DISPATCH

;; ============================================================================
;; op_jump_forward - Unconditional forward jump
;;
;; arg = number of instruction words to skip
;; Each instruction word is 2 bytes, so advance rbx by arg*2 bytes.
;; ============================================================================
global op_jump_forward
op_jump_forward:
    ; ecx = arg (instruction words to skip)
    lea rbx, [rbx + rcx*2]
    DISPATCH

;; ============================================================================
;; op_jump_backward - Unconditional backward jump
;;
;; arg = number of instruction words to go back
;; Subtract arg*2 bytes from rbx.
;; ============================================================================
global op_jump_backward
op_jump_backward:
    ; ecx = arg (instruction words to go back)
    mov rax, rcx
    shl rax, 1                 ; rax = arg * 2
    sub rbx, rax
    DISPATCH

;; ============================================================================
;; op_format_value - Format a value for f-strings
;;
;; arg & 0x03: conversion (0=none, 1=!s, 2=!r, 3=!a)
;; arg & 0x04: format spec present on stack below value
;; Pops value (and optional fmt_spec), pushes formatted string.
;; ============================================================================
global op_format_value
op_format_value:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov [rbp-8], rcx           ; save arg
    mov rax, rcx
    and eax, 4
    mov [rbp-16], rax          ; has_fmt_spec

    ; If format spec present, pop it first (it's under the value)
    test qword [rbp-16], 4
    jz .fv_no_spec
    ; TOS = fmt_spec, TOS1 = value
    VPOP rax                   ; fmt_spec (ignored for now)
    push rax                   ; save for DECREF
.fv_no_spec:

    VPOP rdi                   ; value
    push rdi                   ; save for DECREF

    ; Apply conversion based on arg & 3
    mov eax, [rbp-8]
    and eax, 3
    cmp eax, 2
    je .fv_repr
    ; Default: str() — conversion 0 (none) and 1 (!s) both use str()
    extern obj_str
    call obj_str
    jmp .fv_have_result

.fv_repr:
    extern obj_repr
    call obj_repr

.fv_have_result:
    push rax                   ; save result

    ; DECREF original value
    mov rdi, [rsp + 8]
    DECREF_REG rdi

    ; DECREF fmt_spec if present
    test qword [rbp-16], 4
    jz .fv_push
    mov rdi, [rsp + 16]
    DECREF_REG rdi
    pop rax                    ; result
    add rsp, 16                ; discard saved value and fmt_spec
    jmp .fv_done

.fv_push:
    pop rax                    ; result
    add rsp, 8                 ; discard saved value

.fv_done:
    VPUSH rax
    leave
    DISPATCH

;; ============================================================================
;; op_build_string - Concatenate N strings from the stack
;;
;; ecx = number of string fragments
;; Pops ecx strings, concatenates in order, pushes result.
;; ============================================================================
global op_build_string
op_build_string:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov [rbp-8], rcx           ; count

    test ecx, ecx
    jz .bs_zero
    cmp ecx, 1
    je .bs_one

    ; General case: iterate and concatenate
    ; Pop all items, keeping base pointer
    mov rdi, rcx
    shl rdi, 3                 ; count * 8
    sub r13, rdi               ; pop all at once (r13 = base of items)

    ; Start with first string
    mov rax, [r13]             ; first fragment
    INCREF rax                 ; we'll DECREF all originals later
    mov [rbp-16], rax          ; accumulator

    ; Concatenate remaining
    mov rcx, 1                 ; start from index 1
.bs_loop:
    cmp rcx, [rbp-8]
    jge .bs_decref
    push rcx
    extern str_concat
    mov rdi, [rbp-16]         ; accumulator
    mov rsi, [r13 + rcx*8]   ; next fragment
    call str_concat
    ; DECREF old accumulator
    push rax                   ; save new result
    mov rdi, [rbp-16]
    DECREF_REG rdi
    pop rax
    mov [rbp-16], rax          ; new accumulator
    pop rcx
    inc rcx
    jmp .bs_loop

.bs_decref:
    ; DECREF all original fragments
    xor ecx, ecx
.bs_decref_loop:
    cmp rcx, [rbp-8]
    jge .bs_push
    mov rdi, [r13 + rcx*8]
    push rcx
    DECREF_REG rdi
    pop rcx
    inc rcx
    jmp .bs_decref_loop

.bs_push:
    mov rax, [rbp-16]
    VPUSH rax
    leave
    DISPATCH

.bs_zero:
    ; Empty f-string: push empty string
    extern str_from_cstr
    CSTRING rdi, ""
    call str_from_cstr
    VPUSH rax
    leave
    DISPATCH

.bs_one:
    ; Shortcut: 1 fragment, just leave it on stack
    leave
    DISPATCH

;; ============================================================================
;; Data section - binary op offset lookup table
;; ============================================================================
section .data

;; Maps NB_* argument (0-25) to the byte offset within PyNumberMethods
;; where the corresponding method function pointer resides.
align 8
binary_op_offsets:
    dq 0    ; NB_ADD (0)              -> nb_add          (+0)
    dq 104  ; NB_AND (1)              -> nb_and          (+104)
    dq 144  ; NB_FLOOR_DIVIDE (2)     -> nb_floor_divide (+144)
    dq 88   ; NB_LSHIFT (3)           -> nb_lshift       (+88)
    dq 0    ; NB_MATRIX_MULTIPLY (4)  -> unsupported (placeholder)
    dq 16   ; NB_MULTIPLY (5)         -> nb_multiply     (+16)
    dq 24   ; NB_REMAINDER (6)        -> nb_remainder    (+24)
    dq 120  ; NB_OR (7)               -> nb_or           (+120)
    dq 40   ; NB_POWER (8)            -> nb_power        (+40)
    dq 96   ; NB_RSHIFT (9)           -> nb_rshift       (+96)
    dq 8    ; NB_SUBTRACT (10)        -> nb_subtract     (+8)
    dq 152  ; NB_TRUE_DIVIDE (11)     -> nb_true_divide  (+152)
    dq 112  ; NB_XOR (12)             -> nb_xor          (+112)
    ; Inplace variants (13-25) map to the same PyNumberMethods offsets:
    dq 0    ; NB_INPLACE_ADD (13)              -> nb_add
    dq 104  ; NB_INPLACE_AND (14)              -> nb_and
    dq 144  ; NB_INPLACE_FLOOR_DIVIDE (15)     -> nb_floor_divide
    dq 88   ; NB_INPLACE_LSHIFT (16)           -> nb_lshift
    dq 0    ; NB_INPLACE_MATRIX_MULTIPLY (17)  -> unsupported
    dq 16   ; NB_INPLACE_MULTIPLY (18)         -> nb_multiply
    dq 24   ; NB_INPLACE_REMAINDER (19)        -> nb_remainder
    dq 120  ; NB_INPLACE_OR (20)               -> nb_or
    dq 40   ; NB_INPLACE_POWER (21)            -> nb_power
    dq 96   ; NB_INPLACE_RSHIFT (22)           -> nb_rshift
    dq 8    ; NB_INPLACE_SUBTRACT (23)         -> nb_subtract
    dq 152  ; NB_INPLACE_TRUE_DIVIDE (24)      -> nb_true_divide
    dq 112  ; NB_INPLACE_XOR (25)              -> nb_xor

section .text

;; ============================================================================
;; op_make_cell - Wrap localsplus[arg] in a cell object
;;
;; If localsplus[arg] is not already a cell, create one and wrap the value.
;; If localsplus[arg] is NULL, create an empty cell.
;; ============================================================================
global op_make_cell
op_make_cell:
    lea rdx, [r12 + PyFrame.localsplus]

    ; Get current value
    mov rdi, [rdx + rcx*8]        ; rdi = current value (or NULL)

    ; Save slot address
    push rdx
    push rcx

    ; cell_new(obj) - creates cell wrapping obj (INCREFs if non-NULL)
    call cell_new
    ; rax = new cell

    pop rcx
    pop rdx

    ; XDECREF old value (cell_new already INCREFed it)
    mov rdi, [rdx + rcx*8]
    test rdi, rdi
    jz .mc_store
    push rax
    push rdx
    push rcx
    DECREF_REG rdi
    pop rcx
    pop rdx
    pop rax

.mc_store:
    ; Store cell in localsplus slot
    mov [rdx + rcx*8], rax
    DISPATCH

;; ============================================================================
;; op_copy_free_vars - Copy closure cells into frame's freevar slots
;;
;; arg = count of free vars to copy.
;; Source: current function's func_closure tuple.
;; Destination: localsplus[co_nlocals + ncellvars + i] for i in 0..arg-1
;;
;; In Python 3.12, the function being executed is NOT on the stack.
;; We find it via the calling frame's CALL setup. However, the bytecode
;; compiler ensures COPY_FREE_VARS is the first opcode, and the function
;; object is passed to eval_frame. We need to get it from the frame.
;;
;; Actually, in Python 3.12: the closure tuple is stored in the function
;; object. The function that owns the current frame can be found by
;; looking at the frame's localsplus from the caller. But simpler:
;; we stash the function object in the frame during func_call.
;; ============================================================================
global op_copy_free_vars
op_copy_free_vars:
    ; ecx = number of free vars to copy
    test ecx, ecx
    jz .cfv_done

    ; Get the function object from frame's func_obj slot
    mov rax, [r12 + PyFrame.func_obj]
    test rax, rax
    jz .cfv_done

    ; Get closure tuple from function
    mov rax, [rax + PyFuncObject.func_closure]
    test rax, rax
    jz .cfv_done

    ; rax = closure tuple, ecx = count
    ; Destination: localsplus starts at nlocalsplus - ecx (freevar slots at end)
    ; Actually: Python 3.12 puts freevars after cellvars in localsplus
    ; COPY_FREE_VARS arg tells us the count. The slots are at the END
    ; of localsplus: index [nlocalsplus - arg ... nlocalsplus - 1]
    mov edx, [r12 + PyFrame.nlocalsplus]
    sub edx, ecx                   ; edx = first freevar index

    ; Copy cells from closure tuple to freevar slots
    xor r8d, r8d                   ; loop counter
.cfv_loop:
    cmp r8d, ecx
    jge .cfv_done

    ; Get cell from closure tuple item[i]
    mov r9, [rax + PyTupleObject.ob_item + r8*8]
    INCREF r9

    ; Compute destination index: edx + r8d
    mov r10d, edx
    add r10d, r8d
    mov [r12 + PyFrame.localsplus + r10*8], r9

    inc r8d
    jmp .cfv_loop

.cfv_done:
    DISPATCH

;; ============================================================================
;; op_return_generator - Create generator from current frame
;;
;; RETURN_GENERATOR (75): First instruction in a generator function.
;; Creates a PyGenObject holding the current frame, returns it from eval_frame.
;; The frame is NOT freed by func_call (instr_ptr != 0 signals this).
;; ============================================================================
global op_return_generator
op_return_generator:
    ; Save current execution state in frame for later resumption
    mov [r12 + PyFrame.instr_ptr], rbx
    mov [r12 + PyFrame.stack_ptr], r13

    ; Create generator object: gen_new(frame)
    mov rdi, r12
    call gen_new
    ; rax = new generator object

    ; Return the generator from eval_frame
    ; frame->instr_ptr is non-zero, so func_call will skip frame_free
    jmp eval_return

;; ============================================================================
;; op_yield_value - Yield a value from generator
;;
;; YIELD_VALUE (150): Pop TOS (value to yield), save frame state,
;; return value from eval_frame. The generator is suspended.
;; ============================================================================
global op_yield_value
op_yield_value:
    ; Pop the value to yield
    VPOP rax

    ; Save frame state for resumption
    mov [r12 + PyFrame.instr_ptr], rbx
    mov [r12 + PyFrame.stack_ptr], r13

    ; Return yielded value from eval_frame
    jmp eval_return

;; ============================================================================
;; op_end_send - End of send operation
;;
;; END_SEND (5): Pop TOS1 (receiver/generator), keep TOS (value).
;; ============================================================================
global op_end_send
op_end_send:
    ; TOS = value, TOS1 = receiver
    VPOP rax                   ; rax = value (TOS)
    VPOP rdi                   ; rdi = receiver (TOS1)
    DECREF_REG rdi             ; DECREF receiver
    VPUSH rax                  ; push value back
    DISPATCH

;; ============================================================================
;; op_jump_backward_no_interrupt - Jump backward (no interrupt check)
;;
;; JUMP_BACKWARD_NO_INTERRUPT (134): Same as JUMP_BACKWARD for us.
;; ============================================================================
global op_jump_backward_no_interrupt
op_jump_backward_no_interrupt:
    mov rax, rcx
    shl rax, 1                 ; arg * 2 = byte offset
    sub rbx, rax
    DISPATCH

;; ============================================================================
;; op_call_intrinsic_1 - Call 1-arg intrinsic function
;;
;; CALL_INTRINSIC_1 (173): arg selects the intrinsic.
;; Pop TOS, call intrinsic, push result.
;; Key intrinsics:
;;   3 = INTRINSIC_STOPITERATION_ERROR (convert StopIteration to RuntimeError)
;;   5 = INTRINSIC_UNARY_POSITIVE (+x)
;;   6 = INTRINSIC_LIST_TO_TUPLE
;; ============================================================================
global op_call_intrinsic_1
op_call_intrinsic_1:
    cmp ecx, 3
    je .ci1_stopiter_error
    cmp ecx, 5
    je .ci1_unary_positive
    cmp ecx, 6
    je .ci1_list_to_tuple

    ; Unknown intrinsic — fatal
    CSTRING rdi, "unimplemented CALL_INTRINSIC_1"
    call fatal_error

.ci1_stopiter_error:
    ; Convert StopIteration to RuntimeError
    ; Pop the exception, raise RuntimeError instead
    VPOP rdi
    DECREF_REG rdi
    lea rdi, [rel exc_RuntimeError_type]
    CSTRING rsi, "generator raised StopIteration"
    call raise_exception

.ci1_unary_positive:
    ; +x — for ints/floats, just return x (no-op for numeric types)
    ; TOS is already the value, just leave it
    DISPATCH

.ci1_list_to_tuple:
    ; Convert list to tuple
    VPOP rdi                   ; rdi = list
    push rdi                   ; save for DECREF

    ; Get list size and items
    mov rcx, [rdi + PyListObject.ob_size]
    mov rsi, [rdi + PyListObject.ob_item]
    push rcx
    push rsi

    ; Create tuple of same size
    mov rdi, rcx
    call tuple_new
    mov rbx, rax               ; CAREFUL: clobbers rbx! Save and restore
    ; Actually rbx is the bytecode IP, don't clobber it
    ; Use stack instead
    pop rsi                    ; items ptr
    pop rcx                    ; count
    push rax                   ; save tuple

    ; Copy items from list to tuple, INCREF each
    xor edx, edx
.ci1_l2t_loop:
    cmp rdx, rcx
    jge .ci1_l2t_done
    push rcx
    push rdx
    push rsi

    mov rdi, [rsi + rdx*8]    ; item
    mov rax, [rsp + 24]       ; tuple
    mov [rax + PyTupleObject.ob_item + rdx*8], rdi
    INCREF rdi

    pop rsi
    pop rdx
    pop rcx
    inc rdx
    jmp .ci1_l2t_loop

.ci1_l2t_done:
    pop rax                    ; tuple
    VPUSH rax

    ; DECREF list
    pop rdi
    DECREF_REG rdi

    DISPATCH

;; ============================================================================
;; op_get_len - Push len(TOS) without popping TOS
;;
;; Opcode 30: GET_LEN
;; Used by match statements: push len, keep original on stack.
;; ============================================================================
extern obj_len

global op_get_len
op_get_len:
    ; PEEK TOS (don't pop)
    mov rdi, [r13 - 8]
    push rdi                    ; save obj

    ; Get length
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .gl_try_mapping
    mov rax, [rax + PySequenceMethods.sq_length]
    test rax, rax
    jz .gl_try_mapping
    call rax
    jmp .gl_got_len

.gl_try_mapping:
    pop rdi
    push rdi
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .gl_error
    mov rax, [rax + PyMappingMethods.mp_length]
    test rax, rax
    jz .gl_error
    call rax

.gl_got_len:
    pop rdi                     ; discard saved obj
    ; Convert length (in rax) to SmallInt and push
    bts rax, 63                 ; encode as SmallInt
    VPUSH rax
    DISPATCH

.gl_error:
    pop rdi
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object has no len()"
    call raise_exception

;; ============================================================================
;; op_setup_annotations - Create __annotations__ dict in locals
;;
;; Opcode 85: SETUP_ANNOTATIONS
;; ============================================================================
extern dict_new
extern dict_set
extern str_from_cstr

global op_setup_annotations
op_setup_annotations:
    push rbp
    mov rbp, rsp
    push rbx
    push r12                    ; save eval loop r12

    ; Check if locals dict exists
    mov rbx, [r12 + PyFrame.locals]
    test rbx, rbx
    jz .sa_done

    ; Create __annotations__ dict
    call dict_new
    mov r12, rax                ; r12 = new annotations dict (saved)

    ; Create key string
    lea rdi, [rel .str_annotations]
    call str_from_cstr
    ; rax = key string

    ; dict_set(locals, key, value)
    mov rdi, rbx                ; dict = locals
    mov rsi, rax                ; key = "__annotations__"
    mov rdx, r12                ; value = new annotations dict
    push rax                    ; save key for DECREF
    push rdx                    ; save value for DECREF
    call dict_set
    pop rdi
    call obj_decref             ; DECREF value (dict_set INCREFs)
    pop rdi
    call obj_decref             ; DECREF key

.sa_done:
    pop r12
    pop rbx
    pop rbp
    DISPATCH

section .rodata
.str_annotations: db "__annotations__", 0
section .text

;; ============================================================================
;; op_load_locals - Push locals dict
;;
;; Opcode 87: LOAD_LOCALS
;; ============================================================================
global op_load_locals
op_load_locals:
    mov rax, [r12 + PyFrame.locals]
    test rax, rax
    jz .ll_error
    INCREF rax
    VPUSH rax
    DISPATCH
.ll_error:
    lea rdi, [rel exc_RuntimeError_type]
    CSTRING rsi, "no locals dict"
    call raise_exception

;; ============================================================================
;; op_load_from_dict_or_globals - Load from dict on TOS, fallback to globals
;;
;; Opcode 175: LOAD_FROM_DICT_OR_GLOBALS
;; Used in class body comprehensions.
;; ============================================================================
extern dict_get

global op_load_from_dict_or_globals
op_load_from_dict_or_globals:
    ; ecx = name index
    mov rsi, [r15 + rcx*8]     ; name string
    push rsi

    ; Pop dict from TOS
    VPOP rdi
    push rdi                    ; save dict

    ; Try dict first
    mov rsi, [rsp + 8]         ; name
    call dict_get
    test rax, rax
    jnz .lfdg_found

    ; Try globals
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, [rsp + 8]         ; name
    call dict_get
    test rax, rax
    jnz .lfdg_found

    ; Try builtins
    mov rdi, [r12 + PyFrame.builtins]
    pop rdi                     ; discard saved dict (builtins from frame)
    pop rsi                     ; name
    mov rdi, [r12 + PyFrame.builtins]
    call dict_get
    test rax, rax
    jnz .lfdg_found_no_pop

    ; Not found
    extern exc_NameError_type
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not found"
    call raise_exception

.lfdg_found:
    add rsp, 16                 ; pop saved dict + name
.lfdg_found_no_pop:
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_match_mapping - Check if TOS is a mapping type
;;
;; Opcode 31: MATCH_MAPPING
;; Push True if TOS is dict/mapping, False otherwise. Don't pop TOS.
;; ============================================================================
extern dict_type

global op_match_mapping
op_match_mapping:
    mov rdi, [r13 - 8]            ; peek TOS
    test rdi, rdi
    js .mm_false                   ; SmallInt → not a mapping
    mov rax, [rdi + PyObject.ob_type]
    ; Check if it's a dict or has tp_as_mapping with mp_subscript
    lea rcx, [rel dict_type]
    cmp rax, rcx
    je .mm_true
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .mm_false
    mov rax, [rax + PyMappingMethods.mp_subscript]
    test rax, rax
    jz .mm_false
.mm_true:
    lea rax, [rel bool_true]
    INCREF rax
    VPUSH rax
    DISPATCH
.mm_false:
    lea rax, [rel bool_false]
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_match_sequence - Check if TOS is a sequence type
;;
;; Opcode 32: MATCH_SEQUENCE
;; Push True if TOS is list/tuple/sequence (not str/bytes/dict). Don't pop TOS.
;; ============================================================================
extern tuple_type
extern str_type
extern bytes_type

global op_match_sequence
op_match_sequence:
    mov rdi, [r13 - 8]            ; peek TOS
    test rdi, rdi
    js .ms_false                   ; SmallInt → not a sequence
    mov rax, [rdi + PyObject.ob_type]
    ; Exclude str, bytes, dict
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .ms_false
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .ms_false
    lea rcx, [rel dict_type]
    cmp rax, rcx
    je .ms_false
    ; Check list or tuple type directly
    lea rcx, [rel list_type]
    cmp rax, rcx
    je .ms_true
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .ms_true
    ; Check tp_as_sequence with sq_item
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .ms_false
    mov rax, [rax + PySequenceMethods.sq_item]
    test rax, rax
    jz .ms_false
.ms_true:
    lea rax, [rel bool_true]
    INCREF rax
    VPUSH rax
    DISPATCH
.ms_false:
    lea rax, [rel bool_false]
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_match_keys - Match mapping keys
;;
;; Opcode 33: MATCH_KEYS
;; TOS = keys tuple, TOS1 = subject (mapping)
;; If all keys in tuple exist in subject, push tuple of values + True
;; Otherwise push False
;; ============================================================================
global op_match_keys
op_match_keys:
    push rbp
    mov rbp, rsp
    sub rsp, 32                    ; [rbp-8]=keys, [rbp-16]=subject, [rbp-24]=values, [rbp-32]=nkeys

    ; TOS = keys tuple, TOS1 = subject
    ; Peek at both — don't pop either! Push result on top.
    mov rax, [r13 - 8]            ; keys tuple (TOS)
    mov [rbp-8], rax
    mov rax, [r13 - 16]           ; subject (TOS1)
    mov [rbp-16], rax

    ; Allocate values tuple
    mov rax, [rbp-8]
    mov rdi, [rax + PyTupleObject.ob_size]
    mov [rbp-32], rdi              ; save nkeys
    call tuple_new
    mov [rbp-24], rax              ; values tuple

    xor edx, edx                   ; index

.mk_loop:
    cmp rdx, [rbp-32]
    jge .mk_success

    push rdx

    ; Get key
    mov rax, [rbp-8]
    lea rax, [rax + PyTupleObject.ob_item]
    mov rsi, [rax + rdx*8]        ; key

    ; Look up in subject
    mov rdi, [rbp-16]
    call dict_get
    test rax, rax
    jz .mk_fail

    ; Store value in values tuple
    pop rdx
    push rdx
    INCREF rax
    mov rcx, [rbp-24]
    mov [rcx + PyTupleObject.ob_item + rdx*8], rax

    pop rdx
    inc rdx
    jmp .mk_loop

.mk_success:
    ; Push values tuple on top (stack: subject, keys, values_tuple)
    mov rax, [rbp-24]
    VPUSH rax
    jmp .mk_done

.mk_fail:
    pop rdx
    ; DECREF partial values tuple
    mov rdi, [rbp-24]
    call obj_decref
    ; Push None on top to indicate failure (stack: subject, keys, None)
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH rax

.mk_done:
    leave
    DISPATCH

;; ============================================================================
;; op_call_intrinsic_2 - Call 2-arg intrinsic function
;;
;; Opcode 174: CALL_INTRINSIC_2
;; arg selects the intrinsic.
;; TOS = arg2, TOS1 = arg1
;; Key intrinsics:
;;   1 = INTRINSIC_PREP_RERAISE - set __traceback__
;;   2 = INTRINSIC_TYPEVAR_WITH_BOUND
;;   3 = INTRINSIC_TYPEVAR_WITH_CONSTRAINTS
;;   4 = INTRINSIC_SET_FUNCTION_TYPE_PARAMS
;; ============================================================================
global op_call_intrinsic_2
op_call_intrinsic_2:
    cmp ecx, 1
    je .ci2_prep_reraise

    ; For type parameter intrinsics, just keep TOS1 and discard TOS
    ; (a simplification — full type parameter support would need more)
    VPOP rdi                       ; discard TOS (arg2)
    DECREF_REG rdi
    ; TOS1 stays
    DISPATCH

.ci2_prep_reraise:
    ; INTRINSIC_PREP_RERAISE: TOS = exc, TOS1 = traceback
    ; For now, discard traceback and keep exception
    VPOP rax                       ; exc
    VPOP rdi                       ; traceback
    push rax
    DECREF_REG rdi
    pop rax
    VPUSH rax
    DISPATCH
