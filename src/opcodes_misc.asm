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

;; ============================================================================
;; op_return_value - Return TOS from current frame
;;
;; Phase 4 (simple case): module-level code, no previous frame.
;; Pop return value and jump to eval_return.
;; ============================================================================
global op_return_value
op_return_value:
    VPOP rax                    ; rax = return value
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
    ; Get the specific method function pointer
    mov rax, [rax + r8]

    ; Call the method: rdi=left (a), rsi=right (b) - already set
    call rax
    ; rax = result

    ; Save result, DECREF operands
    mov r8, rax                ; r8 = result (caller-saved, but we control flow)
    pop rdi                    ; rdi = right operand
    DECREF_REG rdi
    pop rdi                    ; rdi = left operand
    DECREF_REG rdi

    ; Push result
    VPUSH r8

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

    ; Get type's tp_richcompare (SmallInt-aware)
    test rdi, rdi
    js .cmp_smallint_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .cmp_have_type
.cmp_smallint_type:
    lea rax, [rel int_type]
.cmp_have_type:
    mov rax, [rax + PyTypeObject.tp_richcompare]

    ; Call tp_richcompare(left, right, op)
    ; rdi = left, rsi = right (already set)
    mov edx, ecx               ; edx = comparison op
    call rax
    ; rax = result (a bool object)

    ; Save result, DECREF operands
    mov r8, rax                ; r8 = result
    pop rdi                    ; rdi = right operand
    DECREF_REG rdi
    pop rdi                    ; rdi = left operand
    DECREF_REG rdi

    ; Push result
    VPUSH r8

    ; Skip 1 CACHE entry = 2 bytes
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
    mov r8, rax                ; save result
    pop rdi                    ; rdi = old operand
    DECREF_REG rdi

    ; Push result
    VPUSH r8
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
    mov r8d, eax               ; r8d = truthiness result

    ; DECREF operand
    pop rdi
    DECREF_REG rdi

    ; NOT inverts: if truthy (1), push False; if falsy (0), push True
    test r8d, r8d
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
    mov r8d, ecx               ; r8d = target offset in instruction words

    VPOP rdi                   ; rdi = value to test

    ; Save value for DECREF
    push rdi

    ; Call obj_is_true(value) -> 0 (false) or 1 (true)
    call obj_is_true
    mov r9d, eax               ; r9d = truthiness

    ; DECREF the popped value
    pop rdi
    DECREF_REG rdi

    ; If false (result == 0), jump to target
    test r9d, r9d
    jnz .no_jump

    ; Jump: relative from current rbx (delta in instruction words)
    lea rbx, [rbx + r8*2]

.no_jump:
    DISPATCH

;; ============================================================================
;; op_pop_jump_if_true - Pop TOS, jump if truthy
;; ============================================================================
global op_pop_jump_if_true
op_pop_jump_if_true:
    ; Save arg (delta in instruction words)
    mov r8d, ecx

    VPOP rdi

    ; Save value for DECREF
    push rdi

    ; Call obj_is_true(value)
    call obj_is_true
    mov r9d, eax

    ; DECREF the popped value
    pop rdi
    DECREF_REG rdi

    ; If true (result != 0), jump to target
    test r9d, r9d
    jz .no_jump

    ; Jump: relative from current rbx
    lea rbx, [rbx + r8*2]

.no_jump:
    DISPATCH

;; ============================================================================
;; op_pop_jump_if_none - Pop TOS, jump if None
;; ============================================================================
global op_pop_jump_if_none
op_pop_jump_if_none:
    ; Save arg (delta in instruction words)
    mov r8d, ecx

    VPOP rax                   ; rax = value

    ; Compare with none_singleton
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    ; Save comparison result before DECREF
    sete cl                    ; cl = 1 if None

    ; DECREF the popped value
    DECREF rax

    ; If was None (cl == 1), jump to target
    test cl, cl
    jz .no_jump

    ; Jump: relative from current rbx
    lea rbx, [rbx + r8*2]

.no_jump:
    DISPATCH

;; ============================================================================
;; op_pop_jump_if_not_none - Pop TOS, jump if NOT None
;; ============================================================================
global op_pop_jump_if_not_none
op_pop_jump_if_not_none:
    ; Save arg (delta in instruction words)
    mov r8d, ecx

    VPOP rax                   ; rax = value

    ; Compare with none_singleton
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    ; Save comparison result before DECREF
    setne cl                   ; cl = 1 if NOT None

    ; DECREF the popped value
    DECREF rax

    ; If was NOT None (cl == 1), jump to target
    test cl, cl
    jz .no_jump

    ; Jump: relative from current rbx
    lea rbx, [rbx + r8*2]

.no_jump:
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
