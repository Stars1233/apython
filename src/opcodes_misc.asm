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

section .text

extern eval_dispatch
extern eval_saved_rbx
extern trace_opcodes
extern opcode_table
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

;; Stack layout constants for binary_op / compare_op generic paths.
;; After 4 pushes: right, right_tag, left, left_tag
;; Offsets relative to rsp immediately after the 4 pushes.
BO_RIGHT equ 0
BO_RTAG  equ 8
BO_LEFT  equ 16
BO_LTAG  equ 24
BO_SIZE  equ 32

;; Stack layout constants for op_format_value (DEF_FUNC, 48 bytes).
FV_ARG     equ 8
FV_HASSPEC equ 16
FV_SPEC    equ 24
FV_VALUE   equ 32
FV_STAG    equ 40    ; fmt_spec tag
FV_VTAG    equ 48    ; value tag
FV_FRAME   equ 48

;; Stack layout constants for op_build_string (DEF_FUNC, 16 bytes).
BS_COUNT   equ 8
BS_ACCUM   equ 16
BS_FRAME   equ 16

;; Stack layout constants for op_send (DEF_FUNC, 40 bytes).
SND_ARG    equ 8
SND_SENT   equ 16
SND_RECV   equ 24
SND_RESULT equ 32
SND_STAG   equ 40    ; sent_value tag
SND_FRAME  equ 40

;; Stack layout constants for op_match_keys (DEF_FUNC, 32 bytes).
MK_KEYS    equ 8
MK_SUBJ    equ 16
MK_VALS    equ 24
MK_NKEYS   equ 32
MK_FRAME   equ 32

;; ============================================================================
;; op_return_value - Return TOS from current frame
;;
;; Phase 4 (simple case): module-level code, no previous frame.
;; Pop return value and jump to eval_return.
;; ============================================================================
DEF_FUNC_BARE op_return_value
    VPOP rax                    ; rax = return value
    mov qword [r12 + PyFrame.instr_ptr], 0  ; mark frame as "returned" (not yielded)
    jmp eval_return
END_FUNC op_return_value

;; ============================================================================
;; op_return_const - Return co_consts[arg] without popping the stack
;;
;; Load constant, INCREF, and jump to eval_return.
;; ============================================================================
DEF_FUNC_BARE op_return_const
    ; ecx = arg (index into co_consts)
    mov rax, [r14 + rcx*8]     ; rax = co_consts[arg]
    INCREF rax
    mov qword [r12 + PyFrame.instr_ptr], 0  ; mark frame as "returned" (not yielded)
    jmp eval_return
END_FUNC op_return_const

;; ============================================================================
;; op_binary_op - Perform a binary operation
;;
;; ecx = NB_* argument (operation selector)
;; Pops right (b) then left (a), dispatches through type's tp_as_number.
;; Followed by 1 CACHE entry (2 bytes) that must be skipped.
;; ============================================================================
DEF_FUNC_BARE op_binary_op
    ; ecx = NB_* op code
    ; Save the op index before pops (VPOP doesn't clobber ecx)
    VPOP rsi                   ; rsi = right operand (b)
    mov r8, [r13 + 8]         ; r8 = right tag
    VPOP rdi                   ; rdi = left operand (a)
    mov r9, [r13 + 8]         ; r9 = left tag

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
    ; Save operands + tags for DECREF after call (push on machine stack)
    ; Stack layout: [rsp+BO_RIGHT], [rsp+BO_RTAG], [rsp+BO_LEFT], [rsp+BO_LTAG]
    push r9                    ; save left tag
    push rdi                   ; save left
    push r8                    ; save right tag
    push rsi                   ; save right

    ; Look up offset in binary_op_offsets table
    ; For inplace variants (13-25), map to same slot as non-inplace (0-12)
    ; The table already has entries for indices 0-25
    lea rax, [rel binary_op_offsets]
    mov r8, [rax + rcx*8]      ; r8 = offset into PyNumberMethods
    mov r9d, ecx               ; r9d = save binary op code (survives float check)

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
    ; For NB_ADD (0/13) and NB_MULTIPLY (5/18): if left is int/SmallInt
    ; and right has sq_concat/sq_repeat, use sequence method instead.
    ; This handles: 3 * "ab", 3 * [1,2], etc.
    test rdi, rdi
    jns .binop_not_smallint_left
    ; Left is SmallInt — check if right has sequence methods
    cmp r9d, 5              ; NB_MULTIPLY
    je .binop_try_right_seq
    cmp r9d, 18             ; NB_INPLACE_MULTIPLY
    je .binop_try_right_seq
    jmp .binop_left_type

.binop_try_right_seq:
    ; Check right operand's tp_as_sequence->sq_repeat
    test rsi, rsi
    js .binop_left_type     ; right is SmallInt, skip
    mov rax, [rsi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .binop_left_type
    mov rax, [rax + PySequenceMethods.sq_repeat]
    test rax, rax
    jz .binop_left_type
    ; Call sq_repeat(right=sequence, left=count): swap args
    xchg rdi, rsi
    call rax
    jmp .binop_have_result

.binop_not_smallint_left:
    mov rax, [rdi + PyObject.ob_type]
    jmp .binop_have_type
.binop_left_type:
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
    test rax, rax
    jz .binop_try_dunder
    jmp .binop_call_method

.use_float_methods:
    lea rax, [rel float_number_methods]

.binop_call_method:
    ; Get the specific method function pointer
    mov rax, [rax + r8]
    test rax, rax
    jz .binop_try_dunder

    ; Call the method: rdi=left (a), rsi=right (b) - already set
    call rax

.binop_have_result:
    ; rax = result
    ; Save result, DECREF operands (tag-aware)
    push rax                   ; save result (+8 shifts all BO_ offsets)
    mov rdi, [rsp + 8 + BO_RIGHT]
    mov rsi, [rsp + 8 + BO_RTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 8 + BO_LEFT]
    mov rsi, [rsp + 8 + BO_LTAG]
    DECREF_VAL rdi, rsi
    pop rax                    ; restore result
    add rsp, BO_SIZE           ; discard saved operands + tags

    ; Push result
    VPUSH_BRANCHLESS rax

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

.binop_try_dunder:
    ; Try dunder method on heaptype objects
    extern binop_dunder_table
    extern binop_rdunder_table
    extern dunder_call_2

    ; Check if left is heaptype
    mov rdi, [rsp + BO_LEFT]
    test rdi, rdi
    js .binop_try_right_dunder ; SmallInt has no dunders
    mov rax, [rdi + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .binop_try_right_dunder

    ; Map op code to dunder name
    mov eax, r9d
    cmp eax, 13
    jl .binop_dunder_idx
    sub eax, 13               ; inplace → base op
.binop_dunder_idx:
    lea rdx, [rel binop_dunder_table]
    mov rdx, [rdx + rax*8]
    test rdx, rdx
    jz .binop_try_right_dunder

    ; dunder_call_2(left, right, name)
    push r9                    ; save op code (+8 shifts BO_ offsets)
    mov rdi, [rsp + 8 + BO_LEFT]
    mov rsi, [rsp + 8 + BO_RIGHT]
    call dunder_call_2
    pop r9
    test rax, rax
    jnz .binop_have_result

.binop_try_right_dunder:
    ; Try reflected dunder on right operand
    mov rdi, [rsp + BO_RIGHT]
    test rdi, rdi
    js .binop_no_method
    mov rax, [rdi + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .binop_no_method

    mov eax, r9d
    cmp eax, 13
    jl .binop_rdunder_idx
    sub eax, 13
.binop_rdunder_idx:
    lea rdx, [rel binop_rdunder_table]
    mov rdx, [rdx + rax*8]
    test rdx, rdx
    jz .binop_no_method

    ; dunder_call_2(right, left, rname) — right is self for reflected
    mov rdi, [rsp + BO_RIGHT]
    mov rsi, [rsp + BO_LEFT]
    call dunder_call_2
    test rax, rax
    jnz .binop_have_result

.binop_no_method:
    ; No method found — raise TypeError
    extern raise_exception
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unsupported operand type(s)"
    call raise_exception

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
    ; Specialize: rewrite opcode to BINARY_OP_ADD_INT (211)
    mov byte [rbx - 2], 211
    ; No DECREF needed (SmallInt are not refcounted)
    VPUSH_INT rax
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
    ; Specialize: rewrite opcode to BINARY_OP_SUBTRACT_INT (212)
    mov byte [rbx - 2], 212
    ; No DECREF needed (SmallInt are not refcounted)
    VPUSH_INT rax
    add rbx, 2
    DISPATCH
END_FUNC op_binary_op

;; ============================================================================
;; op_compare_op - Rich comparison
;;
;; Python 3.12: comparison op = arg >> 4
;; ecx = arg, extract comparison op by shifting right 4.
;; Calls type's tp_richcompare(left, right, op).
;; Followed by 1 CACHE entry (2 bytes) that must be skipped.
;; ============================================================================
DEF_FUNC_BARE op_compare_op
    ; ecx = arg; comparison op = arg >> 4
    shr ecx, 4                 ; ecx = PY_LT/LE/EQ/NE/GT/GE (0-5)

    VPOP rsi                   ; rsi = right operand
    mov r8, [r13 + 8]         ; r8 = right tag
    VPOP rdi                   ; rdi = left operand
    mov r9, [r13 + 8]         ; r9 = left tag

    ; Fast path: both SmallInt — inline compare, no type dispatch
    mov rax, rdi
    and rax, rsi
    jns .cmp_slow_path         ; at least one is NOT SmallInt

    ; Both SmallInt: specialize to COMPARE_OP_INT (209)
    mov byte [rbx - 2], 209

    ; Both SmallInt: decode and compare
    mov rax, rdi
    shl rax, 1
    sar rax, 1                 ; decode left
    mov rdx, rsi
    shl rdx, 1
    sar rdx, 1                 ; decode right
    cmp rax, rdx               ; flags survive LEA + jmp [mem]
    lea r8, [rel .cmp_setcc_table]
    jmp [r8 + rcx*8]          ; 1 indirect branch on comparison op

.cmp_set_lt:
    setl al
    jmp .cmp_push_bool
.cmp_set_le:
    setle al
    jmp .cmp_push_bool
.cmp_set_eq:
    sete al
    jmp .cmp_push_bool
.cmp_set_ne:
    setne al
    jmp .cmp_push_bool
.cmp_set_gt:
    setg al
    jmp .cmp_push_bool
.cmp_set_ge:
    setge al
    ; fall through to .cmp_push_bool

.cmp_push_bool:
    movzx eax, al             ; eax = 0 or 1
    lea rdx, [rel bool_false]
    lea r8, [rel bool_true]
    test eax, eax
    cmovnz rdx, r8            ; rdx = bool_true if true, else bool_false
    inc qword [rdx + PyObject.ob_refcnt]
    VPUSH_PTR rdx
    add rbx, 2
    DISPATCH

section .data
align 8
.cmp_setcc_table:
    dq .cmp_set_lt             ; PY_LT = 0
    dq .cmp_set_le             ; PY_LE = 1
    dq .cmp_set_eq             ; PY_EQ = 2
    dq .cmp_set_ne             ; PY_NE = 3
    dq .cmp_set_gt             ; PY_GT = 4
    dq .cmp_set_ge             ; PY_GE = 5
section .text

.cmp_slow_path:
    ; Save operands + tags and comparison op
    ; Stack layout: [rsp+BO_RIGHT], [rsp+BO_RTAG], [rsp+BO_LEFT], [rsp+BO_LTAG]
    push r9                    ; save left tag
    push rdi                   ; save left
    push r8                    ; save right tag
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
    mov r9, rax                 ; r9 = type (save for dunder check)
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jnz .cmp_do_call

    ; No tp_richcompare — try dunder on heaptype
    mov rdx, [r9 + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .cmp_identity

    ; Map compare op to dunder name via lookup table
    extern cmp_dunder_table
    extern dunder_call_2
    lea rax, [rel cmp_dunder_table]
    movsxd rdx, ecx
    mov rdx, [rax + rdx*8]     ; rdx = dunder name C string

    ; Save ecx (comparison op) since dunder_call_2 clobbers it
    push rcx
    ; dunder_call_2(self=left, other=right, name)
    ; rdi = left (still set from above)
    ; rsi = right (still set)
    call dunder_call_2
    pop rcx

    test rax, rax
    jz .cmp_identity            ; dunder not found → identity fallback
    jmp .cmp_do_call_result     ; rax = result object

.cmp_use_float:
    extern float_compare
    lea rax, [rel float_compare]

.cmp_do_call:

    ; Call tp_richcompare(left, right, op)
    ; rdi = left, rsi = right (already set)
    mov edx, ecx               ; edx = comparison op
    call rax
    ; rax = result (a bool object)

.cmp_do_call_result:
    ; Save result, DECREF operands (tag-aware)
    push rax                   ; save result (+8 shifts all BO_ offsets)
    mov rdi, [rsp + 8 + BO_RIGHT]
    mov rsi, [rsp + 8 + BO_RTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 8 + BO_LEFT]
    mov rsi, [rsp + 8 + BO_LTAG]
    DECREF_VAL rdi, rsi
    pop rax                    ; restore result
    add rsp, BO_SIZE           ; discard saved operands + tags

    ; Push result
    VPUSH_BRANCHLESS rax

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

.cmp_identity:
    ; Fallback: identity comparison (pointer equality)
    mov rsi, [rsp + BO_RIGHT]
    mov rdi, [rsp + BO_LEFT]
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
    ; DECREF both operands (tag-aware), push False
    mov rdi, [rsp + BO_LEFT]
    mov rsi, [rsp + BO_LTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + BO_RIGHT]
    mov rsi, [rsp + BO_RTAG]
    DECREF_VAL rdi, rsi
    add rsp, BO_SIZE
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    VPUSH_PTR rax
    add rbx, 2
    DISPATCH
.cmp_id_true:
    ; DECREF both operands (tag-aware), push True
    mov rdi, [rsp + BO_LEFT]
    mov rsi, [rsp + BO_LTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + BO_RIGHT]
    mov rsi, [rsp + BO_RTAG]
    DECREF_VAL rdi, rsi
    add rsp, BO_SIZE
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    VPUSH_PTR rax
    add rbx, 2
    DISPATCH
END_FUNC op_compare_op

;; ============================================================================
;; op_unary_negative - Negate TOS
;;
;; Calls type's nb_negative from tp_as_number.
;; ============================================================================
DEF_FUNC_BARE op_unary_negative
    VPOP rdi                   ; rdi = operand
    mov r8, [r13 + 8]         ; r8 = operand tag

    ; Save operand + tag for DECREF after call
    push r8
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

    ; DECREF old operand (tag-aware)
    push rax                   ; save result on machine stack
    mov rdi, [rsp + 8]        ; rdi = old operand
    mov rsi, [rsp + 16]       ; rsi = operand tag
    DECREF_VAL rdi, rsi
    pop rax                    ; restore result
    add rsp, 16                ; discard saved operand + tag

    ; Push result
    VPUSH_BRANCHLESS rax
    DISPATCH
END_FUNC op_unary_negative

;; ============================================================================
;; op_unary_invert - Bitwise NOT of TOS (~x)
;;
;; Calls type's nb_invert from tp_as_number.
;; ============================================================================
DEF_FUNC_BARE op_unary_invert
    VPOP rdi                   ; rdi = operand
    mov r8, [r13 + 8]         ; r8 = operand tag
    push r8
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
    mov rsi, [rsp + 16]       ; tag
    DECREF_VAL rdi, rsi
    pop rax
    add rsp, 16
    VPUSH_BRANCHLESS rax
    DISPATCH
END_FUNC op_unary_invert

;; ============================================================================
;; op_unary_not - Logical NOT of TOS
;;
;; Calls obj_is_true, then pushes the inverted boolean.
;; ============================================================================
DEF_FUNC_BARE op_unary_not
    VPOP rdi                   ; rdi = operand
    mov r8, [r13 + 8]         ; r8 = operand tag

    ; Save operand + tag for DECREF
    push r8
    push rdi

    ; Call obj_is_true(operand) -> 0 or 1
    call obj_is_true
    push rax                   ; save truthiness result

    ; DECREF operand (tag-aware)
    mov rdi, [rsp + 8]        ; reload operand
    mov rsi, [rsp + 16]       ; tag
    DECREF_VAL rdi, rsi
    pop rax                    ; restore truthiness
    add rsp, 16                ; discard saved operand + tag

    ; NOT inverts: if truthy (1), push False; if falsy (0), push True
    test eax, eax
    jnz .push_false
    lea rax, [rel bool_true]
    jmp .push_bool
.push_false:
    lea rax, [rel bool_false]
.push_bool:
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_unary_not

;; ============================================================================
;; op_pop_jump_if_false - Pop TOS, jump if falsy
;;
;; Python 3.12: arg is the absolute target offset in instruction words
;; (2-byte units from start of co_code).
;; ============================================================================
DEF_FUNC_BARE op_pop_jump_if_false
    ; Save arg (target offset) before call
    push rcx                   ; save target offset on machine stack

    VPOP rdi                   ; rdi = value to test
    mov r8, [r13 + 8]         ; r8 = value tag

    ; Save value + tag for DECREF
    push r8
    push rdi

    ; Call obj_is_true(value) -> 0 (false) or 1 (true)
    call obj_is_true
    push rax                   ; save truthiness on machine stack

    ; DECREF the popped value (tag-aware)
    mov rdi, [rsp + 8]        ; reload value
    mov rsi, [rsp + 16]       ; tag
    DECREF_VAL rdi, rsi
    pop rax                    ; restore truthiness
    add rsp, 16                ; discard saved value + tag
    pop rcx                    ; restore target offset

    ; If false (result == 0), jump to target
    test eax, eax
    jnz .no_jump

    ; Jump: relative from current rbx (delta in instruction words)
    lea rbx, [rbx + rcx*2]

.no_jump:
    DISPATCH
END_FUNC op_pop_jump_if_false

;; ============================================================================
;; op_pop_jump_if_true - Pop TOS, jump if truthy
;; ============================================================================
DEF_FUNC_BARE op_pop_jump_if_true
    ; Save arg (delta in instruction words)
    push rcx                   ; save target offset on machine stack

    VPOP rdi
    mov r8, [r13 + 8]         ; r8 = value tag

    ; Save value + tag for DECREF
    push r8
    push rdi

    ; Call obj_is_true(value)
    call obj_is_true
    push rax                   ; save truthiness on machine stack

    ; DECREF the popped value (tag-aware)
    mov rdi, [rsp + 8]        ; reload value
    mov rsi, [rsp + 16]       ; tag
    DECREF_VAL rdi, rsi
    pop rax                    ; restore truthiness
    add rsp, 16                ; discard saved value + tag
    pop rcx                    ; restore target offset

    ; If true (result != 0), jump to target
    test eax, eax
    jz .no_jump

    ; Jump: relative from current rbx
    lea rbx, [rbx + rcx*2]

.no_jump:
    DISPATCH
END_FUNC op_pop_jump_if_true

;; ============================================================================
;; op_pop_jump_if_none - Pop TOS, jump if None
;; ============================================================================
DEF_FUNC_BARE op_pop_jump_if_none
    VPOP rax                   ; rax = value
    mov r8, [r13 + 8]         ; r8 = value tag

    ; Compare with none_singleton
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    jne .not_none

    ; IS None: save jump offset, DECREF, jump
    push rcx                   ; save jump offset
    mov rsi, r8
    DECREF_VAL rax, rsi
    pop rcx                    ; restore jump offset
    lea rbx, [rbx + rcx*2]
    DISPATCH

.not_none:
    ; NOT None: just DECREF and continue
    mov rsi, r8
    DECREF_VAL rax, rsi
    DISPATCH
END_FUNC op_pop_jump_if_none

;; ============================================================================
;; op_pop_jump_if_not_none - Pop TOS, jump if NOT None
;; ============================================================================
DEF_FUNC_BARE op_pop_jump_if_not_none
    VPOP rax                   ; rax = value
    mov r8, [r13 + 8]         ; r8 = value tag

    ; Compare with none_singleton
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    je .is_none

    ; NOT None: save jump offset, DECREF, jump
    push rcx                   ; save jump offset
    mov rsi, r8
    DECREF_VAL rax, rsi
    pop rcx                    ; restore jump offset
    lea rbx, [rbx + rcx*2]
    DISPATCH

.is_none:
    ; IS None: just DECREF and continue
    mov rsi, r8
    DECREF_VAL rax, rsi
    DISPATCH
END_FUNC op_pop_jump_if_not_none

;; ============================================================================
;; op_jump_forward - Unconditional forward jump
;;
;; arg = number of instruction words to skip
;; Each instruction word is 2 bytes, so advance rbx by arg*2 bytes.
;; ============================================================================
DEF_FUNC_BARE op_jump_forward
    ; ecx = arg (instruction words to skip)
    lea rbx, [rbx + rcx*2]
    DISPATCH
END_FUNC op_jump_forward

;; ============================================================================
;; op_jump_backward - Unconditional backward jump
;;
;; arg = number of instruction words to go back
;; Subtract arg*2 bytes from rbx.
;; ============================================================================
DEF_FUNC_BARE op_jump_backward
    ; ecx = arg (instruction words to go back)
    shl ecx, 1                 ; ecx = arg * 2 (zero-extends to rcx)
    sub rbx, rcx
    DISPATCH
END_FUNC op_jump_backward

;; ============================================================================
;; op_format_value - Format a value for f-strings
;;
;; arg & 0x03: conversion (0=none, 1=!s, 2=!r, 3=!a)
;; arg & 0x04: format spec present on stack below value
;; Pops value (and optional fmt_spec), pushes formatted string.
;; ============================================================================
DEF_FUNC op_format_value, FV_FRAME

    mov [rbp - FV_ARG], rcx    ; save arg
    mov rax, rcx
    and eax, 4
    mov [rbp - FV_HASSPEC], rax ; has_fmt_spec
    mov qword [rbp - FV_SPEC], 0 ; fmt_spec ptr (0 if absent)
    mov qword [rbp - FV_STAG], 0 ; fmt_spec tag (0 if absent)

    ; If format spec present, pop it first
    ; Stack order: TOS = fmt_spec, TOS1 = value
    test qword [rbp - FV_HASSPEC], 4
    jz .fv_no_spec
    VPOP rax                   ; fmt_spec string
    mov rcx, [r13 + 8]        ; fmt_spec tag
    mov [rbp - FV_SPEC], rax   ; save fmt_spec
    mov [rbp - FV_STAG], rcx   ; save fmt_spec tag
.fv_no_spec:

    VPOP rdi                   ; value
    mov rax, [r13 + 8]        ; value tag
    mov [rbp - FV_VALUE], rdi  ; save value
    mov [rbp - FV_VTAG], rax   ; save value tag

    ; If format spec present AND value is float, use float_format_spec
    test qword [rbp - FV_HASSPEC], 4
    jz .fv_no_format_spec

    ; Check if value is a float
    extern float_type
    test rdi, rdi
    js .fv_no_format_spec      ; SmallInt → not float
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel float_type]
    cmp rax, rcx
    jne .fv_no_format_spec

    ; Float with format spec: call float_format_spec(float, spec_data, spec_len)
    extern float_format_spec
    ; rdi = float obj (still set)
    mov rax, [rbp - FV_SPEC]  ; fmt_spec string
    lea rsi, [rax + PyStrObject.data]  ; spec data
    mov rdx, [rax + PyStrObject.ob_size]  ; spec length
    call float_format_spec
    jmp .fv_have_result

.fv_no_format_spec:
    ; Apply conversion based on arg & 3
    mov rdi, [rbp - FV_VALUE]  ; reload value
    mov eax, [rbp - FV_ARG]
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

    ; DECREF original value (tag-aware)
    mov rdi, [rbp - FV_VALUE]
    mov rsi, [rbp - FV_VTAG]
    DECREF_VAL rdi, rsi

    ; DECREF fmt_spec if present (tag-aware)
    cmp qword [rbp - FV_SPEC], 0
    je .fv_push
    mov rdi, [rbp - FV_SPEC]
    mov rsi, [rbp - FV_STAG]
    DECREF_VAL rdi, rsi

.fv_push:
    pop rax                    ; result
    VPUSH_PTR rax
    leave
    DISPATCH
END_FUNC op_format_value

;; ============================================================================
;; op_build_string - Concatenate N strings from the stack
;;
;; ecx = number of string fragments
;; Pops ecx strings, concatenates in order, pushes result.
;; ============================================================================
DEF_FUNC op_build_string, BS_FRAME

    mov [rbp - BS_COUNT], rcx  ; count

    test ecx, ecx
    jz .bs_zero
    cmp ecx, 1
    je .bs_one

    ; General case: iterate and concatenate
    ; Pop all items, keeping base pointer
    mov rdi, rcx
    shl rdi, 4                 ; count * 16 bytes/slot
    sub r13, rdi               ; pop all at once (r13 = base of items)

    ; Start with first string
    mov rax, [r13]             ; first fragment (payload at slot base)
    INCREF rax                 ; we'll DECREF all originals later
    mov [rbp - BS_ACCUM], rax  ; accumulator

    ; Concatenate remaining
    mov rcx, 1                 ; start from index 1
.bs_loop:
    cmp rcx, [rbp - BS_COUNT]
    jge .bs_decref
    push rcx
    extern str_concat
    mov rdi, [rbp - BS_ACCUM] ; accumulator
    mov rax, rcx
    shl rax, 4                ; index * 16
    mov rsi, [r13 + rax]     ; next fragment (payload)
    call str_concat
    ; DECREF old accumulator
    push rax                   ; save new result
    mov rdi, [rbp - BS_ACCUM]
    DECREF_REG rdi
    pop rax
    mov [rbp - BS_ACCUM], rax  ; new accumulator
    pop rcx
    inc rcx
    jmp .bs_loop

.bs_decref:
    ; DECREF all original fragments
    xor ecx, ecx
.bs_decref_loop:
    cmp rcx, [rbp - BS_COUNT]
    jge .bs_push
    mov rax, rcx
    shl rax, 4                ; index * 16
    mov rdi, [r13 + rax]
    mov rsi, [r13 + rax + 8]  ; tag
    push rcx
    DECREF_VAL rdi, rsi
    pop rcx
    inc rcx
    jmp .bs_decref_loop

.bs_push:
    mov rax, [rbp - BS_ACCUM]
    VPUSH_PTR rax
    leave
    DISPATCH

.bs_zero:
    ; Empty f-string: push empty string
    extern str_from_cstr
    CSTRING rdi, ""
    call str_from_cstr
    VPUSH_PTR rax
    leave
    DISPATCH

.bs_one:
    ; Shortcut: 1 fragment, just leave it on stack
    leave
    DISPATCH
END_FUNC op_build_string

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
DEF_FUNC_BARE op_make_cell
    lea rdx, [rcx*8]              ; slot * 8 (×2 via SIB)

    ; Get current value
    mov rdi, [r12 + rdx*2 + PyFrame.localsplus]  ; rdi = current value (or NULL)

    ; Save slot offset
    push rdx

    ; cell_new(obj) - creates cell wrapping obj (INCREFs if non-NULL)
    call cell_new
    ; rax = new cell

    pop rdx

    ; XDECREF old value (cell_new already INCREFed it)
    mov rdi, [r12 + rdx*2 + PyFrame.localsplus]
    test rdi, rdi
    jz .mc_store
    push rax
    push rdx
    DECREF_REG rdi
    pop rdx
    pop rax

.mc_store:
    ; Store cell in localsplus slot (payload + tag)
    mov [r12 + rdx*2 + PyFrame.localsplus], rax
    mov qword [r12 + rdx*2 + PyFrame.localsplus + 8], TAG_PTR
    DISPATCH
END_FUNC op_make_cell

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
DEF_FUNC_BARE op_copy_free_vars
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

    ; Compute destination index: edx + r8d, then * 16
    mov r10d, edx
    add r10d, r8d
    shl r10, 4                     ; slot * 16 bytes
    mov [r12 + PyFrame.localsplus + r10], r9

    ; Set tag and INCREF if non-NULL
    test r9, r9
    jz .cfv_null_item
    INCREF r9
    mov qword [r12 + PyFrame.localsplus + r10 + 8], TAG_PTR
    jmp .cfv_next
.cfv_null_item:
    mov qword [r12 + PyFrame.localsplus + r10 + 8], TAG_NULL
.cfv_next:
    inc r8d
    jmp .cfv_loop

.cfv_done:
    DISPATCH
END_FUNC op_copy_free_vars

;; ============================================================================
;; op_return_generator - Create generator from current frame
;;
;; RETURN_GENERATOR (75): First instruction in a generator function.
;; Creates a PyGenObject holding the current frame, returns it from eval_frame.
;; The frame is NOT freed by func_call (instr_ptr != 0 signals this).
;; ============================================================================
DEF_FUNC_BARE op_return_generator
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
END_FUNC op_return_generator

;; ============================================================================
;; op_yield_value - Yield a value from generator
;;
;; YIELD_VALUE (150): Pop TOS (value to yield), save frame state,
;; return value from eval_frame. The generator is suspended.
;; ============================================================================
DEF_FUNC_BARE op_yield_value
    ; Pop the value to yield
    VPOP rax

    ; Save frame state for resumption
    mov [r12 + PyFrame.instr_ptr], rbx
    mov [r12 + PyFrame.stack_ptr], r13

    ; Return yielded value from eval_frame
    jmp eval_return
END_FUNC op_yield_value

;; ============================================================================
;; op_end_send - End of send operation
;;
;; END_SEND (5): Pop TOS1 (receiver/generator), keep TOS (value).
;; ============================================================================
DEF_FUNC_BARE op_end_send
    ; TOS = value, TOS1 = receiver
    VPOP rax                   ; rax = value (TOS)
    VPOP rdi                   ; rdi = receiver (TOS1)
    mov rsi, [r13 + 8]        ; rsi = receiver tag
    push rax                   ; save value (DECREF_VAL clobbers caller-saved)
    DECREF_VAL rdi, rsi        ; DECREF receiver (tag-aware)
    pop rax                    ; restore value
    VPUSH_BRANCHLESS rax       ; push value back
    DISPATCH
END_FUNC op_end_send

;; ============================================================================
;; op_send - Send value to generator/coroutine
;;
;; SEND (123): TOS = value_to_send, TOS1 = receiver (generator)
;; arg = jump offset (relative, used if generator exhausted)
;; Calls gen_send(receiver, value). If yielded: push result.
;; If exhausted (StopIteration): jump forward by arg.
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
extern gen_send
extern gen_type

DEF_FUNC op_send, SND_FRAME
    ; ecx = arg (jump offset in instructions for StopIteration)
    ; Stack: ... | receiver | sent_value |
    mov [rbp - SND_ARG], rcx   ; save arg

    VPOP rsi                   ; rsi = sent_value (TOS)
    mov rax, [r13 + 8]        ; sent_value tag
    mov [rbp - SND_SENT], rsi  ; save sent_value
    mov [rbp - SND_STAG], rax  ; save sent_value tag
    VPEEK rdi                  ; rdi = receiver (TOS1, stay on stack)
    mov [rbp - SND_RECV], rdi  ; save receiver

    ; Check if receiver is a generator with iternext
    test rdi, rdi
    js .send_error
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .send_error

    ; Check if sent value is None — use iternext, otherwise gen_send
    mov rsi, [rbp - SND_SENT]
    lea rcx, [rel none_singleton]
    cmp rsi, rcx
    je .send_use_iternext

    ; gen_send(receiver, value)
    mov rdi, [rbp - SND_RECV]
    call gen_send
    jmp .send_check_result

.send_use_iternext:
    ; tp_iternext(receiver)
    mov rdi, [rbp - SND_RECV]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax

.send_check_result:
    mov [rbp - SND_RESULT], rax ; save result

    ; DECREF sent value (tag-aware)
    mov rdi, [rbp - SND_SENT]
    mov rsi, [rbp - SND_STAG]
    DECREF_VAL rdi, rsi

    mov rax, [rbp - SND_RESULT]
    test rax, rax
    jz .send_exhausted

    ; Yielded: push result on top (receiver stays below)
    ; Stack becomes: ... | receiver | yielded_value |
    VPUSH_BRANCHLESS rax

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    leave
    DISPATCH

.send_exhausted:
    ; Generator exhausted. Push None as return value.
    ; Stack: ... | receiver | → becomes ... | receiver | None |
    ; Then jump to END_SEND which will handle cleanup.
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax

    ; Skip 1 CACHE entry = 2 bytes, then jump forward by arg * 2 bytes
    add rbx, 2
    mov rcx, [rbp - SND_ARG]
    lea rbx, [rbx + rcx*2]
    leave
    DISPATCH

.send_error:
    ; Unsupported receiver — just push None and continue
    mov rdi, [rbp - SND_SENT]
    mov rsi, [rbp - SND_STAG]
    DECREF_VAL rdi, rsi
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax
    add rbx, 2
    leave
    DISPATCH
END_FUNC op_send

;; ============================================================================
;; op_get_yield_from_iter - Get iterator for yield-from
;;
;; GET_YIELD_FROM_ITER (69): TOS should be an iterable.
;; If TOS is already a generator, leave it. Otherwise call iter().
;; ============================================================================
DEF_FUNC_BARE op_get_yield_from_iter
    ; TOS = iterable
    VPEEK rdi                  ; rdi = TOS (don't pop)

    ; If it's already a generator, done
    test rdi, rdi
    js .gyfi_call_iter
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel gen_type]
    cmp rax, rcx
    je .gyfi_done              ; already a generator, leave on stack

.gyfi_call_iter:
    ; Not a generator — call tp_iter to get an iterator
    VPOP rdi                   ; pop iterable
    mov r8, [r13 + 8]         ; iterable tag
    push r8                    ; save tag (deeper)
    push rdi                   ; save payload

    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .gyfi_error

    call rax                   ; tp_iter(iterable) -> iterator
    push rax                   ; save iterator

    ; DECREF original iterable (tag-aware)
    mov rdi, [rsp + 8]        ; iterable payload
    mov rsi, [rsp + 16]       ; iterable tag
    DECREF_VAL rdi, rsi

    pop rax                    ; restore iterator
    add rsp, 16                ; discard iterable payload + tag
    VPUSH_PTR rax              ; push iterator as new TOS

.gyfi_done:
    DISPATCH

.gyfi_error:
    add rsp, 16                ; discard iterable payload + tag
    extern exc_TypeError_type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception
END_FUNC op_get_yield_from_iter

;; ============================================================================
;; op_jump_backward_no_interrupt - Jump backward (no interrupt check)
;;
;; JUMP_BACKWARD_NO_INTERRUPT (134): Same as JUMP_BACKWARD for us.
;; ============================================================================
DEF_FUNC_BARE op_jump_backward_no_interrupt
    shl ecx, 1                 ; arg * 2 = byte offset (zero-extends to rcx)
    sub rbx, rcx
    DISPATCH
END_FUNC op_jump_backward_no_interrupt

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
DEF_FUNC_BARE op_call_intrinsic_1
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
    sub r13, 16
    mov rdi, [r13]
    mov rsi, [r13 + 8]
    DECREF_VAL rdi, rsi
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
    VPUSH_PTR rax

    ; DECREF list
    pop rdi
    DECREF_REG rdi

    DISPATCH
END_FUNC op_call_intrinsic_1

;; ============================================================================
;; op_get_len - Push len(TOS) without popping TOS
;;
;; Opcode 30: GET_LEN
;; Used by match statements: push len, keep original on stack.
;; ============================================================================
extern obj_len

DEF_FUNC_BARE op_get_len
    ; PEEK TOS (don't pop, 16 bytes/slot)
    mov rdi, [r13 - 16]
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
    VPUSH_INT rax
    DISPATCH

.gl_error:
    pop rdi
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object has no len()"
    call raise_exception
END_FUNC op_get_len

;; ============================================================================
;; op_setup_annotations - Create __annotations__ dict in locals
;;
;; Opcode 85: SETUP_ANNOTATIONS
;; ============================================================================
extern dict_new
extern dict_set
extern str_from_cstr

DEF_FUNC op_setup_annotations
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
    CSTRING rdi, "__annotations__"
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
END_FUNC op_setup_annotations

;; ============================================================================
;; op_load_locals - Push locals dict
;;
;; Opcode 87: LOAD_LOCALS
;; ============================================================================
DEF_FUNC_BARE op_load_locals
    mov rax, [r12 + PyFrame.locals]
    test rax, rax
    jz .ll_error
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
.ll_error:
    lea rdi, [rel exc_RuntimeError_type]
    CSTRING rsi, "no locals dict"
    call raise_exception
END_FUNC op_load_locals

;; ============================================================================
;; op_load_from_dict_or_globals - Load from dict on TOS, fallback to globals
;;
;; Opcode 175: LOAD_FROM_DICT_OR_GLOBALS
;; Used in class body comprehensions.
;; ============================================================================
extern dict_get

DEF_FUNC_BARE op_load_from_dict_or_globals
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
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_load_from_dict_or_globals

;; ============================================================================
;; op_load_from_dict_or_deref - Load from dict on TOS, fallback to cell deref
;;
;; Opcode 176: LOAD_FROM_DICT_OR_DEREF
;; Used in class bodies that access closure variables directly (e.g. val = x).
;; Pop dict from TOS. Try dict[name] first. If not found, fall back to
;; loading through cell at localsplus[arg] (same as LOAD_DEREF).
;; ============================================================================
global op_load_from_dict_or_deref

LFDOD_DICT  equ 8
LFDOD_ARG   equ 16
LFDOD_FRAME equ 16

DEF_FUNC op_load_from_dict_or_deref, LFDOD_FRAME
    mov [rbp - LFDOD_ARG], ecx    ; save arg (localsplus index)

    ; Get name from co_names
    mov rsi, [r15 + rcx*8]        ; name string

    ; Pop dict from TOS
    VPOP rdi
    mov [rbp - LFDOD_DICT], rdi   ; save dict

    ; Try dict first
    call dict_get
    test rax, rax
    jnz .lfdod_found

    ; Not in dict — fall back to cell deref (like LOAD_DEREF)
    mov ecx, [rbp - LFDOD_ARG]
    lea rax, [rcx*8]              ; slot * 8 (×2 via SIB)
    mov rax, [r12 + rax*2 + PyFrame.localsplus]  ; cell object
    test rax, rax
    jz .lfdod_error
    mov rax, [rax + PyCellObject.ob_ref]
    test rax, rax
    jz .lfdod_error

.lfdod_found:
    INCREF rax
    VPUSH_PTR rax
    leave
    DISPATCH

.lfdod_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "free variable referenced before assignment"
    call raise_exception
END_FUNC op_load_from_dict_or_deref

;; ============================================================================
;; op_match_mapping - Check if TOS is a mapping type
;;
;; Opcode 31: MATCH_MAPPING
;; Push True if TOS is dict/mapping, False otherwise. Don't pop TOS.
;; ============================================================================
extern dict_type

DEF_FUNC_BARE op_match_mapping
    mov rdi, [r13 - 16]           ; peek TOS (16 bytes/slot)
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
    VPUSH_PTR rax
    DISPATCH
.mm_false:
    lea rax, [rel bool_false]
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_match_mapping

;; ============================================================================
;; op_match_sequence - Check if TOS is a sequence type
;;
;; Opcode 32: MATCH_SEQUENCE
;; Push True if TOS is list/tuple/sequence (not str/bytes/dict). Don't pop TOS.
;; ============================================================================
extern tuple_type
extern str_type
extern bytes_type

DEF_FUNC_BARE op_match_sequence
    mov rdi, [r13 - 16]           ; peek TOS (16 bytes/slot)
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
    VPUSH_PTR rax
    DISPATCH
.ms_false:
    lea rax, [rel bool_false]
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_match_sequence

;; ============================================================================
;; op_match_keys - Match mapping keys
;;
;; Opcode 33: MATCH_KEYS
;; TOS = keys tuple, TOS1 = subject (mapping)
;; If all keys in tuple exist in subject, push tuple of values + True
;; Otherwise push False
;; ============================================================================
DEF_FUNC op_match_keys, MK_FRAME

    ; TOS = keys tuple, TOS1 = subject (16 bytes/slot)
    ; Peek at both — don't pop either! Push result on top.
    mov rax, [r13 - 16]           ; keys tuple (TOS)
    mov [rbp - MK_KEYS], rax
    mov rax, [r13 - 32]           ; subject (TOS1)
    mov [rbp - MK_SUBJ], rax

    ; Allocate values tuple
    mov rax, [rbp - MK_KEYS]
    mov rdi, [rax + PyTupleObject.ob_size]
    mov [rbp - MK_NKEYS], rdi     ; save nkeys
    call tuple_new
    mov [rbp - MK_VALS], rax      ; values tuple

    xor edx, edx                   ; index

.mk_loop:
    cmp rdx, [rbp - MK_NKEYS]
    jge .mk_success

    push rdx

    ; Get key
    mov rax, [rbp - MK_KEYS]
    lea rax, [rax + PyTupleObject.ob_item]
    mov rsi, [rax + rdx*8]        ; key

    ; Look up in subject
    mov rdi, [rbp - MK_SUBJ]
    call dict_get
    test rax, rax
    jz .mk_fail

    ; Store value in values tuple
    pop rdx
    push rdx
    INCREF rax
    mov rcx, [rbp - MK_VALS]
    mov [rcx + PyTupleObject.ob_item + rdx*8], rax

    pop rdx
    inc rdx
    jmp .mk_loop

.mk_success:
    ; Push values tuple on top (stack: subject, keys, values_tuple)
    mov rax, [rbp - MK_VALS]
    VPUSH_PTR rax
    jmp .mk_done

.mk_fail:
    pop rdx
    ; DECREF partial values tuple
    mov rdi, [rbp - MK_VALS]
    call obj_decref
    ; Push None on top to indicate failure (stack: subject, keys, None)
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax

.mk_done:
    leave
    DISPATCH
END_FUNC op_match_keys

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
DEF_FUNC_BARE op_call_intrinsic_2
    cmp ecx, 1
    je .ci2_prep_reraise

    ; For type parameter intrinsics, just keep TOS1 and discard TOS
    ; (a simplification — full type parameter support would need more)
    sub r13, 16
    mov rdi, [r13]
    mov rsi, [r13 + 8]
    DECREF_VAL rdi, rsi
    ; TOS1 stays
    DISPATCH

.ci2_prep_reraise:
    ; INTRINSIC_PREP_RERAISE: TOS = exc, TOS1 = traceback
    ; For now, discard traceback and keep exception
    VPOP rax                       ; exc
    VPOP rdi                       ; traceback
    mov rsi, [r13 + 8]            ; traceback tag
    push rax
    DECREF_VAL rdi, rsi
    pop rax
    VPUSH_BRANCHLESS rax
    DISPATCH
END_FUNC op_call_intrinsic_2

;; ============================================================================
;; op_binary_op_add_int - Specialized SmallInt add (opcode 211)
;;
;; Guard: both TOS and TOS1 must be SmallInt (bit 63 set).
;; On guard failure: deopt back to BINARY_OP (122).
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_op_add_int
    VPOP rsi                   ; right
    VPOP rdi                   ; left
    ; Guard: both SmallInt
    mov rax, rdi
    and rax, rsi
    jns .add_int_deopt
    ; Decode, add, check overflow
    mov rax, rdi
    shl rax, 1
    sar rax, 1
    mov rdx, rsi
    shl rdx, 1
    sar rdx, 1
    add rax, rdx
    jo .add_int_deopt_repush
    ; Encode as SmallInt
    bts rax, 63
    VPUSH_INT rax
    add rbx, 2                 ; skip CACHE
    DISPATCH
.add_int_deopt_repush:
    ; Overflow: re-push operands and deopt
    VPUSH_INT rdi
    VPUSH_INT rsi
.add_int_deopt:
    ; Rewrite opcode back to BINARY_OP (122)
    mov byte [rbx - 2], 122
    sub rbx, 2                 ; back up to re-execute as BINARY_OP
    DISPATCH
END_FUNC op_binary_op_add_int

;; ============================================================================
;; op_binary_op_sub_int - Specialized SmallInt subtract (opcode 212)
;;
;; Guard: both TOS and TOS1 must be SmallInt (bit 63 set).
;; On guard failure: deopt back to BINARY_OP (122).
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_op_sub_int
    VPOP rsi                   ; right
    VPOP rdi                   ; left
    ; Guard: both SmallInt
    mov rax, rdi
    and rax, rsi
    jns .sub_int_deopt
    ; Decode, sub, check overflow
    mov rax, rdi
    shl rax, 1
    sar rax, 1
    mov rdx, rsi
    shl rdx, 1
    sar rdx, 1
    sub rax, rdx
    jo .sub_int_deopt_repush
    ; Encode as SmallInt
    bts rax, 63
    VPUSH_INT rax
    add rbx, 2                 ; skip CACHE
    DISPATCH
.sub_int_deopt_repush:
    ; Overflow: re-push operands and deopt
    VPUSH_INT rdi
    VPUSH_INT rsi
.sub_int_deopt:
    ; Rewrite opcode back to BINARY_OP (122)
    mov byte [rbx - 2], 122
    sub rbx, 2                 ; back up to re-execute as BINARY_OP
    DISPATCH
END_FUNC op_binary_op_sub_int

;; ============================================================================
;; op_compare_op_int - Specialized SmallInt comparison (opcode 209)
;;
;; Guard: both TOS and TOS1 must be SmallInt (bit 63 set).
;; On guard failure: deopt back to COMPARE_OP (107).
;; ecx = arg (comparison op = arg >> 4)
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_compare_op_int
    shr ecx, 4                 ; ecx = comparison op (0-5)
    VPOP rsi                   ; right
    VPOP rdi                   ; left
    ; Guard: both SmallInt
    mov rax, rdi
    and rax, rsi
    jns .cmp_int_deopt
    ; Decode
    mov rax, rdi
    shl rax, 1
    sar rax, 1                 ; decoded left
    mov rdx, rsi
    shl rdx, 1
    sar rdx, 1                 ; decoded right
    cmp rax, rdx               ; flags survive LEA + jmp [mem]
    lea r8, [rel .ci_setcc_table]
    jmp [r8 + rcx*8]          ; 1 indirect branch on comparison op

.ci_set_lt:
    setl al
    jmp .ci_push_bool
.ci_set_le:
    setle al
    jmp .ci_push_bool
.ci_set_eq:
    sete al
    jmp .ci_push_bool
.ci_set_ne:
    setne al
    jmp .ci_push_bool
.ci_set_gt:
    setg al
    jmp .ci_push_bool
.ci_set_ge:
    setge al
    ; fall through to .ci_push_bool

.ci_push_bool:
    movzx eax, al             ; eax = 0 or 1
    lea rdx, [rel bool_false]
    lea r8, [rel bool_true]
    test eax, eax
    cmovnz rdx, r8            ; rdx = bool_true if true, else bool_false
    inc qword [rdx + PyObject.ob_refcnt]
    VPUSH_PTR rdx
    add rbx, 2                ; skip CACHE
    DISPATCH

section .data
align 8
.ci_setcc_table:
    dq .ci_set_lt              ; PY_LT = 0
    dq .ci_set_le              ; PY_LE = 1
    dq .ci_set_eq              ; PY_EQ = 2
    dq .ci_set_ne              ; PY_NE = 3
    dq .ci_set_gt              ; PY_GT = 4
    dq .ci_set_ge              ; PY_GE = 5
section .text
.cmp_int_deopt:
    ; Re-push operands
    VPUSH_BRANCHLESS rdi
    VPUSH_BRANCHLESS rsi
    ; Rewrite back to COMPARE_OP (107) and re-execute
    mov byte [rbx - 2], 107
    sub rbx, 2
    DISPATCH
END_FUNC op_compare_op_int
