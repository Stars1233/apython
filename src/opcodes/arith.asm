; opcodes/arith.asm - Arithmetic and comparison opcode handlers
;
; BINARY_OP and COMPARE_OP, the unary operators, and the specialized and fused
; int/float superinstructions the quickening pass emits in their place.
; binary_op_offsets lives here, next to the only handler that reads it.
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack top pointer (Value[], one 64-bit word per slot)
;   r14 = co_consts tuple data pointer (&tuple.ob_item[0])
;
; co_names is accessed via the LOAD_CO_NAMES macro (reads a global).
; ecx = opcode argument on entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

section .text

extern eval_dispatch
extern obj_is_true
extern bool_true
extern bool_false
extern int_type
extern float_type
extern bool_type
extern float_number_methods
extern raise_exception
extern exc_TypeError_type
extern obj_decref
extern eval_saved_r13
extern eval_saved_rbx
extern none_singleton
extern obj_dealloc
extern opcode_table
extern opcode_dispatch_table

;; Stack layout constants for binary_op / compare_op generic paths.
;; After 4 pushes: right, right_tag, left, left_tag
;; Offsets relative to rsp immediately after the 4 pushes.
BO_RIGHT equ 0
BO_RTAG  equ 8
BO_LEFT  equ 16
BO_LTAG  equ 24
BO_SIZE  equ 32

;; Stack layout constants for op_build_string (DEF_FUNC, 16 bytes).

;; Stack layout constants for op_send (DEF_FUNC, 48 bytes).

;; Stack layout constants for op_match_keys (DEF_FUNC, 32 bytes).

; --- moved to a sibling file by the split ---
extern op_send

section .text

;; ============================================================================
;; op_binary_op - Perform a binary operation
;;
;; ecx = NB_* argument (operation selector)
;; Pops right (b) then left (a), dispatches through type's tp_as_number.
;; Followed by 1 CACHE entry (2 bytes) that must be skipped.
;; ============================================================================
; binop_is_number(rdi = payload, rsi = tag) -> eax 0/1
; True for the three things float arithmetic may be coerced with: an int
; immediate, a float immediate, and a heap int or bool.
DEF_FUNC_BARE binop_is_number
    cmp rsi, TAG_SMALLINT
    je .bn_yes
    cmp rsi, TAG_FLOAT
    je .bn_yes
    test rsi, TAG_RC_BIT
    jz .bn_no
    test rdi, rdi
    jz .bn_no
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .bn_yes
    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .bn_yes
    lea rcx, [rel float_type]
    cmp rax, rcx
    je .bn_yes
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jnz .bn_yes
.bn_no:
    xor eax, eax
    ret
.bn_yes:
    mov eax, 1
    ret
END_FUNC binop_is_number

DEF_FUNC_BARE op_binary_op
    ; ecx = NB_* op code
    ; Save the op index before pops (VPOP doesn't clobber ecx)
    VPOP_VAL rsi, r8            ; rsi = right operand (b), r8 = right tag
    VPOP_VAL rdi, r9            ; rdi = left operand (a), r9 = left tag

    ; Bools are heap singletons shaped like PyIntObject, so they arrive as
    ; TAG_PTR and the ordinary int path handles them -- no tag rewriting.

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

    ; Fast path: SmallInt multiply (NB_MULTIPLY=5, NB_INPLACE_MULTIPLY=18)
    cmp ecx, 5                 ; NB_MULTIPLY
    je .binop_try_smallint_mul
    cmp ecx, 18                ; NB_INPLACE_MULTIPLY
    je .binop_try_smallint_mul

    ; Fast path: float truediv (NB_TRUE_DIVIDE=11, NB_INPLACE_TRUE_DIVIDE=24)
    cmp ecx, 11                ; NB_TRUE_DIVIDE
    je .binop_try_float_truediv
    cmp ecx, 24                ; NB_INPLACE_TRUE_DIVIDE
    je .binop_try_float_truediv

    ; Fast path: SmallInt floor divide (NB_FLOOR_DIVIDE=2, NB_INPLACE_FLOOR_DIVIDE=15)
    cmp ecx, 2                 ; NB_FLOOR_DIVIDE
    je .binop_try_smallint_fdiv
    cmp ecx, 15                ; NB_INPLACE_FLOOR_DIVIDE
    je .binop_try_smallint_fdiv

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

    ; Float coercion: if either operand is TAG_FLOAT, use float methods
    ; This handles int+float, float+int, float+float
    ; Skip for NB_REMAINDER (6) / NB_INPLACE_REMAINDER (19) when left is not float,
    ; because str % value should use str_mod, not float methods.
    ; ... but only when the *other* operand is a number too.  Coercing
    ; unconditionally meant "a" + 1.5 evaluated to 1.5 and [1] * 1.5 to 0.0,
    ; reading the string or the list as a double.
    cmp qword [rsp + BO_LTAG], TAG_FLOAT
    jne .binop_check_right_float
    mov rdi, [rsp + BO_RIGHT]
    mov rsi, [rsp + BO_RTAG]
    call binop_is_number
    mov rdi, [rsp + BO_LEFT]
    mov rsi, [rsp + BO_RIGHT]
    test eax, eax
    jz .no_float_coerce
    jmp .use_float_methods

.binop_check_right_float:
    cmp qword [rsp + BO_RTAG], TAG_FLOAT
    jne .no_float_coerce
    ; `"fmt" % 1.5` must reach str_mod rather than float division, and it does:
    ; the binop_is_number test below says no for a str.  Excluding NB_REMAINDER
    ; outright said no for an int as well, so `n % 2.0` went to int's
    ; nb_remainder with a float on the right and dereferenced it as a PyInt.
    mov rdi, [rsp + BO_LEFT]
    mov rsi, [rsp + BO_LTAG]
    call binop_is_number
    mov rdi, [rsp + BO_LEFT]
    mov rsi, [rsp + BO_RIGHT]
    test eax, eax
    jz .no_float_coerce
    jmp .use_float_methods

.no_float_coerce:
    ; For NB_ADD (0/13) and NB_MULTIPLY (5/18): if left is int/SmallInt
    ; and right has sq_concat/sq_repeat, use sequence method instead.
    ; This handles: 3 * "ab", 3 * [1,2], etc.
    cmp qword [rsp + BO_LTAG], TAG_SMALLINT
    jne .binop_not_smallint_left
    ; Left is SmallInt — check if right has sequence methods
    cmp r9d, 5              ; NB_MULTIPLY
    je .binop_try_right_seq
    cmp r9d, 18             ; NB_INPLACE_MULTIPLY
    je .binop_try_right_seq
    jmp .binop_left_type

.binop_try_right_seq:
    ; Check right operand's tp_as_sequence->sq_repeat
    cmp qword [rsp + BO_RTAG], TAG_SMALLINT
    je .binop_left_type
    ; Non-pointer guard: TAG_BOOL/TAG_NONE/TAG_FLOAT can't be sequences
    test qword [rsp + BO_RTAG], TAG_RC_BIT
    jz .binop_left_type
    mov rax, [rsi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .binop_left_type
    mov rax, [rax + PySequenceMethods.sq_repeat]
    test rax, rax
    jz .binop_left_type
    ; Call sq_repeat(right=sequence, left=count): swap args
    xchg rdi, rsi
    mov rdx, [rsp + BO_RTAG]    ; sequence tag (now the left argument)
    mov rcx, [rsp + BO_LTAG]    ; count tag
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    V_UNPACK rax, rdx           ; sq_repeat returns a Value
    jmp .binop_have_result

.binop_not_smallint_left:
    ; TAG_BOOL: route to int (int_unwrap handles TAG_BOOL)
    ; Non-pointer guard: TAG_NONE, TAG_FLOAT can't be dereferenced
    test qword [rsp + BO_LTAG], TAG_RC_BIT
    jz .binop_no_method
    ; Check if left has sq_repeat and right is int (e.g. tuple*3, list*3)
    ; Only for NB_MULTIPLY, not INPLACE (imul uses nb_imul/sq_inplace_repeat)
    cmp r9d, 5              ; NB_MULTIPLY
    je .binop_try_left_seq
    jmp .binop_left_seq_done
.binop_try_left_seq:
    cmp qword [rsp + BO_RTAG], TAG_SMALLINT
    jne .binop_left_seq_done
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .binop_left_seq_done
    mov rax, [rax + PySequenceMethods.sq_repeat]
    test rax, rax
    jz .binop_left_seq_done
    ; Call sq_repeat(left=sequence, right=count)
    ; rdi already = left (sequence), rsi already = right (count)
    mov rdx, [rsp + BO_LTAG]
    mov rcx, [rsp + BO_RTAG]    ; count tag (right operand)
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    V_UNPACK rax, rdx           ; sq_repeat returns a Value
    jmp .binop_have_result
.binop_left_seq_done:
    mov rax, [rdi + PyObject.ob_type]
    jmp .binop_have_type
.binop_left_type:
    ; Get type's tp_as_number method table from left operand
    ; SmallInt check: use saved left tag
    cmp qword [rsp + BO_LTAG], TAG_SMALLINT
    je .binop_smallint_type
    ; TAG_BOOL: route to int (int_unwrap handles TAG_BOOL)
    ; Non-pointer guard: TAG_NONE, TAG_FLOAT can't be dereferenced
    test qword [rsp + BO_LTAG], TAG_RC_BIT
    jz .binop_no_method
    mov rax, [rdi + PyObject.ob_type]
    jmp .binop_have_type
.binop_smallint_type:
    lea rax, [rel int_type]
    jmp .binop_have_type
.binop_have_type:
    push rax                   ; save type ptr for sq fallback
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jnz .binop_have_number
    pop rax                    ; restore type ptr
    jmp .binop_try_seq_fallback
.binop_have_number:
    add rsp, 8                 ; discard saved type ptr
    jmp .binop_call_method

.use_float_methods:
    lea rax, [rel float_number_methods]

.binop_call_method:
    ; Get the specific method function pointer
    mov rax, [rax + r8]
    test rax, rax
    jnz .binop_have_method

    ; If inplace slot was NULL, fall back to non-inplace slot
    cmp r9d, 13
    jl .binop_try_dunder        ; not inplace, no fallback
    ; Map inplace op to non-inplace offset
    mov ecx, r9d
    sub ecx, 13                 ; inplace → base op
    lea rdx, [rel binary_op_offsets]
    mov rdx, [rdx + rcx*8]     ; non-inplace offset
    ; Float coercion: if either operand is float, use float_number_methods
    ; (mirrors the initial float coercion at .use_float_methods)
    cmp qword [rsp + BO_LTAG], TAG_FLOAT
    je .binop_fallback_float
    cmp qword [rsp + BO_RTAG], TAG_FLOAT
    je .binop_fallback_float
    ; Reload type's tp_as_number
    cmp qword [rsp + BO_LTAG], TAG_SMALLINT
    je .binop_fallback_int
    test qword [rsp + BO_LTAG], TAG_RC_BIT
    jz .binop_try_dunder
    mov rax, [rdi + PyObject.ob_type]
    jmp .binop_fallback_have_type
.binop_fallback_float:
    lea rax, [rel float_number_methods]
    jmp .binop_fallback_have_methods
.binop_fallback_int:
    lea rax, [rel int_type]
    jmp .binop_fallback_have_type
.binop_fallback_have_type:
    mov rax, [rax + PyTypeObject.tp_as_number]
.binop_fallback_have_methods:
    test rax, rax
    jz .binop_try_dunder
    mov rax, [rax + rdx]
    test rax, rax
    jz .binop_try_dunder

.binop_have_method:

    ; Guard: if left is SmallInt/Bool and right is a heaptype (not int subclass),
    ; the int nb_* methods can't handle it. Skip to dunder dispatch.
    cmp qword [rsp + BO_LTAG], TAG_SMALLINT
    je .binop_guard_int_left
    jmp .binop_compat_ok

.binop_guard_int_left:
    ; Left is int/bool. Check if right is an incompatible heaptype.
    test qword [rsp + BO_RTAG], TAG_RC_BIT
    jz .binop_compat_ok          ; right not a heap pointer → compatible
    ; Right is a heap pointer (TAG_PTR)
    push rax                     ; save method ptr
    mov r10, [rsp + 8 + BO_RIGHT]
    mov r10, [r10 + PyObject.ob_type]
    test qword [r10 + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .binop_guard_ok           ; not heaptype → could be GMP int, proceed
    test qword [r10 + PyTypeObject.tp_flags], TYPE_FLAG_INT_SUBCLASS
    jnz .binop_guard_ok          ; int subclass → int methods handle it
    ; Heaptype non-int-subclass → skip to dunders
    pop rax
    jmp .binop_try_dunder
.binop_guard_ok:
    pop rax

.binop_compat_ok:

.binop_do_call:
    ; Call the method: rdi = left Value, rsi = right Value
    mov rdx, [rsp + BO_LTAG]
    mov rcx, [rsp + BO_RTAG]
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    V_UNPACK rax, rdx           ; the nb_ slot returns a Value

.binop_have_result:
    ; rax = result payload, rdx = result tag
    ; Save result, DECREF operands (tag-aware)
    SAVE_FAT_RESULT            ; save (rax,rdx) result — shifts rsp refs by +16
    mov rdi, [rsp + 16 + BO_RIGHT]
    mov rsi, [rsp + 16 + BO_RTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 16 + BO_LEFT]
    mov rsi, [rsp + 16 + BO_LTAG]
    DECREF_VAL rdi, rsi
    RESTORE_FAT_RESULT
    add rsp, BO_SIZE           ; discard saved operands + tags

    ; Push result
    VPUSH_VAL rax, rdx

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

.binop_try_seq_fallback:
    ; rax = type ptr. Check if type has tp_as_sequence for ADD/MUL ops.
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .binop_try_dunder
    ; NB_ADD (0) or NB_INPLACE_ADD (13) → sq_concat / sq_inplace_concat
    cmp r9d, 0              ; NB_ADD
    je .binop_seq_concat
    cmp r9d, 13             ; NB_INPLACE_ADD
    je .binop_seq_concat
    ; NB_MULTIPLY (5) or NB_INPLACE_MULTIPLY (18) → sq_repeat
    cmp r9d, 5
    je .binop_seq_repeat_left
    cmp r9d, 18             ; NB_INPLACE_MULTIPLY
    je .binop_seq_repeat_left
    jmp .binop_try_dunder

.binop_seq_concat:
    mov rax, [rax + PySequenceMethods.sq_concat]
    test rax, rax
    jz .binop_try_dunder
    ; sq_concat(left, right): rdi=left, rsi=right already set
    mov rdx, [rsp + BO_LTAG]
    mov rcx, [rsp + BO_RTAG]
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    V_UNPACK rax, rdx           ; sq_concat returns a Value
    jmp .binop_have_result

.binop_seq_repeat_left:
    mov rax, [rax + PySequenceMethods.sq_repeat]
    test rax, rax
    jz .binop_try_dunder
    ; sq_repeat(left=sequence, right=count)
    mov rdx, [rsp + BO_LTAG]
    mov rcx, [rsp + BO_RTAG]
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    V_UNPACK rax, rdx           ; sq_repeat returns a Value
    jmp .binop_have_result

.binop_try_dunder:
    ; Try dunder method on heaptype objects
    extern binop_dunder_table
    extern binop_rdunder_table
    extern binop_inplace_dunder_table
    extern dunder_call_2
    extern dunder_lookup

    ; Check if left is heaptype
    cmp qword [rsp + BO_LTAG], TAG_SMALLINT
    je .binop_try_right_dunder ; SmallInt has no dunders
    ; Non-pointer guard: TAG_BOOL/TAG_NONE/TAG_FLOAT can't have dunders
    test qword [rsp + BO_LTAG], TAG_RC_BIT
    jz .binop_try_right_dunder
    mov rdi, [rsp + BO_LEFT]
    mov rax, [rdi + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .binop_try_right_dunder

    ; For inplace ops, try inplace dunder first
    cmp r9d, 13
    jl .binop_left_dunder

    ; --- Inplace dunder probe ---
    ; Look up inplace dunder on left's type via dunder_lookup
    push r9                    ; save op code (+8 shifts BO_ offsets)
    mov rdi, [rsp + 8 + BO_LEFT]
    mov rdi, [rdi + PyObject.ob_type]
    mov eax, r9d
    sub eax, 13
    lea rsi, [rel binop_inplace_dunder_table]
    mov rsi, [rsi + rax*8]    ; inplace dunder name
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    pop r9
    test edx, edx
    jz .binop_left_dunder      ; not found → fall back to regular dunder
    test edx, TAG_RC_BIT
    jz .binop_no_method        ; non-pointer: cannot be called
    IS_NONE rax, rcx
    je .binop_no_method        ; __i<op>__ = None blocks the fallback (TypeError)

    ; Inplace dunder exists and is callable — call via dunder_call_2
    push r9
    mov eax, r9d
    sub eax, 13
    lea rdx, [rel binop_inplace_dunder_table]
    mov rdx, [rdx + rax*8]    ; inplace dunder name
    mov rdi, [rsp + 8 + BO_LEFT]
    mov rsi, [rsp + 8 + BO_RIGHT]
    mov rcx, [rsp + 8 + BO_RTAG]
    call dunder_call_2
    V_UNPACK rax, rdx           ; returns a Value
    pop r9
    test edx, edx
    jnz .binop_have_result
    ; Inplace dunder call returned NULL unexpectedly — fall through to regular

.binop_left_dunder:
    ; Map op code to regular dunder name
    mov eax, r9d
    cmp eax, 13
    jl .binop_dunder_idx
    sub eax, 13               ; inplace → base op
.binop_dunder_idx:
    lea rdx, [rel binop_dunder_table]
    mov rdx, [rdx + rax*8]
    test rdx, rdx
    jz .binop_try_right_dunder

    ; dunder_call_2(left, right, name, right_tag)
    push r9                    ; save op code (+8 shifts BO_ offsets)
    mov rdi, [rsp + 8 + BO_LEFT]
    mov rsi, [rsp + 8 + BO_RIGHT]
    mov rcx, [rsp + 8 + BO_RTAG]   ; other_tag = right's tag
    call dunder_call_2
    V_UNPACK rax, rdx           ; returns a Value
    pop r9
    test edx, edx
    jnz .binop_have_result

.binop_try_right_dunder:
    ; Try reflected dunder on right operand
    cmp qword [rsp + BO_RTAG], TAG_SMALLINT
    je .binop_no_method
    ; Non-pointer guard: TAG_BOOL/TAG_NONE/TAG_FLOAT can't have dunders
    test qword [rsp + BO_RTAG], TAG_RC_BIT
    jz .binop_no_method
    mov rdi, [rsp + BO_RIGHT]
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

    ; dunder_call_2(right, left, rname, left_tag) — right is self for reflected
    mov rdi, [rsp + BO_RIGHT]
    mov rsi, [rsp + BO_LEFT]
    mov rcx, [rsp + BO_LTAG]       ; other_tag = left's tag
    call dunder_call_2
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jnz .binop_have_result

.binop_no_method:
    ; No method found — raise TypeError
    extern raise_exception
    RAISE exc_TypeError_type, "unsupported operand type(s)"

.binop_try_smallint_add:
    ; Check both TAG_SMALLINT
    cmp r9d, TAG_SMALLINT
    jne .binop_try_float_add
    cmp r8d, TAG_SMALLINT
    jne .binop_generic

    ; Both SmallInt: decode, add, check overflow
    mov rax, rdi
    mov rdx, rsi
    add rax, rdx
    jo .binop_generic          ; overflow → fall back to generic
    ; Specialize: rewrite opcode to BINARY_OP_ADD_INT (211)
    mov byte [rbx - 2], 211
    VPUSH_INT rax, r15
    add rbx, 2
    DISPATCH

.binop_try_float_add:
    cmp r9d, TAG_FLOAT
    jne .binop_generic
    cmp r8d, TAG_FLOAT
    jne .binop_generic
    ; Both float: inline add
    mov byte [rbx - 2], 217   ; BINARY_OP_ADD_FLOAT
    movq xmm0, rdi
    movq xmm1, rsi
    addsd xmm0, xmm1
    movq rax, xmm0
    VPUSH_FLOAT rax, r15
    add rbx, 2
    DISPATCH

.binop_try_smallint_sub:
    ; Check both TAG_SMALLINT
    cmp r9d, TAG_SMALLINT
    jne .binop_try_float_sub
    cmp r8d, TAG_SMALLINT
    jne .binop_generic

    ; Both SmallInt: decode, subtract, check overflow
    mov rax, rdi
    mov rdx, rsi
    sub rax, rdx
    jo .binop_generic          ; overflow → fall back to generic
    ; Specialize: rewrite opcode to BINARY_OP_SUBTRACT_INT (212)
    mov byte [rbx - 2], 212
    VPUSH_INT rax, r15
    add rbx, 2
    DISPATCH

.binop_try_float_sub:
    cmp r9d, TAG_FLOAT
    jne .binop_generic
    cmp r8d, TAG_FLOAT
    jne .binop_generic
    ; Both float: inline sub
    mov byte [rbx - 2], 218   ; BINARY_OP_SUB_FLOAT
    movq xmm0, rdi
    movq xmm1, rsi
    subsd xmm0, xmm1
    movq rax, xmm0
    VPUSH_FLOAT rax, r15
    add rbx, 2
    DISPATCH

.binop_try_smallint_mul:
    ; Check both TAG_SMALLINT
    cmp r9d, TAG_SMALLINT
    jne .binop_try_float_mul
    cmp r8d, TAG_SMALLINT
    jne .binop_generic

    ; Both SmallInt: multiply, check overflow
    mov rax, rdi
    imul rsi
    jo .binop_generic          ; overflow → fall back to generic
    ; Specialize: rewrite opcode to BINARY_OP_MULTIPLY_INT (221)
    mov byte [rbx - 2], 221
    VPUSH_INT rax, r15
    add rbx, 2
    DISPATCH

.binop_try_float_mul:
    cmp r9d, TAG_FLOAT
    jne .binop_generic
    cmp r8d, TAG_FLOAT
    jne .binop_generic
    ; Both float: inline mul
    mov byte [rbx - 2], 219   ; BINARY_OP_MUL_FLOAT
    movq xmm0, rdi
    movq xmm1, rsi
    mulsd xmm0, xmm1
    movq rax, xmm0
    VPUSH_FLOAT rax, r15
    add rbx, 2
    DISPATCH

.binop_try_float_truediv:
    cmp r9d, TAG_FLOAT
    jne .binop_generic
    cmp r8d, TAG_FLOAT
    jne .binop_generic
    ; Both float: check for division by zero
    movq xmm1, rsi
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    je .binop_generic          ; zero divisor → generic path raises ZeroDivisionError
    ; Inline truediv
    mov byte [rbx - 2], 220   ; BINARY_OP_TRUEDIV_FLOAT
    movq xmm0, rdi
    divsd xmm0, xmm1
    movq rax, xmm0
    VPUSH_FLOAT rax, r15
    add rbx, 2
    DISPATCH

.binop_try_smallint_fdiv:
    ; Check both TAG_SMALLINT
    cmp r9d, TAG_SMALLINT
    jne .binop_generic
    cmp r8d, TAG_SMALLINT
    jne .binop_generic
    test rsi, rsi
    jz .binop_generic          ; zero divisor → generic raises error
    mov rax, rdi
    cqo
    idiv rsi                    ; rax=quotient, rdx=remainder
    ; Floor: if remainder != 0 and signs differ, subtract 1
    test rdx, rdx
    jz .fdiv_exact
    mov rcx, rdi
    xor rcx, rsi
    jns .fdiv_exact             ; same sign → truncation == floor
    dec rax
.fdiv_exact:
    mov byte [rbx - 2], 222    ; specialize to BINARY_OP_FLOORDIV_INT
    VPUSH_INT rax, r15
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

    VPOP_VAL rsi, r8            ; rsi = right operand, r8 = right tag
    VPOP_VAL rdi, r9            ; rdi = left operand, r9 = left tag

    ; Fast path: both SmallInt — inline compare, no type dispatch
    cmp r9d, TAG_SMALLINT
    jne .cmp_slow_path
    cmp r8d, TAG_SMALLINT
    jne .cmp_slow_path

    ; Both SmallInt: specialize — check if next opcode is POP_JUMP_IF_FALSE/TRUE
    ; rbx points past 2-byte instruction; CACHE at [rbx], next opcode at [rbx+2]
    cmp byte [rbx + 2], 114    ; POP_JUMP_IF_FALSE
    je .cmp_specialize_jump_false
    cmp byte [rbx + 2], 115    ; POP_JUMP_IF_TRUE
    je .cmp_specialize_jump_true
    mov byte [rbx - 2], 209   ; plain COMPARE_OP_INT
    jmp .cmp_do_compare
.cmp_specialize_jump_false:
    mov byte [rbx - 2], 215   ; COMPARE_OP_INT_JUMP_FALSE
    jmp .cmp_do_compare
.cmp_specialize_jump_true:
    mov byte [rbx - 2], 216   ; COMPARE_OP_INT_JUMP_TRUE
    ; fall through

.cmp_do_compare:
    ; Both SmallInt: decode and compare
    mov rax, rdi
    mov rdx, rsi
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
    VPUSH_BOOL rax             ; (0/1, TAG_BOOL) — no INCREF needed
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

    ; Float coercion: if either operand is TAG_FLOAT, use float_compare
    cmp r9d, TAG_FLOAT
    je .cmp_use_float
    cmp r8d, TAG_FLOAT
    je .cmp_use_float

.cmp_no_float:
    ; Get type's tp_richcompare
    cmp r9d, TAG_SMALLINT
    je .cmp_smallint_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .cmp_have_type
.cmp_smallint_type:
    lea rax, [rel int_type]
    jmp .cmp_have_type
.cmp_bool_type:
    lea rax, [rel bool_type]
    jmp .cmp_have_type
.cmp_none_type:
    lea rax, [rel none_type]
    jmp .cmp_have_type
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
    ; dunder_call_2(self=left, other=right, name, right_tag)
    ; rdi = left (still set from above)
    ; rsi = right (still set)
    mov ecx, [rsp + 16]            ; right_tag from stack
    call dunder_call_2
    V_UNPACK rax, rdx           ; returns a Value
    pop rcx

    test edx, edx
    jnz .cmp_have_dunder_result ; got result, proceed

    ; Dunder not found. If NE, try __eq__ + negate (auto-derivation)
    cmp ecx, PY_NE
    jne .cmp_identity           ; not NE → identity fallback

    ; Every class inherits object's comparison dunders now, and object's
    ; answer for two different objects is NotImplemented.  That is not a
    ; result: it means "no opinion", so the identity fallback below is what
    ; must run, exactly as when the dunder was absent.
.cmp_have_dunder_result:
    cmp edx, TAG_PTR
    jne .cmp_do_call_result
    lea r8, [rel notimpl_singleton]
    cmp rax, r8
    jne .cmp_do_call_result
    push rcx
    mov rdi, rax
    extern obj_decref
    call obj_decref
    pop rcx
    cmp ecx, PY_NE
    je .cmp_ne_from_eq
    jmp .cmp_identity

.cmp_ne_from_eq:
    ; Try __eq__ on left's heaptype
    mov rdi, [rsp + BO_LEFT]
    mov rsi, [rsp + BO_RIGHT]
    lea rax, [rel cmp_dunder_table]
    mov rdx, [rax + PY_EQ*8]   ; rdx = "__eq__" name
    push rcx
    mov ecx, [rsp + 8 + BO_RTAG]  ; right_tag (+8 for push rcx)
    call dunder_call_2
    V_UNPACK rax, rdx           ; returns a Value
    pop rcx
    test edx, edx
    jz .cmp_identity            ; __eq__ also not found → identity
    cmp edx, TAG_PTR
    jne .cmp_ne_negate
    extern notimpl_singleton
    lea r8, [rel notimpl_singleton]
    cmp rax, r8
    jne .cmp_ne_negate
    push rcx
    mov rdi, rax
    call obj_decref
    pop rcx
    jmp .cmp_identity
.cmp_ne_negate:

    ; Negate __eq__ result: if True → False, if False → True
    ; Check for TAG_PTR bool (bool_true/bool_false singletons)
    cmp edx, TAG_PTR
    jne .cmp_do_call_result     ; non-bool result, just use as-is
    extern bool_true
    extern bool_false
    lea rcx, [rel bool_true]
    cmp rax, rcx
    je .ne_return_false
    lea rcx, [rel bool_false]
    cmp rax, rcx
    je .ne_return_true
    jmp .cmp_do_call_result     ; not a bool ptr → use as-is
.ne_negate_tag_bool:
    xor eax, 1                  ; flip 0↔1 for TAG_BOOL
    jmp .cmp_do_call_result
.ne_return_false:
    lea rax, [rel bool_false]
    jmp .cmp_do_call_result
.ne_return_true:
    lea rax, [rel bool_true]
    jmp .cmp_do_call_result

.cmp_use_float:
    extern float_compare
    ; float_compare(left, right, op, left_tag, right_tag)
    mov edx, ecx               ; edx = comparison op
    mov ecx, [rsp + BO_LTAG]   ; ecx = left_tag
    mov r8d, [rsp + BO_RTAG]   ; r8d = right_tag
    push rdx                   ; save comparison op (like .cmp_do_call does)
    V_PACK rdi, rcx             ; left  -> Value
    V_PACK rsi, r8              ; right -> Value
    call float_compare
    V_UNPACK rax, rdx           ; float_compare returns a Value
    ; Check for NotImplemented (NULL return = tag 0)
    test edx, edx
    jz .cmp_try_right          ; try right operand's tp_richcompare
    add rsp, 8                 ; discard saved comparison op
    jmp .cmp_do_call_result

.cmp_do_call:

    ; Call tp_richcompare(left, right, op, left_tag, right_tag)
    ; rdi = left, rsi = right (already set)
    mov edx, ecx               ; edx = comparison op
    mov rcx, [rsp + BO_LTAG]   ; rcx = left_tag
    mov r8, [rsp + BO_RTAG]    ; r8 = right_tag
    push rdx                   ; save comparison op before call
    V_PACK rdi, rcx             ; left  -> Value
    V_PACK rsi, r8              ; right -> Value
    call rax
    V_UNPACK rax, rdx           ; tp_richcompare returns a Value
    ; rax = result payload, edx = result tag
    ; Check for NotImplemented (NULL return = tag 0)
    test edx, edx
    jz .cmp_try_right
    add rsp, 8                 ; discard saved comparison op

.cmp_do_call_result:
    ; Save result, DECREF operands (tag-aware)
    SAVE_FAT_RESULT            ; save (rax,rdx) result — shifts rsp refs by +16
    mov rdi, [rsp + 16 + BO_RIGHT]
    mov rsi, [rsp + 16 + BO_RTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 16 + BO_LEFT]
    mov rsi, [rsp + 16 + BO_LTAG]
    DECREF_VAL rdi, rsi
    RESTORE_FAT_RESULT
    add rsp, BO_SIZE           ; discard saved operands + tags

    ; Push result
    VPUSH_VAL rax, rdx

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

.cmp_try_right:
    ; Left's tp_richcompare returned NotImplemented (NULL).
    ; Try right operand's tp_richcompare with swapped args and swapped op.
    ; Stack: [rsp]=saved_op, [rsp+8+BO_*]=operands
    pop rcx                    ; ecx = original comparison op

    ; Resolve right operand's type
    mov rdi, [rsp + BO_RIGHT] ; right payload (will become left arg)
    mov r8, [rsp + BO_RTAG]   ; right tag
    cmp r8d, TAG_SMALLINT
    je .cmp_right_int
    cmp r8d, TAG_FLOAT
    je .cmp_right_float
    mov rax, [rdi + PyObject.ob_type]
    jmp .cmp_right_have_type
.cmp_right_int:
    lea rax, [rel int_type]
    jmp .cmp_right_have_type
.cmp_right_float:
    lea rax, [rel float_type]
    jmp .cmp_right_have_type
.cmp_right_bool:
    extern bool_type
    lea rax, [rel bool_type]
    jmp .cmp_right_have_type
.cmp_right_none:
    extern none_type
    lea rax, [rel none_type]
.cmp_right_have_type:
    mov r9, rax                ; r9 = right type
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jnz .cmp_right_do_call

    ; No tp_richcompare — try dunder on heaptype (right side)
    mov rdx, [r9 + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .cmp_identity           ; not a heaptype, no dunder → identity

    ; Swap comparison op: LT↔GT, LE↔GE, EQ↔EQ, NE↔NE
    lea rax, [rel .cmp_swap_table]
    movsxd rdx, ecx
    mov edx, [rax + rdx*4]    ; edx = swapped op

    ; Map swapped op to dunder name
    extern cmp_dunder_table
    extern dunder_call_2
    lea rax, [rel cmp_dunder_table]
    movsxd rdx, edx
    mov rdx, [rax + rdx*8]    ; rdx = dunder name C string

    ; dunder_call_2(self=right, other=left, name, other_tag)
    ; rdi = right (already set)
    mov rsi, [rsp + BO_LEFT]   ; other = left payload
    mov ecx, [rsp + BO_LTAG]   ; other_tag = left's tag
    call dunder_call_2
    V_UNPACK rax, rdx           ; returns a Value

    ; Check if dunder returned NULL
    test edx, edx
    jz .cmp_identity           ; no dunder → identity fallback
    jmp .cmp_do_call_result

.cmp_right_do_call:
    ; Swap comparison op: LT↔GT, LE↔GE, EQ↔EQ, NE↔NE
    ; Save original op for potential identity fallback
    push rcx                   ; [rsp] = original comparison op
    lea r9, [rel .cmp_swap_table]
    movsxd rcx, ecx
    mov ecx, [r9 + rcx*4]     ; ecx = swapped op

    ; Call tp_richcompare(right, left, swapped_op, right_tag, left_tag)
    ; rdi = right (already set above)
    mov rsi, [rsp + 8 + BO_LEFT]  ; rsi = left (becomes right arg) (+8 for push)
    mov edx, ecx               ; swapped op
    mov rcx, [rsp + 8 + BO_RTAG]  ; right_tag (now left_tag arg)
    mov r8, [rsp + 8 + BO_LTAG]   ; left_tag (now right_tag arg)
    V_PACK rdi, rcx             ; left  -> Value
    V_PACK rsi, r8              ; right -> Value
    call rax
    V_UNPACK rax, rdx           ; tp_richcompare returns a Value
    ; Check for NotImplemented again
    test edx, edx
    jnz .cmp_try_right_ok
    ; Both sides returned NotImplemented → identity fallback
    pop rcx                    ; restore original comparison op (ecx) for .cmp_identity
    jmp .cmp_identity
.cmp_try_right_ok:
    add rsp, 8                 ; discard saved original op
    jmp .cmp_do_call_result    ; got a result, proceed normally

section .data
align 4
.cmp_swap_table:
    dd 4                       ; PY_LT(0) → PY_GT(4)
    dd 5                       ; PY_LE(1) → PY_GE(5)
    dd 2                       ; PY_EQ(2) → PY_EQ(2)
    dd 3                       ; PY_NE(3) → PY_NE(3)
    dd 0                       ; PY_GT(4) → PY_LT(0)
    dd 1                       ; PY_GE(5) → PY_LE(1)
section .text

.cmp_identity:
    ; Fallback: identity comparison (pointer equality)
    ; For ordering ops (LT, LE, GT, GE) with non-identical objects, raise TypeError
    ; For EQ/NE, use identity comparison
    cmp ecx, PY_EQ
    je .cmp_id_eq_ne
    cmp ecx, PY_NE
    je .cmp_id_eq_ne

    ; Ordering comparison with unsupported types → raise TypeError
    ; DECREF both operands first
    mov rdi, [rsp + BO_LEFT]
    mov rsi, [rsp + BO_LTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + BO_RIGHT]
    mov rsi, [rsp + BO_RTAG]
    DECREF_VAL rdi, rsi
    add rsp, BO_SIZE
    extern raise_exception
    RAISE exc_TypeError_type, "'<' not supported between instances"
    DISPATCH

.cmp_id_eq_ne:
    mov rsi, [rsp + BO_RIGHT]
    mov rdi, [rsp + BO_LEFT]
    cmp rdi, rsi
    jne .cmp_id_not_equal
    ; Payloads match — also check tags (None payload=0 vs SmallInt 0)
    mov rdi, [rsp + BO_LTAG]
    cmp rdi, [rsp + BO_RTAG]
    je .cmp_id_equal
.cmp_id_not_equal:
    ; Not equal
    cmp ecx, PY_NE
    je .cmp_id_true
    jmp .cmp_id_false
.cmp_id_equal:
    cmp ecx, PY_EQ
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
    VPOP_VAL rdi, r8            ; rdi = operand, r8 = operand tag

    ; TAG_FLOAT fast path: inline sign flip, no DECREF needed
    cmp r8d, TAG_FLOAT
    je .neg_float

    ; Save operand + tag for DECREF after call
    push r8
    push rdi

    ; Get nb_negative: type -> tp_as_number -> nb_negative (SmallInt-aware)
    cmp r8d, TAG_SMALLINT
    je .neg_smallint_type
    cmp r8d, TAG_PTR            ; a float took the fast path above; anything
    jne .neg_type_error         ; else that is not a pointer has no type
    mov rax, [rdi + PyObject.ob_type]
    jmp .neg_have_type
.neg_smallint_type:
    lea rax, [rel int_type]
.neg_have_type:
    ; Neither of these loads was guarded.  A type with no numeric protocol
    ; -- None, str, and every user class, whose tp_as_number is zero -- read
    ; nb_negative from address 0 and called it.
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .neg_type_error
    mov rax, [rax + PyNumberMethods.nb_negative]
    test rax, rax
    jz .neg_type_error

    ; Call nb_negative(rdi = operand Value)
    mov rdx, r8                ; tag
    V_PACK rdi, rdx
    call rax                   ; rax = result Value

    ; DECREF old operand (tag-aware)
    push rax                   ; save result Value
    push rax                   ; keep the stack 16-byte aligned
    mov rdi, [rsp + 16]       ; rdi = old operand
    mov rsi, [rsp + 24]       ; rsi = operand tag
    DECREF_VAL rdi, rsi
    add rsp, 8
    pop rax
    add rsp, 16                ; discard saved operand + tag

    ; Push result
    VPUSH rax
    DISPATCH

.neg_float:
    ; Inline float negate: flip sign bit, no refcounting
    btc rdi, 63
    VPUSH_FLOAT rdi, r15
    DISPATCH

.neg_type_error:
    RAISE exc_TypeError_type, "bad operand type for unary -"
END_FUNC op_unary_negative

;; ============================================================================
;; op_unary_invert - Bitwise NOT of TOS (~x)
;;
;; Calls type's nb_invert from tp_as_number.
;; ============================================================================
DEF_FUNC_BARE op_unary_invert
    VPOP_VAL rdi, r8            ; rdi = operand, r8 = operand tag
    push r8
    push rdi

    cmp r8d, TAG_SMALLINT
    je .inv_smallint_type
    cmp r8d, TAG_PTR            ; ~ has no float case at all, so a float's
    jne .inv_type_error         ; raw bits were used as an address
    mov rax, [rdi + PyObject.ob_type]
    jmp .inv_have_type
.inv_smallint_type:
    lea rax, [rel int_type]
.inv_have_type:
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .inv_type_error
    mov rax, [rax + PyNumberMethods.nb_invert]
    test rax, rax
    jz .inv_type_error

    ; Call nb_invert(rdi = operand Value)
    mov rdx, r8                ; tag
    V_PACK rdi, rdx
    xor esi, esi
    call rax                   ; rax = result Value
    push rax
    push rax                   ; alignment
    mov rdi, [rsp + 16]
    mov rsi, [rsp + 24]       ; tag
    DECREF_VAL rdi, rsi
    add rsp, 8
    pop rax
    add rsp, 16
    VPUSH rax
    DISPATCH

.inv_type_error:
    RAISE exc_TypeError_type, "bad operand type for unary ~"
END_FUNC op_unary_invert

;; ============================================================================
;; op_unary_not - Logical NOT of TOS
;;
;; Calls obj_is_true, then pushes the inverted boolean.
;; ============================================================================
DEF_FUNC_BARE op_unary_not
    VPOP_VAL rdi, r8            ; rdi = operand, r8 = operand tag

    ; Save operand + tag for DECREF
    push r8
    push rdi

    ; Call obj_is_true(operand, tag) -> 0 or 1
    mov rsi, r8                ; tag
    V_PACK rdi, rsi
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
section .data

;; Maps NB_* argument (0-25) to the byte offset within PyNumberMethods
;; where the corresponding method function pointer resides.
align 8
binary_op_offsets:
    ; Symbolic, not literal: these are byte offsets into PyNumberMethods, and
    ; a reorder of that struc used to mis-dispatch every binary operator in
    ; silence.  NB_MATRIX_MULTIPLY was a literal 0 -- nb_add -- so 3 @ 4
    ; returned 7.
    dq PyNumberMethods.nb_add            ; NB_ADD (0)
    dq PyNumberMethods.nb_and            ; NB_AND (1)
    dq PyNumberMethods.nb_floor_divide   ; NB_FLOOR_DIVIDE (2)
    dq PyNumberMethods.nb_lshift         ; NB_LSHIFT (3)
    dq PyNumberMethods.nb_matmul         ; NB_MATRIX_MULTIPLY (4)
    dq PyNumberMethods.nb_multiply       ; NB_MULTIPLY (5)
    dq PyNumberMethods.nb_remainder      ; NB_REMAINDER (6)
    dq PyNumberMethods.nb_or             ; NB_OR (7)
    dq PyNumberMethods.nb_power          ; NB_POWER (8)
    dq PyNumberMethods.nb_rshift         ; NB_RSHIFT (9)
    dq PyNumberMethods.nb_subtract       ; NB_SUBTRACT (10)
    dq PyNumberMethods.nb_true_divide    ; NB_TRUE_DIVIDE (11)
    dq PyNumberMethods.nb_xor            ; NB_XOR (12)
    dq PyNumberMethods.nb_iadd           ; NB_INPLACE_ADD (13)
    dq PyNumberMethods.nb_iand           ; NB_INPLACE_AND (14)
    dq PyNumberMethods.nb_ifloor_divide  ; NB_INPLACE_FLOOR_DIVIDE (15)
    dq PyNumberMethods.nb_ilshift        ; NB_INPLACE_LSHIFT (16)
    dq PyNumberMethods.nb_imatmul        ; NB_INPLACE_MATRIX_MULTIPLY (17)
    dq PyNumberMethods.nb_imul           ; NB_INPLACE_MULTIPLY (18)
    dq PyNumberMethods.nb_irem           ; NB_INPLACE_REMAINDER (19)
    dq PyNumberMethods.nb_ior            ; NB_INPLACE_OR (20)
    dq PyNumberMethods.nb_ipow           ; NB_INPLACE_POWER (21)
    dq PyNumberMethods.nb_irshift        ; NB_INPLACE_RSHIFT (22)
    dq PyNumberMethods.nb_isub           ; NB_INPLACE_SUBTRACT (23)
    dq PyNumberMethods.nb_itrue_divide   ; NB_INPLACE_TRUE_DIVIDE (24)
    dq PyNumberMethods.nb_ixor           ; NB_INPLACE_XOR (25)

section .text

;; ============================================================================
;; op_binary_op_add_int - Specialized SmallInt add (opcode 211)
;;
;; Guard: both TOS and TOS1 must be SmallInt (tag-based).
;; On guard failure: deopt back to BINARY_OP (122).
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_op_add_int
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    ; Guard: both SmallInt (tag-based)
    cmp r9d, TAG_SMALLINT
    jne .add_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .add_int_deopt_repush
    ; Add, check overflow
    mov rax, rdi
    mov rdx, rsi
    add rax, rdx
    jo .add_int_deopt_repush
    ; Encode as SmallInt
    VPUSH_INT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.add_int_deopt_repush:
    ; Overflow: re-push operands and deopt
    VPUSH_VAL rdi, r9
    VPUSH_VAL rsi, r8
.add_int_deopt:
    ; Rewrite opcode back to BINARY_OP (122)
    mov byte [rbx - 2], 122
    sub rbx, 2                 ; back up to re-execute as BINARY_OP
    DISPATCH
END_FUNC op_binary_op_add_int

;; ============================================================================
;; op_binary_op_sub_int - Specialized SmallInt subtract (opcode 212)
;;
;; Guard: both TOS and TOS1 must be SmallInt (tag-based).
;; On guard failure: deopt back to BINARY_OP (122).
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_op_sub_int
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    ; Guard: both SmallInt (tag-based)
    cmp r9d, TAG_SMALLINT
    jne .sub_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .sub_int_deopt_repush
    ; Sub, check overflow
    mov rax, rdi
    mov rdx, rsi
    sub rax, rdx
    jo .sub_int_deopt_repush
    ; Encode as SmallInt
    VPUSH_INT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.sub_int_deopt_repush:
    ; Overflow or type mismatch: re-push operands and deopt
    VPUSH_VAL rdi, r9
    VPUSH_VAL rsi, r8
.sub_int_deopt:
    ; Rewrite opcode back to BINARY_OP (122)
    mov byte [rbx - 2], 122
    sub rbx, 2                 ; back up to re-execute as BINARY_OP
    DISPATCH
END_FUNC op_binary_op_sub_int

;; ============================================================================
;; op_binary_op_add_float - Specialized float add (opcode 217)
;;
;; Guard: both TOS and TOS1 must be TAG_FLOAT.
;; On guard failure: deopt back to BINARY_OP (122).
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_op_add_float
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    cmp r9d, TAG_FLOAT
    jne .add_float_deopt_repush
    cmp r8d, TAG_FLOAT
    jne .add_float_deopt_repush
    movq xmm0, rdi
    movq xmm1, rsi
    addsd xmm0, xmm1
    movq rax, xmm0
    VPUSH_FLOAT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.add_float_deopt_repush:
    VUNDROP 2
.add_float_deopt:
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_add_float

;; ============================================================================
;; op_binary_op_sub_float - Specialized float subtract (opcode 218)
;; ============================================================================
DEF_FUNC_BARE op_binary_op_sub_float
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    cmp r9d, TAG_FLOAT
    jne .sub_float_deopt_repush
    cmp r8d, TAG_FLOAT
    jne .sub_float_deopt_repush
    movq xmm0, rdi
    movq xmm1, rsi
    subsd xmm0, xmm1
    movq rax, xmm0
    VPUSH_FLOAT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.sub_float_deopt_repush:
    VUNDROP 2
.sub_float_deopt:
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_sub_float

;; ============================================================================
;; op_binary_op_mul_float - Specialized float multiply (opcode 219)
;; ============================================================================
DEF_FUNC_BARE op_binary_op_mul_float
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    cmp r9d, TAG_FLOAT
    jne .mul_float_deopt_repush
    cmp r8d, TAG_FLOAT
    jne .mul_float_deopt_repush
    movq xmm0, rdi
    movq xmm1, rsi
    mulsd xmm0, xmm1
    movq rax, xmm0
    VPUSH_FLOAT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.mul_float_deopt_repush:
    VUNDROP 2
.mul_float_deopt:
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_mul_float

;; ============================================================================
;; op_binary_op_truediv_float - Specialized float truediv (opcode 220)
;; ============================================================================
DEF_FUNC_BARE op_binary_op_truediv_float
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    cmp r9d, TAG_FLOAT
    jne .truediv_float_deopt_repush
    cmp r8d, TAG_FLOAT
    jne .truediv_float_deopt_repush
    ; Check for division by zero
    movq xmm1, rsi
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    je .truediv_float_deopt_repush  ; zero divisor → deopt to generic (raises ZeroDivisionError)
    movq xmm0, rdi
    divsd xmm0, xmm1
    movq rax, xmm0
    VPUSH_FLOAT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.truediv_float_deopt_repush:
    VUNDROP 2
.truediv_float_deopt:
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_truediv_float

;; ============================================================================
;; op_binary_op_mul_int - Specialized SmallInt multiply (opcode 221)
;;
;; Guard: both TOS and TOS1 must be SmallInt.
;; On guard failure: deopt back to BINARY_OP (122).
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_op_mul_int
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    cmp r9d, TAG_SMALLINT
    jne .mul_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .mul_int_deopt_repush
    mov rax, rdi
    imul rsi
    jo .mul_int_deopt_repush_vals
    VPUSH_INT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.mul_int_deopt_repush_vals:
    ; imul clobbered rax/rdx, use saved values
    VPUSH_VAL rdi, r9
    VPUSH_VAL rsi, r8
    jmp .mul_int_deopt
.mul_int_deopt_repush:
    VUNDROP 2
.mul_int_deopt:
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_mul_int

;; ============================================================================
;; op_binary_op_floordiv_int - Specialized SmallInt floor divide (opcode 222)
;;
;; Guard: both TOS and TOS1 must be SmallInt, right != 0.
;; On guard failure: deopt back to BINARY_OP (122).
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_op_floordiv_int
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    ; Guard: both SmallInt
    cmp r9d, TAG_SMALLINT
    jne .fdiv_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .fdiv_int_deopt_repush
    ; Guard: right != 0
    test rsi, rsi
    jz .fdiv_int_deopt_repush
    ; Floor divide
    mov rax, rdi
    cqo
    idiv rsi                    ; rax=quotient, rdx=remainder
    ; Floor: if remainder != 0 and signs differ, subtract 1
    test rdx, rdx
    jz .fdiv_int_exact
    mov rcx, rdi
    xor rcx, rsi
    jns .fdiv_int_exact         ; same sign → truncation == floor
    dec rax
.fdiv_int_exact:
    VPUSH_INT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.fdiv_int_deopt_repush:
    VUNDROP 2
.fdiv_int_deopt:
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_floordiv_int

;; ============================================================================
;; op_compare_op_int - Specialized SmallInt comparison (opcode 209)
;;
;; Guard: both TOS and TOS1 must be SmallInt (tag-based).
;; On guard failure: deopt back to COMPARE_OP (107).
;; ecx = arg (comparison op = arg >> 4)
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_compare_op_int
    shr ecx, 4                 ; ecx = comparison op (0-5)
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    ; Guard: both SmallInt (tag-based)
    cmp r9d, TAG_SMALLINT
    jne .cmp_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .cmp_int_deopt_repush
    ; Compare
    cmp rdi, rsi               ; flags survive LEA + jmp [mem]
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
    VPUSH_BOOL rax             ; (0/1, TAG_BOOL) — no INCREF needed
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
.cmp_int_deopt_repush:
    ; Re-push operands (slots still intact — just restore stack pointer)
    VUNDROP 2
.cmp_int_deopt:
    ; Rewrite back to COMPARE_OP (107) and re-execute
    mov byte [rbx - 2], 107
    sub rbx, 2
    DISPATCH
END_FUNC op_compare_op_int

;; ============================================================================
;; op_compare_op_int_jump_false - Fused COMPARE_OP_INT + POP_JUMP_IF_FALSE (215)
;;
;; Guard: both TOS and TOS1 must be SmallInt.
;; On guard failure: deopt back to COMPARE_OP (107).
;; ecx = arg (comparison op = arg >> 4).
;; Followed by 1 CACHE entry (2 bytes), then POP_JUMP_IF_FALSE (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_compare_op_int_jump_false
    shr ecx, 4                 ; ecx = comparison op (0-5)
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    ; Guard: both SmallInt
    cmp r9d, TAG_SMALLINT
    jne .cijf_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .cijf_deopt_repush
    ; Read jump target from POP_JUMP_IF_FALSE arg (at rbx+3)
    movzx r8d, byte [rbx + 3]
    ; Compare
    cmp rdi, rsi
    lea r9, [rel .cijf_setcc_table]
    jmp [r9 + rcx*8]

.cijf_lt:
    setl al
    jmp .cijf_branch
.cijf_le:
    setle al
    jmp .cijf_branch
.cijf_eq:
    sete al
    jmp .cijf_branch
.cijf_ne:
    setne al
    jmp .cijf_branch
.cijf_gt:
    setg al
    jmp .cijf_branch
.cijf_ge:
    setge al
    ; fall through
.cijf_branch:
    ; Skip CACHE (2) + POP_JUMP_IF_FALSE (2) = 4 bytes
    add rbx, 4
    test al, al
    jnz .cijf_no_jump          ; truthy → don't jump (POP_JUMP_IF_FALSE)
    lea rbx, [rbx + r8*2]     ; jump (r8 = target offset)
.cijf_no_jump:
    DISPATCH

section .data
align 8
.cijf_setcc_table:
    dq .cijf_lt                ; PY_LT = 0
    dq .cijf_le                ; PY_LE = 1
    dq .cijf_eq                ; PY_EQ = 2
    dq .cijf_ne                ; PY_NE = 3
    dq .cijf_gt                ; PY_GT = 4
    dq .cijf_ge                ; PY_GE = 5
section .text

.cijf_deopt_repush:
    VUNDROP 2
    mov byte [rbx - 2], 107   ; deopt to COMPARE_OP
    sub rbx, 2
    DISPATCH
END_FUNC op_compare_op_int_jump_false

;; ============================================================================
;; op_compare_op_int_jump_true - Fused COMPARE_OP_INT + POP_JUMP_IF_TRUE (216)
;;
;; Same as above but jumps when comparison is TRUE.
;; ============================================================================
DEF_FUNC_BARE op_compare_op_int_jump_true
    shr ecx, 4                 ; ecx = comparison op (0-5)
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    ; Guard: both SmallInt
    cmp r9d, TAG_SMALLINT
    jne .cijt_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .cijt_deopt_repush
    ; Read jump target from POP_JUMP_IF_TRUE arg (at rbx+3)
    movzx r8d, byte [rbx + 3]
    ; Compare
    cmp rdi, rsi
    lea r9, [rel .cijt_setcc_table]
    jmp [r9 + rcx*8]

.cijt_lt:
    setl al
    jmp .cijt_branch
.cijt_le:
    setle al
    jmp .cijt_branch
.cijt_eq:
    sete al
    jmp .cijt_branch
.cijt_ne:
    setne al
    jmp .cijt_branch
.cijt_gt:
    setg al
    jmp .cijt_branch
.cijt_ge:
    setge al
    ; fall through
.cijt_branch:
    ; Skip CACHE (2) + POP_JUMP_IF_TRUE (2) = 4 bytes
    add rbx, 4
    test al, al
    jz .cijt_no_jump           ; falsy → don't jump (POP_JUMP_IF_TRUE)
    lea rbx, [rbx + r8*2]     ; jump (r8 = target offset)
.cijt_no_jump:
    DISPATCH

section .data
align 8
.cijt_setcc_table:
    dq .cijt_lt                ; PY_LT = 0
    dq .cijt_le                ; PY_LE = 1
    dq .cijt_eq                ; PY_EQ = 2
    dq .cijt_ne                ; PY_NE = 3
    dq .cijt_gt                ; PY_GT = 4
    dq .cijt_ge                ; PY_GE = 5
section .text

.cijt_deopt_repush:
    VUNDROP 2
    mov byte [rbx - 2], 107   ; deopt to COMPARE_OP
    sub rbx, 2
    DISPATCH
END_FUNC op_compare_op_int_jump_true
