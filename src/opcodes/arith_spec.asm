; opcodes/arith_spec.asm - the specialized arithmetic and comparison opcodes
;
; The superinstructions op_binary_op and op_compare_op rewrite themselves into
; once they have seen what their operands actually are.  Each guards the shape
; it was specialized for, does the work in registers, and on a guard failure
; writes the generic opcode back and re-dispatches.
;
;   211/212/221/222/226/227/228/229  int add, subtract, multiply, floor divide,
;                                    and, or, xor, remainder
;   217/218/219/220                  float add, subtract, multiply, divide
;   209/215/216                      int compare, and the two fused with the
;                                    jump that follows
;   223/224/225                      the same three for floats
;
; Split out of arith.asm, which keeps the generic protocol and the error
; messages, because that file reached lint's 100k cap.  Nothing here calls
; into arith.asm's file-local helpers -- the deopts write an opcode byte and
; re-dispatch rather than jumping to a label.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

extern eval_saved_rbx
extern eval_saved_r13
extern opcode_dispatch_table
extern bool_true
extern bool_false
extern op_binary_op
extern op_compare_op

section .text

;; ============================================================================
;; op_binary_op_add_int (211) -> nothing; pushes the sum and dispatches
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
    ; Rewrite opcode back to BINARY_OP (122) and re-execute.
    ;
    ; Rewinding rbx is only safe because BINARY_OP and COMPARE_OP arguments are
    ; small -- an operator index, and a comparison plus its mask -- so neither
    ; is ever preceded by EXTENDED_ARG.  The deopts that carry a real offset or
    ; a name index cannot do this; see .fir_deopt in opcodes/build.asm.
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_add_int

;; ============================================================================
;; op_binary_op_sub_int (212) -> nothing; pushes the difference and dispatches
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
;; op_binary_op_add_float (217) -> nothing; pushes the sum and dispatches
;;
;; Guard: both TOS and TOS1 must be TAG_FLOAT.
;; On guard failure: deopt back to BINARY_OP (122).
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_op_add_float
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    FLOAT_PAIR_OR_DEOPT .add_float_deopt_repush
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
;; op_binary_op_sub_float (218) -> nothing; pushes the difference and dispatches
;; ============================================================================
DEF_FUNC_BARE op_binary_op_sub_float
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    FLOAT_PAIR_OR_DEOPT .sub_float_deopt_repush
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
;; op_binary_op_mul_float (219) -> nothing; pushes the product and dispatches
;; ============================================================================
DEF_FUNC_BARE op_binary_op_mul_float
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    FLOAT_PAIR_OR_DEOPT .mul_float_deopt_repush
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
;; op_binary_op_truediv_float (220) -> nothing; pushes the quotient and dispatches
;; ============================================================================
DEF_FUNC_BARE op_binary_op_truediv_float
    VPOP_VAL rsi, r8            ; right + tag
    VPOP_VAL rdi, r9            ; left + tag
    FLOAT_PAIR_OR_DEOPT .truediv_float_deopt_repush
    ; A zero divisor deopts, so the generic path raises ZeroDivisionError.
    ; ucomisd sets ZF for UNORDERED as well, so parity is tested first: a NaN
    ; divisor took the deopt, which answers nan correctly but also rewrote the
    ; site back to BINARY_OP for good, so one nan in a loop cost the
    ; specialization for the rest of the run.
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    jp .tfd_nonzero
    je .truediv_float_deopt_repush
.tfd_nonzero:
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
;; op_binary_op_mul_int (221) -> nothing; pushes the product and dispatches
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
;; op_binary_op_floordiv_int (222) -> nothing; pushes the quotient and dispatches
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
;; op_compare_op_int (209) -> nothing; pushes the bool and dispatches
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
;; op_binary_op_and_int (226) -> nothing; pushes the result and dispatches
;;
;; Guard: both operands TAG_SMALLINT.  There is no overflow arm -- see the
;; note on .binop_try_smallint_and above for why a bitwise op on two
;; immediates is always an immediate.
;; ============================================================================
DEF_FUNC_BARE op_binary_op_and_int
    VPOP_VAL rsi, r8
    VPOP_VAL rdi, r9
    cmp r9d, TAG_SMALLINT
    jne .and_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .and_int_deopt_repush
    mov rax, rdi
    and rax, rsi
    VPUSH_INT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.and_int_deopt_repush:
    VUNDROP 2
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_and_int

;; ============================================================================
;; op_binary_op_or_int (227) -> nothing; pushes the result and dispatches
;;
;; Guard: both operands TAG_SMALLINT.  There is no overflow arm -- see the
;; note on .binop_try_smallint_and above for why a bitwise op on two
;; immediates is always an immediate.
;; ============================================================================
DEF_FUNC_BARE op_binary_op_or_int
    VPOP_VAL rsi, r8
    VPOP_VAL rdi, r9
    cmp r9d, TAG_SMALLINT
    jne .or_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .or_int_deopt_repush
    mov rax, rdi
    or rax, rsi
    VPUSH_INT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.or_int_deopt_repush:
    VUNDROP 2
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_or_int

;; ============================================================================
;; op_binary_op_xor_int (228) -> nothing; pushes the result and dispatches
;;
;; Guard: both operands TAG_SMALLINT.  There is no overflow arm -- see the
;; note on .binop_try_smallint_and above for why a bitwise op on two
;; immediates is always an immediate.
;; ============================================================================
DEF_FUNC_BARE op_binary_op_xor_int
    VPOP_VAL rsi, r8
    VPOP_VAL rdi, r9
    cmp r9d, TAG_SMALLINT
    jne .xor_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .xor_int_deopt_repush
    mov rax, rdi
    xor rax, rsi
    VPUSH_INT rax, r15
    add rbx, 2                 ; skip CACHE
    DISPATCH
.xor_int_deopt_repush:
    VUNDROP 2
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_xor_int

;; ============================================================================
;; op_binary_op_mod_int (229) -> nothing; pushes the result and dispatches
;;
;; Guards: both operands TAG_SMALLINT, and a non-zero divisor -- a zero one
;; deopts so the generic path raises ZeroDivisionError.
;;
;; `idiv` gives C's remainder, which carries the DIVIDEND's sign; Python's
;; carries the DIVISOR's.  A remainder that disagrees with the divisor gets the
;; divisor added.  The magnitude stays below the divisor's, so the answer is
;; still an immediate.
;;
;; INT64_MIN / -1 would fault, but neither operand can be INT64_MIN: an
;; immediate is inside +-2^50.
;; ============================================================================
DEF_FUNC_BARE op_binary_op_mod_int
    VPOP_VAL rsi, r8
    VPOP_VAL rdi, r9
    cmp r9d, TAG_SMALLINT
    jne .mod_int_deopt_repush
    cmp r8d, TAG_SMALLINT
    jne .mod_int_deopt_repush
    test rsi, rsi
    jz .mod_int_deopt_repush
    mov rax, rdi
    cqo
    idiv rsi
    test rdx, rdx
    jz .mod_int_done
    mov rcx, rdx
    xor rcx, rsi
    jns .mod_int_done
    add rdx, rsi
.mod_int_done:
    mov rax, rdx
    VPUSH_INT rax, r15
    add rbx, 2
    DISPATCH
.mod_int_deopt_repush:
    VUNDROP 2
    mov byte [rbx - 2], 122
    sub rbx, 2
    DISPATCH
END_FUNC op_binary_op_mod_int

;; ============================================================================
;; op_compare_op_float (223) -> nothing; pushes the bool and dispatches
;;
;; The float comparison superinstructions (223, 224, 225).
;;
;; op_compare_op had a both-SmallInt arm and nothing else, so a pair of float
;; immediates fell through to the general protocol: float_binop_accepts, then
;; float_compare, which calls float_binop_accepts twice more, fc_wide_int
;; twice and float_to_f64 twice -- about nine calls and four pack/unpack round
;; trips to reach one ucomisd.  It was the only float operation not already
;; ahead of CPython.
;;
;; These read the two Values straight off the stack and guard on the raw
;; high16, so nothing is unpacked into a (payload, tag) pair and re-packed;
;; the deopt is therefore just a rewrite, with the operands still in place.
;;
;; NaN is the one thing the integer forms do not have to think about.
;; `ucomisd` reports unordered as ZF=1, PF=1, CF=1, so `seta`/`setae` are
;; already false for it, while `setb`/`setbe`/`sete` would each be wrongly
;; true and `setne` wrongly false.  Those four are corrected with the parity
;; flag: AND with `setnp` for the three that must become false, OR with
;; `setp` for the one that must become true.
;; ============================================================================
DEF_FUNC_BARE op_compare_op_float
    shr ecx, 4                  ; ecx = PY_LT/LE/EQ/NE/GT/GE (0-5)
    V_TEST_F64_M [r13 - 8], rax
    ja .cf_deopt
    V_TEST_F64_M [r13 - 16], rax
    ja .cf_deopt
    mov rsi, [r13 - 8]          ; right
    mov rdi, [r13 - 16]         ; left
    V_TO_F64 rsi
    V_TO_F64 rdi
    movq xmm1, rsi
    movq xmm0, rdi
    sub r13, 16                 ; both consumed; immediates own nothing
    ucomisd xmm0, xmm1
    lea r8, [rel .cf_setcc_table]
    jmp [r8 + rcx*8]            ; LEA and jmp [mem] leave the flags alone

.cf_lt:
    setb al
    setnp dl
    and al, dl
    jmp .cf_push
.cf_le:
    setbe al
    setnp dl
    and al, dl
    jmp .cf_push
.cf_eq:
    sete al
    setnp dl
    and al, dl
    jmp .cf_push
.cf_ne:
    setne al
    setp dl
    or al, dl
    jmp .cf_push
.cf_gt:
    seta al                     ; false for unordered already
    jmp .cf_push
.cf_ge:
    setae al                    ; likewise
.cf_push:
    movzx eax, al
    VPUSH_BOOL rax
    add rbx, 2                  ; skip CACHE
    DISPATCH

section .data
align 8
.cf_setcc_table:
    dq .cf_lt
    dq .cf_le
    dq .cf_eq
    dq .cf_ne
    dq .cf_gt
    dq .cf_ge
section .text
.cf_deopt:
    ; Nothing was popped, so there is nothing to put back.
    mov byte [rbx - 2], 107     ; COMPARE_OP
    sub rbx, 2
    DISPATCH
END_FUNC op_compare_op_float

;; ============================================================================
;; op_compare_op_float_jump_false (224) -> nothing; branches and dispatches
;;
;; COMPARE_OP_FLOAT fused with the POP_JUMP_IF_FALSE that follows.  No bool
;; object is built at all: the flags decide the branch directly.
;; ============================================================================
DEF_FUNC_BARE op_compare_op_float_jump_false
    shr ecx, 4
    V_TEST_F64_M [r13 - 8], rax
    ja .cfjf_deopt
    V_TEST_F64_M [r13 - 16], rax
    ja .cfjf_deopt
    movzx r9d, byte [rbx + 3]   ; the POP_JUMP_IF_FALSE argument
    mov rsi, [r13 - 8]
    mov rdi, [r13 - 16]
    V_TO_F64 rsi
    V_TO_F64 rdi
    movq xmm1, rsi
    movq xmm0, rdi
    sub r13, 16
    ucomisd xmm0, xmm1
    lea r8, [rel .cfjf_setcc_table]
    jmp [r8 + rcx*8]

.cfjf_lt:
    setb al
    setnp dl
    and al, dl
    jmp .cfjf_branch
.cfjf_le:
    setbe al
    setnp dl
    and al, dl
    jmp .cfjf_branch
.cfjf_eq:
    sete al
    setnp dl
    and al, dl
    jmp .cfjf_branch
.cfjf_ne:
    setne al
    setp dl
    or al, dl
    jmp .cfjf_branch
.cfjf_gt:
    seta al
    jmp .cfjf_branch
.cfjf_ge:
    setae al
.cfjf_branch:
    add rbx, 4                  ; CACHE (2) + POP_JUMP_IF_FALSE (2)
    test al, al
    jnz .cfjf_no_jump
    lea rbx, [rbx + r9*2]
.cfjf_no_jump:
    DISPATCH

section .data
align 8
.cfjf_setcc_table:
    dq .cfjf_lt
    dq .cfjf_le
    dq .cfjf_eq
    dq .cfjf_ne
    dq .cfjf_gt
    dq .cfjf_ge
section .text
.cfjf_deopt:
    mov byte [rbx - 2], 107
    sub rbx, 2
    DISPATCH
END_FUNC op_compare_op_float_jump_false

;; ============================================================================
;; op_compare_op_float_jump_true (225) -> nothing; branches and dispatches
;;
;; The same fused with POP_JUMP_IF_TRUE, so the branch is taken when the
;; comparison holds rather than when it does not.
;; ============================================================================
DEF_FUNC_BARE op_compare_op_float_jump_true
    shr ecx, 4
    V_TEST_F64_M [r13 - 8], rax
    ja .cfjt_deopt
    V_TEST_F64_M [r13 - 16], rax
    ja .cfjt_deopt
    movzx r9d, byte [rbx + 3]
    mov rsi, [r13 - 8]
    mov rdi, [r13 - 16]
    V_TO_F64 rsi
    V_TO_F64 rdi
    movq xmm1, rsi
    movq xmm0, rdi
    sub r13, 16
    ucomisd xmm0, xmm1
    lea r8, [rel .cfjt_setcc_table]
    jmp [r8 + rcx*8]

.cfjt_lt:
    setb al
    setnp dl
    and al, dl
    jmp .cfjt_branch
.cfjt_le:
    setbe al
    setnp dl
    and al, dl
    jmp .cfjt_branch
.cfjt_eq:
    sete al
    setnp dl
    and al, dl
    jmp .cfjt_branch
.cfjt_ne:
    setne al
    setp dl
    or al, dl
    jmp .cfjt_branch
.cfjt_gt:
    seta al
    jmp .cfjt_branch
.cfjt_ge:
    setae al
.cfjt_branch:
    add rbx, 4
    test al, al
    jz .cfjt_no_jump            ; falsy -> do not jump (POP_JUMP_IF_TRUE)
    lea rbx, [rbx + r9*2]
.cfjt_no_jump:
    DISPATCH

section .data
align 8
.cfjt_setcc_table:
    dq .cfjt_lt
    dq .cfjt_le
    dq .cfjt_eq
    dq .cfjt_ne
    dq .cfjt_gt
    dq .cfjt_ge
section .text
.cfjt_deopt:
    mov byte [rbx - 2], 107
    sub rbx, 2
    DISPATCH
END_FUNC op_compare_op_float_jump_true

;; ============================================================================
;; op_compare_op_int_jump_false (215) -> nothing; branches and dispatches
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
;; op_compare_op_int_jump_true (216) -> nothing; branches and dispatches
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
