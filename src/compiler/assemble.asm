; assemble.asm - Instruction stream to PyCodeObject
;
; Five passes, in this order, each depending on the last:
;
;   1. asm_resolve   assign every instruction an offset, iterating until the
;                    EXTENDED_ARG prefix counts stop growing.
;   2. asm_stackdepth  a worklist over the flat stream, giving co_stacksize.
;   3. asm_linetable   PEP 626 line information.
;   4. asm_write       the bytes, with CACHE and EXTENDED_ARG synthesized here
;                      and nowhere else.
;   5. code_new        the object itself.
;
; Everything in passes 1, 2 and 4 is driven by op_meta in tables.asm, which is
; generated from CPython's own opcode module.  That is why a wrong CACHE count
; is a build-time question rather than a runtime mystery: it cannot vary
; between the sizing pass, the depth pass and the writer.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ap_free
extern ap_malloc
extern ap_memcpy
extern buf_free
extern buf_init
extern buf_push_u8
extern buf_reserve
extern bytes_from_data
extern code_new
extern code_spec_clear
extern comp_error
extern sym_at
extern tuple_new
extern op_meta

extern exc_SyntaxError_type

; --- Named frame-layout constants ---
AR_N     equ 16
AR_OFF   equ 32
AR_CHG   equ 40
AR_FRAME equ 40          ; + 5 pushes = 80

section .text

;; ============================================================================
;; asm_nprefix(uint32_t oparg) -> rax = EXTENDED_ARG words needed (0..3)
;; ============================================================================
DEF_FUNC_BARE asm_nprefix
    xor eax, eax
    cmp edi, 0x100
    jb .done
    mov eax, 1
    cmp edi, 0x10000
    jb .done
    mov eax, 2
    cmp edi, 0x1000000
    jb .done
    mov eax, 3
.done:
    ret
END_FUNC asm_nprefix

;; ============================================================================
;; asm_isize(CompUnit *u, uint64_t i) -> rax = size in code units
;; prefix words + the instruction itself + its trailing CACHE words.
;; ============================================================================
DEF_FUNC_BARE asm_isize
    mov rax, [rdi + CompUnit.instrs + Buf.data]
    mov rdx, rsi
    shl rdx, INSTR_SHIFT
    movzx ecx, byte [rax + rdx + Instr.opcode]
    lea r8, [rel op_meta]
    shl rcx, 2
    movzx eax, byte [r8 + rcx + OpMeta.cache]
    inc rax
    mov rdx, [rdi + CompUnit.prefix + Buf.data]
    movzx edx, byte [rdx + rsi]
    add rax, rdx
    ret
END_FUNC asm_isize

;; ============================================================================
;; asm_resolve(CompUnit *u) -> rax = total size in code units
;;
;; Instruction sizes depend on oparg magnitude; a jump's oparg depends on the
;; offsets of everything between it and its target; and those offsets depend on
;; the sizes.  So iterate: lay the stream out, recompute the jump deltas, and
;; grow any instruction whose oparg no longer fits.
;;
;; Prefix counts only ever GROW.  That is what makes this terminate -- the sum
;; of prefixes is a non-decreasing integer bounded by 3 per instruction, so
;; each round either changes nothing and stops, or makes progress toward the
;; bound.  Allowing a shrink admits oscillation: growing an instruction inside
;; a jump's span lengthens that jump, which can lengthen others, which can
;; shorten the first again.  The cost of never shrinking is an occasional
;; redundant EXTENDED_ARG 0, which is harmless -- the interpreter's
;; op_extended_arg accumulates arg<<8 and a zero prefix contributes nothing --
;; and which CPython itself emits.
;;
;; A jump's delta is measured from the end of the whole instruction, INCLUDING
;; its own prefix words and its own CACHE words.  That is what the interpreter
;; does: op_for_iter skips its cache before adding the delta.
;; ============================================================================
DEF_FUNC asm_resolve, AR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov rbx, rdi
    mov r14, [rbx + CompUnit.instrs + Buf.len]
    mov [rbp - AR_N], r14

    ; One prefix byte per instruction, seeded from the literal opargs.  A jump
    ; starts at zero, since its delta is not known yet.
    lea rdi, [rbx + CompUnit.prefix]
    mov rsi, r14
    call buf_reserve
    mov r15, rax
    xor ecx, ecx
.seed:
    cmp rcx, r14
    jae .seeded
    mov byte [r15 + rcx], 0
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, rcx
    shl rdx, INSTR_SHIFT
    test byte [rax + rdx + Instr.flags], IF_LABELARG
    jnz .seed_next
    push rcx
    mov edi, [rax + rdx + Instr.oparg]
    call asm_nprefix
    pop rcx
    mov r15, [rbx + CompUnit.prefix + Buf.data]
    mov [r15 + rcx], al
.seed_next:
    inc rcx
    jmp .seed
.seeded:

.round:
    mov qword [rbp - AR_CHG], 0

    ; --- lay the stream out ---
    xor r12d, r12d                      ; running offset in code units
    xor r13d, r13d                      ; instruction index
.layout:
    cmp r13, r14
    jae .layout_done
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, r13
    shl rdx, INSTR_SHIFT
    mov [rax + rdx + Instr.offset], r12d
    mov rdi, rbx
    mov rsi, r13
    call asm_isize
    add r12, rax
    inc r13
    jmp .layout
.layout_done:
    mov [rbp - AR_OFF], r12             ; total size

    ; --- recompute jump deltas, growing anything that overflowed ---
    xor r13d, r13d
.jumps:
    cmp r13, r14
    jae .jumps_done
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, r13
    shl rdx, INSTR_SHIFT
    lea r15, [rax + rdx]
    test byte [r15 + Instr.flags], IF_LABELARG
    jz .jump_next

    ; base = this instruction's offset + its own size
    mov rdi, rbx
    mov rsi, r13
    call asm_isize
    mov r8d, [r15 + Instr.offset]
    add r8, rax                         ; r8 = the address the delta is from

    ; target = offset of the instruction the label is bound to; a label bound
    ; at the end of the stream resolves to the total size.
    mov rax, [rbx + CompUnit.labels + Buf.data]
    mov ecx, [r15 + Instr.oparg]
    mov ecx, [rax + rcx*4]
    cmp rcx, r14
    jb .have_target
    mov r9, [rbp - AR_OFF]
    jmp .target_off
.have_target:
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, rcx
    shl rdx, INSTR_SHIFT
    mov r9d, [rax + rdx + Instr.offset]
.target_off:

    test byte [r15 + Instr.flags], IF_JREL_BACK
    jnz .backward
    sub r9, r8                          ; forward: target - base
    jmp .have_delta
.backward:
    sub r8, r9
    mov r9, r8                          ; backward: base - target
.have_delta:
    ; A negative delta means a label was bound on the wrong side of its use.
    ; Encoded, it would become an enormous positive oparg and the jump would
    ; land in the middle of nowhere, so catch it here.
    js .bad_jump

    mov rdi, r9
    call asm_nprefix
    mov rdx, [rbx + CompUnit.prefix + Buf.data]
    cmp al, [rdx + r13]
    jbe .jump_next
    mov [rdx + r13], al                 ; grow, never shrink
    mov qword [rbp - AR_CHG], 1
.jump_next:
    inc r13
    jmp .jumps
.jumps_done:
    cmp qword [rbp - AR_CHG], 0
    jne .round

    mov rax, [rbp - AR_OFF]
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bad_jump:
    mov rax, -1
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_resolve

;; ============================================================================
;; asm_jump_delta(CompUnit *u, uint64_t i, uint64_t total) -> rax = the oparg
;; Same arithmetic as the fixpoint, used once more by the writer.
;; ============================================================================
AJ_I     equ 16
AJ_TOT   equ 24
AJ_BASE  equ 32
AJ_FRAME equ 40          ; + 1 push = 48
DEF_FUNC asm_jump_delta, AJ_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - AJ_I], rsi
    mov [rbp - AJ_TOT], rdx

    call asm_isize
    mov rdx, [rbx + CompUnit.instrs + Buf.data]
    mov rcx, [rbp - AJ_I]
    shl rcx, INSTR_SHIFT
    add rdx, rcx                        ; rdx = the Instr*
    mov r8d, [rdx + Instr.offset]
    add r8, rax
    mov [rbp - AJ_BASE], r8

    mov rax, [rbx + CompUnit.labels + Buf.data]
    mov ecx, [rdx + Instr.oparg]
    mov ecx, [rax + rcx*4]
    cmp rcx, [rbx + CompUnit.instrs + Buf.len]
    jb .bound
    mov r9, [rbp - AJ_TOT]
    jmp .have
.bound:
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov r10, rcx
    shl r10, INSTR_SHIFT
    mov r9d, [rax + r10 + Instr.offset]
.have:
    mov r8, [rbp - AJ_BASE]
    test byte [rdx + Instr.flags], IF_JREL_BACK
    jnz .back
    sub r9, r8
    mov rax, r9
    pop rbx
    leave
    ret
.back:
    sub r8, r9
    mov rax, r8
    pop rbx
    leave
    ret
END_FUNC asm_jump_delta

;; ============================================================================
;; asm_effect_var(int opcode, uint32_t oparg) -> eax = net stack effect
;;
;; The escape hatch for the opcodes whose effect op_meta cannot hold in a
;; constant, because it depends on the oparg.  Two of these carry a flag in
;; bit 0 rather than a count -- LOAD_GLOBAL and LOAD_ATTR push an extra NULL
;; when it is set -- and getting that wrong understates co_stacksize, which is
;; the failure mode that corrupts the frame pool rather than raising.
;; ============================================================================
DEF_FUNC_BARE asm_effect_var
    cmp edi, OP_BUILD_TUPLE
    je .one_minus
    cmp edi, OP_BUILD_LIST
    je .one_minus
    cmp edi, OP_BUILD_SET
    je .one_minus
    cmp edi, OP_BUILD_STRING
    je .one_minus
    cmp edi, OP_BUILD_MAP
    je .build_map
    cmp edi, OP_BUILD_CONST_KEY_MAP
    je .neg_arg
    cmp edi, OP_BUILD_SLICE
    je .build_slice
    cmp edi, OP_UNPACK_SEQUENCE
    je .unpack_seq
    cmp edi, OP_UNPACK_EX
    je .unpack_ex
    cmp edi, OP_CALL
    je .call
    cmp edi, OP_CALL_FUNCTION_EX
    je .call_ex
    cmp edi, OP_MAKE_FUNCTION
    je .make_function
    cmp edi, OP_LOAD_ATTR
    je .load_attr
    cmp edi, OP_LOAD_GLOBAL
    je .load_global
    cmp edi, OP_LOAD_SUPER_ATTR
    je .load_super
    cmp edi, OP_RAISE_VARARGS
    je .neg_arg
    cmp edi, OP_FORMAT_VALUE
    je .format_value
    xor eax, eax                        ; anything else contributes nothing
    ret

.one_minus:                             ; BUILD_*: pops oparg, pushes one
    mov eax, 1
    sub eax, esi
    ret
.build_map:                             ; pops 2*oparg, pushes one
    mov eax, esi
    add eax, eax
    neg eax
    inc eax
    ret
.neg_arg:
    mov eax, esi
    neg eax
    ret
.build_slice:
    mov eax, -1
    cmp esi, 3
    jne .done
    mov eax, -2
.done:
    ret
.unpack_seq:
    lea eax, [rsi - 1]
    ret
.unpack_ex:
    mov eax, esi
    and eax, 0xff
    mov edx, esi
    shr edx, 8
    add eax, edx
    ret
.call:
    mov eax, esi
    neg eax
    dec eax
    ret
.call_ex:
    mov eax, esi
    and eax, 1
    neg eax
    sub eax, 2
    ret
.make_function:
    ; one pop per flag bit set among the low four
    mov eax, esi
    and eax, 0x0f
    popcnt eax, eax
    neg eax
    ret
.load_attr:
    mov eax, esi
    and eax, 1                          ; the method form pushes two, pops one
    ret
.load_global:
    mov eax, esi
    and eax, 1
    inc eax
    ret
.load_super:
    mov eax, esi
    and eax, 1
    sub eax, 2
    ret
.format_value:
    xor eax, eax
    test esi, 4                         ; a format spec is an extra operand
    jz .fv_done
    mov eax, -1
.fv_done:
    ret
END_FUNC asm_effect_var

;; ============================================================================
;; asm_effect(CompUnit *u, uint64_t i, int jump) -> eax = net stack effect
;; ============================================================================
DEF_FUNC asm_effect, 16         ; a frame, because .variable now calls out
    mov rax, [rdi + CompUnit.instrs + Buf.data]
    mov r8, rsi
    shl r8, INSTR_SHIFT
    add rax, r8                         ; rax = Instr*
    movzx ecx, byte [rax + Instr.opcode]
    lea r8, [rel op_meta]
    shl rcx, 2
    add r8, rcx
    test edx, edx
    jz .fallthrough
    movsx edx, byte [r8 + OpMeta.jeff]
    jmp .check
.fallthrough:
    movsx edx, byte [r8 + OpMeta.effect]
.check:
    cmp edx, -128                       ; SE_VAR
    je .variable
    ; movsxd, not mov: a stack effect is signed, and zero-extending -1 into rax
    ; gives 0x00000000ffffffff.  Added to a depth that then gets packed with a
    ; jump target in one qword, it sets bit 32 and corrupts the target.
    movsxd rax, edx
    leave
    ret
.variable:
    movzx edi, byte [rax + Instr.opcode]
    mov esi, [rax + Instr.oparg]
    call asm_effect_var
    movsxd rax, eax
    leave
    ret
END_FUNC asm_effect

;; ============================================================================
;; asm_stackdepth(CompUnit *u) -> rax = the maximum depth, or -1 on a conflict
;;
;; A worklist over the flat instruction array: labels are join points, and a
;; join that arrives at two different depths means the emitters disagree about
;; what is on the stack, which is a codegen bug rather than a program error.
;;
;; The result must be an upper bound.  frame_free recomputes a frame's size
;; from co_nlocalsplus + co_stacksize and returns it to the frame pool, so an
;; under-estimate corrupts the pool's free list and crashes somewhere else
;; entirely.  COMP_STACK_SLACK buys margin against exactly that.
;; ============================================================================
AS_N     equ 16
AS_DEPTH equ 24          ; int32 depths[]
AS_WORK  equ 32          ; worklist of packed (index, depth)
AS_MAX   equ 40
AS_I     equ 48
AS_D     equ 56
AS_CAP   equ 64          ; worklist capacity, in entries
AS_ENTRY equ 72          ; the entry being pushed, across a grow
AS_FRAME equ 88          ; + 5 pushes = 128
;; asm_work_grow(rdi = old, rsi = count, rdx = old capacity)
;;   -> rax = the new buffer (0 on failure), rdx = the new capacity
AWG_BUF   equ 8              ; the reallocated work buffer, across the call
DEF_FUNC_LOCAL asm_work_grow, 40          ; + 3 pushes = 64
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    lea r13, [rdx*2]
    lea rdi, [r13*8]
    call ap_malloc
    test rax, rax
    jz .awg_failed
    mov [rbp - AWG_BUF], rax
    mov rdi, rax
    mov rsi, rbx
    lea rdx, [r12*8]
    call ap_memcpy
    mov rdi, rbx
    call ap_free
    mov rax, [rbp - AWG_BUF]
    mov rdx, r13
    pop r13
    pop r12
    pop rbx
    leave
    ret
.awg_failed:
    xor eax, eax
    xor edx, edx
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_work_grow

DEF_FUNC asm_stackdepth, AS_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov rbx, rdi
    mov r14, [rbx + CompUnit.instrs + Buf.len]
    mov [rbp - AS_N], r14
    mov qword [rbp - AS_MAX], 0
    test r14, r14
    jz .empty

    lea rdi, [r14*4]
    call ap_malloc
    mov [rbp - AS_DEPTH], rax
    mov r15, rax
    xor ecx, ecx
.init:
    mov dword [r15 + rcx*4], -1
    inc rcx
    cmp rcx, r14
    jb .init

    ; The worklist holds (index << 32) | depth; depths here are small and
    ; non-negative, so one qword carries both.
    ; An instruction is visited once now -- a second, different depth is an
    ; error rather than a re-walk -- but an unvisited target can still be
    ; pushed once per incoming edge, so the height is not bounded by the
    ; instruction count.  This starts at one entry per instruction plus room
    ; for the handler targets, and grows.  Writing past it was silent memory
    ; corruption.
    lea rax, [r14 + 64]
    mov [rbp - AS_CAP], rax
    lea rdi, [rax*8]
    call ap_malloc
    mov [rbp - AS_WORK], rax
    mov r13, rax
    xor r12d, r12d                      ; worklist height
    mov qword [r13], 0                  ; entry: instruction 0 at depth 0
    inc r12

.round:
.work:
    test r12, r12
    jz .done
    dec r12
    mov rax, [r13 + r12*8]
    mov rcx, rax
    shr rcx, 32                         ; index
    mov [rbp - AS_I], rcx
    and eax, 0xffffffff                 ; depth
    mov [rbp - AS_D], rax

.walk:
    mov rcx, [rbp - AS_I]
    cmp rcx, r14
    jae .work                           ; ran off the end: nothing to do
    mov r15, [rbp - AS_DEPTH]
    mov eax, [r15 + rcx*4]
    cmp eax, -1
    je .fresh
    ; Already visited.  The depth an instruction runs at is a property of the
    ; instruction, not of the path that reached it, so two different depths at
    ; one join mean the emitters disagree about what is on the stack -- a
    ; codegen bug.  Taking the larger kept co_stacksize an upper bound and let
    ; the bug through: a `return` inside a `finally` left the exception state
    ; on the stack, the enclosing loop's back edge rejoined one word higher
    ; every time, and the only symptom was this pass walking the body forever.
    mov rdx, [rbp - AS_D]
    cmp eax, edx
    jne .conflict
    jmp .work                           ; the same depth: nothing new to learn
.fresh:
    mov rdx, [rbp - AS_D]
    mov [r15 + rcx*4], edx
    cmp rdx, [rbp - AS_MAX]
    jbe .no_max
    mov [rbp - AS_MAX], rdx
.no_max:

    ; The taken edge of a jump, if it has one.
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rcx, [rbp - AS_I]
    shl rcx, INSTR_SHIFT
    lea r15, [rax + rcx]
    movzx ecx, byte [r15 + Instr.opcode]
    lea rax, [rel op_meta]
    shl rcx, 2
    add rax, rcx
    movzx ecx, byte [rax + OpMeta.flags]
    test cl, OM_JUMP
    jz .no_jump

    mov rdi, rbx
    mov rsi, [rbp - AS_I]
    mov edx, 1
    call asm_effect
    add rax, [rbp - AS_D]               ; depth on the taken edge
    ; The fallthrough edge guards this too.  Without it a negative depth
    ; survives the shl/or below as every high bit of the packed word set, and
    ; .walk reads an index past the end and silently drops the successor --
    ; co_stacksize then comes from an incomplete graph.
    js .underflow
    ; Push (target index, that depth).
    mov rdx, [rbx + CompUnit.instrs + Buf.data]
    mov rcx, [rbp - AS_I]
    shl rcx, INSTR_SHIFT
    mov ecx, [rdx + rcx + Instr.oparg]
    mov rdx, [rbx + CompUnit.labels + Buf.data]
    mov ecx, [rdx + rcx*4]
    shl rcx, 32
    or rcx, rax
    cmp r12, [rbp - AS_CAP]
    jb .as_push
    mov [rbp - AS_ENTRY], rcx
    mov rdi, r13
    mov rsi, r12
    mov rdx, [rbp - AS_CAP]
    call asm_work_grow
    test rax, rax
    jz .as_grow_failed
    mov r13, rax
    mov [rbp - AS_WORK], rax
    mov [rbp - AS_CAP], rdx
    mov rcx, [rbp - AS_ENTRY]
.as_push:
    mov [r13 + r12*8], rcx
    inc r12

.no_jump:
    ; The fallthrough edge, unless this opcode ends the block.
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rcx, [rbp - AS_I]
    shl rcx, INSTR_SHIFT
    lea r15, [rax + rcx]
    movzx ecx, byte [r15 + Instr.opcode]
    lea rax, [rel op_meta]
    shl rcx, 2
    add rax, rcx
    movzx ecx, byte [rax + OpMeta.flags]
    test cl, OM_NOFALL
    jnz .work
    test byte [r15 + Instr.flags], IF_NOFALL
    jnz .work

    mov rdi, rbx
    mov rsi, [rbp - AS_I]
    xor edx, edx
    call asm_effect
    add rax, [rbp - AS_D]
    js .underflow
    mov [rbp - AS_D], rax
    mov rcx, [rbp - AS_I]
    inc rcx
    mov [rbp - AS_I], rcx
    jmp .walk

.done:
    ; Now that normal flow has settled, seed every handler that has become
    ; reachable and go round again.  A handler's recorded depth is the depth at
    ; the FIRST instruction it protects; its target is entered with the
    ; exception (and the offset, when lasti is set) already pushed, hence the
    ; +1.  Nested handlers become reachable only through an outer one's edge,
    ; which is why this repeats rather than running once.
    mov rdi, rbx
    mov rsi, [rbp - AS_DEPTH]
    lea rdx, [rbp - AS_MAX]
    call asm_seed_handlers
    test eax, eax
    jz .settled
    ; Push the newly reachable handler targets and rerun the worklist.
    mov rdi, rbx
    mov rsi, [rbp - AS_DEPTH]
    mov rdx, r13
    call asm_push_handler_targets
    mov r12, rax
    ; Go round again whenever a depth was newly determined, not only when a
    ; target was pushed: seeding one handler can make a nested one's region
    ; reachable, and that one has to be seeded too.  This terminates because a
    ; depth only ever goes from unknown to known.
    jmp .round

.settled:
    mov rdi, [rbp - AS_WORK]
    call ap_free
    mov rdi, [rbp - AS_DEPTH]
    call ap_free
    mov rax, [rbp - AS_MAX]
    add rax, COMP_STACK_SLACK
    jmp .ret

.as_grow_failed:
    ; Out of memory growing the worklist.  Report it the way an underflow is
    ; reported: -1, which the caller turns into a compiler error.
    mov rdi, [rbp - AS_DEPTH]
    call ap_free
    mov rax, -1
    jmp .ret

.underflow:
    ; A negative depth means an emitter popped something it never pushed.
    mov rdi, [rbp - AS_WORK]
    call ap_free
    mov rdi, [rbp - AS_DEPTH]
    call ap_free
    mov rax, -1
    jmp .ret

.conflict:
    ; Two depths at one instruction: reported rather than papered over.
    mov rdi, [rbp - AS_WORK]
    call ap_free
    mov rdi, [rbp - AS_DEPTH]
    call ap_free
    mov rax, -2
    jmp .ret

.empty:
    mov eax, COMP_STACK_SLACK
.ret:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_stackdepth

;; ============================================================================
;; asm_loc_varint(Buf *b, uint32_t v) / asm_loc_svarint(Buf *b, int32_t d)
;;
;; The line table's varints are 6-bit chunks LEAST significant first, with bit
;; 6 as the continuation flag.  Note that this is the opposite order from the
;; exception table's, which is most-significant first -- two encodings in one
;; code object, and reading tb_read_varint is the only way to know which is
;; which.  Signed values are zigzagged: the sign rides in bit 0.
;; ============================================================================
DEF_FUNC asm_loc_varint, 16     ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
.loop:
    cmp r12, 64
    jb .last
    mov rsi, r12
    and esi, 0x3f
    or esi, 0x40                        ; more chunks follow
    mov rdi, rbx
    call buf_push_u8
    shr r12, 6
    jmp .loop
.last:
    mov rdi, rbx
    mov rsi, r12
    and esi, 0x3f
    call buf_push_u8
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_loc_varint

DEF_FUNC_BARE asm_loc_svarint
    movsx rsi, esi
    test rsi, rsi
    jns .positive
    neg rsi
    shl rsi, 1
    or rsi, 1
    jmp asm_loc_varint
.positive:
    shl rsi, 1
    jmp asm_loc_varint
END_FUNC asm_loc_svarint

;; ============================================================================
;; asm_linetable(CompUnit *u, Buf *out) -> fills out with a PEP 626 table
;;
;; Only two of the fifteen encodings are used: form 13 (a line number, no
;; column information) and form 15 (no location at all, for prologue
;; instructions).  code_addr2line decodes both, and the compact forms buy only
;; columns, which nothing in apython reads.
;;
;; Two constraints come straight from that decoder: the first byte of every
;; entry must have bit 7 set, and an entry covers at most eight code units --
;; it reads the length from the low three bits.  Longer runs are split, and the
;; continuation chunks carry a delta of zero so the line is not counted twice.
;; The entries must also tile the stream exactly, prefixes and caches included,
;; because the decoder walks by length and never resynchronises.
;; ============================================================================
AL_OUT   equ 16
AL_I     equ 24
AL_N     equ 32
AL_CUR   equ 40
AL_SIZE  equ 48
AL_LINE  equ 56
AL_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC asm_linetable, AL_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - AL_OUT], rsi
    mov r13, [rbx + CompUnit.instrs + Buf.len]
    mov [rbp - AL_N], r13
    mov eax, [rbx + CompUnit.firstline]
    mov [rbp - AL_CUR], rax
    mov qword [rbp - AL_I], 0

.each:
    mov r12, [rbp - AL_I]
    cmp r12, [rbp - AL_N]
    jae .done

    mov rdi, rbx
    mov rsi, r12
    call asm_isize
    mov [rbp - AL_SIZE], rax

    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, r12
    shl rdx, INSTR_SHIFT
    add rax, rdx
    mov ecx, [rax + Instr.line]
    test byte [rax + Instr.flags], IF_NOLINE
    jnz .no_location
    test ecx, ecx
    jz .no_location
    mov [rbp - AL_LINE], rcx

    ; Form 13, split into runs of at most eight code units.  The delta lands on
    ; the first chunk; the rest repeat the same line with a delta of zero.
    mov rax, [rbp - AL_LINE]
    sub rax, [rbp - AL_CUR]
    mov [rbp - AL_LINE], rax            ; the delta to emit first
.line_chunk:
    mov rcx, [rbp - AL_SIZE]
    test rcx, rcx
    jz .next
    cmp rcx, 8
    jbe .have_len
    mov rcx, 8
.have_len:
    mov rdi, [rbp - AL_OUT]
    lea rsi, [rcx - 1]
    or rsi, 0x80 | (13 << 3)
    push rcx
    call buf_push_u8
    mov rdi, [rbp - AL_OUT]
    mov rsi, [rbp - AL_LINE]
    call asm_loc_svarint
    mov qword [rbp - AL_LINE], 0        ; only the first chunk carries a delta
    pop rcx
    sub [rbp - AL_SIZE], rcx
    jmp .line_chunk

.no_location:
    mov rcx, [rbp - AL_SIZE]
    test rcx, rcx
    jz .next
    cmp rcx, 8
    jbe .nl_len
    mov rcx, 8
.nl_len:
    mov rdi, [rbp - AL_OUT]
    lea rsi, [rcx - 1]
    or rsi, 0x80 | (15 << 3)
    push rcx
    call buf_push_u8
    pop rcx
    sub [rbp - AL_SIZE], rcx
    jmp .no_location

.next:
    ; The running line only advances for instructions that carried one.
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, [rbp - AL_I]
    shl rdx, INSTR_SHIFT
    add rax, rdx
    mov ecx, [rax + Instr.line]
    test byte [rax + Instr.flags], IF_NOLINE
    jnz .no_advance
    test ecx, ecx
    jz .no_advance
    mov [rbp - AL_CUR], rcx
.no_advance:
    inc qword [rbp - AL_I]
    jmp .each

.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_linetable

;; ============================================================================
;; asm_write(CompUnit *u, uint64_t total, uint8_t *out)
;;
;; The only place CACHE words and EXTENDED_ARG prefixes are ever produced.  The
;; caches are written as zeros deliberately: the interpreter's inline
;; specializer stores cached type pointers and dictionary versions into them
;; (op_load_global writes eight bytes at [rbx+2]), and a nonzero slot would be
;; read as a live cache entry on the very first execution.
;; ============================================================================
AW_TOT   equ 16
AW_OUT   equ 24
AW_I     equ 32
AW_ARG   equ 40
AW_P     equ 48
AW_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC asm_write, AW_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - AW_TOT], rsi
    mov [rbp - AW_OUT], rdx
    mov r13, rdx                        ; write cursor
    mov qword [rbp - AW_I], 0

.each:
    mov r12, [rbp - AW_I]
    cmp r12, [rbx + CompUnit.instrs + Buf.len]
    jae .done

    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, r12
    shl rdx, INSTR_SHIFT
    lea rcx, [rax + rdx]                ; the Instr*

    ; A jump's oparg is its delta, computed from the settled offsets.
    test byte [rcx + Instr.flags], IF_LABELARG
    jz .literal_arg
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - AW_TOT]
    call asm_jump_delta
    mov [rbp - AW_ARG], rax
    jmp .have_arg
.literal_arg:
    mov eax, [rcx + Instr.oparg]
    mov [rbp - AW_ARG], rax
.have_arg:

    mov rdx, [rbx + CompUnit.prefix + Buf.data]
    movzx eax, byte [rdx + r12]
    mov [rbp - AW_P], rax

    ; EXTENDED_ARG words, most significant first.
.prefix_loop:
    mov rax, [rbp - AW_P]
    test rax, rax
    jz .opcode
    mov byte [r13], OP_EXTENDED_ARG
    mov rcx, rax
    shl rcx, 3                          ; 8 * remaining prefix words
    mov rdx, [rbp - AW_ARG]
    shr rdx, cl
    mov [r13 + 1], dl
    add r13, 2
    dec qword [rbp - AW_P]
    jmp .prefix_loop

.opcode:
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, r12
    shl rdx, INSTR_SHIFT
    lea rcx, [rax + rdx]
    movzx eax, byte [rcx + Instr.opcode]
    mov [r13], al
    mov rdx, [rbp - AW_ARG]
    mov [r13 + 1], dl
    add r13, 2

    ; CACHE words, zeroed.
    lea rdx, [rel op_meta]
    shl rax, 2
    movzx eax, byte [rdx + rax + OpMeta.cache]
.cache_loop:
    test eax, eax
    jz .next
    mov word [r13], 0
    add r13, 2
    dec eax
    jmp .cache_loop

.next:
    inc qword [rbp - AW_I]
    jmp .each
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_write

;; ============================================================================
;; asm_tuple_from_values(Buf *b) -> rax = PyTupleObject*, or 0
;; Builds a tuple from an array of Values, taking a reference to each.  The
;; unit's const and name arrays hold borrowed references -- comp.objs owns the
;; literals -- so the tuple must take its own.
;; ============================================================================
AT_TUP   equ 16
AT_FRAME equ 32          ; + 2 pushes = 48
DEF_FUNC asm_tuple_from_values, AT_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov rdi, [rbx + Buf.len]
    call tuple_new
    test rax, rax
    jz .fail
    mov [rbp - AT_TUP], rax
    mov r12, [rax + PyTupleObject.ob_item]
    xor ecx, ecx
.loop:
    cmp rcx, [rbx + Buf.len]
    jae .done
    mov rdx, [rbx + Buf.data]
    mov rax, [rdx + rcx*8]
    mov [r12 + rcx*8], rax
    INCREF_V rax, rdx
    inc rcx
    jmp .loop
.done:
    mov rax, [rbp - AT_TUP]
.fail:
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_tuple_from_values

;; ============================================================================
;; asm_assemble(Comp *c, CompUnit *u) -> rax = PyCodeObject*, or 0
;;
;; Runs the five passes and hands the result to code_new.  Every object
;; reference is built into a zeroed CodeSpec on the stack; code_new steals the
;; lot on success, and code_spec_clear releases exactly the same set on any
;; failure, so no path here has to unwind by hand.
;; ============================================================================
AA_TOTAL equ 24
AA_CODE  equ 32
AA_LTBUF equ 32 + Buf_size          ; a Buf lives here
AA_SPEC  equ AA_LTBUF + CodeSpec_size
AA_FRAME equ ((AA_SPEC + 15) / 16) * 16 + 8      ; + 3 pushes = 16-aligned
DEF_FUNC asm_assemble, AA_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov qword [rbp - AA_CODE], 0

    ; Zero the spec first: every later step either fills a slot or leaves it
    ; NULL, and code_spec_clear relies on that.
    lea rdi, [rbp - AA_SPEC]
    xor eax, eax
    mov ecx, CodeSpec_size / 8
.zero:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero

    ; Every jump's label must have been bound.  An unbound one holds -1, which
    ; the resolver reads as an unsigned value past the end of the stream and
    ; quietly turns into a jump off the end -- the failure then looks like a
    ; corrupt instruction stream somewhere unrelated.
    mov rdi, r12
    call asm_check_labels
    test eax, eax
    jz .bad_jump

    ; --- pass 1: offsets and EXTENDED_ARG prefixes ---
    mov rdi, r12
    call asm_resolve
    cmp rax, -1
    je .bad_jump
    mov [rbp - AA_TOTAL], rax

    ; --- pass 2: stack depth ---
    mov rdi, r12
    call asm_stackdepth
    cmp rax, -1
    je .bad_depth
    cmp rax, -2
    je .bad_join
    mov [r12 + CompUnit.stacksize], eax

    ; --- pass 3: line table ---
    lea rdi, [rbp - AA_LTBUF]
    mov esi, 1
    call buf_init
    mov rdi, r12
    lea rsi, [rbp - AA_LTBUF]
    call asm_linetable
    mov rdi, [rbp - AA_LTBUF + Buf.data]
    mov rsi, [rbp - AA_LTBUF + Buf.len]
    call bytes_from_data
    mov [rbp - AA_SPEC + CodeSpec.linetable], rax
    lea rdi, [rbp - AA_LTBUF]
    call buf_free

    ; --- pass 4: the bytes ---
    mov rax, [rbp - AA_TOTAL]
    shl rax, 1                          ; code units are two bytes
    mov rdi, rax
    add rdi, 8                          ; never ask ap_malloc for zero
    call ap_malloc
    mov r13, rax
    mov rdi, r12
    mov rsi, [rbp - AA_TOTAL]
    mov rdx, r13
    call asm_write

    mov [rbp - AA_SPEC + CodeSpec.code_bytes], r13
    mov rax, [rbp - AA_TOTAL]
    shl rax, 1
    mov [rbp - AA_SPEC + CodeSpec.code_len], rax

    ; --- the object fields ---
    lea rdi, [r12 + CompUnit.consts]
    call asm_tuple_from_values
    test rax, rax
    jz .oom
    mov [rbp - AA_SPEC + CodeSpec.consts], rax

    lea rdi, [r12 + CompUnit.names]
    call asm_tuple_from_values
    test rax, rax
    jz .oom
    mov [rbp - AA_SPEC + CodeSpec.names], rax

    ; co_localsplusnames comes from the scope's settled layout: varnames, then
    ; the cells needing a new slot, then the free variables last.  Emitting it
    ; in any other order breaks COPY_FREE_VARS, which finds the free slots by
    ; counting back from nlocalsplus.
    mov rdi, rbx
    mov rsi, r12
    call asm_localsplus_tuple
    test rax, rax
    jz .oom
    mov [rbp - AA_SPEC + CodeSpec.localsplusnames], rax
    mov rdi, rax
    call asm_kinds_bytes
    mov [rbp - AA_SPEC + CodeSpec.localspluskinds], rax

    ; --- the exception table ---
    mov rdi, r12
    call asm_debug_handlers             ; only under APYTHON_DUMP_HANDLERS
    lea rdi, [rbp - AA_LTBUF]
    mov esi, 1
    call buf_init
    mov rdi, r12
    lea rsi, [rbp - AA_LTBUF]
    mov rdx, [rbp - AA_TOTAL]
    call asm_exctab
    mov rdi, [rbp - AA_LTBUF + Buf.data]
    mov rsi, [rbp - AA_LTBUF + Buf.len]
    call bytes_from_data
    mov [rbp - AA_SPEC + CodeSpec.exceptiontable], rax
    lea rdi, [rbp - AA_LTBUF]
    call buf_free

    mov rax, [r12 + CompUnit.filename]
    INCREF rax
    mov [rbp - AA_SPEC + CodeSpec.filename], rax
    mov rax, [r12 + CompUnit.name]
    INCREF rax
    mov [rbp - AA_SPEC + CodeSpec.name], rax
    mov rax, [r12 + CompUnit.qualname]
    INCREF rax
    mov [rbp - AA_SPEC + CodeSpec.qualname], rax

    mov eax, [r12 + CompUnit.argcount]
    mov [rbp - AA_SPEC + CodeSpec.argcount], eax
    mov eax, [r12 + CompUnit.posonly]
    mov [rbp - AA_SPEC + CodeSpec.posonlyargcount], eax
    mov eax, [r12 + CompUnit.kwonly]
    mov [rbp - AA_SPEC + CodeSpec.kwonlyargcount], eax
    mov rdi, rbx
    mov rsi, r12
    call asm_nlocals
    mov [rbp - AA_SPEC + CodeSpec.nlocals], eax
    mov eax, [r12 + CompUnit.stacksize]
    mov [rbp - AA_SPEC + CodeSpec.stacksize], eax
    mov eax, [r12 + CompUnit.flags]
    mov [rbp - AA_SPEC + CodeSpec.flags], eax
    mov eax, [r12 + CompUnit.firstline]
    mov [rbp - AA_SPEC + CodeSpec.firstlineno], eax

    lea rdi, [rbp - AA_SPEC]
    call code_new
    mov [rbp - AA_CODE], rax

    mov rdi, r13                        ; the scratch bytecode buffer
    call ap_free
    mov rax, [rbp - AA_CODE]
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bad_jump:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "internal error: jump target resolved backwards"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    jmp .fail
.bad_depth:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "internal error: stack underflow in generated code"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    jmp .fail

.bad_join:
    ; Two paths reach one instruction at different depths.  The line is the
    ; code object's first, not the instruction's: comp_error takes no
    ; formatting, and naming the function beats claiming a statement this pass
    ; cannot identify.
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "internal error: two stack depths at one instruction"
    mov ecx, [r12 + CompUnit.firstline]
    xor r8d, r8d
    call comp_error
    jmp .fail
.oom:
    mov rdi, r13
    call ap_free
.fail:
    lea rdi, [rbp - AA_SPEC]
    call code_spec_clear
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_assemble

;; ============================================================================
;; asm_localsplus_tuple(Comp *c, CompUnit *u) -> rax = tuple, or 0
;; ============================================================================
DEF_FUNC asm_localsplus_tuple, 16
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    mov rax, [r12 + CompUnit.comp]
    test rax, rax
    jz .empty
    mov rdi, rax
    mov esi, [r12 + CompUnit.scope]
    call sym_at
    lea rdi, [rax + Scope.localsplus]
    call asm_tuple_from_values
    pop r12
    pop rbx
    leave
    ret
.empty:
    xor edi, edi
    call tuple_new
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_localsplus_tuple

;; ============================================================================
;; asm_nlocals(Comp *c, CompUnit *u) -> eax = len(varnames)
;; The true count, not nlocalsplus: cells and free variables are not locals.
;; ============================================================================
DEF_FUNC asm_nlocals, 8
    push rbx
    mov rax, [rsi + CompUnit.comp]
    test rax, rax
    jz .zero
    mov rdi, rax
    mov esi, [rsi + CompUnit.scope]
    call sym_at
    mov eax, [rax + Scope.nlocals]
    pop rbx
    leave
    ret
.zero:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC asm_nlocals

;; ============================================================================
;; asm_kinds_bytes(PyTupleObject *localsplusnames) -> rax = bytes, or 0
;; apython never reads co_localspluskinds, but emitting it correctly costs a
;; dozen lines and keeps introspection honest if it is ever wired up.
;; ============================================================================
DEF_FUNC asm_kinds_bytes, 16
    push rbx
    push r12
    mov rbx, rdi
    mov r12, [rdi + PyVarObject.ob_size]
    mov rdi, r12
    add rdi, 8
    call ap_malloc
    mov rbx, rax
    xor ecx, ecx
.fill:
    cmp rcx, r12
    jae .make
    mov byte [rbx + rcx], CO_FAST_LOCAL
    inc rcx
    jmp .fill
.make:
    mov rdi, rbx
    mov rsi, r12
    call bytes_from_data
    push rax
    mov rdi, rbx
    call ap_free
    pop rax
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_kinds_bytes

;; ============================================================================
;; asm_exc_varint(Buf *out, uint32_t v, int msb)
;;
;; The exception table's varints are 6-bit chunks MOST significant first, with
;; bit 6 as the continuation flag -- the opposite order from the line table's,
;; which is least-significant first.  Two encodings in one code object; the
;; only way to know which is which is to read the decoders.
;;
;; `msb` sets bit 7, which marks the first byte of an entry.
;; exc_table_find_handler masks it off (`and eax, 0x3f`), so it is written for
;; CPython's benefit rather than apython's -- but a decoder that does check it
;; would reject a table without it.
;; ============================================================================
EV2_MSB   equ 24
EV2_FRAME equ 32          ; + 2 pushes = 40
DEF_FUNC asm_exc_varint, EV2_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - EV2_MSB], rdx

    mov ecx, 24
.find_top:
    cmp ecx, 0
    je .emit_last
    mov rax, r12
    mov r8, rcx
    push rcx
    mov ecx, r8d
    shr rax, cl
    pop rcx
    test rax, rax
    jnz .emit_chunks
    sub ecx, 6
    jmp .find_top

.emit_chunks:
    ; ecx is the shift of the highest non-zero chunk; walk down from there.
    mov r8, rcx
.chunk_loop:
    mov rax, r12
    mov rcx, r8
    shr rax, cl
    and eax, 0x3f
    or eax, 0x40                        ; more chunks follow
    or rax, [rbp - EV2_MSB]
    mov qword [rbp - EV2_MSB], 0        ; only the first byte carries it
    push r8
    mov rdi, rbx
    mov rsi, rax
    call buf_push_u8
    pop r8
    sub r8, 6
    cmp r8, 0
    jg .chunk_loop
.emit_last:
    mov rax, r12
    and eax, 0x3f
    or rax, [rbp - EV2_MSB]
    mov rdi, rbx
    mov rsi, rax
    call buf_push_u8
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_exc_varint

;; ============================================================================
;; asm_exctab(CompUnit *u, Buf *out, uint64_t total)
;;
;; Every instruction carries the innermost handler covering it, so the ranges
;; are just the maximal runs of one stamp.  They come out ascending and
;; disjoint for free, which is what exc_table_find_handler needs: it scans
;; linearly and returns the FIRST entry containing the offset, so an overlap
;; would silently select the wrong handler.
;; ============================================================================
AX_OUT   equ 16
AX_TOTAL equ 24
AX_I     equ 32
AX_J     equ 40
AX_N     equ 48
AX_H     equ 56
AX_START equ 64
AX_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC asm_exctab, AX_FRAME
%ifdef COMP_DEBUG_HANDLERS
%endif
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - AX_OUT], rsi
    mov [rbp - AX_TOTAL], rdx
    mov rax, [rbx + CompUnit.instrs + Buf.len]
    mov [rbp - AX_N], rax
    mov qword [rbp - AX_I], 0

.scan:
    mov rax, [rbp - AX_I]
    cmp rax, [rbp - AX_N]
    jae .done
    mov rdx, [rbx + CompUnit.instrs + Buf.data]
    mov rcx, rax
    shl rcx, INSTR_SHIFT
    movzx r12d, word [rdx + rcx + Instr.handler]
    test r12d, r12d
    jz .next_unprotected
    mov [rbp - AX_H], r12
    mov ecx, [rdx + rcx + Instr.offset]
    mov [rbp - AX_START], rcx

    ; Extend while the stamp is unchanged.
    mov rax, [rbp - AX_I]
    mov [rbp - AX_J], rax
.extend:
    mov rax, [rbp - AX_J]
    inc rax
    cmp rax, [rbp - AX_N]
    jae .emit
    mov rdx, [rbx + CompUnit.instrs + Buf.data]
    mov rcx, rax
    shl rcx, INSTR_SHIFT
    movzx ecx, word [rdx + rcx + Instr.handler]
    cmp rcx, [rbp - AX_H]
    jne .emit
    mov [rbp - AX_J], rax
    jmp .extend

.emit:
    ; The run ends after instruction AX_J, so its size runs to that
    ; instruction's offset plus its own length.
    mov rdi, rbx
    mov rsi, [rbp - AX_J]
    call asm_isize
    mov rdx, [rbx + CompUnit.instrs + Buf.data]
    mov rcx, [rbp - AX_J]
    shl rcx, INSTR_SHIFT
    mov ecx, [rdx + rcx + Instr.offset]
    add rax, rcx
    sub rax, [rbp - AX_START]
    mov r13, rax                        ; the size, in code units
    test r13, r13
    jz .after

    ; The handler this run belongs to.
    mov rax, [rbp - AX_H]
    dec rax                             ; the stamp is biased by one
    imul rax, rax, Handler_size
    add rax, [rbx + CompUnit.handlers + Buf.data]
    mov r12, rax
    cmp dword [r12 + Handler.depth], -1
    je .after                           ; unreachable protected code

    mov rdi, [rbp - AX_OUT]
    mov rsi, [rbp - AX_START]
    mov edx, 0x80                       ; the entry-start marker
    call asm_exc_varint
    mov rdi, [rbp - AX_OUT]
    mov rsi, r13
    xor edx, edx
    call asm_exc_varint

    ; The target, resolved through the label table.
    mov rax, [rbx + CompUnit.labels + Buf.data]
    mov ecx, [r12 + Handler.target]
    mov ecx, [rax + rcx*4]
    cmp rcx, [rbx + CompUnit.instrs + Buf.len]
    jb .target_bound
    mov rsi, [rbp - AX_TOTAL]
    jmp .have_target
.target_bound:
    mov rax, [rbx + CompUnit.instrs + Buf.data]
    mov rdx, rcx
    shl rdx, INSTR_SHIFT
    mov esi, [rax + rdx + Instr.offset]
.have_target:
    mov rdi, [rbp - AX_OUT]
    xor edx, edx
    call asm_exc_varint

    ; depth_lasti packs both into one value.
    mov eax, [r12 + Handler.depth]
    shl eax, 1
    or eax, [r12 + Handler.lasti]
    mov rdi, [rbp - AX_OUT]
    mov rsi, rax
    xor edx, edx
    call asm_exc_varint

.after:
    mov rax, [rbp - AX_J]
    inc rax
    mov [rbp - AX_I], rax
    jmp .scan
.next_unprotected:
    inc qword [rbp - AX_I]
    jmp .scan
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_exctab

;; ============================================================================
;; asm_region_depth(CompUnit *u, uint64_t h, int32_t *depths)
;;   -> rax = the depth where the region was opened, or -1 if unreached
;;
;; The unwinder truncates the value stack to this depth and then pushes the
;; exception, so it is the count of items belonging to enclosing constructs
;; that have to survive.  A region therefore has to START at the depth its body
;; runs at -- and a `with` opens its region *before* the enter-result has been
;; consumed, so that a failing unpack of its `as` target still runs __exit__.
;; Handler.bias records how many items are live at `open` that the handler
;; unwinds away.
;;
;; Taking the minimum over the region instead looks tempting and is wrong: an
;; except clause ends with POP_EXCEPT, which legitimately drops below the level
;; the handler needs restored.
;; ============================================================================
DEF_FUNC_BARE asm_region_depth
    dec rsi                             ; the stamp is biased by one
    mov rcx, [rdi + CompUnit.handlers + Buf.data]
    imul rsi, rsi, Handler_size
    mov r8d, [rcx + rsi + Handler.open]
    cmp r8, [rdi + CompUnit.instrs + Buf.len]
    jae .none
    ; movsxd, not mov: the sentinel is -1 as a signed 32-bit value, and a
    ; zero-extending load turns it into 0x00000000ffffffff, which no longer
    ; compares equal to -1.  The handler then took a garbage depth and the
    ; depth worklist churned on it forever.
    movsxd rax, dword [rdx + r8*4]
    test rax, rax
    js .unreached                       ; -1 means "not reached yet"
    sub eax, [rcx + rsi + Handler.bias]
    ret
.unreached:
    ret
.none:
    mov rax, -1
    ret
END_FUNC asm_region_depth

;; ============================================================================
;; asm_seed_handlers(CompUnit *u, int32_t *depths, uint64_t *maxdepth)
;;   -> rax = 1 if any handler's depth was newly determined
;; ============================================================================
SH_DEPTH equ 16
SH_MAX   equ 24
SH_I     equ 32
SH_N     equ 40
SH_ANY   equ 48
SH_FRAME equ 56           ; + 3 pushes = 80
DEF_FUNC asm_seed_handlers, SH_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - SH_DEPTH], rsi
    mov [rbp - SH_MAX], rdx
    mov qword [rbp - SH_ANY], 0
    mov rax, [rbx + CompUnit.handlers + Buf.len]
    mov [rbp - SH_N], rax
    mov qword [rbp - SH_I], 0
.loop:
    mov rax, [rbp - SH_I]
    cmp rax, [rbp - SH_N]
    jae .done
    mov rdx, [rbx + CompUnit.handlers + Buf.data]
    imul rax, rax, Handler_size
    lea r12, [rdx + rax]
    cmp dword [r12 + Handler.depth], -1
    jne .next                           ; already known

    mov rdi, rbx
    mov rsi, [rbp - SH_I]
    inc rsi                             ; the stamp is biased
    mov rdx, [rbp - SH_DEPTH]
    call asm_region_depth
    cmp rax, -1
    je .next                            ; the region is not reachable yet
    mov [r12 + Handler.depth], eax
    mov qword [rbp - SH_ANY], 1
.next:
    inc qword [rbp - SH_I]
    jmp .loop
.done:
    mov rax, [rbp - SH_ANY]
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_seed_handlers

;; ============================================================================
;; asm_push_handler_targets(CompUnit *u, int32_t *depths, uint64_t *work)
;;   -> rax = how many entries were pushed
;; ============================================================================
PT_DEPTH equ 16
PT_I     equ 32
PT_N     equ 40
PT_CNT   equ 48
PT_FRAME equ 56           ; + 3 pushes = 80
DEF_FUNC asm_push_handler_targets, PT_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - PT_DEPTH], rsi
    mov r13, rdx
    mov qword [rbp - PT_CNT], 0
    mov rax, [rbx + CompUnit.handlers + Buf.len]
    mov [rbp - PT_N], rax
    mov qword [rbp - PT_I], 0
.loop:
    mov rax, [rbp - PT_I]
    cmp rax, [rbp - PT_N]
    jae .done
    mov rdx, [rbx + CompUnit.handlers + Buf.data]
    imul rax, rax, Handler_size
    lea r12, [rdx + rax]
    mov ecx, [r12 + Handler.depth]
    cmp ecx, -1
    je .next

    ; The target is entered with the exception pushed, plus the offset when
    ; lasti is set.
    mov eax, ecx
    add eax, [r12 + Handler.lasti]
    inc eax
    mov rdx, [rbx + CompUnit.labels + Buf.data]
    mov ecx, [r12 + Handler.target]
    mov ecx, [rdx + rcx*4]
    ; Only push it if the target has not been visited at this depth already.
    mov rdx, [rbp - PT_DEPTH]
    cmp rcx, [rbx + CompUnit.instrs + Buf.len]
    jae .next
    cmp dword [rdx + rcx*4], -1
    jne .next
    shl rcx, 32
    or rcx, rax
    mov rdx, [rbp - PT_CNT]
    mov [r13 + rdx*8], rcx
    inc qword [rbp - PT_CNT]
.next:
    inc qword [rbp - PT_I]
    jmp .loop
.done:
    mov rax, [rbp - PT_CNT]
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_push_handler_targets

;; ============================================================================
;; asm_debug_handlers(CompUnit *u)
;; Prints the handler table when APYTHON_DUMP_HANDLERS is set.  The exception
;; table is the one part of the output that cannot be read off the instruction
;; stream, so having the raw handlers to compare against is worth the dozen
;; lines.
;; ============================================================================
extern getenv
extern dis_num
extern dis_puts
DH_I     equ 16
DH_FRAME equ 32           ; + 2 pushes = 40
DEF_FUNC asm_debug_handlers, DH_FRAME
    push rbx
    push r12
    mov rbx, rdi
    lea rdi, [rel dh_env]
    call getenv wrt ..plt
    test rax, rax
    jz .done
    mov qword [rbp - DH_I], 0
.loop:
    mov rax, [rbp - DH_I]
    cmp rax, [rbx + CompUnit.handlers + Buf.len]
    jae .done
    imul rax, rax, Handler_size
    add rax, [rbx + CompUnit.handlers + Buf.data]
    mov r12, rax
    CSTRING rdi, "handler "
    call dis_puts
    mov rdi, [rbp - DH_I]
    mov esi, 3
    call dis_num
    CSTRING rdi, " open "
    call dis_puts
    movsxd rdi, dword [r12 + Handler.open]
    mov esi, 4
    call dis_num
    CSTRING rdi, " depth "
    call dis_puts
    movsxd rdi, dword [r12 + Handler.depth]
    mov esi, 4
    call dis_num
    CSTRING rdi, " parent "
    call dis_puts
    movsxd rdi, dword [r12 + Handler.parent]
    mov esi, 4
    call dis_num
    CSTRING rdi, `\n`
    call dis_puts
    inc qword [rbp - DH_I]
    jmp .loop
.done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC asm_debug_handlers

section .rodata
dh_env: db "APYTHON_DUMP_HANDLERS", 0

;; ============================================================================
;; asm_check_labels(CompUnit *u) -> rax = 1 if every jump target is bound
;; ============================================================================
CL_I     equ 16
CL_N     equ 24
CL_FRAME equ 24           ; + 1 push = 32
section .text
DEF_FUNC asm_check_labels, CL_FRAME
    push rbx
    mov rbx, rdi
    mov rax, [rbx + CompUnit.instrs + Buf.len]
    mov [rbp - CL_N], rax
    mov qword [rbp - CL_I], 0
.loop:
    mov rax, [rbp - CL_I]
    cmp rax, [rbp - CL_N]
    jae .ok
    mov rdx, [rbx + CompUnit.instrs + Buf.data]
    shl rax, INSTR_SHIFT
    add rdx, rax
    ; A jump is exactly an instruction whose oparg is a label id.
    test byte [rdx + Instr.flags], IF_LABELARG
    jz .next
    mov ecx, [rdx + Instr.oparg]
    mov rax, [rbx + CompUnit.labels + Buf.len]
    cmp rcx, rax
    jae .bad
    mov rax, [rbx + CompUnit.labels + Buf.data]
    mov eax, [rax + rcx*4]
    cmp eax, -1
    je .bad
.next:
    inc qword [rbp - CL_I]
    jmp .loop
.ok:
    mov eax, 1
    pop rbx
    leave
    ret
.bad:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC asm_check_labels

ASM_INIT
