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
%include "types.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ap_free
extern ap_malloc
extern buf_free
extern buf_init
extern buf_push_u8
extern buf_reserve
extern bytes_from_data
extern code_new
extern code_spec_clear
extern comp_error
extern sym_at
extern obj_decref
extern tuple_new
extern op_meta

extern exc_SyntaxError_type

; --- Named frame-layout constants ---
AR_UNIT  equ 8
AR_N     equ 16
AR_I     equ 24
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
AJ_UNIT  equ 8
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
DEF_FUNC_BARE asm_effect
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
    mov eax, edx
    ret
.variable:
    movzx edi, byte [rax + Instr.opcode]
    mov esi, [rax + Instr.oparg]
    jmp asm_effect_var
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
AS_UNIT  equ 8
AS_N     equ 16
AS_DEPTH equ 24          ; int32 depths[]
AS_WORK  equ 32          ; worklist of packed (index, depth)
AS_MAX   equ 40
AS_I     equ 48
AS_D     equ 56
AS_FRAME equ 56          ; + 5 pushes = 96
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
    lea rdi, [r14*8]
    add rdi, 64
    call ap_malloc
    mov [rbp - AS_WORK], rax
    mov r13, rax
    xor r12d, r12d                      ; worklist height
    mov qword [r13], 0                  ; entry: instruction 0 at depth 0
    inc r12

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
    ; Already visited.  Two different depths at one instruction means the
    ; emitters disagree; take the larger so co_stacksize stays an upper bound.
    mov rdx, [rbp - AS_D]
    cmp eax, edx
    jae .work                           ; the recorded depth already covers it
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
    ; Push (target index, that depth).
    mov rdx, [rbx + CompUnit.instrs + Buf.data]
    mov rcx, [rbp - AS_I]
    shl rcx, INSTR_SHIFT
    mov ecx, [rdx + rcx + Instr.oparg]
    mov rdx, [rbx + CompUnit.labels + Buf.data]
    mov ecx, [rdx + rcx*4]
    shl rcx, 32
    or rcx, rax
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
    mov rdi, [rbp - AS_WORK]
    call ap_free
    mov rdi, [rbp - AS_DEPTH]
    call ap_free
    mov rax, [rbp - AS_MAX]
    add rax, COMP_STACK_SLACK
    jmp .ret

.underflow:
    ; A negative depth means an emitter popped something it never pushed.
    mov rdi, [rbp - AS_WORK]
    call ap_free
    mov rdi, [rbp - AS_DEPTH]
    call ap_free
    mov rax, -1
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
AL_UNIT  equ 8
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
AW_UNIT  equ 8
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
AT_BUF   equ 8
AT_TUP   equ 16
AT_I     equ 24
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
AA_COMP  equ 8
AA_UNIT  equ 16
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

    xor edi, edi
    xor esi, esi
    call bytes_from_data                ; no exception table yet
    mov [rbp - AA_SPEC + CodeSpec.exceptiontable], rax

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


ASM_INIT
