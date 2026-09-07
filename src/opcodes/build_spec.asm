; opcodes/build_spec.asm - the specialized container and iteration opcodes
;
; Handlers that a generic opcode rewrites itself into once it has seen the
; shape of its operands.  Each guards that shape, does the work inline, and on
; a guard failure writes the generic opcode byte back and enters the generic
; handler.
;
;   213  FOR_ITER_LIST      a for-loop over a list
;   214  FOR_ITER_RANGE     a for-loop over a range
;
; Split out of build.asm, which keeps the generic handlers and the error
; messages, because that file had 3.5k left under lint's 100k cap for a
; hand-written file -- not enough for the subscript and unpack specializations
; that belong beside these.  Same seam as arith.asm/arith_spec.asm and
; load.asm/load_ic.asm: nothing here calls a file-local helper of the file it
; came from.
;
; FOR_ITER carries a jump offset that can be preceded by an EXTENDED_ARG, so
; the deopts here jump to op_for_iter with ecx intact rather than rewinding
; rbx by two -- rewinding would land past the prefix and read the low byte of
; the offset as the whole of it, jumping into the middle of the loop body on
; exhaustion.  op_for_iter_list keeps the offset on the machine stack because
; it uses rcx as its loop index, and each of its three exits has to dispose of
; that push.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

extern eval_saved_rbx
extern eval_saved_r13
extern opcode_dispatch_table
extern obj_dealloc
extern obj_decref
extern list_iter_type
extern range_iter_type
extern op_for_iter

section .text

;; ============================================================================
;; op_for_iter_range (214) -> nothing; pushes the next item, or jumps past
;;   the loop body when the range is exhausted
;;
;; Guard: TOS ob_type == range_iter_type.  Inlines range_iter_next -- decode
;; current/stop/step, check bounds, push the immediate, advance current -- so
;; an ordinary counting loop makes no call at all.
;;
;; ecx = jump offset. Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_for_iter_range
    VPEEK rdi                      ; iterator (don't pop)
    ; Guard: must be range_iter_type
    lea rax, [rel range_iter_type]
    cmp [rdi + PyObject.ob_type], rax
    jne .fir_deopt

    ; Inline range_iter_next
    mov rax, [rdi + PyRangeIterObject.it_current]

    mov r8, [rdi + PyRangeIterObject.it_stop]

    mov r9, [rdi + PyRangeIterObject.it_step]

    ; Check exhaustion
    test r9, r9
    js .fir_neg_step
    ; Positive step: current >= stop -> exhausted
    cmp rax, r8
    jge .fir_exhausted
    jmp .fir_has_value
.fir_neg_step:
    ; Negative step: current <= stop -> exhausted
    cmp rax, r8
    jle .fir_exhausted

.fir_has_value:
    ; Return current as SmallInt (no INCREF needed for SmallInt)
    mov rdx, rax

    ; Advance: current += step
    add rax, r9
    mov [rdi + PyRangeIterObject.it_current], rax

    VPUSH_INT rdx, r15                  ; push value
    add rbx, 2                     ; skip CACHE
    DISPATCH

.fir_exhausted:
    ; Pop iterator, skip CACHE + jump by (arg + 1)
    ; ecx = saved arg (from instruction word)
    lea rcx, [rcx + 1]            ; arg + 1
    add rbx, 2                     ; skip CACHE
    lea rbx, [rbx + rcx*2]        ; jump forward
    VPOP rdi
    DECREF_V rdi, rsi
    DISPATCH

.fir_deopt:
    ; Type mismatch: rewrite to FOR_ITER (93) and run the generic handler.
    ;
    ; NOT by rewinding rbx and re-dispatching.  A FOR_ITER whose jump offset
    ; is over 255 is preceded by EXTENDED_ARG, and rewinding two bytes lands
    ; on the FOR_ITER alone: the prefix is gone, so the re-execution takes the
    ; low byte of the offset as the whole of it and, on exhaustion, jumps into
    ; the middle of its own loop body.  ecx already holds the full argument,
    ; so entering the generic handler directly is both correct and cheaper.
    mov byte [rbx - 2], 93
    jmp op_for_iter
END_FUNC op_for_iter_range

;; ============================================================================
;; op_for_iter_list (213) -> nothing; pushes the next item, or jumps past the
;;   loop body when the list is exhausted
;;
;; Guard: TOS ob_type == list_iter_type.  Inlines list_iter_next -- bounds
;; check against ob_size, load the item, INCREF, advance the index.
;;
;; ecx = jump offset, and it is kept on the machine stack because rcx is this
;; handler's loop index.  All three exits -- hit, exhausted and deopt -- have
;; to dispose of that one push, and they do it three different ways.
;;
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_for_iter_list
    push rcx                       ; save jump offset (ecx will be clobbered)
    VPEEK rdi                      ; iterator (don't pop)
    ; Guard: must be list_iter_type
    lea rax, [rel list_iter_type]
    cmp [rdi + PyObject.ob_type], rax
    jne .fil_deopt

    ; Inline list_iter_next
    mov rax, [rdi + PyListIterObject.it_seq]       ; list ptr
    mov rcx, [rdi + PyListIterObject.it_index]     ; current index

    ; Check bounds
    cmp rcx, [rax + PyListObject.ob_size]
    jge .fil_exhausted

    ; Get item and INCREF (payload + tag arrays)
    ; A list slot already holds a Value.  This used to V_UNPACK it into a
    ; (payload, tag) pair and V_PACK it straight back around the refcount
    ; bump, which made the SPECIALIZATION slower at handling the value than
    ; the generic list_iter_next it exists to beat -- that one has always
    ; been two instructions here.
    mov rdx, [rax + PyListObject.ob_item]
    mov rax, [rdx + rcx * 8]      ; the item Value
    INCREF_V rax, rdx

    ; Advance index
    inc qword [rdi + PyListIterObject.it_index]

    add rsp, 8                     ; discard saved jump offset
    VPUSH rax
    add rbx, 2                     ; skip CACHE
    DISPATCH

.fil_exhausted:
    ; Mark iterator as exhausted: DECREF list, clear it_seq
    push rdi                       ; save iterator ptr
    mov rdi, [rdi + PyListIterObject.it_seq]
    test rdi, rdi
    jz .fil_already_exhausted
    call obj_decref
.fil_already_exhausted:
    pop rdi
    mov qword [rdi + PyListIterObject.it_seq], 0

    ; Restore the original arg (jump offset)
    pop rcx                        ; restore jump offset
    lea rcx, [rcx + 1]            ; arg + 1
    add rbx, 2                     ; skip CACHE
    lea rbx, [rbx + rcx*2]        ; jump forward
    VPOP rdi
    DECREF_V rdi, rsi
    DISPATCH

.fil_deopt:
    pop rcx                        ; restore the full jump offset
    ; Rewrite to FOR_ITER (93) and enter the generic handler with it -- see
    ; .fir_deopt above for why rewinding rbx instead would be wrong.
    mov byte [rbx - 2], 93
    jmp op_for_iter
END_FUNC op_for_iter_list
