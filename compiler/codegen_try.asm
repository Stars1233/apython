; codegen_try.asm - try / except / finally and with
;
; The shapes follow CPython's, because they are the shapes apython's unwinder
; already runs from every .pyc it reads.
;
; A `finally` body is emitted more than once on purpose: once on the normal
; path, once on the exceptional one, and once more before every return, break
; or continue that leaves the block.  That duplication is what makes a finally
; clause work without a dedicated opcode, and it is what CPython does too.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern ast_obj_at
extern cg_block
extern cg_body
extern cg_emit
extern cg_emit_jump
extern cg_expr
extern cg_label_bind
extern cg_label_new
extern cg_nameop
extern cg_pop_handler
extern cg_push_handler
extern cg_except_star_clauses
extern cg_await_value
extern cg_store
extern comp_error

extern buf_push_u32
extern buf_push_ptr
extern none_singleton
extern cg_const

extern ap_free
extern ap_malloc
extern ap_memcpy

extern exc_SyntaxError_type

; The finally stack holds AST node indices; this sentinel stands for "a with
; statement is open here", whose unwind action is a call to __exit__.
CG_WITH_MARK equ 0x7fffffff
; The same sentinel for an `async with`: its __exit__ call has to be awaited,
; and the unwinder has no other way to tell the two apart.
CG_AWITH_MARK equ 0x7ffffffe
; An `except*` clause body.  Nothing may leave one early: the unwinding would
; have to reconstruct a partly-matched exception group, and Python does not
; define what that means, so CPython rejects the attempt outright.
CG_ESTAR_MARK equ 0x7ffffffd

; --- Named frame-layout constants ---
CT2_COMP  equ 8
CT2_UNIT  equ 16
CT2_NODE  equ 24
CT2_LINE  equ 32
CT2_END   equ 40
CT2_EXC   equ 48
CT2_CLEAN equ 56
CT2_I     equ 64
CT2_N     equ 72
CT2_NEXT  equ 80
CT2_H     equ 88
CT2_FIN   equ 96
CT2_FRAME equ 104          ; + 3 pushes = 128

section .text

;; ============================================================================
;; cg_finally_push(CompUnit *u, uint32_t block) / cg_finally_pop(CompUnit *u)
;; The stack of finally bodies a `return`, `break` or `continue` has to run on
;; its way out.
;; ============================================================================
DEF_FUNC cg_finally_push, 16
    ; Each entry also carries the handler that was current when the block was
    ; entered.  Leaving the block early has to emit its cleanup *outside* its
    ; own protected region -- that is what leaving it means -- and without a
    ; record of which region that was, the __exit__ call a `return` emits sat
    ; inside the with's own handler.  An exception from it then entered
    ; PUSH_EXC_INFO with a stack the region's recorded depth did not describe.
    mov edx, [rdi + CompUnit.cur_handler]
    cmp edx, -1
    je .fp_no_parent
    mov rax, [rdi + CompUnit.handlers + Buf.data]
    movsxd rcx, edx
    imul rcx, rcx, Handler_size
    mov edx, [rax + rcx + Handler.parent]
.fp_no_parent:
    shl rdx, 32
    or rsi, rdx
    lea rdi, [rdi + CompUnit.finallys]
    call buf_push_ptr
    leave
    ret
END_FUNC cg_finally_push

DEF_FUNC_BARE cg_finally_pop
    dec qword [rdi + CompUnit.finallys + Buf.len]
    ret
END_FUNC cg_finally_pop

;; ============================================================================
;; cg_unwind_finallys(Comp *c, CompUnit *u, uint64_t down_to, int value_on_top)
;;   -> 1 ok, 0 error
;;
;; Emit the finally bodies between the current depth and `down_to`, innermost
;; first.  This is why `return` inside a try/finally runs the finally clause:
;; the body is duplicated ahead of the return rather than jumped to.
;;
;; `value_on_top` says whether a return value is sitting above the with-blocks'
;; __exit__ functions.  It is, for `return expr`, and the call has to reach past
;; it -- hence the SWAP.
;; ============================================================================
UF_COMP  equ 8
UF_UNIT  equ 16
UF_DOWN  equ 24
UF_I     equ 32
UF_VAL   equ 40
UF_ASYNC equ 48
UF_NODE  equ 56
UF_SAVE  equ 64           ; cur_handler on entry, restored on the way out
UF_J     equ 72           ; loops still to leave
UF_POPL  equ 80           ; whether loop iterators come off too
UF_NPOP  equ 88
UF_KEEP  equ 96           ; the slice of the block stack a finally body may
UF_NSAVE equ 104          ; overwrite, and its size in bytes
UF_BLK   equ 112          ; the finally block's node, across the save
UF_RES   equ 120          ; cg_block's result, across the restore
UF_FRAME equ 136          ; + 3 pushes = 160
DEF_FUNC cg_unwind_finallys, UF_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - UF_DOWN], rdx
    mov [rbp - UF_VAL], rcx
    mov [rbp - UF_POPL], r8
    mov rax, [r12 + CompUnit.loops + Buf.len]
    mov [rbp - UF_J], rax
    mov eax, [r12 + CompUnit.cur_handler]
    mov [rbp - UF_SAVE], rax
    mov rax, [r12 + CompUnit.finallys + Buf.len]
    mov [rbp - UF_I], rax
.loop:
    ; A loop entered when the block stack was at least this high sits inside
    ; everything still to unwind, so its iterator comes off first.  A `return`
    ; leaves every enclosing loop; a `break` or `continue` leaves only the one
    ; it names, whose own items its emitter pops itself.  Without this, a
    ; `return` inside `with: for:` called the loop's iterator as if it were
    ; __exit__.
    cmp qword [rbp - UF_POPL], 0
    je .no_loop_pop
.loop_pop:
    mov rax, [rbp - UF_J]
    test rax, rax
    jz .no_loop_pop
    dec rax
    mov rcx, [r12 + CompUnit.loops + Buf.data]
    imul rdx, rax, LoopFrame_size
    mov edx, [rcx + rdx + LoopFrame.fdepth]
    cmp rdx, [rbp - UF_I]
    jb .no_loop_pop
    mov [rbp - UF_J], rax
    mov rcx, [r12 + CompUnit.loops + Buf.data]
    imul rdx, rax, LoopFrame_size
    mov edx, [rcx + rdx + LoopFrame.npop]
    mov [rbp - UF_NPOP], rdx
.npop_loop:
    cmp qword [rbp - UF_NPOP], 0
    je .loop_pop
    ; With a value on top the item to discard is underneath it, so lift it
    ; out of the way first -- the same SWAP CPython emits here.
    cmp qword [rbp - UF_VAL], 0
    je .npop_no_swap
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    xor ecx, ecx
    call cg_emit
.npop_no_swap:
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    dec qword [rbp - UF_NPOP]
    jmp .npop_loop
.no_loop_pop:

    mov rax, [rbp - UF_I]
    cmp rax, [rbp - UF_DOWN]
    jbe .done
    dec rax
    mov [rbp - UF_I], rax
    mov rdx, [r12 + CompUnit.finallys + Buf.data]
    mov rdx, [rdx + rax*8]
    ; The high half is the region enclosing this block.  Emitting the cleanup
    ; there is what puts it outside the block's own region, exactly as
    ; CPython's exception table has it.
    mov rax, rdx
    shr rax, 32
    mov [r12 + CompUnit.cur_handler], eax
    mov edx, edx                        ; the block, in the low half
    test edx, edx
    jz .loop
    ; A `with` registers itself here too, as a sentinel: leaving it early has
    ; to call __exit__ for the same reason leaving a try/finally has to run the
    ; finally body.
    cmp edx, CG_ESTAR_MARK
    je .in_except_star
    xor r8d, r8d
    cmp edx, CG_WITH_MARK
    je .a_with
    mov r8d, 1
    cmp edx, CG_AWITH_MARK
    je .a_with
    ; An except clause registers its own handler node.  Leaving one early has
    ; to pop the exception state and unbind the name, or the exception stays
    ; "being handled" for the rest of the process -- which the interpreter
    ; reports as an unhandled exception at exit, long after the return.
    push rdx
    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    pop rdx
    cmp ecx, AST_HANDLER
    je .an_except
    jmp .a_finally
.a_with:
    mov [rbp - UF_ASYNC], r8
    ; With a return value above the __exit__ function, lift the function back
    ; to the top so the call can reach it, and the value lands beneath the
    ; result that POP_TOP then discards.
    cmp qword [rbp - UF_VAL], 0
    je .no_swap
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    xor ecx, ecx
    call cg_emit
.no_swap:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, 0
    mov rcx, [rbp - UF_ASYNC]
    call cg_call_exit_none
    test eax, eax
    jz .fail
    jmp .loop
.in_except_star:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "'break', 'continue' and 'return' cannot appear in an except* block"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    jmp .fail

.an_except:
    ; The return value sits above the exception state PUSH_EXC_INFO left, so
    ; it has to be lifted out of the way before POP_EXCEPT reaches it.
    mov [rbp - UF_NODE], rdx
    cmp qword [rbp - UF_VAL], 0
    je .exc_no_swap
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    xor ecx, ecx
    call cg_emit
.exc_no_swap:
    mov rdi, r12
    mov esi, OP_POP_EXCEPT
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
    mov rdi, rbx
    mov rsi, [rbp - UF_NODE]
    call ast_at
    mov ecx, [rax + AstNode.b]          ; the bound name, if any
    test ecx, ecx
    jz .loop
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rcx
    xor ecx, ecx
    call cg_clear_exc_name
    test eax, eax
    jz .fail
    jmp .loop

.a_finally:
    ; While emitting the finally body, it is no longer one of the blocks a
    ; nested return has to unwind -- otherwise a `return` inside a `finally`
    ; would emit that same body again, forever.
    ;
    ; Everything from this index up is a block the caller has NOT left, and the
    ; body about to be emitted can open blocks of its own.  buf_push_ptr writes
    ; at data[len], so those land at exactly these indices and overwrite them;
    ; putting the length back afterwards does not put the entries back.  The
    ; next unwind then read a with-mark where a finally node had been and
    ; called the return value as if it were __exit__.  CPython keeps its one
    ; fblock on the C stack and recurses, so the entries above it are safe by
    ; construction; this is a loop, so it keeps the whole slice itself.
    mov [rbp - UF_BLK], rdx
    mov rax, [r12 + CompUnit.finallys + Buf.len]
    mov r13, rax
    sub rax, [rbp - UF_I]
    shl rax, 3                          ; entries are 8 bytes, and never zero
    mov [rbp - UF_NSAVE], rax           ; of them: UF_I was decremented off len
    mov rdi, rax
    call ap_malloc                      ; fatal on OOM, so no failure path
    mov [rbp - UF_KEEP], rax
    mov rdi, rax
    mov rsi, [r12 + CompUnit.finallys + Buf.data]
    mov rax, [rbp - UF_I]
    lea rsi, [rsi + rax*8]
    mov rdx, [rbp - UF_NSAVE]
    call ap_memcpy

    mov rax, [rbp - UF_I]
    mov [r12 + CompUnit.finallys + Buf.len], rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - UF_BLK]
    call cg_block
    mov [rbp - UF_RES], rax

    mov [r12 + CompUnit.finallys + Buf.len], r13
    ; Re-read Buf.data: the body's own blocks can have grown the array, and the
    ; pointer captured before cg_block would be the freed one.
    mov rdi, [r12 + CompUnit.finallys + Buf.data]
    mov rax, [rbp - UF_I]
    lea rdi, [rdi + rax*8]
    mov rsi, [rbp - UF_KEEP]
    mov rdx, [rbp - UF_NSAVE]
    call ap_memcpy
    mov rdi, [rbp - UF_KEEP]
    call ap_free

    ; .fail returns whatever rax holds, and ap_free left it undefined.
    cmp qword [rbp - UF_RES], 0
    jne .loop
    xor eax, eax
    jmp .fail
.done:
    mov eax, 1
.fail:
    ; The caller carries on emitting inside the blocks it has not left.
    push rax
    mov rax, [rbp - UF_SAVE]
    mov [r12 + CompUnit.cur_handler], eax
    pop rax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_unwind_finallys

;; ============================================================================
;; cg_s_try - try / except / else / finally
;;
;; A finally clause wraps everything, including the except clauses: an
;; exception raised inside a handler still runs the finally body.  So the
;; outermost protected region is the finally's, and the try-body's sits inside
;; it.
;;
;;     [push finally handler]
;;       push body handler -> L_exc
;;       <body>
;;       pop
;;       <else>
;;       JUMP_FORWARD L_end
;;     L_exc:
;;       PUSH_EXC_INFO
;;       push handler -> L_clean, lasti
;;       for each clause: <type>; CHECK_EXC_MATCH; POP_JUMP_IF_FALSE L_next
;;                        <bind>; <body>; POP_EXCEPT; JUMP_FORWARD L_end
;;       RERAISE 0
;;       pop
;;     L_clean: COPY 3; POP_EXCEPT; RERAISE 1
;;     L_end:
;;     [pop finally handler; <finally>; JUMP L_done
;;      L_fexc: PUSH_EXC_INFO; <finally>; RERAISE 0
;;      L_fclean: COPY 3; POP_EXCEPT; RERAISE 1
;;      L_done:]
;; ============================================================================
DEF_FUNC cg_s_try, CT2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CT2_LINE], rcx
    mov ecx, [rax + AstNode.c]
    mov [rbp - CT2_FIN], rcx

    mov rdi, r12
    call cg_label_new
    mov [rbp - CT2_END], rax

    ; --- the finally region, if there is one ---
    cmp qword [rbp - CT2_FIN], 0
    je .no_finally_setup
    mov rdi, r12
    call cg_label_new
    mov [rbp - CT2_H], rax              ; L_fexc
    mov rdi, r12
    mov rsi, rax
    xor edx, edx                        ; lasti = 0
    call cg_push_handler
    mov rdi, r12
    mov rsi, [rbp - CT2_FIN]
    call cg_finally_push
.no_finally_setup:

    ; --- the try body ---
    ; Only install a handler for the body when there is an except clause to
    ; run.  A bare try/finally must let the exception reach whatever encloses
    ; it; catching it here would drop it into the finally's normal path and
    ; swallow it.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.a]
    mov [rbp - CT2_EXC], rcx
    test ecx, ecx
    jz .no_body_handler
    mov rdi, r12
    call cg_label_new
    mov [rbp - CT2_EXC], rax
    mov rdi, r12
    mov rsi, rax
    xor edx, edx
    call cg_push_handler
.no_body_handler:

    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_body
    test eax, eax
    jz .fail
    cmp qword [rbp - CT2_EXC], 0
    jz .no_body_pop
    mov rdi, r12
    call cg_pop_handler
.no_body_pop:

    ; --- else, which runs only when the body did not raise ---
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call cg_block
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - CT2_END]
    mov rcx, [rbp - CT2_LINE]
    call cg_emit_jump

    ; --- the handlers ---
    cmp qword [rbp - CT2_EXC], 0
    jz .no_clauses
    mov rdi, r12
    mov rsi, [rbp - CT2_EXC]
    call cg_label_bind
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - CT2_END]
    call cg_except_clauses
    test eax, eax
    jz .fail
.no_clauses:

    mov rdi, r12
    mov rsi, [rbp - CT2_END]
    call cg_label_bind

    ; --- the finally body, on both paths ---
    cmp qword [rbp - CT2_FIN], 0
    je .done
    mov rdi, r12
    call cg_finally_pop
    mov rdi, r12
    call cg_pop_handler

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CT2_FIN]
    call cg_block
    test eax, eax
    jz .fail
    mov rdi, r12
    call cg_label_new
    mov [rbp - CT2_NEXT], rax           ; L_done
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, rax
    mov rcx, [rbp - CT2_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov rsi, [rbp - CT2_H]
    call cg_label_bind
    mov rdi, r12
    call cg_label_new
    mov [rbp - CT2_CLEAN], rax
    mov rdi, r12
    mov rsi, rax
    mov edx, 1                          ; lasti
    call cg_push_handler
    mov rdi, r12
    mov esi, OP_PUSH_EXC_INFO
    xor edx, edx
    mov rcx, [rbp - CT2_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CT2_FIN]
    call cg_block
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_RERAISE
    xor edx, edx
    mov rcx, [rbp - CT2_LINE]
    call cg_emit
    mov rdi, r12
    call cg_pop_handler

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CT2_CLEAN]
    mov rcx, [rbp - CT2_LINE]
    call cg_exc_cleanup
    mov rdi, r12
    mov rsi, [rbp - CT2_NEXT]
    call cg_label_bind
.done:
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_try

;; ============================================================================
;; cg_exc_cleanup(Comp *c, CompUnit *u, uint64_t label, int line)
;; The three instructions every protected handler falls back to: restore the
;; previous exception state and re-raise.
;; ============================================================================
XC_UNIT  equ 8
XC_LABEL equ 16
XC_LINE  equ 24
XC_FRAME equ 32           ; + 2 pushes = 48
DEF_FUNC cg_exc_cleanup, XC_FRAME
    push rbx
    push r12
    mov rbx, rsi
    mov [rbp - XC_LABEL], rdx
    mov [rbp - XC_LINE], rcx
    mov rdi, rbx
    mov rsi, rdx
    call cg_label_bind
    mov rdi, rbx
    mov esi, OP_COPY
    mov edx, 3
    mov rcx, [rbp - XC_LINE]
    call cg_emit
    mov rdi, rbx
    mov esi, OP_POP_EXCEPT
    xor edx, edx
    mov rcx, [rbp - XC_LINE]
    call cg_emit
    mov rdi, rbx
    mov esi, OP_RERAISE
    mov edx, 1
    mov rcx, [rbp - XC_LINE]
    call cg_emit
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_exc_cleanup

;; ============================================================================
;; cg_except_clauses(Comp *c, CompUnit *u, uint32_t try, uint64_t end)
;;   -> 1 ok, 0 error
;;
;; Entered with the exception on the stack.  Each clause tests its type and
;; either handles it or falls through to the next; falling off the end
;; re-raises, which is what makes an unmatched exception propagate.
;; ============================================================================
DEF_FUNC cg_except_clauses, CT2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - CT2_END], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CT2_LINE], rcx
    mov ecx, [rax + AstNode.a]          ; the block of clauses
    mov [rbp - CT2_H], rcx
    cmp qword [rbp - CT2_H], 0
    je .no_handlers

    ; `except*` shares nothing with this but the clause nodes.
    cmp byte [rax + AstNode.subkind], 0
    je .plain
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - CT2_END]
    call cg_except_star_clauses
    jmp .ret
.plain:

    mov rdi, r12
    call cg_label_new
    mov [rbp - CT2_CLEAN], rax
    mov rdi, r12
    mov rsi, rax
    mov edx, 1
    call cg_push_handler
    mov rdi, r12
    mov esi, OP_PUSH_EXC_INFO
    xor edx, edx
    mov rcx, [rbp - CT2_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, [rbp - CT2_H]
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CT2_N], rcx
    mov qword [rbp - CT2_I], 0
.clause_loop:
    mov rax, [rbp - CT2_I]
    cmp rax, [rbp - CT2_N]
    jae .exhausted
    mov rdi, rbx
    mov rsi, [rbp - CT2_H]
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CT2_I]
    mov rdi, rbx
    call ast_child
    mov [rbp - CT2_NEXT], rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    mov rcx, [rbp - CT2_END]
    call cg_one_except
    test eax, eax
    jz .fail
    inc qword [rbp - CT2_I]
    jmp .clause_loop

.exhausted:
    ; No clause matched: put the exception back the way it arrived.
    mov rdi, r12
    mov esi, OP_RERAISE
    xor edx, edx
    mov rcx, [rbp - CT2_LINE]
    call cg_emit
    mov rdi, r12
    call cg_pop_handler
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CT2_CLEAN]
    mov rcx, [rbp - CT2_LINE]
    call cg_exc_cleanup
.no_handlers:
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_except_clauses

;; ============================================================================
;; cg_one_except(Comp *c, CompUnit *u, uint32_t handler, uint64_t end)
;;
;; `except E as e` binds e for the clause and then deletes it, because Python
;; does not let the name outlive the handler -- and the deletion has to happen
;; on the exceptional path too, which is the inner region here.
;; ============================================================================
OE_COMP  equ 8
OE_UNIT  equ 16
OE_NODE  equ 24
OE_END   equ 32
OE_NEXT  equ 40
OE_LINE  equ 48
OE_NAME  equ 56
OE_DEL   equ 64
OE_FRAME equ 72           ; + 3 pushes = 96
DEF_FUNC cg_one_except, OE_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - OE_END], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - OE_LINE], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - OE_NAME], rcx

    mov rdi, r12
    call cg_label_new
    mov [rbp - OE_NEXT], rax

    ; A bare `except:` matches anything and needs no test.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.a]
    test ecx, ecx
    jz .bind

    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_CHECK_EXC_MATCH
    xor edx, edx
    mov rcx, [rbp - OE_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    mov rdx, [rbp - OE_NEXT]
    mov rcx, [rbp - OE_LINE]
    call cg_emit_jump

.bind:
    cmp qword [rbp - OE_NAME], 0
    jne .bind_name
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - OE_LINE]
    call cg_emit
    jmp .body
.bind_name:
    mov rsi, [rbp - OE_NAME]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    test eax, eax
    jz .fail
    ; From here the name is bound, so an exception inside the clause has to
    ; unbind it before propagating.
    mov rdi, r12
    call cg_label_new
    mov [rbp - OE_DEL], rax
    mov rdi, r12
    mov rsi, rax
    mov edx, 1
    call cg_push_handler

.body:
    ; Registering the handler node itself, rather than a sentinel, is what
    ; lets cg_unwind_finallys find the bound name to unbind.
    mov rdi, r12
    mov rsi, r13
    call cg_finally_push
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_body
    push rax
    mov rdi, r12
    call cg_finally_pop
    pop rax
    test eax, eax
    jz .fail

    cmp qword [rbp - OE_NAME], 0
    je .plain_exit
    mov rdi, r12
    call cg_pop_handler
    mov rdi, r12
    mov esi, OP_POP_EXCEPT
    xor edx, edx
    mov rcx, [rbp - OE_LINE]
    call cg_emit
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - OE_NAME]
    mov rcx, [rbp - OE_LINE]
    call cg_clear_exc_name
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - OE_END]
    mov rcx, [rbp - OE_LINE]
    call cg_emit_jump

    ; The exceptional path out of the clause: unbind, then re-raise.
    mov rdi, r12
    mov rsi, [rbp - OE_DEL]
    call cg_label_bind
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - OE_NAME]
    mov rcx, [rbp - OE_LINE]
    call cg_clear_exc_name
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_RERAISE
    mov edx, 1
    mov rcx, [rbp - OE_LINE]
    call cg_emit
    jmp .next

.plain_exit:
    mov rdi, r12
    mov esi, OP_POP_EXCEPT
    xor edx, edx
    mov rcx, [rbp - OE_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - OE_END]
    mov rcx, [rbp - OE_LINE]
    call cg_emit_jump

.next:
    mov rdi, r12
    mov rsi, [rbp - OE_NEXT]
    call cg_label_bind
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_one_except

;; ============================================================================
;; cg_clear_exc_name(Comp *c, CompUnit *u, uint32_t nameobj, int line)
;; `e = None; del e`, which is what Python does at the end of an except clause.
;; ============================================================================
CE2_COMP  equ 8
CE2_UNIT  equ 16
CE2_NAME  equ 24
CE2_LINE  equ 32
CE2_FRAME equ 40          ; + 3 pushes = 64
DEF_FUNC cg_clear_exc_name, CE2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - CE2_NAME], rdx
    mov [rbp - CE2_LINE], rcx

    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, r12
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CE2_LINE]
    call cg_emit

    mov rsi, [rbp - CE2_NAME]
    mov rdi, rbx
    call ast_obj_at
    mov r13, rax
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    test eax, eax
    jz .fail
    mov rdx, r13
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_DEL
    xor r8d, r8d
    call cg_nameop
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_clear_exc_name


;; ============================================================================
;; cg_s_with - `with a as x, b: body`
;;
;;     <manager>; BEFORE_WITH        -> pushes __exit__ and __enter__'s result
;;     [store the as-target | POP_TOP]
;;     <body>
;;     LOAD_CONST None x3; CALL 2; POP_TOP     -> __exit__(None, None, None)
;;   L_exc:
;;     PUSH_EXC_INFO; WITH_EXCEPT_START
;;     POP_JUMP_IF_TRUE L_suppress
;;     RERAISE 2
;;   L_suppress: POP_TOP; POP_EXCEPT; POP_TOP; POP_TOP
;;
;; The three Nones in the normal-path call are the arguments __exit__ takes;
;; the first occupies the self slot, which is why CALL's oparg is 2 for three
;; operands.  Multiple items nest, innermost last.
;; ============================================================================
CW_COMP  equ 8
CW_UNIT  equ 16
CW_NODE  equ 24
CW_LINE  equ 32
CW_I     equ 40
CW_N     equ 48
CW_ITEM  equ 56
CW_EXC   equ 64
CW_CLEAN equ 72
CW_SUPP  equ 80
CW_FRAME equ 88           ; + 3 pushes = 112
DEF_FUNC cg_s_with, CW_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CW_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CW_N], rcx
    mov qword [rbp - CW_I], 0
    mov rdi, rbx
    mov rsi, r13
    mov rdx, 0
    call cg_with_item
    jmp .ret
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_with

;; ============================================================================
;; cg_with_item(Comp *c, uint32_t with, uint64_t i) -> 1 ok, 0 error
;; One context manager, with the remaining ones (and finally the body) nested
;; inside it.  Recursion is the natural shape here: `with a, b:` is exactly
;; `with a: with b:`.
;; ============================================================================
WI_COMP  equ 8
WI_NODE  equ 16
WI_I     equ 24
WI_UNIT  equ 32
WI_LINE  equ 40
WI_ITEM  equ 48
WI_EXC   equ 56
WI_CLEAN equ 64
WI_SUPP  equ 72
WI_N     equ 80
WI_ASYNC equ 88
WI_TGT   equ 96           ; the `as` target, across the region open
WI_FRAME equ 104          ; + 3 pushes = 128
DEF_FUNC cg_with_item, WI_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r13, rsi
    mov [rbp - WI_I], rdx
    mov r12, [rbx + Comp.cur_unit]
    mov [rbp - WI_UNIT], r12

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - WI_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - WI_N], rcx
    movzx ecx, byte [rax + AstNode.subkind]
    mov [rbp - WI_ASYNC], rcx

    mov rax, [rbp - WI_I]
    cmp rax, [rbp - WI_N]
    jb .one_item
    ; Past the last manager: emit the body.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_block
    jmp .ret

.one_item:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - WI_I]
    mov rdi, rbx
    call ast_child
    mov [rbp - WI_ITEM], rax

    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov edx, [rax + AstNode.a]          ; the manager expression
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    ; `async with` differs only in the two awaits it threads through: one on
    ; what __aenter__ returns, one on what each __aexit__ returns.  The block
    ; structure, the suppression test and the cleanup region are identical.
    cmp qword [rbp - WI_ASYNC], 0
    jne .async_enter
    mov rdi, r12
    mov esi, OP_BEFORE_WITH
    xor edx, edx
    mov rcx, [rbp - WI_LINE]
    call cg_emit
    jmp .entered
.async_enter:
    mov rdi, r12
    mov esi, OP_BEFORE_ASYNC_WITH
    xor edx, edx
    mov rcx, [rbp - WI_LINE]
    call cg_emit
    mov rdi, r12
    mov rsi, [rbp - WI_LINE]
    mov edx, 1
    call cg_await_value
    test eax, eax
    jz .fail
.entered:

    mov rdi, r12
    call cg_label_new
    mov [rbp - WI_EXC], rax

    ; Bind or discard __enter__'s result BEFORE opening the protected region.
    ; The region's depth is taken from its first instruction, and only once the
    ; enter-result is gone is the stack at the level the handler restores to --
    ; just the bound __exit__.  CPython covers this instruction too and derives
    ; the depth differently; the cost here is that an exception raised by an
    ; exotic `as` target (a subscript store, say) would not run __exit__.
    mov rdi, rbx
    mov rsi, [rbp - WI_ITEM]
    call ast_at
    mov ecx, [rax + AstNode.b]
    mov [rbp - WI_TGT], rcx

    ; The region opens immediately after BEFORE_WITH, before the `as` target
    ; is stored -- CPython's range starts there too.  Emitting the store
    ; outside it meant a failing unpack skipped __exit__ entirely.
    mov rdi, r12
    mov rsi, [rbp - WI_EXC]
    mov edx, 1                          ; lasti
    call cg_push_handler
    ; The enter-result is still on the stack where the region opens, and the
    ; handler unwinds it away before PUSH_EXC_INFO.
    mov eax, [r12 + CompUnit.cur_handler]
    mov rdx, [r12 + CompUnit.handlers + Buf.data]
    imul rax, rax, Handler_size
    mov dword [rdx + rax + Handler.bias], 1
    mov esi, CG_WITH_MARK
    cmp qword [rbp - WI_ASYNC], 0
    je .mark_sync
    mov esi, CG_AWITH_MARK
.mark_sync:
    mov rdi, r12
    call cg_finally_push

    mov rcx, [rbp - WI_TGT]
    test ecx, ecx
    jz .discard
    mov rdx, rcx
    mov rdi, rbx
    mov rsi, r12
    call cg_store
    test eax, eax
    jz .fail
    jmp .body
.discard:
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - WI_LINE]
    call cg_emit

.body:
    mov rdi, rbx
    mov rsi, r13
    mov rdx, [rbp - WI_I]
    inc rdx
    call cg_with_item
    test eax, eax
    jz .fail

    mov rdi, r12
    call cg_finally_pop
    mov rdi, r12
    call cg_pop_handler

    ; The normal exit: __exit__(None, None, None), result discarded.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - WI_LINE]
    mov rcx, [rbp - WI_ASYNC]
    call cg_call_exit_none
    test eax, eax
    jz .fail
    mov rdi, r12
    call cg_label_new
    mov [rbp - WI_SUPP], rax
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, rax
    mov rcx, [rbp - WI_LINE]
    call cg_emit_jump

    ; The exceptional exit.
    mov rdi, r12
    mov rsi, [rbp - WI_EXC]
    call cg_label_bind
    ; The cleanup region has to START at PUSH_EXC_INFO, not after it: the depth
    ; the unwinder truncates to is the minimum over the region, and only this
    ; instruction sits at the depth the handler expects to find beneath the
    ; exception.
    mov rdi, r12
    call cg_label_new
    mov [rbp - WI_CLEAN], rax
    mov rdi, r12
    mov rsi, rax
    mov edx, 1
    call cg_push_handler
    mov rdi, r12
    mov esi, OP_PUSH_EXC_INFO
    xor edx, edx
    mov rcx, [rbp - WI_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_WITH_EXCEPT_START
    xor edx, edx
    mov rcx, [rbp - WI_LINE]
    call cg_emit
    cmp qword [rbp - WI_ASYNC], 0
    je .tested
    mov rdi, r12
    mov rsi, [rbp - WI_LINE]
    mov edx, 2
    call cg_await_value
    test eax, eax
    jz .fail
.tested:

    mov rdi, r12
    call cg_label_new
    mov [rbp - WI_ITEM], rax            ; the suppression path
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_TRUE
    mov rdx, rax
    mov rcx, [rbp - WI_LINE]
    call cg_emit_jump
    mov rdi, r12
    mov esi, OP_RERAISE
    mov edx, 2
    mov rcx, [rbp - WI_LINE]
    call cg_emit

    mov rdi, r12
    mov rsi, [rbp - WI_ITEM]
    call cg_label_bind
    mov rdi, r12
    call cg_pop_handler
    ; __exit__ returned true: drop its result, the exception state and the two
    ; values BEFORE_WITH left behind.
    mov ecx, 4
.pop_loop:
    push rcx
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - WI_LINE]
    cmp qword [rsp], 3
    jne .plain_pop
    mov esi, OP_POP_EXCEPT
.plain_pop:
    call cg_emit
    pop rcx
    dec ecx
    jnz .pop_loop

    ; The exception was suppressed, so carry on after the with.  Without this
    ; jump control falls straight into the cleanup block below, which exists
    ; only to be reached through the exception table.
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - WI_SUPP]
    mov rcx, [rbp - WI_LINE]
    call cg_emit_jump

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - WI_CLEAN]
    mov rcx, [rbp - WI_LINE]
    call cg_exc_cleanup

    mov rdi, r12
    mov rsi, [rbp - WI_SUPP]
    call cg_label_bind
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_with_item

;; ============================================================================
;; cg_call_exit_none(Comp *c, CompUnit *u, int line, int is_async)
;; __exit__(None, None, None) on the normal path.  CALL's oparg is 2 because
;; the first None sits in the slot a bound method's self would occupy.  An
;; async manager's __aexit__ returns an awaitable, so the result is driven to
;; completion before it is discarded.
;; ============================================================================
CX_UNIT  equ 8
CX_LINE  equ 16
CX_I     equ 24
CX_ASYNC equ 32
CX_FRAME equ 48           ; + 2 pushes = 64
DEF_FUNC cg_call_exit_none, CX_FRAME
    push rbx
    push r12
    mov rbx, rsi
    mov [rbp - CX_LINE], rdx
    mov [rbp - CX_ASYNC], rcx
    mov qword [rbp - CX_I], 3
.loop:
    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, rbx
    call cg_const
    mov rdx, rax
    mov rdi, rbx
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CX_LINE]
    call cg_emit
    dec qword [rbp - CX_I]
    jnz .loop
    mov rdi, rbx
    mov esi, OP_CALL
    mov edx, 2
    mov rcx, [rbp - CX_LINE]
    call cg_emit
    cmp qword [rbp - CX_ASYNC], 0
    je .discard
    mov rdi, rbx
    mov rsi, [rbp - CX_LINE]
    mov edx, 2
    call cg_await_value
    test eax, eax
    jz .ret
.discard:
    mov rdi, rbx
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CX_LINE]
    call cg_emit
    mov eax, 1
.ret:
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_call_exit_none


ASM_INIT
