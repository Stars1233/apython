; opcodes_stack.asm - Opcode handlers for stack manipulation
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack payload top pointer
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
extern eval_saved_r13
extern opcode_table
extern obj_dealloc

;; ============================================================================
;; op_pop_top - Pop and discard top of stack, DECREF it
;; ============================================================================
DEF_FUNC_BARE op_pop_top
    VPOP rax
    DECREF_V rax, rdx
    DISPATCH
END_FUNC op_pop_top

;; ============================================================================
;; op_push_null - Push NULL (0) sentinel onto the value stack
;;
;; Used before LOAD_GLOBAL/LOAD_ATTR to mark callable slots.
;; ============================================================================
DEF_FUNC_BARE op_push_null
    VPUSH_NULL128
    DISPATCH
END_FUNC op_push_null

;; ============================================================================
;; op_copy - Copy the i-th item (1-based from top) to top of stack
;;
;; ecx = arg = position (1 = top of stack)
;; Stack layout: ... [r13 - N*8] ... [r13 - 8] [r13]
;;                                                ^ TOS (position 1)
;; ============================================================================
DEF_FUNC_BARE op_copy
    ; ecx = position (1-indexed from top)
    ; Compute address: r13 - ecx*8 (8 bytes/slot)
    mov rax, rcx
    mov rdx, r13
    shl rax, 3
    sub rdx, rax               ; slot i
    mov rax, [rdx]             ; peek the Value at position i
    INCREF_V rax, rsi
    mov [r13], rax
    add r13, 8
    DISPATCH
END_FUNC op_copy

;; ============================================================================
;; op_swap - Swap TOS with the i-th item (1-indexed from top)
;;
;; ecx = arg = position (1 = top, so swap(1) is a no-op, swap(2) swaps
;;   top two items, etc.)
;; TOS is at [r13-8], i-th item is at [r13 - i*8]
;; No reference count changes needed (just moving pointers).
;; ============================================================================
DEF_FUNC_BARE op_swap
    ; ecx = position (1-indexed from top)
    ; Swap TOS with the i-th item -- one word each now
    mov rax, rcx
    mov rdx, r13
    shl rax, 3
    sub rdx, rax               ; slot i
    mov r9, [rdx]
    mov r10, [r13 - 8]
    mov [rdx], r10
    mov [r13 - 8], r9
    DISPATCH
END_FUNC op_swap
