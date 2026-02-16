; opcodes_stack.asm - Opcode handlers for stack manipulation
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
extern obj_dealloc

;; ============================================================================
;; op_pop_top - Pop and discard top of stack, DECREF it
;; ============================================================================
DEF_FUNC_BARE op_pop_top
    sub r13, 16
    mov rax, [r13]              ; payload
    mov rdx, [r13 + 8]         ; tag
    DECREF_VAL rax, rdx
    DISPATCH
END_FUNC op_pop_top

;; ============================================================================
;; op_push_null - Push NULL (0) sentinel onto the value stack
;;
;; Used before LOAD_GLOBAL/LOAD_ATTR to mark callable slots.
;; ============================================================================
DEF_FUNC_BARE op_push_null
    mov qword [r13], 0
    mov qword [r13 + 8], TAG_NULL
    add r13, 16
    DISPATCH
END_FUNC op_push_null

;; ============================================================================
;; op_copy - Copy the i-th item (1-based from top) to top of stack
;;
;; ecx = arg = position (1 = top of stack)
;; Stack layout: ... [r13 - N*8] ... [r13 - 16] [r13 - 8]
;;                                                ^ TOS (position 1)
;; ============================================================================
DEF_FUNC_BARE op_copy
    ; ecx = position (1-indexed from top)
    ; Compute address: r13 - ecx*16 (16 bytes/slot)
    mov rax, rcx
    shl rax, 4                 ; rax = ecx * 16
    mov rdx, r13
    sub rdx, rax               ; rdx = r13 - ecx*16
    mov rax, [rdx]             ; peek payload at position i
    mov rsi, [rdx + 8]        ; peek tag at position i
    INCREF_VAL rax, rsi
    mov [r13], rax
    mov [r13 + 8], rsi
    add r13, 16
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
    ; Swap TOS with i-th item using SSE (0 GPR temps for the swap itself)
    mov rax, rcx
    shl rax, 4                 ; rax = ecx * 16
    mov rdx, r13
    sub rdx, rax               ; rdx = &slot[i]
    lea rsi, [r13 - 16]        ; rsi = &TOS
    VSLOT_SWAP rsi, rdx
    DISPATCH
END_FUNC op_swap
