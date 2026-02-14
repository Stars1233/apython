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
extern obj_dealloc

;; ============================================================================
;; op_pop_top - Pop and discard top of stack, DECREF it
;; ============================================================================
global op_pop_top
op_pop_top:
    VPOP rax
    DECREF rax
    DISPATCH

;; ============================================================================
;; op_push_null - Push NULL (0) sentinel onto the value stack
;;
;; Used before LOAD_GLOBAL/LOAD_ATTR to mark callable slots.
;; ============================================================================
global op_push_null
op_push_null:
    mov qword [r13], 0
    add r13, 8
    DISPATCH

;; ============================================================================
;; op_copy - Copy the i-th item (1-based from top) to top of stack
;;
;; ecx = arg = position (1 = top of stack)
;; Stack layout: ... [r13 - N*8] ... [r13 - 16] [r13 - 8]
;;                                                ^ TOS (position 1)
;; ============================================================================
global op_copy
op_copy:
    ; ecx = position (1-indexed from top)
    ; Compute address: r13 - ecx*8
    mov rax, rcx
    shl rax, 3                 ; rax = ecx * 8
    mov rdx, r13
    sub rdx, rax               ; rdx = r13 - ecx*8
    mov rax, [rdx]             ; peek at position i
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_swap - Swap TOS with the i-th item (1-indexed from top)
;;
;; ecx = arg = position (1 = top, so swap(1) is a no-op, swap(2) swaps
;;   top two items, etc.)
;; TOS is at [r13-8], i-th item is at [r13 - i*8]
;; No reference count changes needed (just moving pointers).
;; ============================================================================
global op_swap
op_swap:
    ; ecx = position (1-indexed from top)
    ; Compute address of i-th item: r13 - ecx*8
    mov rax, rcx
    shl rax, 3                 ; rax = ecx * 8
    mov rdx, r13
    sub rdx, rax               ; rdx = &stack[top - i] (i-th from top)
    ; Swap [r13-8] (TOS) and [rdx] (i-th item)
    mov rax, [r13 - 8]         ; rax = TOS
    mov r8, [rdx]              ; r8 = item at position i
    mov [rdx], rax             ; store TOS into position i
    mov [r13 - 8], r8          ; store old position i into TOS
    DISPATCH
