; gc.inc - Garbage collector definitions for apython
; Cycle-collecting GC, CPython 3.12 compatible

%ifndef GC_INC
%define GC_INC

; PyGC_Head — prepended BEFORE PyObject in GC-tracked allocations
; User code gets pointer at alloc_ptr + GC_HEAD_SIZE
struc PyGC_Head
    .gc_next: resq 1       ; +0: doubly-linked list next (ptr to PyGC_Head)
    .gc_prev: resq 1       ; +8: prev ptr; low 2 bits = state flags during collection
endstruc

GC_HEAD_SIZE equ PyGC_Head_size   ; 16

; GC state bits stored in low 2 bits of gc_prev
GC_PREV_MASK_COLLECTING equ 1     ; object is in unreachable set
GC_PREV_MASK_REACHABLE  equ 2     ; object is reachable (moved from unreachable)
GC_PREV_MASK_FINALIZED  equ 3     ; object has been finalized
GC_PREV_SHIFT           equ 2     ; gc_refs stored in bits 2+
GC_PREV_MASK            equ ~3    ; mask to extract prev pointer (clear low 2 bits)

; Macros to convert between object pointer and GC head
; GC_FROM_OBJ: obj_ptr -> gc_head_ptr
%macro GC_FROM_OBJ 2              ; %1 = dest reg, %2 = obj reg
    lea %1, [%2 - GC_HEAD_SIZE]
%endmacro

; OBJ_FROM_GC: gc_head_ptr -> obj_ptr
%macro OBJ_FROM_GC 2              ; %1 = dest reg, %2 = gc reg
    lea %1, [%2 + GC_HEAD_SIZE]
%endmacro

; VISIT_FAT — call visit callback on a value if it's a heap pointer
; r14 must be loaded with the visit callback function pointer before use
; rdi is set to the payload (object pointer) for the callback
%macro VISIT_FAT 2                ; %1 = payload_reg, %2 = tag_reg (64-bit)
    test %2, TAG_RC_BIT
    jz %%skip                     ; no RC bit — not a heap pointer
    test %1, %1
    jz %%skip                     ; NULL payload — skip
    mov rdi, %1
    call r14                      ; visit callback(obj)
%%skip:
%endmacro

; VISIT_PTR — call visit callback on a plain heap pointer (NULL-safe)
%macro VISIT_PTR 1                ; %1 = ptr_reg
    test %1, %1
    jz %%skip
    mov rdi, %1
    call r14
%%skip:
%endmacro

%endif ; GC_INC
