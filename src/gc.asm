; gc.asm - Cycle-collecting garbage collector for apython
; CPython 3.12 compatible generational GC
;
; Three generations: gen0 (young), gen1, gen2 (old)
; Each generation is a doubly-linked list of PyGC_Head nodes.
; gc_refs stored in gc_prev high bits during collection.

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern obj_incref
extern frame_free

;; ============================================================================
;; GC data (BSS + DATA)
;; ============================================================================
section .data

; Generation 0 sentinel (young objects)
align 8
gc_gen0:
    dq gc_gen0          ; gc_next -> self (empty list)
    dq gc_gen0          ; gc_prev -> self
global gc_gen0_count
gc_gen0_count:  dq 0
global gc_gen0_threshold
gc_gen0_threshold: dq 700

; Generation 1 sentinel
align 8
gc_gen1:
    dq gc_gen1
    dq gc_gen1
global gc_gen1_count
gc_gen1_count:  dq 0
global gc_gen1_threshold
gc_gen1_threshold: dq 10
gc_gen1_collections: dq 0

; Generation 2 sentinel
align 8
gc_gen2:
    dq gc_gen2
    dq gc_gen2
global gc_gen2_count
gc_gen2_count:  dq 0
global gc_gen2_threshold
gc_gen2_threshold: dq 10
gc_gen2_collections: dq 0

; GC state
global gc_enabled
gc_enabled:     dq 1
global gc_collecting        ; eval_exception_unwind resets this: a raising
                            ; __del__ during a collection longjmps out and
                            ; would otherwise latch it on for good
gc_collecting:  dq 0    ; reentrancy guard

; Generation table (for indexed access)
align 8
gc_generations:
    dq gc_gen0, gc_gen0_count, gc_gen0_threshold, 0                ; gen 0
    dq gc_gen1, gc_gen1_count, gc_gen1_threshold, gc_gen1_collections ; gen 1
    dq gc_gen2, gc_gen2_count, gc_gen2_threshold, gc_gen2_collections ; gen 2

GEN_ENTRY_SIZE equ 32   ; 4 qwords per generation entry

section .text


;; ============================================================================
;; gc_list_append(rdi=node, rsi=list_sentinel)
;; Insert node at the tail of list (before sentinel)
;; ============================================================================
DEF_FUNC_BARE gc_list_append
    ; node->gc_next = sentinel
    mov [rdi + PyGC_Head.gc_next], rsi
    ; node->gc_prev = sentinel->gc_prev (preserve low bits = 0 for tracked)
    mov rax, [rsi + PyGC_Head.gc_prev]
    and rax, GC_PREV_MASK          ; clear state bits from prev pointer
    mov [rdi + PyGC_Head.gc_prev], rax
    ; old_tail->gc_next = node
    mov [rax + PyGC_Head.gc_next], rdi
    ; sentinel->gc_prev = node (keep state bits = 0)
    mov [rsi + PyGC_Head.gc_prev], rdi
    ret
END_FUNC gc_list_append

;; ============================================================================
;; gc_list_remove(rdi=node)
;; Remove node from its doubly-linked list
;; ============================================================================
DEF_FUNC_BARE gc_list_remove
    mov rax, [rdi + PyGC_Head.gc_prev]
    and rax, GC_PREV_MASK          ; clear state bits
    mov rcx, [rdi + PyGC_Head.gc_next]
    ; prev->gc_next = node->gc_next
    mov [rax + PyGC_Head.gc_next], rcx
    ; next->gc_prev = prev (copy prev, preserve next's state bits)
    mov rdx, [rcx + PyGC_Head.gc_prev]
    and rdx, ~GC_PREV_MASK         ; keep state bits
    or rax, rdx                    ; combine prev ptr with state bits
    mov [rcx + PyGC_Head.gc_prev], rax
    ret
END_FUNC gc_list_remove


;; ============================================================================
;; gc_list_merge(rdi=from_sentinel, rsi=to_sentinel)
;; Move all nodes from 'from' list to end of 'to' list. Empties 'from'.
;; ============================================================================
DEF_FUNC gc_list_merge
    push rbx

    ; Check if from is empty
    mov rax, [rdi + PyGC_Head.gc_next]
    cmp rax, rdi
    je .empty

    ; from_head = from->gc_next, from_tail = from->gc_prev
    mov rbx, rax                   ; rbx = from_head
    mov rcx, [rdi + PyGC_Head.gc_prev]
    and rcx, GC_PREV_MASK          ; rcx = from_tail (clear bits)

    ; to_tail = to->gc_prev
    mov rdx, [rsi + PyGC_Head.gc_prev]
    and rdx, GC_PREV_MASK          ; rdx = to_tail

    ; to_tail->gc_next = from_head
    mov [rdx + PyGC_Head.gc_next], rbx
    ; from_head->gc_prev = to_tail
    mov [rbx + PyGC_Head.gc_prev], rdx
    ; from_tail->gc_next = to_sentinel
    mov [rcx + PyGC_Head.gc_next], rsi
    ; to_sentinel->gc_prev = from_tail
    mov [rsi + PyGC_Head.gc_prev], rcx

    ; Empty the from list
    mov [rdi + PyGC_Head.gc_next], rdi
    mov [rdi + PyGC_Head.gc_prev], rdi

.empty:
    pop rbx
    leave
    ret
END_FUNC gc_list_merge

;; ============================================================================
;; gc_alloc(rdi=size, rsi=type) -> PyObject*
;; Allocate a GC-tracked object. Prepends PyGC_Head, returns obj pointer.
;; ============================================================================
DEF_FUNC gc_alloc
    push rbx
    push r12

    mov rbx, rdi               ; size
    mov r12, rsi               ; type

    ; Allocate size + GC_HEAD_SIZE
    lea rdi, [rbx + GC_HEAD_SIZE]
    call ap_malloc
    ; rax = raw alloc ptr (PyGC_Head)

    ; Zero the GC head
    mov qword [rax + PyGC_Head.gc_next], 0
    mov qword [rax + PyGC_Head.gc_prev], 0

    ; Object starts after GC head
    lea rbx, [rax + GC_HEAD_SIZE]

    ; Init ob_refcnt=1, ob_type=type
    mov qword [rbx + PyObject.ob_refcnt], 1
    mov [rbx + PyObject.ob_type], r12

    mov rax, rbx               ; return obj ptr
    pop r12
    pop rbx
    leave
    ret
END_FUNC gc_alloc

;; ============================================================================
;; gc_track(rdi=obj)
;; Add object to gen0 tracking list. May trigger collection.
;; ============================================================================
DEF_FUNC gc_track
    push rbx
    mov rbx, rdi               ; save obj

    ; gc = obj - GC_HEAD_SIZE
    lea rdi, [rbx - GC_HEAD_SIZE]
    ; Already in a list?  Appending a second time makes the node's own links
    ; point into two places at once, and the next removal writes through the
    ; stale pair -- an arbitrary 8-byte store into freed memory.
    cmp qword [rdi + PyGC_Head.gc_next], 0
    jne .already_tracked
    ; Append to gen0 list
    lea rsi, [rel gc_gen0]
    call gc_list_append

    ; Increment gen0 count
    inc qword [rel gc_gen0_count]

    ; Check threshold
    mov rax, [rel gc_gen0_count]
    cmp rax, [rel gc_gen0_threshold]
    jle .done

    ; Check if GC is enabled and not already collecting
    cmp qword [rel gc_enabled], 0
    je .done
    cmp qword [rel gc_collecting], 0
    jne .done

    ; Trigger gen0 collection
    xor edi, edi               ; gen = 0
    call gc_collect_gen

.done:
    pop rbx
    leave
    ret

.already_tracked:
    pop rbx
    leave
    ret
END_FUNC gc_track

;; ============================================================================
;; gc_untrack(rdi=obj)
;; Remove object from GC tracking list.
;; ============================================================================
DEF_FUNC gc_untrack
    ; gc = obj - GC_HEAD_SIZE
    lea rdi, [rdi - GC_HEAD_SIZE]

    ; Check if actually tracked (gc_next != 0)
    cmp qword [rdi + PyGC_Head.gc_next], 0
    je .not_tracked

    call gc_list_remove

    ; Mark the node untracked.  gc_list_remove only fixes up the neighbours;
    ; it leaves this node's own links pointing into the list.  Every "is it
    ; tracked?" test in the collector and in gc_dealloc reads gc_next, so a
    ; stale one made a second untrack unlink a node that was no longer in any
    ; list -- writing through pointers into freed memory.
    mov qword [rdi + PyGC_Head.gc_next], 0
    mov qword [rdi + PyGC_Head.gc_prev], 0

    ; We don't track which gen it's in, so decrement gen0 count
    ; (approximate — during collection, counts are managed differently)
    dec qword [rel gc_gen0_count]
    jns .done
    mov qword [rel gc_gen0_count], 0

.not_tracked:
.done:
    leave
    ret
END_FUNC gc_untrack

;; ============================================================================
;; gc_dealloc(rdi=obj)
;; Untrack and free a potentially GC-tracked object.
;; If TYPE_FLAG_HAVE_GC is set: untrack + free at obj - GC_HEAD_SIZE
;; Otherwise: plain ap_free(obj) (for objects allocated without GC head)
;; ============================================================================
DEF_FUNC gc_dealloc
    push rbx
    mov rbx, rdi

    ; Check if this type uses GC allocation
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .plain_free                 ; safety: no type -> plain free
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HAVE_GC
    jz .plain_free

    ; GC-allocated object: untrack if tracked
    lea rdi, [rbx - GC_HEAD_SIZE]
    cmp qword [rdi + PyGC_Head.gc_next], 0
    je .gc_free

    mov rdi, rbx
    call gc_untrack

.gc_free:
    ; Free at the GC head address
    lea rdi, [rbx - GC_HEAD_SIZE]
    call ap_free
    jmp .done

.plain_free:
    ; Non-GC object: standard free
    mov rdi, rbx
    call ap_free

.done:
    pop rbx
    leave
    ret
END_FUNC gc_dealloc

;; ============================================================================
;; gc_collect_gen(edi=generation) -> rax = the unreachable objects it cleared
;; Core cycle collection algorithm.
;; ============================================================================
; Local frame layout
GCG_GEN     equ 8
GCG_YOUNG   equ 24    ; 16-byte PyGC_Head sentinel on stack (next+prev)
GCG_UNREACH equ 40    ; 16-byte PyGC_Head sentinel on stack
GCG_FOUND   equ 56    ; how many unreachable objects this pass cleared
GCG_FRAME   equ 64          ; + 5 pushes = 104

global gc_collect_gen
DEF_FUNC gc_collect_gen, GCG_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - GCG_GEN], edi    ; save generation
    mov qword [rbp - GCG_FOUND], 0

    ; Set collecting flag
    mov qword [rel gc_collecting], 1

    ; Initialize young list sentinel on stack
    lea rax, [rbp - GCG_YOUNG]
    mov [rax + PyGC_Head.gc_next], rax
    mov [rax + PyGC_Head.gc_prev], rax

    ; Initialize unreachable list sentinel on stack
    lea rax, [rbp - GCG_UNREACH]
    mov [rax + PyGC_Head.gc_next], rax
    mov [rax + PyGC_Head.gc_prev], rax

    ; Merge gen 0..gen into young list
    xor r12d, r12d             ; i = 0
.merge_loop:
    cmp r12d, [rbp - GCG_GEN]
    jg .merge_done

    ; Get gen[i] sentinel address
    lea rdi, [rel gc_generations]
    imul eax, r12d, GEN_ENTRY_SIZE
    mov rdi, [rdi + rax]      ; gen[i].sentinel

    lea rsi, [rbp - GCG_YOUNG]
    call gc_list_merge

    inc r12d
    jmp .merge_loop
.merge_done:

    ; ---- Phase 1: Set gc_refs = ob_refcnt for all objects in young ----
    lea r12, [rbp - GCG_YOUNG]  ; r12 = young sentinel
    mov rbx, [r12 + PyGC_Head.gc_next]  ; rbx = first gc node

.phase1_loop:
    cmp rbx, r12
    je .phase1_done

    ; obj = gc + GC_HEAD_SIZE
    lea rax, [rbx + GC_HEAD_SIZE]
    mov rcx, [rax + PyObject.ob_refcnt]

    ; Store gc_refs in gc_prev, with the COLLECTING bit set.  That bit is
    ; what marks a node as belonging to *this* collection: phase 2 traverses
    ; young objects but reaches referents in older generations too, and those
    ; still hold a real prev pointer in the same field.  Without the mark,
    ; gc_visit_decref subtracted 4 from an older node's prev pointer and the
    ; list was silently corrupt until something tried to unlink through it.
    shl rcx, GC_PREV_SHIFT
    or rcx, GC_PREV_MASK_COLLECTING
    mov [rbx + PyGC_Head.gc_prev], rcx

    mov rbx, [rbx + PyGC_Head.gc_next]
    jmp .phase1_loop
.phase1_done:

    ; ---- Phase 2: Traverse all objects, decrement gc_refs of visited targets ----
    mov rbx, [r12 + PyGC_Head.gc_next]

.phase2_loop:
    cmp rbx, r12
    je .phase2_done

    lea r13, [rbx + GC_HEAD_SIZE]  ; r13 = obj
    mov rax, [r13 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_traverse]
    test rax, rax
    jz .phase2_next

    ; Call tp_traverse(obj, visit_decref, NULL)
    ; Use r14 for the visit callback used by the VISIT_* macros
    push rbx
    mov rdi, r13               ; obj
    lea r14, [rel gc_visit_decref]  ; visit callback
    call rax                   ; tp_traverse
    pop rbx

.phase2_next:
    mov rbx, [rbx + PyGC_Head.gc_next]
    jmp .phase2_loop
.phase2_done:

    ; ---- Phase 3: Partition into reachable (gc_refs > 0) vs unreachable ----
    ; NOTE: gc_prev was overwritten with gc_refs in phase 1, so we cannot use
    ; gc_list_remove (which needs gc_prev). Instead, track prev pointer (r14)
    ; and rebuild gc_prev as we walk gc_next.
    lea r15, [rbp - GCG_UNREACH]  ; r15 = unreachable sentinel
    mov r14, r12                   ; r14 = prev (starts at young sentinel)

    mov rbx, [r12 + PyGC_Head.gc_next]

.phase3_loop:
    cmp rbx, r12
    je .phase3_done

    mov r13, [rbx + PyGC_Head.gc_next]  ; save next

    ; Check gc_refs (stored in gc_prev high bits from phase 1)
    mov rax, [rbx + PyGC_Head.gc_prev]
    shr rax, GC_PREV_SHIFT     ; gc_refs
    test rax, rax
    jnz .phase3_keep           ; gc_refs > 0 — tentatively reachable

    ; Unreachable: unlink from young by setting prev->gc_next = next
    mov [r14 + PyGC_Head.gc_next], r13
    ; (don't update r14 — prev stays the same)

    ; Append to unreachable list (gc_list_append maintains gc_prev properly)
    mov rdi, rbx
    mov rsi, r15
    call gc_list_append

    ; Mark as collecting (set bit in gc_prev)
    mov rax, [rbx + PyGC_Head.gc_prev]
    or rax, GC_PREV_MASK_COLLECTING
    mov [rbx + PyGC_Head.gc_prev], rax

    jmp .phase3_next

.phase3_keep:
    ; Reachable: restore gc_prev to point to prev node
    mov [rbx + PyGC_Head.gc_prev], r14
    mov r14, rbx               ; update prev

.phase3_next:
    mov rbx, r13
    jmp .phase3_loop

.phase3_done:
    ; Fix young sentinel's gc_prev to point to last reachable node
    mov [r12 + PyGC_Head.gc_prev], r14

    ; ---- Phase 4: Traverse reachable roots, rescue transitively reachable ----
    ; Set the global so gc_visit_reachable knows where to move rescued objects
    mov [rel gc_reachable_sentinel], r12

    mov rbx, [r12 + PyGC_Head.gc_next]

.phase4_loop:
    cmp rbx, r12
    je .phase4_done

    lea r13, [rbx + GC_HEAD_SIZE]  ; obj
    mov rax, [r13 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_traverse]
    test rax, rax
    jz .phase4_next

    push rbx
    mov rdi, r13
    lea r14, [rel gc_visit_reachable]
    call rax
    pop rbx

.phase4_next:
    mov rbx, [rbx + PyGC_Head.gc_next]
    jmp .phase4_loop
.phase4_done:

    ; ---- Phase 5: Clear the unreachable set, letting DECREF free it ----
    ; One pass, and the head is re-read from the sentinel every time round.
    ; Caching a next pointer across tp_clear is what broke here: clearing one
    ; object drops references to others in the same list, so the node that
    ; had been saved as "next" was often freed before the walk reached it.
    ; Holding a reference across tp_clear is the other half -- it keeps the
    ; object under our own feet alive, exactly as delete_garbage does.
.phase5_loop:
    mov rbx, [r15 + PyGC_Head.gc_next]
    cmp rbx, r15
    je .phase5_done
    inc qword [rbp - GCG_FOUND]            ; what gc.collect() answers with

    lea r13, [rbx + GC_HEAD_SIZE]          ; r13 = obj
    inc qword [r13 + PyObject.ob_refcnt]

    mov rax, [r13 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_clear]
    test rax, rax
    jz .phase5_no_clear
    mov rdi, r13
    call rax                               ; tp_clear(obj)
.phase5_no_clear:

    ; Still at the head?  Then clearing it did not take it out of the list,
    ; and the loop has to, or it never advances.  Survivors join the young
    ; list and are promoted with it; CPython moves them to `old` the same way.
    cmp qword [r15 + PyGC_Head.gc_next], rbx
    jne .phase5_drop
    mov rdi, rbx
    call gc_list_remove
    mov rdi, rbx
    mov rsi, r12
    call gc_list_append

.phase5_drop:
    mov rdi, r13
    dec qword [rdi + PyObject.ob_refcnt]
    jnz .phase5_loop
    call obj_dealloc
    jmp .phase5_loop

.phase5_done:

    ; ---- Move surviving reachable objects to next generation ----
    mov eax, [rbp - GCG_GEN]
    cmp eax, 2
    jge .promote_to_gen2
    inc eax
    jmp .do_promote
.promote_to_gen2:
    mov eax, 2
.do_promote:
    ; Get target generation sentinel
    lea rdi, [rel gc_generations]
    imul ecx, eax, GEN_ENTRY_SIZE
    mov rsi, [rdi + rcx]      ; target gen sentinel

    lea rdi, [rbp - GCG_YOUNG]
    call gc_list_merge

    ; Reset gen0 count (approximate)
    mov qword [rel gc_gen0_count], 0

    ; Update collection counters for triggering higher-gen collections
    mov eax, [rbp - GCG_GEN]
    test eax, eax
    jz .check_gen1

    cmp eax, 1
    je .inc_gen2_colls
    jmp .collect_done

.check_gen1:
    ; Gen0 collection done — increment gen1 collection counter
    inc qword [rel gc_gen1_collections]
    mov rax, [rel gc_gen1_collections]
    cmp rax, [rel gc_gen1_threshold]
    jl .collect_done
    mov qword [rel gc_gen1_collections], 0
    ; Trigger gen1 collection
    mov edi, 1
    call gc_collect_gen
    jmp .collect_done

.inc_gen2_colls:
    ; Gen1 collection done — increment gen2 collection counter
    inc qword [rel gc_gen2_collections]
    mov rax, [rel gc_gen2_collections]
    cmp rax, [rel gc_gen2_threshold]
    jl .collect_done
    mov qword [rel gc_gen2_collections], 0
    ; Trigger gen2 collection
    mov edi, 2
    call gc_collect_gen

.collect_done:
    ; Clear collecting flag
    mov qword [rel gc_collecting], 0
    mov rax, [rbp - GCG_FOUND]

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC gc_collect_gen


;; ============================================================================
;; gc_visit_decref(rdi=obj)
;; Visit callback for Phase 2: decrement gc_refs of visited object
;; ============================================================================
DEF_FUNC_BARE gc_visit_decref
    ; Check if this object's type has HAVE_GC flag (non-GC objects have no GC head)
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .skip
    ; gc = obj - GC_HEAD_SIZE
    lea rax, [rdi - GC_HEAD_SIZE]
    ; Check if object is tracked (gc_next != 0)
    cmp qword [rax + PyGC_Head.gc_next], 0
    je .skip
    ; Only nodes in the generation being collected carry gc_refs here; every
    ; other tracked object still has a live prev pointer in this field.
    mov rcx, [rax + PyGC_Head.gc_prev]
    test rcx, GC_PREV_MASK_COLLECTING
    jz .skip
    mov rdx, rcx
    and rdx, ~GC_PREV_MASK    ; extract state bits (low 2)
    shr rcx, GC_PREV_SHIFT    ; gc_refs
    dec rcx                    ; gc_refs--
    jns .store
    xor ecx, ecx              ; clamp to 0
.store:
    shl rcx, GC_PREV_SHIFT
    or rcx, rdx               ; combine with state bits
    mov [rax + PyGC_Head.gc_prev], rcx
.skip:
    ret
END_FUNC gc_visit_decref

;; ============================================================================
;; gc_visit_reachable(rdi=obj)
;; Visit callback for Phase 4: if obj is in unreachable set, move to reachable
;; ============================================================================
DEF_FUNC gc_visit_reachable
    push rbx

    ; Check if this object's type has HAVE_GC flag (non-GC objects have no GC head)
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .done
    ; gc = obj - GC_HEAD_SIZE
    lea rbx, [rdi - GC_HEAD_SIZE]
    ; Check if tracked
    cmp qword [rbx + PyGC_Head.gc_next], 0
    je .done

    ; Check if in unreachable set (collecting bit set)
    mov rax, [rbx + PyGC_Head.gc_prev]
    test rax, GC_PREV_MASK_COLLECTING
    jz .done                   ; not in unreachable — nothing to do

    ; Remove from unreachable list
    mov rdi, rbx
    call gc_list_remove

    ; Clear collecting bit, set gc_refs to 1 (so phase 3 won't re-add)
    mov qword [rbx + PyGC_Head.gc_prev], (1 << GC_PREV_SHIFT)

    ; Append to young/reachable list (r12 = young sentinel, set in gc_collect_gen)
    ; We use a global to pass the reachable list sentinel
    mov rdi, rbx
    lea rsi, [rel gc_reachable_sentinel]
    mov rsi, [rsi]
    call gc_list_append

.done:
    pop rbx
    leave
    ret
END_FUNC gc_visit_reachable

section .data
gc_reachable_sentinel: dq 0    ; set by gc_collect_gen before phase 4

section .text



































