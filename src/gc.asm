; gc.asm - Cycle-collecting garbage collector for apython
; CPython 3.12 compatible generational GC
;
; Three generations: gen0 (young), gen1, gen2 (old)
; Each generation is a doubly-linked list of PyGC_Head nodes.
; gc_refs stored in gc_prev high bits during collection.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "gc.inc"
%include "errcodes.inc"
%include "frame.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern obj_incref
extern frame_free

; ============================================================================
; GC data (BSS + DATA)
; ============================================================================
section .data

; Generation 0 sentinel (young objects)
align 8
gc_gen0:
    dq gc_gen0          ; gc_next -> self (empty list)
    dq gc_gen0          ; gc_prev -> self
global gc_gen0_count
gc_gen0_count:  dq 0
gc_gen0_threshold: dq 700

; Generation 1 sentinel
align 8
gc_gen1:
    dq gc_gen1
    dq gc_gen1
gc_gen1_count:  dq 0
gc_gen1_threshold: dq 10
gc_gen1_collections: dq 0

; Generation 2 sentinel
align 8
gc_gen2:
    dq gc_gen2
    dq gc_gen2
gc_gen2_count:  dq 0
gc_gen2_threshold: dq 10
gc_gen2_collections: dq 0

; GC state
global gc_enabled
gc_enabled:     dq 1
gc_collecting:  dq 0    ; reentrancy guard

; Generation table (for indexed access)
align 8
gc_generations:
    dq gc_gen0, gc_gen0_count, gc_gen0_threshold, 0                ; gen 0
    dq gc_gen1, gc_gen1_count, gc_gen1_threshold, gc_gen1_collections ; gen 1
    dq gc_gen2, gc_gen2_count, gc_gen2_threshold, gc_gen2_collections ; gen 2

GEN_ENTRY_SIZE equ 32   ; 4 qwords per generation entry

section .text

; ============================================================================
; gc_list_init(rdi=sentinel)
; Initialize a doubly-linked list sentinel to point to itself
; ============================================================================
DEF_FUNC_BARE gc_list_init
    mov [rdi + PyGC_Head.gc_next], rdi
    mov [rdi + PyGC_Head.gc_prev], rdi
    ret
END_FUNC gc_list_init

; ============================================================================
; gc_list_append(rdi=node, rsi=list_sentinel)
; Insert node at the tail of list (before sentinel)
; ============================================================================
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

; ============================================================================
; gc_list_remove(rdi=node)
; Remove node from its doubly-linked list
; ============================================================================
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

; ============================================================================
; gc_list_is_empty(rdi=sentinel) -> eax (1=empty, 0=not)
; ============================================================================
DEF_FUNC_BARE gc_list_is_empty
    mov rax, [rdi + PyGC_Head.gc_next]
    cmp rax, rdi
    sete al
    movzx eax, al
    ret
END_FUNC gc_list_is_empty

; ============================================================================
; gc_list_merge(rdi=from_sentinel, rsi=to_sentinel)
; Move all nodes from 'from' list to end of 'to' list. Empties 'from'.
; ============================================================================
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

; ============================================================================
; gc_alloc(rdi=size, rsi=type) -> PyObject*
; Allocate a GC-tracked object. Prepends PyGC_Head, returns obj pointer.
; ============================================================================
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

; ============================================================================
; gc_track(rdi=obj)
; Add object to gen0 tracking list. May trigger collection.
; ============================================================================
DEF_FUNC gc_track
    push rbx
    mov rbx, rdi               ; save obj

    ; gc = obj - GC_HEAD_SIZE
    lea rdi, [rbx - GC_HEAD_SIZE]
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
END_FUNC gc_track

; ============================================================================
; gc_untrack(rdi=obj)
; Remove object from GC tracking list.
; ============================================================================
DEF_FUNC gc_untrack
    ; gc = obj - GC_HEAD_SIZE
    lea rdi, [rdi - GC_HEAD_SIZE]

    ; Check if actually tracked (gc_next != 0)
    cmp qword [rdi + PyGC_Head.gc_next], 0
    je .not_tracked

    call gc_list_remove

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

; ============================================================================
; gc_dealloc(rdi=obj)
; Untrack and free a potentially GC-tracked object.
; If TYPE_FLAG_HAVE_GC is set: untrack + free at obj - GC_HEAD_SIZE
; Otherwise: plain ap_free(obj) (for objects allocated without GC head)
; ============================================================================
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

; ============================================================================
; gc_collect_gen(edi=generation)
; Core cycle collection algorithm.
; ============================================================================
; Local frame layout
GCG_GEN     equ 8
GCG_YOUNG   equ 24    ; 16-byte PyGC_Head sentinel on stack (next+prev)
GCG_UNREACH equ 40    ; 16-byte PyGC_Head sentinel on stack
GCG_FRAME   equ 48

DEF_FUNC gc_collect_gen, GCG_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - GCG_GEN], edi    ; save generation

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

    ; Store gc_refs in gc_prev: (gc_refs << GC_PREV_SHIFT) | 0 (tracked state)
    shl rcx, GC_PREV_SHIFT
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
    ; Use r14 for visit callback in VISIT_FAT macro
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

    ; ---- Phase 5: Clear unreachable objects, then let DECREF handle dealloc ----
    ; First pass: call tp_clear on all unreachable objects
    mov rbx, [r15 + PyGC_Head.gc_next]

.phase5_clear_loop:
    cmp rbx, r15
    je .phase5_clear_done

    mov r13, [rbx + PyGC_Head.gc_next]  ; save next

    lea rdi, [rbx + GC_HEAD_SIZE]  ; obj
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_clear]
    test rax, rax
    jz .phase5_clear_next
    ; rdi already = obj
    call rax                   ; tp_clear(obj)

.phase5_clear_next:
    mov rbx, r13
    jmp .phase5_clear_loop
.phase5_clear_done:

    ; Second pass: unlink from unreachable list and DECREF each object.
    ; tp_clear broke the cycles, so DECREF should reach 0 and trigger dealloc.
    mov rbx, [r15 + PyGC_Head.gc_next]

.phase5_dealloc_loop:
    cmp rbx, r15
    je .phase5_dealloc_done

    mov r13, [rbx + PyGC_Head.gc_next]  ; save next

    ; Unlink from unreachable (so gc_dealloc doesn't double-remove)
    mov rdi, rbx
    call gc_list_remove
    ; Mark as untracked
    mov qword [rbx + PyGC_Head.gc_next], 0
    mov qword [rbx + PyGC_Head.gc_prev], 0

    ; DECREF the object
    lea rdi, [rbx + GC_HEAD_SIZE]
    dec qword [rdi + PyObject.ob_refcnt]
    jnz .phase5_dealloc_next
    call obj_dealloc

.phase5_dealloc_next:
    mov rbx, r13
    jmp .phase5_dealloc_loop
.phase5_dealloc_done:

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

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC gc_collect_gen

; ============================================================================
; gc_collect()
; Collect all generations. Called from Python gc.collect() and exit cleanup.
; ============================================================================
DEF_FUNC gc_collect
    mov edi, 2                 ; collect all 3 generations
    call gc_collect_gen
    leave
    ret
END_FUNC gc_collect

; ============================================================================
; gc_visit_decref(rdi=obj)
; Visit callback for Phase 2: decrement gc_refs of visited object
; ============================================================================
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
    ; Decrement gc_refs (stored in gc_prev bits 2+)
    mov rcx, [rax + PyGC_Head.gc_prev]
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

; ============================================================================
; gc_visit_reachable(rdi=obj)
; Visit callback for Phase 4: if obj is in unreachable set, move to reachable
; ============================================================================
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

; ============================================================================
; TRAVERSE AND CLEAR FUNCTIONS
; ============================================================================
; Convention: tp_traverse(rdi=obj, r14=visit_callback)
;             tp_clear(rdi=obj)
; VISIT_FAT and VISIT_PTR macros use r14 as the callback.

; ---- list_traverse / list_clear ----
DEF_FUNC list_traverse
    push rbx
    push r12
    push r13
    push r15

    mov rbx, rdi                       ; obj
    mov r12, [rbx + PyListObject.ob_item]       ; payloads
    mov r15, [rbx + PyListObject.ob_item_tags]  ; tags
    mov r13, [rbx + PyListObject.ob_size]
    test r13, r13
    jz .done

.loop:
    dec r13
    mov rdi, [r12]                     ; payload
    movzx esi, byte [r15]             ; tag
    VISIT_FAT rdi, rsi
    add r12, 8
    inc r15
    test r13, r13
    jnz .loop
.done:
    pop r15
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_traverse

DEF_FUNC list_clear
    push rbx
    push r12
    push r13
    push r15

    mov rbx, rdi
    mov r12, [rbx + PyListObject.ob_item]       ; payloads
    mov r15, [rbx + PyListObject.ob_item_tags]  ; tags
    mov r13, [rbx + PyListObject.ob_size]
    mov qword [rbx + PyListObject.ob_size], 0

    test r13, r13
    jz .done
.loop:
    dec r13
    mov rdi, [r12]
    movzx esi, byte [r15]
    push r12
    push r15
    DECREF_VAL rdi, rsi
    pop r15
    pop r12
    add r12, 8
    inc r15
    test r13, r13
    jnz .loop
.done:
    pop r15
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_clear

; ---- tuple_traverse / tuple_clear ----
DEF_FUNC tuple_traverse
    push rbx
    push r12
    push r13
    push r15

    mov rbx, rdi
    mov r13, [rbx + PyTupleObject.ob_size]
    mov r12, [rbx + PyTupleObject.ob_item]       ; payloads
    mov r15, [rbx + PyTupleObject.ob_item_tags]  ; tags (callee-saved, survives VISIT_FAT)
    test r13, r13
    jz .done
.loop:
    dec r13
    mov rdi, [r12]
    movzx esi, byte [r15]
    VISIT_FAT rdi, rsi
    add r12, 8
    inc r15
    test r13, r13
    jnz .loop
.done:
    pop r15
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_traverse

DEF_FUNC tuple_clear
    push rbx
    push r12
    push r13
    push r15

    mov rbx, rdi
    mov r13, [rbx + PyTupleObject.ob_size]
    mov r12, [rbx + PyTupleObject.ob_item]       ; payloads
    mov r15, [rbx + PyTupleObject.ob_item_tags]  ; tags
    mov qword [rbx + PyTupleObject.ob_size], 0

    test r13, r13
    jz .done
.loop:
    dec r13
    mov rdi, [r12]
    movzx esi, byte [r15]
    push r12
    push r15
    DECREF_VAL rdi, rsi
    pop r15
    pop r12
    add r12, 8
    inc r15
    test r13, r13
    jnz .loop
.done:
    pop r15
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_clear

; ---- dict_traverse / dict_clear ----
extern ap_memset

DICT_TOMBSTONE_GC equ 0xFF

DEF_FUNC dict_traverse
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    test r13, r13
    jz .done
.loop:
    dec r13
    ; Check for empty/tombstone
    cmp byte [r12 + DictEntry.key_tag], 0
    je .next
    cmp byte [r12 + DictEntry.key_tag], DICT_TOMBSTONE_GC
    je .next

    ; Visit key
    mov rdi, [r12 + DictEntry.key]
    movzx esi, byte [r12 + DictEntry.key_tag]
    VISIT_FAT rdi, rsi
    ; Visit value
    mov rdi, [r12 + DictEntry.value]
    movzx esi, byte [r12 + DictEntry.value_tag]
    VISIT_FAT rdi, rsi

.next:
    add r12, DICT_ENTRY_SIZE
    test r13, r13
    jnz .loop
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_traverse

DEF_FUNC dict_clear_gc
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]

    test r13, r13
    jz .done
.loop:
    dec r13
    cmp byte [r12 + DictEntry.key_tag], 0
    je .next
    cmp byte [r12 + DictEntry.key_tag], DICT_TOMBSTONE_GC
    je .next

    ; DECREF key
    push r12
    push r13
    mov rdi, [r12 + DictEntry.key]
    movzx esi, byte [r12 + DictEntry.key_tag]
    DECREF_VAL rdi, rsi
    pop r13
    pop r12

    ; DECREF value
    push r12
    push r13
    mov rdi, [r12 + DictEntry.value]
    movzx esi, byte [r12 + DictEntry.value_tag]
    DECREF_VAL rdi, rsi
    pop r13
    pop r12

    ; Clear entry
    mov byte [r12 + DictEntry.key_tag], 0
    mov qword [r12 + DictEntry.key], 0
    mov byte [r12 + DictEntry.value_tag], 0
    mov qword [r12 + DictEntry.value], 0

.next:
    add r12, DICT_ENTRY_SIZE
    test r13, r13
    jnz .loop
.done:
    mov qword [rbx + PyDictObject.ob_size], 0

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_clear_gc

; ---- set_traverse / set_clear ----
; Set entries are 24 bytes (hash+key+key_tag_qword), distinct from DictEntry (32 bytes).
SET_ENTRY_SIZE_GC    equ 24
SET_ENTRY_KEY_GC     equ 8
SET_ENTRY_KEY_TAG_GC equ 16
SET_TOMBSTONE_GC     equ 0xDEAD

DEF_FUNC set_traverse
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]   ; set reuses PyDictObject layout for header
    mov r13, [rbx + PyDictObject.capacity]
    test r13, r13
    jz .st_done
.st_loop:
    dec r13
    ; Check for empty (key_tag == 0) or tombstone (key_tag == 0xDEAD)
    cmp qword [r12 + SET_ENTRY_KEY_TAG_GC], 0
    je .st_next
    cmp qword [r12 + SET_ENTRY_KEY_TAG_GC], SET_TOMBSTONE_GC
    je .st_next

    ; Visit key
    mov rdi, [r12 + SET_ENTRY_KEY_GC]
    movzx esi, byte [r12 + SET_ENTRY_KEY_TAG_GC]
    VISIT_FAT rdi, rsi

.st_next:
    add r12, SET_ENTRY_SIZE_GC
    test r13, r13
    jnz .st_loop
.st_done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_traverse

DEF_FUNC set_clear_gc
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]

    test r13, r13
    jz .sc_done
.sc_loop:
    dec r13
    cmp qword [r12 + SET_ENTRY_KEY_TAG_GC], 0
    je .sc_next
    cmp qword [r12 + SET_ENTRY_KEY_TAG_GC], SET_TOMBSTONE_GC
    je .sc_next

    ; DECREF key
    push r12
    push r13
    mov rdi, [r12 + SET_ENTRY_KEY_GC]
    movzx esi, byte [r12 + SET_ENTRY_KEY_TAG_GC]
    DECREF_VAL rdi, rsi
    pop r13
    pop r12

    ; Clear entry
    mov qword [r12 + SET_ENTRY_KEY_TAG_GC], 0
    mov qword [r12 + SET_ENTRY_KEY_GC], 0

.sc_next:
    add r12, SET_ENTRY_SIZE_GC
    test r13, r13
    jnz .sc_loop
.sc_done:
    mov qword [rbx + PyDictObject.ob_size], 0

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_clear_gc

; ---- func_traverse / func_clear ----
DEF_FUNC func_traverse
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyFuncObject.func_code]
    VISIT_PTR rdi
    mov rdi, [rbx + PyFuncObject.func_globals]
    VISIT_PTR rdi
    mov rdi, [rbx + PyFuncObject.func_name]
    VISIT_PTR rdi
    mov rdi, [rbx + PyFuncObject.func_defaults]
    VISIT_PTR rdi
    mov rdi, [rbx + PyFuncObject.func_closure]
    VISIT_PTR rdi
    mov rdi, [rbx + PyFuncObject.func_kwdefaults]
    VISIT_PTR rdi
    mov rdi, [rbx + PyFuncObject.func_dict]
    VISIT_PTR rdi

    pop rbx
    leave
    ret
END_FUNC func_traverse

DEF_FUNC func_clear
    push rbx
    mov rbx, rdi

    ; NULL out closure, defaults, kwdefaults, func_dict — XDECREF each
    mov rdi, [rbx + PyFuncObject.func_closure]
    mov qword [rbx + PyFuncObject.func_closure], 0
    test rdi, rdi
    jz .no_clos
    call obj_decref
.no_clos:
    mov rdi, [rbx + PyFuncObject.func_defaults]
    mov qword [rbx + PyFuncObject.func_defaults], 0
    test rdi, rdi
    jz .no_defs
    call obj_decref
.no_defs:
    mov rdi, [rbx + PyFuncObject.func_kwdefaults]
    mov qword [rbx + PyFuncObject.func_kwdefaults], 0
    test rdi, rdi
    jz .no_kwd
    call obj_decref
.no_kwd:
    mov rdi, [rbx + PyFuncObject.func_dict]
    mov qword [rbx + PyFuncObject.func_dict], 0
    test rdi, rdi
    jz .no_fdict
    call obj_decref
.no_fdict:

    pop rbx
    leave
    ret
END_FUNC func_clear

; ---- cell_traverse / cell_clear ----
DEF_FUNC cell_traverse
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyCellObject.ob_ref]
    mov rsi, [rbx + PyCellObject.ob_ref_tag]
    VISIT_FAT rdi, rsi

    pop rbx
    leave
    ret
END_FUNC cell_traverse

DEF_FUNC cell_clear
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyCellObject.ob_ref]
    mov rsi, [rbx + PyCellObject.ob_ref_tag]
    mov qword [rbx + PyCellObject.ob_ref], 0
    mov qword [rbx + PyCellObject.ob_ref_tag], TAG_NULL
    DECREF_VAL rdi, rsi

    pop rbx
    leave
    ret
END_FUNC cell_clear

; ---- method_traverse / method_clear ----
DEF_FUNC method_traverse
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_func]
    VISIT_PTR rdi
    mov rdi, [rbx + PyMethodObject.im_self]
    VISIT_PTR rdi

    pop rbx
    leave
    ret
END_FUNC method_traverse

DEF_FUNC method_clear
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_func]
    mov qword [rbx + PyMethodObject.im_func], 0
    test rdi, rdi
    jz .no_func
    call obj_decref
.no_func:
    mov rdi, [rbx + PyMethodObject.im_self]
    mov qword [rbx + PyMethodObject.im_self], 0
    test rdi, rdi
    jz .no_self
    call obj_decref
.no_self:

    pop rbx
    leave
    ret
END_FUNC method_clear

; ---- gen_traverse / gen_clear ----
DEF_FUNC gen_traverse
    push rbx
    push r12
    push r13

    mov rbx, rdi

    ; Visit code
    mov rdi, [rbx + PyGenObject.gi_code]
    VISIT_PTR rdi

    ; Visit name
    mov rdi, [rbx + PyGenObject.gi_name]
    VISIT_PTR rdi

    ; Visit return value (fat)
    mov rdi, [rbx + PyGenObject.gi_return_value]
    mov rsi, [rbx + PyGenObject.gi_return_tag]
    VISIT_FAT rdi, rsi

    ; Traverse frame localsplus if frame exists
    mov r12, [rbx + PyGenObject.gi_frame]
    test r12, r12
    jz .done

    ; Get nlocalsplus from code object
    mov rax, [rbx + PyGenObject.gi_code]
    mov r13d, [rax + PyCodeObject.co_nlocalsplus]
    test r13d, r13d
    jz .done

    mov r11, [r12 + PyFrame.locals_tag_base]
    lea r12, [r12 + PyFrame.localsplus]  ; start of payload array
.frame_loop:
    dec r13d
    mov rdi, [r12 + r13*8]
    movzx rsi, byte [r11 + r13]
    VISIT_FAT rdi, rsi
    test r13d, r13d
    jnz .frame_loop

.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_traverse

DEF_FUNC gen_clear
    push rbx
    mov rbx, rdi

    ; Clear return value
    mov rdi, [rbx + PyGenObject.gi_return_value]
    mov rsi, [rbx + PyGenObject.gi_return_tag]
    mov qword [rbx + PyGenObject.gi_return_value], 0
    mov qword [rbx + PyGenObject.gi_return_tag], TAG_NULL
    XDECREF_VAL rdi, rsi

    ; Free frame if held (frame_free DECREFs localsplus)
    mov rdi, [rbx + PyGenObject.gi_frame]
    mov qword [rbx + PyGenObject.gi_frame], 0
    test rdi, rdi
    jz .done
    call frame_free

.done:
    pop rbx
    leave
    ret
END_FUNC gen_clear

; ---- instance_traverse / instance_clear ----
DEF_FUNC instance_traverse
    push rbx
    push r12
    push r13

    mov rbx, rdi

    ; Visit inst_dict
    mov rdi, [rbx + PyInstanceObject.inst_dict]
    VISIT_PTR rdi

    ; Visit __slots__ values (fat value slots after PyInstanceObject header)
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_basicsize]
    sub rax, PyInstanceObject_size
    jle .done
    shr rax, 4                  ; nslots
    mov r13, rax
    lea r12, [rbx + PyInstanceObject_size]

.slot_loop:
    mov rdi, [r12]
    mov rsi, [r12 + 8]
    VISIT_FAT rdi, rsi
    add r12, 16
    dec r13
    jnz .slot_loop

.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC instance_traverse

DEF_FUNC instance_clear
    push rbx
    mov rbx, rdi

    ; XDECREF + NULL inst_dict
    mov rdi, [rbx + PyInstanceObject.inst_dict]
    mov qword [rbx + PyInstanceObject.inst_dict], 0
    test rdi, rdi
    jz .done
    call obj_decref

.done:
    pop rbx
    leave
    ret
END_FUNC instance_clear

; ---- exc_traverse / exc_clear ----
DEF_FUNC exc_traverse
    push rbx
    mov rbx, rdi

    ; Visit exc_value (fat)
    mov rdi, [rbx + PyExceptionObject.exc_value]
    mov rsi, [rbx + PyExceptionObject.exc_value_tag]
    VISIT_FAT rdi, rsi

    ; Visit heap ptrs
    mov rdi, [rbx + PyExceptionObject.exc_tb]
    VISIT_PTR rdi
    mov rdi, [rbx + PyExceptionObject.exc_context]
    VISIT_PTR rdi
    mov rdi, [rbx + PyExceptionObject.exc_cause]
    VISIT_PTR rdi
    mov rdi, [rbx + PyExceptionObject.exc_args]
    VISIT_PTR rdi

    pop rbx
    leave
    ret
END_FUNC exc_traverse

DEF_FUNC exc_clear_gc
    push rbx
    mov rbx, rdi

    ; DECREF_VAL exc_value
    mov rdi, [rbx + PyExceptionObject.exc_value]
    mov rsi, [rbx + PyExceptionObject.exc_value_tag]
    mov qword [rbx + PyExceptionObject.exc_value], 0
    mov qword [rbx + PyExceptionObject.exc_value_tag], TAG_NULL
    DECREF_VAL rdi, rsi

    ; XDECREF + NULL heap ptrs
    mov rdi, [rbx + PyExceptionObject.exc_tb]
    mov qword [rbx + PyExceptionObject.exc_tb], 0
    test rdi, rdi
    jz .no_tb
    call obj_decref
.no_tb:
    mov rdi, [rbx + PyExceptionObject.exc_context]
    mov qword [rbx + PyExceptionObject.exc_context], 0
    test rdi, rdi
    jz .no_ctx
    call obj_decref
.no_ctx:
    mov rdi, [rbx + PyExceptionObject.exc_cause]
    mov qword [rbx + PyExceptionObject.exc_cause], 0
    test rdi, rdi
    jz .no_cause
    call obj_decref
.no_cause:
    mov rdi, [rbx + PyExceptionObject.exc_args]
    mov qword [rbx + PyExceptionObject.exc_args], 0
    test rdi, rdi
    jz .no_args
    call obj_decref
.no_args:

    pop rbx
    leave
    ret
END_FUNC exc_clear_gc

; ---- module_traverse / module_clear ----
DEF_FUNC module_traverse
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyModuleObject.mod_dict]
    VISIT_PTR rdi
    mov rdi, [rbx + PyModuleObject.mod_name]
    VISIT_PTR rdi

    pop rbx
    leave
    ret
END_FUNC module_traverse

DEF_FUNC module_clear_gc
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyModuleObject.mod_dict]
    mov qword [rbx + PyModuleObject.mod_dict], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC module_clear_gc

; ---- code_traverse (no clear — code objects don't form cycles) ----
DEF_FUNC code_traverse
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyCodeObject.co_consts]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_names]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_localsplusnames]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_localspluskinds]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_filename]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_name]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_qualname]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_exceptiontable]
    VISIT_PTR rdi

    pop rbx
    leave
    ret
END_FUNC code_traverse

; ---- staticmethod_traverse / classmethod_traverse / property_traverse ----
DEF_FUNC staticmethod_traverse
    mov rdi, [rdi + PyStaticMethodObject.sm_callable]
    VISIT_PTR rdi
    leave
    ret
END_FUNC staticmethod_traverse

DEF_FUNC staticmethod_clear
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyStaticMethodObject.sm_callable]
    mov qword [rbx + PyStaticMethodObject.sm_callable], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC staticmethod_clear

DEF_FUNC classmethod_traverse
    mov rdi, [rdi + PyClassMethodObject.cm_callable]
    VISIT_PTR rdi
    leave
    ret
END_FUNC classmethod_traverse

DEF_FUNC classmethod_clear
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyClassMethodObject.cm_callable]
    mov qword [rbx + PyClassMethodObject.cm_callable], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC classmethod_clear

DEF_FUNC property_traverse
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyPropertyObject.prop_get]
    VISIT_PTR rdi
    mov rdi, [rbx + PyPropertyObject.prop_set]
    VISIT_PTR rdi
    mov rdi, [rbx + PyPropertyObject.prop_del]
    VISIT_PTR rdi
    pop rbx
    leave
    ret
END_FUNC property_traverse

DEF_FUNC property_clear
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyPropertyObject.prop_get]
    mov qword [rbx + PyPropertyObject.prop_get], 0
    test rdi, rdi
    jz .no_get
    call obj_decref
.no_get:
    mov rdi, [rbx + PyPropertyObject.prop_set]
    mov qword [rbx + PyPropertyObject.prop_set], 0
    test rdi, rdi
    jz .no_set
    call obj_decref
.no_set:
    mov rdi, [rbx + PyPropertyObject.prop_del]
    mov qword [rbx + PyPropertyObject.prop_del], 0
    test rdi, rdi
    jz .no_del
    call obj_decref
.no_del:
    pop rbx
    leave
    ret
END_FUNC property_clear

; ---- slice_traverse / slice_clear ----
DEF_FUNC slice_traverse
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PySliceObject.start]
    mov rsi, [rbx + PySliceObject.start_tag]
    VISIT_FAT rdi, rsi
    mov rdi, [rbx + PySliceObject.stop]
    mov rsi, [rbx + PySliceObject.stop_tag]
    VISIT_FAT rdi, rsi
    mov rdi, [rbx + PySliceObject.step]
    mov rsi, [rbx + PySliceObject.step_tag]
    VISIT_FAT rdi, rsi

    pop rbx
    leave
    ret
END_FUNC slice_traverse

DEF_FUNC slice_clear_gc
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PySliceObject.start]
    mov rsi, [rbx + PySliceObject.start_tag]
    mov qword [rbx + PySliceObject.start], 0
    mov qword [rbx + PySliceObject.start_tag], TAG_NULL
    DECREF_VAL rdi, rsi

    mov rdi, [rbx + PySliceObject.stop]
    mov rsi, [rbx + PySliceObject.stop_tag]
    mov qword [rbx + PySliceObject.stop], 0
    mov qword [rbx + PySliceObject.stop_tag], TAG_NULL
    DECREF_VAL rdi, rsi

    mov rdi, [rbx + PySliceObject.step]
    mov rsi, [rbx + PySliceObject.step_tag]
    mov qword [rbx + PySliceObject.step], 0
    mov qword [rbx + PySliceObject.step_tag], TAG_NULL
    DECREF_VAL rdi, rsi

    pop rbx
    leave
    ret
END_FUNC slice_clear_gc

; ---- Iterator traverse functions (visit the underlying container ref) ----
DEF_FUNC list_iter_traverse
    mov rdi, [rdi + PyListIterObject.it_seq]
    VISIT_PTR rdi
    leave
    ret
END_FUNC list_iter_traverse

DEF_FUNC list_iter_clear
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyListIterObject.it_seq]
    mov qword [rbx + PyListIterObject.it_seq], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC list_iter_clear

DEF_FUNC tuple_iter_traverse
    mov rdi, [rdi + PyTupleIterObject.it_seq]
    VISIT_PTR rdi
    leave
    ret
END_FUNC tuple_iter_traverse

DEF_FUNC tuple_iter_clear
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyTupleIterObject.it_seq]
    mov qword [rbx + PyTupleIterObject.it_seq], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC tuple_iter_clear

DEF_FUNC dict_iter_traverse
    mov rdi, [rdi + PyDictIterObject.it_dict]
    VISIT_PTR rdi
    leave
    ret
END_FUNC dict_iter_traverse

DEF_FUNC dict_iter_clear
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyDictIterObject.it_dict]
    mov qword [rbx + PyDictIterObject.it_dict], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC dict_iter_clear

DEF_FUNC dict_view_traverse
    mov rdi, [rdi + PyDictViewObject.dv_dict]
    VISIT_PTR rdi
    leave
    ret
END_FUNC dict_view_traverse

DEF_FUNC dict_view_clear
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyDictViewObject.dv_dict]
    mov qword [rbx + PyDictViewObject.dv_dict], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC dict_view_clear

; ---- task_traverse / task_clear ----
DEF_FUNC task_traverse
    push rbx
    push r12
    push r13

    mov rbx, rdi

    ; Visit coro
    mov rdi, [rbx + AsyncTask.coro]
    VISIT_PTR rdi

    ; Visit result (fat)
    mov rdi, [rbx + AsyncTask.result]
    mov rsi, [rbx + AsyncTask.result_tag]
    VISIT_FAT rdi, rsi

    ; Visit send_value (fat)
    mov rdi, [rbx + AsyncTask.send_value]
    mov rsi, [rbx + AsyncTask.send_tag]
    VISIT_FAT rdi, rsi

    ; Visit exception
    mov rdi, [rbx + AsyncTask.exception]
    VISIT_PTR rdi

    ; Visit waiters array
    mov r12, [rbx + AsyncTask.waiters]
    test r12, r12
    jz .done
    mov r13d, [rbx + AsyncTask.n_waiters]
    test r13d, r13d
    jz .done
.waiter_loop:
    dec r13d
    mov rdi, [r12]
    VISIT_PTR rdi
    add r12, 8
    test r13d, r13d
    jnz .waiter_loop

.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC task_traverse

DEF_FUNC task_clear
    push rbx
    mov rbx, rdi

    ; XDECREF coro
    mov rdi, [rbx + AsyncTask.coro]
    mov qword [rbx + AsyncTask.coro], 0
    test rdi, rdi
    jz .no_coro
    call obj_decref
.no_coro:

    ; DECREF_VAL result
    mov rdi, [rbx + AsyncTask.result]
    mov rsi, [rbx + AsyncTask.result_tag]
    mov qword [rbx + AsyncTask.result], 0
    mov qword [rbx + AsyncTask.result_tag], TAG_NULL
    DECREF_VAL rdi, rsi

    ; DECREF_VAL send_value
    mov rdi, [rbx + AsyncTask.send_value]
    mov rsi, [rbx + AsyncTask.send_tag]
    mov qword [rbx + AsyncTask.send_value], 0
    mov qword [rbx + AsyncTask.send_tag], TAG_NULL
    DECREF_VAL rdi, rsi

    pop rbx
    leave
    ret
END_FUNC task_clear

; ---- wait_for_traverse / wait_for_clear ----
DEF_FUNC wait_for_traverse
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + WaitForAwaitable.inner_task]
    VISIT_PTR rdi
    mov rdi, [rbx + WaitForAwaitable.outer_task]
    VISIT_PTR rdi
    mov rdi, [rbx + WaitForAwaitable.gi_return_value]
    mov rsi, [rbx + WaitForAwaitable.gi_return_tag]
    VISIT_FAT rdi, rsi

    pop rbx
    leave
    ret
END_FUNC wait_for_traverse

DEF_FUNC wait_for_clear
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + WaitForAwaitable.inner_task]
    mov qword [rbx + WaitForAwaitable.inner_task], 0
    test rdi, rdi
    jz .no_inner
    call obj_decref
.no_inner:
    mov rdi, [rbx + WaitForAwaitable.outer_task]
    mov qword [rbx + WaitForAwaitable.outer_task], 0
    test rdi, rdi
    jz .no_outer
    call obj_decref
.no_outer:

    mov rdi, [rbx + WaitForAwaitable.gi_return_value]
    mov rsi, [rbx + WaitForAwaitable.gi_return_tag]
    mov qword [rbx + WaitForAwaitable.gi_return_value], 0
    mov qword [rbx + WaitForAwaitable.gi_return_tag], TAG_NULL
    DECREF_VAL rdi, rsi

    pop rbx
    leave
    ret
END_FUNC wait_for_clear
