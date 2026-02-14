; opcodes_build.asm - Opcode handlers for subscript, build, and iteration opcodes
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

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern eval_dispatch
extern obj_dealloc
extern obj_decref
extern obj_is_true
extern fatal_error
extern raise_exception
extern exc_TypeError_type
extern int_to_i64
extern tuple_new
extern list_new
extern list_append
extern dict_new
extern dict_set
extern slice_new
extern slice_type
extern slice_indices
extern none_singleton
extern obj_incref

;; ============================================================================
;; op_binary_subscr - obj[key]
;;
;; Pop key, pop obj, call mp_subscript or sq_item, push result.
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
global op_binary_subscr
op_binary_subscr:
    VPOP rsi                   ; rsi = key
    VPOP rdi                   ; rdi = obj

    push rdi                   ; save obj
    push rsi                   ; save key

    ; Try mp_subscript first (handles int keys for all types)
    test rdi, rdi
    js .no_subscript           ; SmallInt can't be subscripted
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .try_sequence
    mov rax, [rax + PyMappingMethods.mp_subscript]
    test rax, rax
    jz .try_sequence

    ; Call mp_subscript(obj, key)
    ; rdi = obj, rsi = key (already set)
    call rax
    jmp .subscr_done

.try_sequence:
    ; Try sq_item (need to convert key to int64)
    mov rdi, [rsp + 8]        ; reload obj
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .no_subscript
    mov rcx, [rax + PySequenceMethods.sq_item]
    test rcx, rcx
    jz .no_subscript

    ; Convert key to int64
    mov rdi, [rsp]             ; key
    call int_to_i64
    mov rsi, rax               ; rsi = int64 index

    mov rdi, [rsp + 8]        ; reload obj
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    mov rax, [rax + PySequenceMethods.sq_item]
    call rax
    jmp .subscr_done

.no_subscript:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not subscriptable"
    call raise_exception

.subscr_done:
    ; rax = result
    push rax                   ; save result on machine stack
    mov rdi, [rsp + 8]        ; key
    DECREF_REG rdi
    mov rdi, [rsp + 16]       ; obj
    DECREF_REG rdi
    pop rax                    ; restore result
    add rsp, 16                ; discard saved operands

    VPUSH rax

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

;; ============================================================================
;; op_store_subscr - obj[key] = value
;;
;; Stack: value, obj, key (TOS)
;; Pop key, pop obj, pop value.
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
global op_store_subscr
op_store_subscr:
    VPOP rsi                   ; rsi = key (TOS)
    VPOP rdi                   ; rdi = obj
    VPOP rdx                   ; rdx = value

    push rdi                   ; save obj
    push rsi                   ; save key
    push rdx                   ; save value

    ; Try mp_ass_subscript first
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .store_try_seq
    mov rax, [rax + PyMappingMethods.mp_ass_subscript]
    test rax, rax
    jz .store_try_seq

    ; Call mp_ass_subscript(obj, key, value)
    ; rdi = obj, rsi = key, rdx = value (already set)
    call rax
    jmp .store_done

.store_try_seq:
    ; Try sq_ass_item
    mov rdi, [rsp + 16]       ; obj
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .store_error
    mov rcx, [rax + PySequenceMethods.sq_ass_item]
    test rcx, rcx
    jz .store_error

    ; Convert key to int64
    mov rdi, [rsp + 8]        ; key
    call int_to_i64
    mov rsi, rax               ; index

    mov rdi, [rsp + 16]       ; obj
    mov rdx, [rsp]             ; value
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    mov rax, [rax + PySequenceMethods.sq_ass_item]
    call rax
    jmp .store_done

.store_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support item assignment"
    call raise_exception

.store_done:
    pop rdi                    ; value
    DECREF_REG rdi
    pop rdi                    ; key
    DECREF_REG rdi
    pop rdi                    ; obj
    DECREF_REG rdi

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

;; ============================================================================
;; op_build_tuple - Create tuple from TOS items
;;
;; ecx = count (number of items to pop)
;; Items are on stack bottom-to-top: first item deepest.
;; ============================================================================
global op_build_tuple
op_build_tuple:
    push rbp
    mov rbp, rsp
    sub rsp, 16                ; [rbp-8] = count

    mov [rbp-8], rcx           ; save count

    ; Allocate tuple
    mov rdi, rcx
    call tuple_new
    mov [rbp-16], rax          ; save tuple

    ; Fill items: tuple[i] = stack[-(count-i)]
    ; Items on stack: [r13 - count*8] = first, [r13 - 8] = last
    mov rcx, [rbp-8]          ; count
    mov rax, [rbp-16]         ; tuple
    xor edx, edx              ; index
    test rcx, rcx
    jz .build_tuple_done

    ; Calculate base of items on value stack
    mov rdi, rcx
    shl rdi, 3                 ; count * 8
    sub r13, rdi               ; pop all items at once

.build_tuple_fill:
    mov rsi, [r13 + rdx*8]    ; item from stack
    mov rax, [rbp-16]
    mov [rax + PyTupleObject.ob_item + rdx*8], rsi  ; no INCREF needed, ownership transfers
    inc rdx
    cmp rdx, [rbp-8]
    jb .build_tuple_fill

.build_tuple_done:
    mov rax, [rbp-16]
    VPUSH rax
    leave
    DISPATCH

;; ============================================================================
;; op_build_list - Create list from TOS items
;;
;; ecx = count
;; ============================================================================
global op_build_list
op_build_list:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov [rbp-8], rcx           ; save count

    ; Allocate list with capacity
    mov rdi, rcx
    test rdi, rdi
    jnz .bl_has_cap
    mov rdi, 4                 ; minimum capacity
.bl_has_cap:
    call list_new
    mov [rbp-16], rax          ; save list

    ; Pop items and append
    mov rcx, [rbp-8]
    test rcx, rcx
    jz .build_list_done

    ; Calculate base
    mov rdi, rcx
    shl rdi, 3
    sub r13, rdi               ; pop all items

    xor edx, edx
.build_list_fill:
    cmp rdx, [rbp-8]
    jge .build_list_done
    push rdx
    mov rdi, [rbp-16]         ; list
    mov rsi, [r13 + rdx*8]    ; item (ownership transfers, no extra INCREF)
    call list_append
    pop rdx
    inc rdx
    jmp .build_list_fill

.build_list_done:
    ; list_append does INCREF, but we're transferring ownership from stack
    ; so we need to adjust: items already had a ref from the stack, list_append
    ; adds another. We should DECREF each to compensate.
    ; Actually: stack items have a ref, list_append INCREFs, so now refcount is
    ; one too high. We need to DECREF each.
    mov rcx, [rbp-8]
    test rcx, rcx
    jz .build_list_push
    xor edx, edx
.build_list_fixref:
    cmp rdx, [rbp-8]
    jge .build_list_push
    mov rdi, [r13 + rdx*8]
    push rdx
    DECREF_REG rdi
    pop rdx
    inc rdx
    jmp .build_list_fixref

.build_list_push:
    mov rax, [rbp-16]
    VPUSH rax
    leave
    DISPATCH

;; ============================================================================
;; op_build_map - Create dict from TOS key/value pairs
;;
;; ecx = count (number of key/value pairs)
;; Stack (bottom to top): key0, val0, key1, val1, ...
;; ============================================================================
global op_build_map
op_build_map:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov [rbp-8], rcx           ; save count

    call dict_new
    mov [rbp-16], rax          ; save dict

    ; Total items on stack = count * 2
    mov rcx, [rbp-8]
    shl rcx, 1                 ; count * 2
    test rcx, rcx
    jz .build_map_done

    mov rdi, rcx
    shl rdi, 3                 ; total_items * 8
    sub r13, rdi               ; pop all items

    xor edx, edx              ; pair index
.build_map_fill:
    cmp rdx, [rbp-8]
    jge .build_map_done
    push rdx
    mov rdi, [rbp-16]         ; dict
    mov rax, rdx
    shl rax, 1                 ; pair_index * 2
    mov rsi, [r13 + rax*8]    ; key
    lea rcx, [rax + 1]
    mov rdx, [r13 + rcx*8]    ; value
    call dict_set
    pop rdx
    inc rdx
    jmp .build_map_fill

.build_map_done:
    ; dict_set does INCREF on key+value, so DECREF all stack items
    mov rcx, [rbp-8]
    shl rcx, 1
    test rcx, rcx
    jz .build_map_push
    xor edx, edx
.build_map_fixref:
    cmp rdx, rcx
    jge .build_map_push
    mov rdi, [r13 + rdx*8]
    push rdx
    push rcx
    DECREF_REG rdi
    pop rcx
    pop rdx
    inc rdx
    jmp .build_map_fixref

.build_map_push:
    mov rax, [rbp-16]
    VPUSH rax
    leave
    DISPATCH

;; ============================================================================
;; op_build_const_key_map - Build dict from const keys tuple + TOS values
;;
;; ecx = count
;; Stack: val0, val1, ..., valN-1, keys_tuple (TOS)
;; ============================================================================
global op_build_const_key_map
op_build_const_key_map:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov [rbp-8], rcx           ; count

    ; Pop keys tuple from TOS
    VPOP rax
    mov [rbp-16], rax          ; keys tuple

    call dict_new
    mov [rbp-24], rax          ; dict

    ; Pop values
    mov rcx, [rbp-8]
    test rcx, rcx
    jz .bckm_done

    mov rdi, rcx
    shl rdi, 3
    sub r13, rdi               ; pop all values

    xor edx, edx
.bckm_fill:
    cmp rdx, [rbp-8]
    jge .bckm_done
    push rdx
    mov rdi, [rbp-24]         ; dict
    mov rax, [rbp-16]         ; keys tuple
    mov rsi, [rax + PyTupleObject.ob_item + rdx*8]  ; key
    mov rdx, [r13 + rdx*8]    ; value
    call dict_set
    pop rdx
    inc rdx
    jmp .bckm_fill

.bckm_done:
    ; DECREF values from stack
    mov rcx, [rbp-8]
    test rcx, rcx
    jz .bckm_push
    xor edx, edx
.bckm_fixref:
    cmp rdx, rcx
    jge .bckm_decref_keys
    mov rdi, [r13 + rdx*8]
    push rdx
    push rcx
    DECREF_REG rdi
    pop rcx
    pop rdx
    inc rdx
    jmp .bckm_fixref

.bckm_decref_keys:
    ; DECREF keys tuple
    mov rdi, [rbp-16]
    call obj_decref

.bckm_push:
    mov rax, [rbp-24]
    VPUSH rax
    leave
    DISPATCH

;; ============================================================================
;; op_unpack_sequence - Unpack iterable into N items on stack
;;
;; ecx = count
;; Pop TOS (tuple/list), push items[count-1], ..., items[0] (reverse order)
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
global op_unpack_sequence
op_unpack_sequence:
    VPOP rdi                   ; rdi = sequence (tuple or list)

    ; Determine if tuple or list and get item array + size
    push rdi                   ; save for DECREF

    mov rax, [rdi + PyObject.ob_type]

    extern tuple_type
    lea rdx, [rel tuple_type]
    cmp rax, rdx
    je .unpack_tuple

    extern list_type
    lea rdx, [rel list_type]
    cmp rax, rdx
    je .unpack_list

    ; Unknown type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "cannot unpack non-sequence"
    call raise_exception

.unpack_tuple:
    ; Items are inline at ob_item
    lea rsi, [rdi + PyTupleObject.ob_item]  ; rsi = items ptr
    jmp .unpack_push

.unpack_list:
    ; Items are at ob_item pointer
    mov rsi, [rdi + PyListObject.ob_item]   ; rsi = items ptr

.unpack_push:
    ; Push items in reverse order: items[count-1] first (becomes deepest on stack)
    ; Actually Python pushes items[count-1] first, so items[0] is TOS
    mov edx, ecx              ; edx = count
    dec edx                    ; start from last item
.unpack_loop:
    test edx, edx
    js .unpack_done
    mov rax, [rsi + rdx*8]
    INCREF rax
    VPUSH rax
    dec edx
    jmp .unpack_loop

.unpack_done:
    ; DECREF the sequence
    pop rdi
    DECREF_REG rdi

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

;; ============================================================================
;; op_get_iter - Get iterator from TOS
;;
;; Pop obj, call tp_iter, push iterator.
;; ============================================================================
global op_get_iter
op_get_iter:
    VPOP rdi                   ; rdi = iterable obj

    push rdi                   ; save for DECREF

    ; Get tp_iter from type
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .not_iterable

    ; Call tp_iter(obj) -> iterator
    call rax
    push rax                   ; save iterator on machine stack

    ; DECREF the original iterable
    mov rdi, [rsp + 8]        ; reload iterable
    DECREF_REG rdi
    pop rax                    ; restore iterator
    add rsp, 8                 ; discard saved iterable

    VPUSH rax
    DISPATCH

.not_iterable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception

;; ============================================================================
;; op_for_iter - Advance iterator, or jump if exhausted
;;
;; ecx = jump offset (instruction words) if exhausted
;; TOS = iterator
;; If iterator has next: push value (iterator stays on stack)
;; If exhausted: pop iterator, jump forward by arg
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
global op_for_iter
align 16
op_for_iter:
    push rcx                   ; save jump offset on machine stack

    ; Peek at iterator (don't pop yet)
    VPEEK rdi

    ; Call tp_iternext(iterator)
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    ; rax = next value, or NULL if exhausted

    test rax, rax
    jz .exhausted

    ; Got a value - push it (iterator stays on stack)
    add rsp, 8                 ; discard saved jump offset
    VPUSH rax

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH

.exhausted:
    ; CPython 3.12: FOR_ITER exhausted pops the iterator and jumps by (arg + 1)
    ; instruction words past the CACHE. The +1 skips the END_FOR instruction.
    pop rcx                    ; restore jump offset
    lea rcx, [rcx + 1]        ; arg + 1 (skip END_FOR too)
    add rbx, 2                 ; skip cache first
    lea rbx, [rbx + rcx*2]    ; then jump forward

    ; Now pop and DECREF the iterator (safe: rbx/r13 are callee-saved)
    VPOP rdi
    DECREF_REG rdi

    DISPATCH

;; ============================================================================
;; op_end_for - End of for loop cleanup
;;
;; In Python 3.12, END_FOR (opcode 4) pops TOS (the exhausted iterator value).
;; Actually END_FOR pops 2 items in 3.12.
;; ============================================================================
global op_end_for
op_end_for:
    ; Pop TOS (end-of-iteration sentinel / last value)
    VPOP rdi
    DECREF_REG rdi
    ; Pop the iterator
    VPOP rdi
    DECREF_REG rdi
    DISPATCH

;; ============================================================================
;; op_list_append - Append TOS to list at stack position
;;
;; ecx = position (1-based from TOS before the value to append)
;; list is at stack[-(ecx+1)] relative to current TOS
;; Pop TOS (value), append to list.
;; ============================================================================
global op_list_append
op_list_append:
    ; TOS = value to append
    VPOP rsi                   ; rsi = value

    ; list is at stack[-(ecx)] after popping
    ; That is: [r13 - ecx*8]
    neg rcx
    mov rdi, [r13 + rcx*8]    ; rdi = list

    push rsi                   ; save value
    call list_append
    ; list_append does INCREF, so DECREF to compensate
    pop rdi                    ; value
    DECREF_REG rdi

    DISPATCH

;; ============================================================================
;; op_list_extend - Extend list with iterable
;;
;; ecx = position (list at stack[-(ecx)] after pop)
;; Pop TOS (iterable), extend list.
;; ============================================================================
global op_list_extend
op_list_extend:
    push rbp
    mov rbp, rsp
    sub rsp, 32                ; locals: [rbp-8]=list, [rbp-16]=iterable, [rbp-24]=count, [rbp-32]=items

    ; TOS = iterable
    VPOP rsi                   ; rsi = iterable (tuple or list)
    mov [rbp-16], rsi          ; save iterable

    ; list is at stack[-(ecx)] after popping
    neg rcx
    mov rdi, [r13 + rcx*8]    ; rdi = list
    mov [rbp-8], rdi           ; save list

    ; Get iterable type to extract items
    mov rax, [rsi + PyObject.ob_type]

    extern tuple_type
    lea rdx, [rel tuple_type]
    cmp rax, rdx
    je .extend_tuple

    extern list_type
    lea rdx, [rel list_type]
    cmp rax, rdx
    je .extend_list

    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "list.extend() argument must be a list or tuple"
    call raise_exception

.extend_tuple:
    mov rcx, [rsi + PyTupleObject.ob_size]
    lea rdx, [rsi + PyTupleObject.ob_item]
    jmp .extend_items

.extend_list:
    mov rcx, [rsi + PyListObject.ob_size]
    mov rdx, [rsi + PyListObject.ob_item]

.extend_items:
    mov [rbp-24], rcx          ; count
    mov [rbp-32], rdx          ; items ptr

    test rcx, rcx
    jz .extend_done
    xor r8d, r8d               ; index
.extend_loop:
    mov rdi, [rbp-8]          ; list
    mov rdx, [rbp-32]         ; items ptr
    mov rsi, [rdx + r8*8]     ; item
    push r8
    call list_append
    pop r8
    inc r8
    cmp r8, [rbp-24]          ; count
    jb .extend_loop

.extend_done:
    ; DECREF iterable
    mov rdi, [rbp-16]
    call obj_decref

    leave
    DISPATCH

;; ============================================================================
;; op_is_op - Identity comparison (is / is not)
;;
;; ecx = 0 for 'is', 1 for 'is not'
;; Pop right, pop left, push True/False.
;; ============================================================================
global op_is_op
op_is_op:
    mov r8d, ecx               ; save invert flag

    VPOP rsi                   ; right
    VPOP rdi                   ; left

    ; Compare pointers
    cmp rdi, rsi
    sete al
    movzx eax, al              ; eax = 1 if same, 0 if different

    ; DECREF both
    push rax
    push r8
    DECREF_REG rsi
    DECREF_REG rdi
    pop r8
    pop rax

    ; Invert if 'is not'
    xor eax, r8d

    ; Push bool result
    extern bool_true
    extern bool_false
    test eax, eax
    jz .is_false
    lea rax, [rel bool_true]
    jmp .is_push
.is_false:
    lea rax, [rel bool_false]
.is_push:
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_contains_op - 'in' / 'not in' test
;;
;; ecx = 0 for 'in', 1 for 'not in'
;; Stack: right (container), left (value)
;; Pop right (container), pop left (value to find).
;; ============================================================================
global op_contains_op
op_contains_op:
    mov r8d, ecx               ; save invert flag

    VPOP rsi                   ; rsi = right (container)
    VPOP rdi                   ; rdi = left (value to find)

    push r8                    ; save invert
    push rdi                   ; save left
    push rsi                   ; save right

    ; Check sq_contains
    mov rax, [rsi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .contains_error
    mov rax, [rax + PySequenceMethods.sq_contains]
    test rax, rax
    jz .contains_error

    ; Call sq_contains(container, value) -> 0/1
    mov rdi, [rsp]             ; container
    mov rsi, [rsp + 8]        ; value
    call rax
    push rax                   ; save result on machine stack

    ; DECREF both
    ; Stack: [rsp]=result, [rsp+8]=right, [rsp+16]=left, [rsp+24]=invert
    mov rdi, [rsp + 8]        ; right
    call obj_decref
    mov rdi, [rsp + 16]       ; left
    call obj_decref
    pop rax                    ; result
    add rsp, 16                ; discard right, left
    pop rcx                    ; invert

    ; Invert if 'not in'
    xor eax, ecx

    ; Push bool
    test eax, eax
    jz .contains_false
    lea rax, [rel bool_true]
    jmp .contains_push
.contains_false:
    lea rax, [rel bool_false]
.contains_push:
    INCREF rax
    VPUSH rax
    DISPATCH

.contains_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument of type is not iterable"
    call raise_exception

;; ============================================================================
;; op_build_slice - Build a slice object
;;
;; arg = 2: pop stop, pop start, step=None
;; arg = 3: pop step, pop stop, pop start
;; ============================================================================
global op_build_slice
op_build_slice:
    cmp ecx, 3
    je .bs_three

    ; arg=2: TOS=stop, TOS1=start
    VPOP rsi               ; stop
    VPOP rdi               ; start
    push rdi               ; save start
    push rsi               ; save stop
    lea rdx, [rel none_singleton]  ; step = None
    call slice_new
    push rax               ; save slice
    mov rdi, [rsp + 8]    ; stop
    DECREF_REG rdi
    mov rdi, [rsp + 16]   ; start
    DECREF_REG rdi
    pop rax
    add rsp, 16
    VPUSH rax
    DISPATCH

.bs_three:
    ; arg=3: TOS=step, TOS1=stop, TOS2=start
    VPOP rdx               ; step
    VPOP rsi               ; stop
    VPOP rdi               ; start
    push rdi
    push rsi
    push rdx
    call slice_new
    push rax               ; save slice
    mov rdi, [rsp + 8]    ; step
    DECREF_REG rdi
    mov rdi, [rsp + 16]   ; stop
    DECREF_REG rdi
    mov rdi, [rsp + 24]   ; start
    DECREF_REG rdi
    pop rax
    add rsp, 24
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_binary_slice - obj[start:stop]
;;
;; Python 3.12: pops stop, start, obj from stack.
;; Creates a slice(start, stop), calls mp_subscript(obj, slice).
;; ============================================================================
global op_binary_slice
op_binary_slice:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    ; Pop stop (TOS), start (TOS1), obj (TOS2)
    VPOP rsi               ; stop
    VPOP rdi               ; start
    mov [rbp-8], rdi       ; save start
    mov [rbp-16], rsi      ; save stop
    VPOP rax
    mov [rbp-24], rax      ; save obj

    ; Create slice(start, stop, None)
    mov rdi, [rbp-8]       ; start
    mov rsi, [rbp-16]      ; stop
    lea rdx, [rel none_singleton]  ; step = None
    call slice_new
    mov [rbp-32], rax      ; save slice

    ; Call mp_subscript(obj, slice)
    mov rdi, [rbp-24]      ; obj
    mov rsi, rax           ; slice as key
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    mov rax, [rax + PyMappingMethods.mp_subscript]
    call rax
    push rax               ; save result

    ; DECREF slice
    mov rdi, [rbp-32]
    DECREF_REG rdi
    ; DECREF start, stop, obj
    mov rdi, [rbp-8]
    DECREF_REG rdi
    mov rdi, [rbp-16]
    DECREF_REG rdi
    mov rdi, [rbp-24]
    DECREF_REG rdi

    pop rax
    VPUSH rax
    leave
    DISPATCH

;; ============================================================================
;; op_store_slice - obj[start:stop] = value
;;
;; Python 3.12: pops stop, start, obj, value from stack.
;; ============================================================================
global op_store_slice
op_store_slice:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    ; Pop stop (TOS), start (TOS1), obj (TOS2), value (TOS3)
    VPOP rsi               ; stop
    VPOP rdi               ; start
    mov [rbp-8], rdi       ; start
    mov [rbp-16], rsi      ; stop
    VPOP rax
    mov [rbp-24], rax      ; obj
    VPOP rax
    mov [rbp-32], rax      ; value

    ; Create slice(start, stop, None)
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    lea rdx, [rel none_singleton]  ; step = None
    call slice_new
    mov [rbp-40], rax      ; save slice

    ; Call mp_ass_subscript(obj, slice, value)
    mov rdi, [rbp-24]      ; obj
    mov rsi, rax           ; slice
    mov rdx, [rbp-32]      ; value
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    mov rax, [rax + PyMappingMethods.mp_ass_subscript]
    call rax

    ; DECREF slice, start, stop, obj, value
    mov rdi, [rbp-40]
    DECREF_REG rdi
    mov rdi, [rbp-8]
    DECREF_REG rdi
    mov rdi, [rbp-16]
    DECREF_REG rdi
    mov rdi, [rbp-24]
    DECREF_REG rdi
    mov rdi, [rbp-32]
    DECREF_REG rdi

    leave
    DISPATCH
