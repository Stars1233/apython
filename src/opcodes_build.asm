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

section .text

extern eval_dispatch
extern eval_saved_rbx
extern trace_opcodes
extern opcode_table
extern obj_dealloc
extern obj_decref
extern obj_is_true
extern fatal_error
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
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
extern dict_get
extern range_iter_type
extern list_iter_type

;; Stack/frame layout constants for tag-aware handlers.
;; Push-based: "tags behind payloads" — payload offsets match old code.
;; When result is pushed on top, add 8 to all offsets.

; op_binary_subscr: 2-operand push layout [rsp+...]
BSUB_KEY   equ 0     ; key payload (pushed last)
BSUB_OBJ   equ 8     ; obj payload
BSUB_KTAG  equ 16    ; key tag
BSUB_OTAG  equ 24    ; obj tag
BSUB_SIZE  equ 32

; op_store_subscr: 3-operand push layout [rsp+...]
SSUB_VAL   equ 0     ; value payload (pushed last)
SSUB_KEY   equ 8     ; key payload
SSUB_OBJ   equ 16    ; obj payload
SSUB_KTAG  equ 24    ; key tag
SSUB_OTAG  equ 32    ; obj tag
SSUB_VTAG  equ 40    ; value tag (pushed first)
SSUB_SIZE  equ 48

; op_build_slice 2-arg: [rsp+...]
BSL2_STOP  equ 0     ; stop payload
BSL2_START equ 8     ; start payload
BSL2_PTAG  equ 16    ; stop tag
BSL2_STAG  equ 24    ; start tag
BSL2_SIZE  equ 32

; op_build_slice 3-arg: [rsp+...]
BSL3_STEP  equ 0     ; step payload
BSL3_STOP  equ 8     ; stop payload
BSL3_START equ 16    ; start payload
BSL3_EPTAG equ 24    ; step tag
BSL3_PTAG  equ 32    ; stop tag
BSL3_STAG  equ 40    ; start tag
BSL3_SIZE  equ 48

; op_binary_slice: rbp-frame layout [rbp - ...]
BSLC_START equ 8
BSLC_STOP  equ 16
BSLC_OBJ   equ 24
BSLC_SLICE equ 32
BSLC_STAG  equ 40    ; start tag
BSLC_PTAG  equ 48    ; stop tag
BSLC_OTAG  equ 56    ; obj tag
BSLC_FRAME equ 56

; op_store_slice: rbp-frame layout [rbp - ...]
SSLC_START equ 8
SSLC_STOP  equ 16
SSLC_OBJ   equ 24
SSLC_VAL   equ 32
SSLC_SLICE equ 40
SSLC_STAG  equ 48    ; start tag
SSLC_PTAG  equ 56    ; stop tag
SSLC_OTAG  equ 64    ; obj tag
SSLC_VTAG  equ 72    ; value tag
SSLC_FRAME equ 72

; op_map_add: 2-operand push layout [rsp+...]
MA_VAL   equ 0     ; value (TOS, pushed last)
MA_KEY   equ 8     ; key (TOS1)
MA_VTAG  equ 16    ; value tag
MA_KTAG  equ 24    ; key tag
MA_SIZE  equ 32

; op_contains_op: push layout with invert at bottom [rsp+...]
CN_RIGHT equ 0     ; container payload
CN_LEFT  equ 8     ; value payload
CN_RTAG  equ 16    ; container tag
CN_LTAG  equ 24    ; value tag
CN_INV   equ 32    ; invert flag
CN_SIZE  equ 40

;; ============================================================================
;; op_binary_subscr - obj[key]
;;
;; Pop key, pop obj, call mp_subscript or sq_item, push result.
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_binary_subscr
    VPOP rsi                   ; rsi = key
    mov r8, [r13 + 8]         ; r8 = key tag
    VPOP rdi                   ; rdi = obj
    mov r9, [r13 + 8]         ; r9 = obj tag

    ; Tags behind payloads: intermediate [rsp] refs unchanged
    push r9                    ; save obj tag (deepest)
    push r8                    ; save key tag
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
    ; Try __class_getitem__ on type objects FIRST (for MyClass[args] syntax)
    mov rdi, [rsp+8]              ; obj
    test rdi, rdi
    js .try_getitem_dunder
    mov rax, [rdi + PyObject.ob_type]
    extern user_type_metatype
    extern type_type
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    je .try_class_getitem
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .try_class_getitem

.try_getitem_dunder:
    ; Try __getitem__ on heaptype
    mov rdi, [rsp+8]          ; obj
    test rdi, rdi
    js .subscr_error
    mov rax, [rdi + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .subscr_error
    extern dunder_getitem
    extern dunder_call_2
    mov rsi, [rsp]            ; key = other
    lea rdx, [rel dunder_getitem]
    call dunder_call_2
    test rax, rax
    jnz .subscr_done
    jmp .subscr_error

.try_class_getitem:
    ; obj is a type — look up __class_getitem__ in its tp_dict (walk MRO)
    ; Stack: [rsp]=key, [rsp+8]=obj
    extern dunder_lookup
    extern classmethod_type
    mov rdi, [rsp+8]              ; obj (the type itself)
    CSTRING rsi, "__class_getitem__"
    call dunder_lookup
    test rax, rax
    jz .subscr_error

    ; rax = __class_getitem__ attr (borrowed ref)
    ; Check if it's a classmethod wrapper — unwrap if so
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    jne .cgi_not_classmethod

    ; Unwrap classmethod: get cm_callable, call with (cls, key)
    mov rax, [rax + PyClassMethodObject.cm_callable]

.cgi_not_classmethod:
    ; Call func(cls, key): tp_call(func, &[cls, key], 2)
    mov rdi, rax
    mov rcx, [rdi + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .subscr_error

    ; Build args: [cls, key]
    mov rax, [rsp]                ; key
    push rax                      ; args[1] = key
    mov rax, [rsp + 16]           ; obj (type/cls) — shifted by push
    push rax                      ; args[0] = cls
    mov rsi, rsp                  ; args ptr
    mov edx, 2                    ; nargs = 2
    call rcx
    add rsp, 16                   ; pop args
    jmp .subscr_done

.subscr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not subscriptable"
    call raise_exception

.subscr_done:
    ; rax = result
    push rax                   ; save result (+8 shifts BSUB_ offsets)
    mov rdi, [rsp + 8 + BSUB_KEY]
    mov rsi, [rsp + 8 + BSUB_KTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 8 + BSUB_OBJ]
    mov rsi, [rsp + 8 + BSUB_OTAG]
    DECREF_VAL rdi, rsi
    pop rax                    ; restore result
    add rsp, BSUB_SIZE

    VPUSH_BRANCHLESS rax

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH
END_FUNC op_binary_subscr

;; ============================================================================
;; op_store_subscr - obj[key] = value
;;
;; Stack: value, obj, key (TOS)
;; Pop key, pop obj, pop value.
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_store_subscr
    VPOP rsi                   ; rsi = key (TOS)
    mov r8, [r13 + 8]         ; r8 = key tag
    VPOP rdi                   ; rdi = obj
    mov r9, [r13 + 8]         ; r9 = obj tag
    VPOP rdx                   ; rdx = value
    mov r10, [r13 + 8]        ; r10 = value tag

    ; Tags behind payloads: intermediate [rsp] refs unchanged
    push r10                   ; save value tag (deepest)
    push r9                    ; save obj tag
    push r8                    ; save key tag
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
    ; Try __setitem__ on heaptype
    mov rdi, [rsp+16]         ; obj
    mov rax, [rdi + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .store_type_error

    ; Look up __setitem__
    extern dunder_setitem
    extern dunder_lookup
    mov rdi, [rsp+16]         ; obj
    mov rdi, [rdi + PyObject.ob_type]
    lea rsi, [rel dunder_setitem]
    call dunder_lookup
    test rax, rax
    jz .store_type_error

    ; Call __setitem__(self, key, value) via tp_call
    mov rcx, rax              ; func
    mov rax, [rcx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .store_type_error

    ; Build args array on machine stack: [self, key, value]
    push qword [rsp+0]       ; args[2] = value  (at original [rsp+0])
    push qword [rsp+16]      ; args[1] = key    (at original [rsp+8], shifted by push)
    push qword [rsp+32]      ; args[0] = self   (at original [rsp+16], shifted by 2 pushes)
    mov rdi, rcx              ; callable
    mov rsi, rsp              ; args ptr
    mov edx, 3                ; nargs
    call rax
    add rsp, 24               ; pop args array
    ; rax = result (discard — __setitem__ returns None)
    jmp .store_done

.store_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support item assignment"
    call raise_exception

.store_done:
    mov rdi, [rsp + SSUB_VAL]
    mov rsi, [rsp + SSUB_VTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + SSUB_KEY]
    mov rsi, [rsp + SSUB_KTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + SSUB_OBJ]
    mov rsi, [rsp + SSUB_OTAG]
    DECREF_VAL rdi, rsi
    add rsp, SSUB_SIZE

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH
END_FUNC op_store_subscr

;; ============================================================================
;; op_build_tuple - Create tuple from TOS items
;;
;; ecx = count (number of items to pop)
;; Items are on stack bottom-to-top: first item deepest.
;; ============================================================================
DEF_FUNC op_build_tuple, 16
    ; [rbp-8] = count

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

    ; Calculate base of items on value stack (16 bytes/slot)
    mov rdi, rcx
    shl rdi, 4                 ; count * 16
    sub r13, rdi               ; pop all items at once

.build_tuple_fill:
    mov rax, rdx
    shl rax, 4                 ; index * 16
    mov rsi, [r13 + rax]      ; item payload from stack
    mov rdi, [r13 + rax + 8]  ; item tag from stack
    mov rax, [rbp-16]
    mov rcx, rdx
    shl rcx, 4                ; dest index * 16
    mov [rax + PyTupleObject.ob_item + rcx], rsi      ; payload
    mov [rax + PyTupleObject.ob_item + rcx + 8], rdi  ; tag
    inc rdx
    cmp rdx, [rbp-8]
    jb .build_tuple_fill

.build_tuple_done:
    mov rax, [rbp-16]
    VPUSH_PTR rax
    leave
    DISPATCH
END_FUNC op_build_tuple

;; ============================================================================
;; op_build_list - Create list from TOS items
;;
;; ecx = count
;; ============================================================================
DEF_FUNC op_build_list, 16

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

    ; Calculate base (16 bytes/slot)
    mov rdi, rcx
    shl rdi, 4
    sub r13, rdi               ; pop all items

    xor edx, edx
.build_list_fill:
    cmp rdx, [rbp-8]
    jge .build_list_done
    push rdx
    mov rdi, [rbp-16]         ; list
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rsi, [r13 + rax]      ; item payload (ownership transfers, no extra INCREF)
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
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rdi, [r13 + rax]
    mov rsi, [r13 + rax + 8]  ; tag
    push rdx
    DECREF_VAL rdi, rsi
    pop rdx
    inc rdx
    jmp .build_list_fixref

.build_list_push:
    mov rax, [rbp-16]
    VPUSH_PTR rax
    leave
    DISPATCH
END_FUNC op_build_list

;; ============================================================================
;; op_build_map - Create dict from TOS key/value pairs
;;
;; ecx = count (number of key/value pairs)
;; Stack (bottom to top): key0, val0, key1, val1, ...
;; ============================================================================
DEF_FUNC op_build_map, 16

    mov [rbp-8], rcx           ; save count

    call dict_new
    mov [rbp-16], rax          ; save dict

    ; Total items on stack = count * 2
    mov rcx, [rbp-8]
    shl rcx, 1                 ; count * 2
    test rcx, rcx
    jz .build_map_done

    mov rdi, rcx
    shl rdi, 4                 ; total_items * 16 bytes/slot
    sub r13, rdi               ; pop all items

    xor edx, edx              ; pair index
.build_map_fill:
    cmp rdx, [rbp-8]
    jge .build_map_done
    push rdx
    mov rdi, [rbp-16]         ; dict
    mov rax, rdx
    shl rax, 5                 ; pair_index * 32 (2 slots * 16 bytes)
    mov rsi, [r13 + rax]      ; key (payload at pair base)
    mov rdx, [r13 + rax + 16] ; value (payload at pair base + 16)
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
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rdi, [r13 + rax]
    mov rsi, [r13 + rax + 8]  ; tag
    push rdx
    push rcx
    DECREF_VAL rdi, rsi
    pop rcx
    pop rdx
    inc rdx
    jmp .build_map_fixref

.build_map_push:
    mov rax, [rbp-16]
    VPUSH_PTR rax
    leave
    DISPATCH
END_FUNC op_build_map

;; ============================================================================
;; op_build_const_key_map - Build dict from const keys tuple + TOS values
;;
;; ecx = count
;; Stack: val0, val1, ..., valN-1, keys_tuple (TOS)
;; ============================================================================
DEF_FUNC op_build_const_key_map, 32

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
    shl rdi, 4                 ; count * 16 bytes/slot
    sub r13, rdi               ; pop all values

    xor edx, edx
.bckm_fill:
    cmp rdx, [rbp-8]
    jge .bckm_done
    push rdx
    mov rdi, [rbp-24]         ; dict
    mov rax, [rbp-16]         ; keys tuple
    mov rcx, rdx
    shl rcx, 4                ; index * 16
    mov rsi, [rax + PyTupleObject.ob_item + rcx]  ; key payload
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rdx, [r13 + rax]      ; value (payload)
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
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rdi, [r13 + rax]
    mov rsi, [r13 + rax + 8]  ; tag
    push rdx
    push rcx
    DECREF_VAL rdi, rsi
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
    VPUSH_PTR rax
    leave
    DISPATCH
END_FUNC op_build_const_key_map

;; ============================================================================
;; op_unpack_sequence - Unpack iterable into N items on stack
;;
;; ecx = count
;; Pop TOS (tuple/list), push items[count-1], ..., items[0] (reverse order)
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
DEF_FUNC_BARE op_unpack_sequence
    VPOP rdi                   ; rdi = sequence (tuple or list)
    mov r8, [r13 + 8]         ; r8 = sequence tag

    ; Determine if tuple or list and get item array + size
    push r8                    ; save tag (deeper)
    push rdi                   ; save payload

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
    ; Items are inline at ob_item, fat 16-byte stride
    mov edx, ecx              ; edx = count
    dec edx
.unpack_tuple_loop:
    test edx, edx
    js .unpack_done
    mov eax, edx               ; zero-extend edx to rax
    shl rax, 4                ; index * 16
    mov rcx, [rdi + PyTupleObject.ob_item + rax + 8]  ; tag
    mov rax, [rdi + PyTupleObject.ob_item + rax]       ; payload
    INCREF_VAL rax, rcx
    push rdx
    push rdi
    VPUSH_VAL rax, rcx
    pop rdi
    pop rdx
    dec edx
    jmp .unpack_tuple_loop

.unpack_list:
    ; Items at ob_item pointer, fat 16-byte stride
    mov rsi, [rdi + PyListObject.ob_item]
    mov edx, ecx
    dec edx
.unpack_list_loop:
    test edx, edx
    js .unpack_done
    mov eax, edx               ; zero-extend edx to rax
    shl rax, 4                 ; index * 16
    mov rcx, [rsi + rax + 8]   ; tag
    mov rax, [rsi + rax]       ; payload
    INCREF_VAL rax, rcx
    push rdx
    push rsi
    VPUSH_VAL rax, rcx
    pop rsi
    pop rdx
    dec edx
    jmp .unpack_list_loop

.unpack_done:
    ; DECREF the sequence (payload + tag)
    pop rdi                    ; sequence payload
    pop rsi                    ; sequence tag
    DECREF_VAL rdi, rsi

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    DISPATCH
END_FUNC op_unpack_sequence

;; ============================================================================
;; op_get_iter - Get iterator from TOS
;;
;; Pop obj, call tp_iter, push iterator.
;; ============================================================================
DEF_FUNC_BARE op_get_iter
    VPOP rdi                   ; rdi = iterable obj
    mov r8, [r13 + 8]         ; r8 = iterable tag

    push r8                    ; save tag (deeper)
    push rdi                   ; save payload

    ; Get tp_iter from type
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, rax               ; save type
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jnz .have_iter

    ; tp_iter NULL — try __iter__ on heaptype
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .not_iterable
    extern dunder_iter
    lea rsi, [rel dunder_iter]
    extern dunder_call_1
    call dunder_call_1
    test rax, rax
    jz .not_iterable
    ; rax = iterator from __iter__, skip the tp_iter call
    jmp .have_iter_result

.have_iter:
    ; Call tp_iter(obj) -> iterator
    call rax
.have_iter_result:
    push rax                   ; save iterator on machine stack

    ; DECREF the original iterable (tag-aware)
    mov rdi, [rsp + 8]        ; iterable payload
    mov rsi, [rsp + 16]       ; iterable tag
    DECREF_VAL rdi, rsi
    pop rax                    ; restore iterator
    add rsp, 16                ; discard iterable payload + tag

    VPUSH_PTR rax
    DISPATCH

.not_iterable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception
END_FUNC op_get_iter

;; ============================================================================
;; op_for_iter - Advance iterator, or jump if exhausted
;;
;; ecx = jump offset (instruction words) if exhausted
;; TOS = iterator
;; If iterator has next: push value (iterator stays on stack)
;; If exhausted: pop iterator, jump forward by arg
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
align 16
DEF_FUNC_BARE op_for_iter
    push rcx                   ; save jump offset on machine stack

    ; Peek at iterator (don't pop yet)
    VPEEK rdi

    ; Try to specialize (first execution)
    mov rax, [rdi + PyObject.ob_type]
    lea rdx, [rel range_iter_type]
    cmp rax, rdx
    je .fi_specialize_range
    lea rdx, [rel list_iter_type]
    cmp rax, rdx
    je .fi_specialize_list
    jmp .fi_no_specialize

.fi_specialize_range:
    mov byte [rbx - 2], 214    ; rewrite to FOR_ITER_RANGE
    jmp .fi_no_specialize      ; continue with normal execution this time

.fi_specialize_list:
    mov byte [rbx - 2], 213    ; rewrite to FOR_ITER_LIST
    ; fall through to normal execution

.fi_no_specialize:
    ; Call tp_iternext(iterator)
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, rax               ; save type
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jnz .have_iternext

    ; tp_iternext NULL — try __next__ on heaptype
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .exhausted
    extern dunder_next
    lea rsi, [rel dunder_next]
    extern dunder_call_1
    call dunder_call_1
    ; rax = value or NULL (if __next__ not found)
    jmp .check_next_result

.have_iternext:
    call rax
.check_next_result:
    ; rax = next value, or NULL if exhausted

    test rax, rax
    jz .exhausted

    ; Got a value - push it (iterator stays on stack)
    add rsp, 8                 ; discard saved jump offset
    VPUSH_BRANCHLESS rax

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
    sub r13, 16
    mov rdi, [r13]
    mov rsi, [r13 + 8]
    DECREF_VAL rdi, rsi

    DISPATCH
END_FUNC op_for_iter

;; ============================================================================
;; op_end_for - End of for loop cleanup
;;
;; In Python 3.12, END_FOR (opcode 4) pops TOS (the exhausted iterator value).
;; Actually END_FOR pops 2 items in 3.12.
;; ============================================================================
DEF_FUNC_BARE op_end_for
    ; Pop TOS (end-of-iteration sentinel / last value)
    sub r13, 16
    mov rdi, [r13]
    mov rsi, [r13 + 8]
    DECREF_VAL rdi, rsi
    ; Pop the iterator
    sub r13, 16
    mov rdi, [r13]
    mov rsi, [r13 + 8]
    DECREF_VAL rdi, rsi
    DISPATCH
END_FUNC op_end_for

;; ============================================================================
;; op_list_append - Append TOS to list at stack position
;;
;; ecx = position (1-based from TOS before the value to append)
;; list is at stack[-(ecx+1)] relative to current TOS
;; Pop TOS (value), append to list.
;; ============================================================================
DEF_FUNC_BARE op_list_append
    ; TOS = value to append
    VPOP rsi                   ; rsi = value
    mov r8, [r13 + 8]         ; r8 = value tag

    ; list is at stack[-(ecx)] after popping (16 bytes/slot)
    neg rcx
    shl rcx, 4                ; -ecx * 16
    mov rdi, [r13 + rcx]      ; rdi = list

    push r8                    ; save value tag (deeper)
    push rsi                   ; save value payload
    call list_append
    ; list_append does INCREF, so DECREF to compensate
    pop rdi                    ; value payload
    pop rsi                    ; value tag
    DECREF_VAL rdi, rsi

    DISPATCH
END_FUNC op_list_append

;; ============================================================================
;; op_list_extend - Extend list with iterable
;;
;; ecx = position (list at stack[-(ecx)] after pop)
;; Pop TOS (iterable), extend list.
;; ============================================================================
DEF_FUNC op_list_extend, 32
    ; locals: [rbp-8]=list, [rbp-16]=iterable, [rbp-24]=count, [rbp-32]=items

    ; TOS = iterable
    VPOP rsi                   ; rsi = iterable (tuple or list)
    mov [rbp-16], rsi          ; save iterable

    ; list is at stack[-(ecx)] after popping (16 bytes/slot)
    neg rcx
    shl rcx, 4                ; -ecx * 16
    mov rdi, [r13 + rcx]      ; rdi = list
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
    mov [rbp-24], rcx          ; count
    test rcx, rcx
    jz .extend_done
    xor r8d, r8d               ; index
.extend_tuple_loop:
    mov rdi, [rbp-8]          ; list
    mov rax, [rbp-16]         ; iterable (tuple)
    mov rcx, r8
    shl rcx, 4                ; index * 16
    mov rsi, [rax + PyTupleObject.ob_item + rcx]  ; payload
    push r8
    call list_append
    pop r8
    inc r8
    cmp r8, [rbp-24]
    jb .extend_tuple_loop
    jmp .extend_done

.extend_list:
    mov rcx, [rsi + PyListObject.ob_size]
    mov rdx, [rsi + PyListObject.ob_item]

    mov [rbp-24], rcx          ; count
    mov [rbp-32], rdx          ; items ptr

    test rcx, rcx
    jz .extend_done
    xor r8d, r8d               ; index
.extend_list_loop:
    mov rdi, [rbp-8]          ; list
    mov rdx, [rbp-32]         ; items ptr
    mov rax, r8
    shl rax, 4                ; index * 16
    mov rsi, [rdx + rax]      ; item payload
    push r8
    call list_append
    pop r8
    inc r8
    cmp r8, [rbp-24]          ; count
    jb .extend_list_loop

.extend_done:
    ; DECREF iterable
    mov rdi, [rbp-16]
    call obj_decref

    leave
    DISPATCH
END_FUNC op_list_extend

;; ============================================================================
;; op_is_op - Identity comparison (is / is not)
;;
;; ecx = 0 for 'is', 1 for 'is not'
;; Pop right, pop left, push True/False.
;; ============================================================================
DEF_FUNC_BARE op_is_op
    mov r8d, ecx               ; save invert flag

    VPOP rsi                   ; right
    mov r9, [r13 + 8]         ; r9 = right tag
    VPOP rdi                   ; left
    mov r10, [r13 + 8]        ; r10 = left tag

    ; Compare pointers
    cmp rdi, rsi
    sete al
    movzx eax, al              ; eax = 1 if same, 0 if different

    ; DECREF both (tag-aware) — save left before DECREF right
    push rax
    push r8
    push r10                   ; save left tag
    push rdi                   ; save left payload
    DECREF_VAL rsi, r9         ; DECREF right (regs live before call)
    pop rdi                    ; restore left payload
    pop rsi                    ; restore left tag
    DECREF_VAL rdi, rsi        ; DECREF left
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
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_is_op

;; ============================================================================
;; op_contains_op - 'in' / 'not in' test
;;
;; ecx = 0 for 'in', 1 for 'not in'
;; Stack: right (container), left (value)
;; Pop right (container), pop left (value to find).
;; ============================================================================
DEF_FUNC_BARE op_contains_op
    mov r8d, ecx               ; save invert flag

    VPOP rsi                   ; rsi = right (container)
    mov r9, [r13 + 8]         ; r9 = right tag
    VPOP rdi                   ; rdi = left (value to find)
    mov r10, [r13 + 8]        ; r10 = left tag

    ; Tags behind payloads, invert at bottom
    push r8                    ; save invert (deepest)
    push r10                   ; left tag
    push r9                    ; right tag
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

    ; DECREF both (tag-aware, +8 for push rax)
    mov rdi, [rsp + 8 + CN_RIGHT]
    mov rsi, [rsp + 8 + CN_RTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 8 + CN_LEFT]
    mov rsi, [rsp + 8 + CN_LTAG]
    DECREF_VAL rdi, rsi
    pop rax                    ; result
    add rsp, CN_SIZE - 8       ; discard payloads + tags (CN_INV popped next)
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
    VPUSH_PTR rax
    DISPATCH

.contains_error:
    ; Try __contains__ on heaptype
    mov rdi, [rsp]            ; container
    mov rax, [rdi + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .contains_type_error

    extern dunder_contains
    extern dunder_call_2
    mov rdi, [rsp]            ; container = self
    mov rsi, [rsp+8]          ; value = other
    lea rdx, [rel dunder_contains]
    call dunder_call_2
    test rax, rax
    jz .contains_type_error

    ; Convert result to boolean (obj_is_true)
    push rax
    mov rdi, rax
    extern obj_is_true
    call obj_is_true
    mov ecx, eax              ; save truthiness
    pop rdi
    DECREF_REG rdi             ; DECREF the __contains__ result
    mov eax, ecx

    ; Continue with result in eax — DECREF operands (tag-aware)
    push rax                   ; +8 shifts CN_ offsets
    mov rdi, [rsp + 8 + CN_RIGHT]
    mov rsi, [rsp + 8 + CN_RTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 8 + CN_LEFT]
    mov rsi, [rsp + 8 + CN_LTAG]
    DECREF_VAL rdi, rsi
    pop rax
    add rsp, CN_SIZE - 8      ; discard payloads + tags
    pop rcx                    ; invert
    xor eax, ecx
    test eax, eax
    jz .contains_false
    lea rax, [rel bool_true]
    jmp .contains_push

.contains_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument of type is not iterable"
    call raise_exception
END_FUNC op_contains_op

;; ============================================================================
;; op_build_slice - Build a slice object
;;
;; arg = 2: pop stop, pop start, step=None
;; arg = 3: pop step, pop stop, pop start
;; ============================================================================
DEF_FUNC_BARE op_build_slice
    cmp ecx, 3
    je .bs_three

    ; arg=2: TOS=stop, TOS1=start
    VPOP rsi               ; stop
    mov r8, [r13 + 8]     ; stop tag
    VPOP rdi               ; start
    mov r9, [r13 + 8]     ; start tag
    ; Tags behind payloads
    push r9                ; start tag (deepest)
    push r8                ; stop tag
    push rdi               ; start
    push rsi               ; stop
    lea rdx, [rel none_singleton]  ; step = None
    call slice_new
    push rax               ; save slice (+8 shifts BSL2_ offsets)
    mov rdi, [rsp + 8 + BSL2_STOP]
    mov rsi, [rsp + 8 + BSL2_PTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 8 + BSL2_START]
    mov rsi, [rsp + 8 + BSL2_STAG]
    DECREF_VAL rdi, rsi
    pop rax
    add rsp, BSL2_SIZE
    VPUSH_PTR rax
    DISPATCH

.bs_three:
    ; arg=3: TOS=step, TOS1=stop, TOS2=start
    VPOP rdx               ; step
    mov r8, [r13 + 8]     ; step tag
    VPOP rsi               ; stop
    mov r9, [r13 + 8]     ; stop tag
    VPOP rdi               ; start
    mov r10, [r13 + 8]    ; start tag
    ; Tags behind payloads
    push r10               ; start tag (deepest)
    push r9                ; stop tag
    push r8                ; step tag
    push rdi               ; start
    push rsi               ; stop
    push rdx               ; step
    call slice_new
    push rax               ; save slice (+8 shifts BSL3_ offsets)
    mov rdi, [rsp + 8 + BSL3_STEP]
    mov rsi, [rsp + 8 + BSL3_EPTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 8 + BSL3_STOP]
    mov rsi, [rsp + 8 + BSL3_PTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + 8 + BSL3_START]
    mov rsi, [rsp + 8 + BSL3_STAG]
    DECREF_VAL rdi, rsi
    pop rax
    add rsp, BSL3_SIZE
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_build_slice

;; ============================================================================
;; op_binary_slice - obj[start:stop]
;;
;; Python 3.12: pops stop, start, obj from stack.
;; Creates a slice(start, stop), calls mp_subscript(obj, slice).
;; ============================================================================
DEF_FUNC op_binary_slice, BSLC_FRAME

    ; Pop stop (TOS), start (TOS1), obj (TOS2) — save payloads + tags
    VPOP rsi               ; stop
    mov rax, [r13 + 8]
    mov [rbp - BSLC_PTAG], rax
    VPOP rdi               ; start
    mov rax, [r13 + 8]
    mov [rbp - BSLC_STAG], rax
    mov [rbp - BSLC_START], rdi
    mov [rbp - BSLC_STOP], rsi
    VPOP rax
    mov rcx, [r13 + 8]
    mov [rbp - BSLC_OTAG], rcx
    mov [rbp - BSLC_OBJ], rax

    ; Create slice(start, stop, None)
    mov rdi, [rbp - BSLC_START]
    mov rsi, [rbp - BSLC_STOP]
    lea rdx, [rel none_singleton]  ; step = None
    call slice_new
    mov [rbp - BSLC_SLICE], rax

    ; Call mp_subscript(obj, slice)
    mov rdi, [rbp - BSLC_OBJ]
    mov rsi, rax           ; slice as key
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    mov rax, [rax + PyMappingMethods.mp_subscript]
    call rax
    push rax               ; save result

    ; DECREF slice (heap ptr, no tag needed)
    mov rdi, [rbp - BSLC_SLICE]
    DECREF_REG rdi
    ; DECREF start, stop, obj (tag-aware)
    mov rdi, [rbp - BSLC_START]
    mov rsi, [rbp - BSLC_STAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rbp - BSLC_STOP]
    mov rsi, [rbp - BSLC_PTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rbp - BSLC_OBJ]
    mov rsi, [rbp - BSLC_OTAG]
    DECREF_VAL rdi, rsi

    pop rax
    VPUSH_BRANCHLESS rax
    leave
    DISPATCH
END_FUNC op_binary_slice

;; ============================================================================
;; op_store_slice - obj[start:stop] = value
;;
;; Python 3.12: pops stop, start, obj, value from stack.
;; ============================================================================
DEF_FUNC op_store_slice, SSLC_FRAME

    ; Pop stop (TOS), start (TOS1), obj (TOS2), value (TOS3) — save tags
    VPOP rsi               ; stop
    mov rax, [r13 + 8]
    mov [rbp - SSLC_PTAG], rax
    VPOP rdi               ; start
    mov rax, [r13 + 8]
    mov [rbp - SSLC_STAG], rax
    mov [rbp - SSLC_START], rdi
    mov [rbp - SSLC_STOP], rsi
    VPOP rax
    mov rcx, [r13 + 8]
    mov [rbp - SSLC_OTAG], rcx
    mov [rbp - SSLC_OBJ], rax
    VPOP rax
    mov rcx, [r13 + 8]
    mov [rbp - SSLC_VTAG], rcx
    mov [rbp - SSLC_VAL], rax

    ; Create slice(start, stop, None)
    mov rdi, [rbp - SSLC_START]
    mov rsi, [rbp - SSLC_STOP]
    lea rdx, [rel none_singleton]  ; step = None
    call slice_new
    mov [rbp - SSLC_SLICE], rax

    ; Call mp_ass_subscript(obj, slice, value)
    mov rdi, [rbp - SSLC_OBJ]
    mov rsi, rax           ; slice
    mov rdx, [rbp - SSLC_VAL]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    mov rax, [rax + PyMappingMethods.mp_ass_subscript]
    call rax

    ; DECREF slice (heap ptr, no tag needed)
    mov rdi, [rbp - SSLC_SLICE]
    DECREF_REG rdi
    ; DECREF start, stop, obj, value (tag-aware)
    mov rdi, [rbp - SSLC_START]
    mov rsi, [rbp - SSLC_STAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rbp - SSLC_STOP]
    mov rsi, [rbp - SSLC_PTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rbp - SSLC_OBJ]
    mov rsi, [rbp - SSLC_OTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rbp - SSLC_VAL]
    mov rsi, [rbp - SSLC_VTAG]
    DECREF_VAL rdi, rsi

    leave
    DISPATCH
END_FUNC op_store_slice

;; ============================================================================
;; op_map_add - Add key:value to dict at stack position
;;
;; MAP_ADD (147): used by dict comprehensions
;; TOS = value, TOS1 = key
;; dict is at stack[-(ecx+2)] relative to current TOS (before pops)
;; ============================================================================
DEF_FUNC op_map_add
    push rcx                   ; save oparg

    VPOP rdx                   ; rdx = value (TOS)
    mov r8, [r13 + 8]         ; r8 = value tag
    VPOP rsi                   ; rsi = key (TOS1)
    mov r9, [r13 + 8]         ; r9 = key tag

    ; dict is at stack[-(ecx)] after the 2 pops (16 bytes/slot)
    pop rcx                    ; restore oparg
    neg rcx
    shl rcx, 4                ; -ecx * 16
    mov rdi, [r13 + rcx]      ; rdi = dict

    ; Save key and value with tags behind payloads
    push r9                    ; key tag (deepest)
    push r8                    ; value tag
    push rsi                   ; key
    push rdx                   ; value
    call dict_set

    ; DECREF key and value (tag-aware, dict_set INCREF'd them)
    mov rdi, [rsp + MA_VAL]
    mov rsi, [rsp + MA_VTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rsp + MA_KEY]
    mov rsi, [rsp + MA_KTAG]
    DECREF_VAL rdi, rsi
    add rsp, MA_SIZE

    leave
    DISPATCH
END_FUNC op_map_add

;; ============================================================================
;; op_dict_update - Update dict with another mapping
;;
;; DICT_UPDATE (165): dict.update(mapping)
;; TOS = mapping, dict at stack[-(ecx+1)] after pop
;; Pop TOS, merge all key:value pairs into dict.
;; ============================================================================
extern dict_type

DEF_FUNC op_dict_update
    push rbx
    push r14                   ; extra callee-saved
    sub rsp, 32                ; locals + alignment

    VPOP rsi                   ; rsi = mapping to merge from
    mov [rbp-24], rsi

    ; dict is at stack[-(ecx)] after pop (16 bytes/slot)
    neg rcx
    shl rcx, 4                ; -ecx * 16
    mov rdi, [r13 + rcx]
    mov [rbp-32], rdi          ; target dict

    ; mapping must be a dict (for now)
    mov rax, [rsi + PyObject.ob_type]
    lea rdx, [rel dict_type]
    cmp rax, rdx
    jne .du_type_error

    ; Iterate over source dict entries and copy to target
    ; Source dict: entries at [rsi + PyDictObject.entries], capacity at +24
    mov rax, [rsi + PyDictObject.capacity]
    mov [rbp-40], rax          ; capacity
    mov rax, [rsi + PyDictObject.entries]
    mov [rbp-48], rax          ; entries ptr
    xor ebx, ebx              ; index

.du_loop:
    cmp rbx, [rbp-40]
    jge .du_done

    ; Check if entry has a key and value_tag != TAG_NULL
    mov rax, [rbp-48]
    imul rcx, rbx, DictEntry_size
    add rax, rcx
    mov rsi, [rax + DictEntry.key]
    test rsi, rsi
    jz .du_next

    cmp qword [rax + DictEntry.value_tag], 0
    je .du_next
    mov rdx, [rax + DictEntry.value]

    ; dict_set(target, key, value)
    push rbx
    mov rdi, [rbp-32]
    call dict_set
    pop rbx

.du_next:
    inc rbx
    jmp .du_loop

.du_done:
    ; DECREF the mapping
    mov rdi, [rbp-24]
    call obj_decref

    add rsp, 32
    pop r14
    pop rbx
    leave
    DISPATCH

.du_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "dict.update() argument must be a dict"
    call raise_exception
END_FUNC op_dict_update

;; ============================================================================
;; op_dict_merge - Merge dict (like dict_update but for **kwargs)
;;
;; DICT_MERGE (164): same as DICT_UPDATE but raises on duplicate keys
;; For simplicity, just delegate to dict_update logic (no dup check).
;; ============================================================================
DEF_FUNC_BARE op_dict_merge
    jmp op_dict_update         ; same behavior for now
END_FUNC op_dict_merge

;; ============================================================================
;; op_unpack_ex - Unpack with *rest
;;
;; UNPACK_EX (94): arg encodes (count_before | count_after << 8)
;; Pop iterable from TOS, push count_after items, then a list of remaining,
;; then count_before items (in reverse order on stack).
;; ============================================================================
extern list_type
extern list_getitem
extern tuple_getitem

DEF_FUNC op_unpack_ex
    push rbx
    push r14
    push r15
    sub rsp, 32                ; locals: [rbp-32]=total_len, [rbp-40]=rest_count, [rbp-48]=iter_tag

    ; Decode arg: count_before = ecx & 0xFF, count_after = ecx >> 8
    mov eax, ecx
    and eax, 0xFF
    mov ebx, eax               ; ebx = count_before
    mov eax, ecx
    shr eax, 8
    mov r14d, eax              ; r14 = count_after

    ; Pop iterable
    VPOP rdi
    mov rax, [r13 + 8]        ; iterable tag
    mov [rbp-48], rax
    mov r15, rdi               ; r15 = iterable

    ; Get length
    mov rax, [r15 + PyObject.ob_type]
    lea rcx, [rel list_type]
    cmp rax, rcx
    je .ue_list

    extern tuple_type
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .ue_tuple

    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "cannot unpack non-sequence"
    call raise_exception

.ue_list:
    mov rax, [r15 + PyListObject.ob_size]
    jmp .ue_have_len
.ue_tuple:
    mov rax, [r15 + PyTupleObject.ob_size]

.ue_have_len:
    ; rax = total length
    ; We need: count_before + count_after <= total_length
    lea rcx, [rbx + r14]      ; count_before + count_after
    cmp rax, rcx
    jl .ue_not_enough

    mov [rbp-32], rax          ; save total_len

    ; Compute rest_count = total_len - count_before - count_after
    sub rax, rbx
    sub rax, r14
    mov [rbp-40], rax          ; rest_count

    ; Push in reverse order (top of stack = last pushed = first in sequence)
    ; Stack order (bottom to top):
    ;   last after_item, ..., first after_item, rest_list, last before_item, ..., first before_item
    ; Wait, Python actually pushes in this order:
    ;   Push count_after items in reverse (items from end)
    ;   Push rest list
    ;   Push count_before items in reverse (items from start)
    ; So TOS = first_before, TOS1 = second_before, ..., then rest, then after items

    ; 1. Push count_after items (from end, in reverse)
    mov rcx, r14
    test rcx, rcx
    jz .ue_no_after

    ; after items are at indices [total_len - count_after .. total_len - 1]
    ; Push them in reverse: index total_len-1, total_len-2, ..., total_len-count_after
    mov rax, [rbp-32]          ; total_len
    dec rax                    ; start from total_len - 1
.ue_after_loop:
    test rcx, rcx
    jz .ue_no_after
    push rcx
    push rax

    ; Get item at index rax from iterable
    mov rdi, r15
    mov rsi, rax
    call .ue_getitem           ; rax = item (borrowed)
    INCREF rax
    VPUSH_BRANCHLESS rax

    pop rax
    pop rcx
    dec rax
    dec rcx
    jmp .ue_after_loop

.ue_no_after:
    ; 2. Build rest list
    mov rdi, [rbp-40]          ; rest_count as initial capacity
    call list_new
    push rax                   ; save rest list

    ; Add items at indices [count_before .. count_before + rest_count - 1]
    mov rcx, [rbp-40]          ; rest_count
    test rcx, rcx
    jz .ue_rest_done
    mov rax, rbx               ; start index = count_before
.ue_rest_loop:
    test rcx, rcx
    jz .ue_rest_done
    push rcx
    push rax

    mov rdi, r15
    mov rsi, rax
    call .ue_getitem           ; rax = item (borrowed)
    mov rsi, rax
    mov rdi, [rsp + 16]        ; rest list (2 pushes deep)
    push rsi
    call list_append           ; list_append does INCREF
    pop rsi                    ; discard
    pop rax
    pop rcx
    inc rax
    dec rcx
    jmp .ue_rest_loop

.ue_rest_done:
    pop rax                    ; rest list
    VPUSH_PTR rax              ; push rest list

    ; 3. Push count_before items in reverse (from index count_before-1 down to 0)
    mov rcx, rbx
    test rcx, rcx
    jz .ue_no_before
    dec rcx                    ; start from count_before - 1
.ue_before_loop:
    push rcx

    mov rdi, r15
    mov rsi, rcx
    call .ue_getitem           ; rax = item (borrowed)
    INCREF rax
    VPUSH_BRANCHLESS rax

    pop rcx
    test rcx, rcx
    jz .ue_no_before
    dec rcx
    jmp .ue_before_loop

.ue_no_before:
    ; DECREF iterable (tag-aware)
    mov rdi, r15
    mov rsi, [rbp-48]         ; iterable tag
    DECREF_VAL rdi, rsi

    add rsp, 32
    pop r15
    pop r14
    pop rbx
    leave
    DISPATCH

.ue_not_enough:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "not enough values to unpack"
    call raise_exception

; Helper: get item at index rsi from iterable r15 (returns borrowed ref in rax)
.ue_getitem:
    mov rax, [r15 + PyObject.ob_type]
    lea rcx, [rel list_type]
    cmp rax, rcx
    je .ue_gi_list
    ; tuple: fat 16-byte stride
    mov rax, rsi
    shl rax, 4
    mov rax, [r15 + PyTupleObject.ob_item + rax]
    ret
.ue_gi_list:
    mov rax, [r15 + PyListObject.ob_item]
    mov rcx, rsi
    shl rcx, 4                ; index * 16
    mov rax, [rax + rcx]      ; payload
    ret
END_FUNC op_unpack_ex

;; ============================================================================
;; op_kw_names - Store keyword argument names for next CALL
;;
;; KW_NAMES (172): Store co_consts[arg] as pending kw_names tuple.
;; The next CALL opcode will use this tuple.
;; ============================================================================
extern kw_names_pending

DEF_FUNC_BARE op_kw_names
    ; ecx = arg (index into co_consts fat array)
    shl ecx, 4                 ; index * 16
    mov rax, [r14 + rcx]       ; payload (always a tuple ptr for kw_names)
    mov [rel kw_names_pending], rax
    DISPATCH
END_FUNC op_kw_names

;; ============================================================================
;; op_build_set - Create set from TOS items
;;
;; ecx = count (number of items to pop)
;; Items are on stack bottom-to-top: first item deepest.
;; ============================================================================
extern set_new
extern set_add
extern set_type

DEF_FUNC op_build_set, 16

    mov [rbp-8], rcx           ; save count

    ; Allocate empty set
    call set_new
    mov [rbp-16], rax          ; save set

    ; Pop items and add to set
    mov rcx, [rbp-8]
    test rcx, rcx
    jz .build_set_done

    ; Calculate base (16 bytes/slot)
    mov rdi, rcx
    shl rdi, 4
    sub r13, rdi               ; pop all items

    xor edx, edx
.build_set_fill:
    cmp rdx, [rbp-8]
    jge .build_set_done
    push rdx
    mov rdi, [rbp-16]         ; set
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rsi, [r13 + rax]     ; item payload
    call set_add               ; set_add does INCREF
    pop rdx
    inc rdx
    jmp .build_set_fill

.build_set_done:
    ; set_add does INCREF on key, so DECREF all stack items to compensate
    mov rcx, [rbp-8]
    test rcx, rcx
    jz .build_set_push
    xor edx, edx
.build_set_fixref:
    cmp rdx, [rbp-8]
    jge .build_set_push
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rdi, [r13 + rax]
    mov rsi, [r13 + rax + 8]  ; tag
    push rdx
    DECREF_VAL rdi, rsi
    pop rdx
    inc rdx
    jmp .build_set_fixref

.build_set_push:
    mov rax, [rbp-16]
    VPUSH_PTR rax
    leave
    DISPATCH
END_FUNC op_build_set

;; ============================================================================
;; op_set_add - Add TOS to set at stack position
;;
;; SET_ADD (146): used by set comprehensions
;; ecx = position (1-based from TOS before the value to add)
;; Pop TOS (value), add to set.
;; ============================================================================
DEF_FUNC_BARE op_set_add

    ; TOS = value to add
    VPOP rsi                   ; rsi = value
    mov r8, [r13 + 8]         ; r8 = value tag

    ; set is at stack[-(ecx)] after popping (16 bytes/slot)
    neg rcx
    shl rcx, 4                ; -ecx * 16
    mov rdi, [r13 + rcx]      ; rdi = set

    push r8                    ; save value tag (deeper)
    push rsi                   ; save value payload
    call set_add
    ; set_add does INCREF, so DECREF to compensate
    pop rdi                    ; value payload
    pop rsi                    ; value tag
    DECREF_VAL rdi, rsi

    DISPATCH
END_FUNC op_set_add

;; ============================================================================
;; op_set_update - Update set with iterable
;;
;; SET_UPDATE (163): set.update(iterable)
;; ecx = position (set at stack[-(ecx)] after pop)
;; Pop TOS (iterable), add each item to set.
;; ============================================================================
DEF_FUNC op_set_update
    push rbx
    push r14
    sub rsp, 40                ; locals: [rbp-24]=set, [rbp-32]=iterable, [rbp-40]=iter, [rbp-48]=iter_tag

    ; TOS = iterable
    VPOP rsi                   ; rsi = iterable
    mov rax, [r13 + 8]        ; iterable tag
    mov [rbp-48], rax
    mov [rbp-32], rsi          ; save iterable

    ; set is at stack[-(ecx)] after popping (16 bytes/slot)
    neg rcx
    shl rcx, 4                ; -ecx * 16
    mov rdi, [r13 + rcx]      ; rdi = set
    mov [rbp-24], rdi          ; save set

    ; Check if iterable is a set (direct iteration over entries)
    mov rax, [rsi + PyObject.ob_type]
    lea rdx, [rel set_type]
    cmp rax, rdx
    je .su_from_set

    ; Generic approach: get iterator via tp_iter, then loop tp_iternext
    mov rdi, rsi
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .su_type_error
    call rax
    mov [rbp-40], rax          ; save iterator

.su_iter_loop:
    mov rdi, [rbp-40]          ; iterator
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    test rax, rax
    jz .su_iter_done

    ; rax = next item (owned ref)
    push rax                   ; save item
    mov rdi, [rbp-24]          ; set
    mov rsi, rax               ; item
    call set_add               ; set_add does INCREF
    pop rdi                    ; item - DECREF to compensate (set_add INCREF'd)
    DECREF_REG rdi
    jmp .su_iter_loop

.su_iter_done:
    ; DECREF iterator (heap ptr, no tag needed)
    mov rdi, [rbp-40]
    call obj_decref

    ; DECREF iterable (tag-aware)
    mov rdi, [rbp-32]
    mov rsi, [rbp-48]
    DECREF_VAL rdi, rsi

    add rsp, 40
    pop r14
    pop rbx
    leave
    DISPATCH

.su_from_set:
    ; Iterable is a set - iterate entries directly
    mov rax, [rsi + PyDictObject.capacity]
    mov [rbp-40], rax          ; capacity (reuse slot)
    xor ebx, ebx              ; index

.su_set_loop:
    cmp rbx, [rbp-40]
    jge .su_set_done

    mov rax, [rbp-32]         ; source set
    mov rax, [rax + PyDictObject.entries]
    imul rcx, rbx, 24         ; SET_ENTRY_SIZE = 24
    add rax, rcx

    ; Check if entry has a key
    mov rsi, [rax + 8]        ; SET_ENTRY_KEY offset = 8
    test rsi, rsi
    jz .su_set_next

    ; set_add(target_set, key)
    push rbx
    mov rdi, [rbp-24]
    call set_add
    pop rbx

.su_set_next:
    inc rbx
    jmp .su_set_loop

.su_set_done:
    ; DECREF iterable (tag-aware)
    mov rdi, [rbp-32]
    mov rsi, [rbp-48]
    DECREF_VAL rdi, rsi

    add rsp, 40
    pop r14
    pop rbx
    leave
    DISPATCH

.su_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception
END_FUNC op_set_update

;; ============================================================================
;; op_for_iter_range - Specialized range iterator (opcode 214)
;;
;; Guard: TOS ob_type == range_iter_type
;; Inlines range_iter_next logic: decode current/stop/step, check bounds,
;; return SmallInt, advance current.
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
    shl rax, 1
    sar rax, 1                     ; current (decoded)

    mov r8, [rdi + PyRangeIterObject.it_stop]
    shl r8, 1
    sar r8, 1                      ; stop (decoded)

    mov r9, [rdi + PyRangeIterObject.it_step]
    shl r9, 1
    sar r9, 1                      ; step (decoded)

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
    bts rdx, 63                    ; encode return value

    ; Advance: current += step
    add rax, r9
    bts rax, 63
    mov [rdi + PyRangeIterObject.it_current], rax

    VPUSH_INT rdx                  ; push value
    add rbx, 2                     ; skip CACHE
    DISPATCH

.fir_exhausted:
    ; Pop iterator, skip CACHE + jump by (arg + 1)
    ; ecx = saved arg (from instruction word)
    lea rcx, [rcx + 1]            ; arg + 1
    add rbx, 2                     ; skip CACHE
    lea rbx, [rbx + rcx*2]        ; jump forward
    sub r13, 16
    mov rdi, [r13]
    mov rsi, [r13 + 8]
    DECREF_VAL rdi, rsi
    DISPATCH

.fir_deopt:
    ; Type mismatch: rewrite to FOR_ITER (93) and re-execute
    mov byte [rbx - 2], 93
    sub rbx, 2
    DISPATCH
END_FUNC op_for_iter_range

;; ============================================================================
;; op_for_iter_list - Specialized list iterator (opcode 213)
;;
;; Guard: TOS ob_type == list_iter_type
;; Inlines list_iter_next: check index < list.ob_size, load item, INCREF,
;; advance index.
;; ecx = jump offset. Followed by 1 CACHE entry (2 bytes).
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

    ; Get fat item and INCREF (16-byte stride)
    mov rdx, [rax + PyListObject.ob_item]
    shl rcx, 4                    ; index * 16
    mov rax, [rdx + rcx]          ; payload
    mov r8, [rdx + rcx + 8]       ; tag
    INCREF_VAL rax, r8

    ; Advance index
    inc qword [rdi + PyListIterObject.it_index]

    add rsp, 8                     ; discard saved jump offset
    VPUSH_VAL rax, r8              ; push fat value
    add rbx, 2                     ; skip CACHE
    DISPATCH

.fil_exhausted:
    ; Restore the original arg (jump offset)
    pop rcx                        ; restore jump offset
    lea rcx, [rcx + 1]            ; arg + 1
    add rbx, 2                     ; skip CACHE
    lea rbx, [rbx + rcx*2]        ; jump forward
    sub r13, 16
    mov rdi, [r13]
    mov rsi, [r13 + 8]
    DECREF_VAL rdi, rsi
    DISPATCH

.fil_deopt:
    pop rcx                        ; restore jump offset (for re-execute)
    ; Type mismatch: rewrite to FOR_ITER (93) and re-execute
    mov byte [rbx - 2], 93
    sub rbx, 2
    DISPATCH
END_FUNC op_for_iter_list
