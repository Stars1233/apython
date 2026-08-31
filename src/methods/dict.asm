; methods/dict.asm - dict methods
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

; External functions
extern ap_memset
extern obj_decref
extern str_from_cstr_heap
extern tuple_new
extern tuple_type
extern dict_new
extern dict_get
extern obj_getattr_opt
extern obj_call_n
extern dict_set
extern dict_del
extern none_singleton
extern raise_exception
extern raise_key_error
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_KeyError_type
extern kw_names_pending
extern tuple_type_call
extern dict_type
extern obj_dealloc

; Set entry layout constants (must match set.asm)

; --- moved to a sibling file by the split ---

section .text

;; ############################################################################
;;                         DICT METHODS
;; ############################################################################

;; ============================================================================
;; dict_method_get(args, nargs) -> value or None
;; args[0]=self, args[1]=key, optionally args[2]=default
;; ============================================================================
DEF_FUNC dict_method_get
    push rbx
    push r12

    mov rax, rdi            ; args
    mov rbx, [rax]          ; self (dict)
    mov r12, rsi            ; nargs
    push rax

    ; dict_get(self, key)
    mov rdi, rbx
    mov rsi, [rax + 8]      ; key Value -- dict_get unpacks it itself, so
    call dict_get           ; decoding here would hand it a bare payload
    V_UNPACK rax, rdx           ; dict_get returns a Value

    test edx, edx               ; the tag, not the payload: a hit may be int 0
    jnz .dg_found

    ; Not found - return default or None
    pop rcx                 ; args
    cmp r12, 3
    jl .dg_ret_none
    ; Return args[2] (default)
    mov rax, [rcx + 16]     ; default payload
    V_UNPACK rax, rdx       ; args[2]
    INCREF_VAL rax, rdx
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.dg_ret_none:
    RET_NONE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.dg_found:
    add rsp, 8              ; discard saved args
    ; INCREF the value (dict_get returns borrowed ref, rdx=tag)
    INCREF_VAL rax, rdx
    ; rdx already has correct tag from dict_get
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_method_get

;; ============================================================================
;; dict_method_keys(args, nargs) -> dict_keys view
;; args[0]=self
;; ============================================================================
extern dict_view_new
DEF_FUNC dict_method_keys
    mov rdi, [rdi]          ; self (dict)
    xor esi, esi            ; kind=0 (keys)
    extern dict_keys_view_type
    lea rdx, [rel dict_keys_view_type]
    call dict_view_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_method_keys

;; ============================================================================
;; dict_method_values(args, nargs) -> dict_values view
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_values
    mov rdi, [rdi]          ; self (dict)
    mov esi, 1              ; kind=1 (values)
    extern dict_values_view_type
    lea rdx, [rel dict_values_view_type]
    call dict_view_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_method_values

;; ============================================================================
;; dict_method_items(args, nargs) -> dict_items view
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_items
    mov rdi, [rdi]          ; self (dict)
    mov esi, 2              ; kind=2 (items)
    extern dict_items_view_type
    lea rdx, [rel dict_items_view_type]
    call dict_view_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_method_items

;; ============================================================================
;; dict_method_pop(args, nargs) -> value
;; args[0]=self, args[1]=key, optionally args[2]=default
;; ============================================================================
DEF_FUNC dict_method_pop
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r14, rdi            ; r14 = args
    mov rbx, [r14]          ; self
    mov r12, rsi            ; nargs
    mov r13, [r14 + 8]     ; key payload (16-byte stride)
    V_UNPACK r13, r15       ; args[1]

    ; Try dict_get
    mov rdi, rbx
    mov rsi, r13
    mov edx, r15d           ; key tag
    V_PACK rsi, rdx           ; dict_get/del take a key Value
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jz .dpop2_not_found

    ; dict_get returns fat (rax=payload, rdx=tag)
    INCREF_VAL rax, rdx
    push rdx                ; save tag across dict_del
    push rax                ; save payload

    mov rdi, rbx
    mov rsi, r13
    mov rdx, r15            ; key tag
    V_PACK rsi, rdx           ; dict_get/del take a key Value
    call dict_del

    pop rax                 ; restore payload
    pop rdx                 ; restore tag
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.dpop2_not_found:
    cmp r12, 3
    jl .dpop2_error
    mov rax, [r14 + 16]     ; default = args[2] payload (16-byte stride)
    V_UNPACK rax, rdx       ; args[2]
    INCREF_VAL rax, rdx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.dpop2_error:
    mov rdi, [r14 + 8]         ; the key Value, still in the argument array
    call raise_key_error
END_FUNC dict_method_pop

;; ============================================================================
;; dict_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_clear
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    ; DECREF all keys and values
    mov r12, [rbx + PyDictObject.capacity]
    xor r13d, r13d

.dc_loop:
    cmp r13, r12
    jge .dc_clear_entries

    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r13, DICT_ENTRY_SIZE
    lea r14, [rax + rcx]    ; r14 = entry ptr

    mov rdi, [r14 + DictEntry.key]
    V_UNPACK rdi, rsi
    test rdi, rdi
    jz .dc_next

    ; DECREF key (tag-aware)
    DECREF_VAL rdi, rsi

    ; DECREF value (tag-aware)
    mov rdi, [r14 + DictEntry.value]
    V_UNPACK rdi, rsi
    DECREF_VAL rdi, rsi

.dc_next:
    inc r13
    jmp .dc_loop

.dc_clear_entries:
    ; Zero out all entries
    mov rdi, [rbx + PyDictObject.entries]
    xor esi, esi
    imul rdx, r12, DICT_ENTRY_SIZE
    call ap_memset

    ; And reset the sparse index array, or every slot would still point at a
    ; dense entry that is now blank.
    mov rdi, [rbx + PyDictObject.dk_indices]
    test rdi, rdi
    jz .dc_no_indices
    mov rcx, r12
    mov rax, DICT_IX_EMPTY
    rep stosq
.dc_no_indices:

    ; Reset size to 0
    mov qword [rbx + PyDictObject.ob_size], 0
    mov qword [rbx + PyDictObject.dk_nentries], 0
    mov qword [rbx + PyDictObject.dk_tombstones], 0
    inc qword [rbx + PyDictObject.dk_version]

    RET_NONE
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_method_clear

;; ============================================================================
;; dict_method_update(args, nargs) -> None
;; args[0]=self; then either a mapping, or an iterable of key/value pairs,
;; and/or keyword arguments.  All three forms are ordinary Python; the old
;; code read args[1] as a PyDictObject unconditionally, so d.update(5)
;; dereferenced the payload, d.update([("a",1)]) read a list's fields as a
;; dict's, and d.update(a=1) treated the keyword's value as the mapping.
;; ============================================================================
DU_ARGS   equ 8
DU_SELF   equ 16
DU_NKW    equ 24
DU_NPOS   equ 32
DU_TMP    equ 40        ; materialised sequence, owned, or 0
DU_PAIRV  equ 48        ; scratch Value, so &it can be passed as an args array
DU_PAIR   equ 56        ; materialised pair, owned, or 0
DU_KWNAMES equ 64       ; the consumed kw_names_pending tuple, borrowed
DU_OTHER  equ 72        ; the positional argument, borrowed
DU_FRAME  equ 96

DEF_FUNC dict_method_update, DU_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]                  ; self
    mov [rbp - DU_ARGS], rdi
    mov [rbp - DU_SELF], rbx
    mov qword [rbp - DU_TMP], 0
    mov qword [rbp - DU_PAIR], 0

    ; Keyword arguments occupy the last n_kw slots and are named by
    ; kw_names_pending.  Consume it here so nothing downstream sees it.
    xor eax, eax
    mov [rbp - DU_KWNAMES], rax
    mov rcx, [rel kw_names_pending]
    test rcx, rcx
    jz .du_have_nkw
    mov [rbp - DU_KWNAMES], rcx
    mov rax, [rcx + PyTupleObject.ob_size]
    mov qword [rel kw_names_pending], 0
.du_have_nkw:
    mov [rbp - DU_NKW], rax
    sub rsi, rax
    mov [rbp - DU_NPOS], rsi        ; positional count, self included

    cmp rsi, 2
    jg .du_too_many
    jl .du_kwargs                   ; self only: nothing positional to merge

    ; ---- positional argument: a mapping, or an iterable of pairs ----------
    mov rdi, [rbp - DU_ARGS]
    mov r12, [rdi + 8]
    V_TEST_PTR r12, rax
    ja .du_from_pairs               ; an immediate is not a mapping; let the
                                    ; iterator protocol produce the TypeError
    mov rax, [r12 + PyObject.ob_type]
    REQUIRE_DICT_TYPE rax, rcx, .du_from_pairs

    ; ---- other is a dict: walk its entry table ----------------------------
    mov r13, [r12 + PyDictObject.capacity]
    xor r14d, r14d
.du_loop:
    cmp r14, r13
    jge .du_kwargs

    mov rax, [r12 + PyDictObject.entries]
    imul rcx, r14, DICT_ENTRY_SIZE
    add rax, rcx

    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .du_next

    mov rdx, [rax + DictEntry.value]
    mov rsi, rdi                    ; key Value
    mov rdi, rbx                    ; self
    call dict_set

.du_next:
    inc r14
    jmp .du_loop

    ; ---- other is an iterable of (key, value) pairs -----------------------
.du_from_pairs:
    ; A mapping is anything with a keys() method -- that one test is how
    ; CPython tells the two shapes apart -- and it is read through keys() and
    ; indexing rather than as a sequence of pairs.  Without this, updating from
    ; a type's __dict__, which is a mappingproxy, reported that its elements
    ; were not two long.  enum builds its classes that way.
    mov [rbp - DU_OTHER], r12
    V_TEST_PTR r12, rax
    ja .du_as_pairs
    test r12, r12
    jz .du_as_pairs
    lea rdi, [rel du_keys_name]
    call str_from_cstr_heap
    test rax, rax
    jz .du_as_pairs
    mov r13, rax
    mov rdi, [rbp - DU_OTHER]
    mov rsi, r13
    call obj_getattr_opt
    mov [rbp - DU_PAIR], rax            ; the bound keys(), owned, or 0
    mov rdi, r13
    call obj_decref
    cmp qword [rbp - DU_PAIR], 0
    je .du_as_pairs

    xor esi, esi
    mov rdi, [rbp - DU_PAIR]
    mov edx, 0
    call obj_call_n
    mov r14, rax
    mov rdi, [rbp - DU_PAIR]
    mov qword [rbp - DU_PAIR], 0
    call obj_decref
    test r14, r14
    jz .du_propagate                    ; keys() raised; it is already pending

    ; Materialise the key sequence, so any iterable of keys is accepted.
    mov [rbp - DU_PAIRV], r14
    lea rsi, [rbp - DU_PAIRV]
    lea rdi, [rel tuple_type]
    mov edx, 1
    call tuple_type_call
    mov [rbp - DU_TMP], rax
    mov rdi, r14
    call obj_decref
    mov r12, [rbp - DU_TMP]
    test r12, r12
    jz .du_propagate                    ; keys() was not iterable
    mov r13, [r12 + PyTupleObject.ob_size]
    xor r14d, r14d
.du_key_loop:
    cmp r14, r13
    jge .du_keys_done
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rsi, [rax + r14 * 8]            ; the key Value
    mov rdi, [rbp - DU_OTHER]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .du_not_mapping
    mov rax, [rax + PyMappingMethods.mp_subscript]
    test rax, rax
    jz .du_not_mapping
    call rax                            ; other[key] -> Value
    test rax, rax
    jz .du_propagate                    ; the lookup raised
    mov [rbp - DU_PAIRV], rax
    mov rdx, rax
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rsi, [rax + r14 * 8]            ; the key again
    mov rdi, [rbp - DU_SELF]
    call dict_set
    mov rdi, [rbp - DU_PAIRV]
    DECREF_V rdi, rsi
    inc r14
    jmp .du_key_loop
.du_not_mapping:
    ; raise_exception does not return -- it jumps into the unwinder -- so
    ; anything this frame owns has to go first.
    call .du_release
    RAISE exc_TypeError_type, "object is not subscriptable"

.du_propagate:
    ; Something we called raised, and the exception is already pending.
    ; Falling through to the success tail returned None and left it to surface
    ; at whatever ran next, with the dict half updated.
    call .du_release
    RET_NULL
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; Local: drop the owned temporaries, whichever of them are live.
.du_release:
    mov rdi, [rbp - DU_TMP]
    test rdi, rdi
    jz .du_rel_pair
    mov qword [rbp - DU_TMP], 0
    call obj_decref
.du_rel_pair:
    mov rdi, [rbp - DU_PAIR]
    test rdi, rdi
    jz .du_rel_done
    mov qword [rbp - DU_PAIR], 0
    call obj_decref
.du_rel_done:
    ret

.du_keys_done:
    mov rdi, [rbp - DU_TMP]
    mov qword [rbp - DU_TMP], 0
    call obj_decref
    mov rbx, [rbp - DU_SELF]
    jmp .du_kwargs

.du_as_pairs:
    mov r12, [rbp - DU_OTHER]
    mov rdi, [rbp - DU_ARGS]
    lea rsi, [rdi + 8]
    lea rdi, [rel tuple_type]
    mov edx, 1
    call tuple_type_call            ; raises for a non-iterable
    mov [rbp - DU_TMP], rax
    mov r12, rax
    ; ...but an iterable whose __next__ raises returns NULL rather than
    ; raising from here, and reading ob_size off that dereferences 0.
    test rax, rax
    jz .du_propagate
    mov r13, [r12 + PyTupleObject.ob_size]
    xor r14d, r14d
.du_pair_loop:
    cmp r14, r13
    jge .du_pairs_done
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rax, [rax + r14 * 8]
    mov [rbp - DU_PAIRV], rax
    ; Materialise the pair too, so any two-element iterable is accepted.
    lea rsi, [rbp - DU_PAIRV]
    lea rdi, [rel tuple_type]
    mov edx, 1
    call tuple_type_call
    mov [rbp - DU_PAIR], rax
    test rax, rax
    jz .du_propagate
    cmp qword [rax + PyTupleObject.ob_size], 2
    jne .du_bad_pair
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rsi, [rcx]                  ; key Value
    mov rdx, [rcx + 8]              ; value Value
    mov rdi, [rbp - DU_SELF]
    call dict_set
    mov rdi, [rbp - DU_PAIR]
    mov qword [rbp - DU_PAIR], 0
    call obj_decref
    inc r14
    jmp .du_pair_loop
.du_pairs_done:
    mov rdi, [rbp - DU_TMP]
    mov qword [rbp - DU_TMP], 0
    call obj_decref
    mov rbx, [rbp - DU_SELF]

    ; ---- keyword arguments -------------------------------------------------
.du_kwargs:
    mov r13, [rbp - DU_NKW]
    test r13, r13
    jz .du_done
    mov r12, [rbp - DU_KWNAMES]
    xor r14d, r14d
.du_kw_loop:
    cmp r14, r13
    jge .du_done
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rsi, [rax + r14 * 8]        ; keyword name str, already a Value
    mov rax, [rbp - DU_NPOS]
    add rax, r14                    ; value slot = n_pos + kw index
    mov rcx, [rbp - DU_ARGS]
    mov rdx, [rcx + rax * 8]        ; value Value
    mov rdi, [rbp - DU_SELF]
    call dict_set
    inc r14
    jmp .du_kw_loop

.du_done:
    RET_NONE
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.du_bad_pair:
    call .du_release
    RAISE exc_ValueError_type, "dictionary update sequence element has length != 2"

.du_too_many:
    RAISE exc_TypeError_type, "update expected at most 1 argument"
END_FUNC dict_method_update

;; ============================================================================
;; dict_method_setdefault(args, nargs) -> value
;; args[0]=self, args[1]=key, args[2]=default (optional, default=None)
;; ============================================================================
DEF_FUNC dict_method_setdefault
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, [rdi]          ; self (dict)
    mov r12, [rdi + 8]     ; key payload
    V_UNPACK r12, r14       ; args[1]
    mov r13, rsi            ; nargs

    ; Save args ptr for default value access
    push rdi

    ; dict_get(self, key)
    mov rdi, rbx
    mov rsi, r12
    mov edx, r14d           ; key tag
    V_PACK rsi, rdx           ; dict_get/del take a key Value
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value

    test edx, edx               ; the tag, not the payload: a hit may be int 0
    jnz .sd_found

    ; Not found - determine default value
    pop rdi                 ; restore args ptr
    cmp r13, 3
    jl .sd_use_none
    mov r13, [rdi + 16]     ; default = args[2] payload
    V_UNPACK r13, r15       ; args[2]
    jmp .sd_set_default

.sd_use_none:
    lea r13, [rel none_singleton]
    mov r15d, TAG_PTR

.sd_set_default:
    ; dict_set(self, key, default_val)
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov ecx, r15d           ; default val tag
    V_PACK rdx, rcx
    mov r8d, r14d           ; key tag
    V_PACK rsi, r8
    call dict_set

    ; INCREF and return default_val
    INCREF_VAL r13, r15
    mov rax, r13
    mov edx, r15d           ; return tag
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sd_found:
    add rsp, 8              ; discard saved args ptr
    ; INCREF the found value (dict_get returns borrowed ref, rdx=tag)
    INCREF_VAL rax, rdx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_method_setdefault

;; ============================================================================
;; dict_method_copy(args, nargs) -> new dict (shallow copy)
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_copy
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    ; Create new dict
    call dict_new
    mov r12, rax            ; r12 = new dict

    ; Iterate over self's entries
    mov r13, [rbx + PyDictObject.capacity]
    xor r14d, r14d          ; index

.dcopy_loop:
    cmp r14, r13
    jge .dcopy_done

    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r14, DICT_ENTRY_SIZE
    add rax, rcx

    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .dcopy_next

    ; dict_set(new_dict, key, value, value_tag, key_tag)
    push r14
    mov rdx, [rax + DictEntry.value]
    mov rsi, rdi            ; key
    mov rdi, r12            ; new dict
    call dict_set
    pop r14

.dcopy_next:
    inc r14
    jmp .dcopy_loop

.dcopy_done:
    mov rax, r12
    mov edx, TAG_PTR         ; dict is heap ptr
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_method_copy

;; ============================================================================
;; dict_classmethod_fromkeys(args, nargs) -> new dict
;; args[0]=cls (type), args[1]=iterable, optional args[2]=value (default None)
;; Creates dict from iterable keys with given value.
;; ============================================================================
DFK_ITER  equ 8
DFK_DICT  equ 16
DFK_VAL   equ 24
DFK_VTAG  equ 32
DFK_FRAME equ 40

DEF_FUNC dict_classmethod_fromkeys, DFK_FRAME
    push rbx
    push r12
    push r13

    ; Default value = None
    extern none_singleton
    lea rax, [rel none_singleton]
    mov [rbp - DFK_VAL], rax
    mov qword [rbp - DFK_VTAG], TAG_PTR

    ; If nargs >= 3, use args[2] as value
    cmp rsi, 3
    jl .dfk_get_iter
    mov rax, [rdi + 16]            ; value payload
    V_UNPACK rax, rcx       ; args[2]
    mov [rbp - DFK_VAL], rax
    mov [rbp - DFK_VTAG], rcx

.dfk_get_iter:
    ; Get iterator from args[1] (iterable)
    ; args array: [0]=cls, [8]=cls_tag, [16]=iterable, [24]=iterable_tag, ...
    mov rax, rdi                   ; save args ptr
    mov rdi, [rax + 8]            ; iterable payload
    V_UNPACK rdi, rsi       ; args[1]
    extern get_iterator
    call get_iterator
    mov [rbp - DFK_ITER], rax

    ; Create new dict
    call dict_new
    mov [rbp - DFK_DICT], rax

.dfk_loop:
    ; Get next key from iterator
    mov rdi, [rbp - DFK_ITER]
    extern call_iternext
    call call_iternext
    V_UNPACK rax, rdx           ; call_iternext returns a Value
    test edx, edx
    jz .dfk_done                   ; iterator exhausted

    ; rax=key payload, rdx=key tag
    ; Save key before loading value (which overwrites rdx)
    mov rsi, rax                   ; key payload
    mov r8, rdx                    ; key tag
    V_PACK rsi, r8

    ; dict_set(dict, key Value, value Value)
    mov rdi, [rbp - DFK_DICT]
    mov rdx, [rbp - DFK_VAL]       ; value payload
    mov rcx, [rbp - DFK_VTAG]      ; value tag
    V_PACK rdx, rcx
    call dict_set

    jmp .dfk_loop

.dfk_done:
    ; DECREF iterator
    mov rdi, [rbp - DFK_ITER]
    call obj_decref

    mov rax, [rbp - DFK_DICT]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_classmethod_fromkeys

;; ============================================================================
;; dict_method_popitem(args, nargs) -> (key, value) tuple
;; args[0]=self. Removes and returns last inserted item.
;; ============================================================================
DEF_FUNC dict_method_popitem
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    ; Check if dict is empty
    cmp qword [rbx + PyDictObject.ob_size], 0
    je .dpopitem_empty

    ; Find last non-NULL entry by scanning backward
    mov r12, [rbx + PyDictObject.capacity]
    dec r12                  ; start from capacity-1

.dpopitem_scan:
    cmp r12, 0
    jl .dpopitem_empty       ; shouldn't happen, but safety
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r12, DICT_ENTRY_SIZE
    add rax, rcx

    mov r13, [rax + DictEntry.key]
    test r13, r13
    jz .dpopitem_prev           ; a NULL key is an empty slot or a tombstone
    ; A second test of rcx used to stand here, left over from a removed
    ; key-tag load; rcx now holds the byte offset, which is 0 at slot 0, so
    ; an occupied slot 0 was skipped and popitem() reported an empty dict.
    mov r14, [rax + DictEntry.value]
    V_UNPACK r14, rcx
    jmp .dpopitem_found

.dpopitem_prev:
    dec r12
    jmp .dpopitem_scan

.dpopitem_found:
    ; r13 = key, r14 = value, rcx = value_tag
    ; Also save key_tag from the entry
    mov rax, [rbx + PyDictObject.entries]
    imul rdx, r12, DICT_ENTRY_SIZE
    add rax, rdx
    V_TAG_OF r8, qword [rax + DictEntry.key]
    V_UNPACK r13, r8         ; r13 held the key as a Value
    push r8                  ; save key_tag
    push rcx                 ; save value_tag across tuple_new
    ; Create 2-tuple
    mov rdi, 2
    call tuple_new
    pop rcx                  ; restore value_tag
    pop r8                   ; restore key_tag
    mov r12, rax             ; r12 = tuple

    ; Set tuple[0] = key with correct tag, tuple[1] = value
    mov r9, [r12 + PyTupleObject.ob_item]
    INCREF_VAL r13, r8
    INCREF_VAL r14, rcx
    mov r10, r13
    mov r11, r8
    V_PACK r10, r11
    mov [r9], r10
    mov r10, r14
    mov r11, rcx
    V_PACK r10, r11
    mov [r9 + 8], r10

    ; Delete key from dict
    mov rdi, rbx
    mov rsi, r13
    mov edx, r8d            ; key tag from the entry
    V_PACK rsi, rdx           ; dict_get/del take a key Value
    call dict_del

    mov rax, r12
    mov edx, TAG_PTR         ; tuple is heap ptr
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.dpopitem_empty:
    RAISE exc_KeyError_type, "dictionary is empty"
END_FUNC dict_method_popitem

section .rodata
du_keys_name:   db "keys", 0
