; methods_set.asm - set and frozenset methods
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
extern ap_malloc
extern gc_alloc
extern gc_track
extern ap_free
extern ap_realloc
extern ap_memcpy
extern ap_memset
extern ap_memmove
extern ap_strcmp
extern ap_strlen
extern ap_strstr
extern ap_memcmp
extern obj_incref
extern obj_decref
extern obj_dealloc
extern obj_repr
extern obj_str
extern str_from_cstr_heap
extern str_new_heap
extern str_type
extern list_new
extern list_append
extern obj_as_index
extern list_type
extern tuple_new
extern tuple_type
extern dict_new
extern dict_get
extern obj_getattr_opt
extern obj_call_n
extern dict_set
extern dict_del
extern dict_type
extern none_singleton
extern bool_true
extern bool_false
extern int_from_i64
extern eval_exception_unwind
extern obj_richcompare_bool
extern int_to_i64
extern builtin_func_new
extern raise_exception
extern raise_key_error
extern fatal_error
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_IndexError_type
extern exc_KeyError_type
extern int_type
extern set_type
extern object_type
extern object_new_fn
extern staticmethod_type
extern obj_is_true
extern list_sorting_error
extern tuple_type_call

; Set entry layout constants (must match set.asm)
SET_ENTRY_HASH    equ 0
SET_ENTRY_KEY     equ 8
SET_ENTRY_SIZE    equ 16
extern set_add
extern set_contains
extern set_remove
extern set_new
extern set_tp_iter

; --- moved to a sibling file by the split ---
extern add_class_getitem
extern add_method_to_dict
extern add_method_to_dict_checked
extern add_new_staticmethod
extern bytes_method_count
extern bytes_method_endswith
extern bytes_method_find
extern bytes_method_hex
extern bytes_method_join
extern bytes_method_replace
extern bytes_method_split
extern bytes_method_startswith
extern container_dunder_new
extern dict_classmethod_fromkeys
extern dict_dunder_delitem
extern dict_dunder_getitem
extern dict_dunder_setitem
extern dict_method_clear
extern dict_method_copy
extern dict_method_get
extern dict_method_items
extern dict_method_keys
extern dict_method_pop
extern dict_method_popitem
extern dict_method_setdefault
extern dict_method_update
extern dict_method_values
extern du_keys_name
extern empty_str_cstr
extern float_classmethod_fromhex
extern float_method___abs__
extern float_method___float__
extern float_method___int__
extern float_method___trunc__
extern float_method_as_integer_ratio
extern float_method_conjugate
extern float_method_hex
extern float_method_is_integer
extern fm_name_equals
extern fm_resolve_field
extern fmtbuf_append
extern generic_method_contains
extern generic_method_delitem
extern generic_method_getitem
extern generic_method_hash
extern generic_method_setitem
extern int_classmethod_from_bytes
extern int_method___abs__
extern int_method___float__
extern int_method___index__
extern int_method___int__
extern int_method_bit_count
extern int_method_bit_length
extern int_method_conjugate
extern int_method_self_to_i64
extern int_method_to_bytes
extern list_dunder_contains
extern list_dunder_delitem
extern list_dunder_getitem
extern list_dunder_iadd
extern list_dunder_init
extern list_dunder_len
extern list_dunder_setitem
extern list_method_append
extern list_method_clear
extern list_method_copy
extern list_method_count
extern list_method_extend
extern list_method_index
extern list_method_insert
extern list_method_pop
extern list_method_remove
extern list_method_reverse
extern list_method_reversed
extern list_method_sort
extern methods_init
extern mn___abs__
extern mn___add__
extern mn___class_getitem__
extern mn___code__
extern mn___contains__
extern mn___delete__
extern mn___delitem__
extern mn___dir__
extern mn___doc__
extern mn___eq__
extern mn___float__
extern mn___format__
extern mn___ge__
extern mn___get__
extern mn___getitem__
extern mn___globals__
extern mn___gt__
extern mn___hash__
extern mn___iadd__
extern mn___index__
extern mn___init__
extern mn___init_subclass__
extern mn___int__
extern mn___iter__
extern mn___le__
extern mn___len__
extern mn___lt__
extern mn___mul__
extern mn___ne__
extern mn___new__
extern mn___reduce__
extern mn___reduce_ex__
extern mn___repr__
extern mn___reversed__
extern mn___rmul__
extern mn___set__
extern mn___setitem__
extern mn___sizeof__
extern mn___str__
extern mn___trunc__
extern mn_add
extern mn_append
extern mn_as_integer_ratio
extern mn_bit_count
extern mn_bit_length
extern mn_capitalize
extern mn_casefold
extern mn_center
extern mn_clear
extern mn_conjugate
extern mn_copy
extern mn_count
extern mn_decode
extern mn_difference
extern mn_discard
extern mn_encode
extern mn_endswith
extern mn_expandtabs
extern mn_extend
extern mn_find
extern mn_format
extern mn_format_map
extern mn_from_bytes
extern mn_fromhex
extern mn_fromkeys
extern mn_get
extern mn_hex
extern mn_index
extern mn_insert
extern mn_intersection
extern mn_is_integer
extern mn_isalnum
extern mn_isalpha
extern mn_isascii
extern mn_isdecimal
extern mn_isdigit
extern mn_isdisjoint
extern mn_isidentifier
extern mn_islower
extern mn_isnumeric
extern mn_isprintable
extern mn_isspace
extern mn_issubset
extern mn_issuperset
extern mn_istitle
extern mn_isupper
extern mn_items
extern mn_join
extern mn_keys
extern mn_ljust
extern mn_lower
extern mn_lstrip
extern mn_maketrans
extern mn_partition
extern mn_pop
extern mn_popitem
extern mn_remove
extern mn_removeprefix
extern mn_removesuffix
extern mn_replace
extern mn_reverse
extern mn_rfind
extern mn_rindex
extern mn_rjust
extern mn_rpartition
extern mn_rsplit
extern mn_rstrip
extern mn_setdefault
extern mn_sort
extern mn_split
extern mn_splitlines
extern mn_startswith
extern mn_strip
extern mn_swapcase
extern mn_symmetric_difference
extern mn_title
extern mn_to_bytes
extern mn_translate
extern mn_union
extern mn_update
extern mn_upper
extern mn_values
extern mn_zfill
extern object_method_dir
extern object_method_eq
extern object_method_format
extern object_method_hash
extern object_method_init
extern object_method_init_subclass
extern object_method_ne
extern object_method_notimpl
extern object_method_reduce
extern object_method_repr
extern object_method_sizeof
extern object_method_str
extern scalar_dunder_new
extern str_affix_dispatch
extern str_endswith_one
extern str_method_capitalize
extern str_method_casefold
extern str_method_center
extern str_method_count
extern str_method_encode
extern str_method_endswith
extern str_method_expandtabs
extern str_method_find
extern str_method_format
extern str_method_format_map
extern str_method_index
extern str_method_isalnum
extern str_method_isalpha
extern str_method_isascii
extern str_method_isdecimal
extern str_method_isdigit
extern str_method_isidentifier
extern str_method_islower
extern str_method_isprintable
extern str_method_isspace
extern str_method_istitle
extern str_method_isupper
extern str_method_join
extern str_method_ljust
extern str_method_lower
extern str_method_lstrip
extern str_method_partition
extern str_method_removeprefix
extern str_method_removesuffix
extern str_method_replace
extern str_method_rfind
extern str_method_rindex
extern str_method_rjust
extern str_method_rpartition
extern str_method_rsplit
extern str_method_rstrip
extern str_method_split
extern str_method_splitlines
extern str_method_startswith
extern str_method_strip
extern str_method_swapcase
extern str_method_title
extern str_method_translate
extern str_method_upper
extern str_method_zfill
extern str_split_impl
extern str_startswith_one
extern str_staticmethod_maketrans
extern str_strip_impl
extern strip_char_matches
extern tuple_dunder_add
extern tuple_dunder_contains
extern tuple_dunder_getitem
extern tuple_dunder_len
extern tuple_dunder_mul
extern tuple_dunder_rmul
extern tuple_method_count
extern tuple_method_index

section .text

;; ============================================================================
;; set_method_add(args, nargs) -> None
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_add
    cmp rsi, 2
    jne .sma_error

    mov rax, rdi            ; args ptr
    mov rdi, [rax]          ; self (set)
    mov rsi, [rax + 8]     ; elem payload
    call set_add

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sma_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "add() takes exactly one argument"
    call raise_exception
END_FUNC set_method_add

;; ============================================================================
;; set_method_remove(args, nargs) -> None (raises KeyError if missing)
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_remove
    cmp rsi, 2
    jne .smr_error

    mov rax, rdi
    mov rdi, [rax]          ; self
    mov rsi, [rax + 8]     ; elem payload
    call set_remove
    test eax, eax
    jnz .smr_keyerr

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smr_keyerr:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "element not in set"
    call raise_exception

.smr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "remove() takes exactly one argument"
    call raise_exception
END_FUNC set_method_remove

;; ============================================================================
;; set_method_discard(args, nargs) -> None (no error if missing)
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_discard
    cmp rsi, 2
    jne .smd_error

    mov rax, rdi
    mov rdi, [rax]          ; self
    mov rsi, [rax + 8]     ; elem payload
    call set_remove
    ; Ignore return value (don't care if not found)

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smd_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "discard() takes exactly one argument"
    call raise_exception
END_FUNC set_method_discard

;; ============================================================================
;; set_method_pop(args, nargs) -> removed element
;; args[0]=self
;; Scans for first occupied entry, removes and returns it.
;; ============================================================================
SMP_FRAME equ 16    ; save self + entry ptr
DEF_FUNC set_method_pop, SMP_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 1
    jne .smpop_error

    mov rbx, [rdi]          ; self (set)

    ; Check empty
    cmp qword [rbx + PyDictObject.ob_size], 0
    je .smpop_empty

    ; Scan for first non-empty entry
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx            ; index

.smpop_scan:
    cmp rcx, r13
    jge .smpop_empty         ; shouldn't happen

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12             ; entry ptr

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    jne .smpop_found
    inc ecx
    jmp .smpop_scan

.smpop_found:
    ; rax = entry ptr with valid key
    ; Get key (return value) — DON'T incref, we're removing it
    mov rcx, [rax + SET_ENTRY_KEY]        ; key payload
    V_UNPACK rcx, r12

    ; Clear the entry (mark as empty)
    mov qword [rax + SET_ENTRY_KEY], 0
    dec qword [rbx + PyDictObject.ob_size]

    ; Return the key (ownership transfers, no INCREF/DECREF needed)
    mov rax, rcx
    mov edx, r12d
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smpop_empty:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "pop from an empty set"
    call raise_exception

.smpop_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "pop() takes no arguments"
    call raise_exception
END_FUNC set_method_pop

;; ============================================================================
;; set_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
DEF_FUNC set_method_clear
    push rbx
    push r12
    push r13

    cmp rsi, 1
    jne .smc_error

    mov rbx, [rdi]          ; self (set)
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx

.smc_loop:
    cmp rcx, r13
    jge .smc_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx                ; save index

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smc_next

    ; DECREF key
    mov rdi, [rax + SET_ENTRY_KEY]
    V_UNPACK rdi, rsi
    mov qword [rax + SET_ENTRY_KEY], 0
    DECREF_VAL rdi, rsi

.smc_next:
    pop rcx
    inc ecx
    jmp .smc_loop

.smc_done:
    mov qword [rbx + PyDictObject.ob_size], 0

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "clear() takes no arguments"
    call raise_exception
END_FUNC set_method_clear

;; ============================================================================
;; set_method_copy(args, nargs) -> new set (shallow copy)
;; args[0]=self
;; ============================================================================
DEF_FUNC set_method_copy
    push rbx
    push r12
    push r13
    push r14

    cmp rsi, 1
    jne .smcp_error

    mov r14, [rdi]          ; self (source set)

    ; Create new empty set
    call set_new
    mov rbx, rax            ; rbx = new set

    ; Iterate source entries
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smcp_loop:
    cmp rcx, r13
    jge .smcp_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smcp_next

    ; Add key to new set
    mov rdi, rbx            ; new set
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_add

.smcp_next:
    pop rcx
    inc ecx
    jmp .smcp_loop

.smcp_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smcp_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "copy() takes no arguments"
    call raise_exception
END_FUNC set_method_copy

;; ============================================================================
;; set_method_union(args, nargs) -> new set = self | other
;; args[0]=self, args[1]=other (iterable)
;; ============================================================================
DEF_FUNC set_method_union
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smu_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 8]     ; other set

    ; Copy self → new set
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    call set_new
    mov rbx, rax            ; new set
    xor ecx, ecx

.smu_copy_self:
    cmp rcx, r13
    jge .smu_add_other

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smu_cs_next

    mov rdi, rbx
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_add

.smu_cs_next:
    pop rcx
    inc ecx
    jmp .smu_copy_self

.smu_add_other:
    ; Now add all elements from other
    mov r12, [r15 + PyDictObject.entries]
    mov r13, [r15 + PyDictObject.capacity]
    xor ecx, ecx

.smu_add_loop:
    cmp rcx, r13
    jge .smu_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smu_al_next

    mov rdi, rbx
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_add

.smu_al_next:
    pop rcx
    inc ecx
    jmp .smu_add_loop

.smu_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smu_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "union() takes exactly one argument"
    call raise_exception
END_FUNC set_method_union

;; ============================================================================
;; set_method_update(args, nargs) -> None
;; args[0]=self, args[1]=any iterable.  Also serves as set.__init__.
;;
;; This read args[1] as a PyDictObject unconditionally, so s.update([2,3])
;; walked a list's fields as a hash table -- and a set subclass, which fills
;; itself through __init__, crashed on construction from any list.
;; ============================================================================
SU_SELF   equ 8
SU_TMP    equ 16        ; materialised sequence, owned, or 0
SU_FRAME  equ 32

DEF_FUNC set_method_update, SU_FRAME
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov [rbp - SU_SELF], rbx
    mov qword [rbp - SU_TMP], 0
    cmp rsi, 2
    jl .supd_done

    mov r12, [rdi + 8]      ; the source Value
    V_TEST_PTR r12, rax
    ja .supd_materialise
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel set_type]
    cmp rax, rcx
    je .supd_from_set
    extern frozenset_type
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    je .supd_from_set

.supd_materialise:
    ; Any other iterable: materialise it and add the elements one by one.
    lea rsi, [rdi + 8]
    lea rdi, [rel tuple_type]
    mov edx, 1
    call tuple_type_call        ; raises for a non-iterable
    mov [rbp - SU_TMP], rax
    mov r12, rax
    mov r13, [r12 + PyTupleObject.ob_size]
    xor ecx, ecx
.supd_seq_loop:
    cmp rcx, r13
    jge .supd_release
    push rcx
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rsi, [rax + rcx * 8]
    mov rdi, [rbp - SU_SELF]
    call set_add
    pop rcx
    inc rcx
    jmp .supd_seq_loop

.supd_release:
    mov rdi, [rbp - SU_TMP]
    mov qword [rbp - SU_TMP], 0
    call obj_decref
    jmp .supd_done

.supd_from_set:
    mov r13, [r12 + PyDictObject.capacity]
    xor ecx, ecx

.supd_loop:
    cmp rcx, r13
    jge .supd_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, [r12 + PyDictObject.entries]
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .supd_next

    mov rdi, [rbp - SU_SELF]
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_add

.supd_next:
    pop rcx
    inc ecx
    jmp .supd_loop

.supd_done:
    extern none_singleton
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC set_method_update

;; ============================================================================
;; set_method_intersection(args, nargs) -> new set = self & other
;; args[0]=self, args[1]=other
;; ============================================================================
DEF_FUNC set_method_intersection
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smi_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 8]     ; other

    call set_new
    mov rbx, rax            ; new set

    ; Iterate self, add if in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smi_loop:
    cmp rcx, r13
    jge .smi_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smi_next

    ; Check if key is in other
    push rax                ; save entry ptr
    mov rdi, r15            ; other set
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    pop rcx                 ; restore entry ptr (was rax)
    test eax, eax
    jz .smi_next

    ; In both — add to result
    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    call set_add

.smi_next:
    pop rcx
    inc ecx
    jmp .smi_loop

.smi_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smi_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "intersection() takes exactly one argument"
    call raise_exception
END_FUNC set_method_intersection

;; ============================================================================
;; set_method_difference(args, nargs) -> new set = self - other
;; args[0]=self, args[1]=other
;; ============================================================================
DEF_FUNC set_method_difference
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smdf_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 8]     ; other

    call set_new
    mov rbx, rax            ; new set

    ; Iterate self, add if NOT in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smdf_loop:
    cmp rcx, r13
    jge .smdf_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smdf_next

    ; Check if key is in other
    push rax
    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    pop rcx                 ; entry ptr
    test eax, eax
    jnz .smdf_next          ; in other — skip

    ; NOT in other — add to result
    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    call set_add

.smdf_next:
    pop rcx
    inc ecx
    jmp .smdf_loop

.smdf_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smdf_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "difference() takes exactly one argument"
    call raise_exception
END_FUNC set_method_difference

;; ============================================================================
;; set_method_symmetric_difference(args, nargs) -> new set = self ^ other
;; args[0]=self, args[1]=other
;; ============================================================================
DEF_FUNC set_method_symmetric_difference
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smsd_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 8]     ; other

    call set_new
    mov rbx, rax            ; new set

    ; Add elements in self but NOT in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smsd_self_loop:
    cmp rcx, r13
    jge .smsd_other

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smsd_s_next

    push rax
    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    pop rcx
    test eax, eax
    jnz .smsd_s_next        ; in other, skip

    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    call set_add

.smsd_s_next:
    pop rcx
    inc ecx
    jmp .smsd_self_loop

.smsd_other:
    ; Add elements in other but NOT in self
    mov r12, [r15 + PyDictObject.entries]
    mov r13, [r15 + PyDictObject.capacity]
    xor ecx, ecx

.smsd_other_loop:
    cmp rcx, r13
    jge .smsd_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smsd_o_next

    push rax
    mov rdi, r14
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    pop rcx
    test eax, eax
    jnz .smsd_o_next        ; in self, skip

    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    call set_add

.smsd_o_next:
    pop rcx
    inc ecx
    jmp .smsd_other_loop

.smsd_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smsd_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "symmetric_difference() takes exactly one argument"
    call raise_exception
END_FUNC set_method_symmetric_difference

;; ============================================================================
;; set_method_issubset(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if every element of self is in other.
;; ============================================================================
DEF_FUNC set_method_issubset
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smss_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 8]     ; other

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smss_loop:
    cmp rcx, r13
    jge .smss_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smss_next

    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    test eax, eax
    jz .smss_false          ; not in other

.smss_next:
    pop rcx
    inc ecx
    jmp .smss_loop

.smss_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smss_false:
    pop rcx                 ; balance the push in loop
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smss_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "issubset() takes exactly one argument"
    call raise_exception
END_FUNC set_method_issubset

;; ============================================================================
;; set_method_issuperset(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if every element of other is in self.
;; ============================================================================
DEF_FUNC set_method_issuperset
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smis_error

    mov r14, [rdi + 8]     ; other (iterate this)
    mov r15, [rdi]          ; self (check contains)

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smis_loop:
    cmp rcx, r13
    jge .smis_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smis_next

    mov rdi, r15            ; check in self
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    test eax, eax
    jz .smis_false

.smis_next:
    pop rcx
    inc ecx
    jmp .smis_loop

.smis_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smis_false:
    pop rcx
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smis_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "issuperset() takes exactly one argument"
    call raise_exception
END_FUNC set_method_issuperset

;; ============================================================================
;; set_method_isdisjoint(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if self and other have no common elements.
;; ============================================================================
DEF_FUNC set_method_isdisjoint
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smdj_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 8]     ; other

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smdj_loop:
    cmp rcx, r13
    jge .smdj_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smdj_next

    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    test eax, eax
    jnz .smdj_false         ; found in other — not disjoint

.smdj_next:
    pop rcx
    inc ecx
    jmp .smdj_loop

.smdj_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smdj_false:
    pop rcx
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smdj_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "isdisjoint() takes exactly one argument"
    call raise_exception
END_FUNC set_method_isdisjoint
