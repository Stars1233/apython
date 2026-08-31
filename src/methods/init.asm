; methods/init.asm - Registering every builtin type's methods
;
; methods_init walks one block per type, each building a dict and hanging it
; off that type's tp_dict.  The four add_* helpers and the mn_* name strings
; are here and stay file-local; the methods themselves live in methods_*.asm.
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
extern gc_alloc
extern gc_track
extern obj_incref
extern obj_decref
extern str_from_cstr_heap
extern str_type
extern list_type
extern tuple_type
extern dict_new
extern dict_set
extern dict_type
extern none_singleton
extern builtin_func_new
extern int_type
extern set_type
extern object_type
extern object_new_fn
extern staticmethod_type
extern bytes_dunder_iter
extern bytes_dunder_len
extern bytes_dunder_repr
extern bytes_dunder_str
extern bytes_type
extern classmethod_type
extern dict_dunder_iter
extern dict_dunder_len
extern float_dunder_repr
extern float_type
extern frozenset_type
extern int_dunder_repr
extern list_dunder_iter
extern set_dunder_iter
extern set_dunder_len
extern str_dunder_iter
extern str_dunder_len
extern str_dunder_repr
extern str_dunder_str
extern tuple_dunder_iter

; Set entry layout constants (must match set.asm)
SET_ENTRY_HASH    equ 0
SET_ENTRY_KEY     equ 8
SET_ENTRY_SIZE    equ 16

; --- moved to a sibling file by the split ---
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
extern float_classmethod_fromhex
extern float_method_as_integer_ratio
extern float_method_conjugate
extern float_method_hex
extern float_method_is_integer
extern generic_method_contains
extern generic_method_hash
extern int_classmethod_from_bytes
extern int_method_bit_count
extern int_method_bit_length
extern int_method_conjugate
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
extern object_method_dir
extern object_method_eq
extern object_method_format
extern object_method_hash
extern object_method_init
extern object_method_init_subclass
extern object_method_ne
extern object_method_reduce
extern object_method_repr
extern object_method_sizeof
extern object_method_str
extern scalar_dunder_new
extern set_method_add
extern set_method_clear
extern set_method_copy
extern set_method_difference
extern set_method_discard
extern set_method_intersection
extern set_method_isdisjoint
extern set_method_issubset
extern set_method_issuperset
extern set_method_pop
extern set_method_remove
extern set_method_symmetric_difference
extern set_method_union
extern set_method_update
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
extern str_staticmethod_maketrans
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
;; HELPER: add_method_to_dict(dict, name_cstr, func_ptr)
;; rdi=dict, rsi=name_cstr, rdx=func_ptr
;; Creates a builtin func wrapper and stores it in the dict.
;; ============================================================================
DEF_FUNC_LOCAL add_method_to_dict
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; dict
    mov r12, rsi            ; name_cstr
    mov r13, rdx            ; func_ptr

    ; Create builtin func wrapper: builtin_func_new(func_ptr, name_cstr)
    mov rdi, r13
    mov rsi, r12
    call builtin_func_new
    push rax                ; save func obj

    ; Create key string from name
    mov rdi, r12
    call str_from_cstr_heap
    push rax                ; save key str

    ; dict_set(dict, key, func_obj)
    mov rdi, rbx
    mov rsi, rax            ; key
    mov rdx, [rsp + 8]     ; func obj
    call dict_set

    ; DECREF key (dict_set did INCREF)
    pop rdi
    call obj_decref

    ; DECREF func obj (dict_set did INCREF)
    pop rdi
    call obj_decref

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC add_method_to_dict

;; ============================================================================
;; HELPER: add_method_to_dict_checked(dict, name_cstr, func_ptr, min_args, max_args)
;; rdi=dict, rsi=name_cstr, rdx=func_ptr, rcx=min_args, r8=max_args
;; Like add_method_to_dict but sets arg count bounds.
;; ============================================================================
extern builtin_func_new_checked
DEF_FUNC_LOCAL add_method_to_dict_checked
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; dict
    mov r12, rsi            ; name_cstr

    ; Create builtin func wrapper with bounds
    mov rdi, rdx            ; func_ptr
    mov rsi, r12            ; name_cstr
    ; rdx = min_args (already in rcx from caller)
    mov rdx, rcx
    ; rcx = max_args (from r8)
    mov rcx, r8
    call builtin_func_new_checked
    push rax                ; save func obj

    ; Create key string from name
    mov rdi, r12
    call str_from_cstr_heap
    push rax                ; save key str

    ; dict_set(dict, key, func_obj)
    mov rdi, rbx
    mov rsi, rax            ; key
    mov rdx, [rsp + 8]     ; func obj
    call dict_set

    ; DECREF key (dict_set did INCREF)
    pop rdi
    call obj_decref

    ; DECREF func obj (dict_set did INCREF)
    pop rdi
    call obj_decref

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC add_method_to_dict_checked

;; add_new_staticmethod(rdi = type dict, rsi = function) -- register `function`
;; as the type's __new__, wrapped so it is not bound to the instance.
extern staticmethod_construct
extern staticmethod_type
DEF_FUNC_LOCAL add_new_staticmethod
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r13, rsi                ; the function to register

    ; Build the plain builtin-function object first.
    sub rsp, 16
    lea rdi, [rel mn___new__]
    call str_from_cstr_heap
    mov r12, rax                ; the name, ours

    mov rdi, r13                            ; func ptr
    lea rsi, [rel mn___new__]               ; name
    mov edx, 1                              ; min args (cls)
    mov rcx, -1                             ; no maximum
    call builtin_func_new_checked
    mov [rsp], rax              ; args[0] for staticmethod()

    lea rdi, [rel staticmethod_type]
    mov rsi, rsp
    mov edx, 1
    call staticmethod_construct
    push rax

    mov rdi, [rsp + 8]          ; the raw function
    call obj_decref
    pop rdx                     ; the staticmethod wrapper
    push rdx
    mov rdi, rbx                ; the type dict
    mov rsi, r12                ; name
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, r12
    call obj_decref
    add rsp, 16

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC add_new_staticmethod

;; add_class_getitem(rdi = type dict)
;; PEP 585: list[int] and friends.  The __class_getitem__ path in
;; op_binary_subscr is already wired for type objects; what was missing was an
;; entry to find.  _collections_abc takes GenericAlias from `type(list[int])`.
DEF_FUNC_LOCAL add_class_getitem
    push rbx
    push r12
    mov rbx, rdi
    extern generic_alias_class_getitem
    lea rdi, [rel generic_alias_class_getitem]
    lea rsi, [rel mn___class_getitem__]
    call builtin_func_new
    push rax
    mov edi, PyClassMethodObject_size
    lea rsi, [rel classmethod_type]
    call gc_alloc
    pop rcx
    mov [rax + PyClassMethodObject.cm_callable], rcx
    mov r12, rax
    mov rdi, rax
    call gc_track
    lea rdi, [rel mn___class_getitem__]
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, rax
    mov rdx, r12
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, r12
    call obj_decref
    pop r12
    pop rbx
    leave
    ret
END_FUNC add_class_getitem


;; ############################################################################
;;                       METHODS_INIT
;; ############################################################################

;; ============================================================================
;; methods_init()
;; Populate tp_dict for str_type, list_type, dict_type
;; ============================================================================
DEF_FUNC methods_init
    push rbx
    push r12

    ;; --- str methods ---
    call dict_new
    mov rbx, rax            ; rbx = str method dict

    ; str's own __str__ and __repr__, by name.  See DEF_DUNDER_STRREPR.
    mov rdi, rbx
    lea rsi, [rel mn___str__]
    lea rdx, [rel str_dunder_str]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel str_dunder_repr]
    call add_method_to_dict

    ; int.__new__ / str.__new__: enum builds each member with
    ; `member_type.__new__(cls, *args)`, and decides which base is the data
    ; type by asking whether __new__ is in its __dict__.
    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    lea rsi, [rel mn_upper]
    lea rdx, [rel str_method_upper]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_lower]
    lea rdx, [rel str_method_lower]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_strip]
    lea rdx, [rel str_method_strip]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_startswith]
    lea rdx, [rel str_method_startswith]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_endswith]
    lea rdx, [rel str_method_endswith]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_find]
    lea rdx, [rel str_method_find]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_replace]
    lea rdx, [rel str_method_replace]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_join]
    lea rdx, [rel str_method_join]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_split]
    lea rdx, [rel str_method_split]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_format]
    lea rdx, [rel str_method_format]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_lstrip]
    lea rdx, [rel str_method_lstrip]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_rstrip]
    lea rdx, [rel str_method_rstrip]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel str_method_count]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_index]
    lea rdx, [rel str_method_index]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_rfind]
    lea rdx, [rel str_method_rfind]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isdigit]
    lea rdx, [rel str_method_isdigit]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isalpha]
    lea rdx, [rel str_method_isalpha]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isidentifier]
    lea rdx, [rel str_method_isidentifier]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isprintable]
    lea rdx, [rel str_method_isprintable]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isascii]
    lea rdx, [rel str_method_isascii]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isdecimal]
    lea rdx, [rel str_method_isdecimal]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isnumeric]
    lea rdx, [rel str_method_isdecimal]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_removeprefix]
    lea rdx, [rel str_method_removeprefix]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_removesuffix]
    lea rdx, [rel str_method_removesuffix]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_encode]
    lea rdx, [rel str_method_encode]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isalnum]
    lea rdx, [rel str_method_isalnum]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isspace]
    lea rdx, [rel str_method_isspace]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isupper]
    lea rdx, [rel str_method_isupper]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_islower]
    lea rdx, [rel str_method_islower]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_title]
    lea rdx, [rel str_method_title]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_capitalize]
    lea rdx, [rel str_method_capitalize]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_swapcase]
    lea rdx, [rel str_method_swapcase]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_casefold]
    lea rdx, [rel str_method_casefold]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_center]
    lea rdx, [rel str_method_center]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_ljust]
    lea rdx, [rel str_method_ljust]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_rjust]
    lea rdx, [rel str_method_rjust]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_zfill]
    lea rdx, [rel str_method_zfill]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_rindex]
    lea rdx, [rel str_method_rindex]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_istitle]
    lea rdx, [rel str_method_istitle]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_partition]
    lea rdx, [rel str_method_partition]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_rpartition]
    lea rdx, [rel str_method_rpartition]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_rsplit]
    lea rdx, [rel str_method_rsplit]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_splitlines]
    lea rdx, [rel str_method_splitlines]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_expandtabs]
    lea rdx, [rel str_method_expandtabs]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_translate]
    lea rdx, [rel str_method_translate]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_format_map]
    lea rdx, [rel str_method_format_map]
    call add_method_to_dict

    ; Add maketrans as staticmethod
    lea rdi, [rel str_staticmethod_maketrans]
    lea rsi, [rel mn_maketrans]
    call builtin_func_new
    push rax

    mov edi, PyStaticMethodObject_size
    lea rsi, [rel staticmethod_type]
    call gc_alloc
    pop rcx
    mov [rax + PyStaticMethodObject.sm_callable], rcx
    push rax
    mov rdi, rax
    call gc_track
    pop rax
    push rax

    lea rdi, [rel mn_maketrans]
    call str_from_cstr_heap
    push rax

    mov rdi, rbx
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set

    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; Store dict in str_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel str_dunder_len]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel str_dunder_iter]
    call add_method_to_dict

    lea rax, [rel str_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    ; INCREF the dict (type holds ref; dict_new gave us refcnt=1, which we keep)

    ;; --- list methods (with arg count validation) ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_append]
    lea rdx, [rel list_method_append]
    mov rcx, 2              ; min: self + item
    mov r8, 2               ; max: self + item
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel list_method_pop]
    mov rcx, 1              ; min: self (index optional)
    mov r8, 2               ; max: self + index
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_insert]
    lea rdx, [rel list_method_insert]
    mov rcx, 3              ; min: self + index + item
    mov r8, 3               ; max
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_reverse]
    lea rdx, [rel list_method_reverse]
    mov rcx, 1              ; min: self
    mov r8, 1               ; max: self
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_sort]
    lea rdx, [rel list_method_sort]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_index]
    lea rdx, [rel list_method_index]
    mov rcx, 2              ; min: self + value
    mov r8, -1              ; max: unlimited (start, stop optional)
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel list_method_count]
    mov rcx, 2              ; min: self + value
    mov r8, 2               ; max: self + value
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_copy]
    lea rdx, [rel list_method_copy]
    mov rcx, 1              ; min: self
    mov r8, 1               ; max: self
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel list_method_clear]
    mov rcx, 1              ; min: self
    mov r8, 1               ; max: self
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_extend]
    lea rdx, [rel list_method_extend]
    mov rcx, 2              ; min: self + iterable
    mov r8, 2               ; max
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_remove]
    lea rdx, [rel list_method_remove]
    mov rcx, 2              ; min: self + value
    mov r8, 2               ; max
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___reversed__]
    lea rdx, [rel list_method_reversed]
    call add_method_to_dict

    ;; list dunder methods
    mov rdi, rbx
    lea rsi, [rel mn___getitem__]
    lea rdx, [rel list_dunder_getitem]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___setitem__]
    lea rdx, [rel list_dunder_setitem]
    mov rcx, 3
    mov r8, 3
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___delitem__]
    lea rdx, [rel list_dunder_delitem]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___contains__]
    lea rdx, [rel list_dunder_contains]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel list_dunder_len]
    mov rcx, 1
    mov r8, 1
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___iadd__]
    lea rdx, [rel list_dunder_iadd]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___init__]
    lea rdx, [rel list_dunder_init]
    mov rcx, 1
    mov r8, -1
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel container_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    call add_class_getitem

    ; Store in list_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel list_dunder_iter]
    call add_method_to_dict

    lea rax, [rel list_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- dict methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_get]
    lea rdx, [rel dict_method_get]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_keys]
    lea rdx, [rel dict_method_keys]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_values]
    lea rdx, [rel dict_method_values]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_items]
    lea rdx, [rel dict_method_items]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel dict_method_pop]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel dict_method_clear]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_update]
    lea rdx, [rel dict_method_update]
    call add_method_to_dict

    ; dict() has no __init__ either; update() is the same operation.
    mov rdi, rbx
    lea rsi, [rel mn___init__]
    lea rdx, [rel dict_method_update]
    mov rcx, 1
    mov r8, -1
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_setdefault]
    lea rdx, [rel dict_method_setdefault]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_copy]
    lea rdx, [rel dict_method_copy]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_popitem]
    lea rdx, [rel dict_method_popitem]
    call add_method_to_dict

    extern dict_reversed
    mov rdi, rbx
    lea rsi, [rel mn___reversed__]
    lea rdx, [rel dict_reversed]
    call add_method_to_dict

    ; Add fromkeys as classmethod
    lea rdi, [rel dict_classmethod_fromkeys]
    lea rsi, [rel mn_fromkeys]
    call builtin_func_new
    push rax

    mov edi, PyClassMethodObject_size
    lea rsi, [rel classmethod_type]
    call gc_alloc
    pop rcx
    mov [rax + PyClassMethodObject.cm_callable], rcx
    push rax
    mov rdi, rax
    call gc_track
    pop rax
    push rax

    lea rdi, [rel mn_fromkeys]
    call str_from_cstr_heap
    push rax

    mov rdi, rbx
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set

    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    mov rdi, rbx
    lea rsi, [rel container_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    lea rsi, [rel mn___contains__]
    lea rdx, [rel generic_method_contains]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn___setitem__]
    lea rdx, [rel dict_dunder_setitem]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn___delitem__]
    lea rdx, [rel dict_dunder_delitem]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn___getitem__]
    lea rdx, [rel dict_dunder_getitem]
    call add_method_to_dict

    mov rdi, rbx
    call add_class_getitem

    ; Store in dict_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel dict_dunder_len]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel dict_dunder_iter]
    call add_method_to_dict

    lea rax, [rel dict_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- tuple methods ---
    call dict_new
    mov rbx, rax

    ; Registered with arity checks: a.count() and u.index() with no argument
    ; must raise TypeError, which seq_tests asserts.
    mov rdi, rbx
    lea rsi, [rel mn_index]
    lea rdx, [rel tuple_method_index]
    mov rcx, 2
    mov r8, 4                   ; self, value, optional start and stop
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel tuple_method_count]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___getitem__]
    lea rdx, [rel tuple_dunder_getitem]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___contains__]
    lea rdx, [rel tuple_dunder_contains]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel tuple_dunder_len]
    mov rcx, 1
    mov r8, 1
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___add__]
    lea rdx, [rel tuple_dunder_add]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___mul__]
    lea rdx, [rel tuple_dunder_mul]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel mn___rmul__]
    lea rdx, [rel tuple_dunder_rmul]
    mov rcx, 2
    mov r8, 2
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel container_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    call add_class_getitem

    ; Store in tuple_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel tuple_dunder_iter]
    call add_method_to_dict

    lea rax, [rel tuple_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- set methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_add]
    lea rdx, [rel set_method_add]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_remove]
    lea rdx, [rel set_method_remove]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_discard]
    lea rdx, [rel set_method_discard]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel set_method_pop]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel set_method_clear]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_copy]
    lea rdx, [rel set_method_copy]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_union]
    lea rdx, [rel set_method_union]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_intersection]
    lea rdx, [rel set_method_intersection]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_difference]
    lea rdx, [rel set_method_difference]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_symmetric_difference]
    lea rdx, [rel set_method_symmetric_difference]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_issubset]
    lea rdx, [rel set_method_issubset]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_issuperset]
    lea rdx, [rel set_method_issuperset]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isdisjoint]
    lea rdx, [rel set_method_isdisjoint]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_update]
    lea rdx, [rel set_method_update]
    call add_method_to_dict

    ; set() has no __init__, so a subclass had nothing to fill it from.
    ; update() already takes (self, iterable) and returns None.
    mov rdi, rbx
    lea rsi, [rel mn___init__]
    lea rdx, [rel set_method_update]
    mov rcx, 1
    mov r8, -1
    call add_method_to_dict_checked

    mov rdi, rbx
    lea rsi, [rel container_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    call add_class_getitem

    mov rdi, rbx
    lea rsi, [rel mn___contains__]
    lea rdx, [rel generic_method_contains]
    call add_method_to_dict

    ; Store in set_type.tp_dict, and in frozenset's: the two share every
    ; method that does not mutate, and frozenset had no dict at all.

    ; frozenset shares this dict, and set's slots, so one pair covers both.
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel set_dunder_len]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel set_dunder_iter]
    call add_method_to_dict

    lea rax, [rel set_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    lea rax, [rel frozenset_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rbx
    call obj_incref

    ;; --- weakref methods ---
    ; weakref.py binds ref.__hash__ and ref.__eq__ into its subclasses at
    ; class definition time, so those have to exist as methods.
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    lea rdx, [rel generic_method_hash]
    call add_method_to_dict
    mov rdi, rbx
    call add_class_getitem
    extern weakref_type
    lea rax, [rel weakref_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- object_type methods (just __new__) ---
    call dict_new
    mov rbx, rax

    ; Create builtin_func for object_new_fn
    lea rdi, [rel object_new_fn]
    lea rsi, [rel mn___new__]
    call builtin_func_new
    push rax                    ; save builtin_func

    ; Wrap in PyStaticMethodObject (GC-tracked)
    mov edi, PyStaticMethodObject_size
    lea rsi, [rel staticmethod_type]
    call gc_alloc
    pop rcx                     ; builtin_func
    mov [rax + PyStaticMethodObject.sm_callable], rcx
    push rax                    ; save staticmethod wrapper
    mov rdi, rax
    call gc_track
    pop rax
    push rax                    ; re-save

    ; Create key string
    lea rdi, [rel mn___new__]
    call str_from_cstr_heap
    push rax                    ; save key

    ; dict_set(dict, key, staticmethod_wrapper, TAG_PTR, TAG_PTR)
    mov rdi, rbx
    mov rsi, rax                ; key
    mov rdx, [rsp + 8]         ; staticmethod wrapper
    call dict_set

    ; DECREF key
    pop rdi
    call obj_decref

    ; DECREF staticmethod wrapper (dict_set did INCREF)
    pop rdi
    call obj_decref

    ; __init__, __str__ and __repr__ so the base type is introspectable
    mov rdi, rbx
    lea rsi, [rel mn___init__]
    lea rdx, [rel object_method_init]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn___str__]
    lea rdx, [rel object_method_str]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel object_method_repr]
    call add_method_to_dict

    ; The rest of what object supplies by name.  types.py and enum both ask
    ; whether a class overrode one of these, which means asking object for its
    ; own first.
    mov rdi, rbx
    lea rsi, [rel mn___format__]
    lea rdx, [rel object_method_format]
    call add_method_to_dict

    ; __doc__ is an attribute, not a method, and object supplies it so that
    ; anything without a docstring answers None rather than raising.  CPython
    ; hands back the type's own docstring; what matters to the code that asks
    ; -- types.DynamicClassAttribute's `doc or fget.__doc__`, with no getter --
    ; is that the lookup succeeds at all.
    lea rdi, [rel mn___doc__]
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    pop rdi
    call obj_decref

    mov rdi, rbx
    lea rsi, [rel mn___sizeof__]
    lea rdx, [rel object_method_sizeof]
    call add_method_to_dict

    ; A classmethod: `super().__init_subclass__()` is how every real one ends.
    lea rdi, [rel object_method_init_subclass]
    lea rsi, [rel mn___init_subclass__]
    call builtin_func_new
    push rax
    mov edi, PyClassMethodObject_size
    lea rsi, [rel classmethod_type]
    call gc_alloc
    pop rcx
    mov [rax + PyClassMethodObject.cm_callable], rcx
    mov r12, rax
    mov rdi, rax
    call gc_track
    lea rdi, [rel mn___init_subclass__]
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, rax
    mov rdx, r12
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, r12
    call obj_decref

    mov rdi, rbx
    lea rsi, [rel mn___dir__]
    lea rdx, [rel object_method_dir]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn___reduce__]
    lea rdx, [rel object_method_reduce]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn___reduce_ex__]
    lea rdx, [rel object_method_reduce]
    call add_method_to_dict

    ; The comparisons, which every class inherits and the stdlib binds by
    ; name: `__ne__ = MutableMapping.__ne__` reaches object's.
    mov rdi, rbx
    lea rsi, [rel mn___eq__]
    lea rdx, [rel object_method_eq]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___ne__]
    lea rdx, [rel object_method_ne]
    call add_method_to_dict
    ; Not the ordering four: a builtin subclass looks __lt__ up in its MRO
    ; and would find object's NotImplemented before reaching the base type's
    ; own comparison, so `sorted([L([2]), L([1])])` on a list subclass would
    ; stop working.  CPython reaches those through slot wrappers this does
    ; not have yet.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    lea rdx, [rel object_method_hash]
    call add_method_to_dict

    ; Store in object_type.tp_dict
    lea rax, [rel object_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- type_type: __new__, so a metaclass can call super().__new__ ---
    extern type_type
    call dict_new
    mov rbx, rax

    extern type_method_new
    lea rdi, [rel type_method_new]
    lea rsi, [rel mn___new__]
    call builtin_func_new
    push rax
    mov edi, PyStaticMethodObject_size
    lea rsi, [rel staticmethod_type]
    call gc_alloc
    pop rcx
    mov [rax + PyStaticMethodObject.sm_callable], rcx
    mov r12, rax
    mov rdi, rax
    call gc_track
    lea rdi, [rel mn___new__]
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, rax
    mov rdx, r12
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, r12
    call obj_decref

    mov rdi, rbx
    call add_class_getitem

    lea rax, [rel type_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- function type: expose its introspection attributes on the type ---
    ;; types.py takes GetSetDescriptorType from `type(FunctionType.__code__)`
    ;; and MemberDescriptorType from `type(FunctionType.__globals__)`, so both
    ;; have to be reachable *through the type*, not just on an instance.
    ;; func_getattr already answers them for instances.
    call dict_new
    mov rbx, rax

    lea rdi, [rel mn___code__]
    call str_from_cstr_heap
    push rax
    xor edi, edi
    xor esi, esi
    mov rdx, rax
    extern getset_descr_new
    call getset_descr_new
    push rax
    mov rdi, rbx
    mov rsi, [rsp + 8]
    mov rdx, rax
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    mov rdi, PyFuncObject.func_globals
    lea rsi, [rel mn___globals__]
    push rdi
    mov rdi, rsi
    call str_from_cstr_heap
    pop rdi
    push rax
    mov rsi, rax
    extern member_descr_new
    call member_descr_new
    push rax
    mov rdi, rbx
    mov rsi, [rsp + 8]
    mov rdx, rax
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; A function is a descriptor.  enum asks hasattr(value, '__get__') to tell
    ; a method in a class body from an enum member, so the binding LOAD_ATTR
    ; does natively has to be reachable by name as well.
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern func_dunder_get
    lea rdx, [rel func_dunder_get]
    call add_method_to_dict

    extern func_type
    lea rax, [rel func_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- staticmethod and classmethod: descriptors for the same reason ---
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern staticmethod_dunder_get
    lea rdx, [rel staticmethod_dunder_get]
    call add_method_to_dict
    lea rax, [rel staticmethod_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern classmethod_dunder_get
    lea rdx, [rel classmethod_dunder_get]
    call add_method_to_dict
    lea rax, [rel classmethod_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- property: the same three, by name ---
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern property_dunder_get
    lea rdx, [rel property_dunder_get]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___set__]
    extern property_dunder_set
    lea rdx, [rel property_dunder_set]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___delete__]
    extern property_dunder_delete
    lea rdx, [rel property_dunder_delete]
    call add_method_to_dict
    extern property_type
    lea rax, [rel property_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- int_type methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel int_dunder_repr]
    call add_method_to_dict

    ; int.__new__ / str.__new__: enum builds each member with
    ; `member_type.__new__(cls, *args)`, and decides which base is the data
    ; type by asking whether __new__ is in its __dict__.
    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    lea rsi, [rel mn_bit_length]
    lea rdx, [rel int_method_bit_length]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_bit_count]
    lea rdx, [rel int_method_bit_count]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_conjugate]
    lea rdx, [rel int_method_conjugate]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_to_bytes]
    lea rdx, [rel int_method_to_bytes]
    call add_method_to_dict

    ; Add from_bytes as classmethod
    lea rdi, [rel int_classmethod_from_bytes]
    lea rsi, [rel mn_from_bytes]
    call builtin_func_new
    push rax                    ; save builtin_func

    ; Wrap in PyClassMethodObject (GC-tracked)
    mov edi, PyClassMethodObject_size
    lea rsi, [rel classmethod_type]
    call gc_alloc
    pop rcx                     ; builtin_func
    mov [rax + PyClassMethodObject.cm_callable], rcx
    push rax                    ; save classmethod wrapper
    mov rdi, rax
    call gc_track
    pop rax
    push rax                    ; re-save

    ; Create key string
    lea rdi, [rel mn_from_bytes]
    call str_from_cstr_heap
    push rax                    ; save key

    ; dict_set(dict, key, classmethod_wrapper, TAG_PTR, TAG_PTR)
    mov rdi, rbx
    mov rsi, rax                ; key
    mov rdx, [rsp + 8]         ; classmethod wrapper
    call dict_set

    ; DECREF key
    pop rdi
    call obj_decref
    ; DECREF classmethod wrapper (dict_set did INCREF)
    pop rdi
    call obj_decref

    ; Store in int_type.tp_dict
    lea rax, [rel int_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- float_type methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel float_dunder_repr]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_is_integer]
    lea rdx, [rel float_method_is_integer]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_conjugate]
    lea rdx, [rel float_method_conjugate]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_as_integer_ratio]
    lea rdx, [rel float_method_as_integer_ratio]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_hex]
    lea rdx, [rel float_method_hex]
    call add_method_to_dict

    ; Add fromhex as classmethod
    lea rdi, [rel float_classmethod_fromhex]
    lea rsi, [rel mn_fromhex]
    call builtin_func_new
    push rax

    mov edi, PyClassMethodObject_size
    lea rsi, [rel classmethod_type]
    call gc_alloc
    pop rcx
    mov [rax + PyClassMethodObject.cm_callable], rcx
    push rax
    mov rdi, rax
    call gc_track
    pop rax
    push rax

    lea rdi, [rel mn_fromhex]
    call str_from_cstr_heap
    push rax

    mov rdi, rbx
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set

    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; Store in float_type.tp_dict
    lea rax, [rel float_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- bytes_type methods (extend tp_dict, keep tp_getattr for .decode()) ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn___str__]
    lea rdx, [rel bytes_dunder_str]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel bytes_dunder_repr]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_hex]
    lea rdx, [rel bytes_method_hex]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_startswith]
    lea rdx, [rel bytes_method_startswith]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_endswith]
    lea rdx, [rel bytes_method_endswith]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel bytes_method_count]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_find]
    lea rdx, [rel bytes_method_find]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_replace]
    lea rdx, [rel bytes_method_replace]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_split]
    lea rdx, [rel bytes_method_split]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_join]
    lea rdx, [rel bytes_method_join]
    call add_method_to_dict

    ; Store in bytes_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel bytes_dunder_len]
    call add_method_to_dict
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel bytes_dunder_iter]
    call add_method_to_dict

    lea rax, [rel bytes_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    pop r12
    pop rbx
    leave
    ret
END_FUNC methods_init

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata

empty_str_cstr: db 0

; Method name strings
mn_upper:       db "upper", 0
mn_lower:       db "lower", 0
mn_strip:       db "strip", 0
mn_startswith:  db "startswith", 0
mn_endswith:    db "endswith", 0
mn_find:        db "find", 0
mn_replace:     db "replace", 0
mn_join:        db "join", 0
mn_split:       db "split", 0
mn_format:      db "format", 0
mn_append:      db "append", 0
mn_pop:         db "pop", 0
mn_insert:      db "insert", 0
mn_reverse:     db "reverse", 0
mn_sort:        db "sort", 0
mn_index:       db "index", 0
mn_count:       db "count", 0
mn_copy:        db "copy", 0
mn_clear:       db "clear", 0
mn_extend:      db "extend", 0
mn_get:         db "get", 0
mn_keys:        db "keys", 0
mn_values:      db "values", 0
mn_items:       db "items", 0
mn_update:      db "update", 0
mn_lstrip:      db "lstrip", 0
mn_rstrip:      db "rstrip", 0
mn_rfind:       db "rfind", 0
mn_isdigit:     db "isdigit", 0
mn_isalpha:     db "isalpha", 0
mn_isidentifier: db "isidentifier", 0
mn_isprintable: db "isprintable", 0
mn_isascii:     db "isascii", 0
mn_isdecimal:   db "isdecimal", 0
mn_isnumeric:   db "isnumeric", 0
mn_removeprefix: db "removeprefix", 0
mn_removesuffix: db "removesuffix", 0
mn_encode:      db "encode", 0
mn_setdefault:  db "setdefault", 0
mn_popitem:     db "popitem", 0
mn_remove:      db "remove", 0
mn_add:         db "add", 0
mn_discard:     db "discard", 0
mn_union:       db "union", 0
mn_intersection: db "intersection", 0
mn_difference:  db "difference", 0
mn_symmetric_difference: db "symmetric_difference", 0
mn_issubset:    db "issubset", 0
mn_issuperset:  db "issuperset", 0
mn_isdisjoint:  db "isdisjoint", 0
mn_isalnum:     db "isalnum", 0
mn_isspace:     db "isspace", 0
mn_isupper:     db "isupper", 0
mn_islower:     db "islower", 0
mn___new__:     db "__new__", 0
mn___get__:     db "__get__", 0
mn___set__:     db "__set__", 0
mn___delete__:  db "__delete__", 0
mn_title:       db "title", 0
mn_capitalize:  db "capitalize", 0
mn_swapcase:    db "swapcase", 0
mn_casefold:    db "casefold", 0
mn_center:      db "center", 0
mn_ljust:       db "ljust", 0
mn_rjust:       db "rjust", 0
mn_zfill:       db "zfill", 0
mn_rindex:      db "rindex", 0
mn_istitle:     db "istitle", 0
mn_partition:   db "partition", 0
mn_rpartition:  db "rpartition", 0
mn_rsplit:      db "rsplit", 0
mn_splitlines:  db "splitlines", 0
mn_expandtabs:  db "expandtabs", 0
mn_translate:   db "translate", 0
mn_format_map:  db "format_map", 0
mn_maketrans:   db "maketrans", 0
; int method names
mn_to_bytes:    db "to_bytes", 0
mn_from_bytes:  db "from_bytes", 0
mn_bit_length:  db "bit_length", 0
mn_bit_count:   db "bit_count", 0
mn___index__:   db "__index__", 0
mn_conjugate:   db "conjugate", 0
mn___abs__:     db "__abs__", 0
mn___int__:     db "__int__", 0
mn___float__:   db "__float__", 0
; float method names
mn_is_integer:  db "is_integer", 0
mn_as_integer_ratio: db "as_integer_ratio", 0
mn___trunc__:   db "__trunc__", 0
; float method names (continued)
mn_fromhex:     db "fromhex", 0
; bytes method names
mn_hex:         db "hex", 0
mn_decode:      db "decode", 0
; dict method names (continued)
mn_fromkeys:    db "fromkeys", 0
mn___reversed__: db "__reversed__", 0
mn___format__:  db "__format__", 0
mn___sizeof__:  db "__sizeof__", 0
mn___doc__:     db "__doc__", 0
mn___init_subclass__: db "__init_subclass__", 0
mn___iter__:    db "__iter__", 0
mn___dir__:     db "__dir__", 0
mn___reduce__:  db "__reduce__", 0
mn___reduce_ex__: db "__reduce_ex__", 0
mn___getitem__: db "__getitem__", 0
mn___setitem__: db "__setitem__", 0
mn___delitem__: db "__delitem__", 0
mn___contains__: db "__contains__", 0
mn___len__:     db "__len__", 0
mn___eq__: db "__eq__", 0
mn___ne__: db "__ne__", 0
mn___lt__: db "__lt__", 0
mn___le__: db "__le__", 0
mn___gt__: db "__gt__", 0
mn___ge__: db "__ge__", 0
mn___hash__:    db "__hash__", 0
mn___add__:     db "__add__", 0
mn___mul__:     db "__mul__", 0
mn___rmul__:    db "__rmul__", 0
mn___iadd__:    db "__iadd__", 0
mn___init__:    db "__init__", 0
mn___str__:     db "__str__", 0
mn___code__:    db "__code__", 0
mn___class_getitem__: db "__class_getitem__", 0
mn___globals__: db "__globals__", 0
mn___repr__:    db "__repr__", 0
