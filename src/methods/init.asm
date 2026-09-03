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
extern memoryview_method_tobytes
extern memoryview_method_tolist
extern memoryview_method_cast
extern memoryview_method_release
extern memoryview_method_enter
extern memoryview_method_exit
extern memoryview_method_hex
extern memoryview_dunder_getitem
extern memoryview_dunder_setitem
extern memoryview_dunder_len
extern memoryview_type
extern bytearray_method_append
extern bytearray_method_extend
extern bytearray_method_insert
extern bytearray_method_pop
extern bytearray_method_remove
extern bytearray_method_clear
extern bytearray_method_reverse
extern bytearray_method_copy
extern ba_shared_hex
extern ba_shared_startswith
extern ba_shared_endswith
extern ba_shared_count
extern ba_shared_find
extern ba_shared_replace
extern ba_shared_split
extern ba_shared_join
extern ba_shared_decode
extern builtin_method_format
extern bytearray_dunder_len
extern bytearray_dunder_iter
extern bytearray_dunder_setitem
extern bytearray_dunder_delitem
extern bytearray_dunder_getitem
extern bytearray_dunder_contains
extern bytearray_type
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
extern frozenset_dunder_hash
extern list_dunder_add
extern list_dunder_mul
extern list_dunder_rmul
extern list_dunder_imul
extern str_dunder_add
extern str_dunder_mul
extern str_dunder_rmul
extern str_dunder_mod
extern str_dunder_rmod
extern str_dunder_getitem
extern bytes_dunder_add
extern bytes_dunder_mul
extern bytes_dunder_rmul
extern bytes_dunder_mod
extern bytes_dunder_rmod
extern bytes_dunder_getitem
extern bytearray_dunder_add
extern bytearray_dunder_mul
extern bytearray_dunder_rmul
extern bytearray_dunder_iadd
extern bytearray_dunder_imul
extern bytearray_dunder_mod
extern bytearray_dunder_rmod
extern dict_dunder_or
extern dict_dunder_ror
extern dict_dunder_ior
extern set_dunder_sub
extern set_dunder_and
extern set_dunder_xor
extern set_dunder_or
extern set_dunder_rsub
extern set_dunder_rand
extern set_dunder_rxor
extern set_dunder_ror
extern type_stamp_methods
extern frozenset_dunder_len
extern frozenset_dunder_iter
extern frozenset_dunder_sub
extern frozenset_dunder_and
extern frozenset_dunder_xor
extern frozenset_dunder_or
extern frozenset_dunder_rsub
extern frozenset_dunder_rand
extern frozenset_dunder_rxor
extern frozenset_dunder_ror
extern object_method_setattr
extern object_method_delattr
extern object_method_getattribute
extern object_method_getstate
extern object_method_subclasshook
extern set_dunder_len
extern str_dunder_iter
extern str_dunder_len
extern str_dunder_repr
extern str_dunder_str
extern tuple_dunder_iter

; Set entry layout constants (must match set.asm)

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
extern complex_type
extern complex_dunder_repr
extern complex_method_conjugate
extern complex_method_complex
extern complex_method_getnewargs
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
;; HELPER: dict_add_builtin_func(dict, name_cstr, func_ptr)
;; rdi=dict, rsi=name_cstr, rdx=func_ptr
;; Creates a builtin func wrapper and stores it in the dict.
;; ============================================================================
;; ============================================================================
;; dict_add_none(rdi = a dict, rsi = a name C string)
;;
;; Stores None under the name.  `__hash__ = None` is how a type says it is
;; unhashable, and the stdlib asks by name: list.__hash__ is None decides
;; whether something can be a dict key long before anyone calls hash().
;; list, dict, set and bytearray all carry hash_not_implemented in tp_hash
;; and had nothing in tp_dict, so the name resolved to object's and they
;; advertised a working __hash__.
;; ============================================================================
DAN_DICT  equ 8
DAN_FRAME equ 16            ; + 0 pushes = 16
DEF_FUNC_LOCAL dict_add_none, DAN_FRAME
    mov [rbp - DAN_DICT], rdi
    mov rdi, rsi
    call str_from_cstr_heap
    push rax
    sub rsp, 8
    mov rdi, [rbp - DAN_DICT]
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    add rsp, 8
    pop rdi
    call obj_decref
    leave
    ret
END_FUNC dict_add_none

DEF_FUNC dict_add_builtin_func
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
END_FUNC dict_add_builtin_func

;; ============================================================================
;; HELPER: dict_add_getset(rdi = dict, rsi = name_cstr, rdx = getter,
;;                         rcx = setter or 0)
;;
;; A named pair of C accessors in a type's dict -- CPython's PyGetSetDef.  This
;; is what makes `int.real` answer a descriptor instead of raising, and what
;; puts real/imag/numerator/denominator into dir().  The instance read still
;; goes through each type's tp_getattr, which is faster and gets there first;
;; both call the same getter.
;; ============================================================================
DEF_FUNC dict_add_getset
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; dict
    mov r12, rsi                ; name_cstr
    mov r13, rdx                ; getter
    mov r14, rcx                ; setter

    mov rdi, r12
    call str_from_cstr_heap
    push rax                    ; the key, and the descriptor's own name

    mov rdi, r13
    mov rsi, r14
    mov rdx, rax
    extern getset_descr_new
    call getset_descr_new
    push rax

    mov rdi, rbx
    mov rsi, [rsp + 8]          ; key
    mov rdx, rax                ; the descriptor
    call dict_set

    pop rdi
    call obj_decref             ; dict_set took its own reference
    pop rdi
    call obj_decref             ; and getset_descr_new took one on the name

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_add_getset

;; ============================================================================
;; HELPER: add_method_to_dict_checked(dict, name_cstr, func_ptr, min_args, max_args)
;; rdi=dict, rsi=name_cstr, rdx=func_ptr, rcx=min_args, r8=max_args
;; Like dict_add_builtin_func but sets arg count bounds.
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

;; ============================================================================
;; add_staticmethod(rdi = type dict, rsi = name cstr, rdx = function)
;;
;; A staticmethod is what keeps maketrans from being handed a self it does not
;; want: bytes.maketrans(a, b) and b"".maketrans(a, b) must both see exactly
;; two arguments.  str's maketrans was the only one of these and did it
;; inline; three of them is enough to want one place.
;; ============================================================================
ASM_FUNC  equ 8
ASM_SM    equ 16
ASM_KEY   equ 24
ASM_FRAME equ 40            ; + 3 pushes = 64

DEF_FUNC_LOCAL add_staticmethod, ASM_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi                ; the type dict
    mov r12, rsi                ; the name
    mov r13, rdx                ; the function

    mov rdi, r13
    mov rsi, r12
    call builtin_func_new
    mov [rbp - ASM_FUNC], rax

    mov edi, PyStaticMethodObject_size
    lea rsi, [rel staticmethod_type]
    call gc_alloc
    mov rcx, [rbp - ASM_FUNC]
    mov [rax + PyStaticMethodObject.sm_callable], rcx
    mov [rbp - ASM_SM], rax
    mov rdi, rax
    call gc_track

    mov rdi, r12
    call str_from_cstr_heap
    mov [rbp - ASM_KEY], rax

    mov rdi, rbx
    mov rsi, rax
    mov rdx, [rbp - ASM_SM]
    call dict_set

    mov rdi, [rbp - ASM_KEY]
    call obj_decref
    mov rdi, [rbp - ASM_SM]
    call obj_decref

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC add_staticmethod

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
;; ============================================================================
;; set_add_shared_methods(rdi = a dict)
;;
;; The methods set and frozenset genuinely share: the ones that only read.
;;
;; The two used to share the whole dict -- one object stored into both types
;; -- which handed frozenset every mutator as well.  None of those bodies
;; inspects self, so they did not raise; they WORKED.  frozenset({1}).add(2)
;; mutated the frozenset in place, and a frozenset is the one type that
;; exists to be a dict key, so mutating one after it had been used as a key
;; left the dict unable to find either the old key or the new.
;; ============================================================================
SASM_DICT  equ 8
SASM_FRAME equ 16           ; + 0 pushes = 16, 16-aligned
DEF_FUNC_LOCAL set_add_shared_methods, SASM_FRAME
    mov [rbp - SASM_DICT], rdi

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn_copy]
    lea rdx, [rel set_method_copy]
    call dict_add_builtin_func

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn_union]
    lea rdx, [rel set_method_union]
    call dict_add_builtin_func

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn_intersection]
    lea rdx, [rel set_method_intersection]
    call dict_add_builtin_func

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn_difference]
    lea rdx, [rel set_method_difference]
    call dict_add_builtin_func

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn_symmetric_difference]
    lea rdx, [rel set_method_symmetric_difference]
    call dict_add_builtin_func

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn_issubset]
    lea rdx, [rel set_method_issubset]
    call dict_add_builtin_func

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn_issuperset]
    lea rdx, [rel set_method_issuperset]
    call dict_add_builtin_func

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn_isdisjoint]
    lea rdx, [rel set_method_isdisjoint]
    call dict_add_builtin_func

    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel mn___contains__]
    lea rdx, [rel generic_method_contains]
    call dict_add_builtin_func

    ; __new__ allocates an empty instance of args[0], so it serves both.
    mov rdi, [rbp - SASM_DICT]
    lea rsi, [rel container_dunder_new]
    call add_new_staticmethod

    mov rdi, [rbp - SASM_DICT]
    call add_class_getitem

    ; The eight operator dunders, __len__ and __iter__ are NOT here.  They
    ; used to be, and a shared body has to admit both receivers -- which made
    ; set.__and__(frozenset(...), ...) and set.__len__(frozenset()) legal
    ; where CPython raises.  Each type registers its own now; see
    ; set_add_operator_methods.  __contains__ stays shared: it is
    ; generic_method_contains, and CPython's refusal there carries the
    ; wrapper-descriptor wording, which is a distinction this tree does not
    ; draw yet.
    leave
    ret
END_FUNC set_add_shared_methods

;; ============================================================================
;; set_add_operator_methods(rdi = a dict, rsi = the ten function pointers)
;; The eight set operators plus __len__ and __iter__, registered into one
;; type's dict from that type's own bodies.  CPython's frozenset really does carry the reflected four as
;; well: hasattr(frozenset, '__rsub__') is True there, and
;; frozenset({2}).__rsub__({1}) is frozenset({1}).
;; ============================================================================
SAOM_DICT equ 8
SAOM_FNS  equ 16
SAOM_IDX  equ 24
SAOM_FRAME equ 32           ; + 0 pushes = 32
DEF_FUNC_LOCAL set_add_operator_methods, SAOM_FRAME
    mov [rbp - SAOM_DICT], rdi
    mov [rbp - SAOM_FNS], rsi
    mov qword [rbp - SAOM_IDX], 0
.saom_loop:
    mov rax, [rbp - SAOM_IDX]
    cmp rax, 10
    jge .saom_done
    mov rdi, [rbp - SAOM_DICT]
    lea rsi, [rel set_operator_names]
    mov rsi, [rsi + rax*8]
    mov rdx, [rbp - SAOM_FNS]
    mov rdx, [rdx + rax*8]
    call dict_add_builtin_func
    inc qword [rbp - SAOM_IDX]
    jmp .saom_loop
.saom_done:
    leave
    ret
END_FUNC set_add_operator_methods

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
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel str_dunder_repr]
    call dict_add_builtin_func

    ; int.__new__ / str.__new__: enum builds each member with
    ; `member_type.__new__(cls, *args)`, and decides which base is the data
    ; type by asking whether __new__ is in its __dict__.
    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    lea rsi, [rel mn_upper]
    lea rdx, [rel str_method_upper]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_lower]
    lea rdx, [rel str_method_lower]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_strip]
    lea rdx, [rel str_method_strip]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_startswith]
    lea rdx, [rel str_method_startswith]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_endswith]
    lea rdx, [rel str_method_endswith]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_find]
    lea rdx, [rel str_method_find]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_replace]
    lea rdx, [rel str_method_replace]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_join]
    lea rdx, [rel str_method_join]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_split]
    lea rdx, [rel str_method_split]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_format]
    lea rdx, [rel str_method_format]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_lstrip]
    lea rdx, [rel str_method_lstrip]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_rstrip]
    lea rdx, [rel str_method_rstrip]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel str_method_count]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_index]
    lea rdx, [rel str_method_index]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_rfind]
    lea rdx, [rel str_method_rfind]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isdigit]
    lea rdx, [rel str_method_isdigit]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isalpha]
    lea rdx, [rel str_method_isalpha]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isidentifier]
    lea rdx, [rel str_method_isidentifier]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isprintable]
    lea rdx, [rel str_method_isprintable]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isascii]
    lea rdx, [rel str_method_isascii]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isdecimal]
    lea rdx, [rel str_method_isdecimal]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isnumeric]
    extern str_method_isnumeric
    lea rdx, [rel str_method_isnumeric]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_removeprefix]
    lea rdx, [rel str_method_removeprefix]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_removesuffix]
    lea rdx, [rel str_method_removesuffix]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_encode]
    lea rdx, [rel str_method_encode]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isalnum]
    lea rdx, [rel str_method_isalnum]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isspace]
    lea rdx, [rel str_method_isspace]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_isupper]
    lea rdx, [rel str_method_isupper]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_islower]
    lea rdx, [rel str_method_islower]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_title]
    lea rdx, [rel str_method_title]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_capitalize]
    lea rdx, [rel str_method_capitalize]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_swapcase]
    lea rdx, [rel str_method_swapcase]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_casefold]
    lea rdx, [rel str_method_casefold]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_center]
    lea rdx, [rel str_method_center]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_ljust]
    lea rdx, [rel str_method_ljust]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_rjust]
    lea rdx, [rel str_method_rjust]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_zfill]
    lea rdx, [rel str_method_zfill]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_rindex]
    lea rdx, [rel str_method_rindex]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_istitle]
    lea rdx, [rel str_method_istitle]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_partition]
    lea rdx, [rel str_method_partition]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_rpartition]
    lea rdx, [rel str_method_rpartition]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_rsplit]
    lea rdx, [rel str_method_rsplit]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_splitlines]
    lea rdx, [rel str_method_splitlines]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_expandtabs]
    lea rdx, [rel str_method_expandtabs]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_translate]
    lea rdx, [rel str_method_translate]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_format_map]
    lea rdx, [rel str_method_format_map]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_maketrans]
    lea rdx, [rel str_staticmethod_maketrans]
    call add_staticmethod

    ; Store dict in str_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel str_dunder_len]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel str_dunder_iter]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___format__]
    lea rdx, [rel builtin_method_format]
    call dict_add_builtin_func

    ; The operators, by name.
    mov rdi, rbx
    lea rsi, [rel mn___add__]
    lea rdx, [rel str_dunder_add]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mul__]
    lea rdx, [rel str_dunder_mul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmul__]
    lea rdx, [rel str_dunder_rmul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mod__]
    lea rdx, [rel str_dunder_mod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmod__]
    lea rdx, [rel str_dunder_rmod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___getitem__]
    lea rdx, [rel str_dunder_getitem]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___contains__]
    lea rdx, [rel generic_method_contains]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    extern str_dunder_hash
    lea rdx, [rel str_dunder_hash]
    call dict_add_builtin_func

    lea rax, [rel str_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods
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
    call dict_add_builtin_func

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
    call dict_add_builtin_func

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
    call dict_add_builtin_func

    ; The operators, by name.  The slots were there and the names were not.
    mov rdi, rbx
    lea rsi, [rel mn___add__]
    lea rdx, [rel list_dunder_add]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mul__]
    lea rdx, [rel list_dunder_mul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmul__]
    lea rdx, [rel list_dunder_rmul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___imul__]
    lea rdx, [rel list_dunder_imul]
    call dict_add_builtin_func

    ; Unhashable: the name has to BE None, not resolve to object's.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    call dict_add_none

    lea rax, [rel list_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- dict methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_get]
    lea rdx, [rel dict_method_get]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_keys]
    lea rdx, [rel dict_method_keys]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_values]
    lea rdx, [rel dict_method_values]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_items]
    lea rdx, [rel dict_method_items]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel dict_method_pop]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel dict_method_clear]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_update]
    lea rdx, [rel dict_method_update]
    call dict_add_builtin_func

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
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_copy]
    lea rdx, [rel dict_method_copy]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_popitem]
    lea rdx, [rel dict_method_popitem]
    call dict_add_builtin_func

    extern dict_reversed
    mov rdi, rbx
    lea rsi, [rel mn___reversed__]
    lea rdx, [rel dict_reversed]
    call dict_add_builtin_func

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
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___setitem__]
    lea rdx, [rel dict_dunder_setitem]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___delitem__]
    lea rdx, [rel dict_dunder_delitem]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___getitem__]
    lea rdx, [rel dict_dunder_getitem]
    call dict_add_builtin_func

    mov rdi, rbx
    call add_class_getitem

    ; Store in dict_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel dict_dunder_len]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel dict_dunder_iter]
    call dict_add_builtin_func

    ; The union operators, by name.
    mov rdi, rbx
    lea rsi, [rel mn___or__]
    lea rdx, [rel dict_dunder_or]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___ror__]
    lea rdx, [rel dict_dunder_ror]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___ior__]
    lea rdx, [rel dict_dunder_ior]
    call dict_add_builtin_func

    ; Unhashable: the name has to BE None, not resolve to object's.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    call dict_add_none

    lea rax, [rel dict_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

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
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    extern tuple_dunder_hash
    lea rdx, [rel tuple_dunder_hash]
    call dict_add_builtin_func

    lea rax, [rel tuple_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- set methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_add]
    lea rdx, [rel set_method_add]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_remove]
    lea rdx, [rel set_method_remove]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_discard]
    lea rdx, [rel set_method_discard]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel set_method_pop]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel set_method_clear]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_update]
    lea rdx, [rel set_method_update]
    call dict_add_builtin_func

    ; set() has no __init__, so a subclass had nothing to fill it from.
    ; update() already takes (self, iterable) and returns None.
    mov rdi, rbx
    lea rsi, [rel mn___init__]
    lea rdx, [rel set_method_update]
    mov rcx, 1
    mov r8, -1
    call add_method_to_dict_checked

    mov rdi, rbx
    call set_add_shared_methods

    mov rdi, rbx
    lea rsi, [rel set_operator_fns]
    call set_add_operator_methods

    ; The reflected four are registered with the forward four.  __iand__ and
    ; __ior__ are deliberately absent -- set has no nb_inplace_* slots here,
    ; so `s &= t` degrades to the binary form, and a by-name __iand__ that
    ; did not mutate in place would be a wrong answer rather than a missing
    ; name.

    ; Unhashable: the name has to BE None, not resolve to object's.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    call dict_add_none

    lea rax, [rel set_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- frozenset methods ---
    ; Its own dict, holding only what reads.  It used to be set's dict, the
    ; same object stored into both types, so frozenset carried add, remove,
    ; discard, pop, clear and update -- and those bodies do not inspect self,
    ; so they did not raise, they worked.  frozenset({1}).add(2) mutated the
    ; frozenset.
    ;
    ; It has no __init__ either: CPython's has none, and a subclass is filled
    ; by frozenset_type_call through type_call's tp_new path before __init__
    ; is ever looked up.
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    call set_add_shared_methods

    ; frozenset's own eight, so its descriptors refuse a set the way
    ; CPython's do.
    mov rdi, rbx
    lea rsi, [rel frozenset_operator_fns]
    call set_add_operator_methods

    ; frozenset.__hash__ names frozenset's OWN hash.  With no entry here the
    ; lookup walked the MRO to object's, which answers the address -- so
    ; frozenset.__hash__(f) and hash(f) disagreed on the one type that exists
    ; to be a dict key.  It cannot go through obj_hash either: that reads
    ; tp_hash, and a subclass defining __hash__ would re-enter itself.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    lea rdx, [rel frozenset_dunder_hash]
    call dict_add_builtin_func

    lea rax, [rel frozenset_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- weakref methods ---
    ; weakref.py binds ref.__hash__ and ref.__eq__ into its subclasses at
    ; class definition time, so those have to exist as methods.
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    lea rdx, [rel generic_method_hash]
    call dict_add_builtin_func
    mov rdi, rbx
    call add_class_getitem
    extern weakref_type
    lea rax, [rel weakref_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

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
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___str__]
    lea rdx, [rel object_method_str]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel object_method_repr]
    call dict_add_builtin_func

    ; The rest of what object supplies by name.  types.py and enum both ask
    ; whether a class overrode one of these, which means asking object for its
    ; own first.
    mov rdi, rbx
    lea rsi, [rel mn___format__]
    lea rdx, [rel object_method_format]
    call dict_add_builtin_func

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
    call dict_add_builtin_func

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
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___reduce__]
    lea rdx, [rel object_method_reduce]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___reduce_ex__]
    lea rdx, [rel object_method_reduce]
    call dict_add_builtin_func

    ; The comparisons, which every class inherits and the stdlib binds by
    ; name: `__ne__ = MutableMapping.__ne__` reaches object's.
    mov rdi, rbx
    lea rsi, [rel mn___eq__]
    lea rdx, [rel object_method_eq]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___ne__]
    lea rdx, [rel object_method_ne]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    lea rdx, [rel object_method_hash]
    call dict_add_builtin_func

    ; The ordering four, which answer NotImplemented.  They are safe for the
    ; same reason __eq__ is: type_install_slots installs no wrapper over a
    ; dunder that came from a type which is not a heaptype, and object is
    ; not, so a builtin subclass keeps its base's comparison rather than
    ; object's.
    mov rdi, rbx
    lea rsi, [rel mn___lt__]
    extern object_method_lt
    lea rdx, [rel object_method_lt]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___le__]
    extern object_method_le
    lea rdx, [rel object_method_le]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___gt__]
    extern object_method_gt
    lea rdx, [rel object_method_gt]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___ge__]
    extern object_method_ge
    lea rdx, [rel object_method_ge]
    call dict_add_builtin_func

    ; The generic attribute dunders, and the two hooks.  All five were
    ; absent, and every type inherits them -- abcmod has been looking for
    ; __subclasshook__ since it was written and silently finding nothing.
    mov rdi, rbx
    lea rsi, [rel mn___setattr__]
    lea rdx, [rel object_method_setattr]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___delattr__]
    lea rdx, [rel object_method_delattr]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___getattribute__]
    lea rdx, [rel object_method_getattribute]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___getstate__]
    lea rdx, [rel object_method_getstate]
    call dict_add_builtin_func

    ; __subclasshook__ is a classmethod: it takes the class explicitly.
    lea rdi, [rel object_method_subclasshook]
    lea rsi, [rel mn___subclasshook__]
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
    lea rdi, [rel mn___subclasshook__]
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

    ; Store in object_type.tp_dict
    lea rax, [rel object_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

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

    ; bool is a static subclass of int -- the only one in the tree -- so it
    ; never went through type_from_parts and had nothing to register it.
    ; CPython lists it in int.__subclasses__(), so it is recorded by hand.
    extern subclass_register
    extern bool_type
    lea rdi, [rel bool_type]
    call subclass_register

    ; type.__subclasses__ -- the direct subclasses, live ones only.
    extern type_method_subclasses
    mov rdi, rbx
    lea rsi, [rel mn___subclasses__]
    lea rdx, [rel type_method_subclasses]
    call dict_add_builtin_func

    lea rax, [rel type_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

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
    call dict_add_builtin_func

    extern func_type
    lea rax, [rel func_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- staticmethod and classmethod: descriptors for the same reason ---
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern staticmethod_dunder_get
    lea rdx, [rel staticmethod_dunder_get]
    call dict_add_builtin_func
    lea rax, [rel staticmethod_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern classmethod_dunder_get
    lea rdx, [rel classmethod_dunder_get]
    call dict_add_builtin_func
    lea rax, [rel classmethod_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- property: the same three, by name ---
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern property_dunder_get
    lea rdx, [rel property_dunder_get]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___set__]
    extern property_dunder_set
    lea rdx, [rel property_dunder_set]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___delete__]
    extern property_dunder_delete
    lea rdx, [rel property_dunder_delete]
    call dict_add_builtin_func
    extern property_type
    lea rax, [rel property_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- getset_descriptor: a descriptor by NAME, not only by slot ---
    ; The stdlib decides what a descriptor is by asking hasattr(v, '__get__')
    ; -- inspect.isdatadescriptor and the enum and dataclasses classifiers all
    ; walk a __dict__ and test exactly that.  getset_descr_type had no
    ; tp_dict, so every getset in the tree answered False.
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern getset_descr_dunder_get
    lea rdx, [rel getset_descr_dunder_get]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___set__]
    extern getset_descr_dunder_set
    lea rdx, [rel getset_descr_dunder_set]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___delete__]
    extern getset_descr_dunder_delete
    lea rdx, [rel getset_descr_dunder_delete]
    call dict_add_builtin_func
    extern getset_descr_type
    lea rax, [rel getset_descr_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- builtin_function_or_method: a NON-data descriptor by name ---
    ; __get__ and no __set__ is how inspect and the enum and dataclasses
    ; classifiers tell a method from a getset.
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn___get__]
    extern builtin_func_dunder_get
    lea rdx, [rel builtin_func_dunder_get]
    call dict_add_builtin_func
    extern builtin_func_type
    lea rax, [rel builtin_func_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- int_type methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel int_dunder_repr]
    call dict_add_builtin_func

    ; int.__new__ / str.__new__: enum builds each member with
    ; `member_type.__new__(cls, *args)`, and decides which base is the data
    ; type by asking whether __new__ is in its __dict__.
    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    lea rsi, [rel mn_bit_length]
    lea rdx, [rel int_method_bit_length]
    call dict_add_builtin_func

    ; The names dir(int) was short of.  __round__ IS builtin_round_fn: a
    ; method's (args, nargs) is the shape round()'s own arguments arrive in.
    mov rdi, rbx
    lea rsi, [rel mn_is_integer]
    extern int_method_is_integer
    lea rdx, [rel int_method_is_integer]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_as_integer_ratio]
    extern int_method_as_integer_ratio
    lea rdx, [rel int_method_as_integer_ratio]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___round__]
    extern int_method_round
    lea rdx, [rel int_method_round]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___floor__]
    extern int_method_identity
    lea rdx, [rel int_method_identity]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___ceil__]
    lea rdx, [rel int_method_identity]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___getnewargs__]
    extern int_method_getnewargs
    lea rdx, [rel int_method_getnewargs]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_bit_count]
    lea rdx, [rel int_method_bit_count]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_conjugate]
    lea rdx, [rel int_method_conjugate]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_to_bytes]
    lea rdx, [rel int_method_to_bytes]
    call dict_add_builtin_func


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

    ;; The unary operators, by name.  Without these an MRO lookup for
    ;; __invert__ on `class I(int, M)` could not find int's before M's, since
    ;; int had nothing in its dict to find, and type_install_slots wrote M's
    ;; wrapper over the nb_invert the class had already inherited.
    mov rdi, rbx
    lea rsi, [rel mn___neg__]
    extern int_dunder_neg
    lea rdx, [rel int_dunder_neg]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___pos__]
    extern int_dunder_pos
    lea rdx, [rel int_dunder_pos]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___abs__]
    extern int_dunder_abs
    lea rdx, [rel int_dunder_abs]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___invert__]
    extern int_dunder_invert
    lea rdx, [rel int_dunder_invert]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___int__]
    extern int_dunder_int
    lea rdx, [rel int_dunder_int]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___float__]
    extern int_dunder_float
    lea rdx, [rel int_dunder_float]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___index__]
    extern int_dunder_index
    lea rdx, [rel int_dunder_index]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___trunc__]
    extern int_dunder_trunc
    lea rdx, [rel int_dunder_trunc]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___bool__]
    extern int_dunder_bool
    lea rdx, [rel int_dunder_bool]
    call dict_add_builtin_func

    ;; and the binary family, forward and reflected.
    mov rdi, rbx
    lea rsi, [rel mn___add__]
    extern int_dunder_add
    lea rdx, [rel int_dunder_add]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___sub__]
    extern int_dunder_sub
    lea rdx, [rel int_dunder_sub]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mul__]
    extern int_dunder_mul
    lea rdx, [rel int_dunder_mul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mod__]
    extern int_dunder_mod
    lea rdx, [rel int_dunder_mod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___divmod__]
    extern int_dunder_divmod
    lea rdx, [rel int_dunder_divmod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___pow__]
    extern int_dunder_pow
    lea rdx, [rel int_dunder_pow]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___lshift__]
    extern int_dunder_lshift
    lea rdx, [rel int_dunder_lshift]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rshift__]
    extern int_dunder_rshift
    lea rdx, [rel int_dunder_rshift]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___and__]
    extern int_dunder_and
    lea rdx, [rel int_dunder_and]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___xor__]
    extern int_dunder_xor
    lea rdx, [rel int_dunder_xor]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___or__]
    extern int_dunder_or
    lea rdx, [rel int_dunder_or]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___floordiv__]
    extern int_dunder_floordiv
    lea rdx, [rel int_dunder_floordiv]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___truediv__]
    extern int_dunder_truediv
    lea rdx, [rel int_dunder_truediv]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___radd__]
    extern int_dunder_radd
    lea rdx, [rel int_dunder_radd]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rsub__]
    extern int_dunder_rsub
    lea rdx, [rel int_dunder_rsub]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmul__]
    extern int_dunder_rmul
    lea rdx, [rel int_dunder_rmul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmod__]
    extern int_dunder_rmod
    lea rdx, [rel int_dunder_rmod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rdivmod__]
    extern int_dunder_rdivmod
    lea rdx, [rel int_dunder_rdivmod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rpow__]
    extern int_dunder_rpow
    lea rdx, [rel int_dunder_rpow]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rlshift__]
    extern int_dunder_rlshift
    lea rdx, [rel int_dunder_rlshift]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rrshift__]
    extern int_dunder_rrshift
    lea rdx, [rel int_dunder_rrshift]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rand__]
    extern int_dunder_rand
    lea rdx, [rel int_dunder_rand]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rxor__]
    extern int_dunder_rxor
    lea rdx, [rel int_dunder_rxor]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___ror__]
    extern int_dunder_ror
    lea rdx, [rel int_dunder_ror]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rfloordiv__]
    extern int_dunder_rfloordiv
    lea rdx, [rel int_dunder_rfloordiv]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rtruediv__]
    extern int_dunder_rtruediv
    lea rdx, [rel int_dunder_rtruediv]
    call dict_add_builtin_func

    ;; real, imag, numerator and denominator, as getset descriptors.  int's
    ;; tp_getattr answers an instance read before this dict is consulted;
    ;; these are what make `int.real` an attribute of the type, and what put
    ;; the four names in dir().
    mov rdi, rbx
    lea rsi, [rel gs_real]
    extern int_get_real
    lea rdx, [rel int_get_real]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_numerator]
    lea rdx, [rel int_get_real]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_imag]
    extern int_get_imag
    lea rdx, [rel int_get_imag]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_denominator]
    extern int_get_denominator
    lea rdx, [rel int_get_denominator]
    xor ecx, ecx
    call dict_add_getset

    ; Store in int_type.tp_dict
    mov rdi, rbx
    lea rsi, [rel mn___format__]
    lea rdx, [rel builtin_method_format]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    extern int_dunder_hash
    lea rdx, [rel int_dunder_hash]
    call dict_add_builtin_func

    lea rax, [rel int_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- complex_type methods ---
    ;
    ; __repr__ goes through DEF_DUNDER_STRREPR, which calls the DEFINING
    ; type's slot rather than the argument's; the naive form recurses on a
    ; subclass (bugs.md).  There is deliberately no __str__: CPython's complex
    ; has no tp_str of its own, so `complex.__str__ is object.__str__` is True
    ; there, and leaving it out reproduces that while tp_str = complex_repr
    ; still keeps print(2j) fast.
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel complex_dunder_repr]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    lea rsi, [rel mn_conjugate]
    lea rdx, [rel complex_method_conjugate]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___complex__]
    lea rdx, [rel complex_method_complex]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___getnewargs__]
    lea rdx, [rel complex_method_getnewargs]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___format__]
    lea rdx, [rel builtin_method_format]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel gs_real]
    extern complex_get_real
    lea rdx, [rel complex_get_real]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_imag]
    extern complex_get_imag
    lea rdx, [rel complex_get_imag]
    xor ecx, ecx
    call dict_add_getset

    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    extern complex_dunder_hash
    lea rdx, [rel complex_dunder_hash]
    call dict_add_builtin_func

    lea rax, [rel complex_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- float_type methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel float_dunder_repr]
    call dict_add_builtin_func

    ; float.__new__, for the same reason int and str carry one: a subclass
    ; that overrides __new__ reaches the base's through super(), and enum
    ; looks the name up in __dict__ to pick its data type.
    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    lea rsi, [rel mn_is_integer]
    lea rdx, [rel float_method_is_integer]
    call dict_add_builtin_func

    ; float's four.  __floor__ and __ceil__ do exactly what MATH_ROUNDER's
    ; native arm does, because adding them newly routes a float SUBCLASS
    ; instance through the dunder: that arm reaches only an immediate.
    mov rdi, rbx
    lea rsi, [rel mn___round__]
    lea rdx, [rel int_method_round]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___floor__]
    extern float_method_floor
    lea rdx, [rel float_method_floor]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___ceil__]
    extern float_method_ceil
    lea rdx, [rel float_method_ceil]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___getnewargs__]
    extern float_method_getnewargs
    lea rdx, [rel float_method_getnewargs]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_conjugate]
    lea rdx, [rel float_method_conjugate]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_as_integer_ratio]
    lea rdx, [rel float_method_as_integer_ratio]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_hex]
    lea rdx, [rel float_method_hex]
    call dict_add_builtin_func


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

    mov rdi, rbx
    lea rsi, [rel mn___format__]
    lea rdx, [rel builtin_method_format]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___neg__]
    extern float_dunder_neg
    lea rdx, [rel float_dunder_neg]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___pos__]
    extern float_dunder_pos
    lea rdx, [rel float_dunder_pos]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___abs__]
    extern float_dunder_abs
    lea rdx, [rel float_dunder_abs]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___int__]
    extern float_dunder_int
    lea rdx, [rel float_dunder_int]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___float__]
    extern float_dunder_float
    lea rdx, [rel float_dunder_float]
    call dict_add_builtin_func

    ;; the binary family, forward and reflected.
    mov rdi, rbx
    lea rsi, [rel mn___add__]
    extern float_dunder_add
    lea rdx, [rel float_dunder_add]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___sub__]
    extern float_dunder_sub
    lea rdx, [rel float_dunder_sub]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mul__]
    extern float_dunder_mul
    lea rdx, [rel float_dunder_mul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mod__]
    extern float_dunder_mod
    lea rdx, [rel float_dunder_mod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___divmod__]
    extern float_dunder_divmod
    lea rdx, [rel float_dunder_divmod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___pow__]
    extern float_dunder_pow
    lea rdx, [rel float_dunder_pow]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___floordiv__]
    extern float_dunder_floordiv
    lea rdx, [rel float_dunder_floordiv]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___truediv__]
    extern float_dunder_truediv
    lea rdx, [rel float_dunder_truediv]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___radd__]
    extern float_dunder_radd
    lea rdx, [rel float_dunder_radd]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rsub__]
    extern float_dunder_rsub
    lea rdx, [rel float_dunder_rsub]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmul__]
    extern float_dunder_rmul
    lea rdx, [rel float_dunder_rmul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmod__]
    extern float_dunder_rmod
    lea rdx, [rel float_dunder_rmod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rdivmod__]
    extern float_dunder_rdivmod
    lea rdx, [rel float_dunder_rdivmod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rpow__]
    extern float_dunder_rpow
    lea rdx, [rel float_dunder_rpow]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rfloordiv__]
    extern float_dunder_rfloordiv
    lea rdx, [rel float_dunder_rfloordiv]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rtruediv__]
    extern float_dunder_rtruediv
    lea rdx, [rel float_dunder_rtruediv]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___trunc__]
    extern float_dunder_trunc
    lea rdx, [rel float_dunder_trunc]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___bool__]
    extern float_dunder_bool
    lea rdx, [rel float_dunder_bool]
    call dict_add_builtin_func

    ; real and imag; float has no numerator or denominator, as CPython has not
    mov rdi, rbx
    lea rsi, [rel gs_real]
    extern float_get_real
    lea rdx, [rel float_get_real]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_imag]
    extern float_get_imag
    lea rdx, [rel float_get_imag]
    xor ecx, ecx
    call dict_add_getset

    ; Store in float_type.tp_dict
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    extern float_dunder_hash
    lea rdx, [rel float_dunder_hash]
    call dict_add_builtin_func

    lea rax, [rel float_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- bytes_type methods (extend tp_dict, keep tp_getattr for .decode()) ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn___str__]
    lea rdx, [rel bytes_dunder_str]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___repr__]
    lea rdx, [rel bytes_dunder_repr]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_hex]
    lea rdx, [rel bytes_method_hex]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_startswith]
    lea rdx, [rel bytes_method_startswith]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_endswith]
    lea rdx, [rel bytes_method_endswith]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel bytes_method_count]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_find]
    lea rdx, [rel bytes_method_find]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_replace]
    lea rdx, [rel bytes_method_replace]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_split]
    lea rdx, [rel bytes_method_split]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_rsplit]
    extern bytes_method_rsplit
    lea rdx, [rel bytes_method_rsplit]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_rfind]
    extern bytes_method_rfind
    lea rdx, [rel bytes_method_rfind]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_index]
    extern bytes_method_index
    lea rdx, [rel bytes_method_index]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rindex]
    extern bytes_method_rindex
    lea rdx, [rel bytes_method_rindex]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_strip]
    extern bytes_method_strip
    lea rdx, [rel bytes_method_strip]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_lstrip]
    extern bytes_method_lstrip
    lea rdx, [rel bytes_method_lstrip]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rstrip]
    extern bytes_method_rstrip
    lea rdx, [rel bytes_method_rstrip]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_partition]
    extern bytes_method_partition
    lea rdx, [rel bytes_method_partition]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rpartition]
    extern bytes_method_rpartition
    lea rdx, [rel bytes_method_rpartition]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_upper]
    extern bytes_method_upper
    lea rdx, [rel bytes_method_upper]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_lower]
    extern bytes_method_lower
    lea rdx, [rel bytes_method_lower]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_swapcase]
    extern bytes_method_swapcase
    lea rdx, [rel bytes_method_swapcase]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_capitalize]
    extern bytes_method_capitalize
    lea rdx, [rel bytes_method_capitalize]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_title]
    extern bytes_method_title
    lea rdx, [rel bytes_method_title]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isalpha]
    extern bytes_method_isalpha
    lea rdx, [rel bytes_method_isalpha]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isdigit]
    extern bytes_method_isdigit
    lea rdx, [rel bytes_method_isdigit]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isspace]
    extern bytes_method_isspace
    lea rdx, [rel bytes_method_isspace]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isalnum]
    extern bytes_method_isalnum
    lea rdx, [rel bytes_method_isalnum]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isascii]
    extern bytes_method_isascii
    lea rdx, [rel bytes_method_isascii]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isupper]
    extern bytes_method_isupper
    lea rdx, [rel bytes_method_isupper]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_islower]
    extern bytes_method_islower
    lea rdx, [rel bytes_method_islower]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_istitle]
    extern bytes_method_istitle
    lea rdx, [rel bytes_method_istitle]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_ljust]
    extern bytes_method_ljust
    lea rdx, [rel bytes_method_ljust]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rjust]
    extern bytes_method_rjust
    lea rdx, [rel bytes_method_rjust]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_center]
    extern bytes_method_center
    lea rdx, [rel bytes_method_center]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_zfill]
    extern bytes_method_zfill
    lea rdx, [rel bytes_method_zfill]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_expandtabs]
    extern bytes_method_expandtabs
    lea rdx, [rel bytes_method_expandtabs]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_translate]
    extern bytes_method_translate
    lea rdx, [rel bytes_method_translate]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_splitlines]
    extern bytes_method_splitlines
    lea rdx, [rel bytes_method_splitlines]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_removeprefix]
    extern bytes_method_removeprefix
    lea rdx, [rel bytes_method_removeprefix]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_removesuffix]
    extern bytes_method_removesuffix
    lea rdx, [rel bytes_method_removesuffix]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_join]
    lea rdx, [rel bytes_method_join]
    call dict_add_builtin_func

    ; Store in bytes_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel bytes_dunder_len]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel bytes_dunder_iter]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn_maketrans]
    extern bytes_staticmethod_maketrans
    lea rdx, [rel bytes_staticmethod_maketrans]
    call add_staticmethod

    ; The operators, by name.
    mov rdi, rbx
    lea rsi, [rel mn___add__]
    lea rdx, [rel bytes_dunder_add]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mul__]
    lea rdx, [rel bytes_dunder_mul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmul__]
    lea rdx, [rel bytes_dunder_rmul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mod__]
    lea rdx, [rel bytes_dunder_mod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmod__]
    lea rdx, [rel bytes_dunder_rmod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___contains__]
    lea rdx, [rel generic_method_contains]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___getitem__]
    lea rdx, [rel bytes_dunder_getitem]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    extern bytes_dunder_hash
    lea rdx, [rel bytes_dunder_hash]
    call dict_add_builtin_func

    lea rax, [rel bytes_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- bytearray_type methods ---
    ;; It had none at all: tp_getattr was 0 and tp_dict was empty, so a
    ;; bytearray had no append, no find, not even __setitem__ by name.  The
    ;; mutators are its own; the read-only ones are bytes', reached through a
    ;; wrapper that hands the bytes body a temporary bytes -- see
    ;; bytearray_shared_call in src/methods/bytes.asm for why that is the
    ;; cheap answer here.
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn_append]
    lea rdx, [rel bytearray_method_append]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_extend]
    lea rdx, [rel bytearray_method_extend]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_insert]
    lea rdx, [rel bytearray_method_insert]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel bytearray_method_pop]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_remove]
    lea rdx, [rel bytearray_method_remove]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel bytearray_method_clear]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_reverse]
    lea rdx, [rel bytearray_method_reverse]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_copy]
    lea rdx, [rel bytearray_method_copy]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_hex]
    lea rdx, [rel ba_shared_hex]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_startswith]
    lea rdx, [rel ba_shared_startswith]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_endswith]
    lea rdx, [rel ba_shared_endswith]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel ba_shared_count]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_find]
    lea rdx, [rel ba_shared_find]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_replace]
    lea rdx, [rel ba_shared_replace]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_split]
    lea rdx, [rel ba_shared_split]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rsplit]
    extern ba_shared_rsplit
    lea rdx, [rel ba_shared_rsplit]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rfind]
    extern ba_shared_rfind
    lea rdx, [rel ba_shared_rfind]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_index]
    extern ba_shared_index
    lea rdx, [rel ba_shared_index]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rindex]
    extern ba_shared_rindex
    lea rdx, [rel ba_shared_rindex]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_strip]
    extern ba_shared_strip
    lea rdx, [rel ba_shared_strip]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_lstrip]
    extern ba_shared_lstrip
    lea rdx, [rel ba_shared_lstrip]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rstrip]
    extern ba_shared_rstrip
    lea rdx, [rel ba_shared_rstrip]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_partition]
    extern ba_shared_partition
    lea rdx, [rel ba_shared_partition]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rpartition]
    extern ba_shared_rpartition
    lea rdx, [rel ba_shared_rpartition]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_upper]
    extern ba_shared_upper
    lea rdx, [rel ba_shared_upper]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_lower]
    extern ba_shared_lower
    lea rdx, [rel ba_shared_lower]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_swapcase]
    extern ba_shared_swapcase
    lea rdx, [rel ba_shared_swapcase]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_capitalize]
    extern ba_shared_capitalize
    lea rdx, [rel ba_shared_capitalize]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_title]
    extern ba_shared_title
    lea rdx, [rel ba_shared_title]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isalpha]
    extern ba_shared_isalpha
    lea rdx, [rel ba_shared_isalpha]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isdigit]
    extern ba_shared_isdigit
    lea rdx, [rel ba_shared_isdigit]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isspace]
    extern ba_shared_isspace
    lea rdx, [rel ba_shared_isspace]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isalnum]
    extern ba_shared_isalnum
    lea rdx, [rel ba_shared_isalnum]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isascii]
    extern ba_shared_isascii
    lea rdx, [rel ba_shared_isascii]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_isupper]
    extern ba_shared_isupper
    lea rdx, [rel ba_shared_isupper]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_islower]
    extern ba_shared_islower
    lea rdx, [rel ba_shared_islower]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_istitle]
    extern ba_shared_istitle
    lea rdx, [rel ba_shared_istitle]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_ljust]
    extern ba_shared_ljust
    lea rdx, [rel ba_shared_ljust]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_rjust]
    extern ba_shared_rjust
    lea rdx, [rel ba_shared_rjust]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_center]
    extern ba_shared_center
    lea rdx, [rel ba_shared_center]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_zfill]
    extern ba_shared_zfill
    lea rdx, [rel ba_shared_zfill]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_expandtabs]
    extern ba_shared_expandtabs
    lea rdx, [rel ba_shared_expandtabs]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_translate]
    extern ba_shared_translate
    lea rdx, [rel ba_shared_translate]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_splitlines]
    extern ba_shared_splitlines
    lea rdx, [rel ba_shared_splitlines]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_removeprefix]
    extern ba_shared_removeprefix
    lea rdx, [rel ba_shared_removeprefix]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_removesuffix]
    extern ba_shared_removesuffix
    lea rdx, [rel ba_shared_removesuffix]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_join]
    lea rdx, [rel ba_shared_join]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_decode]
    lea rdx, [rel ba_shared_decode]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel bytearray_dunder_len]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___iter__]
    lea rdx, [rel bytearray_dunder_iter]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___setitem__]
    lea rdx, [rel bytearray_dunder_setitem]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___delitem__]
    lea rdx, [rel bytearray_dunder_delitem]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___getitem__]
    lea rdx, [rel bytearray_dunder_getitem]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___contains__]
    lea rdx, [rel bytearray_dunder_contains]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_maketrans]
    lea rdx, [rel bytes_staticmethod_maketrans]
    call add_staticmethod

    ; The operators, by name.
    mov rdi, rbx
    lea rsi, [rel mn___add__]
    lea rdx, [rel bytearray_dunder_add]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mul__]
    lea rdx, [rel bytearray_dunder_mul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmul__]
    lea rdx, [rel bytearray_dunder_rmul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___iadd__]
    lea rdx, [rel bytearray_dunder_iadd]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___imul__]
    lea rdx, [rel bytearray_dunder_imul]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___mod__]
    lea rdx, [rel bytearray_dunder_mod]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___rmod__]
    lea rdx, [rel bytearray_dunder_rmod]
    call dict_add_builtin_func

    ; Unhashable: the name has to BE None, not resolve to object's.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    call dict_add_none

    lea rax, [rel bytearray_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- memoryview_type methods ---
    ;; It had none: tp_getattr was 0 and tp_dict was empty.  _pyio calls
    ;; tobytes and cast, and wraps every readinto in `with memoryview(b)`.
    call dict_new
    mov rbx, rax
    mov rdi, rbx
    lea rsi, [rel mn_tobytes]
    lea rdx, [rel memoryview_method_tobytes]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_tolist]
    lea rdx, [rel memoryview_method_tolist]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_cast]
    lea rdx, [rel memoryview_method_cast]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_release]
    lea rdx, [rel memoryview_method_release]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___enter__]
    lea rdx, [rel memoryview_method_enter]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___exit__]
    lea rdx, [rel memoryview_method_exit]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn_hex]
    lea rdx, [rel memoryview_method_hex]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___getitem__]
    lea rdx, [rel memoryview_dunder_getitem]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___setitem__]
    lea rdx, [rel memoryview_dunder_setitem]
    call dict_add_builtin_func
    mov rdi, rbx
    lea rsi, [rel mn___len__]
    lea rdx, [rel memoryview_dunder_len]
    call dict_add_builtin_func
    lea rax, [rel memoryview_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

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
mn_conjugate:   db "conjugate", 0
mn___round__:  db "__round__", 0
mn___floor__:  db "__floor__", 0
mn___ceil__:   db "__ceil__", 0
mn___getnewargs__: db "__getnewargs__", 0
mn___complex__: db "__complex__", 0
; float method names
mn_is_integer:  db "is_integer", 0
mn_as_integer_ratio: db "as_integer_ratio", 0
; float method names (continued)
mn_fromhex:     db "fromhex", 0
; bytes method names
mn_decode:            db "decode", 0
mn_tobytes:          db "tobytes", 0
mn_tolist:           db "tolist", 0
mn_cast:             db "cast", 0
mn_release:          db "release", 0
mn___enter__:        db "__enter__", 0
mn___exit__:         db "__exit__", 0
mn_hex:         db "hex", 0
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
mn___lt__:      db "__lt__", 0
mn___le__:      db "__le__", 0
mn___gt__:      db "__gt__", 0
mn___ge__:      db "__ge__", 0
mn___neg__:     db "__neg__", 0
mn___pos__:     db "__pos__", 0
mn___abs__:     db "__abs__", 0
mn___invert__:  db "__invert__", 0
mn___int__:     db "__int__", 0
mn___float__:   db "__float__", 0
mn___index__:   db "__index__", 0
mn___trunc__:   db "__trunc__", 0
mn___bool__:    db "__bool__", 0
gs_real:        db "real", 0
gs_imag:        db "imag", 0
gs_numerator:   db "numerator", 0
gs_denominator: db "denominator", 0
mn___eq__: db "__eq__", 0
mn___ne__: db "__ne__", 0
mn___hash__:    db "__hash__", 0
mn___subclasshook__: db "__subclasshook__", 0
mn___getstate__: db "__getstate__", 0
mn___getattribute__: db "__getattribute__", 0
mn___delattr__: db "__delattr__", 0
mn___setattr__: db "__setattr__", 0
mn___ior__: db "__ior__", 0
mn___imul__: db "__imul__", 0
mn___subclasses__: db "__subclasses__", 0
mn___add__:     db "__add__", 0
mn___radd__: db "__radd__", 0
mn___sub__: db "__sub__", 0
mn___rsub__: db "__rsub__", 0
mn___mod__: db "__mod__", 0
mn___rmod__: db "__rmod__", 0
mn___divmod__: db "__divmod__", 0
mn___rdivmod__: db "__rdivmod__", 0
mn___pow__: db "__pow__", 0
mn___rpow__: db "__rpow__", 0
mn___lshift__: db "__lshift__", 0
mn___rlshift__: db "__rlshift__", 0
mn___rshift__: db "__rshift__", 0
mn___rrshift__: db "__rrshift__", 0
mn___and__: db "__and__", 0
mn___rand__: db "__rand__", 0
mn___xor__: db "__xor__", 0
mn___rxor__: db "__rxor__", 0
mn___or__: db "__or__", 0
mn___ror__: db "__ror__", 0
mn___floordiv__: db "__floordiv__", 0
mn___rfloordiv__: db "__rfloordiv__", 0
mn___truediv__: db "__truediv__", 0
mn___rtruediv__: db "__rtruediv__", 0
mn___mul__:     db "__mul__", 0
mn___rmul__:    db "__rmul__", 0
mn___iadd__:    db "__iadd__", 0
mn___init__:    db "__init__", 0
mn___str__:     db "__str__", 0
mn___code__:    db "__code__", 0
mn___class_getitem__: db "__class_getitem__", 0
mn___globals__: db "__globals__", 0
mn___repr__:    db "__repr__", 0

;; The set method names set_add_operator_methods walks, in order, and one
;; function table per type.  Data rather than forty open-coded registrations
;; twice over.
align 8
set_operator_names:
    dq mn___len__
    dq mn___iter__
    dq mn___sub__
    dq mn___and__
    dq mn___xor__
    dq mn___or__
    dq mn___rsub__
    dq mn___rand__
    dq mn___rxor__
    dq mn___ror__

set_operator_fns:
    dq set_dunder_len
    dq set_dunder_iter
    dq set_dunder_sub
    dq set_dunder_and
    dq set_dunder_xor
    dq set_dunder_or
    dq set_dunder_rsub
    dq set_dunder_rand
    dq set_dunder_rxor
    dq set_dunder_ror

frozenset_operator_fns:
    dq frozenset_dunder_len
    dq frozenset_dunder_iter
    dq frozenset_dunder_sub
    dq frozenset_dunder_and
    dq frozenset_dunder_xor
    dq frozenset_dunder_or
    dq frozenset_dunder_rsub
    dq frozenset_dunder_rand
    dq frozenset_dunder_rxor
    dq frozenset_dunder_ror
