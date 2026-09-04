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
extern set_dunder_iand
extern set_dunder_ior
extern set_dunder_isub
extern set_dunder_ixor
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
extern set_method_intersection_update
extern set_method_difference_update
extern set_method_symmetric_difference_update
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


;; ADD_FN name, func -- register one method into the dict in rbx.
;; ADD_FN_N name, func, min, max -- the same, with argument-count bounds.
;;
;; These were open-coded: four instructions per method, six for a checked one,
;; 455 times, which is most of what made this file 115k.  The expansion is
;; identical -- the object file is unchanged to the byte.
%macro ADD_FN 2
    mov rdi, rbx
    lea rsi, [rel %1]
    lea rdx, [rel %2]
    call dict_add_builtin_func
%endmacro

;; ADD_FN_D slot, name, func -- the same, for the few blocks whose dict is in a
;; frame slot rather than in rbx.
%macro ADD_FN_D 3
    mov rdi, [rbp - %1]
    lea rsi, [rel %2]
    lea rdx, [rel %3]
    call dict_add_builtin_func
%endmacro

%macro ADD_FN_N 4
    mov rdi, rbx
    lea rsi, [rel %1]
    lea rdx, [rel %2]
    mov rcx, %3
    mov r8, %4
    call add_method_to_dict_checked
%endmacro

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

DEF_FUNC dict_add_builtin_func, 8            ; 3 pushes, so rsp is 16-aligned
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
DEF_FUNC_LOCAL add_method_to_dict_checked, 8            ; 3 pushes, so rsp is 16-aligned
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
DEF_FUNC_LOCAL add_new_staticmethod, 8            ; 3 pushes, so rsp is 16-aligned
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

    ADD_FN_D SASM_DICT, mn_copy, set_method_copy

    ADD_FN_D SASM_DICT, mn_union, set_method_union

    ADD_FN_D SASM_DICT, mn_intersection, set_method_intersection

    ADD_FN_D SASM_DICT, mn_difference, set_method_difference

    ADD_FN_D SASM_DICT, mn_symmetric_difference, set_method_symmetric_difference

    ADD_FN_D SASM_DICT, mn_issubset, set_method_issubset

    ADD_FN_D SASM_DICT, mn_issuperset, set_method_issuperset

    ADD_FN_D SASM_DICT, mn_isdisjoint, set_method_isdisjoint

    ADD_FN_D SASM_DICT, mn___contains__, generic_method_contains

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

;; ADD_CLASSMETHOD name, impl -- the dict is in rbx, as everywhere else here.
%macro ADD_CLASSMETHOD 2
    lea rdi, [rel %2]
    lea rsi, [rel %1]
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
    lea rdi, [rel %1]
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
%endmacro

%macro GEN_GETSET 2             ; %1 = the name string, %2 = the getter
    mov rdi, rbx
    lea rsi, [rel %1]
    extern %2
    lea rdx, [rel %2]
    xor ecx, ecx
    call dict_add_getset
%endmacro

DEF_FUNC methods_init
    push rbx
    push r12

    ;; --- str methods ---
    call dict_new
    mov rbx, rax            ; rbx = str method dict

    ; str's own __str__ and __repr__, by name.  See DEF_DUNDER_STRREPR.
    ADD_FN mn___str__, str_dunder_str
    ADD_FN mn___repr__, str_dunder_repr

    ; int.__new__ / str.__new__: enum builds each member with
    ; `member_type.__new__(cls, *args)`, and decides which base is the data
    ; type by asking whether __new__ is in its __dict__.
    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    ADD_FN mn_upper, str_method_upper

    ADD_FN mn_lower, str_method_lower

    ADD_FN mn_strip, str_method_strip

    ADD_FN mn_startswith, str_method_startswith

    ADD_FN mn_endswith, str_method_endswith

    ADD_FN mn_find, str_method_find

    ADD_FN mn_replace, str_method_replace

    ADD_FN mn_join, str_method_join

    ADD_FN mn_split, str_method_split

    ADD_FN mn_format, str_method_format

    ADD_FN mn_lstrip, str_method_lstrip

    ADD_FN mn_rstrip, str_method_rstrip

    ADD_FN mn_count, str_method_count

    ADD_FN mn_index, str_method_index

    ADD_FN mn_rfind, str_method_rfind

    ADD_FN mn_isdigit, str_method_isdigit

    ADD_FN mn_isalpha, str_method_isalpha

    ADD_FN mn_isidentifier, str_method_isidentifier

    ADD_FN mn_isprintable, str_method_isprintable

    ADD_FN mn_isascii, str_method_isascii

    ADD_FN mn_isdecimal, str_method_isdecimal

    extern str_method_isnumeric
    ADD_FN mn_isnumeric, str_method_isnumeric

    ADD_FN mn_removeprefix, str_method_removeprefix

    ADD_FN mn_removesuffix, str_method_removesuffix

    ADD_FN mn_encode, str_method_encode

    ADD_FN mn_isalnum, str_method_isalnum

    ADD_FN mn_isspace, str_method_isspace

    ADD_FN mn_isupper, str_method_isupper

    ADD_FN mn_islower, str_method_islower

    ADD_FN mn_title, str_method_title

    ADD_FN mn_capitalize, str_method_capitalize

    ADD_FN mn_swapcase, str_method_swapcase

    ADD_FN mn_casefold, str_method_casefold

    ADD_FN mn_center, str_method_center

    ADD_FN mn_ljust, str_method_ljust

    ADD_FN mn_rjust, str_method_rjust

    ADD_FN mn_zfill, str_method_zfill

    ADD_FN mn_rindex, str_method_rindex

    ADD_FN mn_istitle, str_method_istitle

    ADD_FN mn_partition, str_method_partition

    ADD_FN mn_rpartition, str_method_rpartition

    ADD_FN mn_rsplit, str_method_rsplit

    ADD_FN mn_splitlines, str_method_splitlines

    ADD_FN mn_expandtabs, str_method_expandtabs

    ADD_FN mn_translate, str_method_translate

    ADD_FN mn_format_map, str_method_format_map

    mov rdi, rbx
    lea rsi, [rel mn_maketrans]
    lea rdx, [rel str_staticmethod_maketrans]
    call add_staticmethod

    ; Store dict in str_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    ADD_FN mn___len__, str_dunder_len
    ADD_FN mn___iter__, str_dunder_iter

    ADD_FN mn___format__, builtin_method_format

    ; The operators, by name.
    ADD_FN mn___add__, str_dunder_add
    ADD_FN mn___mul__, str_dunder_mul
    ADD_FN mn___rmul__, str_dunder_rmul
    ADD_FN mn___mod__, str_dunder_mod
    ADD_FN mn___rmod__, str_dunder_rmod
    ADD_FN mn___getitem__, str_dunder_getitem
    ADD_FN mn___contains__, generic_method_contains

    extern str_dunder_hash
    ADD_FN mn___hash__, str_dunder_hash

    extern str_dunder_lt
    ADD_FN mn___lt__, str_dunder_lt
    extern str_dunder_le
    ADD_FN mn___le__, str_dunder_le
    extern str_dunder_gt
    ADD_FN mn___gt__, str_dunder_gt
    extern str_dunder_ge
    ADD_FN mn___ge__, str_dunder_ge
    extern str_dunder_eq
    ADD_FN mn___eq__, str_dunder_eq
    extern str_dunder_ne
    ADD_FN mn___ne__, str_dunder_ne

    lea rax, [rel str_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- range's own dict ---
    ;; range had none: `hasattr(range, "index")` was False, and so was
    ;; `hasattr(range, "__len__")`, because every one of these lived only in
    ;; a slot.  A range is a value and the stdlib treats it as one.
    call dict_new
    mov rbx, rax

    extern range_method_index
    ADD_FN mn_index, range_method_index
    extern range_method_count
    ADD_FN mn_count, range_method_count
    extern range_dunder_getitem
    ADD_FN mn___getitem__, range_dunder_getitem
    extern range_dunder_reversed
    ADD_FN mn___reversed__, range_dunder_reversed
    ADD_FN mn___contains__, generic_method_contains
    extern range_dunder_len
    ADD_FN mn___len__, range_dunder_len
    extern range_dunder_iter
    ADD_FN mn___iter__, range_dunder_iter
    extern range_dunder_eq
    ADD_FN mn___eq__, range_dunder_eq
    extern range_dunder_ne
    ADD_FN mn___ne__, range_dunder_ne
    extern range_dunder_hash
    ADD_FN mn___hash__, range_dunder_hash

    ; start, stop and step are read-only: a range is immutable.
    mov rdi, rbx
    lea rsi, [rel gs_start]
    extern range_get_start
    lea rdx, [rel range_get_start]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_stop]
    extern range_get_stop
    lea rdx, [rel range_get_stop]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_step]
    extern range_get_step
    lea rdx, [rel range_get_step]
    xor ecx, ecx
    call dict_add_getset

    extern range_obj_type
    lea rax, [rel range_obj_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- slice's own dict ---
    ;; slice answered start/stop/step through its tp_getattr and had no dict
    ;; at all, so `hasattr(slice, "start")` was False and `slice.indices` did
    ;; not exist in either place.
    call dict_new
    mov rbx, rax

    extern slice_method_indices
    ADD_FN mn_indices, slice_method_indices
    mov rdi, rbx
    lea rsi, [rel gs_start]
    extern slice_get_start
    lea rdx, [rel slice_get_start]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_stop]
    extern slice_get_stop
    lea rdx, [rel slice_get_stop]
    xor ecx, ecx
    call dict_add_getset
    mov rdi, rbx
    lea rsi, [rel gs_step]
    extern slice_get_step
    lea rdx, [rel slice_get_step]
    xor ecx, ecx
    call dict_add_getset

    extern slice_type
    lea rax, [rel slice_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- generator and coroutine dicts ---
    ;; gen_type had no tp_dict, so `hasattr(gen, "__next__")` was False and
    ;; `it.__next__` an AttributeError.  CPython's threading.py does
    ;; `_counter = _count(1).__next__` at import, which is as far as it got.
    call dict_new
    mov rbx, rax

    extern builtin_next_fn
    ADD_FN mn___next__, builtin_next_fn
    extern gen_dunder_iter
    ADD_FN mn___iter__, gen_dunder_iter
    extern _gen_send_impl
    ADD_FN mn_send, _gen_send_impl
    extern _gen_throw_impl
    ADD_FN mn_throw, _gen_throw_impl
    extern _gen_close_impl
    ADD_FN mn_close, _gen_close_impl

    GEN_GETSET gs___name__,     gen_get_name
    GEN_GETSET gs___qualname__, gen_get_name
    ; gi_frame and cr_frame are NOT here: a PyFrame is pooled and recycled and
    ; is not an object with a type, so there is nothing to hand back.  Saying
    ; so by leaving the name absent beats answering None to a caller that is
    ; about to read f_lineno off it.
    GEN_GETSET gs_gi_code,      gen_get_code
    GEN_GETSET gs_gi_running,   gen_get_running

    extern gen_type
    lea rax, [rel gen_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; A coroutine is awaited rather than iterated, so it gets the same
    ;; three methods and the cr_* spellings of the same fields.
    call dict_new
    mov rbx, rax

    extern coro_dunder_iter
    ADD_FN mn___await__, coro_dunder_iter
    ADD_FN mn_send, _gen_send_impl
    ADD_FN mn_throw, _gen_throw_impl
    ADD_FN mn_close, _gen_close_impl

    GEN_GETSET gs___name__,     gen_get_name
    GEN_GETSET gs___qualname__, gen_get_name
    GEN_GETSET gs_cr_code,      gen_get_code
    GEN_GETSET gs_cr_running,   gen_get_running

    extern coro_type
    lea rax, [rel coro_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods
    ; INCREF the dict (type holds ref; dict_new gave us refcnt=1, which we keep)

    ;; --- list methods (with arg count validation) ---
    call dict_new
    mov rbx, rax

    ADD_FN_N mn_append, list_method_append, 2, 2

    ADD_FN_N mn_pop, list_method_pop, 1, 2

    ADD_FN_N mn_insert, list_method_insert, 3, 3

    ADD_FN_N mn_reverse, list_method_reverse, 1, 1

    ADD_FN mn_sort, list_method_sort

    ADD_FN_N mn_index, list_method_index, 2, -1

    ADD_FN_N mn_count, list_method_count, 2, 2

    ADD_FN_N mn_copy, list_method_copy, 1, 1

    ADD_FN_N mn_clear, list_method_clear, 1, 1

    ADD_FN_N mn_extend, list_method_extend, 2, 2

    ADD_FN_N mn_remove, list_method_remove, 2, 2

    ADD_FN mn___reversed__, list_method_reversed

    ;; list dunder methods
    ADD_FN_N mn___getitem__, list_dunder_getitem, 2, 2

    ADD_FN_N mn___setitem__, list_dunder_setitem, 3, 3

    ADD_FN_N mn___delitem__, list_dunder_delitem, 2, 2

    ADD_FN_N mn___contains__, list_dunder_contains, 2, 2

    ADD_FN_N mn___len__, list_dunder_len, 1, 1

    ADD_FN_N mn___iadd__, list_dunder_iadd, 2, 2

    ADD_FN_N mn___init__, list_dunder_init, 1, -1

    mov rdi, rbx
    lea rsi, [rel container_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    call add_class_getitem

    ; Store in list_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    ADD_FN mn___iter__, list_dunder_iter

    ; The operators, by name.  The slots were there and the names were not.
    ADD_FN mn___add__, list_dunder_add
    ADD_FN mn___mul__, list_dunder_mul
    ADD_FN mn___rmul__, list_dunder_rmul
    ADD_FN mn___imul__, list_dunder_imul

    ; Unhashable: the name has to BE None, not resolve to object's.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    call dict_add_none

    extern list_dunder_lt
    ADD_FN mn___lt__, list_dunder_lt
    extern list_dunder_le
    ADD_FN mn___le__, list_dunder_le
    extern list_dunder_gt
    ADD_FN mn___gt__, list_dunder_gt
    extern list_dunder_ge
    ADD_FN mn___ge__, list_dunder_ge
    extern list_dunder_eq
    ADD_FN mn___eq__, list_dunder_eq
    extern list_dunder_ne
    ADD_FN mn___ne__, list_dunder_ne

    lea rax, [rel list_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- dict methods ---
    call dict_new
    mov rbx, rax

    ADD_FN mn_get, dict_method_get

    ADD_FN mn_keys, dict_method_keys

    ADD_FN mn_values, dict_method_values

    ADD_FN mn_items, dict_method_items

    ADD_FN mn_pop, dict_method_pop

    ADD_FN mn_clear, dict_method_clear

    ADD_FN mn_update, dict_method_update

    ; dict() has no __init__ either; update() is the same operation.
    ADD_FN_N mn___init__, dict_method_update, 1, -1

    ADD_FN mn_setdefault, dict_method_setdefault

    ADD_FN mn_copy, dict_method_copy

    ADD_FN mn_popitem, dict_method_popitem

    extern dict_reversed
    ADD_FN mn___reversed__, dict_reversed

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

    ADD_FN mn___contains__, generic_method_contains

    ADD_FN mn___setitem__, dict_dunder_setitem

    ADD_FN mn___delitem__, dict_dunder_delitem

    ADD_FN mn___getitem__, dict_dunder_getitem

    mov rdi, rbx
    call add_class_getitem

    ; Store in dict_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    ADD_FN mn___len__, dict_dunder_len
    ADD_FN mn___iter__, dict_dunder_iter

    ; The union operators, by name.
    ADD_FN mn___or__, dict_dunder_or
    ADD_FN mn___ror__, dict_dunder_ror
    ADD_FN mn___ior__, dict_dunder_ior

    ; Unhashable: the name has to BE None, not resolve to object's.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    call dict_add_none

    extern dict_dunder_lt
    ADD_FN mn___lt__, dict_dunder_lt
    extern dict_dunder_le
    ADD_FN mn___le__, dict_dunder_le
    extern dict_dunder_gt
    ADD_FN mn___gt__, dict_dunder_gt
    extern dict_dunder_ge
    ADD_FN mn___ge__, dict_dunder_ge
    extern dict_dunder_eq
    ADD_FN mn___eq__, dict_dunder_eq
    extern dict_dunder_ne
    ADD_FN mn___ne__, dict_dunder_ne

    lea rax, [rel dict_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- tuple methods ---
    call dict_new
    mov rbx, rax

    ; Registered with arity checks: a.count() and u.index() with no argument
    ; must raise TypeError, which seq_tests asserts.
    ADD_FN_N mn_index, tuple_method_index, 2, 4

    ADD_FN_N mn_count, tuple_method_count, 2, 2

    ADD_FN_N mn___getitem__, tuple_dunder_getitem, 2, 2

    ADD_FN_N mn___contains__, tuple_dunder_contains, 2, 2

    ADD_FN_N mn___len__, tuple_dunder_len, 1, 1

    ADD_FN_N mn___add__, tuple_dunder_add, 2, 2

    ADD_FN_N mn___mul__, tuple_dunder_mul, 2, 2

    ADD_FN_N mn___rmul__, tuple_dunder_rmul, 2, 2

    mov rdi, rbx
    lea rsi, [rel container_dunder_new]
    call add_new_staticmethod

    mov rdi, rbx
    call add_class_getitem

    ; Store in tuple_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    ADD_FN mn___iter__, tuple_dunder_iter

    extern tuple_dunder_hash
    ADD_FN mn___hash__, tuple_dunder_hash

    extern tuple_dunder_lt
    ADD_FN mn___lt__, tuple_dunder_lt
    extern tuple_dunder_le
    ADD_FN mn___le__, tuple_dunder_le
    extern tuple_dunder_gt
    ADD_FN mn___gt__, tuple_dunder_gt
    extern tuple_dunder_ge
    ADD_FN mn___ge__, tuple_dunder_ge
    extern tuple_dunder_eq
    ADD_FN mn___eq__, tuple_dunder_eq
    extern tuple_dunder_ne
    ADD_FN mn___ne__, tuple_dunder_ne

    lea rax, [rel tuple_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- set methods ---
    call dict_new
    mov rbx, rax

    ADD_FN mn_add, set_method_add

    ADD_FN mn_remove, set_method_remove

    ADD_FN mn_discard, set_method_discard

    ADD_FN mn_pop, set_method_pop

    ADD_FN mn_clear, set_method_clear

    ADD_FN mn_update, set_method_update

    ; set() has no __init__, so a subclass had nothing to fill it from.
    ; update() already takes (self, iterable) and returns None.
    ADD_FN_N mn___init__, set_method_update, 1, -1

    mov rdi, rbx
    call set_add_shared_methods

    mov rdi, rbx
    lea rsi, [rel set_operator_fns]
    call set_add_operator_methods

    ; The mutating method forms, on set alone -- frozenset has nothing to
    ; update.  `update` itself is registered with the shared methods, because
    ; it doubles as set.__init__.
    ADD_FN mn_intersection_update, set_method_intersection_update
    ADD_FN mn_difference_update, set_method_difference_update
    ADD_FN mn_symmetric_difference_update, set_method_symmetric_difference_update

    ; The reflected four are registered with the forward four.  The in-place
    ; four go on set alone: they mutate, and frozenset cannot.  They are not
    ; in set_operator_names for exactly that reason -- that table is walked
    ; for both types.
    ADD_FN mn___iand__, set_dunder_iand
    ADD_FN mn___ior__, set_dunder_ior
    ADD_FN mn___isub__, set_dunder_isub
    ADD_FN mn___ixor__, set_dunder_ixor

    ; Unhashable: the name has to BE None, not resolve to object's.
    mov rdi, rbx
    lea rsi, [rel mn___hash__]
    call dict_add_none

    extern set_dunder_lt
    ADD_FN mn___lt__, set_dunder_lt
    extern set_dunder_le
    ADD_FN mn___le__, set_dunder_le
    extern set_dunder_gt
    ADD_FN mn___gt__, set_dunder_gt
    extern set_dunder_ge
    ADD_FN mn___ge__, set_dunder_ge
    extern set_dunder_eq
    ADD_FN mn___eq__, set_dunder_eq
    extern set_dunder_ne
    ADD_FN mn___ne__, set_dunder_ne

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
    ADD_FN mn___hash__, frozenset_dunder_hash

    extern frozenset_dunder_lt
    ADD_FN mn___lt__, frozenset_dunder_lt
    extern frozenset_dunder_le
    ADD_FN mn___le__, frozenset_dunder_le
    extern frozenset_dunder_gt
    ADD_FN mn___gt__, frozenset_dunder_gt
    extern frozenset_dunder_ge
    ADD_FN mn___ge__, frozenset_dunder_ge
    extern frozenset_dunder_eq
    ADD_FN mn___eq__, frozenset_dunder_eq
    extern frozenset_dunder_ne
    ADD_FN mn___ne__, frozenset_dunder_ne

    lea rax, [rel frozenset_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- weakref methods ---
    ; weakref.py binds ref.__hash__ and ref.__eq__ into its subclasses at
    ; class definition time, so those have to exist as methods.
    call dict_new
    mov rbx, rax
    ADD_FN mn___hash__, generic_method_hash
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
    ADD_FN mn___init__, object_method_init

    ADD_FN mn___str__, object_method_str

    ADD_FN mn___repr__, object_method_repr

    ; The rest of what object supplies by name.  types.py and enum both ask
    ; whether a class overrode one of these, which means asking object for its
    ; own first.
    ADD_FN mn___format__, object_method_format

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

    ADD_FN mn___sizeof__, object_method_sizeof

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

    ADD_FN mn___dir__, object_method_dir

    ADD_FN mn___reduce__, object_method_reduce

    ADD_FN mn___reduce_ex__, object_method_reduce

    ; The comparisons, which every class inherits and the stdlib binds by
    ; name: `__ne__ = MutableMapping.__ne__` reaches object's.
    ADD_FN mn___eq__, object_method_eq
    ADD_FN mn___ne__, object_method_ne
    ADD_FN mn___hash__, object_method_hash

    ; The ordering four, which answer NotImplemented.  They are safe for the
    ; same reason __eq__ is: type_install_slots installs no wrapper over a
    ; dunder that came from a type which is not a heaptype, and object is
    ; not, so a builtin subclass keeps its base's comparison rather than
    ; object's.
    extern object_method_lt
    ADD_FN mn___lt__, object_method_lt
    extern object_method_le
    ADD_FN mn___le__, object_method_le
    extern object_method_gt
    ADD_FN mn___gt__, object_method_gt
    extern object_method_ge
    ADD_FN mn___ge__, object_method_ge

    ; The generic attribute dunders, and the two hooks.  All five were
    ; absent, and every type inherits them -- abcmod has been looking for
    ; __subclasshook__ since it was written and silently finding nothing.
    ADD_FN mn___setattr__, object_method_setattr
    ADD_FN mn___delattr__, object_method_delattr
    ADD_FN mn___getattribute__, object_method_getattribute
    ADD_FN mn___getstate__, object_method_getstate

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
    ADD_FN mn___subclasses__, type_method_subclasses

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
    extern func_dunder_get
    ADD_FN mn___get__, func_dunder_get

    extern func_type
    lea rax, [rel func_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- staticmethod and classmethod: descriptors for the same reason ---
    call dict_new
    mov rbx, rax
    extern staticmethod_dunder_get
    ADD_FN mn___get__, staticmethod_dunder_get
    lea rax, [rel staticmethod_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    call dict_new
    mov rbx, rax
    extern classmethod_dunder_get
    ADD_FN mn___get__, classmethod_dunder_get
    lea rax, [rel classmethod_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- property: the same three, by name ---
    call dict_new
    mov rbx, rax
    extern property_dunder_get
    ADD_FN mn___get__, property_dunder_get
    extern property_dunder_set
    ADD_FN mn___set__, property_dunder_set
    extern property_dunder_delete
    ADD_FN mn___delete__, property_dunder_delete
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
    extern getset_descr_dunder_get
    ADD_FN mn___get__, getset_descr_dunder_get
    extern getset_descr_dunder_set
    ADD_FN mn___set__, getset_descr_dunder_set
    extern getset_descr_dunder_delete
    ADD_FN mn___delete__, getset_descr_dunder_delete
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
    extern builtin_func_dunder_get
    ADD_FN mn___get__, builtin_func_dunder_get
    extern builtin_func_type
    lea rax, [rel builtin_func_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- int_type methods ---
    call dict_new
    mov rbx, rax

    ADD_FN mn___repr__, int_dunder_repr

    ; int.__new__ / str.__new__: enum builds each member with
    ; `member_type.__new__(cls, *args)`, and decides which base is the data
    ; type by asking whether __new__ is in its __dict__.
    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    ADD_FN mn_bit_length, int_method_bit_length

    ; The names dir(int) was short of.  __round__ IS builtin_round_fn: a
    ; method's (args, nargs) is the shape round()'s own arguments arrive in.
    extern int_method_is_integer
    ADD_FN mn_is_integer, int_method_is_integer

    extern int_method_as_integer_ratio
    ADD_FN mn_as_integer_ratio, int_method_as_integer_ratio

    extern int_method_round
    ADD_FN mn___round__, int_method_round

    extern int_method_identity
    ADD_FN mn___floor__, int_method_identity

    ADD_FN mn___ceil__, int_method_identity

    extern int_method_getnewargs
    ADD_FN mn___getnewargs__, int_method_getnewargs

    ADD_FN mn_bit_count, int_method_bit_count

    ADD_FN mn_conjugate, int_method_conjugate

    ADD_FN mn_to_bytes, int_method_to_bytes


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
    extern int_dunder_neg
    ADD_FN mn___neg__, int_dunder_neg
    extern int_dunder_pos
    ADD_FN mn___pos__, int_dunder_pos
    extern int_dunder_abs
    ADD_FN mn___abs__, int_dunder_abs
    extern int_dunder_invert
    ADD_FN mn___invert__, int_dunder_invert
    extern int_dunder_int
    ADD_FN mn___int__, int_dunder_int
    extern int_dunder_float
    ADD_FN mn___float__, int_dunder_float
    extern int_dunder_index
    ADD_FN mn___index__, int_dunder_index
    extern int_dunder_trunc
    ADD_FN mn___trunc__, int_dunder_trunc
    extern int_dunder_bool
    ADD_FN mn___bool__, int_dunder_bool

    ;; and the binary family, forward and reflected.
    extern int_dunder_add
    ADD_FN mn___add__, int_dunder_add
    extern int_dunder_sub
    ADD_FN mn___sub__, int_dunder_sub
    extern int_dunder_mul
    ADD_FN mn___mul__, int_dunder_mul
    extern int_dunder_mod
    ADD_FN mn___mod__, int_dunder_mod
    extern int_dunder_divmod
    ADD_FN mn___divmod__, int_dunder_divmod
    extern int_dunder_pow
    ADD_FN mn___pow__, int_dunder_pow
    extern int_dunder_lshift
    ADD_FN mn___lshift__, int_dunder_lshift
    extern int_dunder_rshift
    ADD_FN mn___rshift__, int_dunder_rshift
    extern int_dunder_and
    ADD_FN mn___and__, int_dunder_and
    extern int_dunder_xor
    ADD_FN mn___xor__, int_dunder_xor
    extern int_dunder_or
    ADD_FN mn___or__, int_dunder_or
    extern int_dunder_floordiv
    ADD_FN mn___floordiv__, int_dunder_floordiv
    extern int_dunder_truediv
    ADD_FN mn___truediv__, int_dunder_truediv
    extern int_dunder_radd
    ADD_FN mn___radd__, int_dunder_radd
    extern int_dunder_rsub
    ADD_FN mn___rsub__, int_dunder_rsub
    extern int_dunder_rmul
    ADD_FN mn___rmul__, int_dunder_rmul
    extern int_dunder_rmod
    ADD_FN mn___rmod__, int_dunder_rmod
    extern int_dunder_rdivmod
    ADD_FN mn___rdivmod__, int_dunder_rdivmod
    extern int_dunder_rpow
    ADD_FN mn___rpow__, int_dunder_rpow
    extern int_dunder_rlshift
    ADD_FN mn___rlshift__, int_dunder_rlshift
    extern int_dunder_rrshift
    ADD_FN mn___rrshift__, int_dunder_rrshift
    extern int_dunder_rand
    ADD_FN mn___rand__, int_dunder_rand
    extern int_dunder_rxor
    ADD_FN mn___rxor__, int_dunder_rxor
    extern int_dunder_ror
    ADD_FN mn___ror__, int_dunder_ror
    extern int_dunder_rfloordiv
    ADD_FN mn___rfloordiv__, int_dunder_rfloordiv
    extern int_dunder_rtruediv
    ADD_FN mn___rtruediv__, int_dunder_rtruediv

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
    ADD_FN mn___format__, builtin_method_format

    extern int_dunder_hash
    ADD_FN mn___hash__, int_dunder_hash

    extern int_dunder_lt
    ADD_FN mn___lt__, int_dunder_lt
    extern int_dunder_le
    ADD_FN mn___le__, int_dunder_le
    extern int_dunder_gt
    ADD_FN mn___gt__, int_dunder_gt
    extern int_dunder_ge
    ADD_FN mn___ge__, int_dunder_ge
    extern int_dunder_eq
    ADD_FN mn___eq__, int_dunder_eq
    extern int_dunder_ne
    ADD_FN mn___ne__, int_dunder_ne

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

    ADD_FN mn___repr__, complex_dunder_repr

    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    ADD_FN mn_conjugate, complex_method_conjugate

    ADD_FN mn___complex__, complex_method_complex

    ADD_FN mn___getnewargs__, complex_method_getnewargs

    ADD_FN mn___format__, builtin_method_format

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

    extern complex_dunder_hash
    ADD_FN mn___hash__, complex_dunder_hash

    extern complex_dunder_bool
    ADD_FN mn___bool__, complex_dunder_bool

    lea rax, [rel complex_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- float_type methods ---
    call dict_new
    mov rbx, rax

    ADD_FN mn___repr__, float_dunder_repr

    ; float.__new__, for the same reason int and str carry one: a subclass
    ; that overrides __new__ reaches the base's through super(), and enum
    ; looks the name up in __dict__ to pick its data type.
    mov rdi, rbx
    lea rsi, [rel scalar_dunder_new]
    call add_new_staticmethod

    ADD_FN mn_is_integer, float_method_is_integer

    ; float's four.  __floor__ and __ceil__ do exactly what MATH_ROUNDER's
    ; native arm does, because adding them newly routes a float SUBCLASS
    ; instance through the dunder: that arm reaches only an immediate.
    ADD_FN mn___round__, int_method_round

    extern float_method_floor
    ADD_FN mn___floor__, float_method_floor

    extern float_method_ceil
    ADD_FN mn___ceil__, float_method_ceil

    extern float_method_getnewargs
    ADD_FN mn___getnewargs__, float_method_getnewargs

    ADD_FN mn_conjugate, float_method_conjugate

    ADD_FN mn_as_integer_ratio, float_method_as_integer_ratio

    ADD_FN mn_hex, float_method_hex


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

    ADD_FN mn___format__, builtin_method_format

    extern float_dunder_neg
    ADD_FN mn___neg__, float_dunder_neg
    extern float_dunder_pos
    ADD_FN mn___pos__, float_dunder_pos
    extern float_dunder_abs
    ADD_FN mn___abs__, float_dunder_abs
    extern float_dunder_int
    ADD_FN mn___int__, float_dunder_int
    extern float_dunder_float
    ADD_FN mn___float__, float_dunder_float

    ;; the binary family, forward and reflected.
    extern float_dunder_add
    ADD_FN mn___add__, float_dunder_add
    extern float_dunder_sub
    ADD_FN mn___sub__, float_dunder_sub
    extern float_dunder_mul
    ADD_FN mn___mul__, float_dunder_mul
    extern float_dunder_mod
    ADD_FN mn___mod__, float_dunder_mod
    extern float_dunder_divmod
    ADD_FN mn___divmod__, float_dunder_divmod
    extern float_dunder_pow
    ADD_FN mn___pow__, float_dunder_pow
    extern float_dunder_floordiv
    ADD_FN mn___floordiv__, float_dunder_floordiv
    extern float_dunder_truediv
    ADD_FN mn___truediv__, float_dunder_truediv
    extern float_dunder_radd
    ADD_FN mn___radd__, float_dunder_radd
    extern float_dunder_rsub
    ADD_FN mn___rsub__, float_dunder_rsub
    extern float_dunder_rmul
    ADD_FN mn___rmul__, float_dunder_rmul
    extern float_dunder_rmod
    ADD_FN mn___rmod__, float_dunder_rmod
    extern float_dunder_rdivmod
    ADD_FN mn___rdivmod__, float_dunder_rdivmod
    extern float_dunder_rpow
    ADD_FN mn___rpow__, float_dunder_rpow
    extern float_dunder_rfloordiv
    ADD_FN mn___rfloordiv__, float_dunder_rfloordiv
    extern float_dunder_rtruediv
    ADD_FN mn___rtruediv__, float_dunder_rtruediv
    extern float_dunder_trunc
    ADD_FN mn___trunc__, float_dunder_trunc
    extern float_dunder_bool
    ADD_FN mn___bool__, float_dunder_bool

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
    extern float_dunder_hash
    ADD_FN mn___hash__, float_dunder_hash

    extern float_dunder_lt
    ADD_FN mn___lt__, float_dunder_lt
    extern float_dunder_le
    ADD_FN mn___le__, float_dunder_le
    extern float_dunder_gt
    ADD_FN mn___gt__, float_dunder_gt
    extern float_dunder_ge
    ADD_FN mn___ge__, float_dunder_ge
    extern float_dunder_eq
    ADD_FN mn___eq__, float_dunder_eq
    extern float_dunder_ne
    ADD_FN mn___ne__, float_dunder_ne

    lea rax, [rel float_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    call type_stamp_methods

    ;; --- bytes_type methods (extend tp_dict, keep tp_getattr for .decode()) ---
    call dict_new
    mov rbx, rax

    ADD_FN mn___str__, bytes_dunder_str
    ADD_FN mn___repr__, bytes_dunder_repr

    ADD_FN mn_hex, bytes_method_hex

    ; And its inverse, which binascii.unhexlify needs -- and binascii is what
    ; base64, quopri, uu and plistlib come in behind.
    extern bytes_fromhex_impl
    ADD_CLASSMETHOD mn_fromhex, bytes_fromhex_impl

    ADD_FN mn_startswith, bytes_method_startswith

    ADD_FN mn_endswith, bytes_method_endswith

    ADD_FN mn_count, bytes_method_count

    ADD_FN mn_find, bytes_method_find

    ADD_FN mn_replace, bytes_method_replace

    ADD_FN mn_split, bytes_method_split

    extern bytes_method_rsplit
    ADD_FN mn_rsplit, bytes_method_rsplit

    extern bytes_method_rfind
    ADD_FN mn_rfind, bytes_method_rfind
    extern bytes_method_index
    ADD_FN mn_index, bytes_method_index
    extern bytes_method_rindex
    ADD_FN mn_rindex, bytes_method_rindex
    extern bytes_method_strip
    ADD_FN mn_strip, bytes_method_strip
    extern bytes_method_lstrip
    ADD_FN mn_lstrip, bytes_method_lstrip
    extern bytes_method_rstrip
    ADD_FN mn_rstrip, bytes_method_rstrip
    extern bytes_method_partition
    ADD_FN mn_partition, bytes_method_partition
    extern bytes_method_rpartition
    ADD_FN mn_rpartition, bytes_method_rpartition

    extern bytes_method_upper
    ADD_FN mn_upper, bytes_method_upper
    extern bytes_method_lower
    ADD_FN mn_lower, bytes_method_lower
    extern bytes_method_swapcase
    ADD_FN mn_swapcase, bytes_method_swapcase
    extern bytes_method_capitalize
    ADD_FN mn_capitalize, bytes_method_capitalize
    extern bytes_method_title
    ADD_FN mn_title, bytes_method_title
    extern bytes_method_isalpha
    ADD_FN mn_isalpha, bytes_method_isalpha
    extern bytes_method_isdigit
    ADD_FN mn_isdigit, bytes_method_isdigit
    extern bytes_method_isspace
    ADD_FN mn_isspace, bytes_method_isspace
    extern bytes_method_isalnum
    ADD_FN mn_isalnum, bytes_method_isalnum
    extern bytes_method_isascii
    ADD_FN mn_isascii, bytes_method_isascii
    extern bytes_method_isupper
    ADD_FN mn_isupper, bytes_method_isupper
    extern bytes_method_islower
    ADD_FN mn_islower, bytes_method_islower
    extern bytes_method_istitle
    ADD_FN mn_istitle, bytes_method_istitle
    extern bytes_method_ljust
    ADD_FN mn_ljust, bytes_method_ljust
    extern bytes_method_rjust
    ADD_FN mn_rjust, bytes_method_rjust
    extern bytes_method_center
    ADD_FN mn_center, bytes_method_center
    extern bytes_method_zfill
    ADD_FN mn_zfill, bytes_method_zfill
    extern bytes_method_expandtabs
    ADD_FN mn_expandtabs, bytes_method_expandtabs
    extern bytes_method_translate
    ADD_FN mn_translate, bytes_method_translate
    extern bytes_method_splitlines
    ADD_FN mn_splitlines, bytes_method_splitlines
    extern bytes_method_removeprefix
    ADD_FN mn_removeprefix, bytes_method_removeprefix
    extern bytes_method_removesuffix
    ADD_FN mn_removesuffix, bytes_method_removesuffix
    ADD_FN mn_join, bytes_method_join

    ; Store in bytes_type.tp_dict

    ; the slots, reachable by name: the stdlib reaches for them directly.
    ADD_FN mn___len__, bytes_dunder_len
    ADD_FN mn___iter__, bytes_dunder_iter

    mov rdi, rbx
    lea rsi, [rel mn_maketrans]
    extern bytes_staticmethod_maketrans
    lea rdx, [rel bytes_staticmethod_maketrans]
    call add_staticmethod

    ; The operators, by name.
    ADD_FN mn___add__, bytes_dunder_add
    ADD_FN mn___mul__, bytes_dunder_mul
    ADD_FN mn___rmul__, bytes_dunder_rmul
    ADD_FN mn___mod__, bytes_dunder_mod
    ADD_FN mn___rmod__, bytes_dunder_rmod
    ADD_FN mn___contains__, generic_method_contains
    ADD_FN mn___getitem__, bytes_dunder_getitem

    extern bytes_dunder_hash
    ADD_FN mn___hash__, bytes_dunder_hash

    extern bytes_dunder_lt
    ADD_FN mn___lt__, bytes_dunder_lt
    extern bytes_dunder_le
    ADD_FN mn___le__, bytes_dunder_le
    extern bytes_dunder_gt
    ADD_FN mn___gt__, bytes_dunder_gt
    extern bytes_dunder_ge
    ADD_FN mn___ge__, bytes_dunder_ge
    extern bytes_dunder_eq
    ADD_FN mn___eq__, bytes_dunder_eq
    extern bytes_dunder_ne
    ADD_FN mn___ne__, bytes_dunder_ne

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
    ADD_FN mn_append, bytearray_method_append
    ADD_FN mn_extend, bytearray_method_extend
    ADD_FN mn_insert, bytearray_method_insert
    ADD_FN mn_pop, bytearray_method_pop
    ADD_FN mn_remove, bytearray_method_remove
    ADD_FN mn_clear, bytearray_method_clear
    ADD_FN mn_reverse, bytearray_method_reverse
    ADD_FN mn_copy, bytearray_method_copy
    ADD_FN mn_hex, ba_shared_hex

    ; The same classmethod: it reads the class it was called on and answers a
    ; bytearray when that is bytearray.
    ADD_CLASSMETHOD mn_fromhex, bytes_fromhex_impl
    ADD_FN mn_startswith, ba_shared_startswith
    ADD_FN mn_endswith, ba_shared_endswith
    ADD_FN mn_count, ba_shared_count
    ADD_FN mn_find, ba_shared_find
    ADD_FN mn_replace, ba_shared_replace
    ADD_FN mn_split, ba_shared_split
    extern ba_shared_rsplit
    ADD_FN mn_rsplit, ba_shared_rsplit
    extern ba_shared_rfind
    ADD_FN mn_rfind, ba_shared_rfind
    extern ba_shared_index
    ADD_FN mn_index, ba_shared_index
    extern ba_shared_rindex
    ADD_FN mn_rindex, ba_shared_rindex
    extern ba_shared_strip
    ADD_FN mn_strip, ba_shared_strip
    extern ba_shared_lstrip
    ADD_FN mn_lstrip, ba_shared_lstrip
    extern ba_shared_rstrip
    ADD_FN mn_rstrip, ba_shared_rstrip
    extern ba_shared_partition
    ADD_FN mn_partition, ba_shared_partition
    extern ba_shared_rpartition
    ADD_FN mn_rpartition, ba_shared_rpartition
    extern ba_shared_upper
    ADD_FN mn_upper, ba_shared_upper
    extern ba_shared_lower
    ADD_FN mn_lower, ba_shared_lower
    extern ba_shared_swapcase
    ADD_FN mn_swapcase, ba_shared_swapcase
    extern ba_shared_capitalize
    ADD_FN mn_capitalize, ba_shared_capitalize
    extern ba_shared_title
    ADD_FN mn_title, ba_shared_title
    extern ba_shared_isalpha
    ADD_FN mn_isalpha, ba_shared_isalpha
    extern ba_shared_isdigit
    ADD_FN mn_isdigit, ba_shared_isdigit
    extern ba_shared_isspace
    ADD_FN mn_isspace, ba_shared_isspace
    extern ba_shared_isalnum
    ADD_FN mn_isalnum, ba_shared_isalnum
    extern ba_shared_isascii
    ADD_FN mn_isascii, ba_shared_isascii
    extern ba_shared_isupper
    ADD_FN mn_isupper, ba_shared_isupper
    extern ba_shared_islower
    ADD_FN mn_islower, ba_shared_islower
    extern ba_shared_istitle
    ADD_FN mn_istitle, ba_shared_istitle
    extern ba_shared_ljust
    ADD_FN mn_ljust, ba_shared_ljust
    extern ba_shared_rjust
    ADD_FN mn_rjust, ba_shared_rjust
    extern ba_shared_center
    ADD_FN mn_center, ba_shared_center
    extern ba_shared_zfill
    ADD_FN mn_zfill, ba_shared_zfill
    extern ba_shared_expandtabs
    ADD_FN mn_expandtabs, ba_shared_expandtabs
    extern ba_shared_translate
    ADD_FN mn_translate, ba_shared_translate
    extern ba_shared_splitlines
    ADD_FN mn_splitlines, ba_shared_splitlines
    extern ba_shared_removeprefix
    ADD_FN mn_removeprefix, ba_shared_removeprefix
    extern ba_shared_removesuffix
    ADD_FN mn_removesuffix, ba_shared_removesuffix
    ADD_FN mn_join, ba_shared_join
    ADD_FN mn_decode, ba_shared_decode
    ADD_FN mn___len__, bytearray_dunder_len
    ADD_FN mn___iter__, bytearray_dunder_iter
    ADD_FN mn___setitem__, bytearray_dunder_setitem
    ADD_FN mn___delitem__, bytearray_dunder_delitem
    ADD_FN mn___getitem__, bytearray_dunder_getitem
    ADD_FN mn___contains__, bytearray_dunder_contains
    mov rdi, rbx
    lea rsi, [rel mn_maketrans]
    lea rdx, [rel bytes_staticmethod_maketrans]
    call add_staticmethod

    ; The operators, by name.
    ADD_FN mn___add__, bytearray_dunder_add
    ADD_FN mn___mul__, bytearray_dunder_mul
    ADD_FN mn___rmul__, bytearray_dunder_rmul
    ADD_FN mn___iadd__, bytearray_dunder_iadd
    ADD_FN mn___imul__, bytearray_dunder_imul
    ADD_FN mn___mod__, bytearray_dunder_mod
    ADD_FN mn___rmod__, bytearray_dunder_rmod

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
    ADD_FN mn_tobytes, memoryview_method_tobytes
    ADD_FN mn_tolist, memoryview_method_tolist
    ADD_FN mn_cast, memoryview_method_cast
    ADD_FN mn_release, memoryview_method_release
    ADD_FN mn___enter__, memoryview_method_enter
    ADD_FN mn___exit__, memoryview_method_exit
    ADD_FN mn_hex, memoryview_method_hex
    ADD_FN mn___getitem__, memoryview_dunder_getitem
    ADD_FN mn___setitem__, memoryview_dunder_setitem
    ADD_FN mn___len__, memoryview_dunder_len
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
mn_send:        db "send", 0
mn_throw:       db "throw", 0
mn_close:       db "close", 0
mn_indices:     db "indices", 0
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
mn_intersection_update: db "intersection_update", 0
mn_difference_update: db "difference_update", 0
mn_symmetric_difference_update: db "symmetric_difference_update", 0
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
mn___next__:    db "__next__", 0
mn___await__:   db "__await__", 0
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
gs_start:       db "start", 0
gs_stop:        db "stop", 0
gs_step:        db "step", 0
gs___name__:    db "__name__", 0
gs___qualname__: db "__qualname__", 0
gs_gi_frame:    db "gi_frame", 0
gs_gi_code:     db "gi_code", 0
gs_gi_running:  db "gi_running", 0
gs_cr_frame:    db "cr_frame", 0
gs_cr_code:     db "cr_code", 0
gs_cr_running:  db "cr_running", 0
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
mn___iand__: db "__iand__", 0
mn___isub__: db "__isub__", 0
mn___ixor__: db "__ixor__", 0
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
