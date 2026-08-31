; methods_object.asm - object's own dunders, and the slot-backed ones
;
; object.__init__/__str__/__repr__/__format__ and friends, plus the three
; DEF_DUNDER_* macros that generate __len__, __iter__, __str__ and __repr__
; for six builtin types at once.  Those stay together: one block generates
; methods for str, bytes, int, float, dict, list, tuple and set, so it cannot
; be distributed by type.
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
extern bytes_type
extern float_type
extern notimpl_singleton

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
;; object.__init__ / __str__ / __repr__
;;
;; object_type.tp_dict held only __new__.  types.py builds its type zoo out of
;; `type(object.__init__)` and `type(object().__str__)`, so these have to be
;; reachable before it can import -- and `super().__init__()` in a class that
;; derives straight from object needs the first one anyway.


;; ============================================================================
;; scalar_dunder_new(args, nargs) -> Value   -- int.__new__ / str.__new__
;;
;; `int.__new__(cls, v)` builds an instance of cls carrying v, WITHOUT going
;; back through cls.__new__.  That distinction is the whole reason it has to
;; exist: enum makes each member with `member_type.__new__(cls, *args)` where
;; cls is the enum class, whose own __new__ is what is calling this.
;;
;; It also has to be findable: enum decides which base is the "data type" by
;; asking whether __new__ or __init__ is in that base's __dict__, and int and
;; str had neither.
;; ============================================================================
extern int_sub_new
extern str_sub_new
DEF_FUNC scalar_dunder_new
    push rbx
    push r12
    test rsi, rsi
    jz .sdn_bad
    mov rbx, [rdi]                      ; cls
    lea r12, [rdi + 8]                  ; the rest of the arguments
    dec rsi
    V_TEST_PTR rbx, rax
    ja .sdn_bad

    mov rax, [rbx + PyTypeObject.tp_flags]
    lea rcx, [rel int_type]
    cmp rbx, rcx
    je .sdn_int
    test rax, TYPE_FLAG_INT_SUBCLASS
    jnz .sdn_int
    lea rcx, [rel str_type]
    cmp rbx, rcx
    je .sdn_str
    test rax, TYPE_FLAG_STR_SUBCLASS
    jnz .sdn_str
    jmp .sdn_bad

.sdn_int:
    mov rdi, rbx
    mov rdx, rsi
    mov rsi, r12
    call int_sub_new
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.sdn_str:
    mov rdi, rbx
    mov rdx, rsi
    mov rsi, r12
    call str_sub_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.sdn_bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__new__() argument 1 must be a subclass of int or str"
    call raise_exception
END_FUNC scalar_dunder_new

;; ============================================================================
;; object.__format__(self, format_spec)
;;
;; An empty spec is str(self); anything else is an error, because object has no
;; formatting of its own to apply.  enum reaches for this by name when it
;; decides whether a mixin overrode it.
;; ============================================================================
DEF_FUNC object_method_format
    cmp rsi, 2
    jne .omf_bad
    mov rax, [rdi + 8]                  ; the format spec
    V_TEST_PTR rax, rcx
    ja .omf_bad_spec
    test rax, rax
    jz .omf_bad_spec
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .omf_bad_spec
    cmp qword [rax + PyStrObject.ob_size], 0
    jne .omf_unsupported
    mov rdi, [rdi]
    call obj_str
    leave
    ret
.omf_bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__format__() takes exactly one argument"
    call raise_exception
.omf_bad_spec:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__format__() argument must be str"
    call raise_exception
.omf_unsupported:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unsupported format string passed to object.__format__"
    call raise_exception
END_FUNC object_method_format

;; ============================================================================
;; object.__sizeof__(self) -> the type's tp_basicsize
;; ============================================================================
DEF_FUNC object_method_sizeof
    mov rax, [rdi]
    V_TEST_PTR rax, rcx
    ja .oso_zero
    test rax, rax
    jz .oso_zero
    mov rax, [rax + PyObject.ob_type]
    mov rdi, [rax + PyTypeObject.tp_basicsize]
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret
.oso_zero:
    xor edi, edi
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret
END_FUNC object_method_sizeof

;; ============================================================================
;; object.__dir__(self) -> the names its type and instance dict carry
;; Defers to the dir() builtin, which already walks both.
;; ============================================================================
DEF_FUNC object_method_dir
    extern builtin_dir
    mov esi, 1
    call builtin_dir
    leave
    ret
END_FUNC object_method_dir

;; ============================================================================
;; object.__reduce_ex__(self, protocol) / object.__reduce__(self)
;;
;; These exist so that code which asks whether a class overrode them -- enum
;; does, to decide whether a mixin supplied its own -- finds something to
;; compare against.  Calling one raises: the copyreg machinery CPython builds
;; the default reduction on is not implemented here, and returning a wrong
;; reduction would corrupt a pickle rather than refuse to make one.
;; ============================================================================
DEF_FUNC object_method_reduce
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "cannot pickle this object: object.__reduce_ex__ is not implemented"
    call raise_exception
END_FUNC object_method_reduce


;; ============================================================================
extern str_from_cstr
DEF_FUNC object_method_init
    ; object.__init__(self, ...) accepts anything and does nothing.
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC object_method_init

DEF_FUNC object_method_str
    ; object.__str__ defers to __repr__, which is what CPython does.  It must
    ; not call obj_str: that dispatches back to tp_str, which looks up
    ; __str__, which finds this again.
    test rsi, rsi
    jz .oms_bad
    mov rdi, [rdi]
    extern obj_repr
    call obj_repr
    leave
    ret
.oms_bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__str__() takes exactly one argument"
    call raise_exception
END_FUNC object_method_str

;; object.__repr__ is the *default* implementation, not a re-dispatch: it
;; writes "<Name object>" straight out.  Calling obj_repr here would come
;; back through tp_repr to this same method.
OMR_BUF   equ 136
OMR_FRAME equ 160
DEF_FUNC object_method_repr, OMR_FRAME
    test rsi, rsi
    jz .omr_bad
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .omr_plain
    test rdi, rdi
    jz .omr_plain
    mov rsi, [rdi + PyObject.ob_type]
    mov rsi, [rsi + PyTypeObject.tp_name]
    test rsi, rsi
    jz .omr_plain

    lea rdi, [rbp - OMR_BUF]
    xor ecx, ecx
    mov byte [rdi], '<'
    inc rcx
.omr_name:
    movzx eax, byte [rsi]
    test al, al
    jz .omr_tail
    inc rsi
    cmp rcx, 100
    jae .omr_tail
    mov [rdi + rcx], al
    inc rcx
    jmp .omr_name
.omr_tail:
    CSTRING rsi, " object>"
.omr_tail_copy:
    movzx eax, byte [rsi]
    test al, al
    jz .omr_emit
    inc rsi
    mov [rdi + rcx], al
    inc rcx
    jmp .omr_tail_copy
.omr_emit:
    mov byte [rdi + rcx], 0
    call str_from_cstr
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.omr_plain:
    CSTRING rdi, "<object>"
    call str_from_cstr
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.omr_bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__repr__() takes exactly one argument"
    call raise_exception
END_FUNC object_method_repr

;; ============================================================================
;; object.__init_subclass__(cls) -> None
;; A no-op hook every class inherits, so that a real one can end with
;; `super().__init_subclass__()` -- which is how they are written.  Keywords
;; are an error here: whoever declared them should have consumed them.
;; ============================================================================
DEF_FUNC object_method_init_subclass
    cmp rsi, 2
    jge .omis_bad
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.omis_bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__init_subclass__() takes no keyword arguments"
    call raise_exception
END_FUNC object_method_init_subclass


;; ============================================================================
;; A builtin's own __str__ and __repr__, reachable by name.
;;
;; They were not: `str.__str__` resolved through the MRO to object's, and enum
;; asks precisely that question -- "if member_type.__str__ is object.__str__,
;; use its __repr__ instead" -- so every StrEnum member printed as
;; <Names object>.  The thunk calls the *defining* type's slot, not the
;; argument's, which is what keeps it right on a subclass and out of the
;; recursion a re-dispatch would cause.  An immediate int or float has no
;; ob_type to read a slot from, so it goes through obj_repr, which knows them.
;; ============================================================================
%macro DEF_DUNDER_STRREPR 2     ; %1 = type prefix, %2 = tp_str or tp_repr
DEF_FUNC %1_dunder_%2
    test rsi, rsi
    jz %%bad
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja %%immediate
    test rdi, rdi
    jz %%immediate
    lea rax, [rel %1_type]
    mov rax, [rax + PyTypeObject.tp_%2]
    test rax, rax
    jz %%immediate
    ; int_repr reads edx as the argument's tag, and this one is a pointer.
    mov edx, TAG_PTR
    call rax
    leave
    ret
%%immediate:
    call obj_repr
    leave
    ret
%%bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "expected exactly one argument"
    call raise_exception
END_FUNC %1_dunder_%2
%endmacro

;; A builtin container's __len__ and __iter__, reachable by name.  They were
;; slots and nothing else, so `cache.__len__` -- how functools' lru_cache reads
;; its size without paying for the len() call -- raised AttributeError.  Like
;; the str/repr thunks these read the *defining* type's slot, so a subclass
;; inherits the base's behaviour rather than re-dispatching into itself.
%macro DEF_DUNDER_LEN 1
DEF_FUNC %1_dunder_len
    test rsi, rsi
    jz %%bad
    mov rdi, [rdi]
    lea rax, [rel %1_type]
    mov rcx, [rax + PyTypeObject.tp_as_mapping]
    test rcx, rcx
    jz %%seq
    mov rcx, [rcx + PyMappingMethods.mp_length]
    test rcx, rcx
    jnz %%have
%%seq:
    mov rcx, [rax + PyTypeObject.tp_as_sequence]
    test rcx, rcx
    jz %%bad
    mov rcx, [rcx + PySequenceMethods.sq_length]
    test rcx, rcx
    jz %%bad
%%have:
    call rcx
    mov edx, TAG_SMALLINT
    leave
    V_PACK rax, rdx
    ret
%%bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object has no len()"
    call raise_exception
END_FUNC %1_dunder_len
%endmacro

%macro DEF_DUNDER_ITER 1
DEF_FUNC %1_dunder_iter
    test rsi, rsi
    jz %%bad
    mov rdi, [rdi]
    lea rax, [rel %1_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz %%bad
    call rax
    test rax, rax
    jz %%failed
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
%%failed:
    xor eax, eax
    xor edx, edx
    leave
    V_PACK rax, rdx
    ret
%%bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception
END_FUNC %1_dunder_iter
%endmacro

; list and tuple already have hand-written ones.
DEF_DUNDER_LEN dict
DEF_DUNDER_LEN str
DEF_DUNDER_LEN set
DEF_DUNDER_LEN bytes
DEF_DUNDER_ITER dict
DEF_DUNDER_ITER list
DEF_DUNDER_ITER tuple
DEF_DUNDER_ITER str
DEF_DUNDER_ITER set
DEF_DUNDER_ITER bytes

DEF_DUNDER_STRREPR str, str
DEF_DUNDER_STRREPR str, repr
DEF_DUNDER_STRREPR bytes, str
DEF_DUNDER_STRREPR bytes, repr
DEF_DUNDER_STRREPR int, repr
DEF_DUNDER_STRREPR float, repr


;; ############################################################################
;;                         SET METHODS
;; ############################################################################



;; ============================================================================
;; Slot-backed dunder methods
;;
;; The operators reach the type slots directly, but the methods themselves
;; were absent from most builtin type dicts, so `dict.__setitem__` and
;; `ref.__hash__` -- both of which collections and weakref bind at class
;; definition time -- raised AttributeError.  One implementation per slot,
;; dispatching through whatever the receiver's type provides.
;; ============================================================================



;; object.__eq__ / __ne__ / __hash__ and the ordering four.
;;
;; These must not go back through the comparison protocol: a heaptype's
;; tp_richcompare looks __eq__ up in the MRO, finds object's, and if that
;; re-entered the protocol the two would call each other forever.  CPython's
;; are self-contained for the same reason -- identity for __eq__, the type's
;; own EQ slot for __ne__, the address for __hash__.

;; Like __ne__, this defers to the type's own comparison before falling back to
;; identity.  It is reached only when nothing in the MRO defines __eq__ by name,
;; which for a builtin means its answer lives in tp_richcompare -- so comparing
;; addresses here made `a.__eq__(b)` return NotImplemented for two equal tuples.
;; CPython's constant folding shares one empty tuple, which hid it from every
;; test that spelled the operands out.
DEF_FUNC object_method_eq
    cmp rsi, 2
    jne .ome_error
    push rbx
    mov rbx, [rdi + 8]          ; other
    mov rdi, [rdi]              ; self
    V_TEST_PTR rdi, rax
    ja .ome_identity
    test rdi, rdi
    jz .ome_identity
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .ome_identity
    ; ...but not when that slot is the generic dispatcher.  It looks __eq__ up
    ; by name, walks to object's, and arrives back here -- unbounded recursion
    ; for any class defining a comparison other than __eq__ (a class that
    ; defines __eq__ never reaches this method at all).  __ne__ takes the same
    ; route through here, so both == and != segfaulted.
    extern slot_tp_richcompare
    lea rcx, [rel slot_tp_richcompare]
    cmp rax, rcx
    je .ome_identity
    mov rsi, rbx
    mov edx, CMP_EQ
    call rax
    V_UNPACK rax, rdx
    test edx, edx
    jz .ome_notimpl
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.ome_notimpl:
    lea rax, [rel notimpl_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.ome_identity:
    cmp rdi, rbx
    je .ome_true_pop
    lea rax, [rel notimpl_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.ome_true_pop:
    pop rbx
.ome_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.ome_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__eq__() takes exactly one argument"
    call raise_exception
END_FUNC object_method_eq

;; __ne__ delegates to the type's own EQ comparison and inverts it, so a class
;; that defines only __eq__ still gets a correct !=.
DEF_FUNC object_method_ne
    cmp rsi, 2
    jne .omn_error
    push rbx
    mov rbx, [rdi + 8]          ; other
    mov rdi, [rdi]              ; self
    V_TEST_PTR rdi, rax
    ja .omn_identity
    test rdi, rdi
    jz .omn_identity
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .omn_identity
    mov rsi, rbx
    mov edx, CMP_EQ
    call rax
    V_UNPACK rax, rdx
    test edx, edx
    jz .omn_notimpl
    lea rcx, [rel notimpl_singleton]
    cmp rax, rcx
    je .omn_release_notimpl
    push rax
    push rdx
    mov rdi, rax
    V_PACK rdi, rdx
    extern obj_is_true
    call obj_is_true
    mov ebx, eax
    pop rdx
    pop rdi
    DECREF_VAL rdi, rdx
    test ebx, ebx
    jz .omn_true
    lea rax, [rel bool_false]
    jmp .omn_out
.omn_true:
    lea rax, [rel bool_true]
    jmp .omn_out
.omn_release_notimpl:
    mov rdi, rax
    call obj_decref
.omn_notimpl:
    lea rax, [rel notimpl_singleton]
    jmp .omn_out
.omn_identity:
    cmp rdi, rbx
    je .omn_same
    lea rax, [rel bool_true]
    jmp .omn_out
.omn_same:
    lea rax, [rel bool_false]
.omn_out:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.omn_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__ne__() takes exactly one argument"
    call raise_exception
END_FUNC object_method_ne

DEF_FUNC object_method_notimpl
    lea rax, [rel notimpl_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC object_method_notimpl

;; The address, which is what obj_hash falls back to when a type has no
;; tp_hash -- reached directly so that a heaptype's slot cannot bounce back.
DEF_FUNC object_method_hash
    cmp rsi, 1
    jl .omh_error
    mov rax, [rdi]
    add rax, [rel v_int_bias]
    leave
    ret
.omh_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__hash__() takes no arguments"
    call raise_exception
END_FUNC object_method_hash


;; generic_method_getitem(args, nargs): args[0]=self, args[1]=key
DEF_FUNC generic_method_getitem
    cmp rsi, 2
    jne .gmg_error
    mov rax, [rdi]
    V_TEST_PTR rax, rcx
    ja .gmg_error
    test rax, rax
    jz .gmg_error
    mov rsi, [rdi + 8]
    mov rdi, rax
    mov rcx, [rax + PyObject.ob_type]
    mov rdx, [rcx + PyTypeObject.tp_as_mapping]
    test rdx, rdx
    jz .gmg_seq
    mov rdx, [rdx + PyMappingMethods.mp_subscript]
    test rdx, rdx
    jz .gmg_seq
    call rdx
    leave
    ret
.gmg_seq:
    mov rdx, [rcx + PyTypeObject.tp_as_sequence]
    test rdx, rdx
    jz .gmg_error
    mov rdx, [rdx + PySequenceMethods.sq_item]
    test rdx, rdx
    jz .gmg_error
    V_TO_I64 rsi
    call rdx
    leave
    ret
.gmg_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not subscriptable"
    call raise_exception
END_FUNC generic_method_getitem


;; ============================================================================
;; dict.__getitem__ / __setitem__ / __delitem__
;;
;; These call dict's own implementation rather than dispatching through the
;; instance's mp_ass_subscript.  A dict subclass that overrides __setitem__ has
;; a wrapper in that slot, so a virtual dispatch here turns
;; `super().__setitem__(k, v)` -- the ordinary way to write such an override --
;; into unbounded recursion.  `dict.__setitem__` means dict's, exactly as it
;; does in CPython, where it is a slot wrapper bound to dict.
;; ============================================================================
extern dict_subscript
DEF_FUNC_BARE dict_dunder_getitem
    ; The same guards its two siblings carry.  builtin_func_call validates
    ; neither the count nor the type for these -- add_method_to_dict registers
    ; them with no min or max -- so dict.__getitem__(5, "a") handed the
    ; immediate 5 to dict_get as a pointer and dereferenced it, and
    ; d.__getitem__() read args[1] off the end of the argument array.
    cmp rsi, 2
    jne .ddg_error
    mov rax, [rdi]              ; self
    V_TEST_PTR rax, rcx
    ja .ddg_error
    test rax, rax
    jz .ddg_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_DICT_TYPE rcx, rdx, .ddg_error
    mov rsi, [rdi + 8]          ; the key Value
    mov rdi, rax
    jmp dict_subscript
.ddg_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "descriptor '__getitem__' requires a 'dict' object"
    call raise_exception
END_FUNC dict_dunder_getitem

DEF_FUNC dict_dunder_setitem
    cmp rsi, 3
    jne .dds_error
    mov rax, [rdi]
    V_TEST_PTR rax, rcx
    ja .dds_error
    test rax, rax
    jz .dds_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_DICT_TYPE rcx, rdx, .dds_error
    mov rsi, [rdi + 8]
    mov rdx, [rdi + 16]
    mov rdi, rax
    call dict_set
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.dds_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support item assignment"
    call raise_exception
END_FUNC dict_dunder_setitem

DEF_FUNC dict_dunder_delitem
    cmp rsi, 2
    jne .ddd_error
    mov rax, [rdi]
    V_TEST_PTR rax, rcx
    ja .ddd_error
    test rax, rax
    jz .ddd_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_DICT_TYPE rcx, rdx, .ddd_error
    mov rsi, [rdi + 8]
    mov rdi, rax
    call dict_del
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.ddd_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support item deletion"
    call raise_exception
END_FUNC dict_dunder_delitem

;; generic_method_setitem(args, nargs): args[0]=self, args[1]=key, args[2]=value
DEF_FUNC generic_method_setitem
    cmp rsi, 3
    jne .gms_error
    mov rax, [rdi]
    V_TEST_PTR rax, rcx
    ja .gms_error
    test rax, rax
    jz .gms_error
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_as_mapping]
    test rcx, rcx
    jz .gms_error
    mov rcx, [rcx + PyMappingMethods.mp_ass_subscript]
    test rcx, rcx
    jz .gms_error
    mov rsi, [rdi + 8]
    mov rdx, [rdi + 16]
    mov rdi, rax
    call rcx
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gms_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support item assignment"
    call raise_exception
END_FUNC generic_method_setitem

;; generic_method_delitem(args, nargs): args[0]=self, args[1]=key
DEF_FUNC generic_method_delitem
    cmp rsi, 2
    jne .gmd_error
    mov rax, [rdi]
    V_TEST_PTR rax, rcx
    ja .gmd_error
    test rax, rax
    jz .gmd_error
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_as_mapping]
    test rcx, rcx
    jz .gmd_error
    mov rcx, [rcx + PyMappingMethods.mp_ass_subscript]
    test rcx, rcx
    jz .gmd_error
    mov rsi, [rdi + 8]
    xor edx, edx                ; a NULL value means delete
    mov rdi, rax
    call rcx
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gmd_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support item deletion"
    call raise_exception
END_FUNC generic_method_delitem

;; generic_method_hash(args, nargs): args[0]=self
DEF_FUNC generic_method_hash
    cmp rsi, 1
    jne .gmh_error
    mov rdi, [rdi]
    extern obj_hash
    call obj_hash
    add rax, [rel v_int_bias]
    leave
    ret
.gmh_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__hash__() takes no arguments"
    call raise_exception
END_FUNC generic_method_hash

;; ============================================================================
;; generic_method_contains(args, nargs) -> bool
;; args[0]=self, args[1]=item
;;
;; `x in c` reaches sq_contains directly, but the method itself was never in
;; any type's dict, so `frozenset(names).__contains__` -- keyword.py's
;; iskeyword, among others -- raised AttributeError.  One implementation
;; serves every type that has the slot.
;; ============================================================================
DEF_FUNC generic_method_contains
    cmp rsi, 2
    jne .gmc_error
    mov rax, [rdi]              ; self
    V_TEST_PTR rax, rcx
    ja .gmc_error
    test rax, rax
    jz .gmc_error
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_as_sequence]
    test rcx, rcx
    jz .gmc_error
    mov rcx, [rcx + PySequenceMethods.sq_contains]
    test rcx, rcx
    jz .gmc_error
    mov rsi, [rdi + 8]          ; the item, as a Value
    mov rdi, rax
    call rcx
    test eax, eax
    jz .gmc_false
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gmc_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gmc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__contains__() takes exactly one argument"
    call raise_exception
END_FUNC generic_method_contains
