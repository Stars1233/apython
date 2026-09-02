; methods/object.asm - object's own dunders, and the slot-backed ones
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
extern complex_type
extern obj_decref
extern obj_repr
extern obj_str
extern str_type
extern dict_set
extern dict_del
extern none_singleton
extern bool_true
extern bool_false
extern int_from_i64
extern raise_exception
extern exc_TypeError_type
extern int_type
extern obj_is_true
extern notimpl_singleton
extern dict_type
extern list_type
extern obj_dealloc
extern tuple_type

; Set entry layout constants (must match set.asm)

; --- moved to a sibling file by the split ---

extern bytes_type

extern float_type

extern set_type

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
    extern float_type
    lea rcx, [rel float_type]
    cmp rbx, rcx
    je .sdn_float
    test rax, TYPE_FLAG_FLOAT_SUBCLASS
    jnz .sdn_float
    extern complex_type
    lea rcx, [rel complex_type]
    cmp rbx, rcx
    je .sdn_complex
    test rax, TYPE_FLAG_COMPLEX_SUBCLASS
    jnz .sdn_complex
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

;; float and complex have constructors that already read the type they are
;; handed, so the subclass arm is the same call as the base one.  Both return
;; a fat pair, which the V_PACK below is exactly right for.
.sdn_float:
    extern float_type_call
    mov rdi, rbx
    mov rdx, rsi
    mov rsi, r12
    call float_type_call
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.sdn_complex:
    extern complex_type_call
    mov rdi, rbx
    mov rdx, rsi
    mov rsi, r12
    call complex_type_call
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.sdn_bad:
    RAISE exc_TypeError_type, "__new__() argument 1 must be a subclass of int or str"
END_FUNC scalar_dunder_new

;; ============================================================================
;; ============================================================================
;; builtin_method_format(self, format_spec) -- __format__ for the four types
;; the spec mini-language knows how to render itself.
;;
;; CPython gives int, float, str and complex a __format__ of their own, and a
;; subclass inherits it.  Without one, `class F(float)` inherited OBJECT's,
;; which refuses any non-empty spec: format(F(2.5), ".2f") was "unsupported
;; format string passed to object.__format__".
;; ============================================================================
extern format_apply_spec

DEF_FUNC builtin_method_format
    cmp rsi, 2
    jne .bmf_bad
    mov rax, [rdi + 8]                  ; the format spec
    V_TEST_PTR rax, rcx
    ja .bmf_bad_spec
    test rax, rax
    jz .bmf_bad_spec
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .bmf_bad_spec
    mov rsi, rax
    mov rdi, [rdi]
    call format_apply_spec
    leave
    ret
.bmf_bad:
    RAISE exc_TypeError_type, "__format__() takes exactly one argument"
.bmf_bad_spec:
    RAISE exc_TypeError_type, "__format__() argument must be str"
END_FUNC builtin_method_format

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
    RAISE exc_TypeError_type, "__format__() takes exactly one argument"
.omf_bad_spec:
    RAISE exc_TypeError_type, "__format__() argument must be str"
.omf_unsupported:
    RAISE exc_TypeError_type, "unsupported format string passed to object.__format__"
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
;;
;; The default walk, and only that: dir() is what asks for __dir__ in the
;; first place, so calling it back from here made the pair circular and left
;; neither of them asking the object anything.
;; ============================================================================
DEF_FUNC object_method_dir
    extern dir_default
    mov rdi, [rdi]              ; args[0] = self, a Value
    call dir_default
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
    RAISE exc_TypeError_type, "cannot pickle this object: object.__reduce_ex__ is not implemented"
END_FUNC object_method_reduce

;; ============================================================================
extern str_from_cstr
DEF_FUNC object_method_init
    ; object.__init__(self, ...) accepts anything and does nothing.
    RET_NONE
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
    RAISE exc_TypeError_type, "__str__() takes exactly one argument"
END_FUNC object_method_str

;; object.__repr__ is the *default* implementation, not a re-dispatch: it
;; writes "<Name object>" straight out.  Calling obj_repr here would come
;; back through tp_repr to this same method.
OMR_BUF   equ 136
OMR_FRAME equ 160           ; + 0 pushes = 160
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
    RAISE exc_TypeError_type, "__repr__() takes exactly one argument"
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
    RAISE exc_TypeError_type, "__init_subclass__() takes no keyword arguments"
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
    RAISE exc_TypeError_type, "expected exactly one argument"
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
    RAISE exc_TypeError_type, "object has no len()"
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
    RAISE exc_TypeError_type, "object is not iterable"
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


;; A builtin number's unary dunders, reachable by name.  int and float had
;; none at all: `(-5).__abs__()` was an AttributeError, and -- worse -- an MRO
;; name lookup could not prefer int's operator over a later base's, because
;; int had nothing in its dict to find.  `class I(int, M)` with an M defining
;; __invert__ therefore installed M's, where CPython answers int's.
;;
;; Like the str/repr thunks these call the *defining* type's slot rather than
;; the argument's, which is what keeps a subclass out of its own recursion.
%macro DEF_DUNDER_UNARY 3       ; %1 = type prefix, %2 = suffix, %3 = nb_ field
DEF_FUNC %1_dunder_%2
    cmp rsi, 1
    jne %%bad
    mov rdi, [rdi]              ; args[0] = self, a Value
    lea rax, [rel %1_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz %%bad
    mov rax, [rax + PyNumberMethods.%3]
    test rax, rax
    jz %%bad
    call rax
    leave
    ret
%%bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC %1_dunder_%2
%endmacro

;; __bool__ is the odd one: nb_bool answers 0 or 1 in eax, not a Value.
%macro DEF_DUNDER_BOOL 1
DEF_FUNC %1_dunder_bool
    cmp rsi, 1
    jne %%bad
    mov rdi, [rdi]
    lea rax, [rel %1_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz %%bad
    mov rax, [rax + PyNumberMethods.nb_bool]
    test rax, rax
    jz %%bad
    call rax
    test eax, eax
    jz %%false
    extern bool_true
    lea rax, [rel bool_true]
    INCREF rax
    leave
    ret
%%false:
    extern bool_false
    lea rax, [rel bool_false]
    INCREF rax
    leave
    ret
%%bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC %1_dunder_bool
%endmacro

DEF_DUNDER_UNARY int, neg, nb_negative
DEF_DUNDER_UNARY int, pos, nb_positive
DEF_DUNDER_UNARY int, abs, nb_absolute
DEF_DUNDER_UNARY int, invert, nb_invert
DEF_DUNDER_UNARY int, int, nb_int
DEF_DUNDER_UNARY int, float, nb_float
DEF_DUNDER_UNARY int, index, nb_index
DEF_DUNDER_UNARY int, trunc, nb_int

DEF_DUNDER_UNARY float, neg, nb_negative
DEF_DUNDER_UNARY float, pos, nb_positive
DEF_DUNDER_UNARY float, abs, nb_absolute
DEF_DUNDER_UNARY float, int, nb_int
DEF_DUNDER_UNARY float, float, nb_float
DEF_DUNDER_UNARY float, trunc, nb_int
DEF_DUNDER_BOOL float

;; int's nb_bool takes the (payload, tag) pair rather than a Value -- it hands
;; the pair straight to int_unwrap -- so it cannot go through the macro.
DEF_FUNC int_dunder_bool
    cmp rsi, 1
    jne .idb_bad
    mov rdi, [rdi]
    V_UNPACK rdi, rdx
    extern int_bool
    call int_bool
    test eax, eax
    jz .idb_false
    lea rax, [rel bool_true]
    INCREF rax
    leave
    ret
.idb_false:
    lea rax, [rel bool_false]
    INCREF rax
    leave
    ret
.idb_bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC int_dunder_bool

DEF_DUNDER_STRREPR str, str
DEF_DUNDER_STRREPR str, repr
DEF_DUNDER_STRREPR bytes, str
DEF_DUNDER_STRREPR bytes, repr
DEF_DUNDER_STRREPR int, repr
DEF_DUNDER_STRREPR float, repr
DEF_DUNDER_STRREPR complex, repr

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
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret
.ome_error:
    RAISE exc_TypeError_type, "__eq__() takes exactly one argument"
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
    RAISE exc_TypeError_type, "__ne__() takes exactly one argument"
END_FUNC object_method_ne

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
    RAISE exc_TypeError_type, "__hash__() takes no arguments"
END_FUNC object_method_hash


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
    RAISE exc_TypeError_type, "__hash__() takes no arguments"
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
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret
.gmc_false:
    RET_FALSE
    leave
    V_PACK rax, rdx
    ret
.gmc_error:
    RAISE exc_TypeError_type, "__contains__() takes exactly one argument"
END_FUNC generic_method_contains
