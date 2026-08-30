; object.asm - PyObject base operations
; Allocation, reference counting, type dispatch for repr/str/hash/bool
; Fat-value aware: functions accept (payload, tag) pairs

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern sys_write
extern str_from_cstr
extern dict_get
extern dict_new
extern ap_strcmp
extern none_singleton
extern bool_false
extern bool_true
extern int_repr
extern int_type
extern current_exception
extern int_to_i64
extern float_type
extern float_repr
extern none_repr
extern bool_repr
extern type_getattr
extern type_setattr
extern type_call

; obj_alloc(size_t size, PyTypeObject *type) -> PyObject*
; Allocate a new object with refcount=1 and given type
DEF_FUNC obj_alloc
    push rbx
    push r12
    mov rbx, rdi            ; size
    mov r12, rsi            ; type

    mov rdi, rbx
    call ap_malloc

    ; Initialize header
    mov qword [rax + PyObject.ob_refcnt], 1
    mov [rax + PyObject.ob_type], r12

    pop r12
    pop rbx
    leave
    ret
END_FUNC obj_alloc

; obj_incref(PyObject *obj)
; Increment reference count; NULL-safe.
; Callers must only pass heap pointers (not SmallInts).
DEF_FUNC_BARE obj_incref
    test rdi, rdi
    jz .skip
    inc qword [rdi + PyObject.ob_refcnt]
.skip:
    ret
END_FUNC obj_incref

; obj_decref(PyObject *obj)
; Decrement reference count; deallocate if zero; NULL-safe.
; Callers must only pass heap pointers (not SmallInts).
DEF_FUNC_BARE obj_decref
    test rdi, rdi
    jz .skip
    dec qword [rdi + PyObject.ob_refcnt]
    jnz .skip
    ; refcount hit zero - deallocate
    jmp obj_dealloc
.skip:
    ret
END_FUNC obj_decref

; obj_dealloc(PyObject *obj)
; Calls type's tp_dealloc if present, else just frees
DEF_FUNC_BARE obj_dealloc

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; Weak references to this object have to be emptied, and their callbacks
    ; run, before it is freed.  The links live in a side table rather than in
    ; the object, so the check is one compare against a counter that stays
    ; zero in a program that makes no weak references.
    extern weakref_live
    cmp qword [rel weakref_live], 0
    je .no_weakrefs
    extern weakref_clear_for
    mov rdi, rbx
    call weakref_clear_for
    mov rdi, rbx
.no_weakrefs:

    ; Get type's tp_dealloc
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .just_free
    mov rax, [rax + PyTypeObject.tp_dealloc]
    test rax, rax
    jz .just_free

    ; Call tp_dealloc(obj)
    mov rdi, rbx
    call rax
    pop rbx
    pop rbp
    ret

.just_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    pop rbp
.bail:
    ret
END_FUNC obj_dealloc

; obj_repr(rdi=value) -> PyObject* (string)
; Decodes the Value, then dispatches: int immediate → int_repr, pointer → tp_repr.
DEF_FUNC obj_repr
    extern str_repr
    V_UNPACK rdi, rsi

    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_FLOAT
    je .float_tag

    ; TAG_PTR: use tp_repr
    test rdi, rdi
    jz .null_obj

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .no_repr
    mov rax, [rax + PyTypeObject.tp_repr]
    test rax, rax
    jz .no_repr

    ; tail-call tp_repr(obj, tag)
    mov edx, esi               ; pass tag for tag-aware repr (e.g., int_repr)
    leave
    jmp rax

.smallint:
    ; rdi = raw int value — int_repr checks edx for TAG_SMALLINT
    RET_TAG_SMALLINT
    call int_repr
    leave
    ret

.float_tag:
    ; rdi = raw double bits — pass directly to float_repr
    call float_repr
    leave
    ret

.none_tag:
    call none_repr
    leave
    ret

.bool_tag:
    test rdi, rdi
    jz .bool_false_repr
    lea rdi, [rel bool_true]
    call bool_repr
    leave
    ret
.bool_false_repr:
    lea rdi, [rel bool_false]
    call bool_repr
    leave
    ret

.null_obj:
.no_repr:
    ; Return a NULL *Value*, not just a zero payload: callers test the tag,
    ; and leaving edx stale made print() dereference the NULL.
    RET_NULL
    leave
    ret
END_FUNC obj_repr

; obj_str(rdi=value) -> PyObject* (string)
; Decodes the Value, then dispatches: int immediate → int_repr, pointer → tp_str
; falling back to tp_repr.
DEF_FUNC obj_str
    V_UNPACK rdi, rsi
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi               ; save tag

    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_FLOAT
    je .float_tag

    ; TAG_PTR path
    test rdi, rdi
    jz .fallback

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .fallback

    mov rax, [rax + PyTypeObject.tp_str]
    test rax, rax
    jz .fallback

    mov rdi, rbx
    mov edx, r12d              ; tag for tp_str (e.g., int_repr checks edx)
    call rax
    pop r12
    pop rbx
    leave
    ret

.smallint:
    ; SmallInt: delegate to int_repr
    mov rdi, rbx
    RET_TAG_SMALLINT
    call int_repr
    pop r12
    pop rbx
    leave
    ret

.float_tag:
    ; rbx = raw double bits — pass directly to float_repr
    mov rdi, rbx
    call float_repr
    pop r12
    pop rbx
    leave
    ret

.none_tag:
    call none_repr
    pop r12
    pop rbx
    leave
    ret

.bool_tag:
    test rbx, rbx
    jz .bool_false_str
    lea rdi, [rel bool_true]
    call bool_repr                 ; bool tp_str = bool_repr
    pop r12
    pop rbx
    leave
    ret
.bool_false_str:
    lea rdi, [rel bool_false]
    call bool_repr
    pop r12
    pop rbx
    leave
    ret

.fallback:
    mov rdi, rbx
    mov rsi, r12
    V_PACK rdi, rsi
    call obj_repr
    pop r12
    pop rbx
    leave
    ret
END_FUNC obj_str

; obj_as_index(rdi = payload, edx = tag) -> rax = int64
;
; Convert a Value to a C index, or raise TypeError.  Callers used to hand
; whatever they were given straight to int_to_i64, which reads
; PyIntObject.compact unconditionally: a float's payload is raw IEEE bits, so
; range(1.5) dereferenced 0x3FF8000000000000, and None's fields decoded as a
; garbage length, so range(None) hung.
;
; Takes the same (payload, tag) pair as int_to_i64 so a call site changes by
; one word.  This is where the __index__ protocol belongs once heaptypes
; carry real slots.
DEF_FUNC obj_as_index
    cmp edx, TAG_SMALLINT
    je .oai_immediate
    cmp edx, TAG_PTR
    jne .oai_error
    mov rax, [rdi + PyObject.ob_type]
    REQUIRE_INT_TYPE rax, rcx, .oai_try_dunder
    call int_to_i64
    leave
    ret

.oai_immediate:
    mov rax, rdi
    leave
    ret

.oai_try_dunder:
    ; Not an int, but __index__ makes an object usable wherever one is
    ; wanted -- as a subscript, a repetition count, a slice bound, or an
    ; argument to hex().  This is the single place all of those converge.
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .oai_error
    mov rax, [rax + PyNumberMethods.nb_index]
    test rax, rax
    jz .oai_error
    call rax                    ; nb_index returns a Value
    V_UNPACK rax, rdx
    ; __index__ must itself return an int; one level only, so a class whose
    ; __index__ returns another such class is an error rather than a loop.
    cmp edx, TAG_SMALLINT
    je .oai_dunder_done
    cmp edx, TAG_PTR
    jne .oai_bad_index
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_INT_TYPE rcx, rsi, .oai_bad_index
    mov rdi, rax
    call int_to_i64
    leave
    ret
.oai_dunder_done:
    leave
    ret

.oai_bad_index:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__index__ returned non-int"
    call raise_exception

.oai_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object cannot be interpreted as an integer"
    call raise_exception
END_FUNC obj_as_index

; value_number_methods(rdi = payload, edx = tag) -> rax = PyNumberMethods*, or 0
;
; Resolve a Value's numeric protocol table, immediates included.  Callers that
; want an arithmetic slot need this rather than assuming int: builtin_divmod
; called int_floordiv unconditionally, so divmod(1.5, 1.5) crashed even though
; 1.5 // 1.5 has always worked.
DEF_FUNC_BARE value_number_methods
    cmp edx, TAG_SMALLINT
    je .vnm_int
    cmp edx, TAG_FLOAT
    je .vnm_float
    cmp edx, TAG_PTR
    jne .vnm_none
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    ret
.vnm_int:
    lea rax, [rel int_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    ret
.vnm_float:
    lea rax, [rel float_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    ret
.vnm_none:
    xor eax, eax
    ret
END_FUNC value_number_methods

; value_type(rdi = Value) -> rax = PyTypeObject*, or 0 for a NULL Value
;
; Resolve a Value's type, immediates included.  Several places open-code this
; three-way test; having it once keeps them from disagreeing.
DEF_FUNC_BARE value_type
    V_IS_INT rdi, rax
    jae .vt_int
    V_IS_FLOAT rdi, rax
    jb .vt_float
    test rdi, rdi
    jz .vt_null
    mov rax, [rdi + PyObject.ob_type]
    ; A heaptype's metatype is an internal split -- it exists only so that
    ; heaptypes get a tp_dealloc that static types must not have.  CPython
    ; has one `type`, and `type(C) is type` for an ordinary class, so report
    ; the one the language defines.
    extern user_type_metatype
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    jne .vt_done
    lea rax, [rel type_type]
.vt_done:
    ret
.vt_int:
    lea rax, [rel int_type]
    ret
.vt_float:
    lea rax, [rel float_type]
    ret
.vt_null:
    xor eax, eax
    ret
END_FUNC value_type

; raise_type_error_with_name(rdi = template C string with a single %s marker
;                            written as \x01, rsi = Value whose type to name)
; Composes the message into a static buffer and raises TypeError.  Does not
; return.
RTN_BUFSZ equ 160
global raise_type_error_with_name
DEF_FUNC raise_type_error_with_name
    push rbx
    push r12
    mov rbx, rdi
    mov rdi, rsi
    call value_type
    mov r12, rax                    ; type, or 0

    lea rdi, [rel rtn_buf]
    xor ecx, ecx
.rtn_copy:
    movzx eax, byte [rbx]
    test al, al
    jz .rtn_end
    inc rbx
    cmp al, 1
    je .rtn_insert
    cmp rcx, RTN_BUFSZ - 2
    jae .rtn_copy
    mov [rdi + rcx], al
    inc rcx
    jmp .rtn_copy
.rtn_insert:
    test r12, r12
    jz .rtn_copy
    mov rsi, [r12 + PyTypeObject.tp_name]
.rtn_name:
    movzx eax, byte [rsi]
    test al, al
    jz .rtn_copy
    inc rsi
    cmp rcx, RTN_BUFSZ - 2
    jae .rtn_copy
    mov [rdi + rcx], al
    inc rcx
    jmp .rtn_name
.rtn_end:
    mov byte [rdi + rcx], 0
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel rtn_buf]
    extern exc_TypeError_type
    extern raise_exception
    call raise_exception
    ud2
END_FUNC raise_type_error_with_name

section .bss
rtn_buf: resb RTN_BUFSZ
section .text

; seq_repeat_check_count(rsi = count Value) -- raises TypeError unless the
; count is an int (or a bool, which is one).  Does not return on failure.
global seq_repeat_check_count
DEF_FUNC_BARE seq_repeat_check_count
    V_IS_INT rsi, rax
    jae .src_ok
    V_TEST_PTR rsi, rax
    ja .src_bad
    test rsi, rsi
    jz .src_bad
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .src_ok
    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .src_ok
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jnz .src_ok
.src_bad:
    CSTRING rdi, `can't multiply sequence by non-int of type '\x01'`
    jmp raise_type_error_with_name
.src_ok:
    ret
END_FUNC seq_repeat_check_count

; raise_no_attribute(rdi = object Value, rsi = attribute-name str, edx = 1 for
; a set, 0 for a get) -- raises the AttributeError CPython raises.  Does not
; return.
RNA_OBJ  equ 8
RNA_NAME equ 16
RNA_FRAME equ 16
extern str_type
global raise_no_attribute
DEF_FUNC raise_no_attribute, RNA_FRAME
    push rbx
    push r12
    mov [rbp - RNA_NAME], rsi
    call value_type
    mov r12, rax

    lea rbx, [rel rtn_buf]
    xor ecx, ecx
    mov byte [rbx], 39                  ; '
    inc rcx
    test r12, r12
    jz .rna_after_type
    mov rsi, [r12 + PyTypeObject.tp_name]
.rna_type:
    movzx eax, byte [rsi]
    test al, al
    jz .rna_after_type
    inc rsi
    cmp rcx, RTN_BUFSZ - 2
    jae .rna_after_type
    mov [rbx + rcx], al
    inc rcx
    jmp .rna_type
.rna_after_type:
    CSTRING rsi, `' object has no attribute '`
.rna_mid:
    movzx eax, byte [rsi]
    test al, al
    jz .rna_name
    inc rsi
    cmp rcx, RTN_BUFSZ - 2
    jae .rna_name
    mov [rbx + rcx], al
    inc rcx
    jmp .rna_mid
.rna_name:
    mov rsi, [rbp - RNA_NAME]
    test rsi, rsi
    jz .rna_close
    mov rax, [rsi + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rax, rdx
    jne .rna_close
    mov rdx, [rsi + PyStrObject.ob_size]
    lea rsi, [rsi + PyStrObject.data]
    xor eax, eax
.rna_name_copy:
    cmp rax, rdx
    jge .rna_close
    cmp rcx, RTN_BUFSZ - 3
    jae .rna_close
    mov r8b, [rsi + rax]
    mov [rbx + rcx], r8b
    inc rcx
    inc rax
    jmp .rna_name_copy
.rna_close:
    mov byte [rbx + rcx], 39            ; '
    inc rcx
    mov byte [rbx + rcx], 0
    lea rdi, [rel exc_AttributeError_type]
    extern exc_AttributeError_type
    mov rsi, rbx
    call raise_exception
    ud2
END_FUNC raise_no_attribute

;; ============================================================================
;; obj_generic_attr(rdi = object Value, rsi = name str) -> Value, or 0
;;
;; The attributes every object has regardless of type.  They used to be
;; nobody's job: each tp_getattr special-cased its own names and there was no
;; shared tail, so `(5).__class__` and `obj.__dict__` were AttributeErrors on
;; every type in the tree.  Called from the miss path of the attribute
;; lookups, so a type that defines one of these itself still wins.
;;
;; Returns a new reference, or 0 when the name is not one of these.
;; ============================================================================
OGA_OBJ   equ 8
OGA_NAME  equ 16
OGA_FRAME equ 32
global obj_generic_attr
DEF_FUNC obj_generic_attr, OGA_FRAME
    push rbx
    mov [rbp - OGA_OBJ], rdi
    mov [rbp - OGA_NAME], rsi

    test rsi, rsi
    jz .oga_none
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .oga_none

    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__class__"
    call ap_strcmp
    test eax, eax
    jz .oga_class

    mov rdi, [rbp - OGA_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "__dict__"
    call ap_strcmp
    test eax, eax
    jz .oga_dict

.oga_none:
    xor eax, eax
    pop rbx
    leave
    ret

.oga_class:
    ; Every value has a type, including the immediates.
    mov rdi, [rbp - OGA_OBJ]
    call value_type
    test rax, rax
    jz .oga_none
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
    pop rbx
    leave
    ret

.oga_dict:
    ; Only an object with a real instance dict has one.  tp_dictoffset is 0
    ; for every static type and for the layouts that cannot host a dict
    ; (str subclasses, __slots__ classes) -- those correctly have no
    ; __dict__, as in CPython.
    mov rdi, [rbp - OGA_OBJ]
    V_TEST_PTR rdi, rax
    ja .oga_none
    test rdi, rdi
    jz .oga_none
    LOAD_INST_DICT rbx, rdi, .oga_none
    test rbx, rbx
    jnz .oga_dict_have
    ; Not created yet: an instance gets its dict on first use, so asking for
    ; it has to create one or the attribute would come and go.
    call dict_new
    mov rbx, rax
    mov rdi, [rbp - OGA_OBJ]
    STORE_INST_DICT rdi, rbx, rcx, .oga_dict_have
.oga_dict_have:
    mov rdi, rbx
    call obj_incref
    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC obj_generic_attr

; obj_richcompare_bool(rdi = left Value, rsi = right Value, edx = op)
;   -> eax = 1 (true), 0 (false), or -1 (an exception is pending)
;
; CPython's PyObject_RichCompareBool, which is what every container search
; uses and what none of them used here.  Nine sites open-coded a comparison
; and treated a NULL result as "not equal" -- but NULL means either
; NotImplemented, in which case the reflected operand and then identity must
; be tried, or that the comparison raised, in which case it must propagate.
; None of them read current_exception, so a raising __eq__ inside `x in list`
; silently answered False.
;
; The identity shortcut comes first, as in CPython: a container holding an
; object finds it even if its __eq__ is broken or raises.
ORB_LEFT  equ 8
ORB_RIGHT equ 16
ORB_OP    equ 24
ORB_EXC   equ 32
ORB_RES   equ 40
ORB_FRAME equ 48

DEF_FUNC obj_richcompare_bool, ORB_FRAME
    mov [rbp - ORB_LEFT], rdi
    mov [rbp - ORB_RIGHT], rsi
    mov [rbp - ORB_OP], rdx

    ; Hold a strong reference to both operands for the duration.  A
    ; comparison can run arbitrary Python: CPython's own
    ; test_count_index_remove_crashes has an __eq__ that clears the very list
    ; being searched, which frees the element the caller handed us as a
    ; borrowed slot reference (bpo-38610).  Doing it here rather than in each
    ; of the six search loops means no loop can forget.
    INCREF_V rdi, rax
    INCREF_V rsi, rax

    ; Identity: for == this is true and for != false, without consulting the
    ; type at all.  One compare, since a Value is one word.
    mov rdi, [rbp - ORB_LEFT]
    cmp rdi, [rbp - ORB_RIGHT]
    jne .orb_compare
    mov edx, [rbp - ORB_OP]
    cmp edx, PY_EQ
    je .orb_true
    cmp edx, PY_NE
    je .orb_false

.orb_compare:
    DUNDER_EXC_SAVE [rbp - ORB_EXC]

    ; Left operand's tp_richcompare.
    mov rdi, [rbp - ORB_LEFT]
    call value_type
    test rax, rax
    jz .orb_identity
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .orb_reflected
    mov rdi, [rbp - ORB_LEFT]
    mov rsi, [rbp - ORB_RIGHT]
    mov edx, [rbp - ORB_OP]
    call rax
    test rax, rax
    jnz .orb_have_result
    DUNDER_RAISED [rbp - ORB_EXC], .orb_error

.orb_reflected:
    ; NotImplemented from the left: try the right operand with the op
    ; reversed, which is how a subclass or a mixed-type comparison gets its
    ; say.
    mov rdi, [rbp - ORB_RIGHT]
    call value_type
    test rax, rax
    jz .orb_identity
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .orb_identity
    mov rdi, [rbp - ORB_RIGHT]
    mov rsi, [rbp - ORB_LEFT]
    mov edx, [rbp - ORB_OP]
    lea rcx, [rel orb_swap_table]
    movsxd rdx, edx
    mov edx, [rcx + rdx*4]      ; the reversed op
    call rax
    test rax, rax
    jnz .orb_have_result
    DUNDER_RAISED [rbp - ORB_EXC], .orb_error

.orb_identity:
    ; Neither side had an opinion.  Equality falls back to identity, which
    ; the fast path above already ruled out, so the answer is fixed.
    mov edx, [rbp - ORB_OP]
    cmp edx, PY_EQ
    je .orb_false
    cmp edx, PY_NE
    je .orb_true
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unorderable types"
    call raise_exception

.orb_have_result:
    mov [rbp - ORB_RES], rax    ; the result Value, owned
    mov rdi, rax
    call obj_is_true
    mov [rbp - ORB_OP], rax     ; the op is finished with; reuse the slot
    mov rdi, [rbp - ORB_RES]
    DECREF_V rdi, rdx
    mov rax, [rbp - ORB_OP]
    jmp .orb_done

.orb_true:
    mov eax, 1
    jmp .orb_done

.orb_false:
    xor eax, eax
    jmp .orb_done

.orb_error:
    mov eax, -1

.orb_done:
    mov [rbp - ORB_RES], rax
    mov rdi, [rbp - ORB_LEFT]
    DECREF_V rdi, rdx
    mov rdi, [rbp - ORB_RIGHT]
    DECREF_V rdi, rdx
    mov rax, [rbp - ORB_RES]
    leave
    ret
END_FUNC obj_richcompare_bool

section .rodata
align 4
orb_swap_table:
    dd PY_GT                    ; PY_LT reversed
    dd PY_GE                    ; PY_LE
    dd PY_EQ                    ; PY_EQ
    dd PY_NE                    ; PY_NE
    dd PY_LT                    ; PY_GT
    dd PY_LE                    ; PY_GE
section .text

; hash_not_implemented() -> never returns
; Used as tp_hash for unhashable types (dict, list, set).
; Raises TypeError("unhashable type").
global hash_not_implemented
DEF_FUNC hash_not_implemented
    extern raise_exception
    extern exc_TypeError_type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unhashable type"
    call raise_exception
END_FUNC hash_not_implemented

; obj_hash(rdi=value) -> int64
; Decodes the Value, then dispatches: int immediate → int_hash_i64, pointer → tp_hash.
DEF_FUNC obj_hash
    V_UNPACK rdi, rsi

    cmp esi, TAG_SMALLINT
    je .smallint_hash
    cmp esi, TAG_FLOAT
    je .float_hash

    ; TAG_PTR path
    test rdi, rdi
    jz .default_hash

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .default_hash
    mov rax, [rax + PyTypeObject.tp_hash]
    test rax, rax
    jz .default_hash

    ; tail-call tp_hash(rdi=obj, edx=tag)
    ; tp_hash implementations (int_hash) forward edx to int_unwrap, so the
    ; tag MUST be supplied here -- leaving edx undefined makes int_unwrap
    ; take a random branch and int_hash return the object address.
    mov edx, esi
    leave
    jmp rax

.smallint_hash:
    ; Shared with int_hash / builtin_hash: sign(v) * (|v| mod 2^61-1).
    ; All three must agree or dict and set lookups silently break.
    extern int_hash_i64
    leave
    jmp int_hash_i64

.float_hash:
    ; Inline float: call float_hash for PEP-correct integer-float matching
    extern float_hash
    call float_hash
    leave
    ret

.bool_hash:
    ; Hash of bool: 0 for False, 1 for True (matches Python int hash)
    mov rax, rdi
    leave
    ret

.none_hash:
    ; Hash of None: constant (avoids -1 which is reserved error value)
    mov eax, 0x48ae2ce5
    leave
    ret

.default_hash:
    ; Default: hash is the object address
    mov rax, rdi
    leave
    ret
END_FUNC obj_hash

; obj_is_true(rdi=value) -> int (0 or 1)
; Decodes the Value, then dispatches: int immediate → value != 0, pointer → type-based.
DEF_FUNC_BARE obj_is_true
    V_UNPACK rdi, rsi

    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_FLOAT
    je .float_tag

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; None is false (legacy — TAG_PTR none_singleton)
    lea rax, [rel none_singleton]
    cmp rbx, rax
    je .false

    ; bool False is false (legacy — TAG_PTR bool_false)
    lea rax, [rel bool_false]
    cmp rbx, rax
    je .false

    ; Check for nb_bool in type's number methods
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .true
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .check_seq_len
    mov rax, [rax + PyNumberMethods.nb_bool]
    test rax, rax
    jz .check_seq_len
    mov rdi, rbx
    call rax
    pop rbx
    pop rbp
    ret

.check_seq_len:
    ; Check for sq_length in type's sequence methods
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .check_map_len
    mov rax, [rax + PySequenceMethods.sq_length]
    test rax, rax
    jz .check_map_len
    mov rdi, rbx
    call rax
    test rax, rax
    jnz .true
    jmp .false

.check_map_len:
    ; Check for mp_length in type's mapping methods
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .check_dunder_bool
    mov rax, [rax + PyMappingMethods.mp_length]
    test rax, rax
    jz .check_dunder_bool
    mov rdi, rbx
    call rax
    test rax, rax
    jnz .true
    jmp .false

.check_dunder_bool:
    ; Try __bool__ dunder on heaptype
    mov rax, [rbx + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .true                ; default: objects are truthy

    ; Look up __bool__ in type dict to check for None
    extern dunder_bool
    extern dunder_lookup
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel dunder_bool]
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .check_dunder_len       ; not found (TAG_NULL) → try __len__

    ; Check if __bool__ is None → TypeError
    ; Handle both inline (0, TAG_NONE) and pointer (none_singleton, TAG_PTR) forms
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .dunder_bool_none_error

    ; Call __bool__ via dunder_call_1
    extern dunder_call_1
    mov rdi, rbx
    lea rsi, [rel dunder_bool]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx              ; TAG_NULL = call failed
    jz .check_dunder_len

    ; __bool__ returned a result — must be bool
    ; Check TAG_PTR pointing to bool_type
    cmp edx, TAG_PTR
    jne .dunder_bool_type_error
    test rax, rax
    jz .dunder_bool_type_error
    mov rcx, [rax + PyObject.ob_type]
    extern bool_type
    lea r8, [rel bool_type]
    cmp rcx, r8
    jne .dunder_bool_type_error
    ; TAG_PTR bool singleton: convert to 0/1
    lea rcx, [rel bool_true]
    cmp rax, rcx
    sete al
    movzx eax, al
    pop rbx
    pop rbp
    ret

.dunder_bool_is_bool:
    ; Result is TAG_BOOL: rax payload is 0 or 1
    pop rbx
    pop rbp
    ret

.dunder_bool_none_error:
    extern raise_exception
    extern exc_TypeError_type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "cannot interpret 'NoneType' object as an integer"
    call raise_exception

.dunder_bool_type_error:
    ; __bool__ didn't return bool — DECREF result and raise TypeError
    ; rax=payload, edx=tag from dunder_call_1
    mov rdi, rax
    mov esi, edx
    DECREF_VAL rdi, rsi
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__bool__ should return bool, returned non-bool"
    call raise_exception

.check_dunder_len:
    ; Try __len__ dunder
    extern dunder_len
    mov rdi, rbx
    lea rsi, [rel dunder_len]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx              ; TAG_NULL = not found
    jz .true                ; no __len__ → truthy by default

    ; __len__ returned a result — check for negative
    cmp edx, TAG_SMALLINT
    jne .len_check_ptr
    ; SmallInt: check if negative
    test rax, rax
    js .len_negative_error
    ; Non-negative SmallInt: truthy if != 0
    test rax, rax
    setnz al
    movzx eax, al
    pop rbx
    pop rbp
    ret

.len_check_ptr:
    ; Non-SmallInt result: use obj_is_true
    push rdx                   ; save tag
    push rax                   ; save payload
    mov rdi, rax
    mov rsi, rdx
    V_PACK rdi, rsi
    call obj_is_true
    mov ecx, eax
    pop rdi                    ; payload
    pop rsi                    ; tag
    DECREF_VAL rdi, rsi
    mov eax, ecx
    pop rbx
    pop rbp
    ret

.len_negative_error:
    extern exc_ValueError_type
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "__len__() should return >= 0"
    call raise_exception

.false:
    xor eax, eax
    pop rbx
    pop rbp
    ret

.true:
    mov eax, 1
    pop rbx
    pop rbp
    ret

.smallint:
    ; SmallInt is true iff raw value != 0
    test rdi, rdi
    setnz al
    movzx eax, al
    ret

.float_tag:
    ; Inline float: true iff not 0.0 and not -0.0
    movq xmm0, rdi
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    setne al
    setp cl                    ; NaN is truthy
    or al, cl
    movzx eax, al
    ret

.bool_tag:
    ; TAG_BOOL: payload = 0 (False) or 1 (True)
    mov eax, edi
    and eax, 1
    ret

.none_tag:
    ; TAG_NONE: always false
    xor eax, eax
    ret
END_FUNC obj_is_true

; obj_print(PyObject *obj)
; Print an object's string representation to stdout followed by newline
DEF_FUNC obj_print
    push rbx
    mov rbx, rdi

    ; Get string representation via obj_str(payload, tag)
    call obj_str
    test rax, rax
    jz .print_null

    mov rbx, rax            ; rbx = str obj (heap)

    ; sys_write(1, str_data, ob_size)
    mov edi, 1
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, [rbx + PyStrObject.ob_size]
    call sys_write

    ; sys_write(1, "\n", 1)
    mov edi, 1
    lea rsi, [rel obj_print_newline]
    mov edx, 1
    call sys_write

    pop rbx
    leave
    ret

.print_null:
    ; sys_write(1, "<NULL>\n", 7)
    mov edi, 1
    lea rsi, [rel obj_print_null_str]
    mov edx, 7
    call sys_write

    pop rbx
    leave
    ret
END_FUNC obj_print

;; ============================================================================
;; type_repr(PyObject *type_obj) -> PyStrObject*
;; Formats "<class 'name'>" for a type object.
;; ============================================================================
TR_TYPE  equ 8
TR_LEN   equ 16
TR_BUF   equ 272            ; 256 bytes, [rbp-272, rbp-16)
TR_FRAME equ 288
DEF_FUNC type_repr, TR_FRAME
    push rbx
    push r12
    mov [rbp - TR_TYPE], rdi

    mov rax, [rdi + PyTypeObject.tp_name]  ; C string pointer
    test rax, rax
    jz .type_repr_unknown

    lea rbx, [rbp - TR_BUF]
    CSTRING rsi, `<class '`
    xor r12d, r12d
.tr_open:
    movzx eax, byte [rsi]
    test al, al
    jz .tr_module
    inc rsi
    mov [rbx + r12], al
    inc r12
    jmp .tr_open

.tr_module:
    ; CPython qualifies a class with its module: <class '__main__.C'>.  Only
    ; the bare name was printed, so every class repr differed from CPython's.
    ; Builtins live in "builtins" and are shown unqualified.
    mov rdi, [rbp - TR_TYPE]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tr_name
    mov [rbp - TR_LEN], rdi         ; the type dict
    CSTRING rdi, "__module__"
    call str_from_cstr
    mov rsi, rax
    mov rdi, [rbp - TR_LEN]
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .tr_name
    cmp edx, TAG_PTR
    jne .tr_name
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .tr_name
    mov rdx, [rax + PyStrObject.ob_size]
    test rdx, rdx
    jz .tr_name
    cmp rdx, 8
    jne .tr_copy_module
    lea rdi, [rax + PyStrObject.data]
    CSTRING rsi, "builtins"
    push rax
    push rdx
    call ap_strcmp
    pop rdx
    pop rax
    test eax, eax
    jz .tr_name                 ; module is "builtins": leave it off
.tr_copy_module:
    lea rsi, [rax + PyStrObject.data]
    xor ecx, ecx
.tr_mod_loop:
    cmp rcx, rdx
    jge .tr_mod_done
    cmp r12, 200
    jae .tr_mod_done
    mov al, [rsi + rcx]
    mov [rbx + r12], al
    inc r12
    inc rcx
    jmp .tr_mod_loop
.tr_mod_done:
    mov byte [rbx + r12], '.'
    inc r12

.tr_name:
    mov rax, [rbp - TR_TYPE]
    mov rsi, [rax + PyTypeObject.tp_name]
.tr_name_loop:
    movzx eax, byte [rsi]
    test al, al
    jz .tr_close
    inc rsi
    cmp r12, 250
    jae .tr_close
    mov [rbx + r12], al
    inc r12
    jmp .tr_name_loop

.tr_close:
    mov byte [rbx + r12], 0x27
    mov byte [rbx + r12 + 1], '>'
    mov byte [rbx + r12 + 2], 0
    mov rdi, rbx
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret

.type_repr_unknown:
    lea rdi, [rel type_repr_unknown_str]
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_repr

section .rodata
align 8
extern union_type_or
global type_number_methods
type_number_methods:
    times 15 dq 0
    dq union_type_or          ; nb_or (+120): `int | str` builds a UnionType
    times 20 dq 0

section .rodata
obj_print_newline: db 10
obj_print_null_str: db "<NULL>", 10
type_repr_unknown_str: db "<class '?'>", 0
type_type_name: db "type", 0

section .data
align 8
global type_type
type_type:
    dq 1                      ; ob_refcnt (immortal)
    dq type_type              ; ob_type (self-referential)
    dq type_type_name         ; tp_name
    dq TYPE_OBJECT_SIZE       ; tp_basicsize
    dq 0                      ; tp_dealloc
    dq type_repr              ; tp_repr
    dq type_repr              ; tp_str
    dq 0                      ; tp_hash
    dq type_call              ; tp_call — calling a type creates instances
    dq type_getattr           ; tp_getattr — __name__, tp_dict lookups
    dq type_setattr           ; tp_setattr
    dq 0                      ; tp_richcompare
    dq 0                      ; tp_iter
    dq 0                      ; tp_iternext
    dq 0                      ; tp_init
    dq 0                      ; tp_new
    dq type_number_methods    ; tp_as_number -- PEP 604: int | str
    dq 0                      ; tp_as_sequence
    dq 0                      ; tp_as_mapping
    dq 0                      ; tp_base
    dq 0                      ; tp_dict
    dq 0                      ; tp_mro
    dq TYPE_FLAG_METATYPE     ; tp_flags — instances of `type` are classes
    dq 0                      ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
