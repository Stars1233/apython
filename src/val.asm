; val.asm - Fat value conversion helpers for 128-bit values

%include "macros.inc"
%include "object.inc"

;; ============================================================================
;; fat_to_obj(rdi: payload, rsi: tag) -> rax: PyObject* (owned ref)
;;
;; Convert a fat (payload, tag) pair to a 64-bit heap PyObject*.
;; Returns an owned reference (INCREFs heap pointers).
;; ============================================================================
extern none_singleton
extern bool_true
extern bool_false
extern obj_incref

extern smallstr_to_obj

DEF_FUNC fat_to_obj
    ; SmallStr: spill to heap object
    test rsi, rsi
    js .smallstr

    cmp esi, TAG_PTR
    je .ptr
    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_NONE
    je .none
    cmp esi, TAG_BOOL
    je .bool
    ; TAG_FLOAT, TAG_NULL or unknown: return NULL
    xor eax, eax
    leave
    ret

.ptr:
    ; Heap pointer: INCREF and return
    mov rax, rdi
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.smallint:
    ; Create a heap-allocated PyIntObject from raw int64 payload
    extern int_from_i64_gmp
    call int_from_i64_gmp      ; rdi already has the int value
    leave
    ret

.none:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.bool:
    test rdi, rdi
    jz .bool_false
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.bool_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.smallstr:
    ; SmallStr → heap PyStrObject* (owned ref)
    call smallstr_to_obj       ; rax = PyStrObject*
    leave
    ret
END_FUNC fat_to_obj
