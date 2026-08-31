; pyo/class.asm - Class instances and bound methods for apython
; Phase 10: class instantiation, attribute access, __init__ dispatch

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_decref
extern obj_incref
extern obj_dealloc
extern dict_new
extern dict_get
extern dict_set
extern str_from_cstr
extern str_from_cstr_heap
extern ap_strcmp
extern type_repr
extern fatal_error
extern raise_exception
extern attr_error_pending
extern exc_AttributeError_type
extern exc_TypeError_type
extern func_type
extern type_type
extern tuple_type_call
extern kw_names_pending
extern ap_memcpy
extern eval_exception_unwind
extern none_singleton
extern dunder_lookup
extern sys_write
extern current_exception
extern dict_type
extern tuple_type
extern int_type
extern str_type
extern staticmethod_type
extern classmethod_type
extern property_type
extern eval_frame
extern frame_free

;; ============================================================================
;; instance_new(PyTypeObject *type) -> PyInstanceObject*
;; Allocate a new instance of the given class type.
;; rdi = type (the class)
;; Returns: new instance with refcnt=1, ob_type=type, inst_dict=new dict
;; ============================================================================
;; ============================================================================
;; builtin_sub_init_base(rdi = instance)
;;
;; Give the embedded base portion of a builtin-container subclass a valid
;; empty state.  instance_new zeroes the body, which is already a correct
;; empty tuple, but list and dict want a real backing array -- a NULL
;; ob_item is how list marks "currently being sorted", so the first
;; l.append() on a fresh subclass instance reported "list modified during
;; sort".
;; ============================================================================

;; ============================================================================
;; int_sub_new(rdi = type, rsi = args, rdx = nargs) -> (rax, rdx) value pair
;;
;; An int, or an instance of an int subclass carrying one.  It is what
;; `int(...)` does for such a type, reachable as a function so that
;; `int.__new__(cls, v)` can build the instance WITHOUT going back through
;; cls.__new__ -- which is how enum makes its members, and would otherwise
;; recurse forever.
;; ============================================================================
ISN_TYPE  equ 8
ISN_VAL   equ 16
ISN_TAG   equ 24
ISN_FRAME equ 32          ; + 2 pushes = 48
DEF_FUNC int_sub_new, ISN_FRAME
    push rbx
    push r12
    mov [rbp - ISN_TYPE], rdi
    mov rdi, rsi
    mov rsi, rdx
    extern builtin_int_fn
    call builtin_int_fn
    test edx, edx
    jz .isn_fail
    mov [rbp - ISN_VAL], rax
    mov [rbp - ISN_TAG], rdx

    ; int itself takes the bare value; a subclass wraps it.
    mov rbx, [rbp - ISN_TYPE]
    lea rcx, [rel int_type]
    cmp rbx, rcx
    je .isn_bare

    mov edi, PyIntSubclassObject_size
    mov rsi, rbx
    call gc_alloc
    mov r12, rax
    mov qword [r12 + PyIntSubclassObject.inst_dict], 0
    mov rax, [rbp - ISN_VAL]
    mov rdx, [rbp - ISN_TAG]
    V_PACK rax, rdx
    mov [r12 + PyIntSubclassObject.int_value], rax   ; the reference transfers
    mov rdi, rbx
    INCREF rdi
    mov rdi, r12
    call gc_track
    mov rax, r12
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.isn_bare:
    mov rax, [rbp - ISN_VAL]
    mov rdx, [rbp - ISN_TAG]
    pop r12
    pop rbx
    leave
    ret
.isn_fail:
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret
END_FUNC int_sub_new

;; ============================================================================
;; str_sub_new(rdi = subclass type, rsi = args, rdx = nargs) -> instance
;;
;; A str keeps its characters inline, so its instances are variable-size and
;; instance_new -- which allocates exactly tp_basicsize -- cannot make one.
;; A str subclass therefore has to be built here, from the argument, the way
;; str's own constructor would.  Without this the instance was an empty
;; string of the right type, so CustomStr("100") was "".
;;
;; The instance carries a __dict__ at its tail, past the data and its padding,
;; because there is no fixed offset past inline data to put one at.  The extra
;; word is allocated here and tp_dictoffset says TP_DICT_AT_TAIL.
;; ============================================================================
SSN_TYPE  equ 8
SSN_SRC   equ 16
SSN_FRAME equ 32

DEF_FUNC str_sub_new, SSN_FRAME
    push rbx
    push r12

    mov [rbp - SSN_TYPE], rdi
    mov qword [rbp - SSN_SRC], 0
    test rdx, rdx
    jz .ssn_empty

    ; str(x) of the argument gives a plain str to copy from.
    mov rdi, [rsi]
    extern obj_str
    call obj_str
    V_UNPACK rax, rdx
    test edx, edx
    jz .ssn_failed
    mov [rbp - SSN_SRC], rax
    mov rbx, rax
    mov r12, [rbx + PyStrObject.ob_size]
    jmp .ssn_have_src

.ssn_empty:
    xor ebx, ebx
    xor r12d, r12d

.ssn_have_src:
    ; header + length + 8, matching str_new_heap's padding for the 8-byte
    ; comparisons ap_strcmp does, + 8 more for the tail __dict__ pointer
    lea rdi, [r12 + PyStrObject.data + 16]
    mov rsi, [rbp - SSN_TYPE]
    extern gc_alloc
    call gc_alloc                   ; sets ob_refcnt and ob_type
    mov [rax + PyStrObject.ob_size], r12
    mov qword [rax + PyStrObject.ob_hash], -1
    mov [rax + PyStrObject.ob_length], r12   ; corrected after the copy
    mov qword [rax + PyStrObject.data + r12], 0
    mov qword [rax + PyStrObject.data + r12 + 8], 0     ; the tail __dict__

    test rbx, rbx
    jz .ssn_no_copy
    push rax
    lea rdi, [rax + PyStrObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy
    mov rdi, [rsp]
    extern str_set_length
    call str_set_length
    mov rdi, [rbp - SSN_SRC]
    call obj_decref
    pop rax

.ssn_no_copy:
    ; The tail __dict__, unless __slots__ suppresses it.  It is created here
    ; rather than lazily so that every consumer of LOAD_INST_DICT can keep
    ; reading a NULL as "this family has no dict at all".  SSN_SRC is dead by
    ; now -- the copy path decref'd it.
    mov [rbp - SSN_SRC], rax
    mov rdi, [rbp - SSN_TYPE]
    mov rcx, [rdi + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_HAS_SLOTS
    jnz .ssn_no_tail_dict
    cmp qword [rdi + PyTypeObject.tp_dictoffset], TP_DICT_AT_TAIL
    jne .ssn_no_tail_dict
    extern dict_new
    call dict_new
    mov rdx, [rbp - SSN_SRC]
    INST_DICT_TAIL rcx, rdx
    mov [rcx], rax
.ssn_no_tail_dict:
    mov rax, [rbp - SSN_SRC]

    ; gc_alloc does not INCREF the type it stamps into ob_type.
    push rax
    mov rdi, [rbp - SSN_TYPE]
    call obj_incref
    pop rax
    mov rdi, rax
    push rax
    extern gc_track
    call gc_track
    pop rax
    pop r12
    pop rbx
    leave
    ret

.ssn_failed:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_sub_new

;; ============================================================================
;; tuple_sub_fill(rdi = instance, rsi = args, rdx = nargs)
;;
;; A tuple is immutable and has no __init__, so a subclass cannot be filled
;; after the fact the way list, dict and set are -- the contents have to be
;; put in at construction, which is what tuple.__new__ does.  Without this a
;; tuple subclass was always empty.
;; ============================================================================
TSF_INST  equ 8
TSF_TMP   equ 16
TSF_FRAME equ 32

DEF_FUNC tuple_sub_fill, TSF_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - TSF_INST], rdi
    mov qword [rbp - TSF_TMP], 0
    mov qword [rdi + PyTupleObject.ob_hash], -1
    test rdx, rdx
    jz .tsf_done                ; Sub() is the empty tuple

    ; Materialise the argument, so any iterable works.
    push rsi
    lea rdi, [rel tuple_type]
    mov edx, 1
    pop rsi
    call tuple_type_call
    mov [rbp - TSF_TMP], rax
    mov rbx, rax
    mov r12, [rbx + PyTupleObject.ob_size]
    test r12, r12
    jz .tsf_release

    ; Own copy of the item array: the temporary is about to be released.
    mov rdi, r12
    shl rdi, 3
    call ap_malloc
    mov r13, rax
    mov rcx, [rbp - TSF_INST]
    mov [rcx + PyTupleObject.ob_item], r13
    mov [rcx + PyTupleObject.ob_size], r12

    xor ecx, ecx
.tsf_copy:
    cmp rcx, r12
    jge .tsf_release
    mov rax, [rbx + PyTupleObject.ob_item]
    mov rdi, [rax + rcx * 8]
    mov [r13 + rcx * 8], rdi
    push rcx
    INCREF_V rdi, rax
    pop rcx
    inc rcx
    jmp .tsf_copy

.tsf_release:
    mov rdi, [rbp - TSF_TMP]
    mov qword [rbp - TSF_TMP], 0
    call obj_decref

.tsf_done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_sub_fill

DEF_FUNC builtin_sub_init_base
    push rbx
    mov rbx, rdi
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_flags]

    test rax, TYPE_FLAG_LIST_SUBCLASS
    jnz .bsib_list
    test rax, TYPE_FLAG_DICT_SUBCLASS | TYPE_FLAG_SET_SUBCLASS
    jnz .bsib_dict
    jmp .bsib_done              ; tuple: zeroed is already an empty tuple

.bsib_list:
    mov edi, 4 * 8
    call ap_malloc
    mov [rbx + PyListObject.ob_item], rax
    mov qword [rbx + PyListObject.allocated], 4
    jmp .bsib_done

.bsib_dict:
    ; A dict now owns two arrays, and a set only one -- so let the dict's own
    ; allocator build them rather than hand-rolling a header that would be
    ; missing dk_indices.
    mov rdi, rbx
    mov rax, [rbx + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_SET_SUBCLASS
    jnz .bsib_set_table
    mov rsi, DICT_INIT_CAP
    extern dict_alloc_tables
    call dict_alloc_tables
    mov qword [rbx + PyDictObject.dk_nentries], 0
    jmp .bsib_done

.bsib_set_table:
    ; A set keeps the old single-array layout.
    mov edi, DICT_INIT_CAP * DICT_ENTRY_SIZE
    call ap_malloc
    mov [rbx + PyDictObject.entries], rax
    mov rdi, rax
    mov ecx, DICT_INIT_CAP * DICT_ENTRY_SIZE / 8
    xor eax, eax
    rep stosq
    mov qword [rbx + PyDictObject.capacity], DICT_INIT_CAP

.bsib_done:
    pop rbx
    leave
    ret
END_FUNC builtin_sub_init_base

DEF_FUNC instance_new
    push rbx
    push r12

    mov rbx, rdi                ; rbx = type

    ; Allocate using tp_basicsize (GC-tracked, supports __slots__)
    mov rdi, [rbx + PyTypeObject.tp_basicsize]
    push rdi                    ; save size for zero-fill
    mov rsi, rbx                ; type
    call gc_alloc
    mov r12, rax                ; r12 = instance (ob_refcnt=1, ob_type set)

    ; Zero-fill body past header (handles slot init to TAG_NULL)
    pop rcx                     ; size in bytes
    sub rcx, OBJ_HEADER_SIZE
    jle .skip_zero
    lea rdi, [r12 + OBJ_HEADER_SIZE]
    shr rcx, 3
    xor eax, eax
    rep stosq
.skip_zero:

    ; INCREF type (stored in ob_type)
    mov rdi, rbx
    call obj_incref

    ; Create inst_dict only if class doesn't have __slots__ (or has __dict__ in __slots__)
    mov rax, [rbx + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HAS_SLOTS
    jnz .in_no_dict              ; __slots__ suppresses inst_dict

    cmp qword [rbx + PyTypeObject.tp_dictoffset], 0
    je .in_no_dict              ; this family's instances carry no dict
    cmp qword [rbx + PyTypeObject.tp_dictoffset], TP_DICT_AT_TAIL
    je .in_no_dict              ; a tail dict belongs to str_sub_new, not here
    call dict_new
    STORE_INST_DICT r12, rax, rcx, .in_no_dict

.in_no_dict:
    mov rdi, r12
    call gc_track

    mov rax, r12                ; return instance
    pop r12
    pop rbx
    leave
    ret
END_FUNC instance_new

;; ============================================================================
;; instance_getattr(PyInstanceObject *self, PyObject *name) -> rax = Value
;; Look up an attribute on an instance.
;; 1. Check self->inst_dict — return raw value
;; 2. If not found, check type->tp_dict (walk tp_base chain)
;; 3. If found in type dict and callable, create bound method
;; 4. If found, INCREF and return
;; 5. If not found, return NULL
;;
;; rdi = instance, rsi = name (PyStrObject*)
;; Returns: owned reference to attribute value, or NULL
;; ============================================================================
IG_NAME   equ 8
IG_ORIGIN equ 16        ; the type the MRO walk started from
IG_FRAME  equ 32
DEF_FUNC instance_getattr, IG_FRAME
    push rbx
    push r12
    push r13
    mov qword [rel attr_error_pending], 0

    mov rbx, rdi                ; rbx = self (instance)
    mov r12, rsi                ; r12 = name
    mov [rbp - IG_NAME], rsi    ; r12 is reused as scratch further down

    ; Check self's instance dict first; a type may have none at all.
    LOAD_INST_DICT rdi, rbx, .check_type_dict
    test rdi, rdi
    jz .check_type_dict
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .found_inst

.check_type_dict:

    ; Not in inst_dict -- walk the type's MRO, checking each tp_dict.
    mov rcx, [rbx + PyObject.ob_type]   ; rcx = type (the class)
    mov [rbp - IG_ORIGIN], rcx
.walk_mro:
    mov rdi, [rcx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .try_base

    push rcx                            ; save current type
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rcx                             ; restore current type
    test edx, edx               ; the tag, not the payload: a hit may be int 0
    jnz .found_type                     ; found in type's dict

.try_base:
    MRO_NEXT rcx, [rbp - IG_ORIGIN]
    test rcx, rcx
    jnz .walk_mro

    jmp .not_found

.found_inst:
    ; Found in instance dict — INCREF and return raw value
    mov r13, rax                ; save payload
    mov r12, rdx                ; save tag (name no longer needed)
    INCREF_VAL rax, edx         ; tag-aware INCREF (skips SmallInt/NULL)
    mov rax, r13
    mov rdx, r12                ; restore tag from dict_get
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_type:
    ; Found in type dict — handle method binding.
    ; Descriptors (staticmethod, classmethod, property) are returned as-is
    ; for LOAD_ATTR to unwrap, since LOAD_ATTR knows the push convention.
    ; Member descriptors (slots) read from fixed instance offset.
    ; Regular callables are bound to the instance.
    mov r13, rax                ; r13 = attr (borrowed ref from dict_get)
    mov r12, rdx                ; r12 = attr tag (name no longer needed)
    cmp r12, TAG_PTR
    jne .found_type_raw         ; non-pointer — return as-is

    mov rcx, [rax + PyObject.ob_type]

    ; Check for member descriptor (slot) → read from instance offset
    extern member_descr_type
    lea rdx, [rel member_descr_type]
    cmp rcx, rdx
    je .found_slot

    ; Check for staticmethod/classmethod/property → return raw descriptor
    ; LOAD_ATTR handles unwrapping with the correct push convention
    lea rdx, [rel staticmethod_type]
    cmp rcx, rdx
    je .found_type_raw

    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    je .found_type_raw

    lea rdx, [rel property_type]
    cmp rcx, rdx
    je .found_type_raw

    ; Only bind func_type and builtin_func_type as methods
    ; Types, classes, and other callables are returned as-is
    lea rdx, [rel func_type]
    cmp rcx, rdx
    je .bind_method

    extern builtin_func_type
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    je .bind_method

    jmp .found_type_raw         ; not a function — return raw

.bind_method:
    ; Function found in type dict — create bound method
    mov rdi, r13                ; func
    mov rsi, rbx                ; self (instance)
    call method_new
    ; rax = bound method (method_new INCREFs func and self)
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_slot:
    ; Member descriptor found — read value from instance at fixed offset
    ; r13 = member descriptor, rbx = instance
    mov rcx, [r13 + PyMemberDescrObject.md_offset]
    mov rax, [rbx + rcx]       ; slot Value
    test rax, rax
    jz .slot_not_set            ; 0 = slot not set → AttributeError
    INCREF_V rax, rdx
    V_UNPACK rax, rdx
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_type_raw:
    ; Not callable, SmallInt, or descriptor — INCREF and return
    INCREF_VAL r13, r12         ; tag-aware INCREF
    mov rax, r13
    mov rdx, r12                ; restore tag from dict_get
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.slot_not_set:
    ; Slot exists but not initialized — raise AttributeError directly
    ; (must not return NULL or LOAD_ATTR fallback finds descriptor in tp_dict)
    RAISE exc_AttributeError_type, "slot attribute not set"

.not_found:
    ; Ordinary lookup missed.  __getattr__ is Python's hook for exactly that
    ; -- it runs only when normal resolution fails -- and it was never
    ; consulted, so a class defining it got a bare AttributeError.
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel ig_getattr_name]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .really_not_found
    IS_NONE rax, rcx
    je .really_not_found

    ; dunder_call_2(self, name, "__getattr__", TAG_PTR)
    mov rdi, rbx
    mov rsi, [rbp - IG_NAME]    ; the attribute name str
    lea rdx, [rel ig_getattr_name]
    mov ecx, TAG_PTR
    extern dunder_call_2
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jz .getattr_raised          ; the slot is present, so NULL means it raised
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.getattr_raised:
    ; An AttributeError from __getattr__ is the protocol saying "absent", and
    ; getattr(o, n, default) and hasattr() have to be able to see that and
    ; answer.  Unwinding from here skips their native frames entirely, so they
    ; never got the chance.  Hand back NULL with the exception still pending
    ; and a flag saying so; raise_no_attribute propagates it rather than
    ; replacing it, so `o.missing` still reports what __getattr__ raised.
    ;
    ; Anything else is a genuine failure in the middle of a lookup and keeps
    ; unwinding, which is what it did before.
    mov rax, [rel current_exception]
    test rax, rax
    jz .getattr_unwind
    push rax
    mov rdi, [rax + PyObject.ob_type]
    lea rsi, [rel exc_AttributeError_type]
    extern type_is_subtype
    call type_is_subtype
    pop rcx
    test eax, eax
    jz .getattr_unwind
    mov qword [rel attr_error_pending], 1
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret
.getattr_unwind:
    leave
    jmp eval_exception_unwind

.really_not_found:
    ; A builtin base may answer through its own tp_getattr rather than a
    ; tp_dict entry -- bytes.decode and str.encode live there -- and a
    ; subclass inherits those.  Only the *base's* slot: this type's own is
    ; instance_getattr, which is where we already are.
    mov rdi, [rbx + PyObject.ob_type]
    mov rsi, PyTypeObject.tp_getattr
    call base_slot
    test rax, rax
    jz .no_base_getattr
    lea rcx, [rel instance_getattr]
    cmp rax, rcx
    je .no_base_getattr
    mov rdi, rbx
    mov rsi, [rbp - IG_NAME]
    call rax
    V_UNPACK rax, rdx
    test edx, edx
    jz .no_base_getattr
    ; It hands back an *unbound* builtin -- its own instances reach it through
    ; LOAD_ATTR's method form -- so bind it here, where the caller is about to
    ; be told the answer came from a heaptype and needs no self.
    cmp edx, TAG_PTR
    jne .base_getattr_done
    mov rcx, [rax + PyObject.ob_type]
    extern builtin_func_type
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    jne .base_getattr_ptr
    mov rdi, rax
    mov rsi, rbx
    push rax
    call method_new
    mov r13, rax
    pop rdi
    call obj_decref
    mov rax, r13
.base_getattr_ptr:
    mov edx, TAG_PTR
.base_getattr_done:
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.no_base_getattr:
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC instance_getattr

;; ============================================================================
;; instance_setattr(PyInstanceObject *self, PyObject *name, PyObject *value)
;; Set an attribute on an instance's __dict__.
;; rdi = instance, rsi = name, rdx = value
;; ============================================================================
DEF_FUNC instance_setattr
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; instance
    mov r12, rsi                ; name
    mov r13, rdx                ; value Value

    ; Walk the type's MRO looking for a member descriptor (slot)
    mov rax, [rbx + PyObject.ob_type]
    mov r14, rax                ; origin of the walk
.sa_walk:
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .sa_try_base
    push rax                    ; save current type
    mov rsi, r12                ; name
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    mov r9, rax                 ; save dict_get value
    pop rax                     ; restore current type
    test edx, edx
    jnz .sa_found_type

.sa_try_base:
    MRO_NEXT rax, r14
    test rax, rax
    jnz .sa_walk
    jmp .sa_no_slot

.sa_found_type:
    ; Check if it's a member descriptor (r9 = dict value, rax = type)
    cmp edx, TAG_PTR
    jne .sa_no_slot
    extern member_descr_type
    lea rcx, [rel member_descr_type]
    cmp [r9 + PyObject.ob_type], rcx
    jne .sa_no_slot

    ; Member descriptor! Write value to slot offset
    mov rcx, [r9 + PyMemberDescrObject.md_offset]

    ; XDECREF old value at slot
    push rcx
    mov rdi, [rbx + rcx]       ; old Value
    XDECREF_V rdi, rsi
    pop rcx

    ; INCREF the new value and store it
    INCREF_V r13, r14
    mov [rbx + rcx], r13

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_no_slot:
    ; No slot found. Fall back to the instance dict.
    LOAD_INST_DICT rdi, rbx, .sa_no_dict_slot
    test rdi, rdi
    jnz .sa_have_dict

    ; inst_dict is NULL — check if __slots__ class (can't set arbitrary attrs)
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HAS_SLOTS
    jnz .sa_no_dict_error

    ; Regular class without __slots__ — create dict on the fly
    push r12
    push r13
    push r14
    call dict_new
    STORE_INST_DICT rbx, rax, rcx, .sa_no_dict_slot
    mov rdi, rax
    pop r14
    pop r13
    pop r12
    jmp .sa_dict_set

.sa_have_dict:
.sa_dict_set:
    ; dict_set(inst_dict, name Value, value Value)
    mov rsi, r12                ; name
    mov rdx, r13                ; value
    call dict_set

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_no_dict_error:
    RAISE exc_AttributeError_type, "object has no attribute"

.sa_no_dict_slot:
    ; This type's instances have no dict slot -- a str subclass, or a class
    ; with __slots__ -- so there is nowhere to put the attribute.
    RAISE exc_AttributeError_type, "object has no attribute"
END_FUNC instance_setattr

;; ============================================================================
;; type_setattr(PyTypeObject *type, PyObject *name, PyObject *value, ecx=value_tag)
;; Set an attribute on a type's tp_dict.
;; rdi = type, rsi = name, rdx = value, ecx = value_tag
;; ============================================================================
DEF_FUNC type_setattr
    push rbx
    push rcx                    ; keep the stack aligned

    ; Ensure tp_dict exists
    mov rbx, rdi
    mov rdi, [rbx + PyTypeObject.tp_dict]
    test rdi, rdi
    jnz .ts_have_dict

    ; Allocate a new dict for this type
    push rsi
    push rdx
    call dict_new
    mov [rbx + PyTypeObject.tp_dict], rax
    mov rdi, rax
    pop rdx
    pop rsi

.ts_have_dict:
    ; dict_set(dict, name Value, value Value)
    pop rcx
    call dict_set

    ; Assigning a dunder after the class exists has to take effect, the way
    ; `C.__eq__ = f` does in CPython: the slot is installed at class creation
    ; from what the body defined, and nothing re-ran this.  Only a heaptype
    ; has slots to install; a static type's are in its table.
    mov rax, [rbx + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HEAPTYPE
    jz .ts_done
    mov rdi, rbx
    extern type_install_slots
    call type_install_slots
.ts_done:

    pop rbx
    leave
    ret
END_FUNC type_setattr

;; ============================================================================
;; instance_dealloc(PyObject *self)
;; Deallocate an instance: DECREF inst_dict, DECREF ob_type, free self.
;; rdi = instance
;; ============================================================================
ID_EXC   equ 8
ID_FRAME equ 16
DEF_FUNC instance_dealloc, ID_FRAME
    push rbx

    mov rbx, rdi                ; rbx = self

    ; Check for __del__ dunder on heaptype
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HEAPTYPE
    jz .no_del

    ; Temporarily bump refcount to prevent re-entrant dealloc during __del__
    inc qword [rbx + PyObject.ob_refcnt]

    ; Call __del__(self) — dunder_call_1 handles lookup + call
    extern dunder_del
    extern dunder_call_1
    DUNDER_EXC_SAVE [rbp - ID_EXC]
    mov rdi, rbx
    lea rsi, [rel dunder_del]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    ; Ignore return value — DECREF if non-NULL
    test edx, edx
    jz .del_no_result
    DECREF_VAL rax, rdx
.del_no_result:

    ; A __del__ that raises must not leave the exception pending: the object
    ; is being freed, there is no caller to hand it to, and leaving it set
    ; means the *next* raise silently discards it -- or, if this dealloc came
    ; from the unwinder dropping the value stack, that the handler receives
    ; the wrong exception object.  CPython reports it and clears it.
    DUNDER_RAISED [rbp - ID_EXC], .del_raised
.del_cleared:

    ; Restore refcount (undo the bump)
    dec qword [rbx + PyObject.ob_refcnt]

    jmp .no_del

.del_raised:
    ; Report on stderr and clear, so nothing downstream inherits it.
    mov edi, 2
    lea rsi, [rel id_del_ignored_msg]
    mov edx, id_del_ignored_len
    call sys_write
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    test rdi, rdi
    jz .del_cleared
    call obj_decref
    jmp .del_cleared

.no_del:
    ; XDECREF the instance dict; a type may have no dict slot at all.
    LOAD_INST_DICT rdi, rbx, .no_dict
    test rdi, rdi
    jz .no_dict
    call obj_decref
.no_dict:

    ; Check if this is an int subclass — XDECREF int_value (tag-aware)
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jz .no_int_value
    mov rdi, [rbx + PyIntSubclassObject.int_value]
    V_UNPACK rdi, rsi
    DECREF_VAL rdi, rsi
.no_int_value:

    ; DECREF_VAL each __slots__ slot.  Slots start after the whole instance
    ; header, which for a container subclass is the embedded base plus the
    ; dict word -- not PyInstanceObject_size.  Assuming the latter made a
    ; list subclass treat its own `allocated` and `ob_item` fields as slot
    ; values and DECREF them.
    push r12
    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_dictoffset]
    test rcx, rcx
    jz .id_no_dict_hdr
    cmp rcx, TP_DICT_AT_TAIL
    je .id_no_dict_hdr          ; the dict is past the data, not in the header
    add rcx, 8
    jmp .id_have_hdr
.id_no_dict_hdr:
    ; No dict word: a str subclass, whose header is the base's, not
    ; PyInstanceObject's.  Using 24 there found a phantom slot at +24 --
    ; PyStrObject.ob_hash -- and XDECREF'd the hash as if it were a pointer.
    mov rcx, [rax + PyTypeObject.tp_base]
    test rcx, rcx
    jz .id_no_dict_hdr_default
    mov rcx, [rcx + PyTypeObject.tp_basicsize]
    test rcx, rcx
    jnz .id_have_hdr
.id_no_dict_hdr_default:
    mov rcx, PyInstanceObject_size
.id_have_hdr:
    mov rax, [rax + PyTypeObject.tp_basicsize]
    sub rax, rcx
    jle .no_slots                ; no slots
    shr rax, 3                  ; nslots
    mov r12, rax                ; r12 = remaining count
    add rcx, rbx                ; rcx = first slot address

.slot_decref_loop:
    push rcx
    mov rdi, [rcx]              ; slot Value
    XDECREF_V rdi, rsi
    pop rcx
    add rcx, 8                  ; next slot
    dec r12
    jnz .slot_decref_loop

.no_slots:
    pop r12

    ; Save ob_type before freeing (gc_dealloc reads ob_type, then frees)
    push qword [rbx + PyObject.ob_type]

    ; Free the instance (GC-aware) — must happen before type DECREF
    mov rdi, rbx
    call gc_dealloc

    ; DECREF ob_type (the class) AFTER freeing the instance
    pop rdi
    call obj_decref

    pop rbx
    leave
    ret
END_FUNC instance_dealloc

;; ============================================================================
;; builtin_sub_dealloc(PyObject *self)
;; Dealloc for heap-type subclasses of builtin types (bytes, bytearray, etc.)
;; These don't have inst_dict — just DECREF the type and free.
;; ============================================================================
DEF_FUNC builtin_sub_dealloc
    push rbx
    mov rbx, rdi

    ; Save ob_type before freeing (gc_dealloc reads ob_type)
    push qword [rbx + PyObject.ob_type]

    ; Free the object (may be GC-tracked) — must happen before type DECREF
    mov rdi, rbx
    call gc_dealloc

    ; DECREF ob_type (the class) AFTER freeing the object
    pop rdi
    call obj_decref

    pop rbx
    leave
    ret
END_FUNC builtin_sub_dealloc

;; ============================================================================
;; instance_repr(PyObject *self) -> PyStrObject*
;; Try __repr__ dunder, fall back to "<instance>".
;; rdi = instance
;; ============================================================================
;; ============================================================================
;; base_slot(rdi = type, rsi = slot byte offset) -> rax = the slot, or 0
;;
;; Walk past the heaptypes in the tp_base chain to the concrete builtin
;; underneath and read one of its slots.  A subclass of list embeds a list,
;; so printing it should print the list -- "<instance>" is only right for a
;; class that derives from object.
;; ============================================================================
DEF_FUNC_LOCAL base_slot
.bs_walk:
    mov rdi, [rdi + PyTypeObject.tp_base]
    test rdi, rdi
    jz .bs_none
    mov rax, [rdi + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HEAPTYPE
    jnz .bs_walk
    mov rax, [rdi + rsi]
    leave
    ret
.bs_none:
    xor eax, eax
    leave
    ret
END_FUNC base_slot

IR_EXC   equ 8
IR_FRAME equ 16
DEF_FUNC instance_repr, IR_FRAME
    push rbx
    mov rbx, rdi
    DUNDER_EXC_SAVE [rbp - IR_EXC]

    ; Try __repr__ dunder.  object.__repr__ lives in object_type's dict now,
    ; so the MRO search finds it for *every* class -- but it is the default,
    ; not an override, and taking it would print "<L object>" for a list
    ; subclass instead of the list.  Skip it and let the base slot answer.
    extern dunder_repr
    extern dunder_call_1
    extern dunder_lookup
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel dunder_repr]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .ir_no_dunder
    extern object_method_repr
    extern builtin_func_type
    cmp edx, TAG_PTR
    jne .ir_call_dunder
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel builtin_func_type]
    cmp rcx, r8
    jne .ir_call_dunder
    mov rcx, [rax + PyBuiltinObject.func_ptr]
    lea r8, [rel object_method_repr]
    cmp rcx, r8
    je .ir_no_dunder            ; the inherited default: not an override

.ir_call_dunder:
    mov rdi, rbx
    lea rsi, [rel dunder_repr]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jnz .done
    DUNDER_RAISED [rbp - IR_EXC], .failed   ; __repr__ ran and raised

.ir_no_dunder:

    ; No __repr__.  If a builtin lies under this class, use its repr: a
    ; list subclass should print as a list.
    mov rdi, [rbx + PyObject.ob_type]
    mov rsi, PyTypeObject.tp_repr
    call base_slot
    test rax, rax
    jz .ir_generic
    ; object_type.tp_repr is instance_repr itself, so a plain class would
    ; call straight back into here.
    lea rcx, [rel instance_repr]
    cmp rax, rcx
    je .ir_generic
    mov rdi, rbx
    call rax
    ; The base slot returns a str pointer.  Some of them (str_str) set only
    ; rax, and callers still read rdx as the tag -- builtin_print treats a
    ; zero tag as "skip this argument", which is why printing a str subclass
    ; produced nothing.
    mov edx, TAG_PTR
    jmp .done

.ir_generic:
    lea rdi, [rel instance_repr_cstr]
    call str_from_cstr

.done:
    pop rbx
    leave
    ret

.failed:
    RET_NULL
    pop rbx
    leave
    ret
END_FUNC instance_repr

;; ============================================================================
;; instance_str(PyObject *self) -> PyStrObject*
;; Try __str__ dunder, fall back to instance_repr.
;; rdi = instance
;; ============================================================================
IS_EXC   equ 8
IS_FRAME equ 16
DEF_FUNC instance_str, IS_FRAME
    push rbx
    mov rbx, rdi
    DUNDER_EXC_SAVE [rbp - IS_EXC]

    ; Try __str__ dunder, skipping the inherited object.__str__ default for
    ; the same reason instance_repr does: taking it would print a str
    ; subclass as its repr instead of its text.
    extern dunder_str
    extern object_method_str
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel dunder_str]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .is_no_dunder
    cmp edx, TAG_PTR
    jne .is_call_dunder
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel builtin_func_type]
    cmp rcx, r8
    jne .is_call_dunder
    mov rcx, [rax + PyBuiltinObject.func_ptr]
    lea r8, [rel object_method_str]
    cmp rcx, r8
    je .is_no_dunder

.is_call_dunder:
    mov rdi, rbx
    lea rsi, [rel dunder_str]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jnz .done
    DUNDER_RAISED [rbp - IS_EXC], .failed   ; __str__ ran and raised

.is_no_dunder:

    ; No __str__.  Prefer the underlying builtin's tp_str -- but only when it
    ; is really its own, which means different from its tp_repr.  Most builtins
    ; do what object does and let str() fall through to repr(): CPython says
    ; `int.__str__ is object.__str__`, and so does list's.  Taking the base's
    ; tp_str unconditionally meant an int subclass that defined __repr__ still
    ; printed as the number.
    push r12
    mov rdi, [rbx + PyObject.ob_type]
    mov rsi, PyTypeObject.tp_str
    call base_slot
    mov r12, rax
    mov rdi, [rbx + PyObject.ob_type]
    mov rsi, PyTypeObject.tp_repr
    call base_slot
    cmp rax, r12
    mov rax, r12
    pop r12
    je .is_generic              ; the base defers to its own repr; so do we
    test rax, rax
    jz .is_generic
    lea rcx, [rel instance_str]
    cmp rax, rcx
    je .is_generic
    mov rdi, rbx
    call rax
    ; The base slot returns a str pointer.  Some of them (str_str) set only
    ; rax, and callers still read rdx as the tag -- builtin_print treats a
    ; zero tag as "skip this argument", which is why printing a str subclass
    ; produced nothing.
    mov edx, TAG_PTR
    jmp .done

.is_generic:
    mov rdi, rbx
    call instance_repr

.done:
    pop rbx
    leave
    ret

.failed:
    RET_NULL
    pop rbx
    leave
    ret
END_FUNC instance_str

;; ============================================================================
;; type_call(PyTypeObject *type, PyObject **args, int64_t nargs) -> PyObject*
;; tp_call for user-defined class type objects.
;; Calling a class creates an instance, then calls __init__ if present.
;;
;; rdi = type (the class being called)
;; rsi = args array
;; edx = nargs
;; Returns: new instance
;; ============================================================================
; Local frame offsets for .normal_type_call (rbp-relative, after 5 pushes + sub rsp, 24)
TC_NEW_FUNC equ 48              ; [rbp - 48]: saved __new__ func pointer
TC_NEW_TAG  equ 56              ; [rbp - 56]: saved __new__ result tag
; The keywords this call was made with.  __new__ consumes kw_names_pending, so
; __init__ has to be handed it again -- otherwise `M(name, bases, ns, **kwds)`
; reaches __init__ with the keyword values as extra positional arguments, and
; that is how a metaclass with class keywords fails.
TC_KWNAMES  equ 64

;; ============================================================================
;; tc_winner_metatype(rdi = args) -> rax = the metatype to delegate to, or 0
;;
;; args[1] is the bases tuple.  A base whose metatype is `type` -- or the
;; user_type_metatype that stands in for it here -- contributes nothing; of the
;; rest the most derived wins, which is CPython's rule minus the conflict
;; diagnosis.  0 means an ordinary type() call.
;; ============================================================================
TWM_ARGS  equ 8
TWM_WIN   equ 16
TWM_I     equ 24
TWM_N     equ 32
TWM_ITEM  equ 40
TWM_FRAME equ 40          ; + 1 push = 64
DEF_FUNC_LOCAL tc_winner_metatype, TWM_FRAME
    push rbx
    mov [rbp - TWM_ARGS], rdi
    mov qword [rbp - TWM_WIN], 0

    mov rbx, [rdi + 8]                  ; the bases tuple
    V_TEST_PTR rbx, rax
    ja .twm_done
    test rbx, rbx
    jz .twm_done
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    jne .twm_done

    mov rax, [rbx + PyTupleObject.ob_size]
    mov [rbp - TWM_N], rax
    mov qword [rbp - TWM_I], 0
.twm_scan:
    mov rax, [rbp - TWM_I]
    cmp rax, [rbp - TWM_N]
    jae .twm_done
    mov rcx, [rbx + PyTupleObject.ob_item]
    mov rcx, [rcx + rax*8]
    V_TEST_PTR rcx, rdx
    ja .twm_next
    test rcx, rcx
    jz .twm_next
    mov rcx, [rcx + PyObject.ob_type]
    lea rdx, [rel type_type]
    cmp rcx, rdx
    je .twm_next
    lea rdx, [rel user_type_metatype]
    cmp rcx, rdx
    je .twm_next
    mov [rbp - TWM_ITEM], rcx
    mov rdx, [rbp - TWM_WIN]
    test rdx, rdx
    jz .twm_take
    cmp rdx, rcx
    je .twm_next
    ; Keep whichever is the subclass of the other; a genuine conflict just
    ; leaves the one already found.
    mov rdi, rcx
    mov rsi, rdx
    extern type_is_subtype
    call type_is_subtype
    mov rcx, [rbp - TWM_ITEM]
    test eax, eax
    jz .twm_next
.twm_take:
    mov [rbp - TWM_WIN], rcx
.twm_next:
    inc qword [rbp - TWM_I]
    jmp .twm_scan
.twm_done:
    mov rax, [rbp - TWM_WIN]
    pop rbx
    leave
    ret
END_FUNC tc_winner_metatype

DEF_FUNC type_call
    ; Special case: type(x) with 1 arg when calling type itself
    ; Returns x.__class__ (the type of x)
    lea rax, [rel type_type]
    cmp rdi, rax
    jne .not_type_self
    cmp edx, 3
    jge .type_three_arg         ; the extra arguments are class keywords
    cmp edx, 1
    jne .not_type_self
    ; type(x) → return type of x
    mov rax, [rsi]          ; args[0] payload
    V_TEST_INT_M [rsi], r11      ; args[0] an int immediate?
    jae .type_smallint
    V_TEST_F64_M [rsi], r11      ; args[0] a float?
    jbe .type_float
    mov rax, [rax + PyObject.ob_type]
    ; The heaptype metatype is an implementation split, not a language type:
    ; CPython has one `type`, and `type(C) is type` for an ordinary class.
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    jne .type_of_have_type
    lea rax, [rel type_type]
.type_of_have_type:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
.type_smallint:
    lea rax, [rel int_type]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
.type_float:
    extern float_type
    lea rax, [rel float_type]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
.type_three_arg:
    ; The class a three-argument type() builds belongs to the most derived of
    ; its bases' metatypes, not to `type`.  Going straight to type_from_parts
    ; ignored that and the class keywords with it, so
    ; `type(name, (SomeEnum,), ns, boundary=KEEP)` -- which is how enum's
    ; _simple_enum decorator builds a class -- never ran EnumType at all.
    push rsi
    push rdx
    mov rdi, rsi
    call tc_winner_metatype
    pop rdx
    pop rsi
    test rax, rax
    jz .tta_plain
    mov rdi, rax
    call type_call              ; the metatype's own tp_call, keywords and all
    leave
    ret

.tta_plain:
    ; type(name, bases, namespace) builds a class, exactly as a class
    ; statement does.  Falling through to .normal_type_call instead treated
    ; type_type as an ordinary class: it allocated a PyInstanceObject-sized
    ; block and wrote type fields into it, so the result printed as
    ; <class ''> and the process aborted with a double free.
    push rbx
    mov rbx, rsi                        ; args

    mov rdi, [rbx]                      ; name
    V_TEST_PTR rdi, rax
    ja .type_three_bad_name
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .type_three_bad_name

    ; bases: the tuple goes through as it is; an empty one means object.
    mov rsi, [rbx + 8]
    V_TEST_PTR rsi, rax
    ja .type_three_bad_bases
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    jne .type_three_bad_bases
    mov rcx, [rsi + PyTupleObject.ob_size]
    test rcx, rcx
    jnz .type_three_have_base
    xor esi, esi
.type_three_have_base:

    mov rdx, [rbx + 16]                 ; namespace dict
    V_TEST_PTR rdx, rax
    ja .type_three_bad_ns
    mov rax, [rdx + PyObject.ob_type]
    lea rcx, [rel dict_type]
    cmp rax, rcx
    jne .type_three_bad_ns

    ; type_from_parts takes ownership of a reference to the namespace, which
    ; becomes tp_dict, and keeps the name alive through tp_name.
    inc qword [rdx + PyObject.ob_refcnt]
    mov rdi, [rbx]
    inc qword [rdi + PyObject.ob_refcnt]
    extern type_from_parts
    call type_from_parts
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.type_three_bad_name:
    RAISE exc_TypeError_type, "type() argument 1 must be str"
.type_three_bad_bases:
    RAISE exc_TypeError_type, "type() argument 2 must be a tuple of at most one base"
.type_three_bad_ns:
    RAISE exc_TypeError_type, "type() argument 3 must be dict"

.type_bool:
    extern bool_type
    lea rax, [rel bool_type]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
.type_none:
    extern none_type
    lea rax, [rel none_type]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret

.not_type_self:
    ; Check for a built-in constructor (int(), str(), staticmethod(), ...).
    ; It lives in tp_new, NOT tp_call: tp_call on a type is what makes that
    ; type's INSTANCES callable, so parking a constructor there made every
    ; string, list and heap int callable ("abc"() returned '').
    mov rax, [rdi + PyTypeObject.tp_new]
    test rax, rax
    jz .normal_type_call
    ; Avoid infinite recursion if a constructor is ever type_call itself
    lea rcx, [rel type_call]
    cmp rax, rcx
    je .normal_type_call
    ; Call the constructor: ctor(type, args, nargs).  It still returns a fat
    ; pair, so tp_call has to pack it rather than tail-jump.
    leave
    sub rsp, 8                  ; keep the callee's rsp 16-byte aligned
    call rax
    add rsp, 8
    V_PACK rax, rdx
    ret

.normal_type_call:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40                 ; locals + align (5 pushes + rbp = 48, +40 = 88)
    mov rax, [rel kw_names_pending]
    mov [rbp - TC_KWNAMES], rax
    mov qword [rbp - TC_NEW_TAG], TAG_PTR  ; default return tag

    mov rbx, rdi                ; rbx = type
    mov r12, rsi                ; r12 = args
    mov r13d, edx               ; r13d = nargs
    movsxd r13, r13d            ; sign-extend to 64 bits

    ; An abstract class refuses to be instantiated.  __abstractmethods__ is
    ; the set ABCMeta leaves on the class; a non-empty one is the whole test,
    ; and it lives in the class's own dict, never inherited -- a concrete
    ; subclass gets an empty set of its own.
    mov rax, [rbx + PyTypeObject.tp_dict]
    test rax, rax
    jz .tc_not_abstract
    push rax
    lea rdi, [rel tc_abstract_name]
    call str_from_cstr_heap
    mov rcx, rax
    pop rdi
    push rcx
    mov rsi, rcx
    call dict_get
    pop rdi
    push rax
    call obj_decref
    pop rax
    test rax, rax
    jz .tc_not_abstract
    V_TEST_PTR rax, rcx
    ja .tc_not_abstract
    cmp qword [rax + PyDictObject.ob_size], 0
    je .tc_not_abstract
    RAISE exc_TypeError_type, "Can't instantiate abstract class with abstract methods"
.tc_not_abstract:

    ; Check if this type inherits from an exception type
    extern type_is_exc_subclass
    mov rdi, rbx
    call type_is_exc_subclass
    test eax, eax
    jnz .exc_subclass_call

    ; An int subclass used to short-circuit here, before anything looked for a
    ; __new__ of its own -- so a class that defines one never had it called,
    ; and its extra arguments reached int()'s two-argument form instead:
    ; `NIC(7, "SEVEN")` came back as "int() second arg must be an integer".
    ; The shortcut now waits at .new_not_found, beside the str one.

    ; === Look up __new__ in MRO (stop at object_type) ===
    lea rdi, [rel new_name_cstr]
    call str_from_cstr_heap
    mov r15, rax                ; r15 = "__new__" str

    mov rcx, rbx                ; rcx = current type
.new_mro_walk:
    ; Stop at object_type (default __new__ = instance_new)
    lea rdi, [rel object_type]
    cmp rcx, rdi
    je .new_not_found

    mov rdi, [rcx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .new_try_base

    push rcx
    mov rsi, r15
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rcx
    test edx, edx               ; the tag, not the payload: a hit may be int 0
    jnz .new_found

.new_try_base:
    MRO_NEXT rcx, rbx
    test rcx, rcx
    jnz .new_mro_walk

.new_not_found:
    ; DECREF name string
    mov rdi, r15
    call obj_decref
    ; An int subclass carries its value inline, so it cannot come from
    ; instance_new either.
    mov rax, [rbx + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jz .nnf_check_str
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call int_sub_new
    test edx, edx
    jz .int_sub_error
    mov r14, rax
    jmp .lookup_init
.nnf_check_str:
    ; A str subclass is variable-size, so it cannot come from instance_new.
    mov rax, [rbx + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_STR_SUBCLASS
    jz .nnf_check_base_new
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call str_sub_new
    mov r14, rax
    jmp .lookup_init

.nnf_check_base_new:
    ; A builtin base with a constructor of its own -- bytes, bytearray,
    ; memoryview -- builds the instance, and __init__ still runs on it
    ; afterwards.  Inheriting the base's tp_new outright skipped __init__.
    mov rdi, rbx
    mov rsi, PyTypeObject.tp_new
    call base_slot
    test rax, rax
    jz .tc_plain_new
    lea rcx, [rel type_call]
    cmp rax, rcx
    je .tc_plain_new
    lea rcx, [rel object_type_call]
    cmp rax, rcx
    je .tc_plain_new
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call rax
    V_UNPACK rax, rdx
    test edx, edx
    jz .int_sub_error
    mov r14, rax
    jmp .lookup_init

.tc_plain_new:
    ; Default: instance_new(type)
    mov rdi, rbx
    call instance_new
    mov r14, rax                ; r14 = instance

    ; A subclass of a builtin container embeds that container's layout, so
    ; the embedded part needs the empty state its own constructor would have
    ; given it before __init__ fills it.
    mov rax, [rbx + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_LIST_SUBCLASS | TYPE_FLAG_TUPLE_SUBCLASS | \
              TYPE_FLAG_DICT_SUBCLASS | TYPE_FLAG_SET_SUBCLASS
    jz .lookup_init
    mov rdi, r14
    call builtin_sub_init_base

    ; tuple has no __init__ to fill it later, so do it now.
    mov rax, [rbx + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_TUPLE_SUBCLASS
    jz .lookup_init
    mov rdi, r14
    mov rsi, r12
    mov rdx, r13
    call tuple_sub_fill
    jmp .lookup_init

.new_found:
    ; rax = __new__ func ptr, edx = tag
    ; __new__ is conventionally a staticmethod -- it is registered that way
    ; for the container types, and a user class writing @staticmethod gets
    ; the same wrapper -- so unwrap before calling.  The wrapper itself has
    ; no tp_call.
    cmp edx, TAG_PTR
    jne .tc_new_unwrapped
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel staticmethod_type]
    cmp rcx, rdx
    jne .tc_new_unwrapped
    mov rax, [rax + PyStaticMethodObject.sm_callable]
.tc_new_unwrapped:
    mov [rbp - TC_NEW_FUNC], rax
    ; DECREF name string
    mov rdi, r15
    call obj_decref

    ; Build args for __new__(cls, *original_args)
    lea rax, [r13 + 1]
    shl rax, 4                  ; (nargs+1) * 16
    sub rsp, rax
    mov r15, rsp                ; r15 = new args array

    ; args[0] = cls (a type pointer is its own Value)
    mov [r15], rbx

    ; Copy original args
    xor ecx, ecx
.copy_new_args:
    cmp rcx, r13
    jge .new_args_copied
    mov rax, rcx
    shl rax, 3                  ; one Value per slot
    mov rdx, [r12 + rax]
    lea r9, [rcx + 1]
    shl r9, 3                   ; dest slot (offset by one for self)
    mov [r15 + r9], rdx
    inc rcx
    jmp .copy_new_args
.new_args_copied:

    ; Call __new__'s tp_call
    mov rdi, [rbp - TC_NEW_FUNC]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .new_not_callable

    mov rsi, r15                ; args
    lea rdx, [r13 + 1]          ; nargs + 1
    call rax
    V_UNPACK rax, rdx           ; tp_call returns a Value

    mov r14, rax                ; r14 = instance from __new__
    mov [rbp - TC_NEW_TAG], rdx ; save result tag

    ; Restore stack from args allocation
    lea rax, [r13 + 1]
    shl rax, 4
    add rsp, rax

    ; Check: only call __init__ if __new__ returned instance of cls
    cmp qword [rbp - TC_NEW_TAG], TAG_PTR
    jne .no_init
    mov rax, [r14 + PyObject.ob_type]
    cmp rax, rbx
    jne .no_init

.lookup_init:
    ; Look up __init__ walking the MRO (type + tp_base chain)
    ; Create "__init__" string for lookup (heap — dict key, DECREFed)
    lea rdi, [rel init_name_cstr]
    call str_from_cstr_heap
    mov r15, rax                ; r15 = "__init__" str object

    ; Walk MRO: check type->tp_dict, then tp_base chain
    mov rcx, rbx                ; rcx = current type to check
.init_mro_walk:
    mov rdi, [rcx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .init_try_base

    push rcx                    ; save current type
    mov rsi, r15
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rcx                     ; restore current type
    test edx, edx               ; the tag, not the payload: a hit may be int 0
    jnz .init_found

.init_try_base:
    MRO_NEXT rcx, rbx
    test rcx, rcx
    jnz .init_mro_walk

    ; __init__ not found anywhere — DECREF name string, skip
    mov rdi, r15
    call obj_decref
    jmp .no_init

.init_found:
    mov rbx, rax                ; rbx = __init__ func

    ; DECREF the "__init__" string (no longer needed)
    mov rdi, r15
    call obj_decref

    ; === Call __init__(instance, *args) ===
    ; Build args array on machine stack: [instance, arg0, arg1, ...]
    ; Total args = nargs + 1 (for instance)
    ; Allocate (nargs+1)*16 bytes on the stack (fat values)
    lea rax, [r13 + 1]
    shl rax, 4                  ; (nargs+1) * 16
    sub rsp, rax                ; allocate on stack
    mov r15, rsp                ; r15 = new args array

    ; args[0] = instance (a pointer is its own Value)
    mov [r15], r14

    ; Copy original args: args[1..nargs] (16-byte stride)
    xor ecx, ecx
.copy_args:
    cmp rcx, r13
    jge .args_copied
    mov rax, rcx
    shl rax, 3                  ; one Value per slot
    mov rdx, [r12 + rax]
    lea r9, [rcx + 1]
    shl r9, 3                   ; dest slot (offset by one for self)
    mov [r15 + r9], rdx
    inc rcx
    jmp .copy_args
.args_copied:

    ; Get __init__'s tp_call
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .init_not_callable
    mov r11, rax                        ; tp_call, kept across the check below

    ; Call tp_call(__init_func, args_with_instance, nargs+1), with the same
    ; keywords the class was called with: __new__ has consumed them by now.
    ;
    ; The keywords the class was called with: __new__ has consumed
    ; kw_names_pending by now, and without handing them back a metaclass's
    ; __init__ sees the keyword VALUES as extra positional arguments.
    ;
    ; Only a Python-level __init__ gets them, and a builtin one is left exactly
    ; as it was -- not even cleared.  `L(sequence=())` reaches list's
    ; constructor as its __init__ and is rejected by it, which is where that
    ; TypeError comes from.
    mov rax, [rbx + PyObject.ob_type]
    lea rdx, [rel func_type]
    cmp rax, rdx
    jne .init_no_kw
    mov rcx, [rbp - TC_KWNAMES]
    mov [rel kw_names_pending], rcx     ; func_call clears it again
.init_no_kw:
    mov rdi, rbx                ; callable = __init__ func
    mov rsi, r15                ; args ptr
    lea rdx, [r13 + 1]          ; nargs + 1
    call r11
    V_UNPACK rax, rdx           ; tp_call returns a Value

    ; DECREF __init__'s return value (should be None — TAG_NONE, not a pointer)
    mov rsi, rdx
    DECREF_VAL rax, rsi

    ; Restore stack (undo the sub rsp from args allocation)
    lea rax, [r13 + 1]
    shl rax, 4
    add rsp, rax

.no_init:
    ; Return the instance (tag from TC_NEW_TAG; default TAG_PTR, or __new__ result tag)
    mov rax, r14
    mov rdx, [rbp - TC_NEW_TAG]

    add rsp, 40                 ; undo the locals; must match the sub above
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret

.exc_subclass_call:
    ; User-defined exception subclass — create PyExceptionObject via exc_type_call
    ; rbx = type, r12 = args, r13 = nargs
    extern exc_type_call
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call exc_type_call
    ; rax = exception object (PyExceptionObject)
    mov r14, rax                ; r14 = instance

    ; Check if type has __init__ in its dict (for custom exception __init__)
    mov rdi, [rbx + PyTypeObject.tp_init]
    test rdi, rdi
    jz .exc_sub_no_init

    ; Build args: (instance, *original_args) using 16-byte fat value stride
    lea rax, [r13 + 1]
    shl rax, 4                  ; (nargs+1) * 16
    sub rsp, rax
    mov r15, rsp                ; r15 = new args array
    mov [r15], r14
    ; Copy original args
    xor ecx, ecx
.exc_sub_copy_args:
    cmp rcx, r13
    jge .exc_sub_args_copied
    mov rax, rcx
    shl rax, 3                  ; one Value per slot
    mov rdx, [r12 + rax]
    lea r9, [rcx + 1]
    shl r9, 3                   ; dest slot (offset by one for self)
    mov [r15 + r9], rdx
    inc rcx
    jmp .exc_sub_copy_args
.exc_sub_args_copied:
    ; Get __init__'s tp_call
    mov rdi, [rbx + PyTypeObject.tp_init]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .exc_sub_init_cleanup
    mov rdi, [rbx + PyTypeObject.tp_init]
    mov rsi, r15
    lea rdx, [r13 + 1]
    call rax
    V_UNPACK rax, rdx           ; tp_call returns a Value
    ; DECREF return value (should be None)
    mov rsi, rdx
    DECREF_VAL rax, rsi
.exc_sub_init_cleanup:
    lea rax, [r13 + 1]
    shl rax, 4
    add rsp, rax

.exc_sub_no_init:
    mov rax, r14
    mov edx, TAG_PTR
    add rsp, 40                 ; undo the locals; must match the sub above
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret

.int_subclass_call:
    ; rbx = type, r12 = args, r13 = nargs
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call int_sub_new
    test edx, edx
    jz .int_sub_error
.int_sub_epilogue:
    add rsp, 40                 ; undo the locals; must match the sub above
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
.int_sub_error:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret

.init_not_callable:
    RAISE exc_TypeError_type, "__init__ is not callable"
    ; does not return

.new_not_callable:
    ; Restore stack from args allocation, then error
    lea rax, [r13 + 1]
    shl rax, 4
    add rsp, rax
    RAISE exc_TypeError_type, "__new__ is not callable"
    ; does not return
END_FUNC type_call

;; ============================================================================
;; type_getattr(PyTypeObject *self, PyObject *name) -> PyObject*
;; Look up an attribute on a type object itself (class variables).
;; Also handles __name__ (from tp_name) and __bases__.
;; rdi = type object, rsi = name (PyStrObject*)
;; Returns: owned reference to attribute value, or NULL
;; ============================================================================
extern tuple_new
TGA_ORIGIN equ 8            ; the type the MRO walk started from
TGA_META   equ 16           ; its metatype, for the second walk
TGA_FRAME  equ 32
DEF_FUNC type_getattr, TGA_FRAME
    push rbx
    push r12

    mov rbx, rsi                ; rbx = name
    mov r12, rdi                ; r12 = type (walks)
    mov [rbp - TGA_ORIGIN], rdi

    ; Check for __name__: compare name string data with "__name__"
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [rel tga_name_str]
    call ap_strcmp
    test eax, eax
    jz .tga_return_name

    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__dict__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_dict

    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__mro__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_mro

    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__bases__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_bases

    ; Check type->tp_dict, then walk tp_base chain
.tga_walk:
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tga_next_base

    mov rsi, rbx
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .tga_found

.tga_next_base:
    MRO_NEXT r12, [rbp - TGA_ORIGIN]
    test r12, r12
    jnz .tga_walk
    jmp .tga_not_found

.tga_return_dict:
    ; A class dict is exposed read-only, as CPython does: types.py takes
    ; MappingProxyType straight out of `type(type.__dict__)`, so the wrapper
    ; has to exist and be its own type.  A static type may have no tp_dict
    ; at all; give it an empty one rather than reporting no __dict__.
    mov r12, [rbp - TGA_ORIGIN]
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jnz .tga_have_tp_dict
    extern dict_new
    call dict_new
    mov [r12 + PyTypeObject.tp_dict], rax
    mov rdi, rax
.tga_have_tp_dict:
    extern mappingproxy_new
    call mappingproxy_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.tga_return_mro:
    mov rax, [rbp - TGA_ORIGIN]
    mov rax, [rax + PyTypeObject.tp_mro]
    test rax, rax
    jz .tga_synth_mro
    jmp .tga_return_tuple
.tga_return_bases:
    mov rax, [rbp - TGA_ORIGIN]
    mov rax, [rax + PyTypeObject.tp_bases]
    test rax, rax
    jnz .tga_return_tuple
    ; A static type keeps no tuple; build one from tp_base.
    mov rcx, [rbp - TGA_ORIGIN]
    mov rcx, [rcx + PyTypeObject.tp_base]
    test rcx, rcx
    jz .tga_empty_tuple
    push rcx
    mov edi, 1
    call tuple_new
    pop rcx
    mov rdx, [rax + PyTupleObject.ob_item]
    mov [rdx], rcx
    push rax
    mov rdi, rcx
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.tga_synth_mro:
    ; Likewise for __mro__: walk the base chain into a fresh tuple.
    mov rdi, [rbp - TGA_ORIGIN]
    extern type_mro_len
    call type_mro_len
    push rax
    mov rdi, rax
    call tuple_new
    pop rcx
    mov r12, rax
    mov rdi, [rbp - TGA_ORIGIN]
    mov rsi, [rax + PyTupleObject.ob_item]
    extern type_mro_fill
    call type_mro_fill
    ; The tuple owns a reference to each entry.
    mov rcx, [r12 + PyTupleObject.ob_item]
    xor edx, edx
.tga_mro_incref:
    cmp rdx, rax
    jge .tga_mro_done
    push rax
    push rdx
    push rcx
    mov rdi, [rcx + rdx*8]
    call obj_incref
    pop rcx
    pop rdx
    pop rax
    inc rdx
    jmp .tga_mro_incref
.tga_mro_done:
    mov rax, r12
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.tga_empty_tuple:
    xor edi, edi
    call tuple_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.tga_return_tuple:
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.tga_found:
    ; Found — INCREF and return
    mov rbx, rax                ; save payload (name no longer needed)
    mov r12, rdx                ; save tag (type walk done)
    INCREF_VAL rax, edx         ; tag-aware INCREF (skips SmallInt/NULL)
    mov rax, rbx
    mov rdx, r12                ; restore tag from dict_get

    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.tga_return_name:
    ; __name__ is the last dotted component of tp_name: CPython stores
    ; "types.GenericAlias" but reports "GenericAlias", keeping the qualified
    ; form for the repr.
    mov rdi, [r12 + PyTypeObject.tp_name]
    mov rsi, rdi
    xor ecx, ecx
.tga_name_scan:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .tga_name_done
    cmp al, '.'
    jne .tga_name_next
    lea rdi, [rsi + rcx + 1]
.tga_name_next:
    inc rcx
    jmp .tga_name_scan
.tga_name_done:
    call str_from_cstr
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.tga_not_found:
    ; Then the metatype's own MRO.  A metaclass's methods are attributes of
    ; the classes it makes, bound to the class the way an ordinary class's
    ; methods bind to its instances -- `ByteString.register` is ABCMeta's,
    ; two links up the metatype chain.  Only a user metaclass is walked: the
    ; three builtin metatypes hold entries meant for `type` itself, and
    ; offering those on every class would shadow what a class inherits from
    ; object.
    mov r12, [rbp - TGA_ORIGIN]
    mov r12, [r12 + PyObject.ob_type]
    test r12, r12
    jz .tga_really_not_found
    lea rax, [rel type_type]
    cmp r12, rax
    je .tga_really_not_found
    lea rax, [rel user_type_metatype]
    cmp r12, rax
    je .tga_really_not_found
    extern exc_metatype
    lea rax, [rel exc_metatype]
    cmp r12, rax
    je .tga_really_not_found
    mov [rbp - TGA_META], r12

.tga_meta_walk:
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tga_meta_next
    mov rsi, rbx
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .tga_meta_found
.tga_meta_next:
    MRO_NEXT r12, [rbp - TGA_META]
    test r12, r12
    jnz .tga_meta_walk
    jmp .tga_really_not_found

.tga_meta_found:
    cmp edx, TAG_PTR
    jne .tga_meta_plain
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel func_type]
    cmp rcx, rdx
    jne .tga_meta_plain
    mov rdi, rax
    mov rsi, [rbp - TGA_ORIGIN]
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.tga_meta_plain:
    mov r12, rdx
    INCREF_VAL rax, rdx
    mov rdx, r12
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tga_really_not_found:
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC type_getattr

;; ============================================================================
;; method_new(func, self) -> PyMethodObject*
;; Create a bound method wrapping func+self.
;; rdi = func (callable), rsi = self (instance)
;; ============================================================================
DEF_FUNC method_new
    push rbx
    push r12

    mov rbx, rdi                ; func
    mov r12, rsi                ; self

    mov edi, PyMethodObject_size
    lea rsi, [rel method_type]
    call gc_alloc
    ; ob_refcnt=1, ob_type set by gc_alloc
    mov [rax + PyMethodObject.im_func], rbx
    mov [rax + PyMethodObject.im_self], r12

    ; INCREF func and self.  im_self is a Value, not necessarily a pointer:
    ; binding a builtin method to an immediate int is what `getattr(5,
    ; "bit_length")` asks for, and an unguarded incref would treat the encoded
    ; number as an address.
    push rax
    mov rdi, rbx
    call obj_incref
    INCREF_V r12, rax

    ; Track in GC
    mov rdi, [rsp]
    call gc_track
    pop rax

    pop r12
    pop rbx
    leave
    ret
END_FUNC method_new

;; ============================================================================
;; method_call(self_method, args, nargs) -> rax = Value
;; Call a bound method: prepend im_self to args, dispatch to im_func's tp_call.
;; rdi = PyMethodObject*, rsi = args, rdx = nargs
;; ============================================================================
DEF_FUNC_LOCAL method_call
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; method obj
    mov r12, rsi                ; original args
    mov r13, rdx                ; original nargs

    ; Allocate new args array: (nargs+1) * 16 (fat values)
    lea rdi, [rdx + 1]
    shl rdi, 4
    call ap_malloc
    mov r14, rax                ; new args array

    ; new_args[0] = im_self (a pointer is its own Value)
    mov rcx, [rbx + PyMethodObject.im_self]
    mov [r14], rcx

    ; Copy original args to new_args[1..] (16-byte stride)
    xor ecx, ecx
.mc_copy:
    cmp rcx, r13
    jge .mc_copy_done
    mov rax, rcx
    shl rax, 3                  ; one Value per slot
    mov rdx, [r12 + rax]
    lea r9, [rcx + 1]
    shl r9, 3                   ; dest slot (offset by one for self)
    mov [r14 + r9], rdx
    inc rcx
    jmp .mc_copy
.mc_copy_done:

    ; Call im_func's tp_call(im_func, new_args, nargs+1)
    mov rdi, [rbx + PyMethodObject.im_func]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    mov rsi, r14
    lea rdx, [r13 + 1]
    call rax
    V_UNPACK rax, rdx           ; tp_call returns a Value
    push rax                    ; save result payload
    push rdx                    ; save result tag

    ; Free temp args array
    mov rdi, r14
    call ap_free

    pop rdx                     ; restore result tag
    pop rax                     ; restore result payload
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
END_FUNC method_call

;; ============================================================================
;; method_dealloc(PyObject *self)
;; Free a bound method, DECREF func and self.
;; ============================================================================
DEF_FUNC_LOCAL method_dealloc
    push rbx

    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_func]
    call obj_decref
    mov rdi, [rbx + PyMethodObject.im_self]
    XDECREF_V rdi, rsi
    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC method_dealloc

;; ============================================================================
;; method_getattr(PyMethodObject *self, PyObject *name) -> PyObject* or NULL
;; Delegate attribute lookup to the underlying im_func.
;; rdi = bound method, rsi = name
;; ============================================================================
DEF_FUNC method_getattr
    ; Delegate to the underlying function's getattr
    mov rdi, [rdi + PyMethodObject.im_func]
    extern func_getattr
    call func_getattr           ; already returns a Value
    leave
    ret
END_FUNC method_getattr


;; ============================================================================
;; method_repr(PyMethodObject *self) -> str
;; "<bound method Qual of <self repr>>".  Bound methods had no tp_repr at all,
;; so printing one produced nothing printable.
;; ============================================================================
MR_SELF  equ 8
MR_LEN   equ 16
MR_BUF   equ 1048
MR_FRAME equ 1056
DEF_FUNC method_repr, MR_FRAME
    push rbx
    push r12
    mov [rbp - MR_SELF], rdi
    lea rbx, [rbp - MR_BUF]
    xor r12d, r12d

    CSTRING rsi, "<bound method "
.mr_pre:
    movzx eax, byte [rsi]
    test al, al
    jz .mr_qual
    inc rsi
    mov [rbx + r12], al
    inc r12
    jmp .mr_pre

.mr_qual:
    ; the function's __qualname__, or its __name__ if it has none
    mov rax, [rbp - MR_SELF]
    mov rax, [rax + PyMethodObject.im_func]
    test rax, rax
    jz .mr_of
    ; A qualified name is what CPython shows; the code object carries one,
    ; and a builtin has only its own name field.
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel func_type]
    cmp rcx, rdx
    jne .mr_builtin_name
    mov rdi, [rax + PyFuncObject.func_code]
    test rdi, rdi
    jz .mr_of
    mov rdi, [rdi + PyCodeObject.co_qualname]
    test rdi, rdi
    jnz .mr_copy_name
    mov rax, [rbp - MR_SELF]
    mov rax, [rax + PyMethodObject.im_func]
    mov rdi, [rax + PyFuncObject.func_name]
    test rdi, rdi
    jz .mr_of
    jmp .mr_copy_name
.mr_builtin_name:
    mov rdi, [rax + PyBuiltinObject.func_name]
    test rdi, rdi
    jz .mr_of
.mr_copy_name:
    mov rcx, [rdi + PyStrObject.ob_size]
    lea rsi, [rdi + PyStrObject.data]
    xor edx, edx
.mr_name_loop:
    cmp rdx, rcx
    jge .mr_of
    cmp r12, MR_BUF - 64
    jae .mr_of
    movzx eax, byte [rsi + rdx]
    mov [rbx + r12], al
    inc r12
    inc rdx
    jmp .mr_name_loop

.mr_of:
    CSTRING rsi, " of "
.mr_of_loop:
    movzx eax, byte [rsi]
    test al, al
    jz .mr_self_repr
    inc rsi
    mov [rbx + r12], al
    inc r12
    jmp .mr_of_loop

.mr_self_repr:
    mov rax, [rbp - MR_SELF]
    mov rdi, [rax + PyMethodObject.im_self]
    test rdi, rdi
    jz .mr_close
    ; obj_repr takes a Value, which is what im_self holds.
    mov [rbp - MR_LEN], r12
    extern obj_repr
    call obj_repr
    V_UNPACK rax, rdx
    test rax, rax
    jz .mr_close
    mov r12, [rbp - MR_LEN]
    mov rcx, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor edx, edx
.mr_self_loop:
    cmp rdx, rcx
    jge .mr_self_done
    cmp r12, MR_BUF - 8
    jae .mr_self_done
    push rax
    movzx eax, byte [rsi + rdx]
    mov [rbx + r12], al
    pop rax
    inc r12
    inc rdx
    jmp .mr_self_loop
.mr_self_done:
    mov rdi, rax
    call obj_decref

.mr_close:
    mov byte [rbx + r12], '>'
    inc r12
    mov rdi, rbx
    mov rsi, r12
    extern str_new_heap
    call str_new_heap
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC method_repr

;; ============================================================================
;; object_type_call(args, nargs) -> PyObject*
;; object() returns a bare instance of object_type
;; ============================================================================
DEF_FUNC_BARE object_type_call
    ; Create a bare instance with object_type (gc_alloc since HAVE_GC)
    push rbp
    mov rbp, rsp
    mov edi, PyInstanceObject_size
    lea rsi, [rel object_type]
    call gc_alloc
    mov qword [rax + PyInstanceObject.inst_dict], 0
    ; Track in GC
    push rax
    mov rdi, rax
    call gc_track
    pop rax
    mov edx, TAG_PTR
    pop rbp
    ret
END_FUNC object_type_call

;; ============================================================================
;; object_new_fn(args, nargs) -> instance
;; Implements object.__new__(cls) — creates a bare instance of cls.
;; args[0] = cls (the type to instantiate)
;; ============================================================================
DEF_FUNC object_new_fn
    ; args[0] = cls
    mov rdi, [rdi]              ; cls payload (PyTypeObject*)
    call instance_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC object_new_fn

;; ============================================================================
;; user_type_dealloc(PyTypeObject *type)
;; Deallocator for user-defined heap types (created by __build_class__).
;; Frees tp_dict, tp_name string, and the type object itself.
;; ============================================================================
DEF_FUNC user_type_dealloc
    push rbx
    mov rbx, rdi                ; rbx = type object

    ; DECREF tp_dict if present
    mov rdi, [rbx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .utd_no_dict
    call obj_decref
.utd_no_dict:

    ; DECREF tp_name string (recover from data pointer - PyStrObject.data = 32)
    mov rdi, [rbx + PyTypeObject.tp_name]
    test rdi, rdi
    jz .utd_no_name
    sub rdi, PyStrObject.data   ; point back to PyStrObject base
    call obj_decref
.utd_no_name:

    ; DECREF tp_base if present
    mov rdi, [rbx + PyTypeObject.tp_base]
    test rdi, rdi
    jz .utd_no_base
    call obj_decref
.utd_no_base:

    ; DECREF tp_bases tuple if present
    mov rdi, [rbx + PyTypeObject.tp_bases]
    test rdi, rdi
    jz .utd_no_bases
    call obj_decref
.utd_no_bases:

    ; DECREF tp_mro tuple if present
    mov rdi, [rbx + PyTypeObject.tp_mro]
    test rdi, rdi
    jz .utd_no_mro
    call obj_decref
.utd_no_mro:

    ; Free the type object itself (gc_alloc'd)
    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC user_type_dealloc

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata
ig_getattr_name: db "__getattr__", 0
id_del_ignored_msg: db "Exception ignored in __del__", 10
id_del_ignored_len equ $ - id_del_ignored_msg
section .data

instance_repr_cstr: db "<instance>", 0
init_name_cstr:     db "__init__", 0
tc_abstract_name: db "__abstractmethods__", 0
new_name_cstr:      db "__new__", 0
tga_name_str:       db "__name__", 0
method_name_str:    db "method", 0
object_name_str:    db "object", 0
user_type_name_str: db "type", 0
super_name_str:     db "super", 0

; user_type_metatype - metatype for user-defined classes
; When accessing Foo.x, we go through Foo->ob_type->tp_getattr = type_getattr
; which looks in Foo->tp_dict. When calling Foo(), we go through
; Foo->ob_type->tp_call = type_call which creates instances.
align 8
global user_type_metatype
user_type_metatype:
    dq 1                        ; ob_refcnt (immortal)
    dq user_type_metatype       ; ob_type (self-referential)
    dq user_type_name_str       ; tp_name
    dq TYPE_OBJECT_SIZE         ; tp_basicsize
    dq user_type_dealloc        ; tp_dealloc — free heap types
    dq type_repr                ; tp_repr — <class 'Name'>
    dq type_repr                ; tp_str — same as repr
    dq 0                        ; tp_hash
    dq type_call                ; tp_call — calling a class creates instances
    dq type_getattr             ; tp_getattr — accessing class vars via tp_dict
    dq type_setattr             ; tp_setattr — setting class vars in tp_dict
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq type_type                ; tp_base — metatype inherits from type
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC | TYPE_FLAG_METATYPE  ; tp_flags (heaptypes are gc_alloc'd)
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; object_type - base type for all Python objects
; Used as explicit base class: class Foo(object): pass
; Also callable: object() returns a bare instance
align 8
global object_type
object_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq object_name_str          ; tp_name
    dq PyInstanceObject_size    ; tp_basicsize
    dq instance_dealloc         ; tp_dealloc
    dq instance_repr            ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call  (instances are not callable)
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq instance_traverse                        ; tp_traverse
    dq instance_clear                        ; tp_clear
    dq 0           ; tp_dictoffset

; super_type - placeholder for the 'super' builtin
; LOAD_SUPER_ATTR pops and discards this; it just needs to be loadable.
align 8
global super_type
super_type:
    dq 1                        ; ob_refcnt (immortal)
    dq super_type               ; ob_type (self-referential)
    dq super_name_str           ; tp_name
    dq TYPE_OBJECT_SIZE         ; tp_basicsize
    times 20 dq 0               ; remaining tp_* fields

; method_type - type descriptor for bound methods
align 8
global method_type
method_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq method_name_str          ; tp_name
    dq PyMethodObject_size      ; tp_basicsize
    dq method_dealloc           ; tp_dealloc
    dq method_repr              ; tp_repr
    dq method_repr              ; tp_str
    dq 0                        ; tp_hash
    dq method_call              ; tp_call
    dq method_getattr           ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq method_traverse                        ; tp_traverse
    dq method_clear                        ; tp_clear
    dq 0         ; tp_dictoffset

section .text

;; ============================================================================
;; GC traverse and clear.  These lived in gc.asm, which left the collector
;; holding the reference graph of every type in the system; a type's own
;; file is the only place that knows which of its fields are owned.
;; ============================================================================

; ---- method_traverse / method_clear ----
DEF_FUNC method_traverse
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_func]
    VISIT_PTR rdi
    mov rdi, [rbx + PyMethodObject.im_self]
    VISIT_V rdi, rsi            ; a Value: an immediate self is not an address

    pop rbx
    leave
    ret
END_FUNC method_traverse

DEF_FUNC method_clear
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_func]
    mov qword [rbx + PyMethodObject.im_func], 0
    test rdi, rdi
    jz .no_func
    call obj_decref
.no_func:
    mov rdi, [rbx + PyMethodObject.im_self]
    mov qword [rbx + PyMethodObject.im_self], 0
    XDECREF_V rdi, rsi
.no_self:

    pop rbx
    leave
    ret
END_FUNC method_clear

; ---- instance_traverse / instance_clear ----
DEF_FUNC instance_traverse
    push rbx
    push r12
    push r13

    mov rbx, rdi

    ; Visit the instance dict, wherever this family keeps it
    LOAD_INST_DICT rdi, rbx, .no_inst_dict
    VISIT_PTR rdi
.no_inst_dict:

    ; Visit __slots__ values (one Value each, after the instance header).
    ; The header ends at tp_dictoffset plus the dict word, or at
    ; PyInstanceObject_size when the family keeps no dict.
    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_dictoffset]
    test rcx, rcx
    jz .it_no_dict_hdr
    cmp rcx, TP_DICT_AT_TAIL
    je .it_no_dict_hdr          ; the dict is past the data, not in the header
    add rcx, 8
    jmp .it_have_hdr
.it_no_dict_hdr:
    ; No dict word: a str subclass, whose header is the base's, not
    ; PyInstanceObject's.  Using 24 there found a phantom slot at +24 --
    ; PyStrObject.ob_hash -- and XDECREF'd the hash as if it were a pointer.
    mov rcx, [rax + PyTypeObject.tp_base]
    test rcx, rcx
    jz .it_no_dict_hdr_default
    mov rcx, [rcx + PyTypeObject.tp_basicsize]
    test rcx, rcx
    jnz .it_have_hdr
.it_no_dict_hdr_default:
    mov rcx, PyInstanceObject_size
.it_have_hdr:
    mov rax, [rax + PyTypeObject.tp_basicsize]
    sub rax, rcx
    jle .done
    shr rax, 3                  ; nslots
    mov r13, rax
    lea r12, [rbx + rcx]

.slot_loop:
    mov rdi, [r12]
    VISIT_V rdi, rsi
    add r12, 8
    dec r13
    jnz .slot_loop

.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC instance_traverse

DEF_FUNC instance_clear
    push rbx
    mov rbx, rdi

    ; XDECREF + NULL the instance dict, wherever this family keeps it
    LOAD_INST_DICT rdi, rbx, .done
    test rdi, rdi
    jz .done
    xor eax, eax
    STORE_INST_DICT rbx, rax, rcx, .ic_decref
.ic_decref:
    call obj_decref

.done:
    pop rbx
    leave
    ret
END_FUNC instance_clear
