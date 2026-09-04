; pyo/class.asm - Class instances and bound methods for apython
; Phase 10: class instantiation, attribute access, __init__ dispatch

%include "macros.inc"
%include "object.inc"
extern type_number_methods

extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern raise_exception
extern none_singleton
extern obj_incref
extern dict_new
extern dict_get
extern dict_set
extern str_from_cstr
extern str_from_cstr_heap
extern ap_strcmp
extern type_repr
extern attr_error_pending
extern exc_AttributeError_type
extern exc_TypeError_type
extern func_type
extern type_type
extern kw_names_pending
extern eval_exception_unwind
extern dunder_lookup
extern dunder_call_2
extern builtin_func_type
extern current_exception
extern dict_type
extern tuple_type
extern int_type
extern str_type
extern staticmethod_type
extern method_new
extern instance_new
extern int_sub_new
extern str_sub_new
extern tuple_sub_fill
extern builtin_sub_init_base
extern classmethod_type
extern property_type

;; ============================================================================
;; instance_getattr(rdi = instance, rsi = name str) -> rax = Value, or 0
;;
;; tp_getattr for a heaptype instance, and the entry point of the whole
;; attribute protocol -- which means `__getattribute__` first.  A class that
;; defines one intercepts EVERY access, found or not: `c.x` runs it even when
;; x is a plain class attribute.  It used to be ignored entirely, so the only
;; time a user's ran was when the name was also missing and __getattr__ would
;; have run anyway.
;;
;; object's own is the one every class inherits, and calling it would be an
;; infinite regress, so it is recognised and skipped -- and
;; object.__getattribute__, which a user's almost always delegates to, enters
;; at instance_getattr_default below to skip the hook the same way CPython's
;; slot dispatch does.
;; ============================================================================
IGA_SELF  equ 8
IGA_NAME  equ 16
IGA_FRAME equ 32            ; + 0 pushes = 32, 16-aligned
DEF_FUNC instance_getattr, IGA_FRAME
    mov qword [rel attr_error_pending], 0
    mov [rbp - IGA_SELF], rdi
    mov [rbp - IGA_NAME], rsi
    test rdi, rdi
    jz .iga_default
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .iga_default
    ; object.__getattribute__ delegating back is not the hook running again:
    ; it asks for the ordinary resolution, and it says so by naming the object
    ; it is asking about.  CPython gets this from slot dispatch -- calling
    ; object's slot cannot reach the subclass's -- and this is the same thing
    ; said explicitly.
    mov rax, [rel instance_getattr_skip]
    cmp rax, rdi
    jne .iga_hook
    mov qword [rel instance_getattr_skip], 0
    jmp .iga_default
.iga_hook:
    mov rdi, [rdi + PyObject.ob_type]
    lea rsi, [rel ig_getattribute_name]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .iga_default
    cmp edx, TAG_PTR
    jne .iga_default
    ; object's own is a builtin wrapping object_method_getattribute.  Anything
    ; else is a definition, and definitions run.
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel builtin_func_type]
    cmp rcx, r8
    jne .iga_call
    extern object_method_getattribute
    lea r8, [rel object_method_getattribute]
    cmp [rax + PyBuiltinObject.func_ptr], r8
    je .iga_default
.iga_call:
    mov rdi, [rbp - IGA_SELF]
    mov rsi, [rbp - IGA_NAME]
    lea rdx, [rel ig_getattribute_name]
    mov ecx, TAG_PTR
    call dunder_call_2          ; a (payload, tag) pair, not a Value
    V_UNPACK rax, rdx
    test edx, edx
    jz .iga_raised
    leave
    V_PACK rax, rdx
    ret
.iga_raised:
    ; Whatever __getattribute__ raised is the answer, including a KeyError or
    ; a TypeError.  Returning NULL bare would let raise_no_attribute replace
    ; it with a generic AttributeError; the flag is how instance_getattr
    ; already hands one over.
    cmp qword [rel current_exception], 0
    je .iga_raised_bare
    mov qword [rel attr_error_pending], 1
.iga_raised_bare:
    xor eax, eax
    leave
    ret
.iga_default:
    mov rdi, [rbp - IGA_SELF]
    mov rsi, [rbp - IGA_NAME]
    leave
    jmp instance_getattr_default
END_FUNC instance_getattr

;; ============================================================================
;; instance_getattr_default(PyInstanceObject *self, PyObject *name) -> Value
;; Look up an attribute on an instance, without the __getattribute__ hook.
;; 1. Check self->inst_dict — return raw value
;; 2. If not found, check type->tp_dict (walk tp_base chain)
;; 3. If found in type dict and callable, create bound method
;; 4. If found, INCREF and return
;; 5. If not found, __getattr__, then AttributeError
;;
;; rdi = instance, rsi = name (PyStrObject*)
;; Returns: owned reference to attribute value, or NULL
;; ============================================================================
IG_NAME   equ 8
IG_ORIGIN equ 16        ; the type the MRO walk started from
IG_FRAME  equ 40            ; + 3 pushes = 64, 16-aligned
global instance_getattr_default
DEF_FUNC instance_getattr_default, IG_FRAME
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

    ; A getset descriptor calls its getter.  int, float and complex register
    ; real/imag/numerator/denominator this way, and a subclass instance
    ; reaches them here rather than through the base's tp_getattr.
    extern getset_descr_type
    lea rdx, [rel getset_descr_type]
    cmp rcx, rdx
    je .found_getset

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
    ; Member descriptor found — read the value out of the instance
    ; r13 = member descriptor, rbx = instance
    mov rcx, [r13 + PyMemberDescrObject.md_offset]
    SLOT_ADDR rdx, rbx, rcx
    mov rax, [rdx]             ; slot Value
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

.found_getset:
    ; getset_descr_get answers a Value, or never returns when the attribute
    ; has no getter at all.
    mov rdi, r13
    mov rsi, rbx
    extern getset_descr_get
    call getset_descr_get
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
    ; (must not return NULL or LOAD_ATTR fallback finds descriptor in tp_dict).
    ; CPython names the type and the attribute here as it does everywhere else.
    mov rdi, rbx
    mov rsi, [rbp - IG_NAME]
    extern raise_no_attribute
    call raise_no_attribute

.not_found:
    ; __class__ and __dict__ are part of ordinary resolution, not of the hook:
    ; CPython answers them from getsets on the type, so a class that defines
    ; __getattr__ never sees either name.  Asking the hook first made
    ; `self.__dict__` INSIDE a __getattr__ re-enter it -- which is how
    ; typing.py's _BaseGenericAlias.__getattr__ is written, and it recursed
    ; until the stack ran out, taking the whole typing module with it.
    mov rdi, rbx
    mov rsi, [rbp - IG_NAME]
    extern obj_generic_attr
    call obj_generic_attr
    test rax, rax
    jz .ig_ask_getattr
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.ig_ask_getattr:
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
END_FUNC instance_getattr_default

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
    je .sa_member

    ; A property is a data descriptor too, and this is the only road a DELETE
    ; takes: op_store_attr has a property fast path of its own, op_delete_attr
    ; has none and comes straight here.  So `del obj.prop` never reached the
    ; deleter -- it fell through to the instance dict and did nothing.
    lea rcx, [rel property_type]
    cmp [r9 + PyObject.ob_type], rcx
    jne .sa_check_getset
    mov rdi, r9
    mov rsi, rbx
    mov rdx, r13                ; the value Value; 0 means delete
    extern property_descr_set
    call property_descr_set
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_check_getset:
    ; A getset descriptor is a data descriptor: it takes precedence over the
    ; instance dict, so `I(5).real = 9` is the AttributeError CPython raises
    ; rather than a shadowing instance attribute.
    extern getset_descr_type
    lea rcx, [rel getset_descr_type]
    cmp [r9 + PyObject.ob_type], rcx
    jne .sa_no_slot
    mov rdi, r9
    mov rsi, rbx
    mov rdx, r13
    extern getset_descr_set
    call getset_descr_set
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_member:

    ; Member descriptor! Write the value into the slot
    mov rcx, [r9 + PyMemberDescrObject.md_offset]
    SLOT_ADDR rdx, rbx, rcx

    ; XDECREF old value at slot
    push rdx
    mov rdi, [rdx]             ; old Value
    XDECREF_V rdi, rsi
    pop rdx

    ; INCREF the new value and store it
    INCREF_V r13, r14
    mov [rdx], r13

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
    ; A NULL value means DELETE, not "store a NULL".  dict_set was called
    ; either way, so `del obj.attr` left the key in the instance dict bound
    ; to a NULL Value: vars(obj) could not be repr'd, len(vars(obj)) still
    ; counted it, and deleting twice succeeded.
    test r13, r13
    jz .sa_dict_del
    mov rsi, r12                ; name
    mov rdx, r13                ; value
    call dict_set

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_dict_del:
    mov rsi, r12                ; name
    extern dict_del_opt
    call dict_del_opt           ; -1 when it was never there
    test eax, eax
    jnz .sa_del_missing
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_del_missing:
    mov rdi, rbx
    mov rsi, r12
    call raise_no_attribute     ; does not return

.sa_no_dict_error:
.sa_no_dict_slot:
    ; This type's instances have no dict slot -- a str subclass, or a class
    ; with __slots__ -- so there is nowhere to put the attribute.  The message
    ; names both the type and the attribute, as every other one here does; a
    ; bare "object has no attribute" said neither.
    mov rdi, rbx
    mov rsi, r12
    extern raise_no_attribute
    call raise_no_attribute
END_FUNC instance_setattr

;; ============================================================================
;; type_setattr(PyTypeObject *type, PyObject *name, PyObject *value, ecx=value_tag)
;; Set an attribute on a type's tp_dict.
;; rdi = type, rsi = name, rdx = value, ecx = value_tag
;; ============================================================================
DEF_FUNC type_setattr
    push rbx
    push rcx                    ; keep the stack aligned

    ; --- __name__ renames the class ---
    ; A class's name is tp_name, not a dict entry, so `C.__name__ = "x"` set a
    ; key nothing ever read and the class kept its old name.  typing.py
    ; renames two classes and then registers them in sys.modules UNDER THE
    ; NEW NAME -- so `sys.modules["re"]` became typing's deprecated `re`
    ; class, and the next `import re` handed a class to everything that
    ; wanted the module.
    mov rbx, rdi
    test rsi, rsi
    jz .ts_not_name
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .ts_not_name
    lea rdi, [rsi + PyStrObject.data]
    push rsi
    push rdx
    CSTRING rsi, "__name__"
    call ap_strcmp
    pop rdx
    pop rsi
    test eax, eax
    jz .ts_rename
.ts_not_name:
    mov rdi, rbx
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

.ts_rename:
    ; tp_name points into a PyStrObject's data, and the type owns a reference
    ; to that string -- user_type_dealloc recovers it the same way.  So a
    ; rename is: take the new one, point at its data, drop the old.
    mov rax, rdx
    test rax, rax
    jz .ts_rename_bad
    V_TEST_PTR rax, rcx
    ja .ts_rename_bad
    mov rcx, [rax + PyObject.ob_type]
    lea rdi, [rel str_type]
    cmp rcx, rdi
    jne .ts_rename_bad
    mov rcx, [rbx + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_HEAPTYPE
    jz .ts_rename_static
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
    mov rcx, [rbx + PyTypeObject.tp_name]
    lea rdx, [rax + PyStrObject.data]
    mov [rbx + PyTypeObject.tp_name], rdx
    test rcx, rcx
    jz .ts_rename_done
    sub rcx, PyStrObject.data
    mov rdi, rcx
    call obj_decref
.ts_rename_done:
    xor eax, eax
    pop rcx
    pop rbx
    leave
    ret
.ts_rename_static:
    RAISE exc_TypeError_type, "cannot set __name__ of a built-in type"
.ts_rename_bad:
    RAISE exc_TypeError_type, "can only assign string to __name__"
END_FUNC type_setattr

;; ============================================================================
;; instance_dealloc(PyObject *self)
;; Deallocate an instance: DECREF inst_dict, DECREF ob_type, free self.
;; rdi = instance
;; ============================================================================
ID_EXC   equ 8
ID_OWNED equ 16          ; whether the reference below is ours to drop
ID_FRAME equ 24             ; + 1 push = 32
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
    ; Snapshot what was pending, and hold a reference to it: if __del__ raises,
    ; installing its exception releases the global's reference to this one, and
    ; the saved pointer would be dangling by the time it is put back.
    DUNDER_EXC_SAVE [rbp - ID_EXC]
    mov qword [rbp - ID_OWNED], 0
    mov rdi, [rbp - ID_EXC]
    test rdi, rdi
    jz .del_nothing_pending
    ; ...but only when the global's own reference is real.  The unwinder can
    ; reach here with current_exception pointing at an object whose refcount
    ; is already zero, and taking and dropping a reference on that one frees
    ; an exception that is still being carried.
    ;
    ; No path produces that state today: the case bugs.md used to carry does
    ; not reproduce, valgrind is clean over the async suite, and a watch here
    ; gets no hits across the corpus.  The check stays anyway.  It is two
    ; instructions, and the invariant behind it is genuinely fragile --
    ; raise_exception_obj takes over its caller's reference rather than
    ; adding one, so the global's is often the only reference there is, and
    ; this function runs while the unwinder is releasing the value stack.
    cmp qword [rdi + PyObject.ob_refcnt], 0
    jle .del_nothing_pending
    call obj_incref
    mov qword [rbp - ID_OWNED], 1
.del_nothing_pending:
    mov rdi, rbx
    lea rsi, [rel dunder_del]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    ; Ignore return value — DECREF if non-NULL
    test edx, edx
    jz .del_no_result
    DECREF_VAL rax, rdx
.del_no_result:

    ; A __del__ that raises must not leave the exception pending: the object is
    ; being freed, there is no caller to hand it to, and leaving it set means
    ; the *next* raise silently discards it -- or, if this dealloc came from
    ; the unwinder dropping the value stack, that the handler receives the
    ; wrong exception object.  CPython reports it and puts back what was there.
    ;
    ; "Did it raise?" is EXC_RAISED_SINCE's three-way question and not a bare
    ; inequality.  current_exception is also the exception being HANDLED, and a
    ; __del__ that raises and catches internally leaves the global at 0: a
    ; change, but not a raise.  DUNDER_RAISED read that as one, and this arm
    ; then zeroed the global -- destroying the exception the interpreter was
    ; carrying, so that a __del__ running during an unwind made the enclosing
    ; except block never run.
    EXC_RAISED_SINCE [rbp - ID_EXC], rax, .del_report

.del_restore:
    ; Whatever __del__ left behind, the pending exception goes back to what it
    ; was.  The reference taken above is what the global gets; anything else
    ; sitting there is released.
    mov rax, [rbp - ID_EXC]
    cmp [rel current_exception], rax
    je .del_drop_saved
    mov rdi, [rel current_exception]
    mov [rel current_exception], rax    ; the saved reference moves in here
    test rdi, rdi
    jz .del_cleared
    call obj_decref
    jmp .del_cleared
.del_drop_saved:
    ; Unchanged, so the global still owns its own; drop the extra one.
    cmp qword [rbp - ID_OWNED], 0
    je .del_cleared
    mov rdi, rax
    test rdi, rdi
    jz .del_cleared
    call obj_decref

.del_cleared:

    ; Restore refcount (undo the bump)
    dec qword [rbx + PyObject.ob_refcnt]

    jmp .no_del

.del_report:
    ; A genuinely new exception: report it in full on stderr -- the object it
    ; came out of, and the traceback of where inside __del__ it happened --
    ; then put back the old one.  This used to be one line that named neither.
    ; CPython names the __del__ FUNCTION, not the object it was called on.
    ; The lookup is borrowed, and cannot raise: it just found this method.
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel dunder_del]
    call dunder_lookup
    V_UNPACK rax, rdx
    cmp edx, TAG_PTR
    je .del_report_have_fn
    xor eax, eax
.del_report_have_fn:
    mov rsi, rax
    mov rdi, [rel current_exception]
    extern traceback_print_unraisable
    call traceback_print_unraisable
    jmp .del_restore

.no_del:
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
    ; A base's slots are BELOW the dict word when the subclass is the one that
    ; added the dict: `class C: __slots__ = ('a',)` puts a at 16 and `class
    ; D(C): pass` puts D's dict at 24, so starting past the dict found no
    ; slots at all and D never released C's.  The floor is the family's, and
    ; the walk skips the dict word wherever it sits.
    push rax
    push rcx
    mov rdi, rax
    call instance_slot_floor
    mov rdx, rax
    pop rcx
    pop rax
    test rdx, rdx
    jz .id_have_hdr
    cmp rdx, rcx
    jae .id_have_hdr
    mov rcx, rdx
    ; ...but the header does not end at the dict word when the layout base
    ; keeps fields of its own PAST it.  _io.FileIO is built by
    ; type_from_parts and then has its tp_basicsize patched up to make room
    ; for the descriptor, the flags, the name, the blksize and the mode --
    ; all above tp_dictoffset + 8.  `class F(_io.FileIO): pass` therefore
    ; walked five of FileIO's own fields as if they were __slots__ Values and
    ; XDECREF'd them: a file descriptor of 3 is `dec qword [3]`, a wild write
    ; on ordinary single inheritance with no __slots__ anywhere.
    ;
    ; tp_base is the layout base -- the type whose fields sit below the
    ; subclass's own slots -- so its basicsize is the real floor.
    jmp .id_have_hdr
.id_no_dict_hdr:
    ; No dict word: a str subclass, whose header is the base's, not
    ; PyInstanceObject's.  Using 24 there found a phantom slot at +24 --
    ; PyStrObject.ob_hash -- and XDECREF'd the hash as if it were a pointer.
    push rax
    mov rdi, rax
    call instance_slot_floor
    mov rcx, rax
    pop rax
    test rcx, rcx
    jnz .id_have_hdr
    mov rcx, OBJ_HEADER_SIZE
.id_have_hdr:
    push r13
    mov r13, [rax + PyTypeObject.tp_dictoffset]
    cmp r13, TP_DICT_AT_TAIL
    jne .id_dict_off_ok
    xor r13d, r13d              ; nothing in the header to skip
.id_dict_off_ok:
    mov rax, [rax + PyTypeObject.tp_basicsize]
    cmp rax, rcx
    jbe .id_slots_done          ; no slots
    ; Downwards, from the most derived class's slots to the base's, which is
    ; the order CPython releases them in and therefore the order two __del__s
    ; run in.
    mov r12, rax
.slot_decref_loop:
    sub r12, 8
    cmp r12, rcx
    jb .id_slots_done
    cmp r12, r13
    je .slot_decref_loop        ; the instance dict, released above
    push rcx
    lea rdi, [rbx + r12]
    mov rdi, [rdi]              ; slot Value
    XDECREF_V rdi, rsi
    pop rcx
    jmp .slot_decref_loop
.id_slots_done:
    pop r13

    ; And the slots that live at the TAIL, which a str subclass's do: past
    ; the characters and past the word the tail __dict__ occupies.  They are
    ; not in the header at all, so the walk above cannot reach them.
    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_tailslots]
    test rcx, rcx
    jz .id_tail_done
    INST_DICT_TAIL r12, rbx
    lea r12, [r12 + rcx*8]      ; the last one; downwards, as above
.id_tail_loop:
    push rcx
    mov rdi, [r12]
    XDECREF_V rdi, rsi
    pop rcx
    sub r12, 8
    dec rcx
    jnz .id_tail_loop
.id_tail_done:

.no_slots:
    pop r12

    ; XDECREF the instance dict; a type may have no dict slot at all.  AFTER
    ; the slots, which is the order CPython's subtype_dealloc uses and so the
    ; order two __del__s run in.  The slot walk skips this word.
    LOAD_INST_DICT rdi, rbx, .no_dict
    test rdi, rdi
    jz .no_dict
    call obj_decref
.no_dict:

    ; A bytearray subclass owns a second allocation -- its bytes -- that the
    ; slot walk above knows nothing about, and the base's own dealloc never
    ; runs for a subclass.  Every instance leaked its buffer.
    mov rax, [rbx + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_BYTEARRAY_SUBCLASS
    jz .id_no_bytes
    mov rdi, [rbx + PyByteArrayObject.ob_bytes]
    test rdi, rdi
    jz .id_no_bytes
    mov qword [rbx + PyByteArrayObject.ob_bytes], 0
    mov qword [rbx + PyByteArrayObject.ob_cap], 0
    mov qword [rbx + PyByteArrayObject.ob_size], 0
    extern ap_free
    call ap_free
.id_no_bytes:

    ; The same shape for the three containers that keep their storage out of
    ; line.  A dict, list or set subclass instance owns tables the slot walk
    ; above knows nothing about, and the base's own dealloc never runs for a
    ; subclass -- so every instance leaked both its contents and its tables:
    ; `class D(dict): pass` leaked 256 bytes per D(), and a list or set
    ; subclass the same in its own currency.  The contents go through the
    ; type's existing tp_clear, which is exactly "release everything held and
    ; leave the tables empty"; the tables are freed here afterwards.
    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_DICT_SUBCLASS
    jnz .id_dict_storage
    test rcx, TYPE_FLAG_LIST_SUBCLASS
    jnz .id_list_storage
    test rcx, TYPE_FLAG_SET_SUBCLASS
    jnz .id_set_storage
    test rcx, TYPE_FLAG_TUPLE_SUBCLASS
    jnz .id_tuple_storage
    jmp .id_no_storage

.id_tuple_storage:
    ; tuple_sub_fill gives a subclass instance its own ob_item array and
    ; INCREFs every element into it.  tuple has no tp_clear to borrow, so the
    ; walk is written out.  Only when ob_size is positive: tuple_sub_fill
    ; allocates nothing for an empty one and leaves ob_item unwritten, so
    ; there is nothing there to read, let alone free.
    ;
    ; r12 and r13 belong to the caller here -- instance_dealloc saves only
    ; rbx -- so they are pushed around the loop rather than simply used.
    mov rax, [rbx + PyTupleObject.ob_size]
    test rax, rax
    jle .id_no_storage
    push r12
    push r13
    mov r12, rax
    xor r13d, r13d
.id_tuple_loop:
    cmp r13, r12
    jge .id_tuple_free
    mov rax, [rbx + PyTupleObject.ob_item]
    mov rdi, [rax + r13*8]
    XDECREF_V rdi, rsi
    inc r13
    jmp .id_tuple_loop
.id_tuple_free:
    mov qword [rbx + PyTupleObject.ob_size], 0
    mov rdi, [rbx + PyTupleObject.ob_item]
    mov qword [rbx + PyTupleObject.ob_item], 0
    pop r13
    pop r12
    test rdi, rdi
    jz .id_no_storage
    call ap_free
    jmp .id_no_storage

.id_dict_storage:
    extern dict_clear_gc
    mov rdi, rbx
    call dict_clear_gc
    mov rdi, [rbx + PyDictObject.entries]
    test rdi, rdi
    jz .id_dict_no_entries
    mov qword [rbx + PyDictObject.entries], 0
    call ap_free
.id_dict_no_entries:
    mov rdi, [rbx + PyDictObject.dk_indices]
    test rdi, rdi
    jz .id_no_storage
    mov qword [rbx + PyDictObject.dk_indices], 0
    mov qword [rbx + PyDictObject.capacity], 0
    call ap_free
    jmp .id_no_storage

.id_list_storage:
    extern list_clear
    mov rdi, rbx
    call list_clear
    mov rdi, [rbx + PyListObject.ob_item]
    test rdi, rdi
    jz .id_no_storage
    mov qword [rbx + PyListObject.ob_item], 0
    mov qword [rbx + PyListObject.ob_size], 0
    call ap_free
    jmp .id_no_storage

.id_set_storage:
    extern set_clear_gc
    mov rdi, rbx
    call set_clear_gc
    mov rdi, [rbx + PyDictObject.entries]
    test rdi, rdi
    jz .id_no_storage
    mov qword [rbx + PyDictObject.entries], 0
    mov qword [rbx + PyDictObject.capacity], 0
    call ap_free

.id_no_storage:

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
IR_FRAME equ 24            ; + 1 push = 32, 16-aligned
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
    ; object.__repr__: "<module.qualname object at 0x...>", with the module
    ; left out when it is "builtins".  This used to be the fixed string
    ; "<instance>", which said neither which class nor which object.
    mov rdi, rbx
    call instance_repr_default

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
;; instance_repr_default(rdi = the instance) -> rax = PyStrObject*
;;
;; The shape object.__repr__ has: the defining module and the qualified name,
;; then the address.  The module is dropped when it is "builtins", which is
;; the rule that keeps a builtin's own default repr free of a "builtins."
;; prefix.  Both names come through type_getattr, so a class that sets
;; __qualname__ or __module__ itself is answered with what it set.
;; ============================================================================
IRD_OBJ   equ 8
IRD_MOD   equ 16
IRD_NAME  equ 24
IRD_FRAME equ 40            ; + 1 push = 48, 16-aligned

DEF_FUNC_LOCAL instance_repr_default, IRD_FRAME
    push rbx
    mov [rbp - IRD_OBJ], rdi
    mov qword [rbp - IRD_MOD], 0
    mov qword [rbp - IRD_NAME], 0
    mov rbx, [rdi + PyObject.ob_type]

    CSTRING rdi, "__qualname__"
    extern str_from_cstr_heap
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, rax
    call type_getattr
    V_UNPACK rax, rdx
    cmp edx, TAG_PTR
    je .ird_have_name
    xor eax, eax
.ird_have_name:
    mov [rbp - IRD_NAME], rax
    pop rdi
    call obj_decref

    CSTRING rdi, "__module__"
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, rax
    call type_getattr
    V_UNPACK rax, rdx
    cmp edx, TAG_PTR
    je .ird_have_mod
    xor eax, eax
.ird_have_mod:
    mov [rbp - IRD_MOD], rax
    pop rdi
    call obj_decref

    ; "builtins" is not printed, exactly as CPython's object_repr has it.
    mov rax, [rbp - IRD_MOD]
    test rax, rax
    jz .ird_build
    lea rdi, [rax + PyStrObject.data]
    CSTRING rsi, "builtins"
    extern ap_strcmp
    call ap_strcmp
    test eax, eax
    jne .ird_build
    mov rdi, [rbp - IRD_MOD]
    call obj_decref
    mov qword [rbp - IRD_MOD], 0

.ird_build:
    mov rdi, [rbp - IRD_OBJ]
    xor esi, esi
    mov rax, [rbp - IRD_MOD]
    test rax, rax
    jz .ird_no_mod
    lea rsi, [rax + PyStrObject.data]
.ird_no_mod:
    mov rdx, [rbx + PyTypeObject.tp_name]
    mov rax, [rbp - IRD_NAME]
    test rax, rax
    jz .ird_no_name
    lea rdx, [rax + PyStrObject.data]
.ird_no_name:
    extern obj_default_repr_named
    call obj_default_repr_named
    push rax
    mov rdi, [rbp - IRD_NAME]
    test rdi, rdi
    jz .ird_no_name_ref
    call obj_decref
.ird_no_name_ref:
    mov rdi, [rbp - IRD_MOD]
    test rdi, rdi
    jz .ird_no_mod_ref
    call obj_decref
.ird_no_mod_ref:
    pop rax
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC instance_repr_default

;; ============================================================================
;; instance_str(PyObject *self) -> PyStrObject*
;; Try __str__ dunder, fall back to instance_repr.
;; rdi = instance
;; ============================================================================
IS_EXC   equ 8
IS_FRAME equ 24            ; + 1 push = 32, 16-aligned
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
TC_NEW_FUNC equ 48              ; saved __new__ func pointer
TC_NEW_TAG  equ 56              ; saved __new__ result tag
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
    mov rdi, rbx
    mov rsi, rax
    extern type_abstract_error
    call type_abstract_error    ; does not return
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
    test edx, edx
    jz .init_raised             ; NULL, with the exception still pending

    ; DECREF __init__'s return value (should be None — TAG_NONE, not a pointer)
    mov rsi, rdx
    DECREF_VAL rax, rsi

    ; Restore stack (undo the sub rsp from args allocation)
    lea rax, [r13 + 1]
    shl rax, 4
    add rsp, rax
    jmp .no_init

.init_raised:
    ; What __init__ returned was never looked at, so a raise inside it
    ; produced a "successful" construction: the caller got an object whose
    ; __init__ had not finished, and the exception surfaced at whatever ran
    ; next, as a "During handling of the above exception" chain attached to
    ; code that had nothing to do with it.  io.StringIO(5) is where it turned
    ; up -- it raises TypeError from __init__ and got back a StringIO.
    lea rax, [r13 + 1]
    shl rax, 4
    add rsp, rax
    mov rax, r14
    mov rsi, [rbp - TC_NEW_TAG]
    DECREF_VAL rax, rsi         ; the half-built instance goes no further
    xor r14d, r14d
    mov qword [rbp - TC_NEW_TAG], 0

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
TGA_FROMMETA equ 24         ; where to report which walk answered, or 0
TGA_FRAME  equ 32           ; + 2 pushes = 48
DEF_FUNC_BARE type_getattr
    xor edx, edx                ; no caller wants to know where it came from
    jmp type_getattr_meta
END_FUNC type_getattr

;; ============================================================================
;; type_getattr_meta(rdi = type, rsi = name, rdx = &from_metatype) -> Value
;;
;; The same lookup, reporting WHICH of the two MROs answered: the class's own,
;; or its metatype's.  It writes 1 through rdx for the metatype and 0 for the
;; class, and rdx may be 0.
;;
;; The caller that needs this is the descriptor protocol.  CPython does not
;; run a property's getter when the property was found in the class's own MRO
;; -- `C.prop` IS the property object, which is how `C.prop.__doc__ = ...` can
;; be written at all -- but it does run one found on the METATYPE, which is
;; what makes Enum.__members__ work.  Deciding that from "is the object a
;; class" gets the second case wrong, and until now this function had no way
;; to say which it was.
;; ============================================================================
DEF_FUNC type_getattr_meta, TGA_FRAME
    push rbx
    push r12

    mov [rbp - TGA_FROMMETA], rdx
    test rdx, rdx
    jz .tga_no_out
    mov qword [rdx], 0
.tga_no_out:
    mov rbx, rsi                ; rbx = name
    mov r12, rdi                ; r12 = type (walks)
    mov [rbp - TGA_ORIGIN], rdi

    ; Check for __name__: compare name string data with "__name__"
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [rel tga_name_str]
    call ap_strcmp
    test eax, eax
    jz .tga_return_name

    ; A builtin type's __qualname__ is its __name__, and its __module__ is
    ; "builtins".  Neither existed, so `ValueError.__qualname__` was an
    ; AttributeError -- and CPython's traceback and warnings machinery reads
    ; both off an exception's type.  A heaptype sets them in its own dict,
    ; which the walk below finds first.
    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__qualname__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_qualname

    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__module__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_module

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

    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__base__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_base

    ; The instance layout, as CPython reports it.  These are getsets on the
    ; metatype in CPython, so they are data descriptors and win over anything
    ; a class body puts under the same name -- hence the check here, ahead of
    ; the tp_dict walk, rather than after it.
    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__basicsize__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_basicsize

    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__dictoffset__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_dictoffset

    lea rdi, [rbx + PyStrObject.data]
    CSTRING rsi, "__weakrefoffset__"
    call ap_strcmp
    test eax, eax
    jz .tga_return_weakrefoffset

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

.tga_return_base:
    ; The one base a class's layout comes from.  `object.__base__` is None,
    ; which is also the answer for any other type with no tp_base.
    mov rax, [rbp - TGA_ORIGIN]
    mov rax, [rax + PyTypeObject.tp_base]
    test rax, rax
    jnz .tga_return_object
    extern none_singleton
    lea rax, [rel none_singleton]
.tga_return_object:
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
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

.tga_return_basicsize:
    mov rdi, [r12 + PyTypeObject.tp_basicsize]
    jmp .tga_return_layout_int

.tga_return_dictoffset:
    mov rdi, [r12 + PyTypeObject.tp_dictoffset]
    jmp .tga_return_layout_int

.tga_return_weakrefoffset:
    ; No weakref word in the instance layout yet: the links live in a side
    ; table, so every type reports 0, which is also what CPython reports for
    ; a type whose instances cannot be weak-referenced.
    xor edi, edi

.tga_return_layout_int:
    extern int_from_i64
    call int_from_i64
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tga_return_qualname:
    ; A class defined in Python records its own, which carries the enclosing
    ; scope -- "outer.<locals>.Local".  Only a builtin type falls through to
    ; __name__.
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tga_return_name
    mov rsi, rbx
    extern dict_get
    call dict_get
    test rax, rax
    jz .tga_return_name
    V_UNPACK rax, rdx
    INCREF_VAL rax, rdx
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tga_return_module:
    ; A class defined in Python records its own __module__ in its dict; only
    ; a builtin type falls through to the default below.
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tga_module_builtin
    mov rsi, rbx
    extern dict_get
    call dict_get
    test rax, rax
    jz .tga_module_builtin
    V_UNPACK rax, rdx
    INCREF_VAL rax, rdx
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tga_module_builtin:
    ; The dotted prefix of tp_name when there is one -- "_io.FileIO" is in
    ; module "_io" -- and "builtins" otherwise, as CPython has it.
    mov rsi, [r12 + PyTypeObject.tp_name]
    xor ecx, ecx
    xor r8d, r8d                ; the index just past the last dot, or 0
.tga_mod_scan:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .tga_mod_done
    cmp al, '.'
    jne .tga_mod_next
    mov r8, rcx
.tga_mod_next:
    inc rcx
    jmp .tga_mod_scan
.tga_mod_done:
    test r8, r8
    jz .tga_mod_plain
    mov rdi, rsi
    mov rsi, r8
    extern str_new_heap
    call str_new_heap
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
.tga_mod_plain:
    CSTRING rdi, "builtins"
    extern str_from_cstr_heap
    call str_from_cstr_heap
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tga_return_name:
    ; __name__ is the last dotted component of tp_name: CPython stores
    ; "types.GenericAlias" but reports "GenericAlias", keeping the qualified
    ; form for the repr.  That rule is the STATIC one; a heaptype answers its
    ; whole name, which is how a class renamed to "typing.re" reports the
    ; dotted form CPython's does.
    mov rdi, [r12 + PyTypeObject.tp_name]
    mov rcx, [r12 + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_HEAPTYPE
    jnz .tga_name_done
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
    ; Then the metatype's own MRO.  Anything found from here on came from the
    ; metatype, which the caller may need to know.
    mov rax, [rbp - TGA_FROMMETA]
    test rax, rax
    jz .tga_meta_no_out
    mov qword [rax], 1
.tga_meta_no_out:
    ; A metaclass's methods are attributes of the classes it makes, bound to
    ; the class the way an ordinary class's methods bind to its instances --
    ; `ByteString.register` is ABCMeta's, two links up the metatype chain.
    ; Only a user metaclass is walked: the three builtin metatypes hold
    ; entries meant for `type` itself, and offering those on every class would
    ; shadow what a class inherits from object.
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
    ; rdx is the TAG, and .tga_meta_plain increfs with it.  Borrowing it as
    ; scratch for these compares left it holding a type's ADDRESS, which is
    ; not TAG_PTR, so INCREF_VAL did nothing and the metatype's tp_dict was
    ; left holding a property its caller then released -- a use-after-free
    ; that reproduces as `class Meta(type)` with a property, read twice.
    lea r8, [rel func_type]
    cmp rcx, r8
    je .tga_meta_bind
    ; A builtin binds too, as it does everywhere else a method is fetched.
    ; type.__subclasses__ is one, and this walk is the ONLY road to it for a
    ; class whose metatype is a metaclass of its own -- every ABC and every
    ; Enum -- so A.__subclasses__() came back unbound and answered
    ; "takes no arguments".
    lea r8, [rel builtin_func_type]
    cmp rcx, r8
    jne .tga_meta_plain
.tga_meta_bind:
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
END_FUNC type_getattr_meta

;; ============================================================================
;; object_type_call(args, nargs) -> PyObject*
;; object() returns a bare instance of object_type
;; ============================================================================
DEF_FUNC_BARE object_type_call
    ; Create a bare instance with object_type (gc_alloc since HAVE_GC)
    push rbp
    mov rbp, rsp
    mov edi, OBJ_HEADER_SIZE
    lea rsi, [rel object_type]
    call gc_alloc

    ; gc_alloc does not INCREF the type it stamps into ob_type, and
    ; instance_dealloc DECREFs it -- so without this the reference count of
    ; object_type itself went down by one for every object() that died.  It
    ; starts at 1, so the FIRST such instance took it to zero and handed
    ; &object_type, a .data address, to ap_free: the heap was corrupted from
    ; then on, and the crash landed in whatever allocated next.
    ; instance_new and slots_new both INCREF here for the same reason.
    push rax
    lea rdi, [rel object_type]
    call obj_incref
    pop rax

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

;; ============================================================================
;; type_traverse / type_clear -- a heap type is a GC object like any other
;;
;; user_type_metatype carried TYPE_FLAG_HAVE_GC and a NULL tp_traverse, so
;; every class was tracked and none was reachable THROUGH: the collector could
;; see a class but not what it held.  A class always sits in a cycle -- its
;; own tp_mro tuple contains it -- so with no traverse none of them was ever
;; collected.  `def f(): class Temp: pass` leaked a class per call, and so did
;; every decorator, factory and closure that builds one.
;;
;; type_clear follows CPython's: the dict's contents and tp_mro, and not
;; tp_base or tp_bases.  Breaking the MRO is enough to break the cycle, and
;; the bases are what the type needs to stay coherent while the rest of the
;; collection runs.
;; ============================================================================
global type_traverse
DEF_FUNC type_traverse
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyTypeObject.tp_dict]
    VISIT_PTR rdi
    mov rdi, [rbx + PyTypeObject.tp_base]
    VISIT_PTR rdi
    mov rdi, [rbx + PyTypeObject.tp_bases]
    VISIT_PTR rdi
    mov rdi, [rbx + PyTypeObject.tp_mro]
    VISIT_PTR rdi
    pop rbx
    leave
    ret
END_FUNC type_traverse

global type_clear
DEF_FUNC type_clear, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tcl_mro
    extern dict_clear_gc
    call dict_clear_gc          ; the contents, not the dict itself
.tcl_mro:
    mov rdi, [rbx + PyTypeObject.tp_mro]
    test rdi, rdi
    jz .tcl_done
    mov qword [rbx + PyTypeObject.tp_mro], 0
    call obj_decref
.tcl_done:
    pop rbx
    leave
    ret
END_FUNC type_clear

DEF_FUNC user_type_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi                ; rbx = type object

    ; Out of every base's subclass list first, while tp_bases is still there
    ; to say which they are.  The entries are borrowed, so this is the only
    ; thing that keeps a freed class out of __subclasses__().
    extern subclass_live
    cmp qword [rel subclass_live], 0
    je .utd_no_subclasses
    extern subclass_unregister
    mov rdi, rbx
    call subclass_unregister
.utd_no_subclasses:

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
ig_getattribute_name: db "__getattribute__", 0

section .data
align 8
;; The object object.__getattribute__ is currently resolving for, or 0.  Set
;; around its call and consumed by the first instance_getattr that sees it.
global instance_getattr_skip
instance_getattr_skip: dq 0
section .rodata
id_del_ignored_msg: db "Exception ignored in __del__", 10
id_del_ignored_len equ $ - id_del_ignored_msg
section .data

instance_repr_cstr: db "<instance>", 0
init_name_cstr:     db "__init__", 0
tc_abstract_name: db "__abstractmethods__", 0
new_name_cstr:      db "__new__", 0
tga_name_str:       db "__name__", 0
object_name_str:    db "object", 0
user_type_name_str: db "type", 0

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
    dq type_number_methods      ; tp_as_number -- `C | None` builds a
                                ; union, and a class's `|` is its
                                ; METATYPE's slot, not its own
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq type_type                ; tp_base — metatype inherits from type
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC | TYPE_FLAG_METATYPE  ; tp_flags (heaptypes are gc_alloc'd)
    dq 0                        ; tp_bases
    dq type_traverse            ; tp_traverse
    dq type_clear               ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots

; object_type - base type for all Python objects
; Used as explicit base class: class Foo(object): pass
; Also callable: object() returns a bare instance
align 8
extern object_hash

global object_type
object_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq object_name_str          ; tp_name
    ; object() has no __dict__, so its instances are the bare header.  This
    ; used to be PyInstanceObject_size -- the header PLUS a dict word that
    ; object's own tp_dictoffset of 0 says is not there.  Every object() paid
    ; for it, and `class B(object): pass` put its dict one word further out
    ; than `class B: pass` did, for two different layouts of the same class.
    dq OBJ_HEADER_SIZE          ; tp_basicsize
    dq instance_dealloc         ; tp_dealloc
    dq instance_repr            ; tp_repr
    dq 0                        ; tp_str
    dq object_hash              ; tp_hash
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
    dq 0                        ; tp_tailslots


section .text

;; ============================================================================
;; GC traverse and clear.  These lived in gc.asm, which left the collector
;; holding the reference graph of every type in the system; a type's own
;; file is the only place that knows which of its fields are owned.
;; ============================================================================

; ---- instance_traverse / instance_clear ----
;; ============================================================================
;; instance_slot_floor(rdi = the instance's type) -> rax = where its __slots__
;; begin
;;
;; Every heaptype whose instances this function deallocs lays out its own
;; slots directly above its base's, so a subclass's slot region includes the
;; ones its ancestors declared: `class C: __slots__ = ('a',)` and `class
;; D(C): pass` give D exactly C's layout, and taking the floor from D's own
;; base -- C -- found no slots at all and released none.  D's `a` was never
;; freed, and a cycle through it was uncollectable.
;;
;; The walk stops at the first ancestor that manages its own storage: a
;; static type, or a heaptype with a tp_dealloc of its own.  _io.FileIO is
;; the second kind -- a heaptype with a patched basicsize and five C fields
;; -- and walking past it would hand a file descriptor to DECREF_VAL.
;; ============================================================================
DEF_FUNC_LOCAL instance_slot_floor
    push rbx
    mov rbx, rdi                ; the type whose base chain is walked
    xor eax, eax                ; the floor, 0 until an ancestor sets it
.isf_loop:
    mov rcx, [rbx + PyTypeObject.tp_base]
    test rcx, rcx
    jz .isf_done
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .isf_builtin
    lea rdx, [rel instance_dealloc]
    cmp [rcx + PyTypeObject.tp_dealloc], rdx
    jne .isf_builtin            ; it keeps fields of its own
    mov rbx, rcx
    jmp .isf_loop
.isf_builtin:
    mov rax, [rcx + PyTypeObject.tp_basicsize]
.isf_done:
    pop rbx
    leave
    ret
END_FUNC instance_slot_floor

DEF_FUNC instance_traverse
    push rbx
    push r12
    push r13
    push r15                    ; r14 is the visit callback, VISIT_V's own

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
    ; The same floor and the same skip as instance_dealloc: a base's slots sit
    ; BELOW the dict word the subclass added, and starting past the dict left
    ; them unvisited -- so a cycle through an inherited slot was never
    ; collectable.  The layout base's own fields can sit past the dict word
    ; too, which is why the floor stops at the first ancestor that manages its
    ; own storage: here the consequence of walking those would be the
    ; collector visiting a file descriptor as an object pointer.
    push rax
    push rcx
    mov rdi, rax
    call instance_slot_floor
    mov rdx, rax
    pop rcx
    pop rax
    test rdx, rdx
    jz .it_have_hdr
    cmp rdx, rcx
    jae .it_have_hdr
    mov rcx, rdx
    jmp .it_have_hdr
.it_no_dict_hdr:
    ; No dict word: a str subclass, whose header is the base's, not
    ; PyInstanceObject's.  Using 24 there found a phantom slot at +24 --
    ; PyStrObject.ob_hash -- and XDECREF'd the hash as if it were a pointer.
    push rax
    mov rdi, rax
    call instance_slot_floor
    mov rcx, rax
    pop rax
    test rcx, rcx
    jnz .it_have_hdr
    mov rcx, OBJ_HEADER_SIZE
.it_have_hdr:
    mov r15, [rax + PyTypeObject.tp_dictoffset]
    cmp r15, TP_DICT_AT_TAIL
    jne .it_dict_off_ok
    xor r15d, r15d
.it_dict_off_ok:
    add r15, rbx                ; the dict word's address, or rbx
    mov rax, [rax + PyTypeObject.tp_basicsize]
    sub rax, rcx
    jle .done
    shr rax, 3                  ; nslots
    mov r13, rax
    lea r12, [rbx + rcx]

.slot_loop:
    cmp r12, r15
    je .it_slot_skip            ; the instance dict, visited above
    mov rdi, [r12]
    VISIT_V rdi, rsi
.it_slot_skip:
    add r12, 8
    dec r13
    jnz .slot_loop

.done:
    ; And the slots at the TAIL, which a str subclass's are: past the
    ; characters and past the word the tail __dict__ occupies.  Nothing in
    ; the header walk above can reach them, so a cycle through one was never
    ; collectable.
    mov rax, [rbx + PyObject.ob_type]
    mov r13, [rax + PyTypeObject.tp_tailslots]
    test r13, r13
    jz .it_tail_done
    INST_DICT_TAIL r12, rbx
    add r12, 8                  ; past the dict word
.it_tail_loop:
    mov rdi, [r12]
    VISIT_V rdi, rsi
    add r12, 8
    dec r13
    jnz .it_tail_loop
.it_tail_done:
    pop r15
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC instance_traverse

DEF_FUNC instance_clear, 8            ; 1 pushes, so rsp is 16-aligned
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
