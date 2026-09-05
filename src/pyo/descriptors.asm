; descriptors.asm - staticmethod, classmethod, property descriptor types
;
; staticmethod(func) -> wrapper that prevents method binding
; classmethod(func) -> wrapper that binds class instead of instance
; property(fget[, fset[, fdel]]) -> data descriptor

%include "macros.inc"
%include "object.inc"
extern str_type

extern current_exception
extern kw_names_pending
extern ap_strcmp
extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_incref
extern obj_decref
extern obj_dealloc
extern type_type
extern raise_exception
extern exc_TypeError_type
extern exc_AttributeError_type
extern ap_strcmp
extern obj_call_n
extern method_new
extern builtin_func_new

;; ============================================================================
;; staticmethod_construct(PyObject *type, PyObject **args, int64_t nargs)
;; tp_call for staticmethod_type. Creates a staticmethod wrapper.
;; rdi = staticmethod_type (ignored), rsi = args, rdx = nargs
;; ============================================================================
DEF_FUNC staticmethod_construct, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx

    cmp rdx, 1
    jne .sm_error

    mov rbx, [rsi]              ; rbx = func (args[0])

    ; Allocate wrapper (GC-tracked)
    mov edi, PyStaticMethodObject_size
    lea rsi, [rel staticmethod_type]
    call gc_alloc
    ; ob_refcnt=1, ob_type set
    mov [rax + PyStaticMethodObject.sm_callable], rbx

    ; INCREF func
    push rax
    mov rdi, rbx
    call obj_incref
    mov rdi, [rsp]
    call gc_track
    pop rax

    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.sm_error:
    RAISE exc_TypeError_type, "staticmethod expected 1 argument"
END_FUNC staticmethod_construct

;; ============================================================================
;; staticmethod_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL staticmethod_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyStaticMethodObject.sm_callable]
    call obj_decref

    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC staticmethod_dealloc

;; ============================================================================
;; classmethod_construct(PyObject *type, PyObject **args, int64_t nargs)
;; tp_call for classmethod_type. Creates a classmethod wrapper.
;; rdi = classmethod_type (ignored), rsi = args, rdx = nargs
;; ============================================================================
global classmethod_construct
DEF_FUNC classmethod_construct, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx

    cmp rdx, 1
    jne .cm_error

    mov rbx, [rsi]              ; rbx = func (args[0])

    ; Allocate wrapper (GC-tracked)
    mov edi, PyClassMethodObject_size
    lea rsi, [rel classmethod_type]
    call gc_alloc
    ; ob_refcnt=1, ob_type set
    mov [rax + PyClassMethodObject.cm_callable], rbx

    ; INCREF func
    push rax
    mov rdi, rbx
    call obj_incref
    mov rdi, [rsp]
    call gc_track
    pop rax

    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.cm_error:
    RAISE exc_TypeError_type, "classmethod expected 1 argument"
END_FUNC classmethod_construct

;; ============================================================================
;; classmethod_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL classmethod_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyClassMethodObject.cm_callable]
    call obj_decref

    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC classmethod_dealloc

;; ============================================================================
;; property_construct(PyObject *type, PyObject **args, int64_t nargs)
;; tp_call for property_type. Creates a property descriptor.
;; property(fget) or property(fget, fset) or property(fget, fset, fdel)
;; rdi = property_type (ignored), rsi = args, rdx = nargs
;; ============================================================================
DEF_FUNC property_construct
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rsi                ; args
    mov r12, rdx                ; nargs

    ; The four are positional-or-keyword in CPython, and property(gx,
    ; doc="D") is the common spelling.  Taken positionally, that string
    ; became FSET -- and a property whose setter is a str calls a str on
    ; assignment.  Collect the keywords into the same four slots first, then
    ; let the positionals fill what is left.
    push qword 0                ; [rsp + 24] = doc
    push qword 0                ; [rsp + 16] = fdel
    push qword 0                ; [rsp +  8] = fset
    push qword 0                ; [rsp     ] = fget

    mov rax, [rel kw_names_pending]
    test rax, rax
    jz .pc_positional
    mov r13, [rax + PyTupleObject.ob_size]      ; n_kw
    mov r14, r12
    sub r14, r13                                ; n_pos
    xor ecx, ecx
.pc_kw_loop:
    cmp rcx, r13
    jge .pc_kw_done
    mov rdx, [rax + PyTupleObject.ob_item]
    mov rdx, [rdx + rcx*8]                      ; the keyword's name
    mov rsi, r14
    add rsi, rcx
    mov rsi, [rbx + rsi*8]                      ; the value given for it
    lea rdi, [rdx + PyStrObject.data]

    push rax
    push rcx
    push rsi
    push rdi
    CSTRING rsi, "fget"
    call ap_strcmp
    test eax, eax
    jz .pc_kw_slot0
    mov rdi, [rsp]
    CSTRING rsi, "fset"
    call ap_strcmp
    test eax, eax
    jz .pc_kw_slot1
    mov rdi, [rsp]
    CSTRING rsi, "fdel"
    call ap_strcmp
    test eax, eax
    jz .pc_kw_slot2
    mov rdi, [rsp]
    CSTRING rsi, "doc"
    call ap_strcmp
    test eax, eax
    jnz .pc_kw_bad
    mov edx, 3
    jmp .pc_kw_store
.pc_kw_slot0:
    xor edx, edx
    jmp .pc_kw_store
.pc_kw_slot1:
    mov edx, 1
    jmp .pc_kw_store
.pc_kw_slot2:
    mov edx, 2
.pc_kw_store:
    mov rsi, [rsp + 8]                          ; the value
    mov [rsp + 32 + rdx*8], rsi                 ; past the four pushes
    pop rdi
    pop rsi
    pop rcx
    pop rax
    inc rcx
    jmp .pc_kw_loop
.pc_kw_bad:
    pop rdi
    pop rsi
    pop rcx
    pop rax
    jmp .pc_kw_error
.pc_kw_done:
    mov r12, r14                                ; only the positionals remain
    mov qword [rel kw_names_pending], 0

.pc_positional:
    cmp r12, 4
    ja .pc_error
    xor ecx, ecx
.pc_pos_loop:
    cmp rcx, r12
    jge .pc_pos_done
    mov rax, [rbx + rcx*8]
    cmp qword [rsp + rcx*8], 0
    jne .pc_dup_error
    mov [rsp + rcx*8], rax
    inc rcx
    jmp .pc_pos_loop
.pc_pos_done:

    mov r13, [rsp]              ; fget
    mov r14, [rsp + 8]          ; fset
    test r13, r13
    jnz .pc_alloc
    lea r13, [rel none_singleton]   ; property() with no getter is legal

.pc_alloc:
    ; doc and fdel, kept on the stack because rbx is about to become the new
    ; property and the argument array would be lost with it.
    mov rax, [rsp + 24]
    push rax                    ; doc
    mov rax, [rsp + 24]         ; fdel, now one slot deeper
    push rax

.pc_do_alloc:
    mov edi, PyPropertyObject_size
    lea rsi, [rel property_type]
    call gc_alloc
    mov rbx, rax                ; rbx = new property (ob_refcnt=1, ob_type set)
    mov [rbx + PyPropertyObject.prop_get], r13
    mov [rbx + PyPropertyObject.prop_set], r14
    pop rax                     ; fdel
    mov [rbx + PyPropertyObject.prop_del], rax
    pop rax                     ; doc
    mov [rbx + PyPropertyObject.prop_doc], rax

    ; All four are Values, not pointers.  CPython takes any object for each --
    ; property(f, None, None, 5).__doc__ is 5, and f.__doc__ may be an int --
    ; so an INCREF that dereferences is a segfault on an immediate.
    mov rdi, r13
    INCREF_V rdi, rax

    test r14, r14
    jz .pc_no_fset
    mov rdi, r14
    INCREF_V rdi, rax
.pc_no_fset:

    mov rdi, [rbx + PyPropertyObject.prop_del]
    test rdi, rdi
    jz .pc_no_fdel
    INCREF_V rdi, rax
.pc_no_fdel:

    ; __doc__: the explicit one if given, else fget's own -- which is what
    ; CPython copies, so that help() on a property says something.
    mov rdi, [rbx + PyPropertyObject.prop_doc]
    test rdi, rdi
    jz .pc_doc_from_fget
    INCREF_V rdi, rax
    jmp .pc_doc_done

.pc_doc_from_fget:
    mov rdi, r13
    test rdi, rdi
    jz .pc_doc_done
    V_TEST_PTR rdi, rax
    ja .pc_doc_done
    lea rdi, [rel pc_doc_name]
    extern dunder_name_obj
    call dunder_name_obj
    mov rsi, rax
    mov rdi, r13
    extern obj_getattr_opt
    call obj_getattr_opt
    test rax, rax
    jz .pc_doc_done
    ; obj_getattr_opt hands back a new reference; the property keeps it.
    mov [rbx + PyPropertyObject.prop_doc], rax
.pc_doc_done:

    mov rdi, rbx
    call gc_track

    mov rax, rbx
    mov edx, TAG_PTR

    add rsp, 32                 ; the four collected arguments
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.pc_error:
    RAISE exc_TypeError_type, "property expected 1 to 4 arguments"

.pc_dup_error:
    RAISE exc_TypeError_type, "property() got multiple values for an argument"

.pc_kw_error:
    RAISE exc_TypeError_type, "property() got an unexpected keyword argument"
END_FUNC property_construct

;; ============================================================================
;; property_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL property_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyPropertyObject.prop_get]
    test rdi, rdi
    jz .pd_no_get
    DECREF_V rdi, rcx
.pd_no_get:
    mov rdi, [rbx + PyPropertyObject.prop_set]
    test rdi, rdi
    jz .pd_no_set
    DECREF_V rdi, rcx
.pd_no_set:
    mov rdi, [rbx + PyPropertyObject.prop_del]
    test rdi, rdi
    jz .pd_no_del
    DECREF_V rdi, rcx
.pd_no_del:
    mov rdi, [rbx + PyPropertyObject.prop_doc]
    test rdi, rdi
    jz .pd_no_doc
    DECREF_V rdi, rcx
.pd_no_doc:

    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC property_dealloc

;; ============================================================================
;; property_getattr(PyPropertyObject *self, PyObject *name) -> rax = Value
;; Handles: "setter", "getter", "deleter", "fget", "fset", "fdel"
;; ============================================================================
DEF_FUNC property_getattr
    push rbx
    push r12

    mov rbx, rdi                ; self (property)
    mov r12, rsi                ; name

    ; Compare name against known attributes
    ; name is a PyStrObject — get its data pointer
    lea rdi, [r12 + PyStrObject.data]

    ; Check "setter"
    CSTRING rsi, "setter"
    call ap_strcmp
    test eax, eax
    jz .pga_setter

    ; Check "getter"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "getter"
    call ap_strcmp
    test eax, eax
    jz .pga_getter

    ; Check "deleter"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "deleter"
    call ap_strcmp
    test eax, eax
    jz .pga_deleter

    ; Check "__doc__"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__doc__"
    call ap_strcmp
    test eax, eax
    jz .pga_doc

    ; Check "fget"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "fget"
    call ap_strcmp
    test eax, eax
    jz .pga_fget

    ; Check "fset"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "fset"
    call ap_strcmp
    test eax, eax
    jz .pga_fset

    ; Check "fdel"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "fdel"
    call ap_strcmp
    test eax, eax
    jz .pga_fdel

    ; Not found
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.pga_setter:
    ; Return bound method: method_new(prop_setter_builtin, self)
    call _get_prop_setter_builtin
    mov rdi, rax
    mov rsi, rbx
    call method_new
    jmp .pga_done

.pga_getter:
    call _get_prop_getter_builtin
    mov rdi, rax
    mov rsi, rbx
    call method_new
    jmp .pga_done

.pga_deleter:
    call _get_prop_deleter_builtin
    mov rdi, rax
    mov rsi, rbx
    call method_new
    jmp .pga_done

.pga_fget:
    mov rax, [rbx + PyPropertyObject.prop_get]
    test rax, rax
    jnz .pga_incref_ret
    ; Return None if NULL
    extern none_singleton
    lea rax, [rel none_singleton]
    jmp .pga_incref_ret

.pga_fset:
    mov rax, [rbx + PyPropertyObject.prop_set]
    test rax, rax
    jnz .pga_incref_ret
    lea rax, [rel none_singleton]
    jmp .pga_incref_ret

.pga_fdel:
    mov rax, [rbx + PyPropertyObject.prop_del]
    test rax, rax
    jnz .pga_incref_ret
    lea rax, [rel none_singleton]
    jmp .pga_incref_ret

.pga_doc:
    mov rax, [rbx + PyPropertyObject.prop_doc]
    test rax, rax
    jnz .pga_incref_ret
    lea rax, [rel none_singleton]
    jmp .pga_incref_ret

.pga_incref_ret:
    mov rdi, rax
    push rax
    INCREF_V rdi, rcx
    pop rax

.pga_done:
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC property_getattr

;; ============================================================================
;; Lazy-init helpers for property setter/getter/deleter builtin singletons
;; Each returns a borrowed ref to a cached PyBuiltinObject.
;; ============================================================================

;; _get_prop_setter_builtin() -> PyBuiltinObject* (borrowed)
DEF_FUNC_LOCAL _get_prop_setter_builtin
    mov rax, [rel _prop_setter_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _prop_setter_impl]
    CSTRING rsi, "setter"
    call builtin_func_new
    mov [rel _prop_setter_cache], rax
.ret:
    leave
    ret
END_FUNC _get_prop_setter_builtin

;; _get_prop_getter_builtin() -> PyBuiltinObject* (borrowed)
DEF_FUNC_LOCAL _get_prop_getter_builtin
    mov rax, [rel _prop_getter_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _prop_getter_impl]
    CSTRING rsi, "getter"
    call builtin_func_new
    mov [rel _prop_getter_cache], rax
.ret:
    leave
    ret
END_FUNC _get_prop_getter_builtin

;; _get_prop_deleter_builtin() -> PyBuiltinObject* (borrowed)
DEF_FUNC_LOCAL _get_prop_deleter_builtin
    mov rax, [rel _prop_deleter_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _prop_deleter_impl]
    CSTRING rsi, "deleter"
    call builtin_func_new
    mov [rel _prop_deleter_cache], rax
.ret:
    leave
    ret
END_FUNC _get_prop_deleter_builtin

;; ============================================================================
;; _prop_setter_impl(args, nargs) — property.setter(func)
;; args[0] = property (self from bound method), args[1] = func
;; Returns new property with same fget/fdel, new fset
;; ============================================================================
DEF_FUNC _prop_setter_impl, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx

    ; args[0] = property, args[1] = new fset
    cmp rsi, 2
    jne .psi_error

    mov rbx, rdi                ; args
    mov r8, [rbx]               ; old property
    mov r9, [rbx + 8]          ; new fset

    ; Build args for property_construct: (fget, fset, fdel)
    sub rsp, 32                 ; three Values, rsp stays aligned
    mov rax, [r8 + PyPropertyObject.prop_get]
    mov [rsp], rax              ; args[0] = fget
    mov [rsp + 8], r9           ; args[1] = new fset
    mov rax, [r8 + PyPropertyObject.prop_del]
    mov [rsp + 16], rax         ; args[2] = fdel

    xor edi, edi                ; type (ignored)
    mov rsi, rsp                ; args
    mov edx, 3                  ; nargs
    ; Check if fdel is NULL — if so, pass 2 args
    cmp qword [rsp + 16], 0
    jne .psi_call
    mov edx, 2
.psi_call:
    call property_construct
    add rsp, 32

    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.psi_error:
    RAISE exc_TypeError_type, "setter expected 1 argument"
END_FUNC _prop_setter_impl

;; ============================================================================
;; _prop_getter_impl(args, nargs) — property.getter(func)
;; Returns new property with new fget, same fset/fdel
;; ============================================================================
DEF_FUNC _prop_getter_impl, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx

    cmp rsi, 2
    jne .pgi_error

    mov rbx, rdi
    mov r8, [rbx]               ; old property
    mov r9, [rbx + 8]          ; new fget

    sub rsp, 32                 ; three Values, rsp stays aligned
    mov [rsp], r9               ; args[0] = new fget
    mov rax, [r8 + PyPropertyObject.prop_set]
    mov [rsp + 8], rax          ; args[1] = fset
    mov rax, [r8 + PyPropertyObject.prop_del]
    mov [rsp + 16], rax         ; args[2] = fdel

    xor edi, edi
    mov rsi, rsp
    mov edx, 3
    cmp qword [rsp + 16], 0
    jne .pgi_call
    mov edx, 2
    cmp qword [rsp + 8], 0
    jne .pgi_call
    mov edx, 1
.pgi_call:
    call property_construct
    add rsp, 32

    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pgi_error:
    RAISE exc_TypeError_type, "getter expected 1 argument"
END_FUNC _prop_getter_impl

;; ============================================================================
;; _prop_deleter_impl(args, nargs) — property.deleter(func)
;; Returns new property with same fget/fset, new fdel
;; ============================================================================
DEF_FUNC _prop_deleter_impl, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx

    cmp rsi, 2
    jne .pdi_error

    mov rbx, rdi
    mov r8, [rbx]               ; old property
    mov r9, [rbx + 8]          ; new fdel

    sub rsp, 32                 ; three Values, rsp stays aligned
    mov rax, [r8 + PyPropertyObject.prop_get]
    mov [rsp], rax              ; args[0] = fget
    mov rax, [r8 + PyPropertyObject.prop_set]
    mov [rsp + 8], rax          ; args[1] = fset
    mov [rsp + 16], r9          ; args[2] = new fdel

    xor edi, edi
    mov rsi, rsp
    mov edx, 3
    call property_construct
    add rsp, 32

    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pdi_error:
    RAISE exc_TypeError_type, "deleter expected 1 argument"
END_FUNC _prop_deleter_impl

;; ============================================================================
;; property_descr_get(PyPropertyObject *prop, PyObject *obj) -> PyObject*
;; Called by LOAD_ATTR when a property is found in the type dict.
;; Invokes prop.fget(obj). Returns result (owned ref).
;; ============================================================================
DEF_FUNC property_descr_get
    push rbx
    push r12

    mov rbx, rdi                ; property
    mov r12, rsi                ; obj

    mov rax, [rbx + PyPropertyObject.prop_get]
    test rax, rax
    jz .pdg_no_getter

    ; fget(obj), through the general call path.  Reaching for tp_call directly
    ; missed a getter that is an instance of a class defining __call__ -- an
    ; operator.itemgetter, say, which is exactly what collections.namedtuple
    ; builds its fields from -- and reported it as an unreadable attribute.
    SPUSH_PTR r12               ; args[0] = obj
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call obj_call_n
    add rsp, 16
    V_UNPACK rax, rdx           ; the pair the callers of this expect

    pop r12
    pop rbx
    leave
    ret

.pdg_no_getter:
    RAISE exc_AttributeError_type, "unreadable attribute"
END_FUNC property_descr_get

;; ============================================================================
;; property_descr_set(PyPropertyObject *prop, PyObject *obj, PyObject *value) -> void
;; Called by STORE_ATTR when a property is found in the type dict.
;; Invokes prop.fset(obj, value).
;; ============================================================================
DEF_FUNC property_descr_set
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; property
    mov r12, rsi                ; obj
    mov r13, rdx                ; value Value

    ; A NULL value is `del obj.attr`, and that is the DELETER's business.
    ; Without this arm it reached fset with a NULL Value, so `del d.v` ran the
    ; setter with nothing and the deleter never ran at all.
    test r13, r13
    jz .pds_delete

    mov rax, [rbx + PyPropertyObject.prop_set]
    test rax, rax
    jz .pds_no_setter

    ; Call fset(obj, value): fset.tp_call(fset, args, 2)
    mov rdi, rax
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .pds_no_setter

    ; Build the args array on the stack: [obj, value]
    sub rsp, 16
    mov [rsp], r12              ; args[0] = obj
    mov [rsp + 8], r13          ; args[1] = value (already a Value)
    mov rsi, rsp                ; args ptr
    mov edx, 2                  ; nargs = 2
    call rax
    V_UNPACK rax, rdx           ; tp_call returns a Value
    add rsp, 16                 ; pop args

    ; DECREF result (fset returns None typically)
    DECREF_VAL rax, edx

.pds_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.pds_delete:
    mov rax, [rbx + PyPropertyObject.prop_del]
    test rax, rax
    jz .pds_no_deleter
    mov rdi, rax
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .pds_no_deleter
    sub rsp, 16
    mov [rsp], r12              ; args[0] = obj
    mov rsi, rsp
    mov edx, 1
    call rax
    V_UNPACK rax, rdx
    add rsp, 16
    DECREF_VAL rax, edx
    jmp .pds_done

.pds_no_deleter:
    RAISE exc_AttributeError_type, "can't delete attribute"
.pds_no_setter:
    RAISE exc_AttributeError_type, "can't set attribute"
END_FUNC property_descr_set

;; ============================================================================
;; member_descr_new(i64 offset, PyStrObject *name) -> PyMemberDescrObject*
;; Create a member descriptor for a __slots__ slot.
;; rdi = byte offset in instance, rsi = slot name (INCREF'd, ownership taken),
;; rdx = the class it belongs to, borrowed -- the type owns the dict that owns
;; this, and only the repr reads it.
;; ============================================================================
DEF_FUNC member_descr_new, 8            ; 3 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; offset
    mov r12, rsi            ; name str
    mov r13, rdx            ; the owning class

    mov edi, PyMemberDescrObject_size
    call ap_malloc

    mov qword [rax + PyMemberDescrObject.ob_refcnt], 1
    lea rcx, [rel member_descr_type]
    mov [rax + PyMemberDescrObject.ob_type], rcx
    mov [rax + PyMemberDescrObject.md_offset], rbx
    mov [rax + PyMemberDescrObject.md_name], r12
    mov [rax + PyMemberDescrObject.md_owner], r13

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC member_descr_new

;; ============================================================================
;; member_descr_repr(rdi = the descriptor) -> "<member 'x' of 'S' objects>"
;;
;; CPython's wording, and the same shape a method descriptor's repr has.  It
;; used to fall through to object's, which prints the type and an address and
;; says neither which slot nor whose.
;; ============================================================================
MDR_BUF   equ 272
MDR_FRAME equ 296            ; + 1 push = 304, 16-aligned
DEF_FUNC_LOCAL member_descr_repr, MDR_FRAME
    push rbx
    mov rbx, rdi
    lea rdi, [rbp - MDR_BUF]
    CSTRING rsi, "<member '"
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbx + PyMemberDescrObject.md_name]
    test rsi, rsi
    jz .mdr_no_name
    add rsi, PyStrObject.data
    call rbt_append_cstr
.mdr_no_name:
    mov rdi, rax
    CSTRING rsi, "' of '"
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbx + PyMemberDescrObject.md_owner]
    test rsi, rsi
    jz .mdr_no_owner
    mov rsi, [rsi + PyTypeObject.tp_name]
    call rbt_append_cstr
.mdr_no_owner:
    mov rdi, rax
    CSTRING rsi, "' objects>"
    call rbt_append_cstr
    lea rdi, [rbp - MDR_BUF]
    extern str_from_cstr
    call str_from_cstr
    pop rbx
    leave
    ret
END_FUNC member_descr_repr

;; member_descr_dealloc(PyMemberDescrObject *self)
DEF_FUNC member_descr_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    ; DECREF name string
    mov rdi, [rbx + PyMemberDescrObject.md_name]
    test rdi, rdi
    jz .md_no_name
    call obj_decref
.md_no_name:

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC member_descr_dealloc

;; ============================================================================
;; Data section
;; ============================================================================
;; ============================================================================
;; mappingproxy -- a read-only window onto a dict.
;;
;; type.__dict__ is one in CPython, and types.py does
;; `MappingProxyType = type(type.__dict__)`, so the type has to exist and be
;; distinct from dict before anything downstream of types.py can import.
;; Reads delegate to the wrapped dict; there is no write path.
;; ============================================================================
extern dict_get
extern str_new_heap
extern obj_repr
extern obj_incref
;; ============================================================================
;; mappingproxy_construct(PyObject *type, PyObject **args, int64_t nargs)
;; tp_new for mappingproxy_type: MappingProxyType(mapping).
;;
;; The type existed only to be *named* by types.py, so it had no constructor at
;; all -- and calling it fell through to the ordinary class-construction path,
;; which allocated a proxy-sized block and left mp_mapping holding whatever was
;; there.  enum's `__members__` is a MappingProxyType(...) call.  It goes in
;; tp_new, not tp_call: tp_call on a type is what makes that type's *instances*
;; callable.
;; ============================================================================
DEF_FUNC mappingproxy_construct
    cmp rdx, 1
    jne .mpc_error
    mov rdi, [rsi]                  ; the mapping
    V_TEST_PTR rdi, rax
    ja .mpc_error
    test rdi, rdi
    jz .mpc_error
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel dict_type]
    cmp rax, rcx
    je .mpc_wrap
    ; A proxy of a proxy wraps the same dict, as CPython's does.
    lea rcx, [rel mappingproxy_type]
    cmp rax, rcx
    jne .mpc_error
    mov rdi, [rdi + PyMappingProxyObject.mp_mapping]
.mpc_wrap:
    call mappingproxy_new
    mov edx, TAG_PTR            ; a constructor returns the (payload, tag) pair
    leave
    ret
.mpc_error:
    RAISE exc_TypeError_type, "mappingproxy() argument must be a mapping, not a sequence"
END_FUNC mappingproxy_construct

DEF_FUNC mappingproxy_new, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi                    ; the dict
    mov edi, PyMappingProxyObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel mappingproxy_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyMappingProxyObject.mp_mapping], rbx
    push rax
    mov rdi, rbx
    call obj_incref
    pop rax
    pop rbx
    leave
    ret
END_FUNC mappingproxy_new

DEF_FUNC_LOCAL mappingproxy_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyMappingProxyObject.mp_mapping]
    test rdi, rdi
    jz .mpd_free
    call obj_decref
.mpd_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC mappingproxy_dealloc

; Everything below just forwards to the wrapped dict.
DEF_FUNC_BARE mappingproxy_subscript
    mov rdi, [rdi + PyMappingProxyObject.mp_mapping]
    extern dict_subscript
    jmp dict_subscript
END_FUNC mappingproxy_subscript

DEF_FUNC_BARE mappingproxy_len
    mov rdi, [rdi + PyMappingProxyObject.mp_mapping]
    extern dict_len
    jmp dict_len
END_FUNC mappingproxy_len

DEF_FUNC_BARE mappingproxy_contains
    mov rdi, [rdi + PyMappingProxyObject.mp_mapping]
    extern dict_contains
    jmp dict_contains
END_FUNC mappingproxy_contains

DEF_FUNC_BARE mappingproxy_iter
    mov rdi, [rdi + PyMappingProxyObject.mp_mapping]
    extern dict_tp_iter
    jmp dict_tp_iter
END_FUNC mappingproxy_iter

DEF_FUNC_BARE mappingproxy_repr
    mov rdi, [rdi + PyMappingProxyObject.mp_mapping]
    extern dict_repr
    jmp dict_repr
END_FUNC mappingproxy_repr

MPG_MAP   equ 8
MPG_NAME  equ 16
MPG_PTR   equ 24
MPG_FRAME equ 32            ; + 0 pushes = 32
DEF_FUNC mappingproxy_getattr, MPG_FRAME
    ; A proxy is read-only, so the methods that would write through it are
    ; refused before the wrapped dict is consulted.  Without this, giving dict
    ; a __setitem__ method made `C.__dict__.__setitem__(k, v)` quietly mutate
    ; the class.
    mov [rbp - MPG_MAP], rdi
    mov [rbp - MPG_NAME], rsi
    lea rax, [rel mpg_readonly_names]
    mov [rbp - MPG_PTR], rax
.mpg_deny_loop:
    mov rax, [rbp - MPG_PTR]
    mov rsi, [rax]
    test rsi, rsi
    jz .mpg_allowed
    add qword [rbp - MPG_PTR], 8
    mov rdi, [rbp - MPG_NAME]
    add rdi, PyStrObject.data
    extern ap_strcmp
    call ap_strcmp
    test eax, eax
    jnz .mpg_deny_loop
    xor eax, eax
    xor edx, edx
    leave
    ret

.mpg_allowed:
    mov rdi, [rbp - MPG_MAP]
    mov rsi, [rbp - MPG_NAME]
    ; keys/values/items/get and the rest live on the wrapped dict
    mov rdi, [rdi + PyMappingProxyObject.mp_mapping]
    extern dict_type
    mov rax, [rel dict_type + PyTypeObject.tp_dict]
    test rax, rax
    jz .mpg_none
    push rdi
    mov rdi, rax
    call dict_get
    V_UNPACK rax, rdx
    pop rdi
    test edx, edx
    jz .mpg_none
    ; bind it to the wrapped dict, so proxy.keys() reads the dict
    extern method_new
    mov rsi, rdi
    mov rdi, rax
    call method_new
    mov edx, TAG_PTR
    leave
    ret
.mpg_none:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC mappingproxy_getattr


;; ============================================================================
;; getset_descr_new(rdi = getter, rsi = setter, rdx = name str) -> descriptor
;; A named pair of C accessors, stored in a type's dict.  This is how CPython
;; exposes func.__code__ and the co_* fields, and types.py takes
;; GetSetDescriptorType straight off one of them.
;; ============================================================================
DEF_FUNC getset_descr_new, 8            ; 3 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov edi, PyGetSetDescrObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel getset_descr_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyGetSetDescrObject.gs_get], rbx
    mov [rax + PyGetSetDescrObject.gs_set], r12
    mov [rax + PyGetSetDescrObject.gs_name], r13
    mov qword [rax + PyGetSetDescrObject.gs_owner], 0
    ; gs_name is owned -- getset_descr_dealloc decrefs it -- and was stored
    ; without a reference of its own.  Harmless while the one instance ever
    ; built was immortal; not once every numeric type registers several.
    push rax
    mov rdi, r13
    call obj_incref
    pop rax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC getset_descr_new

;; ============================================================================
;; getset_descr_getattr(rdi = the descriptor, rsi = name str) -> Value, or 0
;;
;; __name__, __qualname__ and __objclass__, which is what a descriptor is
;; asked for once it has been fished out of a type's dict by name.  inspect's
;; _shadowed_dict reads `class_dict.__name__ == "__dict__"` off exactly this
;; object, and an AttributeError there stopped pydoc, pstats and everything
;; through them.
;; ============================================================================
GDA_SELF  equ 8
GDA_FRAME equ 16            ; + 0 pushes = 16
DEF_FUNC_LOCAL getset_descr_getattr, GDA_FRAME
    mov [rbp - GDA_SELF], rdi
    test rsi, rsi
    jz .gda_miss
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .gda_miss

    lea rdi, [rsi + PyStrObject.data]
    push rsi
    CSTRING rsi, "__name__"
    call ap_strcmp
    pop rsi
    test eax, eax
    jz .gda_name

    lea rdi, [rsi + PyStrObject.data]
    push rsi
    CSTRING rsi, "__qualname__"
    call ap_strcmp
    pop rsi
    test eax, eax
    jz .gda_qualname

    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__objclass__"
    call ap_strcmp
    test eax, eax
    jz .gda_objclass
.gda_miss:
    xor eax, eax
    xor edx, edx
    leave
    ret

.gda_name:
    mov rax, [rbp - GDA_SELF]
    mov rax, [rax + PyGetSetDescrObject.gs_name]
    test rax, rax
    jz .gda_miss
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.gda_qualname:
    ; "type.__mro__": the owning type's name, then the attribute's.
    mov rax, [rbp - GDA_SELF]
    mov rcx, [rax + PyGetSetDescrObject.gs_owner]
    test rcx, rcx
    jz .gda_name
    mov rax, [rax + PyGetSetDescrObject.gs_name]
    test rax, rax
    jz .gda_miss
    lea rdi, [rel gda_buf]
    mov rsi, [rcx + PyTypeObject.tp_name]
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, "."
    call rbt_append_cstr
    mov rdi, rax
    mov rcx, [rbp - GDA_SELF]
    mov rsi, [rcx + PyGetSetDescrObject.gs_name]
    lea rsi, [rsi + PyStrObject.data]
    call rbt_append_cstr
    lea rdi, [rel gda_buf]
    call str_from_cstr
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.gda_objclass:
    mov rax, [rbp - GDA_SELF]
    mov rax, [rax + PyGetSetDescrObject.gs_owner]
    test rax, rax
    jz .gda_miss
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC getset_descr_getattr

section .bss
gda_buf: resb 320
extern bool_false
extern bool_true
extern none_type
extern obj_hash
extern union_getattr
extern union_hash
extern union_repr
extern union_richcompare
extern union_type_or
section .text

;; ============================================================================
;; getset_descr_repr(rdi = the descriptor) -> PyStrObject*
;;
;; "<attribute 'real' of 'int' objects>", as CPython words it.  tp_repr was 0,
;; so obj_repr fell to its no-repr arm and repr(int.real) was the empty
;; string -- and the stdlib reads a descriptor's repr to classify it.
;; ============================================================================
global getset_descr_repr
extern rbt_append_cstr
extern str_from_cstr
DEF_FUNC getset_descr_repr
    mov rsi, rdi
    lea rdi, [rel gdr_open]
    xor edx, edx
    call getset_descr_compose
    mov rdi, rax                ; the NUL the composer left
    lea rsi, [rel gdr_close]
    call rbt_append_cstr
    lea rdi, [rel gdr_buf]
    call str_from_cstr
    leave
    ret
END_FUNC getset_descr_repr

;; ============================================================================
;; getset_descr_dunder_get(args, nargs) -- descriptor.__get__(obj[, type])
;; getset_descr_dunder_set(args, nargs) -- descriptor.__set__(obj, value)
;;
;; The stdlib decides what a descriptor IS by asking for these by name:
;; inspect.isdatadescriptor, and the enum and dataclasses classifiers, walk a
;; __dict__ and test hasattr(v, '__get__').  With no tp_dict on the type, the
;; answer was False for every getset in the tree.
;; ============================================================================
global getset_descr_dunder_get
DEF_FUNC getset_descr_dunder_get
    cmp rsi, 2
    jl .gdg_bad
    cmp rsi, 3
    jg .gdg_bad
    mov rax, [rdi]              ; args[0] = the descriptor
    mov rsi, [rdi + 8]          ; args[1] = the instance
    ; descr.__get__(None, cls) answers the descriptor itself, as CPython's does
    IS_NONE rsi, rcx
    je .gdg_self
    mov rdi, rax
    call getset_descr_get
    leave
    ret
.gdg_self:
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gdg_bad:
    RAISE exc_TypeError_type, "expected 1 or 2 arguments"
END_FUNC getset_descr_dunder_get

global getset_descr_dunder_set
DEF_FUNC getset_descr_dunder_set
    cmp rsi, 3
    jne .gds_bad
    mov rax, [rdi]
    mov rsi, [rdi + 8]
    mov rdx, [rdi + 16]
    mov rdi, rax
    call getset_descr_set
    extern none_singleton
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gds_bad:
    RAISE exc_TypeError_type, "expected 2 arguments"
END_FUNC getset_descr_dunder_set

;; getset_descr_dunder_delete(args, nargs) -- descriptor.__delete__(obj)
;; A getset is a DATA descriptor whether or not it has a setter, so all three
;; names exist; a read-only one raises when either is called.
global getset_descr_dunder_delete
DEF_FUNC getset_descr_dunder_delete
    cmp rsi, 2
    jne .gdd_bad
    mov rax, [rdi]
    mov rsi, [rdi + 8]
    xor edx, edx                ; a NULL Value: delete rather than assign
    mov rdi, rax
    call getset_descr_set
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gdd_bad:
    RAISE exc_TypeError_type, "expected 1 argument"
END_FUNC getset_descr_dunder_delete

;; ============================================================================
;; getset_descr_compose(rdi = a leading C string, rsi = the descriptor)
;;   -> rax = the address of the NUL in gdr_buf, which holds
;;      "<lead>'name' of 'T' objects" -- a caller appends its own ending
;;
;; The middle the repr and the not-writable message share.
;; ============================================================================
DEF_FUNC_LOCAL getset_descr_compose, 8            ; 3 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    mov rbx, rsi
    mov r12, rdi
    mov r13, rdx                ; 1 = say "for" where the others say "of"
    lea rdi, [rel gdr_buf]
    mov rsi, r12
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbx + PyGetSetDescrObject.gs_name]
    test rsi, rsi
    jz .gdc_no_name
    add rsi, PyStrObject.data
    jmp .gdc_have_name
.gdc_no_name:
    lea rsi, [rel gdr_unknown]
.gdc_have_name:
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel gdr_of]
    test r13, r13
    jz .gdc_joiner
    lea rsi, [rel gdr_for]
.gdc_joiner:
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbx + PyGetSetDescrObject.gs_owner]
    test rsi, rsi
    jz .gdc_no_owner
    mov rsi, [rsi + PyTypeObject.tp_name]
    jmp .gdc_have_owner
.gdc_no_owner:
    lea rsi, [rel gdr_unknown]
.gdc_have_owner:
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel gdr_tail]
    call rbt_append_cstr        ; rax = the NUL, where a caller appends more
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC getset_descr_compose

section .rodata
gds_ro_open: db "attribute '", 0
gds_ro_tail: db " is not writable", 0
gdr_tail:    db "' objects", 0
section .bss
gdr_buf: resb 320
section .text

section .rodata
gdr_open:    db "<attribute '", 0
gdr_of:      db "' of '", 0
gdr_for:     db "' for '", 0
gdr_close:   db ">", 0
gdr_unknown: db "?", 0
section .text

;; ============================================================================
;; getset_descr_get(rdi = the descriptor, rsi = self Value) -> rax = Value
;;
;; Calls the C getter.  Until now nothing anywhere read gs_get: the type was a
;; stub built once so types.py could name GetSetDescriptorType, and .real and
;; .imag were four separate tp_getattr strcmp chains instead -- which answered
;; an instance and left `int.real` an AttributeError, because a chain is not a
;; thing a type's dict can hold.
;;
;; A NULL getter is a set-only attribute, which CPython reports as an
;; AttributeError naming it.
;; ============================================================================
GDG_DESC  equ 8
GDG_SELF  equ 16
GDG_FRAME equ 32            ; + 0 pushes = 32
DEF_FUNC getset_descr_get, GDG_FRAME
    mov [rbp - GDG_DESC], rdi
    mov [rbp - GDG_SELF], rsi
    call getset_check_receiver  ; clobbers both argument registers
    mov rdi, [rbp - GDG_DESC]
    mov rsi, [rbp - GDG_SELF]
    mov rax, [rdi + PyGetSetDescrObject.gs_get]
    test rax, rax
    jz .gdg_unreadable
    mov rdi, rsi
    call rax
    leave
    ret
.gdg_unreadable:
    RAISE exc_AttributeError_type, "attribute is not readable"
END_FUNC getset_descr_get

;; ============================================================================
;; getset_check_receiver(rdi = the descriptor, rsi = self Value) -- returns,
;; or raises TypeError naming both types
;;
;; The getters are C functions that dereference their argument as an instance
;; of the type the descriptor belongs to, so reaching one UNBOUND --
;; `slice.__dict__['start'].__get__(5)` -- handed an int immediate to
;; `mov rax, [rdi + PySliceObject.start]`.  builtin_func_call has had this
;; check for its own descriptors since they started carrying an owner; a
;; getset carries one too.
;; ============================================================================
GCR_DESC  equ 8
GCR_SELF  equ 16
GCR_FRAME equ 32            ; + 0 pushes = 32
DEF_FUNC_LOCAL getset_check_receiver, GCR_FRAME
    mov [rbp - GCR_DESC], rdi
    mov [rbp - GCR_SELF], rsi
    mov rax, [rdi + PyGetSetDescrObject.gs_owner]
    test rax, rax
    jz .gcr_ok                  ; no owner recorded: nothing to check against
    mov rdi, rsi
    extern value_type
    call value_type
    test rax, rax
    jz .gcr_bad
    mov rdi, rax
    mov rcx, [rbp - GCR_DESC]
    mov rsi, [rcx + PyGetSetDescrObject.gs_owner]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jz .gcr_bad
.gcr_ok:
    leave
    ret
.gcr_bad:
    ; CPython's wording for this one is "for 'T' objects", where the readonly
    ; and repr messages say "of".  getset_descr_compose ends with the shared
    ; tail, so the joining word is passed in.
    lea rdi, [rel gcr_open]
    mov rsi, [rbp - GCR_DESC]
    mov rdx, 1                  ; "for", not "of"
    call getset_descr_compose
    mov rdi, rax
    lea rsi, [rel gcr_mid]
    call rbt_append_cstr
    ; The receiver's type name, worked out BEFORE the append point goes into
    ; rdi -- value_type takes rdi as well.
    push rax                    ; the append point
    sub rsp, 8
    mov rdi, [rbp - GCR_SELF]
    call value_type
    add rsp, 8
    pop rdi                     ; the append point
    test rax, rax
    jz .gcr_unknown
    mov rsi, [rax + PyTypeObject.tp_name]
    jmp .gcr_have
.gcr_unknown:
    lea rsi, [rel gdr_unknown]
.gcr_have:
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel gcr_tail]
    call rbt_append_cstr
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel gdr_buf]
    call raise_exception        ; does not return
END_FUNC getset_check_receiver

section .rodata
gcr_open: db "descriptor '", 0
gcr_mid:  db " doesn't apply to a '", 0
gcr_tail: db "' object", 0
section .text

;; ============================================================================
;; getset_descr_set(rdi = the descriptor, rsi = self Value, rdx = value Value)
;;   -> eax = 0, or never returns
;;
;; A NULL setter is a read-only attribute.  Every getset the tree registers
;; today has one, which is what makes `(5).real = 1` an AttributeError rather
;; than a silent instance attribute on a subclass.
;; ============================================================================
GDS_SELF  equ 8             ; the descriptor
GDS_RECV  equ 16
GDS_VALUE equ 24
GDS_FRAME equ 32            ; + 0 pushes = 32
DEF_FUNC getset_descr_set, GDS_FRAME
    mov [rbp - GDS_SELF], rdi
    mov [rbp - GDS_RECV], rsi
    mov [rbp - GDS_VALUE], rdx
    call getset_check_receiver  ; the setter dereferences it too
    mov rdi, [rbp - GDS_SELF]
    mov rsi, [rbp - GDS_RECV]
    mov rdx, [rbp - GDS_VALUE]
    mov rax, [rdi + PyGetSetDescrObject.gs_set]
    test rax, rax
    jz .gds_readonly
    mov rdi, rsi
    mov rsi, rdx
    call rax
    leave
    ret
.gds_readonly:
    ; Now that the descriptor carries its name and its owner, it can say
    ; which attribute -- which is what CPython's message does.
    lea rdi, [rel gds_ro_open]
    mov rsi, [rbp - GDS_SELF]
    xor edx, edx
    call getset_descr_compose
    mov rdi, rax
    lea rsi, [rel gds_ro_tail]
    call rbt_append_cstr
    lea rdi, [rel exc_AttributeError_type]
    lea rsi, [rel gdr_buf]
    call raise_exception
    ud2
END_FUNC getset_descr_set

DEF_FUNC_LOCAL getset_descr_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyGetSetDescrObject.gs_name]
    test rdi, rdi
    jz .gsd_free
    call obj_decref
.gsd_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC getset_descr_dealloc


;; ============================================================================
;; types.GenericAlias -- what `list[int]` evaluates to (PEP 585).
;;
;; _collections_abc does `GenericAlias = type(list[int])` at import time, and
;; every stdlib module carrying annotations reaches it eventually.  It is a
;; two-field record: the origin type and the argument.  It is callable, so
;; `list[int]()` still builds a list.
;; ============================================================================
DEF_FUNC generic_alias_new
    push rbx
    push r12
    mov rbx, rdi                ; origin
    mov r12, rsi                ; args
    mov edi, PyGenericAliasObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel generic_alias_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyGenericAliasObject.ga_origin], rbx
    mov [rax + PyGenericAliasObject.ga_args], r12
    mov qword [rax + PyGenericAliasObject.ga_starred], 0
    push rax
    mov rdi, rbx
    call obj_incref
    ; ga_args is a VALUE, not a pointer: `list[0]` puts an int immediate here
    ; and obj_incref on one writes through the number.
    mov rax, r12
    INCREF_V rax, rcx
.gan_done:
    pop rax
    pop r12
    pop rbx
    leave
    ret
END_FUNC generic_alias_new

DEF_FUNC_LOCAL generic_alias_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyGenericAliasObject.ga_origin]
    test rdi, rdi
    jz .gad_args
    call obj_decref
.gad_args:
    mov rax, [rbx + PyGenericAliasObject.ga_args]
    test rax, rax
    jz .gad_free
    DECREF_V rax, rcx
.gad_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC generic_alias_dealloc

;; ============================================================================
;; generic_alias_construct(rdi = type, rsi = args, rdx = nargs)
;;   -> rax = a new GenericAlias, rdx = TAG_PTR
;;
;; tp_new for generic_alias_type: types.GenericAlias(origin, args).
;;
;; The type had no constructor, so `GenericAlias(list, str)` fell through to
;; the ordinary class-construction path: it allocated a GC-headed block, left
;; ga_origin and ga_args holding whatever was there, and then freed it with
;; this type's tp_dealloc -- a plain free at the object address rather than at
;; the GC head, which glibc reports as a double free.  os.PathLike is written
;; `__class_getitem__ = classmethod(GenericAlias)`, so `os.PathLike[str]` was
;; the crash; importlib.resources is one line of it.
;; ============================================================================
;; ============================================================================
;; generic_alias_iter(rdi = the alias) -> rax = an iterator over one item, the
;;   same alias with its star set
;;
;; PEP 646 compiles `*tuple[int, str]` in an annotation as the expression
;; followed by UNPACK_SEQUENCE 1, so the value that reaches __annotations__ is
;; whatever iterating the alias yields once.  CPython's is the alias again
;; with __unpacked__ True; without a tp_iter at all the unpack said
;; "'types.GenericAlias' object is not iterable".
;;
;; A one-item tuple and its own iterator, rather than a new iterator type:
;; there is exactly one item and it is built here.
;; ============================================================================
GAI_TUP   equ 8
GAI_FRAME equ 16            ; + 0 pushes = 16-aligned
DEF_FUNC generic_alias_iter, GAI_FRAME
    mov rsi, [rdi + PyGenericAliasObject.ga_args]
    mov rdi, [rdi + PyGenericAliasObject.ga_origin]
    call generic_alias_new
    test rax, rax
    jz .gai_fail
    mov qword [rax + PyGenericAliasObject.ga_starred], 1
    mov [rbp - GAI_TUP], rax
    mov edi, 1
    extern tuple_new
    call tuple_new
    test rax, rax
    jz .gai_fail
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rdx, [rbp - GAI_TUP]
    mov [rcx], rdx              ; the tuple takes the reference
    mov [rbp - GAI_TUP], rax
    mov rdi, rax
    mov esi, TAG_PTR
    extern get_iterator
    call get_iterator
    push rax
    mov rdi, [rbp - GAI_TUP]
    call obj_decref             ; the iterator holds it now
    pop rax
    leave
    ret
.gai_fail:
    xor eax, eax
    leave
    ret
END_FUNC generic_alias_iter

DEF_FUNC generic_alias_construct
    cmp rdx, 2
    jne .gac_error
    mov rdi, [rsi]              ; the origin
    mov rsi, [rsi + 8]          ; the argument, whatever it is
    V_TEST_PTR rdi, rax
    ja .gac_error
    test rdi, rdi
    jz .gac_error
    call generic_alias_new
    mov edx, TAG_PTR            ; a constructor returns the (payload, tag) pair
    leave
    ret
.gac_error:
    RAISE exc_TypeError_type, "GenericAlias expected 2 arguments"
END_FUNC generic_alias_construct

;; The builtin registered as __class_getitem__ on each container type.
;; args[0] = cls, args[1] = the subscript.
DEF_FUNC generic_alias_class_getitem
    cmp rsi, 2
    jl .gacg_bad
    mov rax, [rdi + 8]
    mov rdi, [rdi]
    mov rsi, rax
    call generic_alias_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gacg_bad:
    RAISE exc_TypeError_type, "__class_getitem__() takes exactly one argument"
END_FUNC generic_alias_class_getitem

;; repr: "list[int]" -- origin name, then the argument's repr.
GAR_BUF   equ 264
GAR_SELF  equ 272
GAR_FRAME equ 296            ; + 5 pushes = 336, 16-aligned
DEF_FUNC generic_alias_repr, GAR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov [rbp - GAR_SELF], rdi
    lea rbx, [rbp - GAR_BUF]
    xor r13d, r13d

    ; An UNPACKED alias reprs with a leading star: `*tuple[int, str]` is what
    ; PEP 646 writes and what CPython prints back.
    cmp qword [rdi + PyGenericAliasObject.ga_starred], 0
    je .gar_no_star
    mov byte [rbx], '*'
    mov r13d, 1
.gar_no_star:

    mov rax, [rdi + PyGenericAliasObject.ga_origin]
    test rax, rax
    jz .gar_open
    mov rsi, [rax + PyObject.ob_type]
    test rsi, rsi
    jz .gar_open
    test qword [rsi + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .gar_open
    mov rdi, rax
    call .gar_qualified

.gar_open:
    mov byte [rbx + r13], '['
    inc r13
    mov rdi, [rbp - GAR_SELF]
    mov rdi, [rdi + PyGenericAliasObject.ga_args]
    test rdi, rdi
    jz .gar_close
    ; A tuple argument prints comma-joined without its parentheses, and a
    ; type prints as its name: list[int], not list[<class 'int'>].
    extern tuple_type
    V_TEST_PTR rdi, rax         ; classify before reading ob_type: the
    ja .gar_one                 ; argument may be a number
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    jne .gar_one
    mov r14, [rdi + PyTupleObject.ob_size]
    mov r15, [rdi + PyTupleObject.ob_item]
    xor r12d, r12d
.gar_tuple_loop:
    cmp r12, r14
    jge .gar_close
    test r12, r12
    jz .gar_no_comma
    mov byte [rbx + r13], ','
    mov byte [rbx + r13 + 1], ' '
    add r13, 2
.gar_no_comma:
    mov rdi, [r15 + r12*8]
    call .gar_emit_one
    inc r12
    jmp .gar_tuple_loop

.gar_one:
    call .gar_emit_one
    jmp .gar_close

;; .gar_emit_one(rdi = a Value) -- append its display form to the buffer
.gar_emit_one:
    push r12
    push r14
    push r15
    ; ... shows as "...", which is what CPython's alias repr does even though
    ; repr(Ellipsis) is "Ellipsis"
    extern ellipsis_singleton
    lea rax, [rel ellipsis_singleton]
    cmp rdi, rax
    jne .geo_not_ellipsis
    cmp r13, GAR_BUF - 8
    jae .geo_done
    mov byte [rbx + r13], '.'
    mov byte [rbx + r13 + 1], '.'
    mov byte [rbx + r13 + 2], '.'
    add r13, 3
    jmp .geo_done
.geo_not_ellipsis:
    ; a type shows as its (unqualified) name
    V_TEST_PTR rdi, rax
    ja .geo_repr
    test rdi, rdi
    jz .geo_repr
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .geo_repr
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .geo_repr
    call .gar_qualified
    jmp .geo_done

.geo_repr:
    call obj_repr
    V_UNPACK rax, rdx
    test rax, rax
    jz .geo_done
    push rax
    mov r8, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor ecx, ecx
.geo_repr_copy:
    cmp rcx, r8
    jge .geo_repr_done
    cmp r13, GAR_BUF - 8
    jae .geo_repr_done
    movzx eax, byte [rsi + rcx]
    mov [rbx + r13], al
    inc r13
    inc rcx
    jmp .geo_repr_copy
.geo_repr_done:
    pop rdi
    call obj_decref
.geo_done:
    pop r15
    pop r14
    pop r12
    ret


;; .gar_qualified(rdi = a class) -- append "module.QualName" to rbx at r13.
;;
;; CPython writes an alias's origin and its type arguments the way it writes a
;; class in an annotation: qualified by module, with "builtins" left off, and
;; using __qualname__ so a nested class keeps its "Outer.Inner".  Only the
;; bare tp_name was written, which for a class built by a metaclass of its own
;; -- every ABC, and os.PathLike is one -- was not reached at all, because the
;; test for "is this a class?" was a comparison against the two metatypes this
;; tree ships rather than TYPE_FLAG_METATYPE.
.gar_qualified:
    push r12
    push r14
    push r15
    mov r12, rdi                    ; the class
    mov r14, [rdi + PyTypeObject.tp_dict]

    ; --- the module, unless it is "builtins" ---
    test r14, r14
    jz .gq_module_from_name
    CSTRING rdi, "__module__"
    call str_from_cstr
    mov rsi, rax
    mov rdi, r14
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .gq_module_from_name
    cmp edx, TAG_PTR
    jne .gq_module_from_name
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .gq_module_from_name
    lea rsi, [rax + PyStrObject.data]
    mov rdi, rsi
    CSTRING rsi, "builtins"
    push rdi
    call ap_strcmp
    pop rsi
    test eax, eax
    jz .gq_qualname                 ; "builtins" is left off
    call .gq_copy_cstr
    mov byte [rbx + r13], '.'
    inc r13
    jmp .gq_qualname

.gq_module_from_name:
    ; A static type records no __module__; its tp_name carries the dotted
    ; prefix instead -- "types.GenericAlias" -- and everything else is a
    ; builtin, which prints unqualified either way.
    mov rsi, [r12 + PyTypeObject.tp_name]
    xor ecx, ecx
    xor r15, r15                    ; length of the prefix, 0 for none
.gq_scan:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .gq_scanned
    cmp al, '.'
    jne .gq_scan_next
    lea r15, [rcx + 1]
.gq_scan_next:
    inc rcx
    jmp .gq_scan
.gq_scanned:
    test r15, r15
    jz .gq_qualname
    xor ecx, ecx
.gq_prefix:
    cmp rcx, r15
    jge .gq_qualname
    cmp r13, GAR_BUF - 8
    jae .gq_qualname
    movzx eax, byte [rsi + rcx]
    mov [rbx + r13], al
    inc r13
    inc rcx
    jmp .gq_prefix

.gq_qualname:
    test r14, r14
    jz .gq_from_tp_name
    CSTRING rdi, "__qualname__"
    call str_from_cstr
    mov rsi, rax
    mov rdi, r14
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .gq_from_tp_name
    cmp edx, TAG_PTR
    jne .gq_from_tp_name
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .gq_from_tp_name
    lea rsi, [rax + PyStrObject.data]
    call .gq_copy_cstr
    jmp .gq_done

.gq_from_tp_name:
    ; The last dotted component: "types.GenericAlias" prints "GenericAlias",
    ; the prefix having gone in as the module above.
    mov rsi, [r12 + PyTypeObject.tp_name]
    mov rdi, rsi
    xor ecx, ecx
.gq_last_dot:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .gq_tail
    cmp al, '.'
    jne .gq_dot_next
    lea rdi, [rsi + rcx + 1]
.gq_dot_next:
    inc rcx
    jmp .gq_last_dot
.gq_tail:
    mov rsi, rdi
    call .gq_copy_cstr
.gq_done:
    pop r15
    pop r14
    pop r12
    ret

;; .gq_copy_cstr(rsi = a NUL-terminated string) -- append it, bounded.
.gq_copy_cstr:
    movzx eax, byte [rsi]
    test al, al
    jz .gq_copied
    inc rsi
    cmp r13, GAR_BUF - 8
    jae .gq_copied
    mov [rbx + r13], al
    inc r13
    jmp .gq_copy_cstr
.gq_copied:
    ret

.gar_close:
    mov byte [rbx + r13], ']'
    inc r13
    mov rdi, rbx
    mov rsi, r13
    call str_new_heap
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC generic_alias_repr

;; Calling an alias constructs the origin: list[int]() is a list.
DEF_FUNC_BARE generic_alias_call
    mov rax, [rdi + PyGenericAliasObject.ga_origin]
    mov rdi, rax
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .gac_bad
    jmp rcx
.gac_bad:
    RAISE exc_TypeError_type, "generic alias is not callable"
END_FUNC generic_alias_call

;; __origin__ / __args__
GAG_NAME  equ 8
GAG_FRAME equ 24            ; + 1 push = 32, 16-aligned

DEF_FUNC generic_alias_getattr, GAG_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - GAG_NAME], rsi
    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__origin__"
    call ap_strcmp
    test eax, eax
    jz .gag_origin

    ; __args__ is always a tuple, even when the subscript was one thing:
    ; CPython wraps it, and typing.get_args() and every annotation reader
    ; expect that.  This used to answer NULL, which reads as "no attribute".
    mov rdi, [rbp - GAG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "__args__"
    call ap_strcmp
    test eax, eax
    jz .gag_args

    ; PEP 646 asks whether an alias is the unpacked form, and typing reads it
    ; by name.
    mov rdi, [rbp - GAG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "__unpacked__"
    call ap_strcmp
    test eax, eax
    jnz .gag_missing
    cmp qword [rbx + PyGenericAliasObject.ga_starred], 0
    je .gag_not_unpacked
    lea rax, [rel bool_true]
    jmp .gag_bool
.gag_not_unpacked:
    lea rax, [rel bool_false]
.gag_bool:
    INCREF rax
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.gag_args:

    mov rax, [rbx + PyGenericAliasObject.ga_args]
    test rax, rax
    jz .gag_empty_args
    V_TEST_PTR rax, rcx
    ja .gag_wrap_args
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    jne .gag_wrap_args
    mov rdi, rax
    call obj_incref
    mov rax, [rbx + PyGenericAliasObject.ga_args]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.gag_wrap_args:
    mov edi, 1
    call tuple_new
    test rax, rax
    jz .gag_missing
    mov rcx, [rbx + PyGenericAliasObject.ga_args]
    mov rdx, [rax + PyTupleObject.ob_item]
    mov [rdx], rcx
    push rax
    mov rax, rcx
    INCREF_V rax, rcx
    pop rax
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.gag_empty_args:
    xor edi, edi
    call tuple_new
    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.gag_missing:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
.gag_origin:
    mov rax, [rbx + PyGenericAliasObject.ga_origin]
    push rax
    mov rdi, rax
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC generic_alias_getattr

;; ga_emit_name(rdi = Value, rsi = buffer, rdx = length, r8 = capacity)
;;   -> rax = new length
;; The display form used inside a generic alias or a union: a type shows as
;; its unqualified name, Ellipsis as "...", anything else as its repr.
DEF_FUNC ga_emit_name, 8            ; 3 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    mov rbx, rsi
    mov r13, rdx
    mov r12, rdi

    lea rax, [rel ellipsis_singleton]
    cmp r12, rax
    jne .gen_not_ellipsis
    cmp r13, 240
    jae .gen_out
    mov byte [rbx + r13], '.'
    mov byte [rbx + r13 + 1], '.'
    mov byte [rbx + r13 + 2], '.'
    add r13, 3
    jmp .gen_out

.gen_not_ellipsis:
    ; NoneType prints as None, in a union and in a subscript alike:
    ; `int | None` and `list[None]` are what CPython spells these.
    lea rax, [rel none_type]
    cmp r12, rax
    jne .gen_not_none
    cmp r13, 240
    jae .gen_out
    mov byte [rbx + r13], 'N'
    mov byte [rbx + r13 + 1], 'o'
    mov byte [rbx + r13 + 2], 'n'
    mov byte [rbx + r13 + 3], 'e'
    add r13, 4
    jmp .gen_out

.gen_not_none:
    V_TEST_PTR r12, rax
    ja .gen_repr
    test r12, r12
    jz .gen_repr
    ; Any class, whichever metatype made it -- a class built by a metaclass of
    ; its own is still a class, and comparing ob_type against the two
    ; metatypes this tree ships answered no for it.
    mov rax, [r12 + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .gen_repr

.gen_typename:
    ; CPython qualifies a class with its module here as it does in a repr:
    ; `__main__.C | None`, and `int | None` for anything in builtins.  The
    ; module comes from the type's dict, and the name is what follows the
    ; last dot of tp_name -- a builtin whose tp_name already carries its
    ; module would otherwise be printed with it twice.
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .gen_no_module
    push rdi
    CSTRING rdi, "__module__"
    extern str_from_cstr
    call str_from_cstr
    pop rdi
    test rax, rax
    jz .gen_no_module
    mov rsi, rax
    extern dict_get
    call dict_get
    test rax, rax
    jz .gen_no_module
    V_TEST_PTR rax, rcx
    ja .gen_no_module
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .gen_no_module
    mov rcx, [rax + PyStrObject.ob_size]
    test rcx, rcx
    jz .gen_no_module
    lea rdi, [rax + PyStrObject.data]
    cmp rcx, 8
    jne .gen_copy_module
    push rax
    push rcx
    CSTRING rsi, "builtins"
    call ap_strcmp
    pop rcx
    pop rax
    test eax, eax
    jz .gen_no_module           ; builtins is left off, as CPython leaves it
    lea rdi, [rax + PyStrObject.data]
.gen_copy_module:
    xor edx, edx
.gen_mod_loop:
    cmp rdx, rcx
    jge .gen_mod_done
    cmp r13, 240
    jae .gen_mod_done
    mov al, [rdi + rdx]
    mov [rbx + r13], al
    inc r13
    inc rdx
    jmp .gen_mod_loop
.gen_mod_done:
    cmp r13, 240
    jae .gen_no_module
    mov byte [rbx + r13], '.'
    inc r13

.gen_no_module:
    mov rsi, [r12 + PyTypeObject.tp_name]
    mov rdi, rsi
    xor ecx, ecx
.gen_last_dot:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .gen_name_start
    cmp al, '.'
    jne .gen_dot_next
    lea rdi, [rsi + rcx + 1]
.gen_dot_next:
    inc rcx
    jmp .gen_last_dot
.gen_name_start:
    mov rsi, rdi
.gen_name_copy:
    movzx eax, byte [rsi]
    test al, al
    jz .gen_out
    inc rsi
    cmp r13, 240
    jae .gen_out
    mov [rbx + r13], al
    inc r13
    jmp .gen_name_copy

.gen_repr:
    mov rdi, r12
    call obj_repr
    V_UNPACK rax, rdx
    test rax, rax
    jz .gen_out
    push rax
    mov r8, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor ecx, ecx
.gen_repr_copy:
    cmp rcx, r8
    jge .gen_repr_done
    cmp r13, 240
    jae .gen_repr_done
    movzx eax, byte [rsi + rcx]
    mov [rbx + r13], al
    inc r13
    inc rcx
    jmp .gen_repr_copy
.gen_repr_done:
    pop rdi
    call obj_decref

.gen_out:
    mov rax, r13
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC ga_emit_name


section .data

sm_name_str: db "staticmethod", 0
descr_func_name: db "__func__", 0
align 8
cm_name_str: db "classmethod", 0
prop_name_str: db "property", 0

;; ============================================================================
section .text

;; ============================================================================
;; descr_func_attr(wrapper, PyStrObject *name) -> Value
;;
;; __func__, the wrapped function.  It is the only way to reach the function
;; through the wrapper, and collections.namedtuple needs it: after building
;; _make as a classmethod it does `_make.__func__.__doc__ = ...`.
;;
;; One function serves both wrappers -- sm_callable and cm_callable are the
;; same slot -- so both type tables point straight at it.
;; ============================================================================
DEF_FUNC descr_func_attr, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    lea rdi, [rsi + PyStrObject.data]
    lea rsi, [rel descr_func_name]
    call ap_strcmp
    test eax, eax
    jne .none
    mov rax, [rbx + PyClassMethodObject.cm_callable]
    test rax, rax
    jz .none
    INCREF rax
    mov edx, TAG_PTR
    V_PACK rax, rdx
    pop rbx
    leave
    ret
.none:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
END_FUNC descr_func_attr

;; ============================================================================
;; func_dunder_get(args, nargs) -> Value
;; staticmethod_dunder_get(args, nargs) -> Value
;; classmethod_dunder_get(args, nargs) -> Value
;;
;; The binding LOAD_ATTR performs natively, exposed as `__get__` so that a
;; function answers hasattr(f, '__get__') the way CPython's does.  enum tells a
;; member from a method in a class body by exactly that question, so without
;; these every helper defined inside an Enum body became an enum member --
;; `Flag._member_names_` came out as ['_get_value'].
;;
;; args[0] is the descriptor, args[1] the instance (None through the class),
;; args[2] the owner type when given.
;; ============================================================================
extern none_singleton
DEF_FUNC func_dunder_get
    mov rax, [rdi]                      ; the function itself
    cmp rsi, 2
    jl .fdg_plain
    mov rdx, [rdi + 8]                  ; the instance
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .fdg_plain                       ; unbound access through the class
    test rdx, rdx
    jz .fdg_plain
    mov rdi, rax
    mov rsi, rdx
    call method_new
    leave
    ret
.fdg_plain:
    INCREF rax
    leave
    ret
END_FUNC func_dunder_get

DEF_FUNC staticmethod_dunder_get
    mov rax, [rdi]
    mov rax, [rax + PyStaticMethodObject.sm_callable]
    test rax, rax
    jz .smg_none
    INCREF rax
    leave
    ret
.smg_none:
    lea rax, [rel none_singleton]
    INCREF rax
    leave
    ret
END_FUNC staticmethod_dunder_get

DEF_FUNC classmethod_dunder_get, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, [rdi]
    mov rbx, [rbx + PyClassMethodObject.cm_callable]
    test rbx, rbx
    jz .cmg_none

    ; The owner type, or the instance's type when only an instance is given.
    xor edx, edx
    cmp rsi, 3
    jl .cmg_from_inst
    mov rdx, [rdi + 16]
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    jne .cmg_have_owner
    xor edx, edx
.cmg_from_inst:
    cmp rsi, 2
    jl .cmg_plain
    mov rdx, [rdi + 8]
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .cmg_plain
    test rdx, rdx
    jz .cmg_plain
    mov rdx, [rdx + PyObject.ob_type]
.cmg_have_owner:
    test rdx, rdx
    jz .cmg_plain
    mov rdi, rbx
    mov rsi, rdx
    call method_new
    pop rbx
    leave
    ret
.cmg_plain:
    mov rax, rbx
    INCREF rax
    pop rbx
    leave
    ret
.cmg_none:
    lea rax, [rel none_singleton]
    INCREF rax
    pop rbx
    leave
    ret
END_FUNC classmethod_dunder_get

;; ============================================================================
;; property_dunder_get / _set / _delete(args, nargs) -> Value
;;
;; The descriptor protocol LOAD_ATTR and STORE_ATTR run natively, exposed by
;; name for the same reason the function ones are: `hasattr(p, '__get__')`.
;; ============================================================================
DEF_FUNC property_dunder_get
    mov rax, [rdi]
    cmp rsi, 2
    jl .pdg2_self
    mov rdx, [rdi + 8]
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .pdg2_self                       ; reached through the class
    test rdx, rdx
    jz .pdg2_self
    mov rdi, rax
    mov rsi, rdx
    call property_descr_get
    V_PACK rax, rdx
    leave
    ret
.pdg2_self:
    INCREF rax
    leave
    ret
END_FUNC property_dunder_get

PDS2_EXC   equ 8
PDS2_FRAME equ 16

DEF_FUNC property_dunder_set, PDS2_FRAME
    cmp rsi, 3
    jl .pds2_bad
    mov rax, [rdi]
    mov rdx, [rdi + 16]                 ; the value, already a Value
    mov rsi, [rdi + 8]                  ; the instance
    mov rdi, rax
    DUNDER_EXC_SAVE [rbp - PDS2_EXC]
    call property_descr_set
    DUNDER_RAISED [rbp - PDS2_EXC], .pds2_raised
    lea rax, [rel none_singleton]
    INCREF rax
    leave
    ret
.pds2_raised:
    xor eax, eax
    leave
    ret
.pds2_bad:
    RAISE exc_TypeError_type, "__set__() takes exactly 2 arguments"
END_FUNC property_dunder_set

DEF_FUNC property_dunder_delete, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    cmp rsi, 2
    jl .pdd_no_deleter
    mov rbx, [rdi]
    mov rax, [rbx + PyPropertyObject.prop_del]
    test rax, rax
    jz .pdd_no_deleter
    mov rdx, [rdi + 8]
    SPUSH_PTR rdx                       ; args[0] = the instance
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call obj_call_n
    add rsp, 16
    test rax, rax
    jz .pdd_failed
    V_UNPACK rax, rdx
    DECREF_VAL rax, edx
    lea rax, [rel none_singleton]
    INCREF rax
    pop rbx
    leave
    ret
.pdd_failed:
    xor eax, eax
    pop rbx
    leave
    ret
.pdd_no_deleter:
    RAISE exc_AttributeError_type, "can't delete attribute"
END_FUNC property_dunder_delete

section .text

;; ============================================================================
;; property_setattr(rdi = self, rsi = name, rdx = value Value)
;;
;; Only __doc__ is writable, which is the one CPython allows -- fget, fset and
;; fdel are read-only there too.  dis.py opens with
;; `_Instruction.opname.__doc__ = "Human readable name for operation"`, and
;; with no tp_setattr at all that was "AttributeError: cannot set attribute",
;; which took dis, modulefinder, pathlib, zipapp and mimetypes with it.
;; ============================================================================
PSA_SELF  equ 8
PSA_VAL   equ 16
PSA_FRAME equ 24            ; + 1 push = 32, 16-aligned
DEF_FUNC_LOCAL property_setattr, PSA_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PSA_VAL], rdx

    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__doc__"
    call ap_strcmp
    test eax, eax
    jnz .psa_readonly

    mov rdi, [rbp - PSA_VAL]
    test rdi, rdi
    jz .psa_delete
    INCREF_V rdi, rax
    mov rax, [rbx + PyPropertyObject.prop_doc]
    mov rcx, [rbp - PSA_VAL]
    mov [rbx + PyPropertyObject.prop_doc], rcx
    test rax, rax
    jz .psa_ok
    mov rdi, rax
    DECREF_V rdi, rcx
.psa_ok:
    xor eax, eax
    pop rbx
    leave
    ret

.psa_delete:
    mov rdi, [rbx + PyPropertyObject.prop_doc]
    mov qword [rbx + PyPropertyObject.prop_doc], 0
    test rdi, rdi
    jz .psa_ok
    DECREF_V rdi, rcx
    jmp .psa_ok

.psa_readonly:
    RAISE exc_AttributeError_type, "readonly attribute"
END_FUNC property_setattr

section .data

; staticmethod_type - type descriptor for staticmethod wrapper
align 8
global staticmethod_type
staticmethod_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq sm_name_str              ; tp_name
    dq PyStaticMethodObject_size ; tp_basicsize
    dq staticmethod_dealloc     ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                ; tp_call  (instances are not callable)
    dq descr_func_attr          ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq staticmethod_construct ; tp_new  (constructor)
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq staticmethod_traverse                        ; tp_traverse
    dq staticmethod_clear                        ; tp_clear
    dq 0               ; tp_dictoffset
    dq 0                        ; tp_tailslots

; classmethod_type - type descriptor for classmethod wrapper
align 8
global classmethod_type
classmethod_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq cm_name_str              ; tp_name
    dq PyClassMethodObject_size ; tp_basicsize
    dq classmethod_dealloc      ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                ; tp_call  (instances are not callable)
    dq descr_func_attr          ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq classmethod_construct ; tp_new  (constructor)
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq classmethod_traverse                        ; tp_traverse
    dq classmethod_clear                        ; tp_clear
    dq 0              ; tp_dictoffset
    dq 0                        ; tp_tailslots

; property_type - type descriptor for property descriptor
align 8
global property_type
property_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq prop_name_str            ; tp_name
    dq PyPropertyObject_size    ; tp_basicsize
    dq property_dealloc         ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                ; tp_call  (instances are not callable)
    dq property_getattr         ; tp_getattr (.setter/.getter/.deleter)
    dq property_setattr         ; tp_setattr (__doc__, and nothing else)
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq property_construct   ; tp_new  (constructor)
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq property_traverse                        ; tp_traverse
    dq property_clear                        ; tp_clear
    dq 0           ; tp_dictoffset
    dq 0                        ; tp_tailslots

; member_descr_type - type descriptor for __slots__ member descriptors
md_name_str: db "member_descriptor", 0
align 8
global member_descr_type
member_descr_type:
    dq 1                            ; ob_refcnt (immortal)
    dq type_type                    ; ob_type
    dq md_name_str                  ; tp_name
    dq PyMemberDescrObject_size     ; tp_basicsize
    dq member_descr_dealloc         ; tp_dealloc
    dq member_descr_repr            ; tp_repr
    dq 0                            ; tp_str
    dq 0                            ; tp_hash
    dq 0                            ; tp_call
    dq 0                            ; tp_getattr
    dq 0                            ; tp_setattr
    dq 0                            ; tp_richcompare
    dq 0                            ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq 0                            ; tp_new
    dq 0                            ; tp_as_number
    dq 0                            ; tp_as_sequence
    dq 0                            ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq 0                            ; tp_flags
    dq 0                            ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots

align 8
u_name_str: db "types.UnionType", 0

align 8
global union_type
union_type:
    dq 1                            ; ob_refcnt (immortal)
    dq type_type                    ; ob_type
    dq u_name_str                   ; tp_name
    dq PyGenericAliasObject_size    ; tp_basicsize
    dq generic_alias_dealloc        ; tp_dealloc
    dq union_repr                   ; tp_repr
    dq union_repr                   ; tp_str
    dq union_hash                   ; tp_hash
    dq 0                            ; tp_call
    dq union_getattr                ; tp_getattr
    dq 0                            ; tp_setattr
    dq union_richcompare            ; tp_richcompare
    dq 0                            ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq 0                            ; tp_new
    dq union_number_methods         ; tp_as_number
    dq 0                            ; tp_as_sequence
    dq 0                            ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq 0                            ; tp_flags
    dq 0                            ; tp_bases
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset
    dq 0                        ; tp_tailslots


section .text

section .data
align 8
union_number_methods:
    times 15 dq 0
    dq union_type_or                ; nb_or (+120)
    times 20 dq 0

; A generic alias is unionable too, so `list[int] | None` needs the slot on
; the LEFT operand's type as well as on the right's.
align 8
generic_alias_as_number:
    times 15 dq 0
    dq union_type_or                ; nb_or (+120)
    times 20 dq 0

align 8
ga_name_str: db "types.GenericAlias", 0

align 8
section .text

;; ============================================================================
;; generic_alias_hash(rdi = self) -> rax = hash
;; generic_alias_richcompare(rdi = left, rsi = right, edx = op) -> Value
;;
;; union_type got both of these and generic_alias_type did not, so
;; `{list[int]: 1}[list[int]]` was a KeyError: two aliases spelt the same way
;; hashed by identity and compared by it.  Unlike a union, an alias is ordered
;; -- list[int, str] is not list[str, int] -- so this is a plain combine over
;; (origin, args) rather than union's set equality.
;; ============================================================================
GAH_FRAME equ 16            ; + 2 pushes = 32

DEF_FUNC generic_alias_hash, GAH_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov rdi, [rbx + PyGenericAliasObject.ga_origin]
    test rdi, rdi
    jz .gah_no_origin
    call obj_hash
    jmp .gah_have_origin
.gah_no_origin:
    xor eax, eax
.gah_have_origin:
    mov r12, rax
    mov rdi, [rbx + PyGenericAliasObject.ga_args]
    test rdi, rdi
    jz .gah_done
    call obj_hash
    imul r12, r12, 1000003
    xor r12, rax
.gah_done:
    mov rax, r12
    cmp rax, -1
    jne .gah_ret
    mov rax, -2
.gah_ret:
    pop r12
    pop rbx
    leave
    ret
END_FUNC generic_alias_hash

GRC_LEFT  equ 8
GRC_RIGHT equ 16
GRC_OP    equ 24
GRC_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC generic_alias_richcompare, GRC_FRAME
    cmp edx, PY_EQ
    je .grc_ok
    cmp edx, PY_NE
    jne .grc_decline
.grc_ok:
    mov [rbp - GRC_OP], edx
    ; Both sides must be aliases; anything else declines so the protocol can
    ; try the other operand.
    V_TEST_PTR rdi, rax
    ja .grc_decline
    V_TEST_PTR rsi, rax
    ja .grc_decline
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel generic_alias_type]
    cmp rax, rcx
    jne .grc_decline
    mov rax, [rsi + PyObject.ob_type]
    cmp rax, rcx
    jne .grc_decline
    mov [rbp - GRC_LEFT], rdi
    mov [rbp - GRC_RIGHT], rsi

    mov rdi, [rdi + PyGenericAliasObject.ga_origin]
    mov rsi, [rsi + PyGenericAliasObject.ga_origin]
    mov edx, PY_EQ
    extern obj_richcompare_bool
    call obj_richcompare_bool
    cmp eax, 0
    jl .grc_raised
    test eax, eax
    jz .grc_false

    mov rdi, [rbp - GRC_LEFT]
    mov rdi, [rdi + PyGenericAliasObject.ga_args]
    mov rsi, [rbp - GRC_RIGHT]
    mov rsi, [rsi + PyGenericAliasObject.ga_args]
    mov edx, PY_EQ
    call obj_richcompare_bool
    cmp eax, 0
    jl .grc_raised
    test eax, eax
    jz .grc_false

    mov eax, 1
    jmp .grc_answer
.grc_false:
    xor eax, eax
.grc_answer:
    cmp dword [rbp - GRC_OP], PY_NE
    jne .grc_emit
    xor eax, 1
.grc_emit:
    test eax, eax
    jz .grc_emit_false
    lea rax, [rel bool_true]
    jmp .grc_emit_done
.grc_emit_false:
    lea rax, [rel bool_false]
.grc_emit_done:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.grc_decline:
    xor eax, eax                ; a NULL Value: NotImplemented
    xor edx, edx
    leave
    ret
.grc_raised:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC generic_alias_richcompare

section .data

align 8
global generic_alias_type
generic_alias_type:
    dq 1                            ; ob_refcnt (immortal)
    dq type_type                    ; ob_type
    dq ga_name_str                  ; tp_name
    dq PyGenericAliasObject_size    ; tp_basicsize
    dq generic_alias_dealloc        ; tp_dealloc
    dq generic_alias_repr           ; tp_repr
    dq generic_alias_repr           ; tp_str
    dq generic_alias_hash           ; tp_hash
    dq generic_alias_call           ; tp_call
    dq generic_alias_getattr        ; tp_getattr
    dq 0                            ; tp_setattr
    dq generic_alias_richcompare    ; tp_richcompare
    dq generic_alias_iter           ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq generic_alias_construct      ; tp_new
    dq generic_alias_as_number      ; tp_as_number
    dq 0                            ; tp_as_sequence
    dq 0                            ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq 0                            ; tp_flags
    dq 0                            ; tp_bases
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset
    dq 0                        ; tp_tailslots

align 8
gsd_name_str: db "getset_descriptor", 0

align 8
global getset_descr_type
getset_descr_type:
    dq 1                            ; ob_refcnt (immortal)
    dq type_type                    ; ob_type
    dq gsd_name_str                 ; tp_name
    dq PyGetSetDescrObject_size     ; tp_basicsize
    dq getset_descr_dealloc         ; tp_dealloc
    dq getset_descr_repr            ; tp_repr
    dq getset_descr_repr            ; tp_str
    dq 0                            ; tp_hash
    dq 0                            ; tp_call
    dq getset_descr_getattr         ; tp_getattr
    dq 0                            ; tp_setattr
    dq 0                            ; tp_richcompare
    dq 0                            ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq 0                            ; tp_new
    dq 0                            ; tp_as_number
    dq 0                            ; tp_as_sequence
    dq 0                            ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq 0                            ; tp_flags
    dq 0                            ; tp_bases
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset
    dq 0                        ; tp_tailslots

align 8
mp_name_str: db "mappingproxy", 0

align 8
global mappingproxy_type
mappingproxy_type:
    dq 1                            ; ob_refcnt (immortal)
    dq type_type                    ; ob_type
    dq mp_name_str                  ; tp_name
    dq PyMappingProxyObject_size    ; tp_basicsize
    dq mappingproxy_dealloc         ; tp_dealloc
    dq mappingproxy_repr            ; tp_repr
    dq mappingproxy_repr            ; tp_str
    dq 0                            ; tp_hash
    dq 0                            ; tp_call
    dq mappingproxy_getattr         ; tp_getattr
    dq 0                            ; tp_setattr
    dq 0                            ; tp_richcompare
    dq mappingproxy_iter            ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq mappingproxy_construct       ; tp_new
    dq 0                            ; tp_as_number
    dq mappingproxy_seq_methods     ; tp_as_sequence
    dq mappingproxy_map_methods     ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq 0                            ; tp_flags
    dq 0                            ; tp_bases
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset
    dq 0                        ; tp_tailslots

align 8
mappingproxy_seq_methods:
    dq mappingproxy_len             ; sq_length
    dq 0, 0, 0, 0                   ; concat, repeat, item, ass_item
    dq mappingproxy_contains        ; sq_contains
    dq 0, 0                         ; inplace concat, repeat

align 8
mappingproxy_map_methods:
    dq mappingproxy_len             ; mp_length
    dq mappingproxy_subscript       ; mp_subscript
    dq 0                            ; mp_ass_subscript

; Cached builtin function singletons for property.setter/getter/deleter
_prop_setter_cache: dq 0
_prop_getter_cache: dq 0
_prop_deleter_cache: dq 0

section .rodata
mpg_n_setitem: db "__setitem__", 0
mpg_n_delitem: db "__delitem__", 0
mpg_n_clear:   db "clear", 0
mpg_n_pop:     db "pop", 0
mpg_n_popitem: db "popitem", 0
mpg_n_setdefault: db "setdefault", 0
mpg_n_update:  db "update", 0
align 8
mpg_readonly_names:
    dq mpg_n_setitem, mpg_n_delitem, mpg_n_clear, mpg_n_pop
    dq mpg_n_popitem, mpg_n_setdefault, mpg_n_update, 0

section .text

;; ============================================================================
;; GC traverse and clear.  These lived in gc.asm, which left the collector
;; holding the reference graph of every type in the system; a type's own
;; file is the only place that knows which of its fields are owned.
;; ============================================================================

;; ============================================================================
;; ---- staticmethod_traverse / classmethod_traverse / property_traverse ----
;; ============================================================================
DEF_FUNC staticmethod_traverse
    mov rdi, [rdi + PyStaticMethodObject.sm_callable]
    VISIT_PTR rdi
    leave
    ret
END_FUNC staticmethod_traverse

DEF_FUNC staticmethod_clear, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyStaticMethodObject.sm_callable]
    mov qword [rbx + PyStaticMethodObject.sm_callable], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC staticmethod_clear

DEF_FUNC classmethod_traverse
    mov rdi, [rdi + PyClassMethodObject.cm_callable]
    VISIT_PTR rdi
    leave
    ret
END_FUNC classmethod_traverse

DEF_FUNC classmethod_clear, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyClassMethodObject.cm_callable]
    mov qword [rbx + PyClassMethodObject.cm_callable], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC classmethod_clear

DEF_FUNC property_traverse
    push rbx
    mov rbx, rdi
    mov rax, [rbx + PyPropertyObject.prop_get]
    VISIT_V rax, rcx
    mov rax, [rbx + PyPropertyObject.prop_set]
    VISIT_V rax, rcx
    mov rax, [rbx + PyPropertyObject.prop_del]
    VISIT_V rax, rcx
    mov rax, [rbx + PyPropertyObject.prop_doc]
    VISIT_V rax, rcx
    pop rbx
    leave
    ret
END_FUNC property_traverse

DEF_FUNC property_clear
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyPropertyObject.prop_get]
    mov qword [rbx + PyPropertyObject.prop_get], 0
    test rdi, rdi
    jz .no_get
    DECREF_V rdi, rcx
.no_get:
    mov rdi, [rbx + PyPropertyObject.prop_set]
    mov qword [rbx + PyPropertyObject.prop_set], 0
    test rdi, rdi
    jz .no_set
    DECREF_V rdi, rcx
.no_set:
    mov rdi, [rbx + PyPropertyObject.prop_del]
    mov qword [rbx + PyPropertyObject.prop_del], 0
    test rdi, rdi
    jz .no_del
    DECREF_V rdi, rcx
.no_del:
    mov rdi, [rbx + PyPropertyObject.prop_doc]
    mov qword [rbx + PyPropertyObject.prop_doc], 0
    test rdi, rdi
    jz .no_doc
    DECREF_V rdi, rcx
.no_doc:
    pop rbx
    leave
    ret
END_FUNC property_clear

section .rodata
pc_doc_name: db "__doc__", 0
