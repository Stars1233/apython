; descriptors.asm - staticmethod, classmethod, property descriptor types
;
; staticmethod(func) -> wrapper that prevents method binding
; classmethod(func) -> wrapper that binds class instead of instance
; property(fget[, fset[, fdel]]) -> data descriptor

%include "macros.inc"
%include "object.inc"

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
DEF_FUNC staticmethod_construct
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
DEF_FUNC_LOCAL staticmethod_dealloc
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
DEF_FUNC classmethod_construct
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
DEF_FUNC_LOCAL classmethod_dealloc
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
DEF_FUNC_LOCAL property_dealloc
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
DEF_FUNC _prop_setter_impl
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
DEF_FUNC _prop_getter_impl
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
DEF_FUNC _prop_deleter_impl
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
;; rdi = byte offset in instance, rsi = slot name (INCREF'd, ownership taken)
;; ============================================================================
DEF_FUNC member_descr_new
    push rbx
    push r12

    mov rbx, rdi            ; offset
    mov r12, rsi            ; name str

    mov edi, PyMemberDescrObject_size
    call ap_malloc

    mov qword [rax + PyMemberDescrObject.ob_refcnt], 1
    lea rcx, [rel member_descr_type]
    mov [rax + PyMemberDescrObject.ob_type], rcx
    mov [rax + PyMemberDescrObject.md_offset], rbx
    mov [rax + PyMemberDescrObject.md_name], r12

    pop r12
    pop rbx
    leave
    ret
END_FUNC member_descr_new

;; member_descr_dealloc(PyMemberDescrObject *self)
DEF_FUNC member_descr_dealloc
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

DEF_FUNC mappingproxy_new
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

DEF_FUNC_LOCAL mappingproxy_dealloc
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
DEF_FUNC getset_descr_new
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
DEF_FUNC_LOCAL getset_descr_compose
    push rbx
    push r12
    mov rbx, rsi
    mov r12, rdi
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
DEF_FUNC getset_descr_get
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
;; getset_descr_set(rdi = the descriptor, rsi = self Value, rdx = value Value)
;;   -> eax = 0, or never returns
;;
;; A NULL setter is a read-only attribute.  Every getset the tree registers
;; today has one, which is what makes `(5).real = 1` an AttributeError rather
;; than a silent instance attribute on a subclass.
;; ============================================================================
GDS_SELF  equ 8
GDS_FRAME equ 16            ; + 0 pushes = 16
DEF_FUNC getset_descr_set, GDS_FRAME
    mov [rbp - GDS_SELF], rdi
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
    call getset_descr_compose
    mov rdi, rax
    lea rsi, [rel gds_ro_tail]
    call rbt_append_cstr
    lea rdi, [rel exc_AttributeError_type]
    lea rsi, [rel gdr_buf]
    call raise_exception
    ud2
END_FUNC getset_descr_set

DEF_FUNC_LOCAL getset_descr_dealloc
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

DEF_FUNC_LOCAL generic_alias_dealloc
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
GAR_FRAME equ 288           ; + 5 pushes = 328, not 16-aligned
DEF_FUNC generic_alias_repr, GAR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov [rbp - GAR_SELF], rdi
    lea rbx, [rbp - GAR_BUF]
    xor r13d, r13d

    mov rax, [rdi + PyGenericAliasObject.ga_origin]
    test rax, rax
    jz .gar_open
    mov rsi, [rax + PyObject.ob_type]
    extern type_type
    lea rcx, [rel type_type]
    cmp rsi, rcx
    je .gar_have_name
    extern user_type_metatype
    lea rcx, [rel user_type_metatype]
    cmp rsi, rcx
    jne .gar_open
.gar_have_name:
    mov rsi, [rax + PyTypeObject.tp_name]
.gar_name:
    movzx eax, byte [rsi]
    test al, al
    jz .gar_open
    inc rsi
    cmp r13, 100
    jae .gar_open
    mov [rbx + r13], al
    inc r13
    jmp .gar_name

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
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .geo_typename
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    jne .geo_repr
.geo_typename:
    mov rsi, [rdi + PyTypeObject.tp_name]
    mov rdi, rsi
    xor ecx, ecx
.geo_last_dot:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .geo_name_start
    cmp al, '.'
    jne .geo_dot_next
    lea rdi, [rsi + rcx + 1]
.geo_dot_next:
    inc rcx
    jmp .geo_last_dot
.geo_name_start:
    mov rsi, rdi
.geo_name_copy:
    movzx eax, byte [rsi]
    test al, al
    jz .geo_done
    inc rsi
    cmp r13, GAR_BUF - 8
    jae .geo_done
    mov [rbx + r13], al
    inc r13
    jmp .geo_name_copy

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
GAG_FRAME equ 16            ; + 1 push = 24, not 16-aligned

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
    jnz .gag_missing

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
DEF_FUNC ga_emit_name
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
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .gen_typename
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    jne .gen_repr

.gen_typename:
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

;; ============================================================================
;; types.UnionType -- what `int | str` evaluates to (PEP 604).
;; types.py takes UnionType from `type(int | str)`.  Represented as a
;; GenericAlias-shaped record whose args are the operand tuple; the repr is
;; the pipe form rather than the bracket form.
;; ============================================================================
UTO_LIST  equ 8         ; the member list being accumulated
UTO_LEFT  equ 16
UTO_RIGHT equ 24
UTO_FRAME equ 32            ; + 4 pushes = 64, 16-aligned

DEF_FUNC union_type_or, UTO_FRAME
    ; nb_or(left, right) -> UnionType, for type | type
    push rbx
    push r12
    push r13
    push r14
    mov [rbp - UTO_LEFT], rdi
    mov [rbp - UTO_RIGHT], rsi

    ; both sides must be types (or an existing union, or None)
    call union_operand_ok       ; rdi = left
    test eax, eax
    jz .uto_notimpl
    mov rdi, [rbp - UTO_RIGHT]
    call union_operand_ok
    test eax, eax
    jz .uto_notimpl

    ; Collect the members.  This used to be a bare tuple_new(2) holding the two
    ; operands, so int | str | float nested as ((int|str), float): __args__ was
    ; not flat, and union_richcompare -- which compares the two arg tuples as
    ; sets -- read the inner union as one opaque member and answered False for
    ; (int|str|float) == (float|str|int).  The repr hid it, because a member
    ; that is not a type is printed with obj_repr, which re-enters union_repr.
    extern list_new
    xor edi, edi
    call list_new
    mov rbx, rax
    mov [rbp - UTO_LIST], rax

    mov rdi, [rbp - UTO_LEFT]
    call .uto_add_operand
    mov rdi, [rbp - UTO_RIGHT]
    call .uto_add_operand

    ; int | int is int: a union of one member is that member.
    cmp qword [rbx + PyListObject.ob_size], 1
    jne .uto_build
    mov rax, [rbx + PyListObject.ob_item]
    mov r12, [rax]
    INCREF_V r12, rcx
    mov rdi, rbx
    call obj_decref
    mov rax, r12
    mov edx, TAG_PTR
    jmp .uto_return

.uto_build:
    mov rdi, [rbx + PyListObject.ob_size]
    extern tuple_new
    call tuple_new
    mov r12, rax                ; the member tuple
    mov r13, [rbx + PyListObject.ob_item]
    mov r14, [r12 + PyTupleObject.ob_item]
    xor ecx, ecx
.uto_copy:
    cmp rcx, [rbx + PyListObject.ob_size]
    jge .uto_copied
    mov rax, [r13 + rcx * 8]
    mov [r14 + rcx * 8], rax
    push rcx
    INCREF_V rax, rcx
    pop rcx
    inc rcx
    jmp .uto_copy

.uto_copied:
    mov rdi, rbx
    call obj_decref             ; the accumulator; the tuple owns the members

    xor edi, edi                ; no origin
    mov rsi, r12
    call generic_alias_new
    lea rcx, [rel union_type]
    mov [rax + PyObject.ob_type], rcx
    push rax
    mov rdi, r12
    call obj_decref             ; generic_alias_new took its own ref
    pop rax
    mov edx, TAG_PTR

.uto_return:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.uto_notimpl:
    xor eax, eax
    xor edx, edx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

;; .uto_add_operand(rdi = one operand Value) -- add what it contributes to the
;; list in rbx.  A union contributes its members; anything else contributes
;; itself.  Uses r13 and r14.
.uto_add_operand:
    ; None stands for NoneType inside a union, which is what makes
    ; (None | int) == (int | type(None)) true, as it is in CPython.
    lea rcx, [rel none_singleton]
    cmp rdi, rcx
    jne .uto_ao_typed
    extern none_type
    lea rdi, [rel none_type]
.uto_ao_typed:
    V_TEST_PTR rdi, rax
    ja .uto_ao_one
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel union_type]
    cmp rax, rcx
    jne .uto_ao_one

    mov r13, [rdi + PyGenericAliasObject.ga_args]
    xor r14d, r14d
.uto_ao_loop:
    cmp r14, [r13 + PyTupleObject.ob_size]
    jge .uto_ao_spliced
    mov rax, [r13 + PyTupleObject.ob_item]
    mov rdi, [rax + r14 * 8]
    call .uto_ao_one            ; already flat and already normalised
    inc r14
    jmp .uto_ao_loop
.uto_ao_spliced:
    ret

;; .uto_ao_one(rdi = one member) -- append it unless an equal one is there.
.uto_ao_one:
    push rdi
    mov rsi, rdi
    mov rdi, rbx
    extern list_contains
    call list_contains
    pop rdi
    test eax, eax
    jnz .uto_ao_one_done        ; int | int, and (int|str) | str
    mov rsi, rdi
    mov rdi, rbx
    extern list_append
    call list_append
.uto_ao_one_done:
    ret
END_FUNC union_type_or

;; ============================================================================
;; union_getattr(rdi = self, rsi = name) -> (rax = payload, rdx = tag)
;;
;; A union is a GenericAlias-shaped record, but it is not one: it has no
;; origin, so it cannot simply borrow generic_alias_getattr.  __args__ is the
;; half that matters -- typing and every annotation reader ask for it by name,
;; and union_type carried neither a tp_getattr nor a tp_dict, so it answered
;; AttributeError while the tuple sat in ga_args.
;; ============================================================================
DEF_FUNC union_getattr
    push rbx
    mov rbx, rdi
    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__args__"
    call ap_strcmp
    test eax, eax
    jnz .ug_missing

    mov rax, [rbx + PyGenericAliasObject.ga_args]
    mov rdi, rax
    call obj_incref
    mov rax, [rbx + PyGenericAliasObject.ga_args]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.ug_missing:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
END_FUNC union_getattr

DEF_FUNC_BARE union_operand_ok
    V_TEST_PTR rdi, rax
    ja .uok_no
    test rdi, rdi
    jz .uok_no
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .uok_yes
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    je .uok_yes
    lea rcx, [rel union_type]
    cmp rax, rcx
    je .uok_yes
    extern none_singleton
    lea rcx, [rel none_singleton]
    cmp rdi, rcx
    je .uok_yes
.uok_no:
    xor eax, eax
    ret
.uok_yes:
    mov eax, 1
    ret
END_FUNC union_operand_ok

;; repr: "int | str"
UR_BUF   equ 264
UR_SELF  equ 272
UR_FRAME equ 288            ; + 5 pushes = 328, not 16-aligned
DEF_FUNC union_repr, UR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov [rbp - UR_SELF], rdi
    lea rbx, [rbp - UR_BUF]
    xor r13d, r13d
    mov rdi, [rdi + PyGenericAliasObject.ga_args]
    test rdi, rdi
    jz .ur_done
    mov r14, [rdi + PyTupleObject.ob_size]
    mov r15, [rdi + PyTupleObject.ob_item]
    xor r12d, r12d
.ur_loop:
    cmp r12, r14
    jge .ur_done
    test r12, r12
    jz .ur_no_sep
    cmp r13, UR_BUF - 16
    jae .ur_done
    mov byte [rbx + r13], ' '
    mov byte [rbx + r13 + 1], '|'
    mov byte [rbx + r13 + 2], ' '
    add r13, 3
.ur_no_sep:
    mov rdi, [r15 + r12*8]
    mov rsi, rbx
    mov rdx, r13
    call ga_emit_name
    mov r13, rax
    inc r12
    jmp .ur_loop
.ur_done:
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
END_FUNC union_repr


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
DEF_FUNC descr_func_attr
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

DEF_FUNC classmethod_dunder_get
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

DEF_FUNC property_dunder_delete
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
PSA_FRAME equ 16            ; + 1 push = 24, not 16-aligned
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
    dq 0                        ; tp_weaklistoffset

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
    dq 0                        ; tp_weaklistoffset

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
    dq 0                        ; tp_weaklistoffset

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
    dq 0                            ; tp_repr
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
    dq 0                        ; tp_weaklistoffset

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
    dq 0                        ; tp_weaklistoffset


section .text

;; ============================================================================
;; union_hash(rdi = PyGenericAliasObject*) -> rax = hash
;; union_richcompare(rdi = left, rsi = right, edx = op) -> Value
;;
;; `hash(int | str)` used to raise: builtin_hash_fn raises when tp_hash is 0,
;; and copyreg.py hashes `type(int | str)` two lines after the complex one, so
;; the whole module -- and everything that imports it -- stopped there.
;;
;; CPython hashes a union as `hash(frozenset(args))` and compares them as
;; frozensets.  frozenset_type.tp_hash is 0 here, so instead the args are
;; combined with XOR, which induces exactly the same equivalence: order does
;; not matter and a repeat is absorbed, so `int | str` and `str | int` hash
;; alike.  The values differ from CPython's; nothing observes them, and the
;; language only requires that equal objects hash equal.
;; ============================================================================
UH_FRAME equ 16             ; + 2 pushes = 32
DEF_FUNC union_hash, UH_FRAME
    push rbx
    push r12
    mov rax, [rdi + PyGenericAliasObject.ga_args]
    test rax, rax
    jz .uh_empty
    mov r12, [rax + PyTupleObject.ob_size]
    mov rbx, [rax + PyTupleObject.ob_item]
    xor eax, eax
    test r12, r12
    jz .uh_done
    push rax
    xor ecx, ecx
.uh_loop:
    push rcx
    mov rdi, [rbx + rcx*8]
    extern obj_hash
    call obj_hash
    pop rcx
    pop rdx
    xor rdx, rax
    push rdx
    inc rcx
    cmp rcx, r12
    jb .uh_loop
    pop rax
.uh_done:
    ; A light avalanche, so that two unions differing only in one member do
    ; not collide merely because XOR preserves low bits.
    mov rdx, rax
    shr rdx, 32
    imul rdx, rdx, 1000003
    xor rax, rdx
    cmp rax, -1
    jne .uh_ret
    mov rax, -2
.uh_ret:
    pop r12
    pop rbx
    leave
    ret
.uh_empty:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC union_hash

URC_LEFT  equ 8
URC_RIGHT equ 16
URC_OP    equ 24
URC_FRAME equ 32             ; + 2 pushes = 48
DEF_FUNC union_richcompare, URC_FRAME
    push rbx
    push r12
    cmp edx, PY_EQ
    je .ur_ok
    cmp edx, PY_NE
    jne .ur_decline
.ur_ok:
    mov [rbp - URC_OP], edx
    ; Both sides must be unions; anything else declines so the protocol can
    ; try the other operand.
    V_TEST_PTR rdi, rax
    ja .ur_decline
    V_TEST_PTR rsi, rax
    ja .ur_decline
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel union_type]
    cmp rax, rcx
    jne .ur_decline
    mov rax, [rsi + PyObject.ob_type]
    cmp rax, rcx
    jne .ur_decline
    mov [rbp - URC_LEFT], rdi
    mov [rbp - URC_RIGHT], rsi

    ; Set equality: every member of each side is present in the other.
    mov rdi, [rbp - URC_LEFT]
    mov rsi, [rbp - URC_RIGHT]
    call union_args_subset
    test eax, eax
    jz .ur_false
    mov rdi, [rbp - URC_RIGHT]
    mov rsi, [rbp - URC_LEFT]
    call union_args_subset
    test eax, eax
    jz .ur_false
.ur_true:
    cmp dword [rbp - URC_OP], PY_EQ
    je .ur_ret_true
    jmp .ur_ret_false
.ur_false:
    cmp dword [rbp - URC_OP], PY_EQ
    je .ur_ret_false
.ur_ret_true:
    extern bool_true
    lea rax, [rel bool_true]
    pop r12
    pop rbx
    leave
    ret
.ur_ret_false:
    extern bool_false
    lea rax, [rel bool_false]
    pop r12
    pop rbx
    leave
    ret
.ur_decline:
    xor eax, eax                    ; NULL Value = NotImplemented
    pop r12
    pop rbx
    leave
    ret
END_FUNC union_richcompare

;; ============================================================================
;; union_args_subset(rdi = a, rsi = b) -> eax = 1 when every member of a's
;; args tuple is also in b's.  Members are compared with
;; obj_richcompare_bool, not by pointer: union_operand_ok admits None and
;; nested unions as well as types.
;; ============================================================================
UAS_BITEMS equ 8
UAS_BSIZE  equ 16
UAS_AITEMS equ 24
UAS_ASIZE  equ 32
UAS_I      equ 40
UAS_FRAME  equ 48           ; + 0 pushes = 48
DEF_FUNC_LOCAL union_args_subset, UAS_FRAME
    mov rax, [rdi + PyGenericAliasObject.ga_args]
    test rax, rax
    jz .uas_yes
    mov rcx, [rax + PyTupleObject.ob_size]
    mov [rbp - UAS_ASIZE], rcx
    mov rcx, [rax + PyTupleObject.ob_item]
    mov [rbp - UAS_AITEMS], rcx
    mov rax, [rsi + PyGenericAliasObject.ga_args]
    test rax, rax
    jz .uas_no
    mov rcx, [rax + PyTupleObject.ob_size]
    mov [rbp - UAS_BSIZE], rcx
    mov rcx, [rax + PyTupleObject.ob_item]
    mov [rbp - UAS_BITEMS], rcx
    mov qword [rbp - UAS_I], 0
.uas_outer:
    mov rax, [rbp - UAS_I]
    cmp rax, [rbp - UAS_ASIZE]
    jae .uas_yes
    xor ecx, ecx
.uas_inner:
    cmp rcx, [rbp - UAS_BSIZE]
    jae .uas_no
    push rcx
    mov rax, [rbp - UAS_AITEMS]
    mov r8, [rbp - UAS_I]
    mov rdi, [rax + r8*8]
    mov rax, [rbp - UAS_BITEMS]
    mov rsi, [rax + rcx*8]
    mov edx, PY_EQ
    extern obj_richcompare_bool
    call obj_richcompare_bool
    pop rcx
    ; -1 is "the comparison raised".  There is no way to report that through
    ; tp_richcompare here -- a NULL Value means NotImplemented, not failure --
    ; but the scan must at least stop, or the next comparison runs more Python
    ; over the top of the pending exception.
    cmp eax, 0
    jl .uas_no
    cmp eax, 1
    je .uas_found
    inc rcx
    jmp .uas_inner
.uas_found:
    inc qword [rbp - UAS_I]
    jmp .uas_outer
.uas_yes:
    mov eax, 1
    leave
    ret
.uas_no:
    xor eax, eax
    leave
    ret
END_FUNC union_args_subset

section .data
align 8
union_number_methods:
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
    dq 0                        ; tp_weaklistoffset

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
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset
    dq 0                        ; tp_weaklistoffset

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
    dq 0                        ; tp_weaklistoffset

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

; ---- staticmethod_traverse / classmethod_traverse / property_traverse ----
DEF_FUNC staticmethod_traverse
    mov rdi, [rdi + PyStaticMethodObject.sm_callable]
    VISIT_PTR rdi
    leave
    ret
END_FUNC staticmethod_traverse

DEF_FUNC staticmethod_clear
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

DEF_FUNC classmethod_clear
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
