; descriptors.asm - staticmethod, classmethod, property descriptor types
;
; staticmethod(func) -> wrapper that prevents method binding
; classmethod(func) -> wrapper that binds class instead of instance
; property(fget[, fset[, fdel]]) -> data descriptor

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"

extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_incref
extern obj_decref
extern obj_dealloc
extern type_type
extern staticmethod_traverse
extern staticmethod_clear
extern property_traverse
extern property_clear
extern classmethod_traverse
extern classmethod_clear
extern raise_exception
extern exc_TypeError_type
extern exc_AttributeError_type
extern ap_strcmp
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "staticmethod expected 1 argument"
    call raise_exception
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "classmethod expected 1 argument"
    call raise_exception
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

    cmp r12, 1
    jb .pc_error
    cmp r12, 3
    ja .pc_error

    ; Extract args
    mov r13, [rbx]              ; fget = args[0]
    xor r14d, r14d              ; fset = NULL
    cmp r12, 2
    jb .pc_alloc
    mov r14, [rbx + 8]         ; fset = args[1]

.pc_alloc:
    ; Save fdel
    push qword 0                ; fdel default = NULL
    cmp r12, 3
    jb .pc_do_alloc
    mov rax, [rbx + 16]
    mov [rsp], rax              ; fdel = args[2]

.pc_do_alloc:
    mov edi, PyPropertyObject_size
    lea rsi, [rel property_type]
    call gc_alloc
    mov rbx, rax                ; rbx = new property (ob_refcnt=1, ob_type set)
    mov [rbx + PyPropertyObject.prop_get], r13
    mov [rbx + PyPropertyObject.prop_set], r14
    pop rax                     ; fdel
    mov [rbx + PyPropertyObject.prop_del], rax

    ; INCREF fget
    mov rdi, r13
    call obj_incref

    ; INCREF fset if non-NULL
    test r14, r14
    jz .pc_no_fset
    mov rdi, r14
    call obj_incref
.pc_no_fset:

    ; INCREF fdel if non-NULL
    mov rdi, [rbx + PyPropertyObject.prop_del]
    test rdi, rdi
    jz .pc_no_fdel
    call obj_incref
.pc_no_fdel:

    mov rdi, rbx
    call gc_track

    mov rax, rbx
    mov edx, TAG_PTR

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.pc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "property expected 1 to 3 arguments"
    call raise_exception
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
    call obj_decref
.pd_no_get:
    mov rdi, [rbx + PyPropertyObject.prop_set]
    test rdi, rdi
    jz .pd_no_set
    call obj_decref
.pd_no_set:
    mov rdi, [rbx + PyPropertyObject.prop_del]
    test rdi, rdi
    jz .pd_no_del
    call obj_decref
.pd_no_del:

    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC property_dealloc

;; ============================================================================
;; property_getattr(PyPropertyObject *self, PyObject *name) -> PyObject*
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

.pga_incref_ret:
    mov rdi, rax
    push rax
    call obj_incref
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "setter expected 1 argument"
    call raise_exception
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "getter expected 1 argument"
    call raise_exception
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "deleter expected 1 argument"
    call raise_exception
END_FUNC _prop_deleter_impl

;; ============================================================================
;; property_descr_get(PyPropertyObject *prop, PyObject *obj) -> PyObject*
;; Called by LOAD_ATTR when a property is found in the type dict.
;; Invokes prop.fget(obj). Returns result (owned ref).
;; ============================================================================
global property_descr_get
DEF_FUNC property_descr_get
    push rbx
    push r12

    mov rbx, rdi                ; property
    mov r12, rsi                ; obj

    mov rax, [rbx + PyPropertyObject.prop_get]
    test rax, rax
    jz .pdg_no_getter

    ; Call fget(obj): fget.tp_call(fget, &obj, 1)
    mov rdi, rax
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .pdg_no_getter

    ; Build fat args on stack
    SPUSH_PTR r12              ; args[0] = obj
    mov rsi, rsp                ; args ptr
    mov edx, 1                  ; nargs = 1
    call rax
    V_UNPACK rax, rdx           ; tp_call returns a Value
    add rsp, 16                 ; pop fat args

    pop r12
    pop rbx
    leave
    ret

.pdg_no_getter:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "unreadable attribute"
    call raise_exception
END_FUNC property_descr_get

;; ============================================================================
;; property_descr_set(PyPropertyObject *prop, PyObject *obj, PyObject *value) -> void
;; Called by STORE_ATTR when a property is found in the type dict.
;; Invokes prop.fset(obj, value).
;; ============================================================================
global property_descr_set
DEF_FUNC property_descr_set
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; property
    mov r12, rsi                ; obj
    mov r13, rdx                ; value Value

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

.pds_no_setter:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "can't set attribute"
    call raise_exception
END_FUNC property_descr_set

;; ============================================================================
;; member_descr_new(i64 offset, PyStrObject *name) -> PyMemberDescrObject*
;; Create a member descriptor for a __slots__ slot.
;; rdi = byte offset in instance, rsi = slot name (INCREF'd, ownership taken)
;; ============================================================================
global member_descr_new
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
global member_descr_dealloc
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
global mappingproxy_new
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

DEF_FUNC mappingproxy_getattr
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
global getset_descr_new
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
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC getset_descr_new

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
global generic_alias_new
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
    mov rdi, r12
    test rdi, rdi
    jz .gan_done
    call obj_incref
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
    mov rdi, [rbx + PyGenericAliasObject.ga_args]
    test rdi, rdi
    jz .gad_free
    call obj_decref
.gad_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC generic_alias_dealloc

;; The builtin registered as __class_getitem__ on each container type.
;; args[0] = cls, args[1] = the subscript.
global generic_alias_class_getitem
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__class_getitem__() takes exactly one argument"
    call raise_exception
END_FUNC generic_alias_class_getitem

;; repr: "list[int]" -- origin name, then the argument's repr.
GAR_BUF   equ 264
GAR_SELF  equ 272
GAR_FRAME equ 288
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "generic alias is not callable"
    call raise_exception
END_FUNC generic_alias_call

;; __origin__ / __args__
DEF_FUNC generic_alias_getattr
    push rbx
    mov rbx, rdi
    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__origin__"
    call ap_strcmp
    test eax, eax
    jz .gag_origin
    mov rax, [rbx + PyGenericAliasObject.ga_args]
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
global union_type_or
DEF_FUNC union_type_or
    ; nb_or(left, right) -> UnionType, for type | type
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    ; both sides must be types (or an existing union)
    mov rdi, rbx
    call union_operand_ok
    test eax, eax
    jz .uto_notimpl
    mov rdi, r12
    call union_operand_ok
    test eax, eax
    jz .uto_notimpl

    mov edi, 2
    extern tuple_new
    call tuple_new
    mov rcx, [rax + PyTupleObject.ob_item]
    mov [rcx], rbx
    mov [rcx + 8], r12
    push rax
    mov rdi, rbx
    call obj_incref
    mov rdi, r12
    call obj_incref
    pop rsi                         ; the operand tuple
    push rsi
    xor edi, edi                    ; no origin
    call generic_alias_new
    lea rcx, [rel union_type]
    mov [rax + PyObject.ob_type], rcx
    pop rdi
    push rax
    call obj_decref                 ; generic_alias_new took its own ref
    pop rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.uto_notimpl:
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret
END_FUNC union_type_or

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
UR_FRAME equ 288
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
cm_name_str: db "classmethod", 0
prop_name_str: db "property", 0

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
    dq 0                        ; tp_getattr
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
    dq 0                        ; tp_getattr
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
    dq 0                        ; tp_setattr
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
    dq 0                            ; tp_hash
    dq 0                            ; tp_call
    dq 0                            ; tp_getattr
    dq 0                            ; tp_setattr
    dq 0                            ; tp_richcompare
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

align 8
union_number_methods:
    times 15 dq 0
    dq union_type_or                ; nb_or (+120)
    times 20 dq 0

align 8
ga_name_str: db "types.GenericAlias", 0

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
    dq 0                            ; tp_hash
    dq generic_alias_call           ; tp_call
    dq generic_alias_getattr        ; tp_getattr
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
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset

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
    dq 0                            ; tp_new
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
