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
    mov r14, [rbx + 16]         ; fset = args[1]

.pc_alloc:
    ; Save fdel
    push qword 0                ; fdel default = NULL
    cmp r12, 3
    jb .pc_do_alloc
    mov rax, [rbx + 32]
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
    mov r9, [rbx + 16]          ; new fset

    ; Build args for property_construct: (fget, fset, fdel)
    sub rsp, 48
    mov rax, [r8 + PyPropertyObject.prop_get]
    mov [rsp], rax              ; args[0] = fget
    mov qword [rsp + 8], TAG_PTR ; args[0] tag
    mov [rsp + 16], r9          ; args[1] = new fset
    mov qword [rsp + 24], TAG_PTR ; args[1] tag
    mov rax, [r8 + PyPropertyObject.prop_del]
    mov [rsp + 32], rax         ; args[2] = fdel
    mov qword [rsp + 40], TAG_PTR ; args[2] tag

    xor edi, edi                ; type (ignored)
    mov rsi, rsp                ; args
    mov edx, 3                  ; nargs
    ; Check if fdel is NULL — if so, pass 2 args
    cmp qword [rsp + 32], 0
    jne .psi_call
    mov edx, 2
.psi_call:
    call property_construct
    add rsp, 48

    pop rbx
    leave
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
    mov r9, [rbx + 16]          ; new fget

    sub rsp, 48
    mov [rsp], r9               ; args[0] = new fget
    mov qword [rsp + 8], TAG_PTR ; args[0] tag
    mov rax, [r8 + PyPropertyObject.prop_set]
    mov [rsp + 16], rax         ; args[1] = fset
    mov qword [rsp + 24], TAG_PTR ; args[1] tag
    mov rax, [r8 + PyPropertyObject.prop_del]
    mov [rsp + 32], rax         ; args[2] = fdel
    mov qword [rsp + 40], TAG_PTR ; args[2] tag

    xor edi, edi
    mov rsi, rsp
    mov edx, 3
    cmp qword [rsp + 32], 0
    jne .pgi_call
    mov edx, 2
    cmp qword [rsp + 16], 0
    jne .pgi_call
    mov edx, 1
.pgi_call:
    call property_construct
    add rsp, 48

    pop rbx
    leave
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
    mov r9, [rbx + 16]          ; new fdel

    sub rsp, 48
    mov rax, [r8 + PyPropertyObject.prop_get]
    mov [rsp], rax              ; args[0] = fget
    mov qword [rsp + 8], TAG_PTR ; args[0] tag
    mov rax, [r8 + PyPropertyObject.prop_set]
    mov [rsp + 16], rax         ; args[1] = fset
    mov qword [rsp + 24], TAG_PTR ; args[1] tag
    mov [rsp + 32], r9          ; args[2] = new fdel
    mov qword [rsp + 40], TAG_PTR ; args[2] tag

    xor edi, edi
    mov rsi, rsp
    mov edx, 3
    call property_construct
    add rsp, 48

    pop rbx
    leave
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
    mov r13, rdx                ; value
    mov r14d, ecx               ; value tag

    mov rax, [rbx + PyPropertyObject.prop_set]
    test rax, rax
    jz .pds_no_setter

    ; Call fset(obj, value): fset.tp_call(fset, args, 2)
    mov rdi, rax
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .pds_no_setter

    ; Build fat args on stack: [obj, value]
    sub rsp, 32
    mov [rsp], r12              ; args[0] payload = obj
    mov qword [rsp + 8], TAG_PTR ; args[0] tag
    mov [rsp + 16], r13         ; args[1] payload = value
    mov [rsp + 24], r14         ; args[1] tag (from caller)
    mov rsi, rsp                ; args ptr
    mov edx, 2                  ; nargs = 2
    call rax
    add rsp, 32                 ; pop fat args

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
    dq staticmethod_construct   ; tp_call (constructor)
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
    dq staticmethod_traverse                        ; tp_traverse
    dq staticmethod_clear                        ; tp_clear

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
    dq classmethod_construct    ; tp_call (constructor)
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
    dq classmethod_traverse                        ; tp_traverse
    dq classmethod_clear                        ; tp_clear

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
    dq property_construct       ; tp_call (constructor)
    dq property_getattr         ; tp_getattr (.setter/.getter/.deleter)
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
    dq property_traverse                        ; tp_traverse
    dq property_clear                        ; tp_clear

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

; Cached builtin function singletons for property.setter/getter/deleter
_prop_setter_cache: dq 0
_prop_getter_cache: dq 0
_prop_deleter_cache: dq 0
