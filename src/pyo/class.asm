; class_obj.asm - Class instances and bound methods for apython
; Phase 10: class instantiation, attribute access, __init__ dispatch

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_incref
extern obj_dealloc
extern dict_new
extern dict_get
extern dict_set
extern str_from_cstr
extern ap_strcmp
extern type_repr
extern fatal_error
extern raise_exception
extern exc_AttributeError_type
extern exc_TypeError_type
extern func_type
extern type_type
extern int_type
extern staticmethod_type
extern classmethod_type
extern property_type
extern property_descr_get
extern eval_frame
extern frame_new
extern frame_free

;; ============================================================================
;; instance_new(PyTypeObject *type) -> PyInstanceObject*
;; Allocate a new instance of the given class type.
;; rdi = type (the class)
;; Returns: new instance with refcnt=1, ob_type=type, inst_dict=new dict
;; ============================================================================
DEF_FUNC instance_new
    push rbx
    push r12

    mov rbx, rdi                ; rbx = type

    ; Allocate PyInstanceObject (24 bytes)
    mov edi, PyInstanceObject_size
    call ap_malloc
    mov r12, rax                ; r12 = instance

    ; ob_refcnt = 1
    mov qword [r12 + PyObject.ob_refcnt], 1

    ; ob_type = type
    mov [r12 + PyObject.ob_type], rbx

    ; INCREF type (stored in ob_type)
    mov rdi, rbx
    call obj_incref

    ; inst_dict = dict_new()
    call dict_new
    mov [r12 + PyInstanceObject.inst_dict], rax

    mov rax, r12                ; return instance
    pop r12
    pop rbx
    leave
    ret
END_FUNC instance_new

;; ============================================================================
;; instance_getattr(PyInstanceObject *self, PyObject *name) -> PyObject*
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
DEF_FUNC instance_getattr
    push rbx
    push r12
    push r13

    mov rbx, rdi                ; rbx = self (instance)
    mov r12, rsi                ; r12 = name

    ; Check self->inst_dict first (may be NULL for int subclass instances)
    mov rdi, [rbx + PyInstanceObject.inst_dict]
    test rdi, rdi
    jz .check_type_dict
    mov rsi, r12
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jnz .found_inst

.check_type_dict:

    ; Not in inst_dict -- walk type MRO: check type->tp_dict, then tp_base chain
    mov rcx, [rbx + PyObject.ob_type]   ; rcx = type (the class)
.walk_mro:
    mov rdi, [rcx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .try_base

    push rcx                            ; save current type
    mov rsi, r12
    mov edx, TAG_PTR
    call dict_get
    pop rcx                             ; restore current type
    test edx, edx
    jnz .found_type                     ; found in type's dict

.try_base:
    mov rcx, [rcx + PyTypeObject.tp_base]
    test rcx, rcx
    jnz .walk_mro

    jmp .not_found

.found_inst:
    ; Found in instance dict — INCREF and return raw value
    mov r13, rax                ; save payload
    mov r12, rdx                ; save tag (name no longer needed)
    INCREF_VAL rax, edx         ; tag-aware INCREF (skips SmallInt/SmallStr/NULL)
    mov rax, r13
    mov rdx, r12                ; restore tag from dict_get
    pop r13
    pop r12
    pop rbx
    leave
    ret

.found_type:
    ; Found in type dict — handle method binding.
    ; Descriptors (staticmethod, classmethod, property) are returned as-is
    ; for LOAD_ATTR to unwrap, since LOAD_ATTR knows the push convention.
    ; Regular callables are bound to the instance.
    mov r13, rax                ; r13 = attr (borrowed ref from dict_get)
    mov r12, rdx                ; r12 = attr tag (name no longer needed)
    cmp r12d, TAG_SMALLINT
    je .found_type_raw          ; SmallInt — return as-is

    mov rcx, [rax + PyObject.ob_type]

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
    ret

.found_type_raw:
    ; Not callable, SmallInt, or descriptor — INCREF and return
    INCREF_VAL r13, r12d        ; tag-aware INCREF (skips SmallInt/SmallStr/NULL)
    mov rax, r13
    mov rdx, r12                ; restore tag from dict_get
    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    xor eax, eax               ; return NULL (caller handles raise)
    xor edx, edx               ; TAG_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC instance_getattr

;; ============================================================================
;; instance_setattr(PyInstanceObject *self, PyObject *name, PyObject *value)
;; Set an attribute on an instance's __dict__.
;; rdi = instance, rsi = name, rdx = value
;; ============================================================================
DEF_FUNC instance_setattr
    push rbx
    push rcx                    ; save value_tag from caller
    mov rbx, rdi
    ; Check if inst_dict is NULL (int subclass instances start with NULL dict)
    mov rdi, [rbx + PyInstanceObject.inst_dict]
    test rdi, rdi
    jnz .sa_have_dict

    ; Allocate a new dict for this instance
    push rsi
    push rdx
    call dict_new
    mov [rbx + PyInstanceObject.inst_dict], rax
    mov rdi, rax
    pop rdx
    pop rsi

.sa_have_dict:
    ; dict_set(inst_dict, name, value, value_tag, key_tag)
    ; rdi = dict (already set), rsi = name, rdx = value
    pop rcx                     ; restore value_tag
    mov r8d, TAG_PTR            ; key_tag (name is always heap string)
    call dict_set

    pop rbx
    leave
    ret
END_FUNC instance_setattr

;; ============================================================================
;; instance_dealloc(PyObject *self)
;; Deallocate an instance: DECREF inst_dict, DECREF ob_type, free self.
;; rdi = instance
;; ============================================================================
DEF_FUNC instance_dealloc
    push rbx

    mov rbx, rdi                ; rbx = self

    ; XDECREF inst_dict (may be NULL for int subclass instances)
    mov rdi, [rbx + PyInstanceObject.inst_dict]
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
    mov rsi, [rbx + PyIntSubclassObject.int_value_tag]
    DECREF_VAL rdi, rsi
.no_int_value:

    ; DECREF ob_type (the class)
    mov rdi, [rbx + PyObject.ob_type]
    call obj_decref

    ; Free the instance
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC instance_dealloc

;; ============================================================================
;; builtin_sub_dealloc(PyObject *self)
;; Dealloc for heap-type subclasses of builtin types (bytes, bytearray, etc.)
;; These don't have inst_dict — just DECREF the type and free.
;; ============================================================================
global builtin_sub_dealloc
DEF_FUNC builtin_sub_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF ob_type (the heap type class)
    mov rdi, [rbx + PyObject.ob_type]
    call obj_decref

    ; Free the object
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC builtin_sub_dealloc

;; ============================================================================
;; instance_repr(PyObject *self) -> PyStrObject*
;; Try __repr__ dunder, fall back to "<instance>".
;; rdi = instance
;; ============================================================================
DEF_FUNC instance_repr
    push rbx
    mov rbx, rdi

    ; Try __repr__ dunder
    extern dunder_repr
    extern dunder_call_1
    lea rsi, [rel dunder_repr]
    ; r12 is callee-saved and still holds eval frame from caller chain
    call dunder_call_1
    test edx, edx
    jnz .done

    ; Fall back to "<instance>"
    lea rdi, [rel instance_repr_cstr]
    call str_from_cstr

.done:
    pop rbx
    leave
    ret
END_FUNC instance_repr

;; ============================================================================
;; instance_str(PyObject *self) -> PyStrObject*
;; Try __str__ dunder, fall back to instance_repr.
;; rdi = instance
;; ============================================================================
DEF_FUNC instance_str
    push rbx
    mov rbx, rdi

    ; Try __str__ dunder
    extern dunder_str
    lea rsi, [rel dunder_str]
    call dunder_call_1
    test edx, edx
    jnz .done

    ; Fall back to instance_repr
    mov rdi, rbx
    call instance_repr

.done:
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
DEF_FUNC type_call
    ; Special case: type(x) with 1 arg when calling type itself
    ; Returns x.__class__ (the type of x)
    lea rax, [rel type_type]
    cmp rdi, rax
    jne .not_type_self
    cmp edx, 1
    jne .not_type_self
    ; type(x) → return type of x
    mov rax, [rsi]          ; args[0] payload
    cmp dword [rsi + 8], TAG_SMALLINT
    je .type_smallint       ; SmallInt → int type
    mov rax, [rax + PyObject.ob_type]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.type_smallint:
    lea rax, [rel int_type]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.not_type_self:
    ; Check if type has its own tp_call (built-in constructor, e.g. staticmethod)
    mov rax, [rdi + PyTypeObject.tp_call]
    test rax, rax
    jz .normal_type_call
    ; Avoid infinite recursion: don't tail-call if tp_call is type_call itself
    lea rcx, [rel type_call]
    cmp rax, rcx
    je .normal_type_call
    ; Tail-call the constructor: tp_call(type, args, nargs)
    leave
    jmp rax

.normal_type_call:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                  ; align to 16 bytes (5 pushes + push rbp = 48, +8 = 56 -> 64)

    mov rbx, rdi                ; rbx = type
    mov r12, rsi                ; r12 = args
    mov r13d, edx               ; r13d = nargs
    movsxd r13, r13d            ; sign-extend to 64 bits

    ; Check if this type inherits from an exception type
    extern type_is_exc_subclass
    mov rdi, rbx
    call type_is_exc_subclass
    test eax, eax
    jnz .exc_subclass_call

    ; Check if this type is an int subclass
    mov rax, [rbx + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jnz .int_subclass_call

    ; Create instance: instance_new(type)
    mov rdi, rbx
    call instance_new
    mov r14, rax                ; r14 = instance

    ; Look up __init__ walking the MRO (type + tp_base chain)
    ; Create "__init__" string for lookup
    lea rdi, [rel init_name_cstr]
    call str_from_cstr
    mov r15, rax                ; r15 = "__init__" str object

    ; Walk MRO: check type->tp_dict, then tp_base chain
    mov rcx, rbx                ; rcx = current type to check
.init_mro_walk:
    mov rdi, [rcx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .init_try_base

    push rcx                    ; save current type
    mov rsi, r15
    mov edx, TAG_PTR
    call dict_get
    pop rcx                     ; restore current type
    test edx, edx
    jnz .init_found

.init_try_base:
    mov rcx, [rcx + PyTypeObject.tp_base]
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

    ; args[0] = instance (payload + tag)
    mov [r15], r14
    mov qword [r15 + 8], TAG_PTR

    ; Copy original args: args[1..nargs] (16-byte stride)
    xor ecx, ecx
.copy_args:
    cmp rcx, r13
    jge .args_copied
    mov rax, rcx
    shl rax, 4                  ; source index * 16
    mov rdx, [r12 + rax]       ; source payload
    mov r8, [r12 + rax + 8]    ; source tag
    lea r9, [rcx + 1]
    shl r9, 4                   ; dest index * 16 (offset by 1 for instance)
    mov [r15 + r9], rdx        ; dest payload
    mov [r15 + r9 + 8], r8    ; dest tag
    inc rcx
    jmp .copy_args
.args_copied:

    ; Get __init__'s tp_call
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .init_not_callable

    ; Call tp_call(__init_func, args_with_instance, nargs+1)
    mov rdi, rbx                ; callable = __init__ func
    mov rsi, r15                ; args ptr
    lea rdx, [r13 + 1]          ; nargs + 1
    call rax

    ; DECREF __init__'s return value (should be None)
    mov rdi, rax
    call obj_decref

    ; Restore stack (undo the sub rsp from args allocation)
    lea rax, [r13 + 1]
    shl rax, 4
    add rsp, rax

.no_init:
    ; Return the instance
    mov rax, r14
    mov edx, TAG_PTR

    add rsp, 8                  ; undo alignment
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
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
    mov edx, TAG_PTR
    add rsp, 8                  ; undo alignment
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.int_subclass_call:
    ; Int subclass: get int value via builtin_int_fn, then wrap in subclass instance
    ; rbx = type, r12 = args, r13 = nargs
    extern builtin_int_fn
    mov rdi, r12                ; args
    mov rsi, r13                ; nargs
    call builtin_int_fn
    ; rax = int result (SmallInt or GMP pointer), edx = tag
    test edx, edx
    jz .int_sub_error           ; exception from builtin_int_fn
    mov r14, rax                ; r14 = int value
    mov r15d, edx               ; r15d = int value tag

    ; If type is exactly int_type, return bare int (not a subclass)
    lea rcx, [rel int_type]
    cmp rbx, rcx
    je .int_sub_return_bare

    ; Allocate PyIntSubclassObject
    push r14                     ; save int_value across malloc
    push r15                     ; save int_value_tag across malloc
    mov edi, PyIntSubclassObject_size
    call ap_malloc
    pop r15
    pop r14
    mov qword [rax + PyIntSubclassObject.ob_refcnt], 1
    mov [rax + PyIntSubclassObject.ob_type], rbx
    mov qword [rax + PyIntSubclassObject.inst_dict], 0
    mov [rax + PyIntSubclassObject.int_value], r14
    mov [rax + PyIntSubclassObject.int_value_tag], r15
    ; INCREF the type (subclass object holds a reference)
    push rax
    mov rdi, rbx
    INCREF rdi
    pop rax
    ; int_value ownership: builtin_int_fn returns a new reference,
    ; we transfer it directly into the subclass object (no INCREF needed).
    jmp .int_sub_done

.int_sub_return_bare:
    mov rax, r14
    mov edx, r15d               ; restore saved tag from builtin_int_fn
    jmp .int_sub_epilogue
.int_sub_done:
    mov edx, TAG_PTR            ; subclass instance is always a heap ptr
.int_sub_epilogue:
    add rsp, 8                  ; undo alignment
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.int_sub_error:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.init_not_callable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__init__ is not callable"
    call raise_exception
    ; does not return
END_FUNC type_call

;; ============================================================================
;; type_getattr(PyTypeObject *self, PyObject *name) -> PyObject*
;; Look up an attribute on a type object itself (class variables).
;; Also handles __name__ (from tp_name) and __bases__.
;; rdi = type object, rsi = name (PyStrObject*)
;; Returns: owned reference to attribute value, or NULL
;; ============================================================================
DEF_FUNC type_getattr
    push rbx
    push r12

    mov rbx, rsi                ; rbx = name
    mov r12, rdi                ; r12 = type

    ; Check for __name__: compare name string data with "__name__"
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [rel tga_name_str]
    call ap_strcmp
    test eax, eax
    jz .tga_return_name

    ; Check type->tp_dict, then walk tp_base chain
.tga_walk:
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tga_next_base

    mov rsi, rbx
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jnz .tga_found

.tga_next_base:
    mov r12, [r12 + PyTypeObject.tp_base]
    test r12, r12
    jnz .tga_walk
    jmp .tga_not_found

.tga_found:
    ; Found — INCREF and return
    mov rbx, rax                ; save payload (name no longer needed)
    mov r12, rdx                ; save tag (type walk done)
    INCREF_VAL rax, edx         ; tag-aware INCREF (skips SmallInt/SmallStr/NULL)
    mov rax, rbx
    mov rdx, r12                ; restore tag from dict_get

    pop r12
    pop rbx
    leave
    ret

.tga_return_name:
    ; Return str from tp_name (C string)
    mov rdi, [r12 + PyTypeObject.tp_name]
    call str_from_cstr
    ; rax = new string (already refcnt=1)
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.tga_not_found:
    xor eax, eax               ; return NULL
    xor edx, edx               ; TAG_NULL
    pop r12
    pop rbx
    leave
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
    call ap_malloc
    mov qword [rax + PyMethodObject.ob_refcnt], 1
    lea rcx, [rel method_type]
    mov [rax + PyMethodObject.ob_type], rcx
    mov [rax + PyMethodObject.im_func], rbx
    mov [rax + PyMethodObject.im_self], r12

    ; INCREF func and self
    push rax
    mov rdi, rbx
    call obj_incref
    mov rdi, r12
    call obj_incref
    pop rax

    pop r12
    pop rbx
    leave
    ret
END_FUNC method_new

;; ============================================================================
;; method_call(self_method, args, nargs) -> PyObject*
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

    ; new_args[0] = im_self (payload + tag)
    mov rcx, [rbx + PyMethodObject.im_self]
    mov [r14], rcx
    mov qword [r14 + 8], TAG_PTR

    ; Copy original args to new_args[1..] (16-byte stride)
    xor ecx, ecx
.mc_copy:
    cmp rcx, r13
    jge .mc_copy_done
    mov rax, rcx
    shl rax, 4                  ; source index * 16
    mov rdx, [r12 + rax]       ; source payload
    mov r8, [r12 + rax + 8]    ; source tag
    lea r9, [rcx + 1]
    shl r9, 4                   ; dest index * 16 (offset by 1 for self)
    mov [r14 + r9], rdx        ; dest payload
    mov [r14 + r9 + 8], r8    ; dest tag
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
    call obj_decref
    mov rdi, rbx
    call ap_free

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
    call func_getattr
    leave
    ret
END_FUNC method_getattr

;; ============================================================================
;; object_type_call(args, nargs) -> PyObject*
;; object() returns a bare instance of object_type
;; ============================================================================
global object_type_call
DEF_FUNC_BARE object_type_call
    ; Create a bare instance with object_type
    push rbp
    mov rbp, rsp
    mov edi, PyInstanceObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel object_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyInstanceObject.inst_dict], 0
    mov edx, TAG_PTR
    pop rbp
    ret
END_FUNC object_type_call

;; ============================================================================
;; Data section
;; ============================================================================
section .data

instance_repr_cstr: db "<instance>", 0
init_name_cstr:     db "__init__", 0
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
    dq 0                        ; tp_dealloc
    dq type_repr                ; tp_repr — <class 'Name'>
    dq type_repr                ; tp_str — same as repr
    dq 0                        ; tp_hash
    dq type_call                ; tp_call — calling a class creates instances
    dq type_getattr             ; tp_getattr — accessing class vars via tp_dict
    dq 0                        ; tp_setattr
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
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

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
    dq 0                        ; tp_call  (set by add_builtin_type)
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
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

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
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
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
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
