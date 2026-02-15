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
;; 1. Check self->inst_dict
;; 2. If not found, check type->tp_dict
;; 3. If found and it's a function, return it directly (caller handles binding)
;; 4. If found, INCREF and return
;; 5. If not found, fatal AttributeError
;;
;; rdi = instance, rsi = name (PyStrObject*)
;; Returns: borrowed-turned-owned reference to attribute value
;; ============================================================================
DEF_FUNC instance_getattr
    push rbx
    push r12
    push r13

    mov rbx, rdi                ; rbx = self (instance)
    mov r12, rsi                ; r12 = name

    ; Check self->inst_dict first
    mov rdi, [rbx + PyInstanceObject.inst_dict]
    mov rsi, r12
    call dict_get
    test rax, rax
    jnz .found

    ; Not in inst_dict -- walk type MRO: check type->tp_dict, then tp_base chain
    mov rcx, [rbx + PyObject.ob_type]   ; rcx = type (the class)
.walk_mro:
    mov rdi, [rcx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .try_base

    push rcx                            ; save current type
    mov rsi, r12
    call dict_get
    pop rcx                             ; restore current type
    test rax, rax
    jnz .found                          ; found in this type's dict

.try_base:
    mov rcx, [rcx + PyTypeObject.tp_base]
    test rcx, rcx
    jnz .walk_mro

    jmp .not_found

    ; Found in type dict. Return it (INCREF below).
    ; (If it's a function, the caller/LOAD_ATTR handles method binding.)

.found:
    ; INCREF the result (dict_get returns borrowed ref)
    mov r13, rax
    mov rdi, rax
    call obj_incref
    mov rax, r13

    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    xor eax, eax               ; return NULL (caller handles raise)
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

    ; dict_set(self->inst_dict, name, value)
    mov rdi, [rdi + PyInstanceObject.inst_dict]
    ; rsi = name (already set)
    ; rdx = value (already set)
    call dict_set

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

    ; DECREF inst_dict
    mov rdi, [rbx + PyInstanceObject.inst_dict]
    call obj_decref

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
    test rax, rax
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
    test rax, rax
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
    ; Check if type has its own tp_call (built-in constructor, e.g. staticmethod)
    mov rax, [rdi + PyTypeObject.tp_call]
    test rax, rax
    jz .normal_type_call
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
    call dict_get
    pop rcx                     ; restore current type
    test rax, rax
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
    ; Allocate (nargs+1)*8 bytes on the stack
    lea rax, [r13 + 1]
    shl rax, 3                  ; (nargs+1) * 8
    sub rsp, rax                ; allocate on stack
    mov r15, rsp                ; r15 = new args array

    ; args[0] = instance
    mov [r15], r14

    ; Copy original args: args[1..nargs]
    xor ecx, ecx
.copy_args:
    cmp rcx, r13
    jge .args_copied
    mov rax, [r12 + rcx*8]
    mov [r15 + rcx*8 + 8], rax
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
    shl rax, 3
    add rsp, rax

.no_init:
    ; Return the instance
    mov rax, r14

    add rsp, 8                  ; undo alignment
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
    call dict_get
    test rax, rax
    jnz .tga_found

.tga_next_base:
    mov r12, [r12 + PyTypeObject.tp_base]
    test r12, r12
    jnz .tga_walk
    jmp .tga_not_found

.tga_found:
    ; Found — INCREF and return
    mov rbx, rax
    mov rdi, rax
    call obj_incref
    mov rax, rbx

    pop r12
    pop rbx
    leave
    ret

.tga_return_name:
    ; Return str from tp_name (C string)
    mov rdi, [r12 + PyTypeObject.tp_name]
    call str_from_cstr
    ; rax = new string (already refcnt=1)
    pop r12
    pop rbx
    leave
    ret

.tga_not_found:
    xor eax, eax               ; return NULL
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

    ; Allocate new args array: (nargs+1) * 8
    lea rdi, [rdx + 1]
    shl rdi, 3
    call ap_malloc
    mov r14, rax                ; new args array

    ; new_args[0] = im_self
    mov rcx, [rbx + PyMethodObject.im_self]
    mov [r14], rcx

    ; Copy original args to new_args[1..]
    xor ecx, ecx
.mc_copy:
    cmp rcx, r13
    jge .mc_copy_done
    mov rax, [r12 + rcx*8]
    mov [r14 + rcx*8 + 8], rax
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
    push rax                    ; save result

    ; Free temp args array
    mov rdi, r14
    call ap_free

    pop rax
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
;; Data section
;; ============================================================================
section .data

instance_repr_cstr: db "<instance>", 0
init_name_cstr:     db "__init__", 0
tga_name_str:       db "__name__", 0
method_name_str:    db "method", 0
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
