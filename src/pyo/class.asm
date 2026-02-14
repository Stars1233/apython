; class_obj.asm - Class instances and bound methods for apython
; Phase 10: class instantiation, attribute access, __init__ dispatch

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_incref
extern obj_dealloc
extern dict_new
extern dict_get
extern dict_set
extern str_from_cstr
extern fatal_error
extern raise_exception
extern exc_AttributeError_type
extern exc_TypeError_type
extern func_type
extern eval_frame
extern frame_new
extern frame_free

;; ============================================================================
;; instance_new(PyTypeObject *type) -> PyInstanceObject*
;; Allocate a new instance of the given class type.
;; rdi = type (the class)
;; Returns: new instance with refcnt=1, ob_type=type, inst_dict=new dict
;; ============================================================================
global instance_new
instance_new:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

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
global instance_getattr
instance_getattr:
    push rbp
    mov rbp, rsp
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

    ; Not in inst_dict -- check type->tp_dict
    mov rax, [rbx + PyObject.ob_type]   ; rax = type (the class)
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .not_found

    mov rsi, r12
    call dict_get
    test rax, rax
    jz .not_found

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
    pop rbp
    ret

.not_found:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "attribute not found"
    call raise_exception
    ; does not return

;; ============================================================================
;; instance_setattr(PyInstanceObject *self, PyObject *name, PyObject *value)
;; Set an attribute on an instance's __dict__.
;; rdi = instance, rsi = name, rdx = value
;; ============================================================================
global instance_setattr
instance_setattr:
    push rbp
    mov rbp, rsp

    ; dict_set(self->inst_dict, name, value)
    mov rdi, [rdi + PyInstanceObject.inst_dict]
    ; rsi = name (already set)
    ; rdx = value (already set)
    call dict_set

    pop rbp
    ret

;; ============================================================================
;; instance_dealloc(PyObject *self)
;; Deallocate an instance: DECREF inst_dict, DECREF ob_type, free self.
;; rdi = instance
;; ============================================================================
global instance_dealloc
instance_dealloc:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

;; ============================================================================
;; instance_repr(PyObject *self) -> PyStrObject*
;; Return a simple "<instance>" string.
;; rdi = instance
;; ============================================================================
global instance_repr
instance_repr:
    lea rdi, [rel instance_repr_cstr]
    jmp str_from_cstr

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
global type_call
type_call:
    push rbp
    mov rbp, rsp
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

    ; Look up __init__ in type->tp_dict
    mov rdi, [rbx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .no_init

    ; Create "__init__" string for lookup
    lea rdi, [rel init_name_cstr]
    call str_from_cstr
    mov r15, rax                ; r15 = "__init__" str object

    ; dict_get(type->tp_dict, "__init__" str)
    mov rdi, [rbx + PyTypeObject.tp_dict]
    mov rsi, r15
    call dict_get
    mov rbx, rax                ; rbx = __init__ func or NULL

    ; DECREF the "__init__" string (no longer needed)
    mov rdi, r15
    call obj_decref

    ; If __init__ not found, skip calling it
    test rbx, rbx
    jz .no_init

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

    ; Restore r12 to the caller's eval frame (saved at [rbp-16] by prologue)
    ; so that func_call can read builtins from it correctly.
    mov r12, [rbp - 16]

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
    pop rbp
    ret

.init_not_callable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__init__ is not callable"
    call raise_exception
    ; does not return

;; ============================================================================
;; Data section
;; ============================================================================
section .data

instance_repr_cstr: db "<instance>", 0
init_name_cstr:     db "__init__", 0
method_name_str:    db "method", 0

; method_type - placeholder type descriptor for bound methods
; (Not used in current implementation; method binding uses CALL null_or_self)
align 8
global method_type
method_type:
    dq 1                        ; ob_refcnt (immortal)
    dq 0                        ; ob_type
    dq method_name_str          ; tp_name
    dq PyMethodObject_size      ; tp_basicsize
    dq 0                        ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
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
