; object.asm - PyObject base operations
; Allocation, reference counting, type dispatch for repr/str/hash/bool
; SmallInt-aware: all functions check bit 63 for tagged pointers

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern sys_write
extern str_from_cstr
extern none_singleton
extern bool_false
extern int_repr
extern int_type

; obj_alloc(size_t size, PyTypeObject *type) -> PyObject*
; Allocate a new object with refcount=1 and given type
global obj_alloc
obj_alloc:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    mov rbx, rdi            ; size
    mov r12, rsi            ; type

    mov rdi, rbx
    call ap_malloc

    ; Initialize header
    mov qword [rax + PyObject.ob_refcnt], 1
    mov [rax + PyObject.ob_type], r12

    pop r12
    pop rbx
    pop rbp
    ret

; obj_incref(PyObject *obj)
; Increment reference count; NULL-safe, SmallInt-safe
global obj_incref
obj_incref:
    test rdi, rdi
    jz .skip
    js .skip                ; SmallInt - no refcount
    inc qword [rdi + PyObject.ob_refcnt]
.skip:
    ret

; obj_decref(PyObject *obj)
; Decrement reference count; deallocate if zero; NULL-safe, SmallInt-safe
global obj_decref
obj_decref:
    test rdi, rdi
    jz .skip
    js .skip                ; SmallInt - no refcount
    dec qword [rdi + PyObject.ob_refcnt]
    jnz .skip
    ; refcount hit zero - deallocate
    jmp obj_dealloc
.skip:
    ret

; obj_dealloc(PyObject *obj)
; Calls type's tp_dealloc if present, else just frees
global obj_dealloc
obj_dealloc:
    ; Guard against SmallInt (should never happen)
    test rdi, rdi
    js .bail

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; Get type's tp_dealloc
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .just_free
    mov rax, [rax + PyTypeObject.tp_dealloc]
    test rax, rax
    jz .just_free

    ; Call tp_dealloc(obj)
    mov rdi, rbx
    call rax
    pop rbx
    pop rbp
    ret

.just_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    pop rbp
.bail:
    ret

; obj_repr(PyObject *obj) -> PyObject* (string)
; Calls type's tp_repr. SmallInt → int_repr.
global obj_repr
obj_repr:
    push rbp
    mov rbp, rsp

    test rdi, rdi
    jz .null_obj
    js .smallint

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .no_repr
    mov rax, [rax + PyTypeObject.tp_repr]
    test rax, rax
    jz .no_repr

    ; tail-call tp_repr(obj)
    pop rbp
    jmp rax

.smallint:
    ; Delegate to int_repr which handles SmallInts
    call int_repr
    pop rbp
    ret

.null_obj:
.no_repr:
    xor eax, eax
    pop rbp
    ret

; obj_str(PyObject *obj) -> PyObject* (string)
; Calls type's tp_str, falls back to tp_repr. SmallInt → int_repr.
global obj_str
obj_str:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    test rdi, rdi
    jz .fallback
    js .smallint

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .fallback

    mov rax, [rax + PyTypeObject.tp_str]
    test rax, rax
    jz .fallback

    mov rdi, rbx
    call rax
    pop rbx
    pop rbp
    ret

.smallint:
    ; SmallInt: delegate to int_repr
    mov rdi, rbx
    call int_repr
    pop rbx
    pop rbp
    ret

.fallback:
    mov rdi, rbx
    call obj_repr
    pop rbx
    pop rbp
    ret

; obj_hash(PyObject *obj) -> int64
; Calls type's tp_hash, falls back to address hash. SmallInt → decoded value.
global obj_hash
obj_hash:
    push rbp
    mov rbp, rsp

    test rdi, rdi
    jz .default_hash
    js .smallint_hash

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .default_hash
    mov rax, [rax + PyTypeObject.tp_hash]
    test rax, rax
    jz .default_hash

    ; tail-call tp_hash(obj)
    pop rbp
    jmp rax

.smallint_hash:
    ; Hash of SmallInt = decoded value (avoid -1)
    mov rax, rdi
    shl rax, 1
    sar rax, 1              ; sign-extend from bit 62
    cmp rax, -1
    jne .hash_done
    mov rax, -2
.hash_done:
    pop rbp
    ret

.default_hash:
    ; Default: hash is the object address
    mov rax, rdi
    pop rbp
    ret

; obj_is_true(PyObject *obj) -> int (0 or 1)
; Tests truthiness of an object. SmallInt → decoded value != 0.
global obj_is_true
obj_is_true:
    ; Check SmallInt before any stack setup
    test rdi, rdi
    js .smallint

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; None is false
    lea rax, [rel none_singleton]
    cmp rbx, rax
    je .false

    ; bool False is false
    lea rax, [rel bool_false]
    cmp rbx, rax
    je .false

    ; Check for nb_bool in type's number methods
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .true
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .check_seq_len
    mov rax, [rax + PyNumberMethods.nb_bool]
    test rax, rax
    jz .check_seq_len
    mov rdi, rbx
    call rax
    pop rbx
    pop rbp
    ret

.check_seq_len:
    ; Check for sq_length in type's sequence methods
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .check_map_len
    mov rax, [rax + PySequenceMethods.sq_length]
    test rax, rax
    jz .check_map_len
    mov rdi, rbx
    call rax
    test rax, rax
    jnz .true
    jmp .false

.check_map_len:
    ; Check for mp_length in type's mapping methods
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .true                ; default: objects are truthy
    mov rax, [rax + PyMappingMethods.mp_length]
    test rax, rax
    jz .true
    mov rdi, rbx
    call rax
    test rax, rax
    jnz .true

.false:
    xor eax, eax
    pop rbx
    pop rbp
    ret

.true:
    mov eax, 1
    pop rbx
    pop rbp
    ret

.smallint:
    ; SmallInt is true iff decoded value != 0
    ; SmallInt 0 = 0x8000000000000000 (only tag bit set)
    mov rax, rdi
    shl rax, 1              ; shift out tag bit
    test rax, rax            ; zero if value was 0
    setnz al
    movzx eax, al
    ret

; obj_print(PyObject *obj)
; Print an object's string representation to stdout followed by newline
global obj_print
obj_print:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; Get string representation via obj_str (handles SmallInt)
    call obj_str
    test rax, rax
    jz .print_null

    mov rbx, rax            ; rbx = str obj

    ; sys_write(1, str_data, ob_size)
    mov edi, 1
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, [rbx + PyStrObject.ob_size]
    call sys_write

    ; sys_write(1, "\n", 1)
    mov edi, 1
    lea rsi, [rel obj_print_newline]
    mov edx, 1
    call sys_write

    pop rbx
    pop rbp
    ret

.print_null:
    ; sys_write(1, "<NULL>\n", 7)
    mov edi, 1
    lea rsi, [rel obj_print_null_str]
    mov edx, 7
    call sys_write

    pop rbx
    pop rbp
    ret

section .rodata
obj_print_newline: db 10
obj_print_null_str: db "<NULL>", 10
