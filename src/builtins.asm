; builtins.asm - Builtin functions and builtins dict initialization
; Phase 4: print, len, and builtin function wrapper type

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"

section .text

extern dict_new
extern dict_set
extern str_from_cstr
extern obj_str
extern obj_decref
extern obj_dealloc
extern none_singleton
extern int_from_i64
extern str_type
extern ap_malloc
extern ap_free
extern fatal_error
extern fputs
extern putchar
extern stdout

;; ============================================================================
;; builtin_func_new(void *func_ptr, const char *name_cstr) -> PyBuiltinObject*
;; Create a new builtin function wrapper object
;; ============================================================================
global builtin_func_new
builtin_func_new:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, rdi                ; func_ptr
    mov r12, rsi                ; name_cstr

    ; Create a string object for the name
    mov rdi, r12
    call str_from_cstr
    mov r13, rax                ; r13 = name string object

    ; Allocate PyBuiltinObject
    mov edi, PyBuiltinObject_size
    call ap_malloc
    ; rax = new object

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel builtin_func_type]
    mov [rax + PyObject.ob_type], rcx

    ; Fill builtin-specific fields
    mov qword [rax + PyBuiltinObject.func_id], 0   ; not used for func_ptr dispatch
    mov [rax + PyBuiltinObject.func_name], r13
    mov [rax + PyBuiltinObject.func_ptr], rbx

    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; builtin_func_call(PyObject *self, PyObject **args, int64_t nargs) -> PyObject*
;; Dispatch to the underlying C function: func_ptr(args, nargs)
;; ============================================================================
global builtin_func_call
builtin_func_call:
    ; self = rdi, args = rsi, nargs = rdx
    ; Extract func_ptr from self
    mov rax, [rdi + PyBuiltinObject.func_ptr]
    ; Call func_ptr(args, nargs)
    mov rdi, rsi                ; args
    mov rsi, rdx                ; nargs
    jmp rax                     ; tail call

;; ============================================================================
;; builtin_func_dealloc(PyObject *self)
;; Free the builtin function wrapper
;; ============================================================================
builtin_func_dealloc:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; DECREF the name string
    mov rdi, [rbx + PyBuiltinObject.func_name]
    test rdi, rdi
    jz .no_name
    call obj_decref
.no_name:

    ; Free the object
    mov rdi, rbx
    call ap_free

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; builtin_func_repr(PyObject *self) -> PyObject*
;; Returns "<built-in function NAME>"
;; ============================================================================
builtin_func_repr:
    push rbp
    mov rbp, rsp

    ; For simplicity, just return the name string with INCREF
    mov rax, [rdi + PyBuiltinObject.func_name]
    test rax, rax
    jz .fallback
    inc qword [rax + PyObject.ob_refcnt]
    pop rbp
    ret

.fallback:
    lea rdi, [rel .unknown_str]
    call str_from_cstr
    pop rbp
    ret

section .rodata
.unknown_str: db "<built-in function>", 0
section .text

;; ============================================================================
;; builtin_print(PyObject **args, int64_t nargs) -> PyObject*
;; Print each arg separated by spaces, followed by newline
;; ============================================================================
global builtin_print
builtin_print:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; args array
    mov r12, rsi                ; nargs
    xor r13d, r13d              ; r13 = current index

.print_loop:
    cmp r13, r12
    jge .print_newline

    ; Get string representation: obj_str(args[i])
    mov rdi, [rbx + r13 * 8]
    call obj_str
    mov r14, rax                ; r14 = str obj (or NULL)

    test r14, r14
    jz .skip_arg

    ; Print the string data using fputs(str, stdout)
    lea rdi, [r14 + PyStrObject.data]
    mov rax, [rel stdout wrt ..got]
    mov rsi, [rax]              ; FILE *stdout
    call fputs wrt ..plt

    ; DECREF the string representation
    mov rdi, r14
    call obj_decref

.skip_arg:
    ; Print space separator if not the last arg
    inc r13
    cmp r13, r12
    jge .print_newline

    mov edi, ' '
    call putchar wrt ..plt
    jmp .print_loop

.print_newline:
    mov edi, 10                 ; '\n'
    call putchar wrt ..plt

    ; Return None (with INCREF)
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]

    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; builtin_len(PyObject **args, int64_t nargs) -> PyObject*
;; Returns len() of the first argument
;; Phase 4 stub: checks ob_size for variable-size objects
;; ============================================================================
global builtin_len
builtin_len:
    push rbp
    mov rbp, rsp
    push rbx

    ; Check nargs == 1
    cmp rsi, 1
    jne .len_error

    mov rbx, [rdi]              ; rbx = args[0]

    ; Check if the object has a mapping mp_length
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .len_error

    ; Try tp_as_mapping->mp_length first
    mov rcx, [rax + PyTypeObject.tp_as_mapping]
    test rcx, rcx
    jz .try_sequence
    mov rcx, [rcx + PyMappingMethods.mp_length]
    test rcx, rcx
    jz .try_sequence
    mov rdi, rbx
    call rcx
    jmp .make_int

.try_sequence:
    ; Try tp_as_sequence->sq_length
    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_as_sequence]
    test rcx, rcx
    jz .try_ob_size
    mov rcx, [rcx + PySequenceMethods.sq_length]
    test rcx, rcx
    jz .try_ob_size
    mov rdi, rbx
    call rcx
    jmp .make_int

.try_ob_size:
    ; Fallback: read ob_size at PyVarObject offset +16
    ; This works for strings, tuples, lists, dicts, bytes
    mov rax, [rbx + PyVarObject.ob_size]

.make_int:
    ; rax = length; create an int object
    mov rdi, rax
    call int_from_i64

    pop rbx
    pop rbp
    ret

.len_error:
    lea rdi, [rel .len_err_msg]
    call fatal_error

section .rodata
.len_err_msg: db "TypeError: len() takes exactly one argument", 0
section .text

;; ============================================================================
;; builtins_init() -> PyDictObject*
;; Create and populate the builtins dictionary
;; ============================================================================
global builtins_init
builtins_init:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    ; Create the builtins dict
    call dict_new
    mov rbx, rax                ; rbx = builtins dict

    ; --- Add "print" builtin ---
    ; Create builtin function wrapper
    lea rdi, [rel builtin_print]
    lea rsi, [rel .name_print]
    call builtin_func_new
    mov r12, rax                ; r12 = print func obj

    ; Create key string "print"
    lea rdi, [rel .name_print]
    call str_from_cstr
    push rax                    ; save key

    ; dict_set(dict, key, value)
    mov rdi, rbx
    mov rsi, rax                ; key
    mov rdx, r12                ; value
    call dict_set

    ; DECREF key and value (dict_set did INCREF on both)
    pop rdi                     ; key
    call obj_decref
    mov rdi, r12                ; value
    call obj_decref

    ; --- Add "len" builtin ---
    lea rdi, [rel builtin_len]
    lea rsi, [rel .name_len]
    call builtin_func_new
    mov r12, rax                ; r12 = len func obj

    lea rdi, [rel .name_len]
    call str_from_cstr
    push rax                    ; save key

    mov rdi, rbx
    mov rsi, rax                ; key
    mov rdx, r12                ; value
    call dict_set

    pop rdi                     ; key
    call obj_decref
    mov rdi, r12                ; value
    call obj_decref

    ; Return builtins dict
    mov rax, rbx

    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata

.name_print: db "print", 0
.name_len:   db "len", 0

section .data

builtin_func_name_str: db "builtin_function_or_method", 0

; Builtin function type object
align 8
global builtin_func_type
builtin_func_type:
    dq 1                        ; ob_refcnt (immortal)
    dq 0                        ; ob_type
    dq builtin_func_name_str    ; tp_name
    dq PyBuiltinObject_size     ; tp_basicsize
    dq builtin_func_dealloc     ; tp_dealloc
    dq builtin_func_repr        ; tp_repr
    dq builtin_func_repr        ; tp_str
    dq 0                        ; tp_hash
    dq builtin_func_call        ; tp_call
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
