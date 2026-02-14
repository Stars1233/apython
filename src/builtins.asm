; builtins.asm - Builtin functions and builtins dict initialization
; Phase 4: print, len, and builtin function wrapper type

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"

section .text

extern dict_new
extern dict_get
extern dict_set
extern str_from_cstr
extern obj_str
extern obj_incref
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
extern range_new
extern int_to_i64
extern init_iter_types
extern obj_repr
extern eval_frame
extern frame_new
extern frame_free
extern fwrite
extern memcpy
extern strlen
extern instance_dealloc
extern instance_repr
extern instance_getattr
extern instance_setattr
extern type_call
extern func_type

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
;; Buffered: builds output in stack buffer, single fwrite() at end
;; ============================================================================
global builtin_print
builtin_print:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4104               ; 4096 byte buffer + 8 alignment

    mov rbx, rdi                ; args array
    mov r12, rsi                ; nargs
    xor r13d, r13d              ; r13 = current arg index
    xor r15d, r15d              ; r15 = buffer write offset

.print_loop:
    cmp r13, r12
    jge .print_flush

    ; Get string representation: obj_str(args[i])
    mov rdi, [rbx + r13 * 8]
    call obj_str
    mov r14, rax                ; r14 = str obj (or NULL)

    test r14, r14
    jz .skip_arg

    ; Get string length from ob_size
    mov rcx, [r14 + PyStrObject.ob_size]

    ; Check if it fits in buffer (need room for data + possible space)
    lea rax, [r15 + rcx + 2]   ; +2 for space and newline
    cmp rax, 4096
    jae .flush_and_write_direct

    ; Copy string data into buffer
    lea rdi, [rbp - 4120 + r15] ; dest = buf + offset
    lea rsi, [r14 + PyStrObject.data]  ; src = str data
    mov rdx, rcx                ; len
    ; Inline small copy (most strings are short)
    test rcx, rcx
    jz .copy_done
    call memcpy wrt ..plt
.copy_done:
    add r15, [r14 + PyStrObject.ob_size]

    ; DECREF the string representation
    mov rdi, r14
    call obj_decref

.skip_arg:
    ; Append space separator if not the last arg
    inc r13
    cmp r13, r12
    jge .print_flush

    mov byte [rbp - 4120 + r15], ' '
    inc r15
    jmp .print_loop

.flush_and_write_direct:
    ; Buffer full - flush what we have, then write this string directly
    ; First flush buffer
    test r15, r15
    jz .write_direct
    lea rdi, [rbp - 4120]       ; buf
    mov rsi, 1                  ; size
    mov rdx, r15                ; count
    mov rax, [rel stdout wrt ..got]
    mov rcx, [rax]              ; FILE *stdout
    call fwrite wrt ..plt
    xor r15d, r15d              ; reset offset

.write_direct:
    ; Write this string directly
    lea rdi, [r14 + PyStrObject.data]
    mov rsi, 1                  ; size
    mov rdx, [r14 + PyStrObject.ob_size]  ; count
    mov rax, [rel stdout wrt ..got]
    mov rcx, [rax]              ; FILE *stdout
    call fwrite wrt ..plt

    ; DECREF the string representation
    mov rdi, r14
    call obj_decref
    jmp .skip_arg

.print_flush:
    ; Append newline
    mov byte [rbp - 4120 + r15], 10
    inc r15

    ; Single fwrite for entire output
    lea rdi, [rbp - 4120]       ; buf
    mov rsi, 1                  ; size
    mov rdx, r15                ; count
    mov rax, [rel stdout wrt ..got]
    mov rcx, [rax]              ; FILE *stdout
    call fwrite wrt ..plt

    ; Return None (with INCREF)
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]

    add rsp, 4104
    pop r15
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
;; builtin_range(PyObject **args, int64_t nargs) -> PyObject*
;; range(stop) or range(start, stop) or range(start, stop, step)
;; ============================================================================
global builtin_range
builtin_range:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; args
    mov r12, rsi               ; nargs

    cmp r12, 1
    je .range_1
    cmp r12, 2
    je .range_2
    cmp r12, 3
    je .range_3

    CSTRING rdi, "TypeError: range expected 1 to 3 arguments"
    call fatal_error

.range_1:
    ; range(stop): start=0, stop=args[0], step=1
    mov rdi, [rbx]
    call int_to_i64
    mov rsi, rax               ; stop
    xor edi, edi               ; start = 0
    mov edx, 1                 ; step = 1
    call range_new
    jmp .range_done

.range_2:
    ; range(start, stop): step=1
    mov rdi, [rbx]
    call int_to_i64
    mov r13, rax               ; start
    mov rdi, [rbx + 8]
    call int_to_i64
    mov rsi, rax               ; stop
    mov rdi, r13               ; start
    mov edx, 1                 ; step = 1
    call range_new
    jmp .range_done

.range_3:
    ; range(start, stop, step)
    mov rdi, [rbx]
    call int_to_i64
    push rax                   ; start
    mov rdi, [rbx + 8]
    call int_to_i64
    push rax                   ; stop
    mov rdi, [rbx + 16]
    call int_to_i64
    mov rdx, rax               ; step
    pop rsi                    ; stop
    pop rdi                    ; start
    call range_new

.range_done:
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; builtin_type(PyObject **args, int64_t nargs) -> PyObject*
;; type(obj) -> returns obj's type
;; ============================================================================
global builtin_type
builtin_type:
    push rbp
    mov rbp, rsp

    cmp rsi, 1
    jne .type_error

    mov rdi, [rdi]             ; obj = args[0]

    ; SmallInt check
    test rdi, rdi
    js .type_smallint

    mov rax, [rdi + PyObject.ob_type]
    INCREF rax

    pop rbp
    ret

.type_smallint:
    extern int_type
    lea rax, [rel int_type]
    INCREF rax
    pop rbp
    ret

.type_error:
    CSTRING rdi, "TypeError: type() takes 1 argument"
    call fatal_error

;; ============================================================================
;; builtin_isinstance(PyObject **args, int64_t nargs) -> PyObject*
;; isinstance(obj, type) -> True/False
;; ============================================================================
global builtin_isinstance
builtin_isinstance:
    push rbp
    mov rbp, rsp

    cmp rsi, 2
    jne .isinstance_error

    mov rdi, [rdi]             ; obj = args[0]
    mov rsi, [rdi + 8]        ; WRONG - need to re-read args
    ; Fix: need original args ptr
    pop rbp
    push rbp
    mov rbp, rsp

    mov rax, [rdi]             ; obj
    mov rcx, [rdi + 8]        ; type_to_check

    extern bool_true
    extern bool_false

    ; Get obj's type (SmallInt-aware)
    test rax, rax
    js .isinstance_smallint
    mov rdx, [rax + PyObject.ob_type]
    jmp .isinstance_check

.isinstance_smallint:
    lea rdx, [rel int_type]

.isinstance_check:
    ; Walk the type chain
    cmp rdx, rcx
    je .isinstance_true

    ; Check base type
    mov rdx, [rdx + PyTypeObject.tp_base]
    test rdx, rdx
    jz .isinstance_false
    cmp rdx, rcx
    je .isinstance_true
    jmp .isinstance_false

.isinstance_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop rbp
    ret

.isinstance_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop rbp
    ret

.isinstance_error:
    CSTRING rdi, "TypeError: isinstance() takes 2 arguments"
    call fatal_error

;; ============================================================================
;; builtin_repr(PyObject **args, int64_t nargs) -> PyObject*
;; repr(obj)
;; ============================================================================
global builtin_repr
builtin_repr:
    push rbp
    mov rbp, rsp

    cmp rsi, 1
    jne .repr_error

    mov rdi, [rdi]
    call obj_repr

    pop rbp
    ret

.repr_error:
    CSTRING rdi, "TypeError: repr() takes 1 argument"
    call fatal_error

;; ============================================================================
;; builtin___build_class__(PyObject **args, int64_t nargs) -> PyObject*
;; __build_class__(body_func, class_name, *bases)
;;
;; 1. body_func = args[0], class_name = args[1]
;; 2. Create a class dict
;; 3. Execute body_func with class_dict as locals
;; 4. Create a new type object with class_dict as tp_dict
;; 5. Return the new type
;; ============================================================================
global builtin___build_class__
builtin___build_class__:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8              ; align stack (5 pushes + push rbp = 48, +8 = 56 -> 64)

    ; Check nargs >= 2
    cmp rsi, 2
    jl .build_class_error

    mov rbx, rdi            ; rbx = args
    ; r12 will be used later for the type object

    mov r13, [rbx]          ; r13 = body_func (args[0])
    mov r14, [rbx + 8]      ; r14 = class_name (args[1])

    ; Create class dict (will become tp_dict)
    call dict_new
    mov r15, rax            ; r15 = class_dict

    ; Execute body function with class_dict as locals
    ; frame_new(code, globals, builtins, locals)
    mov rdi, [r13 + PyFuncObject.func_code]     ; code from body func
    mov rsi, [r13 + PyFuncObject.func_globals]  ; globals from body func
    mov rdx, [rel builtins_dict_global]         ; builtins dict
    mov rcx, r15                                ; class_dict as locals
    call frame_new
    mov r12, rax            ; r12 = new frame

    ; eval_frame(frame)
    mov rdi, r12
    call eval_frame
    ; DECREF return value (should be None)
    mov rdi, rax
    call obj_decref

    ; Free the frame
    mov rdi, r12
    call frame_free

    ; Allocate the type object
    mov edi, TYPE_OBJECT_SIZE
    call ap_malloc
    mov r12, rax            ; r12 = new type object

    ; Zero-fill the type object
    mov rdi, r12
    xor eax, eax
    mov ecx, TYPE_OBJECT_SIZE / 8
    rep stosq

    ; Fill type fields
    mov qword [r12 + PyTypeObject.ob_refcnt], 1

    ; ob_type = self (type is its own metatype, so tp_call can be found)
    mov [r12 + PyTypeObject.ob_type], r12

    ; tp_name: point to class_name string's data area
    lea rax, [r14 + PyStrObject.data]
    mov [r12 + PyTypeObject.tp_name], rax

    mov qword [r12 + PyTypeObject.tp_basicsize], PyInstanceObject_size

    ; Wire instance methods
    lea rax, [rel instance_dealloc]
    mov [r12 + PyTypeObject.tp_dealloc], rax

    lea rax, [rel instance_repr]
    mov [r12 + PyTypeObject.tp_repr], rax
    mov [r12 + PyTypeObject.tp_str], rax

    lea rax, [rel type_call]
    mov [r12 + PyTypeObject.tp_call], rax

    lea rax, [rel instance_getattr]
    mov [r12 + PyTypeObject.tp_getattr], rax

    lea rax, [rel instance_setattr]
    mov [r12 + PyTypeObject.tp_setattr], rax

    ; tp_dict = class_dict
    mov [r12 + PyTypeObject.tp_dict], r15

    ; INCREF class_dict (type holds a reference)
    mov rdi, r15
    call obj_incref

    ; INCREF class_name (type object refers to it via tp_name)
    mov rdi, r14
    call obj_incref

    ; Look up "__init__" in class_dict for tp_init
    lea rdi, [rel .bc_init_name]
    call str_from_cstr
    push rax                ; save __init__ str obj

    mov rdi, r15            ; class_dict
    mov rsi, rax            ; "__init__" str
    call dict_get
    mov rbx, rax            ; rbx = __init__ func or NULL

    ; DECREF the "__init__" string
    pop rdi
    call obj_decref

    ; Store tp_init (func ptr or 0)
    mov [r12 + PyTypeObject.tp_init], rbx

    ; Return the new type object
    mov rax, r12

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.build_class_error:
    CSTRING rdi, "TypeError: __build_class__ requires 2+ arguments"
    call fatal_error

section .rodata
.bc_init_name: db "__init__", 0
section .text

;; ============================================================================
;; Helper: add_builtin(dict, name_cstr, func_ptr)
;; Adds a builtin to the dict. Used by builtins_init.
;; rdi=dict, rsi=name_cstr, rdx=func_ptr
;; ============================================================================
add_builtin:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; dict
    mov r12, rsi               ; name_cstr
    mov r13, rdx               ; func_ptr

    ; Create function wrapper
    mov rdi, r13
    mov rsi, r12
    call builtin_func_new
    push rax                   ; save func obj

    ; Create key string
    mov rdi, r12
    call str_from_cstr
    push rax                   ; save key

    ; dict_set
    mov rdi, rbx
    mov rsi, rax               ; key
    mov rdx, [rsp + 8]        ; func obj
    call dict_set

    ; DECREF key and value
    pop rdi                    ; key
    call obj_decref
    pop rdi                    ; func obj
    call obj_decref

    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; builtins_init() -> PyDictObject*
;; Create and populate the builtins dictionary
;; ============================================================================
global builtins_init
builtins_init:
    push rbp
    mov rbp, rsp
    push rbx

    ; Initialize iterator types (patches list/tuple tp_iter)
    call init_iter_types

    ; Create the builtins dict
    call dict_new
    mov rbx, rax                ; rbx = builtins dict

    ; Store globally for __build_class__ to access
    mov [rel builtins_dict_global], rbx

    ; Create __build_class__ wrapper and store globally
    lea rdi, [rel builtin___build_class__]
    lea rsi, [rel .name_build_class]
    call builtin_func_new
    mov [rel build_class_obj], rax

    ; Register __build_class__ in builtins dict
    mov rdi, rbx
    lea rsi, [rel .name_build_class]
    lea rdx, [rel builtin___build_class__]
    call add_builtin

    ; Add builtins using helper
    mov rdi, rbx
    lea rsi, [rel .name_print]
    lea rdx, [rel builtin_print]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel .name_len]
    lea rdx, [rel builtin_len]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel .name_range]
    lea rdx, [rel builtin_range]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel .name_type]
    lea rdx, [rel builtin_type]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel .name_isinstance]
    lea rdx, [rel builtin_isinstance]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel .name_repr]
    lea rdx, [rel builtin_repr]
    call add_builtin

    ; Return builtins dict
    mov rax, rbx

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata

.name_print:        db "print", 0
.name_len:          db "len", 0
.name_range:        db "range", 0
.name_type:         db "type", 0
.name_isinstance:   db "isinstance", 0
.name_repr:         db "repr", 0
.name_build_class:  db "__build_class__", 0

section .data

global builtins_dict_global
builtins_dict_global: dq 0

global build_class_obj
build_class_obj: dq 0

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
