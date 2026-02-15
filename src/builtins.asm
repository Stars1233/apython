; builtins.asm - Builtin functions and builtins dict initialization
; Phase 4: print, len, and builtin function wrapper type

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"

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
extern raise_exception
extern sys_write
extern range_new
extern int_to_i64
extern init_iter_types
extern obj_repr
extern eval_frame
extern frame_new
extern frame_free
extern ap_memcpy
extern instance_dealloc
extern instance_repr
extern instance_getattr
extern instance_setattr
extern type_call
extern user_type_metatype
extern super_type
extern staticmethod_type
extern classmethod_type
extern property_type
extern func_type
extern type_type

; New builtin function implementations (in builtins_extra.asm)
extern builtin_abs
extern builtin_int_fn
extern builtin_str_fn
extern builtin_ord
extern builtin_chr
extern builtin_hex
extern builtin_id
extern builtin_hash_fn
extern builtin_callable
extern builtin_iter_fn
extern builtin_next_fn
extern builtin_any
extern builtin_all
extern builtin_sum
extern builtin_min
extern builtin_max
extern builtin_getattr
extern builtin_hasattr
extern builtin_setattr

; Iterator builtins (in itertools.asm)
extern builtin_enumerate
extern builtin_zip
extern builtin_map
extern builtin_filter
extern builtin_reversed
extern builtin_sorted
extern builtin_globals
extern builtin_locals

; Exception types
extern exc_BaseException_type
extern exc_Exception_type
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_KeyError_type
extern exc_IndexError_type
extern exc_AttributeError_type
extern exc_NameError_type
extern exc_RuntimeError_type
extern exc_StopIteration_type
extern exc_ZeroDivisionError_type
extern exc_ImportError_type
extern exc_NotImplementedError_type
extern exc_OverflowError_type
extern exc_AssertionError_type
extern exc_OSError_type
extern exc_LookupError_type
extern exc_ArithmeticError_type
extern exc_RecursionError_type
extern exc_MemoryError_type
extern exc_KeyboardInterrupt_type
extern exc_SystemExit_type
extern exc_UnicodeError_type

;; ============================================================================
;; builtin_func_new(void *func_ptr, const char *name_cstr) -> PyBuiltinObject*
;; Create a new builtin function wrapper object
;; ============================================================================
DEF_FUNC builtin_func_new
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
    leave
    ret
END_FUNC builtin_func_new

;; ============================================================================
;; builtin_func_call(PyObject *self, PyObject **args, int64_t nargs) -> PyObject*
;; Dispatch to the underlying C function: func_ptr(args, nargs)
;; ============================================================================
DEF_FUNC_BARE builtin_func_call
    ; self = rdi, args = rsi, nargs = rdx
    ; Extract func_ptr from self
    mov rax, [rdi + PyBuiltinObject.func_ptr]
    ; Call func_ptr(args, nargs)
    mov rdi, rsi                ; args
    mov rsi, rdx                ; nargs
    jmp rax                     ; tail call
END_FUNC builtin_func_call

;; ============================================================================
;; builtin_func_dealloc(PyObject *self)
;; Free the builtin function wrapper
;; ============================================================================
DEF_FUNC_LOCAL builtin_func_dealloc
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
    leave
    ret
END_FUNC builtin_func_dealloc

;; ============================================================================
;; builtin_func_repr(PyObject *self) -> PyObject*
;; Returns "<built-in function NAME>"
;; ============================================================================
DEF_FUNC_LOCAL builtin_func_repr

    ; For simplicity, just return the name string with INCREF
    mov rax, [rdi + PyBuiltinObject.func_name]
    test rax, rax
    jz .fallback
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.fallback:
    lea rdi, [rel builtin_func_repr_unknown_str]
    call str_from_cstr
    leave
    ret
END_FUNC builtin_func_repr

section .rodata
builtin_func_repr_unknown_str: db "<built-in function>", 0
section .text

;; ============================================================================
;; builtin_print(PyObject **args, int64_t nargs) -> PyObject*
;; Print each arg separated by spaces, followed by newline
;; Buffered: builds output in stack buffer, single fwrite() at end
;; ============================================================================
DEF_FUNC builtin_print
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

align 16
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
    call ap_memcpy
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
    mov edi, 1                  ; fd = stdout
    lea rsi, [rbp - 4120]      ; buf
    mov rdx, r15                ; len
    call sys_write
    xor r15d, r15d              ; reset offset

.write_direct:
    ; Write this string directly
    mov edi, 1                  ; fd = stdout
    lea rsi, [r14 + PyStrObject.data]
    mov rdx, [r14 + PyStrObject.ob_size]  ; len
    call sys_write

    ; DECREF the string representation
    mov rdi, r14
    call obj_decref
    jmp .skip_arg

.print_flush:
    ; Append newline
    mov byte [rbp - 4120 + r15], 10
    inc r15

    ; Single sys_write for entire output
    mov edi, 1                  ; fd = stdout
    lea rsi, [rbp - 4120]      ; buf
    mov rdx, r15                ; len
    call sys_write

    ; Return None (with INCREF)
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]

    add rsp, 4104
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC builtin_print

;; ============================================================================
;; builtin_len(PyObject **args, int64_t nargs) -> PyObject*
;; Returns len() of the first argument
;; Phase 4 stub: checks ob_size for variable-size objects
;; ============================================================================
DEF_FUNC builtin_len
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
    jz .try_dunder_len
    mov rcx, [rcx + PySequenceMethods.sq_length]
    test rcx, rcx
    jz .try_dunder_len
    mov rdi, rbx
    call rcx
    jmp .make_int

.try_dunder_len:
    ; Try __len__ dunder on heaptype
    mov rax, [rbx + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .try_ob_size

    extern dunder_len
    extern dunder_call_1
    mov rdi, rbx
    lea rsi, [rel dunder_len]
    call dunder_call_1
    test rax, rax
    jz .try_ob_size

    ; __len__ returned a result — extract integer value
    push rax                ; save result for DECREF
    ; Check if SmallInt
    test rax, rax
    js .len_smallint
    ; Heap int — read value (assume fits in 64 bits)
    extern int_to_i64
    mov rdi, rax
    call int_to_i64
    pop rdi                 ; DECREF the int result
    push rax                ; save extracted value
    call obj_decref
    pop rax
    jmp .make_int

.len_smallint:
    ; Decode SmallInt
    shl rax, 1
    sar rax, 1
    add rsp, 8              ; discard saved (SmallInt, no DECREF needed)
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
    leave
    ret

.len_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "len() takes exactly one argument"
    call raise_exception
END_FUNC builtin_len

;; ============================================================================
;; builtin_range(PyObject **args, int64_t nargs) -> PyObject*
;; range(stop) or range(start, stop) or range(start, stop, step)
;; ============================================================================
DEF_FUNC builtin_range
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

    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "range expected 1 to 3 arguments"
    call raise_exception

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
    leave
    ret
END_FUNC builtin_range

;; ============================================================================
;; builtin_type(PyObject **args, int64_t nargs) -> PyObject*
;; type(obj) -> returns obj's type
;; ============================================================================
DEF_FUNC builtin_type

    cmp rsi, 1
    jne .type_error

    mov rdi, [rdi]             ; obj = args[0]

    ; SmallInt check
    test rdi, rdi
    js .type_smallint

    mov rax, [rdi + PyObject.ob_type]
    INCREF rax

    leave
    ret

.type_smallint:
    extern int_type
    lea rax, [rel int_type]
    INCREF rax
    leave
    ret

.type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "type() takes 1 argument"
    call raise_exception
END_FUNC builtin_type

;; ============================================================================
;; builtin_isinstance(PyObject **args, int64_t nargs) -> PyObject*
;; isinstance(obj, type) -> True/False
;; Walks the full tp_base chain for inheritance.
;; ============================================================================
DEF_FUNC builtin_isinstance

    cmp rsi, 2
    jne .isinstance_error

    extern bool_true
    extern bool_false

    mov rax, [rdi]             ; rax = args[0] = obj
    mov rcx, [rdi + 8]        ; rcx = args[1] = type_to_check

    ; Get obj's type (SmallInt-aware)
    test rax, rax
    js .isinstance_smallint
    mov rdx, [rax + PyObject.ob_type]
    jmp .isinstance_check

.isinstance_smallint:
    lea rdx, [rel int_type]

.isinstance_check:
    ; Walk the full type chain: rdx = current type, rcx = target type
    cmp rdx, rcx
    je .isinstance_true
    mov rdx, [rdx + PyTypeObject.tp_base]
    test rdx, rdx
    jnz .isinstance_check

.isinstance_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.isinstance_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.isinstance_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "isinstance() takes 2 arguments"
    call raise_exception
END_FUNC builtin_isinstance

;; ============================================================================
;; builtin_issubclass(PyObject **args, int64_t nargs) -> PyObject*
;; issubclass(cls, parent) -> True/False
;; Walks the full tp_base chain for inheritance.
;; ============================================================================
DEF_FUNC builtin_issubclass

    cmp rsi, 2
    jne .issubclass_error

    mov rdx, [rdi]             ; rdx = args[0] = cls (child type)
    mov rcx, [rdi + 8]        ; rcx = args[1] = parent type

.issubclass_check:
    cmp rdx, rcx
    je .issubclass_true
    mov rdx, [rdx + PyTypeObject.tp_base]
    test rdx, rdx
    jnz .issubclass_check

    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.issubclass_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.issubclass_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "issubclass() takes 2 arguments"
    call raise_exception
END_FUNC builtin_issubclass

;; ============================================================================
;; builtin_repr(PyObject **args, int64_t nargs) -> PyObject*
;; repr(obj)
;; ============================================================================
DEF_FUNC builtin_repr

    cmp rsi, 1
    jne .repr_error

    mov rdi, [rdi]
    call obj_repr

    leave
    ret

.repr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "repr() takes 1 argument"
    call raise_exception
END_FUNC builtin_repr

;; ============================================================================
;; builtin_bool(PyObject **args, int64_t nargs) -> PyObject*
;; bool()    -> False
;; bool(x)   -> True if x is truthy, False otherwise
;; ============================================================================
DEF_FUNC builtin_bool

    cmp rsi, 0
    je .bool_no_args
    cmp rsi, 1
    jne .bool_error

    ; bool(x) - test truthiness
    mov rdi, [rdi]             ; rdi = x
    extern obj_is_true
    call obj_is_true           ; eax = 0 or 1
    test eax, eax
    jz .bool_ret_false
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.bool_ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.bool_no_args:
    ; bool() -> False
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.bool_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "bool() takes at most 1 argument"
    call raise_exception
END_FUNC builtin_bool

;; ============================================================================
;; builtin_float(PyObject **args, int64_t nargs) -> PyObject*
;; float()    -> 0.0
;; float(x)   -> convert x to float
;; ============================================================================
DEF_FUNC builtin_float

    cmp rsi, 0
    je .float_no_args
    cmp rsi, 1
    jne .float_error

    ; float(x) - convert x
    mov rdi, [rdi]             ; rdi = x
    extern float_to_f64
    call float_to_f64          ; xmm0 = double
    extern float_from_f64
    call float_from_f64        ; rax = new float

    leave
    ret

.float_no_args:
    ; float() -> 0.0
    xorpd xmm0, xmm0
    call float_from_f64
    leave
    ret

.float_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "float() takes at most 1 argument"
    call raise_exception
END_FUNC builtin_float

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
DEF_FUNC builtin___build_class__
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24             ; 3 slots: [rbp-48]=base_class, [rbp-56]=unused, [rbp-64]=align

    ; Check nargs >= 2
    cmp rsi, 2
    jl .build_class_error

    mov rbx, rdi            ; rbx = args
    ; r12 will be used later for the type object

    ; Save base class if present (args[2])
    xor eax, eax
    cmp rsi, 3
    jl .bc_no_base
    mov rax, [rbx + 16]    ; base = args[2]
.bc_no_base:
    mov [rbp-48], rax       ; save base_class (or NULL)

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

    ; ob_type = user_type_metatype (metatype handles class variable access + instantiation)
    lea rax, [rel user_type_metatype]
    mov [r12 + PyTypeObject.ob_type], rax

    ; tp_name: point to class_name string's data area
    lea rax, [r14 + PyStrObject.data]
    mov [r12 + PyTypeObject.tp_name], rax

    mov qword [r12 + PyTypeObject.tp_basicsize], PyInstanceObject_size

    ; Wire instance methods
    lea rax, [rel instance_dealloc]
    mov [r12 + PyTypeObject.tp_dealloc], rax

    lea rax, [rel instance_repr]
    mov [r12 + PyTypeObject.tp_repr], rax

    extern instance_str
    lea rax, [rel instance_str]
    mov [r12 + PyTypeObject.tp_str], rax

    ; tp_call left NULL: calling the type goes through metatype.tp_call (type_call).
    ; Calling instances falls through to __call__ dunder dispatch.

    lea rax, [rel instance_getattr]
    mov [r12 + PyTypeObject.tp_getattr], rax

    lea rax, [rel instance_setattr]
    mov [r12 + PyTypeObject.tp_setattr], rax

    ; tp_flags = HEAPTYPE (enables dunder dispatch fallbacks)
    mov qword [r12 + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE

    ; tp_dict = class_dict
    mov [r12 + PyTypeObject.tp_dict], r15

    ; INCREF class_dict (type holds a reference)
    mov rdi, r15
    call obj_incref

    ; INCREF class_name (type object refers to it via tp_name)
    mov rdi, r14
    call obj_incref

    ; Look up "__init__" in class_dict for tp_init
    lea rdi, [rel bc_init_name]
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

    ; Set tp_base if we have a base class
    mov rax, [rbp-48]
    test rax, rax
    jz .bc_no_set_base
    mov [r12 + PyTypeObject.tp_base], rax
    mov rdi, rax
    call obj_incref
.bc_no_set_base:

    ; Call parent's __init_subclass__ if present
    mov rax, [rbp-48]          ; base class
    test rax, rax
    jz .bc_no_init_subclass

    ; Look up __init_subclass__ on the base class (walk MRO)
    extern dunder_lookup
    mov rdi, rax               ; base class (as type)
    CSTRING rsi, "__init_subclass__"
    call dunder_lookup
    test rax, rax
    jz .bc_no_init_subclass

    ; Call __init_subclass__(new_class)
    ; rax = the dunder function (borrowed ref)
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .bc_no_init_subclass

    push r12                   ; args[0] = new class (r12)
    mov rdi, rax               ; callable = __init_subclass__ func
    mov rsi, rsp               ; args
    mov edx, 1                 ; nargs = 1
    call rcx
    add rsp, 8                 ; pop args
    ; DECREF result if non-NULL
    test rax, rax
    jz .bc_no_init_subclass
    mov rdi, rax
    call obj_decref

.bc_no_init_subclass:

    ; Handle __classcell__: look in class_dict for the cell, set its ob_ref to the new type
    lea rdi, [rel bc_classcell_name]
    call str_from_cstr
    push rax                ; save key str
    mov rdi, r15            ; class_dict
    mov rsi, rax
    call dict_get           ; returns cell or NULL
    pop rdi                 ; key str
    push rax                ; save cell
    call obj_decref         ; DECREF key str
    pop rax                 ; restore cell
    test rax, rax
    jz .bc_no_classcell
    ; cell.ob_ref = new type (r12)
    mov [rax + PyCellObject.ob_ref], r12
    mov rdi, r12
    call obj_incref         ; cell holds a ref to the type
.bc_no_classcell:

    ; Return the new type object
    mov rax, r12

    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.build_class_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__build_class__ requires 2+ arguments"
    call raise_exception
END_FUNC builtin___build_class__

section .rodata
bc_init_name: db "__init__", 0
bc_classcell_name: db "__classcell__", 0
section .text

;; ============================================================================
;; Helper: add_builtin(dict, name_cstr, func_ptr)
;; Adds a builtin to the dict. Used by builtins_init.
;; rdi=dict, rsi=name_cstr, rdx=func_ptr
;; ============================================================================
DEF_FUNC_LOCAL add_builtin
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
    leave
    ret
END_FUNC add_builtin

;; ============================================================================
;; builtins_init() -> PyDictObject*
;; Create and populate the builtins dictionary
;; ============================================================================
DEF_FUNC builtins_init
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
    lea rsi, [rel bi_name_build_class]
    call builtin_func_new
    mov [rel build_class_obj], rax

    ; Register __build_class__ in builtins dict
    mov rdi, rbx
    lea rsi, [rel bi_name_build_class]
    lea rdx, [rel builtin___build_class__]
    call add_builtin

    ; Add builtins using helper
    mov rdi, rbx
    lea rsi, [rel bi_name_print]
    lea rdx, [rel builtin_print]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_len]
    lea rdx, [rel builtin_len]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_range]
    lea rdx, [rel builtin_range]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_type]
    lea rdx, [rel builtin_type]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_isinstance]
    lea rdx, [rel builtin_isinstance]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_issubclass]
    lea rdx, [rel builtin_issubclass]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_repr]
    lea rdx, [rel builtin_repr]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_float]
    lea rdx, [rel builtin_float]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_bool]
    lea rdx, [rel builtin_bool]
    call add_builtin

    ; Register new builtins (from builtins_extra.asm)
    mov rdi, rbx
    lea rsi, [rel bi_name_abs]
    lea rdx, [rel builtin_abs]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_int]
    lea rdx, [rel builtin_int_fn]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_str]
    lea rdx, [rel builtin_str_fn]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ord]
    lea rdx, [rel builtin_ord]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_chr]
    lea rdx, [rel builtin_chr]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_hex]
    lea rdx, [rel builtin_hex]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_id]
    lea rdx, [rel builtin_id]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_hash]
    lea rdx, [rel builtin_hash_fn]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_callable]
    lea rdx, [rel builtin_callable]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_iter]
    lea rdx, [rel builtin_iter_fn]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_next]
    lea rdx, [rel builtin_next_fn]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_any]
    lea rdx, [rel builtin_any]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_all]
    lea rdx, [rel builtin_all]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_sum]
    lea rdx, [rel builtin_sum]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_min]
    lea rdx, [rel builtin_min]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_max]
    lea rdx, [rel builtin_max]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_getattr]
    lea rdx, [rel builtin_getattr]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_hasattr]
    lea rdx, [rel builtin_hasattr]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_setattr]
    lea rdx, [rel builtin_setattr]
    call add_builtin

    ; Register iterator builtins (from itertools.asm)
    mov rdi, rbx
    lea rsi, [rel bi_name_enumerate]
    lea rdx, [rel builtin_enumerate]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_zip]
    lea rdx, [rel builtin_zip]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_map]
    lea rdx, [rel builtin_map]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_filter]
    lea rdx, [rel builtin_filter]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_reversed]
    lea rdx, [rel builtin_reversed]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_sorted]
    lea rdx, [rel builtin_sorted]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_globals]
    lea rdx, [rel builtin_globals]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_locals]
    lea rdx, [rel builtin_locals]
    call add_builtin

    ; Register super type as builtin (LOAD_SUPER_ATTR needs it loadable)
    mov rdi, rbx
    lea rsi, [rel bi_name_super]
    lea rdx, [rel super_type]
    call add_exc_type_builtin

    ; Register descriptor types as builtins
    mov rdi, rbx
    lea rsi, [rel bi_name_staticmethod]
    lea rdx, [rel staticmethod_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_classmethod]
    lea rdx, [rel classmethod_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_property]
    lea rdx, [rel property_type]
    call add_exc_type_builtin

    ; Register exception types as builtins
    mov rdi, rbx
    lea rsi, [rel bi_name_BaseException]
    lea rdx, [rel exc_BaseException_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_Exception]
    lea rdx, [rel exc_Exception_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_TypeError]
    lea rdx, [rel exc_TypeError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ValueError]
    lea rdx, [rel exc_ValueError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_KeyError]
    lea rdx, [rel exc_KeyError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_IndexError]
    lea rdx, [rel exc_IndexError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_AttributeError]
    lea rdx, [rel exc_AttributeError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_NameError]
    lea rdx, [rel exc_NameError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_RuntimeError]
    lea rdx, [rel exc_RuntimeError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_StopIteration]
    lea rdx, [rel exc_StopIteration_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ZeroDivisionError]
    lea rdx, [rel exc_ZeroDivisionError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_NotImplementedError]
    lea rdx, [rel exc_NotImplementedError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_OverflowError]
    lea rdx, [rel exc_OverflowError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_AssertionError]
    lea rdx, [rel exc_AssertionError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_OSError]
    lea rdx, [rel exc_OSError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_LookupError]
    lea rdx, [rel exc_LookupError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ArithmeticError]
    lea rdx, [rel exc_ArithmeticError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_RecursionError]
    lea rdx, [rel exc_RecursionError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ImportError]
    lea rdx, [rel exc_ImportError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_MemoryError]
    lea rdx, [rel exc_MemoryError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_KeyboardInterrupt]
    lea rdx, [rel exc_KeyboardInterrupt_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_SystemExit]
    lea rdx, [rel exc_SystemExit_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeError]
    lea rdx, [rel exc_UnicodeError_type]
    call add_exc_type_builtin

    ; Return builtins dict
    mov rax, rbx

    pop rbx
    leave
    ret
END_FUNC builtins_init

;; ============================================================================
;; add_exc_type_builtin(dict, name_cstr, type_ptr)
;; Register an exception type object in the builtins dict.
;; Types are immortal, so no DECREF needed on the value.
;; rdi=dict, rsi=name_cstr, rdx=type_ptr
;; ============================================================================
DEF_FUNC_LOCAL add_exc_type_builtin
    push rbx
    push r12

    mov rbx, rdi               ; dict
    mov r12, rdx               ; type_ptr

    ; Create key string
    mov rdi, rsi
    call str_from_cstr
    push rax                   ; save key

    ; dict_set(dict, key, type_ptr)
    ; INCREF the type (dict_set will INCREF it, types are immortal anyway)
    mov rdi, rbx
    mov rsi, rax               ; key
    mov rdx, r12               ; type object
    call dict_set

    ; DECREF key
    pop rdi
    call obj_decref

    pop r12
    pop rbx
    leave
    ret
END_FUNC add_exc_type_builtin

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata

bi_name_print:        db "print", 0
bi_name_len:          db "len", 0
bi_name_range:        db "range", 0
bi_name_type:         db "type", 0
bi_name_isinstance:   db "isinstance", 0
bi_name_issubclass:   db "issubclass", 0
bi_name_repr:         db "repr", 0
bi_name_float:        db "float", 0
bi_name_bool:         db "bool", 0
bi_name_build_class:  db "__build_class__", 0

; New builtin names
bi_name_abs:          db "abs", 0
bi_name_int:          db "int", 0
bi_name_str:          db "str", 0
bi_name_ord:          db "ord", 0
bi_name_chr:          db "chr", 0
bi_name_hex:          db "hex", 0
bi_name_id:           db "id", 0
bi_name_hash:         db "hash", 0
bi_name_callable:     db "callable", 0
bi_name_iter:         db "iter", 0
bi_name_next:         db "next", 0
bi_name_any:          db "any", 0
bi_name_all:          db "all", 0
bi_name_sum:          db "sum", 0
bi_name_min:          db "min", 0
bi_name_max:          db "max", 0
bi_name_getattr:      db "getattr", 0
bi_name_hasattr:      db "hasattr", 0
bi_name_setattr:      db "setattr", 0

; Iterator builtin names
bi_name_enumerate:    db "enumerate", 0
bi_name_zip:          db "zip", 0
bi_name_map:          db "map", 0
bi_name_filter:       db "filter", 0
bi_name_reversed:     db "reversed", 0
bi_name_sorted:       db "sorted", 0
bi_name_globals:      db "globals", 0
bi_name_locals:       db "locals", 0
bi_name_super:        db "super", 0
bi_name_staticmethod: db "staticmethod", 0
bi_name_classmethod:  db "classmethod", 0
bi_name_property:     db "property", 0

; Exception type names
bi_name_BaseException:     db "BaseException", 0
bi_name_Exception:         db "Exception", 0
bi_name_TypeError:         db "TypeError", 0
bi_name_ValueError:        db "ValueError", 0
bi_name_KeyError:          db "KeyError", 0
bi_name_IndexError:        db "IndexError", 0
bi_name_AttributeError:    db "AttributeError", 0
bi_name_NameError:         db "NameError", 0
bi_name_RuntimeError:      db "RuntimeError", 0
bi_name_StopIteration:     db "StopIteration", 0
bi_name_ZeroDivisionError: db "ZeroDivisionError", 0
bi_name_NotImplementedError: db "NotImplementedError", 0
bi_name_OverflowError:     db "OverflowError", 0
bi_name_AssertionError:    db "AssertionError", 0
bi_name_OSError:           db "OSError", 0
bi_name_LookupError:       db "LookupError", 0
bi_name_ArithmeticError:   db "ArithmeticError", 0
bi_name_RecursionError:    db "RecursionError", 0
bi_name_ImportError:       db "ImportError", 0
bi_name_MemoryError:       db "MemoryError", 0
bi_name_KeyboardInterrupt: db "KeyboardInterrupt", 0
bi_name_SystemExit:        db "SystemExit", 0
bi_name_UnicodeError:      db "UnicodeError", 0

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
    dq type_type                ; ob_type
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
