; builtins.asm - The builtin function object, the core builtins, and the registry
;
; PyBuiltinObject and its type; print, len, range, type, isinstance,
; issubclass, repr, bool and float; and builtins_init, which is the single
; place every name in the builtins dict is registered.  The numeric and object
; builtins live in builtins_num.asm and builtins_obj.asm.

%include "macros.inc"
%include "object.inc"

extern dict_new
extern dict_set
extern str_from_cstr
extern str_from_cstr_heap
extern obj_str
extern obj_decref
extern none_singleton
extern int_from_i64
extern str_type
extern bool_type
extern float_type
extern ap_malloc
extern ap_free
extern raise_exception
extern sys_write
extern fileobj_write_fd
extern range_new
extern int_to_i64
extern obj_as_index
extern init_iter_types
extern obj_repr
extern ap_memcpy
extern user_type_metatype
extern super_type
extern staticmethod_type
extern classmethod_type
extern property_type
extern type_type
extern list_type
extern dict_type
extern tuple_type
extern set_type
extern bytes_type
extern current_exception

; New builtin function implementations (in builtins_extra.asm)
extern builtin_abs
extern builtin_divmod
extern int_type_call
extern str_type_call
extern bool_type_call
extern float_type_call
extern bytearray_type_call
extern memoryview_type_call
extern bytearray_type
extern memoryview_type
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
extern builtin_sorted
extern builtin_chain
extern builtin_globals
extern builtin_locals
extern builtin_dir
extern builtin_breakpoint

; Exception types
extern exc_BaseException_type
extern exc_Exception_type
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_KeyError_type
extern exc_IndexError_type
extern exc_AttributeError_type
extern exc_NameError_type
extern exc_UnboundLocalError_type
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
extern exc_Warning_type
extern exc_DeprecationWarning_type
extern exc_UserWarning_type
extern exc_BaseExceptionGroup_type
extern exc_ExceptionGroup_type
extern exc_CancelledError_type
extern exc_StopAsyncIteration_type
extern exc_TimeoutError_type

; --- moved to a sibling file by the split ---
extern builtin___build_class__

extern none_type

extern int_type

extern dict_add_builtin_func

section .text

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

    ; Create a string object for the name (heap — stored in struct field)
    mov rdi, r12
    call str_from_cstr_heap
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
    mov qword [rax + PyBuiltinObject.min_args], 0  ; 0 = no check
    mov qword [rax + PyBuiltinObject.max_args], -1 ; -1 = no max check
    mov qword [rax + PyBuiltinObject.func_owner], 0
    mov qword [rax + PyBuiltinObject.func_kind], BUILTIN_KIND_FUNCTION

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC builtin_func_new

;; ============================================================================
;; builtin_func_new_checked(void *func_ptr, const char *name_cstr,
;;                          int64_t min_args, int64_t max_args)
;; Like builtin_func_new but sets arg count bounds for validation.
;; rdx = min_args (including self), rcx = max_args (-1 = no max)
;; ============================================================================
DEF_FUNC builtin_func_new_checked
    push r14
    push r15
    mov r14, rdx                ; min_args
    mov r15, rcx                ; max_args
    call builtin_func_new
    mov [rax + PyBuiltinObject.min_args], r14
    mov [rax + PyBuiltinObject.max_args], r15
    pop r15
    pop r14
    leave
    ret
END_FUNC builtin_func_new_checked

;; ============================================================================
;; builtin_func_call(PyObject *self, PyObject **args, int64_t nargs) -> PyObject*
;; Dispatch to the underlying C function: func_ptr(args, nargs)
;; Validates nargs against min_args/max_args if set.
;; ============================================================================
DEF_FUNC_BARE builtin_func_call
    ; self = rdi, args = rsi, nargs = rdx
    ; Check min_args (0 = no check)
    mov rcx, [rdi + PyBuiltinObject.min_args]
    test rcx, rcx
    jz .bfc_no_min_check
    cmp rdx, rcx
    jl .bfc_too_few
.bfc_no_min_check:
    ; Check max_args (-1 = no check)
    mov rcx, [rdi + PyBuiltinObject.max_args]
    cmp rcx, -1
    je .bfc_no_max_check
    cmp rdx, rcx
    jg .bfc_too_many
.bfc_no_max_check:
    ; A method descriptor reached UNBOUND has to be handed one of its own
    ; type's instances.  Nothing checked, so `list.append((1, 2), 9)` read a
    ; tuple's header as a list's and tried to grow it -- "Fatal: out of
    ; memory", from a two-element tuple.  Every builtin method funnels
    ; through here, so one check covers all of them.
    ;
    ; func_owner is 0 for a plain module function, and for the static and
    ; class methods, which are wrapped in their own descriptor objects and so
    ; are never stamped -- exactly the ones whose first argument is a class
    ; rather than an instance.
    mov rcx, [rdi + PyBuiltinObject.func_owner]
    test rcx, rcx
    jz .bfc_receiver_ok
    test rdx, rdx
    jz .bfc_receiver_ok         ; no arguments at all: the arity check ruled
    push rdi
    push rsi
    push rdx
    sub rsp, 8
    mov rdi, [rsi]              ; args[0], the receiver
    extern value_type
    call value_type
    test rax, rax
    jz .bfc_wrong_receiver
    mov rdi, rax
    mov rsi, [rsp + 24]
    mov rsi, [rsi + PyBuiltinObject.func_owner]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jz .bfc_wrong_receiver
    add rsp, 8
    pop rdx
    pop rsi
    pop rdi

.bfc_receiver_ok:
    ; Extract func_ptr from self
    mov rax, [rdi + PyBuiltinObject.func_ptr]
    ; Call func_ptr(args, nargs) — builtins return a Value, so this stays a
    ; tail call.
    mov rdi, rsi                ; args
    mov rsi, rdx                ; nargs
    jmp rax

.bfc_wrong_receiver:
    add rsp, 8
    pop rdx
    pop rsi
    pop rdi
    mov rsi, [rsi]              ; the receiver
    extern raise_descriptor_receiver
    jmp raise_descriptor_receiver   ; rdi = the descriptor; does not return

.bfc_too_few:
    ; "list.append() takes exactly one argument (0 given)".  Both of these
    ; said "function takes at most N arguments" -- with a literal N, and
    ; naming neither the method nor what it was actually given.
    mov rcx, [rdi + PyBuiltinObject.min_args]
    jmp .bfc_arity
.bfc_too_many:
    mov rcx, [rdi + PyBuiltinObject.max_args]
.bfc_arity:
    dec rcx                     ; self counts in neither number
    dec rdx
    mov rsi, rdx
    mov rdx, rcx
    extern raise_builtin_arity
    jmp raise_builtin_arity     ; rdi = the descriptor; does not return
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
;; Returns "<built-in function len>", "<method 'bit_length' of 'int'
;; objects>" or "<slot wrapper '__add__' of 'int' objects>", depending on
;; func_kind.
;;
;; It used to INCREF func_name and hand it back verbatim, so repr(len) was
;; 'len' and repr(int.bit_length) was 'bit_length'.  That is not cosmetic:
;; the stdlib classifies a callable by reading its repr, and the three forms
;; are three different CPython types.
;; ============================================================================
BFR_BUF   equ 264           ; the composed repr; two 80-char names plus text
BFR_FRAME equ 272           ; + 1 push = 280, not 16-aligned
extern rbt_append_cstr
DEF_FUNC_LOCAL builtin_func_repr, BFR_FRAME
    push rbx
    mov rbx, rdi

    mov rax, [rbx + PyBuiltinObject.func_name]
    test rax, rax
    jz .fallback

    mov rcx, [rbx + PyBuiltinObject.func_owner]
    test rcx, rcx
    jz .plain

    ; "<method '" or "<slot wrapper '"
    lea rdi, [rbp - BFR_BUF]
    lea rsi, [rel bfr_method_open]
    cmp qword [rbx + PyBuiltinObject.func_kind], BUILTIN_KIND_WRAPPER
    jne .have_open
    lea rsi, [rel bfr_wrapper_open]
.have_open:
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbx + PyBuiltinObject.func_name]
    add rsi, PyStrObject.data
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel bfr_of]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbx + PyBuiltinObject.func_owner]
    mov rsi, [rsi + PyTypeObject.tp_name]
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel bfr_objects]
    call rbt_append_cstr
    lea rdi, [rbp - BFR_BUF]
    call str_from_cstr
    pop rbx
    leave
    ret

.plain:
    lea rdi, [rbp - BFR_BUF]
    lea rsi, [rel bfr_function_open]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbx + PyBuiltinObject.func_name]
    add rsi, PyStrObject.data
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel bfr_close]
    call rbt_append_cstr
    lea rdi, [rbp - BFR_BUF]
    call str_from_cstr
    pop rbx
    leave
    ret

.fallback:
    lea rdi, [rel builtin_func_repr_unknown_str]
    call str_from_cstr
    pop rbx
    leave
    ret
END_FUNC builtin_func_repr

;; ============================================================================
;; type_stamp_methods(rdi = a type whose tp_dict is complete)
;;
;; Walks the type's dict and tells every PyBuiltinObject in it which type it
;; belongs to and which of CPython's three descriptor kinds it is.  Called
;; once per type from methods_init, after the dict is stored.
;;
;; A stamp rather than an argument on each of the three hundred registration
;; sites, and it also catches the methods a shared helper registered -- the
;; set/frozenset table, the DEF_DUNDER_* generators -- which no per-site
;; argument would have reached without touching every one of them.
;;
;; Only a bare PyBuiltinObject is stamped.  A staticmethod or classmethod
;; wrapper is skipped, which is right: CPython reprs those differently again,
;; and with an address this tree does not print.
;; ============================================================================
TSM_TYPE  equ 8
TSM_FRAME equ 32            ; + 2 pushes = 8 + 32 + 16 = 56, not 16-aligned
global type_stamp_methods
DEF_FUNC type_stamp_methods, TSM_FRAME
    push rbx
    push r12
    mov [rbp - TSM_TYPE], rdi
    mov rbx, [rdi + PyTypeObject.tp_dict]
    test rbx, rbx
    jz .tsm_done

    mov r12, [rbx + PyDictObject.entries]
    test r12, r12
    jz .tsm_done
    mov rcx, [rbx + PyDictObject.capacity]
    xor r8d, r8d
.tsm_loop:
    cmp r8, rcx
    jge .tsm_done
    mov rax, r8
    imul rax, DICT_ENTRY_SIZE
    add rax, r12
    mov rdx, [rax + DictEntry.key]
    test rdx, rdx
    jz .tsm_next
    mov rax, [rax + DictEntry.value]
    V_TEST_PTR rax, r9
    ja .tsm_next                ; an immediate is not a method
    test rax, rax
    jz .tsm_next
    extern getset_descr_type
    lea r9, [rel getset_descr_type]
    cmp [rax + PyObject.ob_type], r9
    je .tsm_getset
    lea r9, [rel builtin_func_type]
    cmp [rax + PyObject.ob_type], r9
    jne .tsm_next
    cmp qword [rax + PyBuiltinObject.func_owner], 0
    jne .tsm_next               ; a shared body keeps its first owner

    push rcx
    push r8
    push rax
    sub rsp, 8
    mov rdi, [rbp - TSM_TYPE]
    mov rsi, [rax + PyBuiltinObject.func_name]
    call builtin_kind_of
    add rsp, 8
    pop rdx                     ; the builtin
    mov rcx, [rbp - TSM_TYPE]
    mov [rdx + PyBuiltinObject.func_owner], rcx
    mov [rdx + PyBuiltinObject.func_kind], rax
    pop r8
    pop rcx
    mov r12, [rbx + PyDictObject.entries]
    jmp .tsm_next
.tsm_getset:
    ; A getset carries its owner for the same reason, and for the same repr.
    cmp qword [rax + PyGetSetDescrObject.gs_owner], 0
    jne .tsm_next
    mov rdx, [rbp - TSM_TYPE]
    mov [rax + PyGetSetDescrObject.gs_owner], rdx
.tsm_next:
    inc r8
    jmp .tsm_loop
.tsm_done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_stamp_methods

;; ============================================================================
;; builtin_func_dunder_get(args, nargs) -- method.__get__(obj[, type])
;;
;; A method descriptor is a NON-data descriptor: hasattr(int.bit_length,
;; '__get__') is True and __set__ is absent, and that pair is exactly how
;; inspect and the enum and dataclasses classifiers tell a method from a
;; getset.  builtin_func_type had no tp_dict, so it answered False to both.
;;
;; The binding itself already happens in op_load_attr; this is the same thing
;; reachable by name.
;; ============================================================================
global builtin_func_dunder_get
DEF_FUNC builtin_func_dunder_get
    cmp rsi, 2
    jl .bfg_bad
    cmp rsi, 3
    jg .bfg_bad
    mov rax, [rdi]              ; args[0] = the method
    mov rsi, [rdi + 8]          ; args[1] = the instance
    IS_NONE rsi, rcx
    je .bfg_self
    V_TEST_PTR rsi, rcx
    ja .bfg_self                ; an immediate binds nothing, as loads do
    mov rdi, rax
    extern method_new
    call method_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.bfg_self:
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.bfg_bad:
    RAISE exc_TypeError_type, "expected 1 or 2 arguments"
END_FUNC builtin_func_dunder_get

;; ============================================================================
;; builtin_kind_of(rdi = the owning type, rsi = the name string)
;;   -> rax = BUILTIN_KIND_METHOD or BUILTIN_KIND_WRAPPER
;;
;; CPython builds a wrapper_descriptor for every name in its slotdefs table
;; and a method_descriptor for everything else, so the answer is a name
;; lookup -- with two names that go both ways.  dict and set answer
;; __contains__ from a real method and list answers __getitem__ from one,
;; where str, bytes, tuple and range answer both from a slot.
;; ============================================================================
BKO_TYPE  equ 8
BKO_FRAME equ 16            ; + 1 push = 24, not 16-aligned
DEF_FUNC_LOCAL builtin_kind_of, BKO_FRAME
    push rbx
    mov [rbp - BKO_TYPE], rdi
    lea rbx, [rsi + PyStrObject.data]

    ; Only a dunder can be a slot wrapper.
    cmp byte [rbx], '_'
    jne .bko_method
    cmp byte [rbx + 1], '_'
    jne .bko_method

    ; The two names that go both ways, each with its own short list of types
    ; that answer it from a real method rather than from a slot.
    lea rdi, [rel bko_contains_name]
    call bko_name_is
    test eax, eax
    jz .bko_try_getitem
    lea rdi, [rel bko_contains_methods]
    mov rsi, [rbp - BKO_TYPE]
    call bko_type_in_table
    test eax, eax
    jnz .bko_method
    jmp .bko_wrapper
.bko_try_getitem:
    lea rdi, [rel bko_getitem_name]
    call bko_name_is
    test eax, eax
    jz .bko_check_wrapper
    lea rdi, [rel bko_getitem_methods]
    mov rsi, [rbp - BKO_TYPE]
    call bko_type_in_table
    test eax, eax
    jnz .bko_method
    jmp .bko_wrapper

.bko_check_wrapper:
    lea rdi, [rel bko_wrapper_names]
    call bko_name_in_table
    test eax, eax
    jz .bko_method
.bko_wrapper:
    mov eax, BUILTIN_KIND_WRAPPER
    pop rbx
    leave
    ret
.bko_method:
    mov eax, BUILTIN_KIND_METHOD
    pop rbx
    leave
    ret
END_FUNC builtin_kind_of

;; bko_name_in_table(rdi = a NULL-terminated table of C strings) -> eax
;; rbx holds the name being looked for; the caller keeps it there.
DEF_FUNC_LOCAL bko_name_in_table
    mov r8, rdi
.bnt_loop:
    mov rsi, [r8]
    test rsi, rsi
    jz .bnt_no
    mov rdi, rbx
    xor ecx, ecx
.bnt_cmp:
    mov al, [rdi + rcx]
    mov dl, [rsi + rcx]
    cmp al, dl
    jne .bnt_next
    test al, al
    jz .bnt_yes
    inc rcx
    jmp .bnt_cmp
.bnt_next:
    add r8, 8
    jmp .bnt_loop
.bnt_yes:
    mov eax, 1
    leave
    ret
.bnt_no:
    xor eax, eax
    leave
    ret
END_FUNC bko_name_in_table

;; bko_name_is(rdi = a C string) -> eax = 1 when it is the name in rbx
DEF_FUNC_LOCAL bko_name_is
    mov rsi, rdi
    xor ecx, ecx
.bni_cmp:
    mov al, [rbx + rcx]
    mov dl, [rsi + rcx]
    cmp al, dl
    jne .bni_no
    test al, al
    jz .bni_yes
    inc rcx
    jmp .bni_cmp
.bni_yes:
    mov eax, 1
    leave
    ret
.bni_no:
    xor eax, eax
    leave
    ret
END_FUNC bko_name_is

;; bko_type_in_table(rdi = a NULL-terminated table of type pointers,
;;                   rsi = a type) -> eax = 1 when it is in the table
DEF_FUNC_LOCAL bko_type_in_table
.bti_loop:
    mov rax, [rdi]
    test rax, rax
    jz .bti_no
    cmp rax, rsi
    je .bti_yes
    add rdi, 8
    jmp .bti_loop
.bti_yes:
    mov eax, 1
    leave
    ret
.bti_no:
    xor eax, eax
    leave
    ret
END_FUNC bko_type_in_table

section .rodata
%macro BKO_NAME 1
    %%s: db %1, 0
    section .data
    dq %%s
    section .rodata
%endmacro

section .data
align 8
bko_wrapper_names:
section .rodata
BKO_NAME "__abs__"
BKO_NAME "__add__"
BKO_NAME "__and__"
BKO_NAME "__bool__"
BKO_NAME "__call__"
BKO_NAME "__delattr__"
BKO_NAME "__delitem__"
BKO_NAME "__divmod__"
BKO_NAME "__eq__"
BKO_NAME "__float__"
BKO_NAME "__floordiv__"
BKO_NAME "__ge__"
BKO_NAME "__getattribute__"
BKO_NAME "__gt__"
BKO_NAME "__hash__"
BKO_NAME "__iadd__"
BKO_NAME "__iand__"
BKO_NAME "__imul__"
BKO_NAME "__index__"
BKO_NAME "__init__"
BKO_NAME "__int__"
BKO_NAME "__invert__"
BKO_NAME "__ior__"
BKO_NAME "__isub__"
BKO_NAME "__iter__"
BKO_NAME "__ixor__"
BKO_NAME "__le__"
BKO_NAME "__len__"
BKO_NAME "__lshift__"
BKO_NAME "__lt__"
BKO_NAME "__mod__"
BKO_NAME "__mul__"
BKO_NAME "__ne__"
BKO_NAME "__neg__"
BKO_NAME "__next__"
BKO_NAME "__or__"
BKO_NAME "__pos__"
BKO_NAME "__pow__"
BKO_NAME "__radd__"
BKO_NAME "__rand__"
BKO_NAME "__rdivmod__"
BKO_NAME "__repr__"
BKO_NAME "__rfloordiv__"
BKO_NAME "__rlshift__"
BKO_NAME "__rmod__"
BKO_NAME "__rmul__"
BKO_NAME "__ror__"
BKO_NAME "__rpow__"
BKO_NAME "__rrshift__"
BKO_NAME "__rshift__"
BKO_NAME "__rsub__"
BKO_NAME "__rtruediv__"
BKO_NAME "__rxor__"
BKO_NAME "__setattr__"
BKO_NAME "__setitem__"
BKO_NAME "__str__"
BKO_NAME "__sub__"
BKO_NAME "__truediv__"
BKO_NAME "__xor__"
section .data
    dq 0

section .rodata
bko_contains_name: db "__contains__", 0
bko_getitem_name:  db "__getitem__", 0

; The types whose __contains__ and __getitem__ CPython builds from a real
; method rather than from a slot.  Everything else answers both from a slot.
section .data
align 8
bko_contains_methods:
    dq dict_type
    dq set_type
    dq frozenset_type
    dq 0
bko_getitem_methods:
    dq dict_type
    dq list_type
    dq 0
section .text
extern dict_type
extern set_type
extern frozenset_type
extern list_type

section .rodata
builtin_func_repr_unknown_str: db "<built-in function>", 0
bfr_function_open: db "<built-in function ", 0
bfr_method_open:   db "<method '", 0
bfr_wrapper_open:  db "<slot wrapper '", 0
bfr_of:            db "' of '", 0
bfr_objects:       db "' objects>", 0
bfr_close:         db ">", 0
section .text

;; ============================================================================
;; builtin_print(PyObject **args, int64_t nargs) -> PyObject*
;; Print each arg separated by spaces, followed by newline
;; Buffered: builds output in stack buffer, single fwrite() at end
;; ============================================================================
; Print frame layout
PR_SEP       equ 8     ; sep string ptr (0 = default " ")
PR_SEP_TAG   equ 16    ; sep tag
PR_END       equ 24    ; end string ptr (0 = default "\n")
PR_END_TAG   equ 32    ; end tag
PR_FILE_FD   equ 40    ; file descriptor (1 = stdout)
PR_FLUSH     equ 48    ; the flush= keyword, once it means something
PR_FRAME     equ 4160  ; total frame size (64 + 4096)

extern kw_names_pending
extern ap_strcmp

DEF_FUNC builtin_print, PR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; args array
    mov r12, rsi                ; nargs
    xor r13d, r13d              ; r13 = current arg index
    xor r15d, r15d              ; r15 = buffer write offset

    ; Initialize defaults
    mov qword [rbp - PR_SEP], 0       ; NULL = default " "
    mov qword [rbp - PR_END], 0       ; NULL = default "\n"
    mov qword [rbp - PR_FILE_FD], 1   ; stdout
    mov qword [rbp - PR_FLUSH], 0

    ; Check for keyword arguments
    mov rax, [rel kw_names_pending]
    test rax, rax
    jz .print_no_kw

    ; Parse kwargs
    mov rcx, [rax + PyTupleObject.ob_size]  ; n_kw
    sub r12, rcx                             ; r12 = n_pos (positional count)
    ; Process each kwarg
    xor r9d, r9d                             ; kw index
.print_kw_loop:
    cmp r9, rcx
    jge .print_kw_done
    push rcx
    push rax
    push r9

    ; Get kwarg name
    mov r10, [rax + PyTupleObject.ob_item]        ; kw names payloads
    mov r10, [r10 + r9*8]                          ; kw name str

    ; Get kwarg value position: n_pos + kw_index
    mov r11, r12                   ; n_pos
    add r11, r9                    ; n_pos + kw_index
    shl r11, 3                     ; one Value per slot
    ; value at [rbx + r11]

    ; Check "sep"
    push r10
    push r11
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "sep"
    call ap_strcmp
    test eax, eax
    pop r11
    pop r10
    jz .print_kw_sep

    ; Check "end"
    push r10
    push r11
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "end"
    call ap_strcmp
    test eax, eax
    pop r11
    pop r10
    jz .print_kw_end

    ; Check "file"
    push r10
    push r11
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "file"
    call ap_strcmp
    test eax, eax
    pop r11
    pop r10
    jz .print_kw_file

    ; Check "flush"
    push r10
    push r11
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "flush"
    call ap_strcmp
    test eax, eax
    pop r11
    pop r10
    jz .print_kw_flush

    ; Unknown keyword — skip (be lenient)
    jmp .print_kw_next

.print_kw_sep:
    mov rax, [rbx + r11]
    V_UNPACK rax, rdx
    mov [rbp - PR_SEP], rax
    mov [rbp - PR_SEP_TAG], rdx
    jmp .print_kw_next

.print_kw_end:
    mov rax, [rbx + r11]
    V_UNPACK rax, rdx
    mov [rbp - PR_END], rax
    mov [rbp - PR_END_TAG], rdx
    jmp .print_kw_next

.print_kw_flush:
    ; It was accepted and ignored, which cost nothing while stdout was
    ; unbuffered and costs the whole point of the keyword now that it is not.
    mov rax, [rbx + r11]
    extern obj_is_true
    push rcx
    push r9
    mov rdi, rax
    call obj_is_true
    pop r9
    pop rcx
    mov [rbp - PR_FLUSH], rax
    jmp .print_kw_next

.print_kw_file:
    ; file kwarg: get file descriptor from file object
    mov rax, [rbx + r11]           ; file object Value
    V_TEST_PTR rax, rdx
    ja .print_kw_next               ; non-pointer file= → ignore
    mov rax, [rax + PyFileObject.file_fd]
    mov [rbp - PR_FILE_FD], rax
    jmp .print_kw_next

.print_kw_next:
    pop r9
    pop rax
    pop rcx
    inc r9
    jmp .print_kw_loop

.print_kw_done:
    mov qword [rel kw_names_pending], 0

.print_no_kw:

align 16
.print_loop:
    cmp r13, r12
    jge .print_flush

    ; Get string representation: obj_str(args[i]) with tag
    mov rax, r13
    shl rax, 3                  ; one Value per slot
    mov rdi, [rbx + rax]       ; arg Value
    call obj_str
    ; obj_str returns (rax=payload, edx=tag)
    mov r14, rax                ; r14 = result payload
    mov r9, rdx                 ; r9 = result tag

    test r9d, r9d
    jz .skip_arg                ; TAG_NULL → skip

    ; Heap string: get length from ob_size
    mov rcx, [r14 + PyStrObject.ob_size]

    ; Check if it fits in buffer (need room for data + possible space)
    lea rax, [r15 + rcx + 2]   ; +2 for space and newline
    cmp rax, 4096
    jae .flush_and_write_direct

    ; Copy string data into buffer
    lea rdi, [rbp - PR_FRAME + r15] ; dest = buf + offset
    lea rsi, [r14 + PyStrObject.data]  ; src = str data
    mov rdx, rcx                ; len
    ; Inline small copy (most strings are short)
    test rcx, rcx
    jz .copy_done
    call ap_memcpy
.copy_done:
    add r15, [r14 + PyStrObject.ob_size]

    ; DECREF the string representation (known TAG_PTR heap string;
    ; r9 tag may have been clobbered by ap_memcpy call above)
    mov rdi, r14
    call obj_decref

.skip_arg:
    ; Append separator if not the last arg
    inc r13
    cmp r13, r12
    jge .print_flush

    ; Check if custom sep was provided
    cmp qword [rbp - PR_SEP], 0
    jne .print_custom_sep

    ; Default: single space
    mov byte [rbp - PR_FRAME + r15], ' '
    inc r15
    jmp .print_loop

.print_custom_sep:
    ; Custom sep — check if None (means default " ")
    mov rax, [rbp - PR_SEP_TAG]
    cmp eax, TAG_PTR
    jne .print_default_sep_fallback

    ; Heap string sep
    mov rax, [rbp - PR_SEP]
    ; Check if None singleton
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .print_default_sep_fallback

    mov rcx, [rax + PyStrObject.ob_size]
    ; The per-argument copy checks the 4096-byte buffer; this one did not, so
    ; print(..., sep="X"*5000) wrote past the frame over the return address.
    lea rdx, [r15 + rcx + 2]
    cmp rdx, 4096
    jae .print_sep_direct
    ; Copy sep bytes into buffer
    lea rdi, [rbp - PR_FRAME + r15]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, rcx
    test rcx, rcx
    jz .print_sep_done
    push rcx
    call ap_memcpy
    pop rcx
.print_sep_done:
    add r15, rcx
    jmp .print_loop

.print_sep_direct:
    ; Flush what is buffered, then write the separator straight out.
    test r15, r15
    jz .print_sep_write
    mov rdi, [rbp - PR_FILE_FD]
    lea rsi, [rbp - PR_FRAME]
    mov rdx, r15
    call fileobj_write_fd
    xor r15d, r15d
.print_sep_write:
    mov rax, [rbp - PR_SEP]
    mov rdi, [rbp - PR_FILE_FD]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    call fileobj_write_fd
    jmp .print_loop

.print_default_sep_fallback:
    mov byte [rbp - PR_FRAME + r15], ' '
    inc r15
    jmp .print_loop

.flush_and_write_direct:
    ; Buffer full - flush what we have, then write this string directly
    ; First flush buffer
    test r15, r15
    jz .write_direct
    mov edi, 1                  ; fd = stdout
    lea rsi, [rbp - PR_FRAME]      ; buf
    mov rdx, r15                ; len
    call fileobj_write_fd
    xor r15d, r15d              ; reset offset

.write_direct:
    ; Write this string directly
    mov edi, 1                  ; fd = stdout
    lea rsi, [r14 + PyStrObject.data]
    mov rdx, [r14 + PyStrObject.ob_size]  ; len
    call fileobj_write_fd

    ; DECREF the string representation (known TAG_PTR heap string;
    ; r9 tag was clobbered by sys_write calls above)
    mov rdi, r14
    call obj_decref
    jmp .skip_arg

.print_flush:
    ; Append end string (default: "\n")
    cmp qword [rbp - PR_END], 0
    jne .print_custom_end

    ; Default: newline
    mov byte [rbp - PR_FRAME + r15], 10
    inc r15
    jmp .print_do_flush

.print_custom_end:
    ; Check if None (means default "\n")
    mov rax, [rbp - PR_END_TAG]
    cmp eax, TAG_PTR
    jne .print_default_end
    mov rax, [rbp - PR_END]
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .print_default_end

    ; Custom end string, bounded the same way
    mov rcx, [rax + PyStrObject.ob_size]
    lea rdx, [r15 + rcx + 2]
    cmp rdx, 4096
    jae .print_end_direct
    lea rdi, [rbp - PR_FRAME + r15]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, rcx
    test rcx, rcx
    jz .print_end_copy_done
    push rcx
    call ap_memcpy
    pop rcx
.print_end_copy_done:
    add r15, rcx
    jmp .print_do_flush

.print_end_direct:
    test r15, r15
    jz .print_end_write
    mov rdi, [rbp - PR_FILE_FD]
    lea rsi, [rbp - PR_FRAME]
    mov rdx, r15
    call fileobj_write_fd
    xor r15d, r15d
.print_end_write:
    mov rax, [rbp - PR_END]
    mov rdi, [rbp - PR_FILE_FD]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    call fileobj_write_fd
    jmp .print_do_flush

.print_default_end:
    mov byte [rbp - PR_FRAME + r15], 10
    inc r15

.print_do_flush:
    ; Single sys_write for entire output
    mov rdi, [rbp - PR_FILE_FD]  ; fd (1 = stdout)
    lea rsi, [rbp - PR_FRAME]      ; buf
    mov rdx, r15                ; len
    call fileobj_write_fd

    cmp qword [rbp - PR_FLUSH], 0
    je .print_no_flush
    extern fileobj_flush_std
    call fileobj_flush_std
.print_no_flush:

    ; Return None (with INCREF)
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC builtin_print

;; ============================================================================
;; builtin_len(PyObject **args, int64_t nargs) -> rax = Value
;; Returns len() of the first argument.  Reads ob_size for a variable-size
;; builtin, and falls back to the sequence and mapping length slots.
;; ============================================================================
LEN_EXC   equ 8
LEN_FRAME equ 16            ; + 1 push = 24, not 16-aligned
DEF_FUNC builtin_len, LEN_FRAME
    push rbx

    ; Check nargs == 1
    cmp rsi, 1
    jne .len_error

    mov rbx, [rdi]              ; rbx = args[0]
    V_TEST_PTR rbx, rax
    ja .len_type_error

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
    jz .len_type_error

    extern dunder_len
    extern dunder_call_1
    mov rdi, rbx
    DUNDER_EXC_SAVE [rbp - LEN_EXC]
    lea rsi, [rel dunder_len]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .no_dunder_len

    ; __len__ returned a result — extract integer value
    push rdx                ; save tag for SmallInt check
    push rax                ; save result for DECREF
    ; Check if SmallInt (tag == TAG_SMALLINT)
    cmp qword [rsp + 8], TAG_SMALLINT
    je .len_smallint
    ; Heap int — read value (assume fits in 64 bits)
    extern int_to_i64
    mov rdi, rax
    call int_to_i64
    pop rdi                 ; DECREF the int result
    add rsp, 8              ; discard saved tag
    push rax                ; save extracted value
    call obj_decref
    pop rax
    jmp .make_int

.len_smallint:
    ; SmallInt: payload IS the int64 value, no DECREF needed
    pop rax                 ; restore payload
    add rsp, 8              ; discard saved tag
    jmp .make_int

.no_dunder_len:
    ; A NULL from dunder_call_1 means either "no __len__ on this type" or
    ; "__len__ raised"; only the first is a fallback.
    DUNDER_RAISED [rbp - LEN_EXC], .len_failed

    ; There is no last-resort ob_size read here any more.  Every real
    ; container reaches len() through sq_length, mp_length or __len__; the
    ; fallback only caught things that have no length at all, and read +16 --
    ; which for an iterator is it_seq, so len(reversed([1,2,3])) returned a
    ; heap address.
    jmp .len_type_error

.make_int:
    ; rax = length; create an int object
    mov rdi, rax
    call int_from_i64
    ; int_from_i64 returns (rax=payload, edx=tag) — preserve edx

    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.len_failed:
    RET_NULL
    pop rbx
    leave
    ret

.len_error:
    RAISE exc_TypeError_type, "len() takes exactly one argument"

.len_type_error:
    RAISE exc_TypeError_type, "object has no len()"
END_FUNC builtin_len

;; ============================================================================
;; builtin_range(PyObject **args, int64_t nargs) -> rax = Value
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

    RAISE exc_TypeError_type, "range expected 1 to 3 arguments"

.range_1:
    ; range(stop): start=0, stop=args[0], step=1
    mov rdi, [rbx]             ; args[0]
    V_UNPACK rdi, rdx
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    mov rsi, rax               ; stop
    xor edi, edi               ; start = 0
    mov edx, 1                 ; step = 1
    call range_new
    jmp .range_done

.range_2:
    ; range(start, stop): step=1
    mov rdi, [rbx]             ; args[0]
    V_UNPACK rdi, rdx
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    mov r13, rax               ; start
    mov rdi, [rbx + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    mov rsi, rax               ; stop
    mov rdi, r13               ; start
    mov edx, 1                 ; step = 1
    call range_new
    jmp .range_done

.range_3:
    ; range(start, stop, step)
    mov rdi, [rbx]             ; args[0]
    V_UNPACK rdi, rdx
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    push rax                   ; start
    mov rdi, [rbx + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    push rax                   ; stop
    mov rdi, [rbx + 16]
    V_UNPACK rdi, rdx       ; args[2]
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    mov rdx, rax               ; step
    ; A zero step makes range_obj_sq_length divide by zero and makes
    ; range_iter_next advance by nothing, so len(range(0,5,0)) was SIGFPE
    ; and `for i in range(0,5,0)` hung.
    test rdx, rdx
    jz .range_zero_step
    pop rsi                    ; stop
    pop rdi                    ; start
    call range_new
    jmp .range_done

.range_zero_step:
    RAISE exc_ValueError_type, "range() arg 3 must not be zero"

.range_done:
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC builtin_range


;; ============================================================================
;; builtin_isinstance(PyObject **args, int64_t nargs) -> rax = Value
;; isinstance(obj, type) -> True/False
;; Walks the full tp_base chain for inheritance.
;; ============================================================================
ISI_OBJ   equ 8         ; the object as a Value, for __instancecheck__
ISI_DECL  equ 16        ; obj.__class__, when it is a class and differs
ISI_FRAME equ 16            ; + 2 pushes = 32

;; ============================================================================
;; obj_declared_class(rdi = the object as a Value) -> rax = its __class__ when
;;   that is a class and is NOT simply type(obj), else 0.  A NEW reference.
;;
;; CPython asks an object what class it says it belongs to, not only what
;; class it is: an object with a __class__ of its own -- a mock, mostly -- is
;; judged by the answer.  isinstance() consults both, and so does
;; _abc_instancecheck.
;;
;; Returns 0 for anything that is not a heap object, has no __class__, or
;; whose __class__ is the type it already has.
;; ============================================================================
extern obj_getattr_opt
extern dunder_name_obj
ODC_OBJ   equ 8
ODC_TYPE  equ 16
ODC_FRAME equ 16            ; + 0 pushes = 16, 16-aligned
DEF_FUNC obj_declared_class, ODC_FRAME
    mov [rbp - ODC_OBJ], rdi
    V_TEST_PTR rdi, rax
    ja .odc_none
    test rdi, rdi
    jz .odc_none

    mov rax, [rdi + PyObject.ob_type]
    mov [rbp - ODC_TYPE], rax   ; the real type

    lea rdi, [rel isi_class_attr]
    call dunder_name_obj        ; borrowed, interned by literal
    mov rsi, rax
    mov rdi, [rbp - ODC_OBJ]
    call obj_getattr_opt
    test rax, rax
    jz .odc_none

    push rax
    mov rdi, rax
    call type_check_is_class
    pop rdi
    test eax, eax
    jz .odc_drop
    cmp rdi, [rbp - ODC_TYPE]
    je .odc_drop                ; the same answer, so it adds nothing
    mov rax, rdi
    leave
    ret
.odc_drop:
    call obj_decref
.odc_none:
    xor eax, eax
    leave
    ret
END_FUNC obj_declared_class

DEF_FUNC builtin_isinstance, ISI_FRAME
    push rbx
    push r12

    cmp rsi, 2
    jne .isinstance_error

    extern bool_true
    extern bool_false

    mov rax, [rdi]
    mov [rbp - ISI_OBJ], rax   ; the object as a Value, for __instancecheck__
    mov rax, [rdi]             ; rax = args[0] = obj
    V_UNPACK rax, r8
    mov rcx, [rdi + 8]         ; rcx = args[1] = type_to_check
    V_UNPACK rcx, r9

    ; Get obj's type (tag-aware for all inline types)
    cmp r8d, TAG_SMALLINT
    je .isinstance_smallint
    cmp r8d, TAG_FLOAT
    je .isinstance_float
    cmp r8d, TAG_PTR
    jne .isinstance_false      ; unknown non-pointer tag → False
    mov rdx, [rax + PyObject.ob_type]
    jmp .isinstance_got_type

.isinstance_none:
    lea rdx, [rel none_type]
    jmp .isinstance_got_type

.isinstance_smallint:
    lea rdx, [rel int_type]
    jmp .isinstance_got_type

.isinstance_float:
    lea rdx, [rel float_type]
    jmp .isinstance_got_type

.isinstance_bool:
    lea rdx, [rel bool_type]

.isinstance_got_type:
    ; What the object SAYS its class is, asked once and used wherever the real
    ; type does not answer.  0 unless it is a class and differs.
    push rcx
    push rdx
    push r9
    push r9                     ; keep rsp 16-byte aligned across the call
    mov rdi, [rbp - ISI_OBJ]
    call obj_declared_class
    mov [rbp - ISI_DECL], rax
    pop r9
    pop r9
    pop rdx
    pop rcx

    ; rdx = obj's type, rcx = type_to_check (may be tuple)
    ; Second arg must be TAG_PTR (type or tuple)
    cmp r9d, TAG_PTR
    jne .isinstance_type_error
    mov rax, [rcx + PyObject.ob_type]
    extern tuple_type
    lea r8, [rel tuple_type]
    cmp rax, r8
    je .isinstance_tuple
    ; Any class, including one built by a user metaclass.
    push rcx
    push rdx
    mov rdi, rcx
    extern type_check_is_class
    call type_check_is_class
    pop rdx
    pop rcx
    test eax, eax
    jz .isinstance_type_error

    ; A metaclass may define __instancecheck__ -- that is how ABCMeta makes
    ; isinstance() consult a registry rather than the MRO.
    push rcx
    push rdx
    mov rdi, rcx                ; the class
    mov rsi, [rbp - ISI_OBJ]    ; the object
    CSTRING rdx, "__instancecheck__"
    extern type_custom_check
    call type_custom_check
    pop rdx
    pop rcx
    cmp eax, -1
    je .isinstance_check
    test eax, eax
    jz .isinstance_false
    jmp .isinstance_true

.isinstance_check:
    ; The MRO, not the tp_base chain: a class with several bases is an
    ; instance of all of them.
    extern type_is_subtype
    push rcx                    ; the target class: type_is_subtype clobbers it
    push rcx
    mov rdi, rdx
    mov rsi, rcx
    call type_is_subtype
    pop rcx
    pop rcx
    test eax, eax
    jnz .isinstance_true
    ; ...and then what the object says it is.
    mov rdi, [rbp - ISI_DECL]
    test rdi, rdi
    jz .isinstance_false
    mov rsi, rcx
    call type_is_subtype
    test eax, eax
    jnz .isinstance_true
    jmp .isinstance_false

.isinstance_tuple:
    ; rcx = tuple of types. Check obj against each.
    mov rbx, rcx               ; rbx = tuple
    mov r12, rdx               ; r12 = obj's type (saved)
    mov rsi, [rbx + PyTupleObject.ob_item]  ; payloads
    mov rcx, [rbx + PyTupleObject.ob_size]
    xor r8d, r8d               ; index
.isinstance_tuple_loop:
    cmp r8, rcx
    jge .isinstance_false
    push rcx
    push r8
    push rsi
    push rsi                   ; keep the stack 16-byte aligned
    mov rdi, [rsi + r8*8]      ; the class from the tuple
    mov rsi, [rbp - ISI_OBJ]   ; the object
    CSTRING rdx, "__instancecheck__"
    call type_custom_check
    cmp eax, -1
    jne .isinstance_tuple_verdict
    mov rsi, [rsp]             ; the saved payload array
    mov r8, [rsp + 16]
    mov rdi, r12               ; obj's type
    mov rsi, [rsi + r8*8]      ; type from tuple
    push rsi
    push rsi
    call type_is_subtype
    pop rsi
    pop rsi
    test eax, eax
    jnz .isinstance_tuple_verdict
    mov rdi, [rbp - ISI_DECL]  ; ...and then what the object says it is
    test rdi, rdi
    jz .isinstance_tuple_verdict
    call type_is_subtype
.isinstance_tuple_verdict:
    pop rsi
    pop rsi
    test eax, eax
    jnz .isinstance_tuple_match
    pop r8
    pop rcx
    inc r8
    jmp .isinstance_tuple_loop

.isinstance_tuple_match:
    add rsp, 16                ; pop saved r8, rcx
    jmp .isinstance_true

.isinstance_false:
    call .isi_release_declared
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isinstance_true:
    call .isi_release_declared
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isi_release_declared:
    mov rdi, [rbp - ISI_DECL]
    test rdi, rdi
    jz .isi_nothing
    mov qword [rbp - ISI_DECL], 0
    jmp obj_decref
.isi_nothing:
    ret

.isinstance_type_error:
    RAISE exc_TypeError_type, "isinstance() arg 2 must be a type, a tuple of types, or a union"

.isinstance_error:
    RAISE exc_TypeError_type, "isinstance() takes 2 arguments"
END_FUNC builtin_isinstance

;; ============================================================================
;; builtin_issubclass(PyObject **args, int64_t nargs) -> rax = Value
;; issubclass(cls, parent) -> True/False
;; Walks the full tp_base chain for inheritance.
;; Supports tuple second arg: issubclass(cls, (type1, type2, ...))
;; ============================================================================
DEF_FUNC builtin_issubclass
    push rbx
    push r12
    push r13

    cmp rsi, 2
    jne .issubclass_error

    mov rdx, [rdi]             ; rdx = args[0] = cls
    V_UNPACK rdx, r8
    mov rcx, [rdi + 8]         ; rcx = args[1] = parent
    V_UNPACK rcx, r9

    ; Validate first arg is a type.  A user metaclass makes its instances
    ; classes too, so this is a subtype test, not three pointer compares.
    cmp r8d, TAG_PTR
    jne .issubclass_arg1_error
    push rcx
    push rdx
    push r9
    mov rdi, rdx
    call type_check_is_class
    pop r9
    pop rdx
    pop rcx
    test eax, eax
    jz .issubclass_arg1_error

    ; Check if second arg is a tuple
    cmp r9d, TAG_PTR
    jne .issubclass_arg2_error
    mov rax, [rcx + PyObject.ob_type]
    lea r10, [rel tuple_type]
    cmp rax, r10
    je .issubclass_tuple
    ; Validate second arg is a type
    push rcx
    push rdx
    mov rdi, rcx
    call type_check_is_class
    pop rdx
    pop rcx
    test eax, eax
    jz .issubclass_arg2_error

    ; Single type check.  A metaclass __subclasscheck__ -- ABCMeta's, above
    ; all -- decides before the MRO is walked, since a virtual subclass is
    ; not in anyone's MRO.
.issubclass_walk:
    push rcx
    push rdx
    mov rdi, rcx                ; the parent class
    mov rsi, rdx                ; the candidate subclass
    CSTRING rdx, "__subclasscheck__"
    call type_custom_check
    pop rdx
    pop rcx
    cmp eax, -1
    jne .issubclass_from_hook
    mov rdi, rdx
    mov rsi, rcx
    call type_is_subtype
    test eax, eax
    jnz .issubclass_true
    jmp .issubclass_false
.issubclass_from_hook:
    test eax, eax
    jnz .issubclass_true
    jmp .issubclass_false

.issubclass_tuple:
    ; rcx = tuple of types. Check cls against each.
    mov rbx, rcx               ; rbx = tuple
    mov r12, rdx               ; r12 = cls (saved)
    mov rsi, [rbx + PyTupleObject.ob_item]  ; payloads array
    mov r13, [rbx + PyTupleObject.ob_size]  ; count
    xor r8d, r8d               ; index
.issubclass_tuple_loop:
    cmp r8, r13
    jge .issubclass_false
    push rsi
    push r8
    mov rdi, [rsi + r8*8]      ; the parent from the tuple
    mov rsi, r12               ; cls
    CSTRING rdx, "__subclasscheck__"
    call type_custom_check
    cmp eax, -1
    jne .issubclass_tuple_verdict
    mov rax, [rsp + 8]         ; the saved payload array
    mov r8, [rsp]
    mov rdi, r12               ; cls
    mov rsi, [rax + r8*8]      ; type from tuple
    call type_is_subtype
.issubclass_tuple_verdict:
    test eax, eax
    jnz .issubclass_tuple_match
    pop r8
    pop rsi
    inc r8
    jmp .issubclass_tuple_loop

.issubclass_tuple_match:
    add rsp, 16               ; pop saved r8, rsi
    jmp .issubclass_true

.issubclass_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.issubclass_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.issubclass_arg1_error:
    RAISE exc_TypeError_type, "issubclass() arg 1 must be a class"

.issubclass_arg2_error:
    RAISE exc_TypeError_type, "issubclass() arg 2 must be a class, a tuple of classes, or a union"

.issubclass_error:
    RAISE exc_TypeError_type, "issubclass() takes 2 arguments"
END_FUNC builtin_issubclass

;; ============================================================================
;; builtin_repr(PyObject **args, int64_t nargs) -> rax = Value
;; repr(obj)
;; ============================================================================
DEF_FUNC builtin_repr

    cmp rsi, 1
    jne .repr_error

    mov rdi, [rdi]             ; args[0]
    call obj_repr
    ; rdx = tag from obj_repr
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.repr_error:
    RAISE exc_TypeError_type, "repr() takes 1 argument"
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
    mov rdi, [rdi]             ; args[0]
    extern obj_is_true
    call obj_is_true           ; eax = 0 or 1
    test eax, eax
    jz .bool_ret_false
    RET_TRUE
    leave
    ret
.bool_ret_false:
    RET_FALSE
    leave
    ret

.bool_no_args:
    ; bool() -> False
    RET_FALSE
    leave
    ret

.bool_error:
    RAISE exc_TypeError_type, "bool() takes at most 1 argument"
END_FUNC builtin_bool

;; ============================================================================
;; builtin_float(PyObject **args, int64_t nargs) -> PyObject*
;; float()    -> 0.0
;; float(x)   -> convert x to float (int, float, or string)
;; ============================================================================
global builtin_float
BF_START  equ 8              ; the string strtod was handed
BF_ENDPTR equ 16            ; where strtod stopped
BF_OBJ    equ 24            ; the str object itself, for the error message
BF_XLAT   equ 32            ; a Unicode-to-ASCII copy of it, or 0
BF_XLEN   equ 40            ; and the length of what is being parsed
BF_FRAME equ 48             ; + 0 pushes = 48
DEF_FUNC builtin_float, BF_FRAME

    cmp rsi, 0
    je .float_no_args
    cmp rsi, 1
    jne .float_error

    ; float(x) - convert x
    mov rdi, [rdi]             ; args[0]
    V_UNPACK rdi, rsi

    ; TAG_FLOAT fast-path: already a float, return as-is
    cmp esi, TAG_FLOAT
    je .float_passthrough

    ; TAG_PTR: check for string
    cmp esi, TAG_PTR
    jne .float_numeric          ; non-pointer tag → numeric conversion

    ; Check if it's a string
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .float_from_str

    ; A class defining __float__ now carries nb_float; float_to_f64 below
    ; knows nothing about it and returned 0.0 for such an object.
    mov rcx, [rax + PyTypeObject.tp_as_number]
    test rcx, rcx
    jz .float_numeric
    mov rcx, [rcx + PyNumberMethods.nb_float]
    test rcx, rcx
    jz .float_numeric
    call rcx                    ; nb_float returns a Value
    mov rdi, rax
    V_UNPACK rdi, rdx
    cmp edx, TAG_FLOAT
    jne .float_numeric          ; not a float: let the generic path complain
    mov rax, rdi
    mov edx, TAG_FLOAT
    leave
    ret

.float_numeric:
    ; float_to_f64 answers 0.0 for anything it does not recognise, so
    ; float(None) and float([1]) quietly produced 0.0.
    push rdi
    push rsi
    extern binop_is_number
    call binop_is_number
    pop rsi
    pop rdi
    test eax, eax
    jz .float_bad_type

    extern float_to_f64
    call float_to_f64          ; xmm0 = double
    extern float_from_f64
    call float_from_f64        ; rax = double bits, edx = TAG_FLOAT

    leave
    ret

.float_bad_type:
    V_PACK rdi, rsi
    mov rsi, rdi
    CSTRING rdi, `float() argument must be a string or a real number, not '\x01'`
    extern raise_type_error_with_name
    call raise_type_error_with_name

.float_passthrough:
    mov rax, rdi
    mov edx, TAG_FLOAT
    leave
    ret

.float_from_str:
    ; rdi = PyStrObject*. Parse string → double via strtod.
    mov [rbp - BF_OBJ], rdi
    ; A Unicode decimal digit is a digit and a Unicode space is a space, as
    ; CPython's _PyUnicode_TransformDecimalAndSpaceToASCII has it.
    mov qword [rbp - BF_XLAT], 0
    extern str_decimal_ascii
    call str_decimal_ascii
    test rax, rax
    jz .float_str_ascii
    mov [rbp - BF_XLAT], rax
    mov [rbp - BF_XLEN], rdx
    mov rdi, rax
    jmp .float_str_have_data
.float_str_ascii:
    mov rdi, [rbp - BF_OBJ]
    mov rax, [rdi + PyStrObject.ob_size]
    mov [rbp - BF_XLEN], rax
    lea rdi, [rdi + PyStrObject.data]   ; rdi = null-terminated string data
.float_str_have_data:
    mov [rbp - BF_START], rdi                  ; save start ptr

    ; An embedded NUL is not the end of the string, and strtod thinks it is:
    ; float("1\x002") answered 1.0 where CPython raises.
    extern strlen
    call strlen wrt ..plt
    cmp rax, [rbp - BF_XLEN]
    jne .float_str_error
    mov rdi, [rbp - BF_START]

    ; Call strtod(str, &endptr)
    extern strtod
    lea rsi, [rbp - BF_ENDPTR]                ; &endptr at [rbp - BF_ENDPTR]
    call strtod wrt ..plt
    ; xmm0 = parsed value, [rbp - BF_ENDPTR] = endptr

    ; Check endptr > start (parsed something)
    mov rax, [rbp - BF_ENDPTR]                ; endptr
    cmp rax, [rbp - BF_START]                 ; compare with start
    je .float_str_error                ; nothing parsed → error

    ; Skip trailing whitespace after parsed portion
.float_skip_ws:
    movzx ecx, byte [rax]
    cmp cl, ' '
    je .float_ws_next
    cmp cl, 9                          ; tab
    je .float_ws_next
    cmp cl, 10                         ; newline
    je .float_ws_next
    cmp cl, 13                         ; carriage return
    je .float_ws_next
    jmp .float_ws_done
.float_ws_next:
    inc rax
    jmp .float_skip_ws
.float_ws_done:
    cmp byte [rax], 0
    jne .float_str_error               ; trailing garbage → ValueError

    ; xmm0 still holds the strtod result
    mov rdi, [rbp - BF_XLAT]
    test rdi, rdi
    jz .float_str_kept
    movq rax, xmm0
    push rax
    extern ap_free
    call ap_free
    pop rax
    movq xmm0, rax
.float_str_kept:
    call float_from_f64                ; rax = double bits, edx = TAG_FLOAT
    leave
    ret

.float_str_error:
    mov rdi, [rbp - BF_XLAT]
    test rdi, rdi
    jz .float_str_error_msg
    mov qword [rbp - BF_XLAT], 0
    call ap_free
.float_str_error_msg:
    ; CPython names the string it could not convert, and int() here already
    ; does; float's message had lost it.
    mov rsi, [rbp - BF_OBJ]
    CSTRING rdi, "could not convert string to float: "
    extern raise_value_error_with_repr
    call raise_value_error_with_repr

.float_no_args:
    ; float() -> 0.0
    xorpd xmm0, xmm0
    call float_from_f64
    leave
    ret

.float_error:
    RAISE exc_TypeError_type, "float() takes at most 1 argument"
END_FUNC builtin_float


;; Helper: add_builtin_type(dict, name_cstr, type_obj, tp_call_fn)
;; Registers a type object directly in builtins (for isinstance to work).
;; Sets type_obj.tp_call = tp_call_fn so the type is callable.
;; rdi=dict, rsi=name_cstr, rdx=type_obj, rcx=tp_call_fn
;; ============================================================================
;; add_builtin_str(dict, const char *key, const char *value)
;; Bind a plain string constant in the builtins dict.
;; ============================================================================
ABS_DICT equ 8
ABS_KEY  equ 16
ABS_FRAME equ 24            ; + 2 pushes = 40, not 16-aligned
DEF_FUNC_LOCAL add_builtin_str, ABS_FRAME
    push rbx
    push r12
    mov [rbp - ABS_DICT], rdi
    mov [rbp - ABS_KEY], rsi
    mov rdi, rdx
    call str_from_cstr_heap
    mov r12, rax
    mov rdi, [rbp - ABS_KEY]
    call str_from_cstr_heap
    mov rbx, rax
    mov rdi, [rbp - ABS_DICT]
    mov rsi, rbx
    mov rdx, r12
    call dict_set
    mov rdi, rbx
    call obj_decref
    mov rdi, r12
    call obj_decref
    pop r12
    pop rbx
    leave
    ret
END_FUNC add_builtin_str

DEF_FUNC_LOCAL add_builtin_type
    push rbx
    push r12

    mov rbx, rdi               ; dict
    mov r12, rdx               ; type_obj

    ; Install the constructor in tp_new.  It must NOT go in tp_call: tp_call
    ; on a type governs whether that type's INSTANCES are callable.
    mov [r12 + PyTypeObject.tp_new], rcx

    ; Create key string (heap — used as dict key, then DECREFed)
    push r12
    mov rdi, rsi
    call str_from_cstr_heap
    mov rcx, rax               ; key str
    V_PACK rdx, rcx

    ; dict_set(dict, key, type_obj)
    mov rdi, rbx
    mov rsi, rcx
    pop rdx                    ; type_obj
    push rcx                   ; save key for DECREF
    call dict_set

    ; DECREF key
    pop rdi
    call obj_decref

    pop r12
    pop rbx
    leave
    ret
END_FUNC add_builtin_type

;; ============================================================================
;; builtins_init() -> PyDictObject*
;; Create and populate the builtins dictionary
;; ============================================================================
DEF_FUNC builtins_init
    push rbx

    ; Initialize iterator types (patches list/tuple tp_iter)
    call init_iter_types

    ; BaseException gets a tp_dict, so that super().__init__ from a user
    ; exception subclass reaches something that sets .args.
    extern exc_install_methods
    call exc_install_methods

    ; Create the builtins dict
    call dict_new
    mov rbx, rax                ; rbx = builtins dict

    ; Store globally for __build_class__ to access
    mov [rel builtins_dict_global], rbx

    ; builtins.__name__ is "builtins", as it is in CPython.  A class body's
    ; prologue does LOAD_NAME __name__ to fill in __module__, and with a bare
    ; dict for globals -- which is what exec(src, {}) gives it -- the lookup
    ; falls all the way through to here.
    mov rdi, rbx
    lea rsi, [rel bi_dunder_name]
    lea rdx, [rel bi_builtins_name]
    call add_builtin_str

    ; Create __build_class__ wrapper and store globally
    lea rdi, [rel builtin___build_class__]
    lea rsi, [rel bi_name_build_class]
    call builtin_func_new
    mov [rel build_class_obj], rax

    ; Register __build_class__ in builtins dict
    mov rdi, rbx
    lea rsi, [rel bi_name_build_class]
    lea rdx, [rel builtin___build_class__]
    call dict_add_builtin_func

    ; Add builtins using helper
    mov rdi, rbx
    lea rsi, [rel bi_name_print]
    lea rdx, [rel builtin_print]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_len]
    lea rdx, [rel builtin_len]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_range]
    extern range_obj_type
    extern range_type_call
    lea rdx, [rel range_obj_type]
    lea rcx, [rel range_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_type]
    lea rdx, [rel type_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_isinstance]
    lea rdx, [rel builtin_isinstance]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_issubclass]
    lea rdx, [rel builtin_issubclass]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_repr]
    lea rdx, [rel builtin_repr]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_float]
    lea rdx, [rel float_type]
    lea rcx, [rel float_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_complex]
    extern complex_type
    extern complex_type_call
    lea rdx, [rel complex_type]
    lea rcx, [rel complex_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_bool]
    lea rdx, [rel bool_type]
    lea rcx, [rel bool_type_call]
    call add_builtin_type

    extern object_type
    extern object_type_call
    mov rdi, rbx
    lea rsi, [rel bi_name_object]
    lea rdx, [rel object_type]
    lea rcx, [rel object_type_call]
    call add_builtin_type

    ; Register new builtins (from builtins_extra.asm)
    mov rdi, rbx
    lea rsi, [rel bi_name_abs]
    lea rdx, [rel builtin_abs]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_divmod]
    lea rdx, [rel builtin_divmod]
    call dict_add_builtin_func

    ; Register int as the int_type object (not a function wrapper)
    ; so isinstance(42, int) works correctly
    mov rdi, rbx
    lea rsi, [rel bi_name_int]
    lea rdx, [rel int_type]
    lea rcx, [rel int_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_str]
    lea rdx, [rel str_type]
    lea rcx, [rel str_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_ord]
    lea rdx, [rel builtin_ord]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_chr]
    lea rdx, [rel builtin_chr]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_hex]
    lea rdx, [rel builtin_hex]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_id]
    lea rdx, [rel builtin_id]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_hash]
    lea rdx, [rel builtin_hash_fn]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_callable]
    lea rdx, [rel builtin_callable]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_iter]
    lea rdx, [rel builtin_iter_fn]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_next]
    lea rdx, [rel builtin_next_fn]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_any]
    lea rdx, [rel builtin_any]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_all]
    lea rdx, [rel builtin_all]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_sum]
    lea rdx, [rel builtin_sum]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_min]
    lea rdx, [rel builtin_min]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_max]
    lea rdx, [rel builtin_max]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_getattr]
    lea rdx, [rel builtin_getattr]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_hasattr]
    lea rdx, [rel builtin_hasattr]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_setattr]
    lea rdx, [rel builtin_setattr]
    call dict_add_builtin_func

    ; Register iterator builtins (from itertools.asm)
    extern enumerate_iter_type
    extern enumerate_type_call
    mov rdi, rbx
    lea rsi, [rel bi_name_enumerate]
    lea rdx, [rel enumerate_iter_type]
    lea rcx, [rel enumerate_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_zip]
    extern zip_iter_type
    extern zip_type_call
    lea rdx, [rel zip_iter_type]
    lea rcx, [rel zip_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_map]
    extern map_iter_type
    extern map_type_call
    lea rdx, [rel map_iter_type]
    lea rcx, [rel map_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_filter]
    extern filter_iter_type
    extern filter_type_call
    lea rdx, [rel filter_iter_type]
    lea rcx, [rel filter_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_reversed]
    extern reversed_iter_type
    extern reversed_type_call
    lea rdx, [rel reversed_iter_type]
    lea rcx, [rel reversed_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_sorted]
    lea rdx, [rel builtin_sorted]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_chain]
    lea rdx, [rel builtin_chain]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_globals]
    lea rdx, [rel builtin_globals]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_locals]
    lea rdx, [rel builtin_locals]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_dir]
    lea rdx, [rel builtin_dir]
    call dict_add_builtin_func

    mov rdi, rbx
    lea rsi, [rel bi_name_breakpoint]
    lea rdx, [rel builtin_breakpoint]
    call dict_add_builtin_func

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

    ; Register NotImplemented singleton as builtin constant
    extern notimpl_singleton
    mov rdi, rbx
    lea rsi, [rel bi_name_NotImplemented]
    lea rdx, [rel notimpl_singleton]
    call add_exc_type_builtin

    ; Register Ellipsis singleton as builtin constant
    extern ellipsis_singleton
    mov rdi, rbx
    lea rsi, [rel bi_name_Ellipsis]
    lea rdx, [rel ellipsis_singleton]
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
    lea rsi, [rel bi_name_UnboundLocalError]
    lea rdx, [rel exc_UnboundLocalError_type]
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

    ; OSError's own constructor: it parses (errno, strerror, filename, ...),
    ; truncates .args, and remaps to a subclass by errno.  It goes in tp_new,
    ; which exc_type_call consults, and only on OSError itself -- DEF_EXC_TYPE
    ; leaves the slot 0 on the subclasses, which is what CPython wants: the
    ; remapping applies when the type is exactly OSError.
    extern oserror_new
    lea rax, [rel exc_OSError_type]
    lea rcx, [rel oserror_new]
    mov [rax + PyTypeObject.tp_new], rcx

    mov rdi, rbx
    lea rsi, [rel bi_name_FileNotFoundError]
    extern exc_FileNotFoundError_type
    lea rdx, [rel exc_FileNotFoundError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_FileExistsError]
    extern exc_FileExistsError_type
    lea rdx, [rel exc_FileExistsError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeTranslateError]
    extern exc_UnicodeTranslateError_type
    lea rdx, [rel exc_UnicodeTranslateError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_IOError]
    extern exc_OSError_type
    lea rdx, [rel exc_OSError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_EnvironmentError]
    extern exc_OSError_type
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

    mov rdi, rbx
    lea rsi, [rel bi_name_Warning]
    lea rdx, [rel exc_Warning_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_DeprecationWarning]
    lea rdx, [rel exc_DeprecationWarning_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_UserWarning]
    lea rdx, [rel exc_UserWarning_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_BaseExceptionGroup]
    lea rdx, [rel exc_BaseExceptionGroup_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ExceptionGroup]
    lea rdx, [rel exc_ExceptionGroup_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_CancelledError]
    lea rdx, [rel exc_CancelledError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_StopAsyncIteration]
    lea rdx, [rel exc_StopAsyncIteration_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_TimeoutError]
    lea rdx, [rel exc_TimeoutError_type]
    call add_exc_type_builtin

    extern exc_GeneratorExit_type
    mov rdi, rbx
    lea rsi, [rel bi_name_GeneratorExit]
    lea rdx, [rel exc_GeneratorExit_type]
    call add_exc_type_builtin

    extern exc_ModuleNotFoundError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ModuleNotFoundError]
    lea rdx, [rel exc_ModuleNotFoundError_type]
    call add_exc_type_builtin

    extern exc_SyntaxError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_SyntaxError]
    lea rdx, [rel exc_SyntaxError_type]
    call add_exc_type_builtin

    extern exc_IndentationError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_IndentationError]
    lea rdx, [rel exc_IndentationError_type]
    call add_exc_type_builtin

    extern exc_TabError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_TabError]
    lea rdx, [rel exc_TabError_type]
    call add_exc_type_builtin

    extern exc_EOFError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_EOFError]
    lea rdx, [rel exc_EOFError_type]
    call add_exc_type_builtin

    extern exc_UnicodeDecodeError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeDecodeError]
    lea rdx, [rel exc_UnicodeDecodeError_type]
    call add_exc_type_builtin

    extern exc_UnicodeEncodeError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeEncodeError]
    lea rdx, [rel exc_UnicodeEncodeError_type]
    call add_exc_type_builtin

    extern exc_ConnectionError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ConnectionError]
    lea rdx, [rel exc_ConnectionError_type]
    call add_exc_type_builtin

    extern exc_ConnectionResetError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ConnectionResetError]
    lea rdx, [rel exc_ConnectionResetError_type]
    call add_exc_type_builtin

    extern exc_ConnectionRefusedError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ConnectionRefusedError]
    lea rdx, [rel exc_ConnectionRefusedError_type]
    call add_exc_type_builtin

    extern exc_ConnectionAbortedError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ConnectionAbortedError]
    lea rdx, [rel exc_ConnectionAbortedError_type]
    call add_exc_type_builtin

    extern exc_BrokenPipeError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_BrokenPipeError]
    lea rdx, [rel exc_BrokenPipeError_type]
    call add_exc_type_builtin

    extern exc_PermissionError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_PermissionError]
    lea rdx, [rel exc_PermissionError_type]
    call add_exc_type_builtin

    extern exc_IsADirectoryError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_IsADirectoryError]
    lea rdx, [rel exc_IsADirectoryError_type]
    call add_exc_type_builtin

    extern exc_NotADirectoryError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_NotADirectoryError]
    lea rdx, [rel exc_NotADirectoryError_type]
    call add_exc_type_builtin

    extern exc_ProcessLookupError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ProcessLookupError]
    lea rdx, [rel exc_ProcessLookupError_type]
    call add_exc_type_builtin

    extern exc_ChildProcessError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ChildProcessError]
    lea rdx, [rel exc_ChildProcessError_type]
    call add_exc_type_builtin

    extern exc_BlockingIOError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_BlockingIOError]
    lea rdx, [rel exc_BlockingIOError_type]
    call add_exc_type_builtin

    extern exc_InterruptedError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_InterruptedError]
    lea rdx, [rel exc_InterruptedError_type]
    call add_exc_type_builtin

    extern exc_FloatingPointError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_FloatingPointError]
    lea rdx, [rel exc_FloatingPointError_type]
    call add_exc_type_builtin

    extern exc_BufferError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_BufferError]
    lea rdx, [rel exc_BufferError_type]
    call add_exc_type_builtin

    extern exc_ReferenceError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ReferenceError]
    lea rdx, [rel exc_ReferenceError_type]
    call add_exc_type_builtin

    extern exc_SystemError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_SystemError]
    lea rdx, [rel exc_SystemError_type]
    call add_exc_type_builtin

    extern exc_RuntimeWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_RuntimeWarning]
    lea rdx, [rel exc_RuntimeWarning_type]
    call add_exc_type_builtin

    extern exc_FutureWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_FutureWarning]
    lea rdx, [rel exc_FutureWarning_type]
    call add_exc_type_builtin

    extern exc_ImportWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ImportWarning]
    lea rdx, [rel exc_ImportWarning_type]
    call add_exc_type_builtin

    extern exc_UnicodeWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeWarning]
    lea rdx, [rel exc_UnicodeWarning_type]
    call add_exc_type_builtin

    extern exc_ResourceWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ResourceWarning]
    lea rdx, [rel exc_ResourceWarning_type]
    call add_exc_type_builtin

    extern exc_BytesWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_BytesWarning]
    lea rdx, [rel exc_BytesWarning_type]
    call add_exc_type_builtin

    extern exc_PendingDeprecationWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_PendingDeprecationWarning]
    lea rdx, [rel exc_PendingDeprecationWarning_type]
    call add_exc_type_builtin

    extern exc_SyntaxWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_SyntaxWarning]
    lea rdx, [rel exc_SyntaxWarning_type]
    call add_exc_type_builtin

    extern exc_EncodingWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_EncodingWarning]
    lea rdx, [rel exc_EncodingWarning_type]
    call add_exc_type_builtin

    ; Register data types as builtins
    mov rdi, rbx
    lea rsi, [rel bi_name_list]
    lea rdx, [rel list_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_dict]
    lea rdx, [rel dict_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_tuple]
    lea rdx, [rel tuple_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_set]
    lea rdx, [rel set_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_bytes]
    lea rdx, [rel bytes_type]
    extern bytes_type_call
    lea rcx, [rel bytes_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_bytearray]
    lea rdx, [rel bytearray_type]
    lea rcx, [rel bytearray_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_memoryview]
    lea rdx, [rel memoryview_type]
    lea rcx, [rel memoryview_type_call]
    call add_builtin_type

    ; eval
    mov rdi, rbx
    lea rsi, [rel bi_name_eval]
    extern builtin_eval_fn
    lea rdx, [rel builtin_eval_fn]
    call dict_add_builtin_func

    ; compile
    mov rdi, rbx
    lea rsi, [rel bi_name_compile]
    extern builtin_compile_fn
    lea rdx, [rel builtin_compile_fn]
    call dict_add_builtin_func

    ; exec
    mov rdi, rbx
    lea rsi, [rel bi_name_exec]
    extern builtin_exec_fn
    lea rdx, [rel builtin_exec_fn]
    call dict_add_builtin_func

    ; round
    mov rdi, rbx
    lea rsi, [rel bi_name_round]
    extern builtin_round_fn
    lea rdx, [rel builtin_round_fn]
    call dict_add_builtin_func

    ; pow
    mov rdi, rbx
    lea rsi, [rel bi_name_pow]
    extern builtin_pow_fn
    lea rdx, [rel builtin_pow_fn]
    call dict_add_builtin_func

    ; input
    mov rdi, rbx
    lea rsi, [rel bi_name_input]
    extern builtin_input_fn
    lea rdx, [rel builtin_input_fn]
    call dict_add_builtin_func

    ; open
    mov rdi, rbx
    lea rsi, [rel bi_name_open]
    extern builtin_open_fn
    lea rdx, [rel builtin_open_fn]
    call dict_add_builtin_func

    ; bin
    mov rdi, rbx
    lea rsi, [rel bi_name_bin]
    extern builtin_bin
    lea rdx, [rel builtin_bin]
    call dict_add_builtin_func

    ; oct
    mov rdi, rbx
    lea rsi, [rel bi_name_oct]
    extern builtin_oct
    lea rdx, [rel builtin_oct]
    call dict_add_builtin_func

    ; ascii
    mov rdi, rbx
    lea rsi, [rel bi_name_ascii]
    extern builtin_ascii_fn
    lea rdx, [rel builtin_ascii_fn]
    call dict_add_builtin_func

    ; format
    mov rdi, rbx
    lea rsi, [rel bi_name_format]
    extern builtin_format_fn
    lea rdx, [rel builtin_format_fn]
    call dict_add_builtin_func

    ; vars
    mov rdi, rbx
    lea rsi, [rel bi_name_vars]
    extern builtin_vars_fn
    lea rdx, [rel builtin_vars_fn]
    call dict_add_builtin_func

    ; delattr
    mov rdi, rbx
    lea rsi, [rel bi_name_delattr]
    extern builtin_delattr_fn
    lea rdx, [rel builtin_delattr_fn]
    call dict_add_builtin_func

    ; aiter
    mov rdi, rbx
    lea rsi, [rel bi_name_aiter]
    extern builtin_aiter_fn
    lea rdx, [rel builtin_aiter_fn]
    call dict_add_builtin_func

    ; anext
    mov rdi, rbx
    lea rsi, [rel bi_name_anext]
    extern builtin_anext_fn
    lea rdx, [rel builtin_anext_fn]
    call dict_add_builtin_func

    ; __import__
    mov rdi, rbx
    lea rsi, [rel bi_name___import__]
    extern builtin_import_fn
    lea rdx, [rel builtin_import_fn]
    call dict_add_builtin_func

    ; slice
    mov rdi, rbx
    lea rsi, [rel bi_name_slice]
    extern slice_type
    extern slice_type_call
    lea rdx, [rel slice_type]
    lea rcx, [rel slice_type_call]
    call add_builtin_type

    ; frozenset
    mov rdi, rbx
    lea rsi, [rel bi_name_frozenset]
    extern frozenset_type
    extern frozenset_type_call
    lea rdx, [rel frozenset_type]
    lea rcx, [rel frozenset_type_call]
    call add_builtin_type

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

    ; Create key string (heap — used as dict key, then DECREFed)
    mov rdi, rsi
    call str_from_cstr_heap
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

bi_name_breakpoint:   db "breakpoint", 0
bi_name_print:        db "print", 0
bi_name_len:          db "len", 0
bi_name_range:        db "range", 0
bi_name_type:         db "type", 0
bi_name_isinstance:   db "isinstance", 0
bi_name_issubclass:   db "issubclass", 0
bi_name_repr:         db "repr", 0
bi_name_float:        db "float", 0
bi_name_complex:      db "complex", 0
bi_name_bool:         db "bool", 0
bi_name_object:       db "object", 0
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
isi_class_attr:  db "__class__", 0
bi_name_sorted:       db "sorted", 0
bi_name_chain:        db "chain", 0
bi_name_divmod:       db "divmod", 0
bi_name_globals:      db "globals", 0
bi_name_locals:       db "locals", 0
bi_name_dir:          db "dir", 0
bi_name_eval:         db "eval", 0
bi_dunder_name:       db "__name__", 0
bi_builtins_name:     db "builtins", 0
bi_name_compile:      db "compile", 0
bi_name_exec:         db "exec", 0
bi_name_super:        db "super", 0
bi_name_staticmethod: db "staticmethod", 0
bi_name_classmethod:  db "classmethod", 0
bi_name_property:     db "property", 0
bi_name_NotImplemented: db "NotImplemented", 0
bi_name_Ellipsis:      db "Ellipsis", 0

; Exception type names
bi_name_BaseException:     db "BaseException", 0
bi_name_Exception:         db "Exception", 0
bi_name_TypeError:         db "TypeError", 0
bi_name_ValueError:        db "ValueError", 0
bi_name_KeyError:          db "KeyError", 0
bi_name_IndexError:        db "IndexError", 0
bi_name_AttributeError:    db "AttributeError", 0
bi_name_NameError:         db "NameError", 0
bi_name_UnboundLocalError: db "UnboundLocalError", 0
bi_name_RuntimeError:      db "RuntimeError", 0
bi_name_StopIteration:     db "StopIteration", 0
bi_name_ZeroDivisionError: db "ZeroDivisionError", 0
bi_name_NotImplementedError: db "NotImplementedError", 0
bi_name_OverflowError:     db "OverflowError", 0
bi_name_AssertionError:    db "AssertionError", 0
bi_name_OSError:           db "OSError", 0
bi_name_FileNotFoundError: db "FileNotFoundError", 0
bi_name_FileExistsError: db "FileExistsError", 0
bi_name_UnicodeTranslateError: db "UnicodeTranslateError", 0
bi_name_IOError: db "IOError", 0
bi_name_EnvironmentError: db "EnvironmentError", 0
bi_name_LookupError:       db "LookupError", 0
bi_name_ArithmeticError:   db "ArithmeticError", 0
bi_name_RecursionError:    db "RecursionError", 0
bi_name_ImportError:       db "ImportError", 0
bi_name_MemoryError:       db "MemoryError", 0
bi_name_KeyboardInterrupt: db "KeyboardInterrupt", 0
bi_name_SystemExit:        db "SystemExit", 0
bi_name_UnicodeError:      db "UnicodeError", 0
bi_name_Warning:           db "Warning", 0
bi_name_DeprecationWarning: db "DeprecationWarning", 0
bi_name_UserWarning:       db "UserWarning", 0
bi_name_BaseExceptionGroup: db "BaseExceptionGroup", 0
bi_name_ExceptionGroup:    db "ExceptionGroup", 0
bi_name_CancelledError:    db "CancelledError", 0
bi_name_StopAsyncIteration: db "StopAsyncIteration", 0
bi_name_TimeoutError:      db "TimeoutError", 0
bi_name_GeneratorExit:     db "GeneratorExit", 0
bi_name_ModuleNotFoundError: db "ModuleNotFoundError", 0
bi_name_SyntaxError:       db "SyntaxError", 0
bi_name_IndentationError:  db "IndentationError", 0
bi_name_TabError:          db "TabError", 0
bi_name_EOFError:          db "EOFError", 0
bi_name_UnicodeDecodeError: db "UnicodeDecodeError", 0
bi_name_UnicodeEncodeError: db "UnicodeEncodeError", 0
bi_name_ConnectionError:   db "ConnectionError", 0
bi_name_ConnectionResetError: db "ConnectionResetError", 0
bi_name_ConnectionRefusedError: db "ConnectionRefusedError", 0
bi_name_ConnectionAbortedError: db "ConnectionAbortedError", 0
bi_name_BrokenPipeError:   db "BrokenPipeError", 0
bi_name_PermissionError:   db "PermissionError", 0
bi_name_IsADirectoryError: db "IsADirectoryError", 0
bi_name_NotADirectoryError: db "NotADirectoryError", 0
bi_name_ProcessLookupError: db "ProcessLookupError", 0
bi_name_ChildProcessError: db "ChildProcessError", 0
bi_name_BlockingIOError:   db "BlockingIOError", 0
bi_name_InterruptedError:  db "InterruptedError", 0
bi_name_FloatingPointError: db "FloatingPointError", 0
bi_name_BufferError:       db "BufferError", 0
bi_name_ReferenceError:    db "ReferenceError", 0
bi_name_SystemError:       db "SystemError", 0
bi_name_RuntimeWarning:    db "RuntimeWarning", 0
bi_name_FutureWarning:     db "FutureWarning", 0
bi_name_ImportWarning:     db "ImportWarning", 0
bi_name_UnicodeWarning:    db "UnicodeWarning", 0
bi_name_ResourceWarning:   db "ResourceWarning", 0
bi_name_BytesWarning:      db "BytesWarning", 0
bi_name_PendingDeprecationWarning: db "PendingDeprecationWarning", 0
bi_name_SyntaxWarning:     db "SyntaxWarning", 0
bi_name_EncodingWarning:   db "EncodingWarning", 0
bi_name_list:              db "list", 0
bi_name_dict:              db "dict", 0
bi_name_tuple:             db "tuple", 0
bi_name_set:               db "set", 0
bi_name_bytes:             db "bytes", 0
bi_name_bytearray:         db "bytearray", 0
bi_name_memoryview:        db "memoryview", 0
bi_name_round:             db "round", 0
bi_name_pow:               db "pow", 0
bi_name_input:             db "input", 0
bi_name_open:              db "open", 0
bi_name_bin:               db "bin", 0
bi_name_oct:               db "oct", 0
bi_name_ascii:             db "ascii", 0
bi_name_format:            db "format", 0
bi_name_vars:              db "vars", 0
bi_name_delattr:           db "delattr", 0
bi_name_aiter:             db "aiter", 0
bi_name_anext:             db "anext", 0
bi_name___import__:        db "__import__", 0
bi_name_slice:             db "slice", 0
bi_name_frozenset:         db "frozenset", 0

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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots
