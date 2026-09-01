; iomod.asm - the _io built-in module
;
; CPython implements _io in C and ships Lib/_pyio.py as a Python replica of
; it.  This is the split the same way round: what needs the machine -- the
; type objects the rest of the stack subclasses, and the exception the whole
; module raises -- lives here, and the buffering and text layers live in
; lib/_pyio.py, which is CPython's own code with the parts this interpreter
; does not need removed.
;
; The four base types are near-empty on purpose.  They exist because
; Lib/io.py writes
;
;     class IOBase(_io._IOBase, metaclass=abc.ABCMeta): ...
;
; for each of them, and because _compression, tarfile and gzip subclass the
; public names in turn.  Making them heaptypes with type_from_parts rather
; than hand-written static tables is what lets a metaclass, a __dict__ and
; abc.register all work without four more tables to keep in step.

%include "macros.inc"
%include "object.inc"

ASM_INIT

extern dict_new
extern dict_set
extern str_from_cstr_heap
extern module_new
extern builtin_func_new
extern obj_decref
extern obj_incref
extern obj_dealloc
extern tuple_new
extern type_from_parts
extern none_singleton
extern raise_exception
extern exc_TypeError_type
extern exc_OSError_type
extern exc_ValueError_type
extern exc_BlockingIOError_type
extern str_type

; The buffer size Lib/io.py exports and _pyio's open() defaults to.  CPython
; picks it from the file's st_blksize when that is larger; this is its floor
; and the value every caller sees when it is not.
IO_DEFAULT_BUFFER_SIZE equ 8192

section .rodata

im_name:            db "_io", 0
im_n_IOBase:        db "_IOBase", 0
im_n_RawIOBase:     db "_RawIOBase", 0
im_n_BufferedIOBase: db "_BufferedIOBase", 0
im_n_TextIOBase:    db "_TextIOBase", 0
im_n_Unsupported:   db "UnsupportedOperation", 0
im_n_BlockingIO:    db "BlockingIOError", 0
im_n_defbufsize:    db "DEFAULT_BUFFER_SIZE", 0
im_n_text_encoding: db "text_encoding", 0
im_n_module:        db "__module__", 0
im_n_qualname:      db "__qualname__", 0
im_v_locale:        db "locale", 0

section .text

;; ============================================================================
;; io_new_type(rdi = name cstr, rsi = bases tuple or NULL) -> rax = type
;;
;; A class with an empty body, built the way `class X(B): pass` is.  __module__
;; is set to "_io" in the namespace rather than patched afterwards, because
;; that is where type_from_parts reads it and where repr() looks.
;; ============================================================================
INT_NAME  equ 8
INT_BASES equ 16
INT_NS    equ 24
INT_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC_LOCAL io_new_type, INT_FRAME
    mov [rbp - INT_BASES], rsi
    call str_from_cstr_heap
    mov [rbp - INT_NAME], rax

    call dict_new
    test rax, rax
    jz .int_fail
    mov [rbp - INT_NS], rax

    ; namespace["__module__"] = "_io"
    lea rdi, [rel im_n_module]
    call str_from_cstr_heap
    push rax
    lea rdi, [rel im_name]
    call str_from_cstr_heap
    push rax
    mov rdi, [rbp - INT_NS]
    mov rsi, [rsp + 8]
    mov rdx, rax
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; namespace["__qualname__"] = the name itself
    lea rdi, [rel im_n_qualname]
    call str_from_cstr_heap
    push rax
    mov rdi, [rbp - INT_NS]
    mov rsi, rax
    mov rdx, [rbp - INT_NAME]
    call dict_set
    pop rdi
    call obj_decref

    mov rdi, [rbp - INT_NAME]
    mov rsi, [rbp - INT_BASES]
    mov rdx, [rbp - INT_NS]
    call type_from_parts
    ; The namespace is NOT released here: type_from_parts takes it over as
    ; tp_dict without an incref, so a decref frees the type's own dict out
    ; from under it and the next dict allocated lands on top of it.
    push rax
    mov rdi, [rbp - INT_NAME]
    call obj_decref
    pop rax
    leave
    ret
.int_fail:
    xor eax, eax
    leave
    ret
END_FUNC io_new_type

;; ============================================================================
;; io_bases1(rdi = a type) -> rax = a one-element tuple holding it
;; io_bases2(rdi, rsi = two types) -> rax = a two-element tuple
;;
;; type_from_parts takes ownership of nothing, but the tuple holds strong
;; references, so each base is increfed on the way in.
;; ============================================================================
DEF_FUNC_LOCAL io_bases1
    push rbx
    push r12
    mov rbx, rdi
    mov edi, 1
    call tuple_new
    mov r12, rax
    mov rdi, rbx
    call obj_incref
    mov rax, [r12 + PyTupleObject.ob_item]
    mov [rax], rbx
    mov rax, r12
    pop r12
    pop rbx
    leave
    ret
END_FUNC io_bases1

DEF_FUNC_LOCAL io_bases2
    push rbx
    push r12
    push r13
    sub rsp, 8                  ; 3 pushes + 8 = 32, and rsp stays aligned
    mov rbx, rdi
    mov r13, rsi
    mov edi, 2
    call tuple_new
    mov r12, rax
    mov rdi, rbx
    call obj_incref
    mov rdi, r13
    call obj_incref
    mov rax, [r12 + PyTupleObject.ob_item]
    mov [rax], rbx
    mov [rax + 8], r13
    mov rax, r12
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC io_bases2

;; ============================================================================
;; _io.text_encoding(encoding, stacklevel=2) -> str
;;
;; CPython's helper for "the caller did not say": it hands back the encoding
;; it was given, and "locale" when that was None.  Every open() in the stdlib
;; that takes an encoding= routes through it, which is why it is here and not
;; in the Python layer.
;; ============================================================================
DEF_FUNC io_text_encoding_fn
    test rsi, rsi
    jz .te_argerr
    mov rax, [rdi]
    test rax, rax
    jz .te_default
    LOAD_NONE rcx
    cmp rax, rcx
    je .te_default
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    leave
    ret
.te_default:
    lea rdi, [rel im_v_locale]
    call str_from_cstr_heap
    mov edx, TAG_PTR
    leave
    ret
.te_argerr:
    RAISE exc_TypeError_type, "text_encoding expected at least 1 argument, got 0"
END_FUNC io_text_encoding_fn

;; ============================================================================
;; io_module_create() -> rax = the _io module object
;; ============================================================================
IMC_ENT   equ 8
IMC_KEY   equ 16
IMC_BASE  equ 24
IMC_FRAME equ 32            ; + 3 pushes = 56, not 16-aligned

%macro IO_ADD_OBJ 2         ; %1 = name symbol, %2 = an owned object in a reg
    mov [rbp - IMC_ENT], %2
    lea rdi, [rel %1]
    call str_from_cstr_heap
    mov [rbp - IMC_KEY], rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rbp - IMC_ENT]
    call dict_set
    mov rdi, [rbp - IMC_ENT]
    call obj_decref
    mov rdi, [rbp - IMC_KEY]
    call obj_decref
%endmacro

%macro IO_ADD_INT 2         ; %1 = name symbol, %2 = the value
    mov rdi, %2
    V_PACK_I64 rdi, rcx
    mov [rbp - IMC_ENT], rdi
    lea rdi, [rel %1]
    call str_from_cstr_heap
    mov [rbp - IMC_KEY], rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rbp - IMC_ENT]
    call dict_set
    mov rax, [rbp - IMC_ENT]
    DECREF_V rax, rcx           ; V_PACK_I64 may have boxed it
    mov rdi, [rbp - IMC_KEY]
    call obj_decref
%endmacro

global io_module_create
DEF_FUNC io_module_create, IMC_FRAME
    push rbx
    push r12
    push r13

    call dict_new
    mov r12, rax                ; r12 = module dict

    ; _IOBase(object), then the three that derive from it.
    lea rdi, [rel im_n_IOBase]
    xor esi, esi
    call io_new_type
    mov rbx, rax                ; rbx = _IOBase, one reference kept here
    mov rdi, rax
    call obj_incref
    IO_ADD_OBJ im_n_IOBase, rax

    mov rdi, rbx
    call io_bases1
    mov [rbp - IMC_BASE], rax
    lea rdi, [rel im_n_RawIOBase]
    mov rsi, rax
    call io_new_type
    mov r13, rax
    mov rdi, [rbp - IMC_BASE]
    call obj_decref
    IO_ADD_OBJ im_n_RawIOBase, r13

    mov rdi, rbx
    call io_bases1
    mov [rbp - IMC_BASE], rax
    lea rdi, [rel im_n_BufferedIOBase]
    mov rsi, rax
    call io_new_type
    mov r13, rax
    mov rdi, [rbp - IMC_BASE]
    call obj_decref
    IO_ADD_OBJ im_n_BufferedIOBase, r13

    mov rdi, rbx
    call io_bases1
    mov [rbp - IMC_BASE], rax
    lea rdi, [rel im_n_TextIOBase]
    mov rsi, rax
    call io_new_type
    mov r13, rax
    mov rdi, [rbp - IMC_BASE]
    call obj_decref
    IO_ADD_OBJ im_n_TextIOBase, r13

    mov rdi, rbx
    call obj_decref             ; the reference io_new_type handed back

    ; UnsupportedOperation derives from BOTH OSError and ValueError, which is
    ; not decoration: code that has never heard of io catches one or the other
    ; -- a seek on a pipe must look like an OSError, and a bad argument like a
    ; ValueError.
    lea rdi, [rel exc_OSError_type]
    lea rsi, [rel exc_ValueError_type]
    call io_bases2
    mov [rbp - IMC_BASE], rax
    lea rdi, [rel im_n_Unsupported]
    mov rsi, rax
    call io_new_type
    mov r13, rax
    mov rdi, [rbp - IMC_BASE]
    call obj_decref
    IO_ADD_OBJ im_n_Unsupported, r13

    ; BlockingIOError is an ordinary builtin exception; _io re-exports it so
    ; that `from _io import BlockingIOError` works as it does in CPython.
    lea rax, [rel exc_BlockingIOError_type]
    inc qword [rax + PyObject.ob_refcnt]
    IO_ADD_OBJ im_n_BlockingIO, rax

    IO_ADD_INT im_n_defbufsize, IO_DEFAULT_BUFFER_SIZE

    MODULE_ADD_FUNC io_text_encoding_fn, im_n_text_encoding

    lea rdi, [rel im_name]
    call str_from_cstr_heap
    push rax
    mov rdi, rax
    mov rsi, r12
    call module_new
    mov r13, rax
    pop rdi
    call obj_decref
    mov rdi, r12
    call obj_decref             ; module_new took its own reference

    mov rax, r13
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC io_module_create
