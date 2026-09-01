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

; EAGAIN, the one read/write error that is not an error: on a non-blocking
; descriptor it means "nothing yet", and CPython answers None rather than
; raising so the caller can tell it apart from EOF.
EAGAIN equ 11
EISDIR equ 21
S_IFMT  equ 0o170000
S_IFDIR equ 0o040000

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
    mov rdi, r13
    call obj_incref             ; IO_ADD_OBJ consumes a reference; FileIO
    IO_ADD_OBJ im_n_RawIOBase, r13   ; still needs the type as its base

    ; FileIO is built here, while _RawIOBase is still in r13 and has not been
    ; handed to the module dict yet.
    mov rdi, r13
    call io_make_fileio
    push r13                    ; _RawIOBase, the extra reference above
    mov r13, rax
    IO_ADD_OBJ im_n_FileIO, r13
    pop rdi
    call obj_decref

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
    mov [rel io_unsupported_type], rax   ; the raise sites in FileIO need it
    mov rdi, [rbp - IMC_BASE]
    call obj_decref
    mov rdi, r13
    call obj_incref             ; one reference stays in the global, immortal
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

;; ============================================================================
;; _io.FileIO -- the raw layer.  One syscall per call, no buffering, bytes
;; only; everything above it is built out of read/readinto/write/seek.
;;
;; FileIO is a heaptype over _RawIOBase with a patched tp_basicsize, not a
;; static table: type_call allocates tp_basicsize and zero-fills it, so the
;; fields past PyInstanceObject's layout cost one patched number and the type
;; still behaves like a class -- subclassable, with a __dict__, and reachable
;; from a metaclass.
;;
;; name, mode and closed live in that __dict__ rather than behind properties.
;; Reading them is identical; writing one is possible where CPython refuses.
;; ============================================================================

extern sys_open
extern sys_close
extern sys_read
extern sys_write
extern sys_lseek
extern sys_fstat
extern sys_ftruncate
extern sys_ioctl
extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_memcpy
extern bytes_from_data
extern bytes_new
extern str_from_cstr_heap
extern obj_as_index
extern int_is_integer
extern val_to_i64
extern raise_oserror
extern bool_true
extern bool_false
extern dict_new
extern instance_dealloc
extern bytes_like_ptr_len
extern obj_is_true
extern obj_repr
extern str_concat
extern dict_get
extern exc_IndexError_type
extern str_type
extern bytes_type
extern bytearray_type
extern memoryview_type
extern property_type
extern property_construct

section .rodata

im_n_FileIO:     db "FileIO", 0
im_n_init:       db "__init__", 0
im_n_repr:       db "__repr__", 0
im_n_enter:      db "__enter__", 0
im_n_exit:       db "__exit__", 0
im_n_read:       db "read", 0
im_n_readall:    db "readall", 0
im_n_readinto:   db "readinto", 0
im_n_write:      db "write", 0
im_n_seek:       db "seek", 0
im_n_tell:       db "tell", 0
im_n_truncate:   db "truncate", 0
im_n_close:      db "close", 0
im_n_fileno:     db "fileno", 0
im_n_isatty:     db "isatty", 0
im_n_readable:   db "readable", 0
im_n_writable:   db "writable", 0
im_n_seekable:   db "seekable", 0
im_a_name:       db "name", 0
im_a_mode:       db "mode", 0
im_a_closed:     db "closed", 0
im_repr_open:    db "<_io.FileIO name=", 0
im_repr_mid:     db " mode='", 0
im_repr_end:     db "' closefd=True>", 0
im_repr_closed:  db "<_io.FileIO [closed]>", 0
im_default_mode: db "rb", 0
io_msg_not:      db ", not ", 0
io_msg_int:      db "int", 0
io_msg_readinto: db "readinto() argument 1 must be read-write bytes-like object", 0
io_msg_modetype: db "FileIO() argument 'mode' must be str", 0
io_msg_filetype: db "expected str, bytes or os.PathLike object", 0
io_msg_badmode:  db "invalid mode: ", 0
io_msg_onemode:  db "Must have exactly one of create/read/write/append mode and at most one plus", 0
io_msg_nofile:   db "FileIO() missing required argument 'file' (pos 1)", 0

section .bss
io_badmode_char: resb 1
io_badmode_msg:  resb 64

section .rodata

section .text

;; ============================================================================
;; fileio_check(rdi = self) -> returns with the fd in rax, or raises
;;
;; Every operation on a closed file raises ValueError, which is what the
;; buffered layer above relies on to notice a double close.
;; ============================================================================
DEF_FUNC_BARE fileio_check
    test qword [rdi + PyFileIOObject.fio_flags], FIO_OPEN
    jz fileio_closed_error
    mov rax, [rdi + PyFileIOObject.fio_fd]
    ret
END_FUNC fileio_check


;; ============================================================================
;; io_raise_typename(rdi = exception type, rsi = prefix cstr, rdx = an object)
;;
;; Builds "<prefix>, not <typename>" and raises it.  CPython names the type it
;; was handed in every one of these messages, and the name is the useful half:
;; "must be read-write bytes-like object" leaves the caller guessing what was
;; wrong with what they passed.
;; ============================================================================
section .bss
io_msgbuf: resb 256

section .text

DEF_FUNC io_raise_typename
    push rbx
    push r12
    sub rsp, 8
    mov rbx, rdi                ; the exception type
    mov r12, rdx                ; the object

    lea rdi, [rel io_msgbuf]
    mov rdx, 200
    call io_copy_bounded        ; rax = one past the prefix

    lea rsi, [rel io_msg_not]
    mov rdi, rax
    mov rdx, 24
    call io_copy_bounded

    ; The type name, from the type itself: an int subclass reports its own
    ; name, which is what CPython prints too.
    mov rsi, r12
    V_TEST_PTR rsi, rcx
    ja .irt_unknown
    test rsi, rsi
    jz .irt_unknown
    mov rsi, [rsi + PyObject.ob_type]
    mov rsi, [rsi + PyTypeObject.tp_name]
    jmp .irt_have_name
.irt_unknown:
    lea rsi, [rel io_msg_int]
.irt_have_name:
    mov rdi, rax
    mov rdx, 30
    call io_copy_bounded
    mov byte [rax], 0

    mov rdi, rbx
    lea rsi, [rel io_msgbuf]
    call raise_exception
    ud2
END_FUNC io_raise_typename

;; io_copy_bounded(rdi = dest, rsi = src cstr, rdx = max) -> rax = the NUL
DEF_FUNC_LOCAL io_copy_bounded
    xor ecx, ecx
.icb_loop:
    cmp rcx, rdx
    jge .icb_done
    mov al, [rsi + rcx]
    test al, al
    jz .icb_done
    mov [rdi + rcx], al
    inc rcx
    jmp .icb_loop
.icb_done:
    lea rax, [rdi + rcx]
    mov byte [rax], 0
    leave
    ret
END_FUNC io_copy_bounded

DEF_FUNC fileio_closed_error
    RAISE exc_ValueError_type, "I/O operation on closed file"
END_FUNC fileio_closed_error

;; The wrong mode is UnsupportedOperation, not ValueError -- it is a ValueError
;; as well, since UnsupportedOperation derives from both, but code that tests
;; the type by name expects the io one.
DEF_FUNC fileio_not_readable_error
    mov rdi, [rel io_unsupported_type]
    CSTRING rsi, "File not open for reading"
    call raise_exception
    ud2
END_FUNC fileio_not_readable_error

DEF_FUNC fileio_not_writable_error
    mov rdi, [rel io_unsupported_type]
    CSTRING rsi, "File not open for writing"
    call raise_exception
    ud2
END_FUNC fileio_not_writable_error

;; ============================================================================
;; fileio_set_attr(rdi = self, rsi = name cstr, rdx = value object, borrowed)
;;
;; name / mode / closed are ordinary entries in the instance dict, created on
;; demand -- instance_new zero-fills, so the dict is NULL until something
;; needs it.
;; ============================================================================
FSA_SELF  equ 8
FSA_VAL   equ 16
FSA_KEY   equ 24
FSA_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC_LOCAL fileio_set_attr, FSA_FRAME
    mov [rbp - FSA_SELF], rdi
    mov [rbp - FSA_VAL], rdx
    mov rax, [rdi + PyFileIOObject.inst_dict]
    test rax, rax
    jnz .fsa_have_dict
    call dict_new
    mov rdi, [rbp - FSA_SELF]
    mov [rdi + PyFileIOObject.inst_dict], rax
.fsa_have_dict:
    mov rdi, rsi
    call str_from_cstr_heap
    mov [rbp - FSA_KEY], rax
    mov rdi, [rbp - FSA_SELF]
    mov rdi, [rdi + PyFileIOObject.inst_dict]
    mov rsi, [rbp - FSA_KEY]
    mov rdx, [rbp - FSA_VAL]
    call dict_set
    mov rdi, [rbp - FSA_KEY]
    call obj_decref
    leave
    ret
END_FUNC fileio_set_attr

;; ============================================================================
;; fileio_mode_flags(rdi = mode str) -> rax = open() flags, edx = FIO_ flags
;;                                      or rax = -1 on a bad mode
;;
;; CPython's rules: exactly one of r/w/x/a, an optional +, and a b that is
;; required here because a raw file has no other mode.  'U' is gone in 3.12.
;; ============================================================================
DEF_FUNC_LOCAL fileio_mode_flags
    lea rsi, [rdi + PyStrObject.data]
    xor r8d, r8d                ; how many of r/w/x/a seen
    xor r9d, r9d                ; the FIO_ bits
    xor r10d, r10d              ; the O_ bits
    xor r11d, r11d              ; saw '+'
.fmf_loop:
    movzx eax, byte [rsi]
    test al, al
    jz .fmf_done
    inc rsi
    cmp al, 'b'
    je .fmf_loop
    cmp al, 'r'
    je .fmf_r
    cmp al, 'w'
    je .fmf_w
    cmp al, 'x'
    je .fmf_x
    cmp al, 'a'
    je .fmf_a
    cmp al, '+'
    je .fmf_plus
    mov [rel io_badmode_char], al
    mov rax, -2                 ; an unknown letter, which the message names
    leave
    ret
.fmf_r:
    inc r8d
    or r9d, FIO_READABLE
    jmp .fmf_loop
.fmf_w:
    inc r8d
    or r9d, FIO_WRITABLE
    or r10d, O_CREAT | O_TRUNC
    jmp .fmf_loop
.fmf_x:
    inc r8d
    or r9d, FIO_WRITABLE | FIO_CREATED
    or r10d, O_CREAT | O_EXCL
    jmp .fmf_loop
.fmf_a:
    inc r8d
    or r9d, FIO_WRITABLE | FIO_APPENDING
    or r10d, O_CREAT | O_APPEND
    jmp .fmf_loop
.fmf_plus:
    mov r11d, 1
    jmp .fmf_loop
.fmf_done:
    cmp r8d, 1
    jne .fmf_bad
    test r11d, r11d
    jz .fmf_no_plus
    or r9d, FIO_READABLE | FIO_WRITABLE
.fmf_no_plus:
    ; The access mode is decided by what the file can do, not by the letter.
    mov eax, r9d
    and eax, FIO_READABLE | FIO_WRITABLE
    cmp eax, FIO_READABLE | FIO_WRITABLE
    je .fmf_rdwr
    cmp eax, FIO_WRITABLE
    je .fmf_wronly
    or r10d, O_RDONLY
    jmp .fmf_flags
.fmf_rdwr:
    or r10d, O_RDWR
    jmp .fmf_flags
.fmf_wronly:
    or r10d, O_WRONLY
.fmf_flags:
    or r10d, O_CLOEXEC          ; CPython opens non-inheritable and so do we
    mov eax, r10d
    mov edx, r9d
    leave
    ret
.fmf_bad:
    mov rax, -1
    leave
    ret
END_FUNC fileio_mode_flags

;; ============================================================================
;; FileIO.__init__(self, file, mode='r', closefd=True, opener=None)
;;
;; `file` is a path or an already-open descriptor.  An int means adopt it,
;; which is how the buffered layer wraps stdin and stdout.
;; ============================================================================
FI_SELF   equ 8
FI_FILE   equ 16
FI_MODE   equ 24
FI_FLAGS  equ 32
FI_OFLAGS equ 40
FI_ARGS   equ 48
FI_NARGS  equ 56
; Derived, not guessed: a hand-picked offset for a 144-byte struct overlaps
; the scalars above it the first time the struct grows.
FI_STAT   equ 64 + StatBuf_size
FI_FRAME  equ FI_STAT       ; 208, + 0 pushes, 16-aligned

DEF_FUNC fileio_init_fn, FI_FRAME
    cmp rsi, 2
    jl .fi_argerr
    mov [rbp - FI_ARGS], rdi
    mov [rbp - FI_NARGS], rsi
    mov rax, [rdi]
    mov [rbp - FI_SELF], rax
    mov rax, [rdi + 8]
    mov [rbp - FI_FILE], rax
    mov qword [rbp - FI_MODE], 0
    cmp rsi, 3
    jl .fi_have_mode
    mov rax, [rdi + 16]
    mov [rbp - FI_MODE], rax
.fi_have_mode:

    ; The mode string, defaulting to "rb".
    mov rdi, [rbp - FI_MODE]
    test rdi, rdi
    jz .fi_default_mode
    LOAD_NONE rax
    cmp rdi, rax
    je .fi_default_mode
    ; Classify before dereferencing: FileIO(path, 5) put an int immediate here,
    ; and reading ob_type off one uses the number as an address.
    V_TEST_PTR rdi, rax
    ja .fi_mode_type
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .fi_mode_type
    call obj_incref
    mov rdi, [rbp - FI_MODE]
    jmp .fi_parse_mode
.fi_default_mode:
    lea rdi, [rel im_default_mode]
    call str_from_cstr_heap
    mov [rbp - FI_MODE], rax
    mov rdi, rax
.fi_parse_mode:
    call fileio_mode_flags
    cmp rax, -1
    je .fi_mode_bad
    cmp rax, -2
    je .fi_mode_letter
    mov [rbp - FI_OFLAGS], rax
    mov rax, rdx
    or rax, FIO_OPEN | FIO_CLOSEFD
    mov [rbp - FI_FLAGS], rax

    ; An int for `file` adopts the descriptor; closefd then defaults to False
    ; the way CPython's does not -- CPython keeps it True and demands the
    ; caller say otherwise, so match that and only honour an explicit False.
    mov rdi, [rbp - FI_FILE]
    V_UNPACK rdi, rdx
    call int_is_integer
    test eax, eax
    jz .fi_open_path

    mov rdi, [rbp - FI_FILE]
    V_UNPACK rdi, rdx
    call obj_as_index
    cmp rax, 0
    jl .fi_fd_bad
    mov rdi, [rbp - FI_SELF]
    mov [rdi + PyFileIOObject.fio_fd], rax
    jmp .fi_opened

.fi_open_path:
    mov rdi, [rbp - FI_FILE]
    test rdi, rdi
    jz .fi_file_type
    V_TEST_PTR rdi, rax
    ja .fi_file_type
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .fi_file_type
    lea rdi, [rdi + PyStrObject.data]
    mov rsi, [rbp - FI_OFLAGS]
    mov edx, 0o666
    call sys_open
    test rax, rax
    js .fi_open_failed
    mov rdi, [rbp - FI_SELF]
    mov [rdi + PyFileIOObject.fio_fd], rax

.fi_opened:
    mov rdi, [rbp - FI_SELF]
    mov rax, [rbp - FI_FLAGS]
    mov [rdi + PyFileIOObject.fio_flags], rax

    ; closefd=False, argument four.  It only ever turns the bit off: a path
    ; this object opened itself must be closed by it.
    cmp qword [rbp - FI_NARGS], 4
    jl .fi_closefd_done
    mov rdi, [rbp - FI_ARGS]
    mov rdi, [rdi + 24]
    call obj_is_true
    test eax, eax
    jnz .fi_closefd_done
    mov rax, [rbp - FI_SELF]
    and qword [rbp - FI_FLAGS], ~FIO_CLOSEFD
    mov rcx, [rbp - FI_FLAGS]
    mov [rax + PyFileIOObject.fio_flags], rcx
.fi_closefd_done:
    mov rax, [rbp - FI_SELF]

    ; st_blksize gives readall() something better than a fixed guess.
    mov qword [rbp - FI_STAT], 0
    mov rdi, [rax + PyFileIOObject.fio_fd]
    lea rsi, [rbp - FI_STAT]
    call sys_fstat
    mov rcx, IO_DEFAULT_BUFFER_SIZE
    test rax, rax
    js .fi_blk_default

    ; Opening a directory succeeds on Linux and every read then fails with
    ; EISDIR, so the error arrives far from its cause.  CPython checks here
    ; and closes again; the one fstat is already being made for st_blksize.
    mov eax, [rbp - FI_STAT + StatBuf.st_mode]
    and eax, S_IFMT
    cmp eax, S_IFDIR
    je .fi_is_dir
    mov rcx, [rbp - FI_STAT + StatBuf.st_blksize]
    cmp rcx, 1
    jge .fi_blk_have
    mov rcx, IO_DEFAULT_BUFFER_SIZE
.fi_blk_have:
.fi_blk_default:
    mov rax, [rbp - FI_SELF]
    mov [rax + PyFileIOObject.fio_blksize], rcx

    ; name, mode and closed, as ordinary attributes.  fio_name holds a VALUE,
    ; not a pointer: FileIO(3, "rb") adopts a descriptor and the name is then
    ; the number 3, which obj_incref would treat as an address.
    mov rax, [rbp - FI_FILE]
    INCREF_V rax, rcx
    mov rax, [rbp - FI_SELF]
    mov rdi, [rbp - FI_FILE]
    mov [rax + PyFileIOObject.fio_name], rdi
    mov rdi, rax
    lea rsi, [rel im_a_name]
    mov rdx, [rbp - FI_FILE]
    call fileio_set_attr

    ; mode and closed are properties, so neither is an instance attribute:
    ; CPython's FileIO keeps only `name` in its instance dict, and code that
    ; copies a file object's __dict__ would otherwise carry a stale `closed`.
    mov rax, [rbp - FI_SELF]
    mov rcx, [rbp - FI_MODE]
    mov [rax + PyFileIOObject.fio_mode], rcx   ; the reference moves here

    LOAD_NONE rax
    mov rdi, rax
    call obj_incref
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret

.fi_is_dir:
    mov rax, [rbp - FI_SELF]
    mov rdi, [rax + PyFileIOObject.fio_fd]
    call sys_close
    mov edi, EISDIR
    mov rsi, [rbp - FI_FILE]
    call raise_oserror
    ud2

.fi_open_failed:
    neg rax
    mov rdi, rax
    mov rsi, [rbp - FI_FILE]
    call raise_oserror
    ud2
.fi_mode_bad:
    lea rdi, [rel exc_ValueError_type]
    lea rsi, [rel io_msg_onemode]
    call raise_exception
    ud2
.fi_mode_letter:
    lea rdi, [rel io_badmode_msg]
    lea rsi, [rel io_msg_badmode]
    mov rdx, 40
    call io_copy_bounded
    mov cl, [rel io_badmode_char]
    mov [rax], cl
    mov byte [rax + 1], 0
    lea rdi, [rel exc_ValueError_type]
    lea rsi, [rel io_badmode_msg]
    call raise_exception
    ud2
.fi_fd_bad:
    RAISE exc_ValueError_type, "negative file descriptor"
.fi_file_type:
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel io_msg_filetype]
    mov rdx, [rbp - FI_FILE]
    call io_raise_typename
.fi_mode_type:
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel io_msg_modetype]
    mov rdx, [rbp - FI_MODE]
    call io_raise_typename
.fi_argerr:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "FileIO() missing required argument 'file' (pos 1)"
    call raise_exception
    ud2
END_FUNC fileio_init_fn

;; ============================================================================
;; FileIO.read(size=-1) -> bytes or None
;;
;; One read().  A short read is not an error and not EOF: the buffered layer
;; loops.  None means EAGAIN on a non-blocking descriptor, which is the one
;; case CPython distinguishes from b"".
;; ============================================================================
FR_SELF   equ 8
FR_BUF    equ 16
FR_SIZE   equ 24
FR_FRAME  equ 32            ; + 0 pushes = 32

DEF_FUNC fileio_read_fn, FR_FRAME
    test rsi, rsi
    jz .fr_argerr
    mov rax, [rdi]
    mov [rbp - FR_SELF], rax
    mov r8, -1
    cmp rsi, 2
    jl .fr_have_size
    mov rcx, [rdi + 8]
    LOAD_NONE rax
    cmp rcx, rax
    je .fr_have_size
    push rsi
    mov rdi, rcx
    V_UNPACK rdi, rdx
    call obj_as_index
    pop rsi
    mov r8, rax
.fr_have_size:
    mov [rbp - FR_SIZE], r8
    mov rdi, [rbp - FR_SELF]
    call fileio_check
    test qword [rdi + PyFileIOObject.fio_flags], FIO_READABLE
    jz .fr_not_readable
    cmp qword [rbp - FR_SIZE], 0
    jl .fr_readall
    je .fr_empty

    mov rdi, [rbp - FR_SIZE]
    call ap_malloc
    test rax, rax
    jz .fr_nomem
    mov [rbp - FR_BUF], rax
    mov rdi, [rbp - FR_SELF]
    mov rdi, [rdi + PyFileIOObject.fio_fd]
    mov rsi, rax
    mov rdx, [rbp - FR_SIZE]
    call sys_read
    test rax, rax
    js .fr_failed
    mov rdi, [rbp - FR_BUF]
    mov rsi, rax
    call bytes_from_data
    push rax
    mov rdi, [rbp - FR_BUF]
    call ap_free
    pop rax
    mov edx, TAG_PTR
    leave
    ret

.fr_empty:
    xor edi, edi
    xor esi, esi
    call bytes_from_data
    mov edx, TAG_PTR
    leave
    ret

.fr_readall:
    mov rdi, [rbp - FR_SELF]
    call fileio_readall_impl
    mov edx, TAG_PTR
    leave
    ret

.fr_failed:
    neg rax
    cmp rax, EAGAIN
    je .fr_would_block
    mov rdi, [rbp - FR_BUF]
    push rax
    call ap_free
    pop rdi
    xor esi, esi
    call raise_oserror
    ud2
.fr_would_block:
    mov rdi, [rbp - FR_BUF]
    call ap_free
    LOAD_NONE rax
    mov rdi, rax
    call obj_incref
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.fr_nomem:
    RAISE exc_OSError_type, "out of memory"
.fr_not_readable:
    jmp fileio_not_readable_error
.fr_argerr:
    RAISE exc_TypeError_type, "read() missing self"
END_FUNC fileio_read_fn

;; ============================================================================
;; fileio_readall_impl(rdi = self) -> rax = bytes
;;
;; Reads to EOF, growing by st_blksize and doubling once the file turns out to
;; be bigger than its own block size -- which is how a 10 MB file costs a
;; handful of reallocs rather than ten thousand.
;; ============================================================================
FRA_SELF  equ 8
FRA_BUF   equ 16
FRA_CAP   equ 24
FRA_LEN   equ 32
FRA_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC_LOCAL fileio_readall_impl, FRA_FRAME
    mov [rbp - FRA_SELF], rdi
    mov rax, [rdi + PyFileIOObject.fio_blksize]
    cmp rax, 1
    jge .fra_cap_ok
    mov rax, IO_DEFAULT_BUFFER_SIZE
.fra_cap_ok:
    mov [rbp - FRA_CAP], rax
    mov qword [rbp - FRA_LEN], 0
    mov rdi, rax
    call ap_malloc
    test rax, rax
    jz .fra_nomem
    mov [rbp - FRA_BUF], rax

.fra_loop:
    mov rax, [rbp - FRA_LEN]
    cmp rax, [rbp - FRA_CAP]
    jl .fra_room
    ; Full: double, so the total copying stays linear in the file's size.
    mov rax, [rbp - FRA_CAP]
    add rax, rax
    mov [rbp - FRA_CAP], rax
    mov rdi, [rbp - FRA_BUF]
    mov rsi, rax
    call ap_realloc
    test rax, rax
    jz .fra_nomem_free
    mov [rbp - FRA_BUF], rax
.fra_room:
    mov rdi, [rbp - FRA_SELF]
    mov rdi, [rdi + PyFileIOObject.fio_fd]
    mov rsi, [rbp - FRA_BUF]
    add rsi, [rbp - FRA_LEN]
    mov rdx, [rbp - FRA_CAP]
    sub rdx, [rbp - FRA_LEN]
    call sys_read
    test rax, rax
    js .fra_failed
    jz .fra_eof
    add [rbp - FRA_LEN], rax
    jmp .fra_loop

.fra_eof:
    mov rdi, [rbp - FRA_BUF]
    mov rsi, [rbp - FRA_LEN]
    call bytes_from_data
    push rax
    mov rdi, [rbp - FRA_BUF]
    call ap_free
    pop rax
    leave
    ret

.fra_failed:
    neg rax
    cmp rax, EAGAIN
    je .fra_eof                 ; what has been read so far is the answer
    push rax
    mov rdi, [rbp - FRA_BUF]
    call ap_free
    pop rdi
    xor esi, esi
    call raise_oserror
    ud2
.fra_nomem_free:
    mov rdi, [rbp - FRA_BUF]
    call ap_free
.fra_nomem:
    RAISE exc_OSError_type, "out of memory"
END_FUNC fileio_readall_impl

DEF_FUNC fileio_readall_fn
    test rsi, rsi
    jz .fral_argerr
    mov rdi, [rdi]
    call fileio_check
    call fileio_readall_impl
    mov edx, TAG_PTR
    leave
    ret
.fral_argerr:
    RAISE exc_TypeError_type, "readall() missing self"
END_FUNC fileio_readall_fn

;; ============================================================================
;; FileIO.readinto(b) -> int or None
;;
;; The whole reason the buffered layer takes a memoryview: it reads straight
;; into the caller's buffer with no bytes object in between.
;; ============================================================================
FRI_SELF  equ 8
FRI_FRAME equ 16            ; + 0 pushes = 16

DEF_FUNC fileio_readinto_fn, FRI_FRAME
    cmp rsi, 2
    jl .fri_argerr
    mov rax, [rdi]
    mov [rbp - FRI_SELF], rax
    mov rsi, [rdi + 8]
    mov rdi, rax
    call fileio_check
    test qword [rdi + PyFileIOObject.fio_flags], FIO_READABLE
    jz .fri_not_readable

    mov rdi, rsi
    call fileio_writable_buffer   ; rax = data, r10 = length
    mov rsi, rax
    mov rdx, r10
    mov rdi, [rbp - FRI_SELF]
    mov rdi, [rdi + PyFileIOObject.fio_fd]
    call sys_read
    test rax, rax
    js .fri_failed
    mov rdx, rax
    V_PACK_I64 rdx, rcx
    mov rax, rdx
    leave
    ret
.fri_failed:
    neg rax
    cmp rax, EAGAIN
    je .fri_would_block
    mov rdi, rax
    xor esi, esi
    call raise_oserror
    ud2
.fri_would_block:
    LOAD_NONE rax
    mov rdi, rax
    call obj_incref
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.fri_not_readable:
    jmp fileio_not_readable_error
.fri_argerr:
    RAISE exc_TypeError_type, "readinto() takes exactly one argument"
END_FUNC fileio_readinto_fn

;; ============================================================================
;; fileio_writable_buffer(rdi = a Value) -> rax = data, r10 = length
;;
;; readinto() writes through its argument, so a bytes is not acceptable even
;; though bytes_like_ptr_len would happily hand back its data.
;; ============================================================================
DEF_FUNC_LOCAL fileio_writable_buffer
    push rbx
    mov rbx, rdi
    V_TEST_PTR rdi, rax
    ja .fwb_bad
    test rdi, rdi
    jz .fwb_bad
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .fwb_ok
    lea rcx, [rel memoryview_type]
    cmp rax, rcx
    jne .fwb_bad
    cmp qword [rdi + PyMemoryViewObject.mv_readonly], 0
    jne .fwb_bad
.fwb_ok:
    mov rdi, rbx
    call bytes_like_ptr_len
    test ecx, ecx
    jz .fwb_bad
    pop rbx
    leave
    ret
.fwb_bad:
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel io_msg_readinto]
    mov rdx, rbx
    call io_raise_typename
END_FUNC fileio_writable_buffer

;; ============================================================================
;; FileIO.write(b) -> int or None
;; ============================================================================
FW_SELF   equ 8
FW_FRAME  equ 16            ; + 0 pushes = 16

DEF_FUNC fileio_write_fn, FW_FRAME
    cmp rsi, 2
    jl .fw_argerr
    mov rax, [rdi]
    mov [rbp - FW_SELF], rax
    mov rsi, [rdi + 8]
    mov rdi, rax
    call fileio_check
    test qword [rdi + PyFileIOObject.fio_flags], FIO_WRITABLE
    jz .fw_not_writable

    mov rdi, rsi
    call bytes_like_ptr_len       ; rax = data, r10 = length, ecx = ok
    test ecx, ecx
    jz .fw_type
    mov rsi, rax
    mov rdx, r10
    mov rdi, [rbp - FW_SELF]
    mov rdi, [rdi + PyFileIOObject.fio_fd]
    call sys_write
    test rax, rax
    js .fw_failed
    mov rdx, rax
    V_PACK_I64 rdx, rcx
    mov rax, rdx
    leave
    ret
.fw_failed:
    neg rax
    cmp rax, EAGAIN
    je .fw_would_block
    mov rdi, rax
    xor esi, esi
    call raise_oserror
    ud2
.fw_would_block:
    LOAD_NONE rax
    mov rdi, rax
    call obj_incref
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.fw_not_writable:
    jmp fileio_not_writable_error
.fw_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
.fw_argerr:
    RAISE exc_TypeError_type, "write() takes exactly one argument"
END_FUNC fileio_write_fn

;; ============================================================================
;; FileIO.seek(pos, whence=SEEK_SET) -> int   /   FileIO.tell() -> int
;; ============================================================================
FS_SELF   equ 8
FS_POS    equ 16
FS_FRAME  equ 32            ; + 0 pushes = 32

DEF_FUNC fileio_seek_fn, FS_FRAME
    cmp rsi, 2
    jl .fs_argerr
    mov rax, [rdi]
    mov [rbp - FS_SELF], rax
    push rdi
    push rsi
    mov rdi, [rdi + 8]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov [rbp - FS_POS], rax
    pop rsi
    pop rdi
    xor r8d, r8d
    cmp rsi, 3
    jl .fs_have_whence
    mov rdi, [rdi + 16]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov r8, rax
.fs_have_whence:
    push r8
    mov rdi, [rbp - FS_SELF]
    call fileio_check
    pop rdx
    mov rdi, rax
    mov rsi, [rbp - FS_POS]
    call sys_lseek
    test rax, rax
    js .fs_failed
    mov rdx, rax
    V_PACK_I64 rdx, rcx
    mov rax, rdx
    leave
    ret
.fs_failed:
    neg rax
    mov rdi, rax
    xor esi, esi
    call raise_oserror
    ud2
.fs_argerr:
    RAISE exc_TypeError_type, "seek() takes at least one argument"
END_FUNC fileio_seek_fn

DEF_FUNC fileio_tell_fn
    test rsi, rsi
    jz .ft_argerr
    mov rdi, [rdi]
    call fileio_check
    mov rdi, rax
    xor esi, esi
    mov edx, SEEK_CUR
    call sys_lseek
    test rax, rax
    js .ft_failed
    mov rdx, rax
    V_PACK_I64 rdx, rcx
    mov rax, rdx
    leave
    ret
.ft_failed:
    neg rax
    mov rdi, rax
    xor esi, esi
    call raise_oserror
    ud2
.ft_argerr:
    RAISE exc_TypeError_type, "tell() takes no arguments"
END_FUNC fileio_tell_fn

;; ============================================================================
;; FileIO.truncate(size=None) -> int
;;
;; A size of None means "here", which is the current position -- and CPython
;; leaves the position alone either way.
;; ============================================================================
FT_SELF   equ 8
FT_SIZE   equ 16
FT_FRAME  equ 32            ; + 0 pushes = 32

DEF_FUNC fileio_truncate_fn, FT_FRAME
    test rsi, rsi
    jz .ftr_argerr
    mov rax, [rdi]
    mov [rbp - FT_SELF], rax
    mov qword [rbp - FT_SIZE], -1
    cmp rsi, 2
    jl .ftr_have_size
    mov rcx, [rdi + 8]
    LOAD_NONE rax
    cmp rcx, rax
    je .ftr_have_size
    mov rdi, rcx
    V_UNPACK rdi, rdx
    call obj_as_index
    mov [rbp - FT_SIZE], rax
.ftr_have_size:
    mov rdi, [rbp - FT_SELF]
    call fileio_check
    test qword [rdi + PyFileIOObject.fio_flags], FIO_WRITABLE
    jz .ftr_not_writable
    cmp qword [rbp - FT_SIZE], 0
    jge .ftr_do
    ; None: truncate at the current position.
    mov rdi, [rbp - FT_SELF]
    mov rdi, [rdi + PyFileIOObject.fio_fd]
    xor esi, esi
    mov edx, SEEK_CUR
    call sys_lseek
    test rax, rax
    js .ftr_failed
    mov [rbp - FT_SIZE], rax
.ftr_do:
    mov rdi, [rbp - FT_SELF]
    mov rdi, [rdi + PyFileIOObject.fio_fd]
    mov rsi, [rbp - FT_SIZE]
    call sys_ftruncate
    test rax, rax
    js .ftr_failed
    mov rdx, [rbp - FT_SIZE]
    V_PACK_I64 rdx, rcx
    mov rax, rdx
    leave
    ret
.ftr_failed:
    neg rax
    mov rdi, rax
    xor esi, esi
    call raise_oserror
    ud2
.ftr_not_writable:
    jmp fileio_not_writable_error
.ftr_argerr:
    RAISE exc_TypeError_type, "truncate() missing self"
END_FUNC fileio_truncate_fn

;; ============================================================================
;; FileIO.close(), and the predicates
;;
;; close() is idempotent, which the buffered layer relies on: it closes the
;; raw file in its own close() and again in __del__.
;; ============================================================================
FC_SELF   equ 8
FC_FRAME  equ 16            ; + 0 pushes = 16

DEF_FUNC fileio_close_fn, FC_FRAME
    test rsi, rsi
    jz .fc_argerr
    mov rdi, [rdi]
    mov [rbp - FC_SELF], rdi
    test qword [rdi + PyFileIOObject.fio_flags], FIO_OPEN
    jz .fc_done
    mov rax, [rdi + PyFileIOObject.fio_flags]
    and rax, ~FIO_OPEN
    mov [rdi + PyFileIOObject.fio_flags], rax
    test rax, FIO_CLOSEFD
    jz .fc_done
    mov rdi, [rdi + PyFileIOObject.fio_fd]
    call sys_close
.fc_done:
    LOAD_NONE rax
    mov rdi, rax
    call obj_incref
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.fc_argerr:
    RAISE exc_TypeError_type, "close() takes no arguments"
END_FUNC fileio_close_fn

DEF_FUNC fileio_fileno_fn
    test rsi, rsi
    jz .ffn_argerr
    mov rdi, [rdi]
    call fileio_check
    mov rdx, rax
    V_PACK_I64 rdx, rcx
    mov rax, rdx
    leave
    ret
.ffn_argerr:
    RAISE exc_TypeError_type, "fileno() takes no arguments"
END_FUNC fileio_fileno_fn

FIT_BUF   equ 80            ; struct termios is 60 bytes
FIT_FRAME equ 80            ; + 0 pushes = 80

DEF_FUNC fileio_isatty_fn, FIT_FRAME
    test rsi, rsi
    jz .fit_argerr
    mov rdi, [rdi]
    call fileio_check
    mov rdi, rax
    mov esi, TCGETS
    lea rdx, [rbp - FIT_BUF]
    call sys_ioctl
    test rax, rax
    js .fit_false
    lea rax, [rel bool_true]
    jmp .fit_out
.fit_false:
    lea rax, [rel bool_false]
.fit_out:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.fit_argerr:
    RAISE exc_TypeError_type, "isatty() takes no arguments"
END_FUNC fileio_isatty_fn

;; A predicate answers about the mode the file was opened in, and raises on a
;; closed file rather than answering about one.
%macro FIO_PREDICATE 3          ; %1 = name, %2 = the flag, %3 = error label
DEF_FUNC %1
    test rsi, rsi
    jz %%argerr
    mov rdi, [rdi]
    call fileio_check
    test qword [rdi + PyFileIOObject.fio_flags], %2
    jz %%false
    lea rax, [rel bool_true]
    jmp %%out
%%false:
    lea rax, [rel bool_false]
%%out:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
%%argerr:
    RAISE exc_TypeError_type, %3
END_FUNC %1
%endmacro

FIO_PREDICATE fileio_readable_fn, FIO_READABLE, "readable() takes no arguments"
FIO_PREDICATE fileio_writable_fn, FIO_WRITABLE, "writable() takes no arguments"

;; seekable() costs one lseek, and the answer is remembered: a pipe is asked
;; on every buffered read otherwise, and each ask is a failing syscall.
DEF_FUNC fileio_seekable_fn
    test rsi, rsi
    jz .fsk_argerr
    push rbx
    mov rdi, [rdi]
    mov rbx, rdi
    call fileio_check
    test qword [rbx + PyFileIOObject.fio_flags], FIO_SEEK_KNOWN
    jnz .fsk_known
    mov rdi, rax
    xor esi, esi
    mov edx, SEEK_CUR
    call sys_lseek
    mov rcx, [rbx + PyFileIOObject.fio_flags]
    or rcx, FIO_SEEK_KNOWN
    test rax, rax
    js .fsk_store
    or rcx, FIO_SEEKABLE
.fsk_store:
    mov [rbx + PyFileIOObject.fio_flags], rcx
.fsk_known:
    test qword [rbx + PyFileIOObject.fio_flags], FIO_SEEKABLE
    jz .fsk_false
    lea rax, [rel bool_true]
    jmp .fsk_out
.fsk_false:
    lea rax, [rel bool_false]
.fsk_out:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.fsk_argerr:
    RAISE exc_TypeError_type, "seekable() takes no arguments"
END_FUNC fileio_seekable_fn

;; ============================================================================
;; The context manager, which is how nearly every open() in real code is
;; written.  __exit__ closes and swallows nothing.
;; ============================================================================
DEF_FUNC fileio_enter_fn
    test rsi, rsi
    jz .fen_argerr
    mov rdi, [rdi]
    call fileio_check
    call obj_incref
    mov rax, rdi
    mov edx, TAG_PTR
    leave
    ret
.fen_argerr:
    RAISE exc_TypeError_type, "__enter__() takes no arguments"
END_FUNC fileio_enter_fn

DEF_FUNC fileio_exit_fn
    test rsi, rsi
    jz .fex_argerr
    mov esi, 1
    call fileio_close_fn
    leave
    ret
.fex_argerr:
    RAISE exc_TypeError_type, "__exit__() missing self"
END_FUNC fileio_exit_fn

;; ============================================================================
;; repr(FileIO), which CPython prints with the name as given -- a path stays
;; quoted, an adopted descriptor stays a number.
;; ============================================================================
FRP_SELF  equ 8
FRP_PARTS equ 16
FRP_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC fileio_repr_fn, FRP_FRAME
    test rsi, rsi
    jz .frp_argerr
    mov rdi, [rdi]
    mov [rbp - FRP_SELF], rdi
    test qword [rdi + PyFileIOObject.fio_flags], FIO_OPEN
    jz .frp_closed

    lea rdi, [rel im_repr_open]
    call str_from_cstr_heap
    mov [rbp - FRP_PARTS], rax

    mov rdi, [rbp - FRP_SELF]
    mov rdi, [rdi + PyFileIOObject.fio_name]
    call obj_repr
    mov rsi, rax
    lea rdi, [rbp - FRP_PARTS]
    call fileio_str_append

    lea rdi, [rel im_repr_mid]
    call str_from_cstr_heap
    mov rsi, rax
    lea rdi, [rbp - FRP_PARTS]
    call fileio_str_append

    mov rdi, [rbp - FRP_SELF]
    call fileio_mode_str
    mov rsi, rax
    lea rdi, [rbp - FRP_PARTS]
    call fileio_str_append

    lea rdi, [rel im_repr_end]
    call str_from_cstr_heap
    mov rsi, rax
    lea rdi, [rbp - FRP_PARTS]
    call fileio_str_append

    mov rax, [rbp - FRP_PARTS]
    mov edx, TAG_PTR
    leave
    ret
.frp_closed:
    lea rdi, [rel im_repr_closed]
    call str_from_cstr_heap
    mov edx, TAG_PTR
    leave
    ret
.frp_argerr:
    RAISE exc_TypeError_type, "__repr__() takes no arguments"
END_FUNC fileio_repr_fn

;; fileio_str_append(rdi = slot address holding a str, rsi = an owned str)
;; concatenates in place and releases both inputs.
DEF_FUNC_LOCAL fileio_str_append
    push rbx
    push r12
    sub rsp, 8
    mov rbx, rdi
    mov r12, rsi
    mov rdi, [rbx]
    call str_concat
    push rax
    mov rdi, [rbx]
    call obj_decref
    mov rdi, r12
    call obj_decref
    pop rax
    mov [rbx], rax
    add rsp, 8
    pop r12
    pop rbx
    leave
    ret
END_FUNC fileio_str_append

;; fileio_mode_str(rdi = self) -> rax = the mode, one new reference
DEF_FUNC_LOCAL fileio_mode_str
    mov rax, [rdi + PyFileIOObject.fio_mode]
    test rax, rax
    jz .fms_default
    mov rdi, rax
    call obj_incref
    mov rax, rdi
    leave
    ret
.fms_default:
    lea rdi, [rel im_default_mode]
    call str_from_cstr_heap
    leave
    ret
END_FUNC fileio_mode_str

;; ============================================================================
;; fileio_dealloc(rdi = self)
;;
;; A file that goes out of scope still holds a descriptor, and the process has
;; a few thousand.  CPython closes here too (with a ResourceWarning this
;; interpreter has no warnings machinery for).  The heaptype's own dealloc
;; still runs afterwards -- it is the one that releases the instance dict and
;; untracks the object -- so this is a prologue to it, not a replacement.
;; ============================================================================
section .data
align 8
fileio_base_dealloc: dq 0
; UnsupportedOperation is built at module init, so the raise sites cannot name
; it the way RAISE names a static type.
global io_unsupported_type
io_unsupported_type: dq 0

section .text

;; mode and closed are read-only attributes in CPython, so they are properties
;; here too rather than dict entries -- see io_add_property.
DEF_FUNC fileio_mode_get_fn
    test rsi, rsi
    jz .fmg_argerr
    mov rdi, [rdi]
    call fileio_mode_str
    mov edx, TAG_PTR
    leave
    ret
.fmg_argerr:
    RAISE exc_TypeError_type, "mode getter takes no arguments"
END_FUNC fileio_mode_get_fn

DEF_FUNC fileio_closed_get_fn
    test rsi, rsi
    jz .fcg_argerr
    mov rdi, [rdi]
    test qword [rdi + PyFileIOObject.fio_flags], FIO_OPEN
    jz .fcg_true
    lea rax, [rel bool_false]
    jmp .fcg_out
.fcg_true:
    lea rax, [rel bool_true]
.fcg_out:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.fcg_argerr:
    RAISE exc_TypeError_type, "closed getter takes no arguments"
END_FUNC fileio_closed_get_fn

DEF_FUNC_LOCAL fileio_dealloc
    push rbx
    mov rbx, rdi
    test qword [rbx + PyFileIOObject.fio_flags], FIO_OPEN
    jz .fd_no_fd
    mov rax, [rbx + PyFileIOObject.fio_flags]
    and rax, ~FIO_OPEN
    mov [rbx + PyFileIOObject.fio_flags], rax
    test rax, FIO_CLOSEFD
    jz .fd_no_fd
    mov rdi, [rbx + PyFileIOObject.fio_fd]
    call sys_close
.fd_no_fd:
    mov rax, [rbx + PyFileIOObject.fio_name]
    test rax, rax
    jz .fd_no_name
    mov qword [rbx + PyFileIOObject.fio_name], 0
    DECREF_V rax, rcx
.fd_no_name:
    ; instance_dealloc walks every word from the dict slot to tp_basicsize and
    ; DECREF_VALs it, because that is where a subclass's __slots__ live.  The
    ; four fields below are raw numbers, and a descriptor of 3 read as a Value
    ; is a pointer to address 3.  Zeroing them first makes each a NULL Value,
    ; which the walk skips.
    mov rdi, [rbx + PyFileIOObject.fio_mode]
    test rdi, rdi
    jz .fd_no_mode
    mov qword [rbx + PyFileIOObject.fio_mode], 0
    call obj_decref
.fd_no_mode:
    mov qword [rbx + PyFileIOObject.fio_fd], 0
    mov qword [rbx + PyFileIOObject.fio_flags], 0
    mov qword [rbx + PyFileIOObject.fio_blksize], 0
    mov rdi, rbx
    pop rbx
    leave
    jmp [rel fileio_base_dealloc]
END_FUNC fileio_dealloc

;; ============================================================================
;; io_add_method(rdi = namespace dict, rsi = name cstr, rdx = implementation)
;; ============================================================================
IAM_NS    equ 8
IAM_KEY   equ 16
IAM_FN    equ 24
IAM_NAME  equ 32
IAM_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC_LOCAL io_add_method, IAM_FRAME
    mov [rbp - IAM_NS], rdi
    mov [rbp - IAM_NAME], rsi   ; parked: builtin_func_new clobbers it, and
                                ; the name is needed again as the dict key
    mov rdi, rdx
    call builtin_func_new
    mov [rbp - IAM_FN], rax
    mov rdi, [rbp - IAM_NAME]
    call str_from_cstr_heap
    mov [rbp - IAM_KEY], rax
    mov rdi, [rbp - IAM_NS]
    mov rsi, rax
    mov rdx, [rbp - IAM_FN]
    call dict_set
    mov rdi, [rbp - IAM_FN]
    call obj_decref
    mov rdi, [rbp - IAM_KEY]
    call obj_decref
    leave
    ret
END_FUNC io_add_method

%macro IO_METHOD 2              ; %1 = name symbol, %2 = implementation
    mov rdi, rbx
    lea rsi, [rel %1]
    lea rdx, [rel %2]
    call io_add_method
%endmacro

%macro IO_PROPERTY 2            ; %1 = name symbol, %2 = the getter
    mov rdi, rbx
    lea rsi, [rel %1]
    lea rdx, [rel %2]
    call io_add_property
%endmacro

;; ============================================================================
;; io_add_property(rdi = namespace, rsi = name cstr, rdx = getter)
;;
;; A read-only attribute has to be a descriptor, not a dict entry: `closed`
;; is computed from the flags, and an entry would let a caller assign to it
;; and would show up in every instance's __dict__.
;; ============================================================================
IAP_NS    equ 8
IAP_NAME  equ 16
IAP_KEY   equ 24
IAP_PROP  equ 32
IAP_ARG   equ 40
IAP_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC_LOCAL io_add_property, IAP_FRAME
    mov [rbp - IAP_NS], rdi
    mov [rbp - IAP_NAME], rsi
    mov rdi, rdx
    call builtin_func_new
    mov [rbp - IAP_ARG], rax

    ; property(fget) -- property_construct is property_type's tp_call, and it
    ; takes the argument array directly.
    lea rdi, [rel property_type]
    lea rsi, [rbp - IAP_ARG]
    mov edx, 1
    call property_construct
    V_UNPACK rax, rdx
    mov [rbp - IAP_PROP], rax

    mov rdi, [rbp - IAP_ARG]
    call obj_decref

    mov rdi, [rbp - IAP_NAME]
    call str_from_cstr_heap
    mov [rbp - IAP_KEY], rax
    mov rdi, [rbp - IAP_NS]
    mov rsi, rax
    mov rdx, [rbp - IAP_PROP]
    call dict_set
    mov rdi, [rbp - IAP_PROP]
    call obj_decref
    mov rdi, [rbp - IAP_KEY]
    call obj_decref
    leave
    ret
END_FUNC io_add_property

;; ============================================================================
;; io_make_fileio(rdi = _RawIOBase) -> rax = the FileIO type
;; ============================================================================
MFI_BASES equ 8
MFI_NS    equ 16
MFI_NAME  equ 24
MFI_FRAME equ 32            ; + 1 push = 40, not 16-aligned

DEF_FUNC_LOCAL io_make_fileio, MFI_FRAME
    push rbx
    call io_bases1
    mov [rbp - MFI_BASES], rax

    call dict_new
    mov rbx, rax
    mov [rbp - MFI_NS], rax

    IO_METHOD im_n_init,     fileio_init_fn
    IO_METHOD im_n_read,     fileio_read_fn
    IO_METHOD im_n_readall,  fileio_readall_fn
    IO_METHOD im_n_readinto, fileio_readinto_fn
    IO_METHOD im_n_write,    fileio_write_fn
    IO_METHOD im_n_seek,     fileio_seek_fn
    IO_METHOD im_n_tell,     fileio_tell_fn
    IO_METHOD im_n_truncate, fileio_truncate_fn
    IO_METHOD im_n_close,    fileio_close_fn
    IO_METHOD im_n_fileno,   fileio_fileno_fn
    IO_METHOD im_n_isatty,   fileio_isatty_fn
    IO_METHOD im_n_readable, fileio_readable_fn
    IO_METHOD im_n_writable, fileio_writable_fn
    IO_METHOD im_n_seekable, fileio_seekable_fn
    IO_METHOD im_n_enter,    fileio_enter_fn
    IO_METHOD im_n_exit,     fileio_exit_fn
    IO_METHOD im_n_repr,     fileio_repr_fn

    IO_PROPERTY im_a_mode,   fileio_mode_get_fn
    IO_PROPERTY im_a_closed, fileio_closed_get_fn

    ; __module__ so the repr reads _io.FileIO, as CPython's does.
    lea rdi, [rel im_n_module]
    call str_from_cstr_heap
    push rax
    lea rdi, [rel im_name]
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, [rsp + 8]
    mov rdx, rax
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    lea rdi, [rel im_n_FileIO]
    call str_from_cstr_heap
    mov [rbp - MFI_NAME], rax
    mov rdi, rax
    mov rsi, [rbp - MFI_BASES]
    mov rdx, [rbp - MFI_NS]
    call type_from_parts        ; takes over the namespace
    mov rbx, rax

    mov rdi, [rbp - MFI_NAME]
    call obj_decref
    mov rdi, [rbp - MFI_BASES]
    call obj_decref

    ; The two patches that make this a C-level type rather than a plain class:
    ; room for the fields past PyInstanceObject's layout, and a dealloc that
    ; closes the descriptor before the generic one releases the dict.
    mov qword [rbx + PyTypeObject.tp_basicsize], PyFileIOObject_size
    mov rax, [rbx + PyTypeObject.tp_dealloc]
    mov [rel fileio_base_dealloc], rax
    lea rax, [rel fileio_dealloc]
    mov [rbx + PyTypeObject.tp_dealloc], rax

    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC io_make_fileio
