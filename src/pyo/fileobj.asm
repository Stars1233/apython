; fileobj.asm - File-like object type for sys.stdout/stderr/stdin
; Provides write/flush/fileno/isatty/readable/writable/seekable/close methods

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern str_from_cstr
extern str_from_cstr_heap
extern str_type
extern int_from_i64
extern none_singleton
extern bool_true
extern bool_false
extern type_type
extern sys_write
extern sys_close
extern dict_new
extern dict_set
extern builtin_func_new

; ============================================================================
; fileobj_new(int fd, const char *name_cstr, const char *mode_cstr) -> PyFileObject*
; ============================================================================
DEF_FUNC fileobj_new
    push rbx
    push r12
    push r13

    mov rbx, rdi                ; fd
    mov r12, rsi                ; name_cstr
    mov r13, rdx                ; mode_cstr

    ; Allocate PyFileObject
    mov edi, PyFileObject_size
    call ap_malloc
    mov rdi, rax
    push rdi                    ; save obj

    ; Fill header
    mov qword [rdi + PyObject.ob_refcnt], 1
    lea rax, [rel file_type]
    mov [rdi + PyObject.ob_type], rax
    mov [rdi + PyFileObject.file_fd], rbx

    ; Create name string (heap — stored in single-qword struct field)
    mov rdi, r12
    call str_from_cstr_heap
    mov rdi, [rsp]
    mov [rdi + PyFileObject.file_name], rax

    ; Create mode string (heap — stored in single-qword struct field)
    mov rdi, r13
    call str_from_cstr_heap
    mov rdi, [rsp]
    mov [rdi + PyFileObject.file_mode], rax

    pop rax                     ; return obj
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC fileobj_new

; ============================================================================
; fileobj_dealloc(PyObject *self)
; ============================================================================
DEF_FUNC_LOCAL fileobj_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF name
    mov rdi, [rbx + PyFileObject.file_name]
    test rdi, rdi
    jz .no_name
    call obj_decref
.no_name:
    ; DECREF mode
    mov rdi, [rbx + PyFileObject.file_mode]
    test rdi, rdi
    jz .no_mode
    call obj_decref
.no_mode:
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC fileobj_dealloc

; ============================================================================
; fileobj_repr(PyObject *self) -> PyObject*
; Returns "<_io.TextIOWrapper name='<stdout>' mode='w' encoding='utf-8'>"
; Simplified: just return the name
; ============================================================================
DEF_FUNC_LOCAL fileobj_repr
    mov rax, [rdi + PyFileObject.file_name]
    test rax, rax
    jz .fallback
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.fallback:
    lea rdi, [rel fileobj_repr_str]
    call str_from_cstr
    leave
    ret
END_FUNC fileobj_repr

; ============================================================================
; fileobj_write(PyObject **args, int64_t nargs) -> PyObject*
; write(str) -> int (char count)
; args[0] = self (file obj), args[1] = str to write
; Called as a builtin method via tp_dict dispatch
; ============================================================================
DEF_FUNC fileobj_write
    cmp rsi, 2
    jl .write_error

    ; rdi = args array, rsi = nargs
    ; args[0] = self (file obj), args[1] = string to write
    mov rax, rdi                ; rax = args
    mov rdi, [rax]              ; rdi = self (file obj)
    mov rsi, [rax + 16]        ; rsi = string arg payload (16-byte stride)
    mov r9, [rax + 24]         ; r9 = string arg tag

    ; Get fd
    mov rcx, [rdi + PyFileObject.file_fd]

    ; SmallStr check
    test r9, r9
    js .write_smallstr

    ; Heap string: get data + length
    lea rdx, [rsi + PyStrObject.data]
    mov r8, [rsi + PyStrObject.ob_size]

    ; sys_write(fd, buf, len)
    push r8                     ; save length for return
    mov rdi, rcx                ; fd
    mov rsi, rdx                ; buf
    mov rdx, r8                 ; len
    call sys_write
    pop rdi                     ; length

    ; Return char count as int
    call int_from_i64
    leave
    ret

.write_smallstr:
    ; SmallStr: spill to stack for contiguous bytes
    ; rcx = fd, rsi = payload, r9 = tag
    push rbx
    mov ebx, ecx               ; save fd

    SMALLSTR_LEN r8, r9        ; r8 = length
    ; Extract string bytes 8-13 from tag (skip TAG_SMALLSTR)
    mov rax, r9
    shr rax, 8
    mov rdx, 0x0000FFFFFFFFFFFF
    and rax, rdx
    sub rsp, 16
    mov [rsp], rsi             ; bytes 0-7
    mov [rsp + 8], rax         ; bytes 8-13
    ; sys_write(fd, buf, len)
    push r8                    ; save length for return
    mov edi, ebx               ; fd
    lea rsi, [rsp + 8]        ; buf (past saved r8)
    mov rdx, r8                ; len
    call sys_write
    pop rdi                    ; length
    add rsp, 16                ; remove temp buffer
    pop rbx
    ; Return char count as int
    call int_from_i64
    leave
    ret

.write_error:
    extern raise_exception
    extern exc_TypeError_type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "write() takes exactly 1 argument"
    call raise_exception
END_FUNC fileobj_write

; ============================================================================
; fileobj_flush(PyObject **args, int64_t nargs) -> PyObject*
; No-op for unbuffered I/O
; ============================================================================
DEF_FUNC fileobj_flush
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
END_FUNC fileobj_flush

; ============================================================================
; fileobj_fileno(PyObject **args, int64_t nargs) -> PyObject*
; ============================================================================
DEF_FUNC fileobj_fileno
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_fd]
    call int_from_i64
    leave
    ret
END_FUNC fileobj_fileno

; ============================================================================
; fileobj_isatty(PyObject **args, int64_t nargs) -> PyObject*
; Returns True for fd <= 2, False otherwise
; ============================================================================
DEF_FUNC fileobj_isatty
    mov rax, [rdi]              ; self
    mov rcx, [rax + PyFileObject.file_fd]
    cmp rcx, 2
    jbe .is_tty
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.is_tty:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
END_FUNC fileobj_isatty

; ============================================================================
; fileobj_writable(PyObject **args, int64_t nargs) -> PyObject*
; ============================================================================
DEF_FUNC fileobj_writable
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_mode]
    ; Check if mode contains 'w'
    cmp byte [rdi + PyStrObject.data], 'w'
    je .yes
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.yes:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
END_FUNC fileobj_writable

; ============================================================================
; fileobj_readable(PyObject **args, int64_t nargs) -> PyObject*
; ============================================================================
DEF_FUNC fileobj_readable
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_mode]
    cmp byte [rdi + PyStrObject.data], 'r'
    je .yes
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.yes:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
END_FUNC fileobj_readable

; ============================================================================
; fileobj_seekable(PyObject **args, int64_t nargs) -> PyObject*
; ============================================================================
DEF_FUNC fileobj_seekable
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
END_FUNC fileobj_seekable

; ============================================================================
; fileobj_close_method(PyObject **args, int64_t nargs) -> PyObject*
; ============================================================================
DEF_FUNC fileobj_close_method
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_fd]
    call sys_close
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
END_FUNC fileobj_close_method

; ============================================================================
; fileobj_read(PyObject **args, int64_t nargs) -> str
; Read all content from file (or up to size bytes if arg given)
; args[0] = self (fileobj)
; ============================================================================
extern sys_read
extern ap_memcpy

FR_FRAME equ 8208  ; 8192 buf + 16 overhead
DEF_FUNC fileobj_read, FR_FRAME
    push rbx
    push r12

    mov rbx, [rdi]              ; self (fileobj)
    mov r12, [rbx + PyFileObject.file_fd]  ; fd

    ; Read into stack buffer
    lea rsi, [rbp - FR_FRAME]
    mov edx, 8192
    mov edi, r12d
    call sys_read
    ; rax = bytes read
    test rax, rax
    jle .fr_empty

    ; Null-terminate and create string
    mov rbx, rax                ; save length
    lea rdi, [rbp - FR_FRAME]
    mov byte [rdi + rbx], 0
    call str_from_cstr

    pop r12
    pop rbx
    leave
    ret

.fr_empty:
    CSTRING rdi, ""
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret
END_FUNC fileobj_read

; ============================================================================
; fileobj_readline(PyObject **args, int64_t nargs) -> str
; Read one line from file
; ============================================================================
FRL_FRAME equ 8208
DEF_FUNC fileobj_readline, FRL_FRAME
    push rbx
    push r12
    push r13

    mov rbx, [rdi]              ; self
    mov r12, [rbx + PyFileObject.file_fd]  ; fd
    xor r13d, r13d              ; bytes read so far

.frl_loop:
    cmp r13, 8190
    jge .frl_done               ; buffer full

    ; Read one byte at a time
    lea rsi, [rbp - FRL_FRAME]
    add rsi, r13
    mov edx, 1
    mov edi, r12d
    call sys_read
    test rax, rax
    jle .frl_done               ; EOF or error

    ; Check for newline
    lea rdi, [rbp - FRL_FRAME]
    cmp byte [rdi + r13], 10    ; '\n'
    je .frl_got_newline

    inc r13
    jmp .frl_loop

.frl_got_newline:
    inc r13                     ; include the newline

.frl_done:
    ; Create string from buffer
    lea rdi, [rbp - FRL_FRAME]
    mov byte [rdi + r13], 0     ; null-terminate
    call str_from_cstr
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC fileobj_readline

; ============================================================================
; fileobj_getattr(PyObject *self, PyObject *name_str) -> PyObject*
; Attribute access for file objects: encoding, errors, name, mode, methods
; ============================================================================
DEF_FUNC fileobj_getattr
    push rbx
    push r12
    mov rbx, rdi                ; self
    mov r12, rsi                ; name_str

    ; Compare name against known attributes
    lea rdi, [r12 + PyStrObject.data]

    ; Check "write"
    lea rsi, [rel fa_write]
    extern ap_strcmp
    call ap_strcmp
    test eax, eax
    jz .ret_write

    ; Check "flush"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_flush]
    call ap_strcmp
    test eax, eax
    jz .ret_flush

    ; Check "fileno"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_fileno]
    call ap_strcmp
    test eax, eax
    jz .ret_fileno

    ; Check "isatty"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_isatty]
    call ap_strcmp
    test eax, eax
    jz .ret_isatty

    ; Check "writable"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_writable]
    call ap_strcmp
    test eax, eax
    jz .ret_writable

    ; Check "readable"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_readable]
    call ap_strcmp
    test eax, eax
    jz .ret_readable

    ; Check "seekable"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_seekable]
    call ap_strcmp
    test eax, eax
    jz .ret_seekable

    ; Check "close"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_close]
    call ap_strcmp
    test eax, eax
    jz .ret_close

    ; Check "read"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "read"
    call ap_strcmp
    test eax, eax
    jz .ret_read

    ; Check "readline"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "readline"
    call ap_strcmp
    test eax, eax
    jz .ret_readline

    ; Check "encoding"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_encoding]
    call ap_strcmp
    test eax, eax
    jz .ret_encoding

    ; Check "errors"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_errors]
    call ap_strcmp
    test eax, eax
    jz .ret_errors

    ; Check "name"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_name]
    call ap_strcmp
    test eax, eax
    jz .ret_name

    ; Check "mode"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_mode]
    call ap_strcmp
    test eax, eax
    jz .ret_mode

    ; Check "closed"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_closed]
    call ap_strcmp
    test eax, eax
    jz .ret_closed

    ; Check "newlines"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_newlines]
    call ap_strcmp
    test eax, eax
    jz .ret_newlines

    ; Check "line_buffering"
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_line_buffering]
    call ap_strcmp
    test eax, eax
    jz .ret_line_buffering

    ; Unknown attribute
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.ret_write:
    lea rdi, [rel fileobj_write]
    lea rsi, [rel fa_write]
    call builtin_func_new
    jmp .bind_method

.ret_flush:
    lea rdi, [rel fileobj_flush]
    lea rsi, [rel fa_flush]
    call builtin_func_new
    jmp .bind_method

.ret_fileno:
    lea rdi, [rel fileobj_fileno]
    lea rsi, [rel fa_fileno]
    call builtin_func_new
    jmp .bind_method

.ret_isatty:
    lea rdi, [rel fileobj_isatty]
    lea rsi, [rel fa_isatty]
    call builtin_func_new
    jmp .bind_method

.ret_writable:
    lea rdi, [rel fileobj_writable]
    lea rsi, [rel fa_writable]
    call builtin_func_new
    jmp .bind_method

.ret_readable:
    lea rdi, [rel fileobj_readable]
    lea rsi, [rel fa_readable]
    call builtin_func_new
    jmp .bind_method

.ret_seekable:
    lea rdi, [rel fileobj_seekable]
    lea rsi, [rel fa_seekable]
    call builtin_func_new
    jmp .bind_method

.ret_close:
    lea rdi, [rel fileobj_close_method]
    lea rsi, [rel fa_close]
    call builtin_func_new
    jmp .bind_method

.ret_read:
    lea rdi, [rel fileobj_read]
    CSTRING rsi, "read"
    call builtin_func_new
    jmp .bind_method

.ret_readline:
    lea rdi, [rel fileobj_readline]
    CSTRING rsi, "readline"
    call builtin_func_new
    jmp .bind_method

.bind_method:
    ; Create bound method: method_new(func, self)
    push rax                    ; save func
    extern method_new
    mov rdi, rax                ; func
    mov rsi, rbx                ; self
    call method_new
    ; DECREF the unbound func
    push rax
    mov rdi, [rsp + 8]
    call obj_decref
    pop rax
    add rsp, 8
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ret_encoding:
    lea rdi, [rel fa_utf8]
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret

.ret_errors:
    lea rdi, [rel fa_surrogateescape]
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret

.ret_name:
    mov rax, [rbx + PyFileObject.file_name]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ret_mode:
    mov rax, [rbx + PyFileObject.file_mode]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ret_closed:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ret_newlines:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ret_line_buffering:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

END_FUNC fileobj_getattr

; ============================================================================
; Data
; ============================================================================
section .rodata

fileobj_repr_str: db "<file object>", 0
fileobj_type_name: db "TextIOWrapper", 0

; Attribute names
fa_write:     db "write", 0
fa_flush:     db "flush", 0
fa_fileno:    db "fileno", 0
fa_isatty:    db "isatty", 0
fa_writable:  db "writable", 0
fa_readable:  db "readable", 0
fa_seekable:  db "seekable", 0
fa_close:     db "close", 0
fa_encoding:  db "encoding", 0
fa_errors:    db "errors", 0
fa_name:      db "name", 0
fa_mode:      db "mode", 0
fa_closed:    db "closed", 0
fa_newlines:  db "newlines", 0
fa_line_buffering: db "line_buffering", 0
fa_utf8:      db "utf-8", 0
fa_surrogateescape: db "surrogateescape", 0

; ============================================================================
; file_type type object
; ============================================================================
section .data
align 8
global file_type
file_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq fileobj_type_name        ; tp_name
    dq PyFileObject_size        ; tp_basicsize
    dq fileobj_dealloc          ; tp_dealloc
    dq fileobj_repr             ; tp_repr
    dq fileobj_repr             ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq fileobj_getattr          ; tp_getattr
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
