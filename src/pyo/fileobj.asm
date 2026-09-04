; fileobj.asm - File-like object type for sys.stdout/stderr/stdin
; Provides write/flush/fileno/isatty/readable/writable/seekable/close methods

%include "macros.inc"
%include "object.inc"

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
extern ap_memcpy
extern builtin_func_new

;; ============================================================================
;; fileobj_new(int fd, const char *name_cstr, const char *mode_cstr) -> PyFileObject*
;; ============================================================================
DEF_FUNC fileobj_new, 8            ; 3 pushes, so rsp is 16-aligned
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
    mov qword [rdi + PyFileObject.file_len], 0

    ; Block-buffered when it is not a terminal, which is CPython's rule and
    ; the only one that is observable: a terminal wants each line as it is
    ; produced, and a pipe or a file wants whole blocks.  stderr is never
    ; buffered -- CPython's is line-buffered and everything written to it is
    ; already a whole line -- so the interleaving a program sees through a
    ; pipe is stderr first and stdout at the end, which is what CPython
    ; shows and what this did not.
    mov qword [rdi + PyFileObject.file_buffered], 0
    cmp rbx, 1
    jne .fn_unbuffered
    push rdi
    mov rdi, rbx
    call fileobj_fd_isatty
    pop rdi
    test eax, eax
    jnz .fn_unbuffered
    mov qword [rdi + PyFileObject.file_buffered], 1
.fn_unbuffered:

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

;; ============================================================================
;; fileobj_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL fileobj_dealloc, 8            ; 1 push, so rsp is 16-aligned
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

;; ============================================================================
;; fileobj_repr(PyObject *self) -> PyObject*
;; Returns "<_io.TextIOWrapper name='<stdout>' mode='w' encoding='utf-8'>"
;; Simplified: just return the name
;; ============================================================================
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

;; ============================================================================
;; fileobj_write(PyObject **args, int64_t nargs) -> rax = Value
;; write(str) -> int (char count)
;; args[0] = self (file obj), args[1] = str to write
;; Called as a builtin method via tp_dict dispatch
;; ============================================================================
DEF_FUNC fileobj_write
    cmp rsi, 2
    jl .write_error

    ; rdi = args array, rsi = nargs
    ; args[0] = self (file obj), args[1] = string to write
    mov rax, rdi                ; rax = args
    mov rdi, [rax]              ; rdi = self (file obj)
    mov rsi, [rax + 8]        ; rsi = the string argument Value
    V_UNPACK rsi, r9       ; args[1]

    ; The data pointer is taken below, so the argument has to be a str: an
    ; immediate's payload is not an address, and nothing else carries its
    ; text at PyStrObject.data.  write(5) read and emitted wild memory.
    cmp r9d, TAG_PTR
    jne .write_type_error
    mov rcx, [rsi + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .write_type_error

    ; Heap string: get data + length
    lea rdx, [rsi + PyStrObject.data]
    mov r8, [rsi + PyStrObject.ob_size]

    push r8                     ; save length for return
    mov rsi, rdx                ; buf
    mov rdx, r8                 ; len
    call fileobj_emit           ; rdi = self, rsi = buf, rdx = len
    pop rdi                     ; length

    ; Return char count as int
    call int_from_i64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.write_error:
    extern raise_exception
    extern exc_TypeError_type
    RAISE exc_TypeError_type, "write() takes exactly 1 argument"

.write_type_error:
    RAISE exc_TypeError_type, "write() argument must be str"
END_FUNC fileobj_write

;; ============================================================================
;; fileobj_emit(rdi = self, rsi = data, rdx = length)
;;
;; One write, or a buffered one.  Anything longer than the buffer goes
;; straight out behind whatever is already waiting, which keeps the order
;; right without growing the buffer for a single large write.
;; ============================================================================
FE_SELF  equ 8
FE_DATA  equ 16
FE_LEN   equ 24
FE_FRAME equ 32             ; 24 used + 8 pad = 32, 16-aligned
DEF_FUNC fileobj_emit, FE_FRAME
    mov [rbp - FE_SELF], rdi
    mov [rbp - FE_DATA], rsi
    mov [rbp - FE_LEN], rdx

    cmp qword [rdi + PyFileObject.file_buffered], 0
    je .fe_direct

    ; Would it fit?  If not, drain first, and then take the straight path if
    ; it still would not.
    mov rax, [rdi + PyFileObject.file_len]
    add rax, rdx
    cmp rax, FILE_BUFSZ
    jbe .fe_append
    call fileobj_drain
    mov rdi, [rbp - FE_SELF]
    mov rdx, [rbp - FE_LEN]
    cmp rdx, FILE_BUFSZ
    ja .fe_direct

.fe_append:
    mov rdi, [rbp - FE_SELF]
    mov rax, [rdi + PyFileObject.file_len]
    lea rdi, [rdi + PyFileObject.file_buf]
    add rdi, rax
    mov rsi, [rbp - FE_DATA]
    mov rdx, [rbp - FE_LEN]
    call ap_memcpy
    mov rdi, [rbp - FE_SELF]
    mov rax, [rbp - FE_LEN]
    add [rdi + PyFileObject.file_len], rax
    leave
    ret

.fe_direct:
    mov rdi, [rbp - FE_SELF]
    mov rdi, [rdi + PyFileObject.file_fd]
    mov rsi, [rbp - FE_DATA]
    mov rdx, [rbp - FE_LEN]
    call sys_write
    leave
    ret
END_FUNC fileobj_emit

;; ============================================================================
;; fileobj_drain(rdi = self) -- write out whatever is waiting.  Safe on an
;; unbuffered file, where there never is any.
;; ============================================================================
global fileobj_drain
DEF_FUNC fileobj_drain, 8            ; 1 push, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdx, [rbx + PyFileObject.file_len]
    test rdx, rdx
    jz .fd_done
    mov qword [rbx + PyFileObject.file_len], 0
    mov rdi, [rbx + PyFileObject.file_fd]
    lea rsi, [rbx + PyFileObject.file_buf]
    call sys_write
.fd_done:
    pop rbx
    leave
    ret
END_FUNC fileobj_drain

;; ============================================================================
;; fileobj_write_fd(rdi = fd, rsi = data, rdx = length)
;;
;; What print() writes through.  print assembles a line in a stack buffer and
;; hands it to a descriptor rather than to a file object -- the `file=`
;; keyword is read as an fd -- so this is where the two meet: a write to the
;; descriptor sys.stdout owns goes through its buffer, and anything else goes
;; straight out.  Without it print bypassed the buffer entirely and the
;; interleaving was unchanged.
;; ============================================================================
WFD_FD   equ 8
WFD_BUF  equ 16
WFD_LEN  equ 24
WFD_FRAME equ 32            ; 24 used + 8 pad = 32, 16-aligned
global fileobj_write_fd
DEF_FUNC fileobj_write_fd, WFD_FRAME
    mov [rbp - WFD_FD], rdi
    mov [rbp - WFD_BUF], rsi
    mov [rbp - WFD_LEN], rdx

    extern sys_stdout_obj
    mov rax, [rel sys_stdout_obj]
    test rax, rax
    jz .wfd_direct
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel file_type]
    cmp rcx, rdx
    jne .wfd_direct             ; sys.stdout was replaced by a Python object
    cmp qword [rax + PyFileObject.file_buffered], 0
    je .wfd_direct
    cmp rdi, [rax + PyFileObject.file_fd]
    jne .wfd_direct

    mov rdi, rax
    mov rsi, [rbp - WFD_BUF]
    mov rdx, [rbp - WFD_LEN]
    call fileobj_emit
    leave
    ret

.wfd_direct:
    mov rdi, [rbp - WFD_FD]
    mov rsi, [rbp - WFD_BUF]
    mov rdx, [rbp - WFD_LEN]
    call sys_write
    leave
    ret
END_FUNC fileobj_write_fd

;; ============================================================================
;; fileobj_flush_std() -- drain sys.stdout, wherever the interpreter is about
;; to write somewhere else or stop.  Called at exit, and before anything is
;; read from stdin.
;; ============================================================================
global fileobj_flush_std
DEF_FUNC fileobj_flush_std
    extern sys_stdout_obj
    mov rdi, [rel sys_stdout_obj]
    test rdi, rdi
    jz .ffs_done
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel file_type]
    cmp rax, rcx
    jne .ffs_done               ; sys.stdout was replaced by a Python object
    call fileobj_drain
.ffs_done:
    leave
    ret
END_FUNC fileobj_flush_std

;; ============================================================================
;; fileobj_flush(PyObject **args, int64_t nargs) -> rax = Value
;; ============================================================================
DEF_FUNC fileobj_flush
    mov rdi, [rdi]              ; self
    call fileobj_drain
    RET_NONE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
END_FUNC fileobj_flush

;; ============================================================================
;; fileobj_fileno(PyObject **args, int64_t nargs) -> rax = Value
;; ============================================================================
DEF_FUNC fileobj_fileno
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_fd]
    call int_from_i64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC fileobj_fileno

;; ============================================================================
;; fileobj_isatty(PyObject **args, int64_t nargs) -> rax = Value
;; Asks the kernel.  Assuming fd <= 2 is a terminal answered True for a
;; redirected stdout, which is exactly when a program checks.
;; ============================================================================
IAT_BUF   equ 72          ; struct termios is 60 bytes
IAT_FRAME equ 80            ; + 0 pushes = 80

;; fileobj_fd_isatty(rdi = fd) -> eax = 1 when it is a terminal.  The same
;; question fileobj_new asks to decide whether to buffer.
DEF_FUNC fileobj_fd_isatty, IAT_FRAME
    mov esi, 0x5401             ; TCGETS
    lea rdx, [rbp - IAT_BUF]
    extern sys_ioctl
    call sys_ioctl
    test rax, rax
    js .fdi_no
    mov eax, 1
    leave
    ret
.fdi_no:
    xor eax, eax
    leave
    ret
END_FUNC fileobj_fd_isatty

DEF_FUNC fileobj_isatty, IAT_FRAME
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_fd]
    call fileobj_fd_isatty
    test eax, eax
    jnz .is_tty
    RET_FALSE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
.is_tty:
    RET_TRUE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
END_FUNC fileobj_isatty

;; ============================================================================
;; fileobj_writable(PyObject **args, int64_t nargs) -> rax = Value
;; ============================================================================
DEF_FUNC fileobj_writable
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_mode]
    ; Check if mode contains 'w'
    cmp byte [rdi + PyStrObject.data], 'w'
    je .yes
    RET_FALSE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
.yes:
    RET_TRUE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
END_FUNC fileobj_writable

;; ============================================================================
;; fileobj_readable(PyObject **args, int64_t nargs) -> rax = Value
;; ============================================================================
DEF_FUNC fileobj_readable
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_mode]
    cmp byte [rdi + PyStrObject.data], 'r'
    je .yes
    RET_FALSE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
.yes:
    RET_TRUE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
END_FUNC fileobj_readable

;; ============================================================================
;; fileobj_seekable(PyObject **args, int64_t nargs) -> rax = Value
;; ============================================================================
DEF_FUNC fileobj_seekable
    RET_FALSE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
END_FUNC fileobj_seekable

;; ============================================================================
; fileobj_enter(args, nargs) -> the file itself
DEF_FUNC fileobj_enter
    mov rax, [rdi]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC fileobj_enter

; fileobj_exit(args, nargs) -> False, after closing
DEF_FUNC fileobj_exit
    call fileobj_close_method
    ; The result is None; __exit__ answers False so an exception propagates.
    V_UNPACK rax, rdx
    push rax
    push rdx
    mov rdi, rax
    call obj_decref
    add rsp, 16
    extern bool_false
    RET_FALSE
    leave
    V_PACK rax, rdx
    ret
END_FUNC fileobj_exit

; fileobj_close_method(PyObject **args, int64_t nargs) -> rax = Value
;; ============================================================================
DEF_FUNC fileobj_close_method
    mov rax, [rdi]              ; self
    mov rdi, [rax + PyFileObject.file_fd]
    call sys_close
    RET_NONE
    leave                       ; then read it as an int tag and biased the
    V_PACK rax, rdx             ; singleton pointer into a large integer
    ret
END_FUNC fileobj_close_method

;; ============================================================================
;; fileobj_read(PyObject **args, int64_t nargs) -> str
;; Read all content from file (or up to size bytes if arg given)
;; args[0] = self (fileobj)
;; ============================================================================
extern sys_read

FR_FRAME equ 8208  ; 8192 buf + 16 overhead
DEF_FUNC fileobj_read, FR_FRAME
    ; Anything waiting on stdout goes out before anything is read: a prompt
    ; written with print() and then read against has to be visible first,
    ; which is why CPython flushes stdout at the same point.
    push rdi
    push rsi
    call fileobj_flush_std
    pop rsi
    pop rdi
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
    V_PACK rax, rdx             ; builtins return one Value
    ret

.fr_empty:
    CSTRING rdi, ""
    call str_from_cstr
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC fileobj_read

;; ============================================================================
;; fileobj_readline(PyObject **args, int64_t nargs) -> str
;; Read one line from file
;; ============================================================================
FRL_FRAME equ 8208          ; + 3 pushes = 8232, not 16-aligned
DEF_FUNC fileobj_readline, FRL_FRAME
    ; Anything waiting on stdout goes out before anything is read: a prompt
    ; written with print() and then read against has to be visible first,
    ; which is why CPython flushes stdout at the same point.
    push rdi
    push rsi
    call fileobj_flush_std
    pop rsi
    pop rdi
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
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC fileobj_readline

;; ============================================================================
;; fileobj_getattr(PyObject *self, PyObject *name_str) -> rax = Value
;; Attribute access for file objects: encoding, errors, name, mode, methods
;; ============================================================================
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

    ; A file is a context manager: `with open(...) as f` is the ordinary way
    ; to use one, and it raised TypeError because neither dunder existed.
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_enter]
    call ap_strcmp
    test eax, eax
    jz .ret_enter

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel fa_exit]
    call ap_strcmp
    test eax, eax
    jz .ret_exit

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
    V_PACK rax, rdx             ; return one Value
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

.ret_enter:
    lea rdi, [rel fileobj_enter]
    lea rsi, [rel fa_enter]
    call builtin_func_new
    jmp .bind_method

.ret_exit:
    lea rdi, [rel fileobj_exit]
    lea rsi, [rel fa_exit]
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
    V_PACK rax, rdx             ; return one Value
    ret

.ret_encoding:
    lea rdi, [rel fa_utf8]
    call str_from_cstr
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ret_errors:
    lea rdi, [rel fa_surrogateescape]
    call str_from_cstr
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ret_name:
    mov rax, [rbx + PyFileObject.file_name]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ret_mode:
    mov rax, [rbx + PyFileObject.file_mode]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ret_closed:
    RET_FALSE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ret_newlines:
    RET_NONE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ret_line_buffering:
    RET_FALSE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

END_FUNC fileobj_getattr

;; ============================================================================
;; Data
;; ============================================================================
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
fa_enter:     db "__enter__", 0
fa_exit:      db "__exit__", 0
fa_encoding:  db "encoding", 0
fa_errors:    db "errors", 0
fa_name:      db "name", 0
fa_mode:      db "mode", 0
fa_closed:    db "closed", 0
fa_newlines:  db "newlines", 0
fa_line_buffering: db "line_buffering", 0
fa_utf8:      db "utf-8", 0
fa_surrogateescape: db "surrogateescape", 0

;; ============================================================================
;; file_type type object
;; ============================================================================
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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots
