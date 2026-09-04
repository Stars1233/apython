; asyncio_streams.asm - Async I/O stream types for asyncio
;
; Provides: StreamReader, StreamWriter, open_connection, start_server
;
; StreamReader.read(n) — read up to n bytes, returns bytes/str
; StreamWriter.write(data) — write data, returns length
; StreamWriter.close() — close the fd
; StreamWriter.drain() — awaitable flush (no-op for raw sockets)
;
; open_connection(host, port) — connect TCP, return (reader, writer)
; start_server(callback, host, port) — bind + listen + accept loop

%include "macros.inc"
%include "object.inc"
%include "eventloop.inc"

extern ap_malloc
extern ap_free
extern obj_incref
extern obj_dealloc
extern str_from_cstr
extern str_new_heap
extern int_from_i64
extern none_singleton
extern bool_true
extern bool_false
extern type_type
extern builtin_func_new
extern method_new
extern tuple_new
extern raise_exception
extern exc_TypeError_type
extern exc_OSError_type
extern sys_socket
extern sys_connect
extern sys_bind
extern sys_listen
extern sys_accept4
extern sys_setsockopt
extern sys_close
extern sys_read
extern sys_write
extern sys_fcntl

; Socket constants
AF_INET     equ 2
SOCK_STREAM equ 1
SOL_SOCKET  equ 1
SO_REUSEADDR equ 2
F_SETFL     equ 4
O_NONBLOCK  equ 2048

; Stream buffer size
STREAM_BUFSIZE equ 8192

;; ============================================================================
;; StreamReader type implementation
;; ============================================================================

;; stream_reader_new(int fd) -> AsyncStreamReader*
DEF_FUNC stream_reader_new
    push rbx
    mov ebx, edi               ; save fd

    mov edi, AsyncStreamReader_size
    call ap_malloc
    mov qword [rax + AsyncStreamReader.ob_refcnt], 1
    lea rcx, [rel stream_reader_type]
    mov [rax + AsyncStreamReader.ob_type], rcx
    mov [rax + AsyncStreamReader.fd], ebx
    mov dword [rax + AsyncStreamReader.eof], 0

    pop rbx
    leave
    ret
END_FUNC stream_reader_new

;; stream_reader_dealloc(self)
DEF_FUNC_BARE stream_reader_dealloc
    ; Close fd if still open
    mov edi, [rdi + AsyncStreamReader.fd]
    cmp edi, -1
    je .srd_free
    push rdi                   ; save self
    call sys_close
    pop rdi
.srd_free:
    jmp ap_free
END_FUNC stream_reader_dealloc

;; stream_reader_read(args, nargs) — builtin for reader.read(n)
;; args[0] = nbytes (int)
;; Returns a ReadAwaitable

;; stream_reader_getattr(self, name) -> fat value
;; Dispatches: "read" -> returns ReadAwaitable, "close" -> close fd
DEF_FUNC stream_reader_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self (AsyncStreamReader*)
    mov r12, rsi               ; name (PyStrObject*)

    ; Compare name against known attributes
    CSTRING rdi, "read"
    mov rsi, r12
    call _stream_strcmp
    test eax, eax
    jz .srga_read

    CSTRING rdi, "close"
    mov rsi, r12
    call _stream_strcmp
    test eax, eax
    jz .srga_close

    CSTRING rdi, "readline"
    mov rsi, r12
    call _stream_strcmp
    test eax, eax
    jz .srga_readline

    ; Unknown attribute
    RET_NULL

    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.srga_read:
    ; Return a bound method for read
    lea rdi, [rel stream_reader_read_impl]
    lea rsi, [rel srn_read]
    call builtin_func_new
    mov rdi, rax
    mov rsi, rbx
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.srga_close:
    ; Close the fd, return None
    mov edi, [rbx + AsyncStreamReader.fd]
    cmp edi, -1
    je .srga_close_none
    push rbx
    call sys_close
    pop rbx
    mov dword [rbx + AsyncStreamReader.fd], -1
.srga_close_none:
    RET_NONE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.srga_readline:
    ; Return a bound method for readline
    lea rdi, [rel stream_reader_readline_impl]
    lea rsi, [rel srn_readline]
    call builtin_func_new
    mov rdi, rax
    mov rsi, rbx
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC stream_reader_getattr

;; stream_reader_read_impl(args, nargs) — method impl
;; Called via method_call: args[0] = self (reader), args[1] = n (optional)
;; Returns ReadAwaitable
DEF_FUNC stream_reader_read_impl
    push rbx
    push r12

    ; args[0] = self (AsyncStreamReader*)
    mov rbx, [rdi]             ; self payload
    ; Default n = STREAM_BUFSIZE
    mov r12d, STREAM_BUFSIZE

    cmp rsi, 2
    jb .srri_create
    ; args[1] = n
    mov rax, [rdi + 8]       ; payload
    V_UNPACK rax, rdx       ; args[1]
    cmp edx, TAG_SMALLINT
    jne .srri_create
    mov r12d, eax

.srri_create:
    ; Create ReadAwaitable
    mov edi, ReadAwaitable_size
    call ap_malloc
    mov qword [rax + ReadAwaitable.ob_refcnt], 1
    lea rcx, [rel read_awaitable_type]
    mov [rax + ReadAwaitable.ob_type], rcx
    mov ecx, [rbx + AsyncStreamReader.fd]
    mov [rax + ReadAwaitable.fd], ecx
    mov [rax + ReadAwaitable.nbytes], r12d
    mov dword [rax + ReadAwaitable.yielded], 0

    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC stream_reader_read_impl

;; stream_reader_readline_impl(args, nargs) — method impl
;; Called via method_call: args[0] = self (reader)
;; For simplicity: read up to STREAM_BUFSIZE, return everything up to \n
DEF_FUNC stream_reader_readline_impl
    push rbx

    ; args[0] = self (AsyncStreamReader*)
    mov rbx, [rdi]             ; self payload

    ; Create ReadAwaitable with nbytes = STREAM_BUFSIZE (we'll read a chunk)
    mov edi, ReadAwaitable_size
    call ap_malloc
    mov qword [rax + ReadAwaitable.ob_refcnt], 1
    lea rcx, [rel read_awaitable_type]
    mov [rax + ReadAwaitable.ob_type], rcx
    mov ecx, [rbx + AsyncStreamReader.fd]
    mov [rax + ReadAwaitable.fd], ecx
    mov dword [rax + ReadAwaitable.nbytes], STREAM_BUFSIZE
    mov dword [rax + ReadAwaitable.yielded], 0

    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC stream_reader_readline_impl

;; ============================================================================
;; ReadAwaitable — tp_iter / tp_iternext for async read
;; ============================================================================

DEF_FUNC_BARE read_awaitable_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC read_awaitable_iter_self

;; read_awaitable_iternext(self) -> fat value
;; First call: yield TAG_IO_WAIT (fd | POLLIN<<32) to wait for readability
;; Second call: do the actual read, return string result (StopIteration-like)
DEF_FUNC_BARE read_awaitable_iternext
    cmp dword [rdi + ReadAwaitable.yielded], 0
    jne .rai_read

    ; First call: yield IO_WAIT for POLLIN
    mov dword [rdi + ReadAwaitable.yielded], 1
    mov eax, [rdi + ReadAwaitable.fd]
    ; payload = fd | (POLLIN << 32)
    mov rdx, POLLIN
    shl rdx, 32
    or rax, rdx
    or rax, [rel v_iowait_lo]   ; IO_WAIT sentinel Value
    ret

.rai_read:
    ; Second call: do the actual read now that fd is readable
    push rbx
    push r12
    sub rsp, STREAM_BUFSIZE    ; allocate read buffer on stack

    mov ebx, [rdi + ReadAwaitable.fd]
    mov r12d, [rdi + ReadAwaitable.nbytes]

    ; Clamp nbytes to STREAM_BUFSIZE
    cmp r12d, STREAM_BUFSIZE
    jbe .rai_read_ok
    mov r12d, STREAM_BUFSIZE
.rai_read_ok:
    ; sys_read(fd, buf, len)
    mov edi, ebx
    mov rsi, rsp
    mov edx, r12d
    call sys_read
    ; rax = bytes read, or -errno

    test rax, rax
    jle .rai_eof

    ; Create string from buffer
    mov rdi, rsp               ; data
    mov rsi, rax               ; len
    call str_new_heap
    ; rax = string ptr, edx = TAG_PTR

    add rsp, STREAM_BUFSIZE
    pop r12
    pop rbx
    ret

.rai_eof:
    ; EOF or error: return empty string
    CSTRING rdi, ""
    call str_from_cstr
    ; rax = empty string, edx = tag

    add rsp, STREAM_BUFSIZE
    pop r12
    pop rbx
    ret
END_FUNC read_awaitable_iternext

DEF_FUNC_BARE read_awaitable_dealloc
    jmp ap_free
END_FUNC read_awaitable_dealloc

;; ============================================================================
;; StreamWriter type implementation
;; ============================================================================

;; stream_writer_new(int fd) -> AsyncStreamWriter*
DEF_FUNC stream_writer_new
    push rbx
    mov ebx, edi               ; save fd

    mov edi, AsyncStreamWriter_size
    call ap_malloc
    mov qword [rax + AsyncStreamWriter.ob_refcnt], 1
    lea rcx, [rel stream_writer_type]
    mov [rax + AsyncStreamWriter.ob_type], rcx
    mov [rax + AsyncStreamWriter.fd], ebx
    mov dword [rax + AsyncStreamWriter.closed], 0

    pop rbx
    leave
    ret
END_FUNC stream_writer_new

;; stream_writer_dealloc(self)
;; Writer does NOT own the fd — reader owns it.
;; Explicit close via writer.close() attribute is handled by getattr.
DEF_FUNC_BARE stream_writer_dealloc
    jmp ap_free
END_FUNC stream_writer_dealloc

;; stream_writer_getattr(self, name) -> fat value
DEF_FUNC stream_writer_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self (AsyncStreamWriter*)
    mov r12, rsi               ; name

    CSTRING rdi, "write"
    mov rsi, r12
    call _stream_strcmp
    test eax, eax
    jz .swga_write

    CSTRING rdi, "close"
    mov rsi, r12
    call _stream_strcmp
    test eax, eax
    jz .swga_close

    CSTRING rdi, "drain"
    mov rsi, r12
    call _stream_strcmp
    test eax, eax
    jz .swga_drain

    ; Unknown
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.swga_write:
    lea rdi, [rel stream_writer_write_impl]
    lea rsi, [rel swn_write]
    call builtin_func_new
    mov rdi, rax
    mov rsi, rbx
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.swga_close:
    ; Close the fd and return None
    mov edi, [rbx + AsyncStreamWriter.fd]
    cmp edi, -1
    je .swga_close_none
    push rbx
    call sys_close
    pop rbx
    mov dword [rbx + AsyncStreamWriter.fd], -1
    mov dword [rbx + AsyncStreamWriter.closed], 1
.swga_close_none:
    RET_NONE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.swga_drain:
    ; Return a DrainAwaitable (trivial — completes immediately for raw sockets)
    lea rdi, [rel stream_writer_drain_impl]
    lea rsi, [rel swn_drain]
    call builtin_func_new
    mov rdi, rax
    mov rsi, rbx
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC stream_writer_getattr

;; stream_writer_write_impl(args, nargs) — method impl
;; Called via method_call: args[0] = self (writer), args[1] = data
;; Does synchronous write (data should be small for sockets)
;; Returns int (bytes written)
DEF_FUNC stream_writer_write_impl
    push rbx
    push r12
    push r13
    sub rsp, 8                 ; align stack

    ; args[0] = self (AsyncStreamWriter*)
    mov rbx, [rdi]             ; self payload
    cmp rsi, 2
    jb .swwi_error

    ; args[1] = data (string)
    mov rax, [rdi + 8]       ; data payload
    V_UNPACK rax, rdx       ; args[1]

    cmp edx, TAG_PTR
    jne .swwi_type_error

    ; Heap string: get data ptr and length
    mov r12, rax               ; string object
    mov r13, [rax + PyStrObject.ob_size]
    lea rdi, [rax + 32]       ; str.data (PyStrObject.data = +32)

    ; .swwi_do_write:
    ; rdi = data ptr, r13 = length
    mov rsi, rdi               ; buf
    mov edi, [rbx + AsyncStreamWriter.fd]
    mov edx, r13d              ; len
    call sys_write

.swwi_return_count:
    ; rax = bytes written
    test rax, rax
    js .swwi_write_error
    mov rdi, rax
    call int_from_i64
    ; rax = payload (int), edx = TAG_SMALLINT

    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.swwi_error:
    RAISE exc_TypeError_type, "write() requires exactly 1 argument"

.swwi_type_error:
    RAISE exc_TypeError_type, "write() argument must be a string"

.swwi_write_error:
    ; Write failed — return 0
    xor edi, edi
    call int_from_i64
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC stream_writer_write_impl

;; stream_writer_drain_impl(args, nargs) — method impl
;; Returns DrainAwaitable (completes immediately for raw sockets)
DEF_FUNC stream_writer_drain_impl
    push rbx

    ; args[0] = self (AsyncStreamWriter*)
    mov rbx, [rdi]             ; self payload

    ; Create DrainAwaitable
    mov edi, DrainAwaitable_size
    call ap_malloc
    mov qword [rax + DrainAwaitable.ob_refcnt], 1
    lea rcx, [rel drain_awaitable_type]
    mov [rax + DrainAwaitable.ob_type], rcx
    mov ecx, [rbx + AsyncStreamWriter.fd]
    mov [rax + DrainAwaitable.fd], ecx
    mov dword [rax + DrainAwaitable.yielded], 0

    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC stream_writer_drain_impl

;; ============================================================================
;; DrainAwaitable — tp_iter / tp_iternext (completes immediately)
;; ============================================================================

DEF_FUNC_BARE drain_awaitable_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC drain_awaitable_iter_self

DEF_FUNC_BARE drain_awaitable_iternext
    ; Drain completes immediately for raw sockets — return NULL (done)
    RET_NULL
    ret
END_FUNC drain_awaitable_iternext

DEF_FUNC_BARE drain_awaitable_dealloc
    jmp ap_free
END_FUNC drain_awaitable_dealloc

;; ============================================================================
;; ConnectAwaitable — for open_connection result
;; First call: yield TAG_IO_WAIT (fd | POLLOUT<<32) to wait for connect
;; Second call: return (reader, writer) tuple
;; ============================================================================

DEF_FUNC_BARE connect_awaitable_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC connect_awaitable_iter_self

;; connect_awaitable_iternext(self) -> fat value
DEF_FUNC_BARE connect_awaitable_iternext
    cmp dword [rdi + ConnectAwaitable.yielded], 0
    jne .cai_result

    ; First call: yield TAG_IO_WAIT for POLLOUT (connect completion)
    mov dword [rdi + ConnectAwaitable.yielded], 1
    mov eax, [rdi + ConnectAwaitable.fd]
    mov rdx, POLLOUT
    shl rdx, 32
    or rax, rdx
    or rax, [rel v_iowait_lo]   ; IO_WAIT sentinel Value
    ret

.cai_result:
    ; Second call: create (reader, writer) tuple
    push rbx
    push r12

    mov ebx, [rdi + ConnectAwaitable.fd]

    ; Create reader
    mov edi, ebx
    call stream_reader_new
    mov r12, rax               ; r12 = reader

    ; Create writer
    mov edi, ebx
    call stream_writer_new
    push rax                   ; save writer

    ; Create 2-tuple
    mov edi, 2
    call tuple_new
    mov rbx, rax               ; rbx = tuple

    ; Set tuple[0] = reader, tuple[1] = writer
    mov r9, [rax + PyTupleObject.ob_item]
    mov [r9], r12
    pop rcx                    ; writer
    mov [r9 + 8], rcx

    mov rax, rbx

    pop r12
    pop rbx
    ret
END_FUNC connect_awaitable_iternext

DEF_FUNC_BARE connect_awaitable_dealloc
    jmp ap_free
END_FUNC connect_awaitable_dealloc

;; ============================================================================
;; AcceptAwaitable — for start_server result (non-blocking accept)
;; First call: yield TAG_IO_WAIT (listen_fd | POLLIN<<32) to wait for connection
;; Second call: accept4, create reader+writer, return (reader, writer) tuple
;; ============================================================================

DEF_FUNC_BARE accept_awaitable_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC accept_awaitable_iter_self

;; accept_awaitable_iternext(self) -> fat value
DEF_FUNC_BARE accept_awaitable_iternext
    cmp dword [rdi + AcceptAwaitable.yielded], 0
    jne .aai_accept

    ; First call: yield TAG_IO_WAIT for POLLIN (accept readiness)
    mov dword [rdi + AcceptAwaitable.yielded], 1
    mov eax, [rdi + AcceptAwaitable.listen_fd]
    mov rdx, POLLIN
    shl rdx, 32
    or rax, rdx
    or rax, [rel v_iowait_lo]   ; IO_WAIT sentinel Value
    ret

.aai_accept:
    ; Second call: accept and create (reader, writer) tuple
    push rbx
    push r12

    mov ebx, [rdi + AcceptAwaitable.listen_fd]

    ; accept4(listen_fd, NULL, NULL, 0)
    mov edi, ebx
    xor esi, esi               ; addr = NULL
    xor edx, edx               ; addrlen = NULL
    xor ecx, ecx               ; flags = 0
    call sys_accept4
    mov r12d, eax              ; r12d = client fd

    ; Close listen socket
    mov edi, ebx
    call sys_close

    test r12d, r12d
    js .aai_error

    ; Set client fd to non-blocking
    mov edi, r12d
    mov esi, F_SETFL
    mov edx, O_NONBLOCK
    call sys_fcntl

    ; Create reader + writer for client fd
    mov edi, r12d
    call stream_reader_new
    mov rbx, rax               ; reader

    mov edi, r12d
    call stream_writer_new
    push rax                   ; save writer

    ; Create 2-tuple
    mov edi, 2
    call tuple_new
    mov r12, rax               ; tuple

    mov r9, [rax + PyTupleObject.ob_item]
    mov [r9], rbx                  ; ob_item[0] = reader
    pop rcx                        ; writer
    mov [r9 + 8], rcx              ; ob_item[1] = writer

    mov rax, r12
    pop r12
    pop rbx
    ret

.aai_error:
    RAISE exc_OSError_type, "start_server() accept failed"
END_FUNC accept_awaitable_iternext

DEF_FUNC_BARE accept_awaitable_dealloc
    ; Close listen fd if still open
    mov edi, [rdi + AcceptAwaitable.listen_fd]
    cmp edi, -1
    je .aad_free
    push rdi
    call sys_close
    pop rdi
.aad_free:
    jmp ap_free
END_FUNC accept_awaitable_dealloc

;; ============================================================================
;; asyncio.open_connection(host, port) — create TCP connection
;; Returns a ConnectAwaitable
;; ============================================================================
OC_FRAME equ 32             ; + 3 pushes = 56, not 16-aligned
DEF_FUNC asyncio_open_connection_func, OC_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 2
    jne .oc_error

    ; args[0] = host (string), args[1] = port (int)
    mov rax, [rdi + 8]       ; port payload
    V_UNPACK rax, rdx       ; args[1]
    cmp edx, TAG_SMALLINT
    jne .oc_port_error
    mov r12d, eax              ; r12d = port number

    ; Create socket
    mov edi, AF_INET           ; domain
    mov esi, SOCK_STREAM       ; type
    xor edx, edx               ; protocol
    call sys_socket
    test eax, eax
    js .oc_socket_error
    mov ebx, eax               ; ebx = socket fd

    ; Build sockaddr_in on stack
    ; struct sockaddr_in { sa_family_t sin_family; uint16_t sin_port; uint32_t sin_addr; char sin_zero[8]; }
    sub rsp, 16                ; 16 bytes for sockaddr_in
    mov word [rsp], AF_INET    ; sin_family
    ; Convert port to network byte order (big-endian)
    mov eax, r12d
    xchg al, ah                ; swap bytes for 16-bit
    mov [rsp + 2], ax          ; sin_port (network byte order)
    ; sin_addr = INADDR_ANY = 0 for now (connect to localhost = 127.0.0.1)
    mov dword [rsp + 4], 0x0100007f  ; 127.0.0.1 in network byte order
    mov qword [rsp + 8], 0    ; sin_zero

    ; Set non-blocking
    mov edi, ebx
    mov esi, F_SETFL
    mov edx, O_NONBLOCK
    call sys_fcntl

    ; Connect (non-blocking — will return EINPROGRESS)
    mov edi, ebx               ; fd
    mov rsi, rsp               ; addr
    mov edx, 16                ; addrlen
    call sys_connect
    ; rax = 0 (connected) or -EINPROGRESS (-115)
    add rsp, 16

    ; Create ConnectAwaitable regardless of connect result
    mov edi, ConnectAwaitable_size
    call ap_malloc
    mov qword [rax + ConnectAwaitable.ob_refcnt], 1
    lea rcx, [rel connect_awaitable_type]
    mov [rax + ConnectAwaitable.ob_type], rcx
    mov [rax + ConnectAwaitable.fd], ebx
    mov dword [rax + ConnectAwaitable.yielded], 0

    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.oc_error:
    RAISE exc_TypeError_type, "open_connection() requires 2 arguments (host, port)"

.oc_port_error:
    RAISE exc_TypeError_type, "open_connection() port must be an integer"

.oc_socket_error:
    RAISE exc_OSError_type, "open_connection() socket creation failed"
END_FUNC asyncio_open_connection_func

;; ============================================================================
;; asyncio.start_server(callback, host, port) — bind + listen
;; For simplicity: creates a listening socket, returns (reader, writer) for
;; first accepted connection. A full implementation would accept in a loop.
;; Returns a ConnectAwaitable that resolves to (reader, writer) on accept.
;; ============================================================================
SS_FRAME equ 48             ; + 3 pushes = 72, not 16-aligned
DEF_FUNC asyncio_start_server_func, SS_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 3
    jne .ss_error

    ; args[0] = callback, args[1] = host, args[2] = port
    mov rax, [rdi + 16]       ; port payload
    V_UNPACK rax, rdx       ; args[2]
    cmp edx, TAG_SMALLINT
    jne .ss_port_error
    mov r12d, eax              ; r12d = port

    ; Create socket
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    call sys_socket
    test eax, eax
    js .ss_socket_error
    mov ebx, eax               ; ebx = listen fd

    ; Set SO_REUSEADDR
    sub rsp, 4
    mov dword [rsp], 1         ; optval = 1
    mov edi, ebx
    mov esi, SOL_SOCKET
    mov edx, SO_REUSEADDR
    mov rcx, rsp               ; optval
    mov r8d, 4                 ; optlen
    call sys_setsockopt
    add rsp, 4

    ; Build sockaddr_in on stack
    sub rsp, 16
    mov word [rsp], AF_INET
    mov eax, r12d
    xchg al, ah
    mov [rsp + 2], ax          ; port in network byte order
    mov dword [rsp + 4], 0    ; INADDR_ANY
    mov qword [rsp + 8], 0

    ; Bind
    mov edi, ebx
    mov rsi, rsp
    mov edx, 16
    call sys_bind
    test eax, eax
    jnz .ss_bind_cleanup

    ; Listen
    mov edi, ebx
    mov esi, 5                 ; backlog
    call sys_listen
    test eax, eax
    jnz .ss_bind_cleanup

    add rsp, 16

    ; Set non-blocking for accept
    mov edi, ebx
    mov esi, F_SETFL
    mov edx, O_NONBLOCK
    call sys_fcntl

    ; Create AcceptAwaitable that yields TAG_IO_WAIT(POLLIN) then accepts
    mov edi, AcceptAwaitable_size
    call ap_malloc
    mov qword [rax + AcceptAwaitable.ob_refcnt], 1
    lea rcx, [rel accept_awaitable_type]
    mov [rax + AcceptAwaitable.ob_type], rcx
    mov [rax + AcceptAwaitable.listen_fd], ebx
    mov dword [rax + AcceptAwaitable.yielded], 0

    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.ss_error:
    RAISE exc_TypeError_type, "start_server() requires 3 arguments (callback, host, port)"

.ss_port_error:
    RAISE exc_TypeError_type, "start_server() port must be an integer"

.ss_socket_error:
    RAISE exc_OSError_type, "start_server() socket creation failed"

.ss_bind_cleanup:
    add rsp, 16
    mov edi, ebx
    call sys_close
    RAISE exc_OSError_type, "start_server() bind/listen failed"

END_FUNC asyncio_start_server_func

;; ============================================================================
;; ap_strcmp — compare C string with Python string object
;; rdi = C string, rsi = PyStrObject*
;; Returns 0 if equal, nonzero otherwise
;; ============================================================================
DEF_FUNC_LOCAL _stream_strcmp
    push rbx
    push r12

    mov rbx, rdi               ; C string
    mov r12, rsi               ; Python string object

    ; Get string data and length from Python str object
    ; PyStrObject: ob_size at +16, data at +32
    mov rdi, [r12 + PyStrObject.ob_size]
    lea rsi, [r12 + 32]       ; data (PyStrObject.data)

    ; Compare byte by byte
    xor ecx, ecx
.sc_loop:
    mov al, [rbx + rcx]       ; C string byte
    cmp rcx, rdi               ; past python string end?
    jge .sc_check_null
    cmp al, [rsi + rcx]       ; python string byte
    jne .sc_ne
    test al, al                ; end of C string?
    jz .sc_eq
    inc rcx
    jmp .sc_loop

.sc_check_null:
    ; At end of python string — C string must also end here
    test al, al
    jz .sc_eq
.sc_ne:
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
.sc_eq:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC _stream_strcmp

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata
srn_read:       db "read", 0
srn_readline:   db "readline", 0
swn_write:      db "write", 0
swn_drain:      db "drain", 0

stream_reader_name: db "StreamReader", 0
stream_writer_name: db "StreamWriter", 0
read_awaitable_name: db "ReadAwaitable", 0
drain_awaitable_name: db "DrainAwaitable", 0
connect_awaitable_name: db "ConnectAwaitable", 0
accept_awaitable_name: db "AcceptAwaitable", 0

section .data
align 8

global stream_reader_type
stream_reader_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq stream_reader_name       ; tp_name
    dq AsyncStreamReader_size   ; tp_basicsize
    dq stream_reader_dealloc    ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq stream_reader_getattr    ; tp_getattr
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

global stream_writer_type
stream_writer_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq stream_writer_name       ; tp_name
    dq AsyncStreamWriter_size   ; tp_basicsize
    dq stream_writer_dealloc    ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq stream_writer_getattr    ; tp_getattr
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

read_awaitable_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq read_awaitable_name      ; tp_name
    dq ReadAwaitable_size       ; tp_basicsize
    dq read_awaitable_dealloc   ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq read_awaitable_iter_self ; tp_iter
    dq read_awaitable_iternext  ; tp_iternext
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

drain_awaitable_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq drain_awaitable_name     ; tp_name
    dq DrainAwaitable_size      ; tp_basicsize
    dq drain_awaitable_dealloc  ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq drain_awaitable_iter_self ; tp_iter
    dq drain_awaitable_iternext ; tp_iternext
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

connect_awaitable_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq connect_awaitable_name   ; tp_name
    dq ConnectAwaitable_size    ; tp_basicsize
    dq connect_awaitable_dealloc ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq connect_awaitable_iter_self ; tp_iter
    dq connect_awaitable_iternext ; tp_iternext
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

accept_awaitable_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq accept_awaitable_name    ; tp_name
    dq AcceptAwaitable_size     ; tp_basicsize
    dq accept_awaitable_dealloc ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq accept_awaitable_iter_self ; tp_iter
    dq accept_awaitable_iternext ; tp_iternext
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
