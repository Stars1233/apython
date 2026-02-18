; eventloop.inc - Event loop structs and constants
;
; IOBackend vtable for abstracting io_uring vs poll()

%ifndef EVENTLOOP_INC
%define EVENTLOOP_INC

; I/O backend vtable — each backend fills in function pointers
struc IOBackend
    .init:           resq 1  ; () -> 0 ok, -1 fail
    .teardown:       resq 1  ; () -> void
    .submit_timeout: resq 1  ; (task*, delay_ns) -> void
    .submit_poll_fd: resq 1  ; (task*, fd, events) -> void
    .cancel_io:      resq 1  ; (task*) -> void
    .wait_and_drain: resq 1  ; () -> drain completions, enqueue ready tasks
endstruc

; EventLoop singleton struct
struc EventLoop
    .running:    resd 1  ; +0: 1 = loop is running
    .pad1:       resd 1  ; +4
    .ready_head: resq 1  ; +8: head of ready task queue
    .ready_tail: resq 1  ; +16: tail of ready task queue
    .backend:    resq 1  ; +24: IOBackend* (active backend)
    .root_task:  resq 1  ; +32: root task for asyncio.run
endstruc

; Timer entry for poll backend min-heap
struc TimerEntry
    .deadline_ns: resq 1  ; +0: absolute deadline in nanoseconds
    .task:        resq 1  ; +8: AsyncTask*
endstruc

; Poll backend state
POLL_MAX_FDS    equ 1024
POLL_MAX_TIMERS equ 256

; pollfd structure (matches Linux struct pollfd)
struc PollFd
    .fd:      resd 1  ; +0: int fd
    .events:  resw 1  ; +4: short events (POLLIN=1, POLLOUT=4)
    .revents: resw 1  ; +6: short revents
endstruc

; poll event constants
POLLIN   equ 1
POLLOUT  equ 4
POLLERR  equ 8
POLLHUP  equ 16

; io_uring constants
IORING_OP_NOP         equ 0
IORING_OP_POLL_ADD    equ 6
IORING_OP_TIMEOUT     equ 11
IORING_OP_ASYNC_CANCEL equ 14
IORING_ENTER_GETEVENTS equ 1

; io_uring SQE structure (64 bytes)
struc IoUringSqe
    .opcode:    resb 1  ; +0
    .flags:     resb 1  ; +1
    .ioprio:    resw 1  ; +2
    .fd:        resd 1  ; +4
    .off:       resq 1  ; +8: union { off, addr2 }
    .addr:      resq 1  ; +16: union { addr, splice_off_in }
    .len:       resd 1  ; +24
    .rw_flags:  resd 1  ; +28: union { rw_flags, poll_events, ... }
    .user_data: resq 1  ; +32
    .buf_index: resw 1  ; +40
    .personality: resw 1 ; +42
    .splice_fd_in: resd 1 ; +44
    .pad2:      resq 2  ; +48
endstruc

; io_uring CQE structure (16 bytes)
struc IoUringCqe
    .user_data: resq 1  ; +0
    .res:       resd 1  ; +8
    .flags:     resd 1  ; +12
endstruc

; io_uring_params structure for setup
struc IoUringParams
    .sq_entries:     resd 1  ; +0
    .cq_entries:     resd 1  ; +4
    .flags:          resd 1  ; +8
    .sq_thread_cpu:  resd 1  ; +12
    .sq_thread_idle: resd 1  ; +16
    .features:       resd 1  ; +20
    .wq_fd:          resd 1  ; +24
    .resv:           resd 3  ; +28
    .sq_off:         resb 40 ; +40: struct io_sqring_offsets
    .cq_off:         resb 40 ; +80: struct io_cqring_offsets
endstruc

%endif ; EVENTLOOP_INC
