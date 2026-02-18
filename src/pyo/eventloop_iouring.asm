; eventloop_iouring.asm - io_uring I/O backend for event loop
;
; Linux 5.1+ io_uring async I/O backend.
; Falls back to poll if io_uring_setup returns ENOSYS.

%include "macros.inc"
%include "object.inc"
%include "eventloop.inc"

extern sys_io_uring_setup
extern sys_io_uring_enter
extern sys_mmap
extern sys_munmap
extern sys_close
extern ready_enqueue
extern ap_malloc
extern ap_free
extern none_singleton

; mmap constants
PROT_READ      equ 1
PROT_WRITE     equ 2
MAP_SHARED     equ 1
MAP_POPULATE   equ 0x8000

; io_sqring_offsets fields (within IoUringParams.sq_off, 40 bytes)
; Offsets from IoUringParams + 40
SQOFF_HEAD         equ 0
SQOFF_TAIL         equ 4
SQOFF_RING_MASK    equ 8
SQOFF_RING_ENTRIES equ 12
SQOFF_FLAGS        equ 16
SQOFF_DROPPED      equ 20
SQOFF_ARRAY        equ 24
SQOFF_RESV1        equ 28
SQOFF_RESV2        equ 32

; io_cqring_offsets fields (within IoUringParams + 80, 40 bytes)
CQOFF_HEAD         equ 0
CQOFF_TAIL         equ 4
CQOFF_RING_MASK    equ 8
CQOFF_RING_ENTRIES equ 12
CQOFF_OVERFLOW     equ 16
CQOFF_CQES         equ 20

URING_RING_ENTRIES equ 256

;; ============================================================================
;; State
;; ============================================================================
section .bss
align 8
; io_uring file descriptor
uring_fd:        resd 1
uring_pad:       resd 1

; Ring pointers
uring_sq_ring:   resq 1  ; mmap'd SQ ring base
uring_cq_ring:   resq 1  ; mmap'd CQ ring base
uring_sqes:      resq 1  ; mmap'd SQE array base
uring_sq_size:   resq 1  ; SQ ring mmap size
uring_cq_size:   resq 1  ; CQ ring mmap size
uring_sqe_size:  resq 1  ; SQE array mmap size

; Ring parameters (cached from mmap'd ring)
uring_sq_mask:   resd 1
uring_cq_mask:   resd 1
uring_sq_head:   resq 1  ; ptr to sq head in shared memory
uring_sq_tail:   resq 1  ; ptr to sq tail in shared memory
uring_sq_array:  resq 1  ; ptr to sq index array
uring_cq_head:   resq 1  ; ptr to cq head in shared memory
uring_cq_tail:   resq 1  ; ptr to cq tail in shared memory
uring_cqes_base: resq 1  ; ptr to CQE array

; Setup params (on stack in init, but we need the offsets)
uring_params_buf: resb IoUringParams_size

section .text

;; ============================================================================
;; uring_init() -> 0 ok, -1 fail
;; Try to set up io_uring. Returns -1 if unavailable.
;; ============================================================================
DEF_FUNC uring_init
    push rbx
    push r12

    ; Zero params
    lea rdi, [rel uring_params_buf]
    xor eax, eax
    mov ecx, IoUringParams_size
.zi_loop:
    mov byte [rdi + rcx - 1], 0
    dec ecx
    jnz .zi_loop

    ; io_uring_setup(entries=256, params)
    mov edi, URING_RING_ENTRIES
    lea rsi, [rel uring_params_buf]
    call sys_io_uring_setup
    ; rax = fd or -errno

    test eax, eax
    js .ui_fail

    mov [rel uring_fd], eax
    mov ebx, eax              ; save fd

    ; Calculate mmap sizes from params
    lea r12, [rel uring_params_buf]

    ; SQ ring size = sq_off.array + sq_entries * 4
    mov eax, [r12 + 40 + SQOFF_ARRAY]         ; sq_off.array offset
    mov ecx, [r12 + IoUringParams.sq_entries]
    shl ecx, 2                                  ; * 4
    add eax, ecx
    mov [rel uring_sq_size], rax

    ; mmap SQ ring
    xor edi, edi               ; addr = NULL
    mov rsi, rax               ; length
    mov edx, PROT_READ | PROT_WRITE
    mov ecx, MAP_SHARED | MAP_POPULATE
    mov r8d, ebx               ; fd
    xor r9d, r9d               ; offset = IORING_OFF_SQ_RING = 0
    call sys_mmap
    cmp rax, -1
    je .ui_close_fail
    mov [rel uring_sq_ring], rax

    ; Cache SQ ring pointers
    mov rcx, rax               ; base
    mov edx, [r12 + 40 + SQOFF_HEAD]
    lea rdi, [rcx + rdx]
    mov [rel uring_sq_head], rdi
    mov edx, [r12 + 40 + SQOFF_TAIL]
    lea rdi, [rcx + rdx]
    mov [rel uring_sq_tail], rdi
    mov edx, [r12 + 40 + SQOFF_RING_MASK]
    mov eax, [rcx + rdx]
    mov [rel uring_sq_mask], eax
    mov edx, [r12 + 40 + SQOFF_ARRAY]
    lea rdi, [rcx + rdx]
    mov [rel uring_sq_array], rdi

    ; SQE array size = sq_entries * 64
    mov ecx, [r12 + IoUringParams.sq_entries]
    shl ecx, 6                 ; * 64
    mov [rel uring_sqe_size], rcx

    ; mmap SQEs: offset = 0x10000000ULL (IORING_OFF_SQES)
    xor edi, edi
    mov rsi, rcx
    mov edx, PROT_READ | PROT_WRITE
    mov ecx, MAP_SHARED | MAP_POPULATE
    mov r8d, ebx
    mov r9, 0x10000000         ; IORING_OFF_SQES
    call sys_mmap
    cmp rax, -1
    je .ui_unmap_sq
    mov [rel uring_sqes], rax

    ; CQ ring size = cq_off.cqes + cq_entries * 16
    mov eax, [r12 + 80 + CQOFF_CQES]
    mov ecx, [r12 + IoUringParams.cq_entries]
    shl ecx, 4                 ; * 16 (CQE size)
    add eax, ecx
    mov [rel uring_cq_size], rax

    ; mmap CQ ring: offset = 0x8000000ULL (IORING_OFF_CQ_RING)
    xor edi, edi
    mov rsi, rax
    mov edx, PROT_READ | PROT_WRITE
    mov ecx, MAP_SHARED | MAP_POPULATE
    mov r8d, ebx
    mov r9, 0x8000000          ; IORING_OFF_CQ_RING
    call sys_mmap
    cmp rax, -1
    je .ui_unmap_sqes
    mov [rel uring_cq_ring], rax

    ; Cache CQ ring pointers
    mov rcx, rax
    mov edx, [r12 + 80 + CQOFF_HEAD]
    lea rdi, [rcx + rdx]
    mov [rel uring_cq_head], rdi
    mov edx, [r12 + 80 + CQOFF_TAIL]
    lea rdi, [rcx + rdx]
    mov [rel uring_cq_tail], rdi
    mov edx, [r12 + 80 + CQOFF_RING_MASK]
    mov eax, [rcx + rdx]
    mov [rel uring_cq_mask], eax
    mov edx, [r12 + 80 + CQOFF_CQES]
    lea rdi, [rcx + rdx]
    mov [rel uring_cqes_base], rdi

    ; Success
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.ui_unmap_sqes:
    mov rdi, [rel uring_sqes]
    mov rsi, [rel uring_sqe_size]
    call sys_munmap
.ui_unmap_sq:
    mov rdi, [rel uring_sq_ring]
    mov rsi, [rel uring_sq_size]
    call sys_munmap
.ui_close_fail:
    mov edi, ebx
    call sys_close
.ui_fail:
    mov eax, -1
    pop r12
    pop rbx
    leave
    ret
END_FUNC uring_init

;; ============================================================================
;; uring_teardown()
;; ============================================================================
DEF_FUNC uring_teardown
    ; Unmap rings
    mov rdi, [rel uring_cq_ring]
    mov rsi, [rel uring_cq_size]
    call sys_munmap

    mov rdi, [rel uring_sqes]
    mov rsi, [rel uring_sqe_size]
    call sys_munmap

    mov rdi, [rel uring_sq_ring]
    mov rsi, [rel uring_sq_size]
    call sys_munmap

    ; Close uring fd
    mov edi, [rel uring_fd]
    call sys_close

    leave
    ret
END_FUNC uring_teardown

;; ============================================================================
;; uring_get_sqe() -> IoUringSqe* or NULL
;; Get next available SQE slot.
;; ============================================================================
DEF_FUNC_LOCAL uring_get_sqe
    mov rax, [rel uring_sq_tail]
    mov ecx, [rax]             ; tail value
    mov rdx, [rel uring_sq_head]
    mov edx, [rdx]             ; head value

    ; Check if full: tail - head >= entries
    mov r8d, ecx
    sub r8d, edx
    cmp r8d, [rel uring_sq_mask]
    ja .ugs_full               ; ring is full

    ; SQE index = tail & mask
    mov eax, ecx
    and eax, [rel uring_sq_mask]

    ; Set sq_array[index] = index (identity mapping)
    mov rdx, [rel uring_sq_array]
    mov [rdx + rax*4], eax

    ; Return SQE pointer
    shl eax, 6                ; * 64
    add rax, [rel uring_sqes]

    ; Zero the SQE
    push rax
    xor ecx, ecx
    mov rdi, rax
.ugs_zero:
    cmp ecx, 64
    jge .ugs_zero_done
    mov qword [rdi + rcx], 0
    add ecx, 8
    jmp .ugs_zero
.ugs_zero_done:
    pop rax

    ; Advance tail
    mov rdx, [rel uring_sq_tail]
    inc dword [rdx]

    leave
    ret

.ugs_full:
    xor eax, eax
    leave
    ret
END_FUNC uring_get_sqe

;; ============================================================================
;; uring_submit_timeout(AsyncTask *task, uint64_t delay_ns)
;; ============================================================================
section .bss
align 8
uring_ts_buf: resq 2          ; timespec for timeout SQE

section .text
DEF_FUNC uring_submit_timeout
    push rbx
    push r12

    mov rbx, rdi               ; task
    mov r12, rsi               ; delay_ns

    ; Convert delay_ns to timespec
    mov rax, r12
    mov rcx, 1000000000
    xor edx, edx
    div rcx
    mov [rel uring_ts_buf], rax      ; tv_sec
    mov [rel uring_ts_buf + 8], rdx  ; tv_nsec

    call uring_get_sqe
    test rax, rax
    jz .ust_done

    ; Fill SQE for IORING_OP_TIMEOUT
    mov byte [rax + IoUringSqe.opcode], IORING_OP_TIMEOUT
    mov dword [rax + IoUringSqe.fd], -1
    lea rcx, [rel uring_ts_buf]
    mov [rax + IoUringSqe.addr], rcx
    mov dword [rax + IoUringSqe.len], 1    ; count = 1
    mov [rax + IoUringSqe.user_data], rbx  ; task as user_data

.ust_done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC uring_submit_timeout

;; ============================================================================
;; uring_submit_poll(AsyncTask *task, int fd, int events)
;; ============================================================================
DEF_FUNC uring_submit_poll
    push rbx

    mov rbx, rdi               ; task
    mov r8d, esi               ; fd
    mov r9d, edx               ; events

    call uring_get_sqe
    test rax, rax
    jz .usp_done

    mov byte [rax + IoUringSqe.opcode], IORING_OP_POLL_ADD
    mov [rax + IoUringSqe.fd], r8d
    mov [rax + IoUringSqe.rw_flags], r9d   ; poll_events
    mov [rax + IoUringSqe.user_data], rbx

.usp_done:
    pop rbx
    leave
    ret
END_FUNC uring_submit_poll

;; ============================================================================
;; uring_cancel_io(AsyncTask *task)
;; ============================================================================
DEF_FUNC uring_cancel_io
    push rbx
    mov rbx, rdi

    call uring_get_sqe
    test rax, rax
    jz .uc_done

    mov byte [rax + IoUringSqe.opcode], IORING_OP_ASYNC_CANCEL
    mov [rax + IoUringSqe.addr], rbx       ; user_data of target
    mov qword [rax + IoUringSqe.user_data], 0

.uc_done:
    pop rbx
    leave
    ret
END_FUNC uring_cancel_io

;; ============================================================================
;; uring_wait_and_drain()
;; Submit pending SQEs and drain CQ ring.
;; ============================================================================
DEF_FUNC uring_wait_and_drain
    push rbx
    push r12

    ; io_uring_enter(fd, to_submit=0, min_complete=1, flags=GETEVENTS)
    mov edi, [rel uring_fd]
    xor esi, esi               ; to_submit (SQEs already queued via tail advance)
    mov edx, 1                 ; min_complete
    mov ecx, IORING_ENTER_GETEVENTS
    xor r8d, r8d               ; sig = NULL
    xor r9d, r9d               ; sigsz = 0

    ; Count pending submissions: tail - head
    mov rax, [rel uring_sq_tail]
    mov eax, [rax]
    mov rcx, [rel uring_sq_head]
    sub eax, [rcx]
    mov esi, eax               ; to_submit = pending count

    mov edi, [rel uring_fd]
    mov edx, 1
    mov ecx, IORING_ENTER_GETEVENTS
    xor r8d, r8d
    xor r9d, r9d
    call sys_io_uring_enter

    ; Drain CQ ring
    mov rax, [rel uring_cq_head]
    mov ebx, [rax]             ; head
    mov rax, [rel uring_cq_tail]
    mov r12d, [rax]            ; tail

.uwd_drain:
    cmp ebx, r12d
    je .uwd_done

    ; Get CQE at head & mask
    mov eax, ebx
    and eax, [rel uring_cq_mask]
    shl eax, 4                 ; * 16 (CQE size)
    add rax, [rel uring_cqes_base]

    ; Extract task from user_data
    mov rdi, [rax + IoUringCqe.user_data]
    test rdi, rdi
    jz .uwd_skip               ; cancel completion, ignore

    ; Set send_value = None, enqueue
    lea rcx, [rel none_singleton]
    mov [rdi + AsyncTask.send_value], rcx
    mov qword [rdi + AsyncTask.send_tag], TAG_PTR

    push rbx
    call ready_enqueue
    pop rbx

.uwd_skip:
    inc ebx
    jmp .uwd_drain

.uwd_done:
    ; Update CQ head
    mov rax, [rel uring_cq_head]
    mov [rax], ebx

    pop r12
    pop rbx
    leave
    ret
END_FUNC uring_wait_and_drain

;; ============================================================================
;; Data: uring_backend vtable
;; ============================================================================
section .data
align 8
global uring_backend
uring_backend:
    dq uring_init
    dq uring_teardown
    dq uring_submit_timeout
    dq uring_submit_poll
    dq uring_cancel_io
    dq uring_wait_and_drain
