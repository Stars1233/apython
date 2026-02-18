; frame.asm - Frame allocation and deallocation
; Manages execution frames for the bytecode interpreter
;
; Frame pooling: 4 size classes (256, 512, 1024, 2048 bytes).
; Each class has a freelist (singly-linked, max POOL_MAX_FREE entries).
; frame_pool_get(size) checks freelist first, falls back to ap_malloc.
; frame_pool_put(frame, size) pushes to freelist or ap_free if full.

%include "macros.inc"
%include "object.inc"
%include "frame.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_dealloc

; Pool constants
POOL_CLASS_0  equ 256
POOL_CLASS_1  equ 512
POOL_CLASS_2  equ 1024
POOL_CLASS_3  equ 2048
POOL_MAX_FREE equ 16      ; max frames per freelist

;; ============================================================================
;; frame_pool_get(size) -> ptr
;; Allocate from pool or ap_malloc. rdi = requested size.
;; Returns rax = pointer (size rounded up to next pool class).
;; ============================================================================
DEF_FUNC frame_pool_get
    ; Round up to pool class
    cmp rdi, POOL_CLASS_0
    jbe .fp_class0
    cmp rdi, POOL_CLASS_1
    jbe .fp_class1
    cmp rdi, POOL_CLASS_2
    jbe .fp_class2
    cmp rdi, POOL_CLASS_3
    jbe .fp_class3
    ; Too large for pool — ap_malloc
    jmp .fp_malloc

.fp_class0:
    lea rcx, [rel pool_free_0]
    mov edi, POOL_CLASS_0
    jmp .fp_try_pool
.fp_class1:
    lea rcx, [rel pool_free_1]
    mov edi, POOL_CLASS_1
    jmp .fp_try_pool
.fp_class2:
    lea rcx, [rel pool_free_2]
    mov edi, POOL_CLASS_2
    jmp .fp_try_pool
.fp_class3:
    lea rcx, [rel pool_free_3]
    mov edi, POOL_CLASS_3

.fp_try_pool:
    ; rcx = &pool_free_N, edi = class size
    mov rax, [rcx]             ; head of freelist
    test rax, rax
    jz .fp_malloc              ; empty freelist
    ; Pop from freelist: head = head->next
    mov rdx, [rax]             ; next pointer (stored at offset 0)
    mov [rcx], rdx
    ; Decrement count
    lea rdx, [rcx + 8]        ; &pool_count_N (count is 8 bytes after freelist head)
    dec dword [rdx]
    ; rax = recycled frame
    leave
    ret

.fp_malloc:
    call ap_malloc
    leave
    ret
END_FUNC frame_pool_get

;; ============================================================================
;; frame_pool_put(ptr, size)
;; Return to pool or ap_free. rdi = ptr, rsi = size.
;; ============================================================================
DEF_FUNC frame_pool_put
    ; Determine pool class
    cmp rsi, POOL_CLASS_0
    jbe .fpp_class0
    cmp rsi, POOL_CLASS_1
    jbe .fpp_class1
    cmp rsi, POOL_CLASS_2
    jbe .fpp_class2
    cmp rsi, POOL_CLASS_3
    jbe .fpp_class3
    ; Too large — ap_free
    jmp ap_free                ; tail call

.fpp_class0:
    lea rcx, [rel pool_free_0]
    jmp .fpp_try_push
.fpp_class1:
    lea rcx, [rel pool_free_1]
    jmp .fpp_try_push
.fpp_class2:
    lea rcx, [rel pool_free_2]
    jmp .fpp_try_push
.fpp_class3:
    lea rcx, [rel pool_free_3]

.fpp_try_push:
    ; rcx = &pool_free_N
    lea rdx, [rcx + 8]        ; &pool_count_N
    cmp dword [rdx], POOL_MAX_FREE
    jge .fpp_full
    ; Push to freelist: frame->next = head; head = frame
    mov rax, [rcx]             ; old head
    mov [rdi], rax             ; frame->next = old head
    mov [rcx], rdi             ; head = frame
    inc dword [rdx]            ; count++
    leave
    ret

.fpp_full:
    ; Pool is full — ap_free
    jmp ap_free                ; tail call (rdi already set)
END_FUNC frame_pool_put

; frame_new(PyCodeObject *code, PyObject *globals, PyObject *builtins, PyObject *locals) -> PyFrame*
; Allocates and initializes a new execution frame.
; rdi = code, rsi = globals, rdx = builtins, rcx = locals
DEF_FUNC frame_new
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi            ; rbx = code
    mov r12, rsi            ; r12 = globals
    mov r13, rdx            ; r13 = builtins
    mov r14, rcx            ; r14 = locals

    ; Calculate frame size: FRAME_HEADER_SIZE + (nlocalsplus + stacksize) * 16
    mov eax, [rbx + PyCodeObject.co_nlocalsplus]
    add eax, [rbx + PyCodeObject.co_stacksize]
    mov r15d, eax           ; r15d = nlocalsplus + stacksize (total slots)
    shl rax, 4              ; * 16 bytes per slot (128-bit fat values)
    add rax, FRAME_HEADER_SIZE
    mov rdi, rax
    call frame_pool_get
    ; rax = frame pointer

    ; Fill frame header fields
    mov qword [rax + PyFrame.prev_frame], 0
    mov [rax + PyFrame.code], rbx
    mov [rax + PyFrame.globals], r12
    mov [rax + PyFrame.builtins], r13
    mov [rax + PyFrame.locals], r14
    mov qword [rax + PyFrame.instr_ptr], 0
    mov qword [rax + PyFrame.stack_ptr], 0
    mov dword [rax + PyFrame.return_offset], 0

    ; Set nlocalsplus and func_obj
    mov ecx, [rbx + PyCodeObject.co_nlocalsplus]
    mov [rax + PyFrame.nlocalsplus], ecx
    mov qword [rax + PyFrame.func_obj], 0

    ; stack_base = &localsplus[nlocalsplus] (16 bytes/slot)
    mov edx, ecx            ; edx = nlocalsplus
    lea rdi, [rax + PyFrame.localsplus]
    shl rdx, 4              ; nlocalsplus * 16
    lea rsi, [rdi + rdx]    ; rsi = &localsplus[nlocalsplus]
    mov [rax + PyFrame.stack_base], rsi

    ; Zero all localsplus entries (set to NULL, 16 bytes/slot = 2 qwords each)
    ; ecx still holds nlocalsplus
    test ecx, ecx
    jz .done
    push rax                ; save frame pointer
    mov rdi, rax            ; rdi = frame (for localsplus base calc)
    lea rdi, [rdi + PyFrame.localsplus]
    xor eax, eax
    mov ecx, ecx            ; zero-extend ecx (already done but be explicit)
    shl ecx, 1              ; 2 qwords per 16-byte slot
    rep stosq               ; store ecx qwords of 0 at [rdi]
    pop rax                 ; restore frame pointer

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC frame_new

; frame_free(PyFrame *frame)
; XDECREF all non-NULL localsplus entries, then free the frame.
; rdi = frame
DEF_FUNC frame_free
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; rbx = frame
    mov r12d, [rbx + PyFrame.nlocalsplus]  ; r12d = nlocalsplus
    xor r13d, r13d          ; r13d = loop index

    ; Iterate through localsplus entries
.loop:
    cmp r13d, r12d
    jge .free_frame

    ; Load localsplus[r13] (16 bytes/slot — payload + tag)
    mov rax, r13
    shl rax, 4              ; r13 * 16
    mov rdi, [rbx + PyFrame.localsplus + rax]
    mov rsi, [rbx + PyFrame.localsplus + rax + 8]  ; tag
    ; XDECREF_VAL: tag-aware, handles TAG_NULL, TAG_SMALLINT etc.
    XDECREF_VAL rdi, rsi

.next:
    inc r13d
    jmp .loop

.free_frame:
    ; Calculate frame size for pool return
    mov rdi, [rbx + PyFrame.code]
    mov eax, [rdi + PyCodeObject.co_nlocalsplus]
    add eax, [rdi + PyCodeObject.co_stacksize]
    shl rax, 4
    add rax, FRAME_HEADER_SIZE

    mov rdi, rbx            ; ptr
    mov rsi, rax            ; size
    call frame_pool_put

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC frame_free

;; ============================================================================
;; Pool data
;; ============================================================================
section .data

; Freelists: each is (head_ptr, count)
align 8
pool_free_0:  dq 0        ; 256B class freelist head
pool_count_0: dd 0         ; count
              dd 0         ; padding

pool_free_1:  dq 0         ; 512B class freelist head
pool_count_1: dd 0
              dd 0

pool_free_2:  dq 0         ; 1024B class freelist head
pool_count_2: dd 0
              dd 0

pool_free_3:  dq 0         ; 2048B class freelist head
pool_count_3: dd 0
              dd 0

section .text
