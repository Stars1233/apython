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
    leave
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
    leave
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

    ; Calculate frame size: FRAME_HEADER_SIZE + (nlocalsplus + stacksize) * 8
    mov eax, [rbx + PyCodeObject.co_nlocalsplus]
    add eax, [rbx + PyCodeObject.co_stacksize]
    mov r15d, eax           ; r15d = nlocalsplus + stacksize (total slots)
    shl rax, 3              ; * 8 bytes per slot (payload only)
    add rax, FRAME_HEADER_SIZE
    mov rdi, rax
    call frame_pool_get
    ; rax = frame pointer
    mov r11, rax

    ; Fill frame header fields
    mov qword [r11 + PyFrame.prev_frame], 0
    mov [r11 + PyFrame.code], rbx
    mov [r11 + PyFrame.globals], r12
    mov [r11 + PyFrame.builtins], r13
    mov [r11 + PyFrame.locals], r14
    mov qword [r11 + PyFrame.instr_ptr], 0
    mov qword [r11 + PyFrame.stack_ptr], 0
    mov dword [r11 + PyFrame.return_offset], 0

    ; Set nlocalsplus and func_obj
    mov ecx, [rbx + PyCodeObject.co_nlocalsplus]
    mov [r11 + PyFrame.nlocalsplus], ecx
    mov qword [r11 + PyFrame.func_obj], 0

    ; stack_base = &localsplus[nlocalsplus] (8 bytes/slot)
    mov ecx, [r11 + PyFrame.nlocalsplus]
    mov edx, ecx            ; edx = nlocalsplus
    lea rdi, [r11 + PyFrame.localsplus]
    shl rdx, 3              ; nlocalsplus * 8
    lea rsi, [rdi + rdx]    ; rsi = &localsplus[nlocalsplus]
    mov [r11 + PyFrame.stack_base], rsi

    ; Zero the locals (one Value per slot; an empty slot is 0)
    mov ecx, [r11 + PyFrame.nlocalsplus]
    test ecx, ecx
    jz .done
    push r11                ; save frame pointer
    lea rdi, [r11 + PyFrame.localsplus]
    xor eax, eax
    rep stosq               ; store ecx qwords of 0 at [rdi]
    pop r11                 ; restore frame pointer

.done:
    mov rax, r11            ; return frame pointer
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

    mov rax, r13
    shl rax, 3              ; r13 * 8
    mov rdi, [rbx + PyFrame.localsplus + rax]
    XDECREF_V rdi, rsi      ; no-op for NULL and for immediates

.next:
    inc r13d
    jmp .loop

.free_frame:
    ; Calculate frame size for pool return
    mov rdi, [rbx + PyFrame.code]
    mov eax, [rdi + PyCodeObject.co_nlocalsplus]
    add eax, [rdi + PyCodeObject.co_stacksize]
    shl rax, 3
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
;; frame_pool_drain()
;; Free all entries in all pool freelists. Called at exit.
;; ============================================================================
global frame_pool_drain
DEF_FUNC frame_pool_drain
    push rbx
    push r12        ; alignment

    ; Drain pool class 0
    lea rbx, [rel pool_free_0]
.drain_0:
    mov rdi, [rbx]
    test rdi, rdi
    jz .done_0
    mov rax, [rdi]          ; next = head->next
    mov [rbx], rax          ; head = next
    call ap_free
    jmp .drain_0
.done_0:
    mov dword [rbx + 8], 0

    ; Drain pool class 1
    lea rbx, [rel pool_free_1]
.drain_1:
    mov rdi, [rbx]
    test rdi, rdi
    jz .done_1
    mov rax, [rdi]
    mov [rbx], rax
    call ap_free
    jmp .drain_1
.done_1:
    mov dword [rbx + 8], 0

    ; Drain pool class 2
    lea rbx, [rel pool_free_2]
.drain_2:
    mov rdi, [rbx]
    test rdi, rdi
    jz .done_2
    mov rax, [rdi]
    mov [rbx], rax
    call ap_free
    jmp .drain_2
.done_2:
    mov dword [rbx + 8], 0

    ; Drain pool class 3
    lea rbx, [rel pool_free_3]
.drain_3:
    mov rdi, [rbx]
    test rdi, rdi
    jz .done_3
    mov rax, [rdi]
    mov [rbx], rax
    call ap_free
    jmp .drain_3
.done_3:
    mov dword [rbx + 8], 0

    pop r12
    pop rbx
    leave
    ret
END_FUNC frame_pool_drain

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

;; ============================================================================
;; frame_fast_to_locals(PyFrame *f) -> PyDictObject*, owned, or NULL
;;
;; A function frame keeps its locals in the localsplus array, not in a mapping,
;; so PyFrame.locals is NULL for one -- and everything that wants a mapping
;; substituted globals instead.  locals() therefore returned the module dict,
;; and eval("lv + 1") inside a function raised NameError for a name that was
;; sitting in a slot two words away.
;;
;; This is CPython's PyFrame_FastToLocalsWithError: walk co_localsplusnames
;; against the slots, unwrapping the cells, and skip anything unbound.  It is a
;; snapshot -- writing to it does not write back, which is also what CPython
;; does outside a tracing hook.
;; ============================================================================
FTL_FRAME_P equ 8
FTL_DICT    equ 16
FTL_NAMES   equ 24
FTL_I       equ 32
FTL_N       equ 40
FTL_SIZE    equ 48        ; + 1 push = 56... padded below
FTL_FRAME   equ 56        ; + 1 push = 64
global frame_fast_to_locals
extern dict_new
extern dict_set
extern cell_type
DEF_FUNC frame_fast_to_locals, FTL_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - FTL_FRAME_P], rdi

    call dict_new
    test rax, rax
    jz .ftl_fail
    mov [rbp - FTL_DICT], rax

    mov rax, [rbx + PyFrame.code]
    test rax, rax
    jz .ftl_done
    mov rax, [rax + PyCodeObject.co_localsplusnames]
    test rax, rax
    jz .ftl_done
    mov [rbp - FTL_NAMES], rax
    mov rcx, [rax + PyTupleObject.ob_size]
    mov [rbp - FTL_N], rcx
    ; The frame's own count is the authority on how many slots exist.
    mov ecx, [rbx + PyFrame.nlocalsplus]
    cmp rcx, [rbp - FTL_N]
    jae .ftl_have_n
    mov [rbp - FTL_N], rcx
.ftl_have_n:
    mov qword [rbp - FTL_I], 0

.ftl_loop:
    mov rax, [rbp - FTL_I]
    cmp rax, [rbp - FTL_N]
    jae .ftl_done

    lea rdx, [rbx + PyFrame.localsplus]     ; an inline array, not a pointer
    mov rdx, [rdx + rax*8]
    test rdx, rdx
    jz .ftl_next                        ; the slot was never bound

    ; A cell or free slot holds the cell, not the value.
    V_TEST_PTR rdx, rcx
    ja .ftl_have_value
    mov rcx, [rdx + PyObject.ob_type]
    lea r8, [rel cell_type]
    cmp rcx, r8
    jne .ftl_have_value
    mov rdx, [rdx + PyCellObject.ob_ref]
    test rdx, rdx
    jz .ftl_next                        ; an empty cell is an unbound name

.ftl_have_value:
    mov rax, [rbp - FTL_NAMES]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rcx, [rbp - FTL_I]
    mov rsi, [rax + rcx*8]              ; the name, as a Value
    test rsi, rsi
    jz .ftl_next
    mov rdi, [rbp - FTL_DICT]
    call dict_set
.ftl_next:
    inc qword [rbp - FTL_I]
    jmp .ftl_loop

.ftl_done:
    mov rax, [rbp - FTL_DICT]
    pop rbx
    leave
    ret
.ftl_fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC frame_fast_to_locals
