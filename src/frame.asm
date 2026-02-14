; frame.asm - Frame allocation and deallocation
; Manages execution frames for the bytecode interpreter

%include "macros.inc"
%include "object.inc"
%include "frame.inc"

section .text

extern ap_malloc
extern ap_free
extern obj_decref

; frame_new(PyCodeObject *code, PyObject *globals, PyObject *builtins, PyObject *locals) -> PyFrame*
; Allocates and initializes a new execution frame.
; rdi = code, rsi = globals, rdx = builtins, rcx = locals
global frame_new
frame_new:
    push rbp
    mov rbp, rsp
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
    shl rax, 3              ; * 8 bytes per slot
    add rax, FRAME_HEADER_SIZE
    mov rdi, rax
    call ap_malloc
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

    ; Set nlocalsplus
    mov ecx, [rbx + PyCodeObject.co_nlocalsplus]
    mov [rax + PyFrame.nlocalsplus], ecx

    ; stack_base = &localsplus[nlocalsplus]
    mov edx, ecx            ; edx = nlocalsplus
    lea rdi, [rax + PyFrame.localsplus]
    lea rsi, [rdi + rdx*8]  ; rsi = &localsplus[nlocalsplus]
    mov [rax + PyFrame.stack_base], rsi

    ; Zero all localsplus entries (set to NULL)
    ; ecx still holds nlocalsplus
    test ecx, ecx
    jz .done
    push rax                ; save frame pointer
    mov rdi, rax            ; rdi = frame (for localsplus base calc)
    lea rdi, [rdi + PyFrame.localsplus]
    xor eax, eax
    mov ecx, ecx            ; zero-extend ecx (already done but be explicit)
    rep stosq               ; store ecx qwords of 0 at [rdi]
    pop rax                 ; restore frame pointer

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; frame_free(PyFrame *frame)
; XDECREF all non-NULL localsplus entries, then free the frame.
; rdi = frame
global frame_free
frame_free:
    push rbp
    mov rbp, rsp
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

    ; Load localsplus[r13]
    mov rdi, [rbx + PyFrame.localsplus + r13*8]
    test rdi, rdi
    jz .next

    ; DECREF non-NULL entry
    call obj_decref

.next:
    inc r13d
    jmp .loop

.free_frame:
    mov rdi, rbx
    call ap_free

    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
