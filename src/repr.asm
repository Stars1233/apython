; repr.asm - Container repr implementations (list, tuple, dict, set)
;
; Uses a heap buffer that grows as needed. Each repr function:
; 1. Allocates initial buffer
; 2. Appends opening bracket
; 3. For each element, calls obj_repr and appends result
; 4. Appends closing bracket
; 5. Converts buffer to PyStrObject

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern ap_realloc
extern obj_repr
extern obj_decref
extern str_from_cstr
extern str_type
extern str_repr

; Internal buffer struct (on stack):
;   [rbp-8]  = buf ptr
;   [rbp-16] = buf used (length of content)
;   [rbp-24] = buf capacity

; buf_ensure_space(needed)
; Ensures buf has at least 'needed' more bytes available.
; Uses [rbp-8], [rbp-16], [rbp-24]
; Clobbers rdi, rsi, rax
%macro BUF_ENSURE 1
    mov rax, [rbp-16]          ; used
    add rax, %1                ; used + needed
    add rax, 1                 ; +1 for NUL
    cmp rax, [rbp-24]          ; compare with capacity
    jbe %%ok
    ; Grow: new_cap = max(cap*2, used+needed+1)
    mov rdi, [rbp-24]
    shl rdi, 1                 ; cap * 2
    cmp rdi, rax
    cmovb rdi, rax             ; max(cap*2, needed)
    mov [rbp-24], rdi          ; save new capacity
    mov rsi, rdi               ; new size
    mov rdi, [rbp-8]           ; old ptr
    call ap_realloc
    mov [rbp-8], rax           ; save new ptr
%%ok:
%endmacro

; Append a single byte to buffer
%macro BUF_BYTE 1
    mov rax, [rbp-8]
    mov rcx, [rbp-16]
    mov byte [rax + rcx], %1
    inc qword [rbp-16]
%endmacro

;; ============================================================================
;; list_repr(PyListObject *self) -> PyStrObject*
;; Returns string like "[1, 2, 3]"
;; ============================================================================
global list_repr
list_repr:
    push rbp
    mov rbp, rsp
    sub rsp, 24                ; buf ptr, used, capacity
    push rbx                   ; self
    push r12                   ; index
    push r13                   ; count

    mov rbx, rdi               ; rbx = list

    ; Get count
    mov r13, [rbx + PyListObject.ob_size]

    ; Allocate initial buffer (256 bytes)
    mov edi, 256
    call ap_malloc
    mov [rbp-8], rax           ; buf ptr
    mov qword [rbp-16], 0      ; used = 0
    mov qword [rbp-24], 256    ; capacity = 256

    ; Append '['
    BUF_BYTE '['

    ; Iterate elements
    xor r12d, r12d             ; index = 0
.lr_loop:
    cmp r12, r13
    jge .lr_done

    ; If not first element, append ", "
    test r12, r12
    jz .lr_no_comma
    BUF_ENSURE 2
    BUF_BYTE ','
    BUF_BYTE ' '
.lr_no_comma:

    ; Get element
    mov rax, [rbx + PyListObject.ob_item]
    mov rdi, [rax + r12*8]     ; items[index]

    ; Call obj_repr
    call obj_repr
    test rax, rax
    jz .lr_next

    ; Append repr string to buffer
    push rax                   ; save repr str for DECREF
    mov rcx, [rax + PyStrObject.ob_size]
    BUF_ENSURE rcx
    ; Copy repr data into buffer
    mov rsi, [rsp]             ; repr str
    lea rsi, [rsi + PyStrObject.data]
    mov rdi, [rbp-8]
    add rdi, [rbp-16]          ; buf + used
    mov rcx, [rsp]
    mov rcx, [rcx + PyStrObject.ob_size]
    add [rbp-16], rcx          ; used += len
    ; memcpy
    rep movsb

    ; DECREF repr str
    pop rdi
    call obj_decref

.lr_next:
    inc r12
    jmp .lr_loop

.lr_done:
    ; Append ']' and NUL
    BUF_ENSURE 2
    BUF_BYTE ']'
    mov rax, [rbp-8]
    mov rcx, [rbp-16]
    mov byte [rax + rcx], 0    ; NUL terminate

    ; Convert to PyStrObject
    mov rdi, [rbp-8]
    call str_from_cstr
    push rax                   ; save result

    ; Free buffer
    mov rdi, [rbp-8]
    call ap_free

    pop rax                    ; return str
    pop r13
    pop r12
    pop rbx
    leave
    ret

;; ============================================================================
;; tuple_repr(PyTupleObject *self) -> PyStrObject*
;; Returns string like "(1, 2, 3)" or "(1,)" for single-element
;; ============================================================================
global tuple_repr
tuple_repr:
    push rbp
    mov rbp, rsp
    sub rsp, 24
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; rbx = tuple

    mov r13, [rbx + PyTupleObject.ob_size]

    ; Allocate buffer
    mov edi, 256
    call ap_malloc
    mov [rbp-8], rax
    mov qword [rbp-16], 0
    mov qword [rbp-24], 256

    BUF_BYTE '('

    xor r12d, r12d
.tr_loop:
    cmp r12, r13
    jge .tr_done

    test r12, r12
    jz .tr_no_comma
    BUF_ENSURE 2
    BUF_BYTE ','
    BUF_BYTE ' '
.tr_no_comma:

    ; Get element (tuple items are inline at +32)
    lea rax, [rbx + PyTupleObject.ob_item]
    mov rdi, [rax + r12*8]
    call obj_repr
    test rax, rax
    jz .tr_next

    push rax
    mov rcx, [rax + PyStrObject.ob_size]
    BUF_ENSURE rcx
    mov rsi, [rsp]
    lea rsi, [rsi + PyStrObject.data]
    mov rdi, [rbp-8]
    add rdi, [rbp-16]
    mov rcx, [rsp]
    mov rcx, [rcx + PyStrObject.ob_size]
    add [rbp-16], rcx
    rep movsb

    pop rdi
    call obj_decref

.tr_next:
    inc r12
    jmp .tr_loop

.tr_done:
    ; Single-element tuple needs trailing comma
    cmp r13, 1
    jne .tr_no_trailing
    BUF_ENSURE 1
    BUF_BYTE ','
.tr_no_trailing:

    BUF_ENSURE 2
    BUF_BYTE ')'
    mov rax, [rbp-8]
    mov rcx, [rbp-16]
    mov byte [rax + rcx], 0

    mov rdi, [rbp-8]
    call str_from_cstr
    push rax

    mov rdi, [rbp-8]
    call ap_free

    pop rax
    pop r13
    pop r12
    pop rbx
    leave
    ret

;; ============================================================================
;; dict_repr(PyDictObject *self) -> PyStrObject*
;; Returns string like "{'a': 1, 'b': 2}"
;; Iterates the entries array directly.
;; ============================================================================
global dict_repr
dict_repr:
    push rbp
    mov rbp, rsp
    sub rsp, 24
    push rbx                   ; self
    push r12                   ; entry index
    push r13                   ; capacity
    push r14                   ; items printed count

    mov rbx, rdi

    ; Allocate buffer
    mov edi, 256
    call ap_malloc
    mov [rbp-8], rax
    mov qword [rbp-16], 0
    mov qword [rbp-24], 256

    BUF_BYTE '{'

    mov r13, [rbx + PyDictObject.capacity]
    xor r12d, r12d             ; entry index = 0
    xor r14d, r14d             ; items printed = 0

.dr_loop:
    cmp r12, r13
    jge .dr_done

    ; Check if entry is occupied: entries[i].key != NULL
    mov rax, [rbx + PyDictObject.entries]
    ; Each DictEntry is 24 bytes: hash(8) + key(8) + value(8)
    imul rcx, r12, 24
    mov rdi, [rax + rcx + 8]   ; key
    test rdi, rdi
    jz .dr_next_entry

    ; Print separator if not first
    test r14, r14
    jz .dr_no_comma
    BUF_ENSURE 2
    BUF_BYTE ','
    BUF_BYTE ' '
.dr_no_comma:

    ; repr(key)
    ; rdi already = key
    push r12                   ; save entry index across calls
    call obj_repr
    test rax, rax
    jz .dr_after_key

    push rax
    mov rcx, [rax + PyStrObject.ob_size]
    BUF_ENSURE rcx
    mov rsi, [rsp]
    lea rsi, [rsi + PyStrObject.data]
    mov rdi, [rbp-8]
    add rdi, [rbp-16]
    mov rcx, [rsp]
    mov rcx, [rcx + PyStrObject.ob_size]
    add [rbp-16], rcx
    rep movsb
    pop rdi
    call obj_decref

.dr_after_key:
    ; Append ": "
    BUF_ENSURE 2
    BUF_BYTE ':'
    BUF_BYTE ' '

    ; repr(value)
    pop r12                    ; restore entry index
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r12, 24
    mov rdi, [rax + rcx + 16]  ; value
    push r12
    call obj_repr
    test rax, rax
    jz .dr_after_val

    push rax
    mov rcx, [rax + PyStrObject.ob_size]
    BUF_ENSURE rcx
    mov rsi, [rsp]
    lea rsi, [rsi + PyStrObject.data]
    mov rdi, [rbp-8]
    add rdi, [rbp-16]
    mov rcx, [rsp]
    mov rcx, [rcx + PyStrObject.ob_size]
    add [rbp-16], rcx
    rep movsb
    pop rdi
    call obj_decref

.dr_after_val:
    pop r12                    ; restore entry index
    inc r14                    ; items printed++

.dr_next_entry:
    inc r12
    jmp .dr_loop

.dr_done:
    BUF_ENSURE 2
    BUF_BYTE '}'
    mov rax, [rbp-8]
    mov rcx, [rbp-16]
    mov byte [rax + rcx], 0

    mov rdi, [rbp-8]
    call str_from_cstr
    push rax

    mov rdi, [rbp-8]
    call ap_free

    pop rax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

;; ============================================================================
;; set_repr(PySetObject *self) -> PyStrObject*
;; Returns string like "{1, 2, 3}" or "set()" for empty
;; ============================================================================
global set_repr
set_repr:
    push rbp
    mov rbp, rsp
    sub rsp, 24
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi

    ; Check empty set
    cmp qword [rbx + PyDictObject.ob_size], 0
    jne .sr_notempty
    lea rdi, [rel .sr_empty_str]
    call str_from_cstr
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sr_notempty:
    mov edi, 256
    call ap_malloc
    mov [rbp-8], rax
    mov qword [rbp-16], 0
    mov qword [rbp-24], 256

    BUF_BYTE '{'

    mov r13, [rbx + PyDictObject.capacity]
    xor r12d, r12d
    xor r14d, r14d

.sr_loop:
    cmp r12, r13
    jge .sr_done

    ; SetEntry is 16 bytes: hash(8) + key(8)
    mov rax, [rbx + PyDictObject.entries]
    mov rcx, r12
    shl rcx, 4                    ; index * 16
    mov rdi, [rax + rcx + 8]     ; key
    test rdi, rdi
    jz .sr_next

    test r14, r14
    jz .sr_no_comma
    BUF_ENSURE 2
    BUF_BYTE ','
    BUF_BYTE ' '
.sr_no_comma:

    push r12
    call obj_repr
    test rax, rax
    jz .sr_after_elem

    push rax
    mov rcx, [rax + PyStrObject.ob_size]
    BUF_ENSURE rcx
    mov rsi, [rsp]
    lea rsi, [rsi + PyStrObject.data]
    mov rdi, [rbp-8]
    add rdi, [rbp-16]
    mov rcx, [rsp]
    mov rcx, [rcx + PyStrObject.ob_size]
    add [rbp-16], rcx
    rep movsb
    pop rdi
    call obj_decref

.sr_after_elem:
    pop r12
    inc r14

.sr_next:
    inc r12
    jmp .sr_loop

.sr_done:
    BUF_ENSURE 2
    BUF_BYTE '}'
    mov rax, [rbp-8]
    mov rcx, [rbp-16]
    mov byte [rax + rcx], 0

    mov rdi, [rbp-8]
    call str_from_cstr
    push rax

    mov rdi, [rbp-8]
    call ap_free

    pop rax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

section .rodata
.sr_empty_str: db "set()", 0
