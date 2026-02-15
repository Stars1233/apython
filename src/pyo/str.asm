; str_obj.asm - String type
; Phase 8: full string operations

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern ap_strlen
extern ap_memcpy
extern ap_strcmp
extern bool_true
extern bool_false
extern int_from_i64
extern int_to_i64
extern fatal_error
extern raise_exception
extern exc_IndexError_type
extern slice_type
extern slice_indices
extern type_type

; str_from_cstr(const char *cstr) -> PyStrObject*
; Creates a new string object from a C string
DEF_FUNC str_from_cstr
    push rbx
    push r12

    mov rbx, rdi            ; save cstr

    ; Get string length
    call ap_strlen
    mov r12, rax             ; r12 = length

    ; Allocate: PyStrObject header + length + 1 (null terminator)
    lea rdi, [rax + PyStrObject.data + 1]
    call ap_malloc
    ; rax = new PyStrObject*

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyStrObject.ob_size], r12
    mov qword [rax + PyStrObject.ob_hash], -1  ; not computed

    ; Copy string data
    push rax                 ; save obj ptr
    lea rdi, [rax + PyStrObject.data]
    mov rsi, rbx             ; source = cstr
    lea rdx, [r12 + 1]      ; length + null
    call ap_memcpy
    pop rax                  ; restore obj ptr

    pop r12
    pop rbx
    leave
    ret
END_FUNC str_from_cstr

; str_new(const char *data, int64_t len) -> PyStrObject*
; Creates a new string object from data with given length (need not be null-terminated)
DEF_FUNC str_new
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; save data ptr
    mov r12, rsi            ; save length

    ; Allocate: header + length + 1
    lea rdi, [r12 + PyStrObject.data + 1]
    call ap_malloc
    mov r13, rax             ; r13 = new PyStrObject*

    ; Fill header
    mov qword [r13 + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [r13 + PyObject.ob_type], rcx
    mov [r13 + PyStrObject.ob_size], r12
    mov qword [r13 + PyStrObject.ob_hash], -1

    ; Copy data
    lea rdi, [r13 + PyStrObject.data]
    mov rsi, rbx
    mov rdx, r12
    call ap_memcpy

    ; Null-terminate
    mov byte [r13 + PyStrObject.data + r12], 0

    mov rax, r13
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_new

; str_dealloc(PyObject *self)
DEF_FUNC_BARE str_dealloc
    ; String data is inline, just free the object
    jmp ap_free
END_FUNC str_dealloc

;; ============================================================================
;; str_repr(PyObject *self) -> PyObject*
;; Returns string with surrounding single quotes: 'hello'
;; ============================================================================
DEF_FUNC str_repr
    push rbx
    push r12

    mov rbx, rdi            ; rbx = self
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = length

    ; Allocate new string: header + 2 quotes + length + 1 null
    lea rdi, [r12 + PyStrObject.data + 3]
    call ap_malloc
    ; rax = new str

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    lea rcx, [r12 + 2]
    mov [rax + PyStrObject.ob_size], rcx
    mov qword [rax + PyStrObject.ob_hash], -1

    ; Write opening quote
    mov byte [rax + PyStrObject.data], "'"

    ; Copy string data
    push rax
    lea rdi, [rax + PyStrObject.data + 1]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy
    pop rax

    ; Write closing quote and null
    lea rcx, [r12 + 1]
    mov byte [rax + PyStrObject.data + rcx], "'"
    lea rcx, [r12 + 2]
    mov byte [rax + PyStrObject.data + rcx], 0

    pop r12
    pop rbx
    leave
    ret
END_FUNC str_repr

;; ============================================================================
;; str_str(PyObject *self) -> PyObject*
;; tp_str: returns self with INCREF (no quotes)
;; ============================================================================
DEF_FUNC_BARE str_str
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC str_str

;; ============================================================================
;; str_hash(PyObject *self) -> int64
;; FNV-1a hash
;; ============================================================================
DEF_FUNC str_hash

    ; Check cached hash
    mov rax, [rdi + PyStrObject.ob_hash]
    cmp rax, -1
    jne .done

    ; Compute FNV-1a
    mov rcx, [rdi + PyStrObject.ob_size]
    lea rsi, [rdi + PyStrObject.data]
    mov rax, 0xcbf29ce484222325     ; FNV offset basis
    mov rdx, 0x100000001b3          ; FNV prime
align 16
.loop:
    test rcx, rcx
    jz .store
    movzx r8d, byte [rsi]
    xor rax, r8
    imul rax, rdx
    inc rsi
    dec rcx
    jmp .loop
.store:
    ; Ensure hash is never -1
    cmp rax, -1
    jne .cache
    mov rax, -2
.cache:
    mov [rdi + PyStrObject.ob_hash], rax
.done:
    leave
    ret
END_FUNC str_hash

;; ============================================================================
;; str_concat(PyObject *a, PyObject *b) -> PyObject*
;; String concatenation via nb_add
;; ============================================================================
DEF_FUNC str_concat
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; a
    mov r12, rsi            ; b

    ; Get lengths
    mov r13, [rbx + PyStrObject.ob_size]   ; len_a
    add r13, [r12 + PyStrObject.ob_size]   ; total length

    ; Allocate new string
    lea rdi, [r13 + PyStrObject.data + 1]
    call ap_malloc
    push rax                ; save new str

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyStrObject.ob_size], r13
    mov qword [rax + PyStrObject.ob_hash], -1

    ; Copy first string
    lea rdi, [rax + PyStrObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, [rbx + PyStrObject.ob_size]
    call ap_memcpy

    ; Copy second string
    mov rax, [rsp]          ; reload new str
    mov rcx, [rbx + PyStrObject.ob_size]
    lea rdi, [rax + PyStrObject.data + rcx]
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, [r12 + PyStrObject.ob_size]
    call ap_memcpy

    ; Null-terminate
    pop rax
    mov byte [rax + PyStrObject.data + r13], 0

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_concat

;; ============================================================================
;; str_repeat(PyObject *str_obj, PyObject *int_obj) -> PyObject*
;; String repetition via nb_multiply
;; ============================================================================
DEF_FUNC str_repeat
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; str
    mov rdi, rsi            ; int
    call int_to_i64
    mov r12, rax             ; r12 = repeat count

    ; Clamp negative to 0
    test r12, r12
    jg .positive
    xor r12d, r12d
.positive:

    mov r13, [rbx + PyStrObject.ob_size]   ; r13 = str length
    imul r14, r13, 1                        ; r14 = str length (copy)
    imul r14, r12                           ; r14 = total length

    ; Allocate new string
    lea rdi, [r14 + PyStrObject.data + 1]
    call ap_malloc
    push rax                ; save

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyStrObject.ob_size], r14
    mov qword [rax + PyStrObject.ob_hash], -1

    ; Copy str r12 times
    lea rdi, [rax + PyStrObject.data]
    xor ecx, ecx            ; ecx = iteration counter
.repeat_loop:
    cmp rcx, r12
    jge .repeat_done
    push rcx
    push rdi
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r13
    call ap_memcpy
    pop rdi
    pop rcx
    add rdi, r13
    inc rcx
    jmp .repeat_loop

.repeat_done:
    ; Null-terminate
    pop rax
    mov byte [rax + PyStrObject.data + r14], 0

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_repeat

;; ============================================================================
;; str_compare(PyObject *a, PyObject *b, int op) -> PyObject*
;; Rich comparison for strings
;; ============================================================================
DEF_FUNC str_compare
    push rbx
    push r12

    mov ebx, edx            ; save op

    ; Compare using strcmp on the data
    push rdi
    push rsi
    lea rdi, [rdi + PyStrObject.data]
    lea rsi, [rsi + PyStrObject.data]
    call ap_strcmp
    mov r12d, eax            ; r12d = strcmp result
    pop rsi
    pop rdi

    ; Dispatch on comparison op
    cmp ebx, PY_LT
    je .do_lt
    cmp ebx, PY_LE
    je .do_le
    cmp ebx, PY_EQ
    je .do_eq
    cmp ebx, PY_NE
    je .do_ne
    cmp ebx, PY_GT
    je .do_gt
    jmp .do_ge

.do_lt:
    test r12d, r12d
    js .ret_true
    jmp .ret_false
.do_le:
    test r12d, r12d
    jle .ret_true
    jmp .ret_false
.do_eq:
    test r12d, r12d
    jz .ret_true
    jmp .ret_false
.do_ne:
    test r12d, r12d
    jnz .ret_true
    jmp .ret_false
.do_gt:
    test r12d, r12d
    jg .ret_true
    jmp .ret_false
.do_ge:
    test r12d, r12d
    jge .ret_true
    jmp .ret_false

.ret_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    leave
    ret
.ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_compare

;; ============================================================================
;; str_len(PyObject *self) -> int64_t
;; sq_length: returns ob_size
;; ============================================================================
DEF_FUNC_BARE str_len
    mov rax, [rdi + PyStrObject.ob_size]
    ret
END_FUNC str_len

;; ============================================================================
;; str_getitem(PyObject *self, int64_t index) -> PyObject*
;; sq_item: return single-char string at index
;; ============================================================================
DEF_FUNC str_getitem
    push rbx
    push r12

    mov rbx, rdi            ; self
    mov r12, rsi            ; index

    ; Handle negative index
    test r12, r12
    jns .positive
    add r12, [rbx + PyStrObject.ob_size]
.positive:

    ; Bounds check
    cmp r12, [rbx + PyStrObject.ob_size]
    jge .index_error
    cmp r12, 0
    jl .index_error

    ; Create single-char string
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r12
    mov rsi, 1
    call str_new

    pop r12
    pop rbx
    leave
    ret

.index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "string index out of range"
    call raise_exception
END_FUNC str_getitem

;; ============================================================================
;; str_subscript(PyObject *self, PyObject *key) -> PyObject*
;; mp_subscript: index with int or slice key (for BINARY_SUBSCR)
;; ============================================================================
DEF_FUNC str_subscript
    push rbx

    mov rbx, rdi            ; save self

    ; Check if key is a slice
    test rsi, rsi
    js .ss_int               ; SmallInt -> int path
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .ss_slice

.ss_int:
    ; Convert key to i64
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax

    ; Call str_getitem
    mov rdi, rbx
    call str_getitem

    pop rbx
    leave
    ret

.ss_slice:
    mov rdi, rbx
    ; rsi = slice
    call str_getslice
    pop rbx
    leave
    ret
END_FUNC str_subscript

;; ============================================================================
;; str_contains(PyObject *self, PyObject *substr) -> int (0/1)
;; sq_contains: check if substr is in self using strstr
;; ============================================================================
DEF_FUNC str_contains

    extern ap_strstr
    lea rdi, [rdi + PyStrObject.data]
    lea rsi, [rsi + PyStrObject.data]
    call ap_strstr
    test rax, rax
    setnz al
    movzx eax, al

    leave
    ret
END_FUNC str_contains

;; ============================================================================
;; str_bool(PyObject *self) -> int (0/1)
;; nb_bool: true if len > 0
;; ============================================================================
DEF_FUNC_BARE str_bool
    cmp qword [rdi + PyStrObject.ob_size], 0
    setne al
    movzx eax, al
    ret
END_FUNC str_bool

;; ============================================================================
;; str_getslice(PyStrObject *str, PySliceObject *slice) -> PyStrObject*
;; Creates a new string from a slice of the original.
;; ============================================================================
DEF_FUNC str_getslice
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                 ; align

    mov rbx, rdi               ; str
    mov r12, rsi               ; slice

    ; Get slice indices
    mov rdi, r12
    mov rsi, [rbx + PyStrObject.ob_size]
    call slice_indices
    mov r13, rax               ; start
    mov r14, rdx               ; stop
    mov r15, rcx               ; step

    ; Compute slicelength
    test r15, r15
    jg .sgs_pos_step
    ; Negative step
    mov rax, r13
    sub rax, r14
    dec rax
    mov rcx, r15
    neg rcx
    xor edx, edx
    div rcx
    inc rax
    jmp .sgs_have_len

.sgs_pos_step:
    mov rax, r14
    sub rax, r13
    jle .sgs_empty
    dec rax
    xor edx, edx
    div r15
    inc rax
    jmp .sgs_have_len

.sgs_empty:
    xor eax, eax

.sgs_have_len:
    ; rax = slicelength
    push rax                   ; save slicelength

    ; For step=1, fast path: use str_new with contiguous data
    cmp r15, 1
    jne .sgs_general

    ; Fast path: contiguous slice
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r13               ; data + start
    mov rsi, rax               ; length = slicelength
    call str_new
    add rsp, 8                 ; discard slicelength
    jmp .sgs_ret

.sgs_general:
    ; General case: build char by char on stack buffer
    ; Allocate: header + slicelength + 1
    mov rdi, rax
    add rdi, PyStrObject.data + 1
    call ap_malloc
    push rax                   ; save new str obj

    ; Fill header
    mov rcx, [rsp + 8]        ; slicelength
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rdx, [rel str_type]
    mov [rax + PyObject.ob_type], rdx
    mov [rax + PyStrObject.ob_size], rcx
    mov qword [rax + PyStrObject.ob_hash], -1

    ; Copy chars: for i=0..slicelength-1, dst[i] = src[start + i*step]
    xor ecx, ecx
.sgs_copy:
    cmp rcx, [rsp + 8]        ; slicelength
    jge .sgs_null_term
    mov rax, rcx
    imul rax, r15              ; i * step
    add rax, r13               ; start + i*step
    movzx edx, byte [rbx + PyStrObject.data + rax]
    mov rax, [rsp]             ; new str
    mov [rax + PyStrObject.data + rcx], dl
    inc rcx
    jmp .sgs_copy

.sgs_null_term:
    mov rax, [rsp]             ; new str
    mov rcx, [rsp + 8]        ; slicelength
    mov byte [rax + PyStrObject.data + rcx], 0

    pop rax                    ; new str
    add rsp, 8                 ; discard slicelength

.sgs_ret:
    add rsp, 8                 ; undo alignment
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_getslice

;; ============================================================================
;; Data section
;; ============================================================================
section .data

str_name: db "str", 0

; String number methods (for + and * operators)
align 8
str_number_methods:
    dq str_concat           ; nb_add          +0
    dq 0                    ; nb_subtract     +8
    dq str_repeat           ; nb_multiply     +16
    dq 0                    ; nb_remainder    +24
    dq 0                    ; nb_divmod       +32
    dq 0                    ; nb_power        +40
    dq 0                    ; nb_negative     +48
    dq 0                    ; nb_positive     +56
    dq 0                    ; nb_absolute     +64
    dq str_bool             ; nb_bool         +72
    dq 0                    ; nb_invert       +80
    dq 0                    ; nb_lshift       +88
    dq 0                    ; nb_rshift       +96
    dq 0                    ; nb_and          +104
    dq 0                    ; nb_xor          +112
    dq 0                    ; nb_or           +120
    dq 0                    ; nb_int          +128
    dq 0                    ; nb_float        +136
    dq 0                    ; nb_floor_divide +144
    dq 0                    ; nb_true_divide  +152
    dq 0                    ; nb_index        +160

; String sequence methods
align 8
str_sequence_methods:
    dq str_len              ; sq_length       +0
    dq 0                    ; sq_concat       +8
    dq 0                    ; sq_repeat       +16
    dq str_getitem          ; sq_item         +24
    dq 0                    ; sq_ass_item     +32
    dq str_contains         ; sq_contains     +40
    dq 0                    ; sq_inplace_concat +48
    dq 0                    ; sq_inplace_repeat +56

; String mapping methods (for BINARY_SUBSCR with int key)
align 8
str_mapping_methods:
    dq str_len              ; mp_length       +0
    dq str_subscript         ; mp_subscript    +8
    dq 0                    ; mp_ass_subscript +16

; str type object
align 8
global str_type
str_type:
    dq 1                ; ob_refcnt
    dq type_type        ; ob_type
    dq str_name         ; tp_name
    dq PyStrObject.data ; tp_basicsize (minimum, without data)
    dq str_dealloc      ; tp_dealloc
    dq str_repr         ; tp_repr
    dq str_str          ; tp_str (returns self for strings, no quotes)
    dq str_hash         ; tp_hash
    dq 0                ; tp_call
    dq 0                ; tp_getattr
    dq 0                ; tp_setattr
    dq str_compare      ; tp_richcompare
    dq 0                ; tp_iter
    dq 0                ; tp_iternext
    dq 0                ; tp_init
    dq 0                ; tp_new
    dq str_number_methods    ; tp_as_number
    dq str_sequence_methods  ; tp_as_sequence
    dq str_mapping_methods   ; tp_as_mapping
    dq 0                ; tp_base
    dq 0                ; tp_dict
    dq 0                ; tp_mro
    dq TYPE_FLAG_STR_SUBCLASS ; tp_flags
    dq 0                ; tp_bases
