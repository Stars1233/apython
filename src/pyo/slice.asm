; slice.asm - Slice type implementation
;
; PySliceObject layout (from object.inc):
;   +0  ob_refcnt (8 bytes)
;   +8  ob_type   (8 bytes)
;   +16 start     (8 bytes: PyObject*)
;   +24 stop      (8 bytes: PyObject*)
;   +32 step      (8 bytes: PyObject*)
;   Total: 40 bytes

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_incref
extern obj_dealloc
extern obj_decref
extern str_from_cstr
extern none_singleton
extern int_type
extern type_type
extern slice_traverse
extern slice_clear_gc
extern raise_exception
extern exc_TypeError_type
extern exc_AttributeError_type
extern ap_strcmp

;; ============================================================================
;; slice_new(PyObject *start, PyObject *stop, PyObject *step) -> PySliceObject*
;; INCREFs all three args. Caller should pass none_singleton for missing values.
;; ============================================================================
DEF_FUNC slice_new
    ; rdi=start, rsi=stop, rdx=step, ecx=start_tag, r8d=stop_tag, r9d=step_tag
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi           ; start
    mov r12, rsi           ; stop
    mov r13, rdx           ; step
    mov r14d, ecx          ; start_tag
    mov r15d, r8d          ; stop_tag
    push r9                ; save step_tag across malloc

    mov edi, PySliceObject_size
    lea rsi, [rel slice_type]
    call gc_alloc

    pop r9                  ; step_tag
    ; ob_refcnt=1, ob_type set by gc_alloc
    mov [rax + PySliceObject.start], rbx
    mov [rax + PySliceObject.start_tag], r14
    mov [rax + PySliceObject.stop], r12
    mov [rax + PySliceObject.stop_tag], r15
    mov [rax + PySliceObject.step], r13
    mov [rax + PySliceObject.step_tag], r9

    ; INCREF all three (tag-aware)
    push rax
    INCREF_VAL rbx, r14
    INCREF_VAL r12, r15
    INCREF_VAL r13, r9

    ; Track in GC
    mov rdi, [rsp]          ; obj ptr saved on stack
    call gc_track
    pop rax

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC slice_new

;; ============================================================================
;; slice_dealloc(PySliceObject *self)
;; ============================================================================
DEF_FUNC slice_dealloc
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PySliceObject.start]
    mov rsi, [rbx + PySliceObject.start_tag]
    DECREF_VAL rdi, rsi
    mov rdi, [rbx + PySliceObject.stop]
    mov rsi, [rbx + PySliceObject.stop_tag]
    DECREF_VAL rdi, rsi
    mov rdi, [rbx + PySliceObject.step]
    mov rsi, [rbx + PySliceObject.step_tag]
    DECREF_VAL rdi, rsi
    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC slice_dealloc

;; ============================================================================
;; slice_repr(PySliceObject *self) -> PyStrObject*
;; ============================================================================
DEF_FUNC_BARE slice_repr
    lea rdi, [rel slice_repr_str]
    jmp str_from_cstr
END_FUNC slice_repr

;; ============================================================================
;; pyobj_to_i64(PyObject *obj) -> int64 in rax
;; Converts SmallInt or GMP int to int64. For None, returns special sentinel.
;; ============================================================================
pyobj_to_i64:
    ; rdi = payload, esi = tag
    cmp esi, TAG_SMALLINT
    je .smallint
    ; Check for None: inline TAG_NONE or pointer-to-none_singleton
    cmp esi, TAG_NONE
    je .is_none
    lea rax, [rel none_singleton]
    cmp rdi, rax
    je .is_none
    ; GMP int: use mpz_get_si
    push rbp
    mov rbp, rsp
    lea rdi, [rdi + PyIntObject.mpz]
    extern __gmpz_get_si
    call __gmpz_get_si wrt ..plt
    leave
    ret
.smallint:
    mov rax, rdi
    ret
.is_none:
    mov rax, 0x7FFFFFFFFFFFFFFF  ; sentinel for "not specified"
    ret
END_FUNC pyobj_to_i64

;; ============================================================================
;; slice_indices(PySliceObject *slice, int64 length)
;;   -> (start, stop, step) in rax, rdx, rcx
;; Resolves None values, handles negatives, clamps to bounds.
;; ============================================================================
DEF_FUNC slice_indices
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8             ; align

    mov rbx, rdi           ; slice
    mov r14, rsi           ; length

    ; Sentinel constant for "None" from pyobj_to_i64
    ; (can't use cmp rax, imm64 — x86-64 doesn't support it)
%define NONE_SENTINEL 0x7FFFFFFFFFFFFFFF

    ; Get step (default 1)
    mov rdi, [rbx + PySliceObject.step]
    mov esi, [rbx + PySliceObject.step_tag]
    call pyobj_to_i64
    mov rcx, NONE_SENTINEL
    cmp rax, rcx
    jne .have_step
    mov rax, 1
.have_step:
    mov r15, rax           ; r15 = step

    ; Get start (default: 0 if step>0, length-1 if step<0)
    mov rdi, [rbx + PySliceObject.start]
    mov esi, [rbx + PySliceObject.start_tag]
    call pyobj_to_i64
    mov rcx, NONE_SENTINEL
    cmp rax, rcx
    jne .have_start
    test r15, r15
    js .start_neg_step
    xor eax, eax           ; start = 0
    jmp .have_start
.start_neg_step:
    mov rax, r14
    dec rax                ; start = length - 1
.have_start:
    ; Handle negative start
    test rax, rax
    jns .start_pos
    add rax, r14           ; start += length
    test rax, rax
    jns .start_pos
    xor eax, eax           ; clamp to 0
.start_pos:
    cmp rax, r14
    jl .start_ok
    mov rax, r14           ; clamp to length
.start_ok:
    mov r12, rax           ; r12 = start

    ; Get stop (default: length if step>0, -1 if step<0)
    mov rdi, [rbx + PySliceObject.stop]
    mov esi, [rbx + PySliceObject.stop_tag]
    call pyobj_to_i64
    mov rcx, NONE_SENTINEL
    cmp rax, rcx
    jne .have_stop
    test r15, r15
    js .stop_neg_step
    mov rax, r14           ; stop = length
    jmp .have_stop
.stop_neg_step:
    mov rax, -1            ; stop = -1 (before start)
    jmp .stop_ok           ; default value is already correct, skip adjustment
.have_stop:
    ; Handle negative stop
    test rax, rax
    jns .stop_pos
    add rax, r14           ; stop += length
    test rax, rax
    jns .stop_pos
    ; For negative step, stop=-1 is valid (means "go to beginning")
    test r15, r15
    js .stop_ok
    xor eax, eax           ; clamp to 0 for positive step
    jmp .stop_ok
.stop_pos:
    cmp rax, r14
    jle .stop_ok
    mov rax, r14           ; clamp to length
.stop_ok:
    mov r13, rax           ; r13 = stop

    ; Return: rax=start, rdx=stop, rcx=step
    mov rax, r12
    mov rdx, r13
    mov rcx, r15

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC slice_indices

;; ============================================================================
;; slice_getattr(PySliceObject *self, PyObject *name) -> (rax, edx) fat value
;; Returns start, stop, step attributes.
;; rdi = self, rsi = name (PyStrObject*)
;; ============================================================================
global slice_getattr
DEF_FUNC slice_getattr
    push rbx
    push r12
    mov rbx, rdi               ; rbx = self (slice object)
    mov r12, rsi               ; r12 = name string

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "start"
    call ap_strcmp
    test eax, eax
    jz .sg_start

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "stop"
    call ap_strcmp
    test eax, eax
    jz .sg_stop

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "step"
    call ap_strcmp
    test eax, eax
    jz .sg_step

    ; Unknown attribute
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "slice object has no such attribute"
    call raise_exception

.sg_start:
    mov rax, [rbx + PySliceObject.start]
    mov rdx, [rbx + PySliceObject.start_tag]
    INCREF_VAL rax, rdx
    pop r12
    pop rbx
    leave
    ret

.sg_stop:
    mov rax, [rbx + PySliceObject.stop]
    mov rdx, [rbx + PySliceObject.stop_tag]
    INCREF_VAL rax, rdx
    pop r12
    pop rbx
    leave
    ret

.sg_step:
    mov rax, [rbx + PySliceObject.step]
    mov rdx, [rbx + PySliceObject.step_tag]
    INCREF_VAL rax, rdx
    pop r12
    pop rbx
    leave
    ret
END_FUNC slice_getattr

;; ============================================================================
;; slice_type_call(self, args, nargs) -> (rax, edx) fat value
;; slice(stop), slice(start, stop), slice(start, stop, step)
;; rdi = self (slice_type), rsi = args (16-byte fat slots), rdx = nargs
;; ============================================================================
global slice_type_call
DEF_FUNC slice_type_call
    push rbx

    mov rbx, rsi               ; rbx = args ptr

    cmp rdx, 1
    je .stc_one
    cmp rdx, 2
    je .stc_two
    cmp rdx, 3
    je .stc_three
    jmp .stc_error

.stc_one:
    ; slice(stop) → slice(None, stop, None)
    lea rdi, [rel none_singleton]  ; start = None
    mov ecx, TAG_PTR               ; start_tag
    mov rsi, [rbx]                 ; stop payload
    mov r8d, [rbx + 8]            ; stop_tag
    lea rdx, [rel none_singleton]  ; step = None
    mov r9d, TAG_PTR               ; step_tag
    call slice_new
    mov edx, TAG_PTR
    jmp .stc_done

.stc_two:
    ; slice(start, stop) → slice(start, stop, None)
    mov rdi, [rbx]             ; start payload
    mov ecx, [rbx + 8]        ; start_tag
    mov rsi, [rbx + 16]       ; stop payload
    mov r8d, [rbx + 24]       ; stop_tag
    lea rdx, [rel none_singleton]  ; step = None
    mov r9d, TAG_PTR           ; step_tag
    call slice_new
    mov edx, TAG_PTR
    jmp .stc_done

.stc_three:
    ; slice(start, stop, step)
    mov rdi, [rbx]             ; start payload
    mov ecx, [rbx + 8]        ; start_tag
    mov rsi, [rbx + 16]       ; stop payload
    mov r8d, [rbx + 24]       ; stop_tag
    mov rdx, [rbx + 32]       ; step payload
    mov r9d, [rbx + 40]       ; step_tag
    call slice_new
    mov edx, TAG_PTR
    jmp .stc_done

.stc_done:
    pop rbx
    leave
    ret

.stc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "slice() takes 1 to 3 arguments"
    call raise_exception
END_FUNC slice_type_call

;; ============================================================================
;; Data
;; ============================================================================
section .data

slice_name_str: db "slice", 0
slice_repr_str: db "slice(...)", 0

align 8
global slice_type
slice_type:
    dq 1                      ; ob_refcnt (immortal)
    dq type_type              ; ob_type
    dq slice_name_str         ; tp_name
    dq PySliceObject_size     ; tp_basicsize
    dq slice_dealloc          ; tp_dealloc
    dq slice_repr             ; tp_repr
    dq slice_repr             ; tp_str
    dq 0                      ; tp_hash
    dq slice_type_call        ; tp_call
    dq slice_getattr          ; tp_getattr
    dq 0                      ; tp_setattr
    dq 0                      ; tp_richcompare
    dq 0                      ; tp_iter
    dq 0                      ; tp_iternext
    dq 0                      ; tp_init
    dq 0                      ; tp_new
    dq 0                      ; tp_as_number
    dq 0                      ; tp_as_sequence
    dq 0                      ; tp_as_mapping
    dq 0                      ; tp_base
    dq 0                      ; tp_dict
    dq 0                      ; tp_mro
    dq TYPE_FLAG_HAVE_GC                      ; tp_flags
    dq 0                      ; tp_bases
    dq slice_traverse                        ; tp_traverse
    dq slice_clear_gc                        ; tp_clear
