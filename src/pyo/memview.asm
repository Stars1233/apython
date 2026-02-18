; memview.asm - memoryview type implementation (minimal, for test_int.py)
; Read-only view over a bytes-like object's buffer

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern ap_memcpy
extern type_type
extern obj_incref
extern obj_decref
extern raise_exception
extern exc_TypeError_type
extern exc_IndexError_type
extern bytes_type
extern int_to_i64
extern slice_type
extern slice_indices

section .text

;; ============================================================================
;; memoryview_type_call(type, args, nargs) -> PyMemoryViewObject*
;; Constructor: memoryview(bytes_obj)
;; ============================================================================
global memoryview_type_call
MV_FRAME equ 8
DEF_FUNC memoryview_type_call, MV_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    cmp rdx, 1
    jne .mv_error
    mov rdi, [rsi]                     ; arg0 payload
    ; Must be a bytes-like object
    cmp dword [rsi + 8], TAG_SMALLINT
    je .mv_error                       ; SmallInt → error
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .mv_check_bytearray

.mv_from_bytes:
    ; rdi = bytes obj
    push rdi
    mov edi, PyMemoryViewObject_size
    call ap_malloc
    pop rdi                            ; source bytes

    ; Init header
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx
    mov [rax + PyMemoryViewObject.mv_source], rdi
    push rax                           ; save result
    push rdi                           ; save for INCREF
    INCREF rdi
    pop rdi
    pop rax
    ; Set buffer pointer and length
    mov rcx, [rdi + PyBytesObject.ob_size]
    mov [rax + PyMemoryViewObject.mv_len], rcx
    lea rcx, [rdi + PyBytesObject.data]
    mov [rax + PyMemoryViewObject.mv_buf], rcx
    mov edx, TAG_PTR
    leave
    ret

.mv_check_bytearray:
    extern bytearray_type
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .mv_from_bytes                  ; same layout as bytes
    jmp .mv_error

.mv_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "memoryview: a bytes-like object is required"
    call raise_exception
END_FUNC memoryview_type_call

;; ============================================================================
;; memoryview_dealloc(obj)
;; ============================================================================
DEF_FUNC memoryview_dealloc
    mov rdi, [rdi + PyMemoryViewObject.mv_source]
    call obj_decref
    ; Free the memoryview itself (rdi was clobbered, but we need original obj)
    ; Actually, we need the original obj pointer. Rethink:
    leave
    ret                                ; leak for now... fix below
END_FUNC memoryview_dealloc

;; Proper dealloc:
DEF_FUNC memoryview_dealloc_proper
    push rdi                           ; save self
    mov rdi, [rdi + PyMemoryViewObject.mv_source]
    call obj_decref
    pop rdi                            ; restore self
    call ap_free
    leave
    ret
END_FUNC memoryview_dealloc_proper

;; ============================================================================
;; memoryview_subscript(obj, key) -> PyMemoryViewObject* (slice)
;; ============================================================================
MS_OBJ   equ 8
MS_KEY   equ 16
MS_FRAME equ 16
DEF_FUNC memoryview_subscript, MS_FRAME
    mov [rbp - MS_OBJ], rdi
    mov [rbp - MS_KEY], rsi

    ; Check if key is a SmallInt (edx = key tag from caller)
    cmp edx, TAG_SMALLINT
    je .ms_int_index                   ; SmallInt index
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    jne .ms_int_index_heap

    ; Slice: call slice_indices(slice, length) -> rax=start, rdx=stop, rcx=step
    mov rdi, rsi                       ; slice obj
    mov rsi, [rbp - MS_OBJ]
    mov rsi, [rsi + PyMemoryViewObject.mv_len]  ; length
    call slice_indices
    ; rax=start, rdx=stop, rcx=step

    ; Only support step=1 for now
    cmp rcx, 1
    jne .ms_step_error

    mov r8, rax                        ; start
    sub rdx, rax
    mov r9, rdx                        ; slicelength = stop - start

    ; Create new memoryview pointing to slice of source
    push r8                            ; save start
    push r9                            ; save slicelength
    mov edi, PyMemoryViewObject_size
    call ap_malloc
    pop r9                             ; slicelength
    pop r8                             ; start

    ; Init the new memoryview
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx

    ; Share the same source object
    mov rdi, [rbp - MS_OBJ]
    mov rcx, [rdi + PyMemoryViewObject.mv_source]
    mov [rax + PyMemoryViewObject.mv_source], rcx

    ; Buffer = original buffer + start
    mov rdx, [rdi + PyMemoryViewObject.mv_buf]
    add rdx, r8
    mov [rax + PyMemoryViewObject.mv_buf], rdx

    ; Length = slicelength
    mov [rax + PyMemoryViewObject.mv_len], r9

    ; INCREF source
    push rax
    mov rdi, rcx
    INCREF rdi
    pop rax

    mov edx, TAG_PTR
    leave
    ret

.ms_int_index:
    ; SmallInt index — return single byte as SmallInt
    mov rdi, [rbp - MS_OBJ]
    ; Handle negative index
    mov rcx, [rdi + PyMemoryViewObject.mv_len]
    test rsi, rsi
    jns .ms_check_bounds
    add rsi, rcx
.ms_check_bounds:
    cmp rsi, 0
    jl .ms_index_error
    cmp rsi, rcx
    jge .ms_index_error
    mov rdx, [rdi + PyMemoryViewObject.mv_buf]
    movzx eax, byte [rdx + rsi]
    RET_TAG_SMALLINT
    leave
    ret

.ms_int_index_heap:
    ; Heap int index — convert to i64
    push rdi
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax
    pop rdi
    jmp .ms_check_bounds

.ms_index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "index out of range"
    call raise_exception

.ms_step_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "memoryview: unsupported step"
    call raise_exception
END_FUNC memoryview_subscript

;; ============================================================================
;; memoryview_len(obj) -> int64
;; ============================================================================
DEF_FUNC_BARE memoryview_len
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    ret
END_FUNC memoryview_len

;; ============================================================================
;; Type object
;; ============================================================================
section .data

align 8
mv_name_str:  db "memoryview", 0

align 8
memoryview_seq_methods:
    dq memoryview_len       ; +0: sq_length
    dq 0                    ; +8: sq_concat
    dq 0                    ; +16: sq_repeat
    dq 0                    ; +24: sq_item
    dq 0                    ; +32: sq_ass_item
    dq 0                    ; +40: sq_contains
    dq 0                    ; +48: sq_inplace_concat
    dq 0                    ; +56: sq_inplace_repeat

align 8
memoryview_mapping_methods:
    dq memoryview_len       ; +0: mp_length
    dq memoryview_subscript ; +8: mp_subscript
    dq 0                    ; +16: mp_ass_subscript

align 8
global memoryview_type
memoryview_type:
    dq 1                             ; ob_refcnt
    dq type_type                     ; ob_type
    dq mv_name_str                   ; tp_name
    dq PyMemoryViewObject_size       ; tp_basicsize
    dq memoryview_dealloc_proper     ; tp_dealloc
    dq 0                             ; tp_repr
    dq 0                             ; tp_str
    dq 0                             ; tp_hash
    dq 0                             ; tp_call (set by add_builtin_type)
    dq 0                             ; tp_getattr
    dq 0                             ; tp_setattr
    dq 0                             ; tp_richcompare
    dq 0                             ; tp_iter
    dq 0                             ; tp_iternext
    dq 0                             ; tp_init
    dq 0                             ; tp_new
    dq 0                             ; tp_as_number
    dq memoryview_seq_methods        ; tp_as_sequence
    dq memoryview_mapping_methods    ; tp_as_mapping
    dq 0                             ; tp_base
    dq 0                             ; tp_dict
    dq 0                             ; tp_mro
    dq 0                             ; tp_flags
    dq 0                             ; tp_bases
