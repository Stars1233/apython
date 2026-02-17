; object.asm - PyObject base operations
; Allocation, reference counting, type dispatch for repr/str/hash/bool
; SmallInt-aware: all functions check bit 63 for tagged pointers

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern sys_write
extern str_from_cstr
extern none_singleton
extern bool_false
extern bool_true
extern int_repr
extern int_type
extern float_type
extern float_repr
extern none_repr
extern bool_repr
extern type_getattr
extern type_setattr
extern type_call

; obj_alloc(size_t size, PyTypeObject *type) -> PyObject*
; Allocate a new object with refcount=1 and given type
DEF_FUNC obj_alloc
    push rbx
    push r12
    mov rbx, rdi            ; size
    mov r12, rsi            ; type

    mov rdi, rbx
    call ap_malloc

    ; Initialize header
    mov qword [rax + PyObject.ob_refcnt], 1
    mov [rax + PyObject.ob_type], r12

    pop r12
    pop rbx
    leave
    ret
END_FUNC obj_alloc

; obj_incref(PyObject *obj)
; Increment reference count; NULL-safe.
; Callers must only pass heap pointers (not SmallInts).
DEF_FUNC_BARE obj_incref
    test rdi, rdi
    jz .skip
    inc qword [rdi + PyObject.ob_refcnt]
.skip:
    ret
END_FUNC obj_incref

; obj_decref(PyObject *obj)
; Decrement reference count; deallocate if zero; NULL-safe.
; Callers must only pass heap pointers (not SmallInts).
DEF_FUNC_BARE obj_decref
    test rdi, rdi
    jz .skip
    dec qword [rdi + PyObject.ob_refcnt]
    jnz .skip
    ; refcount hit zero - deallocate
    jmp obj_dealloc
.skip:
    ret
END_FUNC obj_decref

; obj_dealloc(PyObject *obj)
; Calls type's tp_dealloc if present, else just frees
DEF_FUNC_BARE obj_dealloc

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; Get type's tp_dealloc
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .just_free
    mov rax, [rax + PyTypeObject.tp_dealloc]
    test rax, rax
    jz .just_free

    ; Call tp_dealloc(obj)
    mov rdi, rbx
    call rax
    pop rbx
    pop rbp
    ret

.just_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    pop rbp
.bail:
    ret
END_FUNC obj_dealloc

; obj_repr(rdi=payload, rsi=tag) -> PyObject* (string)
; Dispatches on tag. SmallInt → int_repr. TAG_PTR → tp_repr.
; Also handles TAG_FLOAT, TAG_NONE, TAG_BOOL inline.
DEF_FUNC obj_repr

    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_FLOAT
    je .float_tag
    cmp esi, TAG_NONE
    je .none_tag
    cmp esi, TAG_BOOL
    je .bool_tag

    ; TAG_PTR: use tp_repr
    test rdi, rdi
    jz .null_obj

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .no_repr
    mov rax, [rax + PyTypeObject.tp_repr]
    test rax, rax
    jz .no_repr

    ; tail-call tp_repr(obj, tag)
    mov edx, esi               ; pass tag for tag-aware repr (e.g., int_repr)
    leave
    jmp rax

.smallint:
    ; rdi = raw int value — int_repr handles SmallInt payloads
    RET_TAG_SMALLINT
    call int_repr
    leave
    ret

.float_tag:
    ; Create temp PyFloatObject on stack, call float_repr
    sub rsp, 32
    mov qword [rsp], 1            ; ob_refcnt
    lea rax, [rel float_type]
    mov [rsp + 8], rax             ; ob_type
    mov [rsp + 16], rdi            ; value (double bits)
    mov rdi, rsp
    call float_repr
    add rsp, 32
    leave
    ret

.none_tag:
    call none_repr
    leave
    ret

.bool_tag:
    test rdi, rdi
    jz .bool_false_repr
    lea rdi, [rel bool_true]
    call bool_repr
    leave
    ret
.bool_false_repr:
    lea rdi, [rel bool_false]
    call bool_repr
    leave
    ret

.null_obj:
.no_repr:
    xor eax, eax
    leave
    ret
END_FUNC obj_repr

; obj_str(rdi=payload, rsi=tag) -> PyObject* (string)
; Dispatches on tag. SmallInt → int_repr. TAG_PTR → tp_str, falls back to tp_repr.
; Also handles TAG_FLOAT, TAG_NONE, TAG_BOOL inline.
DEF_FUNC obj_str
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi               ; save tag

    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_FLOAT
    je .float_tag
    cmp esi, TAG_NONE
    je .none_tag
    cmp esi, TAG_BOOL
    je .bool_tag

    ; TAG_PTR path
    test rdi, rdi
    jz .fallback

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .fallback

    mov rax, [rax + PyTypeObject.tp_str]
    test rax, rax
    jz .fallback

    mov rdi, rbx
    mov edx, r12d              ; tag for tp_str (e.g., int_repr checks edx)
    call rax
    pop r12
    pop rbx
    leave
    ret

.smallint:
    ; SmallInt: delegate to int_repr
    mov rdi, rbx
    RET_TAG_SMALLINT
    call int_repr
    pop r12
    pop rbx
    leave
    ret

.float_tag:
    ; Create temp PyFloatObject on stack, call float_repr (float's tp_str)
    sub rsp, 32
    mov qword [rsp], 1            ; ob_refcnt
    lea rax, [rel float_type]
    mov [rsp + 8], rax             ; ob_type
    mov [rsp + 16], rbx            ; value (double bits from payload)
    mov rdi, rsp
    call float_repr
    add rsp, 32
    pop r12
    pop rbx
    leave
    ret

.none_tag:
    call none_repr
    pop r12
    pop rbx
    leave
    ret

.bool_tag:
    test rbx, rbx
    jz .bool_false_str
    lea rdi, [rel bool_true]
    call bool_repr                 ; bool tp_str = bool_repr
    pop r12
    pop rbx
    leave
    ret
.bool_false_str:
    lea rdi, [rel bool_false]
    call bool_repr
    pop r12
    pop rbx
    leave
    ret

.fallback:
    mov rdi, rbx
    mov rsi, r12
    call obj_repr
    pop r12
    pop rbx
    leave
    ret
END_FUNC obj_str

; obj_hash(rdi=payload, rsi=tag) -> int64
; Dispatches on tag. SmallInt → raw value. TAG_PTR → tp_hash.
DEF_FUNC obj_hash

    cmp esi, TAG_SMALLINT
    je .smallint_hash

    ; TAG_PTR path
    test rdi, rdi
    jz .default_hash

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .default_hash
    mov rax, [rax + PyTypeObject.tp_hash]
    test rax, rax
    jz .default_hash

    ; tail-call tp_hash(obj)
    leave
    jmp rax

.smallint_hash:
    ; Hash of SmallInt = raw value (avoid -1)
    mov rax, rdi
    cmp rax, -1
    jne .hash_done
    mov rax, -2
.hash_done:
    leave
    ret

.default_hash:
    ; Default: hash is the object address
    mov rax, rdi
    leave
    ret
END_FUNC obj_hash

; obj_is_true(rdi=payload, rsi=tag) -> int (0 or 1)
; Dispatches on tag. SmallInt → value != 0. TAG_PTR → type-based truthiness.
DEF_FUNC_BARE obj_is_true
    cmp esi, TAG_SMALLINT
    je .smallint

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; None is false
    lea rax, [rel none_singleton]
    cmp rbx, rax
    je .false

    ; bool False is false
    lea rax, [rel bool_false]
    cmp rbx, rax
    je .false

    ; Check for nb_bool in type's number methods
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .true
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .check_seq_len
    mov rax, [rax + PyNumberMethods.nb_bool]
    test rax, rax
    jz .check_seq_len
    mov rdi, rbx
    call rax
    pop rbx
    pop rbp
    ret

.check_seq_len:
    ; Check for sq_length in type's sequence methods
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .check_map_len
    mov rax, [rax + PySequenceMethods.sq_length]
    test rax, rax
    jz .check_map_len
    mov rdi, rbx
    call rax
    test rax, rax
    jnz .true
    jmp .false

.check_map_len:
    ; Check for mp_length in type's mapping methods
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .check_dunder_bool
    mov rax, [rax + PyMappingMethods.mp_length]
    test rax, rax
    jz .check_dunder_bool
    mov rdi, rbx
    call rax
    test rax, rax
    jnz .true
    jmp .false

.check_dunder_bool:
    ; Try __bool__ dunder on heaptype
    mov rax, [rbx + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .true                ; default: objects are truthy

    extern dunder_bool
    extern dunder_call_1
    mov rdi, rbx
    lea rsi, [rel dunder_bool]
    call dunder_call_1
    test edx, edx              ; TAG_NULL = not found
    jz .check_dunder_len

    ; __bool__ returned a result — convert to int (check if it's True/False)
    push rdx                   ; save tag
    push rax                   ; save payload
    extern obj_is_true
    mov rdi, rax
    mov rsi, rdx
    call obj_is_true
    mov ecx, eax
    pop rdi                    ; payload
    pop rsi                    ; tag
    DECREF_VAL rdi, rsi
    mov eax, ecx
    pop rbx
    pop rbp
    ret

.check_dunder_len:
    ; Try __len__ dunder
    extern dunder_len
    mov rdi, rbx
    lea rsi, [rel dunder_len]
    call dunder_call_1
    test edx, edx              ; TAG_NULL = not found
    jz .true                ; no __len__ → truthy by default

    ; __len__ returned a result — truthy if > 0
    push rdx                   ; save tag
    push rax                   ; save payload
    mov rdi, rax
    mov rsi, rdx
    call obj_is_true
    mov ecx, eax
    pop rdi                    ; payload
    pop rsi                    ; tag
    DECREF_VAL rdi, rsi
    mov eax, ecx
    pop rbx
    pop rbp
    ret

.false:
    xor eax, eax
    pop rbx
    pop rbp
    ret

.true:
    mov eax, 1
    pop rbx
    pop rbp
    ret

.smallint:
    ; SmallInt is true iff raw value != 0
    test rdi, rdi
    setnz al
    movzx eax, al
    ret
END_FUNC obj_is_true

; obj_print(PyObject *obj)
; Print an object's string representation to stdout followed by newline
DEF_FUNC obj_print
    push rbx
    mov rbx, rdi

    ; Get string representation via obj_str(payload, tag)
    mov esi, TAG_PTR
    call obj_str
    test rax, rax
    jz .print_null

    mov rbx, rax            ; rbx = str obj

    ; sys_write(1, str_data, ob_size)
    mov edi, 1
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, [rbx + PyStrObject.ob_size]
    call sys_write

    ; sys_write(1, "\n", 1)
    mov edi, 1
    lea rsi, [rel obj_print_newline]
    mov edx, 1
    call sys_write

    pop rbx
    leave
    ret

.print_null:
    ; sys_write(1, "<NULL>\n", 7)
    mov edi, 1
    lea rsi, [rel obj_print_null_str]
    mov edx, 7
    call sys_write

    pop rbx
    leave
    ret
END_FUNC obj_print

;; ============================================================================
;; type_repr(PyObject *type_obj) -> PyStrObject*
;; Formats "<class 'name'>" for a type object.
;; ============================================================================
DEF_FUNC type_repr
    push rbx
    sub rsp, 72                ; 64 bytes buffer + 8 alignment

    ; rdi = type object ptr
    mov rax, [rdi + PyTypeObject.tp_name]  ; C string pointer
    test rax, rax
    jz .type_repr_unknown

    ; Build "<class 'NAME'>" in stack buffer
    mov rbx, rax               ; rbx = tp_name C string

    ; Calculate name length
    xor ecx, ecx
.name_len:
    cmp byte [rbx + rcx], 0
    je .name_len_done
    inc ecx
    jmp .name_len
.name_len_done:
    ; ecx = name len; buffer at [rsp]
    lea rdi, [rsp]

    ; Write "<class '"
    mov byte [rdi], '<'
    mov byte [rdi+1], 'c'
    mov byte [rdi+2], 'l'
    mov byte [rdi+3], 'a'
    mov byte [rdi+4], 's'
    mov byte [rdi+5], 's'
    mov byte [rdi+6], ' '
    mov byte [rdi+7], 0x27     ; single quote

    ; Copy name bytes
    xor r8d, r8d
.copy_name:
    cmp r8d, ecx
    je .copy_name_done
    mov al, [rbx + r8]
    mov [rdi + 8 + r8], al
    inc r8d
    jmp .copy_name
.copy_name_done:
    ; Append "'>" and null terminator
    lea eax, [r8d + 8]
    mov byte [rdi + rax], 0x27  ; single quote
    mov byte [rdi + rax + 1], '>'
    mov byte [rdi + rax + 2], 0

    ; Create string from buffer
    ; rdi already = [rsp] = buffer start
    call str_from_cstr

    add rsp, 72
    pop rbx
    leave
    ret

.type_repr_unknown:
    lea rdi, [rel type_repr_unknown_str]
    call str_from_cstr
    add rsp, 72
    pop rbx
    leave
    ret
END_FUNC type_repr

section .rodata
obj_print_newline: db 10
obj_print_null_str: db "<NULL>", 10
type_repr_unknown_str: db "<class '?'>", 0
type_type_name: db "type", 0

section .data
align 8
global type_type
type_type:
    dq 1                      ; ob_refcnt (immortal)
    dq type_type              ; ob_type (self-referential)
    dq type_type_name         ; tp_name
    dq TYPE_OBJECT_SIZE       ; tp_basicsize
    dq 0                      ; tp_dealloc
    dq type_repr              ; tp_repr
    dq type_repr              ; tp_str
    dq 0                      ; tp_hash
    dq type_call              ; tp_call — calling a type creates instances
    dq type_getattr           ; tp_getattr — __name__, tp_dict lookups
    dq type_setattr           ; tp_setattr
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
    dq 0                      ; tp_flags
    dq 0                      ; tp_bases
