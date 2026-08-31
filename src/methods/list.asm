; methods/list.asm - list and tuple methods
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

; External functions
extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_memcpy
extern ap_memmove
extern ap_strcmp
extern obj_decref
extern list_new
extern list_append
extern list_type
extern tuple_type
extern none_singleton
extern bool_true
extern bool_false
extern int_from_i64
extern eval_exception_unwind
extern obj_richcompare_bool
extern int_to_i64
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_IndexError_type
extern int_type
extern list_sorting_error
extern obj_dealloc

; Set entry layout constants (must match set.asm)

; --- moved to a sibling file by the split ---

section .text

;; ############################################################################
;;                         LIST METHODS
;; ############################################################################

;; ============================================================================
;; list_method_append(args, nargs) -> None
;; args[0]=self, args[1]=item
;; ============================================================================
DEF_FUNC list_method_append

    mov rax, [rdi]          ; self (list)
    ; Check if list is being sorted (ob_item == NULL)
    cmp qword [rax + PyListObject.ob_item], 0
    je list_sorting_error
    mov rsi, [rdi + 8]     ; item payload
    V_UNPACK rsi, rdx       ; args[1]
    mov rdi, rax
    V_PACK rsi, rdx         ; list_append takes a Value
    call list_append

    RET_NONE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_method_append

;; ============================================================================
;; list_method_pop(args, nargs) -> removed item
;; args[0]=self, optionally args[1]=index (default: last)
;; ============================================================================
DEF_FUNC list_method_pop
    push rbx
    push r12
    push r13

    mov rax, rdi            ; rax = args ptr
    mov rbx, [rax]          ; self (list)
    ; Check if list is being sorted (ob_item == NULL)
    cmp qword [rbx + PyListObject.ob_item], 0
    je list_sorting_error
    mov r12, rsi            ; nargs

    ; Get index
    cmp r12, 2
    jge .pop_idx
    ; Default: pop last element
    mov r13, [rbx + PyListObject.ob_size]
    dec r13                 ; index = size - 1
    jmp .pop_do

.pop_idx:
    mov rdi, [rax + 8]    ; args[1]
    V_UNPACK rdi, rdx       ; args[1]
    call int_to_i64
    mov r13, rax

    ; Handle negative index
    test r13, r13
    jns .pop_do
    add r13, [rbx + PyListObject.ob_size]

.pop_do:
    ; Bounds check
    cmp r13, 0
    jl .pop_error
    cmp r13, [rbx + PyListObject.ob_size]
    jge .pop_error

    ; Get the item (it already has refs from being in the list)
    mov rax, [rbx + PyListObject.ob_item]
    mov r12, [rax + r13 * 8]        ; payload to return
    V_UNPACK r12, rdx
    push rdx                        ; save tag on stack
    ; Don't DECREF since we're transferring ownership to caller

    ; Shift payloads down: memmove(&payloads[idx], &payloads[idx+1], (size-1-idx)*8)
    mov rax, [rbx + PyListObject.ob_item]
    lea rdi, [rax + r13 * 8]        ; dst = &payloads[idx]
    lea rsi, [rdi + 8]              ; src = &payloads[idx+1]
    mov rdx, [rbx + PyListObject.ob_size]
    sub rdx, r13
    dec rdx                         ; count = size - idx - 1
    shl rdx, 3                      ; bytes = count * 8
    jz .pop_shrink                  ; nothing to shift if popping last
    call ap_memmove

.pop_shrink:
    dec qword [rbx + PyListObject.ob_size]

    ; Return item (ownership transferred, no extra INCREF needed)
    mov rax, r12
    pop rdx                  ; item tag
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pop_error:
    RAISE exc_IndexError_type, "pop index out of range"
END_FUNC list_method_pop

;; ============================================================================
;; list_method_insert(args, nargs) -> None
;; args[0]=self, args[1]=index, args[2]=item
;; ============================================================================
DEF_FUNC list_method_insert
    push rbx
    push r12
    push r13
    push r14

    mov rax, rdi            ; args (16-byte stride)
    mov rbx, [rax]          ; self = args[0]
    ; Check if list is being sorted (ob_item == NULL)
    cmp qword [rbx + PyListObject.ob_item], 0
    je list_sorting_error
    push rax

    ; Get index
    mov rdi, [rax + 8]     ; args[1] payload (16B stride)
    V_UNPACK rdi, rdx       ; args[1]
    call int_to_i64
    mov r12, rax            ; index

    pop rax
    mov r13, [rax + 16]     ; item = args[2] payload (16B stride)
    V_UNPACK r13, r14       ; args[2]

    ; Clamp index to [0, size]
    test r12, r12
    jns .ins_pos
    add r12, [rbx + PyListObject.ob_size]
    test r12, r12
    jns .ins_pos
    xor r12d, r12d
.ins_pos:
    cmp r12, [rbx + PyListObject.ob_size]
    jle .ins_ok
    mov r12, [rbx + PyListObject.ob_size]
.ins_ok:

    ; First append a dummy to grow the list if needed
    ; (reuse list_append logic for growth, then shift)
    ; Actually, let's just handle growth manually:
    ; Check if size == allocated
    mov rax, [rbx + PyListObject.ob_size]
    cmp rax, [rbx + PyListObject.allocated]
    jl .ins_no_grow
    ; Double capacity
    mov rdi, [rbx + PyListObject.allocated]
    shl rdi, 1
    mov [rbx + PyListObject.allocated], rdi
    mov rdi, [rbx + PyListObject.ob_item]
    mov rsi, [rbx + PyListObject.allocated]
    shl rsi, 3              ; new_cap * 8
    call ap_realloc
    mov [rbx + PyListObject.ob_item], rax
.ins_no_grow:

    ; Shift items up: memmove(&items[idx+1], &items[idx], (size-idx)*8)
    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r12
    shl rcx, 3              ; idx * 8
    lea rsi, [rax + rcx]    ; src = &items[idx]
    lea rdi, [rsi + 8]      ; dst = &items[idx+1]
    mov rdx, [rbx + PyListObject.ob_size]
    sub rdx, r12            ; count = size - idx
    shl rdx, 3              ; bytes = count * 8
    jz .ins_place           ; nothing to shift if inserting at end
    call ap_memmove

.ins_place:
    ; Place item at index
    mov rax, [rbx + PyListObject.ob_item]
    INCREF_VAL r13, r14
    V_PACK r13, r14
    mov [rax + r12 * 8], r13
    inc qword [rbx + PyListObject.ob_size]

    RET_NONE
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_method_insert

;; ============================================================================
;; list_method_reverse(args, nargs) -> None
;; args[0]=self, reverse in place
;; ============================================================================
DEF_FUNC list_method_reverse
    push rbx
    mov rax, [rdi]          ; self
    ; Check if list is being sorted (ob_item == NULL)
    cmp qword [rax + PyListObject.ob_item], 0
    je list_sorting_error
    mov rcx, [rax + PyListObject.ob_size]
    test rcx, rcx
    jz .rev_done

    mov rdi, [rax + PyListObject.ob_item]       ; payloads
    xor esi, esi            ; lo = 0
    dec rcx                 ; hi = size - 1
.rev_loop:
    cmp rsi, rcx
    jge .rev_done
    ; Swap payloads
    mov r8, [rdi + rsi * 8]      ; lo payload
    mov r10, [rdi + rcx * 8]     ; hi payload
    mov [rdi + rsi * 8], r10
    mov [rdi + rcx * 8], r8
    inc rsi
    dec rcx
    jmp .rev_loop

.rev_done:
    RET_NONE
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_method_reverse

;; ============================================================================
;; list_method_sort(args, nargs) -> None
;; Stable bottom-up merge sort with key= and reverse= support
;; args[0]=self
;; ============================================================================
LS_LIST    equ 8      ; list object ptr
LS_N       equ 16     ; element count
LS_SRC     equ 24     ; current source array (items or temp)
LS_DST     equ 32     ; current dest array (temp or items)
LS_TEMP    equ 40     ; temp array (for freeing)
LS_REV     equ 48     ; reverse flag (0=normal, 1=reverse)
LS_KEY     equ 56     ; key function payload (0=none)
LS_KSRC    equ 64     ; keys source array (swapped during sort)
LS_KDST    equ 72     ; keys dest array (swapped during sort)
LS_KTEMP   equ 80     ; keys temp array (2nd alloc, for freeing)
LS_KORIG   equ 168    ; original keys array (1st alloc, for freeing)
LS_WIDTH   equ 88     ; current merge width
LS_OUTI    equ 96     ; outer loop index
LS_MI      equ 104    ; merge: left index
LS_MJ      equ 112    ; merge: right index (j)
LS_MMID    equ 120    ; merge: mid boundary
LS_MREND   equ 128    ; merge: right end boundary
LS_MK      equ 136    ; merge: dest index (k)
LS_SAVED_ITEMS equ 144  ; saved fat items buffer
LS_SAVED_SIZE  equ 152  ; saved ob_size before sort
LS_SAVED_PAYLOADS equ 176 ; saved payload array ptr
LS_SAVED_TAGS     equ 184 ; saved tag array ptr
LS_FRAME   equ 192     ; includes saved payload/tag pointers
DEF_FUNC list_method_sort, LS_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    ; sort() takes no positional argument beyond self; only the keywords key
    ; and reverse.  nargs was never compared against anything, so
    ; l.sort(42, 42) was accepted.  The check has to be here rather than in
    ; add_method_to_dict_checked, which counts keyword values in nargs and
    ; would therefore reject l.sort(key=f).
    mov rax, rsi                ; nargs, self included
    mov rcx, [rel kw_names_pending]
    test rcx, rcx
    jz .ls_have_npos
    sub rax, [rcx + PyTupleObject.ob_size]
.ls_have_npos:
    cmp rax, 1
    jg .ls_too_many

    mov rbx, [rdi]              ; self (list)
    mov r12, [rbx + PyListObject.ob_size]
    mov [rbp - LS_LIST], rbx
    mov [rbp - LS_N], r12
    mov qword [rbp - LS_REV], 0
    mov qword [rbp - LS_KEY], 0
    mov qword [rbp - LS_KSRC], 0
    mov qword [rbp - LS_KDST], 0
    mov qword [rbp - LS_KTEMP], 0
    mov qword [rbp - LS_KORIG], 0

    ; --- Parse keyword arguments ---
    extern kw_names_pending
    extern ap_strcmp
    mov rax, [rel kw_names_pending]
    test rax, rax
    jz .sort_no_kw

    push rdi                       ; save args ptr
    push rsi                       ; save nargs

    mov rcx, [rax + PyTupleObject.ob_size]  ; n_kw
    mov r8, rsi
    sub r8, rcx                    ; r8 = n_pos
    xor r9d, r9d                   ; kw index

.sort_kw_loop:
    cmp r9, rcx
    jge .sort_kw_done

    ; Get kwarg name string ptr from kw_names tuple
    mov r10, r9
    mov rbx, [rax + PyTupleObject.ob_item]
    mov r10, [rbx + r10 * 8]

    ; Kwarg value offset: (n_pos + kw_idx) * 8
    mov r11, r8
    add r11, r9
    shl r11, 3

    ; --- Check "reverse" ---
    push rax
    push rcx
    push r8
    push r9
    push r11
    push rdi
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "reverse"
    call ap_strcmp
    mov r10d, eax
    pop rdi
    pop r11
    pop r9
    pop r8
    pop rcx
    pop rax
    test r10d, r10d
    jnz .sort_kw_not_reverse

    ; Extract reverse value
    mov r10, [rdi + r11]           ; the value Value
    V_UNPACK r10, r13
    cmp r13d, TAG_SMALLINT
    je .sort_rev_int
    ; TAG_PTR: check if bool_true
    lea r13, [rel bool_true]
    cmp r10, r13
    sete r10b
    movzx r10d, r10b
    mov [rbp - LS_REV], r10
    jmp .sort_kw_next
.sort_rev_bool:
    mov [rbp - LS_REV], r10       ; 0 or 1
    jmp .sort_kw_next
.sort_rev_int:
    test r10, r10
    setnz r10b
    movzx r10d, r10b
    mov [rbp - LS_REV], r10
    jmp .sort_kw_next

.sort_kw_not_reverse:
    ; --- Check "key" ---
    ; r10 was clobbered by strcmp result above, reload kwarg name
    mov r10, r9
    mov rbx, [rax + PyTupleObject.ob_item]
    mov r10, [rbx + r10 * 8]
    push rax
    push rcx
    push r8
    push r9
    push r11
    push rdi
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "key"
    call ap_strcmp
    mov r10d, eax
    pop rdi
    pop r11
    pop r9
    pop r8
    pop rcx
    pop rax
    test r10d, r10d
    jnz .sort_kw_next              ; not "key" either, skip

    ; Extract key function value
    mov r10, [rdi + r11]           ; the key Value (a callable, so a pointer)
    ; key=None means no key function
    lea r14, [rel none_singleton]
    cmp r10, r14
    je .sort_kw_next
    mov [rbp - LS_KEY], r10
    jmp .sort_kw_next

.sort_kw_next:
    inc r9
    jmp .sort_kw_loop

.sort_kw_done:
    pop rsi
    pop rdi
    mov qword [rel kw_names_pending], 0
    mov rbx, [rbp - LS_LIST]           ; reload list (clobbered by kw parsing)

.sort_no_kw:
    ; Initialize saved state (needed for sort_done even on early exit)
    mov qword [rbp - LS_SAVED_ITEMS], 0
    mov qword [rbp - LS_SAVED_SIZE], 0
    mov qword [rbp - LS_SAVED_PAYLOADS], 0
    mov qword [rbp - LS_SAVED_TAGS], 0

    ; If n < 2, nothing to sort
    cmp r12, 2
    jl .sort_trivial_done

    ; Save list state and empty it during sort (mutation detection)
    mov rax, [rbx + PyListObject.ob_item]
    mov [rbp - LS_SAVED_PAYLOADS], rax
    mov [rbp - LS_SAVED_TAGS], rax
    mov [rbp - LS_SAVED_SIZE], r12

    ; Allocate fat buffer (n * 16) and copy payload+tag into it
    mov rdi, r12
    shl rdi, 4
    extern ap_malloc
    call ap_malloc
    mov [rbp - LS_SAVED_ITEMS], rax
    mov rdi, rax                          ; dest fat buffer
    mov rsi, [rbp - LS_SAVED_PAYLOADS]    ; src payloads
    mov rdx, [rbp - LS_SAVED_TAGS]        ; src tags
    xor rcx, rcx
.sort_copy_items:
    cmp rcx, r12
    jge .sort_copy_items_done
    mov r8, [rsi + rcx * 8]               ; payload
    V_UNPACK r8, r9
    mov r10, rcx
    shl r10, 4
    mov [rdi + r10], r8
    mov [rdi + r10 + 8], r9
    inc rcx
    jmp .sort_copy_items
.sort_copy_items_done:

    mov qword [rbx + PyListObject.ob_item], 0
    mov qword [rbx + PyListObject.ob_size], 0

    ; --- Pre-compute keys if key= provided ---
    cmp qword [rbp - LS_KEY], 0
    jz .sort_alloc_temp

    ; Allocate keys array: n * 16 bytes
    mov rdi, r12
    shl rdi, 4
    extern ap_malloc
    call ap_malloc
    mov [rbp - LS_KSRC], rax
    mov [rbp - LS_KORIG], rax      ; save original allocation for freeing
    mov r14, rax                   ; r14 = keys array

    ; Compute key(items[i]) for each i
    xor r15d, r15d                 ; i = 0
.sort_keys_loop:
    cmp r15, [rbp - LS_N]
    jge .sort_keys_done

    ; Get items[i] and push as single arg on stack (use saved items, list is empty during sort)
    mov rax, [rbp - LS_SAVED_ITEMS]
    mov rcx, r15
    shl rcx, 4
    mov rdi, [rax + rcx]          ; item payload
    mov rsi, [rax + rcx + 8]      ; item tag
    V_PACK rdi, rsi
    sub rsp, 16                    ; one Value; 16 keeps rsp aligned
    mov [rsp], rdi                 ; args[0] = item

    ; Get key function's tp_call
    mov rdi, [rbp - LS_KEY]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .sort_key_try_meta

    ; tp_call(rdi=callable, rsi=args, rdx=nargs)
    mov rsi, rsp                   ; args ptr → &[item]
    mov edx, 1                     ; nargs = 1
    call rax
    V_UNPACK rax, rdx           ; tp_call returns a Value
    jmp .sort_key_store

.sort_key_try_meta:
    ; tp_call NULL — check if heaptype instance with __call__
    mov rdi, [rbp - LS_KEY]
    mov rax, [rdi + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .sort_key_meta_builtin

    ; Heaptype instance: use __call__(key, item) via dunder_call_2
    mov rsi, [rsp]                 ; the item Value
    V_UNPACK rsi, rcx
    extern dunder_call
    lea rdx, [rel dunder_call]
    extern dunder_call_2
    call dunder_call_2
    V_UNPACK rax, rdx           ; returns a Value
    jmp .sort_key_store

.sort_key_meta_builtin:
    ; Built-in type: try metatype's tp_call (e.g., for type objects used as key)
    mov rdi, [rbp - LS_KEY]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyObject.ob_type]  ; metatype
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .sort_key_error
    mov rsi, rsp
    mov edx, 1
    call rax
    V_UNPACK rax, rdx           ; tp_call returns a Value

.sort_key_store:
    add rsp, 16                    ; pop item from stack
    ; rax = key result payload, edx = key result tag
    test edx, edx
    jz .sort_cleanup_keys          ; NULL return → error (item already popped)
    ; Store key in keys[i]
    mov rcx, r15
    shl rcx, 4
    mov [r14 + rcx], rax
    mov [r14 + rcx + 8], rdx
    inc r15
    jmp .sort_keys_loop

.sort_key_error:
    add rsp, 16                    ; pop item if still on stack
    ; DECREF any keys computed so far, free keys array
    jmp .sort_cleanup_keys

.sort_keys_done:
    ; Allocate keys temp array
    mov rdi, [rbp - LS_N]
    shl rdi, 4
    call ap_malloc
    mov [rbp - LS_KTEMP], rax
    mov [rbp - LS_KDST], rax

.sort_alloc_temp:
    ; Allocate temp array: n * 16 bytes
    mov rdi, [rbp - LS_N]
    shl rdi, 4
    call ap_malloc
    mov [rbp - LS_TEMP], rax
    mov [rbp - LS_DST], rax

    ; Source = saved list items array (list is empty during sort)
    mov rax, [rbp - LS_SAVED_ITEMS]
    mov [rbp - LS_SRC], rax

    ; =========================================================================
    ; Bottom-up merge sort: for width=1,2,4,...; merge adjacent pairs
    ; =========================================================================
    mov qword [rbp - LS_WIDTH], 1

.sort_width_loop:
    mov rax, [rbp - LS_WIDTH]
    cmp rax, [rbp - LS_N]
    jge .sort_width_done

    ; For i = 0; i < n; i += 2*width
    mov qword [rbp - LS_OUTI], 0

.sort_outer_loop:
    mov rax, [rbp - LS_OUTI]
    cmp rax, [rbp - LS_N]
    jge .sort_outer_done

    ; left = i
    mov [rbp - LS_MI], rax
    ; mid = min(i + width, n)
    add rax, [rbp - LS_WIDTH]
    cmp rax, [rbp - LS_N]
    jle .sort_mid_ok
    mov rax, [rbp - LS_N]
.sort_mid_ok:
    mov [rbp - LS_MMID], rax
    ; right_end = min(i + 2*width, n)
    mov rax, [rbp - LS_OUTI]
    mov rcx, [rbp - LS_WIDTH]
    lea rax, [rax + rcx*2]
    cmp rax, [rbp - LS_N]
    jle .sort_right_ok
    mov rax, [rbp - LS_N]
.sort_right_ok:
    mov [rbp - LS_MREND], rax
    ; k = i (dest index starts at i)
    mov rax, [rbp - LS_OUTI]
    mov [rbp - LS_MK], rax
    ; j = mid
    mov rax, [rbp - LS_MMID]
    mov [rbp - LS_MJ], rax

    ; =====================================================================
    ; Merge loop: merge src[left..mid) and src[mid..right_end) into dst
    ; =====================================================================
.merge_loop:
    ; Check if left run exhausted
    mov rax, [rbp - LS_MI]
    cmp rax, [rbp - LS_MMID]
    jge .merge_copy_right

    ; Check if right run exhausted
    mov rax, [rbp - LS_MJ]
    cmp rax, [rbp - LS_MREND]
    jge .merge_copy_left

    ; Load elements for comparison (use keys if available, else items)
    ; Python's sort uses right < left (PY_LT on right), not left > right.
    ; This ensures __lt__ works (more commonly defined than __gt__).
    mov rax, [rbp - LS_KSRC]
    test rax, rax
    jnz .merge_have_cmp_arr
    mov rax, [rbp - LS_SRC]
.merge_have_cmp_arr:
    ; For comparison: we do right < left (ascending) or right > left (descending)
    ; Load right element first (will be "self" in dunder call)
    mov rcx, [rbp - LS_MJ]
    shl rcx, 4
    mov rdi, [rax + rcx]          ; right payload (self for comparison)
    mov r8, [rax + rcx + 8]       ; right tag (full 64-bit)
    ; Load left element (will be "other" in dunder call)
    mov rcx, [rbp - LS_MI]
    shl rcx, 4
    mov rsi, [rax + rcx]          ; left payload (other for comparison)
    mov r9, [rax + rcx + 8]       ; left tag (full 64-bit)

    ; Type dispatch on right element for tp_richcompare
    ; Float coercion: if either operand is TAG_FLOAT, use float_compare
    cmp r8d, TAG_FLOAT
    je .merge_use_float
    cmp r9d, TAG_FLOAT
    je .merge_use_float

    cmp r8d, TAG_SMALLINT
    je .merge_si_type
    test r8d, TAG_RC_BIT
    jz .merge_take_left            ; TAG_NONE/TAG_BOOL: take left (stable)
    mov rax, [rdi + PyObject.ob_type]
    jmp .merge_have_type
.merge_si_type:
    lea rax, [rel int_type]
.merge_have_type:
    mov r10, rax                   ; save type ptr for dunder fallback
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .merge_try_dunder

    ; tp_richcompare(rdi=right, rsi=left, edx=op, rcx=right_tag, r8=left_tag)
    ; Comparing: right < left (ascending) or right > left (descending)
    mov rcx, r8                    ; right_tag
    mov r8, r9                     ; left_tag
    cmp qword [rbp - LS_REV], 0
    je .merge_use_lt
    mov edx, PY_GT                 ; reversed: right > left
    jmp .merge_do_cmp
.merge_use_lt:
    mov edx, PY_LT                 ; normal: right < left
.merge_do_cmp:
    V_PACK rdi, rcx             ; left  -> Value
    V_PACK rsi, r8              ; right -> Value
    call rax
    V_UNPACK rax, rdx           ; tp_richcompare returns a Value
    jmp .merge_check_result

.merge_use_float:
    ; float_compare(right, left, op, right_tag, left_tag)
    extern float_compare
    mov rcx, r8                    ; right_tag (full 64-bit)
    mov r8, r9                     ; left_tag (full 64-bit)
    cmp qword [rbp - LS_REV], 0
    je .merge_float_lt
    mov edx, PY_GT                 ; reversed: right > left
    jmp .merge_float_cmp
.merge_float_lt:
    mov edx, PY_LT                 ; normal: right < left
.merge_float_cmp:
    V_PACK rdi, rcx             ; left  -> Value
    V_PACK rsi, r8              ; right -> Value
    call float_compare
    V_UNPACK rax, rdx           ; float_compare returns a Value
    jmp .merge_check_result

.merge_try_dunder:
    ; No tp_richcompare — try dunder on heaptype (right side, the "self")
    mov rdx, [r10 + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .merge_take_left            ; not heaptype, give up

    ; Reload right/left from comparison array
    ; right = self, left = other (for right < left comparison)
    mov rax, [rbp - LS_KSRC]
    test rax, rax
    jnz .merge_dunder_have_arr
    mov rax, [rbp - LS_SRC]
.merge_dunder_have_arr:
    ; right element = self (index MJ)
    mov rcx, [rbp - LS_MJ]
    shl rcx, 4
    mov rdi, [rax + rcx]          ; right payload (self)
    mov r11d, [rax + rcx + 8]     ; right_tag (save temporarily)
    ; left element = other (index MI)
    mov rcx, [rbp - LS_MI]
    shl rcx, 4
    mov rsi, [rax + rcx]          ; left payload (other)
    mov ecx, [rax + rcx + 8]      ; left_tag (32-bit for dunder_call_2)

    ; dunder_call_2(rdi=self, rsi=other, rdx=name, ecx=other_tag)
    ; self=right, other=left: comparing right < left (ascending)
    cmp qword [rbp - LS_REV], 0
    je .merge_dunder_lt
    extern dunder_gt
    lea rdx, [rel dunder_gt]       ; reversed: right > left
    jmp .merge_dunder_call
.merge_dunder_lt:
    extern dunder_lt
    lea rdx, [rel dunder_lt]       ; normal: right < left
.merge_dunder_call:
    extern dunder_call_2
    call dunder_call_2
    V_UNPACK rax, rdx           ; returns a Value
    ; fall through to check_result

.merge_check_result:
    ; (rax=payload, edx=tag) — check if comparison is true
    test edx, edx
    jz .merge_cmp_null             ; NULL → check for error or unorderable types
    ; TAG_PTR: check for NotImplemented, then check bool_true
    extern notimpl_singleton
    lea rcx, [rel notimpl_singleton]
    cmp rax, rcx
    je .merge_cmp_type_error       ; NotImplemented → raise TypeError
    push rax                       ; save for DECREF
    lea rcx, [rel bool_true]
    cmp rax, rcx
    sete cl
    movzx ecx, cl                  ; ecx = 1 if true (take right)
    mov rdi, rax
    push rcx
    call obj_decref
    pop rcx
    add rsp, 8                     ; discard saved ptr
    test ecx, ecx
    jnz .merge_take_right
    jmp .merge_take_left

.merge_cmp_null:
    ; NULL return — check current_exception
    mov rax, [rel current_exception]
    test rax, rax
    jnz .sort_free_temp            ; real exception → cleanup and propagate
    ; No exception → unorderable types, raise TypeError
.merge_cmp_type_error:
    ; IMPORTANT: raise_exception does not return (non-local jump to eval_exception_unwind)
    ; Must free temp buffer and restore list state BEFORE raising.
    
    ; Free temp buffer
    mov rdi, [rbp - LS_TEMP]
    call ap_free
    
    ; If keys were used, DECREF keys and free arrays
    cmp qword [rbp - LS_KEY], 0
    jz .mcte_no_keys
    mov r14, [rbp - LS_KSRC]
    test r14, r14
    jz .mcte_free_ktemp
    xor r15d, r15d
.mcte_decref_keys:
    cmp r15, [rbp - LS_N]
    jge .mcte_free_keys
    mov rcx, r15
    shl rcx, 4
    mov rdi, [r14 + rcx]
    mov esi, [r14 + rcx + 8]
    DECREF_VAL rdi, rsi
    inc r15
    jmp .mcte_decref_keys
.mcte_free_keys:
    mov rdi, [rbp - LS_KORIG]
    call ap_free
    mov rdi, [rbp - LS_KTEMP]
    call ap_free
    jmp .mcte_restore_list
.mcte_free_ktemp:
    mov rdi, [rbp - LS_KTEMP]
    test rdi, rdi
    jz .mcte_restore_list
    call ap_free
.mcte_no_keys:
.mcte_restore_list:
    ; Restore list items (list is empty during sort)
    mov rbx, [rbp - LS_LIST]
    mov rax, [rbx + PyListObject.ob_item]
    test rax, rax
    jnz .mcte_already_restored     ; someone else restored
    mov rax, [rbp - LS_SAVED_PAYLOADS]
    mov [rbx + PyListObject.ob_item], rax
    mov rax, [rbp - LS_SAVED_TAGS]
    mov rax, [rbp - LS_SAVED_SIZE]
    mov [rbx + PyListObject.ob_size], rax
.mcte_already_restored:
    ; Now raise TypeError
    extern exc_TypeError_type
    extern raise_exception
    RAISE exc_TypeError_type, "'<' not supported between instances"
    ; raise_exception does not return
.merge_bool_result:
    ; eax = 0 (false) or 1 (true)
    test eax, eax
    jnz .merge_take_right
    ; fall through: take left (equal → left wins for stability)

.merge_take_left:
    ; Copy src[left] to dst[k] (16 bytes)
    mov rax, [rbp - LS_SRC]
    mov rcx, [rbp - LS_MI]
    shl rcx, 4
    mov r8, [rax + rcx]
    mov r9, [rax + rcx + 8]
    mov rax, [rbp - LS_DST]
    mov rcx, [rbp - LS_MK]
    shl rcx, 4
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
    ; If keys, copy ksrc[left] to kdst[k]
    mov rax, [rbp - LS_KSRC]
    test rax, rax
    jz .merge_left_nokeys
    mov rcx, [rbp - LS_MI]
    shl rcx, 4
    mov r8, [rax + rcx]
    mov r9, [rax + rcx + 8]
    mov rax, [rbp - LS_KDST]
    mov rcx, [rbp - LS_MK]
    shl rcx, 4
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
.merge_left_nokeys:
    inc qword [rbp - LS_MI]
    inc qword [rbp - LS_MK]
    jmp .merge_loop

.merge_take_right:
    ; Copy src[j] to dst[k] (16 bytes)
    mov rax, [rbp - LS_SRC]
    mov rcx, [rbp - LS_MJ]
    shl rcx, 4
    mov r8, [rax + rcx]
    mov r9, [rax + rcx + 8]
    mov rax, [rbp - LS_DST]
    mov rcx, [rbp - LS_MK]
    shl rcx, 4
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
    ; If keys, copy ksrc[j] to kdst[k]
    mov rax, [rbp - LS_KSRC]
    test rax, rax
    jz .merge_right_nokeys
    mov rcx, [rbp - LS_MJ]
    shl rcx, 4
    mov r8, [rax + rcx]
    mov r9, [rax + rcx + 8]
    mov rax, [rbp - LS_KDST]
    mov rcx, [rbp - LS_MK]
    shl rcx, 4
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
.merge_right_nokeys:
    inc qword [rbp - LS_MJ]
    inc qword [rbp - LS_MK]
    jmp .merge_loop

.merge_copy_right:
    ; Left exhausted — copy remaining right elements to dst
    mov rax, [rbp - LS_MJ]
    cmp rax, [rbp - LS_MREND]
    jge .merge_done
    mov rax, [rbp - LS_SRC]
    mov rcx, [rbp - LS_MJ]
    shl rcx, 4
    mov r8, [rax + rcx]
    mov r9, [rax + rcx + 8]
    mov rax, [rbp - LS_DST]
    mov rcx, [rbp - LS_MK]
    shl rcx, 4
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
    ; Keys
    mov rax, [rbp - LS_KSRC]
    test rax, rax
    jz .merge_cr_nokeys
    mov rcx, [rbp - LS_MJ]
    shl rcx, 4
    mov r8, [rax + rcx]
    mov r9, [rax + rcx + 8]
    mov rax, [rbp - LS_KDST]
    mov rcx, [rbp - LS_MK]
    shl rcx, 4
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
.merge_cr_nokeys:
    inc qword [rbp - LS_MJ]
    inc qword [rbp - LS_MK]
    jmp .merge_copy_right

.merge_copy_left:
    ; Right exhausted — copy remaining left elements to dst
    mov rax, [rbp - LS_MI]
    cmp rax, [rbp - LS_MMID]
    jge .merge_done
    mov rax, [rbp - LS_SRC]
    mov rcx, [rbp - LS_MI]
    shl rcx, 4
    mov r8, [rax + rcx]
    mov r9, [rax + rcx + 8]
    mov rax, [rbp - LS_DST]
    mov rcx, [rbp - LS_MK]
    shl rcx, 4
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
    ; Keys
    mov rax, [rbp - LS_KSRC]
    test rax, rax
    jz .merge_cl_nokeys
    mov rcx, [rbp - LS_MI]
    shl rcx, 4
    mov r8, [rax + rcx]
    mov r9, [rax + rcx + 8]
    mov rax, [rbp - LS_KDST]
    mov rcx, [rbp - LS_MK]
    shl rcx, 4
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
.merge_cl_nokeys:
    inc qword [rbp - LS_MI]
    inc qword [rbp - LS_MK]
    jmp .merge_copy_left

.merge_done:
    ; Advance to next pair of runs
    mov rax, [rbp - LS_OUTI]
    mov rcx, [rbp - LS_WIDTH]
    lea rax, [rax + rcx*2]
    mov [rbp - LS_OUTI], rax
    jmp .sort_outer_loop

.sort_outer_done:
    ; Swap src/dst pointers (result now in "new src" for next pass)
    mov rax, [rbp - LS_SRC]
    mov rcx, [rbp - LS_DST]
    mov [rbp - LS_SRC], rcx
    mov [rbp - LS_DST], rax
    ; Swap keys src/dst if keys exist
    mov rax, [rbp - LS_KSRC]
    test rax, rax
    jz .sort_no_key_swap
    mov rcx, [rbp - LS_KDST]
    mov [rbp - LS_KSRC], rcx
    mov [rbp - LS_KDST], rax
.sort_no_key_swap:
    ; width *= 2
    shl qword [rbp - LS_WIDTH], 1
    jmp .sort_width_loop

.sort_width_done:
    ; After loop, result is in LS_SRC. If not list's items, copy back.
    mov rax, [rbp - LS_SAVED_ITEMS]
    cmp rax, [rbp - LS_SRC]
    je .sort_free_temp             ; result already in items

    ; memcpy items ← src, n*16 bytes
    mov rdi, rax                   ; dest = saved items
    mov rsi, [rbp - LS_SRC]       ; src = temp (where result is)
    mov rdx, [rbp - LS_N]
    shl rdx, 4                     ; byte count
    extern ap_memcpy
    call ap_memcpy

.sort_free_temp:
    ; Free temp array
    mov rdi, [rbp - LS_TEMP]
    extern ap_free
    call ap_free

    ; If keys were used, DECREF all keys and free arrays
    cmp qword [rbp - LS_KEY], 0
    jz .sort_done

    ; DECREF each key in the final keys array (in LS_KSRC after swaps)
    mov r14, [rbp - LS_KSRC]
    test r14, r14
    jz .sort_free_ktemp
    xor r15d, r15d
.sort_decref_keys:
    cmp r15, [rbp - LS_N]
    jge .sort_free_keys
    mov rcx, r15
    shl rcx, 4
    mov rdi, [r14 + rcx]          ; key payload
    mov esi, [r14 + rcx + 8]      ; key tag
    DECREF_VAL rdi, rsi
    inc r15
    jmp .sort_decref_keys

.sort_free_keys:
    ; Free both keys arrays (use LS_KORIG not LS_KSRC - they may swap)
    mov rdi, [rbp - LS_KORIG]
    call ap_free
    mov rdi, [rbp - LS_KTEMP]
    call ap_free
    jmp .sort_done

.sort_free_ktemp:
    mov rdi, [rbp - LS_KTEMP]
    test rdi, rdi
    jz .sort_done
    call ap_free
    jmp .sort_done

.sort_cleanup_keys:
    ; Error during key computation — DECREF computed keys and free
    mov r14, [rbp - LS_KSRC]
    test r14, r14
    jz .sort_done
    xor r13d, r13d
.sort_cleanup_keys_loop:
    cmp r13, r15                   ; r15 = keys computed so far
    jge .sort_cleanup_keys_free
    mov rcx, r13
    shl rcx, 4
    mov rdi, [r14 + rcx]
    mov esi, [r14 + rcx + 8]
    DECREF_VAL rdi, rsi
    inc r13
    jmp .sort_cleanup_keys_loop
.sort_cleanup_keys_free:
    mov rdi, r14
    call ap_free
    ; Error path: propagate exception (return TAG_NULL)
    extern current_exception
    mov rax, [rel current_exception]
    test rax, rax
    jnz .sort_error_return

.sort_trivial_done:
    ; n < 2, no sort needed, return None
    RET_NONE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sort_done:
    ; Restore list state: put sorted items back
    mov rbx, [rbp - LS_LIST]
    ; Check if list was mutated during sort (ob_item or ob_size changed)
    mov rax, [rbx + PyListObject.ob_item]
    test rax, rax
    jnz .sort_mutated              ; ob_item != NULL → someone put items back
    test rax, rax
    jnz .sort_mutated              ; ob_item != NULL → someone put items back
    mov rax, [rbx + PyListObject.ob_size]
    test rax, rax
    jnz .sort_mutated              ; ob_size != 0 → someone changed it

    ; No mutation: copy sorted fat buffer back to payload/tag arrays
    mov rdi, [rbp - LS_SAVED_ITEMS]       ; fat buffer
    mov rsi, [rbp - LS_SAVED_PAYLOADS]    ; payloads
    mov rdx, [rbp - LS_SAVED_TAGS]        ; tags
    mov rcx, [rbp - LS_SAVED_SIZE]
    xor r8d, r8d
.sort_copy_back:
    cmp r8, rcx
    jge .sort_copy_back_done
    mov r9, r8
    shl r9, 4
    mov r10, [rdi + r9]           ; payload
    mov r11, [rdi + r9 + 8]       ; tag (low byte)
    V_PACK r10, r11
    mov [rsi + r8 * 8], r10
    inc r8
    jmp .sort_copy_back
.sort_copy_back_done:
    ; Free fat buffer
    mov rdi, [rbp - LS_SAVED_ITEMS]
    test rdi, rdi
    jz .sort_restore_ptrs
    call ap_free
    mov qword [rbp - LS_SAVED_ITEMS], 0
.sort_restore_ptrs:
    ; Restore list pointers and size
    mov rax, [rbp - LS_SAVED_PAYLOADS]
    mov [rbx + PyListObject.ob_item], rax
    mov rax, [rbp - LS_SAVED_TAGS]
    mov rax, [rbp - LS_SAVED_SIZE]
    mov [rbx + PyListObject.ob_size], rax

    ; Check if an exception was raised during sort
    mov rax, [rel current_exception]
    test rax, rax
    jnz .sort_error_return
    RET_NONE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sort_mutated:
    ; List was mutated during sort — this is an error
    ; IMPORTANT: raise_exception does not return (non-local jump)
    ; Must cleanup BEFORE raising.
    
    ; First free temp buffer (allocated during sort)
    mov rdi, [rbp - LS_TEMP]
    call ap_free
    
    ; If keys were used, DECREF keys and free arrays
    cmp qword [rbp - LS_KEY], 0
    jz .sm_no_keys
    mov r14, [rbp - LS_KSRC]
    test r14, r14
    jz .sm_free_ktemp
    xor r15d, r15d
.sm_decref_keys:
    cmp r15, [rbp - LS_N]
    jge .sm_free_keys
    mov rcx, r15
    shl rcx, 4
    mov rdi, [r14 + rcx]
    mov esi, [r14 + rcx + 8]
    DECREF_VAL rdi, rsi
    inc r15
    jmp .sm_decref_keys
.sm_free_keys:
    mov rdi, [rbp - LS_KORIG]
    call ap_free
    mov rdi, [rbp - LS_KTEMP]
    call ap_free
    jmp .sm_handle_mutation
.sm_free_ktemp:
    mov rdi, [rbp - LS_KTEMP]
    test rdi, rdi
    jz .sm_handle_mutation
    call ap_free
.sm_no_keys:
.sm_handle_mutation:
    ; Save mutated items for cleanup
    mov rcx, [rbx + PyListObject.ob_item]       ; mutated payloads
    mov r8, [rbx + PyListObject.ob_size]

    ; Restore our sorted items from fat buffer
    mov rdi, [rbp - LS_SAVED_ITEMS]       ; fat buffer
    mov rsi, [rbp - LS_SAVED_PAYLOADS]
    mov rdx, [rbp - LS_SAVED_TAGS]
    mov r10, [rbp - LS_SAVED_SIZE]
    xor r11d, r11d
.sort_mut_copy_back:
    cmp r11, r10
    jge .sort_mut_copy_back_done
    mov rax, r11
    shl rax, 4
    mov r12, [rdi + rax]          ; payload
    mov r13, [rdi + rax + 8]      ; tag
    V_PACK r12, r13
    mov [rsi + r11 * 8], r12
    inc r11
    jmp .sort_mut_copy_back
.sort_mut_copy_back_done:
    ; Free fat buffer
    mov rdi, [rbp - LS_SAVED_ITEMS]
    test rdi, rdi
    jz .sort_mut_restore_ptrs
    call ap_free
.sort_mut_restore_ptrs:
    mov rax, [rbp - LS_SAVED_PAYLOADS]
    mov [rbx + PyListObject.ob_item], rax
    mov rax, [rbp - LS_SAVED_TAGS]
    mov rax, [rbp - LS_SAVED_SIZE]
    mov [rbx + PyListObject.ob_size], rax

    ; DECREF all mutated items and free the arrays
    push rcx
    push r9
    push r8
    test rcx, rcx
    jz .sort_mut_no_decref
    xor r11d, r11d
.sort_mut_decref_loop:
    cmp r11, r8
    jge .sort_mut_decref_done
    mov rdi, [rcx + r11 * 8]          ; payload
    V_UNPACK rdi, rsi
    push rcx
    push r9
    push r8
    push r11
    DECREF_VAL rdi, rsi
    pop r11
    pop r8
    pop r9
    pop rcx
    inc r11
    jmp .sort_mut_decref_loop
.sort_mut_decref_done:
    mov rdi, rcx
    call ap_free
    mov rdi, r9
    call ap_free
.sort_mut_no_decref:
    pop r8
    pop r9
    pop rcx
    ; Raise ValueError
    RAISE exc_ValueError_type, "list modified during sort"
    ; raise_exception does not return

.sort_error_return:
    ; Restore list items if still saved (error during sort before merge)
    mov rbx, [rbp - LS_LIST]
    mov rax, [rbx + PyListObject.ob_item]
    test rax, rax
    jnz .sort_error_already_restored
    ; List is still empty — restore saved pointers
    mov rax, [rbp - LS_SAVED_PAYLOADS]
    mov [rbx + PyListObject.ob_item], rax
    mov rax, [rbp - LS_SAVED_TAGS]
    mov rax, [rbp - LS_SAVED_SIZE]
    mov [rbx + PyListObject.ob_size], rax
.sort_error_already_restored:
    ; Free fat buffer if allocated
    mov rdi, [rbp - LS_SAVED_ITEMS]
    test rdi, rdi
    jz .sort_error_done
    call ap_free
.sort_error_done:
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.ls_too_many:
    RAISE exc_TypeError_type, "sort() takes no positional arguments"
END_FUNC list_method_sort

;; ============================================================================
;; list_method_index(args, nargs) -> SmallInt index
;; args[0]=self, args[1]=value, optional args[2]=start, args[3]=stop
;; Linear scan with identity check then __eq__ protocol
;; ============================================================================
LI_LIST   equ 8
LI_VPAY   equ 16   ; value payload
LI_IDX    equ 32
LI_SIZE   equ 40
LI_ARGS   equ 48   ; save args pointer
LI_NARGS  equ 56   ; save nargs
LI_FRAME  equ 56
DEF_FUNC list_method_index, LI_FRAME
    push rbx
    push r12

    mov [rbp - LI_ARGS], rdi  ; save args
    mov [rbp - LI_NARGS], rsi ; save nargs
    mov rax, [rdi]           ; self
    mov [rbp - LI_LIST], rax
    mov rax, [rdi + 8]      ; args[1], the value to find
    mov [rbp - LI_VPAY], rax    ; kept whole: obj_richcompare_bool takes a Value
    mov rcx, [rbp - LI_LIST]
    mov rcx, [rcx + PyListObject.ob_size]

    ; Default stop = list size
    mov [rbp - LI_SIZE], rcx

    ; Default start = 0
    mov qword [rbp - LI_IDX], 0

    ; Check for optional start arg (nargs >= 3)
    cmp qword [rbp - LI_NARGS], 3
    jl .li_have_bounds
    ; Get start from args[2]
    mov rax, [rbp - LI_ARGS]
    mov rdi, [rax + 16]      ; args[2] payload
    V_UNPACK rdi, rdx       ; args[2]
    call int_to_i64
    ; Handle negative start
    test rax, rax
    jns .li_start_pos
    add rax, [rbp - LI_SIZE]  ; start += len
    test rax, rax
    jns .li_start_pos
    xor eax, eax              ; clamp to 0
.li_start_pos:
    mov [rbp - LI_IDX], rax

    ; Check for optional stop arg (nargs >= 4)
    cmp qword [rbp - LI_NARGS], 4
    jl .li_have_bounds
    ; Get stop from args[3]
    mov rax, [rbp - LI_ARGS]
    mov rdi, [rax + 24]      ; args[3] payload
    V_UNPACK rdi, rdx       ; args[3]
    call int_to_i64
    ; Handle negative stop
    test rax, rax
    jns .li_stop_pos
    add rax, [rbp - LI_SIZE]  ; stop += len
    test rax, rax
    jns .li_stop_pos
    xor eax, eax              ; clamp to 0
.li_stop_pos:
    ; Clamp stop to list size
    mov rcx, [rbp - LI_LIST]
    mov rcx, [rcx + PyListObject.ob_size]
    cmp rax, rcx
    jle .li_stop_ok
    mov rax, rcx
.li_stop_ok:
    mov [rbp - LI_SIZE], rax

.li_have_bounds:

.index_loop:
    mov rax, [rbp - LI_IDX]
    cmp rax, [rbp - LI_SIZE]
    jge .index_not_found
    ; Re-read the size: an element's __eq__ can shorten the list.
    mov rbx, [rbp - LI_LIST]
    cmp rax, [rbx + PyListObject.ob_size]
    jge .index_not_found

    mov rbx, [rbx + PyListObject.ob_item]
    mov rdi, [rbx + rax * 8]    ; the element Value

    ; Was a hand-rolled type switch feeding tp_richcompare, with a NULL
    ; result meaning "no match" -- so NotImplemented never tried the
    ; reflected operand and a raising __eq__ was reported as absence.
    mov rsi, [rbp - LI_VPAY]
    mov edx, PY_EQ
    call obj_richcompare_bool
    cmp eax, -1
    je .index_error
    test eax, eax
    jnz .index_found

    inc qword [rbp - LI_IDX]
    jmp .index_loop

.index_found:
    mov rdi, [rbp - LI_IDX]
    call int_from_i64
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.index_error:
    leave
    jmp eval_exception_unwind

.index_not_found:
    RAISE exc_ValueError_type, "x not in list"
END_FUNC list_method_index

;; ============================================================================
;; list_method_count(args, nargs) -> SmallInt
;; args[0]=self, args[1]=value
;; ============================================================================
LC_IDX    equ 8
LC_FRAME  equ 8

DEF_FUNC list_method_count, LC_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; the value Value
    xor r14d, r14d          ; count = 0
    mov qword [rbp - LC_IDX], 0

.count_loop:
    mov rcx, [rbp - LC_IDX]
    ; The size is re-read every pass: an element's __eq__ can shorten the
    ; list under us.
    mov r13, [rbx + PyListObject.ob_size]
    cmp rcx, r13
    jge .count_done

    mov rax, [rbx + PyListObject.ob_item]
    mov rdi, [rax + rcx * 8]    ; the element Value
    mov rsi, r12
    mov edx, PY_EQ
    call obj_richcompare_bool
    cmp eax, -1
    je .count_error
    test eax, eax
    jz .count_next
    inc r14

.count_next:
    inc qword [rbp - LC_IDX]
    jmp .count_loop

.count_done:
    mov rdi, r14
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.count_error:
    leave
    jmp eval_exception_unwind
END_FUNC list_method_count

;; ============================================================================
;; list_method_copy(args, nargs) -> new list (shallow copy)
;; args[0]=self
;; ============================================================================
DEF_FUNC list_method_copy
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rbx + PyListObject.ob_size]

    ; Create new list with same capacity
    mov rdi, r12
    test rdi, rdi
    jnz .copy_alloc
    mov rdi, 4
.copy_alloc:
    call list_new
    mov r13, rax            ; new list

    ; Append each item (list_append does INCREF)
    xor ecx, ecx
.copy_loop:
    cmp rcx, r12
    jge .copy_done
    push rcx
    mov rax, [rbx + PyListObject.ob_item]
    mov rsi, [rax + rcx * 8]    ; payload
    mov rdi, r13
    call list_append
    pop rcx
    inc rcx
    jmp .copy_loop

.copy_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_method_copy

;; ============================================================================
;; list.__getitem__(self, key) → calls list_subscript
;; ============================================================================
extern list_subscript
;; ============================================================================
;; tuple dunders.  tuple_type.tp_dict held only index and count, so
;; hasattr((1,), '__getitem__') was False and the operators worked solely
;; through the type slots -- which is what CPython's seq_tests probes.
;; ============================================================================
extern tuple_subscript

extern tuple_contains


extern tuple_concat

extern tuple_repeat


DEF_FUNC_BARE list_dunder_getitem
    mov rax, [rdi]          ; self
    mov rsi, [rdi + 8]     ; key payload
    mov rdi, rax
    jmp list_subscript
END_FUNC list_dunder_getitem

;; ============================================================================
;; list.__setitem__(self, key, value) → calls list_ass_subscript
;; ============================================================================
extern list_ass_subscript
DEF_FUNC list_dunder_setitem
    mov rax, [rdi]          ; self
    mov rsi, [rdi + 8]      ; args[1] = key   (already a Value)
    mov rdx, [rdi + 16]     ; args[2] = value (already a Value)
    mov rdi, rax
    call list_ass_subscript
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
END_FUNC list_dunder_setitem

;; ============================================================================
;; list.__delitem__(self, key) → calls list_ass_subscript with NULL value
;; ============================================================================
DEF_FUNC list_dunder_delitem
    mov rax, [rdi]          ; self
    mov rsi, [rdi + 8]     ; key payload
    xor edx, edx            ; a NULL value Value means "delete"
    mov rdi, rax
    call list_ass_subscript
    extern none_singleton
    RET_NONE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_dunder_delitem

;; ============================================================================
;; list.__contains__(self, item) → calls list_contains
;; ============================================================================
extern list_contains
DEF_FUNC list_dunder_contains
    mov rax, [rdi]          ; self
    mov rsi, [rdi + 8]     ; item payload
    mov rdi, rax
    call list_contains
    ; eax = 0 or 1 → return bool
    test eax, eax
    jz .ldc_false
    extern bool_true
    lea rax, [rel bool_true]
    jmp .ldc_done
.ldc_false:
    extern bool_false
    lea rax, [rel bool_false]
.ldc_done:
    mov edx, TAG_PTR
    INCREF rax
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_dunder_contains

;; ============================================================================
;; list.__len__(self) → returns SmallInt length
;; ============================================================================
DEF_FUNC list_dunder_len
    mov rax, [rdi]          ; self
    mov rax, [rax + PyListObject.ob_size]
    mov rdi, rax
    call int_from_i64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_dunder_len

;; ============================================================================
;; list.__iadd__(self, other) → calls list_inplace_concat
;; ============================================================================
extern list_inplace_concat
DEF_FUNC_BARE list_dunder_iadd
    mov rax, [rdi]          ; self
    mov rsi, [rdi + 8]     ; other payload
    V_UNPACK rsi, rcx       ; args[1]
    mov rdi, rax
    jmp list_inplace_concat
END_FUNC list_dunder_iadd

;; ============================================================================
;; list.__init__(self, [iterable]) → re-initialize list
;; Uses list_extend to populate from iterable after clearing.
;; ============================================================================
;; ============================================================================
;; container_dunder_new(args, nargs) -> a new empty instance of args[0]
;;
;; list, tuple, dict and set had no __new__ in their type dicts, so
;; super().__new__(cls, seq) inside a subclass's own __new__ found nothing.
;; It is registered as a staticmethod, as CPython does: __new__ takes the
;; class explicitly and must not be bound to anything.
;; ============================================================================
extern instance_new
extern builtin_sub_init_base
extern tuple_sub_fill
DEF_FUNC container_dunder_new
    push rbx
    push r12
    push r13

    test rsi, rsi
    jz .cdn_error
    mov rbx, [rdi]              ; cls
    mov r12, rdi                ; args
    mov r13, rsi                ; nargs

    V_TEST_PTR rbx, rax
    ja .cdn_error

    mov rdi, rbx
    call instance_new
    push rax
    mov rdi, rax
    call builtin_sub_init_base
    pop rax

    ; tuple is immutable, so its contents arrive here rather than through
    ; __init__.
    mov rcx, [rbx + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_TUPLE_SUBCLASS
    jz .cdn_done
    push rax
    mov rdi, rax
    lea rsi, [r12 + 8]          ; the arguments after cls
    lea rdx, [r13 - 1]
    call tuple_sub_fill
    pop rax

.cdn_done:
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.cdn_error:
    RAISE exc_TypeError_type, "__new__() takes a class argument"
END_FUNC container_dunder_new

DEF_FUNC list_dunder_init
    push rbx
    push r12

    ; list() takes no keyword arguments.  A subclass that overrides __init__
    ; or __new__ absorbs them itself -- func_call clears kw_names_pending on
    ; the way in, so by the time super().__init__(seq) reaches here it is
    ; unset.  Seeing it still set means the keywords were aimed at list's own
    ; init, which CPython rejects: subclass(sequence=()) is a TypeError.
    cmp qword [rel kw_names_pending], 0
    jne .ldi_no_keywords

    mov rbx, rdi            ; save args ptr
    mov r12, rsi            ; save nargs

    ; self = args[0]
    mov rax, [rbx]          ; self (list)

    ; Clear: DECREF all items, set size to 0
    push rax
    mov rcx, [rax + PyListObject.ob_size]
    test rcx, rcx
    jz .ldi_cleared
    ; Simple clear: just set size to 0 (items leak but safe for now)
    mov qword [rax + PyListObject.ob_size], 0
.ldi_cleared:
    pop rax

    ; If nargs >= 2, use list_extend to add items from args[1]
    cmp r12, 2
    jl .ldi_done

    ; Build args for list_extend: args[0]=self, args[1]=iterable
    ; Our args are already in the right format: [self, iterable, ...]
    mov rdi, rbx            ; args ptr (already has self + iterable)
    mov rsi, 2              ; nargs = 2
    call list_method_extend

.ldi_done:
    RET_NONE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.ldi_no_keywords:
    RAISE exc_TypeError_type, "list() takes no keyword arguments"
END_FUNC list_dunder_init

;; ============================================================================
;; list_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
DEF_FUNC list_method_clear
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    ; Check if list is being sorted (ob_item == NULL)
    cmp qword [rbx + PyListObject.ob_item], 0
    je list_sorting_error
    mov r12, [rbx + PyListObject.ob_size]

    ; DECREF all items (fat 16-byte slots)
    xor r13d, r13d
.clear_loop:
    cmp r13, r12
    jge .clear_done
    mov rax, [rbx + PyListObject.ob_item]
    mov rdi, [rax + r13 * 8]    ; payload
    V_UNPACK rdi, rsi
    push r13
    DECREF_VAL rdi, rsi
    pop r13
    inc r13
    jmp .clear_loop

.clear_done:
    mov qword [rbx + PyListObject.ob_size], 0

    RET_NONE
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_method_clear

;; ============================================================================
;; list_method_extend(args, nargs) -> None
;; args[0]=self, args[1]=iterable (list, tuple, or generic iterable)
;; ============================================================================
LE_SELF   equ 8
LE_ITER   equ 16
LE_FRAME  equ 16
DEF_FUNC list_method_extend, LE_FRAME
    push rbx
    push r12
    push r13

    mov rbx, [rdi]           ; self
    ; Check if list is being sorted (ob_item == NULL)
    cmp qword [rbx + PyListObject.ob_item], 0
    je list_sorting_error
    mov r12, [rdi + 8]      ; iterable payload
    V_UNPACK r12, r13       ; args[1]
    mov [rbp - LE_SELF], rbx

    ; Check iterable type for fast paths
    test r13d, TAG_RC_BIT
    jz .extend_generic         ; non-pointer → must use generic iter

    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel list_type]
    cmp rax, rcx
    je .extend_list
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .extend_tuple

    ; Generic iterable path
    jmp .extend_generic

.extend_list:
    mov r13, [r12 + PyListObject.ob_size]
    xor ecx, ecx
.extend_list_loop:
    cmp rcx, r13
    jge .extend_done
    push rcx
    mov rax, [r12 + PyListObject.ob_item]
    mov rsi, [rax + rcx * 8]       ; payload
    mov rdi, [rbp - LE_SELF]
    call list_append
    pop rcx
    inc rcx
    jmp .extend_list_loop

.extend_tuple:
    mov r13, [r12 + PyTupleObject.ob_size]
    xor ecx, ecx
.extend_tuple_loop:
    cmp rcx, r13
    jge .extend_done
    push rcx
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rsi, [rax + rcx * 8]      ; payload
    mov rdi, [rbp - LE_SELF]
    call list_append
    pop rcx
    inc rcx
    jmp .extend_tuple_loop

.extend_generic:
    ; Get tp_iter from iterable type
    test r13d, TAG_RC_BIT
    jz .extend_type_error       ; non-pointer has no tp_iter
    mov rax, [r12 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .extend_type_error
    mov rdi, r12
    call rax                    ; tp_iter(iterable) → iterator
    test rax, rax
    jz .extend_type_error
    mov [rbp - LE_ITER], rax

.extend_iter_loop:
    mov rdi, [rbp - LE_ITER]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .extend_iter_done
    mov rdi, [rbp - LE_ITER]
    call rax                    ; tp_iternext(iter) → (payload, tag)
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx
    jz .extend_iter_done        ; StopIteration

    ; Append item to list
    push rax
    push rdx
    mov rdi, [rbp - LE_SELF]
    mov rsi, rax
    ; edx = tag (already set)
    V_PACK rsi, rdx         ; list_append takes a Value
    call list_append
    ; DECREF item (list_append INCREFs internally)
    pop rsi                     ; tag
    pop rdi                     ; payload
    DECREF_VAL rdi, rsi
    jmp .extend_iter_loop

.extend_iter_done:
    ; DECREF iterator
    mov rdi, [rbp - LE_ITER]
    call obj_decref

.extend_done:
    RET_NONE
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.extend_type_error:
    RAISE exc_TypeError_type, "list.extend() argument must be iterable"
END_FUNC list_method_extend

;; ============================================================================
;; list_method_remove(args, nargs) -> None
;; args[0]=self, args[1]=value
;; Removes first occurrence of value. Raises ValueError if not found.
;; ============================================================================
DEF_FUNC list_method_remove
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, [rdi]          ; self (list)
    ; Check if list is being sorted (ob_item == NULL)
    cmp qword [rbx + PyListObject.ob_item], 0
    je list_sorting_error
    mov r12, [rdi + 8]      ; the value Value
    xor r14d, r14d          ; index = 0

.lremove_loop:
    ; Re-read the size each pass: an element's __eq__ can shorten the list.
    mov r13, [rbx + PyListObject.ob_size]
    cmp r14, r13
    jge .lremove_not_found

    mov rax, [rbx + PyListObject.ob_item]
    mov rdi, [rax + r14 * 8]    ; the element Value

    ; Was a hand-rolled type switch feeding tp_richcompare, treating a NULL
    ; result as "no match" -- so NotImplemented never reached the reflected
    ; operand and a raising __eq__ became a ValueError about absence.
    mov rsi, r12
    mov edx, PY_EQ
    call obj_richcompare_bool
    cmp eax, -1
    je .lremove_error
    test eax, eax
    jnz .lremove_found

    inc r14
    jmp .lremove_loop

.lremove_error:
    leave
    jmp eval_exception_unwind

.lremove_found:
    ; r14 = index of found item
    ; Get the item for DECREF
    mov rax, [rbx + PyListObject.ob_item]
    mov r12, [rax + r14 * 8]        ; the item Value

    ; Shift payloads left: memmove(&payloads[idx], &payloads[idx+1], (size-1-idx)*8)
    mov rax, [rbx + PyListObject.ob_item]
    lea rdi, [rax + r14 * 8]
    lea rsi, [rdi + 8]
    mov rdx, [rbx + PyListObject.ob_size]
    sub rdx, r14
    dec rdx                 ; count = size - idx - 1
    shl rdx, 3              ; bytes = count * 8
    jz .lremove_shrink      ; nothing to shift if removing last
    call ap_memmove

.lremove_shrink:
    dec qword [rbx + PyListObject.ob_size]

    ; DECREF the removed item
    mov rdi, r12
    DECREF_V rdi, rsi

    ; Return None
    RET_NONE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.lremove_not_found:
    RAISE exc_ValueError_type, "list.remove(x): x not in list"
END_FUNC list_method_remove

;; ============================================================================
;; list_method_reversed(args, nargs) -> reversed iterator
;; args[0]=self
;; ============================================================================
extern reversed_iter_type
DEF_FUNC list_method_reversed
    push rbx

    mov rbx, [rdi]            ; self (list)

    ; Allocate ReversedIterObject (32 bytes: refcnt, type, it_seq, it_index)
    mov edi, 32
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel reversed_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + 16], rbx       ; it_seq = self
    INCREF rbx
    mov rcx, [rbx + PyListObject.ob_size]
    dec rcx                   ; it_index = ob_size - 1
    mov [rax + 24], rcx

    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC list_method_reversed


