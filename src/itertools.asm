; itertools.asm - Iterator builtins: enumerate, zip, map, filter, reversed, sorted
;
; Each iterator type has: type object, _new(), _iternext(), _dealloc(), iter_self
; Builtin signatures: func(PyObject **args, int64_t nargs) -> PyObject*

%include "macros.inc"
%include "object.inc"
%include "types.inc"


extern ap_malloc
extern ap_free
extern obj_incref
extern obj_decref
extern obj_dealloc
extern obj_is_true
extern fatal_error
extern raise_exception
extern exc_TypeError_type
extern exc_StopIteration_type
extern none_singleton
extern tuple_new
extern list_new
extern list_append
extern int_from_i64
extern int_to_i64
extern list_method_sort
extern type_type

;; ============================================================================
;; Struct definitions (inline, 32 bytes each)
;; ============================================================================
;; EnumerateIterObject: +0 refcnt, +8 type, +16 it_iter, +24 it_count
;; ZipIterObject:       +0 refcnt, +8 type, +16 it_iters, +24 it_count
;; MapIterObject:       +0 refcnt, +8 type, +16 it_func, +24 it_iter
;; FilterIterObject:    +0 refcnt, +8 type, +16 it_func, +24 it_iter
;; ReversedIterObject:  +0 refcnt, +8 type, +16 it_seq, +24 it_index

; Offsets (all 32-byte objects)
%define IT_FIELD1  16     ; first custom field
%define IT_FIELD2  24     ; second custom field
%define ITER_OBJ_SIZE 32

;; ============================================================================
;; Common: iter_self(self) -> self with INCREF
;; tp_iter for all our iterator types: return self
;; ============================================================================
itertools_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret

;; ============================================================================
;; Helper: get_iterator(obj) -> iterator
;; Calls tp_iter on obj, returns iterator. Raises TypeError if no tp_iter.
;; Clobbers caller-saved regs.
;; ============================================================================
DEF_FUNC_LOCAL get_iterator
    ; rdi = obj payload, esi = obj tag

    ; Check SmallInt (cannot iterate)
    cmp esi, TAG_SMALLINT
    je .no_iter

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .no_iter
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .no_iter
    call rax
    leave
    ret

.no_iter:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception
END_FUNC get_iterator

;; ============================================================================
;; ENUMERATE
;; ============================================================================

;; builtin_enumerate(args, nargs) -> EnumerateIterObject*
;; nargs=1: enumerate(iterable), start=0
;; nargs=2: enumerate(iterable, start)
DEF_FUNC builtin_enumerate
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; args
    mov r12, rsi            ; nargs

    ; Validate nargs
    cmp r12, 1
    jl .enum_error
    cmp r12, 2
    jg .enum_error

    ; Get start value
    xor r13d, r13d          ; default start = 0
    cmp r12, 2
    jne .enum_get_iter

    ; start = int(args[1])
    mov rdi, [rbx + 16]
    mov edx, [rbx + 24]
    call int_to_i64
    mov r13, rax

.enum_get_iter:
    ; Get iterator from args[0]
    mov rdi, [rbx]
    mov esi, [rbx + 8]         ; args[0] tag
    call get_iterator
    mov rbx, rax            ; rbx = underlying iterator

    ; Allocate EnumerateIterObject
    mov edi, ITER_OBJ_SIZE
    call ap_malloc

    ; Fill fields
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel enumerate_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + IT_FIELD1], rbx       ; it_iter
    mov [rax + IT_FIELD2], r13       ; it_count (raw i64, not SmallInt)

    pop r13
    pop r12
    pop rbx
    leave
    ret

.enum_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "enumerate() requires 1 or 2 arguments"
    call raise_exception
END_FUNC builtin_enumerate

;; enumerate_iternext(self) -> PyObject* (2-tuple) or NULL
DEF_FUNC_LOCAL enumerate_iternext
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; self

    ; Call underlying iterator's iternext
    mov rdi, [rbx + IT_FIELD1]       ; it_iter
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    test edx, edx
    jz .enum_exhausted
    mov r12, rax             ; r12 = value payload from iternext
    push rdx                 ; save value tag from iternext

    ; Create SmallInt for current count
    mov rdi, [rbx + IT_FIELD2]       ; it_count (raw i64)
    call int_from_i64
    mov r13, rax             ; r13 = count payload
    push rdx                 ; save count tag from int_from_i64

    ; Increment it_count
    inc qword [rbx + IT_FIELD2]

    ; Create 2-tuple
    mov rdi, 2
    call tuple_new
    ; rax = new tuple
    ; Fill: tuple[0] = count, tuple[1] = value (fat 16-byte slots)
    pop rcx                  ; count tag
    mov [rax + PyTupleObject.ob_item], r13            ; count payload (slot 0)
    mov [rax + PyTupleObject.ob_item + 8], rcx        ; count tag
    pop rcx                  ; value tag
    mov [rax + PyTupleObject.ob_item + 16], r12       ; value payload (slot 1)
    mov [rax + PyTupleObject.ob_item + 24], rcx       ; value tag

    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR               ; fat return tag
    leave
    ret

.enum_exhausted:
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC enumerate_iternext

;; enumerate_dealloc(self)
DEF_FUNC_LOCAL enumerate_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF the underlying iterator
    mov rdi, [rbx + IT_FIELD1]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC enumerate_dealloc

;; ============================================================================
;; ZIP
;; ============================================================================

;; builtin_zip(args, nargs) -> ZipIterObject*
DEF_FUNC builtin_zip
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; args
    mov r12, rsi            ; nargs

    ; Handle zero args: zip() returns empty iterator
    test r12, r12
    jz .zip_zero

    ; Allocate array of iterator pointers: nargs * 8
    lea rdi, [r12 * 8]
    call ap_malloc
    mov r13, rax             ; r13 = iterator array

    ; For each arg, get its iterator
    xor r14d, r14d          ; i = 0
.zip_iter_loop:
    cmp r14, r12
    jge .zip_create

    mov rax, r14
    shl rax, 4                  ; rax = i * 16
    mov rdi, [rbx + rax]
    mov esi, [rbx + rax + 8]   ; arg tag
    push r13
    push r14
    call get_iterator
    pop r14
    pop r13
    mov [r13 + r14 * 8], rax    ; store iterator

    inc r14
    jmp .zip_iter_loop

.zip_create:
    ; Allocate ZipIterObject
    mov edi, ITER_OBJ_SIZE
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel zip_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + IT_FIELD1], r13       ; it_iters (array ptr)
    mov [rax + IT_FIELD2], r12       ; it_count (number of iterators)

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.zip_zero:
    ; Create a zip with 0 iterators (will immediately exhaust)
    mov edi, ITER_OBJ_SIZE
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel zip_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + IT_FIELD1], 0   ; NULL iters array
    mov qword [rax + IT_FIELD2], 0   ; 0 iterators

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC builtin_zip

;; zip_iternext(self) -> PyObject* (tuple) or NULL
DEF_FUNC_LOCAL zip_iternext
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi            ; self
    mov r12, [rbx + IT_FIELD2]   ; it_count
    mov r13, [rbx + IT_FIELD1]   ; it_iters array

    ; Zero iterators = exhausted
    test r12, r12
    jz .zip_exhausted

    ; Create result tuple of size it_count
    mov rdi, r12
    call tuple_new
    mov r14, rax             ; r14 = result tuple

    ; For each iterator, call iternext
    xor r15d, r15d          ; i = 0
.zip_next_loop:
    cmp r15, r12
    jge .zip_done

    mov rdi, [r13 + r15 * 8]    ; iterator[i]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    test edx, edx
    jz .zip_partial_cleanup

    ; Store value in tuple at fat stride (rdx = tag from iternext)
    mov rcx, r15
    shl rcx, 4                     ; index * 16
    mov [r14 + PyTupleObject.ob_item + rcx], rax       ; payload
    mov [r14 + PyTupleObject.ob_item + rcx + 8], rdx   ; tag

    inc r15
    jmp .zip_next_loop

.zip_done:
    mov rax, r14
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    ret

.zip_partial_cleanup:
    ; One iterator exhausted. DECREF items already stored in tuple, then free tuple.
    ; r15 = number of items already stored (items 0..r15-1)
    xor ecx, ecx
.zip_cleanup_loop:
    cmp rcx, r15
    jge .zip_free_tuple
    push rcx
    mov rax, rcx
    shl rax, 4
    mov rdi, [r14 + PyTupleObject.ob_item + rax]
    mov rsi, [r14 + PyTupleObject.ob_item + rax + 8]
    DECREF_VAL rdi, rsi
    pop rcx
    inc rcx
    jmp .zip_cleanup_loop

.zip_free_tuple:
    ; Zero out remaining items (both payload and tag) to avoid double-free in tuple_dealloc
    mov rcx, r15
.zip_zero_loop:
    cmp rcx, r12
    jge .zip_do_free
    mov rax, rcx
    shl rax, 4
    mov qword [r14 + PyTupleObject.ob_item + rax], 0      ; payload
    mov qword [r14 + PyTupleObject.ob_item + rax + 8], 0   ; tag
    inc rcx
    jmp .zip_zero_loop
.zip_do_free:
    mov rdi, r14
    call obj_decref

.zip_exhausted:
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC zip_iternext

;; zip_dealloc(self)
DEF_FUNC_LOCAL zip_dealloc
    push rbx
    push r12
    push r13
    mov rbx, rdi

    mov r12, [rbx + IT_FIELD2]   ; count
    mov r13, [rbx + IT_FIELD1]   ; iters array

    ; DECREF each iterator
    test r13, r13
    jz .zip_dealloc_free

    xor ecx, ecx
.zip_dealloc_loop:
    cmp rcx, r12
    jge .zip_free_array
    push rcx
    mov rdi, [r13 + rcx * 8]
    call obj_decref
    pop rcx
    inc rcx
    jmp .zip_dealloc_loop

.zip_free_array:
    mov rdi, r13
    call ap_free

.zip_dealloc_free:
    mov rdi, rbx
    call ap_free

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC zip_dealloc

;; ============================================================================
;; MAP
;; ============================================================================

;; builtin_map(args, nargs) -> MapIterObject*
;; nargs=2: map(func, iterable)
DEF_FUNC builtin_map
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; args
    mov r12, rsi            ; nargs

    cmp r12, 2
    jne .map_error

    ; INCREF func
    mov r13, [rbx]          ; r13 = func
    INCREF r13

    ; Get iterator from args[1]
    mov rdi, [rbx + 16]
    mov esi, [rbx + 24]       ; args[1] tag
    call get_iterator
    mov rbx, rax             ; rbx = underlying iterator

    ; Allocate MapIterObject
    mov edi, ITER_OBJ_SIZE
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel map_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + IT_FIELD1], r13       ; it_func
    mov [rax + IT_FIELD2], rbx       ; it_iter

    pop r13
    pop r12
    pop rbx
    leave
    ret

.map_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "map() requires exactly 2 arguments"
    call raise_exception
END_FUNC builtin_map

;; map_iternext(self) -> PyObject* or NULL
;; IMPORTANT: Do not clobber r12 before calling tp_call, because func_call
;; reads r12 expecting the eval loop's current frame pointer.
DEF_FUNC_LOCAL map_iternext
    push rbx
    push r13
    push r14
    sub rsp, 8              ; align to 16

    mov rbx, rdi            ; self

    ; Get next item from underlying iterator
    mov rdi, [rbx + IT_FIELD2]       ; it_iter
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    test edx, edx
    jz .map_exhausted
    mov r13, rax             ; r13 = item payload
    mov r14, rdx             ; r14 = item tag

    ; Call func(item): tp_call(func, &item, 1)
    ; Put item on stack as fat args array (16 bytes)
    sub rsp, 16
    mov [rsp], r13           ; args[0] payload = item
    mov [rsp + 8], r14       ; args[0] tag
    mov rdi, [rbx + IT_FIELD1]   ; func
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    mov rsi, rsp             ; args pointer
    mov edx, 1               ; nargs = 1
    call rax
    add rsp, 16             ; pop fat args
    ; Save result across DECREF
    mov [rsp], rdx           ; save result tag in alignment slot
    push rax                 ; save result payload

    ; DECREF_VAL item
    DECREF_VAL r13, r14

    pop rax                  ; restore result payload
    mov rdx, [rsp]           ; restore result tag
    add rsp, 8
    pop r14
    pop r13
    pop rbx
    leave
    ret

.map_exhausted:
    RET_NULL
    add rsp, 8
    pop r14
    pop r13
    pop rbx
    leave
    ret
END_FUNC map_iternext

;; map_dealloc(self)
DEF_FUNC_LOCAL map_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF func
    mov rdi, [rbx + IT_FIELD1]
    call obj_decref

    ; DECREF iterator
    mov rdi, [rbx + IT_FIELD2]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC map_dealloc

;; ============================================================================
;; FILTER
;; ============================================================================

;; builtin_filter(args, nargs) -> FilterIterObject*
;; nargs=2: filter(func_or_none, iterable)
DEF_FUNC builtin_filter
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; args
    mov r12, rsi            ; nargs

    cmp r12, 2
    jne .filter_error

    ; Check if func is None
    mov r13, [rbx]          ; r13 = func_or_none
    lea rax, [rel none_singleton]
    cmp r13, rax
    je .filter_none_func

    ; INCREF func
    INCREF r13
    jmp .filter_get_iter

.filter_none_func:
    xor r13d, r13d          ; it_func = NULL for identity/truthiness

.filter_get_iter:
    ; Get iterator from args[1]
    mov rdi, [rbx + 16]
    mov esi, [rbx + 24]       ; args[1] tag
    call get_iterator
    mov rbx, rax             ; rbx = underlying iterator

    ; Allocate FilterIterObject
    mov edi, ITER_OBJ_SIZE
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel filter_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + IT_FIELD1], r13       ; it_func (or NULL)
    mov [rax + IT_FIELD2], rbx       ; it_iter

    pop r13
    pop r12
    pop rbx
    leave
    ret

.filter_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "filter() requires exactly 2 arguments"
    call raise_exception
END_FUNC builtin_filter

;; filter_iternext(self) -> PyObject* or NULL
;; IMPORTANT: Do not clobber r12 before calling tp_call, because func_call
;; reads r12 expecting the eval loop's current frame pointer.
DEF_FUNC_LOCAL filter_iternext
    push rbx
    push r13
    push r14
    push r15

    mov rbx, rdi            ; self

.filter_loop:
    ; Get next item from underlying iterator
    mov rdi, [rbx + IT_FIELD2]       ; it_iter
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    test edx, edx
    jz .filter_exhausted
    mov r13, rax             ; r13 = item payload (we own ref)
    push rdx                 ; save item tag from iternext

    ; Check if func is NULL (identity/truthiness test)
    mov r14, [rbx + IT_FIELD1]   ; it_func
    test r14, r14
    jz .filter_identity

    ; Call func(item) and test truthiness of result
    sub rsp, 16             ; args[0] (16B slot)
    mov [rsp], r13          ; args[0].payload = item
    mov rax, [rsp + 16]    ; item tag (saved above push)
    mov [rsp + 8], rax     ; args[0].tag
    mov rdi, r14             ; func
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    mov rsi, rsp             ; &args[0]
    mov edx, 1
    call rax
    add rsp, 16             ; pop args
    mov r14, rax             ; r14 = result payload
    mov r15, rdx             ; r15 = result tag

    ; Test truthiness of result
    mov rdi, r14
    mov esi, r15d
    call obj_is_true
    push rax                 ; save truthiness

    ; DECREF result
    mov rdi, r14
    mov rsi, r15
    DECREF_VAL rdi, rsi

    pop rax                  ; restore truthiness
    test eax, eax
    jnz .filter_accept

    ; Not truthy: DECREF item, continue
    pop rsi                  ; item tag
    mov rdi, r13
    DECREF_VAL rdi, rsi
    jmp .filter_loop

.filter_identity:
    ; Test truthiness of item itself
    mov rdi, r13
    mov esi, [rsp]           ; item tag (saved on stack)
    call obj_is_true
    test eax, eax
    jnz .filter_accept

    ; Not truthy: DECREF item, continue
    pop rsi                  ; item tag
    mov rdi, r13
    DECREF_VAL rdi, rsi
    jmp .filter_loop

.filter_accept:
    mov rax, r13             ; payload
    pop rdx                  ; tag from iternext
    pop r15
    pop r14
    pop r13
    pop rbx
    leave
    ret

.filter_exhausted:
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop rbx
    leave
    ret
END_FUNC filter_iternext

;; filter_dealloc(self)
DEF_FUNC_LOCAL filter_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF func (if not NULL)
    mov rdi, [rbx + IT_FIELD1]
    test rdi, rdi
    jz .filter_dealloc_iter
    call obj_decref

.filter_dealloc_iter:
    ; DECREF iterator
    mov rdi, [rbx + IT_FIELD2]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC filter_dealloc

;; ============================================================================
;; REVERSED
;; ============================================================================

;; builtin_reversed(args, nargs) -> ReversedIterObject*
;; nargs=1: reversed(sequence)
DEF_FUNC builtin_reversed
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; args
    mov r12, rsi            ; nargs

    cmp r12, 1
    jne .rev_error

    mov r12, [rbx]          ; r12 = sequence

    ; SmallInt check - cannot reverse
    cmp dword [rbx + 8], TAG_SMALLINT
    je .rev_type_error

    ; Get length of sequence via sq_length or ob_size
    mov rax, [r12 + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_as_sequence]
    test rcx, rcx
    jz .rev_try_ob_size
    mov rcx, [rcx + PySequenceMethods.sq_length]
    test rcx, rcx
    jz .rev_try_ob_size
    mov rdi, r12
    call rcx
    jmp .rev_have_len

.rev_try_ob_size:
    ; Fallback: read ob_size at +16
    mov rax, [r12 + PyVarObject.ob_size]

.rev_have_len:
    ; rax = length
    mov r13, rax             ; r13 = length
    dec r13                  ; it_index = length - 1

    ; INCREF the sequence
    INCREF r12

    ; Allocate ReversedIterObject
    mov edi, ITER_OBJ_SIZE
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel reversed_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + IT_FIELD1], r12       ; it_seq
    mov [rax + IT_FIELD2], r13       ; it_index

    pop r13
    pop r12
    pop rbx
    leave
    ret

.rev_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "reversed() takes exactly 1 argument"
    call raise_exception

.rev_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument to reversed() must be a sequence"
    call raise_exception
END_FUNC builtin_reversed

;; reversed_iternext(self) -> PyObject* or NULL
DEF_FUNC_LOCAL reversed_iternext
    push rbx

    mov rbx, rdi            ; self

    ; Check if index < 0
    mov rax, [rbx + IT_FIELD2]   ; it_index
    test rax, rax
    js .rev_exhausted

    ; Get item at index using sq_item
    mov rdi, [rbx + IT_FIELD1]   ; it_seq
    mov rsi, rax                 ; index
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_as_sequence]
    test rcx, rcx
    jz .rev_exhausted
    mov rcx, [rcx + PySequenceMethods.sq_item]
    test rcx, rcx
    jz .rev_exhausted
    call rcx
    ; rax = item (with INCREF from sq_item)

    ; Decrement index
    dec qword [rbx + IT_FIELD2]

    ; rdx = tag from sq_item (fat return)
    pop rbx
    leave
    ret

.rev_exhausted:
    RET_NULL
    pop rbx
    leave
    ret
END_FUNC reversed_iternext

;; reversed_dealloc(self)
DEF_FUNC_LOCAL reversed_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF the sequence
    mov rdi, [rbx + IT_FIELD1]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC reversed_dealloc

;; ============================================================================
;; SORTED
;; ============================================================================

;; builtin_sorted(args, nargs) -> PyListObject*
;; nargs=1: sorted(iterable) -> new sorted list
DEF_FUNC builtin_sorted
    push rbx
    push r12

    mov rbx, rdi            ; args
    mov r12, rsi            ; nargs

    cmp r12, 1
    jne .sorted_error

    ; Get iterator from args[0]
    mov rdi, [rbx]
    mov esi, [rbx + 8]         ; args[0] tag
    call get_iterator
    mov rbx, rax             ; rbx = iterator

    ; Create new empty list
    xor edi, edi             ; capacity = 0 (list_new defaults to 4)
    call list_new
    mov r12, rax             ; r12 = new list

    ; Iterate and append each item
.sorted_loop:
    mov rdi, rbx             ; iterator
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    test edx, edx
    jz .sorted_done_iter

    ; Append item to list (list_append does INCREF, and we own the ref from
    ; iternext, so we need to DECREF after append)
    push rdx                 ; save item tag
    push rax                 ; save item payload
    mov rdi, r12             ; list
    mov rsi, rax             ; item
    ; edx already has item tag from iternext
    call list_append
    pop rdi                  ; item payload
    pop rsi                  ; item tag
    DECREF_VAL rdi, rsi      ; DECREF the ref from iternext (list_append already INCREFed)

    jmp .sorted_loop

.sorted_done_iter:
    ; DECREF the iterator
    mov rdi, rbx
    call obj_decref

    ; Sort the list in-place using list_method_sort
    ; list_method_sort expects (args, nargs) where args[0] = self (16-byte fat slot)
    SPUSH_PTR r12            ; args[0] = list (fat arg)
    mov rdi, rsp             ; args ptr
    mov rsi, 1               ; nargs = 1
    call list_method_sort
    add rsp, 16

    ; DECREF the None returned by sort
    DECREF_VAL rax, rdx

    ; Return the list
    mov rax, r12
    mov edx, TAG_PTR

    pop r12
    pop rbx
    leave
    ret

.sorted_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "sorted() requires exactly 1 argument"
    call raise_exception
END_FUNC builtin_sorted

;; ============================================================================
;; Data section - type name strings and type objects
;; ============================================================================
section .data

enumerate_iter_name: db "enumerate", 0
zip_iter_name:       db "zip", 0
map_iter_name:       db "map", 0
filter_iter_name:    db "filter", 0
reversed_iter_name:  db "reversed", 0

; Enumerate iterator type
align 8
global enumerate_iter_type
enumerate_iter_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq enumerate_iter_name      ; tp_name
    dq ITER_OBJ_SIZE            ; tp_basicsize
    dq enumerate_dealloc        ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq itertools_iter_self      ; tp_iter (return self)
    dq enumerate_iternext       ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

; Zip iterator type
align 8
global zip_iter_type
zip_iter_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq zip_iter_name            ; tp_name
    dq ITER_OBJ_SIZE            ; tp_basicsize
    dq zip_dealloc              ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq itertools_iter_self      ; tp_iter
    dq zip_iternext             ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

; Map iterator type
align 8
global map_iter_type
map_iter_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq map_iter_name            ; tp_name
    dq ITER_OBJ_SIZE            ; tp_basicsize
    dq map_dealloc              ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq itertools_iter_self      ; tp_iter
    dq map_iternext             ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

; Filter iterator type
align 8
global filter_iter_type
filter_iter_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq filter_iter_name         ; tp_name
    dq ITER_OBJ_SIZE            ; tp_basicsize
    dq filter_dealloc           ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq itertools_iter_self      ; tp_iter
    dq filter_iternext          ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

; Reversed iterator type
align 8
global reversed_iter_type
reversed_iter_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq reversed_iter_name       ; tp_name
    dq ITER_OBJ_SIZE            ; tp_basicsize
    dq reversed_dealloc         ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq itertools_iter_self      ; tp_iter
    dq reversed_iternext        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
