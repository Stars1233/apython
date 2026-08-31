; builtins_obj.asm - The object, iteration and I/O builtins
;
; str(), id, hash, callable, iter/next, any/all/sum/min/max, the getattr
; family, globals/locals/vars/dir, input, open, ascii, format, aiter/anext,
; __import__ and breakpoint.  Same convention: name(args, nargs) -> PyObject*,
; args borrowed, return a new reference.

%include "macros.inc"
%include "object.inc"

; External symbols used
extern int_from_i64
extern int_add
extern ap_malloc
extern ap_free
extern str_from_cstr
extern str_from_cstr_heap
extern obj_str
extern obj_repr
extern obj_is_true
extern obj_incref
extern obj_decref
extern type_is_subtype
extern raise_exception
extern obj_getattr_opt
extern exc_new
extern current_exception
extern eval_exception_unwind
extern none_singleton
extern eval_saved_r12
extern obj_dealloc

extern float_type
extern str_type
extern bool_true
extern bool_false

extern exc_TypeError_type
extern exc_ValueError_type
extern exc_AttributeError_type
extern exc_StopIteration_type
extern gen_type
extern raise_exception_obj
extern list_new
extern list_append
extern list_contains
extern dict_tp_iter
extern type_type
extern user_type_metatype
extern ap_strcmp
extern dict_new

; ============================================================================
; 1. builtin_abs(args, nargs) - abs(x)
; ============================================================================

; --- moved to a sibling file by the split ---
extern builtin_abs

section .text

global str_type_call
DEF_FUNC_BARE str_type_call
    mov rdi, rsi
    mov rsi, rdx
    jmp builtin_str_fn
END_FUNC str_type_call

; ============================================================================
; 3. builtin_str_fn(args, nargs) - str(x)
; ============================================================================
DEF_FUNC builtin_str_fn

    test rsi, rsi
    jz .str_no_args

    cmp rsi, 1
    jne .str_error

    mov rdi, [rdi]             ; args[0]
    call obj_str
    leave
    ret

.str_no_args:
    CSTRING rdi, ""
    call str_from_cstr
    leave
    ret

.str_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "str() takes at most 1 argument"
    call raise_exception
END_FUNC builtin_str_fn

; ============================================================================
; 7. builtin_id(args, nargs) - id(x)
; ============================================================================
DEF_FUNC builtin_id

    cmp rsi, 1
    jne .id_error

    V_TEST_INT_M [rdi], rax            ; args[0] an int immediate?
    mov rdi, [rdi]                     ; args[0]
    jae .id_smallint

    call int_from_i64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.id_smallint:
    V_TO_I64 rdi
    call int_from_i64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.id_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "id() takes exactly one argument"
    call raise_exception
END_FUNC builtin_id

; ============================================================================
; 8. builtin_hash_fn(args, nargs) - hash(x)
; ============================================================================
DEF_FUNC builtin_hash_fn
    push rbx
    sub rsp, 8

    cmp rsi, 1
    jne .hash_nargs_error

    mov rbx, [rdi]

    V_TEST_INT_M [rdi], r11      ; args[0] an int immediate?
    jae .hash_smallint

    V_TEST_F64_M [rdi], r11      ; args[0] a float?
    jbe .hash_float

    ; Check non-pointer tags before dereference

    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_hash]
    test rcx, rcx
    jz .hash_type_error

    mov rdi, rbx
    mov edx, TAG_PTR            ; tp_hash forwards edx to int_unwrap
    call rcx
    mov rdi, rax
    call int_from_i64
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.hash_float:
    ; A float immediate: float_hash gives the PEP-correct int/float match
    extern float_hash
    mov rdi, rbx
    V_TO_F64 rdi
    call float_hash
    mov rdi, rax
    call int_from_i64
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.hash_smallint:
    extern int_hash_i64
    mov rdi, rbx
    V_TO_I64 rdi
    call int_hash_i64
.hash_si_ok:
    mov rdi, rax
    call int_from_i64
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.hash_bool:
    ; hash(True) = 1, hash(False) = 0 — payload is already 0 or 1
    mov rax, rbx
    jmp .hash_si_ok

.hash_none:
    ; hash(None) — CPython convention
    mov eax, 0x48ae2ce5
    jmp .hash_si_ok

.hash_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unhashable type"
    call raise_exception

.hash_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "hash() takes exactly one argument"
    call raise_exception
END_FUNC builtin_hash_fn

; ============================================================================
; 9. builtin_callable(args, nargs) - callable(x)
; ============================================================================
DEF_FUNC builtin_callable

    cmp rsi, 1
    jne .callable_error

    V_TEST_INT_M [rdi], r11      ; args[0] an int immediate?
    jae .callable_false
    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .callable_false
    mov rdi, [rdi]                     ; args[0] payload

    ; Get type of arg
    mov rax, [rdi + PyObject.ob_type]

    ; Check if arg is a type (all types are callable via type_call)
    extern type_type
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .callable_true
    extern exc_metatype
    lea rcx, [rel exc_metatype]
    cmp rax, rcx
    je .callable_true
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    je .callable_true

    ; For heaptypes (user-defined classes): tp_call is set only when __call__ defined
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jnz .callable_check_heaptype

    ; For built-in types: only known callable types return True
    ; (func, builtin_func, method have genuinely callable instances)
    extern func_type
    lea rcx, [rel func_type]
    cmp rax, rcx
    je .callable_true
    extern builtin_func_type
    lea rcx, [rel builtin_func_type]
    cmp rax, rcx
    je .callable_true
    extern method_type
    lea rcx, [rel method_type]
    cmp rax, rcx
    je .callable_true

    ; Not a known callable built-in type (dict, list, set, etc. instances → not callable)
    jmp .callable_false

.callable_check_heaptype:
    ; Heaptype instance: check if type has tp_call set (set when __call__ defined)
    mov rcx, [rax + PyTypeObject.tp_call]
    test rcx, rcx
    jnz .callable_true
    jmp .callable_false

.callable_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.callable_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.callable_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "callable() takes exactly one argument"
    call raise_exception
END_FUNC builtin_callable

; ============================================================================
; 10. builtin_iter_fn(args, nargs) - iter(x)
; ============================================================================
DEF_FUNC builtin_iter_fn

    cmp rsi, 1
    jne .iter_error

    mov rdi, [rdi]                     ; args[0]
    V_UNPACK rdi, rsi

    ; Use get_iterator which handles tp_iter, __iter__, __getitem__, validation
    extern get_iterator
    call get_iterator
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.iter_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "iter() takes exactly one argument"
    call raise_exception
END_FUNC builtin_iter_fn

; ============================================================================
; 11. builtin_next_fn(args, nargs) - next(x)
; ============================================================================
DEF_FUNC builtin_next_fn
    push rbx

    cmp rsi, 1
    je .next_one_arg
    cmp rsi, 2
    je .next_two_args
    jmp .next_error

.next_two_args:
    ; next(iterator, default) — return default on StopIteration
    push qword [rdi + 8]           ; save the default Value
    push qword [rdi + 8]           ; keep rsp 16-byte aligned
    ; Fall through to same iterator logic, but with default on stack
    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .next_two_type_error
    mov rdi, [rdi]                 ; args[0] = iterator
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .next_two_type_error
    call rax
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx
    jz .next_two_default           ; exhausted → return default
    ; Got value — discard saved default
    add rsp, 16
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.next_two_default:
    ; Clear any StopIteration exception
    extern current_exception
    mov rax, [rel current_exception]
    test rax, rax
    jz .next_two_ret_default
    push rdi
    mov rdi, rax
    mov qword [rel current_exception], 0
    call obj_decref
    pop rdi
.next_two_ret_default:
    pop rax                        ; the default Value
    add rsp, 8                     ; drop the alignment copy
    INCREF_V rax, rdx
    V_UNPACK rax, rdx              ; next() still returns a fat pair
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.next_two_type_error:
    add rsp, 16                    ; discard saved default
    jmp .next_type_error

.next_one_arg:

    V_TEST_INT_M [rdi], r11      ; args[0] an int immediate?
    jae .next_type_error
    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .next_type_error
    mov rdi, [rdi]                     ; args[0] payload

    mov rax, [rdi + PyObject.ob_type]
    mov rcx, rax                       ; save type
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jnz .next_have_iternext

    ; tp_iternext NULL — try __next__ on heaptype
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .next_type_error
    mov rbx, rdi                       ; save iterator
    extern dunder_next
    lea rsi, [rel dunder_next]
    extern dunder_call_1
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jnz .next_got_val                  ; got a value
    ; NULL from __next__ — check for StopIteration in current_exception
    extern current_exception
    mov rax, [rel current_exception]
    test rax, rax
    jz .next_stop                      ; no exception, clean exhaustion
    mov rcx, [rax + PyObject.ob_type]
    extern exc_StopIteration_type
    lea rdx, [rel exc_StopIteration_type]
    cmp rcx, rdx
    jne .next_got_val_null             ; other exception: leave it, propagate
    ; It's StopIteration — leave it as current_exception for raise
    jmp .next_stop
.next_got_val_null:
    ; Non-StopIteration exception set — return NULL to propagate
    RET_NULL
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.next_have_iternext:
    mov rbx, rdi                       ; save iterator for StopIteration.value
    call rax
    V_UNPACK rax, rdx                  ; tp_iternext returns a Value
    test edx, edx
    jz .next_stop

.next_got_val:
    ; tp_iternext / __next__ returns fat (rax=payload, rdx=tag)
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.next_stop:
    ; Check if iterator is a generator (has gi_return_value)
    lea rax, [rel gen_type]
    cmp [rbx + PyObject.ob_type], rax
    jne .next_stop_no_val
    ; Get generator's return value for StopIteration (already a Value)
    mov rsi, [rbx + PyGenObject.gi_return_value]
    test rsi, rsi
    jnz .next_stop_with_val
.next_stop_no_val:
    lea rsi, [rel none_singleton]
.next_stop_with_val:
    lea rdi, [rel exc_StopIteration_type]
    call exc_new
    mov rdi, rax
    call raise_exception_obj

.next_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not an iterator"
    call raise_exception

.next_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "next() takes exactly one argument"
    call raise_exception
END_FUNC builtin_next_fn

; ============================================================================
; 12. builtin_any(args, nargs) - any(iterable)
; ============================================================================
DEF_FUNC builtin_any
    push rbx
    push r12
    push r13
    push r14

    cmp rsi, 1
    jne .any_error

    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .any_type_error
    mov rdi, [rdi]
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iter]
    test rcx, rcx
    jz .any_type_error
    call rcx
    V_UNPACK rax, rdx           ; tp_call returns a Value
    mov rbx, rax

    mov rax, [rbx + PyObject.ob_type]
    mov r12, [rax + PyTypeObject.tp_iternext]

.any_loop:
    mov rdi, rbx
    call r12
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx             ; TAG_NULL = exhausted
    jz .any_false

    mov r13, rax               ; item payload
    mov r14, rdx               ; item tag

    mov rdi, r13
    mov rsi, r14
    V_PACK rdi, rsi
    call obj_is_true
    test eax, eax
    jnz .any_found_true

    ; Falsy: DECREF item and continue
    DECREF_VAL r13, r14
    jmp .any_loop

.any_found_true:
    DECREF_VAL r13, r14

.any_true:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.any_false:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.any_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.any_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "any() takes exactly one argument"
    call raise_exception
END_FUNC builtin_any

; ============================================================================
; 13. builtin_all(args, nargs) - all(iterable)
; ============================================================================
DEF_FUNC builtin_all
    push rbx
    push r12
    push r13
    push r14

    cmp rsi, 1
    jne .all_error

    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .all_type_error
    mov rdi, [rdi]
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iter]
    test rcx, rcx
    jz .all_type_error
    call rcx
    mov rbx, rax

    mov rax, [rbx + PyObject.ob_type]
    mov r12, [rax + PyTypeObject.tp_iternext]

.all_loop:
    mov rdi, rbx
    call r12
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx             ; TAG_NULL = exhausted
    jz .all_true

    mov r13, rax               ; item payload
    mov r14, rdx               ; item tag

    mov rdi, r13
    mov rsi, r14
    V_PACK rdi, rsi
    call obj_is_true
    test eax, eax
    jz .all_found_false

    ; Truthy: DECREF item and continue
    DECREF_VAL r13, r14
    jmp .all_loop

.all_found_false:
    DECREF_VAL r13, r14

.all_false:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.all_true:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.all_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.all_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "all() takes exactly one argument"
    call raise_exception
END_FUNC builtin_all

; ============================================================================
; 14. builtin_sum(args, nargs) - sum(iterable[, start])
; ============================================================================
DEF_FUNC builtin_sum
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov rbx, rdi
    mov r14, rsi

    cmp r14, 1
    jb .sum_error
    cmp r14, 2
    ja .sum_error

    cmp r14, 2
    je .sum_has_start
    xor eax, eax
    mov r13, rax
    mov qword [rsp], TAG_SMALLINT      ; accum_tag = SmallInt (0)
    jmp .sum_get_iter

.sum_has_start:
    mov r13, [rbx + 8]            ; args[1] payload (start value, 16-byte stride)
    V_UNPACK r13, rax       ; args[1]
    mov [rsp], eax                 ; accum_tag
    cmp eax, TAG_PTR
    jne .sum_get_iter
    inc qword [r13 + PyObject.ob_refcnt]

.sum_get_iter:
    V_TEST_PTR_M [rbx], r11      ; args[0] a pointer?
    ja .sum_type_error
    mov rdi, [rbx]                     ; args[0] payload (iterable)
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iter]
    test rcx, rcx
    jz .sum_type_error
    call rcx
    mov rbx, rax

    mov rax, [rbx + PyObject.ob_type]
    mov r12, [rax + PyTypeObject.tp_iternext]

.sum_loop:
    mov rdi, rbx
    call r12
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx
    jz .sum_done

    mov r14, rax                   ; item payload
    mov r15d, edx                  ; item tag

    mov rdi, r13                   ; accum payload
    mov rsi, r14                   ; item payload
    mov edx, [rsp]                 ; accum tag (left_tag)
    mov ecx, r15d                  ; item tag (right_tag)
    ; Use float_add if either operand is float, else int_add
    cmp edx, TAG_FLOAT
    je .sum_float_add
    cmp ecx, TAG_FLOAT
    je .sum_float_add
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call int_add
    V_UNPACK rax, rdx           ; int_add returns a Value
    jmp .sum_have_result
.sum_float_add:
    extern float_add
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call float_add
    V_UNPACK rax, rdx           ; float_add returns a Value
.sum_have_result:
    ; rax = new accum payload, edx = new accum tag

    ; Save new accum before DECREFs
    push rax
    push rdx

    ; DECREF old accumulator (tag at [rsp+16] = original [rsp])
    mov rdi, r13
    mov esi, [rsp + 16]
    DECREF_VAL rdi, rsi

    ; DECREF item
    mov rdi, r14
    mov esi, r15d
    DECREF_VAL rdi, rsi

    ; Restore new accum
    pop rdx                        ; new accum tag
    pop r13                        ; new accum payload
    mov [rsp], edx                 ; update accum_tag slot

    jmp .sum_loop

.sum_done:
    mov rdi, rbx
    call obj_decref
    mov rax, r13
    mov edx, [rsp]                 ; accum_tag
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sum_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.sum_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "sum expected 1-2 arguments"
    call raise_exception
END_FUNC builtin_sum

; ============================================================================
; 15-16. builtin_min / builtin_max
; ============================================================================
; Shared implementation: minmax_impl(args, nargs, cmp_op)
;   rdi = args, rsi = nargs, edx = cmp_op (PY_LT=0 for min, PY_GT=4 for max)
; Returns (rax=payload, rdx=tag)
;
; Stack layout:
;   [rsp + MM_TAG]     = current best tag (64-bit)
;   [rsp + MM_CMP_RES] = richcompare result ptr
;   [rsp + MM_ITER]    = iterator ptr (iter path only)
;   [rsp + MM_ITERNX]  = tp_iternext fn ptr (iter path only)
;   [rsp + MM_CMP_OP]  = comparison op (PY_LT or PY_GT)
MM_TAG     equ 8
MM_CMP_RES equ 16
MM_ITER    equ 24
MM_ITERNX  equ 32
MM_CMP_OP  equ 40
MM_FRAME   equ 48

DEF_FUNC_BARE builtin_min
    xor edx, edx                   ; PY_LT = 0
    jmp minmax_impl
END_FUNC builtin_min

DEF_FUNC_BARE builtin_max
    mov edx, PY_GT                 ; PY_GT = 4
    jmp minmax_impl
END_FUNC builtin_max

DEF_FUNC_LOCAL minmax_impl, MM_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - MM_CMP_OP], edx    ; save comparison op

    cmp rsi, 1
    jb .mm_error

    ; nargs == 1 → iterate the single argument
    cmp rsi, 1
    je .mm_iter_path

    ; --- Multi-arg path: min/max(a, b, ...) ---
    mov rbx, rdi                   ; args array
    mov r12, rsi                   ; nargs
    mov r13, 1                     ; index = 1

    mov r14, [rbx]                 ; args[0] = current best
    V_UNPACK r14, rax
    mov [rbp - MM_TAG], rax
    INCREF_VAL r14, rax

.mm_loop:
    cmp r13, r12
    jge .mm_done

    mov rax, r13
    shl rax, 3
    mov r15, [rbx + rax]          ; candidate Value
    V_UNPACK r15, rcx

    ; SmallInt fast path: both SmallInt?
    cmp qword [rbp - MM_TAG], TAG_SMALLINT
    jne .mm_slow
    cmp rcx, TAG_SMALLINT
    jne .mm_slow
    ; For min (PY_LT=0): update if candidate < best
    ; For max (PY_GT=4): update if candidate > best
    cmp dword [rbp - MM_CMP_OP], 0
    jne .mm_si_max
    cmp r15, r14
    jge .mm_no_update
    mov r14, r15
    jmp .mm_no_update
.mm_si_max:
    cmp r15, r14
    jle .mm_no_update
    mov r14, r15
    jmp .mm_no_update

.mm_slow:
    ; Resolve candidate type for richcompare
    mov r8, rcx                    ; save candidate tag
    test rcx, rcx
    js .mm_cand_ss
    cmp rcx, TAG_PTR
    jne .mm_try_float
    mov rdi, r15
    mov rax, [rdi + PyObject.ob_type]
    jmp .mm_have_type
.mm_cand_ss:
    lea rax, [rel str_type]
    jmp .mm_have_type
.mm_try_float:
    cmp rcx, TAG_FLOAT
    jne .mm_no_update
    lea rax, [rel float_type]
.mm_have_type:
    mov rcx, [rax + PyTypeObject.tp_richcompare]
    test rcx, rcx
    jz .mm_no_update

    ; tp_richcompare(candidate, best, cmp_op, cand_tag, best_tag)
    mov rdi, r15
    mov rsi, r14
    mov edx, [rbp - MM_CMP_OP]
    mov rax, rcx                   ; fn ptr
    mov rcx, r8                    ; left_tag = candidate tag
    mov r8, [rbp - MM_TAG]         ; right_tag = best tag
    V_PACK rdi, rcx             ; left  -> Value
    V_PACK rsi, r8              ; right -> Value
    call rax

    lea rcx, [rel bool_true]
    cmp rax, rcx
    mov [rbp - MM_CMP_RES], rax
    jne .mm_slow_no_upd

    ; Update best: DECREF old, set new = candidate
    mov rdi, r14
    mov rsi, [rbp - MM_TAG]
    DECREF_VAL rdi, rsi
    mov r14, r15
    mov rax, r13
    shl rax, 3
    mov rax, [rbx + rax]
    V_UNPACK rax, rcx
    mov [rbp - MM_TAG], rcx
    INCREF_VAL r14, rcx

    mov rdi, [rbp - MM_CMP_RES]
    call obj_decref
    jmp .mm_no_update

.mm_slow_no_upd:
    mov rdi, [rbp - MM_CMP_RES]
    call obj_decref

.mm_no_update:
    inc r13
    jmp .mm_loop

.mm_done:
    mov rax, r14
    mov rdx, [rbp - MM_TAG]
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

    ; --- Iterator path: min/max(iterable) ---
.mm_iter_path:
    ; Get iterator from args[0]
    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .mm_iter_type_error
    mov rdi, [rdi]                     ; iterable
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iter]
    test rcx, rcx
    jz .mm_iter_type_error
    call rcx
    test rax, rax
    jz .mm_iter_type_error
    mov [rbp - MM_ITER], rax
    mov rbx, [rax + PyObject.ob_type]
    mov rbx, [rbx + PyTypeObject.tp_iternext]
    mov [rbp - MM_ITERNX], rbx

    ; Get first element → initial best
    mov rdi, [rbp - MM_ITER]
    call rbx
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx
    jz .mm_iter_empty

    mov r14, rax                       ; best payload
    mov [rbp - MM_TAG], rdx            ; best tag
    INCREF_VAL r14, rdx
    DECREF_VAL rax, rdx                ; DECREF iternext result

.mm_iter_loop:
    mov rdi, [rbp - MM_ITER]
    call qword [rbp - MM_ITERNX]
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx
    jz .mm_iter_done

    mov r15, rax                       ; candidate payload
    mov r12, rdx                       ; candidate tag

    ; SmallInt fast path
    cmp qword [rbp - MM_TAG], TAG_SMALLINT
    jne .mm_iter_slow
    cmp r12, TAG_SMALLINT
    jne .mm_iter_slow
    cmp dword [rbp - MM_CMP_OP], 0
    jne .mm_iter_si_max
    cmp r15, r14
    jge .mm_iter_no_update
    mov r14, r15
    jmp .mm_iter_no_update
.mm_iter_si_max:
    cmp r15, r14
    jle .mm_iter_no_update
    mov r14, r15
    jmp .mm_iter_no_update

.mm_iter_slow:
    ; Resolve candidate type for richcompare
    mov rcx, r12
    test rcx, rcx
    js .mm_iter_cand_ss
    cmp rcx, TAG_PTR
    jne .mm_iter_try_float
    mov rdi, r15
    mov rax, [rdi + PyObject.ob_type]
    jmp .mm_iter_have_type
.mm_iter_cand_ss:
    lea rax, [rel str_type]
    jmp .mm_iter_have_type
.mm_iter_try_float:
    cmp rcx, TAG_FLOAT
    jne .mm_iter_no_update
    lea rax, [rel float_type]
.mm_iter_have_type:
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .mm_iter_no_update

    ; tp_richcompare(candidate, best, cmp_op, cand_tag, best_tag)
    mov rdi, r15
    mov rsi, r14
    mov edx, [rbp - MM_CMP_OP]
    mov rcx, r12
    mov r8, [rbp - MM_TAG]
    V_PACK rdi, rcx             ; left  -> Value
    V_PACK rsi, r8              ; right -> Value
    call rax
    V_UNPACK rax, rdx           ; tp_richcompare returns a Value

    lea rcx, [rel bool_true]
    cmp rax, rcx
    mov [rbp - MM_CMP_RES], rax
    jne .mm_iter_slow_no_upd

    ; Update best
    mov rdi, r14
    mov rsi, [rbp - MM_TAG]
    DECREF_VAL rdi, rsi
    mov r14, r15
    mov [rbp - MM_TAG], r12
    INCREF_VAL r14, r12

    mov rdi, [rbp - MM_CMP_RES]
    call obj_decref
    jmp .mm_iter_no_update

.mm_iter_slow_no_upd:
    mov rdi, [rbp - MM_CMP_RES]
    call obj_decref

.mm_iter_no_update:
    ; DECREF candidate
    DECREF_VAL r15, r12
    jmp .mm_iter_loop

.mm_iter_done:
    mov rdi, [rbp - MM_ITER]
    call obj_decref
    mov rax, r14
    mov rdx, [rbp - MM_TAG]
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.mm_iter_empty:
    mov rdi, [rbp - MM_ITER]
    call obj_decref
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "min()/max() arg is an empty sequence"
    call raise_exception

.mm_iter_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.mm_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "min()/max() expected at least 1 argument"
    call raise_exception
END_FUNC minmax_impl

; ============================================================================
; 17. builtin_getattr(args, nargs) - getattr(obj, name[, default])
; ============================================================================
DEF_FUNC builtin_getattr, 24
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi

    cmp r12, 2
    jb .getattr_error
    cmp r12, 3
    ja .getattr_error

    ; One lookup, with the descriptor protocol run over it -- the same answer
    ; `obj.name` gives.  Doing it by hand here is what made getattr() hand back
    ; the property object instead of calling it.
    DUNDER_EXC_SAVE [rbp - 8]
    mov rdi, [rbx]                 ; args[0], as a Value
    mov rsi, [rbx + 8]             ; args[1], the name
    call obj_getattr_opt
    test rax, rax
    jz .getattr_missing
    pop r12
    pop rbx
    leave
    ret

.getattr_missing:
    ; A getter that raised is not a missing attribute: returning the default,
    ; or an AttributeError, would bury the real exception.  current_exception
    ; is also whatever is being HANDLED, so it has to be compared against the
    ; snapshot rather than tested for emptiness.
    DUNDER_RAISED [rbp - 8], .getattr_check_type
.getattr_absent:
    cmp r12, 3
    jne .getattr_raise
    mov rax, [rbx + 16]            ; args[2], the default
    INCREF_V rax, rdx
    pop r12
    pop rbx
    leave
    ret

.getattr_check_type:
    ; Something was raised.  Only an AttributeError means "absent" -- that is
    ; the exception the __getattr__ and descriptor protocols use to say so, and
    ; the only one CPython swallows here.  Anything else is a real failure and
    ; returning the default would bury it.
    mov rax, [rel current_exception]
    test rax, rax
    jz .getattr_absent
    mov rdi, [rax + PyObject.ob_type]
    lea rsi, [rel exc_AttributeError_type]
    call type_is_subtype           ; a subclass of AttributeError counts too
    test eax, eax
    jz .getattr_propagate
    ; With no default to fall back on, CPython re-raises what was raised --
    ; __getattr__'s own message, not a manufactured one -- so leave it pending.
    cmp r12, 3
    jne .getattr_propagate
    ; Clear it before releasing, so a dealloc that re-enters cannot see a
    ; pointer that is about to go away.
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    call obj_decref
    jmp .getattr_absent

.getattr_propagate:
    xor eax, eax                   ; NULL with the exception pending: op_call unwinds
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret

.getattr_raise:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "object has no attribute"
    call raise_exception

.getattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "getattr expected 2 or 3 arguments"
    call raise_exception
END_FUNC builtin_getattr

; ============================================================================
; 18. builtin_hasattr(args, nargs) - hasattr(obj, name)
; ============================================================================
DEF_FUNC builtin_hasattr, 24
    push rbx
    mov rbx, rdi
    cmp rsi, 2
    jne .hasattr_error

    ; The same lookup getattr() does, so the two cannot disagree about what
    ; exists.  A getter that raises propagates rather than reading as absent,
    ; which is what CPython does for anything but an AttributeError.
    DUNDER_EXC_SAVE [rbp - 8]
    mov rdi, [rbx]
    mov rsi, [rbx + 8]
    call obj_getattr_opt
    test rax, rax
    jz .hasattr_missing
    mov rdi, rax
    DECREF_V rdi, rsi
    lea rax, [rel bool_true]
    INCREF rax
    pop rbx
    leave
    ret
.hasattr_missing:
    ; hasattr swallows a missing attribute, not a getter that blew up.
    DUNDER_RAISED [rbp - 8], .hasattr_check_type
.hasattr_false:
    lea rax, [rel bool_false]
    INCREF rax
    pop rbx
    leave
    ret
.hasattr_check_type:
    ; As getattr: only an AttributeError reads as absent.
    mov rax, [rel current_exception]
    test rax, rax
    jz .hasattr_false
    mov rdi, [rax + PyObject.ob_type]
    lea rsi, [rel exc_AttributeError_type]
    call type_is_subtype
    test eax, eax
    jz .hasattr_propagate
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    call obj_decref
    jmp .hasattr_false

.hasattr_propagate:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
.hasattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "hasattr expected 2 arguments"
    call raise_exception
END_FUNC builtin_hasattr

; ============================================================================
; 19. builtin_setattr(args, nargs) - setattr(obj, name, value)
; ============================================================================
DEF_FUNC builtin_setattr
    mov rbp, rsp
    push rbx
    sub rsp, 8

    cmp rsi, 3
    jne .setattr_error

    mov rbx, rdi

    V_TEST_PTR_M [rbx], r11      ; args[0] a pointer?
    ja .setattr_no_attr
    mov rdi, [rbx]                     ; args[0] payload (obj)

    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_setattr]
    test rax, rax
    jz .setattr_no_attr

    push rax                           ; save tp_setattr
    mov rdi, [rbx]                     ; args[0] payload (obj)
    mov rsi, [rbx + 8]               ; args[1] payload (name, 16-byte stride)
    mov rdx, [rbx + 16]               ; args[2] payload (value, 16-byte stride)
    pop rax                            ; restore tp_setattr
    call rax

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.setattr_no_attr:
    ; CPython reports the missing attribute, not a generic "unsupported":
    ; setattr(5, "x", 1) is AttributeError: 'int' object has no attribute 'x'.
    mov rdi, [rbx]
    mov rsi, [rbx + 8]
    mov edx, 1
    extern raise_no_attribute
    call raise_no_attribute

.setattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "setattr() takes exactly 3 arguments"
    call raise_exception
END_FUNC builtin_setattr

; ============================================================================
; builtin_globals(args, nargs) - globals()
; Returns the globals dict of the current frame.
; ============================================================================
DEF_FUNC builtin_globals
    cmp rsi, 0
    jne .globals_error

    ; Get current eval frame from saved r12
    mov rax, [rel eval_saved_r12]
    mov rax, [rax + PyFrame.globals]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.globals_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "globals() takes no arguments"
    call raise_exception
END_FUNC builtin_globals

; ============================================================================
; builtin_locals(args, nargs) - locals()
; Returns the locals dict if available, otherwise globals.
; In module scope, locals() == globals().
; In class body, returns the class dict.
; In function scope, returns globals as approximation.
; ============================================================================
DEF_FUNC builtin_locals
    cmp rsi, 0
    jne .locals_error

    ; Get current eval frame
    mov rax, [rel eval_saved_r12]
    ; Check if frame has a locals dict
    mov rcx, [rax + PyFrame.locals]
    test rcx, rcx
    jz .locals_use_globals
    ; Has locals dict - return it
    mov rax, rcx
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.locals_use_globals:
    ; No locals mapping means a function frame, whose locals live in the
    ; localsplus array.  Returning globals there was simply the wrong answer:
    ; locals() inside a function listed the module's names, not its own.
    mov rdi, rax
    extern frame_fast_to_locals
    call frame_fast_to_locals
    test rax, rax
    jz .locals_no_frame
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.locals_no_frame:
    mov rax, [rel eval_saved_r12]
    mov rax, [rax + PyFrame.globals]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.locals_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "locals() takes no arguments"
    call raise_exception
END_FUNC builtin_locals

; ============================================================================
; builtin_dir(args, nargs) - dir(obj)
; Returns list of attribute names from obj's type (and base chain) dicts.
; ============================================================================
DIR_LIST    equ 8       ; result list
DIR_OBJ     equ 16      ; the object
DIR_ORIGIN  equ 24      ; the type whose MRO is being listed
DIR_FRAME   equ 32

global builtin_dir
DEF_FUNC builtin_dir, DIR_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 1
    jne .dir_error

    mov rax, [rdi]           ; args[0]
    V_UNPACK rax, r12        ; r12 = obj tag
    mov [rbp - DIR_OBJ], rax

    ; Create result list
    xor edi, edi
    call list_new
    mov [rbp - DIR_LIST], rax
    mov rbx, rax            ; rbx = result list

    ; Determine which dict to iterate:
    ; If obj is a type (ob_type == type_type or user_type_metatype), iterate tp_dict
    ; Otherwise, iterate instance __dict__ (if any), then class dict
    mov rax, [rbp - DIR_OBJ]
    cmp r12d, TAG_SMALLINT
    je .dir_done            ; SmallInt: no attributes
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel type_type]
    cmp rcx, rdx
    je .dir_from_type
    lea rdx, [rel user_type_metatype]
    cmp rcx, rdx
    je .dir_from_type

    ; Instance: get its type, iterate its MRO
    mov r12, [rax + PyObject.ob_type]   ; r12 = type
    mov [rbp - DIR_ORIGIN], r12
    jmp .dir_walk_chain

.dir_from_type:
    ; obj IS a type: iterate its own MRO
    mov r12, [rbp - DIR_OBJ]
    mov [rbp - DIR_ORIGIN], r12

.dir_walk_chain:
    ; r12 = current type to get keys from
    test r12, r12
    jz .dir_done

    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .dir_next_base

    ; Iterate this dict's keys
    call dict_tp_iter
    mov r13, rax            ; r13 = iterator

.dir_iter_loop:
    mov rdi, r13
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .dir_iter_done
    mov rdi, r13
    call rax                ; tp_iternext(iter) -> key or NULL
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx
    jz .dir_iter_done

    ; Check if key already in result list (avoid duplicates from base classes)
    push rax                ; save key
    mov rdi, rbx            ; list
    mov rsi, rax            ; key
    V_PACK rsi, rdx         ; list_contains takes a Value
    call list_contains
    test eax, eax
    pop rax                 ; restore key
    jnz .dir_iter_loop      ; already present, skip

    ; Append key to result
    push rax
    mov rdi, rbx
    mov rsi, rax
    call list_append
    pop rdi
    call obj_decref
    jmp .dir_iter_loop

.dir_iter_done:
    ; DECREF iterator
    mov rdi, r13
    call obj_decref

.dir_next_base:
    MRO_NEXT r12, [rbp - DIR_ORIGIN]
    jmp .dir_walk_chain

.dir_done:
    mov rax, rbx            ; return result list
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.dir_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "dir() takes exactly 1 argument"
    call raise_exception
END_FUNC builtin_dir

section .rodata
fmt_dunder_name: db "__format__", 0

section .text

; ============================================================================
; builtin_input_fn(args, nargs) - input([prompt])
; 0 args: read line from stdin
; 1 arg: print prompt, then read line
; ============================================================================
extern sys_write
extern sys_read

global builtin_input_fn
INP_BUF_SIZE equ 4096
INP_FRAME equ INP_BUF_SIZE + 16  ; buffer + saved values
DEF_FUNC builtin_input_fn, INP_FRAME
    cmp rsi, 0
    je .inp_no_prompt
    cmp rsi, 1
    jne .inp_error

    ; Print prompt to stdout
    mov rax, [rdi]          ; args[0] = prompt
    V_TEST_PTR rax, rcx
    ja .inp_type_error
    ; Write prompt string data
    mov rsi, rax
    add rsi, PyStrObject.data  ; buf ptr
    mov rdx, [rax + PyStrObject.ob_size]  ; len
    mov edi, 1              ; stdout
    call sys_write

.inp_no_prompt:
    ; Read line from stdin into stack buffer
    lea rsi, [rbp - INP_FRAME]  ; buffer
    mov edx, INP_BUF_SIZE - 1
    xor edi, edi            ; stdin (fd=0)
    call sys_read
    ; rax = bytes read (or negative on error)
    test rax, rax
    jle .inp_empty

    ; Strip trailing newline
    lea rdi, [rbp - INP_FRAME]
    mov rcx, rax
    dec rcx
    cmp byte [rdi + rcx], 10  ; '\n'
    jne .inp_no_strip
    dec rax                  ; exclude newline
.inp_no_strip:
    ; Null-terminate
    mov byte [rdi + rax], 0

    ; Create string from buffer
    ; rdi already points to buffer
    call str_from_cstr
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.inp_empty:
    ; EOF or error: return empty string
    CSTRING rdi, ""
    call str_from_cstr
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.inp_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "input() takes at most 1 argument"
    call raise_exception

.inp_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "input() prompt must be a string"
    call raise_exception
END_FUNC builtin_input_fn

; ============================================================================
; builtin_open_fn(args, nargs) - open(filename[, mode])
; 1 arg: open for reading ('r')
; 2 args: open with specified mode
; ============================================================================
extern sys_open
extern file_type

global builtin_open_fn
OPN_FRAME equ 32
DEF_FUNC builtin_open_fn, OPN_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 1
    je .opn_default_mode
    cmp rsi, 2
    je .opn_with_mode
    jmp .opn_error

.opn_default_mode:
    ; filename only — default mode 'r'
    mov rax, [rdi]          ; args[0] = filename
    V_TEST_PTR rax, rcx
    ja .opn_type_error
    mov rbx, rax            ; save filename str

    ; Open read-only: O_RDONLY=0
    lea rdi, [rax + PyStrObject.data]
    xor esi, esi            ; flags = O_RDONLY
    xor edx, edx            ; mode = 0
    call sys_open
    mov r12, rax            ; fd
    test rax, rax
    js .opn_file_error

    ; Create default mode string "r" (heap — stored in PyFileObject struct field)
    CSTRING rdi, "r"
    call str_from_cstr_heap
    mov r13, rax            ; mode str
    jmp .opn_create_fileobj

.opn_with_mode:
    mov rax, [rdi]          ; args[0] = filename
    push rdi                ; save args ptr
    V_TEST_PTR rax, rcx
    ja .opn_type_error_pop
    mov rbx, rax            ; save filename str
    pop rdi                 ; restore args ptr

    mov rax, [rdi + 8]    ; mode str
    V_UNPACK rax, rcx       ; args[1]
    cmp rcx, TAG_PTR
    jne .opn_type_error
    mov r13, rax            ; save mode str

    ; Parse mode string
    lea rdi, [rax + PyStrObject.data]
    movzx eax, byte [rdi]

    cmp al, 'r'
    je .opn_mode_r
    cmp al, 'w'
    je .opn_mode_w
    cmp al, 'a'
    je .opn_mode_a
    cmp al, 'x'
    je .opn_mode_x
    jmp .opn_bad_mode

.opn_mode_r:
    ; Check for 'r+' or 'rb' or just 'r'
    movzx ecx, byte [rdi + 1]
    cmp cl, '+'
    je .opn_rw
    xor esi, esi            ; O_RDONLY
    jmp .opn_do_open

.opn_rw:
    mov esi, 2              ; O_RDWR
    jmp .opn_do_open

.opn_mode_w:
    mov esi, 0x241          ; O_WRONLY|O_CREAT|O_TRUNC (1|0x40|0x200)
    jmp .opn_do_open

.opn_mode_a:
    mov esi, 0x441          ; O_WRONLY|O_CREAT|O_APPEND (1|0x40|0x400)
    jmp .opn_do_open

.opn_mode_x:
    mov esi, 0xC1           ; O_WRONLY|O_CREAT|O_EXCL (1|0x40|0x80)
    jmp .opn_do_open

.opn_do_open:
    push rsi                ; save flags
    lea rdi, [rbx + PyStrObject.data]  ; filename cstr
    pop rsi                 ; restore flags
    mov edx, 0644o          ; default file permissions
    call sys_open
    mov r12, rax
    test rax, rax
    js .opn_file_error

    ; INCREF mode str (we're storing a ref)
    mov rdi, r13
    call obj_incref

.opn_create_fileobj:
    ; Allocate PyFileObject
    mov edi, PyFileObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel file_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyFileObject.file_fd], r12
    mov [rax + PyFileObject.file_name], rbx
    mov [rax + PyFileObject.file_mode], r13

    ; INCREF filename (storing ref)
    push rax
    mov rdi, rbx
    call obj_incref
    pop rax

    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.opn_file_error:
    extern exc_FileNotFoundError_type
    lea rdi, [rel exc_FileNotFoundError_type]
    CSTRING rsi, "No such file or directory"
    call raise_exception

.opn_bad_mode:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "invalid mode string"
    call raise_exception

.opn_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "open() takes 1 or 2 arguments"
    call raise_exception

.opn_type_error_pop:
    add rsp, 8                 ; discard saved args ptr
.opn_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "open() arguments must be strings"
    call raise_exception
END_FUNC builtin_open_fn

; ============================================================================
; builtin_ascii_fn(args, nargs) - ascii(obj)
; Like repr() but escapes non-ASCII characters to \xNN / \uNNNN / \UNNNNNNNN
; ============================================================================
global builtin_ascii_fn
AA_REPR   equ 8
AA_FRAME  equ 16
DEF_FUNC builtin_ascii_fn, AA_FRAME

    cmp rsi, 1
    jne .aa_nargs_error

    ; Get repr(obj)
    mov rdi, [rdi]            ; args[0]
    call obj_repr
    test edx, edx
    jz .aa_nargs_error

    ; Check if all chars are ASCII (fast path)
    mov [rbp - AA_REPR], rax
    lea rsi, [rax + PyStrObject.data]
    mov rcx, [rax + PyStrObject.ob_size]
    xor edx, edx              ; edx = index
.aa_check_loop:
    cmp edx, ecx
    jge .aa_all_ascii
    movzx eax, byte [rsi + rdx]
    cmp eax, 128
    jae .aa_need_escape
    inc edx
    jmp .aa_check_loop

.aa_all_ascii:
    ; Repr is all ASCII — just return it
    mov rax, [rbp - AA_REPR]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.aa_need_escape:
    ; We need to build a new string with non-ASCII chars escaped
    ; For simplicity, allocate a buffer big enough (4x original + 1)
    push rbx
    push r12
    push r13

    mov rbx, [rbp - AA_REPR]  ; rbx = repr str
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = original length
    lea rdi, [r12*4 + 8]      ; worst case: every char becomes \xNN (4 chars) + 8 NUL pad
    call ap_malloc
    mov r13, rax               ; r13 = output buffer

    lea rsi, [rbx + PyStrObject.data]  ; rsi = input
    mov rdi, r13               ; rdi = output
    xor ecx, ecx              ; ecx = input index
.aa_escape_loop:
    cmp ecx, r12d
    jge .aa_escape_done
    movzx eax, byte [rsi + rcx]
    cmp eax, 128
    jae .aa_do_escape
    mov byte [rdi], al
    inc rdi
    inc ecx
    jmp .aa_escape_loop

.aa_do_escape:
    ; Emit \xHH
    mov byte [rdi], '\'
    mov byte [rdi + 1], 'x'
    add rdi, 2
    ; High nibble
    mov edx, eax
    shr edx, 4
    cmp edx, 10
    jb .aa_hi_dec
    add edx, ('a' - 10)
    jmp .aa_hi_store
.aa_hi_dec:
    add edx, '0'
.aa_hi_store:
    mov byte [rdi], dl
    inc rdi
    ; Low nibble
    mov edx, eax
    and edx, 0xF
    cmp edx, 10
    jb .aa_lo_dec
    add edx, ('a' - 10)
    jmp .aa_lo_store
.aa_lo_dec:
    add edx, '0'
.aa_lo_store:
    mov byte [rdi], dl
    inc rdi
    inc ecx
    jmp .aa_escape_loop

.aa_escape_done:
    mov qword [rdi], 0         ; 8-byte zero-fill for ap_strcmp
    sub rdi, r13               ; rdi = output length

    ; Create string from buffer
    mov rdi, r13
    call str_from_cstr
    push rax
    push rdx

    ; Free buffer
    mov rdi, r13
    call ap_free

    ; DECREF original repr
    mov rdi, rbx
    call obj_decref

    pop rdx
    pop rax
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.aa_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "ascii() takes exactly one argument"
    call raise_exception
END_FUNC builtin_ascii_fn

; ============================================================================
; builtin_format_fn(args, nargs) - format(value[, format_spec])
; Calls value.__format__(format_spec) or str(value) if no __format__
; ============================================================================
global builtin_format_fn
FMT_OBJ     equ 8
FMT_SPEC    equ 24
FMT_FRAME   equ 32
DEF_FUNC builtin_format_fn, FMT_FRAME

    cmp rsi, 1
    jb .fmt_nargs_error
    cmp rsi, 2
    ja .fmt_nargs_error

    push rbx
    mov rbx, rsi               ; rbx = nargs

    ; Save obj.  args[0] is a Value; the slot below used to be filled from
    ; args[1] as though it were a separate tag, which is what the fat-value
    ; representation looked like -- so it held the format spec instead.
    mov rax, [rdi]
    mov [rbp - FMT_OBJ], rax

    ; Get format spec (empty string if not provided)
    cmp rbx, 2
    jb .fmt_empty_spec
    mov rax, [rdi + 8]
    mov [rbp - FMT_SPEC], rax
    jmp .fmt_have_spec

.fmt_empty_spec:
    CSTRING rdi, ""
    call str_from_cstr
    mov [rbp - FMT_SPEC], rax

.fmt_have_spec:
    ; A class defining __format__ formats itself.  This used to fall straight
    ; through to str(), so f"{obj:>5}" ignored both the spec and the method.
    V_TEST_PTR_M [rbp - FMT_OBJ], rcx
    ja .fmt_apply_spec
    mov rdi, [rbp - FMT_OBJ]
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .fmt_apply_spec
    mov rsi, [rbp - FMT_SPEC]
    extern dunder_call_2
    lea rdx, [rel fmt_dunder_name]
    mov ecx, TAG_PTR
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jnz .fmt_dunder_ok
    ; NULL means either "no __format__" or "__format__ raised"; falling
    ; through in the second case replaced the real exception.
    cmp qword [rel current_exception], 0
    jne .fmt_propagate
    jmp .fmt_apply_spec
.fmt_dunder_ok:
    ; If an empty spec was allocated here, release it.
    cmp rbx, 2
    jge .fmt_done
    push rax
    push rdx
    mov rdi, [rbp - FMT_SPEC]
    call obj_decref
    pop rdx
    pop rax
    jmp .fmt_done

.fmt_propagate:
    extern eval_exception_unwind
    leave
    jmp eval_exception_unwind

.fmt_apply_spec:
    ; Not a class with its own __format__: apply the spec directly.
    extern format_apply_spec
    mov rdi, [rbp - FMT_OBJ]
    mov rsi, [rbp - FMT_SPEC]
    call format_apply_spec
    V_UNPACK rax, rdx
    cmp rbx, 2
    jge .fmt_done
    push rax
    push rdx
    mov rdi, [rbp - FMT_SPEC]
    call obj_decref
    pop rdx
    pop rax
    jmp .fmt_done

.fmt_use_str:
    ; Just call str(value) — simple fallback
    mov rdi, [rbp - FMT_OBJ]    ; already a Value
    call obj_str
    ; If we allocated an empty spec, DECREF it
    cmp rbx, 2
    jge .fmt_done
    push rax
    push rdx
    mov rdi, [rbp - FMT_SPEC]
    call obj_decref
    pop rdx
    pop rax
.fmt_done:
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.fmt_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "format() takes 1 or 2 arguments"
    call raise_exception
END_FUNC builtin_format_fn

; ============================================================================
; builtin_vars_fn(args, nargs) - vars([obj])
; 0 args: returns frame locals dict (same as locals())
; 1 arg: returns obj.__dict__
; ============================================================================
extern eval_saved_r12
global builtin_vars_fn
VR_FRAME equ 8
DEF_FUNC builtin_vars_fn, VR_FRAME

    test rsi, rsi
    jz .vars_no_arg
    cmp rsi, 1
    jne .vars_nargs_error

    ; vars(obj): return obj.__dict__
    V_TEST_PTR_M [rdi], rax   ; args[0] a pointer?
    ja .vars_no_dict

    mov rdi, [rdi]            ; obj pointer
    ; Try inst_dict (user-defined class instances)
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_flags]
    test ecx, TYPE_FLAG_HEAPTYPE
    jz .vars_no_dict

    ; User instance: get inst_dict
    mov rax, [rdi + PyInstanceObject.inst_dict]
    test rax, rax
    jz .vars_empty_dict
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.vars_empty_dict:
    ; Instance has no dict yet — create empty dict
    call dict_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.vars_no_arg:
    ; Same as locals()
    extern builtin_locals
    xor edi, edi
    xor esi, esi
    call builtin_locals         ; already returns a Value
    leave
    ret

.vars_no_dict:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "vars() argument must have __dict__ attribute"
    call raise_exception

.vars_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "vars() takes at most 1 argument"
    call raise_exception
END_FUNC builtin_vars_fn

; ============================================================================
; builtin_delattr_fn(args, nargs) - delattr(obj, name)
; Calls tp_setattr(obj, name, NULL) to delete
; ============================================================================
global builtin_delattr_fn
DA2_OBJ   equ 8
DA2_NAME  equ 16
DA2_FRAME equ 24
DEF_FUNC builtin_delattr_fn, DA2_FRAME

    cmp rsi, 2
    jne .da2_nargs_error

    ; Get obj and name
    mov rax, [rdi]             ; obj payload
    mov [rbp - DA2_OBJ], rax
    mov rax, [rdi + 8]       ; name payload
    mov [rbp - DA2_NAME], rax

    ; obj must be a heap pointer
    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .da2_type_error

    ; Get type and tp_setattr
    mov rdi, [rbp - DA2_OBJ]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_setattr]
    test rax, rax
    jz .da2_attr_error

    ; Call tp_setattr(obj, name, NULL=delete)
    mov rdi, [rbp - DA2_OBJ]
    mov rsi, [rbp - DA2_NAME]
    xor edx, edx              ; value = NULL means delete
    xor ecx, ecx              ; value tag = TAG_NULL
    call rax

    ; Return None
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.da2_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "delattr: first argument must be an object"
    call raise_exception

.da2_attr_error:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "object does not support attribute deletion"
    call raise_exception

.da2_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "delattr() takes exactly 2 arguments"
    call raise_exception
END_FUNC builtin_delattr_fn

; ============================================================================
; builtin_aiter_fn(args, nargs) - aiter(async_iterable)
; Calls tp_iter on the async iterable
; ============================================================================
global builtin_aiter_fn
DEF_FUNC builtin_aiter_fn

    cmp rsi, 1
    jne .aiter_nargs_error

    ; Get the object
    mov rdi, [rdi]            ; args[0]

    ; Must be a heap pointer
    V_TEST_PTR rdi, rsi
    ja .aiter_type_error

    ; Call tp_iter
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .aiter_type_error

    call rax                   ; tp_iter returns rax=ptr only
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.aiter_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not an async iterable"
    call raise_exception

.aiter_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "aiter() takes exactly 1 argument"
    call raise_exception
END_FUNC builtin_aiter_fn

; ============================================================================
; builtin_anext_fn(args, nargs) - anext(async_iterator[, default])
; Calls tp_iternext; on StopAsyncIteration returns default
; ============================================================================
extern current_exception
global builtin_anext_fn
AN_ITER    equ 8
AN_DEFAULT equ 16
AN_DEFTAG  equ 24
AN_NARGS   equ 32
AN_FRAME   equ 40
DEF_FUNC builtin_anext_fn, AN_FRAME

    cmp rsi, 1
    jb .an_nargs_error
    cmp rsi, 2
    ja .an_nargs_error

    mov [rbp - AN_NARGS], rsi

    ; Save iterator
    mov rax, [rdi]
    mov [rbp - AN_ITER], rax

    ; Save default if present
    cmp rsi, 2
    jb .an_no_default
    mov rax, [rdi + 8]
    V_UNPACK rax, rdx
    mov [rbp - AN_DEFAULT], rax
    mov [rbp - AN_DEFTAG], rdx
    jmp .an_call

.an_no_default:
    mov qword [rbp - AN_DEFAULT], 0
    mov qword [rbp - AN_DEFTAG], 0

.an_call:
    ; Call tp_iternext
    mov rdi, [rbp - AN_ITER]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .an_type_error

    mov rdi, [rbp - AN_ITER]
    call rax                   ; returns (rax, edx)
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx
    jnz .an_got_value

    ; Got NULL — check if we have a default
    cmp qword [rbp - AN_NARGS], 2
    jb .an_reraise

    ; Clear the exception and return default
    mov qword [rel current_exception], 0
    mov rax, [rbp - AN_DEFAULT]
    mov edx, [rbp - AN_DEFTAG]
    INCREF_VAL rax, rdx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.an_got_value:
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.an_reraise:
    ; No default — let the exception propagate
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.an_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not an async iterator"
    call raise_exception

.an_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "anext() takes 1 or 2 arguments"
    call raise_exception
END_FUNC builtin_anext_fn

; ============================================================================
; builtin_import_fn(args, nargs) - __import__(name, ...)
; Wraps import_module(name_str, fromlist=NULL, level=0)
; Only uses first arg (name), ignores globals/locals/fromlist/level for now
; ============================================================================
extern import_module
global builtin_import_fn
DEF_FUNC builtin_import_fn

    cmp rsi, 1
    jb .imp_nargs_error

    ; Get name string
    mov rdi, [rdi]             ; name payload (must be str)
    xor esi, esi               ; fromlist = NULL
    xor edx, edx              ; level = 0
    call import_module
    ; import_module never sets rdx, so V_PACK was branching on whatever the
    ; last call left there -- re-encoding the module *pointer* as an int or a
    ; double, i.e. a Value whose payload is a pointer but whose tag says
    ; otherwise.  A module is a pointer; a pointer is its own Value.
    mov edx, TAG_PTR
    test rax, rax
    jnz .imp_done
    ; NULL means the module body raised and the exception is still pending.
    extern current_exception
    extern eval_exception_unwind
    cmp qword [rel current_exception], 0
    jne .imp_propagate
    lea rdi, [rel exc_ImportError_type]
    extern exc_ImportError_type
    CSTRING rsi, "import failed"
    call raise_exception
.imp_propagate:
    leave
    jmp eval_exception_unwind
.imp_done:
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.imp_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__import__() requires at least 1 argument"
    call raise_exception
END_FUNC builtin_import_fn

; ============================================================================
; builtin_breakpoint(args, nargs) - breakpoint() stub (no-op)
; ============================================================================
global builtin_breakpoint
DEF_FUNC_BARE builtin_breakpoint
    ; No-op: return None
    xor eax, eax
    RET_NONE
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC builtin_breakpoint
