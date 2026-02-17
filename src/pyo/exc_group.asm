; exc_group.asm - ExceptionGroup type implementation (PEP 654)
;
; Provides:
;   - exc_BaseExceptionGroup_type, exc_ExceptionGroup_type
;   - eg_new(type, msg_str, exc_tuple) -> PyExceptionGroupObject*
;   - eg_type_call(type, args, nargs) -> PyObject*  (constructor)
;   - eg_dealloc(eg)
;   - eg_str(eg) -> str
;   - eg_getattr(eg, name) -> PyObject* or NULL
;   - eg_split(eg, match_type) -> rax=match, rdx=rest
;   - eg_is_base_exception_group(obj) -> 0/1
;   - prep_reraise_star(orig, excs_list) -> PyObject*

%include "macros.inc"
%include "errcodes.inc"
%include "object.inc"
%include "types.inc"

extern ap_free
extern ap_malloc
extern ap_strcmp
extern exc_BaseException_type
extern exc_Exception_type
extern exc_getattr
extern exc_isinstance
extern exc_metatype
extern exc_repr
extern exc_str
extern exc_TypeError_type
extern exc_ValueError_type
extern list_append
extern list_new
extern list_type
extern none_singleton
extern obj_decref
extern obj_dealloc
extern obj_incref
extern raise_exception
extern str_from_cstr
extern tuple_new
extern tuple_type
extern type_getattr
extern type_repr
extern type_type

;; ============================================================================
;; eg_new(PyTypeObject *type, PyObject *msg_str, PyObject *exc_tuple)
;;   -> PyExceptionGroupObject*
;; Allocate and initialize an ExceptionGroup.
;; msg_str and exc_tuple are INCREFed. type is immortal.
;; ============================================================================
EGN_EG    equ 8
EGN_FRAME equ 8
DEF_FUNC eg_new, EGN_FRAME
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; type
    mov r12, rsi            ; msg_str
    mov r13, rdx            ; exc_tuple

    ; Allocate
    mov edi, PyExceptionGroupObject_size
    call ap_malloc

    ; Initialize fields
    mov qword [rax + PyExceptionGroupObject.ob_refcnt], 1
    mov [rax + PyExceptionGroupObject.ob_type], rbx
    mov [rax + PyExceptionGroupObject.exc_type], rbx
    mov [rax + PyExceptionGroupObject.exc_value], r12
    mov qword [rax + PyExceptionGroupObject.exc_tb], 0
    mov qword [rax + PyExceptionGroupObject.exc_context], 0
    mov qword [rax + PyExceptionGroupObject.exc_cause], 0
    mov [rax + PyExceptionGroupObject.eg_exceptions], r13

    ; INCREF msg_str
    test r12, r12
    jz .no_msg
    INCREF r12
.no_msg:

    ; INCREF exc_tuple
    test r13, r13
    jz .no_tup
    INCREF r13
.no_tup:

    ; Build args tuple: (msg, excs)
    mov [rbp - EGN_EG], rax
    mov edi, 2
    call tuple_new
    mov rcx, [rbp - EGN_EG]

    ; args[0] = msg_str
    test r12, r12
    jz .args_no_msg
    INCREF r12
    mov [rax + PyTupleObject.ob_item], r12
    mov qword [rax + PyTupleObject.ob_item + 8], TAG_PTR   ; slot 0 tag
    jmp .args_set_excs
.args_no_msg:
    ; Push None for msg if NULL
    lea rdx, [rel none_singleton]
    INCREF rdx
    mov [rax + PyTupleObject.ob_item], rdx
    mov qword [rax + PyTupleObject.ob_item + 8], TAG_PTR   ; slot 0 tag
.args_set_excs:
    ; args[1] = exc_tuple (fat slot 1 at offset +16)
    test r13, r13
    jz .args_no_excs
    INCREF r13
    mov [rax + PyTupleObject.ob_item + 16], r13
    mov qword [rax + PyTupleObject.ob_item + 24], TAG_PTR  ; slot 1 tag
    jmp .args_done
.args_no_excs:
    lea rdx, [rel none_singleton]
    INCREF rdx
    mov [rax + PyTupleObject.ob_item + 16], rdx
    mov qword [rax + PyTupleObject.ob_item + 24], TAG_PTR  ; slot 1 tag
.args_done:
    mov [rcx + PyExceptionGroupObject.exc_args], rax
    mov rax, rcx

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC eg_new

;; ============================================================================
;; eg_type_call(PyTypeObject *type, PyObject **args, int64_t nargs)
;;   -> PyObject*
;; Constructor: ExceptionGroup(msg, excs)
;; rdi = type, rsi = args array, rdx = nargs
;; ============================================================================
EGC_TYPE  equ 8
EGC_ARGS  equ 16
EGC_NARGS equ 24
EGC_FRAME equ 24
DEF_FUNC eg_type_call, EGC_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - EGC_TYPE], rdi
    mov [rbp - EGC_ARGS], rsi
    mov [rbp - EGC_NARGS], rdx

    ; Require exactly 2 args
    cmp rdx, 2
    jne .bad_nargs

    ; args[0] = msg (must be str), args[1] = excs (list or tuple)
    mov rbx, [rsi]          ; msg
    mov r12, [rsi + 8]      ; excs

    ; Check if excs is a tuple already
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .have_tuple

    ; Check if excs is a list — convert to tuple
    lea rcx, [rel list_type]
    cmp rax, rcx
    jne .bad_excs

    ; Convert list to tuple
    mov rcx, [r12 + PyListObject.ob_size]
    test rcx, rcx
    jz .empty_excs

    push rbx                ; save msg
    mov rdi, rcx
    call tuple_new
    mov r13, rax            ; r13 = new tuple
    pop rbx

    ; Copy list items to tuple
    mov rcx, [r12 + PyListObject.ob_size]
    mov rsi, [r12 + PyListObject.ob_item]
    xor edx, edx
.copy_list:
    mov rcx, [r12 + PyListObject.ob_size]  ; reload loop limit (clobbered below)
    cmp rdx, rcx
    jge .list_done
    mov rcx, rdx
    shl rcx, 4                ; index * 16
    mov rdi, [rsi + rcx]      ; list item payload (fat 16-byte stride)
    INCREF rdi
    mov [r13 + PyTupleObject.ob_item + rcx], rdi
    mov qword [r13 + PyTupleObject.ob_item + rcx + 8], TAG_PTR
    inc rdx
    jmp .copy_list
.list_done:
    mov r12, r13            ; r12 = exc_tuple (owned, refcnt=1)
    jmp .check_nonempty

.have_tuple:
    ; Tuple — INCREF since eg_new will INCREF again, we need our own ref
    INCREF r12
    jmp .check_nonempty

.check_nonempty:
    ; Validate: len(excs) > 0
    mov rcx, [r12 + PyTupleObject.ob_size]
    test rcx, rcx
    jz .empty_excs_decref

    ; Call eg_new(type, msg, exc_tuple)
    mov rdi, [rbp - EGC_TYPE]
    mov rsi, rbx            ; msg
    mov rdx, r12            ; exc_tuple
    call eg_new
    ; rax = new EG object

    ; DECREF our ref to exc_tuple (eg_new INCREFed it)
    push rax
    mov rdi, r12
    call obj_decref
    pop rax

    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bad_nargs:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "ExceptionGroup requires exactly 2 arguments"
    call raise_exception

.bad_excs:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "second argument must be a sequence of exceptions"
    call raise_exception

.empty_excs:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "second argument (exceptions) must be a non-empty sequence"
    call raise_exception

.empty_excs_decref:
    ; DECREF exc_tuple before raising
    push r12
    mov rdi, r12
    call obj_decref
    pop r12
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "second argument (exceptions) must be a non-empty sequence"
    call raise_exception

END_FUNC eg_type_call

;; ============================================================================
;; eg_dealloc(PyExceptionGroupObject *eg)
;; Free exception group and DECREF all fields.
;; ============================================================================
DEF_FUNC eg_dealloc
    push rbx
    mov rbx, rdi

    ; XDECREF exc_value
    mov rdi, [rbx + PyExceptionGroupObject.exc_value]
    test rdi, rdi
    jz .no_val
    call obj_decref
.no_val:

    ; XDECREF exc_tb
    mov rdi, [rbx + PyExceptionGroupObject.exc_tb]
    test rdi, rdi
    jz .no_tb
    call obj_decref
.no_tb:

    ; XDECREF exc_context
    mov rdi, [rbx + PyExceptionGroupObject.exc_context]
    test rdi, rdi
    jz .no_ctx
    call obj_decref
.no_ctx:

    ; XDECREF exc_cause
    mov rdi, [rbx + PyExceptionGroupObject.exc_cause]
    test rdi, rdi
    jz .no_cause
    call obj_decref
.no_cause:

    ; XDECREF exc_args
    mov rdi, [rbx + PyExceptionGroupObject.exc_args]
    test rdi, rdi
    jz .no_args
    call obj_decref
.no_args:

    ; XDECREF eg_exceptions
    mov rdi, [rbx + PyExceptionGroupObject.eg_exceptions]
    test rdi, rdi
    jz .no_excs
    call obj_decref
.no_excs:

    ; Free the object
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC eg_dealloc

;; ============================================================================
;; eg_str(PyExceptionGroupObject *eg) -> PyObject* (string)
;; Returns the message string (exc_value), like exc_str.
;; ============================================================================
DEF_FUNC_BARE eg_str
    jmp exc_str
END_FUNC eg_str

;; ============================================================================
;; eg_getattr(PyExceptionGroupObject *eg, PyStrObject *name) -> PyObject* or NULL
;; Handle: message, exceptions, args, __context__, __cause__, __traceback__
;; ============================================================================
DEF_FUNC eg_getattr
    push rbx
    push r12

    mov rbx, rdi            ; eg
    mov r12, rsi            ; name

    ; Check "message"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "message"
    call ap_strcmp
    test eax, eax
    jz .get_message

    ; Check "exceptions"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "exceptions"
    call ap_strcmp
    test eax, eax
    jz .get_exceptions

    ; Fall through to exc_getattr for args, __context__, __cause__, __traceback__
    mov rdi, rbx
    mov rsi, r12
    pop r12
    pop rbx
    leave
    jmp exc_getattr

.get_message:
    ; Return exc_value (message string)
    mov rax, [rbx + PyExceptionGroupObject.exc_value]
    test rax, rax
    jz .return_none
    INCREF rax
    pop r12
    pop rbx
    leave
    ret

.get_exceptions:
    ; Return eg_exceptions tuple
    mov rax, [rbx + PyExceptionGroupObject.eg_exceptions]
    test rax, rax
    jz .return_none
    INCREF rax
    pop r12
    pop rbx
    leave
    ret

.return_none:
    lea rax, [rel none_singleton]
    INCREF rax
    pop r12
    pop rbx
    leave
    ret
END_FUNC eg_getattr

;; ============================================================================
;; eg_split(PyExceptionGroupObject *eg, PyTypeObject *match_type)
;;   -> rax = match_eg (or NULL if none matched)
;;   -> rdx = rest_eg  (or NULL if all matched)
;; Flat partition of sub-exceptions by isinstance check.
;; ============================================================================
EGS_EG       equ 8
EGS_MTYPE    equ 16
EGS_MLIST    equ 24
EGS_RLIST    equ 32
EGS_IDX      equ 40
EGS_COUNT    equ 48
EGS_FRAME    equ 48
DEF_FUNC eg_split, EGS_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - EGS_EG], rdi
    mov [rbp - EGS_MTYPE], rsi

    ; Get exceptions tuple
    mov rax, [rdi + PyExceptionGroupObject.eg_exceptions]
    mov rcx, [rax + PyTupleObject.ob_size]
    mov [rbp - EGS_COUNT], rcx

    ; Create two lists for match and rest
    xor edi, edi
    call list_new
    mov [rbp - EGS_MLIST], rax

    xor edi, edi
    call list_new
    mov [rbp - EGS_RLIST], rax

    ; Iterate sub-exceptions
    mov qword [rbp - EGS_IDX], 0
.split_loop:
    mov rcx, [rbp - EGS_IDX]
    cmp rcx, [rbp - EGS_COUNT]
    jge .split_done

    ; Get exc = eg.eg_exceptions[i] (fat tuple: *16 stride)
    mov rax, [rbp - EGS_EG]
    mov rax, [rax + PyExceptionGroupObject.eg_exceptions]
    mov r8, rcx
    shl r8, 4
    mov rdi, [rax + PyTupleObject.ob_item + r8]

    ; exc_isinstance(exc, match_type)
    mov rsi, [rbp - EGS_MTYPE]
    call exc_isinstance
    test eax, eax
    jz .split_rest

    ; Match: append to match_list
    mov rcx, [rbp - EGS_IDX]
    mov rax, [rbp - EGS_EG]
    mov rax, [rax + PyExceptionGroupObject.eg_exceptions]
    mov r8, rcx
    shl r8, 4
    mov rsi, [rax + PyTupleObject.ob_item + r8]
    mov rdi, [rbp - EGS_MLIST]
    call list_append
    jmp .split_next

.split_rest:
    ; No match: append to rest_list
    mov rcx, [rbp - EGS_IDX]
    mov rax, [rbp - EGS_EG]
    mov rax, [rax + PyExceptionGroupObject.eg_exceptions]
    mov r8, rcx
    shl r8, 4
    mov rsi, [rax + PyTupleObject.ob_item + r8]
    mov rdi, [rbp - EGS_RLIST]
    call list_append

.split_next:
    inc qword [rbp - EGS_IDX]
    jmp .split_loop

.split_done:
    ; Build match EG if match_list non-empty, else NULL
    mov rax, [rbp - EGS_MLIST]
    mov rcx, [rax + PyListObject.ob_size]
    test rcx, rcx
    jz .no_match_eg

    ; Convert match_list to tuple
    push rcx
    mov rdi, rcx
    call tuple_new
    mov rbx, rax             ; rbx = match_tuple
    pop rcx
    mov rax, [rbp - EGS_MLIST]
    mov rsi, [rax + PyListObject.ob_item]
    xor edx, edx
.copy_match:
    cmp rdx, rcx
    jge .match_tuple_done
    push rcx
    mov rcx, rdx
    shl rcx, 4                ; index * 16
    mov rdi, [rsi + rcx]      ; list item payload (fat 16-byte stride)
    INCREF rdi
    mov [rbx + PyTupleObject.ob_item + rcx], rdi
    mov qword [rbx + PyTupleObject.ob_item + rcx + 8], TAG_PTR
    pop rcx
    inc rdx
    jmp .copy_match
.match_tuple_done:
    ; Create match EG with same message
    mov rax, [rbp - EGS_EG]
    mov rdi, [rax + PyExceptionGroupObject.ob_type]
    mov rsi, [rax + PyExceptionGroupObject.exc_value]
    mov rdx, rbx
    call eg_new
    mov r12, rax             ; r12 = match_eg
    ; DECREF match_tuple (eg_new INCREFed it)
    mov rdi, rbx
    call obj_decref
    jmp .build_rest

.no_match_eg:
    xor r12d, r12d           ; match_eg = NULL

.build_rest:
    ; Build rest EG if rest_list non-empty, else NULL
    mov rax, [rbp - EGS_RLIST]
    mov rcx, [rax + PyListObject.ob_size]
    test rcx, rcx
    jz .no_rest_eg

    ; Convert rest_list to tuple
    push rcx
    mov rdi, rcx
    call tuple_new
    mov rbx, rax             ; rbx = rest_tuple
    pop rcx
    mov rax, [rbp - EGS_RLIST]
    mov rsi, [rax + PyListObject.ob_item]
    xor edx, edx
.copy_rest:
    cmp rdx, rcx
    jge .rest_tuple_done
    push rcx
    mov rcx, rdx
    shl rcx, 4                ; index * 16
    mov rdi, [rsi + rcx]      ; list item payload (fat 16-byte stride)
    INCREF rdi
    mov [rbx + PyTupleObject.ob_item + rcx], rdi
    mov qword [rbx + PyTupleObject.ob_item + rcx + 8], TAG_PTR
    pop rcx
    inc rdx
    jmp .copy_rest
.rest_tuple_done:
    ; Create rest EG with same message
    mov rax, [rbp - EGS_EG]
    mov rdi, [rax + PyExceptionGroupObject.ob_type]
    mov rsi, [rax + PyExceptionGroupObject.exc_value]
    mov rdx, rbx
    call eg_new
    mov r13, rax             ; r13 = rest_eg
    ; DECREF rest_tuple (eg_new INCREFed it)
    mov rdi, rbx
    call obj_decref
    jmp .cleanup

.no_rest_eg:
    xor r13d, r13d           ; rest_eg = NULL

.cleanup:
    ; DECREF the two temp lists
    push r12
    push r13
    mov rdi, [rbp - EGS_MLIST]
    call obj_decref
    mov rdi, [rbp - EGS_RLIST]
    call obj_decref
    pop r13
    pop r12

    ; Return: rax = match_eg, rdx = rest_eg
    mov rax, r12
    mov rdx, r13

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC eg_split

;; ============================================================================
;; eg_is_base_exception_group(PyObject *obj) -> int (0/1)
;; Check if obj's type (or base chain) has tp_dealloc == eg_dealloc.
;; ============================================================================
DEF_FUNC_BARE eg_is_base_exception_group
    mov rax, [rdi + PyObject.ob_type]
    lea rdx, [rel eg_dealloc]
.walk:
    test rax, rax
    jz .no
    cmp [rax + PyTypeObject.tp_dealloc], rdx
    je .yes
    mov rax, [rax + PyTypeObject.tp_base]
    jmp .walk
.yes:
    mov eax, 1
    ret
.no:
    xor eax, eax
    ret
END_FUNC eg_is_base_exception_group

;; ============================================================================
;; prep_reraise_star(orig, excs_list) -> PyObject*
;; Filter Nones from excs_list, merge remaining into new EG if needed.
;; rdi = orig exception (owned ref), rsi = list of rest values (owned ref)
;; Returns: None (all handled) or exception to re-raise (new ref).
;; Consumes references to both orig and excs_list.
;; ============================================================================
PRS_ORIG  equ 8
PRS_LIST  equ 16
PRS_FIRST equ 24
PRS_FLAT  equ 32
PRS_FRAME equ 32
global prep_reraise_star
DEF_FUNC prep_reraise_star, PRS_FRAME
    push rbx
    push r12

    mov [rbp - PRS_ORIG], rdi
    mov [rbp - PRS_LIST], rsi

    ; Phase 1: Count non-None entries, find first non-None
    mov rcx, [rsi + PyListObject.ob_size]
    mov rsi, [rsi + PyListObject.ob_item]
    xor ebx, ebx                ; non-None count
    xor r12d, r12d              ; first non-None ptr
    xor edx, edx
    lea r8, [rel none_singleton]
.scan:
    cmp rdx, rcx
    jge .scan_done
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rdi, [rsi + rax]      ; list item payload (fat 16-byte stride)
    cmp rdi, r8
    je .scan_next
    inc ebx
    test r12, r12
    jnz .scan_next
    mov r12, rdi                ; save first non-None
.scan_next:
    inc rdx
    jmp .scan
.scan_done:
    mov [rbp - PRS_FIRST], r12

    test ebx, ebx
    jz .all_none
    cmp ebx, 1
    je .single

    ; Multiple non-None: check if orig is an ExceptionGroup
    mov rdi, [rbp - PRS_ORIG]
    call eg_is_base_exception_group
    test eax, eax
    jz .not_eg

    ; orig IS an EG: flatten sub-exceptions from all non-None rest items
    xor edi, edi
    call list_new
    mov [rbp - PRS_FLAT], rax

    ; Iterate excs_list, collect sub-exceptions from non-None items
    mov rax, [rbp - PRS_LIST]
    mov rcx, [rax + PyListObject.ob_size]
    xor edx, edx
.flat_loop:
    cmp rdx, rcx
    jge .flat_done
    mov rax, [rbp - PRS_LIST]
    mov rsi, [rax + PyListObject.ob_item]
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rdi, [rsi + rax]      ; list item payload (fat 16-byte stride)
    lea r8, [rel none_singleton]
    cmp rdi, r8
    je .flat_next

    ; non-None item — check if it's an EG
    push rdx
    push rcx
    call eg_is_base_exception_group
    pop rcx
    pop rdx
    test eax, eax
    jz .flat_append_single

    ; It's an EG: append each sub-exception from eg_exceptions tuple
    mov rax, [rbp - PRS_LIST]
    mov rsi, [rax + PyListObject.ob_item]
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rdi, [rsi + rax]      ; the EG payload (fat 16-byte stride)
    mov rax, [rdi + PyExceptionGroupObject.eg_exceptions]
    mov r8, [rax + PyTupleObject.ob_size]
    push rdx
    push rcx
    xor ecx, ecx
.sub_loop:
    cmp rcx, r8
    jge .sub_done
    push rcx
    push r8
    push rax
    mov r9, rcx
    shl r9, 4
    mov rsi, [rax + PyTupleObject.ob_item + r9]
    mov rdi, [rbp - PRS_FLAT]
    call list_append
    pop rax
    pop r8
    pop rcx
    inc rcx
    jmp .sub_loop
.sub_done:
    pop rcx
    pop rdx
    jmp .flat_next

.flat_append_single:
    ; Non-EG exception: append directly
    push rdx
    push rcx
    mov rax, [rbp - PRS_LIST]
    mov rsi, [rax + PyListObject.ob_item]
    mov rax, rdx
    shl rax, 4                ; index * 16
    mov rsi, [rsi + rax]      ; list item payload (fat 16-byte stride)
    mov rdi, [rbp - PRS_FLAT]
    call list_append
    pop rcx
    pop rdx

.flat_next:
    inc rdx
    jmp .flat_loop

.flat_done:
    ; Convert flat list to tuple
    mov rax, [rbp - PRS_FLAT]
    mov rcx, [rax + PyListObject.ob_size]
    push rcx
    mov edi, ecx
    call tuple_new
    mov rbx, rax                   ; rbx = new tuple
    pop rcx
    mov rax, [rbp - PRS_FLAT]
    mov rsi, [rax + PyListObject.ob_item]
    xor edx, edx
.copy_flat:
    cmp rdx, rcx
    jge .copy_flat_done
    push rcx
    mov rcx, rdx
    shl rcx, 4                ; index * 16
    mov rdi, [rsi + rcx]      ; list item payload (fat 16-byte stride)
    INCREF rdi
    mov [rbx + PyTupleObject.ob_item + rcx], rdi
    mov qword [rbx + PyTupleObject.ob_item + rcx + 8], TAG_PTR
    pop rcx
    inc rdx
    jmp .copy_flat
.copy_flat_done:

    ; Create new EG(orig.ob_type, orig.exc_value, flat_tuple)
    mov rax, [rbp - PRS_ORIG]
    mov rdi, [rax + PyExceptionGroupObject.ob_type]
    mov rsi, [rax + PyExceptionGroupObject.exc_value]
    mov rdx, rbx
    call eg_new
    mov r12, rax                   ; r12 = result

    ; DECREF the tuple (eg_new INCREFed it)
    mov rdi, rbx
    call obj_decref
    ; DECREF flat list
    mov rdi, [rbp - PRS_FLAT]
    call obj_decref

    mov rax, r12
    jmp .cleanup

.not_eg:
    ; orig is not EG — return first non-None
    mov rax, [rbp - PRS_FIRST]
    INCREF rax
    jmp .cleanup

.single:
    ; Single non-None: return it
    mov rax, r12
    INCREF rax
    jmp .cleanup

.all_none:
    ; All handled: return None
    lea rax, [rel none_singleton]
    INCREF rax

.cleanup:
    ; DECREF orig and excs_list
    push rax
    mov rdi, [rbp - PRS_LIST]
    call obj_decref
    mov rdi, [rbp - PRS_ORIG]
    call obj_decref
    pop rax

    pop r12
    pop rbx
    leave
    ret
END_FUNC prep_reraise_star

;; ============================================================================
;; Data section - ExceptionGroup type objects
;; ============================================================================
section .data

exc_name_BaseExceptionGroup: db "BaseExceptionGroup", 0
exc_name_ExceptionGroup:     db "ExceptionGroup", 0

; BaseExceptionGroup type — base = BaseException
align 8
global exc_BaseExceptionGroup_type
exc_BaseExceptionGroup_type:
    dq 1                        ; ob_refcnt (immortal)
    dq exc_metatype             ; ob_type
    dq exc_name_BaseExceptionGroup ; tp_name
    dq PyExceptionGroupObject_size ; tp_basicsize
    dq eg_dealloc               ; tp_dealloc
    dq exc_repr                 ; tp_repr
    dq eg_str                   ; tp_str
    dq 0                        ; tp_hash
    dq eg_type_call             ; tp_call — enables constructor
    dq eg_getattr               ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq exc_BaseException_type   ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

; ExceptionGroup type — base = BaseExceptionGroup (also inherits from Exception)
align 8
global exc_ExceptionGroup_type
exc_ExceptionGroup_type:
    dq 1                        ; ob_refcnt (immortal)
    dq exc_metatype             ; ob_type
    dq exc_name_ExceptionGroup  ; tp_name
    dq PyExceptionGroupObject_size ; tp_basicsize
    dq eg_dealloc               ; tp_dealloc
    dq exc_repr                 ; tp_repr
    dq eg_str                   ; tp_str
    dq 0                        ; tp_hash
    dq eg_type_call             ; tp_call — enables constructor
    dq eg_getattr               ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq exc_BaseExceptionGroup_type ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
