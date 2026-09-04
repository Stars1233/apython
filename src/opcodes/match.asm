; opcodes/match.asm - Structural pattern matching and the intrinsics
;
; The MATCH_* family behind `match`, CALL_INTRINSIC_1/2, and the handful of
; introspection opcodes that go with them: GET_LEN, SETUP_ANNOTATIONS,
; LOAD_LOCALS and the LOAD_FROM_DICT_OR_* pair.
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack top pointer (Value[], one 64-bit word per slot)
;   r14 = co_consts tuple data pointer (&tuple.ob_item[0])
;
; co_names is accessed via the LOAD_CO_NAMES macro (reads a global).
; ecx = opcode argument on entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

section .text

extern eval_dispatch
extern eval_saved_r13
extern fatal_error
extern async_gen_wrap_value
extern none_singleton
extern bool_true
extern bool_false
extern int_type
extern float_type
extern raise_exception
extern exc_RuntimeError_type
extern exc_SystemError_type
extern exc_StopIteration_type
extern exc_TypeError_type
extern current_exception
extern eval_exception_unwind
extern obj_decref
extern prep_reraise_star
extern tuple_new
extern list_type
extern eval_co_names
extern eval_saved_rbx
extern obj_dealloc
extern opcode_table
extern opcode_dispatch_table

;; Stack layout constants for binary_op / compare_op generic paths.
;; After 4 pushes: right, right_tag, left, left_tag
;; Offsets relative to rsp immediately after the 4 pushes.

;; Stack layout constants for op_build_string (DEF_FUNC, 16 bytes).

;; Stack layout constants for op_send (DEF_FUNC, 48 bytes).

;; Stack layout constants for op_match_keys (DEF_FUNC, 32 bytes).
MK_KEYS    equ 8
MK_SUBJ    equ 16
MK_VALS    equ 24
MK_NKEYS   equ 32
MK_FRAME   equ 32           ; + 0 pushes = 32

; --- moved to a sibling file by the split ---
extern op_send

section .text

;; ============================================================================
;; ci_unsupported(const char *prefix, uint32_t sel) -- raise
;; `SystemError: <prefix> N`.  Does not return.
;;
;; The message is built into a static buffer because RAISE takes a literal and
;; the selector is the one thing worth saying.  One process-wide buffer is
;; safe: it is written and handed to raise_exception before anything else can
;; run, and the exception copies it.
;; ============================================================================
section .bss
ci_msgbuf: resb 64
section .text
DEF_FUNC_BARE ci_unsupported
    lea r9, [rel ci_msgbuf]
.cu_copy:
    mov al, [rdi]
    test al, al
    jz .cu_space
    mov [r9], al
    inc rdi
    inc r9
    jmp .cu_copy
.cu_space:
    mov byte [r9], ' '
    inc r9
    mov r10d, 100                       ; the divisor, 100 then 10 then 1
    xor r11d, r11d                      ; a digit has been written
.cu_digit:
    mov eax, esi
    xor edx, edx
    div r10d                            ; eax = digit, edx = what is left
    test eax, eax
    jnz .cu_emit
    test r11d, r11d
    jnz .cu_emit
    cmp r10d, 1
    jne .cu_next
.cu_emit:
    add al, '0'
    mov [r9], al
    inc r9
    mov r11d, 1
.cu_next:
    mov esi, edx
    mov eax, r10d
    xor edx, edx
    mov ecx, 10
    div ecx
    mov r10d, eax
    test r10d, r10d
    jnz .cu_digit
    mov byte [r9], 0
    lea rdi, [rel exc_SystemError_type]
    lea rsi, [rel ci_msgbuf]
    call raise_exception
END_FUNC ci_unsupported

;; ============================================================================
;; op_call_intrinsic_1 - Call 1-arg intrinsic function
;;
;; CALL_INTRINSIC_1 (173): arg selects the intrinsic.
;; Pop TOS, call intrinsic, push result.
;; Key intrinsics:
;;   3 = INTRINSIC_STOPITERATION_ERROR (convert StopIteration to RuntimeError)
;;   5 = INTRINSIC_UNARY_POSITIVE (+x)
;;   6 = INTRINSIC_LIST_TO_TUPLE
;; ============================================================================
DEF_FUNC_BARE op_call_intrinsic_1
    cmp ecx, 2
    je .ci1_import_star
    cmp ecx, 3
    je .ci1_stopiter_error
    cmp ecx, 4
    je .ci1_async_gen_wrap
    cmp ecx, 5
    je .ci1_unary_positive
    cmp ecx, 6
    je .ci1_list_to_tuple

    ; Anything else is a real program reaching an intrinsic this interpreter
    ; does not have -- the PEP 695 family (7, 10, 11) is the live example, and
    ; a CPython .pyc holding `type X = int` used to kill the process here.
    ; A SystemError naming the selector is what CPython raises for an
    ; intrinsic it cannot dispatch, and it leaves the program able to report
    ; it.  TOS is left where it is: the unwinder empties the stack.
    CSTRING rdi, "CALL_INTRINSIC_1 selector"
    mov esi, ecx
    jmp ci_unsupported


;; INTRINSIC_IMPORT_STAR (arg=2): import * from module
;; TOS = module object. Copy module's exported names into frame.locals.
;; If module has __all__, use that list. Otherwise copy all non-underscore names.
IS_SAVED_RBX equ 8      ; the eval-loop bytecode IP, pushed on entry
IS_MOD      equ 16      ; module ptr
IS_MODDICT  equ 24      ; module's __dict__
IS_LOCALS   equ 32      ; frame's locals dict
IS_IDX      equ 40      ; loop index
IS_LIMIT    equ 48      ; capacity or count
IS_ITEMS    equ 56      ; items payload ptr (__all__ path)
IS_ITEM_TAGS equ 64     ; items tag ptr (__all__ path)
IS_FRAME    equ 64      ; sub rsp, 64 (after push rbp + push rbx = 72 total)
extern dict_get
extern dict_set
extern str_from_cstr_heap
extern obj_decref

.ci1_import_star:
    ; Pop module from TOS (r13 = eval value stack)
    VPOP_VAL rdi, rsi
    cmp rsi, TAG_PTR
    jne .is_done

    ; Set up stack frame
    push rbp
    mov rbp, rsp
    push rbx                          ; [rbp - IS_SAVED_RBX] = saved eval-loop bytecode IP
    sub rsp, IS_FRAME
    mov [rbp - IS_MOD], rdi           ; save module ptr

    ; Get mod_dict (+24)
    mov rax, [rdi + PyModuleObject.mod_dict]
    test rax, rax
    jz .is_done
    mov [rbp - IS_MODDICT], rax

    ; Get frame locals
    mov rax, [r12 + PyFrame.locals]
    test rax, rax
    jz .is_done
    mov [rbp - IS_LOCALS], rax

    ; Look up "__all__" in mod_dict
    CSTRING rdi, "__all__"
    call str_from_cstr_heap           ; rax = heap str (owned, refcnt=1)
    mov rbx, rax                      ; save key in callee-saved rbx
    mov rdi, [rbp - IS_MODDICT]
    mov rsi, rax                      ; key = "__all__"
    call dict_get                     ; → (rax=value, rdx=tag) or (0, 0)
    V_UNPACK rax, rdx           ; dict_get returns a Value
    ; Save result before DECREF of key
    push rax
    push rdx
    mov rdi, rbx                      ; DECREF "__all__" key string
    call obj_decref
    pop rdx                           ; value tag
    pop rax                           ; value payload

    test edx, edx                     ; TAG_NULL = not found?
    jz .is_no_all

    ;; --- __all__ found: rax = list/tuple ptr ---
    ; Determine items array and count
    mov rbx, rax                      ; rbx = __all__ object
    mov rcx, [rbx + PyVarObject.ob_size]  ; count (same offset for list/tuple)
    mov [rbp - IS_LIMIT], rcx

    ; Check if list or tuple
    extern list_type
    mov rax, [rbx + PyObject.ob_type]
    lea rdx, [rel list_type]
    cmp rax, rdx
    jne .is_all_tuple
    ; List: items = payload/tag arrays
    mov rax, [rbx + PyListObject.ob_item]
    jmp .is_all_have_items
.is_all_tuple:
    ; Tuple: items = payload/tag arrays
    mov rax, [rbx + PyTupleObject.ob_item]
.is_all_have_items:
    mov [rbp - IS_ITEMS], rax         ; save payloads ptr
    mov [rbp - IS_ITEM_TAGS], rdx     ; save tags ptr
    mov qword [rbp - IS_IDX], 0

.is_all_loop:
    mov rcx, [rbp - IS_IDX]
    cmp rcx, [rbp - IS_LIMIT]
    jge .is_done

    ; Get name from items[idx]
    mov rax, [rbp - IS_ITEMS]
    mov rdx, [rbp - IS_ITEM_TAGS]
    mov rsi, [rax + rcx * 8]          ; name payload

    ; Look up name in mod_dict
    mov rdi, [rbp - IS_MODDICT]
    ; rsi = key payload, rdx = key_tag (already set)
    call dict_get                     ; → (rax=value, rdx=value_tag) or (0, 0)
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jz .is_all_next                   ; name not in module dict → skip

    ; dict_set(locals, key=name, value, value_tag, key_tag)
    ; Reload name from items array (caller-saved regs clobbered by dict_get)
    mov r9, rax                       ; save value payload
    mov r10, rdx                      ; save value tag
    mov rcx, [rbp - IS_IDX]
    mov rax, [rbp - IS_ITEMS]
    mov rdx, [rbp - IS_ITEM_TAGS]
    mov rsi, [rax + rcx * 8]          ; name payload
    mov rdi, [rbp - IS_LOCALS]
    mov rdx, r9                       ; value payload
    mov rcx, r10                      ; value tag
    V_PACK rdx, rcx
    call dict_set

.is_all_next:
    inc qword [rbp - IS_IDX]
    jmp .is_all_loop

    ;; --- No __all__: walk dict entries, skip _-prefixed names ---
.is_no_all:
    mov rax, [rbp - IS_MODDICT]
    mov rcx, [rax + PyDictObject.capacity]
    mov [rbp - IS_LIMIT], rcx
    mov qword [rbp - IS_IDX], 0

.is_dict_loop:
    mov rcx, [rbp - IS_IDX]
    cmp rcx, [rbp - IS_LIMIT]
    jge .is_done

    ; Entry address: entries + idx * DICT_ENTRY_SIZE (40)
    mov rax, [rbp - IS_MODDICT]
    mov rsi, [rax + PyDictObject.entries]
    imul rcx, DICT_ENTRY_SIZE
    lea rbx, [rsi + rcx]              ; rbx = entry ptr (callee-saved)

    ; Skip an empty or tombstoned slot.  This tested r8 -- left over from the
    ; previous iteration since the key-tag load it guarded was removed -- so
    ; `from mod import *` bound nothing at all.
    mov rsi, [rbx + DictEntry.key]
    test rsi, rsi
    jz .is_dict_next
    V_UNPACK rsi, r8

    ; Skip names starting with '_'
    cmp r8d, TAG_PTR
    jne .is_dict_copy                 ; non-string → copy
    ; Heap string: check first data byte
    cmp byte [rsi + PyStrObject.data], '_'
    je .is_dict_next

.is_dict_copy:
    ; dict_set(locals, key Value, value Value)
    mov rdi, [rbp - IS_LOCALS]
    ; rsi = key Value (already set), value comes straight from the entry
    mov rdx, [rbx + DictEntry.value]
    call dict_set

.is_dict_next:
    inc qword [rbp - IS_IDX]
    jmp .is_dict_loop

.is_done:
    ; DECREF module
    mov rdi, [rbp - IS_MOD]
    call obj_decref

    ; Restore and return
    mov rbx, [rbp - IS_SAVED_RBX]                ; restore eval-loop bytecode IP
    leave                             ; mov rsp, rbp; pop rbp
    VPUSH_NONE
    DISPATCH

.ci1_async_gen_wrap:
    ; INTRINSIC_ASYNC_GEN_WRAP: box the value an async generator is about to
    ; yield, so ags_iternext can tell it from an `await` passing through the
    ; same YIELD_VALUE.  Without the box every awaited value is delivered to
    ; the consumer as though it were an item.
    VPOP rdi
    call async_gen_wrap_value
    VPUSH_PTR rax
    DISPATCH

.ci1_stopiter_error:
    ; INTRINSIC_STOPITERATION_ERROR: convert StopIteration to RuntimeError
    ; Only converts if exception IS StopIteration; otherwise re-raise as-is
    mov rax, [r13 - 8]            ; TOS payload (exception)
    test rax, rax
    jz .ci1_si_convert
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel exc_StopIteration_type]
    cmp rcx, rdx
    jne .ci1_si_reraise
.ci1_si_convert:
    ; Pop the exception, raise RuntimeError instead
    VPOP rdi
    DECREF_V rdi, rsi
    mov [rel eval_saved_r13], r13  ; update — popped and DECREF'd
    RAISE exc_RuntimeError_type, "generator raised StopIteration"
.ci1_si_reraise:
    ; Not StopIteration — pop from TOS, set as current_exception, re-raise
    VPOP_VAL rax, rsi              ; exception (ref transferred from stack)
    mov [rel eval_saved_r13], r13  ; update — popped and transferred
    mov rcx, [rel current_exception]
    mov [rel current_exception], rax
    cmp rcx, rax
    je .ci1_si_go
    test rcx, rcx
    jz .ci1_si_go
    push rax
    mov rdi, rcx
    call obj_decref
    pop rax
.ci1_si_go:
    ; This is a re-raise, so it adds no traceback entry -- the frame already
    ; recorded one where the exception was first raised.
    extern tb_suppress_frame
    mov byte [rel tb_suppress_frame], 1
    jmp eval_exception_unwind

.ci1_unary_positive:
    ; +x calls the type's nb_positive.  This used to only *test* the slot and
    ; leave the value alone, which was indistinguishable from identity for
    ; int and float -- but a user class defining __pos__ now has a real slot,
    ; and ignoring it returned the operand unchanged.
    mov rax, [r13 - 8]
    V_TEST_PTR rax, rcx
    ja .ci1_pos_done            ; an int or float immediate: +x is x
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_as_number]
    test rcx, rcx
    jz .ci1_pos_error
    mov rcx, [rcx + PyNumberMethods.nb_positive]
    test rcx, rcx
    jz .ci1_pos_error
    push rax
    push rax                    ; twice: keep rsp 16-byte aligned
    mov rdi, rax                ; a pointer is its own Value
    call rcx                    ; nb_positive returns a Value, or NULL
    test rax, rax
    jz .ci1_pos_declined        ; a slot may decline; storing the NULL put a
    mov [r13 - 8], rax          ; raw 0 on the value stack for later to find
    pop rdi
    pop rdi                     ; rdi = the operand, still owned
    DECREF_V rdi, rcx

.ci1_pos_done:
    DISPATCH

.ci1_pos_declined:
    pop rdi
    pop rdi
.ci1_pos_error:
    RAISE exc_TypeError_type, "bad operand type for unary +"

.ci1_list_to_tuple:
    ; Convert list to tuple
    VPOP_VAL rdi, rsi           ; rdi = list, rsi = tag
    cmp rsi, TAG_PTR
    jne .ci1_l2t_error
    push rdi                   ; save for DECREF

    ; Get list size and items
    mov rcx, [rdi + PyListObject.ob_size]
    mov rsi, [rdi + PyListObject.ob_item]
    push rcx
    push rsi
    push rdx

    ; Create tuple of same size
    mov rdi, rcx
    call tuple_new
    ; (tuple in rax — use stack, do NOT clobber rbx which is the bytecode IP)
    pop r11                    ; tags ptr
    pop rsi                    ; payloads ptr
    pop rcx                    ; count
    push rax                   ; save tuple

    ; Copy items from list to tuple, INCREF each
    xor edx, edx
.ci1_l2t_loop:
    cmp rdx, rcx
    jge .ci1_l2t_done
    push rcx
    push rdx
    push rsi

    mov rdi, [rsi + rdx * 8]        ; item Value
    mov rax, [rsp + 24]             ; tuple from stack
    mov r8, [rax + PyTupleObject.ob_item]
    mov [r8 + rdx * 8], rdi
    INCREF_V rdi, r9

    pop rsi
    pop rdx
    pop rcx
    inc rdx
    jmp .ci1_l2t_loop

.ci1_l2t_done:
    pop rax                    ; tuple
    VPUSH_PTR rax

    ; DECREF list
    pop rdi
    DECREF_REG rdi

    DISPATCH

.ci1_l2t_error:
    RAISE exc_TypeError_type, "list expected"
END_FUNC op_call_intrinsic_1

;; ============================================================================
;; op_get_len - Push len(TOS) without popping TOS
;;
;; Opcode 30: GET_LEN
;; Used by match statements: push len, keep original on stack.
;; ============================================================================

DEF_FUNC_BARE op_get_len
    ; PEEK TOS (don't pop)
    mov rdi, [r13 - 8]
    V_TEST_PTR rdi, rax
    ja .gl_error_nopop          ; an immediate has no len()
    push rdi                    ; save obj

    ; Get length
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .gl_try_mapping
    mov rax, [rax + PySequenceMethods.sq_length]
    test rax, rax
    jz .gl_try_mapping
    call rax
    jmp .gl_got_len

.gl_try_mapping:
    pop rdi
    push rdi
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .gl_error
    mov rax, [rax + PyMappingMethods.mp_length]
    test rax, rax
    jz .gl_error
    call rax

.gl_got_len:
    pop rdi                     ; discard saved obj
    ; Convert length (in rax) to SmallInt and push
    VPUSH_INT rax, r15
    DISPATCH

.gl_error:
    pop rdi
.gl_error_nopop:
    RAISE exc_TypeError_type, "object has no len()"
END_FUNC op_get_len

;; ============================================================================
;; op_setup_annotations - Create __annotations__ dict in locals
;;
;; Opcode 85: SETUP_ANNOTATIONS
;; ============================================================================
extern dict_new
extern dict_set

DEF_FUNC op_setup_annotations
    push rbx
    push r12                    ; save eval loop r12

    ; Check if locals dict exists
    mov rbx, [r12 + PyFrame.locals]
    test rbx, rbx
    jz .sa_done

    ; Create __annotations__ dict
    call dict_new
    mov r12, rax                ; r12 = new annotations dict (saved)

    ; Create key string (heap — dict key, DECREFed)
    extern str_from_cstr_heap
    CSTRING rdi, "__annotations__"
    call str_from_cstr_heap
    ; rax = key string

    ; dict_set(locals, key, value, value_tag)
    mov rdi, rbx                ; dict = locals
    mov rsi, rax                ; key = "__annotations__"
    mov rdx, r12                ; value = new annotations dict
    push rax                    ; save key for DECREF
    push rdx                    ; save value for DECREF
    call dict_set
    pop rdi
    call obj_decref             ; DECREF value (dict_set INCREFs)
    pop rdi
    call obj_decref             ; DECREF key

.sa_done:
    pop r12
    pop rbx
    pop rbp
    DISPATCH
END_FUNC op_setup_annotations

;; ============================================================================
;; op_load_locals - Push locals dict
;;
;; Opcode 87: LOAD_LOCALS
;; ============================================================================
DEF_FUNC_BARE op_load_locals
    mov rax, [r12 + PyFrame.locals]
    test rax, rax
    jz .ll_error
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
.ll_error:
    RAISE exc_RuntimeError_type, "no locals dict"
END_FUNC op_load_locals

;; ============================================================================
;; op_load_from_dict_or_globals - Load from dict on TOS, fallback to globals
;;
;; Opcode 175: LOAD_FROM_DICT_OR_GLOBALS
;; Used in class body comprehensions.
;; ============================================================================
extern dict_get

DEF_FUNC_BARE op_load_from_dict_or_globals
    ; ecx = name index (payload array: 8-byte stride)
    shl ecx, 3
    LOAD_CO_NAMES rsi
    mov rsi, [rsi + rcx]       ; name string
    push rsi

    ; Pop dict from TOS
    VPOP_VAL rdi, r8
    push rdi                    ; save dict
    cmp r8, TAG_PTR
    jne .lfdg_not_dict

    ; Try dict first
    mov rsi, [rsp + 8]         ; name
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .lfdg_found

    ; Try globals
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, [rsp + 8]         ; name
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .lfdg_found

    ; DECREF dict (owned ref from TOS) before builtins lookup
    pop rdi                     ; saved dict
    DECREF rdi
    pop rsi                     ; name

    ; Try builtins
    mov rdi, [r12 + PyFrame.builtins]
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .lfdg_found_no_pop

    ; Not found
    extern exc_NameError_type
    RAISE exc_NameError_type, "name not found"

.lfdg_found:
    ; INCREF result (borrowed ref) before DECREF dict
    INCREF_VAL rax, rdx
    ; Save result across DECREF
    push rax
    push rdx
    mov rdi, [rsp + 16]        ; saved dict (shifted by 2 pushes)
    DECREF rdi
    pop rdx
    pop rax
    add rsp, 16                 ; pop saved dict + name
    VPUSH_VAL rax, rdx
    DISPATCH

.lfdg_not_dict:
    ; Not a dict on TOS
    pop rdi
    pop rsi
    RAISE exc_TypeError_type, "dict expected"

.lfdg_found_no_pop:
    ; dict already DECREFed in builtins path
    INCREF_VAL rax, rdx
    VPUSH_VAL rax, rdx
    DISPATCH
END_FUNC op_load_from_dict_or_globals

;; ============================================================================
;; op_load_from_dict_or_deref - Load from dict on TOS, fallback to cell deref
;;
;; Opcode 176: LOAD_FROM_DICT_OR_DEREF
;; Used in class bodies that access closure variables directly (e.g. val = x).
;; Pop dict from TOS. Try dict[name] first. If not found, fall back to
;; loading through cell at localsplus[arg] (same as LOAD_DEREF).
;; ============================================================================
global op_load_from_dict_or_deref

LFDOD_DICT  equ 8
LFDOD_ARG   equ 16
LFDOD_FRAME equ 16          ; + 0 pushes = 16

DEF_FUNC op_load_from_dict_or_deref, LFDOD_FRAME
    mov [rbp - LFDOD_ARG], ecx    ; save arg (localsplus index)

    ; Get name from co_names (payload array: 8-byte stride)
    shl ecx, 3
    LOAD_CO_NAMES rsi
    mov rsi, [rsi + rcx]          ; name string

    ; Pop dict from TOS
    VPOP_VAL rdi, r8
    mov [rbp - LFDOD_DICT], rdi   ; save dict
    cmp r8, TAG_PTR
    jne .lfdod_error

    ; Try dict first (the name is a string, so it is already a Value)
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .lfdod_found

    ; Not in dict — fall back to cell deref (like LOAD_DEREF)
    mov ecx, [rbp - LFDOD_ARG]
    mov rax, [r12 + PyFrame.localsplus + rcx*8]  ; cell object
    test rax, rax
    jz .lfdod_error
    mov rax, [rax + PyCellObject.ob_ref]
    test rax, rax                 ; 0 means an empty cell
    jz .lfdod_error
    V_UNPACK rax, rdx

.lfdod_found:
    ; INCREF result (borrowed ref) before DECREF dict
    INCREF_VAL rax, rdx
    ; Save result across DECREF of owned dict ref
    push rax
    push rdx
    mov rdi, [rbp - LFDOD_DICT]
    DECREF rdi
    pop rdx
    pop rax
    VPUSH_VAL rax, rdx
    leave
    DISPATCH

.lfdod_error:
    RAISE exc_NameError_type, "free variable referenced before assignment"
END_FUNC op_load_from_dict_or_deref

;; ============================================================================
;; op_match_mapping - Check if TOS is a mapping type
;;
;; Opcode 31: MATCH_MAPPING
;; Push True if TOS is dict/mapping, False otherwise. Don't pop TOS.
;; ============================================================================
extern dict_type

DEF_FUNC_BARE op_match_mapping
    mov rdi, [r13 - 8]            ; peek TOS
    V_TEST_PTR rdi, rax
    ja .mm_false                   ; an immediate is not a mapping
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel dict_type]
    cmp rax, rcx
    je .mm_true

    ; Having mp_subscript is not enough: a list has one, so a list answered
    ; yes here and MATCH_KEYS then looked keys up in it and crashed.  CPython
    ; asks a type flag only real mappings carry; the nearest thing available is
    ; dict and its subclasses, so anything else that is subscriptable is
    ; rejected -- the same shape as MATCH_SEQUENCE excluding dict on its side.
    push rdi
    mov rdi, rax
    lea rsi, [rel dict_type]
    extern type_is_subtype
    call type_is_subtype
    pop rdi
    test eax, eax
    jz .mm_false
.mm_true:
    lea rax, [rel bool_true]
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
.mm_false:
    lea rax, [rel bool_false]
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_match_mapping

;; ============================================================================
;; op_match_sequence - Check if TOS is a sequence type
;;
;; Opcode 32: MATCH_SEQUENCE
;; Push True if TOS is list/tuple/sequence (not str/bytes/dict). Don't pop TOS.
;; ============================================================================
extern tuple_type
extern str_type
extern bytes_type

DEF_FUNC_BARE op_match_sequence
    mov rdi, [r13 - 8]            ; peek TOS
    V_TEST_PTR rdi, rax
    ja .ms_false                   ; an immediate is not a sequence
    mov rax, [rdi + PyObject.ob_type]
    ; Exclude str, bytes, dict
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .ms_false
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .ms_false
    lea rcx, [rel dict_type]
    cmp rax, rcx
    je .ms_false
    ; Check list or tuple type directly
    lea rcx, [rel list_type]
    cmp rax, rcx
    je .ms_true
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .ms_true
    ; Check tp_as_sequence with sq_item
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .ms_false
    mov rax, [rax + PySequenceMethods.sq_item]
    test rax, rax
    jz .ms_false
.ms_true:
    lea rax, [rel bool_true]
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
.ms_false:
    lea rax, [rel bool_false]
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_match_sequence

;; ============================================================================
;; op_match_keys - Match mapping keys
;;
;; Opcode 33: MATCH_KEYS
;; TOS = keys tuple, TOS1 = subject (mapping)
;; If all keys in tuple exist in subject, push tuple of values + True
;; Otherwise push False
;; ============================================================================
DEF_FUNC op_match_keys, MK_FRAME

    ; TOS = keys tuple, TOS1 = subject (16 bytes/slot)
    ; Peek at both — don't pop either! Push result on top.
    mov rax, [r13 - 8]            ; keys tuple (TOS)
    mov [rbp - MK_KEYS], rax
    mov rax, [r13 - 16]           ; subject (TOS1)
    mov [rbp - MK_SUBJ], rax

    ; Allocate values tuple
    mov rax, [rbp - MK_KEYS]
    mov rdi, [rax + PyTupleObject.ob_size]
    mov [rbp - MK_NKEYS], rdi     ; save nkeys
    call tuple_new
    mov [rbp - MK_VALS], rax      ; values tuple

    xor edx, edx                   ; index

.mk_loop:
    cmp rdx, [rbp - MK_NKEYS]
    jge .mk_success

    push rdx

    ; Get key
    mov rax, [rbp - MK_KEYS]
    mov rsi, [rax + PyTupleObject.ob_item]        ; payloads
    mov rsi, [rsi + rdx*8]                         ; key payload

    ; Look up in subject
    mov rdi, [rbp - MK_SUBJ]
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jz .mk_fail

    ; Save dict_get tag (rdx) before restoring loop index
    mov r9, rdx                 ; r9 = value tag from dict_get

    ; Store value in values tuple
    pop rdx
    push rdx
    INCREF_VAL rax, r9          ; tag-aware INCREF
    mov rcx, [rbp - MK_VALS]
    mov r8, [rcx + PyTupleObject.ob_item]         ; payloads
    V_PACK rax, r9
    mov [r8 + rdx * 8], rax

    pop rdx
    inc rdx
    jmp .mk_loop

.mk_success:
    ; Push values tuple on top (stack: subject, keys, values_tuple)
    mov rax, [rbp - MK_VALS]
    VPUSH_PTR rax
    jmp .mk_done

.mk_fail:
    pop rdx
    ; DECREF partial values tuple
    mov rdi, [rbp - MK_VALS]
    call obj_decref
    ; Push None on top to indicate failure (stack: subject, keys, None)
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax

.mk_done:
    leave
    DISPATCH
END_FUNC op_match_keys

;; ============================================================================
;; op_match_class - Structural pattern matching: match class
;;
;; Opcode 152: MATCH_CLASS
;; Stack before: subject(TOS2), class(TOS1), kw_attrs_tuple(TOS)
;; Arg (ecx) = npos (number of positional sub-patterns)
;; Stack after: attrs_tuple (success) or None (failure)
;; All 3 inputs consumed.
;; ============================================================================

;; Stack layout constants (MC_ prefix)
MC_SUBJ      equ 8
MC_CLASS     equ 16
MC_KWATTRS   equ 24
MC_NPOS      equ 32
MC_RESULT    equ 40
MC_MATCHARGS equ 48
MC_IDX       equ 56
MC_SUBJ_TAG  equ 64
MC_ORIGIN    equ 72   ; the subject's type, for the __match_args__ walk
MC_FRAME     equ 88         ; + 0 pushes = 88, not 16-aligned

extern str_type

DEF_FUNC op_match_class, MC_FRAME

    ; Pop all 3 inputs
    VPOP rax                        ; kw_attrs tuple (TOS)
    mov [rbp - MC_KWATTRS], rax
    VPOP rax                        ; class (TOS1)
    mov [rbp - MC_CLASS], rax
    VPOP_VAL rax, rdx               ; subject (TOS2) + tag
    mov [rbp - MC_SUBJ], rax
    mov [rbp - MC_SUBJ_TAG], rdx

    mov [rbp - MC_NPOS], rcx        ; save npos
    mov qword [rbp - MC_RESULT], 0  ; result tuple (NULL initially)
    mov qword [rbp - MC_MATCHARGS], 0  ; __match_args__ (NULL initially)

    ;; --- isinstance check ---
    ;; Get subject's type (SmallInt/None-aware)
    mov rax, [rbp - MC_SUBJ]
    cmp qword [rbp - MC_SUBJ_TAG], TAG_SMALLINT
    je .mc_smallint_type
    cmp qword [rbp - MC_SUBJ_TAG], TAG_FLOAT
    je .mc_float_type
    ; Everything else is a real pointer -- None included, since it is an
    ; ordinary heap singleton.  (The arm here used to be `jz .mc_none_type`,
    ; which tests the same flag as the `je` above it: the None arm was dead
    ; and a float subject fell into the dereference below.)
    mov rdx, [rax + PyObject.ob_type]
    jmp .mc_got_type

.mc_smallint_type:
    lea rdx, [rel int_type]
    jmp .mc_got_type

.mc_float_type:
    lea rdx, [rel float_type]

.mc_got_type:
    ; rdx = subject's type; the test is over its MRO, not its tp_base chain
    mov [rbp - MC_ORIGIN], rdx
    mov rdi, rdx
    mov rsi, [rbp - MC_CLASS]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jnz .mc_isinstance_ok
    ; Not an instance of class — fail
    jmp .mc_fail

.mc_isinstance_ok:
    ;; --- Get __match_args__ if npos > 0 ---
    mov rcx, [rbp - MC_NPOS]
    test rcx, rcx
    jz .mc_no_matchargs_needed

    ; Look up __match_args__ on the class via tp_dict chain
    mov r8, [rbp - MC_CLASS]       ; start at class
.mc_matchargs_walk:
    mov rdi, [r8 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .mc_matchargs_next_base

    ; Look up "__match_args__" in dict
    push r8
    push rdi                        ; save dict
    lea rdi, [rel .mc_matchargs_cstr]
    call str_from_cstr_heap
    mov rsi, rax                    ; rsi = "__match_args__" str obj
    pop rdi                         ; restore dict
    push rsi                        ; save string for DECREF
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rsi                         ; rsi = string to DECREF
    push rdx                        ; save dict_get tag
    push rax                        ; save dict_get payload
    mov rdi, rsi
    call obj_decref
    pop rax                         ; restore dict_get payload
    pop rdx                         ; restore dict_get tag
    pop r8                          ; restore type pointer

    test edx, edx
    jnz .mc_matchargs_found

.mc_matchargs_next_base:
    MRO_NEXT r8, [rbp - MC_ORIGIN]
    test r8, r8
    jnz .mc_matchargs_walk

    ; __match_args__ not found and npos > 0 — fail
    jmp .mc_fail

.mc_matchargs_found:
    ; rax = __match_args__ tuple (borrowed ref from dict_get)
    INCREF rax
    mov [rbp - MC_MATCHARGS], rax

    ; Verify length >= npos
    mov rcx, [rbp - MC_NPOS]
    mov rdx, [rax + PyTupleObject.ob_size]
    cmp rdx, rcx
    jl .mc_fail                     ; not enough match_args

.mc_no_matchargs_needed:
    ;; --- Allocate result tuple: npos + len(kw_attrs) ---
    mov rdi, [rbp - MC_NPOS]
    mov rax, [rbp - MC_KWATTRS]
    add rdi, [rax + PyTupleObject.ob_size]
    call tuple_new
    mov [rbp - MC_RESULT], rax

    ;; --- Positional loop: i=0..npos-1 ---
    mov qword [rbp - MC_IDX], 0
.mc_pos_loop:
    mov rcx, [rbp - MC_IDX]
    cmp rcx, [rbp - MC_NPOS]
    jge .mc_kw_start

    ; Get attr name from __match_args__[i]
    mov rax, [rbp - MC_MATCHARGS]
    mov rsi, [rax + PyTupleObject.ob_item]       ; payloads
    mov rsi, [rsi + rcx*8]                       ; name string

    ; Call subject's tp_getattr(subject, name)
    mov rdi, [rbp - MC_SUBJ]
    cmp qword [rbp - MC_SUBJ_TAG], TAG_SMALLINT
    je .mc_fail                     ; SmallInt has no attrs
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_getattr]
    test rax, rax
    jz .mc_fail
    call rax
    V_UNPACK rax, rdx           ; tp_getattr returns a Value
    test edx, edx
    jz .mc_fail                     ; attr not found

    ; Store in result tuple[i] (already owns a ref from tp_getattr, fat: *16)
    ; rdx = tag from tp_getattr (save before clobbering)
    mov r9, rdx                     ; save tag
    mov rcx, [rbp - MC_IDX]
    mov rdx, [rbp - MC_RESULT]
    mov r8, [rdx + PyTupleObject.ob_item]        ; payloads
    V_PACK rax, r9
    mov [r8 + rcx * 8], rax

    inc qword [rbp - MC_IDX]
    jmp .mc_pos_loop

.mc_kw_start:
    ;; --- Keyword loop: j=0..nkw-1 ---
    mov qword [rbp - MC_IDX], 0
.mc_kw_loop:
    mov rcx, [rbp - MC_IDX]
    mov rax, [rbp - MC_KWATTRS]
    cmp rcx, [rax + PyTupleObject.ob_size]
    jge .mc_success

    ; Get attr name from kw_attrs[j]
    mov r8, [rax + PyTupleObject.ob_item]        ; payloads
    mov rsi, [r8 + rcx*8]                        ; name string

    ; Call subject's tp_getattr(subject, name)
    mov rdi, [rbp - MC_SUBJ]
    cmp qword [rbp - MC_SUBJ_TAG], TAG_SMALLINT
    je .mc_fail                     ; SmallInt has no attrs
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_getattr]
    test rax, rax
    jz .mc_fail
    call rax
    V_UNPACK rax, rdx           ; tp_getattr returns a Value
    test edx, edx
    jz .mc_fail                     ; attr not found

    ; Store in result tuple[npos + j] (fat: *16)
    ; rdx = tag from tp_getattr (save before clobbering)
    mov r9, rdx                     ; save tag
    mov rcx, [rbp - MC_IDX]
    add rcx, [rbp - MC_NPOS]
    mov rdx, [rbp - MC_RESULT]
    mov r8, [rdx + PyTupleObject.ob_item]        ; payloads
    V_PACK rax, r9
    mov [r8 + rcx * 8], rax

    inc qword [rbp - MC_IDX]
    jmp .mc_kw_loop

.mc_success:
    ; Push result tuple, DECREF inputs
    mov rax, [rbp - MC_RESULT]
    push rax                        ; save result

    ; DECREF __match_args__ if held
    mov rdi, [rbp - MC_MATCHARGS]
    test rdi, rdi
    jz .mc_success_decref_inputs
    call obj_decref

.mc_success_decref_inputs:
    ; DECREF subject (tag-aware, may be SmallInt)
    mov rdi, [rbp - MC_SUBJ]
    mov rsi, [rbp - MC_SUBJ_TAG]
    DECREF_VAL rdi, rsi
    ; DECREF class
    mov rdi, [rbp - MC_CLASS]
    DECREF_REG rdi
    ; DECREF kw_attrs tuple
    mov rdi, [rbp - MC_KWATTRS]
    DECREF_REG rdi

    pop rax                         ; restore result tuple
    VPUSH_PTR rax
    leave
    DISPATCH

.mc_fail:
    ; DECREF partial result tuple if allocated (tuple_new zeros items,
    ; tuple_dealloc skips NULLs, so partial is safe)
    mov rdi, [rbp - MC_RESULT]
    test rdi, rdi
    jz .mc_fail_matchargs
    call obj_decref

.mc_fail_matchargs:
    ; XDECREF __match_args__ if held
    mov rdi, [rbp - MC_MATCHARGS]
    test rdi, rdi
    jz .mc_fail_decref_inputs
    call obj_decref

.mc_fail_decref_inputs:
    ; DECREF subject (tag-aware, may be SmallInt)
    mov rdi, [rbp - MC_SUBJ]
    mov rsi, [rbp - MC_SUBJ_TAG]
    DECREF_VAL rdi, rsi
    ; DECREF class
    mov rdi, [rbp - MC_CLASS]
    DECREF_REG rdi
    ; DECREF kw_attrs tuple
    mov rdi, [rbp - MC_KWATTRS]
    DECREF_REG rdi

    ; Push None
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax
    leave
    DISPATCH

section .rodata
.mc_matchargs_cstr: db "__match_args__", 0
section .text

END_FUNC op_match_class

;; ============================================================================
;; op_call_intrinsic_2 - Call 2-arg intrinsic function
;;
;; Opcode 174: CALL_INTRINSIC_2
;; arg selects the intrinsic.
;; TOS = arg2, TOS1 = arg1
;; Key intrinsics:
;;   1 = INTRINSIC_PREP_RERAISE - set __traceback__
;;   2 = INTRINSIC_TYPEVAR_WITH_BOUND
;;   3 = INTRINSIC_TYPEVAR_WITH_CONSTRAINTS
;;   4 = INTRINSIC_SET_FUNCTION_TYPE_PARAMS
;; ============================================================================
DEF_FUNC_BARE op_call_intrinsic_2
    cmp ecx, 1
    je .ci2_prep_reraise

    ; The rest are the PEP 695 constructors (2, 3, 4), which this interpreter
    ; does not have.  Dropping one operand and keeping the other silently
    ; produced a wrong TypeVar rather than an error; say so instead.
    CSTRING rdi, "CALL_INTRINSIC_2 selector"
    mov esi, ecx
    jmp ci_unsupported

.ci2_prep_reraise:
    ; INTRINSIC_PREP_RERAISE_STAR: TOS = exc_list, TOS1 = orig_exc
    ; Delegate to prep_reraise_star(orig, excs_list)
    VPOP_VAL rsi, rdx              ; rsi = exc_list
    VPOP_VAL rdi, rcx              ; rdi = orig_exc
    call prep_reraise_star
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_call_intrinsic_2
