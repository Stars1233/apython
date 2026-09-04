; opcodes/exc.asm - The exception opcodes
;
; PUSH_EXC_INFO, POP_EXCEPT, CHECK_EXC_MATCH, CHECK_EG_MATCH, RAISE_VARARGS and
; RERAISE.  They lived in eval.asm, which CLAUDE.md describes as the dispatch
; loop and nothing else; the unwinder they cooperate with -- and which does
; belong there -- is eval_exception_unwind.
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack top pointer (Value[], one 64-bit word per slot)
;   r14 = co_consts tuple data pointer (&tuple.ob_item[0])
;   r15 = free
;
; co_names is accessed via the LOAD_CO_NAMES macro (reads a global).
; ecx = opcode argument on entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.

%include "macros.inc"
%include "object.inc"

extern opcode_table
extern opcode_dispatch_table
extern eval_saved_rbx
extern eval_saved_r13

extern current_exception
extern handled_exception

extern eg_is_base_exception_group

extern eg_new

extern eg_split

extern eval_exception_unwind

extern exc_ExceptionGroup_type

extern exc_isinstance

extern exc_new

extern exc_set_context

extern exc_TypeError_type

extern none_singleton

extern obj_dealloc

extern obj_decref

extern raise_exception

extern tb_suppress_frame

extern tuple_new

section .text

;; ============================================================================
;; Exception-related opcode handlers (inline in eval.asm for access to globals)
;; ============================================================================

; op_push_exc_info (35) - Push exception info for try/except
; TOS has the exception. Save the handled-exception state, install the new one.
; Stack effect: exc -> prev_exc, exc
;
; The whole of the saved state is one word on the value stack, with None
; standing for "nothing was being handled".  The global's reference goes with
; it, so the stack slot owns it until POP_EXCEPT hands it back -- which is why
; a generator suspended inside an except block keeps the caller's exception
; alive on its own stack, and why frame_free releasing that stack is what
; releases it if the generator never resumes.
DEF_FUNC_BARE op_push_exc_info
    ; TOS = new exception
    VPOP rax                 ; rax = new exception

    ; Push the previous handled_exception (or None if NULL)
    mov rdx, [rel handled_exception]
    test rdx, rdx
    jnz .have_prev
    lea rdx, [rel none_singleton]
    INCREF rdx
.have_prev:
    VPUSH_PTR rdx            ; push prev_exc

    ; Set new exception as handled and push it too
    ; INCREF for the value stack copy
    INCREF rax
    mov [rel handled_exception], rax
    VPUSH_PTR rax            ; push new exc

    DISPATCH
END_FUNC op_push_exc_info

; op_pop_except (89) - Restore the previous handled-exception state
; TOS = the exception to restore
DEF_FUNC_BARE op_pop_except
    VPOP rax                 ; rax = exception to restore

    ; XDECREF old handled_exception
    push rax
    mov rdi, [rel handled_exception]
    test rdi, rdi
    jz .no_old
    call obj_decref
.no_old:
    pop rax

    ; Set restored exception as handled (or NULL if None)
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    jne .set_exc
    ; It's None - set handled to NULL and DECREF the None
    mov qword [rel handled_exception], 0
    DECREF rax
    DISPATCH
.set_exc:
    mov [rel handled_exception], rax
    DISPATCH
END_FUNC op_pop_except

; op_check_exc_match (36) - Check if exception matches a type
; TOS = type to match against, TOS1 = exception
; Push True/False, don't pop the exception
DEF_FUNC_BARE op_check_exc_match
    VPOP rsi                 ; rsi = type to match
    VPEEK rdi                ; rdi = exception (don't pop)

    ; Save type for DECREF
    push rsi

    ; Call exc_isinstance(exc, type)
    call exc_isinstance
    ; eax = 0 or 1

    ; DECREF the type
    push rax
    mov rdi, [rsp + 8]
    call obj_decref
    pop rax
    add rsp, 8

    ; Push bool result
    test eax, eax
    jz .no_match
    extern bool_true
    lea rax, [rel bool_true]
    jmp .push_result
.no_match:
    extern bool_false
    lea rax, [rel bool_false]
.push_result:
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_check_exc_match

;; op_check_eg_match (37) - Check exception group match for except*
;; Stack in:  [..., exc_value, match_type]
;; On match:  [..., rest_or_None, match_eg]  (pop exc_value, push rest, push match)
;; No match:  [..., exc_value, None]          (keep exc_value, push None)
;;
;; Cases:
;; 1. exc_value isinstance match_type AND is ExceptionGroup → eg_split
;; 2. exc_value isinstance match_type AND is NOT ExceptionGroup → wrap in EG, rest=None
;; 3. exc_value is ExceptionGroup but NOT isinstance → eg_split (may return NULL match)
;; 4. No match at all → push None

CEM_EXC    equ 8
CEM_MTYPE  equ 16
CEM_MATCH  equ 24
CEM_REST   equ 32
CEM_TMP1   equ 40
CEM_TMP2   equ 48
CEM_FRAME  equ 48           ; + 0 pushes = 48
DEF_FUNC op_check_eg_match, CEM_FRAME

    VPOP rsi                 ; rsi = match_type
    VPEEK rdi                ; rdi = exc_value (don't pop yet)
    mov [rbp - CEM_EXC], rdi
    mov [rbp - CEM_MTYPE], rsi

    ; Check if exc_value is None → no match
    lea rax, [rel none_singleton]
    cmp rdi, rax
    je .cem_no_match

    ; Case 1/2: isinstance(exc_value, match_type)?
    ; rdi = exc, rsi = type already set
    call exc_isinstance
    test eax, eax
    jz .cem_check_group_split

    ; Match! Check if exc_value is an ExceptionGroup
    mov rdi, [rbp - CEM_EXC]
    call eg_is_base_exception_group
    test eax, eax
    jnz .cem_full_group_match

    ; Case 2: Naked exception matches — wrap in ExceptionGroup
    ; Create a 1-element tuple containing the exception
    mov edi, 1
    call tuple_new
    mov [rbp - CEM_TMP1], rax ; TMP1 = tuple
    mov rcx, [rbp - CEM_EXC]
    INCREF rcx
    mov rdx, [rax + PyTupleObject.ob_item]
    mov [rdx], rcx

    ; Create empty message string (heap — stored in exception struct)
    extern str_from_cstr_heap
    CSTRING rdi, ""
    call str_from_cstr_heap
    mov [rbp - CEM_TMP2], rax ; TMP2 = empty msg str

    ; eg_new(ExceptionGroup_type, empty_str, tuple)
    lea rdi, [rel exc_ExceptionGroup_type]
    mov rsi, [rbp - CEM_TMP2]
    mov rdx, [rbp - CEM_TMP1]
    call eg_new
    mov [rbp - CEM_MATCH], rax  ; match_eg

    ; DECREF temp empty str (eg_new INCREFed it)
    mov rdi, [rbp - CEM_TMP2]
    call obj_decref
    ; DECREF temp tuple (eg_new INCREFed it)
    mov rdi, [rbp - CEM_TMP1]
    call obj_decref

    ; Pop exc_value from stack, push None (rest), push match_eg
    VPOP rdi                 ; pop exc_value
    call obj_decref

    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax            ; push rest = None

    mov rax, [rbp - CEM_MATCH]
    VPUSH_PTR rax            ; push match_eg (owns ref from eg_new)

    ; DECREF match_type
    mov rdi, [rbp - CEM_MTYPE]
    call obj_decref

    leave
    DISPATCH

.cem_full_group_match:
    ; Case 1: exc_value is ExceptionGroup and isinstance matches entirely
    ; Do eg_split to separate matching from non-matching
    mov rdi, [rbp - CEM_EXC]
    mov rsi, [rbp - CEM_MTYPE]
    call eg_split
    ; rax = match_eg (or NULL), rdx = rest_eg (or NULL)
    mov [rbp - CEM_MATCH], rax
    mov [rbp - CEM_REST], rdx

    ; Pop exc_value, push rest, push match
    VPOP rdi
    call obj_decref

    ; Push rest (or None if NULL)
    mov rax, [rbp - CEM_REST]
    test rax, rax
    jnz .cem_push_rest
    lea rax, [rel none_singleton]
    INCREF rax
.cem_push_rest:
    VPUSH_PTR rax

    ; Push match (or None if NULL — shouldn't happen since isinstance matched)
    mov rax, [rbp - CEM_MATCH]
    test rax, rax
    jnz .cem_push_match
    lea rax, [rel none_singleton]
    INCREF rax
.cem_push_match:
    VPUSH_PTR rax

    ; DECREF match_type
    mov rdi, [rbp - CEM_MTYPE]
    call obj_decref

    leave
    DISPATCH

.cem_check_group_split:
    ; Not a direct isinstance match. Check if exc_value is an ExceptionGroup
    ; and split by match_type.
    mov rdi, [rbp - CEM_EXC]
    call eg_is_base_exception_group
    test eax, eax
    jz .cem_no_match

    ; It IS an ExceptionGroup — split it
    mov rdi, [rbp - CEM_EXC]
    mov rsi, [rbp - CEM_MTYPE]
    call eg_split
    ; rax = match_eg (or NULL), rdx = rest_eg (or NULL)
    mov [rbp - CEM_MATCH], rax
    mov [rbp - CEM_REST], rdx

    ; If match is NULL, no match at all
    test rax, rax
    jz .cem_split_no_match

    ; Pop exc_value, push rest, push match
    VPOP rdi
    call obj_decref

    ; Push rest (or None if NULL)
    mov rax, [rbp - CEM_REST]
    test rax, rax
    jnz .cem_split_push_rest
    lea rax, [rel none_singleton]
    INCREF rax
.cem_split_push_rest:
    VPUSH_PTR rax

    ; Push match
    mov rax, [rbp - CEM_MATCH]
    VPUSH_PTR rax

    ; DECREF match_type
    mov rdi, [rbp - CEM_MTYPE]
    call obj_decref

    leave
    DISPATCH

.cem_split_no_match:
    ; Split returned no match — clean up and push None
    ; rest_eg might be non-NULL, DECREF it
    mov rdi, [rbp - CEM_REST]
    test rdi, rdi
    jz .cem_no_match
    call obj_decref
    ; Fall through to no_match

.cem_no_match:
    ; No match — keep exc_value on stack, push None
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax

    ; DECREF match_type
    mov rdi, [rbp - CEM_MTYPE]
    call obj_decref

    leave
    DISPATCH
END_FUNC op_check_eg_match

; op_raise_varargs (130) - Raise an exception
; arg 0: reraise current exception
; arg 1: raise TOS
; arg 2: raise TOS1 from TOS (chaining, simplified)
DEF_FUNC_BARE op_raise_varargs
    cmp ecx, 0
    je .reraise
    cmp ecx, 1
    je .raise_exc
    cmp ecx, 2
    je .raise_from

    ; Invalid arg
    CSTRING rdi, "SystemError: bad RAISE_VARARGS arg"
    extern fatal_error
    call fatal_error

.reraise:
    ; A bare `raise` re-raises the exception being HANDLED, not one in flight:
    ; it is only legal inside an except block, and what it names is that
    ; block's exception.  Reading current_exception instead made it a
    ; RuntimeError everywhere the handler had suspended and come back --
    ; across an `await`, most visibly.
    mov rax, [rel handled_exception]
    test rax, rax
    jnz .do_reraise
    ; Nothing is being handled - raise RuntimeError
    extern exc_RuntimeError_type
    RAISE exc_RuntimeError_type, "No active exception to re-raise"
    ; does not return here

.do_reraise:
    ; It stays installed -- the handler is still running -- so the in-flight
    ; copy needs a reference of its own.  No traceback entry: a bare `raise`
    ; re-raises what this frame is already in the traceback for, and CPython's
    ; RAISE_VARARGS 0 goes straight to the unwind rather than through the
    ; label that records one.  Without this every re-raise added a second
    ; entry for the same frame, pointing at the `raise` line.
    INCREF rax
    mov rdi, [rel current_exception]
    mov [rel current_exception], rax
    test rdi, rdi
    jz .do_reraise_go
    call obj_decref
.do_reraise_go:
    mov byte [rel tb_suppress_frame], 1
    jmp eval_exception_unwind

.raise_exc:
    ; TOS is the exception to raise
    VPOP_VAL rdi, r8
    mov [rel eval_saved_r13], r13  ; update saved stack — VPOP consumed the item

    ; Check if it's already an exception object or a type
    ; If it's a type, create an instance with no args
    cmp r8d, TAG_PTR
    jne .raise_bad_no_decref  ; non-pointer can't be an exception
    test rdi, rdi
    jz .raise_bad_no_decref   ; NULL can't be an exception

    ; Check INSTANCE first (most common case: raise SomeException("msg"))
    ; An instance's ob_type chain might be an exception type
    extern type_is_exc_subclass
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .raise_bad
    push rdi
    mov rdi, rax
    call type_is_exc_subclass
    pop rdi
    test eax, eax
    jnz .raise_exc_obj

    ; Check if rdi is an exception TYPE (e.g., bare "raise ValueError")
    ; First verify rdi is actually a type object (ob_type == type_type, exc_metatype,
    ; or user_type_metatype) to avoid segfault on non-type objects like strings
    mov rax, [rdi + PyObject.ob_type]
    extern type_type
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .raise_check_type
    extern exc_metatype
    lea rcx, [rel exc_metatype]
    cmp rax, rcx
    je .raise_check_type
    extern user_type_metatype
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    jne .raise_bad               ; not a type object at all

.raise_check_type:
    ; rdi is a type object — check if it's an exception subclass
    push rdi
    call type_is_exc_subclass
    pop rdi
    test eax, eax
    jnz .raise_type

    jmp .raise_bad

.raise_type:
    ; rdi = exception type - create instance with no message
    push rdi
    xor esi, esi              ; no message
    xor edx, edx              ; no tag (NULL msg)
    call exc_new
    pop rdi                  ; discard type (immortal, no DECREF needed)
    mov rdi, rax
    jmp .raise_exc_obj

.raise_exc_obj:
    ; rdi = exception object, owned -- the value stack's reference, which
    ; exc_install takes over along with the __context__ rule.
    extern exc_install
    call exc_install
    jmp eval_exception_unwind

.raise_bad:
    ; DECREF the bad value (pointer guaranteed here) and raise TypeError
    call obj_decref
.raise_bad_no_decref:
    RAISE exc_TypeError_type, "exceptions must derive from BaseException"

.raise_from:
    ; TOS = cause, TOS1 = exception
    VPOP_VAL rsi, rcx         ; cause payload + tag
    push rcx                 ; save cause tag
    push rsi                 ; save cause payload
    VPOP_VAL rdi, r8          ; exception payload
    mov [rel eval_saved_r13], r13  ; update saved stack — VPOPs consumed both items
    push rdi                 ; save exception

    ; Store __cause__ on exception object (if exception is a pointer)
    ; cause is at [rsp+8], cause_tag at [rsp+16]
    mov rax, [rsp + 8]      ; cause payload
    mov rcx, [rsp + 16]     ; cause tag
    ; `raise X from Y` suppresses the implicit context either way, and
    ; `from None` leaves no cause at all -- storing the None singleton there
    ; made the traceback printer read a traceback off a 16-byte object.
    mov qword [rdi + PyExceptionObject.exc_suppress], 1
    test ecx, TAG_RC_BIT
    jz .raise_from_no_cause
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    je .raise_from_no_cause
    ; Store cause (transfer ownership — no INCREF, we own the ref from VPOP)
    mov [rdi + PyExceptionObject.exc_cause], rax
    jmp .raise_from_done

.raise_from_no_cause:
    ; Non-pointer cause or None — DECREF if needed and set cause to NULL
    mov rdi, rax
    mov rsi, rcx
    DECREF_VAL rdi, rsi
    mov rdi, [rsp]           ; restore exception
    mov qword [rdi + PyExceptionObject.exc_cause], 0

.raise_from_done:
    ; Raise the exception
    pop rdi
    add rsp, 16
    jmp .raise_exc_obj
END_FUNC op_raise_varargs

; op_reraise (119) - Re-raise the current exception
; TOS = exception to re-raise
DEF_FUNC_BARE op_reraise
    ; Pop the exception from value stack
    VPOP_VAL rdi, r8
    mov [rel eval_saved_r13], r13  ; update saved stack — VPOP consumed the item

    ; Store it as current exception
    push rdi
    mov rax, [rel current_exception]
    test rax, rax
    jz .no_prev_rr
    push rdi
    mov rdi, rax
    call obj_decref
    pop rdi
.no_prev_rr:
    pop rdi
    mov [rel current_exception], rdi
    ; RERAISE must not add a traceback entry: CPython records one at its
    ; `error:` label, which RERAISE skips by jumping straight to the unwind.
    ; Without this the implicit cleanup handler at the end of every `except`
    ; block added a second entry for the same frame, pointing at the
    ; `except` line.
    mov byte [rel tb_suppress_frame], 1
    jmp eval_exception_unwind
END_FUNC op_reraise
