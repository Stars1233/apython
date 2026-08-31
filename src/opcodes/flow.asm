; opcodes/flow.asm - Control flow, f-strings, and generators
;
; RETURN_*, the conditional and unconditional jumps, FORMAT_VALUE and
; BUILD_STRING, cell creation, and the generator/coroutine send protocol.
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
extern eval_co_consts
extern eval_return
extern obj_is_true
extern none_singleton
extern cell_new
extern gen_new
extern coro_new
extern async_gen_new
extern raise_exception
extern exc_TypeError_type
extern current_exception
extern eval_exception_unwind
extern obj_decref
extern eval_saved_rbx
extern obj_dealloc
extern opcode_table

;; Stack layout constants for binary_op / compare_op generic paths.
;; After 4 pushes: right, right_tag, left, left_tag
;; Offsets relative to rsp immediately after the 4 pushes.
BO_RIGHT equ 0
BO_RTAG  equ 8
BO_LEFT  equ 16
BO_LTAG  equ 24
BO_SIZE  equ 32

;; Stack layout constants for op_format_value (DEF_FUNC, 48 bytes).
FV_ARG     equ 8
FV_HASSPEC equ 16
FV_SPEC    equ 24
FV_VALUE   equ 32
FV_STAG    equ 40    ; fmt_spec tag
FV_VTAG    equ 48    ; value tag
FV_FRAME   equ 48

;; Stack layout constants for op_build_string (DEF_FUNC, 16 bytes).
BS_COUNT   equ 8
BS_ACCUM   equ 16
BS_FRAME   equ 16

;; Stack layout constants for op_send (DEF_FUNC, 48 bytes).
SND_ARG    equ 8
SND_SENT   equ 16
SND_RECV   equ 24
SND_RESULT equ 32
SND_STAG   equ 40    ; sent_value tag
SND_RTAG   equ 48    ; result tag
SND_FRAME  equ 48

;; Stack layout constants for op_match_keys (DEF_FUNC, 32 bytes).
MK_KEYS    equ 8
MK_SUBJ    equ 16
MK_VALS    equ 24
MK_NKEYS   equ 32
MK_FRAME   equ 32

; --- moved to a sibling file by the split ---
extern op_match_keys

section .text

;; ============================================================================
;; op_return_value - Return TOS from current frame
;;
;; Phase 4 (simple case): module-level code, no previous frame.
;; Pop return value and jump to eval_return.
;; ============================================================================
DEF_FUNC_BARE op_return_value
    VPOP rax                     ; rax = the return Value
    mov qword [r12 + PyFrame.instr_ptr], 0  ; mark frame as "returned" (not yielded)
    jmp eval_return
END_FUNC op_return_value

;; ============================================================================
;; op_return_const - Return co_consts[arg] without popping the stack
;;
;; Load constant, INCREF, and jump to eval_return.
;; ============================================================================
DEF_FUNC_BARE op_return_const
    ; ecx = arg (index into co_consts)
    mov rax, [rel eval_co_consts]
    mov rax, [rax + rcx * 8]
    INCREF_V rax, rdx
    mov qword [r12 + PyFrame.instr_ptr], 0  ; mark frame as "returned" (not yielded)
    jmp eval_return
END_FUNC op_return_const

;; ============================================================================
;; op_pop_jump_if_false - Pop TOS, jump if falsy
;;
;; Python 3.12: arg is the absolute target offset in instruction words
;; (2-byte units from start of co_code).
;; ============================================================================
DEF_FUNC_BARE op_pop_jump_if_false
    VPOP_VAL rdi, r8            ; rdi = value to test, r8 = value tag

    ; Fast path: TAG_BOOL — payload is 0/1, no DECREF needed

    ; Slow path: call obj_is_true + DECREF
    push rcx                   ; save target offset
    push r8                    ; save tag for DECREF
    push rdi                   ; save value for DECREF
    mov rsi, r8                ; tag
    V_PACK rdi, rsi
    call obj_is_true
    push rax                   ; save truthiness
    mov rdi, [rsp + 8]        ; reload value
    mov rsi, [rsp + 16]       ; tag
    DECREF_VAL rdi, rsi
    pop rax                    ; restore truthiness
    add rsp, 16                ; discard saved value + tag
    pop rcx                    ; restore target offset
    test eax, eax
    jnz .no_jump
    lea rbx, [rbx + rcx*2]
.no_jump:
    DISPATCH

.pjif_bool_fast:
    test edi, edi
    jnz .pjif_no_jump          ; truthy → don't jump
    lea rbx, [rbx + rcx*2]    ; jump
.pjif_no_jump:
    DISPATCH
END_FUNC op_pop_jump_if_false

;; ============================================================================
;; op_pop_jump_if_true - Pop TOS, jump if truthy
;; ============================================================================
DEF_FUNC_BARE op_pop_jump_if_true
    VPOP_VAL rdi, r8            ; rdi = value to test, r8 = value tag

    ; Fast path: TAG_BOOL — payload is 0/1, no DECREF needed

    ; Slow path: call obj_is_true + DECREF
    push rcx                   ; save target offset
    push r8                    ; save tag for DECREF
    push rdi                   ; save value for DECREF
    mov rsi, r8                ; tag
    V_PACK rdi, rsi
    call obj_is_true
    push rax                   ; save truthiness
    mov rdi, [rsp + 8]        ; reload value
    mov rsi, [rsp + 16]       ; tag
    DECREF_VAL rdi, rsi
    pop rax                    ; restore truthiness
    add rsp, 16                ; discard saved value + tag
    pop rcx                    ; restore target offset
    test eax, eax
    jz .no_jump
    lea rbx, [rbx + rcx*2]
.no_jump:
    DISPATCH

.pjit_bool_fast:
    test edi, edi
    jz .pjit_no_jump           ; falsy → don't jump
    lea rbx, [rbx + rcx*2]    ; jump
.pjit_no_jump:
    DISPATCH
END_FUNC op_pop_jump_if_true

;; ============================================================================
;; op_pop_jump_if_none - Pop TOS, jump if None
;; ============================================================================
DEF_FUNC_BARE op_pop_jump_if_none
    VPOP_VAL rax, r8            ; rax = value, r8 = value tag

    ; Check for None: TAG_NONE or (TAG_PTR with none_singleton payload)
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    jne .not_none

.is_none:
    ; IS None: save jump offset, DECREF, jump
    push rcx                   ; save jump offset
    mov rsi, r8
    DECREF_VAL rax, rsi
    pop rcx                    ; restore jump offset
    lea rbx, [rbx + rcx*2]
    DISPATCH

.not_none:
    ; NOT None: just DECREF and continue
    mov rsi, r8
    DECREF_VAL rax, rsi
    DISPATCH
END_FUNC op_pop_jump_if_none

;; ============================================================================
;; op_pop_jump_if_not_none - Pop TOS, jump if NOT None
;; ============================================================================
DEF_FUNC_BARE op_pop_jump_if_not_none
    VPOP_VAL rax, r8            ; rax = value, r8 = value tag

    ; Check for None: TAG_NONE or (TAG_PTR with none_singleton payload)
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    je .is_none

    ; NOT None: save jump offset, DECREF, jump
    push rcx                   ; save jump offset
    mov rsi, r8
    DECREF_VAL rax, rsi
    pop rcx                    ; restore jump offset
    lea rbx, [rbx + rcx*2]
    DISPATCH

.is_none:
    ; IS None: just DECREF and continue
    mov rsi, r8
    DECREF_VAL rax, rsi
    DISPATCH
END_FUNC op_pop_jump_if_not_none

;; ============================================================================
;; op_jump_forward - Unconditional forward jump
;;
;; arg = number of instruction words to skip
;; Each instruction word is 2 bytes, so advance rbx by arg*2 bytes.
;; ============================================================================
DEF_FUNC_BARE op_jump_forward
    ; ecx = arg (instruction words to skip)
    lea rbx, [rbx + rcx*2]
    DISPATCH
END_FUNC op_jump_forward

;; ============================================================================
;; op_jump_backward - Unconditional backward jump
;;
;; arg = number of instruction words to go back
;; Subtract arg*2 bytes from rbx.
;; ============================================================================
DEF_FUNC_BARE op_jump_backward
    ; ecx = arg (instruction words to go back)
    shl ecx, 1                 ; ecx = arg * 2 (zero-extends to rcx)
    sub rbx, rcx
    DISPATCH
END_FUNC op_jump_backward

;; ============================================================================
;; op_format_value - Format a value for f-strings
;;
;; arg & 0x03: conversion (0=none, 1=!s, 2=!r, 3=!a)
;; arg & 0x04: format spec present on stack below value
;; Pops value (and optional fmt_spec), pushes formatted string.
;; ============================================================================
DEF_FUNC op_format_value, FV_FRAME

    mov [rbp - FV_ARG], rcx    ; save arg
    mov rax, rcx
    and eax, 4
    mov [rbp - FV_HASSPEC], rax ; has_fmt_spec
    mov qword [rbp - FV_SPEC], 0 ; fmt_spec ptr (0 if absent)
    mov qword [rbp - FV_STAG], 0 ; fmt_spec tag (0 if absent)

    ; If format spec present, pop it first
    ; Stack order: TOS = fmt_spec, TOS1 = value
    test qword [rbp - FV_HASSPEC], 4
    jz .fv_no_spec
    VPOP_VAL rax, rcx           ; fmt_spec string + tag
    mov [rbp - FV_SPEC], rax   ; save fmt_spec
    mov [rbp - FV_STAG], rcx   ; save fmt_spec tag
.fv_no_spec:

    VPOP_VAL rdi, rax           ; value + tag
    mov [rbp - FV_VALUE], rdi  ; save value
    mov [rbp - FV_VTAG], rax   ; save value tag

    ; !s / !r / !a apply *before* the format spec.  The conversion used to be
    ; handled only on the no-spec path, so f"{x!r:>6}" formatted x itself and
    ; dropped the repr; !a was never handled at all.
    mov eax, [rbp - FV_ARG]
    and eax, 3
    jz .fv_converted
    test qword [rbp - FV_HASSPEC], 4
    jz .fv_converted            ; the no-spec path below already converts
    mov rdi, [rbp - FV_VALUE]
    mov rsi, [rbp - FV_VTAG]
    V_PACK rdi, rsi
    cmp eax, 1
    je .fv_conv_str
    extern obj_repr
    call obj_repr               ; !r and !a: repr, which is ASCII already
    jmp .fv_conv_done
.fv_conv_str:
    extern obj_str
    call obj_str
.fv_conv_done:
    V_UNPACK rax, rdx
    test edx, edx
    jz .fv_conv_failed
    push rax
    push rdx
    mov rdi, [rbp - FV_VALUE]
    mov rsi, [rbp - FV_VTAG]
    DECREF_VAL rdi, rsi
    pop rdx
    pop rax
    mov [rbp - FV_VALUE], rax
    mov [rbp - FV_VTAG], rdx
    ; the converted string formats as a string from here on
    mov qword [rbp - FV_ARG], 4
.fv_converted:

    ; If format spec present AND value is float, use float_format_spec
    test qword [rbp - FV_HASSPEC], 4
    jz .fv_no_format_spec

    ; A class defining __format__ formats itself.  Only float had a spec path
    ; here, so f"{obj:>5}" ignored both the spec and the method.
    cmp qword [rbp - FV_VTAG], TAG_PTR
    jne .fv_spec_not_ptr
    mov rdi, [rbp - FV_VALUE]
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .fv_spec_not_ptr
    mov rax, [rbp - FV_SPEC]
    cmp qword [rbp - FV_STAG], TAG_PTR
    jne .fv_type_error
    mov rsi, rax
    lea rdx, [rel fv_format_name]
    mov ecx, TAG_PTR
    extern dunder_call_2
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jnz .fv_have_result         ; __format__ produced the string
    ; NULL is either "no __format__" or "__format__ raised"; falling through
    ; in the second case replaced the exception with a formatting result.
    cmp qword [rel current_exception], 0
    jne .fv_conv_failed
    mov rdi, [rbp - FV_VALUE]   ; no __format__: fall through as before

.fv_spec_not_ptr:
    ; Everything else goes through the full spec grammar.  Only float had a
    ; path here, and it understood just a precision and a type letter, so
    ; f"{255:08b}" was "255" and f"{5:>5}" was "5".
    mov rax, [rbp - FV_SPEC]
    cmp qword [rbp - FV_STAG], TAG_PTR
    jne .fv_type_error
    mov rdi, [rbp - FV_VALUE]
    mov rsi, [rbp - FV_VTAG]
    V_PACK rdi, rsi
    mov rsi, rax
    extern format_apply_spec
    call format_apply_spec
    V_UNPACK rax, rdx
    jmp .fv_have_result

.fv_conv_failed:
    extern eval_exception_unwind
    mov [rel eval_saved_r13], r13
    leave
    jmp eval_exception_unwind

.fv_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "format spec must be str"
    call raise_exception

.fv_no_format_spec:
    ; Apply conversion based on arg & 3
    mov rdi, [rbp - FV_VALUE]  ; reload value payload
    mov rsi, [rbp - FV_VTAG]   ; reload value tag
    mov eax, [rbp - FV_ARG]
    and eax, 3
    cmp eax, 2
    jge .fv_repr               ; !r and !a both go through repr
    test eax, eax
    jnz .fv_use_str            ; !s asks for str() explicitly

    ; No conversion and no spec: f"{obj}" still goes through __format__ with
    ; an empty spec in CPython, not through str().
    cmp qword [rbp - FV_VTAG], TAG_PTR
    jne .fv_use_str
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .fv_use_str
    extern str_from_cstr_heap
    push rdi
    CSTRING rdi, ""
    call str_from_cstr_heap
    mov rsi, rax               ; the empty spec
    pop rdi
    push rsi
    lea rdx, [rel fv_format_name]
    mov ecx, TAG_PTR
    call dunder_call_2
    V_UNPACK rax, rdx
    pop rdi                    ; the empty spec, ours to release
    push rax
    push rdx
    call obj_decref
    pop rdx
    pop rax
    test edx, edx
    jnz .fv_have_result
    cmp qword [rel current_exception], 0
    jne .fv_conv_failed
    ; No __format__: fall through to str() as before.
    mov rdi, [rbp - FV_VALUE]
    mov rsi, [rbp - FV_VTAG]

.fv_use_str:
    ; Default: str() — conversion 0 (none) and 1 (!s) both use str()
    extern obj_str
    V_PACK rdi, rsi
    call obj_str
    jmp .fv_have_result

.fv_repr:
    extern obj_repr
    V_PACK rdi, rsi
    call obj_repr

.fv_have_result:
    push rdx                   ; save result tag
    push rax                   ; save result payload

    ; DECREF original value (tag-aware)
    mov rdi, [rbp - FV_VALUE]
    mov rsi, [rbp - FV_VTAG]
    DECREF_VAL rdi, rsi

    ; DECREF fmt_spec if present (tag-aware)
    cmp qword [rbp - FV_SPEC], 0
    je .fv_push
    mov rdi, [rbp - FV_SPEC]
    mov rsi, [rbp - FV_STAG]
    DECREF_VAL rdi, rsi

.fv_push:
    pop rax                    ; result payload
    pop rdx                    ; result tag
    VPUSH_VAL rax, rdx
    leave
    DISPATCH
END_FUNC op_format_value

;; ============================================================================
;; op_build_string - Concatenate N strings from the stack
;;
;; ecx = number of string fragments
;; Pops ecx strings, concatenates in order, pushes result.
;; ============================================================================
DEF_FUNC op_build_string, BS_FRAME

    mov [rbp - BS_COUNT], rcx  ; count

    test ecx, ecx
    jz .bs_zero
    cmp ecx, 1
    je .bs_one

    ; General case: iterate and concatenate
    ; Pop all items, keeping base pointers
    mov rdi, rcx
    shl rdi, 3                 ; count * 8 bytes/slot
    sub r13, rdi               ; pop all items at once (r13 = base)

    ; Start with first string
    mov rax, [r13]             ; first fragment
    V_TEST_PTR rax, r9
    ja .bs_type_error
    INCREF rax                 ; heap str needs INCREF
    mov [rbp - BS_ACCUM], rax  ; accumulator (heap)

    ; Concatenate remaining
    mov rcx, 1                 ; start from index 1
.bs_loop:
    cmp rcx, [rbp - BS_COUNT]
    jge .bs_decref
    ; Get next fragment — must be heap str
    mov rax, rcx
    mov rsi, [r13 + rax*8]     ; fragment
    V_TEST_PTR rsi, rdx
    ja .bs_type_error
    push rcx
    extern str_concat
    mov rdi, [rbp - BS_ACCUM] ; accumulator
    mov ecx, TAG_PTR           ; right_tag (heap str guaranteed)
    call str_concat
    ; DECREF old accumulator
    push rax                   ; save new result
    mov rdi, [rbp - BS_ACCUM]
    DECREF_REG rdi
    pop rax
    mov [rbp - BS_ACCUM], rax  ; new accumulator
    pop rcx
    inc rcx
    jmp .bs_loop

.bs_decref:
    ; DECREF all original fragments
    xor ecx, ecx
.bs_decref_loop:
    cmp rcx, [rbp - BS_COUNT]
    jge .bs_push
    mov rax, rcx
    mov rdi, [r13 + rax*8]
    push rcx
    DECREF_V rdi, rsi
    pop rcx
    inc rcx
    jmp .bs_decref_loop

.bs_push:
    mov rax, [rbp - BS_ACCUM]
    VPUSH_PTR rax
    leave
    DISPATCH

.bs_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "build_string expects str"
    call raise_exception

.bs_zero:
    ; Empty f-string: push empty string
    extern str_from_cstr
    CSTRING rdi, ""
    call str_from_cstr
    VPUSH_VAL rax, rdx
    leave
    DISPATCH

.bs_one:
    ; Shortcut: 1 fragment, just leave it on stack
    leave
    DISPATCH
END_FUNC op_build_string

;; ============================================================================
;; Data section - binary op offset lookup table
;; ============================================================================

;; ============================================================================
;; op_make_cell - Wrap localsplus[arg] in a cell object
;;
;; If localsplus[arg] is not already a cell, create one and wrap the value.
;; If localsplus[arg] is NULL, create an empty cell.
;; ============================================================================
DEF_FUNC_BARE op_make_cell
    lea rdx, [rcx*8]              ; slot * 8

    ; Current Value in the slot
    mov rdi, [r12 + PyFrame.localsplus + rdx]

    push rdx                ; slot offset
    push rdi                ; old Value, for the DECREF below

    call cell_new              ; takes the contents Value in rdi
    ; rax = new cell

    pop rdi                 ; old Value
    pop rdx                 ; slot offset

    ; Release the slot's reference; cell_new took its own
    push rax
    push rdx
    DECREF_V rdi, rsi
    pop rdx
    pop rax

    ; A cell pointer is its own Value
    mov [r12 + PyFrame.localsplus + rdx], rax
    DISPATCH
END_FUNC op_make_cell

;; ============================================================================
;; op_copy_free_vars - Copy closure cells into frame's freevar slots
;;
;; arg = count of free vars to copy.
;; Source: current function's func_closure tuple.
;; Destination: localsplus[co_nlocals + ncellvars + i] for i in 0..arg-1
;;
;; In Python 3.12, the function being executed is NOT on the stack.
;; We find it via the calling frame's CALL setup. However, the bytecode
;; compiler ensures COPY_FREE_VARS is the first opcode, and the function
;; object is passed to eval_frame. We need to get it from the frame.
;;
;; Actually, in Python 3.12: the closure tuple is stored in the function
;; object. The function that owns the current frame can be found by
;; looking at the frame's localsplus from the caller. But simpler:
;; we stash the function object in the frame during func_call.
;; ============================================================================
DEF_FUNC_BARE op_copy_free_vars
    ; ecx = number of free vars to copy
    test ecx, ecx
    jz .cfv_done

    ; Get the function object from frame's func_obj slot
    mov rax, [r12 + PyFrame.func_obj]
    test rax, rax
    jz .cfv_done

    ; Get closure tuple from function
    mov rax, [rax + PyFuncObject.func_closure]
    test rax, rax
    jz .cfv_done

    ; rax = closure tuple, ecx = count
    ; Destination: localsplus starts at nlocalsplus - ecx (freevar slots at end)
    ; Actually: Python 3.12 puts freevars after cellvars in localsplus
    ; COPY_FREE_VARS arg tells us the count. The slots are at the END
    ; of localsplus: index [nlocalsplus - arg ... nlocalsplus - 1]
    mov edx, [r12 + PyFrame.nlocalsplus]
    sub edx, ecx                   ; edx = first freevar index

    ; Copy cells from closure tuple to freevar slots
    mov rdi, [rax + PyTupleObject.ob_item]       ; payloads
    xor r8d, r8d                   ; loop counter
.cfv_loop:
    cmp r8d, ecx
    jge .cfv_done

    ; Get cell from closure tuple item[i] (tuples still carry a tag sidecar)
    mov r9, [rdi + r8*8]                               ; payload
    V_UNPACK r9, r11

    ; INCREF while the tag is still around, then pack into a Value
    INCREF_VAL r9, r11
    V_PACK r9, r11

    ; Compute destination index: edx + r8d
    mov r10d, edx
    add r10d, r8d
    mov [r12 + PyFrame.localsplus + r10*8], r9
.cfv_next:
    inc r8d
    jmp .cfv_loop

.cfv_done:
    DISPATCH
END_FUNC op_copy_free_vars

;; ============================================================================
;; op_return_generator - Create generator from current frame
;;
;; RETURN_GENERATOR (75): First instruction in a generator function.
;; Creates a PyGenObject holding the current frame, returns it from eval_frame.
;; The frame is NOT freed by func_call (instr_ptr != 0 signals this).
;; ============================================================================
DEF_FUNC_BARE op_return_generator
    ; Save current execution state in frame for later resumption
    mov [r12 + PyFrame.instr_ptr], rbx
    mov [r12 + PyFrame.stack_ptr], r13

    ; Check co_flags to decide which object type to create
    mov rax, [r12 + PyFrame.code]
    mov eax, [rax + PyCodeObject.co_flags]

    ; Create the appropriate object: gen_new/coro_new/async_gen_new(frame)
    mov rdi, r12
    test eax, CO_COROUTINE
    jnz .ret_gen_coro
    test eax, CO_ASYNC_GENERATOR
    jnz .ret_gen_async

    ; Plain generator
    call gen_new
    jmp .ret_gen_done

.ret_gen_coro:
    call coro_new
    jmp .ret_gen_done

.ret_gen_async:
    call async_gen_new

.ret_gen_done:
    ; rax = new gen/coro/async_gen object — a pointer is its own Value

    ; Return from eval_frame
    ; frame->instr_ptr is non-zero, so func_call will skip frame_free
    jmp eval_return
END_FUNC op_return_generator

;; ============================================================================
;; op_yield_value - Yield a value from generator
;;
;; YIELD_VALUE (150): Pop TOS (value to yield), save frame state,
;; return value from eval_frame. The generator is suspended.
;; ============================================================================
DEF_FUNC_BARE op_yield_value
    ; Pop the Value to yield
    VPOP rax

    ; Save frame state for resumption
    mov [r12 + PyFrame.instr_ptr], rbx
    mov [r12 + PyFrame.stack_ptr], r13

    ; Return yielded value from eval_frame
    jmp eval_return
END_FUNC op_yield_value

;; ============================================================================
;; op_end_send - End of send operation
;;
;; END_SEND (5): Pop TOS1 (receiver/generator), keep TOS (value).
;; ============================================================================
DEF_FUNC_BARE op_end_send
    ; TOS = value, TOS1 = receiver
    VPOP_VAL rax, r8            ; value payload + tag
    VPOP_VAL rdi, rsi           ; receiver payload + tag
    push r8                    ; save value tag
    push rax                   ; save value payload
    DECREF_VAL rdi, rsi        ; DECREF receiver (tag-aware)
    pop rax
    pop rdx
    VPUSH_VAL rax, rdx         ; push value back with tag
    DISPATCH
END_FUNC op_end_send

;; ============================================================================
;; op_send - Send value to generator/coroutine
;;
;; SEND (123): TOS = value_to_send, TOS1 = receiver (generator)
;; arg = jump offset (relative, used if generator exhausted)
;; Calls gen_send(receiver, value). If yielded: push result.
;; If exhausted (StopIteration): jump forward by arg.
;; Followed by 1 CACHE entry (2 bytes).
;; ============================================================================
extern gen_send
extern gen_type
extern coro_type
extern async_gen_type

DEF_FUNC op_send, SND_FRAME
    ; ecx = arg (jump offset in instructions for StopIteration)
    ; Stack: ... | receiver | sent_value |
    mov [rbp - SND_ARG], rcx   ; save arg

    VPOP_VAL rsi, rax           ; sent_value payload + tag
    mov [rbp - SND_SENT], rsi  ; save sent_value
    mov [rbp - SND_STAG], rax  ; save sent_value tag
    VPEEK rdi                  ; rdi = receiver (TOS1, stay on stack)
    mov [rbp - SND_RECV], rdi  ; save receiver

    ; Check if receiver is a generator with iternext
    V_TEST_PTR rdi, rax
    ja .send_error
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .send_error

    ; Check if sent value is None — use iternext, otherwise gen_send
    ; Handle both inline TAG_NONE and pointer-to-none_singleton forms
    mov rsi, [rbp - SND_SENT]
    lea rcx, [rel none_singleton]
    cmp rsi, rcx
    je .send_use_iternext

    ; Only call gen_send if receiver is gen/coro/async_gen type
    mov rdi, [rbp - SND_RECV]
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel gen_type]
    cmp rax, rcx
    je .send_gen_send
    lea rcx, [rel coro_type]
    cmp rax, rcx
    je .send_gen_send
    lea rcx, [rel async_gen_type]
    cmp rax, rcx
    je .send_gen_send
    ; Not a generator — use tp_iternext (value is discarded)
    jmp .send_use_iternext

.send_gen_send:
    ; gen_send(receiver, value, value_tag)
    mov rdi, [rbp - SND_RECV]
    mov rsi, [rbp - SND_SENT]
    movzx edx, byte [rbp - SND_STAG]
    V_PACK rsi, rdx
    call gen_send
    V_UNPACK rax, rdx         ; gen_send returns a Value
    jmp .send_check_result

.send_use_iternext:
    ; tp_iternext(receiver)
    mov rdi, [rbp - SND_RECV]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    V_UNPACK rax, rdx           ; tp_iternext returns a Value

.send_check_result:
    mov [rbp - SND_RESULT], rax ; save result payload
    mov [rbp - SND_RTAG], rdx   ; save result tag

    ; DECREF sent value (tag-aware)
    mov rdi, [rbp - SND_SENT]
    movzx esi, byte [rbp - SND_STAG]
    DECREF_VAL rdi, rsi

    mov rax, [rbp - SND_RESULT]
    movzx edx, byte [rbp - SND_RTAG]
    test edx, edx
    jz .send_exhausted

    ; Yielded: push result on top (receiver stays below)
    ; Stack becomes: ... | receiver | yielded_value |
    movzx edx, byte [rbp - SND_RTAG]
    VPUSH_VAL rax, rdx

    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    leave
    DISPATCH

.send_exhausted:
    ; Receiver exhausted. Push return value (for yield-from protocol).
    ; Gen/coro/task/awaitable/asend all have gi_return_value at offset +48.
    ; Guard: only read if receiver's type has tp_basicsize > 56 (enough for +48 field).
    ; Plain iterators (str_iter, list_iter) have smaller objects → push None.
    mov rdi, [rbp - SND_RECV]
    V_TEST_PTR rdi, rax
    ja .send_no_retval
    mov rax, [rdi + PyObject.ob_type]
    ; gi_return_value lives at +48, so the object must be at least 56 bytes.
    ; (Was `jle 56` when the struct still carried a separate tag word.)
    cmp qword [rax + PyTypeObject.tp_basicsize], 56
    jl .send_no_retval
    mov rax, [rdi + PyGenObject.gi_return_value]
    V_UNPACK rax, rdx
    test edx, edx
    jnz .send_have_retval
.send_no_retval:
    ; No return value — push None
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax
    jmp .send_exhausted_jump
.send_have_retval:
    ; INCREF the return value (we're copying it onto the stack)
    INCREF_VAL rax, rdx
    VPUSH_VAL rax, rdx
.send_exhausted_jump:
    ; Skip 1 CACHE entry = 2 bytes, then jump forward by arg * 2 bytes
    add rbx, 2
    mov rcx, [rbp - SND_ARG]
    lea rbx, [rbx + rcx*2]
    leave
    DISPATCH

.send_error:
    ; Unsupported receiver — just push None and continue
    mov rdi, [rbp - SND_SENT]
    mov rsi, [rbp - SND_STAG]
    DECREF_VAL rdi, rsi
    lea rax, [rel none_singleton]
    INCREF rax
    VPUSH_PTR rax
    add rbx, 2
    leave
    DISPATCH
END_FUNC op_send

;; ============================================================================
;; op_get_yield_from_iter - Get iterator for yield-from
;;
;; GET_YIELD_FROM_ITER (69): TOS should be an iterable.
;; If TOS is already a generator, leave it. Otherwise call iter().
;; ============================================================================
DEF_FUNC_BARE op_get_yield_from_iter
    ; TOS = iterable
    VPEEK rdi                  ; rdi = TOS (don't pop)

    ; If it's already a generator or coroutine, done — must be a real object
    V_TEST_PTR rdi, rax
    ja .gyfi_call_iter
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel gen_type]
    cmp rax, rcx
    je .gyfi_done              ; already a generator, leave on stack
    lea rcx, [rel coro_type]
    cmp rax, rcx
    je .gyfi_done              ; already a coroutine, leave on stack

.gyfi_call_iter:
    ; Not a generator — call tp_iter to get an iterator
    VPOP_VAL rdi, r8            ; pop iterable + tag

    ; Must be TAG_PTR to dereference ob_type
    cmp r8, TAG_PTR
    jne .gyfi_error_nopush

    push r8                    ; save tag (deeper)
    push rdi                   ; save payload

    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .gyfi_error

    call rax                   ; tp_iter(iterable) -> iterator
    push rax                   ; save iterator

    ; DECREF original iterable (tag-aware)
    mov rdi, [rsp + 8]        ; iterable payload
    mov rsi, [rsp + 16]       ; iterable tag
    DECREF_VAL rdi, rsi

    pop rax                    ; restore iterator
    add rsp, 16                ; discard iterable payload + tag
    VPUSH_PTR rax              ; push iterator as new TOS

.gyfi_done:
    DISPATCH

.gyfi_error:
    add rsp, 16                ; discard iterable payload + tag
.gyfi_error_nopush:
    extern exc_TypeError_type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception
END_FUNC op_get_yield_from_iter

;; ============================================================================
;; op_jump_backward_no_interrupt - Jump backward (no interrupt check)
;;
;; JUMP_BACKWARD_NO_INTERRUPT (134): Same as JUMP_BACKWARD for us.
;; ============================================================================
DEF_FUNC_BARE op_jump_backward_no_interrupt
    shl ecx, 1                 ; arg * 2 = byte offset (zero-extends to rcx)
    sub rbx, rcx
    DISPATCH
END_FUNC op_jump_backward_no_interrupt

section .rodata
fv_format_name: db "__format__", 0
