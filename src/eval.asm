; eval.asm - Bytecode evaluation loop
; Core dispatch loop and opcode table for the Python 3.12 interpreter
; Includes exception unwind mechanism

%include "macros.inc"
%include "object.inc"
%include "frame.inc"
%include "opcodes.inc"
%include "types.inc"
%include "errcodes.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

; External opcode handlers (defined in opcodes_*.asm files)
extern op_pop_top
extern op_push_null
extern op_return_value
extern op_return_const
extern op_load_const
extern op_load_fast
extern op_store_fast
extern op_load_global
extern op_load_name
extern op_store_name
extern op_store_global
extern op_binary_op
extern op_call
extern op_compare_op
extern op_pop_jump_if_false
extern op_pop_jump_if_true
extern op_jump_forward
extern op_jump_backward
extern op_copy
extern op_swap
extern op_unary_negative
extern op_unary_not
extern op_pop_jump_if_none
extern op_pop_jump_if_not_none
extern op_make_function
extern op_binary_subscr
extern op_store_subscr
extern op_build_tuple
extern op_build_list
extern op_build_map
extern op_build_const_key_map
extern op_unpack_sequence
extern op_get_iter
extern op_for_iter
extern op_end_for
extern op_list_append
extern op_list_extend
extern op_map_add
extern op_dict_update
extern op_dict_merge
extern op_unpack_ex
extern op_kw_names
extern op_is_op
extern op_contains_op
extern op_load_build_class
extern op_store_attr
extern op_load_attr
extern op_unary_invert
extern op_make_cell
extern op_load_closure
extern op_load_deref
extern op_store_deref
extern op_delete_deref
extern op_copy_free_vars
extern op_build_slice
extern op_binary_slice
extern op_store_slice
extern op_format_value
extern op_build_string
extern op_delete_fast
extern op_delete_name
extern op_delete_global
extern op_delete_attr
extern op_delete_subscr
extern op_load_fast_check
extern op_load_fast_and_clear
extern op_return_generator
extern op_yield_value
extern op_end_send
extern op_jump_backward_no_interrupt
extern op_call_intrinsic_1
extern op_call_function_ex
extern op_before_with
extern op_with_except_start
extern op_build_set
extern op_set_add
extern op_set_update
extern op_get_len
extern op_setup_annotations
extern op_load_locals
extern op_load_from_dict_or_globals
extern op_match_mapping
extern op_match_sequence
extern op_match_keys
extern op_call_intrinsic_2

; External error handler
extern error_unimplemented_opcode

; Exception infrastructure
extern exc_table_find_handler
extern exc_isinstance
extern exc_new
extern exc_from_cstr
extern obj_decref
extern obj_incref
extern obj_dealloc
extern obj_str
extern sys_write
extern sys_exit
extern str_type
extern none_singleton

; Exception type singletons (for raising)
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_BaseException_type
extern exc_Exception_type

; eval_frame(PyFrame *frame) -> PyObject*
; Main entry point: sets up registers from the frame and enters the dispatch loop.
; Returns NULL if an unhandled exception propagated out.
; rdi = frame
global eval_frame
eval_frame:
    push rbp
    mov rbp, rsp
    SAVE_EVAL_REGS

    ; r12 = frame
    mov r12, rdi

    ; Load code object from frame
    mov rax, [r12 + PyFrame.code]

    ; Check for generator resume (instr_ptr != 0 means resume)
    mov rbx, [r12 + PyFrame.instr_ptr]
    test rbx, rbx
    jnz .eval_resume

    ; Normal entry: start from co_code beginning
    lea rbx, [rax + PyCodeObject.co_code]
    mov r13, [r12 + PyFrame.stack_base]
    jmp .eval_setup_consts

.eval_resume:
    ; Generator resume: use saved IP and stack pointer
    mov r13, [r12 + PyFrame.stack_ptr]

.eval_setup_consts:
    ; r14 = &co_consts->ob_item (co_consts tuple data pointer)
    mov rcx, [rax + PyCodeObject.co_consts]
    lea r14, [rcx + PyTupleObject.ob_item]

    ; r15 = &co_names->ob_item (co_names tuple data pointer)
    mov rcx, [rax + PyCodeObject.co_names]
    lea r15, [rcx + PyTupleObject.ob_item]

    ; Save caller's eval_saved_r12 (for nested eval_frame calls)
    mov rax, [rel eval_saved_r12]
    push rax
    mov [rel eval_saved_r12], r12

    ; Save caller's eval_base_rsp (for nested eval_frame calls)
    mov rax, [rel eval_base_rsp]
    push rax
    ; Save machine stack pointer for exception unwind cleanup
    mov [rel eval_base_rsp], rsp

    ; Fall through to eval_dispatch

; eval_dispatch - Main dispatch point
; Reads the next opcode and arg, advances rbx, and jumps to the handler.
global eval_dispatch
align 16
eval_dispatch:
    mov [rel eval_saved_rbx], rbx  ; save bytecode IP for exception unwind
    movzx eax, byte [rbx]      ; load opcode
    movzx ecx, byte [rbx+1]    ; load arg into ecx
    add rbx, 2                  ; advance past instruction word
    lea rdx, [rel opcode_table]
    jmp [rdx + rax*8]              ; dispatch to handler

; eval_return - Return from eval_frame
; rax contains the return value. Restores callee-saved regs and returns.
global eval_return
eval_return:
    ; Restore caller's eval_base_rsp and eval_saved_r12
    pop rdx
    mov [rel eval_base_rsp], rdx
    pop rdx
    mov [rel eval_saved_r12], rdx
    RESTORE_EVAL_REGS
    pop rbp
    ret

; ============================================================================
; Exception unwind mechanism
; ============================================================================

; eval_exception_unwind - Called when an exception is raised
; The exception object must already be stored in [current_exception].
; This routine searches the exception table for a handler. If found,
; it adjusts the value stack and jumps to the handler. If not found,
; it returns NULL from eval_frame to propagate to the caller.
global eval_exception_unwind
eval_exception_unwind:
    ; Restore machine stack to eval frame level (discard intermediate frames)
    mov rsp, [rel eval_base_rsp]

    ; Restore eval loop registers that may have been corrupted.
    ; When raise_exception is called from inside a function that saved/modified
    ; callee-saved regs (e.g. list_subscript saves rbx for temp use), the
    ; non-local jump to here bypasses the restore, leaving regs corrupted.
    ; rbx: use saved copy from eval_dispatch (pre-advance, points to instruction)
    ; r12: reload from frame pointer (saved in eval_frame_r12)
    ; r14/r15: re-derive from code object
    mov rbx, [rel eval_saved_rbx]   ; restore bytecode IP (pre-advance copy)
    mov r12, [rel eval_saved_r12]   ; restore frame pointer

    ; Re-derive r14/r15 from the code object
    mov rax, [r12 + PyFrame.code]
    mov rcx, [rax + PyCodeObject.co_consts]
    lea r14, [rcx + PyTupleObject.ob_item]
    mov rcx, [rax + PyCodeObject.co_names]
    lea r15, [rcx + PyTupleObject.ob_item]

    ; Compute bytecode offset in instruction units (halfwords)
    ; eval_saved_rbx points to the instruction word (before add rbx, 2)
    lea rcx, [rax + PyCodeObject.co_code]
    mov rdi, rax             ; rdi = code object for exc_table_find_handler
    mov rsi, rbx             ; rbx = saved pre-advance bytecode IP
    sub rsi, rcx             ; rsi = byte offset from co_code start
    shr esi, 1               ; rsi = offset in instruction units (halfwords)

    ; Call exc_table_find_handler(code, offset)
    ; Returns: rax = handler target (in halfwords), edx = depth, ecx = push_lasti
    ; Or rax = -1 if no handler
    call exc_table_find_handler

    cmp rax, -1
    je .no_handler

    ; Handler found!
    ; rax = handler target in instruction units
    ; edx = stack depth (number of items on value stack relative to stack_base)
    ; ecx = push_lasti flag

    ; Save handler info
    push rax                 ; save target
    push rcx                 ; save push_lasti flag

    ; Adjust value stack to target depth
    ; target r13 = stack_base + depth * 8
    mov rdi, [r12 + PyFrame.stack_base]
    mov eax, edx
    shl rax, 3               ; depth * 8
    add rdi, rax
    ; DECREF any items being popped from stack
    cmp r13, rdi
    jbe .stack_adjusted
.pop_stack:
    sub r13, 8
    cmp r13, rdi
    jb .stack_adjusted
    push rdi                 ; save target stack ptr
    mov rdi, [r13]
    test rdi, rdi
    jz .pop_next
    call obj_decref
.pop_next:
    pop rdi
    cmp r13, rdi
    ja .pop_stack
.stack_adjusted:
    mov r13, rdi             ; set stack to target depth

    ; Check push_lasti flag
    pop rcx                  ; restore push_lasti
    pop rax                  ; restore target

    ; If push_lasti, push the instruction offset (as SmallInt)
    test ecx, ecx
    jz .no_lasti
    ; Push a dummy lasti value (we don't use it for now)
    mov rdx, 0
    bts rdx, 63             ; SmallInt 0
    VPUSH rdx
.no_lasti:

    ; Push the exception onto the value stack (transfer ownership)
    mov rdx, [rel current_exception]
    mov qword [rel current_exception], 0   ; clear: ownership moves to value stack
    VPUSH rdx

    ; Set rbx to handler target
    ; target is in instruction units (halfwords), so bytes = target * 2
    mov rcx, [r12 + PyFrame.code]
    lea rbx, [rcx + PyCodeObject.co_code]
    shl rax, 1               ; target * 2 = byte offset
    add rbx, rax

    DISPATCH

.no_handler:
    ; No handler found - return NULL to propagate exception to caller
    xor eax, eax
    jmp eval_return

; raise_exception(PyTypeObject *type, const char *msg_cstr)
; Create an exception from a C string and begin unwinding.
; Callable from opcode handlers - uses eval loop registers.
global raise_exception
raise_exception:
    push rbp
    mov rbp, rsp

    ; Create exception: exc_from_cstr(type, msg)
    call exc_from_cstr
    ; rax = exception object

    ; Store in current_exception
    ; First XDECREF any existing exception
    push rax
    mov rdi, [rel current_exception]
    test rdi, rdi
    jz .no_prev
    call obj_decref
.no_prev:
    pop rax
    mov [rel current_exception], rax

    leave
    jmp eval_exception_unwind

; raise_exception_obj(PyExceptionObject *exc)
; Set exception and begin unwinding.
global raise_exception_obj
raise_exception_obj:
    push rbp
    mov rbp, rsp

    ; INCREF the exception (we're taking ownership)
    push rdi
    INCREF rdi
    pop rdi

    ; XDECREF any existing exception
    push rdi
    mov rax, [rel current_exception]
    test rax, rax
    jz .no_prev2
    push rdi
    mov rdi, rax
    call obj_decref
    pop rdi
.no_prev2:
    pop rdi
    mov [rel current_exception], rdi

    leave
    jmp eval_exception_unwind

; ============================================================================
; Exception-related opcode handlers (inline in eval.asm for access to globals)
; ============================================================================

; op_push_exc_info (35) - Push exception info for try/except
; TOS has the exception. Save current exception state, install new one.
; Stack effect: exc -> prev_exc, exc
global op_push_exc_info
op_push_exc_info:
    ; TOS = new exception
    VPOP rax                 ; rax = new exception

    ; Push the previous current_exception (or None if NULL)
    mov rdx, [rel current_exception]
    test rdx, rdx
    jnz .have_prev
    lea rdx, [rel none_singleton]
    INCREF rdx
.have_prev:
    VPUSH rdx                ; push prev_exc

    ; Set new exception as current and push it too
    ; INCREF for the value stack copy
    INCREF rax
    mov [rel current_exception], rax
    VPUSH rax                ; push new exc

    DISPATCH

; op_pop_except (89) - Restore previous exception state
; TOS = the exception to restore as current
global op_pop_except
op_pop_except:
    VPOP rax                 ; rax = exception to restore

    ; XDECREF old current_exception
    push rax
    mov rdi, [rel current_exception]
    test rdi, rdi
    jz .no_old
    call obj_decref
.no_old:
    pop rax

    ; Set restored exception as current (or NULL if None)
    lea rdx, [rel none_singleton]
    cmp rax, rdx
    jne .set_exc
    ; It's None - set current to NULL and DECREF the None
    mov qword [rel current_exception], 0
    DECREF rax
    DISPATCH
.set_exc:
    mov [rel current_exception], rax
    DISPATCH

; op_check_exc_match (36) - Check if exception matches a type
; TOS = type to match against, TOS1 = exception
; Push True/False, don't pop the exception
global op_check_exc_match
op_check_exc_match:
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
    VPUSH rax
    DISPATCH

; op_raise_varargs (130) - Raise an exception
; arg 0: reraise current exception
; arg 1: raise TOS
; arg 2: raise TOS1 from TOS (chaining, simplified)
global op_raise_varargs
op_raise_varargs:
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
    ; Re-raise current exception
    mov rax, [rel current_exception]
    test rax, rax
    jnz .do_reraise
    ; No current exception - raise RuntimeError
    lea rdi, [rel exc_RuntimeError_type]
    extern exc_RuntimeError_type
    CSTRING rsi, "No active exception to re-raise"
    call raise_exception
    ; does not return here

.do_reraise:
    ; current_exception is already set, just unwind
    jmp eval_exception_unwind

.raise_exc:
    ; TOS is the exception to raise
    VPOP rdi

    ; Check if it's already an exception object or a type
    ; If it's a type, create an instance with no args
    test rdi, rdi
    js .raise_bad             ; SmallInt can't be an exception
    jz .raise_bad             ; NULL can't be an exception

    ; First check if rdi is an exception TYPE (has exc_dealloc as tp_dealloc)
    ; Exception type objects have tp_dealloc = exc_dealloc
    extern exc_dealloc
    lea rdx, [rel exc_dealloc]
    mov rcx, [rdi + PyTypeObject.tp_dealloc]
    cmp rcx, rdx
    je .raise_type

    ; Check if rdi is an exception INSTANCE (ob_type->tp_dealloc == exc_dealloc)
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .raise_bad
    mov rcx, [rax + PyTypeObject.tp_dealloc]
    cmp rcx, rdx
    je .raise_exc_obj

    jmp .raise_bad

.raise_type:
    ; rdi = exception type - create instance with no message
    push rdi
    mov rsi, 0               ; no message
    call exc_new
    pop rdi                  ; discard type (immortal, no DECREF needed)
    mov rdi, rax
    jmp .raise_exc_obj

.raise_exc_obj:
    ; rdi = exception object
    ; Store as current_exception
    push rdi
    mov rax, [rel current_exception]
    test rax, rax
    jz .no_prev_raise
    push rdi
    mov rdi, rax
    call obj_decref
    pop rdi
.no_prev_raise:
    pop rdi
    mov [rel current_exception], rdi
    ; Don't DECREF rdi - we transferred ownership from value stack to current_exception
    jmp eval_exception_unwind

.raise_bad:
    ; DECREF the bad value and raise TypeError
    call obj_decref
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "exceptions must derive from BaseException"
    call raise_exception

.raise_from:
    ; TOS = cause, TOS1 = exception
    VPOP rsi                 ; cause (simplified: just DECREF it)
    push rsi
    VPOP rdi                 ; exception
    push rdi

    ; DECREF cause (we don't support __cause__ yet)
    mov rdi, [rsp + 8]
    call obj_decref

    ; Raise the exception
    pop rdi
    add rsp, 8
    jmp .raise_exc_obj

; op_reraise (119) - Re-raise the current exception
; TOS = exception to re-raise
global op_reraise
op_reraise:
    ; Pop the exception from value stack
    VPOP rdi

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
    jmp eval_exception_unwind

; op_unimplemented - Handler for unimplemented opcodes
; The opcode is in eax (set by dispatch). Calls fatal error.
op_unimplemented:
    ; eax still holds the opcode from dispatch
    ; but dispatch already jumped here, so we need the opcode
    ; Recalculate: rbx was advanced by 2, so opcode is at [rbx-2]
    movzx edi, byte [rbx-2]
    call error_unimplemented_opcode
    ; does not return

; op_cache - CACHE opcode (0): no-op, just dispatch next
op_cache:
    DISPATCH

; op_nop - NOP opcode (9): no-op, just dispatch next
op_nop:
    DISPATCH

; op_resume - RESUME opcode (151): no-op, just dispatch next
op_resume:
    DISPATCH

; op_interpreter_exit - INTERPRETER_EXIT opcode (3)
; Pop the return value from the value stack and return from eval_frame.
op_interpreter_exit:
    ; Check for unhandled exception
    mov rax, [rel current_exception]
    test rax, rax
    jnz .unhandled_exception
    VPOP rax
    jmp eval_return

.unhandled_exception:
    ; Print traceback and exit
    ; For now, print "Traceback (most recent call last):" and the exception
    push rbp
    mov rbp, rsp

    ; Print traceback header
    mov edi, 2
    lea rsi, [rel tb_header]
    mov edx, tb_header_len
    call sys_write

    ; Print "  File "<filename>", line N, in <name>\n"
    ; Get filename and function name from code object
    mov rax, [r12 + PyFrame.code]
    mov rdi, [rax + PyCodeObject.co_filename]
    test rdi, rdi
    jz .no_filename

    ; Print "  File \""
    push rax
    mov edi, 2
    lea rsi, [rel tb_file_prefix]
    mov edx, tb_file_prefix_len
    call sys_write
    pop rax

    ; Print filename
    push rax
    mov rdi, [rax + PyCodeObject.co_filename]
    mov esi, 2
    lea rdx, [rdi + PyStrObject.data]
    mov rcx, [rdi + PyStrObject.ob_size]
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, rcx
    call sys_write
    pop rax

    ; Print "\", line ???, in "
    push rax
    mov edi, 2
    lea rsi, [rel tb_line_prefix]
    mov edx, tb_line_prefix_len
    call sys_write
    pop rax

    ; Print function name
    mov rdi, [rax + PyCodeObject.co_name]
    test rdi, rdi
    jz .no_funcname
    push rax
    lea rsi, [rdi + PyStrObject.data]
    mov rdx, [rdi + PyStrObject.ob_size]
    mov edi, 2
    call sys_write
    pop rax
.no_funcname:
    ; Print newline
    mov edi, 2
    lea rsi, [rel tb_newline]
    mov edx, 1
    call sys_write

.no_filename:
    ; Print exception: "TypeName: message\n"
    mov rdi, [rel current_exception]
    test rdi, rdi
    jz .exit_now

    ; Get type name
    mov rax, [rdi + PyExceptionObject.ob_type]
    mov rsi, [rax + PyTypeObject.tp_name]
    ; Print type name
    push rdi
    ; strlen of type name
    mov rdi, rsi
    xor ecx, ecx
.strlen1:
    cmp byte [rdi + rcx], 0
    je .strlen1_done
    inc ecx
    jmp .strlen1
.strlen1_done:
    mov edx, ecx
    mov edi, 2
    call sys_write
    pop rdi

    ; Check for message
    mov rax, [rdi + PyExceptionObject.exc_value]
    test rax, rax
    jz .no_message

    ; Print ": "
    push rax
    mov edi, 2
    lea rsi, [rel tb_colon]
    mov edx, 2
    call sys_write
    pop rax

    ; Print message (must be a string)
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .no_message

    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    mov edi, 2
    call sys_write

.no_message:
    ; Print final newline
    mov edi, 2
    lea rsi, [rel tb_newline]
    mov edx, 1
    call sys_write

.exit_now:
    ; Exit with code 1
    mov edi, 1
    call sys_exit

; ---------------------------------------------------------------------------
; op_extended_arg - Extend the arg of the NEXT instruction
;
; Shifts current arg left 8 bits, combines with next instruction's arg,
; then dispatches next instruction with the combined arg.
; Can chain: multiple EXTENDED_ARGs shift 8 more bits each time.
; ---------------------------------------------------------------------------
op_extended_arg:
    shl ecx, 8                 ; shift current arg left 8
    movzx eax, byte [rbx]     ; next opcode
    movzx edx, byte [rbx+1]   ; next arg
    or ecx, edx               ; combine args
    add rbx, 2                 ; advance past next instruction
    lea rdx, [rel opcode_table]
    jmp [rdx + rax*8]         ; dispatch with combined arg in ecx

; ---------------------------------------------------------------------------
; op_load_assertion_error - Push AssertionError type
; ---------------------------------------------------------------------------
extern exc_AssertionError_type
op_load_assertion_error:
    lea rax, [rel exc_AssertionError_type]
    INCREF rax
    VPUSH rax
    DISPATCH

; ---------------------------------------------------------------------------
; Opcode dispatch table (256 entries, section .data for potential patching)
; ---------------------------------------------------------------------------
section .data
align 8
global opcode_table
opcode_table:
    dq op_cache              ; 0   = CACHE
    dq op_pop_top            ; 1   = POP_TOP
    dq op_push_null          ; 2   = PUSH_NULL
    dq op_interpreter_exit   ; 3   = INTERPRETER_EXIT
    dq op_end_for            ; 4   = END_FOR
    dq op_end_send           ; 5   = END_SEND
    dq op_unimplemented      ; 6
    dq op_unimplemented      ; 7
    dq op_unimplemented      ; 8
    dq op_nop                ; 9   = NOP
    dq op_unimplemented      ; 10
    dq op_unary_negative     ; 11  = UNARY_NEGATIVE
    dq op_unary_not          ; 12  = UNARY_NOT
    dq op_unimplemented      ; 13
    dq op_unimplemented      ; 14
    dq op_unary_invert       ; 15  = UNARY_INVERT
    dq op_unimplemented      ; 16
    dq op_unimplemented      ; 17  = RESERVED
    dq op_unimplemented      ; 18
    dq op_unimplemented      ; 19
    dq op_unimplemented      ; 20
    dq op_unimplemented      ; 21
    dq op_unimplemented      ; 22
    dq op_unimplemented      ; 23
    dq op_unimplemented      ; 24
    dq op_binary_subscr      ; 25  = BINARY_SUBSCR
    dq op_binary_slice       ; 26  = BINARY_SLICE
    dq op_store_slice        ; 27  = STORE_SLICE
    dq op_unimplemented      ; 28
    dq op_unimplemented      ; 29
    dq op_get_len            ; 30  = GET_LEN
    dq op_match_mapping      ; 31  = MATCH_MAPPING
    dq op_match_sequence     ; 32  = MATCH_SEQUENCE
    dq op_match_keys         ; 33  = MATCH_KEYS
    dq op_unimplemented      ; 34
    dq op_push_exc_info      ; 35  = PUSH_EXC_INFO
    dq op_check_exc_match    ; 36  = CHECK_EXC_MATCH
    dq op_unimplemented      ; 37  = CHECK_EG_MATCH
    dq op_unimplemented      ; 38
    dq op_unimplemented      ; 39
    dq op_unimplemented      ; 40
    dq op_unimplemented      ; 41
    dq op_unimplemented      ; 42
    dq op_unimplemented      ; 43
    dq op_unimplemented      ; 44
    dq op_unimplemented      ; 45
    dq op_unimplemented      ; 46
    dq op_unimplemented      ; 47
    dq op_unimplemented      ; 48
    dq op_with_except_start  ; 49  = WITH_EXCEPT_START
    dq op_unimplemented      ; 50  = GET_AITER
    dq op_unimplemented      ; 51  = GET_ANEXT
    dq op_unimplemented      ; 52  = BEFORE_ASYNC_WITH
    dq op_before_with        ; 53  = BEFORE_WITH
    dq op_unimplemented      ; 54  = END_ASYNC_FOR
    dq op_unimplemented      ; 55  = CLEANUP_THROW
    dq op_unimplemented      ; 56
    dq op_unimplemented      ; 57
    dq op_unimplemented      ; 58
    dq op_unimplemented      ; 59
    dq op_store_subscr       ; 60  = STORE_SUBSCR
    dq op_delete_subscr      ; 61  = DELETE_SUBSCR
    dq op_unimplemented      ; 62
    dq op_unimplemented      ; 63
    dq op_unimplemented      ; 64
    dq op_unimplemented      ; 65
    dq op_unimplemented      ; 66
    dq op_unimplemented      ; 67
    dq op_get_iter           ; 68  = GET_ITER
    dq op_unimplemented      ; 69  = GET_YIELD_FROM_ITER
    dq op_unimplemented      ; 70
    dq op_load_build_class   ; 71  = LOAD_BUILD_CLASS
    dq op_unimplemented      ; 72
    dq op_unimplemented      ; 73
    dq op_load_assertion_error ; 74  = LOAD_ASSERTION_ERROR
    dq op_return_generator   ; 75  = RETURN_GENERATOR
    dq op_unimplemented      ; 76
    dq op_unimplemented      ; 77
    dq op_unimplemented      ; 78
    dq op_unimplemented      ; 79
    dq op_unimplemented      ; 80
    dq op_unimplemented      ; 81
    dq op_unimplemented      ; 82
    dq op_return_value       ; 83  = RETURN_VALUE
    dq op_unimplemented      ; 84
    dq op_setup_annotations  ; 85  = SETUP_ANNOTATIONS
    dq op_unimplemented      ; 86
    dq op_load_locals        ; 87  = LOAD_LOCALS
    dq op_unimplemented      ; 88
    dq op_pop_except         ; 89  = POP_EXCEPT
    dq op_store_name         ; 90  = STORE_NAME
    dq op_delete_name        ; 91  = DELETE_NAME
    dq op_unpack_sequence    ; 92  = UNPACK_SEQUENCE
    dq op_for_iter           ; 93  = FOR_ITER
    dq op_unpack_ex          ; 94  = UNPACK_EX
    dq op_store_attr         ; 95  = STORE_ATTR
    dq op_delete_attr        ; 96  = DELETE_ATTR
    dq op_store_global       ; 97  = STORE_GLOBAL
    dq op_delete_global      ; 98  = DELETE_GLOBAL
    dq op_swap               ; 99  = SWAP
    dq op_load_const         ; 100 = LOAD_CONST
    dq op_load_name          ; 101 = LOAD_NAME
    dq op_build_tuple        ; 102 = BUILD_TUPLE
    dq op_build_list         ; 103 = BUILD_LIST
    dq op_build_set          ; 104 = BUILD_SET
    dq op_build_map          ; 105 = BUILD_MAP
    dq op_load_attr          ; 106 = LOAD_ATTR
    dq op_compare_op         ; 107 = COMPARE_OP
    dq op_unimplemented      ; 108 = IMPORT_NAME
    dq op_unimplemented      ; 109 = IMPORT_FROM
    dq op_jump_forward       ; 110 = JUMP_FORWARD
    dq op_unimplemented      ; 111
    dq op_unimplemented      ; 112
    dq op_unimplemented      ; 113
    dq op_pop_jump_if_false  ; 114 = POP_JUMP_IF_FALSE
    dq op_pop_jump_if_true   ; 115 = POP_JUMP_IF_TRUE
    dq op_load_global        ; 116 = LOAD_GLOBAL
    dq op_is_op              ; 117 = IS_OP
    dq op_contains_op        ; 118 = CONTAINS_OP
    dq op_reraise            ; 119 = RERAISE
    dq op_copy               ; 120 = COPY
    dq op_return_const       ; 121 = RETURN_CONST
    dq op_binary_op          ; 122 = BINARY_OP
    dq op_unimplemented      ; 123 = SEND
    dq op_load_fast          ; 124 = LOAD_FAST
    dq op_store_fast         ; 125 = STORE_FAST
    dq op_delete_fast        ; 126 = DELETE_FAST
    dq op_load_fast_check    ; 127 = LOAD_FAST_CHECK
    dq op_pop_jump_if_not_none ; 128 = POP_JUMP_IF_NOT_NONE
    dq op_pop_jump_if_none   ; 129 = POP_JUMP_IF_NONE
    dq op_raise_varargs      ; 130 = RAISE_VARARGS
    dq op_unimplemented      ; 131 = GET_AWAITABLE
    dq op_make_function      ; 132 = MAKE_FUNCTION
    dq op_build_slice        ; 133 = BUILD_SLICE
    dq op_jump_backward_no_interrupt ; 134 = JUMP_BACKWARD_NO_INTERRUPT
    dq op_make_cell          ; 135 = MAKE_CELL
    dq op_load_closure       ; 136 = LOAD_CLOSURE
    dq op_load_deref         ; 137 = LOAD_DEREF
    dq op_store_deref        ; 138 = STORE_DEREF
    dq op_delete_deref       ; 139 = DELETE_DEREF
    dq op_jump_backward      ; 140 = JUMP_BACKWARD
    dq op_unimplemented      ; 141 = LOAD_SUPER_ATTR
    dq op_call_function_ex   ; 142 = CALL_FUNCTION_EX
    dq op_load_fast_and_clear ; 143 = LOAD_FAST_AND_CLEAR
    dq op_extended_arg       ; 144 = EXTENDED_ARG
    dq op_list_append        ; 145 = LIST_APPEND
    dq op_set_add            ; 146 = SET_ADD
    dq op_map_add            ; 147 = MAP_ADD
    dq op_unimplemented      ; 148
    dq op_copy_free_vars     ; 149 = COPY_FREE_VARS
    dq op_yield_value        ; 150 = YIELD_VALUE
    dq op_resume             ; 151 = RESUME
    dq op_unimplemented      ; 152 = MATCH_CLASS
    dq op_unimplemented      ; 153
    dq op_unimplemented      ; 154
    dq op_format_value       ; 155 = FORMAT_VALUE
    dq op_build_const_key_map ; 156 = BUILD_CONST_KEY_MAP
    dq op_build_string       ; 157 = BUILD_STRING
    dq op_unimplemented      ; 158
    dq op_unimplemented      ; 159
    dq op_unimplemented      ; 160
    dq op_unimplemented      ; 161
    dq op_list_extend        ; 162 = LIST_EXTEND
    dq op_set_update         ; 163 = SET_UPDATE
    dq op_dict_merge         ; 164 = DICT_MERGE
    dq op_dict_update        ; 165 = DICT_UPDATE
    dq op_unimplemented      ; 166
    dq op_unimplemented      ; 167
    dq op_unimplemented      ; 168
    dq op_unimplemented      ; 169
    dq op_unimplemented      ; 170
    dq op_call               ; 171 = CALL
    dq op_kw_names           ; 172 = KW_NAMES
    dq op_call_intrinsic_1   ; 173 = CALL_INTRINSIC_1
    dq op_call_intrinsic_2   ; 174 = CALL_INTRINSIC_2
    dq op_load_from_dict_or_globals ; 175 = LOAD_FROM_DICT_OR_GLOBALS
    dq op_unimplemented      ; 176 = LOAD_FROM_DICT_OR_DEREF
    dq op_unimplemented      ; 177
    dq op_unimplemented      ; 178
    dq op_unimplemented      ; 179
    dq op_unimplemented      ; 180
    dq op_unimplemented      ; 181
    dq op_unimplemented      ; 182
    dq op_unimplemented      ; 183
    dq op_unimplemented      ; 184
    dq op_unimplemented      ; 185
    dq op_unimplemented      ; 186
    dq op_unimplemented      ; 187
    dq op_unimplemented      ; 188
    dq op_unimplemented      ; 189
    dq op_unimplemented      ; 190
    dq op_unimplemented      ; 191
    dq op_unimplemented      ; 192
    dq op_unimplemented      ; 193
    dq op_unimplemented      ; 194
    dq op_unimplemented      ; 195
    dq op_unimplemented      ; 196
    dq op_unimplemented      ; 197
    dq op_unimplemented      ; 198
    dq op_unimplemented      ; 199
    dq op_unimplemented      ; 200
    dq op_unimplemented      ; 201
    dq op_unimplemented      ; 202
    dq op_unimplemented      ; 203
    dq op_unimplemented      ; 204
    dq op_unimplemented      ; 205
    dq op_unimplemented      ; 206
    dq op_unimplemented      ; 207
    dq op_unimplemented      ; 208
    dq op_unimplemented      ; 209
    dq op_unimplemented      ; 210
    dq op_unimplemented      ; 211
    dq op_unimplemented      ; 212
    dq op_unimplemented      ; 213
    dq op_unimplemented      ; 214
    dq op_unimplemented      ; 215
    dq op_unimplemented      ; 216
    dq op_unimplemented      ; 217
    dq op_unimplemented      ; 218
    dq op_unimplemented      ; 219
    dq op_unimplemented      ; 220
    dq op_unimplemented      ; 221
    dq op_unimplemented      ; 222
    dq op_unimplemented      ; 223
    dq op_unimplemented      ; 224
    dq op_unimplemented      ; 225
    dq op_unimplemented      ; 226
    dq op_unimplemented      ; 227
    dq op_unimplemented      ; 228
    dq op_unimplemented      ; 229
    dq op_unimplemented      ; 230
    dq op_unimplemented      ; 231
    dq op_unimplemented      ; 232
    dq op_unimplemented      ; 233
    dq op_unimplemented      ; 234
    dq op_unimplemented      ; 235
    dq op_unimplemented      ; 236
    dq op_unimplemented      ; 237
    dq op_unimplemented      ; 238
    dq op_unimplemented      ; 239
    dq op_unimplemented      ; 240
    dq op_unimplemented      ; 241
    dq op_unimplemented      ; 242
    dq op_unimplemented      ; 243
    dq op_unimplemented      ; 244
    dq op_unimplemented      ; 245
    dq op_unimplemented      ; 246
    dq op_unimplemented      ; 247
    dq op_unimplemented      ; 248
    dq op_unimplemented      ; 249
    dq op_unimplemented      ; 250
    dq op_unimplemented      ; 251
    dq op_unimplemented      ; 252
    dq op_unimplemented      ; 253
    dq op_unimplemented      ; 254
    dq op_unimplemented      ; 255

; ============================================================================
; Global exception state (BSS)
; ============================================================================
section .bss
global current_exception
current_exception: resq 1    ; PyExceptionObject* or NULL
eval_base_rsp: resq 1        ; machine stack pointer at eval dispatch level
eval_saved_rbx: resq 1       ; bytecode IP saved at dispatch (for exception unwind)
eval_saved_r12: resq 1       ; frame pointer saved at frame entry (for exception unwind)

global kw_names_pending
kw_names_pending: resq 1     ; tuple of kw names for next CALL, or NULL

; ============================================================================
; Read-only data for traceback printing
; ============================================================================
section .rodata
tb_header: db "Traceback (most recent call last):", 10
tb_header_len equ $ - tb_header

tb_file_prefix: db '  File "', 0
tb_file_prefix_len equ $ - tb_file_prefix - 1

tb_line_prefix: db '", line ?, in '
tb_line_prefix_len equ $ - tb_line_prefix

tb_colon: db ": "
tb_newline: db 10
