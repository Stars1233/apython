; eval.asm - Bytecode evaluation loop
; Core dispatch loop and opcode table for the Python 3.12 interpreter

%include "macros.inc"
%include "object.inc"
%include "frame.inc"
%include "opcodes.inc"

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
extern op_is_op
extern op_contains_op
extern op_load_build_class
extern op_store_attr
extern op_load_attr

; External error handler
extern error_unimplemented_opcode

; eval_frame(PyFrame *frame) -> PyObject*
; Main entry point: sets up registers from the frame and enters the dispatch loop.
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

    ; rbx = &code->co_code (bytecode instruction pointer)
    lea rbx, [rax + PyCodeObject.co_code]

    ; r13 = frame->stack_base (value stack top pointer)
    mov r13, [r12 + PyFrame.stack_base]

    ; r14 = &co_consts->ob_item (co_consts tuple data pointer)
    mov rcx, [rax + PyCodeObject.co_consts]
    lea r14, [rcx + PyTupleObject.ob_item]

    ; r15 = &co_names->ob_item (co_names tuple data pointer)
    mov rcx, [rax + PyCodeObject.co_names]
    lea r15, [rcx + PyTupleObject.ob_item]

    ; Fall through to eval_dispatch

; eval_dispatch - Main dispatch point
; Reads the next opcode and arg, advances rbx, and jumps to the handler.
global eval_dispatch
align 16
eval_dispatch:
    movzx eax, byte [rbx]      ; load opcode
    movzx ecx, byte [rbx+1]    ; load arg into ecx
    add rbx, 2                  ; advance past instruction word
    lea rdx, [rel opcode_table]
    jmp [rdx + rax*8]              ; dispatch to handler

; eval_return - Return from eval_frame
; rax contains the return value. Restores callee-saved regs and returns.
global eval_return
eval_return:
    RESTORE_EVAL_REGS
    pop rbp
    ret

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
    VPOP rax
    jmp eval_return

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
    dq op_unimplemented      ; 5   = END_SEND
    dq op_unimplemented      ; 6
    dq op_unimplemented      ; 7
    dq op_unimplemented      ; 8
    dq op_nop                ; 9   = NOP
    dq op_unimplemented      ; 10
    dq op_unary_negative     ; 11  = UNARY_NEGATIVE
    dq op_unary_not          ; 12  = UNARY_NOT
    dq op_unimplemented      ; 13
    dq op_unimplemented      ; 14
    dq op_unimplemented      ; 15  = UNARY_INVERT
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
    dq op_unimplemented      ; 26  = BINARY_SLICE
    dq op_unimplemented      ; 27  = STORE_SLICE
    dq op_unimplemented      ; 28
    dq op_unimplemented      ; 29
    dq op_unimplemented      ; 30  = GET_LEN
    dq op_unimplemented      ; 31  = MATCH_MAPPING
    dq op_unimplemented      ; 32  = MATCH_SEQUENCE
    dq op_unimplemented      ; 33  = MATCH_KEYS
    dq op_unimplemented      ; 34
    dq op_unimplemented      ; 35  = PUSH_EXC_INFO
    dq op_unimplemented      ; 36  = CHECK_EXC_MATCH
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
    dq op_unimplemented      ; 49  = WITH_EXCEPT_START
    dq op_unimplemented      ; 50  = GET_AITER
    dq op_unimplemented      ; 51  = GET_ANEXT
    dq op_unimplemented      ; 52  = BEFORE_ASYNC_WITH
    dq op_unimplemented      ; 53  = BEFORE_WITH
    dq op_unimplemented      ; 54  = END_ASYNC_FOR
    dq op_unimplemented      ; 55  = CLEANUP_THROW
    dq op_unimplemented      ; 56
    dq op_unimplemented      ; 57
    dq op_unimplemented      ; 58
    dq op_unimplemented      ; 59
    dq op_store_subscr       ; 60  = STORE_SUBSCR
    dq op_unimplemented      ; 61  = DELETE_SUBSCR
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
    dq op_unimplemented      ; 74  = LOAD_ASSERTION_ERROR
    dq op_unimplemented      ; 75  = RETURN_GENERATOR
    dq op_unimplemented      ; 76
    dq op_unimplemented      ; 77
    dq op_unimplemented      ; 78
    dq op_unimplemented      ; 79
    dq op_unimplemented      ; 80
    dq op_unimplemented      ; 81
    dq op_unimplemented      ; 82
    dq op_return_value       ; 83  = RETURN_VALUE
    dq op_unimplemented      ; 84
    dq op_unimplemented      ; 85  = SETUP_ANNOTATIONS
    dq op_unimplemented      ; 86
    dq op_unimplemented      ; 87  = LOAD_LOCALS
    dq op_unimplemented      ; 88
    dq op_unimplemented      ; 89  = POP_EXCEPT
    dq op_store_name         ; 90  = STORE_NAME
    dq op_unimplemented      ; 91  = DELETE_NAME
    dq op_unpack_sequence    ; 92  = UNPACK_SEQUENCE
    dq op_for_iter           ; 93  = FOR_ITER
    dq op_unimplemented      ; 94  = UNPACK_EX
    dq op_store_attr         ; 95  = STORE_ATTR
    dq op_unimplemented      ; 96  = DELETE_ATTR
    dq op_store_global       ; 97  = STORE_GLOBAL
    dq op_unimplemented      ; 98  = DELETE_GLOBAL
    dq op_swap               ; 99  = SWAP
    dq op_load_const         ; 100 = LOAD_CONST
    dq op_load_name          ; 101 = LOAD_NAME
    dq op_build_tuple        ; 102 = BUILD_TUPLE
    dq op_build_list         ; 103 = BUILD_LIST
    dq op_unimplemented      ; 104 = BUILD_SET
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
    dq op_unimplemented      ; 119 = RERAISE
    dq op_copy               ; 120 = COPY
    dq op_return_const       ; 121 = RETURN_CONST
    dq op_binary_op          ; 122 = BINARY_OP
    dq op_unimplemented      ; 123 = SEND
    dq op_load_fast          ; 124 = LOAD_FAST
    dq op_store_fast         ; 125 = STORE_FAST
    dq op_unimplemented      ; 126 = DELETE_FAST
    dq op_load_fast          ; 127 = LOAD_FAST_CHECK (same handler as LOAD_FAST)
    dq op_pop_jump_if_not_none ; 128 = POP_JUMP_IF_NOT_NONE
    dq op_pop_jump_if_none   ; 129 = POP_JUMP_IF_NONE
    dq op_unimplemented      ; 130 = RAISE_VARARGS
    dq op_unimplemented      ; 131 = GET_AWAITABLE
    dq op_make_function      ; 132 = MAKE_FUNCTION
    dq op_unimplemented      ; 133 = BUILD_SLICE
    dq op_unimplemented      ; 134 = JUMP_BACKWARD_NO_INTERRUPT
    dq op_unimplemented      ; 135 = MAKE_CELL
    dq op_unimplemented      ; 136 = LOAD_CLOSURE
    dq op_unimplemented      ; 137 = LOAD_DEREF
    dq op_unimplemented      ; 138 = STORE_DEREF
    dq op_unimplemented      ; 139 = DELETE_DEREF
    dq op_jump_backward      ; 140 = JUMP_BACKWARD
    dq op_unimplemented      ; 141 = LOAD_SUPER_ATTR
    dq op_unimplemented      ; 142 = CALL_FUNCTION_EX
    dq op_unimplemented      ; 143 = LOAD_FAST_AND_CLEAR
    dq op_unimplemented      ; 144 = EXTENDED_ARG
    dq op_list_append        ; 145 = LIST_APPEND
    dq op_unimplemented      ; 146 = SET_ADD
    dq op_unimplemented      ; 147 = MAP_ADD
    dq op_unimplemented      ; 148
    dq op_unimplemented      ; 149 = COPY_FREE_VARS
    dq op_unimplemented      ; 150 = YIELD_VALUE
    dq op_resume             ; 151 = RESUME
    dq op_unimplemented      ; 152 = MATCH_CLASS
    dq op_unimplemented      ; 153
    dq op_unimplemented      ; 154
    dq op_unimplemented      ; 155 = FORMAT_VALUE
    dq op_build_const_key_map ; 156 = BUILD_CONST_KEY_MAP
    dq op_unimplemented      ; 157 = BUILD_STRING
    dq op_unimplemented      ; 158
    dq op_unimplemented      ; 159
    dq op_unimplemented      ; 160
    dq op_unimplemented      ; 161
    dq op_list_extend        ; 162 = LIST_EXTEND
    dq op_unimplemented      ; 163 = SET_UPDATE
    dq op_unimplemented      ; 164 = DICT_MERGE
    dq op_unimplemented      ; 165 = DICT_UPDATE
    dq op_unimplemented      ; 166
    dq op_unimplemented      ; 167
    dq op_unimplemented      ; 168
    dq op_unimplemented      ; 169
    dq op_unimplemented      ; 170
    dq op_call               ; 171 = CALL
    dq op_unimplemented      ; 172 = KW_NAMES
    dq op_unimplemented      ; 173 = CALL_INTRINSIC_1
    dq op_unimplemented      ; 174 = CALL_INTRINSIC_2
    dq op_unimplemented      ; 175 = LOAD_FROM_DICT_OR_GLOBALS
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
