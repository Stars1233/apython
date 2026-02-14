; opcodes_load.asm - Opcode handlers for loading values onto the stack
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack top pointer
;   r14 = co_consts tuple data pointer (&tuple.ob_item[0])
;   r15 = co_names tuple data pointer (&tuple.ob_item[0])
;
; ecx = opcode argument on entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "opcodes.inc"
%include "frame.inc"

section .text

extern eval_dispatch
extern obj_dealloc
extern dict_get
extern fatal_error

;; ============================================================================
;; op_load_const - Load constant from co_consts[arg]
;; ============================================================================
global op_load_const
op_load_const:
    ; ecx = arg (index into co_consts)
    mov rax, [r14 + rcx*8]     ; r14 = &co_consts.ob_item[0]
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_load_fast - Load local variable from frame localsplus[arg]
;; ============================================================================
global op_load_fast
op_load_fast:
    ; ecx = arg (slot index in localsplus)
    lea rax, [r12 + PyFrame.localsplus]
    mov rax, [rax + rcx*8]
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_load_global - Load global (or builtin) variable by name
;;
;; Python 3.12 encoding:
;;   bit 0 of arg = push-null-before flag
;;   actual name index = arg >> 1
;;
;; Search order: globals dict -> builtins dict
;; Followed by 4 CACHE entries (8 bytes) that must be skipped.
;; ============================================================================
global op_load_global
op_load_global:
    ; ecx = arg
    ; Check bit 0: if set, push NULL first
    test ecx, 1
    jz .no_push_null
    mov qword [r13], 0
    add r13, 8
.no_push_null:
    ; Name index = arg >> 1
    shr ecx, 1
    ; Get name string from co_names
    mov rdi, [r15 + rcx*8]     ; rdi = name (PyStrObject*)

    ; Save name on the regular stack for retry
    push rdi

    ; Try globals first: dict_get(globals, name) -> value or NULL
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, [rsp]             ; rsi = name
    call dict_get
    test rax, rax
    jnz .found

    ; Try builtins: dict_get(builtins, name)
    mov rdi, [r12 + PyFrame.builtins]
    pop rsi                    ; rsi = name
    call dict_get
    test rax, rax
    jnz .found_no_pop

    ; Not found in either dict - fatal error
    CSTRING rdi, "NameError: name not found"
    call fatal_error

.found:
    add rsp, 8                 ; discard saved name
.found_no_pop:
    INCREF rax
    VPUSH rax
    ; Skip 4 CACHE entries = 8 bytes
    add rbx, 8
    DISPATCH

;; ============================================================================
;; op_load_name - Load name from locals -> globals -> builtins
;;
;; Similar to LOAD_GLOBAL but checks locals dict first.
;; ============================================================================
global op_load_name
op_load_name:
    ; ecx = arg (index into co_names)
    mov rsi, [r15 + rcx*8]     ; rsi = name (PyStrObject*)
    push rsi                   ; save name

    ; Check if frame has a locals dict
    mov rdi, [r12 + PyFrame.locals]
    test rdi, rdi
    jz .try_globals

    ; Try locals first: dict_get(locals, name)
    mov rsi, [rsp]             ; rsi = name
    call dict_get
    test rax, rax
    jnz .found

.try_globals:
    ; Try globals: dict_get(globals, name)
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, [rsp]             ; rsi = name
    call dict_get
    test rax, rax
    jnz .found

    ; Try builtins: dict_get(builtins, name)
    mov rdi, [r12 + PyFrame.builtins]
    pop rsi                    ; rsi = name
    call dict_get
    test rax, rax
    jnz .found_no_pop

    ; Not found in any dict - fatal error
    CSTRING rdi, "NameError: name not found"
    call fatal_error

.found:
    add rsp, 8                 ; discard saved name
.found_no_pop:
    INCREF rax
    VPUSH rax
    DISPATCH
