; macros.inc - Common NASM macros for apython
; System V AMD64 ABI conventions
;
; ==========================================================================
; REGISTER CONVENTION STANDARD
; ==========================================================================
;
; 1. EVAL LOOP REGISTERS (callee-saved, global interpreter state)
;    rbx  = bytecode instruction pointer (IP into co_code[])
;    r12  = current frame pointer (PyFrame*)
;    r13  = value stack top pointer
;    r14  = co_consts tuple data pointer (&tuple.ob_item[0])
;    r15  = co_names tuple data pointer (&tuple.ob_item[0])
;    ecx  = opcode arg on handler entry (set by eval_dispatch)
;
; 2. OPCODE HANDLER RULE
;    Never hold live values in caller-saved registers (rax, rcx, rdx,
;    rsi, rdi, r8-r11) across any `call` instruction or DECREF/DECREF_REG
;    macro.  Two safe patterns for preserving values across calls:
;      a) Machine stack: push val before call, pop val after
;      b) Callee-saved reg or rbp-frame local [rbp-N]
;
; 3. DECREF SAFETY
;    DECREF_REG contains `call obj_dealloc` which clobbers ALL caller-saved
;    regs.  DECREF saves/restores rdi only.  Any value in rax, rcx, rdx,
;    rsi, r8-r11 is destroyed if the refcount reaches zero.
;
; 4. REGULAR FUNCTION CONVENTION (SysV ABI)
;    rdi = self/first arg.  Use callee-saved regs (rbx, r12-r15) or
;    machine stack to preserve values across calls.  Prefer tail calls
;    (jmp) over call+ret when no cleanup is needed after the call.
; ==========================================================================

%ifndef MACROS_INC
%define MACROS_INC

; Function prologue
%macro FUNC_PROLOG 0
    push rbp
    mov rbp, rsp
%endmacro

; Function prologue with local space
%macro FUNC_PROLOG 1
    push rbp
    mov rbp, rsp
    sub rsp, %1
%endmacro

; Function epilogue
%macro FUNC_EPILOG 0
    leave
    ret
%endmacro

; Value stack push (r13 = stack top pointer)
%macro VPUSH 1
    mov [r13], %1
    add r13, 8
%endmacro

; Value stack pop
%macro VPOP 1
    sub r13, 8
    mov %1, [r13]
%endmacro

; Value stack peek (top of stack without popping)
%macro VPEEK 1
    mov %1, [r13 - 8]
%endmacro

; Value stack peek at offset (0 = top, 1 = second, etc.)
%macro VPEEK_AT 2
    mov %1, [r13 - 8 - %2*8]
%endmacro

; SmallInt tagged pointer support
; Bit 63 set = SmallInt, bits 0-62 = signed integer value
; Valid range: -2^62 to 2^62 - 1
; x86-64 canonical user-space addresses have bit 63 = 0
SMALLINT_TAG equ 0x8000000000000000

; Reference counting (SmallInt-aware: bit 63 set = skip)
%macro INCREF 1
    test %1, %1
    js %%skip
    inc qword [%1 + PyObject.ob_refcnt]
    %%skip:
%endmacro

%macro DECREF 1
    test %1, %1
    js %%skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    push rdi
    mov rdi, %1
    call obj_dealloc
    pop rdi
    %%skip:
%endmacro

; Safe DECREF for register (doesn't push/pop if arg is rdi)
%macro DECREF_REG 1
    test %1, %1
    js %%skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

%macro XDECREF 1
    test %1, %1
    jz %%skip
    js %%skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    push rdi
    mov rdi, %1
    call obj_dealloc
    pop rdi
    %%skip:
%endmacro

; Opcode dispatch - jump back to eval loop
%macro DISPATCH 0
    jmp eval_dispatch
%endmacro

; Declare extern libc function
%macro EXTERN_C 1
    extern %1
%endmacro

; Align stack to 16 bytes before call (System V ABI requirement)
; Usage: ALIGN_STACK_CALL func_name
; Note: most of the time push rbp / mov rbp, rsp handles this

; Save/restore callee-saved registers used in eval loop
%macro SAVE_EVAL_REGS 0
    push rbx
    push r12
    push r13
    push r14
    push r15
%endmacro

%macro RESTORE_EVAL_REGS 0
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
%endmacro

; Create a C string in .rodata and load its address
%macro CSTRING 2  ; %1 = register, %2 = string
    section .rodata
    %%str: db %2, 0
    section .text
    lea %1, [rel %%str]
%endmacro

; Mark stack as non-executable (include in every .asm file via macros.inc)
section .note.GNU-stack noalloc noexec nowrite progbits

; Reset to .text section so files including macros.inc start in .text
section .text

%endif ; MACROS_INC
