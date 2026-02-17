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

; Standard global function with RBP frame
%macro DEF_FUNC 1
    global %1:function (%1 %+ .end - %1)
%1:
    push rbp
    mov rbp, rsp
%endmacro

; Global function with RBP frame + local space
%macro DEF_FUNC 2
    global %1:function (%1 %+ .end - %1)
%1:
    push rbp
    mov rbp, rsp
    sub rsp, %2
%endmacro

; Global function without prologue (opcode handlers, leaf, tail-call)
%macro DEF_FUNC_BARE 1
    global %1:function (%1 %+ .end - %1)
%1:
%endmacro

; File-local function with RBP frame (not exported)
%macro DEF_FUNC_LOCAL 1
%1:
    push rbp
    mov rbp, rsp
%endmacro

; End any function — emits .end label for ELF size
%macro END_FUNC 1
%1 %+ .end:
%endmacro

; File initialization (GNU-stack + text section reset)
%macro ASM_INIT 0
    section .note.GNU-stack noalloc noexec nowrite progbits
    section .text
%endmacro

; Value stack push (r13 = stack top pointer, 16 bytes/slot)
; Auto-classifies payload: SmallInt (bit 63) → TAG_SMALLINT, NULL → TAG_NULL, else → TAG_PTR
%macro VPUSH 1
    mov [r13], %1
    test %1, %1
    js %%smallint
    jz %%null
    mov qword [r13 + 8], TAG_PTR
    jmp %%done
%%smallint:
    mov qword [r13 + 8], TAG_SMALLINT
    jmp %%done
%%null:
    mov qword [r13 + 8], TAG_NULL
%%done:
    add r13, 16
%endmacro

; Value stack pop (16 bytes/slot)
%macro VPOP 1
    sub r13, 16
    mov %1, [r13]
%endmacro

; Value stack peek (top of stack without popping, 16 bytes/slot)
%macro VPEEK 1
    mov %1, [r13 - 16]
%endmacro

; Value stack peek at offset (0 = top, 1 = second, etc.; 16 bytes/slot)
%macro VPEEK_AT 2
    mov %1, [r13 - 16 - %2*16]
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

; Opcode dispatch - inline decode + jump (direct threading)
; Each DISPATCH site gets its own BTB entry for better branch prediction.
; Falls back to centralized eval_dispatch when tracing is enabled.
%macro DISPATCH 0
    mov [rel eval_saved_rbx], rbx  ; save bytecode IP for exception unwind
    cmp byte [rel trace_opcodes], 0
    jnz eval_dispatch              ; cold: centralized trace handler
    movzx eax, byte [rbx]         ; load opcode
    movzx ecx, byte [rbx+1]      ; load arg into ecx
    add rbx, 2                     ; advance past instruction word
    lea rdx, [rel opcode_table]
    jmp [rdx + rax*8]             ; dispatch to handler
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

; ==========================================================================
; 128-BIT FAT VALUE SUPPORT
; ==========================================================================
;
; Fat values: 64-bit payload + 64-bit tag. Used on value stack and localsplus.
; Heap containers remain 64-bit (use val_widen/val_narrow at boundaries).
;
; Tag word encoding (two modes, distinguished by bit 63):
;
;   bit 63 = 0:  Normal tag mode
;     bits 0-7:   type tag (TAG_SMALLINT..TAG_PTR)
;     bit  8:     refcounted flag (1 = payload is heap pointer needing INCREF/DECREF)
;     bits 9-62:  reserved (0)
;     bit  63:    0
;
;   bit 63 = 1:  SmallStr mode (inline string <= 15 bytes)
;     bits 0-55:  string bytes 8-14 (7 bytes, zero-padded)
;     bits 56-62: string length (0-15)
;     bit  63:    1 (SmallStr marker)
;     payload:    string bytes 0-7 (8 bytes, zero-padded)

; Tag constants (normal mode, bit 63 = 0)
TAG_NULL     equ 0           ; null/empty slot
TAG_SMALLINT equ 1           ; payload = int64_t (full 64-bit range)
TAG_FLOAT    equ 2           ; payload = double bits (IEEE 754)
TAG_NONE     equ 3           ; payload = 0 (unused)
TAG_BOOL     equ 4           ; payload = 0 (False) or 1 (True)
TAG_PTR      equ 0x105       ; 5 | TAG_RC_BIT — heap PyObject*, refcounted
TAG_RC_BIT   equ (1 << 8)    ; bit 8: refcounted flag

; 128-bit value stack push (r13 = stack top pointer, 16 bytes/slot)
%macro VPUSH_VAL 2              ; %1 = payload reg, %2 = tag reg
    mov [r13], %1
    mov [r13 + 8], %2
    add r13, 16
%endmacro

; 128-bit value stack pop
%macro VPOP_VAL 2               ; %1 = payload dest reg, %2 = tag dest reg
    sub r13, 16
    mov %1, [r13]
    mov %2, [r13 + 8]
%endmacro

; Push SmallInt (payload = raw int64, no encode needed)
%macro VPUSH_INT 1              ; %1 = payload reg containing int64
    mov [r13], %1
    mov qword [r13 + 8], TAG_SMALLINT
    add r13, 16
%endmacro

; Push float (payload = IEEE 754 double bits)
%macro VPUSH_FLOAT 1            ; %1 = payload reg containing double bits
    mov [r13], %1
    mov qword [r13 + 8], TAG_FLOAT
    add r13, 16
%endmacro

; Push None (payload = 0, tag = TAG_NONE)
%macro VPUSH_NONE 0
    mov qword [r13], 0
    mov qword [r13 + 8], TAG_NONE
    add r13, 16
%endmacro

; Push bool (payload = 0 or 1)
%macro VPUSH_BOOL 1             ; %1 = payload reg (0=False, 1=True)
    mov [r13], %1
    mov qword [r13 + 8], TAG_BOOL
    add r13, 16
%endmacro

; Push heap pointer (refcounted)
%macro VPUSH_PTR 1              ; %1 = payload reg containing PyObject*
    mov [r13], %1
    mov qword [r13 + 8], TAG_PTR
    add r13, 16
%endmacro

; Push null/empty slot (16 bytes of zero, tag = TAG_NULL = 0)
%macro VPUSH_NULL128 0
    mov qword [r13], 0
    mov qword [r13 + 8], TAG_NULL
    add r13, 16
%endmacro

; Push unknown/polymorphic value: branchless tag classification (clobbers r8, r9)
; Classifies: NULL -> TAG_NULL, SmallInt (bit63) -> TAG_SMALLINT, else -> TAG_PTR
; Safe for any payload register including rax, rdx.
%macro VPUSH_BRANCHLESS 1      ; %1 = payload reg (clobbers r8, r9)
    mov [r13], %1
    xor r9d, r9d               ; TAG_NULL = 0 (before test to avoid flag clobber)
    test %1, %1                ; set ZF/SF based on payload
    mov r8d, TAG_PTR
    cmovz r8d, r9d             ; null -> TAG_NULL
    mov r9d, TAG_SMALLINT
    cmovs r8d, r9d             ; bit63 set -> TAG_SMALLINT
    mov [r13 + 8], r8
    add r13, 16
%endmacro

; Push SmallStr (caller must pre-build payload + tag with SmallStr encoding)
%macro VPUSH_STR 2              ; %1 = payload reg, %2 = tag reg (pre-built)
    mov [r13], %1
    mov [r13 + 8], %2
    add r13, 16
%endmacro

; Peek tag of top-of-stack (without popping)
%macro VPEEK_TAG 1              ; %1 = dest reg
    mov %1, [r13 - 8]
%endmacro

; Peek payload of top-of-stack (without popping)
%macro VPEEK_PAYLOAD 1          ; %1 = dest reg
    mov %1, [r13 - 16]
%endmacro

; Peek 128-bit value at offset from TOS (0 = TOS, 1 = second, etc.)
%macro VPEEK_AT_VAL 3           ; %1 = payload dest, %2 = tag dest, %3 = offset (0=TOS)
    mov %1, [r13 - 16 - %3*16]
    mov %2, [r13 - 8 - %3*16]
%endmacro

; 128-bit INCREF — only if refcounted (tag bit 8 set, bit 63 clear)
; SmallStr (bit 63=1) and inline tags (bit 8=0) skip.
; Hot path: TAG_PTR (bit 8 set, bit 63 clear) falls through to inc.
%macro INCREF_VAL 2             ; %1 = payload, %2 = tag
    test %2, TAG_RC_BIT
    jz %%skip                   ; not refcounted: skip (not-taken for TAG_PTR)
    test %2, %2
    js %%skip                   ; SmallStr: skip (very cold)
    inc qword [%1 + PyObject.ob_refcnt]
    %%skip:
%endmacro

; 128-bit DECREF — only if refcounted (tag bit 8 set, bit 63 clear)
; Calls obj_dealloc if refcount reaches zero. Clobbers rdi + all caller-saved.
; Hot path: TAG_PTR (bit 8 set, bit 63 clear) falls through to dec.
%macro DECREF_VAL 2             ; %1 = payload, %2 = tag
    test %2, TAG_RC_BIT
    jz %%skip                   ; not refcounted: skip
    test %2, %2
    js %%skip                   ; SmallStr: skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

; 128-bit XDECREF — NULL-safe DECREF_VAL (checks tag==TAG_NULL first)
%macro XDECREF_VAL 2            ; %1 = payload, %2 = tag
    test %2, %2
    jz %%skip                   ; tag == TAG_NULL (0), skip
    js %%skip                   ; bit 63 set = SmallStr, skip
    test %2, TAG_RC_BIT
    jz %%skip                   ; bit 8 clear = not refcounted, skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

; ===== Fat result save/restore across DECREF calls =====
; Save fat result (rax=payload, rdx=tag) on machine stack.
; IMPORTANT: All rsp-relative addresses between SAVE and RESTORE need +16 offset.
%macro SAVE_FAT_RESULT 0
    push rdx                   ; save result tag
    push rax                   ; save result payload
%endmacro

; Restore fat result (rax=payload, rdx=tag) from machine stack.
%macro RESTORE_FAT_RESULT 0
    pop rax                    ; restore result payload
    pop rdx                    ; restore result tag
%endmacro

; Undo N VPOP operations by re-advancing the value stack pointer.
; Useful for deopt paths where popped slots are still intact in memory.
%macro VUNDROP 1              ; %1 = number of slots to un-pop
    add r13, %1 * 16
%endmacro

; Swap two 16-byte value stack slots via SSE (no GPR temps needed for the swap)
; Both addresses must be 16-byte aligned (guaranteed by malloc + 16B slot layout)
%macro VSLOT_SWAP 2         ; %1 = addr1, %2 = addr2
    movdqa xmm0, [%1]
    movdqa xmm1, [%2]
    movdqa [%1], xmm1
    movdqa [%2], xmm0
%endmacro

; Mark stack as non-executable and reset to .text section
ASM_INIT

%endif ; MACROS_INC
