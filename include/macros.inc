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

; File-local function with RBP frame + local space (not exported)
%macro DEF_FUNC_LOCAL 2
%1:
    push rbp
    mov rbp, rsp
    sub rsp, %2
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

; Value stack pop (16 bytes/slot)
%macro VPOP 1
    sub r13, 16
    mov %1, [r13]
%endmacro

; Value stack peek (top of stack without popping, 16 bytes/slot)
%macro VPEEK 1
    mov %1, [r13 - 16]
%endmacro

; Reference counting (heap pointers only — callers must not pass SmallInts)
%macro INCREF 1
    inc qword [%1 + PyObject.ob_refcnt]
    %%skip:
%endmacro

%macro DECREF 1
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
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

%macro XDECREF 1
    test %1, %1
    jz %%skip
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
; Fat values: 64-bit payload + 64-bit tag. Used on value stack, localsplus,
; and heap containers (tuples, lists, dicts, sets).
;
; Tag word encoding (two modes, distinguished by bit 63):
;
;   bit 63 = 0:  Normal tag mode
;     bits 0-7:   type tag (TAG_SMALLINT..TAG_PTR)
;     bit  8:     refcounted flag (1 = payload is heap pointer needing INCREF/DECREF)
;     bits 9-62:  reserved (0)
;     bit  63:    0
;
;   bit 63 = 1:  SmallStr mode (inline string <= 14 bytes)
;     bits 0-7:   TAG_SMALLSTR (0x06) — fixed, prevents tag collision
;     bits 8-55:  string bytes 8-13 (6 bytes, zero-padded)
;     bits 56-62: string length (0-14)
;     bit  63:    1 (SmallStr marker)
;     payload:    string bytes 0-7 (8 bytes, zero-padded)
;
;   TAG_SMALLSTR in bits 0-7 ensures:
;     - test edx, edx → nonzero (never confused with TAG_NULL)
;     - cmp edx, TAG_* → never matches any other tag
;   Tags with TAG_RC_BIT set (TAG_PTR, TAG_TASK) are refcounted heap pointers.

; Tag constants (normal mode, bit 63 = 0)
TAG_NULL     equ 0           ; null/empty slot
TAG_SMALLINT equ 1           ; payload = int64_t (full 64-bit range)
TAG_FLOAT    equ 2           ; payload = double bits (IEEE 754)
TAG_NONE     equ 3           ; payload = 0 (unused)
TAG_BOOL     equ 4           ; payload = 0 (False) or 1 (True)
TAG_PTR      equ 0x105       ; 5 | TAG_RC_BIT — heap PyObject*, refcounted
TAG_RC_BIT   equ (1 << 8)    ; bit 8: refcounted flag
TAG_SMALLSTR equ 0x06        ; SmallStr low byte — always in tag bits 0-7
TAG_SLEEP    equ 7           ; payload = delay_ns (uint64) — async sleep sentinel
TAG_IO_WAIT  equ 8           ; payload = fd | (direction << 32) — async I/O sentinel
TAG_TASK     equ 0x109       ; payload = AsyncTask*, RC bit SET (9 | TAG_RC_BIT)
TAG_WAIT_FOR equ 0x10A       ; payload = WaitForAwaitable*, RC bit SET (10 | TAG_RC_BIT)

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

; Set fat return to NULL error (rax=0, edx=TAG_NULL=0)
; Use on error paths in functions returning (rax=payload, edx=tag).
%macro RET_NULL 0
    xor eax, eax
    xor edx, edx
%endmacro

; Set fat return tag to SmallInt (edx=TAG_SMALLINT)
; Caller sets rax=payload. Use when returning inline integers.
%macro RET_TAG_SMALLINT 0
    mov edx, TAG_SMALLINT
%endmacro

; Push fat arg onto machine stack (rsp-based, for building tp_call arg arrays)
; Use when building a 16-byte-stride args array on the stack for tp_call.
%macro SPUSH_PTR 1              ; %1 = payload reg containing PyObject*
    sub rsp, 16
    mov [rsp], %1
    mov qword [rsp + 8], TAG_PTR
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

; 128-bit INCREF — only if TAG_RC_BIT set AND not SmallStr (bit 63).
; SmallStr tags pack string data in bits 8-55, so bit 8 may be set.
; Must check bit 63 first to exclude SmallStr before testing TAG_RC_BIT.
%macro INCREF_VAL 2             ; %1 = payload, %2 = tag (must be 64-bit reg)
    bt %2, 63
    jc %%skip                  ; SmallStr — not refcounted
    test %2, TAG_RC_BIT
    jz %%skip                  ; no RC bit — not refcounted
    inc qword [%1 + PyObject.ob_refcnt]
    %%skip:
%endmacro

; 128-bit DECREF — only if TAG_RC_BIT set AND not SmallStr (bit 63).
; Calls obj_dealloc if refcount reaches zero. Clobbers rdi + all caller-saved.
%macro DECREF_VAL 2             ; %1 = payload, %2 = tag (must be 64-bit reg)
    bt %2, 63
    jc %%skip                  ; SmallStr — not refcounted
    test %2, TAG_RC_BIT
    jz %%skip                  ; no RC bit — not refcounted
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

; 128-bit XDECREF — NULL-safe DECREF_VAL
%macro XDECREF_VAL 2            ; %1 = payload, %2 = tag (must be 64-bit reg)
    bt %2, 63
    jc %%skip                  ; SmallStr — not refcounted
    test %2, TAG_RC_BIT
    jz %%skip                  ; no RC bit — not refcounted
    test %1, %1
    jz %%skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

; ===== SmallStr helpers =====
; Extract SmallStr length from tag register into dest register
; SmallStr tag: bits 56-62 = length, bit 63 = SmallStr marker
%macro SMALLSTR_LEN 2          ; %1 = dest reg, %2 = tag reg
    mov %1, %2
    shr %1, 56
    and %1, 0x7F
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
