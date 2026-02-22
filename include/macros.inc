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
;    r13  = value stack top pointer (payload array, u64[])
;    r14  = co_consts tuple data pointer (&tuple.ob_item[0])
;    r15  = tag stack top pointer (sidecar tag array, u8[])
;    ecx  = opcode arg on handler entry (set by eval_dispatch)
;
;    co_names accessed via LOAD_CO_NAMES / LOAD_CO_NAMES_TAGS macros
;    (reads eval_co_names / eval_co_names_tags globals).
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

; Value stack pop (payload only)
%macro VPOP 1
    sub r13, 8
    sub r15, 1
    mov %1, [r13]
%endmacro

; Value stack peek (top of stack without popping)
%macro VPEEK 1
    mov %1, [r13 - 8]
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
    mov [rel eval_saved_r13], r13  ; save value stack ptr for exception unwind
    mov [rel eval_saved_r15], r15  ; save tag stack ptr for exception unwind
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

; Load co_names payload pointer (&tuple.ob_item[0]) into a register
%macro LOAD_CO_NAMES 1          ; %1 = dest reg
    mov %1, [rel eval_co_names]
%endmacro

; Load co_names tag pointer (&tuple.ob_item_tags[0]) into a register
%macro LOAD_CO_NAMES_TAGS 1     ; %1 = dest reg
    mov %1, [rel eval_co_names_tags]
%endmacro

; ==========================================================================
; VALUE64 + TAG (u8) SUPPORT
; ==========================================================================
;
; Value64: 64-bit payload + 8-bit tag stored in parallel sidecar arrays.
; Value stack: r13 → u64[] payloads, r15 → u8[] tags.
; Containers (list/tuple/dict): ob_item → u64[], ob_item_tags → u8[].
; Frame locals: localsplus → u64[], locals_tag_base → u8[].
; Functions return (rax=payload, edx=tag).
;
; Tag encoding (u8):
;   bits 0-6: type tag (TAG_SMALLINT..TAG_PTR)
;   bit 7:    TAG_RC_BIT — payload is a refcounted heap pointer

; Tag constants (u8)
TAG_NULL     equ 0           ; null/empty slot
TAG_SMALLINT equ 1           ; payload = int64_t (full 64-bit range)
TAG_I64      equ TAG_SMALLINT
TAG_FLOAT    equ 2           ; payload = double bits (IEEE 754)
TAG_F64      equ TAG_FLOAT
TAG_NONE     equ 3           ; payload = 0 (unused)
TAG_BOOL     equ 4           ; payload = 0 (False) or 1 (True)
TAG_RC_BIT   equ 0x80        ; bit 7: refcounted flag
TAG_PTR      equ (5 | TAG_RC_BIT)    ; heap PyObject*, refcounted
TAG_SLEEP    equ 7           ; payload = delay_ns (uint64) — async sleep sentinel
TAG_IO_WAIT  equ 8           ; payload = fd | (direction << 32) — async I/O sentinel
TAG_TASK     equ (9 | TAG_RC_BIT)    ; payload = AsyncTask*, RC bit SET
TAG_WAIT_FOR equ (10 | TAG_RC_BIT)   ; payload = WaitForAwaitable*, RC bit SET

; Value stack push (r13 = payload top, r15 = tag top)
%macro VPUSH_VAL 2              ; %1 = payload reg, %2 = tag reg
    mov [r13], %1
%ifidni %2, rax
    mov byte [r15], al
%elifidni %2, eax
    mov byte [r15], al
%elifidni %2, rbx
    mov byte [r15], bl
%elifidni %2, ebx
    mov byte [r15], bl
%elifidni %2, rcx
    mov byte [r15], cl
%elifidni %2, ecx
    mov byte [r15], cl
%elifidni %2, rdx
    mov byte [r15], dl
%elifidni %2, edx
    mov byte [r15], dl
%elifidni %2, rsi
    mov byte [r15], sil
%elifidni %2, esi
    mov byte [r15], sil
%elifidni %2, rdi
    mov byte [r15], dil
%elifidni %2, edi
    mov byte [r15], dil
%elifidni %2, r8
    mov byte [r15], r8b
%elifidni %2, r8d
    mov byte [r15], r8b
%elifidni %2, r9
    mov byte [r15], r9b
%elifidni %2, r9d
    mov byte [r15], r9b
%elifidni %2, r10
    mov byte [r15], r10b
%elifidni %2, r10d
    mov byte [r15], r10b
%elifidni %2, r11
    mov byte [r15], r11b
%elifidni %2, r11d
    mov byte [r15], r11b
%elifidni %2, r12
    mov byte [r15], r12b
%elifidni %2, r12d
    mov byte [r15], r12b
%elifidni %2, r13
    mov byte [r15], r13b
%elifidni %2, r13d
    mov byte [r15], r13b
%elifidni %2, r14
    mov byte [r15], r14b
%elifidni %2, r14d
    mov byte [r15], r14b
%elifidni %2, r15
    mov byte [r15], r15b
%elifidni %2, r15d
    mov byte [r15], r15b
%else
    mov byte [r15], %2
%endif
    add r13, 8
    add r15, 1
%endmacro

; Value stack pop
%macro VPOP_VAL 2               ; %1 = payload dest reg, %2 = tag dest reg
    sub r13, 8
    sub r15, 1
    mov %1, [r13]
    movzx %2, byte [r15]
%endmacro

; Push SmallInt (payload = raw int64, no encode needed)
%macro VPUSH_INT 1              ; %1 = payload reg containing int64
    mov [r13], %1
    mov byte [r15], TAG_SMALLINT
    add r13, 8
    add r15, 1
%endmacro

; Push float (payload = IEEE 754 double bits)
%macro VPUSH_FLOAT 1            ; %1 = payload reg containing double bits
    mov [r13], %1
    mov byte [r15], TAG_FLOAT
    add r13, 8
    add r15, 1
%endmacro

; Push None (payload = 0, tag = TAG_NONE)
%macro VPUSH_NONE 0
    mov qword [r13], 0
    mov byte [r15], TAG_NONE
    add r13, 8
    add r15, 1
%endmacro

; Push bool (payload = 0 or 1)
%macro VPUSH_BOOL 1             ; %1 = payload reg (0=False, 1=True)
    mov [r13], %1
    mov byte [r15], TAG_BOOL
    add r13, 8
    add r15, 1
%endmacro

; Push heap pointer (refcounted)
%macro VPUSH_PTR 1              ; %1 = payload reg containing PyObject*
    mov [r13], %1
    mov byte [r15], TAG_PTR
    add r13, 8
    add r15, 1
%endmacro

; Push null/empty slot (16 bytes of zero, tag = TAG_NULL = 0)
%macro VPUSH_NULL128 0
    mov qword [r13], 0
    mov byte [r15], TAG_NULL
    add r13, 8
    add r15, 1
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
    movzx %1, byte [r15 - 1]
%endmacro

; Peek payload of top-of-stack (without popping)
%macro VPEEK_PAYLOAD 1          ; %1 = dest reg
    mov %1, [r13 - 8]
%endmacro

; Peek 128-bit value at offset from TOS (0 = TOS, 1 = second, etc.)
%macro VPEEK_AT_VAL 3           ; %1 = payload dest, %2 = tag dest, %3 = offset (0=TOS)
    mov %1, [r13 - 8 - %3*8]
    movzx %2, byte [r15 - 1 - %3]
%endmacro

; Value64 INCREF — only if TAG_RC_BIT (bit 7) is set in the u8 tag.
%macro INCREF_VAL 2             ; %1 = payload, %2 = tag (reg with u8 tag value)
    test %2, TAG_RC_BIT
    jz %%skip                  ; no RC bit — not refcounted
    inc qword [%1 + PyObject.ob_refcnt]
    %%skip:
%endmacro

; Value64 DECREF — only if TAG_RC_BIT (bit 7) is set in the u8 tag.
; Calls obj_dealloc if refcount reaches zero. Clobbers rdi + all caller-saved.
%macro DECREF_VAL 2             ; %1 = payload, %2 = tag (reg with u8 tag value)
    test %2, TAG_RC_BIT
    jz %%skip                  ; no RC bit — not refcounted
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

; Value64 XDECREF — NULL-safe DECREF_VAL
%macro XDECREF_VAL 2            ; %1 = payload, %2 = tag (reg with u8 tag value)
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
    add r13, %1 * 8
    add r15, %1
%endmacro

; Swap two 16-byte value stack slots via SSE (no GPR temps needed for the swap)
; Both addresses must be 16-byte aligned (guaranteed by malloc + 16B slot layout)
%macro VSLOT_SWAP 2         ; %1 = addr1, %2 = addr2
    movdqa xmm0, [%1]
    movdqa xmm1, [%2]
    movdqa [%1], xmm1
    movdqa [%2], xmm0
%endmacro

; ==========================================================================
; FRAME STACK PUSH — push onto a saved frame's value stack (stack_ptr/stack_tag_ptr)
; Used by generator resume to push sent/None values before calling eval_frame.
; ==========================================================================

; Push None onto a frame's saved value stack.
; %1 = frame register, %2 = scratch register (clobbered)
%macro FRAME_PUSH_NONE 2
    mov %2, [%1 + PyFrame.stack_ptr]
    mov qword [%2], 0
    add %2, 8
    mov [%1 + PyFrame.stack_ptr], %2
    mov %2, [%1 + PyFrame.stack_tag_ptr]
    mov byte [%2], TAG_NONE
    inc %2
    mov [%1 + PyFrame.stack_tag_ptr], %2
%endmacro

; Push a value onto a frame's saved value stack.
; %1 = frame register, %2 = payload register (64-bit), %3 = tag byte, %4 = scratch register
%macro FRAME_PUSH_VAL 4
    mov %4, [%1 + PyFrame.stack_ptr]
    mov [%4], %2
    add %4, 8
    mov [%1 + PyFrame.stack_ptr], %4
    mov %4, [%1 + PyFrame.stack_tag_ptr]
    mov byte [%4], %3
    inc %4
    mov [%1 + PyFrame.stack_tag_ptr], %4
%endmacro

; Mark stack as non-executable and reset to .text section
ASM_INIT

%endif ; MACROS_INC
