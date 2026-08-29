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
;    r13  = value stack top pointer (u64[] of Values)
;    r14  = unused (freed by the single-word representation)
;    r15  = unused (freed by the single-word representation)
;    ecx  = opcode arg on handler entry (set by eval_dispatch)
;
;    co_names / co_consts are read from the eval_co_names / eval_co_consts
;    globals via the LOAD_CO_NAMES macro.
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

%include "value.inc"

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


; ==========================================================================
; LEGACY (payload, tag) SUPPORT — being retired
; ==========================================================================
;
; Every value store -- the stack, frame locals, containers and object fields
; -- now holds one NaN-boxed Value (see include/value.inc).  What is left of
; the split form is the runtime function ABI: functions still return
; (rax=payload, edx=tag) and take tags as extra arguments.  The V_PACK /
; V_UNPACK macros bridge the two until P5 finishes the conversion.
;
; Tag encoding (u8):
;   bits 0-6: type tag (TAG_SMALLINT..TAG_PTR)
;   bit 7:    TAG_RC_BIT — payload is a refcounted heap pointer

; Tag constants (u8).  There is no TAG_NONE or TAG_BOOL: None, True and
; False are the immortal heap singletons and travel as TAG_PTR.
TAG_NULL     equ 0           ; null/empty slot
TAG_SMALLINT equ 1           ; payload = int64_t (full 64-bit range)
TAG_I64      equ TAG_SMALLINT
TAG_FLOAT    equ 2           ; payload = double bits (IEEE 754)
TAG_F64      equ TAG_FLOAT
TAG_RC_BIT   equ 0x80        ; bit 7: refcounted flag
TAG_PTR      equ (5 | TAG_RC_BIT)    ; heap PyObject*, refcounted
TAG_SLEEP    equ 7           ; payload = delay_ns (uint64) — async sleep sentinel
TAG_IO_WAIT  equ 8           ; payload = fd | (direction << 32) — async I/O sentinel

; ==========================================================================
; VALUE STACK
;
; One u64 per slot, holding a NaN-boxed Value.  r13 is the top; there is no
; tag sidecar and r15 is free.
; ==========================================================================

; Push a Value.
%macro VPUSH 1                  ; %1 = Value
    mov [r13], %1
    add r13, 8
%endmacro

; Pop a Value.
%macro VPOP 1                   ; %1 = dest
    sub r13, 8
    mov %1, [r13]
%endmacro

; Peek at the top Value without popping.
%macro VPEEK 1                  ; %1 = dest
    mov %1, [r13 - 8]
%endmacro

; --- (payload, tag) shims, for call sites not yet converted ---------------

; Push from an old fat pair.  %2 must not be rax.
%macro VPUSH_VAL 2              ; %1 = payload (clobbered), %2 = tag (clobbered)
    V_PACK %1, %2
    mov [r13], %1
    add r13, 8
%endmacro

; Pop into an old fat pair.
%macro VPOP_VAL 2               ; %1 = payload dest, %2 = tag dest
    sub r13, 8
    mov %1, [r13]
    V_UNPACK %1, %2
%endmacro

; Push a raw int64, boxing it when it falls outside the immediate range.
; %2 must not be rax.
%macro VPUSH_INT 2              ; %1 = int64 (clobbered), %2 = scratch
    V_PACK_I64 %1, %2
    mov [r13], %1
    add r13, 8
%endmacro

; Push raw IEEE-754 double bits.
%macro VPUSH_FLOAT 2            ; %1 = bits (clobbered), %2 = scratch
    V_FROM_F64 %1, %2
    mov [r13], %1
    add r13, 8
%endmacro

; Push the None singleton (owned).  No scratch needed: the binary is linked
; -no-pie, so the address fits an imm32.
%macro VPUSH_NONE 0
    mov qword [r13], none_singleton
    inc qword [rel none_singleton]      ; ob_refcnt is at offset 0
    add r13, 8
%endmacro

; Push True/False (owned) from a 0/1 value.  Clobbers %1.
%macro VPUSH_BOOL 1             ; %1 = 0 (False) or 1 (True)
    test %1, %1
    jz %%false
    lea %1, [rel bool_true]
    jmp %%store
%%false:
    lea %1, [rel bool_false]
%%store:
    inc qword [%1 + PyObject.ob_refcnt]
    mov [r13], %1
    add r13, 8
%endmacro

; Push a heap pointer.  A pointer is its own Value.
%macro VPUSH_PTR 1
    mov [r13], %1
    add r13, 8
%endmacro

; Push an empty slot.
%macro VPUSH_NULL128 0
    mov qword [r13], 0
    add r13, 8
%endmacro

; Undo N pops by re-advancing the stack pointer (deopt paths, where the
; popped slots are still intact in memory).
%macro VUNDROP 1                ; %1 = number of slots
    add r13, %1 * 8
%endmacro

; ==========================================================================
; None / True / False as owned singleton pointers
;
; These have no immediate form: they are the immortal heap singletons.  The
; including file must declare the externs itself (none.asm / bool.asm define
; these symbols and must not also extern them).
; ==========================================================================

; Fat return of None: (rax = none_singleton, edx = TAG_PTR).  Clobbers rax.
%macro RET_NONE 0
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
%endmacro

; Fat return of a bool from a 0/1 value already in rax.  Clobbers rax, rdx.
%macro RET_BOOL_RAX 0
    lea rdx, [rel bool_false]
    test rax, rax
    jz %%false
    lea rdx, [rel bool_true]
%%false:
    mov rax, rdx
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
%endmacro

; Compare a value against the None singleton; ZF is set when it is None.
%macro IS_NONE 2                ; %1 = payload reg/mem, %2 = scratch reg
    lea %2, [rel none_singleton]
    cmp %1, %2
%endmacro

; Load the None singleton (owned) into a register.  Clobbers only %1.
%macro LOAD_NONE 1              ; %1 = dest
    lea %1, [rel none_singleton]
    inc qword [%1 + PyObject.ob_refcnt]
%endmacro

; --- legacy (payload, tag) refcounting and returns ------------------------

; INCREF a fat value — only when the tag says the payload is refcounted.
%macro INCREF_VAL 2             ; %1 = payload, %2 = tag reg
    test %2, TAG_RC_BIT
    jz %%skip
    inc qword [%1 + PyObject.ob_refcnt]
    %%skip:
%endmacro

; DECREF a fat value.  Clobbers rdi and all caller-saved regs when the
; object is freed.
%macro DECREF_VAL 2             ; %1 = payload, %2 = tag reg
    test %2, TAG_RC_BIT
    jz %%skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

; NULL-safe DECREF_VAL.
%macro XDECREF_VAL 2            ; %1 = payload, %2 = tag reg
    test %2, TAG_RC_BIT
    jz %%skip
    test %1, %1
    jz %%skip
    dec qword [%1 + PyObject.ob_refcnt]
    jnz %%skip
    mov rdi, %1
    call obj_dealloc
    %%skip:
%endmacro

; Fat return of NULL/error: (rax = 0, edx = TAG_NULL = 0).
%macro RET_NULL 0
    xor eax, eax
    xor edx, edx
%endmacro

; Fat return tag for an inline integer; the caller sets rax.
%macro RET_TAG_SMALLINT 0
    mov edx, TAG_SMALLINT
%endmacro

; Push one pointer Value as a single-argument array on the machine stack.
%macro SPUSH_PTR 1              ; %1 = PyObject*
    sub rsp, 16                 ; one Value; 16 keeps rsp 16-byte aligned
    mov [rsp], %1
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

; ==========================================================================
; FRAME STACK PUSH — push onto a saved frame's value stack (stack_ptr)
; Used by generator resume to push sent/None values before calling eval_frame.
; ==========================================================================

; Push None onto a frame's saved value stack.
; %1 = frame register, %2 = scratch register (clobbered)
%macro FRAME_PUSH_NONE 2
    mov %2, [%1 + PyFrame.stack_ptr]
    mov qword [%2], none_singleton
    inc qword [rel none_singleton]      ; ob_refcnt is at offset 0
    add %2, 8
    mov [%1 + PyFrame.stack_ptr], %2
%endmacro

; Push a Value onto a frame's saved value stack.
; %1 = frame register, %2 = Value register, %3 = scratch register
%macro FRAME_PUSH_VALUE 3
    mov %3, [%1 + PyFrame.stack_ptr]
    mov [%3], %2
    add %3, 8
    mov [%1 + PyFrame.stack_ptr], %3
%endmacro

; Mark stack as non-executable and reset to .text section
ASM_INIT

%endif ; MACROS_INC
