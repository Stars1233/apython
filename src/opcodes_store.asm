; opcodes_store.asm - Opcode handlers for storing values
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
extern dict_set

;; ============================================================================
;; op_store_fast - Store TOS into localsplus[arg]
;;
;; Pops value from stack, stores in fast local slot, XDECREF old value.
;; VPOP does not clobber ecx (it only does sub r13,8 / mov reg,[r13]).
;; ============================================================================
global op_store_fast
op_store_fast:
    ; ecx = arg (slot index)
    VPOP rax                    ; rax = new value
    lea rdx, [r12 + PyFrame.localsplus]
    mov rdi, [rdx + rcx*8]     ; rdi = old value
    mov [rdx + rcx*8], rax     ; store new value
    ; XDECREF old value
    test rdi, rdi
    jz .done
    DECREF_REG rdi
.done:
    DISPATCH

;; ============================================================================
;; op_store_name - Store TOS under co_names[arg] in locals or globals dict
;;
;; If frame->locals is not NULL, store there; otherwise store in globals.
;; Uses dict_set(dict, key, value) extern.
;; dict_set signature: dict_set(PyDictObject *dict, PyObject *key, PyObject *value)
;; ============================================================================
global op_store_name
op_store_name:
    ; ecx = arg (index into co_names)
    ; Get name string before popping (VPOP does not clobber ecx)
    mov r8, [r15 + rcx*8]      ; r8 = name (key) - caller-saved, safe temp
    VPOP r9                    ; r9 = value to store

    ; Determine target dict: locals if present, else globals
    mov rdi, [r12 + PyFrame.locals]
    test rdi, rdi
    jnz .have_dict
    mov rdi, [r12 + PyFrame.globals]
.have_dict:
    ; dict_set(dict, key, value)
    ; rdi = dict (already set)
    mov rsi, r8                ; rsi = name (key)
    mov rdx, r9                ; rdx = value
    call dict_set
    DISPATCH
