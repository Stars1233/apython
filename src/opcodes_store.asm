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

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern eval_dispatch
extern obj_dealloc
extern obj_decref
extern dict_set
extern fatal_error

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

;; ============================================================================
;; op_store_global - Store TOS under co_names[arg] in globals dict
;;
;; Same as store_name but always uses globals.
;; ============================================================================
global op_store_global
op_store_global:
    ; ecx = arg (index into co_names)
    mov r8, [r15 + rcx*8]      ; r8 = name (key)
    VPOP r9                    ; r9 = value to store

    ; Always store in globals
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, r8                ; rsi = name (key)
    mov rdx, r9                ; rdx = value
    call dict_set
    DISPATCH

;; ============================================================================
;; op_store_attr - Store TOS-1 as attribute of TOS
;;
;; Python 3.12 STORE_ATTR (opcode 95):
;;   ecx = name index in co_names
;;
;; Stack: ... | value | obj |  (obj=TOS, value=TOS-1)
;; Pops obj, pops value, sets obj.name = value via tp_setattr.
;; DECREF obj and value after the store.
;; Followed by 4 CACHE entries (8 bytes) that must be skipped.
;; ============================================================================
global op_store_attr
op_store_attr:
    push rbp
    mov rbp, rsp
    sub rsp, 32               ; [rbp-8]=obj, [rbp-16]=value, [rbp-24]=name

    ; Get name
    mov rax, [r15 + rcx*8]
    mov [rbp-24], rax

    ; Pop obj (TOS)
    VPOP rdi
    mov [rbp-8], rdi

    ; Pop value
    VPOP rdi
    mov [rbp-16], rdi

    ; Check tp_setattr
    mov rdi, [rbp-8]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_setattr]
    test rax, rax
    jz .sa_no_setattr

    ; Call tp_setattr(obj, name, value)
    mov rdi, [rbp-8]
    mov rsi, [rbp-24]
    mov rdx, [rbp-16]
    call rax

    ; DECREF value
    mov rdi, [rbp-16]
    call obj_decref
    ; DECREF obj
    mov rdi, [rbp-8]
    call obj_decref

    add rbx, 8                ; skip 4 CACHE entries
    leave
    DISPATCH

.sa_no_setattr:
    CSTRING rdi, "AttributeError: cannot set attribute"
    call fatal_error
