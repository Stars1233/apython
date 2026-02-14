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
extern raise_exception
extern obj_incref
extern exc_AttributeError_type
extern exc_TypeError_type
extern exc_NameError_type
extern dict_del
extern dict_get

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
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "cannot set attribute"
    call raise_exception

;; ============================================================================
;; op_store_deref - Store TOS into cell at localsplus[arg]
;;
;; Gets cell from localsplus[arg], sets cell.ob_ref = TOS.
;; INCREFs new value, DECREFs old value.
;; ============================================================================
global op_store_deref
op_store_deref:
    VPOP rax                        ; rax = new value
    lea rdx, [r12 + PyFrame.localsplus]
    mov rdx, [rdx + rcx*8]         ; rdx = cell object

    ; INCREF new value (may be SmallInt)
    push rax
    push rdx
    mov rdi, rax
    call obj_incref
    pop rdx
    pop rax

    ; Get old value from cell
    mov rdi, [rdx + PyCellObject.ob_ref]

    ; Store new value in cell
    mov [rdx + PyCellObject.ob_ref], rax

    ; XDECREF old value
    test rdi, rdi
    jz .sd_done
    DECREF_REG rdi
.sd_done:
    DISPATCH

;; ============================================================================
;; op_delete_deref - Set cell at localsplus[arg] to empty (NULL)
;;
;; DECREFs old value if present.
;; ============================================================================
global op_delete_deref
op_delete_deref:
    lea rax, [r12 + PyFrame.localsplus]
    mov rax, [rax + rcx*8]         ; rax = cell object
    mov rdi, [rax + PyCellObject.ob_ref]
    mov qword [rax + PyCellObject.ob_ref], 0
    ; XDECREF old value
    test rdi, rdi
    jz .dd_done
    DECREF_REG rdi
.dd_done:
    DISPATCH

;; ============================================================================
;; op_delete_fast - Delete local variable (set localsplus[arg] = NULL)
;;
;; DECREF old value if present.
;; ============================================================================
global op_delete_fast
op_delete_fast:
    lea rax, [r12 + PyFrame.localsplus]
    mov rdi, [rax + rcx*8]     ; old value
    mov qword [rax + rcx*8], 0 ; set to NULL
    test rdi, rdi
    jz .df_done
    DECREF_REG rdi
.df_done:
    DISPATCH

;; ============================================================================
;; op_delete_name - Delete name from locals or globals dict
;; ============================================================================
global op_delete_name
op_delete_name:
    mov rsi, [r15 + rcx*8]     ; name
    ; Try locals first
    mov rdi, [r12 + PyFrame.locals]
    test rdi, rdi
    jz .dn_globals
    push rsi
    call dict_del
    pop rsi
    test eax, eax
    jz .dn_ok                  ; found and deleted
.dn_globals:
    mov rdi, [r12 + PyFrame.globals]
    call dict_del
    test eax, eax
    jnz .dn_error
.dn_ok:
    DISPATCH
.dn_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not defined"
    call raise_exception

;; ============================================================================
;; op_delete_global - Delete name from globals dict
;; ============================================================================
global op_delete_global
op_delete_global:
    mov rsi, [r15 + rcx*8]     ; name
    mov rdi, [r12 + PyFrame.globals]
    call dict_del
    test eax, eax
    jnz .dg_error
    DISPATCH
.dg_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not defined"
    call raise_exception

;; ============================================================================
;; op_delete_attr - Delete attribute from object
;;
;; Calls tp_setattr(obj, name, NULL) to delete.
;; Followed by 4 CACHE entries (8 bytes) - WAIT, DELETE_ATTR has no CACHE in 3.12.
;; ============================================================================
global op_delete_attr
op_delete_attr:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov rax, [r15 + rcx*8]     ; name
    mov [rbp-8], rax

    VPOP rdi
    mov [rbp-16], rdi           ; obj

    ; Call tp_setattr(obj, name, NULL) to delete attr
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_setattr]
    test rax, rax
    jz .da_error

    mov rdi, [rbp-16]
    mov rsi, [rbp-8]
    xor edx, edx               ; value = NULL means delete
    call rax

    ; DECREF obj
    mov rdi, [rbp-16]
    call obj_decref

    leave
    DISPATCH

.da_error:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "cannot delete attribute"
    call raise_exception

;; ============================================================================
;; op_delete_subscr - Delete obj[key]
;;
;; Pops key (TOS), pops obj (TOS1).
;; Calls mp_ass_subscript(obj, key, NULL) to delete.
;; ============================================================================
global op_delete_subscr
op_delete_subscr:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    VPOP rsi                    ; key
    VPOP rdi                    ; obj
    mov [rbp-8], rdi            ; save obj
    mov [rbp-16], rsi           ; save key

    ; Call mp_ass_subscript(obj, key, NULL)
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .ds_error
    mov rax, [rax + PyMappingMethods.mp_ass_subscript]
    test rax, rax
    jz .ds_error

    xor edx, edx               ; value = NULL (delete)
    call rax

    ; DECREF key and obj
    mov rdi, [rbp-16]
    DECREF_REG rdi
    mov rdi, [rbp-8]
    DECREF_REG rdi

    leave
    DISPATCH

.ds_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support item deletion"
    call raise_exception
