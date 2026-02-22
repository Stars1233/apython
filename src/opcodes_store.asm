; opcodes_store.asm - Opcode handlers for storing values
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack payload top pointer
;   r14 = co_consts payload pointer (&tuple.ob_item[0])
;   r15 = value stack tag top pointer
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
extern eval_saved_rbx
extern eval_saved_r13
extern eval_saved_r15
extern eval_co_names
extern trace_opcodes
extern opcode_table
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
extern property_type
extern property_descr_set
extern dunder_set
extern dunder_call_3
extern dunder_lookup

;; --- Named frame-layout constants ---

; op_store_attr: rbp-frame (48 bytes)
SA_OBJ    equ 8
SA_VAL    equ 16
SA_NAME   equ 24
SA_DESC   equ 32    ; general descriptor (MRO walk)
SA_OTAG   equ 40
SA_VTAG   equ 48
SA_FRAME  equ 48

; op_delete_attr: rbp-frame (16 bytes)
DA_NAME   equ 8
DA_OBJ    equ 16
DA_FRAME  equ 16

; op_delete_subscr: rbp-frame (32 bytes)
DS_OBJ    equ 8
DS_KEY    equ 16
DS_OTAG   equ 24
DS_KTAG   equ 32
DS_FRAME  equ 32

;; ============================================================================
;; op_store_fast - Store TOS into localsplus[arg]
;;
;; Pops value from stack, stores in fast local slot, XDECREF old value.
;; VPOP does not clobber ecx (it only does sub r13,8 / mov reg,[r13]).
;; ============================================================================
DEF_FUNC_BARE op_store_fast
    ; ecx = arg (slot index)
    VPOP_VAL rax, r8             ; rax = new value payload, r8 = new value tag
    mov rsi, [r12 + PyFrame.locals_tag_base]
    mov rdi, [r12 + PyFrame.localsplus + rcx*8]       ; rdi = old value (payload)
    movzx r9, byte [rsi + rcx]                        ; r9 = old value tag
    mov [r12 + PyFrame.localsplus + rcx*8], rax       ; store new payload
    mov byte [rsi + rcx], r8b                         ; store new tag
    ; XDECREF_VAL old value (tag-aware)
    XDECREF_VAL rdi, r9
    DISPATCH
END_FUNC op_store_fast

;; ============================================================================
;; op_store_name - Store TOS under co_names[arg] in locals or globals dict
;;
;; If frame->locals is not NULL, store there; otherwise store in globals.
;; Uses dict_set(dict, key, value) extern.
;; dict_set signature: dict_set(PyDictObject *dict, PyObject *key, PyObject *value)
;; ============================================================================
DEF_FUNC_BARE op_store_name
    ; ecx = arg (index into co_names)
    ; Get name string before popping (payload array: 8-byte stride)
    shl ecx, 3
    LOAD_CO_NAMES r8
    mov r8, [r8 + rcx]        ; r8 = name (key) - caller-saved, safe temp
    VPOP_VAL r9, r10           ; r9 = value payload, r10 = value tag

    ; Determine target dict: locals if present, else globals
    mov rdi, [r12 + PyFrame.locals]
    test rdi, rdi
    jnz .have_dict
    mov rdi, [r12 + PyFrame.globals]
.have_dict:
    ; dict_set(dict, key, value, value_tag, key_tag)
    ; rdi = dict (already set)
    mov rsi, r8                ; rsi = name (key)
    mov rdx, r9                ; rdx = value payload
    mov rcx, r10               ; rcx = value tag
    mov r8d, TAG_PTR
    push r9
    push r10
    call dict_set
    pop r10
    pop r9
    ; DECREF value to release the stack's reference (dict_set INCREFed it)
    DECREF_VAL r9, r10
    DISPATCH
END_FUNC op_store_name

;; ============================================================================
;; op_store_global - Store TOS under co_names[arg] in globals dict
;;
;; Same as store_name but always uses globals.
;; ============================================================================
DEF_FUNC_BARE op_store_global
    ; ecx = arg (index into co_names, payload array: 8-byte stride)
    shl ecx, 3
    LOAD_CO_NAMES r8
    mov r8, [r8 + rcx]        ; r8 = name (key)
    VPOP_VAL r9, r10           ; r9 = value payload, r10 = value tag

    ; Always store in globals
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, r8                ; rsi = name (key)
    mov rdx, r9                ; rdx = value payload
    mov rcx, r10               ; rcx = value tag
    mov r8d, TAG_PTR
    push r9
    push r10
    call dict_set
    pop r10
    pop r9
    ; DECREF value to release the stack's reference (dict_set INCREFed it)
    DECREF_VAL r9, r10
    DISPATCH
END_FUNC op_store_global

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
DEF_FUNC op_store_attr, SA_FRAME

    ; Get name (payload array: 8-byte stride)
    shl ecx, 3
    LOAD_CO_NAMES rax
    mov rax, [rax + rcx]
    mov [rbp - SA_NAME], rax

    ; Pop obj (TOS)
    VPOP_VAL rdi, rax
    mov [rbp - SA_OBJ], rdi
    mov [rbp - SA_OTAG], rax

    ; Pop value
    VPOP_VAL rdi, rax
    mov [rbp - SA_VAL], rdi
    mov [rbp - SA_VTAG], rax

    ; Non-pointer obj can't have attrs set (SmallInt, Float, None, Bool)
    cmp qword [rbp - SA_OTAG], TAG_PTR
    jne .sa_no_setattr

    ; Check for property/descriptor in type dict (walk MRO) before regular setattr
    mov rdi, [rbp - SA_OBJ]       ; obj
    mov rcx, [rdi + PyObject.ob_type]  ; rcx = type (walks chain)

.sa_walk_mro:
    test rcx, rcx
    jz .sa_no_property

    mov rdi, [rcx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .sa_walk_next

    push rcx                      ; save current type
    mov rsi, [rbp - SA_NAME]      ; name
    mov edx, TAG_PTR
    call dict_get
    pop rcx
    test edx, edx
    jnz .sa_found_in_type         ; found attr in type dict
.sa_walk_next:
    mov rcx, [rcx + PyTypeObject.tp_base]
    jmp .sa_walk_mro

.sa_found_in_type:

    ; Check if it's a descriptor (only TAG_PTR can be a descriptor)
    cmp edx, TAG_PTR
    jne .sa_no_property           ; non-pointer — not a descriptor
    mov rcx, [rax + PyObject.ob_type]

    ; Check property first (fast path)
    lea rdx, [rel property_type]
    cmp rcx, rdx
    jne .sa_check_general_set

    ; Found property descriptor — call fset(obj, value, ecx=value_tag)
    mov rdi, rax                  ; property
    mov rsi, [rbp - SA_OBJ]      ; obj
    mov rdx, [rbp - SA_VAL]      ; value
    mov ecx, [rbp - SA_VTAG]     ; value tag
    call property_descr_set
    jmp .sa_descr_cleanup

.sa_check_general_set:
    ; Check for general __set__ on heaptype descriptor
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .sa_no_property

    ; Save the descriptor for potential __set__ call
    mov [rbp - SA_DESC], rax      ; save descriptor (borrowed ref, still in dict)

    ; Check if descriptor's type has __set__
    mov rdi, rcx                  ; descriptor's type
    lea rsi, [rel dunder_set]
    call dunder_lookup
    test edx, edx
    jz .sa_no_property

    ; Has __set__! Call descriptor.__set__(obj, value)
    mov rdi, [rbp - SA_DESC]     ; descriptor
    mov rsi, [rbp - SA_OBJ]     ; obj
    mov rdx, [rbp - SA_VAL]     ; value
    lea rcx, [rel dunder_set]
    mov r8d, [rbp - SA_VTAG]    ; value tag
    call dunder_call_3
    ; DECREF result if non-NULL
    test edx, edx
    jz .sa_descr_cleanup
    DECREF_VAL rax, rdx

.sa_descr_cleanup:

    ; DECREF value (tag-aware)
    mov rdi, [rbp - SA_VAL]
    mov rsi, [rbp - SA_VTAG]
    DECREF_VAL rdi, rsi
    ; DECREF obj (tag-aware)
    mov rdi, [rbp - SA_OBJ]
    mov rsi, [rbp - SA_OTAG]
    DECREF_VAL rdi, rsi

    add rbx, 8
    leave
    DISPATCH

.sa_no_property:
    ; Check tp_setattr
    mov rdi, [rbp - SA_OBJ]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_setattr]
    test rax, rax
    jz .sa_no_setattr

    ; Call tp_setattr(obj, name, value, ecx=value_tag)
    mov rdi, [rbp - SA_OBJ]
    mov rsi, [rbp - SA_NAME]
    mov rdx, [rbp - SA_VAL]
    mov ecx, [rbp - SA_VTAG]
    call rax

    ; DECREF value (tag-aware)
    mov rdi, [rbp - SA_VAL]
    mov rsi, [rbp - SA_VTAG]
    DECREF_VAL rdi, rsi
    ; DECREF obj (tag-aware)
    mov rdi, [rbp - SA_OBJ]
    mov rsi, [rbp - SA_OTAG]
    DECREF_VAL rdi, rsi

    add rbx, 8                ; skip 4 CACHE entries
    leave
    DISPATCH

.sa_no_setattr:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "cannot set attribute"
    call raise_exception
END_FUNC op_store_attr

;; ============================================================================
;; op_store_deref - Store TOS into cell at localsplus[arg]
;;
;; Gets cell from localsplus[arg], sets cell.ob_ref = TOS.
;; Ownership transfers from stack to cell (no INCREF needed).
;; DECREFs old cell value.
;; ============================================================================
DEF_FUNC_BARE op_store_deref
    VPOP_VAL rax, r8               ; rax = new payload, r8 = new tag
    mov rdx, [r12 + PyFrame.localsplus + rcx*8]  ; rdx = cell object (payload)

    ; Ownership transfers from stack to cell - no INCREF needed

    ; Get old value + tag from cell
    mov rdi, [rdx + PyCellObject.ob_ref]
    mov rsi, [rdx + PyCellObject.ob_ref_tag]

    ; Store new value + tag in cell
    mov [rdx + PyCellObject.ob_ref], rax
    mov [rdx + PyCellObject.ob_ref_tag], r8

    ; DECREF old value (tag-aware, handles NULL automatically)
    DECREF_VAL rdi, rsi
    DISPATCH
END_FUNC op_store_deref

;; ============================================================================
;; op_delete_deref - Set cell at localsplus[arg] to empty (NULL)
;;
;; DECREFs old value if present.
;; ============================================================================
DEF_FUNC_BARE op_delete_deref
    mov rax, [r12 + PyFrame.localsplus + rcx*8]  ; rax = cell object (payload)
    mov rdi, [rax + PyCellObject.ob_ref]
    mov rsi, [rax + PyCellObject.ob_ref_tag]
    mov qword [rax + PyCellObject.ob_ref], 0
    mov qword [rax + PyCellObject.ob_ref_tag], 0   ; TAG_NULL
    ; DECREF old value (tag-aware)
    DECREF_VAL rdi, rsi
    DISPATCH
END_FUNC op_delete_deref

;; ============================================================================
;; op_delete_fast - Delete local variable (set localsplus[arg] = NULL)
;;
;; DECREF old value if present.
;; ============================================================================
DEF_FUNC_BARE op_delete_fast
    mov rdi, [r12 + PyFrame.localsplus + rcx*8]       ; old value (payload)
    mov rsi, [r12 + PyFrame.locals_tag_base]
    movzx rsi, byte [rsi + rcx]                       ; old value tag
    mov qword [r12 + PyFrame.localsplus + rcx*8], 0   ; clear payload
    mov rdx, [r12 + PyFrame.locals_tag_base]
    mov byte [rdx + rcx], 0                           ; clear tag
    XDECREF_VAL rdi, rsi
    DISPATCH
END_FUNC op_delete_fast

;; ============================================================================
;; op_delete_name - Delete name from locals or globals dict
;; ============================================================================
DEF_FUNC_BARE op_delete_name
    shl ecx, 3                ; payload array: 8-byte stride
    LOAD_CO_NAMES rsi
    mov rsi, [rsi + rcx]      ; name
    ; Try locals first
    mov rdi, [r12 + PyFrame.locals]
    test rdi, rdi
    jz .dn_globals
    push rsi
    mov edx, TAG_PTR
    call dict_del
    pop rsi
    test eax, eax
    jz .dn_ok                  ; found and deleted
.dn_globals:
    mov rdi, [r12 + PyFrame.globals]
    mov edx, TAG_PTR
    call dict_del
    test eax, eax
    jnz .dn_error
.dn_ok:
    DISPATCH
.dn_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not defined"
    call raise_exception
END_FUNC op_delete_name

;; ============================================================================
;; op_delete_global - Delete name from globals dict
;; ============================================================================
DEF_FUNC_BARE op_delete_global
    shl ecx, 3                ; payload array: 8-byte stride
    LOAD_CO_NAMES rsi
    mov rsi, [rsi + rcx]      ; name
    mov rdi, [r12 + PyFrame.globals]
    mov edx, TAG_PTR
    call dict_del
    test eax, eax
    jnz .dg_error
    DISPATCH
.dg_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not defined"
    call raise_exception
END_FUNC op_delete_global

;; ============================================================================
;; op_delete_attr - Delete attribute from object
;;
;; Calls tp_setattr(obj, name, NULL) to delete.
;; Followed by 4 CACHE entries (8 bytes) - WAIT, DELETE_ATTR has no CACHE in 3.12.
;; ============================================================================
DEF_FUNC op_delete_attr, DA_FRAME

    shl ecx, 3                ; payload array: 8-byte stride
    LOAD_CO_NAMES rax
    mov rax, [rax + rcx]      ; name
    mov [rbp - DA_NAME], rax

    VPOP_VAL rdi, rax
    mov [rbp - DA_OBJ], rdi

    ; Non-pointer obj can't have attrs deleted
    cmp rax, TAG_PTR
    jne .da_error

    ; Call tp_setattr(obj, name, NULL) to delete attr
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_setattr]
    test rax, rax
    jz .da_error_decref

    mov rdi, [rbp - DA_OBJ]
    mov rsi, [rbp - DA_NAME]
    xor edx, edx               ; value = NULL means delete
    xor ecx, ecx               ; value tag = TAG_NULL
    call rax

    ; DECREF obj
    mov rdi, [rbp - DA_OBJ]
    call obj_decref

    leave
    DISPATCH

.da_error_decref:
    mov rdi, [rbp - DA_OBJ]
    call obj_decref
.da_error:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "cannot delete attribute"
    call raise_exception
END_FUNC op_delete_attr

;; ============================================================================
;; op_delete_subscr - Delete obj[key]
;;
;; Pops key (TOS), pops obj (TOS1).
;; Calls mp_ass_subscript(obj, key, NULL) to delete.
;; ============================================================================
DEF_FUNC op_delete_subscr, DS_FRAME

    VPOP_VAL rsi, rax            ; key + tag
    VPOP_VAL rdi, rcx            ; obj + tag
    mov [rbp - DS_OBJ], rdi     ; save obj
    mov [rbp - DS_KEY], rsi     ; save key
    mov [rbp - DS_OTAG], rcx    ; save obj tag
    mov [rbp - DS_KTAG], rax    ; save key tag

    ; Non-pointer obj can't have items deleted
    cmp qword [rbp - DS_OTAG], TAG_PTR
    jne .ds_error

    ; Call mp_ass_subscript(obj, key, NULL)
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .ds_error
    mov rax, [rax + PyMappingMethods.mp_ass_subscript]
    test rax, rax
    jz .ds_error

    xor edx, edx               ; value = NULL (delete)
    mov rcx, [rbp - DS_KTAG]  ; key tag (4th arg, 64-bit for SmallStr)
    xor r8d, r8d               ; value tag = TAG_NULL (5th arg)
    call rax

    ; DECREF key and obj (tag-aware)
    mov rdi, [rbp - DS_KEY]
    mov rsi, [rbp - DS_KTAG]
    DECREF_VAL rdi, rsi
    mov rdi, [rbp - DS_OBJ]
    mov rsi, [rbp - DS_OTAG]
    DECREF_VAL rdi, rsi

    leave
    DISPATCH

.ds_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support item deletion"
    call raise_exception
END_FUNC op_delete_subscr
