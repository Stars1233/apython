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

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern eval_dispatch
extern obj_dealloc
extern dict_get
extern fatal_error
extern raise_exception
extern obj_incref
extern obj_decref
extern func_type
extern cell_type
extern exc_NameError_type
extern exc_AttributeError_type

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

    ; Not found in either dict - raise NameError
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not found"
    call raise_exception

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

    ; Not found in any dict - raise NameError
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not found"
    call raise_exception

.found:
    add rsp, 8                 ; discard saved name
.found_no_pop:
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_load_build_class - Push __build_class__ builtin onto the stack
;;
;; Opcode 71: LOAD_BUILD_CLASS
;; Pushes the __build_class__ function from the global build_class_obj.
;; ============================================================================
extern build_class_obj

global op_load_build_class
op_load_build_class:
    mov rax, [rel build_class_obj]
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_load_attr - Load attribute from object
;;
;; Python 3.12 LOAD_ATTR (opcode 106):
;;   ecx = arg
;;   name_index = ecx >> 1
;;   flag = ecx & 1
;;
;; Pop obj from value stack, look up attr by name on obj.
;; flag=0: push attr, DECREF obj
;; flag=1: method-style load:
;;   If attr is a function: push obj as self, push attr
;;   Else: push NULL, push attr, DECREF obj
;;
;; Followed by 9 CACHE entries (18 bytes) that must be skipped.
;; ============================================================================
global op_load_attr
op_load_attr:
    push rbp
    mov rbp, rsp
    sub rsp, 32             ; [rbp-8]=flag, [rbp-16]=obj, [rbp-24]=name, [rbp-32]=attr

    ; Extract flag and name_index
    mov eax, ecx
    and eax, 1
    mov [rbp-8], rax        ; flag

    shr ecx, 1              ; name_index
    mov rsi, [r15 + rcx*8]  ; name string
    mov [rbp-24], rsi

    ; Pop obj
    VPOP rdi
    mov [rbp-16], rdi

    ; Look up attribute
    ; Check if obj's type has tp_getattr
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_getattr]
    test rax, rax
    jz .la_try_dict

    ; Call tp_getattr(obj, name)
    mov rdi, [rbp-16]
    mov rsi, [rbp-24]
    call rax
    test rax, rax
    jz .la_attr_error
    mov [rbp-32], rax
    jmp .la_got_attr

.la_try_dict:
    ; No tp_getattr - try obj's type's tp_dict directly
    mov rdi, [rbp-16]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jz .la_attr_error

    ; dict_get(type->tp_dict, name)
    mov rdi, rax
    mov rsi, [rbp-24]
    call dict_get
    test rax, rax
    jz .la_attr_error

    ; INCREF the result (dict_get returns borrowed ref)
    mov [rbp-32], rax
    mov rdi, rax
    call obj_incref
    jmp .la_got_attr

.la_attr_error:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "object has no attribute"
    call raise_exception

.la_got_attr:
    ; Check flag
    cmp qword [rbp-8], 0
    jne .la_method_load

    ; flag=0: simple attribute load
    mov rax, [rbp-32]
    VPUSH rax

    ; DECREF obj
    mov rdi, [rbp-16]
    call obj_decref

    jmp .la_done

.la_method_load:
    ; flag=1: method-style load
    ; Check if attr is callable (has tp_call) — works for func_type AND builtin methods
    mov rax, [rbp-32]
    test rax, rax
    js .la_not_method              ; SmallInt can't be a method
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .la_not_method

    ; It's a function -> method call pattern
    ; Push self (obj), then push func
    ; Don't DECREF obj since it stays on stack as self
    mov rax, [rbp-16]
    VPUSH rax              ; push self (obj already has a ref from the pop)
    mov rax, [rbp-32]
    VPUSH rax              ; push func
    jmp .la_done

.la_not_method:
    ; Non-function attr with flag=1: push NULL then attr
    mov rdi, [rbp-16]
    call obj_decref        ; DECREF obj
    xor eax, eax
    VPUSH rax              ; push NULL
    mov rax, [rbp-32]
    VPUSH rax              ; push attr

.la_done:
    add rbx, 18            ; skip 9 CACHE entries
    leave
    DISPATCH

;; ============================================================================
;; op_load_closure - Load cell from localsplus[arg]
;;
;; Same as LOAD_FAST — loads the cell object itself (not its contents).
;; In Python 3.12, LOAD_CLOSURE is same opcode behavior as LOAD_FAST.
;; ============================================================================
global op_load_closure
op_load_closure:
    lea rax, [r12 + PyFrame.localsplus]
    mov rax, [rax + rcx*8]
    INCREF rax
    VPUSH rax
    DISPATCH

;; ============================================================================
;; op_load_deref - Load value through cell in localsplus[arg]
;;
;; Gets cell from localsplus[arg], then loads cell.ob_ref.
;; Raises NameError if cell is empty (ob_ref == NULL).
;; ============================================================================
global op_load_deref
op_load_deref:
    lea rax, [r12 + PyFrame.localsplus]
    mov rax, [rax + rcx*8]        ; rax = cell object
    test rax, rax
    jz .deref_error
    mov rax, [rax + PyCellObject.ob_ref]   ; rax = cell contents
    test rax, rax
    jz .deref_error
    INCREF rax
    VPUSH rax
    DISPATCH

.deref_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "free variable referenced before assignment"
    call raise_exception

;; ============================================================================
;; op_load_fast_check - Load local with NULL check
;;
;; Same as LOAD_FAST but raises UnboundLocalError if slot is NULL.
;; Used after DELETE_FAST and in exception handlers.
;; ============================================================================
global op_load_fast_check
op_load_fast_check:
    lea rax, [r12 + PyFrame.localsplus]
    mov rax, [rax + rcx*8]
    test rax, rax
    jz .lfc_error
    INCREF rax
    VPUSH rax
    DISPATCH

.lfc_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "cannot access local variable"
    call raise_exception

;; ============================================================================
;; op_load_fast_and_clear - Load local and set slot to NULL
;;
;; Used by comprehensions to save/restore iteration variable.
;; If slot is NULL, pushes NULL (no error).
;; ============================================================================
global op_load_fast_and_clear
op_load_fast_and_clear:
    lea rax, [r12 + PyFrame.localsplus]
    mov rdx, [rax + rcx*8]     ; current value (may be NULL)
    mov qword [rax + rcx*8], 0 ; clear slot
    ; Push value (or NULL) - no INCREF needed since we're transferring ownership
    VPUSH rdx
    DISPATCH
