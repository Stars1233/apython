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

section .text

extern eval_dispatch
extern eval_saved_rbx
extern eval_saved_r13
extern trace_opcodes
extern opcode_table
extern obj_dealloc
extern dict_get
extern dict_get_index
extern fatal_error
extern raise_exception
extern obj_incref
extern obj_decref
extern func_type
extern cell_type
extern exc_NameError_type
extern exc_AttributeError_type
extern method_new
extern method_type
extern staticmethod_type
extern classmethod_type
extern property_type
extern property_descr_get
extern user_type_metatype
extern dunder_get
extern dunder_call_3
extern dunder_lookup
extern smallstr_to_obj
extern str_type
extern int_type
extern float_type
extern none_type

; --- Named frame-layout constants ---

; op_load_attr frame layout (DEF_FUNC op_load_attr, LA_FRAME)
LA_FLAG      equ 8
LA_OBJ       equ 16
LA_NAME      equ 24
LA_ATTR      equ 32
LA_FROM_TYPE equ 40
LA_CLASS     equ 48   ; used by classmethod path
LA_ATTR_TAG  equ 56
LA_OBJ_TAG   equ 64
LA_FRAME     equ 72

; op_load_super_attr frame layout (DEF_FUNC op_load_super_attr, LSA_FRAME)
LSA_SELF     equ 8
LSA_CLASS    equ 16
LSA_NAME     equ 24
LSA_FLAG     equ 32
LSA_ATTR_TAG equ 40
LSA_FRAME    equ 48

;; ============================================================================
;; op_load_const - Load constant from co_consts[arg]
;; ============================================================================
DEF_FUNC_BARE op_load_const
    ; ecx = arg (index into co_consts fat array)
    shl ecx, 4                 ; index * 16
    mov rax, [r14 + rcx]       ; payload
    mov rdx, [r14 + rcx + 8]   ; tag
    INCREF_VAL rax, rdx
    VPUSH_VAL rax, rdx
    DISPATCH
END_FUNC op_load_const

;; ============================================================================
;; op_load_fast - Load local variable from frame localsplus[arg]
;; ============================================================================
DEF_FUNC_BARE op_load_fast
    ; ecx = arg (slot index in localsplus, 16 bytes/slot)
    lea rsi, [rcx*8]         ; slot * 8 (×2 via SIB, avoids flags clobber)
    mov rdx, [r12 + rsi*2 + PyFrame.localsplus + 8]  ; tag
    mov rax, [r12 + rsi*2 + PyFrame.localsplus]       ; payload
    INCREF_VAL rax, rdx     ; tag-aware INCREF
    mov [r13], rax
    mov [r13 + 8], rdx
    add r13, 16
    DISPATCH
END_FUNC op_load_fast

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
DEF_FUNC_BARE op_load_global
    ; ecx = arg
    ; Check bit 0: if set, push NULL first
    test ecx, 1
    jz .no_push_null
    mov qword [r13], 0
    mov qword [r13 + 8], TAG_NULL
    add r13, 16
.no_push_null:
    ; Name index = arg >> 1
    shr ecx, 1
    ; Get name string from co_names (fat tuple: 16-byte stride)
    shl ecx, 4
    mov rdi, [r15 + rcx]       ; rdi = name (PyStrObject*)

    ; Save name on the regular stack for retry
    push rdi

    ; Try globals first: dict_get_index(globals, name) -> slot or -1
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, [rsp]             ; rsi = name
    mov edx, TAG_PTR
    call dict_get_index
    cmp rax, -1
    je .try_builtins

    ; Found in globals — try to specialize to LOAD_GLOBAL_MODULE
    ; rax = slot index, rbx points at CACHE[0]
    mov word [rbx + 2], ax     ; CACHE[1] = index (low 16 bits)
    mov rdi, [r12 + PyFrame.globals]
    mov rdi, [rdi + PyDictObject.dk_version]
    mov word [rbx + 4], di     ; CACHE[2] = module_keys_version
    mov byte [rbx - 2], 200    ; rewrite opcode to LOAD_GLOBAL_MODULE

    ; Load the value now (via entry slot)
    mov rdi, [r12 + PyFrame.globals]
    mov rdi, [rdi + PyDictObject.entries]
    movzx eax, word [rbx + 2]  ; index
    imul rax, rax, DICT_ENTRY_SIZE
    add rdi, rax               ; rdi = entry ptr
    mov rax, [rdi + DictEntry.value]
    mov rdx, [rdi + DictEntry.value_tag]
    add rsp, 8                 ; discard saved name
    jmp .lg_push_result

.try_builtins:
    ; Try builtins: dict_get_index(builtins, name) -> slot or -1
    mov rdi, [r12 + PyFrame.builtins]
    pop rsi                    ; rsi = name
    mov edx, TAG_PTR
    call dict_get_index
    cmp rax, -1
    je .not_found

    ; Found in builtins — specialize to LOAD_GLOBAL_BUILTIN
    mov word [rbx + 2], ax     ; CACHE[1] = index
    mov rdi, [r12 + PyFrame.globals]
    mov rdi, [rdi + PyDictObject.dk_version]
    mov word [rbx + 4], di     ; CACHE[2] = module_keys_version (guard globals hasn't added it)
    mov rdi, [r12 + PyFrame.builtins]
    mov rdi, [rdi + PyDictObject.dk_version]
    mov word [rbx + 6], di     ; CACHE[3] = builtin_keys_version

    mov byte [rbx - 2], 201    ; rewrite opcode to LOAD_GLOBAL_BUILTIN

    ; Load the value now
    mov rdi, [r12 + PyFrame.builtins]
    mov rdi, [rdi + PyDictObject.entries]
    movzx eax, word [rbx + 2]
    imul rax, rax, DICT_ENTRY_SIZE
    add rdi, rax               ; rdi = entry ptr
    mov rax, [rdi + DictEntry.value]
    mov rdx, [rdi + DictEntry.value_tag]
    jmp .lg_push_result

.not_found:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not found"
    call raise_exception

.lg_push_result:
    INCREF_VAL rax, rdx
    VPUSH_VAL rax, rdx
    ; Skip 4 CACHE entries = 8 bytes
    add rbx, 8
    DISPATCH
END_FUNC op_load_global

;; ============================================================================
;; op_load_global_module (200) - Specialized LOAD_GLOBAL for globals dict hit
;;
;; Fast path: check globals dict version, load by cached index.
;; CACHE layout at rbx: [+0]=counter [+2]=index [+4]=mod_ver [+6]=bi_ver
;; ============================================================================
DEF_FUNC_BARE op_load_global_module
    ; Version guard FIRST (before any stack modification)
    mov rdi, [r12 + PyFrame.globals]
    mov rax, [rdi + PyDictObject.dk_version]
    cmp ax, word [rbx + 4]     ; compare low 16 bits with CACHE[2]
    jne .lgm_deopt

    ; Fast path: load from globals entries by cached index
    mov rdi, [rdi + PyDictObject.entries]
    movzx eax, word [rbx + 2]  ; CACHE[1] = index
    imul rax, rax, DICT_ENTRY_SIZE
    add rdi, rax               ; rdi = entry ptr
    mov rdx, [rdi + DictEntry.value_tag]
    test rdx, rdx
    jz .lgm_deopt              ; TAG_NULL = deleted entry
    mov rax, [rdi + DictEntry.value]

    ; Guards passed — now push NULL if needed
    test ecx, 1
    jz .lgm_no_null
    mov qword [r13], 0
    mov qword [r13 + 8], TAG_NULL
    add r13, 16
.lgm_no_null:
    INCREF_VAL rax, rdx
    VPUSH_VAL rax, rdx
    add rbx, 8
    DISPATCH

.lgm_deopt:
    ; Deopt: rewrite back to LOAD_GLOBAL (116), re-execute cleanly
    mov byte [rbx - 2], 116
    sub rbx, 2
    DISPATCH
END_FUNC op_load_global_module

;; ============================================================================
;; op_load_global_builtin (201) - Specialized LOAD_GLOBAL for builtins dict hit
;;
;; Fast path: guard both globals AND builtins versions, load by cached index.
;; ============================================================================
DEF_FUNC_BARE op_load_global_builtin
    ; Guards FIRST (before any stack modification)
    ; Guard 1: globals version must not have changed (name might now be in globals)
    mov rdi, [r12 + PyFrame.globals]
    mov rax, [rdi + PyDictObject.dk_version]
    cmp ax, word [rbx + 4]     ; CACHE[2] = module_keys_version
    jne .lgb_deopt

    ; Guard 2: builtins version must match
    mov rdi, [r12 + PyFrame.builtins]
    mov rax, [rdi + PyDictObject.dk_version]
    cmp ax, word [rbx + 6]     ; CACHE[3] = builtin_keys_version
    jne .lgb_deopt

    ; Fast path: load from builtins entries by cached index
    mov rdi, [rdi + PyDictObject.entries]
    movzx eax, word [rbx + 2]  ; CACHE[1] = index
    imul rax, rax, DICT_ENTRY_SIZE
    add rdi, rax               ; rdi = entry ptr
    mov rdx, [rdi + DictEntry.value_tag]
    test rdx, rdx
    jz .lgb_deopt              ; TAG_NULL = deleted entry
    mov rax, [rdi + DictEntry.value]

    ; Guards passed — now push NULL if needed
    test ecx, 1
    jz .lgb_no_null
    mov qword [r13], 0
    mov qword [r13 + 8], TAG_NULL
    add r13, 16
.lgb_no_null:
    INCREF_VAL rax, rdx
    VPUSH_VAL rax, rdx
    add rbx, 8
    DISPATCH

.lgb_deopt:
    mov byte [rbx - 2], 116
    sub rbx, 2
    DISPATCH
END_FUNC op_load_global_builtin

;; ============================================================================
;; op_load_name - Load name from locals -> globals -> builtins
;;
;; Similar to LOAD_GLOBAL but checks locals dict first.
;; ============================================================================
DEF_FUNC_BARE op_load_name
    ; ecx = arg (index into co_names)
    shl ecx, 4                ; fat tuple: 16-byte stride
    mov rsi, [r15 + rcx]       ; rsi = name (PyStrObject*)
    push rsi                   ; save name

    ; Check if frame has a locals dict
    mov rdi, [r12 + PyFrame.locals]
    test rdi, rdi
    jz .try_globals

    ; Try locals first: dict_get(locals, name)
    mov rsi, [rsp]             ; rsi = name
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jnz .found

.try_globals:
    ; Try globals: dict_get(globals, name)
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, [rsp]             ; rsi = name
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jnz .found

    ; Try builtins: dict_get(builtins, name)
    mov rdi, [r12 + PyFrame.builtins]
    pop rsi                    ; rsi = name
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jnz .found_no_pop

    ; Not found in any dict - raise NameError
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not found"
    call raise_exception

.found:
    add rsp, 8                 ; discard saved name
.found_no_pop:
    INCREF_VAL rax, rdx
    VPUSH_VAL rax, rdx
    DISPATCH
END_FUNC op_load_name

;; ============================================================================
;; op_load_build_class - Push __build_class__ builtin onto the stack
;;
;; Opcode 71: LOAD_BUILD_CLASS
;; Pushes the __build_class__ function from the global build_class_obj.
;; ============================================================================
extern build_class_obj

DEF_FUNC_BARE op_load_build_class
    mov rax, [rel build_class_obj]
    INCREF rax
    VPUSH_PTR rax
    DISPATCH
END_FUNC op_load_build_class

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
DEF_FUNC op_load_attr, LA_FRAME

    ; Extract flag and name_index
    mov eax, ecx
    and eax, 1
    mov [rbp - LA_FLAG], rax
    mov qword [rbp - LA_FROM_TYPE], 0

    shr ecx, 1              ; name_index
    mov eax, ecx
    shl eax, 4              ; fat tuple: 16-byte stride
    mov rsi, [r15 + rax]    ; name string
    mov [rbp - LA_NAME], rsi

    ; Pop obj
    VPOP rdi
    mov rax, [r13 + 8]            ; obj tag (after VPOP, tag at r13+8)
    mov [rbp - LA_OBJ], rdi
    mov [rbp - LA_OBJ_TAG], rax

    ; Dispatch on obj tag — resolve non-pointer tags to their type
    cmp qword [rbp - LA_OBJ_TAG], TAG_PTR
    je .la_is_ptr
    bt qword [rbp - LA_OBJ_TAG], 63
    jc .la_smallstr_spill
    cmp qword [rbp - LA_OBJ_TAG], TAG_BOOL
    je .la_resolve_bool
    cmp qword [rbp - LA_OBJ_TAG], TAG_SMALLINT
    je .la_resolve_int
    cmp qword [rbp - LA_OBJ_TAG], TAG_FLOAT
    je .la_resolve_float
    cmp qword [rbp - LA_OBJ_TAG], TAG_NONE
    je .la_resolve_none
    jmp .la_attr_error

    ; --- Non-pointer tag resolution: look up attr in type's tp_getattr or tp_dict ---
.la_resolve_bool:
    extern bool_type
    lea r8, [rel bool_type]
    jmp .la_resolve_tag_type

.la_resolve_int:
    lea r8, [rel int_type]
    jmp .la_resolve_tag_type

.la_resolve_float:
    lea r8, [rel float_type]
    jmp .la_resolve_tag_type

.la_resolve_none:
    lea r8, [rel none_type]
    ; fall through

.la_resolve_tag_type:
    ; r8 = type object for the non-pointer value
    ; First try tp_getattr
    mov rax, [r8 + PyTypeObject.tp_getattr]
    test rax, rax
    jz .la_resolve_tag_dict
    ; Call tp_getattr(self_payload, name) — rdi already has payload
    mov rsi, [rbp - LA_NAME]
    call rax
    test edx, edx
    jz .la_attr_error
    mov [rbp - LA_ATTR], rax
    mov [rbp - LA_ATTR_TAG], rdx
    jmp .la_got_attr

.la_resolve_tag_dict:
    ; No tp_getattr — try type's tp_dict
    mov rax, [r8 + PyTypeObject.tp_dict]
    test rax, rax
    jz .la_attr_error
    mov rdi, rax
    mov rsi, [rbp - LA_NAME]
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jz .la_attr_error
    mov [rbp - LA_ATTR], rax
    mov [rbp - LA_ATTR_TAG], rdx
    INCREF_VAL rax, rdx
    mov qword [rbp - LA_FROM_TYPE], 1
    jmp .la_got_attr

.la_smallstr_spill:
    ; SmallStr: spill to heap PyStrObject* and update LA_OBJ/LA_OBJ_TAG
    mov rdi, [rbp - LA_OBJ]       ; payload
    mov rsi, [rbp - LA_OBJ_TAG]   ; tag
    call smallstr_to_obj           ; rax = heap str (refcount=1)
    mov [rbp - LA_OBJ], rax
    mov qword [rbp - LA_OBJ_TAG], TAG_PTR
    mov rdi, rax
    ; fall through to .la_is_ptr

.la_is_ptr:
    ; Look up attribute
    ; Check if obj's type has tp_getattr
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_getattr]
    test rax, rax
    jz .la_try_dict

    ; Call tp_getattr(obj, name)
    ; tp_getattr handles all descriptor/binding logic (staticmethod, classmethod,
    ; property, method binding). Result is fully resolved.
    mov rdi, [rbp - LA_OBJ]
    mov rsi, [rbp - LA_NAME]
    call rax
    test edx, edx
    jz .la_try_dict             ; tp_getattr returned NULL — fallback to tp_dict
    mov [rbp - LA_ATTR], rax
    mov [rbp - LA_ATTR_TAG], rdx   ; save tag from tp_getattr
    ; LA_FROM_TYPE stays 0 — tp_getattr already handled binding
    jmp .la_got_attr

.la_try_dict:
    ; No tp_getattr - try obj's type's tp_dict directly
    mov rdi, [rbp - LA_OBJ]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jz .la_attr_error

    ; dict_get(type->tp_dict, name)
    mov rdi, rax
    mov rsi, [rbp - LA_NAME]
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jz .la_attr_error

    ; INCREF the result (dict_get returns borrowed ref — may be SmallInt)
    mov [rbp - LA_ATTR], rax
    mov [rbp - LA_ATTR_TAG], rdx   ; save tag from dict_get
    INCREF_VAL rax, rdx
    mov qword [rbp - LA_FROM_TYPE], 1
    jmp .la_got_attr

.la_attr_error:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "object has no attribute"
    call raise_exception

.la_got_attr:
    ; === Descriptor protocol: check for staticmethod/classmethod ===
    mov rax, [rbp - LA_ATTR]   ; attr
    cmp qword [rbp - LA_ATTR_TAG], TAG_PTR
    jne .la_check_flag         ; not a heap pointer — skip descriptor check
    mov rcx, [rax + PyObject.ob_type]

    lea rdx, [rel staticmethod_type]
    cmp rcx, rdx
    je .la_handle_staticmethod

    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    je .la_handle_classmethod

    lea rdx, [rel property_type]
    cmp rcx, rdx
    je .la_handle_property

    ; General descriptor protocol: check for __get__ on attr's type
    ; Only check if attr's type is a heaptype (user-defined descriptor)
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .la_check_flag

    ; Check if attr's type has __get__
    mov rdi, rcx               ; attr's type
    lea rsi, [rel dunder_get]
    call dunder_lookup
    test edx, edx
    jz .la_check_flag          ; no __get__, treat normally

    ; Has __get__! Call descriptor.__get__(obj, type(obj))
    mov rdi, [rbp - LA_ATTR]   ; descriptor (attr)
    mov rsi, [rbp - LA_OBJ]    ; obj (instance)
    mov rdx, [rsi + PyObject.ob_type] ; type(obj)
    lea rcx, [rel dunder_get]
    mov r8d, TAG_PTR             ; type(obj) is always heap ptr
    call dunder_call_3

    ; rax = result from __get__, rdx = result tag
    SAVE_FAT_RESULT            ; save (rax,rdx) across DECREF calls

    ; DECREF descriptor wrapper
    mov rdi, [rbp - LA_ATTR]
    call obj_decref
    ; DECREF obj
    mov rdi, [rbp - LA_OBJ]
    call obj_decref

    RESTORE_FAT_RESULT
    cmp qword [rbp - LA_FLAG], 0
    jne .la_descr_get_flag1
    VPUSH_VAL rax, rdx
    jmp .la_done

.la_descr_get_flag1:
    ; flag=1: push NULL + result
    xor ecx, ecx
    VPUSH_NULL128
    VPUSH_VAL rax, rdx
    jmp .la_done

.la_check_flag:
    ; Check flag
    cmp qword [rbp - LA_FLAG], 0
    jne .la_method_load

    ; flag=0: simple attribute load
    ; If attr came from type dict and is callable, create bound method
    cmp qword [rbp - LA_FROM_TYPE], 0
    je .la_simple_push
    ; Can only create bound methods for pointer self (method_new INCREFs self)
    cmp qword [rbp - LA_OBJ_TAG], TAG_PTR
    jne .la_nonptr_type_attr
    mov rax, [rbp - LA_ATTR]
    cmp qword [rbp - LA_ATTR_TAG], TAG_PTR
    jne .la_simple_push         ; not a heap pointer
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .la_simple_push

    ; Create bound method(func=attr, self=obj)
    mov rdi, [rbp - LA_ATTR]   ; func
    mov rsi, [rbp - LA_OBJ]    ; self
    call method_new
    VPUSH_PTR rax

    ; DECREF the raw func (method_new INCREFed it)
    mov rdi, [rbp - LA_ATTR]
    call obj_decref
    ; DECREF obj (method_new INCREFed it)
    mov rdi, [rbp - LA_OBJ]
    call obj_decref
    jmp .la_done

.la_nonptr_type_attr:
    ; Non-pointer self (SmallInt, Float, etc.) — push attr without binding
    ; No obj_decref needed for non-pointer self (no TAG_RC_BIT)
    mov rax, [rbp - LA_ATTR]
    mov rdx, [rbp - LA_ATTR_TAG]
    VPUSH_VAL rax, rdx
    jmp .la_done

.la_simple_push:
    mov rax, [rbp - LA_ATTR]
    mov rdx, [rbp - LA_ATTR_TAG]
    VPUSH_VAL rax, rdx

    ; DECREF obj
    mov rdi, [rbp - LA_OBJ]
    call obj_decref

    jmp .la_done

.la_method_load:
    ; flag=1: method-style load
    mov rax, [rbp - LA_ATTR]
    cmp qword [rbp - LA_ATTR_TAG], TAG_PTR
    jne .la_not_method             ; non-pointer can't be a method
    mov rcx, [rax + PyObject.ob_type]

    ; If attr is a bound method (returned by instance_getattr with binding),
    ; unwrap into [im_func, im_self] push pattern
    lea rdx, [rel method_type]
    cmp rcx, rdx
    je .la_unwrap_bound_method

    ; Only bind func_type and builtin_func_type as methods
    ; Types and other callables should NOT be bound
    lea rdx, [rel func_type]
    cmp rcx, rdx
    je .la_is_method_func

    extern builtin_func_type
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    je .la_is_method_func

    jmp .la_not_method

.la_is_method_func:

    ; === IC: try to specialize as LOAD_ATTR_METHOD (203) ===
    ; Only when attr came from type dict (no tp_getattr path)
    cmp qword [rbp - LA_FROM_TYPE], 0
    je .la_method_push             ; not from type dict, skip IC

    ; Verify type has tp_dict with valid dk_version
    mov rdi, [rbp - LA_OBJ]       ; obj
    cmp qword [rbp - LA_OBJ_TAG], TAG_PTR
    jne .la_method_push            ; non-pointer obj, skip IC
    mov rcx, [rdi + PyObject.ob_type]
    mov rdx, [rcx + PyTypeObject.tp_dict]
    test rdx, rdx
    jz .la_method_push             ; no tp_dict, skip

    ; Write CACHE: [+0]=dk_version(16b), [+2]=type_ptr(64b), [+10]=descr(64b)
    mov rdx, [rdx + PyDictObject.dk_version]
    mov word [rbx], dx             ; CACHE[0] = dk_version (low 16 bits)
    mov [rbx + 2], rcx             ; CACHE[1..4] = type_ptr (8 bytes unaligned)
    mov rax, [rbp - LA_ATTR]
    mov [rbx + 10], rax            ; CACHE[5..8] = descr (8 bytes unaligned)
    mov byte [rbx - 2], 203       ; rewrite opcode to LOAD_ATTR_METHOD

.la_method_push:
    ; It's a function -> method call pattern
    ; CPython order: push func (deeper), then self (TOS)
    ; Don't DECREF obj since it stays on stack as self
    mov rax, [rbp - LA_ATTR]
    VPUSH_PTR rax              ; push func (deeper slot = callable)
    mov rax, [rbp - LA_OBJ]
    mov rdx, [rbp - LA_OBJ_TAG]
    VPUSH_VAL rax, rdx         ; push self with correct tag (SmallInt/Float/etc.)
    jmp .la_done

.la_unwrap_bound_method:
    ; Attr is a bound method from instance_getattr.
    ; Unwrap: push im_func (deeper), im_self (TOS).
    ; DECREF obj (not used — method has its own self ref).
    mov rdi, [rbp - LA_OBJ]
    call obj_decref

    mov rax, [rbp - LA_ATTR]    ; bound method
    ; INCREF im_func and im_self (we're creating new refs on the value stack)
    mov rdi, [rax + PyMethodObject.im_func]
    push rax
    call obj_incref
    pop rax
    mov rdi, [rax + PyMethodObject.im_self]
    push rax
    call obj_incref
    pop rax

    ; Push [im_func, im_self] then DECREF the method wrapper
    mov rcx, [rax + PyMethodObject.im_func]
    VPUSH_PTR rcx                    ; func (deeper)
    mov rcx, [rax + PyMethodObject.im_self]
    VPUSH_PTR rcx                    ; self (TOS)

    ; DECREF the method wrapper
    mov rdi, rax
    call obj_decref
    jmp .la_done

.la_not_method:
    ; Non-function attr with flag=1: push NULL then attr
    mov rdi, [rbp - LA_OBJ]
    call obj_decref        ; DECREF obj
    xor eax, eax
    VPUSH_NULL128              ; push NULL
    mov rax, [rbp - LA_ATTR]
    mov rdx, [rbp - LA_ATTR_TAG]
    VPUSH_VAL rax, rdx         ; push attr
    jmp .la_done

.la_handle_staticmethod:
    ; Unwrap: extract sm_callable from wrapper
    mov rdi, [rax + PyStaticMethodObject.sm_callable]
    push rdi                   ; save unwrapped func
    call obj_incref            ; INCREF unwrapped func

    ; DECREF wrapper
    mov rdi, [rbp - LA_ATTR]
    call obj_decref

    ; Update attr to unwrapped func
    pop rax
    mov [rbp - LA_ATTR], rax

    ; DECREF obj (not binding it as self)
    mov rdi, [rbp - LA_OBJ]
    call obj_decref

    cmp qword [rbp - LA_FLAG], 0
    jne .la_sm_flag1

    ; flag=0: push just the unwrapped func
    mov rax, [rbp - LA_ATTR]
    VPUSH_PTR rax
    jmp .la_done

.la_sm_flag1:
    ; flag=1: push NULL + func (no self binding)
    xor eax, eax
    VPUSH_NULL128
    mov rax, [rbp - LA_ATTR]
    VPUSH_PTR rax
    jmp .la_done

.la_handle_property:
    ; Property descriptor: always intercept and call fget(obj)
    ; (property objects found via instance_getattr still need descriptor invocation)

    ; Call property_descr_get(property, obj)
    mov rdi, [rbp - LA_ATTR]   ; property descriptor
    mov rsi, [rbp - LA_OBJ]    ; obj
    call property_descr_get
    SAVE_FAT_RESULT            ; save (rax,rdx) across DECREF calls

    ; DECREF property wrapper
    mov rdi, [rbp - LA_ATTR]
    call obj_decref
    ; DECREF obj
    mov rdi, [rbp - LA_OBJ]
    call obj_decref

    RESTORE_FAT_RESULT
    ; Push result (property_descr_get already returns owned ref)
    cmp qword [rbp - LA_FLAG], 0
    jne .la_prop_flag1
    VPUSH_VAL rax, rdx
    jmp .la_done

.la_prop_flag1:
    ; flag=1: push NULL + result (it's a value, not a method)
    xor ecx, ecx
    VPUSH_NULL128
    VPUSH_VAL rax, rdx
    jmp .la_done

.la_handle_classmethod:
    ; Unwrap: extract cm_callable from wrapper
    mov rdi, [rax + PyClassMethodObject.cm_callable]
    push rdi                   ; save unwrapped func
    call obj_incref            ; INCREF unwrapped func

    ; DECREF wrapper
    mov rdi, [rbp - LA_ATTR]
    call obj_decref

    ; Update attr to unwrapped func
    pop rax
    mov [rbp - LA_ATTR], rax

    ; Determine class: if obj is a type, class=obj. Else class=type(obj).
    mov rdi, [rbp - LA_OBJ]    ; obj
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    je .la_cm_obj_is_type

    ; obj is an instance -> class = ob_type
    mov [rbp - LA_CLASS], rax  ; save class
    mov rdi, rax
    call obj_incref
    jmp .la_cm_have_class

.la_cm_obj_is_type:
    ; obj is a type -> class = obj itself
    mov [rbp - LA_CLASS], rdi  ; save class (= obj)
    call obj_incref

.la_cm_have_class:
    ; DECREF obj
    mov rdi, [rbp - LA_OBJ]
    call obj_decref

    cmp qword [rbp - LA_FLAG], 0
    jne .la_cm_flag1

    ; flag=0: create bound method(func, class) and push
    mov rdi, [rbp - LA_ATTR]   ; func
    mov rsi, [rbp - LA_CLASS]  ; class (as self)
    call method_new            ; INCREFs both func and class
    ; DECREF our refs to func and class
    push rax                   ; save method
    mov rdi, [rbp - LA_ATTR]
    call obj_decref
    mov rdi, [rbp - LA_CLASS]
    call obj_decref
    pop rax
    VPUSH_PTR rax
    jmp .la_done

.la_cm_flag1:
    ; flag=1: CPython order: push func (deeper), then class (TOS as self)
    mov rax, [rbp - LA_ATTR]   ; func
    VPUSH_PTR rax
    mov rax, [rbp - LA_CLASS]  ; class
    VPUSH_PTR rax
    jmp .la_done

.la_done:
    add rbx, 18            ; skip 9 CACHE entries
    leave
    DISPATCH
END_FUNC op_load_attr

;; ============================================================================
;; op_load_attr_method (203) - Specialized LOAD_ATTR for method-style loads
;;
;; Fast path for flag=1 method loads from type dict (no tp_getattr path).
;; Guards: ob_type matches cached type_ptr, tp_dict dk_version matches.
;; CACHE layout at rbx: [+0]=dk_version(16b), [+2]=type_ptr(64b), [+10]=descr(64b)
;;
;; Stack effect: ..., obj -> ..., obj(self), method
;; (obj stays as self, cached method pushed on top)
;; ============================================================================
DEF_FUNC_BARE op_load_attr_method
    ; ecx = arg (name_index << 1 | flag=1)
    ; VPEEK obj (don't pop -- stays as self if guards pass, or for deopt)
    VPEEK rdi

    ; Non-pointer check (tag at r13-8 for TOS) — must be TAG_PTR to use IC
    cmp qword [r13 - 8], TAG_PTR
    jne .lam_deopt

    ; Guard 1: ob_type == cached type_ptr
    mov rax, [rdi + PyObject.ob_type]
    cmp rax, [rbx + 2]            ; compare 8 bytes at CACHE[+2]
    jne .lam_deopt

    ; Guard 2: type->tp_dict->dk_version == cached dk_version
    mov rax, [rax + PyTypeObject.tp_dict]
    mov rax, [rax + PyDictObject.dk_version]
    cmp ax, word [rbx]             ; compare low 16 bits at CACHE[+0]
    jne .lam_deopt

    ; Guards passed! CPython order: method (deeper), obj/self (TOS)
    ; obj is currently at [r13-16]; overwrite it with method, push obj on top
    mov rax, [rbx + 10]           ; cached descriptor (method ptr)
    INCREF rax
    mov rcx, [r13 - 16]           ; save obj (payload of TOS)
    mov [r13 - 16], rax           ; overwrite obj position with method
    mov qword [r13 - 8], TAG_PTR  ; ensure method tag is correct (defensive)
    VPUSH_PTR rcx                  ; push obj on top as self

    ; Skip 9 CACHE entries = 18 bytes
    add rbx, 18
    DISPATCH

.lam_deopt:
    ; Deopt: rewrite to LOAD_ATTR (106), re-execute
    mov byte [rbx - 2], 106
    sub rbx, 2
    DISPATCH
END_FUNC op_load_attr_method

;; ============================================================================
;; op_load_closure - Load cell from localsplus[arg]
;;
;; Same as LOAD_FAST -- loads the cell object itself (not its contents).
;; In Python 3.12, LOAD_CLOSURE is same opcode behavior as LOAD_FAST.
;; ============================================================================
DEF_FUNC_BARE op_load_closure
    lea rsi, [rcx*8]         ; slot * 8 (×2 via SIB, avoids flags clobber)
    mov rdx, [r12 + rsi*2 + PyFrame.localsplus + 8]  ; tag
    mov rax, [r12 + rsi*2 + PyFrame.localsplus]       ; payload
    INCREF_VAL rax, rdx     ; tag-aware INCREF
    mov [r13], rax
    mov [r13 + 8], rdx
    add r13, 16
    DISPATCH
END_FUNC op_load_closure

;; ============================================================================
;; op_load_deref - Load value through cell in localsplus[arg]
;;
;; Gets cell from localsplus[arg], then loads cell.ob_ref.
;; Raises NameError if cell is empty (ob_ref == NULL).
;; ============================================================================
DEF_FUNC_BARE op_load_deref
    lea rax, [rcx*8]              ; slot * 8 (×2 via SIB)
    mov rax, [r12 + rax*2 + PyFrame.localsplus]  ; rax = cell object (payload)
    test rax, rax
    jz .deref_error
    mov rdx, [rax + PyCellObject.ob_ref_tag]  ; rdx = value tag
    mov rax, [rax + PyCellObject.ob_ref]       ; rax = value payload
    test rdx, rdx                              ; check tag for NULL (empty cell)
    jz .deref_error
    INCREF_VAL rax, rdx
    VPUSH_VAL rax, rdx
    DISPATCH

.deref_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "free variable referenced before assignment"
    call raise_exception
END_FUNC op_load_deref

;; ============================================================================
;; op_load_fast_check - Load local with NULL check
;;
;; Same as LOAD_FAST but raises UnboundLocalError if slot is NULL.
;; Used after DELETE_FAST and in exception handlers.
;; ============================================================================
DEF_FUNC_BARE op_load_fast_check
    lea rdx, [rcx*8]        ; slot * 8 (×2 via SIB, avoids flags clobber)
    cmp qword [r12 + rdx*2 + PyFrame.localsplus + 8], 0  ; check tag for TAG_NULL
    je .lfc_error
    mov rax, [r12 + rdx*2 + PyFrame.localsplus]       ; payload
    mov rdx, [r12 + rdx*2 + PyFrame.localsplus + 8]  ; tag
    INCREF_VAL rax, rdx     ; tag-aware INCREF
    mov [r13], rax
    mov [r13 + 8], rdx
    add r13, 16
    DISPATCH

.lfc_error:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "cannot access local variable"
    call raise_exception
END_FUNC op_load_fast_check

;; ============================================================================
;; op_load_fast_and_clear - Load local and set slot to NULL
;;
;; Used by comprehensions to save/restore iteration variable.
;; If slot is NULL, pushes NULL (no error).
;; ============================================================================
DEF_FUNC_BARE op_load_fast_and_clear
    lea rsi, [rcx*8]           ; slot * 8 (×2 via SIB, avoids flags clobber)
    mov rax, [r12 + rsi*2 + PyFrame.localsplus]       ; payload (may be NULL)
    mov rdx, [r12 + rsi*2 + PyFrame.localsplus + 8]   ; tag
    mov qword [r12 + rsi*2 + PyFrame.localsplus], 0   ; clear payload
    mov qword [r12 + rsi*2 + PyFrame.localsplus + 8], 0 ; clear tag
    ; Push with preserved tag - no INCREF needed (transferring ownership)
    mov [r13], rax
    mov [r13 + 8], rdx
    add r13, 16
    DISPATCH
END_FUNC op_load_fast_and_clear

;; ============================================================================
;; op_load_super_attr - Load attribute via super()
;;
;; Opcode 141: LOAD_SUPER_ATTR
;; Stack: TOS=self, TOS1=class, TOS2=global_super
;; arg encoding: name_index = arg >> 2, method = arg & 1
;; Followed by 1 CACHE entry (2 bytes).
;;
;; Pops all three stack values, looks up attribute in class->tp_base->tp_dict
;; (walking the MRO chain), and pushes result.
;; If method flag: push self + func. Otherwise: push NULL + attr.
;; ============================================================================
DEF_FUNC op_load_super_attr, LSA_FRAME

    ; Save method flag
    mov eax, ecx
    and eax, 1
    mov [rbp - LSA_FLAG], rax

    ; Get name from co_names
    shr ecx, 2
    mov eax, ecx
    shl eax, 4                    ; fat tuple: 16-byte stride
    mov rax, [r15 + rax]          ; name string
    mov [rbp - LSA_NAME], rax

    ; Pop self, class, global_super
    VPOP rax                       ; self
    mov [rbp - LSA_SELF], rax
    VPOP rax                       ; class
    mov [rbp - LSA_CLASS], rax
    VPOP rdi                       ; global_super -- DECREF and discard
    mov rsi, [r13 + 8]            ; global_super tag
    DECREF_VAL rdi, rsi

    ; Walk from class->tp_base up the chain looking for name
    mov rax, [rbp - LSA_CLASS]     ; class
    mov rax, [rax + PyTypeObject.tp_base]
    test rax, rax
    jz .lsa_not_found

.lsa_walk:
    ; Check this type's tp_dict
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .lsa_next_base

    push rax                       ; save current type
    mov rsi, [rbp - LSA_NAME]      ; name
    mov edx, TAG_PTR
    call dict_get
    pop rcx                        ; restore current type
    test edx, edx
    jnz .lsa_found

.lsa_next_base:
    mov rax, [rcx + PyTypeObject.tp_base]
    test rax, rax
    jnz .lsa_walk

.lsa_not_found:
    ; DECREF class and self
    mov rdi, [rbp - LSA_CLASS]
    call obj_decref
    mov rdi, [rbp - LSA_SELF]
    call obj_decref
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "super: attribute not found"
    call raise_exception

.lsa_found:
    ; rax = attribute value, rdx = tag (from dict_get)
    mov [rbp - LSA_ATTR_TAG], rdx  ; save tag before INCREF/DECREF
    INCREF_VAL rax, rdx
    push rax                       ; save attr

    ; DECREF class
    mov rdi, [rbp - LSA_CLASS]
    call obj_decref

    pop rax                        ; restore attr

    ; Check method flag
    cmp qword [rbp - LSA_FLAG], 0
    je .lsa_attr_mode

    ; Method mode: CPython order: push func (deeper), then self (TOS)
    mov rdx, [rbp - LSA_ATTR_TAG]
    VPUSH_VAL rax, rdx             ; push func (deeper = callable)
    mov rax, [rbp - LSA_SELF]     ; self (already has ref from stack)
    VPUSH_PTR rax                  ; push self (TOS)
    jmp .lsa_done

.lsa_attr_mode:
    ; Attr mode: DECREF self, push NULL + attr
    push rax                      ; save attr
    mov rdi, [rbp - LSA_SELF]
    call obj_decref
    xor eax, eax
    VPUSH_NULL128                  ; push NULL
    pop rax
    mov rdx, [rbp - LSA_ATTR_TAG]
    VPUSH_VAL rax, rdx             ; push attr
    jmp .lsa_done

.lsa_done:
    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    leave
    DISPATCH
END_FUNC op_load_super_attr
