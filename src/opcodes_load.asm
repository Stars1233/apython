; opcodes_load.asm - Opcode handlers for loading values onto the stack
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (current position in co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack payload top pointer
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
extern eval_co_names
extern eval_co_consts
extern opcode_table
extern obj_dealloc
extern dict_get
extern dict_get_index
extern fatal_error
extern raise_exception
extern obj_incref
extern obj_decref
extern type_type
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
LA_OBJVAL    equ 72   ; the object as a Value, for the generic tail
LA_WALK      equ 80   ; the MRO cursor while searching the type dicts
LA_TAGTYPE   equ 88   ; the type an immediate resolved to, the walk's origin
LA_FRAME     equ 96

; op_load_super_attr frame layout (DEF_FUNC op_load_super_attr, LSA_FRAME)
LSA_SELF     equ 8
LSA_CLASS    equ 16
LSA_NAME     equ 24
LSA_FLAG     equ 32
LSA_ATTR_TAG equ 40
LSA_ATTR     equ 48
LSA_BIND     equ 56
LSA_ORIGIN   equ 64      ; the MRO super() searches: the instance's, not the class's
LSA_FRAME    equ 80

;; ============================================================================
;; op_load_const - Load constant from co_consts[arg]
;; ============================================================================
DEF_FUNC_BARE op_load_const
    ; ecx = arg (index into co_consts)
    mov rax, [rel eval_co_consts]
    mov rax, [rax + rcx * 8]
    INCREF_V rax, rdx
    VPUSH rax
    DISPATCH
END_FUNC op_load_const

;; ============================================================================
;; op_load_fast - Load local variable from frame localsplus[arg]
;; ============================================================================
DEF_FUNC_BARE op_load_fast
    ; ecx = arg (slot index in localsplus)
    mov rax, [r12 + PyFrame.localsplus + rcx*8]
    INCREF_V rax, rdx
    VPUSH rax
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
    VPUSH_NULL
.no_push_null:
    ; Name index = arg >> 1
    shr ecx, 1
    ; Get name string from co_names (payload array)
    shl ecx, 3
    LOAD_CO_NAMES rdi
    mov rdi, [rdi + rcx]       ; rdi = name (PyStrObject*)

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
    V_UNPACK rax, rdx
    add rsp, 8                 ; discard saved name
    jmp .lg_push_result

.try_builtins:
    ; Try builtins: dict_get_index(builtins, name) -> slot or -1
    mov rdi, [r12 + PyFrame.builtins]
    pop rsi                    ; rsi = name
    push rsi                   ; save name for error message
    mov edx, TAG_PTR
    call dict_get_index
    cmp rax, -1
    je .not_found

    ; Found in builtins — specialize to LOAD_GLOBAL_BUILTIN
    add rsp, 8                 ; discard saved name
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
    V_UNPACK rax, rdx
    jmp .lg_push_result

.not_found:
    pop rdi                    ; name (PyStrObject*)
    call raise_name_not_defined
    ; (does not return)

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
    test edx, edx
    jz .lgm_deopt              ; TAG_NULL = deleted entry
    mov rax, [rdi + DictEntry.value]
    V_UNPACK rax, rdx

    ; Guards passed — now push NULL if needed
    test ecx, 1
    jz .lgm_no_null
    VPUSH_NULL
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
    test edx, edx
    jz .lgb_deopt              ; TAG_NULL = deleted entry
    mov rax, [rdi + DictEntry.value]
    V_UNPACK rax, rdx

    ; Guards passed — now push NULL if needed
    test ecx, 1
    jz .lgb_no_null
    VPUSH_NULL
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
    shl ecx, 3                ; payload array: 8-byte stride
    LOAD_CO_NAMES rsi
    mov rsi, [rsi + rcx]       ; rsi = name (PyStrObject*)
    push rsi                   ; save name

    ; Check if frame has a locals dict
    mov rdi, [r12 + PyFrame.locals]
    test rdi, rdi
    jz .try_globals

    ; Try locals first: dict_get(locals, name)
    mov rsi, [rsp]             ; rsi = name
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .found

.try_globals:
    ; Try globals: dict_get(globals, name)
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, [rsp]             ; rsi = name
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .found

    ; Try builtins: dict_get(builtins, name)
    mov rdi, [r12 + PyFrame.builtins]
    pop rsi                    ; rsi = name
    push rsi                   ; save for error message
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .found

    ; Not found in any dict - raise NameError with name
    pop rdi                    ; name (PyStrObject*)
    call raise_name_not_defined
    ; (does not return)

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
extern module_type
extern classmethod_type
extern staticmethod_type
DEF_FUNC op_load_attr, LA_FRAME

    ; Extract flag and name_index
    mov eax, ecx
    and eax, 1
    mov [rbp - LA_FLAG], rax
    mov qword [rbp - LA_FROM_TYPE], 0

    shr ecx, 1              ; name_index
    mov eax, ecx
    shl eax, 3              ; payload array: 8-byte stride
    LOAD_CO_NAMES rsi
    mov rsi, [rsi + rax]    ; name string
    mov [rbp - LA_NAME], rsi

    ; Pop obj.  Keep the Value as well as the (payload, tag) pair: the
    ; generic tail classifies a Value, and an immediate's payload is not one.
    VPEEK rcx
    mov [rbp - LA_OBJVAL], rcx
    VPOP_VAL rdi, rax
    mov [rbp - LA_OBJ], rdi
    mov [rbp - LA_OBJ_TAG], rax

    ; Dispatch on obj tag — resolve non-pointer tags to their type
    cmp qword [rbp - LA_OBJ_TAG], TAG_PTR
    je .la_is_ptr
    cmp qword [rbp - LA_OBJ_TAG], TAG_SMALLINT
    je .la_resolve_int
    cmp qword [rbp - LA_OBJ_TAG], TAG_FLOAT
    je .la_resolve_float
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
    V_UNPACK rax, rdx           ; tp_getattr returns a Value
    test edx, edx
    jz .la_attr_error
    mov [rbp - LA_ATTR], rax
    mov [rbp - LA_ATTR_TAG], rdx
    jmp .la_got_attr

.la_resolve_tag_dict:
    ; No tp_getattr: walk the MRO's tp_dicts, so an immediate reaches what
    ; object supplies too -- `(1).__eq__` lives there and nowhere else.
    mov [rbp - LA_TAGTYPE], r8
    mov [rbp - LA_WALK], r8
.la_tag_loop:
    mov rax, [rbp - LA_WALK]
    test rax, rax
    jz .la_attr_error
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jz .la_tag_next
    mov rdi, rax
    mov rsi, [rbp - LA_NAME]
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .la_tag_found
.la_tag_next:
    mov rax, [rbp - LA_WALK]
    MRO_NEXT rax, [rbp - LA_TAGTYPE]
    mov [rbp - LA_WALK], rax
    jmp .la_tag_loop
.la_tag_found:
    mov [rbp - LA_ATTR], rax
    mov [rbp - LA_ATTR_TAG], rdx
    INCREF_VAL rax, rdx
    mov qword [rbp - LA_FROM_TYPE], 1
    jmp .la_got_attr

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
    V_UNPACK rax, rdx           ; tp_getattr returns a Value
    test edx, edx
    jz .la_try_dict             ; tp_getattr returned NULL — fallback to tp_dict
    mov [rbp - LA_ATTR], rax
    mov [rbp - LA_ATTR_TAG], rdx   ; save tag from tp_getattr
    ; LA_FROM_TYPE stays 0 — tp_getattr already handled binding
    jmp .la_got_attr

.la_try_dict:
    ; No tp_getattr, or it found nothing: walk the MRO's tp_dicts.  Reading only
    ; the exact type's hid everything object supplies -- `[].__len__` and
    ; `None.__new__` among them.
    mov rdi, [rbp - LA_OBJ]
    mov rax, [rdi + PyObject.ob_type]
    mov [rbp - LA_WALK], rax
.la_dict_loop:
    mov rax, [rbp - LA_WALK]
    test rax, rax
    jz .la_attr_error
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jz .la_dict_next
    mov rdi, rax
    mov rsi, [rbp - LA_NAME]
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .la_dict_found
.la_dict_next:
    mov rdi, [rbp - LA_OBJ]
    mov rcx, [rdi + PyObject.ob_type]
    mov rax, [rbp - LA_WALK]
    MRO_NEXT rax, rcx
    mov [rbp - LA_WALK], rax
    jmp .la_dict_loop
.la_dict_found:

    ; INCREF the result (dict_get returns borrowed ref — may be SmallInt)
    mov [rbp - LA_ATTR], rax
    mov [rbp - LA_ATTR_TAG], rdx   ; save tag from dict_get
    INCREF_VAL rax, rdx
    mov qword [rbp - LA_FROM_TYPE], 1
    jmp .la_got_attr

.la_attr_error:
    ; Last: the attributes every object has -- __class__, __dict__ -- which
    ; no individual tp_getattr provides.  A type that defines one itself has
    ; already been consulted above and wins.
    mov rdi, [rbp - LA_OBJVAL]
    mov rsi, [rbp - LA_NAME]
    extern obj_generic_attr
    call obj_generic_attr
    test rax, rax
    jz .la_no_such_attr
    mov [rbp - LA_ATTR], rax
    mov qword [rbp - LA_ATTR_TAG], TAG_PTR
    mov qword [rbp - LA_FROM_TYPE], 0
    jmp .la_got_attr

.la_no_such_attr:
    mov rdi, [rbp - LA_OBJVAL]
    mov rsi, [rbp - LA_NAME]
    extern raise_no_attribute
    xor edx, edx
    call raise_no_attribute

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
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .la_check_flag          ; no __get__, treat normally

    ; Has __get__! Call descriptor.__get__(obj, type(obj))
    mov rdi, [rbp - LA_ATTR]   ; descriptor (attr)
    mov rsi, [rbp - LA_OBJ]    ; obj (instance)
    mov rdx, [rsi + PyObject.ob_type] ; type(obj)
    lea rcx, [rel dunder_get]
    mov r8d, TAG_PTR             ; type(obj) is always heap ptr
    call dunder_call_3
    V_UNPACK rax, rdx           ; returns a Value

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
    VPUSH_NULL
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

    ; DECREF obj -- tag-aware.  An immediate's payload is not an address,
    ; and this path is reachable with one now that the generic tail
    ; (__class__, __dict__) answers for every kind of value.
    mov rdi, [rbp - LA_OBJ]
    mov rsi, [rbp - LA_OBJ_TAG]
    DECREF_VAL rdi, rsi

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
    jne .la_ic_check               ; from type dict → IC + method_push
    ; from_type=0: came from tp_getattr.  Most builtin types' tp_getattr hands
    ; back an unbound method that still needs self; the exceptions are the ones
    ; whose tp_getattr reads out of a namespace rather than off a type, where
    ; the answer is already a plain function.
    cmp qword [rbp - LA_OBJ_TAG], TAG_PTR
    jne .la_method_push            ; non-ptr obj can't be heaptype
    mov rdi, [rbp - LA_OBJ]
    mov rax, [rdi + PyObject.ob_type]
    ; If obj IS a type, the attribute is unbound → [NULL, func]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jnz .la_not_method             ; class attribute → [NULL, func]
    test dword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jnz .la_not_method             ; heaptype instance attr → [NULL, func]
    ; A module's tp_getattr answers out of the module's own namespace, not out
    ; of a type -- `m.f` is the global f, never a method bound to m.  It is not
    ; a heaptype, so without this it fell into the built-in case below and was
    ; called with the module as its first argument.
    lea rcx, [rel module_type]
    cmp rax, rcx
    je .la_not_method
    ; The same for a classmethod or staticmethod wrapper: its __func__ is the
    ; function it wraps, not a method of the wrapper.
    lea rcx, [rel classmethod_type]
    cmp rax, rcx
    je .la_not_method
    lea rcx, [rel staticmethod_type]
    cmp rax, rcx
    je .la_not_method
    jmp .la_method_push            ; built-in tp_getattr → [func, self]

.la_ic_check:

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
    VPUSH_NULL              ; push NULL
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
    VPUSH_NULL
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
    VPUSH_NULL
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
    ; "obj is a type" is a flag on its metatype, not a comparison against the
    ; two we ship: a class built by a metaclass of its own is still a class,
    ; and asking the narrow question bound the metaclass instead.
    mov rdi, [rbp - LA_OBJ]    ; obj
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jnz .la_cm_obj_is_type

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

    ; The inline cache only applies to real objects
    V_TEST_PTR rdi, rax
    ja .lam_deopt

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
    ; obj is currently at [r13-8]; overwrite it with method, push obj on top
    mov rax, [rbx + 10]           ; cached descriptor (method ptr)
    INCREF rax
    mov rcx, [r13 - 8]            ; save obj (payload of TOS)
    mov [r13 - 8], rax            ; overwrite obj position with method
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
    mov rax, [r12 + PyFrame.localsplus + rcx*8]
    INCREF_V rax, rdx
    VPUSH rax
    DISPATCH
END_FUNC op_load_closure

;; ============================================================================
;; op_load_deref - Load value through cell in localsplus[arg]
;;
;; Gets cell from localsplus[arg], then loads cell.ob_ref.
;; Raises NameError if cell is empty (ob_ref == NULL).
;; ============================================================================
DEF_FUNC_BARE op_load_deref
    mov rax, [r12 + PyFrame.localsplus + rcx*8]  ; rax = cell object (payload)
    test rax, rax
    jz .deref_error
    mov rax, [rax + PyCellObject.ob_ref]       ; rax = contained Value
    test rax, rax                              ; 0 means an empty cell
    jz .deref_error
    INCREF_V rax, rdx
    VPUSH rax
    DISPATCH

.deref_error:
    extern exc_UnboundLocalError_type
    lea rdi, [rel exc_UnboundLocalError_type]
    CSTRING rsi, "cannot access variable before assignment"
    call raise_exception
END_FUNC op_load_deref

;; ============================================================================
;; op_load_fast_check - Load local with NULL check
;;
;; Same as LOAD_FAST but raises UnboundLocalError if slot is NULL.
;; Used after DELETE_FAST and in exception handlers.
;; ============================================================================
DEF_FUNC_BARE op_load_fast_check
    mov rax, [r12 + PyFrame.localsplus + rcx*8]
    test rax, rax           ; an empty slot is 0
    jz .lfc_error
    INCREF_V rax, rdx
    VPUSH rax
    DISPATCH

.lfc_error:
    extern exc_UnboundLocalError_type
    lea rdi, [rel exc_UnboundLocalError_type]
    CSTRING rsi, "cannot access local variable before assignment"
    call raise_exception
END_FUNC op_load_fast_check

;; ============================================================================
;; op_load_fast_and_clear - Load local and set slot to NULL
;;
;; Used by comprehensions to save/restore iteration variable.
;; If slot is NULL, pushes NULL (no error).
;; ============================================================================
DEF_FUNC_BARE op_load_fast_and_clear
    mov rax, [r12 + PyFrame.localsplus + rcx*8]       ; may be empty (0)
    mov qword [r12 + PyFrame.localsplus + rcx*8], 0
    ; Ownership transfers to the stack, so no INCREF
    VPUSH rax
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
    shl eax, 3                    ; payload array: 8-byte stride
    LOAD_CO_NAMES rsi
    mov rax, [rsi + rax]          ; name string
    mov [rbp - LSA_NAME], rax

    ; Pop self, class, global_super
    VPOP_VAL rax, rdx              ; self
    mov [rbp - LSA_SELF], rax
    VPOP_VAL rax, rdx              ; class
    mov [rbp - LSA_CLASS], rax
    VPOP rdi              ; global_super -- DECREF and discard
    DECREF_V rdi, rsi

    ; super() searches the *instance's* MRO starting just past the class the
    ; method was defined in -- that is the whole point of it in a diamond,
    ; and following the defining class's own tp_base chain skipped the
    ; sibling branch entirely.
    extern type_is_subtype
    mov rdi, [rbp - LSA_SELF]
    mov rdi, [rdi + PyObject.ob_type]
    mov rsi, [rbp - LSA_CLASS]
    call type_is_subtype
    test eax, eax
    jz .lsa_origin_try_self
    mov rax, [rbp - LSA_SELF]
    mov rax, [rax + PyObject.ob_type]
    jmp .lsa_have_origin
.lsa_origin_try_self:
    ; A classmethod gets the class itself as the second argument.
    mov rdi, [rbp - LSA_SELF]
    mov rsi, [rbp - LSA_CLASS]
    call type_is_subtype
    test eax, eax
    jz .lsa_origin_class
    mov rax, [rbp - LSA_SELF]
    jmp .lsa_have_origin
.lsa_origin_class:
    mov rax, [rbp - LSA_CLASS]
.lsa_have_origin:
    mov [rbp - LSA_ORIGIN], rax
    mov rdi, rax
    mov rsi, [rbp - LSA_CLASS]
    extern type_mro_next
    call type_mro_next
    test rax, rax
    jz .lsa_not_found

.lsa_walk:
    ; rcx tracks the current type for the .lsa_next_base step, including the
    ; no-tp_dict path that used to fall through with rcx undefined.
    mov rcx, rax
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .lsa_next_base

    push rax                       ; save current type
    mov rsi, [rbp - LSA_NAME]      ; name
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rcx                        ; restore current type
    test edx, edx               ; the tag, not the payload: a hit may be int 0
    jnz .lsa_found

.lsa_next_base:
    MRO_NEXT rcx, [rbp - LSA_ORIGIN]
    mov rax, rcx
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

    ; A staticmethod reached through super() must be unwrapped and pushed
    ; unbound.  super().__new__(cls) supplies cls itself, so binding self here
    ; would call __new__(cls, cls, ...) — and a staticmethod wrapper has no
    ; tp_call, so it would raise "object is not callable" first.  LOAD_ATTR
    ; does the same thing at .la_handle_staticmethod.
    ; rax is the decoded payload here, not a Value, so the tag is what says
    ; whether it is safe to dereference.
    cmp qword [rbp - LSA_ATTR_TAG], TAG_PTR
    jne .lsa_check_flag
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel staticmethod_type]
    cmp rcx, rdx
    je .lsa_staticmethod
    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    je .lsa_classmethod

.lsa_check_flag:
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
    ; Attr mode: super().meth is a *bound* method, exactly as it is in
    ; CPython -- the lookup goes through the descriptor protocol.  Pushing
    ; the raw function meant super().__init__(*args) called it with no self,
    ; so the first argument stood in for self and the real instance was
    ; never touched: a list subclass whose __init__ did super().__init__(*a)
    ; came out empty.  The non-star form works because the compiler emits
    ; method mode for it, which is why this went unnoticed.
    cmp qword [rbp - LSA_ATTR_TAG], TAG_PTR
    jne .lsa_attr_plain
    mov rcx, [rax + PyObject.ob_type]
    extern func_type
    lea rdx, [rel func_type]
    cmp rcx, rdx
    je .lsa_attr_bind
    ; A builtin method is equally a descriptor: super().__init__ on a list
    ; subclass resolves to list.__init__, which is one of these.
    extern builtin_func_type
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    jne .lsa_attr_plain

.lsa_attr_bind:
    mov [rbp - LSA_ATTR], rax
    mov rdi, rax
    mov rsi, [rbp - LSA_SELF]
    call method_new                ; INCREFs both
    push rax                       ; the bound method, ours
    mov rdi, [rbp - LSA_ATTR]
    call obj_decref                ; release our ref on the function
    mov rdi, [rbp - LSA_SELF]
    call obj_decref                ; and on self
    pop rax
    VPUSH_NULL
    VPUSH_PTR rax
    jmp .lsa_done

.lsa_attr_plain:
    ; Not a function: a plain class attribute, returned as-is.
    push rax                      ; save attr
    mov rdi, [rbp - LSA_SELF]
    call obj_decref
    xor eax, eax
    VPUSH_NULL                  ; push NULL
    pop rax
    mov rdx, [rbp - LSA_ATTR_TAG]
    VPUSH_VAL rax, rdx             ; push attr
    jmp .lsa_done

.lsa_staticmethod:
    ; Unwrap sm_callable, release the wrapper and self, push (NULL, callable)
    ; in both flag modes — a staticmethod never binds self.
    mov rcx, [rax + PyStaticMethodObject.sm_callable]
    mov [rbp - LSA_ATTR], rcx
    push rax                       ; save the wrapper
    mov rdi, rcx
    call obj_incref                ; we now own the callable
    pop rdi                        ; the wrapper
    call obj_decref
    mov rdi, [rbp - LSA_SELF]
    call obj_decref                ; not binding self
    VPUSH_NULL
    mov rax, [rbp - LSA_ATTR]
    VPUSH_PTR rax
    jmp .lsa_done

.lsa_classmethod:
    ; Unwrap cm_callable and bind it to the derived class, not to self:
    ; super().cm() must pass type(self), matching .la_handle_classmethod.
    mov rcx, [rax + PyClassMethodObject.cm_callable]
    mov [rbp - LSA_ATTR], rcx
    push rax                       ; save the wrapper
    mov rdi, rcx
    call obj_incref                ; we now own the callable
    pop rdi                        ; the wrapper
    call obj_decref

    ; class = self when self is already a type, else type(self)
    mov rdi, [rbp - LSA_SELF]
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jnz .lsa_cm_self_is_type
    mov [rbp - LSA_BIND], rax
    mov rdi, rax
    call obj_incref
    jmp .lsa_cm_have_class
.lsa_cm_self_is_type:
    mov [rbp - LSA_BIND], rdi
    call obj_incref
.lsa_cm_have_class:
    mov rdi, [rbp - LSA_SELF]
    call obj_decref                ; the class stands in for self

    cmp qword [rbp - LSA_FLAG], 0
    jne .lsa_cm_flag1

    ; Attr mode: push NULL + a method bound to the class
    mov rdi, [rbp - LSA_ATTR]
    mov rsi, [rbp - LSA_BIND]
    call method_new                ; INCREFs both
    push rax                       ; save the bound method
    mov rdi, [rbp - LSA_ATTR]
    call obj_decref
    mov rdi, [rbp - LSA_BIND]
    call obj_decref
    pop rax
    VPUSH_NULL
    VPUSH_PTR rax
    jmp .lsa_done

.lsa_cm_flag1:
    ; Method mode: func deeper, the class as self on top
    mov rax, [rbp - LSA_ATTR]
    VPUSH_PTR rax
    mov rax, [rbp - LSA_BIND]
    VPUSH_PTR rax
    jmp .lsa_done

.lsa_done:
    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    leave
    DISPATCH
END_FUNC op_load_super_attr

;; ============================================================================
;; raise_name_not_defined(PyStrObject *name)
;; Raise NameError with message "name 'X' is not defined"
;; rdi = name string object
;; Does not return.
;; ============================================================================
RNND_BUF   equ 256
RNND_FRAME equ RNND_BUF
global raise_name_not_defined
DEF_FUNC raise_name_not_defined, RNND_FRAME
    ; Build "name 'X' is not defined" in stack buffer
    lea rcx, [rbp - RNND_BUF]
    lea rsi, [rdi + PyStrObject.data]   ; name C-string

    ; "name '"
    mov dword [rcx], "name"
    mov word [rcx+4], " '"
    add rcx, 6

    ; Copy name
.rnnd_copy:
    mov al, [rsi]
    test al, al
    jz .rnnd_name_done
    mov [rcx], al
    inc rcx
    inc rsi
    jmp .rnnd_copy
.rnnd_name_done:

    ; "' is not defined"
    mov dword [rcx], "' is"
    mov dword [rcx+4], " not"
    mov dword [rcx+8], " def"
    mov dword [rcx+12], "ined"
    mov byte [rcx+16], 0
    ; Total appended: 16 chars

    extern exc_NameError_type
    lea rdi, [rel exc_NameError_type]
    lea rsi, [rbp - RNND_BUF]
    call raise_exception
END_FUNC raise_name_not_defined

;; ============================================================================
;; obj_getattr_opt(Value obj, PyStrObject *name) -> rax = Value, or 0
;;
;; One attribute lookup, with the descriptor protocol run over the result --
;; the same answer `obj.name` gives.  Returns 0 when there is no such
;; attribute; a getter that raises unwinds out of here the way it unwinds out
;; of any other call, so 0 never means "raised".
;;
;; getattr() and hasattr() each used to do their own partial version of this,
;; stopping at the raw type-dict entry.  That returned the property object
;; itself rather than calling it, so `p.v` and `getattr(p, "v")` disagreed --
;; and collections.namedtuple, which reaches its fields through descriptors
;; built at runtime, was on the wrong side of the difference.
;;
;; op_load_attr still has its own copy: it needs the unbound attribute and the
;; from-type flag to decide the two-slot push its method form uses, which a
;; function returning one value cannot give it.  tests/test_getattr_descriptors.py
;; pins the two to agree.
;; ============================================================================
GA_OBJ      equ 8
GA_NAME     equ 16
GA_ATTR     equ 24
GA_ATTRTAG  equ 32
GA_FROMTYPE equ 40
GA_CLASS    equ 48
GA_TYPE     equ 56
GA_SAVE     equ 64
GA_SAVETAG  equ 72
GA_WALK     equ 80          ; the MRO cursor
GA_FRAME    equ 88          ; + 1 push = 96
DEF_FUNC obj_getattr_opt, GA_FRAME
    push rbx
    mov [rbp - GA_OBJ], rdi
    mov [rbp - GA_NAME], rsi
    mov qword [rbp - GA_FROMTYPE], 0

    ; The type to look in, and whether the object is a real pointer.
    V_TEST_PTR rdi, rax
    ja .ga_immediate
    test rdi, rdi
    jz .ga_none
    mov rax, [rdi + PyObject.ob_type]
    mov [rbp - GA_TYPE], rax

    ; tp_getattr resolves the whole thing when a type has one.
    mov rcx, [rax + PyTypeObject.tp_getattr]
    test rcx, rcx
    jz .ga_type_dict
    mov rdi, [rbp - GA_OBJ]
    mov rsi, [rbp - GA_NAME]
    call rcx
    V_UNPACK rax, rdx
    test edx, edx
    jz .ga_type_dict
    mov [rbp - GA_ATTR], rax
    mov [rbp - GA_ATTRTAG], rdx
    jmp .ga_have_attr

.ga_immediate:
    ; An int or a float immediate: its type is fixed by the tag.
    lea rax, [rel float_type]
    V_IS_INT rdi, rcx
    jb .ga_have_type
    lea rax, [rel int_type]
.ga_have_type:
    mov [rbp - GA_TYPE], rax
    jmp .ga_type_dict

.ga_none:
    xor eax, eax
    pop rbx
    leave
    ret

.ga_type_dict:
    ; Walk the MRO, not just the exact type.  Stopping at the first tp_dict
    ; made everything object supplies invisible from an instance: `None.__new__`
    ; raised while `object.__new__` was fine, because only lookups on a TYPE
    ; walked.
    mov rax, [rbp - GA_TYPE]
    mov [rbp - GA_WALK], rax
.ga_mro_loop:
    mov rax, [rbp - GA_WALK]
    test rax, rax
    jz .ga_generic
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jz .ga_mro_next
    mov rdi, rax
    mov rsi, [rbp - GA_NAME]
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .ga_from_type
.ga_mro_next:
    mov rax, [rbp - GA_WALK]
    MRO_NEXT rax, [rbp - GA_TYPE]
    mov [rbp - GA_WALK], rax
    jmp .ga_mro_loop
.ga_from_type:
    mov [rbp - GA_ATTR], rax
    mov [rbp - GA_ATTRTAG], rdx
    INCREF_VAL rax, rdx
    mov qword [rbp - GA_FROMTYPE], 1
    jmp .ga_have_attr

.ga_generic:
    ; The attributes every object has -- __class__, __dict__ -- which no
    ; individual tp_getattr provides.
    mov rdi, [rbp - GA_OBJ]
    mov rsi, [rbp - GA_NAME]
    call obj_generic_attr
    test rax, rax
    jz .ga_missing
    mov [rbp - GA_ATTR], rax
    mov qword [rbp - GA_ATTRTAG], TAG_PTR

.ga_have_attr:
    ; --- the descriptor protocol, over whatever the lookup produced ---
    cmp qword [rbp - GA_ATTRTAG], TAG_PTR
    jne .ga_plain
    mov rax, [rbp - GA_ATTR]
    mov rcx, [rax + PyObject.ob_type]

    lea rdx, [rel staticmethod_type]
    cmp rcx, rdx
    je .ga_static

    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    je .ga_class

    lea rdx, [rel property_type]
    cmp rcx, rdx
    je .ga_property

    ; A user-defined descriptor: a heaptype whose own type defines __get__.
    test dword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .ga_plain
    mov rdi, rcx
    lea rsi, [rel dunder_get]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .ga_plain
    mov rdi, [rbp - GA_ATTR]
    mov rsi, [rbp - GA_OBJ]
    mov rdx, [rbp - GA_TYPE]
    lea rcx, [rel dunder_get]
    mov r8d, TAG_PTR
    call dunder_call_3
    mov [rbp - GA_SAVE], rax
    mov rdi, [rbp - GA_ATTR]
    call obj_decref
    mov rax, [rbp - GA_SAVE]
    pop rbx
    leave
    ret

.ga_static:
    mov rax, [rbp - GA_ATTR]
    mov rbx, [rax + PyStaticMethodObject.sm_callable]
    INCREF rbx
    mov rdi, rax
    call obj_decref
    mov rax, rbx
    pop rbx
    leave
    ret

.ga_class:
    ; Bound to the class: to the object itself when that is already a type,
    ; and to its type otherwise.
    mov rax, [rbp - GA_ATTR]
    mov rbx, [rax + PyClassMethodObject.cm_callable]
    mov rdi, [rbp - GA_OBJ]
    mov rcx, [rdi + PyObject.ob_type]
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jnz .ga_class_self
    mov rdi, [rbp - GA_TYPE]
.ga_class_self:
    mov [rbp - GA_CLASS], rdi
    mov rdi, rbx
    mov rsi, [rbp - GA_CLASS]
    call method_new
    mov [rbp - GA_SAVE], rax
    mov rdi, [rbp - GA_ATTR]
    call obj_decref
    mov rax, [rbp - GA_SAVE]
    pop rbx
    leave
    ret

.ga_property:
    mov rdi, [rbp - GA_ATTR]
    mov rsi, [rbp - GA_OBJ]
    call property_descr_get
    ; property_descr_get hands back the (payload, tag) pair, already unpacked.
    ; Unpacking it again read the payload as a Value: a small int came back as
    ; a pointer to its own numeric value, and the first thing to touch it died.
    mov [rbp - GA_SAVE], rax
    mov [rbp - GA_SAVETAG], rdx
    mov rdi, [rbp - GA_ATTR]
    call obj_decref
    mov rax, [rbp - GA_SAVE]
    mov rdx, [rbp - GA_SAVETAG]
    V_PACK rax, rdx
    pop rbx
    leave
    ret

.ga_plain:
    ; A function found on the type binds to the instance; one found on the
    ; instance does not.
    cmp qword [rbp - GA_FROMTYPE], 0
    je .ga_done
    cmp qword [rbp - GA_ATTRTAG], TAG_PTR
    jne .ga_done
    mov rdi, [rbp - GA_OBJ]
    V_TEST_PTR rdi, rax
    ja .ga_done                         ; an immediate cannot be a self
    mov rax, [rbp - GA_ATTR]
    mov rcx, [rax + PyObject.ob_type]
    cmp qword [rcx + PyTypeObject.tp_call], 0
    je .ga_done
    ; A type is callable but is not a method of its instance.
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jnz .ga_done
    mov rdi, rax
    mov rsi, [rbp - GA_OBJ]
    call method_new
    mov [rbp - GA_SAVE], rax
    mov rdi, [rbp - GA_ATTR]
    call obj_decref
    mov rax, [rbp - GA_SAVE]
    pop rbx
    leave
    ret

.ga_done:
    mov rax, [rbp - GA_ATTR]
    mov rdx, [rbp - GA_ATTRTAG]
    V_PACK rax, rdx
    pop rbx
    leave
    ret

.ga_missing:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC obj_getattr_opt
