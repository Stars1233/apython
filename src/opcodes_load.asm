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

;; ============================================================================
;; op_load_const - Load constant from co_consts[arg]
;; ============================================================================
DEF_FUNC_BARE op_load_const
    ; ecx = arg (index into co_consts)
    mov rax, [r14 + rcx*8]     ; r14 = &co_consts.ob_item[0]
    INCREF rax
    VPUSH rax
    DISPATCH
END_FUNC op_load_const

;; ============================================================================
;; op_load_fast - Load local variable from frame localsplus[arg]
;; ============================================================================
DEF_FUNC_BARE op_load_fast
    ; ecx = arg (slot index in localsplus)
    lea rax, [r12 + PyFrame.localsplus]
    mov rax, [rax + rcx*8]
    INCREF rax
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
    mov qword [r13], 0
    add r13, 8
.no_push_null:
    ; Name index = arg >> 1
    shr ecx, 1
    ; Get name string from co_names
    mov rdi, [r15 + rcx*8]     ; rdi = name (PyStrObject*)

    ; Save name on the regular stack for retry
    push rdi

    ; Try globals first: dict_get_index(globals, name) -> slot or -1
    mov rdi, [r12 + PyFrame.globals]
    mov rsi, [rsp]             ; rsi = name
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
    mov rax, [rdi + rax + DictEntry.value]
    add rsp, 8                 ; discard saved name
    jmp .lg_push_result

.try_builtins:
    ; Try builtins: dict_get_index(builtins, name) -> slot or -1
    mov rdi, [r12 + PyFrame.builtins]
    pop rsi                    ; rsi = name
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
    mov rax, [rdi + rax + DictEntry.value]
    jmp .lg_push_result

.not_found:
    lea rdi, [rel exc_NameError_type]
    CSTRING rsi, "name not found"
    call raise_exception

.lg_push_result:
    INCREF rax
    VPUSH rax
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
    mov rax, [rdi + rax + DictEntry.value]
    test rax, rax
    jz .lgm_deopt              ; value slot cleared (deleted entry)

    ; Guards passed — now push NULL if needed
    test ecx, 1
    jz .lgm_no_null
    mov qword [r13], 0
    add r13, 8
.lgm_no_null:
    INCREF rax
    VPUSH rax
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
    mov rax, [rdi + rax + DictEntry.value]
    test rax, rax
    jz .lgb_deopt

    ; Guards passed — now push NULL if needed
    test ecx, 1
    jz .lgb_no_null
    mov qword [r13], 0
    add r13, 8
.lgb_no_null:
    INCREF rax
    VPUSH rax
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
    VPUSH rax
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
DEF_FUNC op_load_attr, 48
    ; [rbp-8]=flag, [rbp-16]=obj, [rbp-24]=name, [rbp-32]=attr, [rbp-40]=from_type_dict

    ; Extract flag and name_index
    mov eax, ecx
    and eax, 1
    mov [rbp-8], rax        ; flag
    mov qword [rbp-40], 0   ; from_type_dict = 0

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
    mov qword [rbp-40], 1   ; from_type_dict = 1
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
    ; If attr came from type dict and is callable, create bound method
    cmp qword [rbp-40], 0
    je .la_simple_push
    mov rax, [rbp-32]
    test rax, rax
    js .la_simple_push          ; SmallInt
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .la_simple_push

    ; Create bound method(func=attr, self=obj)
    mov rdi, [rbp-32]          ; func
    mov rsi, [rbp-16]          ; self
    call method_new
    VPUSH rax

    ; DECREF the raw func (method_new INCREFed it)
    mov rdi, [rbp-32]
    call obj_decref
    ; DECREF obj (method_new INCREFed it)
    mov rdi, [rbp-16]
    call obj_decref
    jmp .la_done

.la_simple_push:
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

    ; === IC: try to specialize as LOAD_ATTR_METHOD (203) ===
    ; Only when attr came from type dict (no tp_getattr path)
    cmp qword [rbp-40], 0
    je .la_method_push             ; not from type dict, skip IC

    ; Verify type has tp_dict with valid dk_version
    mov rdi, [rbp-16]             ; obj
    test rdi, rdi
    js .la_method_push             ; SmallInt obj, skip
    mov rcx, [rdi + PyObject.ob_type]
    mov rdx, [rcx + PyTypeObject.tp_dict]
    test rdx, rdx
    jz .la_method_push             ; no tp_dict, skip

    ; Write CACHE: [+0]=dk_version(16b), [+2]=type_ptr(64b), [+10]=descr(64b)
    mov rdx, [rdx + PyDictObject.dk_version]
    mov word [rbx], dx             ; CACHE[0] = dk_version (low 16 bits)
    mov [rbx + 2], rcx             ; CACHE[1..4] = type_ptr (8 bytes unaligned)
    mov rax, [rbp-32]
    mov [rbx + 10], rax            ; CACHE[5..8] = descr (8 bytes unaligned)
    mov byte [rbx - 2], 203       ; rewrite opcode to LOAD_ATTR_METHOD

.la_method_push:
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
END_FUNC op_load_attr

;; ============================================================================
;; op_load_attr_method (203) - Specialized LOAD_ATTR for method-style loads
;;
;; Fast path for flag=1 method loads from type dict (no tp_getattr path).
;; Guards: ob_type matches cached type_ptr, tp_dict dk_version matches.
;; CACHE layout at rbx: [+0]=dk_version(16b), [+2]=type_ptr(64b), [+10]=descr(64b)
;;
;; Stack effect: ..., obj → ..., obj(self), method
;; (obj stays as self, cached method pushed on top)
;; ============================================================================
DEF_FUNC_BARE op_load_attr_method
    ; ecx = arg (name_index << 1 | flag=1)
    ; VPEEK obj (don't pop — stays as self if guards pass, or for deopt)
    VPEEK rdi

    ; SmallInt check
    test rdi, rdi
    js .lam_deopt

    ; Guard 1: ob_type == cached type_ptr
    mov rax, [rdi + PyObject.ob_type]
    cmp rax, [rbx + 2]            ; compare 8 bytes at CACHE[+2]
    jne .lam_deopt

    ; Guard 2: type->tp_dict->dk_version == cached dk_version
    mov rax, [rax + PyTypeObject.tp_dict]
    mov rax, [rax + PyDictObject.dk_version]
    cmp ax, word [rbx]             ; compare low 16 bits at CACHE[+0]
    jne .lam_deopt

    ; Guards passed! obj stays on stack as self, push cached method
    mov rax, [rbx + 10]           ; cached descriptor (method ptr)
    INCREF rax
    VPUSH rax                     ; push method on top of self

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
;; Same as LOAD_FAST — loads the cell object itself (not its contents).
;; In Python 3.12, LOAD_CLOSURE is same opcode behavior as LOAD_FAST.
;; ============================================================================
DEF_FUNC_BARE op_load_closure
    lea rax, [r12 + PyFrame.localsplus]
    mov rax, [rax + rcx*8]
    INCREF rax
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
END_FUNC op_load_deref

;; ============================================================================
;; op_load_fast_check - Load local with NULL check
;;
;; Same as LOAD_FAST but raises UnboundLocalError if slot is NULL.
;; Used after DELETE_FAST and in exception handlers.
;; ============================================================================
DEF_FUNC_BARE op_load_fast_check
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
END_FUNC op_load_fast_check

;; ============================================================================
;; op_load_fast_and_clear - Load local and set slot to NULL
;;
;; Used by comprehensions to save/restore iteration variable.
;; If slot is NULL, pushes NULL (no error).
;; ============================================================================
DEF_FUNC_BARE op_load_fast_and_clear
    lea rax, [r12 + PyFrame.localsplus]
    mov rdx, [rax + rcx*8]     ; current value (may be NULL)
    mov qword [rax + rcx*8], 0 ; clear slot
    ; Push value (or NULL) - no INCREF needed since we're transferring ownership
    VPUSH rdx
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
DEF_FUNC op_load_super_attr, 32
    ; [rbp-8]=self, [rbp-16]=class, [rbp-24]=name, [rbp-32]=method_flag

    ; Save method flag
    mov eax, ecx
    and eax, 1
    mov [rbp-32], rax

    ; Get name from co_names
    shr ecx, 2
    mov rax, [r15 + rcx*8]        ; name string
    mov [rbp-24], rax

    ; Pop self, class, global_super
    VPOP rax                       ; self
    mov [rbp-8], rax
    VPOP rax                       ; class
    mov [rbp-16], rax
    VPOP rdi                       ; global_super — DECREF and discard
    DECREF_REG rdi

    ; Walk from class->tp_base up the chain looking for name
    mov rax, [rbp-16]              ; class
    mov rax, [rax + PyTypeObject.tp_base]
    test rax, rax
    jz .lsa_not_found

.lsa_walk:
    ; Check this type's tp_dict
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .lsa_next_base

    push rax                       ; save current type
    mov rsi, [rbp-24]              ; name
    call dict_get
    pop rcx                        ; restore current type
    test rax, rax
    jnz .lsa_found

.lsa_next_base:
    mov rax, [rcx + PyTypeObject.tp_base]
    test rax, rax
    jnz .lsa_walk

.lsa_not_found:
    ; DECREF class and self
    mov rdi, [rbp-16]
    call obj_decref
    mov rdi, [rbp-8]
    call obj_decref
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "super: attribute not found"
    call raise_exception

.lsa_found:
    ; rax = attribute value (borrowed ref from dict_get)
    INCREF rax
    push rax                       ; save attr

    ; DECREF class
    mov rdi, [rbp-16]
    call obj_decref

    pop rax                        ; restore attr

    ; Check method flag
    cmp qword [rbp-32], 0
    je .lsa_attr_mode

    ; Method mode: push self + func
    mov rdx, [rbp-8]              ; self (already has ref from stack)
    VPUSH rdx                     ; push self
    VPUSH rax                     ; push func
    jmp .lsa_done

.lsa_attr_mode:
    ; Attr mode: DECREF self, push NULL + attr
    push rax                      ; save attr
    mov rdi, [rbp-8]
    call obj_decref
    xor eax, eax
    VPUSH rax                     ; push NULL
    pop rax
    VPUSH rax                     ; push attr
    jmp .lsa_done

.lsa_done:
    ; Skip 1 CACHE entry = 2 bytes
    add rbx, 2
    leave
    DISPATCH
END_FUNC op_load_super_attr
