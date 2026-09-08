; module.asm - Dict-backed attribute namespaces
;
; A module and a SimpleNamespace are the same object with different repr:
; a dict, and getattr/setattr that read and write it by name.
; PyModuleObject: name + dict

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern raise_exception
extern exc_TypeError_type
extern obj_incref
extern str_from_cstr
extern str_type
extern dict_new
extern dict_get
extern dict_set
extern type_type
extern none_singleton
extern ap_strcmp

;; ============================================================================
;; module_new(PyObject *name_str, PyObject *dict) -> PyModuleObject*
;; Create a new module with given name and dict
;; If dict is NULL, creates a new empty dict
;;
;; module_new_of takes the TYPE as well, for a subclass of ModuleType, and
;; ecx = 0 to leave the dict EMPTY -- which is what module.__new__ hands back
;; before module.__init__ has run.  The constructor used to allocate
;; module_type unconditionally, so `class M(ModuleType)` produced a plain
;; module: type(M("x")) was `module` and isinstance(m, M) was False.
;; ============================================================================
DEF_FUNC module_new
    lea rdx, [rel module_type]
    mov ecx, 1
    leave
    jmp module_new_of
END_FUNC module_new

;; ============================================================================
;; module_new_of(rdi = name_str, rsi = dict or NULL, rdx = the type,
;;               ecx = 1 to write __name__ and __doc__) -> PyModuleObject*
;; ============================================================================
MNO_FILL  equ 8
MNO_FRAME equ 24            ; + 3 pushes = 48, 16-aligned
global module_new_of
DEF_FUNC module_new_of, MNO_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - MNO_FILL], rcx

    mov rbx, rdi                ; name_str
    mov r12, rsi                ; dict (or NULL)
    mov r13, rdx                ; the type

    ; Create dict if NULL
    test r12, r12
    jnz .have_dict
    call dict_new
    mov r12, rax
    jmp .alloc
.have_dict:
    ; INCREF the dict (module holds a reference)
    mov rdi, r12
    call obj_incref

.alloc:
    ; Allocate PyModuleObject (GC-tracked).  A subclass is wider, and gc_alloc
    ; does not zero what it hands back, so the tail past the module's own
    ; fields is cleared here -- instance_traverse walks it as Values.
    mov rdi, [r13 + PyTypeObject.tp_basicsize]
    cmp rdi, PyModuleObject_size
    jae .alloc_size_ok
    mov rdi, PyModuleObject_size
.alloc_size_ok:
    push rdi
    mov rsi, r13
    call gc_alloc
    pop rcx
    ; ob_refcnt=1, ob_type set by gc_alloc
    mov rdx, PyModuleObject_size
.alloc_zero:
    cmp rdx, rcx
    jae .alloc_zeroed
    mov qword [rax + rdx], 0
    add rdx, 8
    jmp .alloc_zero
.alloc_zeroed:

    ; INCREF name
    push rax
    mov rdi, rbx
    call obj_incref
    pop rax

    ; Fill module fields
    mov [rax + PyModuleObject.mod_name], rbx
    mov [rax + PyModuleObject.mod_dict], r12
    cmp qword [rbp - MNO_FILL], 0
    je .mn_tracked

    ; __name__ lives in the dict, not only in mod_name: module_getattr looks
    ; nowhere else, so every builtin module answered AttributeError for it --
    ; and reading a module's own __name__ is ordinary code, not introspection.
    ; CPython's module init writes it into the dict for the same reason.
    push rax
    lea rdi, [rel mod_name_key]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, rbx
    call dict_set
    pop rdi
    call obj_decref
    pop rax

    ; __doc__ = None, which is what a module without a docstring has.  It was
    ; simply absent, so reading __doc__ at module level was a NameError -- and
    ; the module body's own STORE_NAME overwrites this when there is one.
    push rax
    lea rdi, [rel mod_doc_key]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    pop rdi
    call obj_decref
    pop rax

.mn_tracked:
    push rax
    mov rdi, rax
    call gc_track
    pop rax

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC module_new_of

;; ============================================================================
;; module_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL module_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    ; XDECREF name
    mov rdi, [rbx + PyModuleObject.mod_name]
    test rdi, rdi
    jz .no_name
    call obj_decref
.no_name:
    ; XDECREF dict
    mov rdi, [rbx + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .no_dict
    call obj_decref
.no_dict:
    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC module_dealloc

;; ============================================================================
;; module_getattr(PyObject *self, PyObject *name_str) -> rax = Value
;; Look up attribute in module's dict
;; ============================================================================
DEF_FUNC module_getattr
    push rbx
    push r12
    mov rbx, rdi                ; self
    mov r12, rsi                ; save name_str

    ; Check for __dict__ special attribute
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel ma_dunder_dict]
    call ap_strcmp
    test eax, eax
    jnz .normal_lookup

    ; Return the module's dict directly (always a heap object)
    mov rax, [rbx + PyModuleObject.mod_dict]
    test rax, rax
    jz .not_found
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.normal_lookup:
    ; dict_get(mod_dict, name_str)
    mov rdi, [rbx + PyModuleObject.mod_dict]
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value

    ; INCREF if found (dict_get returns borrowed ref)
    test edx, edx
    jz .not_found
    INCREF_VAL rax, rdx         ; tag-aware: skip for SmallInt
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.not_found:
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC module_getattr

;; ============================================================================
;; module_setattr(PyObject *self, PyObject *name_str, PyObject *value) -> int
;; Set attribute in module's dict
;; ============================================================================
DEF_FUNC module_setattr
    ; A NULL value is tp_setattr's DELETE convention -- the same one
    ; dict_ass_subscript honours by routing to dict_del.  This handed it to
    ; dict_set, which stored the NULL and kept the key, so `del mod.x` left
    ; the entry findable with a NULL value.  Two things followed: dk_version
    ; did not move, because a dict's keys version tracks the KEY SET and
    ; overwriting a value is not a change to it, so every LOAD_GLOBAL_BUILTIN
    ; inline cache guarding on it stayed valid; and when a cache did notice
    ; the NULL and deopt, op_load_global looked the name up again, FOUND it,
    ; re-specialized, and pushed the NULL.  A NULL Value on the value stack is
    ; not an error anything notices -- it propagates until something
    ; dereferences it.
    mov rax, rdi                ; self
    test rdx, rdx
    jz .ms_delete
    mov rdi, [rax + PyModuleObject.mod_dict]
    ; rsi = name_str and rdx = value are both already Values
    call dict_set
    xor eax, eax               ; return 0 (success)
    leave
    ret

.ms_delete:
    push rax                    ; the module, for the error message
    push rsi                    ; the name, likewise
    mov rdi, [rax + PyModuleObject.mod_dict]
    extern dict_del_opt
    call dict_del_opt           ; -1 when the name was never there
    pop rsi
    pop rdi
    test eax, eax
    js .ms_missing
    xor eax, eax               ; return 0 (success)
    leave
    ret

.ms_missing:
    ; CPython raises AttributeError naming the module, exactly as a failed
    ; READ of the same name does.  Storing a NULL under a fresh key, which is
    ; what this used to do, invented an entry instead.
    xor edx, edx
    extern raise_no_attribute
    call raise_no_attribute     ; does not return
END_FUNC module_setattr

;; ============================================================================
;; module_repr(PyObject *self) -> PyObject*
;;
;; "<module 'sys' (built-in)>" for a module with no __file__, and
;; "<module 'x' from '/path'>" for one that has it.  This used to answer the
;; bare name, which reads as a string rather than a module and is the one
;; repr in the tree that did not even have angle brackets.
;;
;; Note that __file__ here is the .pyc a module was loaded from, where
;; CPython's is the .py -- a difference in that attribute, not in this.
;; ============================================================================
MR_FILE  equ 8
MR_BUILTIN equ 16       ; is this name in builtin_module_table?
MR_FRAME equ 24            ; + 1 push = 32, 16-aligned
DEF_FUNC module_repr, MR_FRAME
    push rbx
    mov rbx, rdi
    mov qword [rbp - MR_FILE], 0

    extern obj_repr_buf
    extern obj_repr_buf_str
    extern rbt_append_cstr

    ; __file__ out of the module dict, when it is a str.
    mov rax, [rbx + PyModuleObject.mod_dict]
    test rax, rax
    jz .mr_no_file
    mov rdi, rax
    lea rdi, [rel mod_file_key]
    call str_from_cstr
    push rax
    mov rdi, [rbx + PyModuleObject.mod_dict]
    mov rsi, rax
    extern dict_get
    call dict_get
    pop rdi
    push rax
    call obj_decref
    pop rax
    test rax, rax
    jz .mr_no_file
    V_UNPACK rax, rdx
    cmp edx, TAG_PTR
    jne .mr_no_file
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .mr_no_file
    mov [rbp - MR_FILE], rax
.mr_no_file:

    ; "(built-in)" is for a module that really is one.  The decision used to
    ; be __file__ alone, so anything without one -- types.ModuleType('x'), and
    ; every module a test builds by hand -- claimed to be built-in.  CPython
    ; asks __spec__ first and says plain `<module 'x'>` for a module that is
    ; neither built-in nor loaded from a file.
    ;
    ; __spec__ cannot answer here: it is None or absent on every module in
    ; this tree.  builtin_module_table can, exactly -- it is the list
    ; sys.builtin_module_names is built from, so "is this name in it" IS the
    ; question "is this module built-in".
    mov qword [rbp - MR_BUILTIN], 0
    mov rsi, [rbx + PyModuleObject.mod_name]
    test rsi, rsi
    jz .mr_not_builtin
    lea rsi, [rsi + PyStrObject.data]
    extern builtin_module_table
    extern builtin_module_count
    lea r8, [rel builtin_module_table]
    mov r9, [rel builtin_module_count]
.mr_bm_loop:
    test r9, r9
    jz .mr_not_builtin
    mov rdi, [r8]                   ; the row's name, a C string
    push r8
    push r9
    push rsi
    call ap_strcmp
    pop rsi
    pop r9
    pop r8
    test eax, eax
    jz .mr_is_builtin
    add r8, BuiltinModule_size
    dec r9
    jmp .mr_bm_loop
.mr_is_builtin:
    mov qword [rbp - MR_BUILTIN], 1
.mr_not_builtin:

    lea rdi, [rel mod_repr_open]
    call obj_repr_buf
    mov rdi, rax
    mov rsi, [rbx + PyModuleObject.mod_name]
    test rsi, rsi
    jz .mr_unnamed
    lea rsi, [rsi + PyStrObject.data]
    jmp .mr_have_name
.mr_unnamed:
    lea rsi, [rel mod_repr_unknown]
.mr_have_name:
    call rbt_append_cstr
    mov rdi, rax
    cmp qword [rbp - MR_FILE], 0
    jne .mr_from
    cmp qword [rbp - MR_BUILTIN], 0
    je .mr_plain
    lea rsi, [rel mod_repr_builtin]
    call rbt_append_cstr
    jmp .mr_done
.mr_plain:
    lea rsi, [rel mod_repr_plain]
    call rbt_append_cstr
    jmp .mr_done
.mr_from:
    lea rsi, [rel mod_repr_from]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - MR_FILE]
    lea rsi, [rsi + PyStrObject.data]
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel mod_repr_close]
    call rbt_append_cstr
.mr_done:
    call obj_repr_buf_str
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC module_repr

;; ============================================================================
;; Data
;; ============================================================================
section .rodata

mod_name_key: db "__name__", 0
mod_doc_key: db "__doc__", 0
mod_file_key: db "__file__", 0
mod_repr_open:    db "<module '", 0
mod_repr_unknown: db "?", 0
mod_repr_builtin: db "' (built-in)>", 0
mod_repr_plain:   db "'>", 0
mod_repr_from:    db "' from '", 0
mod_repr_close:   db "'>", 0
module_type_name: db "module", 0
ma_dunder_dict: db "__dict__", 0

section .data
align 8
global module_type


module_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq module_type_name         ; tp_name
    dq PyModuleObject_size      ; tp_basicsize
    dq module_dealloc           ; tp_dealloc
    dq module_repr              ; tp_repr
    dq module_repr              ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq module_getattr           ; tp_getattr
    dq module_setattr           ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq module_type_new          ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC | TYPE_FLAG_BASETYPE   ; tp_flags
    dq 0                        ; tp_bases
    dq module_traverse                        ; tp_traverse
    dq module_clear_gc                        ; tp_clear
    ; A module's own dict IS its __dict__, which is what makes `self.x = 1`
    ; work on a ModuleType subclass -- CPython's PyModule_Type says the same
    ; thing with offsetof(PyModuleObject, md_dict).
    dq PyModuleObject.mod_dict  ; tp_dictoffset
    dq 0                        ; tp_tailslots

;; ============================================================================
;; (was src/pyo/namespace.asm)
;; ============================================================================

section .text

extern ap_malloc
extern ap_free
extern obj_incref
extern obj_decref
extern dict_new
extern dict_get
extern dict_set
extern dict_type
extern str_type
extern str_from_cstr
extern str_from_cstr_heap
extern str_new_heap
extern obj_repr
extern type_type
extern raise_exception
extern exc_TypeError_type

section .text

;; ============================================================================
;; namespace_new() -> PySimpleNamespaceObject* with a fresh dict
;; ============================================================================
DEF_FUNC namespace_new, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov edi, PySimpleNamespaceObject_size
    call ap_malloc
    mov rbx, rax
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel namespace_type]
    mov [rbx + PyObject.ob_type], rax
    call dict_new
    mov [rbx + PySimpleNamespaceObject.ns_dict], rax
    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC namespace_new

;; namespace_set(rdi = ns, rsi = name cstr, rdx = value Value)
;; Helper for building one from assembly; steals nothing, INCREFs via dict_set.
DEF_FUNC namespace_set
    push rbx
    push r12
    mov rbx, [rdi + PySimpleNamespaceObject.ns_dict]
    mov r12, rdx
    mov rdi, rsi
    call str_from_cstr_heap
    push rax
    mov rdi, rbx
    mov rsi, rax
    mov rdx, r12
    call dict_set
    pop rdi
    call obj_decref
    pop r12
    pop rbx
    leave
    ret
END_FUNC namespace_set

DEF_FUNC_LOCAL namespace_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PySimpleNamespaceObject.ns_dict]
    test rdi, rdi
    jz .nsd_free
    call obj_decref
.nsd_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC namespace_dealloc

;; namespace_getattr(rdi = self, rsi = name) -> Value or NULL
DEF_FUNC namespace_getattr, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    cmp qword [rdi + PySimpleNamespaceObject.ns_dict], 0
    je .nsg_none
    mov rdi, [rdi + PySimpleNamespaceObject.ns_dict]
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .nsg_generic
    INCREF_VAL rax, rdx
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.nsg_generic:
.nsg_none:
    ; Not here: LOAD_ATTR's shared tail still gets a chance at __class__
    ; and __dict__, so returning NULL is the right answer.
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC namespace_getattr

;; namespace_setattr(rdi = self, rsi = name, rdx = value Value) -> 0
DEF_FUNC namespace_setattr
    mov rdi, [rdi + PySimpleNamespaceObject.ns_dict]
    test rdi, rdi
    jz .nss_no_dict
    call dict_set
    xor eax, eax
    leave
    ret
.nss_no_dict:
    RAISE exc_TypeError_type, "namespace has no attribute storage"
END_FUNC namespace_setattr

;; namespace_repr(rdi = self) -> str   "namespace(a=1, b=2)"
NR_SELF  equ 8
NR_DICT  equ 16
NR_IDX   equ 24
NR_COUNT equ 32
NR_CAP   equ 40
NR_BUF   equ 1080        ; 1024 bytes, [rbp-1080, rbp-56)
NR_FRAME equ 1088           ; + 2 pushes = 1104
DEF_FUNC namespace_repr, NR_FRAME
    push rbx
    push r13
    mov [rbp - NR_SELF], rdi
    mov qword [rbp - NR_IDX], 0
    mov qword [rbp - NR_COUNT], 0

    lea rbx, [rbp - NR_BUF]
    xor r13d, r13d
    CSTRING rsi, "namespace("
.nr_prefix:
    movzx eax, byte [rsi]
    test al, al
    jz .nr_setup
    inc rsi
    mov [rbx + r13], al
    inc r13
    jmp .nr_prefix

.nr_setup:
    mov rax, [rbp - NR_SELF]
    mov rax, [rax + PySimpleNamespaceObject.ns_dict]
    test rax, rax
    jz .nr_close
    mov [rbp - NR_DICT], rax
    mov rcx, [rax + PyDictObject.capacity]
    mov [rbp - NR_CAP], rcx

.nr_scan:
    mov rcx, [rbp - NR_IDX]
    cmp rcx, [rbp - NR_CAP]
    jge .nr_close
    mov rax, [rbp - NR_DICT]
    mov rax, [rax + PyDictObject.entries]
    imul rdx, rcx, DICT_ENTRY_SIZE
    mov rsi, [rax + rdx + DictEntry.key]
    test rsi, rsi
    jz .nr_next
    cmp r13, NR_BUF - 64
    jae .nr_close

    ; ", " between items
    cmp qword [rbp - NR_COUNT], 0
    je .nr_no_sep
    mov byte [rbx + r13], ','
    mov byte [rbx + r13 + 1], ' '
    add r13, 2
.nr_no_sep:
    inc qword [rbp - NR_COUNT]

    ; key -- always a str here
    mov r8, [rsi + PyStrObject.ob_size]
    lea rsi, [rsi + PyStrObject.data]
    xor ecx, ecx
.nr_key:
    cmp rcx, r8
    jge .nr_key_done
    cmp r13, NR_BUF - 40
    jae .nr_key_done
    movzx eax, byte [rsi + rcx]
    mov [rbx + r13], al
    inc r13
    inc rcx
    jmp .nr_key
.nr_key_done:
    mov byte [rbx + r13], '='
    inc r13

    ; = repr(value)
    mov rax, [rbp - NR_DICT]
    mov rax, [rax + PyDictObject.entries]
    mov rcx, [rbp - NR_IDX]
    imul rdx, rcx, DICT_ENTRY_SIZE
    mov rdi, [rax + rdx + DictEntry.value]
    call obj_repr
    V_UNPACK rax, rdx
    test rax, rax
    jz .nr_next
    mov r8, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor ecx, ecx
.nr_val:
    cmp rcx, r8
    jge .nr_val_done
    cmp r13, NR_BUF - 8
    jae .nr_val_done
    movzx edx, byte [rsi + rcx]
    mov [rbx + r13], dl
    inc r13
    inc rcx
    jmp .nr_val
.nr_val_done:
    mov rdi, rax
    call obj_decref

.nr_next:
    inc qword [rbp - NR_IDX]
    jmp .nr_scan

.nr_close:
    mov byte [rbx + r13], ')'
    inc r13
    mov rdi, rbx
    mov rsi, r13
    call str_new_heap
    mov edx, TAG_PTR
    pop r13
    pop rbx
    leave
    ret
END_FUNC namespace_repr

section .data

align 8
ns_name_str: db "types.SimpleNamespace", 0

align 8
global namespace_type
namespace_type:
    dq 1                            ; ob_refcnt (immortal)
    dq type_type                    ; ob_type
    dq ns_name_str                  ; tp_name
    dq PySimpleNamespaceObject_size ; tp_basicsize
    dq namespace_dealloc            ; tp_dealloc
    dq namespace_repr               ; tp_repr
    dq namespace_repr               ; tp_str
    dq 0                            ; tp_hash
    dq 0                            ; tp_call
    dq namespace_getattr            ; tp_getattr
    dq namespace_setattr            ; tp_setattr
    dq 0                            ; tp_richcompare
    dq 0                            ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq 0                            ; tp_new
    dq 0                            ; tp_as_number
    dq 0                            ; tp_as_sequence
    dq 0                            ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq 0                            ; tp_flags
    dq 0                            ; tp_bases
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset
    dq 0                        ; tp_tailslots

section .text

;; ============================================================================
;; GC traverse and clear.  These lived in gc.asm, which left the collector
;; holding the reference graph of every type in the system; a type's own
;; file is the only place that knows which of its fields are owned.
;; ============================================================================

;; ============================================================================
;; ---- module_traverse / module_clear ----
;; ============================================================================
DEF_FUNC module_traverse, 8        ; rsp 16-aligned at the call the macros below expand to
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyModuleObject.mod_dict]
    VISIT_PTR rdi
    mov rdi, [rbx + PyModuleObject.mod_name]
    VISIT_PTR rdi

    pop rbx
    leave
    ret
END_FUNC module_traverse

DEF_FUNC module_clear_gc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyModuleObject.mod_dict]
    mov qword [rbx + PyModuleObject.mod_dict], 0
    test rdi, rdi
    jz .done
    call obj_decref
.done:
    pop rbx
    leave
    ret
END_FUNC module_clear_gc

section .text


;; MTN_SET_NONE key -- one dict entry bound to None, on the module in MTN_MOD.
%macro MTN_SET_NONE 1
    lea rdi, [rel %1]
    call str_from_cstr_heap
    push rax
    push rax                    ; pad
    mov rcx, [rbp - MTN_MOD]
    mov rdi, [rcx + PyModuleObject.mod_dict]
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    pop rdi
    pop rdi
    call obj_decref
%endmacro

;; ============================================================================
;; module_type_new(rdi = type, rsi = args, rdx = nargs)
;;   -> fat (rax = the new module, rdx = TAG_PTR); raises and does not return
;;      on a bad argument
;;
;; `types.ModuleType(name[, doc])`.  module_type had no tp_new and no tp_init,
;; so type_call fell through to instance_new, which allocated
;; PyModuleObject_size bytes and handed back a module whose mod_name and
;; mod_dict were whatever the allocator left -- before the arity check refused
;; the arguments and reported "module() takes no arguments".  A constructor
;; belongs in tp_new, which is what type_call consults; tp_call on a type is
;; what makes that type's INSTANCES callable.  mappingproxy hit the same
;; pattern.
;;
;; module_new already writes __name__ and __doc__ = None into the dict, so the
;; only extra work is the optional doc.
;; ============================================================================
MTN_MOD   equ 8
MTN_TYPE  equ 16
MTN_FRAME equ 32            ; + 0 pushes = 32, 16-aligned
DEF_FUNC module_type_new, MTN_FRAME
    mov [rbp - MTN_TYPE], rdi
    cmp rdx, 1
    jl .mtn_arity
    cmp rdx, 2
    jg .mtn_arity
    push rdx                    ; nargs
    push rsi                    ; args

    mov rdi, [rsi]              ; args[0], the name, as a Value
    V_TEST_PTR rdi, rax
    ja .mtn_bad_name
    test rdi, rdi
    jz .mtn_bad_name
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .mtn_bad_name

    mov rdx, [rbp - MTN_TYPE]   ; the class that was called, not module_type
    xor esi, esi                ; a dict of its own
    mov ecx, 1
    extern module_new_of
    call module_new_of
    mov [rbp - MTN_MOD], rax

    pop rsi
    pop rdx
    cmp rdx, 2
    jne .mtn_done

    ; The docstring, over the None module_new left there.
    mov rcx, [rsi + 8]          ; args[1]
    mov rax, [rbp - MTN_MOD]
    mov rdi, [rax + PyModuleObject.mod_dict]
    push rcx
    push rcx                    ; pad
    lea rdi, [rel mod_doc_key]
    call str_from_cstr_heap
    pop rcx
    pop rcx
    mov rsi, rax
    push rax
    push rax                    ; pad
    mov rax, [rbp - MTN_MOD]
    mov rdi, [rax + PyModuleObject.mod_dict]
    mov rdx, rcx
    call dict_set
    pop rdi
    pop rdi
    call obj_decref

.mtn_done:
    ; CPython's module.__init__ leaves these three in the dict as None, and
    ; sorted(m.__dict__) is how a test notices they are missing.  A module the
    ; import system builds gets real ones written over these.
    MTN_SET_NONE mtn_loader_key
    MTN_SET_NONE mtn_package_key
    MTN_SET_NONE mtn_spec_key
    mov rax, [rbp - MTN_MOD]
    mov edx, TAG_PTR
    leave
    ret

.mtn_bad_name:
    add rsp, 16
    RAISE exc_TypeError_type, "module.__init__() argument 1 must be str, not None"
.mtn_arity:
    CSTRING rdi, "module() takes at most 2 arguments ("
    mov rsi, rdx
    CSTRING rdx, " given)"
    extern raise_type_error_counted
    jmp raise_type_error_counted
END_FUNC module_type_new

;; MMI_SET key_cstr, value -- write one entry into the module being
;; initialised.  rdi (args) and rsi (nargs) are preserved across it.
%macro MMI_SET 2
    mov rax, %2
    push rsi
    push rdi
    push rax
    push rax                    ; pad
    lea rdi, [rel %1]
    call str_from_cstr_heap
    pop rdx
    pop rdx                     ; the value again
    push rax
    push rax                    ; pad
    mov rcx, [rbp - MMI_SELF]
    mov rdi, [rcx + PyModuleObject.mod_dict]
    mov rsi, rax
    call dict_set
    pop rdi
    pop rdi
    call obj_decref
    pop rdi
    pop rsi
%endmacro

%macro MMI_SET_NONE 1
    push rsi
    push rdi
    lea rdi, [rel %1]
    call str_from_cstr_heap
    push rax
    push rax                    ; pad
    mov rcx, [rbp - MMI_SELF]
    mov rdi, [rcx + PyModuleObject.mod_dict]
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    pop rdi
    pop rdi
    call obj_decref
    pop rdi
    pop rsi
%endmacro


;; ============================================================================
;; module_method_new(rdi = args, rsi = nargs) -> a Value
;;
;; `module.__new__(cls, *args)`.  CPython's is PyType_GenericNew: it allocates
;; and nothing more, and module.__init__ is what writes the name.  The split
;; matters because a ModuleType subclass is written that way --
;;
;;     class M(ModuleType):
;;         def __init__(self, name):
;;             super().__init__(name)
;;
;; -- and with neither name in module's tp_dict, `super().__init__` resolved
;; to object's and raised, while `super().__new__` hit the object.__new__
;; gate.  module_type had no tp_dict at all.
;; ============================================================================
MMN_TYPE  equ 8
MMN_FRAME equ 16            ; + 0 pushes = 16, 16-aligned
DEF_FUNC module_method_new, MMN_FRAME
    test rsi, rsi
    jle .mmn_no_args
    mov rdi, [rdi]              ; args[0], the class
    V_TEST_PTR rdi, rax
    ja .mmn_not_a_type
    test rdi, rdi
    jz .mmn_not_a_type
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .mmn_not_a_type
    mov [rbp - MMN_TYPE], rdi
    lea rsi, [rel module_type]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jz .mmn_not_a_module

    ; An empty name rather than a NULL one: module_repr and module_dealloc
    ; both read the field, and __init__ overwrites it a moment later.
    lea rdi, [rel mod_empty_name]
    call str_from_cstr_heap
    push rax
    mov rdi, rax
    xor esi, esi                ; a dict of its own
    mov rdx, [rbp - MMN_TYPE]
    xor ecx, ecx                ; and an EMPTY one, as CPython's is
    call module_new_of
    pop rdi
    push rax
    call obj_decref             ; the module holds the name now
    pop rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.mmn_no_args:
    RAISE exc_TypeError_type, "module.__new__(): not enough arguments"
.mmn_not_a_type:
    RAISE exc_TypeError_type, "module.__new__(X): X is not a type object"
.mmn_not_a_module:
    RAISE exc_TypeError_type, "module.__new__(X): X is not a subtype of module"
END_FUNC module_method_new

;; ============================================================================
;; module_method_init(rdi = args, rsi = nargs) -> a Value (None)
;;
;; `module.__init__(self, name[, doc])` -- the half that writes the name, the
;; docstring and the three attributes CPython leaves as None.
;; ============================================================================
MMI_SELF  equ 8
MMI_FRAME equ 16            ; + 0 pushes = 16, 16-aligned
DEF_FUNC module_method_init, MMI_FRAME
    cmp rsi, 2
    jl .mmi_arity
    cmp rsi, 3
    jg .mmi_arity
    push rsi
    push rdi
    mov rdi, [rdi]              ; args[0], self
    V_TEST_PTR rdi, rax
    ja .mmi_bad_self
    test rdi, rdi
    jz .mmi_bad_self
    mov rax, [rdi + PyObject.ob_type]
    push rdi
    push rdi
    mov rdi, rax
    lea rsi, [rel module_type]
    call type_is_subtype
    pop rdi
    pop rdi
    test eax, eax
    jz .mmi_bad_self
    mov [rbp - MMI_SELF], rdi

    pop rdi                     ; args
    pop rsi                     ; nargs
    mov rdx, [rdi + 8]          ; args[1], the name
    V_TEST_PTR rdx, rax
    ja .mmi_bad_name
    test rdx, rdx
    jz .mmi_bad_name
    mov rax, [rdx + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .mmi_bad_name

    ; The name, in the field and in the dict both.
    push rsi
    push rdi
    mov rdi, rdx
    call obj_incref
    pop rdi
    pop rsi
    mov rdx, [rdi + 8]
    mov rcx, [rbp - MMI_SELF]
    mov rax, [rcx + PyModuleObject.mod_name]
    mov [rcx + PyModuleObject.mod_name], rdx
    push rsi
    push rdi
    mov rdi, rax
    call obj_decref             ; whatever __new__ left there
    pop rdi
    pop rsi

    MMI_SET mod_name_key, [rdi + 8]
    cmp rsi, 3
    jl .mmi_no_doc
    MMI_SET mod_doc_key, [rdi + 16]
    jmp .mmi_rest
.mmi_no_doc:
    MMI_SET_NONE mod_doc_key
.mmi_rest:
    MMI_SET_NONE mtn_loader_key
    MMI_SET_NONE mtn_package_key
    MMI_SET_NONE mtn_spec_key

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.mmi_bad_self:
    add rsp, 16
    RAISE exc_TypeError_type, \
        "module.__init__() argument 1 must be a module"
.mmi_bad_name:
    RAISE exc_TypeError_type, \
        "module.__init__() argument 1 must be str, not None"
.mmi_arity:
    RAISE exc_TypeError_type, "module.__init__() takes at most 2 arguments"
END_FUNC module_method_init


section .rodata
mtn_loader_key:  db "__loader__", 0
mtn_package_key: db "__package__", 0
mtn_spec_key:    db "__spec__", 0
mod_empty_name:  db "", 0
