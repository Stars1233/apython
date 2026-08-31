; module.asm - Dict-backed attribute namespaces
;
; A module and a SimpleNamespace are the same object with different repr:
; a dict, and getattr/setattr that read and write it by name.
; PyModuleObject: name + dict

%include "macros.inc"
%include "object.inc"


%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern obj_incref
extern str_from_cstr
extern str_type
extern dict_new
extern dict_get
extern dict_set
extern type_type
extern module_traverse
extern module_clear_gc
extern none_singleton
extern ap_strcmp

; ============================================================================
; module_new(PyObject *name_str, PyObject *dict) -> PyModuleObject*
; Create a new module with given name and dict
; If dict is NULL, creates a new empty dict
; ============================================================================
DEF_FUNC module_new
    push rbx
    push r12

    mov rbx, rdi                ; name_str
    mov r12, rsi                ; dict (or NULL)

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
    ; Allocate PyModuleObject (GC-tracked)
    mov edi, PyModuleObject_size
    lea rsi, [rel module_type]
    call gc_alloc
    ; ob_refcnt=1, ob_type set by gc_alloc

    ; INCREF name
    push rax
    mov rdi, rbx
    call obj_incref
    pop rax

    ; Fill module fields
    mov [rax + PyModuleObject.mod_name], rbx
    mov [rax + PyModuleObject.mod_dict], r12

    push rax
    mov rdi, rax
    call gc_track
    pop rax

    pop r12
    pop rbx
    leave
    ret
END_FUNC module_new

; ============================================================================
; module_dealloc(PyObject *self)
; ============================================================================
DEF_FUNC_LOCAL module_dealloc
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

; ============================================================================
; module_getattr(PyObject *self, PyObject *name_str) -> PyObject*
; Look up attribute in module's dict
; ============================================================================
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

; ============================================================================
; module_setattr(PyObject *self, PyObject *name_str, PyObject *value) -> int
; Set attribute in module's dict
; ============================================================================
DEF_FUNC module_setattr
    ; dict_set(mod_dict, name, value, value_tag, key_tag)
    mov rax, rdi                ; self
    mov rdi, [rax + PyModuleObject.mod_dict]
    ; rsi = name_str and rdx = value are both already Values
    call dict_set
    xor eax, eax               ; return 0 (success)
    leave
    ret
END_FUNC module_setattr

; ============================================================================
; module_repr(PyObject *self) -> PyObject*
; Returns "<module 'name'>"
; Simplified: just returns the name
; ============================================================================
DEF_FUNC module_repr
    mov rax, [rdi + PyModuleObject.mod_name]
    test rax, rax
    jz .fallback
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.fallback:
    lea rdi, [rel module_repr_str]
    call str_from_cstr
    leave
    ret
END_FUNC module_repr

; ============================================================================
; Data
; ============================================================================
section .rodata
module_repr_str: db "<module>", 0
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
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq module_traverse                        ; tp_traverse
    dq module_clear_gc                        ; tp_clear
    dq 0            ; tp_dictoffset

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
extern kw_names_pending
extern obj_generic_attr

section .text

;; ============================================================================
;; namespace_new() -> PySimpleNamespaceObject* with a fresh dict
;; ============================================================================
global namespace_new
DEF_FUNC namespace_new
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
global namespace_set
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

DEF_FUNC_LOCAL namespace_dealloc
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
DEF_FUNC namespace_getattr
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "namespace has no attribute storage"
    call raise_exception
END_FUNC namespace_setattr

;; namespace_repr(rdi = self) -> str   "namespace(a=1, b=2)"
NR_SELF  equ 8
NR_DICT  equ 16
NR_IDX   equ 24
NR_COUNT equ 32
NR_CAP   equ 40
NR_BUF   equ 1080        ; 1024 bytes, [rbp-1080, rbp-56)
NR_FRAME equ 1088
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
