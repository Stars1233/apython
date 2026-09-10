; init_repr.asm - the containers' __repr__, reachable by name, from one table
;
; `list.__repr__ is object.__repr__` was True here and is False in CPython.
; That is CLAUDE.md's "a builtin's behaviour that lives only in a slot" again,
; and pprint is where it hurts: its dispatch table is keyed on the UNBOUND
; repr -- `_dispatch[list.__repr__] = _pprint_list`, `_dispatch[dict.__repr__]
; = _pprint_dict`, ten lines like it -- so list, tuple, dict, set, frozenset,
; bytearray, mappingproxy and SimpleNamespace all hashed to the SAME key, and
; the table ended up holding whichever line ran last.  pprint then handed a
; list to the SimpleNamespace printer, which reads `object.__dict__.items()`:
; every `'list' object has no attribute '__dict__'` in the sweep came from
; there, and none of them from __dict__.
;
; A table rather than a line per type in init.asm, for the same two reasons
; init_iter.asm is a table: each is otherwise identical, and init.asm is within
; a couple of kilobytes of the hand-written 100k cap.
;
; The thunks are DEF_DUNDER_STRREPR in methods/object.asm, beside every other
; dunder generator -- a NASM macro does not cross a file boundary.  Each reads
; the DEFINING type's slot, so a subclass with a __repr__ of its own wins and
; one without inherits the base's rather than re-dispatching into itself.
;
; __repr__ and not __str__, even though every one of these types has
; tp_str == tp_repr: CPython gives none of them a __str__ of its own, and
; `member_type.__str__ is object.__str__` is a question enum asks by name.

%include "macros.inc"
%include "object.inc"

extern dict_new
extern dict_add_builtin_func
extern type_stamp_methods

extern mn___repr__

;; ============================================================================
;; repr_types_init() -> nothing; every container gains a __repr__ entry
;;
;; Called from methods_init once every tp_dict it builds is in place.  Four of
;; these types have no tp_dict at all -- bool, mappingproxy, SimpleNamespace
;; and the three dict views -- so the loop builds one where it finds none
;; rather than assuming, and adds into the existing dict where there is one.
;; ============================================================================
RTI_IDX   equ 8
RTI_DICT  equ 16
RTI_FRAME equ 40            ; + 1 push = 48, 16-aligned

global repr_types_init
DEF_FUNC repr_types_init, RTI_FRAME
    push rbx
    mov qword [rbp - RTI_IDX], 0

.rti_loop:
    mov rax, [rbp - RTI_IDX]
    cmp rax, REPR_TYPE_COUNT
    jge .rti_done

    imul rax, rax, REPR_ROW_SIZE
    lea rbx, [rel repr_type_table]
    add rbx, rax

    mov rax, [rbx + REPR_ROW_TYPE]
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jnz .rti_have_dict
    call dict_new
    mov rcx, [rbx + REPR_ROW_TYPE]
    mov [rcx + PyTypeObject.tp_dict], rax
.rti_have_dict:
    mov [rbp - RTI_DICT], rax

    mov rdi, rax
    lea rsi, [rel mn___repr__]
    mov rdx, [rbx + REPR_ROW_FN]
    call dict_add_builtin_func

    ; type_stamp_methods gives the new entry its func_owner, which is what
    ; makes builtin_func_call refuse a wrong receiver.  Entries already
    ; stamped keep the owner they have, so re-running it costs a walk.
    mov rdi, [rbx + REPR_ROW_TYPE]
    call type_stamp_methods

    inc qword [rbp - RTI_IDX]
    jmp .rti_loop

.rti_done:
    pop rbx
    leave
    ret
END_FUNC repr_types_init

section .rodata
REPR_ROW_TYPE equ 0
REPR_ROW_FN   equ 8
REPR_ROW_SIZE equ 16

%macro REPR_ROW 1
    extern %1_type
    extern %1_dunder_repr
    dq %1_type, %1_dunder_repr
%endmacro

align 8
repr_type_table:
    REPR_ROW list
    REPR_ROW tuple
    REPR_ROW dict
    REPR_ROW set
    REPR_ROW frozenset
    REPR_ROW bytearray
    REPR_ROW bool
    REPR_ROW range_obj
    REPR_ROW slice
    REPR_ROW type
    REPR_ROW namespace
    REPR_ROW mappingproxy
    REPR_ROW memoryview
    REPR_ROW dict_keys_view
    REPR_ROW dict_values_view
    REPR_ROW dict_items_view
REPR_TYPE_COUNT equ ($ - repr_type_table) / REPR_ROW_SIZE
