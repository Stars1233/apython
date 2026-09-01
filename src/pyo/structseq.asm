; structseq.asm - tuple-with-named-fields, the shape os.stat() returns
;
; A struct sequence is a tuple whose entries also have names, plus a tail of
; fields reachable only by name.  os.stat() returns one; so do os.uname(),
; os.terminal_size(), sys.version_info and its three siblings.
;
; The instance IS a tuple -- the PyTupleObject header sits at offset 0, so
; every piece of tuple code reads it unchanged, `isinstance(st, tuple)` is
; True, unpacking works, and tuple's own methods come through the MRO from
; tuple_type.tp_dict.  bool_type does exactly this one layer down: a static
; type whose tp_basicsize is its base's, with tp_base patched at init.
;
;     +0   ob_refcnt   }
;     +8   ob_type     }  PyTupleObject, read verbatim by tuple's code
;     +16  ob_size     }  = n_in_sequence, NOT the field count
;     +24  ob_hash     }
;     +32  ob_item -> Value[n_in_sequence]
;     +40  the named-only fields, one Value each
;
; ob_item is a separate allocation rather than inline storage, which is what
; makes the tail safe: nothing tuple does can reach past the 40-byte header.
; That is the difference from str and bytes, whose subclasses need
; TP_DICT_AT_TAIL because their data IS inline.
;
; Each type carries a StructSeqDesc pointer in one extra qword just past
; PyTypeObject, so tp_getattr, tp_repr and tp_dealloc can all find their own
; descriptor from the instance's type.  Without it every type would need
; three thunks whose only job was to name a table.
;
; NOT GC-tracked, and deliberately.  A struct sequence holds numbers and
; strings; it cannot be part of a cycle, and CPython's are not tracked
; either.  So: ap_malloc, no tp_traverse, no TYPE_FLAG_HAVE_GC -- and its own
; tp_dealloc, because tuple_dealloc would hand a 40-plus-tail object to the
; tuple free pool, from which it would come back out as a plain tuple.

%include "macros.inc"
%include "object.inc"

ASM_INIT

extern ap_malloc
extern ap_free
extern ap_strcmp
extern obj_decref
extern obj_dealloc
extern obj_repr
extern str_from_cstr_heap
extern str_concat
extern tuple_type
extern type_type
extern exc_TypeError_type
extern raise_exception

section .text

;; ============================================================================
;; structseq_new(rdi = the type) -> rax = a zeroed instance, or 0
;;
;; ob_size is n_in_sequence, not the field count: the tail is reachable by
;; name only, and every tuple operation must see the shorter length.
;; ============================================================================
SSN_TYPE  equ 8
SSN_DESC  equ 16
SSN_OBJ   equ 24
SSN_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC structseq_new, SSN_FRAME
    mov [rbp - SSN_TYPE], rdi
    mov rax, [rdi + STRUCTSEQ_DESC]         ; the descriptor, just past the type
    mov [rbp - SSN_DESC], rax

    mov rdi, [rdi + PyTypeObject.tp_basicsize]
    call ap_malloc
    test rax, rax
    jz .ssn_out
    mov [rbp - SSN_OBJ], rax

    mov qword [rax + PyObject.ob_refcnt], 1
    mov rcx, [rbp - SSN_TYPE]
    mov [rax + PyObject.ob_type], rcx
    inc qword [rcx + PyObject.ob_refcnt]
    mov qword [rax + PyTupleObject.ob_hash], -1

    mov rdx, [rbp - SSN_DESC]
    mov rdx, [rdx + StructSeqDesc.n_in_sequence]
    mov [rax + PyTupleObject.ob_size], rdx

    ; Zero the tail before anything can look at it: the fields are filled one
    ; at a time, and a repr or a dealloc in between must see NULLs.
    mov rcx, [rbp - SSN_TYPE]
    mov rcx, [rcx + PyTypeObject.tp_basicsize]
    lea rsi, [rax + PyTupleObject_size]
    sub rcx, PyTupleObject_size
.ssn_zero_tail:
    cmp rcx, 8
    jb .ssn_items
    mov qword [rsi], 0
    add rsi, 8
    sub rcx, 8
    jmp .ssn_zero_tail

.ssn_items:
    ; ap_malloc(0) is not a thing worth relying on, so an empty sequence still
    ; gets one word.
    lea rdi, [rdx*8]
    test rdi, rdi
    jnz .ssn_items_alloc
    mov edi, 8
.ssn_items_alloc:
    push rdx
    push rdx                    ; twice, to keep rsp 16-byte aligned
    call ap_malloc
    pop rdx
    pop rdx
    test rax, rax
    jz .ssn_no_items
    mov rcx, [rbp - SSN_OBJ]
    mov [rcx + PyTupleObject.ob_item], rax
    xor ecx, ecx
.ssn_zero_items:
    cmp rcx, rdx
    jge .ssn_done
    mov qword [rax + rcx*8], 0
    inc rcx
    jmp .ssn_zero_items

.ssn_no_items:
    mov rdi, [rbp - SSN_OBJ]
    call ap_free
    xor eax, eax
    jmp .ssn_out
.ssn_done:
    mov rax, [rbp - SSN_OBJ]
.ssn_out:
    leave
    ret
END_FUNC structseq_new

;; ============================================================================
;; structseq_set(rdi = obj, esi = field index, rdx = Value)
;;
;; Takes over the caller's reference, so a builder reads as one line per
;; field.  Field indices below n_in_sequence land in the tuple, the rest in
;; the tail -- one numbering across both, as the descriptor lists them.
;; ============================================================================
DEF_FUNC_BARE structseq_set
    movsxd rsi, esi
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + STRUCTSEQ_DESC]
    mov rcx, [rax + StructSeqDesc.n_in_sequence]
    cmp rsi, rcx
    jge .sss_tail
    mov rax, [rdi + PyTupleObject.ob_item]
    mov [rax + rsi*8], rdx
    ret
.sss_tail:
    sub rsi, rcx
    lea rax, [rdi + PyTupleObject_size]
    mov [rax + rsi*8], rdx
    ret
END_FUNC structseq_set

;; ============================================================================
;; structseq_getattr(rdi = self, rsi = name str) -> rax = Value, or NULL
;;
;; NULL for an unknown name rather than a raise, so op_load_attr falls through
;; to the MRO's tp_dicts and tuple's index(), count() and __len__ still work.
;; That is the contract slice_getattr states and every other tp_getattr keeps.
;; ============================================================================
SSG_SELF  equ 8
SSG_NAME  equ 16
SSG_DESC  equ 24
SSG_FRAME equ 40            ; + 1 push = 48, 16-byte aligned

DEF_FUNC structseq_getattr, SSG_FRAME
    push rbx
    mov [rbp - SSG_SELF], rdi
    mov [rbp - SSG_NAME], rsi
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + STRUCTSEQ_DESC]
    mov [rbp - SSG_DESC], rax
    xor ebx, ebx

.ssg_loop:
    mov rax, [rbp - SSG_DESC]
    cmp rbx, [rax + StructSeqDesc.n_fields]
    jge .ssg_missing
    mov rax, [rax + StructSeqDesc.fields]
    mov rcx, rbx
    shl rcx, 4                              ; StructSeqField_size
    mov rdi, [rax + rcx + StructSeqField.name]
    mov rsi, [rbp - SSG_NAME]
    lea rsi, [rsi + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .ssg_found
    inc rbx
    jmp .ssg_loop

.ssg_found:
    ; The field's own index, which is not the loop index: a descriptor may
    ; list its fields in any order.
    mov rax, [rbp - SSG_DESC]
    mov rcx, [rax + StructSeqDesc.fields]
    mov rdx, rbx
    shl rdx, 4
    mov rdx, [rcx + rdx + StructSeqField.index]
    mov rcx, [rax + StructSeqDesc.n_in_sequence]
    mov rdi, [rbp - SSG_SELF]
    cmp rdx, rcx
    jge .ssg_tail
    mov rax, [rdi + PyTupleObject.ob_item]
    mov rax, [rax + rdx*8]
    jmp .ssg_out
.ssg_tail:
    sub rdx, rcx
    lea rax, [rdi + PyTupleObject_size]
    mov rax, [rax + rdx*8]
.ssg_out:
    INCREF_V rax, rcx
    pop rbx
    leave
    ret

.ssg_missing:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC structseq_getattr

;; ============================================================================
;; structseq_repr(rdi = self, edx = tag) -> rax = str, edx = TAG_PTR
;;
;; "os.stat_result(st_mode=33188, st_ino=..., ...)" -- the type's full dotted
;; tp_name, then the SEQUENCE fields only.  CPython's shows no more than
;; those, however many named-only ones follow.
;;
;; Built by repeated concatenation, which is quadratic in the field count and
;; entirely fine at twenty of them.
;; ============================================================================
SSR_SELF  equ 8
SSR_DESC  equ 16
SSR_ACC   equ 24
SSR_TMP   equ 32
SSR_FRAME equ 40            ; + 1 push = 48

DEF_FUNC structseq_repr, SSR_FRAME
    push rbx
    mov [rbp - SSR_SELF], rdi
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + STRUCTSEQ_DESC]
    mov [rbp - SSR_DESC], rax

    mov rdi, [rbp - SSR_SELF]
    mov rdi, [rdi + PyObject.ob_type]
    mov rdi, [rdi + PyTypeObject.tp_name]
    call str_from_cstr_heap
    mov [rbp - SSR_ACC], rax
    mov rdi, rax
    CSTRING rsi, "("
    call ssr_append_cstr
    mov [rbp - SSR_ACC], rax

    xor ebx, ebx
.ssr_loop:
    mov rax, [rbp - SSR_DESC]
    cmp rbx, [rax + StructSeqDesc.n_in_sequence]
    jge .ssr_close
    test rbx, rbx
    jz .ssr_name
    mov rdi, [rbp - SSR_ACC]
    CSTRING rsi, ", "
    call ssr_append_cstr
    mov [rbp - SSR_ACC], rax
.ssr_name:
    ; The field whose index is rbx, which the loop finds rather than assumes.
    mov rax, [rbp - SSR_DESC]
    mov rcx, [rax + StructSeqDesc.fields]
    xor edx, edx
.ssr_find:
    cmp rdx, [rax + StructSeqDesc.n_fields]
    jge .ssr_close              ; a descriptor missing a sequence index
    mov rsi, rdx
    shl rsi, 4
    cmp qword [rcx + rsi + StructSeqField.index], rbx
    je .ssr_have_field
    inc rdx
    jmp .ssr_find
.ssr_have_field:
    mov rsi, [rcx + rsi + StructSeqField.name]
    mov rdi, [rbp - SSR_ACC]
    call ssr_append_cstr
    mov [rbp - SSR_ACC], rax
    mov rdi, rax
    CSTRING rsi, "="
    call ssr_append_cstr
    mov [rbp - SSR_ACC], rax

    mov rdi, [rbp - SSR_SELF]
    mov rdi, [rdi + PyTupleObject.ob_item]
    mov rdi, [rdi + rbx*8]
    call obj_repr
    test rax, rax
    jz .ssr_fail
    mov rsi, rax
    mov rdi, [rbp - SSR_ACC]
    call ssr_append_obj         ; takes over the reference obj_repr returned
    mov [rbp - SSR_ACC], rax
    inc rbx
    jmp .ssr_loop

.ssr_close:
    mov rdi, [rbp - SSR_ACC]
    CSTRING rsi, ")"
    call ssr_append_cstr
    mov [rbp - SSR_ACC], rax
    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.ssr_fail:
    mov rdi, [rbp - SSR_ACC]
    call obj_decref
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
END_FUNC structseq_repr

;; ssr_append_cstr(rdi = accumulator str, rsi = C string) -> rax = the new one
;; ssr_append_obj (rdi = accumulator str, rsi = a str) -> rax = the new one
;;
;; Both release the accumulator they were given and the piece appended, so a
;; caller keeps exactly one reference and the loop is one line per piece.
;; The accumulator is an argument rather than a frame slot the helper reaches
;; into: DEF_FUNC_LOCAL builds a frame of its own, so `[rbp - SSR_ACC]` inside
;; one of these read the helper's frame, not structseq_repr's.
DEF_FUNC_LOCAL ssr_append_cstr
    push rbx
    push r12
    mov rbx, rdi
    mov rdi, rsi
    call str_from_cstr_heap
    mov r12, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, TAG_PTR
    call str_concat
    push rax
    push rax
    mov rdi, rbx
    call obj_decref             ; the old accumulator
    mov rdi, r12
    call obj_decref             ; the piece
    pop rax
    pop rax
    pop r12
    pop rbx
    leave
    ret
END_FUNC ssr_append_cstr

DEF_FUNC_LOCAL ssr_append_obj
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    mov ecx, TAG_PTR
    call str_concat             ; rdi is the accumulator already
    push rax
    push rax
    mov rdi, rbx
    call obj_decref             ; the old accumulator
    mov rdi, r12
    call obj_decref             ; the piece
    pop rax
    pop rax
    pop r12
    pop rbx
    leave
    ret
END_FUNC ssr_append_obj

;; ============================================================================
;; structseq_dealloc(rdi = self)
;;
;; Its own, not tuple's: tuple_dealloc returns a small tuple to the free pool
;; by ob_size, and a struct sequence with three visible fields would come back
;; out of it as a plain three-tuple -- with a tail nothing knew was there.
;; ============================================================================
SSD_SELF  equ 8
SSD_N     equ 16
SSD_FRAME equ 16            ; + 2 pushes = 32

DEF_FUNC structseq_dealloc, SSD_FRAME
    push rbx
    push r12
    mov [rbp - SSD_SELF], rdi

    ; The visible entries, then their array.
    mov r12, [rdi + PyTupleObject.ob_size]
    mov rbx, [rdi + PyTupleObject.ob_item]
    test rbx, rbx
    jz .ssd_tail
    xor ecx, ecx
.ssd_items:
    cmp rcx, r12
    jge .ssd_free_items
    push rcx
    push rcx
    mov rdi, [rbx + rcx*8]
    DECREF_V rdi, rsi
    pop rcx
    pop rcx
    inc rcx
    jmp .ssd_items
.ssd_free_items:
    mov rdi, rbx
    call ap_free

.ssd_tail:
    ; The named-only fields, which the tuple length says nothing about.
    mov rdi, [rbp - SSD_SELF]
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_basicsize]
    sub rcx, PyTupleObject_size
    shr rcx, 3                  ; how many tail slots
    lea rbx, [rdi + PyTupleObject_size]
    xor r12d, r12d
.ssd_tail_loop:
    cmp r12, rcx
    jge .ssd_type
    push rcx
    push rcx
    mov rdi, [rbx + r12*8]
    DECREF_V rdi, rsi
    pop rcx
    pop rcx
    inc r12
    jmp .ssd_tail_loop

.ssd_type:
    mov rdi, [rbp - SSD_SELF]
    mov rdi, [rdi + PyObject.ob_type]
    call obj_decref             ; the instance held one, as every instance does
    mov rdi, [rbp - SSD_SELF]
    call ap_free
    pop r12
    pop rbx
    leave
    ret
END_FUNC structseq_dealloc

;; ============================================================================
;; structseq_init_type(rdi = the type)
;;
;; The two slots a static type cannot spell out for itself: tp_base, and the
;; tp_iter that init_iter_types patches into tuple_type at startup.  Slot
;; reads do not walk the MRO, so `for x in st` needs the pointer copied here
;; rather than inherited.  bool_init does the first of these for the same
;; reason.  Must run after init_iter_types.
;; ============================================================================
DEF_FUNC structseq_init_type
    lea rax, [rel tuple_type]
    mov [rdi + PyTypeObject.tp_base], rax
    mov rcx, [rax + PyTypeObject.tp_iter]
    mov [rdi + PyTypeObject.tp_iter], rcx
    mov rcx, [rax + PyTypeObject.tp_hash]
    mov [rdi + PyTypeObject.tp_hash], rcx
    mov rcx, [rax + PyTypeObject.tp_richcompare]
    mov [rdi + PyTypeObject.tp_richcompare], rcx
    mov rcx, [rax + PyTypeObject.tp_as_sequence]
    mov [rdi + PyTypeObject.tp_as_sequence], rcx
    mov rcx, [rax + PyTypeObject.tp_as_mapping]
    mov [rdi + PyTypeObject.tp_as_mapping], rcx
    leave
    ret
END_FUNC structseq_init_type

;; ============================================================================
;; sys.version_info -- the first struct sequence, and the one that proves the
;; mechanism.  It was a bare 5-tuple, so .major was an AttributeError and
;; type(sys.version_info).__name__ was 'tuple'.
;; ============================================================================
section .rodata

vi_name:        db "sys.version_info", 0
vi_f_major:     db "major", 0
vi_f_minor:     db "minor", 0
vi_f_micro:     db "micro", 0
vi_f_release:   db "releaselevel", 0
vi_f_serial:    db "serial", 0

align 8
vi_fields:
    dq vi_f_major,   0
    dq vi_f_minor,   1
    dq vi_f_micro,   2
    dq vi_f_release, 3
    dq vi_f_serial,  4

align 8
vi_desc:
    dq 5                        ; n_in_sequence
    dq 5                        ; n_fields
    dq vi_fields

section .data
align 8
global version_info_type
version_info_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq vi_name                  ; tp_name -- the full dotted name, as CPython's
    dq PyTupleObject_size       ; tp_basicsize: no named-only tail
    dq structseq_dealloc        ; tp_dealloc (NOT tuple's: it pools by size)
    dq structseq_repr           ; tp_repr
    dq structseq_repr           ; tp_str
    dq 0                        ; tp_hash          } all four copied from
    dq 0                        ; tp_call          } tuple_type by
    dq structseq_getattr        ; tp_getattr       } structseq_init_type,
    dq 0                        ; tp_setattr       } which must run first
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
    dq TYPE_FLAG_TUPLE_SUBCLASS ; tp_flags -- no HAVE_GC: it owns no cycles
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset
    dq vi_desc                  ; STRUCTSEQ_DESC, one qword past the type

;; ============================================================================
;; sys.float_info, sys.int_info and sys.hash_info.  All three were absent --
;; sysmod.asm carried "Skip for now" where each should have been -- and the
;; numeric stdlib reads them.  Their values are filled in by sys_module_init.
;; ============================================================================

;; sys.float_info
section .rodata

float_info_name: db "sys.float_info", 0
float_info_f0: db "max", 0
float_info_f1: db "max_exp", 0
float_info_f2: db "max_10_exp", 0
float_info_f3: db "min", 0
float_info_f4: db "min_exp", 0
float_info_f5: db "min_10_exp", 0
float_info_f6: db "dig", 0
float_info_f7: db "mant_dig", 0
float_info_f8: db "epsilon", 0
float_info_f9: db "radix", 0
float_info_f10: db "rounds", 0

align 8
float_info_fields:
    dq float_info_f0, 0
    dq float_info_f1, 1
    dq float_info_f2, 2
    dq float_info_f3, 3
    dq float_info_f4, 4
    dq float_info_f5, 5
    dq float_info_f6, 6
    dq float_info_f7, 7
    dq float_info_f8, 8
    dq float_info_f9, 9
    dq float_info_f10, 10

align 8
float_info_desc:
    dq 11
    dq 11
    dq float_info_fields

section .data
align 8
global float_info_type
float_info_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq float_info_name          ; tp_name
    dq PyTupleObject_size       ; tp_basicsize: no named-only tail
    dq structseq_dealloc        ; tp_dealloc
    dq structseq_repr           ; tp_repr
    dq structseq_repr           ; tp_str
    dq 0                        ; tp_hash          } copied from tuple_type by
    dq 0                        ; tp_call          } structseq_init_type, which
    dq structseq_getattr        ; tp_getattr       } must run first
    dq 0                        ; tp_setattr
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
    dq TYPE_FLAG_TUPLE_SUBCLASS ; tp_flags -- no HAVE_GC
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset
    dq float_info_desc          ; STRUCTSEQ_DESC, one qword past the type

;; sys.int_info
section .rodata

int_info_name: db "sys.int_info", 0
int_info_f0: db "bits_per_digit", 0
int_info_f1: db "sizeof_digit", 0
int_info_f2: db "default_max_str_digits", 0
int_info_f3: db "str_digits_check_threshold", 0

align 8
int_info_fields:
    dq int_info_f0, 0
    dq int_info_f1, 1
    dq int_info_f2, 2
    dq int_info_f3, 3

align 8
int_info_desc:
    dq 4
    dq 4
    dq int_info_fields

section .data
align 8
global int_info_type
int_info_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq int_info_name            ; tp_name
    dq PyTupleObject_size       ; tp_basicsize: no named-only tail
    dq structseq_dealloc        ; tp_dealloc
    dq structseq_repr           ; tp_repr
    dq structseq_repr           ; tp_str
    dq 0                        ; tp_hash          } copied from tuple_type by
    dq 0                        ; tp_call          } structseq_init_type, which
    dq structseq_getattr        ; tp_getattr       } must run first
    dq 0                        ; tp_setattr
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
    dq TYPE_FLAG_TUPLE_SUBCLASS ; tp_flags -- no HAVE_GC
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset
    dq int_info_desc            ; STRUCTSEQ_DESC, one qword past the type

;; sys.hash_info
section .rodata

hash_info_name: db "sys.hash_info", 0
hash_info_f0: db "width", 0
hash_info_f1: db "modulus", 0
hash_info_f2: db "inf", 0
hash_info_f3: db "nan", 0
hash_info_f4: db "imag", 0
hash_info_f5: db "algorithm", 0
hash_info_f6: db "hash_bits", 0
hash_info_f7: db "seed_bits", 0
hash_info_f8: db "cutoff", 0

align 8
hash_info_fields:
    dq hash_info_f0, 0
    dq hash_info_f1, 1
    dq hash_info_f2, 2
    dq hash_info_f3, 3
    dq hash_info_f4, 4
    dq hash_info_f5, 5
    dq hash_info_f6, 6
    dq hash_info_f7, 7
    dq hash_info_f8, 8

align 8
hash_info_desc:
    dq 9
    dq 9
    dq hash_info_fields

section .data
align 8
global hash_info_type
hash_info_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq hash_info_name           ; tp_name
    dq PyTupleObject_size       ; tp_basicsize: no named-only tail
    dq structseq_dealloc        ; tp_dealloc
    dq structseq_repr           ; tp_repr
    dq structseq_repr           ; tp_str
    dq 0                        ; tp_hash          } copied from tuple_type by
    dq 0                        ; tp_call          } structseq_init_type, which
    dq structseq_getattr        ; tp_getattr       } must run first
    dq 0                        ; tp_setattr
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
    dq TYPE_FLAG_TUPLE_SUBCLASS ; tp_flags -- no HAVE_GC
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset
    dq hash_info_desc           ; STRUCTSEQ_DESC, one qword past the type

section .rodata
align 8
global float_info_v0
float_info_v0: dq 0x7FEFFFFFFFFFFFFF    ; 1.7976931348623157e+308
global float_info_v3
float_info_v3: dq 0x0010000000000000    ; 2.2250738585072014e-308
global float_info_v8
float_info_v8: dq 0x3CB0000000000000    ; 2.220446049250313e-16
global hash_info_v5
hash_info_v5: db "fnv", 0

;; ============================================================================
;; sys.flags.  _pyio reads sys.flags.utf8_mode and .dev_mode at module level,
;; so without it the module could not be imported at all.  The values are the
;; ones that describe this interpreter, not CPython's defaults copied over:
;; hash_randomization is 0 because str_hash is an unseeded FNV-1a, and
;; int_max_str_digits matches what sys.get_int_max_str_digits() reports.
;; ============================================================================

;; sys.flags
section .rodata

flags_name: db "sys.flags", 0
flags_f0: db "debug", 0
flags_f1: db "inspect", 0
flags_f2: db "interactive", 0
flags_f3: db "optimize", 0
flags_f4: db "dont_write_bytecode", 0
flags_f5: db "no_user_site", 0
flags_f6: db "no_site", 0
flags_f7: db "ignore_environment", 0
flags_f8: db "verbose", 0
flags_f9: db "bytes_warning", 0
flags_f10: db "quiet", 0
flags_f11: db "hash_randomization", 0
flags_f12: db "isolated", 0
flags_f13: db "dev_mode", 0
flags_f14: db "utf8_mode", 0
flags_f15: db "warn_default_encoding", 0
flags_f16: db "safe_path", 0
flags_f17: db "int_max_str_digits", 0

align 8
flags_fields:
    dq flags_f0, 0
    dq flags_f1, 1
    dq flags_f2, 2
    dq flags_f3, 3
    dq flags_f4, 4
    dq flags_f5, 5
    dq flags_f6, 6
    dq flags_f7, 7
    dq flags_f8, 8
    dq flags_f9, 9
    dq flags_f10, 10
    dq flags_f11, 11
    dq flags_f12, 12
    dq flags_f13, 13
    dq flags_f14, 14
    dq flags_f15, 15
    dq flags_f16, 16
    dq flags_f17, 17

align 8
flags_desc:
    dq 18
    dq 18
    dq flags_fields

section .data
align 8
global flags_type
flags_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq flags_name               ; tp_name
    dq PyTupleObject_size       ; tp_basicsize
    dq structseq_dealloc        ; tp_dealloc
    dq structseq_repr           ; tp_repr
    dq structseq_repr           ; tp_str
    dq 0                        ; tp_hash          } copied from tuple_type by
    dq 0                        ; tp_call          } structseq_init_type
    dq structseq_getattr        ; tp_getattr
    dq 0                        ; tp_setattr
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
    dq TYPE_FLAG_TUPLE_SUBCLASS ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset
    dq flags_desc               ; STRUCTSEQ_DESC
