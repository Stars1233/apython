; methods_str_pred.asm - str: the is* predicates, case mapping, justification
;
; Also count, index and rfind, which sit with them because the predicates and
; the searches share the same code-point walk.
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
extern ap_malloc
extern gc_alloc
extern gc_track
extern ap_free
extern ap_realloc
extern ap_memcpy
extern ap_memset
extern ap_memmove
extern ap_strcmp
extern ap_strlen
extern ap_strstr
extern ap_memcmp
extern obj_incref
extern obj_decref
extern obj_dealloc
extern obj_repr
extern obj_str
extern str_from_cstr_heap
extern str_new_heap
extern str_type
extern list_new
extern list_append
extern obj_as_index
extern list_type
extern tuple_new
extern tuple_type
extern dict_new
extern dict_get
extern obj_getattr_opt
extern obj_call_n
extern dict_set
extern dict_del
extern dict_type
extern none_singleton
extern bool_true
extern bool_false
extern int_from_i64
extern eval_exception_unwind
extern obj_richcompare_bool
extern int_to_i64
extern builtin_func_new
extern raise_exception
extern raise_key_error
extern fatal_error
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_IndexError_type
extern exc_KeyError_type
extern int_type
extern set_type
extern object_type
extern object_new_fn
extern staticmethod_type
extern obj_is_true
extern list_sorting_error
extern str_byte_to_cp

; Set entry layout constants (must match set.asm)
SET_ENTRY_HASH    equ 0
SET_ENTRY_KEY     equ 8
SET_ENTRY_SIZE    equ 16
extern set_add
extern set_contains
extern set_remove
extern set_new
extern set_tp_iter

; --- moved to a sibling file by the split ---
extern add_class_getitem
extern add_method_to_dict
extern add_method_to_dict_checked
extern add_new_staticmethod
extern bytes_method_count
extern bytes_method_endswith
extern bytes_method_find
extern bytes_method_hex
extern bytes_method_join
extern bytes_method_replace
extern bytes_method_split
extern bytes_method_startswith
extern container_dunder_new
extern dict_classmethod_fromkeys
extern dict_dunder_delitem
extern dict_dunder_getitem
extern dict_dunder_setitem
extern dict_method_clear
extern dict_method_copy
extern dict_method_get
extern dict_method_items
extern dict_method_keys
extern dict_method_pop
extern dict_method_popitem
extern dict_method_setdefault
extern dict_method_update
extern dict_method_values
extern du_keys_name
extern empty_str_cstr
extern float_classmethod_fromhex
extern float_method___abs__
extern float_method___float__
extern float_method___int__
extern float_method___trunc__
extern float_method_as_integer_ratio
extern float_method_conjugate
extern float_method_hex
extern float_method_is_integer
extern fm_name_equals
extern fm_resolve_field
extern fmtbuf_append
extern generic_method_contains
extern generic_method_delitem
extern generic_method_getitem
extern generic_method_hash
extern generic_method_setitem
extern int_classmethod_from_bytes
extern int_method___abs__
extern int_method___float__
extern int_method___index__
extern int_method___int__
extern int_method_bit_count
extern int_method_bit_length
extern int_method_conjugate
extern int_method_self_to_i64
extern int_method_to_bytes
extern list_dunder_contains
extern list_dunder_delitem
extern list_dunder_getitem
extern list_dunder_iadd
extern list_dunder_init
extern list_dunder_len
extern list_dunder_setitem
extern list_method_append
extern list_method_clear
extern list_method_copy
extern list_method_count
extern list_method_extend
extern list_method_index
extern list_method_insert
extern list_method_pop
extern list_method_remove
extern list_method_reverse
extern list_method_reversed
extern list_method_sort
extern methods_init
extern mn___abs__
extern mn___add__
extern mn___class_getitem__
extern mn___code__
extern mn___contains__
extern mn___delete__
extern mn___delitem__
extern mn___dir__
extern mn___doc__
extern mn___eq__
extern mn___float__
extern mn___format__
extern mn___ge__
extern mn___get__
extern mn___getitem__
extern mn___globals__
extern mn___gt__
extern mn___hash__
extern mn___iadd__
extern mn___index__
extern mn___init__
extern mn___init_subclass__
extern mn___int__
extern mn___iter__
extern mn___le__
extern mn___len__
extern mn___lt__
extern mn___mul__
extern mn___ne__
extern mn___new__
extern mn___reduce__
extern mn___reduce_ex__
extern mn___repr__
extern mn___reversed__
extern mn___rmul__
extern mn___set__
extern mn___setitem__
extern mn___sizeof__
extern mn___str__
extern mn___trunc__
extern mn_add
extern mn_append
extern mn_as_integer_ratio
extern mn_bit_count
extern mn_bit_length
extern mn_capitalize
extern mn_casefold
extern mn_center
extern mn_clear
extern mn_conjugate
extern mn_copy
extern mn_count
extern mn_decode
extern mn_difference
extern mn_discard
extern mn_encode
extern mn_endswith
extern mn_expandtabs
extern mn_extend
extern mn_find
extern mn_format
extern mn_format_map
extern mn_from_bytes
extern mn_fromhex
extern mn_fromkeys
extern mn_get
extern mn_hex
extern mn_index
extern mn_insert
extern mn_intersection
extern mn_is_integer
extern mn_isalnum
extern mn_isalpha
extern mn_isascii
extern mn_isdecimal
extern mn_isdigit
extern mn_isdisjoint
extern mn_isidentifier
extern mn_islower
extern mn_isnumeric
extern mn_isprintable
extern mn_isspace
extern mn_issubset
extern mn_issuperset
extern mn_istitle
extern mn_isupper
extern mn_items
extern mn_join
extern mn_keys
extern mn_ljust
extern mn_lower
extern mn_lstrip
extern mn_maketrans
extern mn_partition
extern mn_pop
extern mn_popitem
extern mn_remove
extern mn_removeprefix
extern mn_removesuffix
extern mn_replace
extern mn_reverse
extern mn_rfind
extern mn_rindex
extern mn_rjust
extern mn_rpartition
extern mn_rsplit
extern mn_rstrip
extern mn_setdefault
extern mn_sort
extern mn_split
extern mn_splitlines
extern mn_startswith
extern mn_strip
extern mn_swapcase
extern mn_symmetric_difference
extern mn_title
extern mn_to_bytes
extern mn_translate
extern mn_union
extern mn_update
extern mn_upper
extern mn_values
extern mn_zfill
extern object_method_dir
extern object_method_eq
extern object_method_format
extern object_method_hash
extern object_method_init
extern object_method_init_subclass
extern object_method_ne
extern object_method_notimpl
extern object_method_reduce
extern object_method_repr
extern object_method_sizeof
extern object_method_str
extern scalar_dunder_new
extern set_method_add
extern set_method_clear
extern set_method_copy
extern set_method_difference
extern set_method_discard
extern set_method_intersection
extern set_method_isdisjoint
extern set_method_issubset
extern set_method_issuperset
extern set_method_pop
extern set_method_remove
extern set_method_symmetric_difference
extern set_method_union
extern set_method_update
extern str_affix_dispatch
extern str_endswith_one
extern str_method_encode
extern str_method_endswith
extern str_method_expandtabs
extern str_method_find
extern str_method_format
extern str_method_format_map
extern str_method_istitle
extern str_method_join
extern str_method_lower
extern str_method_lstrip
extern str_method_partition
extern str_method_removeprefix
extern str_method_removesuffix
extern str_method_replace
extern str_method_rindex
extern str_method_rpartition
extern str_method_rsplit
extern str_method_rstrip
extern str_method_split
extern str_method_splitlines
extern str_method_startswith
extern str_method_strip
extern str_method_translate
extern str_method_upper
extern str_split_impl
extern str_startswith_one
extern str_staticmethod_maketrans
extern str_strip_impl
extern strip_char_matches
extern tuple_dunder_add
extern tuple_dunder_contains
extern tuple_dunder_getitem
extern tuple_dunder_len
extern tuple_dunder_mul
extern tuple_dunder_rmul
extern tuple_method_count
extern tuple_method_index

section .text

;; ============================================================================
;; str_method_count(args, nargs) -> SmallInt count of occurrences
;; args[0]=self, args[1]=sub
;; ============================================================================
DEF_FUNC str_method_count
    push rbx
    push r12
    push r13
    push r14

    ; Validate args[1] is a string
    mov rax, [rdi + 8]         ; args[1]
    V_TEST_PTR rax, rcx
    ja .count_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .count_type_error

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]     ; substr (now guaranteed heap str)
    xor r13d, r13d          ; r13 = count
    mov r14, [r12 + PyStrObject.ob_size]  ; sub length

    ; If sub is empty, return len+1
    test r14, r14
    jz .count_empty_sub

    ; Start scanning from self.data
    lea rdi, [rbx + PyStrObject.data]

.count_scan:
    lea rsi, [r12 + PyStrObject.data]
    push rdi
    call ap_strstr
    pop rdi                 ; restore (not needed, but stack balance)
    test rax, rax
    jz .count_done

    ; Found one occurrence
    inc r13
    ; Advance past this match
    lea rdi, [rax + r14]    ; move past the match
    jmp .count_scan

.count_empty_sub:
    ; Empty substring: count = len(self) + 1
    mov r13, [rbx + PyStrObject.ob_size]
    inc r13

.count_done:
    mov rdi, r13
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.count_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "must be str, not other type"
    call raise_exception
END_FUNC str_method_count

;; ============================================================================
;; str_method_index(args, nargs) -> SmallInt index (raises ValueError if not found)
;; args[0]=self, args[1]=substr
;; ============================================================================
DEF_FUNC str_method_index
    push rbx
    push r12

    ; Validate args[1] is a string
    mov rax, [rdi + 8]         ; args[1]
    V_TEST_PTR rax, rcx
    ja .idx_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .idx_type_error

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]     ; substr

    ; Use ap_strstr to find substring
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strstr

    test rax, rax
    jz .str_index_not_found

    ; Byte offset in, code point index out.
    lea rcx, [rbx + PyStrObject.data]
    sub rax, rcx
    mov rdi, rbx
    mov rsi, rax
    call str_byte_to_cp
    mov rdi, rax
    call int_from_i64

    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.str_index_not_found:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "substring not found"
    call raise_exception

.idx_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "must be str, not other type"
    call raise_exception
END_FUNC str_method_index

;; ============================================================================
;; str_method_rfind(args, nargs) -> SmallInt index or -1
;; args[0]=self, args[1]=substr
;; Find rightmost occurrence of substr in self.
;; ============================================================================
DEF_FUNC str_method_rfind
    push rbx
    push r12
    push r13
    push r14

    ; Validate args[1] is a string
    mov rax, [rdi + 8]         ; args[1]
    V_TEST_PTR rax, rcx
    ja .rfind_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .rfind_type_error

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]     ; substr (now guaranteed heap str)
    mov r13, [rbx + PyStrObject.ob_size]   ; self length
    mov r14, [r12 + PyStrObject.ob_size]   ; sub length

    ; If sub_len > self_len, return -1
    cmp r14, r13
    jg .rfind_not_found

    ; If sub_len == 0, return self_len
    test r14, r14
    jz .rfind_empty_sub

    ; Walk backward from (self_len - sub_len) down to 0
    mov rcx, r13
    sub rcx, r14            ; rcx = last possible start position

.rfind_loop:
    cmp rcx, 0
    jl .rfind_not_found

    ; Compare sub with self[rcx..rcx+sub_len]
    push rcx
    lea rdi, [rbx + PyStrObject.data]
    add rdi, rcx
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, r14
    call ap_memcmp
    pop rcx

    test eax, eax
    jz .rfind_found

    dec rcx
    jmp .rfind_loop

.rfind_found:
    mov rdi, rbx
    mov rsi, rcx
    call str_byte_to_cp
    mov rdi, rax
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rfind_empty_sub:
    mov rdi, [rbx + PyStrObject.ob_length]
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rfind_not_found:
    mov rdi, -1
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rfind_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "must be str, not other type"
    call raise_exception
END_FUNC str_method_rfind

;; ============================================================================
;; str_method_isdigit(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all chars are digits and len>0, else False
;; ============================================================================
DEF_FUNC str_method_isdigit
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isdigit_false

    xor edx, edx            ; index
.isdigit_loop:
    cmp rdx, rcx
    jge .isdigit_true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, '0'
    jb .isdigit_false
    cmp sil, '9'
    ja .isdigit_false
    inc rdx
    jmp .isdigit_loop

.isdigit_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isdigit_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_isdigit


;; ============================================================================
;; str_method_isidentifier / isprintable / isascii / isdecimal / isnumeric
;;
;; ASCII-only, like the rest of the str predicates here: a str is still a byte
;; string, so a non-ASCII byte can only be reported honestly as "not one of
;; these".  isidentifier is what functools, enum, dataclasses and textwrap all
;; reach for; the other four keep the family complete.
;; ============================================================================
DEF_FUNC str_method_isidentifier
    mov rax, [rdi]
    mov rcx, [rax + PyStrObject.ob_size]
    test rcx, rcx
    jz .false
    ; First character: a letter or underscore.
    movzx esi, byte [rax + PyStrObject.data]
    cmp sil, '_'
    je .rest
    call .is_alpha_sil
    jz .false
.rest:
    mov edx, 1
.loop:
    cmp rdx, rcx
    jge .true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, '_'
    je .next
    cmp sil, '0'
    jb .not_alnum
    cmp sil, '9'
    jbe .next
.not_alnum:
    push rax
    push rcx
    push rdx
    call .is_alpha_sil
    pop rdx
    pop rcx
    pop rax
    jz .false
.next:
    inc rdx
    jmp .loop

; Sets ZF when sil is not an ASCII letter.
.is_alpha_sil:
    cmp sil, 'A'
    jb .not_letter
    cmp sil, 'Z'
    jbe .letter
    cmp sil, 'a'
    jb .not_letter
    cmp sil, 'z'
    ja .not_letter
.letter:
    test esp, esp               ; clears ZF (rsp is never zero)
    ret
.not_letter:
    xor r8d, r8d                ; sets ZF
    ret

.true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC str_method_isidentifier

;; Every byte printable and not a space-only string; the empty string is True.
DEF_FUNC str_method_isprintable
    mov rax, [rdi]
    mov rcx, [rax + PyStrObject.ob_size]
    xor edx, edx
.loop:
    cmp rdx, rcx
    jge .true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 0x20
    jb .false
    cmp sil, 0x7e
    ja .false
    inc rdx
    jmp .loop
.true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC str_method_isprintable

DEF_FUNC str_method_isascii
    mov rax, [rdi]
    mov rcx, [rax + PyStrObject.ob_size]
    xor edx, edx
.loop:
    cmp rdx, rcx
    jge .true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 0x7f
    ja .false
    inc rdx
    jmp .loop
.true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC str_method_isascii

;; isdecimal and isnumeric agree with isdigit over ASCII, which is all a byte
;; string can represent.
DEF_FUNC str_method_isdecimal
    mov rax, [rdi]
    mov rcx, [rax + PyStrObject.ob_size]
    test rcx, rcx
    jz .false
    xor edx, edx
.loop:
    cmp rdx, rcx
    jge .true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, '0'
    jb .false
    cmp sil, '9'
    ja .false
    inc rdx
    jmp .loop
.true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC str_method_isdecimal

;; ============================================================================
;; str_method_isalpha(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all chars are alphabetic and len>0, else False
;; ============================================================================
DEF_FUNC str_method_isalpha
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isalpha_false

    xor edx, edx            ; index
.isalpha_loop:
    cmp rdx, rcx
    jge .isalpha_true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 'A'
    jb .isalpha_false
    cmp sil, 'Z'
    jbe .isalpha_next        ; A-Z is alpha
    cmp sil, 'a'
    jb .isalpha_false
    cmp sil, 'z'
    ja .isalpha_false
.isalpha_next:
    inc rdx
    jmp .isalpha_loop

.isalpha_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isalpha_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_isalpha

;; ============================================================================
;; str_method_isalnum(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all chars are alphanumeric (0-9, A-Z, a-z) and len>0
;; ============================================================================
DEF_FUNC str_method_isalnum
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isalnum_false

    xor edx, edx            ; index
.isalnum_loop:
    cmp rdx, rcx
    jge .isalnum_true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, '0'
    jb .isalnum_false
    cmp sil, '9'
    jbe .isalnum_next        ; 0-9
    cmp sil, 'A'
    jb .isalnum_false
    cmp sil, 'Z'
    jbe .isalnum_next        ; A-Z
    cmp sil, 'a'
    jb .isalnum_false
    cmp sil, 'z'
    ja .isalnum_false
.isalnum_next:
    inc rdx
    jmp .isalnum_loop

.isalnum_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isalnum_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_isalnum

;; ============================================================================
;; str_method_isspace(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all chars are whitespace (space/tab/newline/CR/VT/FF) and len>0
;; ============================================================================
DEF_FUNC str_method_isspace
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isspace_false

    xor edx, edx            ; index
.isspace_loop:
    cmp rdx, rcx
    jge .isspace_true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 0x20           ; space
    je .isspace_next
    cmp sil, 0x09           ; tab
    jb .isspace_false
    cmp sil, 0x0D           ; tab(09), newline(0A), VT(0B), FF(0C), CR(0D)
    ja .isspace_false
.isspace_next:
    inc rdx
    jmp .isspace_loop

.isspace_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isspace_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_isspace

;; ============================================================================
;; str_method_isupper(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all cased chars are uppercase, and there is at least one cased char
;; ============================================================================
DEF_FUNC str_method_isupper
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isupper_false

    xor edx, edx            ; index
    xor r8d, r8d            ; has_cased flag
.isupper_loop:
    cmp rdx, rcx
    jge .isupper_check_cased
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 'A'
    jb .isupper_next         ; non-alpha, skip
    cmp sil, 'Z'
    jbe .isupper_found_upper ; A-Z: uppercase, good
    cmp sil, 'a'
    jb .isupper_next         ; non-alpha, skip
    cmp sil, 'z'
    jbe .isupper_false       ; a-z: lowercase, fail
.isupper_next:
    inc rdx
    jmp .isupper_loop
.isupper_found_upper:
    mov r8d, 1               ; found at least one cased char
    inc rdx
    jmp .isupper_loop
.isupper_check_cased:
    test r8d, r8d
    jz .isupper_false        ; no cased chars found

    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isupper_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_isupper

;; ============================================================================
;; str_method_islower(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all cased chars are lowercase, and there is at least one cased char
;; ============================================================================
DEF_FUNC str_method_islower
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .islower_false

    xor edx, edx            ; index
    xor r8d, r8d            ; has_cased flag
.islower_loop:
    cmp rdx, rcx
    jge .islower_check_cased
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 'a'
    jb .islower_check_upper
    cmp sil, 'z'
    jbe .islower_found_lower ; a-z: lowercase, good
    jmp .islower_next        ; > 'z', non-alpha, skip
.islower_check_upper:
    cmp sil, 'A'
    jb .islower_next         ; non-alpha, skip
    cmp sil, 'Z'
    jbe .islower_false       ; A-Z: uppercase, fail
.islower_next:
    inc rdx
    jmp .islower_loop
.islower_found_lower:
    mov r8d, 1               ; found at least one cased char
    inc rdx
    jmp .islower_loop
.islower_check_cased:
    test r8d, r8d
    jz .islower_false        ; no cased chars found

    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.islower_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_islower

;; ============================================================================
;; str_method_title(args, nargs) -> new titlecased string
;; Uppercase after non-alpha, lowercase after alpha
;; ============================================================================
DEF_FUNC str_method_title
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rbx + PyStrObject.ob_size]

    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new_heap
    mov r13, rax

    xor ecx, ecx            ; i = 0
    mov r8d, 1               ; prev_is_sep = true (start of string)
.title_loop:
    cmp rcx, r12
    jge .title_done
    movzx eax, byte [r13 + PyStrObject.data + rcx]
    ; Check if alpha
    cmp al, 'A'
    jb .title_not_alpha
    cmp al, 'Z'
    jbe .title_is_upper
    cmp al, 'a'
    jb .title_not_alpha
    cmp al, 'z'
    ja .title_not_alpha
    ; lowercase char
    test r8d, r8d
    jz .title_to_lower       ; prev was alpha → stay lower
    ; prev was non-alpha → capitalize
    sub al, 32
    mov [r13 + PyStrObject.data + rcx], al
    xor r8d, r8d             ; prev_is_sep = false
    jmp .title_next
.title_is_upper:
    test r8d, r8d
    jnz .title_keep_upper     ; prev was non-alpha → keep upper
    ; prev was alpha → lowercase it
    add al, 32
    mov [r13 + PyStrObject.data + rcx], al
    xor r8d, r8d
    jmp .title_next
.title_keep_upper:
    xor r8d, r8d
    jmp .title_next
.title_to_lower:
    ; already lowercase, prev was alpha → keep as-is
    xor r8d, r8d
    jmp .title_next
.title_not_alpha:
    mov r8d, 1               ; prev_is_sep = true
.title_next:
    inc rcx
    jmp .title_loop
.title_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_title

;; ============================================================================
;; str_method_capitalize(args, nargs) -> new string
;; First char upper, rest lower
;; ============================================================================
DEF_FUNC str_method_capitalize
    push rbx
    push r12
    push r13

    mov rbx, [rdi]
    mov r12, [rbx + PyStrObject.ob_size]

    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new_heap
    mov r13, rax

    ; First char → upper
    test r12, r12
    jz .cap_done
    movzx eax, byte [r13 + PyStrObject.data]
    cmp al, 'a'
    jb .cap_rest
    cmp al, 'z'
    ja .cap_rest
    sub al, 32
    mov [r13 + PyStrObject.data], al

.cap_rest:
    ; Remaining chars → lower
    mov rcx, 1
.cap_loop:
    cmp rcx, r12
    jge .cap_done
    movzx eax, byte [r13 + PyStrObject.data + rcx]
    cmp al, 'A'
    jb .cap_next
    cmp al, 'Z'
    ja .cap_next
    add al, 32
    mov [r13 + PyStrObject.data + rcx], al
.cap_next:
    inc rcx
    jmp .cap_loop
.cap_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_capitalize

;; ============================================================================
;; str_method_swapcase(args, nargs) -> new string
;; Upper→lower, lower→upper
;; ============================================================================
DEF_FUNC str_method_swapcase
    push rbx
    push r12
    push r13

    mov rbx, [rdi]
    mov r12, [rbx + PyStrObject.ob_size]

    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new_heap
    mov r13, rax

    xor ecx, ecx
.swap_loop:
    cmp rcx, r12
    jge .swap_done
    movzx eax, byte [r13 + PyStrObject.data + rcx]
    cmp al, 'A'
    jb .swap_next
    cmp al, 'Z'
    jbe .swap_to_lower
    cmp al, 'a'
    jb .swap_next
    cmp al, 'z'
    ja .swap_next
    ; lowercase → upper
    sub al, 32
    mov [r13 + PyStrObject.data + rcx], al
    jmp .swap_next
.swap_to_lower:
    add al, 32
    mov [r13 + PyStrObject.data + rcx], al
.swap_next:
    inc rcx
    jmp .swap_loop
.swap_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_swapcase

;; ============================================================================
;; str_method_casefold(args, nargs) -> new string
;; ASCII casefold = lowercase (full Unicode casefold deferred)
;; ============================================================================
DEF_FUNC str_method_casefold
    push rbx
    push r12
    push r13

    mov rbx, [rdi]
    mov r12, [rbx + PyStrObject.ob_size]

    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new_heap
    mov r13, rax

    xor ecx, ecx
.cf_loop:
    cmp rcx, r12
    jge .cf_done
    movzx eax, byte [r13 + PyStrObject.data + rcx]
    cmp al, 'A'
    jb .cf_next
    cmp al, 'Z'
    ja .cf_next
    add al, 32
    mov [r13 + PyStrObject.data + rcx], al
.cf_next:
    inc rcx
    jmp .cf_loop
.cf_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_casefold

;; ============================================================================
;; str_method_center(args, nargs) -> new centered string
;; args[0]=self, args[1]=width, args[2]=fillchar (optional, default ' ')
;; ============================================================================
PA_SELF   equ 8
PA_LEN    equ 16            ; length in bytes, for the copies
PA_ARGS   equ 24
PA_NARGS  equ 32
PA_CPLEN  equ 40            ; length in code points, which is what a width means
PA_FRAME  equ 48
DEF_FUNC str_method_center, PA_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - PA_ARGS], rdi
    mov [rbp - PA_NARGS], rsi
    mov rbx, [rdi]                      ; self
    mov r12, [rbx + PyStrObject.ob_size]; self_len
    mov [rbp - PA_SELF], rbx
    mov [rbp - PA_LEN], r12
    mov rax, [rbx + PyStrObject.ob_length]
    mov [rbp - PA_CPLEN], rax

    ; Get width
    mov rdi, [rbp - PA_ARGS]
    mov rax, rdi
    mov rdi, [rax + 8]                 ; args[1] payload
    V_UNPACK rdi, rdx       ; args[1]
    call int_to_i64
    mov r13, rax                         ; r13 = width

    ; Get fillchar (default ' ')
    mov ecx, ' '
    cmp qword [rbp - PA_NARGS], 3
    jl .center_have_fill
    mov rax, [rbp - PA_ARGS]
    mov rdx, [rax + 16]                 ; args[2] payload (char str)
    movzx ecx, byte [rdx + PyStrObject.data]
.center_have_fill:
    ; A width counts characters, so it is compared against the code point
    ; length; what has to be allocated is that many characters' worth of
    ; bytes -- the padding, which is ASCII, plus whatever the string occupies.
    cmp r13, [rbp - PA_CPLEN]
    jle .center_return_self
    sub r13, [rbp - PA_CPLEN]
    add r13, r12

    ; Allocate new string of size width
    mov rdi, r13
    push rcx                             ; save fillchar
    call ap_malloc
    pop rcx
    mov rbx, rax                         ; rbx = new string buffer (raw)
    ; Fill entire buffer with fillchar
    push rcx
    mov rdi, rbx
    movzx esi, cl
    mov rdx, r13
    call ap_memset
    pop rcx

    ; Now create proper str object: str_new_heap(data, len)
    mov rdi, rbx
    mov rsi, r13
    call str_new_heap
    push rax                             ; save new str

    ; Free temp buffer
    mov rdi, rbx
    call ap_free
    pop r13                              ; r13 = new str

    ; Copy self data into center position
    mov rbx, [rbp - PA_SELF]
    mov r12, [rbp - PA_LEN]
    mov rax, [rbp - PA_LEN]
    mov rcx, [r13 + PyStrObject.ob_size]
    sub rcx, rax                         ; pad = width - len
    shr rcx, 1                           ; left_pad = pad / 2
    lea rdi, [r13 + PyStrObject.data + rcx]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy

    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.center_return_self:
    ; Return copy of self
    mov rbx, [rbp - PA_SELF]
    mov r12, [rbp - PA_LEN]
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new_heap
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_center

;; ============================================================================
;; str_method_ljust(args, nargs) -> left-justified string
;; args[0]=self, args[1]=width, args[2]=fillchar (optional, default ' ')
;; ============================================================================
DEF_FUNC str_method_ljust, PA_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - PA_ARGS], rdi
    mov [rbp - PA_NARGS], rsi
    mov rbx, [rdi]
    mov r12, [rbx + PyStrObject.ob_size]
    mov [rbp - PA_SELF], rbx
    mov [rbp - PA_LEN], r12
    mov rax, [rbx + PyStrObject.ob_length]
    mov [rbp - PA_CPLEN], rax

    ; Get width
    mov rax, [rbp - PA_ARGS]
    mov rdi, [rax + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call int_to_i64
    mov r13, rax

    ; Get fillchar
    mov ecx, ' '
    cmp qword [rbp - PA_NARGS], 3
    jl .ljust_have_fill
    mov rax, [rbp - PA_ARGS]
    mov rdx, [rax + 16]
    V_UNPACK rdx, rax       ; args[2]
    test rax, rax
    js .ljust_fill_ss
    movzx ecx, byte [rdx + PyStrObject.data]
    jmp .ljust_have_fill
.ljust_fill_ss:
    movzx ecx, dl
.ljust_have_fill:
    cmp r13, [rbp - PA_CPLEN]
    jle .ljust_return_self
    sub r13, [rbp - PA_CPLEN]
    add r13, r12

    ; Allocate, fill, copy self at start
    mov rdi, r13
    push rcx
    call ap_malloc
    pop rcx
    mov rbx, rax
    mov rdi, rbx
    movzx esi, cl
    mov rdx, r13
    call ap_memset
    mov rdi, rbx
    mov rsi, r13
    call str_new_heap
    push rax
    mov rdi, rbx
    call ap_free
    pop r13

    ; Copy self at position 0
    mov rbx, [rbp - PA_SELF]
    mov r12, [rbp - PA_LEN]
    lea rdi, [r13 + PyStrObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy

    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.ljust_return_self:
    mov rbx, [rbp - PA_SELF]
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, [rbp - PA_LEN]
    call str_new_heap
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_ljust

;; ============================================================================
;; str_method_rjust(args, nargs) -> right-justified string
;; ============================================================================
DEF_FUNC str_method_rjust, PA_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - PA_ARGS], rdi
    mov [rbp - PA_NARGS], rsi
    mov rbx, [rdi]
    mov r12, [rbx + PyStrObject.ob_size]
    mov [rbp - PA_SELF], rbx
    mov [rbp - PA_LEN], r12
    mov rax, [rbx + PyStrObject.ob_length]
    mov [rbp - PA_CPLEN], rax

    mov rax, [rbp - PA_ARGS]
    mov rdi, [rax + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call int_to_i64
    mov r13, rax

    mov ecx, ' '
    cmp qword [rbp - PA_NARGS], 3
    jl .rjust_have_fill
    mov rax, [rbp - PA_ARGS]
    mov rdx, [rax + 16]
    V_UNPACK rdx, rax       ; args[2]
    test rax, rax
    js .rjust_fill_ss
    movzx ecx, byte [rdx + PyStrObject.data]
    jmp .rjust_have_fill
.rjust_fill_ss:
    movzx ecx, dl
.rjust_have_fill:
    cmp r13, [rbp - PA_CPLEN]
    jle .rjust_return_self
    sub r13, [rbp - PA_CPLEN]
    add r13, r12

    mov rdi, r13
    push rcx
    call ap_malloc
    pop rcx
    mov rbx, rax
    mov rdi, rbx
    movzx esi, cl
    mov rdx, r13
    call ap_memset
    mov rdi, rbx
    mov rsi, r13
    call str_new_heap
    push rax
    mov rdi, rbx
    call ap_free
    pop r13

    ; Copy self at end (offset = width - len)
    mov rbx, [rbp - PA_SELF]
    mov r12, [rbp - PA_LEN]
    mov rcx, [r13 + PyStrObject.ob_size]
    sub rcx, r12
    lea rdi, [r13 + PyStrObject.data + rcx]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy

    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rjust_return_self:
    mov rbx, [rbp - PA_SELF]
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, [rbp - PA_LEN]
    call str_new_heap
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_rjust

;; ============================================================================
;; str_method_zfill(args, nargs) -> zero-filled string
;; args[0]=self, args[1]=width
;; ============================================================================
DEF_FUNC str_method_zfill, PA_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - PA_ARGS], rdi
    mov rbx, [rdi]
    mov r12, [rbx + PyStrObject.ob_size]
    mov [rbp - PA_SELF], rbx
    mov [rbp - PA_LEN], r12
    mov rax, [rbx + PyStrObject.ob_length]
    mov [rbp - PA_CPLEN], rax

    mov rax, [rbp - PA_ARGS]
    mov rdi, [rax + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call int_to_i64
    mov r13, rax                         ; width

    cmp r13, [rbp - PA_CPLEN]
    jle .zfill_return_self
    sub r13, [rbp - PA_CPLEN]
    add r13, r12

    ; Allocate filled with '0'
    mov rdi, r13
    call ap_malloc
    mov rbx, rax
    mov rdi, rbx
    mov esi, '0'
    mov rdx, r13
    call ap_memset
    mov rdi, rbx
    mov rsi, r13
    call str_new_heap
    push rax
    mov rdi, rbx
    call ap_free
    pop r13

    ; Copy self at end
    mov rbx, [rbp - PA_SELF]
    mov r12, [rbp - PA_LEN]
    mov rcx, [r13 + PyStrObject.ob_size]
    sub rcx, r12
    ; Check for sign prefix: '+' or '-' at position 0 of self
    test r12, r12
    jz .zfill_no_sign
    movzx eax, byte [rbx + PyStrObject.data]
    cmp al, '-'
    je .zfill_sign
    cmp al, '+'
    je .zfill_sign
.zfill_no_sign:
    lea rdi, [r13 + PyStrObject.data + rcx]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy
    jmp .zfill_done
.zfill_sign:
    ; Move sign to position 0, copy digits (skip sign) after zeros
    mov [r13 + PyStrObject.data], al
    lea rdi, [r13 + PyStrObject.data + rcx + 1]  ; after padding + sign
    lea rsi, [rbx + PyStrObject.data + 1]          ; skip sign in source
    mov rdx, r12
    dec rdx                                         ; len - 1
    call ap_memcpy
.zfill_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.zfill_return_self:
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new_heap
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_zfill
