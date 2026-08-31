; methods/bytes.asm - bytes and bytearray methods
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
extern tuple_type_call

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
extern str_method_capitalize
extern str_method_casefold
extern str_method_center
extern str_method_count
extern str_method_encode
extern str_method_endswith
extern str_method_expandtabs
extern str_method_find
extern str_method_format
extern str_method_format_map
extern str_method_index
extern str_method_isalnum
extern str_method_isalpha
extern str_method_isascii
extern str_method_isdecimal
extern str_method_isdigit
extern str_method_isidentifier
extern str_method_islower
extern str_method_isprintable
extern str_method_isspace
extern str_method_istitle
extern str_method_isupper
extern str_method_join
extern str_method_ljust
extern str_method_lower
extern str_method_lstrip
extern str_method_partition
extern str_method_removeprefix
extern str_method_removesuffix
extern str_method_replace
extern str_method_rfind
extern str_method_rindex
extern str_method_rjust
extern str_method_rpartition
extern str_method_rsplit
extern str_method_rstrip
extern str_method_split
extern str_method_splitlines
extern str_method_startswith
extern str_method_strip
extern str_method_swapcase
extern str_method_title
extern str_method_translate
extern str_method_upper
extern str_method_zfill
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

;; ############################################################################
;;                       BYTES METHODS
;; ############################################################################

;; ============================================================================
;; bytes_method_hex(args, nargs) -> str
;; Converts bytes to hex string like b'\xab\xcd'.hex() -> 'abcd'
;; ============================================================================
extern bytes_type
BH_SELF   equ 8
BH_BUF    equ 16
BH_HEXLEN equ 24
BH_FRAME  equ 32

DEF_FUNC bytes_method_hex, BH_FRAME
    mov rax, [rdi]              ; self = bytes obj ptr
    mov [rbp - BH_SELF], rax

    ; Get length
    mov rcx, [rax + PyBytesObject.ob_size]
    test rcx, rcx
    jz .bh_empty

    ; Allocate temp buffer for hex chars: 2 chars per byte
    lea rdi, [rcx * 2]
    mov [rbp - BH_HEXLEN], rdi
    call ap_malloc
    mov [rbp - BH_BUF], rax

    ; Fill hex chars into temp buffer
    mov rdx, [rbp - BH_SELF]
    mov rdi, rax                ; dest = temp buf
    lea rsi, [rdx + PyBytesObject.data]
    mov rcx, [rdx + PyBytesObject.ob_size]
    xor r8d, r8d                ; byte index

.bh_loop:
    cmp r8, rcx
    jge .bh_done
    movzx eax, byte [rsi + r8]

    ; High nibble
    mov r9d, eax
    shr r9d, 4
    cmp r9d, 10
    jb .bh_hi_digit
    add r9d, ('a' - 10)
    jmp .bh_hi_store
.bh_hi_digit:
    add r9d, '0'
.bh_hi_store:
    mov [rdi], r9b
    inc rdi

    ; Low nibble
    and eax, 0x0F
    cmp eax, 10
    jb .bh_lo_digit
    add eax, ('a' - 10)
    jmp .bh_lo_store
.bh_lo_digit:
    add eax, '0'
.bh_lo_store:
    mov [rdi], al
    inc rdi

    inc r8
    jmp .bh_loop

.bh_done:
    ; Create string from temp buffer
    mov rdi, [rbp - BH_BUF]
    mov rsi, [rbp - BH_HEXLEN]
    call str_new_heap
    push rax                    ; save result

    ; Free temp buffer
    mov rdi, [rbp - BH_BUF]
    call ap_free

    pop rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bh_empty:
    ; Return empty string
    lea rdi, [rel empty_str_cstr]
    xor esi, esi                ; length = 0
    call str_new_heap
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC bytes_method_hex

;; ============================================================================
;; bytes_method_startswith(args, nargs) -> Bool
;; args[0]=self (bytes), args[1]=prefix (bytes)
;; ============================================================================
DEF_FUNC bytes_method_startswith
    cmp rsi, 2
    jne .bsw_error

    mov rax, [rdi]              ; self
    mov rcx, [rdi + 8]         ; prefix

    ; Get lengths
    mov r8, [rax + PyBytesObject.ob_size]   ; self len
    mov r9, [rcx + PyBytesObject.ob_size]   ; prefix len

    ; If prefix longer than self: False
    cmp r9, r8
    ja .bsw_false

    ; Compare first r9 bytes
    lea rdi, [rax + PyBytesObject.data]
    lea rsi, [rcx + PyBytesObject.data]
    mov rdx, r9
    test rdx, rdx
    jz .bsw_true                ; empty prefix always matches
    call ap_memcmp
    test eax, eax
    jnz .bsw_false

.bsw_true:
    mov eax, 1
    RET_BOOL_RAX
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bsw_false:
    xor eax, eax
    RET_BOOL_RAX
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bsw_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "startswith() takes exactly one argument"
    call raise_exception
END_FUNC bytes_method_startswith

;; ============================================================================
;; bytes_method_endswith(args, nargs) -> Bool
;; args[0]=self (bytes), args[1]=suffix (bytes)
;; ============================================================================
DEF_FUNC bytes_method_endswith
    cmp rsi, 2
    jne .bew_error

    mov rax, [rdi]              ; self
    mov rcx, [rdi + 8]         ; suffix

    ; Get lengths
    mov r8, [rax + PyBytesObject.ob_size]   ; self len
    mov r9, [rcx + PyBytesObject.ob_size]   ; suffix len

    ; If suffix longer than self: False
    cmp r9, r8
    ja .bew_false

    ; Compare last r9 bytes
    mov rdx, r8
    sub rdx, r9                             ; offset = self_len - suffix_len
    lea rdi, [rax + PyBytesObject.data + rdx]
    lea rsi, [rcx + PyBytesObject.data]
    mov rdx, r9
    test rdx, rdx
    jz .bew_true                ; empty suffix always matches
    call ap_memcmp
    test eax, eax
    jnz .bew_false

.bew_true:
    mov eax, 1
    RET_BOOL_RAX
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bew_false:
    xor eax, eax
    RET_BOOL_RAX
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bew_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "endswith() takes exactly one argument"
    call raise_exception
END_FUNC bytes_method_endswith

;; ============================================================================
;; bytes_method_count(args, nargs) -> SmallInt
;; args[0]=self (bytes), args[1]=sub (bytes)
;; Count non-overlapping occurrences of sub in self.
;; ============================================================================
BC_SELF   equ 8
BC_SUB    equ 16
BC_FRAME  equ 24

DEF_FUNC bytes_method_count, BC_FRAME
    cmp rsi, 2
    jne .bc_error

    mov rax, [rdi]              ; self
    mov rcx, [rdi + 8]         ; sub
    mov [rbp - BC_SELF], rax
    mov [rbp - BC_SUB], rcx

    mov r8, [rax + PyBytesObject.ob_size]   ; self_len
    mov r9, [rcx + PyBytesObject.ob_size]   ; sub_len

    ; If sub_len == 0: count = self_len + 1
    test r9, r9
    jz .bc_empty_sub

    ; If sub_len > self_len: count = 0
    cmp r9, r8
    ja .bc_zero

    ; Scan
    xor r10d, r10d              ; count = 0
    xor r11d, r11d              ; offset = 0

.bc_loop:
    mov rax, r8
    sub rax, r11                ; remaining = self_len - offset
    cmp rax, r9
    jb .bc_result               ; not enough bytes left

    mov rdi, [rbp - BC_SELF]
    lea rdi, [rdi + PyBytesObject.data + r11]
    mov rsi, [rbp - BC_SUB]
    lea rsi, [rsi + PyBytesObject.data]
    mov rdx, r9
    push r8
    push r9
    push r10
    push r11
    call ap_memcmp
    pop r11
    pop r10
    pop r9
    pop r8
    test eax, eax
    jnz .bc_nomatch

    ; Match found
    inc r10
    add r11, r9                 ; skip sub_len (non-overlapping)
    jmp .bc_loop

.bc_nomatch:
    inc r11
    jmp .bc_loop

.bc_result:
    mov rax, r10
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bc_empty_sub:
    lea rax, [r8 + 1]
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bc_zero:
    xor eax, eax
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "count() takes exactly one argument"
    call raise_exception
END_FUNC bytes_method_count

;; ============================================================================
;; bytes_method_find(args, nargs) -> SmallInt
;; args[0]=self (bytes), args[1]=sub (bytes)
;; Returns index of first occurrence, or -1 if not found.
;; ============================================================================
BF_SELF   equ 8
BF_SUB    equ 16
BF_FRAME  equ 24

DEF_FUNC bytes_method_find, BF_FRAME
    cmp rsi, 2
    jne .bf_error

    mov rax, [rdi]              ; self
    mov rcx, [rdi + 8]         ; sub
    mov [rbp - BF_SELF], rax
    mov [rbp - BF_SUB], rcx

    mov r8, [rax + PyBytesObject.ob_size]   ; self_len
    mov r9, [rcx + PyBytesObject.ob_size]   ; sub_len

    ; If sub_len == 0: return 0
    test r9, r9
    jz .bf_found_zero

    ; If sub_len > self_len: return -1
    cmp r9, r8
    ja .bf_not_found

    ; Scan
    xor r11d, r11d              ; offset = 0

.bf_loop:
    mov rax, r8
    sub rax, r11                ; remaining
    cmp rax, r9
    jb .bf_not_found

    mov rdi, [rbp - BF_SELF]
    lea rdi, [rdi + PyBytesObject.data + r11]
    mov rsi, [rbp - BF_SUB]
    lea rsi, [rsi + PyBytesObject.data]
    mov rdx, r9
    push r8
    push r9
    push r11
    call ap_memcmp
    pop r11
    pop r9
    pop r8
    test eax, eax
    jz .bf_found

    inc r11
    jmp .bf_loop

.bf_found:
    mov rax, r11
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bf_found_zero:
    xor eax, eax
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bf_not_found:
    mov rax, -1
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bf_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "find() takes exactly one argument"
    call raise_exception
END_FUNC bytes_method_find

;; ============================================================================
;; bytes_method_replace(args, nargs) -> new bytes
;; args[0]=self (bytes), args[1]=old (bytes), args[2]=new (bytes)
;; Scan self for old subsequence, build new PyBytesObject with replacements.
;; ============================================================================
extern bytes_new
extern bytes_from_data

BR_SELF   equ 8
BR_OLD    equ 16
BR_NEW    equ 24
BR_BUF    equ 32
BR_BUFSZ  equ 40
BR_WPOS   equ 48
BR_FRAME  equ 56

DEF_FUNC bytes_method_replace, BR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 3
    jne .br_error

    mov rax, [rdi]              ; self
    mov rcx, [rdi + 8]         ; old
    mov rdx, [rdi + 16]         ; new
    mov [rbp - BR_SELF], rax
    mov [rbp - BR_OLD], rcx
    mov [rbp - BR_NEW], rdx

    ; rbx=self, r12=old, r13=new
    mov rbx, rax
    mov r12, rcx
    mov r13, rdx

    mov r14, [rbx + PyBytesObject.ob_size]    ; self_len
    mov r15, [r12 + PyBytesObject.ob_size]    ; old_len

    ; If old_len == 0, return copy of self
    test r15, r15
    jz .br_copy_self

    ; Allocate initial buffer: self_len * 2 + 64
    lea rdi, [r14 * 2 + 64]
    mov [rbp - BR_BUFSZ], rdi
    call ap_malloc
    mov [rbp - BR_BUF], rax
    mov qword [rbp - BR_WPOS], 0

    xor ecx, ecx               ; scan position

.br_scan:
    ; Remaining bytes
    mov rax, r14
    sub rax, rcx
    cmp rax, r15
    jl .br_copy_tail

    ; memcmp at scan position
    push rcx
    mov rdi, [rbp - BR_SELF]
    lea rdi, [rdi + PyBytesObject.data]
    add rdi, rcx
    mov rsi, [rbp - BR_OLD]
    lea rsi, [rsi + PyBytesObject.data]
    mov rdx, r15
    call ap_memcmp
    pop rcx
    test eax, eax
    jnz .br_no_match

    ; Match found at rcx — ensure buffer space
    mov rax, [rbp - BR_WPOS]
    add rax, [r13 + PyBytesObject.ob_size]
    add rax, r14
    cmp rax, [rbp - BR_BUFSZ]
    jl .br_space_ok
    shl rax, 1
    mov [rbp - BR_BUFSZ], rax
    push rcx
    mov rdi, [rbp - BR_BUF]
    mov rsi, rax
    call ap_realloc
    mov [rbp - BR_BUF], rax
    pop rcx
.br_space_ok:

    ; Copy new_str into buffer
    mov rax, [r13 + PyBytesObject.ob_size]
    test rax, rax
    jz .br_skip_new
    push rcx
    push rax
    mov rdi, [rbp - BR_BUF]
    add rdi, [rbp - BR_WPOS]
    mov rsi, [rbp - BR_NEW]
    lea rsi, [rsi + PyBytesObject.data]
    mov rdx, rax
    call ap_memcpy
    pop rax
    pop rcx
    add [rbp - BR_WPOS], rax
.br_skip_new:
    add rcx, r15                ; advance past old
    jmp .br_scan

.br_no_match:
    ; Copy one byte from self
    mov rdi, [rbp - BR_BUF]
    add rdi, [rbp - BR_WPOS]
    mov rax, [rbp - BR_SELF]
    movzx eax, byte [rax + PyBytesObject.data + rcx]
    mov [rdi], al
    inc qword [rbp - BR_WPOS]
    inc rcx
    jmp .br_scan

.br_copy_tail:
    ; Copy remaining bytes
    mov rax, r14
    sub rax, rcx
    test rax, rax
    jz .br_make_bytes
    push rax
    push rcx
    mov rdi, [rbp - BR_BUF]
    add rdi, [rbp - BR_WPOS]
    mov rsi, [rbp - BR_SELF]
    lea rsi, [rsi + PyBytesObject.data]
    add rsi, rcx
    mov rdx, rax
    call ap_memcpy
    pop rcx
    pop rax
    add [rbp - BR_WPOS], rax

.br_make_bytes:
    mov rdi, [rbp - BR_BUF]
    mov rsi, [rbp - BR_WPOS]
    call bytes_from_data
    push rax

    mov rdi, [rbp - BR_BUF]
    call ap_free

    pop rax
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.br_copy_self:
    ; Return copy of self
    lea rdi, [rbx + PyBytesObject.data]
    mov rsi, r14
    call bytes_from_data
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.br_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "replace() takes exactly 2 arguments"
    call raise_exception
END_FUNC bytes_method_replace

;; ============================================================================
;; bytes_method_split(args, nargs) -> list of bytes
;; nargs==1: split by whitespace; nargs==2: split by separator bytes
;; ============================================================================
DEF_FUNC bytes_method_split
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                  ; align

    mov rbx, [rdi]              ; self (bytes obj)
    mov r14, rsi                ; nargs

    cmp r14, 2
    jl .bsp_no_sep

    ; Separator mode
    mov r15, [rdi + 8]         ; separator bytes obj
    jmp .bsp_by_sep

.bsp_no_sep:
    ; Split by whitespace
    mov r12, [rbx + PyBytesObject.ob_size]

    mov rdi, 8
    call list_new
    mov r13, rax                ; result list

    xor ecx, ecx
.bsp_ws_scan:
    cmp rcx, r12
    jge .bsp_ws_done
    movzx eax, byte [rbx + PyBytesObject.data + rcx]
    cmp al, ' '
    je .bsp_ws_skip
    cmp al, 9
    je .bsp_ws_skip
    cmp al, 10
    je .bsp_ws_skip
    cmp al, 13
    je .bsp_ws_skip
    jmp .bsp_ws_word

.bsp_ws_skip:
    inc rcx
    jmp .bsp_ws_scan

.bsp_ws_word:
    mov r15, rcx                ; word start
.bsp_ws_wordscan:
    inc rcx
    cmp rcx, r12
    jge .bsp_ws_wordend
    movzx eax, byte [rbx + PyBytesObject.data + rcx]
    cmp al, ' '
    je .bsp_ws_wordend
    cmp al, 9
    je .bsp_ws_wordend
    cmp al, 10
    je .bsp_ws_wordend
    cmp al, 13
    je .bsp_ws_wordend
    jmp .bsp_ws_wordscan

.bsp_ws_wordend:
    push rcx
    lea rdi, [rbx + PyBytesObject.data]
    add rdi, r15
    mov rsi, rcx
    sub rsi, r15
    call bytes_from_data
    mov rdi, r13
    mov rsi, rax
    push rax
    call list_append
    pop rdi
    call obj_decref
    pop rcx
    jmp .bsp_ws_scan

.bsp_ws_done:
    mov rax, r13
    mov edx, TAG_PTR
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bsp_by_sep:
    mov r12, [rbx + PyBytesObject.ob_size]   ; self_len
    mov r14, [r15 + PyBytesObject.ob_size]   ; sep_len

    mov rdi, 8
    call list_new
    mov r13, rax                ; result list

    test r14, r14
    jz .bsp_empty_sep

    ; r11 = segment start, rcx = scan position
    xor ecx, ecx
    xor r11d, r11d              ; segment start = 0

.bsp_sep_scan:
    ; Check if enough bytes remain for separator
    mov rax, r12
    sub rax, rcx
    cmp rax, r14
    jl .bsp_sep_tail

    ; memcmp at scan position
    push rcx
    push r11
    mov rdi, rbx
    lea rdi, [rdi + PyBytesObject.data]
    add rdi, rcx
    lea rsi, [r15 + PyBytesObject.data]
    mov rdx, r14
    call ap_memcmp
    pop r11
    pop rcx
    test eax, eax
    jnz .bsp_sep_nomatch

    ; Found separator at rcx — extract segment [r11..rcx)
    push rcx
    push r11
    lea rdi, [rbx + PyBytesObject.data]
    add rdi, r11
    mov rsi, rcx
    sub rsi, r11
    call bytes_from_data
    mov rdi, r13
    mov rsi, rax
    push rax
    call list_append
    pop rdi
    call obj_decref
    pop r11
    pop rcx

    ; Advance past separator
    add rcx, r14
    mov r11, rcx               ; new segment start
    jmp .bsp_sep_scan

.bsp_sep_nomatch:
    inc rcx
    jmp .bsp_sep_scan

.bsp_sep_tail:
    ; Remaining segment from r11 to end
    lea rdi, [rbx + PyBytesObject.data]
    add rdi, r11
    mov rsi, r12
    sub rsi, r11
    call bytes_from_data
    mov rdi, r13
    mov rsi, rax
    push rax
    call list_append
    pop rdi
    call obj_decref

    mov rax, r13
    mov edx, TAG_PTR
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bsp_empty_sep:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "empty separator"
    call raise_exception
END_FUNC bytes_method_split

;; ============================================================================
;; bytes_method_join(args, nargs) -> new bytes
;; args[0]=self (separator bytes), args[1]=list
;; ============================================================================
BJ_SEP    equ 8
BJ_LIST   equ 16
BJ_TOTAL  equ 24
BJ_BUF    equ 32
BJ_WPOS   equ 40
BJ_TMP    equ 48        ; materialised sequence, owned, or 0
BJ_FRAME  equ 64

; Release the sequence bytes.join() materialised for itself, if it made one.
%macro BJ_RELEASE_TMP 0
    mov rdi, [rbp - BJ_TMP]
    test rdi, rdi
    jz %%no_tmp
    mov qword [rbp - BJ_TMP], 0
    call obj_decref
%%no_tmp:
%endmacro

DEF_FUNC bytes_method_join, BJ_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .bj_error

    mov rax, [rdi]              ; self = separator bytes
    mov rcx, [rdi + 8]         ; the sequence Value
    mov [rbp - BJ_SEP], rax
    mov [rbp - BJ_LIST], rcx
    mov qword [rbp - BJ_TMP], 0

    ; The loop below indexes ob_item directly, so the argument has to be a
    ; list or a tuple.  join() takes any iterable, and the type check here
    ; used to dereference the operand before making it -- b",".join(5) read
    ; ob_type off the payload.
    V_TEST_PTR_M [rdi + 8], rdx
    ja .bj_materialise
    mov rdx, [rcx + PyObject.ob_type]
    lea r8, [rel list_type]
    cmp rdx, r8
    je .bj_seq_ready
    lea r8, [rel tuple_type]
    cmp rdx, r8
    je .bj_seq_ready
.bj_materialise:
    lea rsi, [rdi + 8]          ; &args[1]; rdi is still the args pointer
    lea rdi, [rel tuple_type]
    mov edx, 1
    call tuple_type_call        ; raises for a non-iterable, as CPython does
    mov [rbp - BJ_TMP], rax
    mov [rbp - BJ_LIST], rax
    mov rcx, rax
.bj_seq_ready:

    ; Get count
    mov r12, [rcx + PyListObject.ob_size]   ; count
    test r12, r12
    jz .bj_empty

    ; Compute total length: sum of all item sizes + (count-1)*sep_len
    mov rbx, [rbp - BJ_SEP]
    mov r14, [rbx + PyBytesObject.ob_size]  ; sep_len

    xor r13d, r13d              ; total = 0
    xor ecx, ecx               ; index = 0
.bj_len_loop:
    cmp rcx, r12
    jge .bj_len_done
    mov rax, [rbp - BJ_LIST]
    mov rax, [rax + PyListObject.ob_item]
    mov rax, [rax + rcx * 8]  ; item Value (8-byte stride)
    ; Each item must really be bytes: its ob_size is read as a length and
    ; its data copied, so a str item produced garbage rather than TypeError.
    V_TEST_PTR rax, rdx
    ja .bj_item_error
    mov rdx, [rax + PyObject.ob_type]
    lea r8, [rel bytes_type]
    cmp rdx, r8
    jne .bj_item_error
    add r13, [rax + PyBytesObject.ob_size]
    inc rcx
    jmp .bj_len_loop
.bj_len_done:
    ; Add separator lengths: (count-1) * sep_len
    mov rax, r12
    dec rax
    imul rax, r14
    add r13, rax
    mov [rbp - BJ_TOTAL], r13

    ; Allocate buffer
    mov rdi, r13
    call ap_malloc
    mov [rbp - BJ_BUF], rax
    mov qword [rbp - BJ_WPOS], 0

    ; Copy data
    xor r15d, r15d              ; item index
.bj_copy_loop:
    cmp r15, r12
    jge .bj_make_bytes

    ; Insert separator before all items except first
    test r15, r15
    jz .bj_no_sep
    mov rax, [rbp - BJ_SEP]
    mov rcx, [rax + PyBytesObject.ob_size]
    test rcx, rcx
    jz .bj_no_sep
    push rcx
    mov rdi, [rbp - BJ_BUF]
    add rdi, [rbp - BJ_WPOS]
    lea rsi, [rax + PyBytesObject.data]
    mov rdx, rcx
    call ap_memcpy
    pop rcx
    add [rbp - BJ_WPOS], rcx
.bj_no_sep:
    ; Copy item bytes
    mov rax, [rbp - BJ_LIST]
    mov rax, [rax + PyListObject.ob_item]
    mov rax, [rax + r15 * 8]  ; item bytes obj (8-byte stride)
    mov rcx, [rax + PyBytesObject.ob_size]
    test rcx, rcx
    jz .bj_next_item
    push rcx
    mov rdi, [rbp - BJ_BUF]
    add rdi, [rbp - BJ_WPOS]
    lea rsi, [rax + PyBytesObject.data]
    mov rdx, rcx
    call ap_memcpy
    pop rcx
    add [rbp - BJ_WPOS], rcx
.bj_next_item:
    inc r15
    jmp .bj_copy_loop

.bj_make_bytes:
    mov rdi, [rbp - BJ_BUF]
    mov rsi, [rbp - BJ_TOTAL]
    call bytes_from_data
    push rax

    mov rdi, [rbp - BJ_BUF]
    call ap_free
    BJ_RELEASE_TMP

    pop rax
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bj_empty:
    ; Return empty bytes
    BJ_RELEASE_TMP
    xor edi, edi
    call bytes_new
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bj_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "join() argument must be a list of bytes"
    call raise_exception

.bj_item_error:
    BJ_RELEASE_TMP
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "sequence item: expected a bytes-like object"
    call raise_exception
END_FUNC bytes_method_join

section .rodata
empty_str_cstr: db 0
