; bytearray_methods.asm - bytearray's share of bytes' methods
;
; bytes keeps its data inline and bytearray keeps it out of line, so the
; bytes bodies cannot read a bytearray directly.  Each read-only method is a
; trampoline that copies self into a temporary bytes, runs the bytes body on
; it and wraps the answer back -- and this file is that block, split out of
; methods/bytes.asm when it crossed the 100k cap.

%include "macros.inc"
%include "object.inc"

extern bool_false
extern bool_true
extern _bytes_decode_impl
extern bytes_method_count
extern bytes_method_endswith
extern bytes_method_find
extern bytes_method_hex
extern bytes_method_index
extern bytes_method_join
extern bytes_method_lstrip
extern bytes_method_partition
extern bytes_method_replace
extern bytes_method_rfind
extern bytes_method_rindex
extern bytes_method_rpartition
extern bytes_method_rsplit
extern bytes_method_rstrip
extern bytes_method_split
extern bytes_method_startswith
extern bytes_method_strip
extern exc_MemoryError_type
extern none_singleton
extern bytes_type
extern bytearray_type
extern str_type
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_IndexError_type
extern raise_exception
extern obj_decref
extern obj_incref
extern bytearray_new
extern bytearray_data
extern bytes_from_data
extern bytearray_from_data
extern bytearray_subscript
extern bytearray_ass_subscript
extern bytearray_contains
extern bytearray_tp_iter
extern bytes_from_bytes_like
extern raise_type_error_with_name
extern list_type
extern list_new
extern list_append
extern tuple_type

section .text

;; ============================================================================
;; bytearray's share of bytes' read-only methods.
;;
;; bytes keeps its data inline and bytearray keeps it out of line, so the
;; bytes bodies cannot read a bytearray directly.  Rather than thread a
;; (pointer, length) pair through sixty-odd read sites in two files -- churn
;; on the hot, well-tested type for the benefit of the scratch one -- each
;; wrapper builds a temporary bytes, runs the bytes body on it and releases
;; it.  A bytearray is a scratch buffer by definition; the copy is cheap
;; against the risk of that refactor, and it is the sort of thing to revisit
;; only if bytearray ever becomes hot.
;;
;; Some of these answer with a bytes-like where CPython answers with a
;; bytearray, so the result is converted back where it should be.
;; ============================================================================
BSC_ARGS  equ 8
BSC_NARGS equ 16
BSC_TMP   equ 24            ; the temporary bytes standing in for self
BSC_COPY  equ 32            ; the argument array with args[0] replaced
BSC_RES   equ 40
BSC_FRAME equ 64            ; + 1 push = 72... see the DEF_FUNC below

;; bytearray_shared_call(rdi = args, rsi = nargs, rdx = the bytes body,
;;                       ecx = 0 raw / 1 wrap a bytes-like / 2 wrap a list)
;;   -> the body's Value
DEF_FUNC bytearray_shared_call, 72
    push rbx
    mov [rbp - BSC_ARGS], rdi
    mov [rbp - BSC_NARGS], rsi
    mov [rbp - BSC_RES], rdx
    mov rbx, rcx                ; the wrap mode

    test rsi, rsi
    jz .bsc_bad
    mov rdi, [rdi]              ; self
    mov r8, [rdi + PyByteArrayObject.ob_size]
    push r8
    call bytearray_data
    pop r8
    mov rdi, rax
    mov rsi, r8
    call bytes_from_data
    test rax, rax
    jz .bsc_oom
    mov [rbp - BSC_TMP], rax

    ; Copy the arguments, with args[0] swapped for the temporary.  Eight
    ; slots is more than any of these methods takes.
    mov rcx, [rbp - BSC_NARGS]
    cmp rcx, 8
    ja .bsc_bad_free
    sub rsp, 64
    mov [rbp - BSC_COPY], rsp
    mov rax, [rbp - BSC_TMP]
    mov [rsp], rax
    mov rsi, [rbp - BSC_ARGS]
    mov edx, 1
.bsc_copy_loop:
    cmp rdx, rcx
    jge .bsc_copied
    mov rax, [rsi + rdx*8]
    mov [rsp + rdx*8], rax
    inc rdx
    jmp .bsc_copy_loop
.bsc_copied:
    mov rdi, rsp
    mov rsi, [rbp - BSC_NARGS]
    call qword [rbp - BSC_RES]
    add rsp, 64
    mov [rbp - BSC_RES], rax

    mov rdi, [rbp - BSC_TMP]
    call obj_decref

    mov rax, [rbp - BSC_RES]
    test rax, rax
    jz .bsc_out                 ; it raised, or answered NULL
    cmp rbx, 1
    je .bsc_wrap_one
    cmp rbx, 2
    je .bsc_wrap_list
.bsc_out:
    pop rbx
    leave
    ret

.bsc_wrap_one:
    ; A bytes result becomes a bytearray, as CPython's does -- and the bytes
    ; the body made is released, which it was not.
    mov [rbp - BSC_RES], rax
    mov rdi, rax
    call bytearray_from_bytes
    mov [rbp - BSC_TMP], rax
    mov rdi, [rbp - BSC_RES]
    call obj_decref
    mov rax, [rbp - BSC_TMP]
    pop rbx
    leave
    ret

.bsc_wrap_list:
    ; Every element of the list, likewise.
    mov [rbp - BSC_RES], rax
    mov rcx, [rax + PyListObject.ob_size]
    xor esi, esi
.bsc_wrap_loop:
    cmp rsi, rcx
    jge .bsc_wrapped
    mov rax, [rbp - BSC_RES]
    mov rax, [rax + PyListObject.ob_item]
    mov rdi, [rax + rsi*8]
    push rsi
    push rcx
    call bytearray_from_bytes
    pop rcx
    pop rsi
    test rax, rax
    jz .bsc_wrapped
    mov rdx, [rbp - BSC_RES]
    mov rdx, [rdx + PyListObject.ob_item]
    push rax
    push rsi
    mov rdi, [rdx + rsi*8]
    call obj_decref             ; the bytes the body made
    pop rsi
    pop rax
    mov rdx, [rbp - BSC_RES]
    mov rdx, [rdx + PyListObject.ob_item]
    mov [rdx + rsi*8], rax
    mov rcx, [rbp - BSC_RES]
    mov rcx, [rcx + PyListObject.ob_size]
    inc rsi
    jmp .bsc_wrap_loop
.bsc_wrapped:
    mov rax, [rbp - BSC_RES]
    pop rbx
    leave
    ret

.bsc_bad_free:
    mov rdi, [rbp - BSC_TMP]
    call obj_decref
.bsc_bad:
    RAISE exc_TypeError_type, "descriptor requires a bytearray object"
.bsc_oom:
    RAISE exc_MemoryError_type, "out of memory"
END_FUNC bytearray_shared_call

;; bytearray_from_bytes(rdi = a bytes, borrowed) -> rax = a new bytearray
DEF_FUNC bytearray_from_bytes, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    V_TEST_PTR rdi, rax
    ja .bfb_passthrough
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .bfb_passthrough        ; not a bytes: hand it back untouched
    mov rsi, [rbx + PyBytesObject.ob_size]
    lea rdi, [rbx + PyBytesObject.data]
    call bytearray_new
    pop rbx
    leave
    ret
.bfb_passthrough:
    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC bytearray_from_bytes

;; ============================================================================
;; ba_shared_hex(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.hex, run as bytes_method_hex over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_hex
    lea rdx, [rel bytes_method_hex]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_hex

;; ============================================================================
;; ba_shared_startswith(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.startswith, run as bytes_method_startswith over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_startswith
    lea rdx, [rel bytes_method_startswith]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_startswith

;; ============================================================================
;; ba_shared_endswith(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.endswith, run as bytes_method_endswith over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_endswith
    lea rdx, [rel bytes_method_endswith]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_endswith

;; ============================================================================
;; ba_shared_count(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.count, run as bytes_method_count over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_count
    lea rdx, [rel bytes_method_count]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_count

;; ============================================================================
;; ba_shared_find(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.find, run as bytes_method_find over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_find
    lea rdx, [rel bytes_method_find]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_find

;; ============================================================================
;; ba_shared_decode(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.decode, run as _bytes_decode_impl over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_decode
    lea rdx, [rel _bytes_decode_impl]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_decode

;; ============================================================================
;; ba_shared_replace(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.replace, run as bytes_method_replace over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_replace
    lea rdx, [rel bytes_method_replace]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_replace

;; ============================================================================
;; ba_shared_split(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.split, run as bytes_method_split over a temporary bytes;
;; the result becomes a list of bytearrays.
;; ============================================================================
DEF_FUNC ba_shared_split
    lea rdx, [rel bytes_method_split]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_split

;; ============================================================================
;; ba_shared_rsplit(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.rsplit, run as bytes_method_rsplit over a temporary bytes;
;; the result becomes a list of bytearrays.
;; ============================================================================
DEF_FUNC ba_shared_rsplit
    lea rdx, [rel bytes_method_rsplit]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rsplit

;; ============================================================================
;; ba_shared_rfind(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.rfind, run as bytes_method_rfind over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_rfind
    lea rdx, [rel bytes_method_rfind]
    xor ecx, ecx
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rfind

;; ============================================================================
;; ba_shared_index(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.index, run as bytes_method_index over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_index
    lea rdx, [rel bytes_method_index]
    xor ecx, ecx
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_index

;; ============================================================================
;; ba_shared_rindex(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.rindex, run as bytes_method_rindex over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_rindex
    lea rdx, [rel bytes_method_rindex]
    xor ecx, ecx
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rindex

;; ============================================================================
;; ba_shared_strip(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.strip, run as bytes_method_strip over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_strip
    lea rdx, [rel bytes_method_strip]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_strip

;; ============================================================================
;; ba_shared_lstrip(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.lstrip, run as bytes_method_lstrip over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_lstrip
    lea rdx, [rel bytes_method_lstrip]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_lstrip

;; ============================================================================
;; ba_shared_rstrip(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.rstrip, run as bytes_method_rstrip over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_rstrip
    lea rdx, [rel bytes_method_rstrip]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rstrip

;; ============================================================================
;; ba_shared_partition(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.partition, run as bytes_method_partition over a temporary bytes;
;; the result becomes a list of bytearrays.
;; ============================================================================
DEF_FUNC ba_shared_partition
    lea rdx, [rel bytes_method_partition]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_partition

;; ============================================================================
;; ba_shared_rpartition(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.rpartition, run as bytes_method_rpartition over a temporary bytes;
;; the result becomes a list of bytearrays.
;; ============================================================================
DEF_FUNC ba_shared_rpartition
    lea rdx, [rel bytes_method_rpartition]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rpartition

;; ============================================================================
;; ba_shared_join(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.join, run as bytes_method_join over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_join
    lea rdx, [rel bytes_method_join]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_join

;; ============================================================================
;; bytearray_dunder_len(rdi = args, rsi = nargs) -> rax = Value
;;
;; The slots, reachable by name.  __setitem__ and __delitem__ especially:
;; CPython's own code calls them directly, and `del b[i]` compiles to
;; DELETE_SUBSCR but `b.__delitem__(i)` does not.
;; ============================================================================
DEF_FUNC bytearray_dunder_len
    REQUIRE_SELF bytearray_type, "__len__"
    test rsi, rsi
    jz .badl_bad
    mov rdi, [rdi]
    mov rax, [rdi + PyByteArrayObject.ob_size]
    V_PACK_I64 rax, rcx
    mov edx, TAG_PTR
    leave
    ret
.badl_bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC bytearray_dunder_len

;; ============================================================================
;; bytearray_dunder_iter(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.__iter__, reached by name rather than through the slot.
;; ============================================================================
DEF_FUNC bytearray_dunder_iter
    REQUIRE_SELF bytearray_type, "__iter__"
    test rsi, rsi
    jz .badi_bad
    mov rdi, [rdi]
    call bytearray_tp_iter
    mov edx, TAG_PTR
    leave
    ret
.badi_bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC bytearray_dunder_iter

;; ============================================================================
;; bytearray_dunder_getitem(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.__getitem__, reached by name rather than through the slot.
;; ============================================================================
DEF_FUNC bytearray_dunder_getitem
    REQUIRE_SELF bytearray_type, "__getitem__"
    cmp rsi, 2
    jne .badg_bad
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call bytearray_subscript
    mov edx, TAG_PTR
    leave
    ret
.badg_bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC bytearray_dunder_getitem

;; ============================================================================
;; bytearray_dunder_setitem(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.__setitem__, reached by name rather than through the slot.
;; ============================================================================
DEF_FUNC bytearray_dunder_setitem
    REQUIRE_SELF bytearray_type, "__setitem__"
    cmp rsi, 3
    jne .bads_bad
    mov rdx, [rdi + 16]
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call bytearray_ass_subscript
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.bads_bad:
    RAISE exc_TypeError_type, "expected exactly two arguments"
END_FUNC bytearray_dunder_setitem

;; ============================================================================
;; bytearray_dunder_delitem(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.__delitem__, reached by name rather than through the slot.
;; ============================================================================
DEF_FUNC bytearray_dunder_delitem
    REQUIRE_SELF bytearray_type, "__delitem__"
    cmp rsi, 2
    jne .badd_bad
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    xor edx, edx                ; a NULL value Value means delete
    call bytearray_ass_subscript
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.badd_bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC bytearray_dunder_delitem

;; ============================================================================
;; bytearray_dunder_contains(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.__contains__, reached by name rather than through the slot.
;; ============================================================================
DEF_FUNC bytearray_dunder_contains
    REQUIRE_SELF bytearray_type, "__contains__"
    cmp rsi, 2
    jne .badc_bad
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call bytearray_contains
    test eax, eax
    jz .badc_false
    lea rax, [rel bool_true]
    jmp .badc_out
.badc_false:
    lea rax, [rel bool_false]
.badc_out:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.badc_bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC bytearray_dunder_contains

;; ============================================================================
;; bytearray's share of the string-shaped methods in methods/bytes_str.asm.
;;
;; Same shape as the trampolines above: the bytes body runs on a temporary
;; bytes and the wrap mode says what the answer has to become -- a bytearray
;; for the ones that build a new buffer, a list of bytearrays for splitlines,
;; and nothing at all for the predicates, which answer with a bool.
;;
;; ba_shared_upper(rdi = args, rsi = nargs) -> rax = Value, and so does every
;; one of its siblings below.
;; ============================================================================

DEF_FUNC ba_shared_upper
    extern bytes_method_upper
    lea rdx, [rel bytes_method_upper]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_upper

;; ============================================================================
;; ba_shared_lower(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.lower, run as bytes_method_lower over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_lower
    extern bytes_method_lower
    lea rdx, [rel bytes_method_lower]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_lower

;; ============================================================================
;; ba_shared_swapcase(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.swapcase, run as bytes_method_swapcase over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_swapcase
    extern bytes_method_swapcase
    lea rdx, [rel bytes_method_swapcase]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_swapcase

;; ============================================================================
;; ba_shared_capitalize(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.capitalize, run as bytes_method_capitalize over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_capitalize
    extern bytes_method_capitalize
    lea rdx, [rel bytes_method_capitalize]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_capitalize

;; ============================================================================
;; ba_shared_title(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.title, run as bytes_method_title over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_title
    extern bytes_method_title
    lea rdx, [rel bytes_method_title]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_title

;; ============================================================================
;; ba_shared_isalpha(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.isalpha, run as bytes_method_isalpha over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_isalpha
    extern bytes_method_isalpha
    lea rdx, [rel bytes_method_isalpha]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isalpha

;; ============================================================================
;; ba_shared_isdigit(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.isdigit, run as bytes_method_isdigit over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_isdigit
    extern bytes_method_isdigit
    lea rdx, [rel bytes_method_isdigit]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isdigit

;; ============================================================================
;; ba_shared_isspace(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.isspace, run as bytes_method_isspace over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_isspace
    extern bytes_method_isspace
    lea rdx, [rel bytes_method_isspace]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isspace

;; ============================================================================
;; ba_shared_isalnum(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.isalnum, run as bytes_method_isalnum over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_isalnum
    extern bytes_method_isalnum
    lea rdx, [rel bytes_method_isalnum]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isalnum

;; ============================================================================
;; ba_shared_isascii(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.isascii, run as bytes_method_isascii over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_isascii
    extern bytes_method_isascii
    lea rdx, [rel bytes_method_isascii]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isascii

;; ============================================================================
;; ba_shared_isupper(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.isupper, run as bytes_method_isupper over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_isupper
    extern bytes_method_isupper
    lea rdx, [rel bytes_method_isupper]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isupper

;; ============================================================================
;; ba_shared_islower(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.islower, run as bytes_method_islower over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_islower
    extern bytes_method_islower
    lea rdx, [rel bytes_method_islower]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_islower

;; ============================================================================
;; ba_shared_istitle(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.istitle, run as bytes_method_istitle over a temporary bytes;
;; the body answers, unchanged.
;; ============================================================================
DEF_FUNC ba_shared_istitle
    extern bytes_method_istitle
    lea rdx, [rel bytes_method_istitle]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_istitle

;; ============================================================================
;; ba_shared_ljust(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.ljust, run as bytes_method_ljust over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_ljust
    extern bytes_method_ljust
    lea rdx, [rel bytes_method_ljust]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_ljust

;; ============================================================================
;; ba_shared_rjust(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.rjust, run as bytes_method_rjust over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_rjust
    extern bytes_method_rjust
    lea rdx, [rel bytes_method_rjust]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rjust

;; ============================================================================
;; ba_shared_center(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.center, run as bytes_method_center over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_center
    extern bytes_method_center
    lea rdx, [rel bytes_method_center]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_center

;; ============================================================================
;; ba_shared_zfill(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.zfill, run as bytes_method_zfill over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_zfill
    extern bytes_method_zfill
    lea rdx, [rel bytes_method_zfill]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_zfill

;; ============================================================================
;; ba_shared_expandtabs(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.expandtabs, run as bytes_method_expandtabs over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_expandtabs
    extern bytes_method_expandtabs
    lea rdx, [rel bytes_method_expandtabs]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_expandtabs

;; ============================================================================
;; ba_shared_translate(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.translate, run as bytes_method_translate over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_translate
    extern bytes_method_translate
    lea rdx, [rel bytes_method_translate]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_translate

;; ============================================================================
;; ba_shared_splitlines(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.splitlines, run as bytes_method_splitlines over a temporary bytes;
;; the result becomes a list of bytearrays.
;; ============================================================================
DEF_FUNC ba_shared_splitlines
    extern bytes_method_splitlines
    lea rdx, [rel bytes_method_splitlines]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_splitlines

;; ============================================================================
;; ba_shared_removeprefix(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.removeprefix, run as bytes_method_removeprefix over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_removeprefix
    extern bytes_method_removeprefix
    lea rdx, [rel bytes_method_removeprefix]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_removeprefix

;; ============================================================================
;; ba_shared_removesuffix(rdi = args, rsi = nargs) -> rax = Value
;; bytearray.removesuffix, run as bytes_method_removesuffix over a temporary bytes;
;; the result becomes a bytearray.
;; ============================================================================
DEF_FUNC ba_shared_removesuffix
    extern bytes_method_removesuffix
    lea rdx, [rel bytes_method_removesuffix]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_removesuffix
