; methods/bytes.asm - bytes and bytearray methods
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
extern str_type
extern bytes_new
extern bytearray_type_call
extern bytearray_type
extern bytes_like_ptr_len
extern int_is_integer
extern obj_as_index
extern bytearray_data
extern bytearray_new
extern bytearray_tp_iter
extern bytearray_subscript
extern bytearray_ass_subscript
extern bytearray_contains
extern exc_MemoryError_type
extern none_singleton
extern _bytes_decode_impl
extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_memcpy
extern ap_memcmp
extern obj_decref
extern str_new_heap
extern list_new
extern list_append
extern list_type
extern tuple_type
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
extern tuple_type_call
extern bool_false
extern bool_true

; Set entry layout constants (must match set.asm)

; --- moved to a sibling file by the split ---

section .text

;; ============================================================================
;; BYTES_NEEDLE sub_slot, scratch_slot
;;
;; find(), count() and index() take an INT as well as a bytes-like -- CPython's
;; do, and `charmap.find(1, q)` in the regex compiler is exactly that call.
;; Reading an int as a PyBytesObject header is a wild dereference, and it
;; segfaulted.
;;
;; Rather than give every reader a second path, a one-byte bytes header is
;; built in the caller's frame and the sub slot pointed at it: ob_size = 1 and
;; one data byte, which is all the bodies below read.  It lives exactly as
;; long as the frame, so nothing owns or releases it.
;; ============================================================================
%macro BYTES_NEEDLE 2           ; %1 = the sub slot, %2 = the scratch slot
    ; int_is_integer, not a pointer test: True, every int under INT_STRESS=1,
    ; and every int subclass instance all arrive as pointers, and a tag test
    ; sends them down the bytes-like path to be read as an object header.
    mov rdi, [rbp - %1]
    V_UNPACK rdi, rdx
    call int_is_integer
    test eax, eax
    jz %%done                   ; a bytes-like: leave it alone
    mov rdi, [rbp - %1]
    V_UNPACK rdi, rdx
    call obj_as_index
    cmp rax, 0
    jl %%range
    cmp rax, 255
    jle %%in_range
%%range:
    RAISE exc_ValueError_type, "byte must be in range(0, 256)"
%%in_range:
    lea rcx, [rbp - %2]
    mov qword [rcx + PyBytesObject.ob_size], 1
    mov [rcx + PyBytesObject.data], al
    ; ob_type too: the fabricated header now goes through bytes_like_ptr_len
    ; like any other argument, and that reads the type before the data.
    lea rdx, [rel bytes_type]
    mov [rcx + PyObject.ob_type], rdx
    mov [rbp - %1], rcx
%%done:
%endmacro


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
BH_FRAME  equ 32            ; + 0 pushes = 32

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
    and eax, 0x0f
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
;; bytes.fromhex(s) / bytearray.fromhex(s) -- a classmethod on both.
;;
;; hex() was here and its inverse was not, which is the half that
;; binascii.unhexlify needs -- and binascii is what base64, quopri, uu and
;; plistlib come in behind.  CPython skips ASCII whitespace between BYTES
;; (not inside a pair), and rejects anything else.
;; ============================================================================
BFH_SRC   equ 8
BFH_OUT   equ 16
BFH_N     equ 24
BFH_I     equ 32
BFH_POS   equ 40
BFH_TYPE  equ 48
BFH_FRAME equ 64            ; + 0 pushes = 64

DEF_FUNC bytes_fromhex_impl, BFH_FRAME
    ; rdi = args, rsi = nargs; args[0] is the class, args[1] the string.
    cmp rsi, 2
    jne .bfh_args
    mov rax, [rdi]
    mov [rbp - BFH_TYPE], rax
    mov rdi, [rdi + 8]
    V_TEST_PTR rdi, rax
    ja .bfh_type
    test rdi, rdi
    jz .bfh_type
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .bfh_type
    mov [rbp - BFH_SRC], rdi
    mov rax, [rdi + PyStrObject.ob_size]
    mov [rbp - BFH_N], rax

    ; At most one byte per two characters; whitespace only shortens it.
    shr rax, 1
    inc rax
    mov rdi, rax
    extern ap_malloc
    call ap_malloc
    test rax, rax
    jz .bfh_nomem
    mov [rbp - BFH_OUT], rax
    mov qword [rbp - BFH_I], 0
    mov qword [rbp - BFH_POS], 0

.bfh_loop:
    mov rcx, [rbp - BFH_I]
    cmp rcx, [rbp - BFH_N]
    jge .bfh_done
    mov rdx, [rbp - BFH_SRC]
    movzx eax, byte [rdx + PyStrObject.data + rcx]
    ; Whitespace between bytes is skipped, as CPython's is.
    cmp al, ' '
    je .bfh_skip
    cmp al, 9
    je .bfh_skip
    cmp al, 10
    je .bfh_skip
    cmp al, 13
    je .bfh_skip

    call bfh_digit
    cmp eax, -1
    je .bfh_bad
    mov r8d, eax
    shl r8d, 4
    inc qword [rbp - BFH_I]
    mov rcx, [rbp - BFH_I]
    cmp rcx, [rbp - BFH_N]
    jge .bfh_odd
    mov rdx, [rbp - BFH_SRC]
    movzx eax, byte [rdx + PyStrObject.data + rcx]
    push r8
    call bfh_digit
    pop r8
    cmp eax, -1
    je .bfh_bad
    or r8d, eax
    mov rdx, [rbp - BFH_OUT]
    mov rcx, [rbp - BFH_POS]
    mov [rdx + rcx], r8b
    inc qword [rbp - BFH_POS]
    inc qword [rbp - BFH_I]
    jmp .bfh_loop

.bfh_skip:
    inc qword [rbp - BFH_I]
    jmp .bfh_loop

.bfh_done:
    ; bytes_new takes the SIZE and allocates; the data is copied in after.
    mov rdi, [rbp - BFH_POS]
    extern bytes_new
    call bytes_new
    test rax, rax
    jz .bfh_nomem
    push rax
    sub rsp, 8
    lea rdi, [rax + PyBytesObject.data]
    mov rsi, [rbp - BFH_OUT]
    mov rdx, [rbp - BFH_POS]
    extern ap_memcpy
    call ap_memcpy
    add rsp, 8
    pop rax
    push rax
    sub rsp, 8
    mov rdi, [rbp - BFH_OUT]
    extern ap_free
    call ap_free
    add rsp, 8
    pop rax

    ; bytearray.fromhex answers a bytearray; the class comes in as args[0].
    mov rcx, [rbp - BFH_TYPE]
    lea rdx, [rel bytearray_type]
    cmp rcx, rdx
    jne .bfh_return
    push rax
    sub rsp, 8
    lea rdi, [rel bytearray_type]
    lea rsi, [rsp + 8]
    mov edx, 1
    extern bytearray_type_call
    call bytearray_type_call
    mov rcx, rax
    add rsp, 8
    pop rdi
    push rcx
    sub rsp, 8
    extern obj_decref
    call obj_decref
    add rsp, 8
    pop rax
    test rax, rax
    jz .bfh_nomem
    leave
    ret

.bfh_return:
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bfh_odd:
    mov rdi, [rbp - BFH_OUT]
    call ap_free
    mov rsi, [rbp - BFH_I]
    call bfh_raise_position
.bfh_bad:
    mov rdi, [rbp - BFH_OUT]
    call ap_free
    mov rsi, [rbp - BFH_I]
    call bfh_raise_position
.bfh_nomem:
    xor eax, eax
    leave
    ret
.bfh_type:
    mov rsi, rdi
    CSTRING rdi, `fromhex() argument must be str, not \x01`
    extern raise_type_error_with_name
    call raise_type_error_with_name
.bfh_args:
    RAISE exc_TypeError_type, "fromhex() takes exactly one argument"
END_FUNC bytes_fromhex_impl

;; bfh_digit(eax = a character) -> eax = its value, or -1
DEF_FUNC_BARE bfh_digit
    cmp al, '0'
    jb .bd_no
    cmp al, '9'
    jbe .bd_dec
    cmp al, 'A'
    jb .bd_no
    cmp al, 'F'
    jbe .bd_upper
    cmp al, 'a'
    jb .bd_no
    cmp al, 'f'
    jbe .bd_lower
.bd_no:
    mov eax, -1
    ret
.bd_dec:
    sub eax, '0'
    ret
.bd_upper:
    sub eax, 'A' - 10
    ret
.bd_lower:
    sub eax, 'a' - 10
    ret
END_FUNC bfh_digit

;; bfh_raise_position(rsi = the character index) -- does not return
BRP_POS   equ 8
BRP_BUF   equ 176
BRP_FRAME equ 176           ; + 0 pushes = 176, 16-aligned
DEF_FUNC_LOCAL bfh_raise_position, BRP_FRAME
    mov [rbp - BRP_POS], rsi
    lea rdi, [rbp - BRP_BUF]
    CSTRING rsi, "non-hexadecimal number found in fromhex() arg at position "
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - BRP_POS]
    extern msg_append_i64
    call msg_append_i64
    lea rdi, [rel exc_ValueError_type]
    lea rsi, [rbp - BRP_BUF]
    extern raise_exception
    call raise_exception
END_FUNC bfh_raise_position

;; ============================================================================
;; bytes_affix_match(rdi = one affix Value, rsi = the subject's data,
;;                   rdx = its length, ecx = 0 for a prefix, 1 for a suffix)
;;   -> eax = 1 match, 0 no match, -1 the affix is not bytes-like
;; ============================================================================
BAM_DATA  equ 8
BAM_LEN   equ 16
BAM_END   equ 24
BAM_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC_LOCAL bytes_affix_match, BAM_FRAME
    mov [rbp - BAM_DATA], rsi
    mov [rbp - BAM_LEN], rdx
    mov [rbp - BAM_END], rcx
    call bytes_like_ptr_len     ; rax = data, r10 = length, ecx = ok
    test ecx, ecx
    jz .bam_bad
    cmp r10, [rbp - BAM_LEN]
    ja .bam_no                  ; longer than the subject
    test r10, r10
    jz .bam_yes                 ; an empty affix always matches
    mov rsi, rax
    mov rdi, [rbp - BAM_DATA]
    cmp qword [rbp - BAM_END], 0
    je .bam_have_start
    add rdi, [rbp - BAM_LEN]    ; a suffix starts len - affixlen in
    sub rdi, r10
.bam_have_start:
    mov rdx, r10
    call ap_memcmp
    test eax, eax
    jnz .bam_no
.bam_yes:
    mov eax, 1
    leave
    ret
.bam_no:
    xor eax, eax
    leave
    ret
.bam_bad:
    mov eax, -1
    leave
    ret
END_FUNC bytes_affix_match

;; ============================================================================
;; bytes_method_affix(rdi = args, rsi = nargs, edx = 0 prefix / 1 suffix,
;;                    rcx = the method's name) -> a bool Value
;;
;; startswith and endswith differ only in which end they compare, and both
;; take EITHER one affix or a tuple of them -- `data.startswith((b'PK',
;; b'\x1f\x8b'))` is the idiom that matters, and routing the whole argument
;; through bytes_like_ptr_len turned it into a TypeError.
;; ============================================================================
BAF_SELF  equ 8
BAF_SLEN  equ 16
BAF_ARG   equ 24
BAF_END   equ 32
BAF_NAME  equ 40
BAF_I     equ 48
BAF_N     equ 56
BAF_BAD   equ 64            ; the element the message names, for a tuple
BAF_FRAME equ 80            ; + 0 pushes = 80

DEF_FUNC_LOCAL bytes_method_affix, BAF_FRAME
    cmp rsi, 2
    jne .baf_argerr
    mov [rbp - BAF_END], rdx
    mov [rbp - BAF_NAME], rcx
    mov rax, [rdi + 8]
    mov [rbp - BAF_ARG], rax

    ; The subject: a bytearray keeps its bytes out of line, so reading data
    ; and ob_size off it with a bytes layout found the capacity word.
    mov rdi, [rdi]
    call bytes_like_ptr_len
    test ecx, ecx
    jz .baf_self_type
    mov [rbp - BAF_SELF], rax
    mov [rbp - BAF_SLEN], r10

    mov rdi, [rbp - BAF_ARG]
    V_TEST_PTR rdi, rax
    ja .baf_single
    test rdi, rdi
    jz .baf_single
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    jne .baf_single

    ; A tuple: true if any element matches.
    mov rax, [rdi + PyTupleObject.ob_size]
    mov [rbp - BAF_N], rax
    mov qword [rbp - BAF_I], 0
.baf_loop:
    mov rcx, [rbp - BAF_I]
    cmp rcx, [rbp - BAF_N]
    jge .baf_false
    mov rax, [rbp - BAF_ARG]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rdi, [rax + rcx*8]
    mov [rbp - BAF_BAD], rdi    ; the item, in case it is not bytes-like
    mov rsi, [rbp - BAF_SELF]
    mov rdx, [rbp - BAF_SLEN]
    mov rcx, [rbp - BAF_END]
    call bytes_affix_match
    cmp eax, 0
    jl .baf_item_type           ; an ELEMENT of the tuple, worded differently
    test eax, eax
    jnz .baf_true
    inc qword [rbp - BAF_I]
    jmp .baf_loop

.baf_single:
    mov rdi, [rbp - BAF_ARG]
    mov [rbp - BAF_BAD], rdi
    mov rsi, [rbp - BAF_SELF]
    mov rdx, [rbp - BAF_SLEN]
    mov rcx, [rbp - BAF_END]
    call bytes_affix_match
    cmp eax, 0
    jl .baf_arg_type
    test eax, eax
    jnz .baf_true

.baf_false:
    xor eax, eax
    RET_BOOL_RAX
    leave
    V_PACK rax, rdx
    ret
.baf_true:
    mov eax, 1
    RET_BOOL_RAX
    leave
    V_PACK rax, rdx
    ret

.baf_self_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
.baf_item_type:
    ; CPython words a bad ELEMENT differently from a bad argument: "a
    ; bytes-like object is required, not 'str'" rather than "first arg must
    ; be bytes or a tuple of bytes".
    lea rdi, [rel bj_msgbuf]
    lea rsi, [rel baf_msg_item]
    call bj_append_cstr
    mov rdi, rax
    mov rsi, [rbp - BAF_BAD]
    call baf_append_quoted_typename
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel bj_msgbuf]
    call raise_exception
    ud2
.baf_arg_type:
    ; CPython names the method and the offending type: "startswith first arg
    ; must be bytes or a tuple of bytes, not str".
    lea rdi, [rel bj_msgbuf]
    mov rsi, [rbp - BAF_NAME]
    call bj_append_cstr
    mov rdi, rax
    lea rsi, [rel baf_msg_first]
    call bj_append_cstr
    mov rdi, rax
    mov rsi, [rbp - BAF_BAD]
    call baf_append_typename
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel bj_msgbuf]
    call raise_exception
    ud2
.baf_argerr:
    lea rdi, [rel bj_msgbuf]
    mov rsi, [rbp - BAF_NAME]
    call bj_append_cstr
    mov rdi, rax
    lea rsi, [rel baf_msg_args]
    call bj_append_cstr
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel bj_msgbuf]
    call raise_exception
    ud2
END_FUNC bytes_method_affix

DEF_FUNC_LOCAL baf_append_quoted_typename   ; (rdi = dest, rsi = a Value)
    push rbx
    mov rbx, rdi
    mov byte [rbx], 0x27        ; an apostrophe
    lea rdi, [rbx + 1]
    call baf_append_typename
    mov byte [rax], 0x27
    mov byte [rax + 1], 0
    lea rax, [rax + 1]
    pop rbx
    leave
    ret
END_FUNC baf_append_quoted_typename

DEF_FUNC_LOCAL baf_append_typename  ; (rdi = dest, rsi = a Value) -> rax
    V_TEST_PTR rsi, rax
    ja .bat2_int
    test rsi, rsi
    jz .bat2_int
    mov rsi, [rsi + PyObject.ob_type]
    mov rsi, [rsi + PyTypeObject.tp_name]
    jmp .bat2_have
.bat2_int:
    lea rsi, [rel bj_name_int]
.bat2_have:
    call bj_append_cstr
    leave
    ret
END_FUNC baf_append_typename

;; ============================================================================
;; bytes_method_startswith(args, nargs) -> Bool
;; args[0]=self (bytes), args[1]=prefix (bytes)
;; ============================================================================
;; bytes_method_startswith / bytes_method_endswith -- one implementation, two
;; ends.  Both accept a tuple of affixes, as CPython's do.
;; ============================================================================
DEF_FUNC bytes_method_startswith
    xor edx, edx
    lea rcx, [rel baf_name_startswith]
    call bytes_method_affix
    leave
    ret
END_FUNC bytes_method_startswith

;; ============================================================================
;; bytes_method_endswith(args, nargs) -> Bool
;; args[0]=self (bytes), args[1]=suffix (bytes)
DEF_FUNC bytes_method_endswith
    mov edx, 1
    lea rcx, [rel baf_name_endswith]
    call bytes_method_affix
    leave
    ret
END_FUNC bytes_method_endswith

;; ============================================================================
;; bytes_method_count(args, nargs) -> SmallInt
;; args[0]=self (bytes), args[1]=sub (bytes)
;; Count non-overlapping occurrences of sub in self.
;; ============================================================================
BC_SELF   equ 8
BC_SUB    equ 16
BC_ONE    equ 56            ; a one-byte bytes header, for an int needle
BC_ARGS   equ 64
BC_NARGS  equ 72
BC_FRAME  equ 80            ; + 0 pushes = 80

DEF_FUNC bytes_method_count, BC_FRAME
    cmp rsi, 2
    jl .bc_noargs
    cmp rsi, 4
    jg .bc_error

    mov rax, [rdi]              ; self
    mov rcx, [rdi + 8]         ; sub
    mov [rbp - BC_SELF], rax
    mov [rbp - BC_SUB], rcx
    mov [rbp - BC_ARGS], rdi
    mov [rbp - BC_NARGS], rsi
    BYTES_NEEDLE BC_SUB, BC_ONE

    ; Both sides through bytes_like_ptr_len: a bytearray argument was read
    ; with a bytes layout, so ob_size found its capacity and the data pointer
    ; landed inside the header.  The one-byte needle BYTES_NEEDLE fabricates
    ; above is bytes-shaped, so it comes through the same door.
    mov rdi, [rbp - BC_SELF]
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bc_type
    mov [rbp - BC_SELF], rax
    mov r8, r10                 ; self_len
    mov rdi, [rbp - BC_SUB]
    push r8
    sub rsp, 8
    call bytes_like_ptr_len
    add rsp, 8
    pop r8
    test ecx, ecx
    jz .bc_type
    mov [rbp - BC_SUB], rax
    mov r9, r10                 ; sub_len

    ; count(sub[, start[, end]]), the same range arguments find() takes.
    ; Without them `re.error` could not build its message: it counts newlines
    ; up to the offending position, and the three-argument call raised a
    ; TypeError inside the constructor, so a bad pattern reported a raw tuple
    ; instead of "bad escape \q at position 0".
    xor r11d, r11d              ; start = 0
    cmp qword [rbp - BC_NARGS], 3
    jl .bc_have_range
    push r8
    push r9
    mov rdi, [rbp - BC_ARGS]
    mov rdi, [rdi + 16]
    V_UNPACK rdi, rdx
    call obj_as_index
    pop r9
    pop r8
    mov r11, rax
    test r11, r11
    jns .bc_start_ok
    add r11, r8                 ; a negative start counts from the end
    jns .bc_start_ok
    xor r11d, r11d
.bc_start_ok:
    cmp r11, r8
    ja .bc_zero
    cmp qword [rbp - BC_NARGS], 4
    jl .bc_have_range
    push r8
    push r9
    push r11
    sub rsp, 8
    mov rdi, [rbp - BC_ARGS]
    mov rdi, [rdi + 24]
    V_UNPACK rdi, rdx
    call obj_as_index
    add rsp, 8
    pop r11
    pop r9
    pop r8
    test rax, rax
    jns .bc_end_ok
    add rax, r8
    jns .bc_end_ok
    xor eax, eax
.bc_end_ok:
    cmp rax, r8
    jbe .bc_end_clamped
    mov rax, r8
.bc_end_clamped:
    mov r8, rax                 ; the scan stops here
    cmp r11, r8
    ja .bc_zero

.bc_have_range:
    ; If sub_len == 0: one match at every position in the range, plus one
    test r9, r9
    jz .bc_empty_sub

    ; If sub_len > what is left: count = 0
    mov rax, r8
    sub rax, r11
    cmp r9, rax
    ja .bc_zero

    ; Scan
    xor r10d, r10d              ; count = 0

.bc_loop:
    mov rax, r8
    sub rax, r11                ; remaining = self_len - offset
    cmp rax, r9
    jb .bc_result               ; not enough bytes left

    mov rdi, [rbp - BC_SELF]
    add rdi, r11
    mov rsi, [rbp - BC_SUB]
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
    mov rax, r8
    sub rax, r11
    inc rax
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

.bc_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
.bc_error:
    RAISE exc_TypeError_type, "count() takes at most 3 arguments"
.bc_noargs:
    RAISE exc_TypeError_type, "count() takes at least 1 argument (0 given)"
END_FUNC bytes_method_count


;; ============================================================================
;; bytes_method_find(args, nargs) -> SmallInt
;; args[0]=self (bytes), args[1]=sub (bytes)
;; Returns index of first occurrence, or -1 if not found.
;; ============================================================================
BF_SELF   equ 8
BF_SUB    equ 16
BF_ARGS   equ 24
BF_NARGS  equ 32
BF_ONE    equ 72            ; a one-byte bytes header, for an int needle
; Derived, not picked: the fabricated header above runs from rbp-BF_ONE
; upward, so a hand-chosen 48 landed on its data byte and the int-needle
; find() searched for a length instead of a character.
BF_SLEN   equ BF_ONE + 8
BF_NLEN   equ BF_ONE + 16
BF_RIGHT  equ BF_ONE + 24   ; 1 for rfind and rindex
BF_MISS   equ BF_ONE + 32   ; 1 to raise instead of answering -1
BF_FRAME  equ BF_ONE + 40   ; + 0 pushes = 112

;; ============================================================================
;; bytes_find_impl(rdi = args, rsi = nargs, edx = from_right, ecx = raise)
;;   -> the index, or -1
;;
;; One body for find, rfind, index and rindex: the two directions differ only
;; in where the scan starts, and index and rindex differ from the first two
;; only in answering a miss with a ValueError.  bytes had the first of the
;; four and neither of the others.
;; ============================================================================
DEF_FUNC bytes_find_impl, BF_FRAME
    mov [rbp - BF_RIGHT], rdx
    mov [rbp - BF_MISS], rcx
    cmp rsi, 2
    jl .bf_error
    cmp rsi, 4
    jg .bf_error

    mov rax, [rdi]              ; self
    mov rcx, [rdi + 8]         ; sub
    mov [rbp - BF_SELF], rax
    mov [rbp - BF_SUB], rcx
    mov [rbp - BF_ARGS], rdi
    mov [rbp - BF_NARGS], rsi
    BYTES_NEEDLE BF_SUB, BF_ONE

    ; The slots hold (pointer, length) from here on, not objects: a bytearray
    ; argument read with a bytes layout found its capacity where the length
    ; should be and its header where the data should be.
    mov rdi, [rbp - BF_SELF]
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bf_type
    mov [rbp - BF_SELF], rax
    mov [rbp - BF_SLEN], r10
    mov rdi, [rbp - BF_SUB]
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bf_type
    mov [rbp - BF_SUB], rax
    mov [rbp - BF_NLEN], r10

    ; find(sub[, start[, end]]).  CPython's takes both, and the regex
    ; compiler's `charmap.find(1, q)` walks a 256-byte map with the start
    ; argument -- without it the loop never advances.
    mov r8, [rbp - BF_SLEN]
    xor r11d, r11d              ; start = 0
    cmp qword [rbp - BF_NARGS], 3
    jl .bf_have_range
    mov rdi, [rbp - BF_ARGS]
    mov rdi, [rdi + 16]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov r11, rax
    mov r8, [rbp - BF_SLEN]
    test r11, r11
    jns .bf_start_ok
    add r11, r8                 ; a negative start counts from the end
    jns .bf_start_ok
    xor r11d, r11d
.bf_start_ok:
    cmp qword [rbp - BF_NARGS], 4
    jl .bf_have_range
    push r11
    mov rdi, [rbp - BF_ARGS]
    mov rdi, [rdi + 24]
    V_UNPACK rdi, rdx
    call obj_as_index
    pop r11
    mov rcx, [rbp - BF_SLEN]
    test rax, rax
    jns .bf_end_ok
    add rax, rcx
    jns .bf_end_ok
    xor eax, eax
.bf_end_ok:
    cmp rax, rcx
    jbe .bf_end_clamped
    mov rax, rcx
.bf_end_clamped:
    mov r8, rax                 ; the scan stops here

.bf_have_range:
    mov r9, [rbp - BF_NLEN]                 ; sub_len

    ; An empty needle is found at the start position -- at the END position
    ; when the scan runs the other way, which is what rfind answers.
    test r9, r9
    jnz .bf_nonempty
    cmp qword [rbp - BF_RIGHT], 0
    je .bf_found_at_start
    mov r11, r8
    jmp .bf_found_at_start
.bf_nonempty:

    cmp r11, r8
    ja .bf_not_found
    mov rax, r8
    sub rax, r11
    cmp r9, rax
    ja .bf_not_found

    cmp qword [rbp - BF_RIGHT], 0
    je .bf_loop

    ; From the right: start at the last position the needle can begin at and
    ; walk down to the start.
    mov r10, r8
    sub r10, r9                 ; the last candidate
.bf_rloop:
    cmp r10, r11
    jl .bf_not_found
    mov rdi, [rbp - BF_SELF]
    add rdi, r10
    mov rsi, [rbp - BF_SUB]
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
    jz .bf_rfound
    dec r10
    jmp .bf_rloop
.bf_rfound:
    mov r11, r10
    jmp .bf_found

.bf_loop:
    mov rax, r8
    sub rax, r11                ; remaining
    cmp rax, r9
    jb .bf_not_found

    mov rdi, [rbp - BF_SELF]
    add rdi, r11
    mov rsi, [rbp - BF_SUB]
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
    cmp qword [rbp - BF_MISS], 0
    jne .bf_miss_error
    mov rax, -1
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bf_miss_error:
    RAISE exc_ValueError_type, "subsection not found"

.bf_found_at_start:
    mov rax, r11
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx
    ret

.bf_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
.bf_error:
    RAISE exc_TypeError_type, "find() takes at most 3 arguments"
END_FUNC bytes_find_impl

;; ============================================================================
;; find / rfind / index / rindex, which differ only in direction and in what a
;; miss answers.
;; ============================================================================
DEF_FUNC_BARE bytes_method_find
    xor edx, edx
    xor ecx, ecx
    jmp bytes_find_impl
END_FUNC bytes_method_find

DEF_FUNC_BARE bytes_method_rfind
    mov edx, 1
    xor ecx, ecx
    jmp bytes_find_impl
END_FUNC bytes_method_rfind

DEF_FUNC_BARE bytes_method_index
    xor edx, edx
    mov ecx, 1
    jmp bytes_find_impl
END_FUNC bytes_method_index

DEF_FUNC_BARE bytes_method_rindex
    mov edx, 1
    mov ecx, 1
    jmp bytes_find_impl
END_FUNC bytes_method_rindex

;; ============================================================================
;; bytes_strip_impl(rdi = args, rsi = nargs, edx = mode)
;;   mode 0 = both ends, 1 = left only, 2 = right only
;;
;; strip([chars]): with no argument, ASCII whitespace; with one, every byte in
;; it, as a set.  bytes had none of the three.
;; ============================================================================
BST_SELF  equ 8
BST_CHARS equ 16
BST_SLEN  equ 24
BST_CLEN  equ 32
BST_MODE  equ 40
BST_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC bytes_strip_impl, BST_FRAME
    mov [rbp - BST_MODE], rdx
    cmp rsi, 1
    jl .bst_error
    cmp rsi, 2
    jg .bst_error

    mov qword [rbp - BST_CHARS], 0
    mov qword [rbp - BST_CLEN], 0
    push rdi
    push rsi
    mov rdi, [rdi]
    call bytes_like_ptr_len
    pop rsi
    pop rdi
    test ecx, ecx
    jz .bst_type
    mov [rbp - BST_SELF], rax
    mov [rbp - BST_SLEN], r10

    cmp rsi, 2
    jl .bst_have_chars
    mov rax, [rdi + 8]
    LOAD_NONE rcx
    cmp rax, rcx
    je .bst_have_chars
    mov rdi, rax
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bst_type
    mov [rbp - BST_CHARS], rax
    mov [rbp - BST_CLEN], r10

.bst_have_chars:
    xor r8d, r8d                        ; start
    mov r9, [rbp - BST_SLEN]            ; end, exclusive

    cmp qword [rbp - BST_MODE], 2
    je .bst_right
.bst_left_loop:
    cmp r8, r9
    jge .bst_left_done
    mov rax, [rbp - BST_SELF]
    movzx edi, byte [rax + r8]
    call bst_in_set
    test eax, eax
    jz .bst_left_done
    inc r8
    jmp .bst_left_loop
.bst_left_done:
    cmp qword [rbp - BST_MODE], 1
    je .bst_build

.bst_right:
    cmp r9, r8
    jle .bst_build
    mov rax, [rbp - BST_SELF]
    movzx edi, byte [rax + r9 - 1]
    call bst_in_set
    test eax, eax
    jz .bst_build
    dec r9
    jmp .bst_right

.bst_build:
    mov rdi, [rbp - BST_SELF]
    add rdi, r8
    mov rsi, r9
    sub rsi, r8
    call bytes_from_data
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.bst_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
.bst_error:
    RAISE exc_TypeError_type, "strip() takes at most 1 argument"
END_FUNC bytes_strip_impl

;; bst_in_set(edi = a byte) -> eax = 1 when it is to be stripped.  Reads the
;; caller's BST_CHARS/BST_CLEN through rbp, which is why it is not a
;; standalone function.
DEF_FUNC_BARE bst_in_set
    push rcx
    push rdx
    mov rcx, [rbp - BST_CHARS]
    test rcx, rcx
    jz .bis_space
    mov rdx, [rbp - BST_CLEN]
    xor eax, eax
.bis_scan:
    cmp rax, rdx
    jge .bis_no
    cmp dil, [rcx + rax]
    je .bis_yes
    inc rax
    jmp .bis_scan
.bis_space:
    cmp dil, ' '
    je .bis_yes
    cmp dil, 9
    je .bis_yes
    cmp dil, 10
    je .bis_yes
    cmp dil, 11
    je .bis_yes
    cmp dil, 12
    je .bis_yes
    cmp dil, 13
    je .bis_yes
.bis_no:
    xor eax, eax
    pop rdx
    pop rcx
    ret
.bis_yes:
    mov eax, 1
    pop rdx
    pop rcx
    ret
END_FUNC bst_in_set

DEF_FUNC_BARE bytes_method_strip
    xor edx, edx
    jmp bytes_strip_impl
END_FUNC bytes_method_strip

DEF_FUNC_BARE bytes_method_lstrip
    mov edx, 1
    jmp bytes_strip_impl
END_FUNC bytes_method_lstrip

DEF_FUNC_BARE bytes_method_rstrip
    mov edx, 2
    jmp bytes_strip_impl
END_FUNC bytes_method_rstrip

;; ============================================================================
;; bytes_partition_impl(rdi = args, rsi = nargs, edx = from_right)
;;   -> a 3-tuple (head, sep, tail)
;;
;; partition and rpartition.  A separator that is not there answers the whole
;; string and two empties, at whichever end.
;; ============================================================================
BPT_SELF  equ 8
BPT_SEP   equ 16
BPT_SLEN  equ 24
BPT_NLEN  equ 32
BPT_RIGHT equ 40
BPT_POS   equ 48
BPT_FRAME equ 56            ; + 1 push = 64, 16-aligned

extern tuple_new
DEF_FUNC bytes_partition_impl, BPT_FRAME
    push rbx
    mov [rbp - BPT_RIGHT], rdx
    cmp rsi, 2
    jne .bpt_error

    push rdi
    mov rdi, [rdi]
    call bytes_like_ptr_len
    pop rdi
    test ecx, ecx
    jz .bpt_type
    mov [rbp - BPT_SELF], rax
    mov [rbp - BPT_SLEN], r10
    push rdi
    mov rdi, [rdi + 8]
    call bytes_like_ptr_len
    pop rdi
    test ecx, ecx
    jz .bpt_type
    mov [rbp - BPT_SEP], rax
    mov [rbp - BPT_NLEN], r10
    test r10, r10
    jz .bpt_empty_sep

    ; Where the separator sits, from whichever end.
    mov r8, [rbp - BPT_SLEN]
    sub r8, [rbp - BPT_NLEN]
    js .bpt_missing
    cmp qword [rbp - BPT_RIGHT], 0
    jne .bpt_rscan
    xor r9d, r9d
.bpt_scan:
    cmp r9, r8
    jg .bpt_missing
    mov rdi, [rbp - BPT_SELF]
    add rdi, r9
    mov rsi, [rbp - BPT_SEP]
    mov rdx, [rbp - BPT_NLEN]
    push r8
    push r9
    call ap_memcmp
    pop r9
    pop r8
    test eax, eax
    jz .bpt_found
    inc r9
    jmp .bpt_scan

.bpt_rscan:
    mov r9, r8
.bpt_rscan_loop:
    test r9, r9
    js .bpt_missing
    mov rdi, [rbp - BPT_SELF]
    add rdi, r9
    mov rsi, [rbp - BPT_SEP]
    mov rdx, [rbp - BPT_NLEN]
    push r8
    push r9
    call ap_memcmp
    pop r9
    pop r8
    test eax, eax
    jz .bpt_found
    dec r9
    jmp .bpt_rscan_loop

.bpt_found:
    ; The position goes to the frame first: tuple_new is a call, and r9 is
    ; caller-saved.
    mov [rbp - BPT_POS], r9
    mov edi, 3
    call tuple_new
    mov rbx, rax
    mov rdi, [rbp - BPT_SELF]
    mov rsi, [rbp - BPT_POS]
    call bytes_from_data
    mov rcx, [rbx + PyTupleObject.ob_item]
    mov [rcx], rax
    mov rdi, [rbp - BPT_SEP]
    mov rsi, [rbp - BPT_NLEN]
    call bytes_from_data
    mov rcx, [rbx + PyTupleObject.ob_item]
    mov [rcx + 8], rax
    mov r9, [rbp - BPT_POS]
    mov rdi, [rbp - BPT_SELF]
    add rdi, r9
    add rdi, [rbp - BPT_NLEN]
    mov rsi, [rbp - BPT_SLEN]
    sub rsi, r9
    sub rsi, [rbp - BPT_NLEN]
    call bytes_from_data
    mov rcx, [rbx + PyTupleObject.ob_item]
    mov [rcx + 16], rax
    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.bpt_missing:
    ; partition puts the whole string first, rpartition puts it last.
    mov edi, 3
    call tuple_new
    mov rbx, rax
    mov rdi, [rbp - BPT_SELF]
    mov rsi, [rbp - BPT_SLEN]
    call bytes_from_data
    mov rcx, [rbx + PyTupleObject.ob_item]
    cmp qword [rbp - BPT_RIGHT], 0
    jne .bpt_missing_right
    mov [rcx], rax
    mov r8, 8
    jmp .bpt_missing_fill
.bpt_missing_right:
    mov [rcx + 16], rax
    xor r8d, r8d
.bpt_missing_fill:
    push r8
    xor edi, edi
    xor esi, esi
    call bytes_from_data
    pop r8
    mov rcx, [rbx + PyTupleObject.ob_item]
    mov [rcx + r8], rax
    push r8
    xor edi, edi
    xor esi, esi
    call bytes_from_data
    pop r8
    mov rcx, [rbx + PyTupleObject.ob_item]
    add r8, 8
    mov [rcx + r8], rax
    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.bpt_empty_sep:
    RAISE exc_ValueError_type, "empty separator"
.bpt_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
.bpt_error:
    RAISE exc_TypeError_type, "partition() takes exactly one argument"
END_FUNC bytes_partition_impl

DEF_FUNC_BARE bytes_method_partition
    xor edx, edx
    jmp bytes_partition_impl
END_FUNC bytes_method_partition

DEF_FUNC_BARE bytes_method_rpartition
    mov edx, 1
    jmp bytes_partition_impl
END_FUNC bytes_method_rpartition

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
BR_NEWLEN equ 56
BR_FRAME  equ 72            ; + 5 pushes = 112, 16-aligned -- replace reaches
                            ; glibc through ap_malloc

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

    ; The three slots hold (pointer, length) from here on: a bytearray
    ; argument keeps its bytes out of line, so reading them with a bytes
    ; layout found the capacity and the header instead.
    mov rdi, rax
    call bytes_like_ptr_len
    test ecx, ecx
    jz .br_type
    mov [rbp - BR_SELF], rax
    mov r14, r10                            ; self_len
    mov rdi, [rbp - BR_OLD]
    call bytes_like_ptr_len
    test ecx, ecx
    jz .br_type
    mov [rbp - BR_OLD], rax
    mov r15, r10                            ; old_len
    mov rdi, [rbp - BR_NEW]
    call bytes_like_ptr_len
    test ecx, ecx
    jz .br_type
    mov [rbp - BR_NEW], rax
    mov [rbp - BR_NEWLEN], r10

    ; rbx=self data, r12=old data, r13=new data
    mov rbx, [rbp - BR_SELF]
    mov r12, [rbp - BR_OLD]
    mov r13, [rbp - BR_NEW]

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
    add rdi, rcx
    mov rsi, [rbp - BR_OLD]
    mov rdx, r15
    call ap_memcmp
    pop rcx
    test eax, eax
    jnz .br_no_match

    ; Match found at rcx — ensure buffer space
    mov rax, [rbp - BR_WPOS]
    add rax, [rbp - BR_NEWLEN]
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
    mov rax, [rbp - BR_NEWLEN]
    test rax, rax
    jz .br_skip_new
    push rcx
    push rax
    mov rdi, [rbp - BR_BUF]
    add rdi, [rbp - BR_WPOS]
    mov rsi, [rbp - BR_NEW]
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
    movzx eax, byte [rax + rcx]
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
    mov rdi, rbx
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

.br_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
.br_error:
    RAISE exc_TypeError_type, "replace() takes exactly 2 arguments"
END_FUNC bytes_method_replace

;; ============================================================================
;; bytes_split_impl(rdi = args, rsi = nargs, edx = from_right) -> list of bytes
;; nargs==1: split by whitespace; nargs==2: split by separator bytes
;;
;; One implementation for split and rsplit, the shape str_split_impl already
;; has.  The two differ only in which end maxsplit counts from, so the
;; right-hand arms scan backwards and insert each piece at the front.
;; ============================================================================
BSP_SEPLEN equ 8
BSP_MAX    equ 16           ; splits left, or negative for "no limit"
BSP_RIGHT  equ 24           ; 1 for rsplit
BSP_FRAME  equ 40           ; + 5 pushes = 80, 16-aligned

DEF_FUNC bytes_split_impl, BSP_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov [rbp - BSP_RIGHT], rdx

    ; rbx and r15 hold DATA POINTERS, not objects: a bytearray keeps its bytes
    ; out of line, so reading them through a bytes layout found the header.
    mov r14, rsi                ; nargs
    push rdi
    sub rsp, 8
    mov rdi, [rdi]
    call bytes_like_ptr_len
    add rsp, 8
    pop rdi
    test ecx, ecx
    jz .bsp_type
    mov rbx, rax                ; self data
    mov r12, r10                ; self_len

    ; maxsplit, argument three.  It was never read: `cmp r14, 2 / jl` chose
    ; between whitespace and separator mode and nothing else looked at nargs,
    ; so b'a,b,,c'.split(b',', 1) answered four pieces.  A negative value, and
    ; the default, mean no limit.
    mov qword [rbp - BSP_MAX], -1
    cmp r14, 3
    jl .bsp_have_max
    push rdi
    sub rsp, 8
    mov rdi, [rdi + 16]         ; args[2]
    V_UNPACK rdi, rdx
    extern obj_as_index
    call obj_as_index
    add rsp, 8
    pop rdi
    mov [rbp - BSP_MAX], rax
.bsp_have_max:

    cmp r14, 2
    jl .bsp_no_sep
    ; sep=None asks for whitespace mode, which bytes_like_ptr_len below would
    ; refuse: b'a b'.split(None, 1) was a TypeError.
    mov rax, [rdi + 8]
    LOAD_NONE rcx
    cmp rax, rcx
    je .bsp_no_sep

    ; Separator mode
    push r12
    sub rsp, 8
    mov rdi, [rdi + 8]
    call bytes_like_ptr_len
    add rsp, 8
    pop r12
    test ecx, ecx
    jz .bsp_type
    mov r15, rax                ; separator data
    mov [rbp - BSP_SEPLEN], r10 ; the slot, not rbp-8: that is the saved rbx
    jmp .bsp_by_sep

.bsp_no_sep:
    ; Split by whitespace

    mov rdi, 8
    call list_new
    mov r13, rax                ; result list

    cmp qword [rbp - BSP_RIGHT], 0
    jne .bsp_wsr_loop

    xor ecx, ecx
.bsp_ws_scan:
    cmp rcx, r12
    jge .bsp_ws_done
    movzx eax, byte [rbx + rcx]
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
    cmp qword [rbp - BSP_MAX], 0
    jne .bsp_ws_wordscan
    mov rcx, r12                ; no splits left: the rest is the last piece,
    jmp .bsp_ws_wordend         ; internal whitespace and all
.bsp_ws_wordscan:
    inc rcx
    cmp rcx, r12
    jge .bsp_ws_wordend
    movzx eax, byte [rbx + rcx]
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
    mov rdi, rbx
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
    cmp qword [rbp - BSP_MAX], 0
    jl .bsp_ws_scan             ; negative: no limit, never counts down
    dec qword [rbp - BSP_MAX]
    jmp .bsp_ws_scan

.bsp_ws_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bsp_by_sep:
    mov r14, [rbp - BSP_SEPLEN]              ; sep_len

    mov rdi, 8
    call list_new
    mov r13, rax                ; result list

    test r14, r14
    jz .bsp_empty_sep

    cmp qword [rbp - BSP_RIGHT], 0
    jne .bsp_sepr_scan_init

    ; r11 = segment start, rcx = scan position
    xor ecx, ecx
    xor r11d, r11d              ; segment start = 0

.bsp_sep_scan:
    cmp qword [rbp - BSP_MAX], 0
    je .bsp_sep_tail            ; no splits left: the rest is one piece
    ; Check if enough bytes remain for separator
    mov rax, r12
    sub rax, rcx
    cmp rax, r14
    jl .bsp_sep_tail

    ; memcmp at scan position
    push rcx
    push r11
    mov rdi, rbx
    add rdi, rcx
    mov rsi, r15
    mov rdx, r14
    call ap_memcmp
    pop r11
    pop rcx
    test eax, eax
    jnz .bsp_sep_nomatch

    ; Found separator at rcx — extract segment [r11..rcx)
    push rcx
    push r11
    mov rdi, rbx
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
    cmp qword [rbp - BSP_MAX], 0
    jl .bsp_sep_scan           ; negative: no limit, never counts down
    dec qword [rbp - BSP_MAX]
    jmp .bsp_sep_scan

.bsp_sep_nomatch:
    inc rcx
    jmp .bsp_sep_scan

.bsp_sep_tail:
    ; Remaining segment from r11 to end
    mov rdi, rbx
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
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bsp_sepr_scan_init:
    mov r11, r12                ; segment end, exclusive

.bsp_sepr_scan:
    cmp qword [rbp - BSP_MAX], 0
    je .bsp_sepr_head           ; no splits left: the head is one piece
    mov rcx, r11
    sub rcx, r14                ; the last position a separator could start at
    js .bsp_sepr_head

.bsp_sepr_probe:
    cmp rcx, 0
    jl .bsp_sepr_head
    push rcx
    push r11
    mov rdi, rbx
    add rdi, rcx
    mov rsi, r15
    mov rdx, r14
    call ap_memcmp
    pop r11
    pop rcx
    test eax, eax
    jz .bsp_sepr_found
    dec rcx
    jmp .bsp_sepr_probe

.bsp_sepr_found:
    ; the piece after this separator: [rcx + sep_len, r11)
    push rcx
    push r11
    mov rdi, rbx
    add rdi, rcx
    add rdi, r14
    mov rsi, r11
    sub rsi, rcx
    sub rsi, r14
    call .bsp_emit_front
    pop r11
    pop rcx
    mov r11, rcx                ; the next piece ends where this one started
    cmp qword [rbp - BSP_MAX], 0
    jl .bsp_sepr_scan           ; negative: no limit, never counts down
    dec qword [rbp - BSP_MAX]
    jmp .bsp_sepr_scan

.bsp_sepr_head:
    mov rdi, rbx
    mov rsi, r11
    call .bsp_emit_front
    jmp .bsp_split_done

.bsp_wsr_loop:
    ; rsplit() with no separator: the same pieces, but maxsplit counts from
    ; the right, so collect from the right and insert at the front.
    ; r12 is the length here and doubles as the scan position.
    test r12, r12
    jle .bsp_split_done
    movzx eax, byte [rbx + r12 - 1]
    call .bsp_is_space
    test eax, eax
    jz .bsp_wsr_piece
    dec r12
    jmp .bsp_wsr_loop

.bsp_wsr_piece:
    cmp qword [rbp - BSP_MAX], 0
    jne .bsp_wsr_scan
    ; No splits left: everything to the left is the first piece, internal
    ; whitespace and all -- b' a b '.rsplit(None, 1) is [b' a', b'b'].
    xor r15d, r15d
    jmp .bsp_wsr_emit

.bsp_wsr_scan:
    mov r15, r12                ; word start, scanning left
.bsp_wsr_find:
    test r15, r15
    jle .bsp_wsr_emit
    movzx eax, byte [rbx + r15 - 1]
    call .bsp_is_space
    test eax, eax
    jnz .bsp_wsr_emit
    dec r15
    jmp .bsp_wsr_find

.bsp_wsr_emit:
    mov rdi, rbx
    add rdi, r15
    mov rsi, r12
    sub rsi, r15
    call .bsp_emit_front
    mov r12, r15
    cmp qword [rbp - BSP_MAX], 0
    jl .bsp_wsr_loop            ; negative: no limit, never counts down
    dec qword [rbp - BSP_MAX]
    jmp .bsp_wsr_loop

.bsp_split_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

;; .bsp_is_space(al = a byte) -> eax = 1 when it is one of the four bytes the
;; forward scan treats as whitespace.
.bsp_is_space:
    cmp al, ' '
    je .bsp_is_space_yes
    cmp al, 9
    je .bsp_is_space_yes
    cmp al, 10
    je .bsp_is_space_yes
    cmp al, 13
    je .bsp_is_space_yes
    xor eax, eax
    ret
.bsp_is_space_yes:
    mov eax, 1
    ret

;; .bsp_emit_front(rdi = data, rsi = length) -- build a bytes and put it at
;; the front of the list in r13, through list.insert's own args interface.
.bsp_emit_front:
    call bytes_from_data
    sub rsp, 32
    mov [rsp], r13                  ; args[0] = the list
    mov rcx, [rel v_int_bias]       ; the Value for 0
    mov [rsp + 8], rcx
    mov [rsp + 16], rax
    push rax
    lea rdi, [rsp + 8]
    mov rsi, 3
    extern list_method_insert
    call list_method_insert
    pop rdi
    add rsp, 32
    call obj_decref
    ret

.bsp_empty_sep:
    RAISE exc_ValueError_type, "empty separator"

.bsp_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
END_FUNC bytes_split_impl

;; ============================================================================
;; bytes_method_split(args, nargs) / bytes_method_rsplit(args, nargs)
;; ============================================================================
DEF_FUNC_BARE bytes_method_split
    xor edx, edx                ; scan from the left
    jmp bytes_split_impl
END_FUNC bytes_method_split

DEF_FUNC_BARE bytes_method_rsplit
    mov edx, 1                  ; scan from the right
    jmp bytes_split_impl
END_FUNC bytes_method_rsplit

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
BJ_FRAME  equ 64            ; + 5 pushes = 104, not 16-aligned

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
    mov rdi, [rbp - BJ_SEP]
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bj_item_error
    mov r14, r10                            ; sep_len

    xor r13d, r13d              ; total = 0
    xor ecx, ecx               ; index = 0
.bj_len_loop:
    cmp rcx, r12
    jge .bj_len_done
    mov rax, [rbp - BJ_LIST]
    mov rax, [rax + PyListObject.ob_item]
    mov rax, [rax + rcx * 8]  ; item Value (8-byte stride)
    ; Each item must really be bytes-like: its length is read and its data
    ; copied, so a str item produced garbage rather than TypeError -- and a
    ; bytearray item, whose bytes are out of line, produced garbage too.
    push rcx
    push r12
    push r13
    sub rsp, 8
    mov rdi, rax
    call bytes_like_ptr_len
    mov r9d, ecx                ; the answer, before the pops take rcx back
    add rsp, 8
    pop r13
    pop r12
    pop rcx
    test r9d, r9d
    jz .bj_bad_item
    add r13, r10
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
    mov rdi, [rbp - BJ_SEP]
    call bytes_like_ptr_len
    mov rcx, r10
    test rcx, rcx
    jz .bj_no_sep
    push rcx
    mov rsi, rax
    mov rdi, [rbp - BJ_BUF]
    add rdi, [rbp - BJ_WPOS]
    mov rdx, rcx
    call ap_memcpy
    pop rcx
    add [rbp - BJ_WPOS], rcx
.bj_no_sep:
    ; Copy item bytes
    mov rax, [rbp - BJ_LIST]
    mov rax, [rax + PyListObject.ob_item]
    mov rdi, [rax + r15 * 8]  ; item Value (8-byte stride)
    call bytes_like_ptr_len
    mov rcx, r10
    test rcx, rcx
    jz .bj_next_item
    push rcx
    mov rsi, rax
    mov rdi, [rbp - BJ_BUF]
    add rdi, [rbp - BJ_WPOS]
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
    RAISE exc_TypeError_type, "join() argument must be a list of bytes"

.bj_bad_item:
    ; CPython names the position and the type, which is the whole difference
    ; between "one of these is wrong" and "the second one is a str".
    mov [rbp - BJ_TOTAL], rcx   ; the index, free until the lengths are summed
    mov rax, [rbp - BJ_LIST]
    mov rax, [rax + PyListObject.ob_item]
    mov rcx, [rbp - BJ_TOTAL]
    mov rax, [rax + rcx*8]
    ; The NAME, not the object: BJ_RELEASE_TMP frees the converted list, and
    ; for `b"".join("ab")` that list holds the only reference to the item --
    ; reading its type afterwards reads freed memory.  A type outlives it.
    V_TEST_PTR rax, rdx
    ja .bj_name_int
    test rax, rax
    jz .bj_name_int
    mov rax, [rax + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_name]
    jmp .bj_name_have
.bj_name_int:
    lea rax, [rel bj_name_int]
.bj_name_have:
    mov [rbp - BJ_WPOS], rax
    BJ_RELEASE_TMP
    lea rdi, [rel bj_msg_item]
    mov rsi, [rbp - BJ_TOTAL]
    call bj_append_i64
    mov rdi, rax
    lea rsi, [rel bj_msg_expected]
    call bj_append_cstr
    mov rdi, rax
    mov rsi, [rbp - BJ_WPOS]
    call bj_append_typename
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel bj_msgbuf]
    call raise_exception
    ud2

.bj_item_error:
    BJ_RELEASE_TMP
    RAISE exc_TypeError_type, "sequence item: expected a bytes-like object"
END_FUNC bytes_method_join

;; The three pieces of join's message, kept apart from the scan loop so that
;; the loop stays a loop.
DEF_FUNC_LOCAL bj_append_cstr   ; (rdi = dest, rsi = src) -> rax = the NUL
    xor ecx, ecx
.bac_loop:
    cmp rcx, 100
    jge .bac_done
    mov al, [rsi + rcx]
    test al, al
    jz .bac_done
    mov [rdi + rcx], al
    inc rcx
    jmp .bac_loop
.bac_done:
    lea rax, [rdi + rcx]
    mov byte [rax], 0
    leave
    ret
END_FUNC bj_append_cstr

DEF_FUNC_LOCAL bj_append_i64    ; (rdi = prefix cstr, rsi = n) -> rax = the NUL
    push rbx
    push r12
    sub rsp, 8
    mov r12, rsi
    mov rsi, rdi
    lea rdi, [rel bj_msgbuf]
    call bj_append_cstr
    mov rbx, rax
    mov rax, r12
    lea r8, [rel bj_numbuf + 24]
    mov byte [r8], 0
    mov r9, 10
.bai_loop:
    xor edx, edx
    div r9
    dec r8
    add dl, '0'
    mov [r8], dl
    test rax, rax
    jnz .bai_loop
    mov rdi, rbx
    mov rsi, r8
    call bj_append_cstr
    add rsp, 8
    pop r12
    pop rbx
    leave
    ret
END_FUNC bj_append_i64

DEF_FUNC_LOCAL bj_append_typename   ; (rdi = dest, rsi = a type name) -> rax
    call bj_append_cstr
    mov rdi, rax
    lea rsi, [rel bj_msg_found]
    call bj_append_cstr
    leave
    ret
END_FUNC bj_append_typename

section .rodata

bj_msg_item:     db "sequence item ", 0
bj_msg_expected: db ": expected a bytes-like object, ", 0
bj_msg_found:    db " found", 0
bj_name_int:     db "int", 0
baf_name_startswith: db "startswith", 0
baf_name_endswith:   db "endswith", 0
baf_msg_first:   db " first arg must be bytes or a tuple of bytes, not ", 0
baf_msg_args:    db "() takes exactly one argument", 0
baf_msg_item:    db "a bytes-like object is required, not ", 0

section .bss
bj_msgbuf: resb 192
bj_numbuf: resb 32

section .rodata
empty_str_cstr: db 0

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
DEF_FUNC bytearray_from_bytes
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

DEF_FUNC ba_shared_hex
    lea rdx, [rel bytes_method_hex]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_hex

DEF_FUNC ba_shared_startswith
    lea rdx, [rel bytes_method_startswith]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_startswith

DEF_FUNC ba_shared_endswith
    lea rdx, [rel bytes_method_endswith]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_endswith

DEF_FUNC ba_shared_count
    lea rdx, [rel bytes_method_count]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_count

DEF_FUNC ba_shared_find
    lea rdx, [rel bytes_method_find]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_find

DEF_FUNC ba_shared_decode
    lea rdx, [rel _bytes_decode_impl]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_decode

DEF_FUNC ba_shared_replace
    lea rdx, [rel bytes_method_replace]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_replace

DEF_FUNC ba_shared_split
    lea rdx, [rel bytes_method_split]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_split

DEF_FUNC ba_shared_rsplit
    lea rdx, [rel bytes_method_rsplit]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rsplit

DEF_FUNC ba_shared_rfind
    lea rdx, [rel bytes_method_rfind]
    xor ecx, ecx
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rfind

DEF_FUNC ba_shared_index
    lea rdx, [rel bytes_method_index]
    xor ecx, ecx
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_index

DEF_FUNC ba_shared_rindex
    lea rdx, [rel bytes_method_rindex]
    xor ecx, ecx
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rindex

DEF_FUNC ba_shared_strip
    lea rdx, [rel bytes_method_strip]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_strip

DEF_FUNC ba_shared_lstrip
    lea rdx, [rel bytes_method_lstrip]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_lstrip

DEF_FUNC ba_shared_rstrip
    lea rdx, [rel bytes_method_rstrip]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rstrip

DEF_FUNC ba_shared_partition
    lea rdx, [rel bytes_method_partition]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_partition

DEF_FUNC ba_shared_rpartition
    lea rdx, [rel bytes_method_rpartition]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rpartition

DEF_FUNC ba_shared_join
    lea rdx, [rel bytes_method_join]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_join

;; The slots, reachable by name.  __setitem__ and __delitem__ especially:
;; CPython's own code calls them directly, and `del b[i]` compiles to
;; DELETE_SUBSCR but `b.__delitem__(i)` does not.
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
;; ============================================================================

DEF_FUNC ba_shared_upper
    extern bytes_method_upper
    lea rdx, [rel bytes_method_upper]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_upper

DEF_FUNC ba_shared_lower
    extern bytes_method_lower
    lea rdx, [rel bytes_method_lower]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_lower

DEF_FUNC ba_shared_swapcase
    extern bytes_method_swapcase
    lea rdx, [rel bytes_method_swapcase]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_swapcase

DEF_FUNC ba_shared_capitalize
    extern bytes_method_capitalize
    lea rdx, [rel bytes_method_capitalize]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_capitalize

DEF_FUNC ba_shared_title
    extern bytes_method_title
    lea rdx, [rel bytes_method_title]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_title

DEF_FUNC ba_shared_isalpha
    extern bytes_method_isalpha
    lea rdx, [rel bytes_method_isalpha]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isalpha

DEF_FUNC ba_shared_isdigit
    extern bytes_method_isdigit
    lea rdx, [rel bytes_method_isdigit]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isdigit

DEF_FUNC ba_shared_isspace
    extern bytes_method_isspace
    lea rdx, [rel bytes_method_isspace]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isspace

DEF_FUNC ba_shared_isalnum
    extern bytes_method_isalnum
    lea rdx, [rel bytes_method_isalnum]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isalnum

DEF_FUNC ba_shared_isascii
    extern bytes_method_isascii
    lea rdx, [rel bytes_method_isascii]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isascii

DEF_FUNC ba_shared_isupper
    extern bytes_method_isupper
    lea rdx, [rel bytes_method_isupper]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_isupper

DEF_FUNC ba_shared_islower
    extern bytes_method_islower
    lea rdx, [rel bytes_method_islower]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_islower

DEF_FUNC ba_shared_istitle
    extern bytes_method_istitle
    lea rdx, [rel bytes_method_istitle]
    mov ecx, 0
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_istitle

DEF_FUNC ba_shared_ljust
    extern bytes_method_ljust
    lea rdx, [rel bytes_method_ljust]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_ljust

DEF_FUNC ba_shared_rjust
    extern bytes_method_rjust
    lea rdx, [rel bytes_method_rjust]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_rjust

DEF_FUNC ba_shared_center
    extern bytes_method_center
    lea rdx, [rel bytes_method_center]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_center

DEF_FUNC ba_shared_zfill
    extern bytes_method_zfill
    lea rdx, [rel bytes_method_zfill]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_zfill

DEF_FUNC ba_shared_expandtabs
    extern bytes_method_expandtabs
    lea rdx, [rel bytes_method_expandtabs]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_expandtabs

DEF_FUNC ba_shared_translate
    extern bytes_method_translate
    lea rdx, [rel bytes_method_translate]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_translate

DEF_FUNC ba_shared_splitlines
    extern bytes_method_splitlines
    lea rdx, [rel bytes_method_splitlines]
    mov ecx, 2
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_splitlines

DEF_FUNC ba_shared_removeprefix
    extern bytes_method_removeprefix
    lea rdx, [rel bytes_method_removeprefix]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_removeprefix

DEF_FUNC ba_shared_removesuffix
    extern bytes_method_removesuffix
    lea rdx, [rel bytes_method_removesuffix]
    mov ecx, 1
    leave
    jmp bytearray_shared_call
END_FUNC ba_shared_removesuffix
