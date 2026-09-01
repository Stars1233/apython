; methods/bytes.asm - bytes and bytearray methods
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
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
;; bytes_method_startswith(args, nargs) -> Bool
;; args[0]=self (bytes), args[1]=prefix (bytes)
;; ============================================================================
BSW_SELF equ 8
BSW_SLEN equ 16
BSW_FRAME equ 32            ; + 1 push = 40, not 16-aligned

DEF_FUNC bytes_method_startswith, BSW_FRAME
    cmp rsi, 2
    jne .bsw_error
    push rbx

    ; Both operands go through bytes_like_ptr_len.  Reading data and ob_size
    ; off the argument directly was right only for a bytes: a bytearray keeps
    ; its bytes out of line now, so +24 is its CAPACITY and +32 the pointer,
    ; and the comparison silently ran against the wrong words.
    mov rbx, [rdi + 8]          ; the prefix, kept for the error message
    mov rdi, [rdi]
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bsw_self_type
    mov [rbp - BSW_SELF], rax
    mov [rbp - BSW_SLEN], r10

    mov rdi, rbx
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bsw_arg_type
    mov rcx, rax                ; prefix data
    mov r9, r10                 ; prefix length
    mov r8, [rbp - BSW_SLEN]

    ; If prefix longer than self: False
    cmp r9, r8
    ja .bsw_false

    ; Compare first r9 bytes
    mov rdi, [rbp - BSW_SELF]
    mov rsi, rcx
    mov rdx, r9
    test rdx, rdx
    jz .bsw_true                ; empty prefix always matches
    call ap_memcmp
    test eax, eax
    jnz .bsw_false

.bsw_true:
    mov eax, 1
    RET_BOOL_RAX
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bsw_false:
    xor eax, eax
    RET_BOOL_RAX
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bsw_self_type:
    pop rbx
    RAISE exc_TypeError_type, "startswith() requires a bytes-like object"
.bsw_arg_type:
    pop rbx
    RAISE exc_TypeError_type, "startswith first arg must be bytes or a tuple of bytes, not str"
.bsw_error:
    RAISE exc_TypeError_type, "startswith() takes exactly one argument"
END_FUNC bytes_method_startswith

;; ============================================================================
;; bytes_method_endswith(args, nargs) -> Bool
;; args[0]=self (bytes), args[1]=suffix (bytes)
;; ============================================================================
BEW_SELF equ 8
BEW_SLEN equ 16
BEW_FRAME equ 32            ; + 1 push = 40, not 16-aligned

DEF_FUNC bytes_method_endswith, BEW_FRAME
    cmp rsi, 2
    jne .bew_error
    push rbx

    mov rbx, [rdi + 8]          ; the suffix
    mov rdi, [rdi]
    call bytes_like_ptr_len     ; see startswith: a bytearray's bytes are not
    test ecx, ecx               ; where a bytes keeps them
    jz .bew_self_type
    mov [rbp - BEW_SELF], rax
    mov [rbp - BEW_SLEN], r10

    mov rdi, rbx
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bew_arg_type
    mov rcx, rax
    mov r9, r10
    mov r8, [rbp - BEW_SLEN]

    ; If suffix longer than self: False
    cmp r9, r8
    ja .bew_false

    ; Compare last r9 bytes
    mov rdx, r8
    sub rdx, r9                             ; offset = self_len - suffix_len
    mov rdi, [rbp - BEW_SELF]
    add rdi, rdx
    mov rsi, rcx
    mov rdx, r9
    test rdx, rdx
    jz .bew_true                ; empty suffix always matches
    call ap_memcmp
    test eax, eax
    jnz .bew_false

.bew_true:
    mov eax, 1
    RET_BOOL_RAX
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bew_false:
    xor eax, eax
    RET_BOOL_RAX
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bew_self_type:
    pop rbx
    RAISE exc_TypeError_type, "endswith() requires a bytes-like object"
.bew_arg_type:
    pop rbx
    RAISE exc_TypeError_type, "endswith first arg must be bytes or a tuple of bytes, not str"
.bew_error:
    RAISE exc_TypeError_type, "endswith() takes exactly one argument"
END_FUNC bytes_method_endswith

;; ============================================================================
;; bytes_method_count(args, nargs) -> SmallInt
;; args[0]=self (bytes), args[1]=sub (bytes)
;; Count non-overlapping occurrences of sub in self.
;; ============================================================================
BC_SELF   equ 8
BC_SUB    equ 16
BC_ONE    equ 56            ; a one-byte bytes header, for an int needle
BC_FRAME  equ 64            ; + 0 pushes = 64

DEF_FUNC bytes_method_count, BC_FRAME
    cmp rsi, 2
    jne .bc_error

    mov rax, [rdi]              ; self
    mov rcx, [rdi + 8]         ; sub
    mov [rbp - BC_SELF], rax
    mov [rbp - BC_SUB], rcx
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

.bc_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
.bc_error:
    RAISE exc_TypeError_type, "count() takes exactly one argument"
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
BF_FRAME  equ BF_ONE + 24   ; + 0 pushes = 96

DEF_FUNC bytes_method_find, BF_FRAME
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

    ; An empty needle is found at the start position.
    test r9, r9
    jz .bf_found_at_start

    cmp r11, r8
    ja .bf_not_found
    mov rax, r8
    sub rax, r11
    cmp r9, rax
    ja .bf_not_found

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
    mov rax, -1
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

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
BR_NEWLEN equ 56
BR_FRAME  equ 64            ; + 5 pushes = 104, not 16-aligned

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
;; bytes_method_split(args, nargs) -> list of bytes
;; nargs==1: split by whitespace; nargs==2: split by separator bytes
;; ============================================================================
BSP_SEPLEN equ 8
BSP_FRAME  equ 8            ; + 5 pushes = 48, 16-aligned

DEF_FUNC bytes_method_split, BSP_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

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

    cmp r14, 2
    jl .bsp_no_sep

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

.bsp_empty_sep:
    RAISE exc_ValueError_type, "empty separator"

.bsp_type:
    RAISE exc_TypeError_type, "a bytes-like object is required"
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
