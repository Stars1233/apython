; methods/str.asm - str: case, strip, search, replace, join, split, format
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
extern str_cp_width
extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_memcpy
extern ap_memcmp
extern obj_decref
extern obj_repr
extern obj_str
extern str_from_cstr_heap
extern str_new_heap
extern list_new
extern list_append
extern obj_as_index
extern list_type
extern tuple_type
extern dict_get
extern none_singleton
extern bool_true
extern bool_false
extern int_from_i64
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_IndexError_type
extern exc_KeyError_type
extern kw_names_pending
extern obj_dealloc
extern str_type

; Set entry layout constants (must match set.asm)

; --- moved to a sibling file by the split ---
extern list_method_insert

section .text

;; ############################################################################
;;                         STRING METHODS
;; ############################################################################

;; ============================================================================
;; str_method_upper(args, nargs) -> new uppercase string
;; args[0] = self (PyStrObject*)
;; ============================================================================
DEF_FUNC str_method_upper
    push rbx
    push r12
    push r13

    mov rax, [rdi]          ; self = args[0]
    mov rbx, rax            ; rbx = self
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = length

    ; Create new string: str_new(data, len)
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new_heap
    mov r13, rax            ; r13 = new string

    ; Convert each byte to uppercase in-place
    xor ecx, ecx
.upper_loop:
    cmp rcx, r12
    jge .upper_done
    movzx eax, byte [r13 + PyStrObject.data + rcx]
    cmp al, 'a'
    jb .upper_next
    cmp al, 'z'
    ja .upper_next
    sub al, 32             ; 'a'-'A' = 32
    mov [r13 + PyStrObject.data + rcx], al
.upper_next:
    inc rcx
    jmp .upper_loop
.upper_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_upper

;; ============================================================================
;; str_method_lower(args, nargs) -> new lowercase string
;; ============================================================================
DEF_FUNC str_method_lower
    push rbx
    push r12
    push r13

    mov rax, [rdi]          ; self
    mov rbx, rax
    mov r12, [rbx + PyStrObject.ob_size]

    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new_heap
    mov r13, rax

    xor ecx, ecx
.lower_loop:
    cmp rcx, r12
    jge .lower_done
    movzx eax, byte [r13 + PyStrObject.data + rcx]
    cmp al, 'A'
    jb .lower_next
    cmp al, 'Z'
    ja .lower_next
    add al, 32
    mov [r13 + PyStrObject.data + rcx], al
.lower_next:
    inc rcx
    jmp .lower_loop
.lower_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_lower

;; ============================================================================
;; ============================================================================
;; strip_char_matches(dil = byte, rsi = chars data or 0, rdx = chars len)
;;   -> eax = 1 when the byte should be stripped
;;
;; With no chars argument the set is whitespace, as before.  The argument was
;; accepted and then ignored outright, so "xxaxx".strip("x") returned the
;; string unchanged.
;; ============================================================================
DEF_FUNC_BARE strip_char_matches
    test rsi, rsi
    jz .scm_whitespace
    xor ecx, ecx
.scm_loop:
    cmp rcx, rdx
    jge .scm_no
    cmp dil, [rsi + rcx]
    je .scm_yes
    inc rcx
    jmp .scm_loop

.scm_whitespace:
    cmp dil, ' '
    je .scm_yes
    cmp dil, 9                  ; tab
    je .scm_yes
    cmp dil, 10                 ; newline
    je .scm_yes
    cmp dil, 13                 ; carriage return
    je .scm_yes
    cmp dil, 11                 ; vertical tab
    je .scm_yes
    cmp dil, 12                 ; form feed
    je .scm_yes
.scm_no:
    xor eax, eax
    ret
.scm_yes:
    mov eax, 1
    ret
END_FUNC strip_char_matches

;; ============================================================================
;; str_strip_impl(rdi = args, rsi = nargs, edx = mode) -> Value
;; mode: bit 0 = strip the left, bit 1 = strip the right.
;; ============================================================================
SSI_CHARS equ 8
SSI_CLEN  equ 16
SSI_MODE  equ 24
SSI_FRAME equ 32            ; + 4 pushes = 64

DEF_FUNC_LOCAL str_strip_impl, SSI_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov [rbp - SSI_MODE], rdx
    mov qword [rbp - SSI_CHARS], 0
    mov qword [rbp - SSI_CLEN], 0

    mov rbx, [rdi]              ; self
    mov r12, [rbx + PyStrObject.ob_size]

    cmp rsi, 2
    jl .ssi_have_chars
    mov rax, [rdi + 8]          ; the chars argument
    V_TEST_PTR rax, rcx
    ja .ssi_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .ssi_type_error
    lea rcx, [rax + PyStrObject.data]
    mov [rbp - SSI_CHARS], rcx
    mov rcx, [rax + PyStrObject.ob_size]
    mov [rbp - SSI_CLEN], rcx

.ssi_have_chars:
    xor r13d, r13d              ; start
    mov r14, r12                ; end, exclusive

    test qword [rbp - SSI_MODE], 1
    jz .ssi_right
.ssi_left_loop:
    cmp r13, r14
    jge .ssi_make
    movzx edi, byte [rbx + PyStrObject.data + r13]
    mov rsi, [rbp - SSI_CHARS]
    mov rdx, [rbp - SSI_CLEN]
    call strip_char_matches
    test eax, eax
    jz .ssi_right
    inc r13
    jmp .ssi_left_loop

.ssi_right:
    test qword [rbp - SSI_MODE], 2
    jz .ssi_make
.ssi_right_loop:
    cmp r14, r13
    jle .ssi_make
    movzx edi, byte [rbx + PyStrObject.data + r14 - 1]
    mov rsi, [rbp - SSI_CHARS]
    mov rdx, [rbp - SSI_CLEN]
    call strip_char_matches
    test eax, eax
    jz .ssi_make
    dec r14
    jmp .ssi_right_loop

.ssi_make:
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r13
    mov rsi, r14
    sub rsi, r13
    call str_new_heap
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.ssi_type_error:
    RAISE exc_TypeError_type, "strip arg must be None or str"
END_FUNC str_strip_impl

;; ============================================================================
;; str_method_strip(args, nargs) -> new string with both ends stripped
;; args[0]=self, args[1]=chars (optional)
;; ============================================================================
DEF_FUNC_BARE str_method_strip
    mov edx, 3
    jmp str_strip_impl
END_FUNC str_method_strip

;; ============================================================================
;; str_method_startswith(args, nargs) -> bool_true/bool_false
;; args[0]=self, args[1]=prefix
;; ============================================================================
;; ============================================================================
;; str_affix_dispatch(rdi = args, rsi = nargs, rdx = single-affix function)
;;   -> True when any element of a tuple argument matches
;;
;; startswith and endswith accept a tuple of candidates in Python.  Only a
;; single str was accepted here, so "He".startswith(("X", "He")) raised
;; TypeError.
;; ============================================================================
SAD_ARGS  equ 8
SAD_NARGS equ 16
SAD_FN    equ 24
SAD_TUP   equ 32
SAD_IDX   equ 40
SAD_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC_LOCAL str_affix_dispatch, SAD_FRAME
    mov [rbp - SAD_ARGS], rdi
    mov [rbp - SAD_NARGS], rsi
    mov [rbp - SAD_FN], rdx

    cmp rsi, 2
    jl .sad_single
    mov rax, [rdi + 8]
    V_TEST_PTR rax, rcx
    ja .sad_single
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    jne .sad_single

    mov [rbp - SAD_TUP], rax
    mov qword [rbp - SAD_IDX], 0

.sad_loop:
    mov rax, [rbp - SAD_TUP]
    mov rcx, [rbp - SAD_IDX]
    cmp rcx, [rax + PyTupleObject.ob_size]
    jge .sad_false

    ; Build (self, candidate) plus any start/stop the caller passed.
    sub rsp, 64
    mov rdx, [rbp - SAD_ARGS]
    mov r8, [rdx]
    mov [rsp], r8                       ; self
    mov r9, [rax + PyTupleObject.ob_item]
    mov r9, [r9 + rcx * 8]
    mov [rsp + 8], r9                   ; the candidate
    mov r10, [rbp - SAD_NARGS]
    cmp r10, 3
    jl .sad_no_extra
    mov r8, [rdx + 16]
    mov [rsp + 16], r8
    cmp r10, 4
    jl .sad_no_extra
    mov r8, [rdx + 24]
    mov [rsp + 24], r8
.sad_no_extra:
    mov rdi, rsp
    mov rsi, [rbp - SAD_NARGS]
    call [rbp - SAD_FN]
    add rsp, 64

    V_UNPACK rax, rdx
    lea rcx, [rel bool_true]
    cmp rax, rcx
    je .sad_true

    inc qword [rbp - SAD_IDX]
    jmp .sad_loop

.sad_single:
    mov rdi, [rbp - SAD_ARGS]
    mov rsi, [rbp - SAD_NARGS]
    call [rbp - SAD_FN]
    leave
    ret

.sad_true:
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret

.sad_false:
    RET_FALSE
    leave
    V_PACK rax, rdx
    ret
END_FUNC str_affix_dispatch

AFF_ARGS  equ 8
AFF_NARGS equ 16
AFF_WPTR  equ 40            ; the 3-word window str_search_window fills
AFF_WLEN  equ 32
AFF_WOFF  equ 24
AFF_FRAME equ 56            ; + 5 pushes = 96
DEF_FUNC_LOCAL str_startswith_one, AFF_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - AFF_ARGS], rdi
    mov [rbp - AFF_NARGS], rsi

    ; Validate args[1] is a string
    mov rax, [rdi + 8]         ; args[1]
    V_TEST_PTR rax, rcx
    ja .sw_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .sw_type_error

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]     ; prefix (args[1])

    mov r13, [r12 + PyStrObject.ob_size]  ; prefix length

    ; start and end narrow the region examined.  They are *code point* indices
    ; and were applied as byte offsets, so "αβαβ".startswith("β", 1) was False:
    ; offset 1 landed inside alpha's two bytes.  str_search_window converts.
    push r14
    push r15
    mov rdi, rbx
    mov rsi, [rbp - AFF_ARGS]
    mov rdx, [rbp - AFF_NARGS]
    lea rcx, [rbp - AFF_WPTR]
    call str_search_window
    test eax, eax
    jz .sw_false_pop
    mov r14, [rbp - AFF_WOFF]       ; window start, in bytes
    mov r15, [rbp - AFF_WLEN]       ; window length, in bytes

    ; The prefix must fit inside the window
    cmp r13, r15
    jg .sw_false_pop

    mov rdi, [rbp - AFF_WPTR]
    lea rsi, [r12 + PyStrObject.data]
    xor ecx, ecx
.sw_cmp:
    cmp rcx, r13
    jge .sw_true_pop
    movzx eax, byte [rdi + rcx]
    cmp al, [rsi + rcx]
    jne .sw_false_pop
    inc rcx
    jmp .sw_cmp

.sw_true_pop:
    pop r15
    pop r14
    jmp .sw_true
.sw_false_pop:
    pop r15
    pop r14
    jmp .sw_false

.sw_true:
    RET_TRUE
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sw_false:
    RET_FALSE
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sw_type_error:
    RAISE exc_TypeError_type, "must be str, not other type"
END_FUNC str_startswith_one

DEF_FUNC_BARE str_method_startswith
    lea rdx, [rel str_startswith_one]
    jmp str_affix_dispatch
END_FUNC str_method_startswith

;; ============================================================================
;; str_method_endswith(args, nargs) -> bool_true/bool_false
;; args[0]=self, args[1]=suffix
;; ============================================================================
DEF_FUNC_LOCAL str_endswith_one, AFF_FRAME
    push rbx
    push r12
    push r13
    push r14
    mov [rbp - AFF_ARGS], rdi
    mov [rbp - AFF_NARGS], rsi

    ; Validate args[1] is a string
    mov rax, [rdi + 8]         ; args[1]
    V_TEST_PTR rax, rcx
    ja .ew_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .ew_type_error

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]     ; suffix
    mov r13, [r12 + PyStrObject.ob_size]  ; suffix length
    mov r14, [rbx + PyStrObject.ob_size]  ; self length

    ; start and end narrow the region examined.  Code point indices, applied as
    ; byte offsets until now -- the same bug startswith had.
    push r15
    mov rdi, rbx
    mov rsi, [rbp - AFF_ARGS]
    mov rdx, [rbp - AFF_NARGS]
    lea rcx, [rbp - AFF_WPTR]
    call str_search_window
    test eax, eax
    jz .ew_false_pop
    mov r14, [rbp - AFF_WPTR]       ; window base pointer
    mov r15, [rbp - AFF_WLEN]       ; window length, in bytes

    ; The suffix must fit inside the window
    cmp r13, r15
    jg .ew_false_pop

    lea rdi, [r14 + r15]
    sub rdi, r13                    ; the window's last suffix_len bytes
    lea rsi, [r12 + PyStrObject.data]
    xor ecx, ecx
.ew_cmp:
    cmp rcx, r13
    jge .ew_true_pop
    movzx eax, byte [rdi + rcx]
    cmp al, [rsi + rcx]
    jne .ew_false_pop
    inc rcx
    jmp .ew_cmp

.ew_true_pop:
    pop r15
    jmp .ew_true
.ew_false_pop:
    pop r15
    jmp .ew_false

.ew_true:
    RET_TRUE
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.ew_false:
    RET_FALSE
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.ew_type_error:
    RAISE exc_TypeError_type, "must be str, not other type"
END_FUNC str_endswith_one

DEF_FUNC_BARE str_method_endswith
    lea rdx, [rel str_endswith_one]
    jmp str_affix_dispatch
END_FUNC str_method_endswith

;; ============================================================================
;; str_method_find(args, nargs) -> SmallInt code point index, or -1
;; args[0]=self, args[1]=substr, args[2]=start, args[3]=end
;;
;; str_find_impl carries find, rfind, index and rindex; they differ only in
;; which direction the scan runs and in what a miss does.  Both of those used
;; to ignore start and end outright -- "abcabc".find("b", 3) was 1 -- and both
;; searched with the C-string ap_strstr, so they also stopped at the first NUL:
;; "a\x00b".find("b") was -1.
;;
;; edx on entry is a bitmask: bit 0 = scan in reverse, bit 1 = raise
;; ValueError on a miss instead of returning -1.  So 0 = find, 1 = rfind,
;; 2 = index, 3 = rindex.
;; ============================================================================
extern str_search_window
extern ap_memfind
extern ap_memrfind
extern str_byte_to_cp

FND_ARGS  equ 8
FND_NARGS equ 16
FND_SELF  equ 24
FND_DIR   equ 32
FND_WPTR  equ 56            ; the 3-word window: 56, 48, 40
FND_WLEN  equ 48
FND_FRAME equ 64            ; + 0 pushes = 64
DEF_FUNC str_find_impl, FND_FRAME
    mov [rbp - FND_ARGS], rdi
    mov [rbp - FND_NARGS], rsi
    mov [rbp - FND_DIR], rdx

    mov rax, [rdi + 8]              ; args[1]
    V_TEST_PTR rax, rcx
    ja .find_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .find_type_error

    mov rcx, [rdi]                  ; self
    mov [rbp - FND_SELF], rcx

    mov rdi, rcx
    mov rsi, [rbp - FND_ARGS]
    mov rdx, [rbp - FND_NARGS]
    lea rcx, [rbp - FND_WPTR]
    call str_search_window
    test eax, eax
    jz .find_not_found

    mov rdi, [rbp - FND_WPTR]
    mov rsi, [rbp - FND_WLEN]
    mov rdx, [rbp - FND_ARGS]
    mov rdx, [rdx + 8]              ; the needle
    mov rcx, [rdx + PyStrObject.ob_size]
    lea rdx, [rdx + PyStrObject.data]
    test qword [rbp - FND_DIR], 1
    jnz .find_reverse
    call ap_memfind
    jmp .find_have_hit
.find_reverse:
    call ap_memrfind
.find_have_hit:
    test rax, rax
    jz .find_not_found

    ; A pointer into self.data; Python wants a code point index.
    mov rdi, [rbp - FND_SELF]
    sub rax, rdi
    sub rax, PyStrObject.data
    mov rsi, rax
    call str_byte_to_cp
    mov rdi, rax
    call int_from_i64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.find_not_found:
    test qword [rbp - FND_DIR], 2
    jnz .find_missing
    mov rdi, -1
    call int_from_i64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.find_missing:
    RAISE exc_ValueError_type, "substring not found"

.find_type_error:
    RAISE exc_TypeError_type, "must be str, not other type"
END_FUNC str_find_impl

DEF_FUNC_BARE str_method_find
    xor edx, edx
    jmp str_find_impl
END_FUNC str_method_find

;; ============================================================================
;; str_method_replace(args, nargs) -> new string with replacements
;; args[0]=self, args[1]=old, args[2]=new
;; Uses callee-saved regs for key state, stack locals for buffer management.
;; ============================================================================
RPL_BUF    equ 48           ; the growing result buffer
RPL_ALLOC  equ 56           ; its allocated size
RPL_WPOS   equ 64           ; how much of it is written
RPL_SLEN   equ 72           ; self's length in bytes
RPL_LEFT   equ 80           ; replacements still allowed, -1 = unlimited
RPL_ARGS   equ 88
RPL_LOCALS equ 56           ; + 5 pushes = 96
DEF_FUNC str_method_replace
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, RPL_LOCALS

    ; Validate args[1] is a string
    mov rax, [rdi + 8]         ; args[1]
    V_TEST_PTR rax, rcx
    ja .repl_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .repl_type_error

    ; Validate args[2] is a string
    mov rax, [rdi + 16]         ; args[2]
    V_TEST_PTR rax, rcx
    ja .repl_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .repl_type_error

    ; The third argument caps how many replacements happen, and was ignored:
    ; "aXbXc".replace("X", "-", 1) replaced both.  A negative count means
    ; unlimited, which is also what its absence means.
    mov qword [rbp - RPL_LEFT], -1
    cmp rsi, 4
    jl .repl_count_done
    mov [rbp - RPL_ARGS], rdi
    mov rdi, [rdi + 24]         ; args[3]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rdi, [rbp - RPL_ARGS]
    test rax, rax
    js .repl_count_done
    mov [rbp - RPL_LEFT], rax
.repl_count_done:

    ; rbx = self, r12 = old_str, r13 = new_str, r14 = self_len, r15 = scan_pos
    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]     ; old
    mov r13, [rdi + 16]     ; new
    mov r14, [rbx + PyStrObject.ob_size]
    mov [rbp - RPL_SLEN], r14

    ; If old_str is empty, interleave new_str between each char
    cmp qword [r12 + PyStrObject.ob_size], 0
    je .replace_interleave

    ; Allocate initial buffer: self_len * 2 + 64
    lea rdi, [r14 * 2 + 64]
    mov [rbp - RPL_ALLOC], rdi       ; buf_alloc
    call ap_malloc
    mov [rbp - RPL_BUF], rax       ; buf_ptr
    mov qword [rbp - RPL_WPOS], 0   ; write_pos = 0

    xor r15d, r15d          ; r15 = scan position

.replace_scan:
    cmp qword [rbp - RPL_LEFT], 0
    je .replace_copy_tail       ; the count is spent; the rest stays as it is

    ; Check if remaining text is long enough for old_str
    mov rax, r14
    sub rax, r15
    cmp rax, [r12 + PyStrObject.ob_size]
    jl .replace_copy_tail

    ; Search for old_str from scan pos.  ap_strstr stopped at the first NUL,
    ; so "a\x00b".replace("b", "!") left the string alone.
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r15
    mov rsi, r14
    sub rsi, r15
    lea rdx, [r12 + PyStrObject.data]
    mov rcx, [r12 + PyStrObject.ob_size]
    call ap_memfind
    test rax, rax
    jz .replace_copy_tail

    ; Found at rax; compute found_pos relative to self.data start
    lea rcx, [rbx + PyStrObject.data]
    sub rax, rcx            ; rax = found_pos (absolute index in self)
    push rax                ; save found_pos on stack

    ; --- ensure buffer space ---
    mov rcx, rax
    sub rcx, r15            ; prefix_len = found_pos - scan_pos
    mov rdx, [rbp - RPL_WPOS]       ; write_pos
    add rdx, rcx
    add rdx, [r13 + PyStrObject.ob_size]
    add rdx, r14            ; generous upper bound for rest
    cmp rdx, [rbp - RPL_ALLOC]
    jl .replace_space_ok
    shl rdx, 1
    mov [rbp - RPL_ALLOC], rdx
    mov rdi, [rbp - RPL_BUF]
    mov rsi, rdx
    call ap_realloc
    mov [rbp - RPL_BUF], rax
.replace_space_ok:

    ; --- copy prefix: bytes from scan_pos to found_pos ---
    pop rax                 ; found_pos
    push rax                ; keep on stack
    mov rcx, rax
    sub rcx, r15            ; prefix_len
    test rcx, rcx
    jz .replace_no_prefix

    mov rdi, [rbp - RPL_BUF]
    add rdi, [rbp - RPL_WPOS]
    lea rsi, [rbx + PyStrObject.data]
    add rsi, r15
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp - RPL_WPOS], rcx

.replace_no_prefix:
    ; --- copy new_str ---
    mov rcx, [r13 + PyStrObject.ob_size]
    test rcx, rcx
    jz .replace_adv

    mov rdi, [rbp - RPL_BUF]
    add rdi, [rbp - RPL_WPOS]
    lea rsi, [r13 + PyStrObject.data]
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp - RPL_WPOS], rcx

.replace_adv:
    cmp qword [rbp - RPL_LEFT], 0
    jl .replace_adv_unlimited
    dec qword [rbp - RPL_LEFT]
.replace_adv_unlimited:
    pop rax                 ; found_pos
    add rax, [r12 + PyStrObject.ob_size]
    mov r15, rax            ; advance scan past old_str
    jmp .replace_scan

.replace_copy_tail:
    ; Copy remaining bytes from scan_pos to end
    mov rcx, r14
    sub rcx, r15
    test rcx, rcx
    jz .replace_make_str

    mov rdi, [rbp - RPL_BUF]
    add rdi, [rbp - RPL_WPOS]
    lea rsi, [rbx + PyStrObject.data]
    add rsi, r15
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp - RPL_WPOS], rcx

.replace_make_str:
    mov rdi, [rbp - RPL_BUF]
    mov rsi, [rbp - RPL_WPOS]       ; result length
    call str_new_heap
    push rax

    mov rdi, [rbp - RPL_BUF]
    call ap_free

    pop rax
    mov edx, TAG_PTR
    add rsp, RPL_LOCALS
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.replace_interleave:
    ; Interleave: insert new_str before each char and after last
    ; Result = new + ch0 + new + ch1 + ... + ch(n-1) + new
    ; Result len = (self_len + 1) * new_len + self_len
    mov rcx, [r13 + PyStrObject.ob_size]  ; new_len
    lea rax, [r14 + 1]         ; self_len + 1
    imul rax, rcx              ; (self_len + 1) * new_len
    add rax, r14               ; + self_len
    add rax, 1                 ; + NUL
    mov [rbp - RPL_ALLOC], rax          ; buf_alloc
    mov rdi, rax
    call ap_malloc
    mov [rbp - RPL_BUF], rax          ; buf_ptr
    mov qword [rbp - RPL_WPOS], 0      ; write_pos = 0

    xor r15d, r15d             ; scan_pos = 0

.ri_loop:
    cmp qword [rbp - RPL_LEFT], 0
    je .ri_tail                 ; the count is spent
    ; Copy new_str
    mov rcx, [r13 + PyStrObject.ob_size]
    test rcx, rcx
    jz .ri_skip_new
    mov rdi, [rbp - RPL_BUF]
    add rdi, [rbp - RPL_WPOS]
    lea rsi, [r13 + PyStrObject.data]
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp - RPL_WPOS], rcx
.ri_skip_new:
    cmp qword [rbp - RPL_LEFT], 0
    jl .ri_unlimited
    dec qword [rbp - RPL_LEFT]
.ri_unlimited:
    ; Check if all chars copied
    cmp r15, r14
    jge .replace_make_str

    ; Copy one *code point* from self, not one byte: inserting between the
    ; bytes of a multi-byte character split it, so "αβ".replace("", "-") came
    ; back as four mojibake halves instead of "-α-β-".  r15 is callee-saved,
    ; so str_cp_width preserves it.
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r14
    mov rdx, r15
    call str_cp_width
    mov rcx, rax                ; the character's width in bytes
    mov rdi, [rbp - RPL_BUF]
    add rdi, [rbp - RPL_WPOS]
    lea rsi, [rbx + PyStrObject.data]
    add rsi, r15
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp - RPL_WPOS], rcx
    add r15, rcx
    jmp .ri_loop

.ri_tail:
    ; The count ran out; everything still unscanned is copied verbatim.
    mov rcx, r14
    sub rcx, r15
    test rcx, rcx
    jz .replace_make_str
    mov rdi, [rbp - RPL_BUF]
    add rdi, [rbp - RPL_WPOS]
    lea rsi, [rbx + PyStrObject.data]
    add rsi, r15
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp - RPL_WPOS], rcx
    jmp .replace_make_str

.replace_copy_self:
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r14
    call str_new_heap
    mov edx, TAG_PTR
    add rsp, RPL_LOCALS
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.repl_type_error:
    RAISE exc_TypeError_type, "must be str, not other type"
END_FUNC str_method_replace

;; ============================================================================
;; str_method_join(args, nargs) -> new string
;; args[0]=self (separator), args[1]=list
;; self.join(iterable)
;; Regs: rbx=self(sep), r12=list, r13=count, r14=sep_len
;; Stack: the SJ_* slots below
;; ============================================================================
extern tuple_type_call

; Release the sequence join() materialised for itself, if it made one.
%macro JOIN_RELEASE_TMP 0
    mov rdi, [rbp - SJ_TMP]
    test rdi, rdi
    jz %%no_tmp
    mov qword [rbp - SJ_TMP], 0
    call obj_decref
%%no_tmp:
%endmacro

SJ_TOTAL equ 48
SJ_BUF   equ 56
SJ_POS   equ 64
SJ_TMP   equ 72         ; materialised sequence, owned, or 0
DEF_FUNC str_method_join
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 48             ; 4 locals + alignment pad

    ; Load separator
    mov r15, rdi             ; save args ptr (r15 free until later)
    mov rbx, [rdi]           ; self (separator)
    INCREF rbx               ; borrow → own
    mov r12, [r15 + 8]       ; args[1] = the sequence Value

    ; The loop below indexes ob_item directly, which only a list or a tuple
    ; has.  join() takes any iterable, so materialise anything else -- that
    ; includes a generator, a set, a str and a dict, all of which used to
    ; read [obj+16] as a count and [obj+32] as an item array.
    mov qword [rbp - SJ_TMP], 0
    V_TEST_PTR_M [r15 + 8], rax
    ja .join_materialise
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel list_type]
    cmp rax, rcx
    je .join_seq_ready
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .join_seq_ready
.join_materialise:
    lea rdi, [rel tuple_type]
    lea rsi, [r15 + 8]
    mov edx, 1
    call tuple_type_call        ; raises for a non-iterable, as CPython does
    mov [rbp - SJ_TMP], rax
    mov r12, rax
.join_seq_ready:

    mov r13, [r12 + PyListObject.ob_size]  ; item count
    mov r14, [rbx + PyStrObject.ob_size]   ; sep length

    ; If list is empty, return empty string
    test r13, r13
    jz .join_empty

    ; First pass: compute total length
    xor r15d, r15d          ; r15 = total data length
    xor ecx, ecx
.join_len_loop:
    cmp rcx, r13
    jge .join_len_done
    push rcx
    mov rax, [r12 + PyListObject.ob_item]
    mov rax, [rax + rcx * 8]    ; payload
    V_UNPACK rax, rsi
    ; Verify element is TAG_PTR and a str
    cmp esi, TAG_PTR
    jne .join_type_error
    mov rdi, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rdi, r8, .join_type_error
    add r15, [rax + PyStrObject.ob_size]
    pop rcx
    inc rcx
    jmp .join_len_loop

.join_len_done:
    ; Add separator lengths: sep_len * (count - 1)
    mov rax, r13
    dec rax
    imul rax, r14
    add r15, rax
    mov [rbp - SJ_TOTAL], r15   ; total_len

    ; Allocate buffer
    lea rdi, [r15 + 8]
    call ap_malloc
    mov [rbp - SJ_BUF], rax     ; buf_ptr
    mov qword [rbp - SJ_POS], 0 ; write_pos = 0

    ; Second pass: copy data
    xor ecx, ecx
.join_copy_loop:
    cmp rcx, r13
    jge .join_make_str
    push rcx

    ; If not the first item, copy separator
    test rcx, rcx
    jz .join_no_sep

    mov rdi, [rbp - SJ_BUF]
    add rdi, [rbp - SJ_POS]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r14
    call ap_memcpy
    add [rbp - SJ_POS], r14

.join_no_sep:
    mov rcx, [rsp]          ; reload index
    mov rax, [r12 + PyListObject.ob_item]
    mov rax, [rax + rcx * 8]    ; item payload

    ; Heap string element
    mov rdx, [rax + PyStrObject.ob_size]
    push rdx                ; save item_len
    mov rdi, [rbp - SJ_BUF]
    add rdi, [rbp - SJ_POS]
    lea rsi, [rax + PyStrObject.data]
    call ap_memcpy
    pop rdx                 ; item_len
    add [rbp - SJ_POS], rdx
    pop rcx
    inc rcx
    jmp .join_copy_loop

.join_make_str:
    mov rdi, [rbp - SJ_BUF]
    mov rsi, [rbp - SJ_TOTAL]   ; total_len
    call str_new_heap
    push rax

    mov rdi, [rbp - SJ_BUF]
    call ap_free

    ; DECREF owned separator
    mov rdi, rbx
    call obj_decref
    JOIN_RELEASE_TMP

    pop rax
    mov edx, TAG_PTR
    add rsp, 48
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.join_empty:
    ; DECREF owned separator
    mov rdi, rbx
    call obj_decref
    JOIN_RELEASE_TMP

    lea rdi, [rel empty_str_cstr]
    call str_from_cstr_heap
    mov edx, TAG_PTR
    add rsp, 48
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.join_type_error:
    pop rcx                 ; clean up pushed index from len_loop
    mov rdi, rbx
    call obj_decref         ; DECREF owned separator
    JOIN_RELEASE_TMP
    RAISE exc_TypeError_type, "sequence item: expected str instance"
END_FUNC str_method_join

;; ============================================================================
;; str_method_split(args, nargs) -> list of strings
;; If nargs==1: split by whitespace
;; If nargs==2: split by args[1]
;; ============================================================================
;; ============================================================================
;; str_split_impl(rdi = args, rsi = nargs, edx = from_right) -> list
;;
;; One implementation for split and rsplit.  maxsplit was accepted and
;; ignored by both, and rsplit was a plain jump to split -- so
;; "a-b-c".rsplit("-", 1) returned three pieces instead of ['a-b', 'c'].
;;
;; With no separator the split is on runs of whitespace and leading and
;; trailing whitespace produce no empty pieces; with one, every occurrence
;; separates, so "a,,b".split(",") is three pieces.
;; ============================================================================
SPI_SELF   equ 8
SPI_SEP    equ 16        ; separator data, or 0 for whitespace
SPI_SEPLEN equ 24
SPI_MAX    equ 32        ; remaining splits allowed, -1 for no limit
SPI_LIST   equ 40
SPI_RIGHT  equ 48
SPI_LEN    equ 56
SPI_FRAME  equ 64           ; + 4 pushes = 96

DEF_FUNC_LOCAL str_split_impl, SPI_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov [rbp - SPI_RIGHT], rdx
    mov rbx, [rdi]                  ; self
    mov [rbp - SPI_SELF], rbx
    mov rax, [rbx + PyStrObject.ob_size]
    mov [rbp - SPI_LEN], rax
    mov qword [rbp - SPI_SEP], 0
    mov qword [rbp - SPI_SEPLEN], 0
    mov qword [rbp - SPI_MAX], -1

    cmp rsi, 2
    jl .spi_ready
    mov rax, [rdi + 8]              ; separator, or None
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .spi_check_max
    V_TEST_PTR rax, rcx
    ja .spi_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .spi_type_error
    mov rcx, [rax + PyStrObject.ob_size]
    test rcx, rcx
    jz .spi_empty_sep
    mov [rbp - SPI_SEPLEN], rcx
    lea rcx, [rax + PyStrObject.data]
    mov [rbp - SPI_SEP], rcx

.spi_check_max:
    cmp rsi, 3
    jl .spi_ready
    push rdi
    mov rdi, [rdi + 16]
    V_UNPACK rdi, rdx
    call obj_as_index
    pop rdi
    test rax, rax
    js .spi_ready                   ; a negative maxsplit means no limit
    mov [rbp - SPI_MAX], rax

.spi_ready:
    xor edi, edi
    call list_new
    mov [rbp - SPI_LIST], rax

    cmp qword [rbp - SPI_SEP], 0
    je .spi_whitespace

    ; ---- explicit separator ------------------------------------------------
    cmp qword [rbp - SPI_RIGHT], 0
    jne .spi_sep_right

    xor r12d, r12d                  ; start of the current piece
.spi_sep_loop:
    cmp qword [rbp - SPI_MAX], 0
    je .spi_sep_tail
    mov r13, r12                    ; scan position
.spi_sep_scan:
    mov rax, [rbp - SPI_LEN]
    sub rax, [rbp - SPI_SEPLEN]
    cmp r13, rax
    jg .spi_sep_tail
    mov rdi, rbx
    lea rdi, [rdi + PyStrObject.data]
    add rdi, r13
    mov rsi, [rbp - SPI_SEP]
    mov rdx, [rbp - SPI_SEPLEN]
    call ap_memcmp
    test eax, eax
    jz .spi_sep_hit
    inc r13
    jmp .spi_sep_scan

.spi_sep_hit:
    mov r14, r13
    sub r14, r12                    ; piece length
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r12
    mov rsi, r14
    call .spi_emit
    mov r12, r13
    add r12, [rbp - SPI_SEPLEN]
    cmp qword [rbp - SPI_MAX], 0
    jl .spi_sep_loop
    dec qword [rbp - SPI_MAX]
    jmp .spi_sep_loop

.spi_sep_tail:
    mov r14, [rbp - SPI_LEN]
    sub r14, r12
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r12
    mov rsi, r14
    call .spi_emit
    jmp .spi_done

    ; ---- explicit separator, scanning from the right ----------------------
.spi_sep_right:
    mov r12, [rbp - SPI_LEN]        ; end of the current piece, exclusive
.spi_sepr_loop:
    cmp qword [rbp - SPI_MAX], 0
    je .spi_sepr_tail
    mov r13, r12
    sub r13, [rbp - SPI_SEPLEN]
.spi_sepr_scan:
    test r13, r13
    js .spi_sepr_tail
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r13
    mov rsi, [rbp - SPI_SEP]
    mov rdx, [rbp - SPI_SEPLEN]
    call ap_memcmp
    test eax, eax
    jz .spi_sepr_hit
    dec r13
    jmp .spi_sepr_scan

.spi_sepr_hit:
    mov r14, r13
    add r14, [rbp - SPI_SEPLEN]
    mov rsi, r12
    sub rsi, r14                    ; piece length
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r14
    call .spi_emit_front
    mov r12, r13
    cmp qword [rbp - SPI_MAX], 0
    jl .spi_sepr_loop
    dec qword [rbp - SPI_MAX]
    jmp .spi_sepr_loop

.spi_sepr_tail:
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call .spi_emit_front
    jmp .spi_done

    ; ---- whitespace --------------------------------------------------------
.spi_whitespace:
    cmp qword [rbp - SPI_RIGHT], 0
    jne .spi_ws_right
    xor r12d, r12d
.spi_ws_loop:
    ; skip leading whitespace
    cmp r12, [rbp - SPI_LEN]
    jge .spi_done
    movzx edi, byte [rbx + PyStrObject.data + r12]
    xor esi, esi
    xor edx, edx
    call strip_char_matches
    test eax, eax
    jz .spi_ws_piece
    inc r12
    jmp .spi_ws_loop

.spi_ws_piece:
    cmp qword [rbp - SPI_MAX], 0
    jne .spi_ws_scan
    ; Out of splits: the rest is one piece, *including* its trailing
    ; whitespace.  CPython skips only the whitespace before the remainder --
    ; ' a b '.split(None, 1) is ['a', 'b '], not ['a', 'b'].
    mov r13, [rbp - SPI_LEN]
.spi_ws_emit_last:
    mov rsi, r13
    sub rsi, r12
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r12
    call .spi_emit
    jmp .spi_done

.spi_ws_scan:
    mov r13, r12
.spi_ws_find_end:
    cmp r13, [rbp - SPI_LEN]
    jge .spi_ws_emit
    movzx edi, byte [rbx + PyStrObject.data + r13]
    xor esi, esi
    xor edx, edx
    call strip_char_matches
    test eax, eax
    jnz .spi_ws_emit
    inc r13
    jmp .spi_ws_find_end
.spi_ws_emit:
    mov rsi, r13
    sub rsi, r12
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r12
    call .spi_emit
    mov r12, r13
    cmp qword [rbp - SPI_MAX], 0
    jl .spi_ws_loop
    dec qword [rbp - SPI_MAX]
    jmp .spi_ws_loop

.spi_ws_right:
    ; rsplit() with no separator: same pieces, but maxsplit counts from the
    ; right, so collect from the right and prepend.
    mov r12, [rbp - SPI_LEN]
.spi_wsr_loop:
    ; skip trailing whitespace
    test r12, r12
    jle .spi_done
    movzx edi, byte [rbx + PyStrObject.data + r12 - 1]
    xor esi, esi
    xor edx, edx
    call strip_char_matches
    test eax, eax
    jz .spi_wsr_piece
    dec r12
    jmp .spi_wsr_loop

.spi_wsr_piece:
    cmp qword [rbp - SPI_MAX], 0
    jne .spi_wsr_scan
    ; Likewise from the other end: ' a b '.rsplit(None, 1) is [' a', 'b'].
    mov r13, 0
.spi_wsr_emit_last:
    mov rsi, r12
    sub rsi, r13
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r13
    call .spi_emit_front
    jmp .spi_done

.spi_wsr_scan:
    mov r13, r12
.spi_wsr_find:
    test r13, r13
    jle .spi_wsr_emit
    movzx edi, byte [rbx + PyStrObject.data + r13 - 1]
    xor esi, esi
    xor edx, edx
    call strip_char_matches
    test eax, eax
    jnz .spi_wsr_emit
    dec r13
    jmp .spi_wsr_find
.spi_wsr_emit:
    mov rsi, r12
    sub rsi, r13
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r13
    call .spi_emit_front
    mov r12, r13
    cmp qword [rbp - SPI_MAX], 0
    jl .spi_wsr_loop
    dec qword [rbp - SPI_MAX]
    jmp .spi_wsr_loop

.spi_done:
    mov rax, [rbp - SPI_LIST]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.spi_empty_sep:
    RAISE exc_ValueError_type, "empty separator"

.spi_type_error:
    RAISE exc_TypeError_type, "must be str or None, not other type"

;; Append a piece (rdi = data, rsi = length) to the result list.
.spi_emit:
    push r12
    push r13
    call str_new_heap
    push rax
    mov rdi, [rbp - SPI_LIST]
    mov rsi, rax
    call list_append
    pop rdi
    call obj_decref
    pop r13
    pop r12
    ret

;; The same, but inserted at the front: the right-hand scans produce pieces
;; in reverse.
.spi_emit_front:
    push r12
    push r13
    call str_new_heap
    ; list.insert(0, piece), through the method's own args-array interface
    sub rsp, 32
    mov rcx, [rbp - SPI_LIST]
    mov [rsp], rcx
    mov rcx, [rel v_int_bias]       ; the Value for 0
    mov [rsp + 8], rcx
    mov [rsp + 16], rax
    push rax
    lea rdi, [rsp + 8]
    mov rsi, 3
    call list_method_insert
    pop rdi
    add rsp, 32
    call obj_decref
    pop r13
    pop r12
    ret
END_FUNC str_split_impl

DEF_FUNC_BARE str_method_split
    xor edx, edx                ; scan from the left
    jmp str_split_impl
END_FUNC str_method_split


;; ============================================================================
;; fmtbuf_append(rdi = &{buf, used, cap}, rsi = data, rdx = length)
;; ============================================================================
DEF_FUNC_LOCAL fmtbuf_append
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    test r13, r13
    jz .fa_done
    mov rax, [rbx + 8]
    add rax, r13
    cmp rax, [rbx + 16]
    jbe .fa_room
.fa_grow:
    mov rcx, [rbx + 16]
    shl rcx, 1
    cmp rcx, rax
    jae .fa_have_cap
    mov rcx, rax
.fa_have_cap:
    mov [rbx + 16], rcx
    mov rdi, [rbx]
    mov rsi, rcx
    call ap_realloc
    mov [rbx], rax
.fa_room:
    mov rdi, [rbx]
    add rdi, [rbx + 8]
    mov rsi, r12
    mov rdx, r13
    call ap_memcpy
    add [rbx + 8], r13
.fa_done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC fmtbuf_append

;; ============================================================================
;; str_method_format(args, nargs) -> new formatted string
;;
;; The whole replacement-field grammar: {}, {2}, {name}, an optional !r/!s/!a
;; conversion and an optional :spec, which is handed to the same formatter an
;; f-string uses.  Only {} and {N} were understood before, and anything else
;; made the function bail and return whatever it had accumulated -- so
;; "{:>6}".format("ab") was the empty string.
;; ============================================================================
SF_STATE  equ 24            ; {buf, used, cap} as three consecutive qwords
SF_ARGS   equ 32
SF_NPOS   equ 40
SF_AUTO   equ 48
SF_KWN    equ 56            ; the kw_names tuple, or 0
SF_NKW    equ 64
SF_FSTART equ 72
SF_FEND   equ 80
SF_CONV   equ 88
SF_SSTART equ 96
SF_SEND   equ 104
SF_VALUE  equ 112
SF_FRAME  equ 128           ; + 5 pushes = 168, not 16-aligned
DEF_FUNC str_method_format, SF_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - SF_ARGS], rdi
    mov rbx, rdi

    ; Keyword arguments arrive as trailing values with their names in
    ; kw_names_pending; clear it so a nested call cannot see ours.
    mov rax, [rel kw_names_pending]
    mov [rbp - SF_KWN], rax
    mov qword [rbp - SF_NKW], 0
    test rax, rax
    jz .fm_no_kw
    mov rcx, [rax + PyTupleObject.ob_size]
    mov [rbp - SF_NKW], rcx
    sub rsi, rcx
    mov qword [rel kw_names_pending], 0
.fm_no_kw:
    dec rsi                     ; drop self
    mov [rbp - SF_NPOS], rsi

    mov rax, [rbx]
    lea r12, [rax + PyStrObject.data]
    mov r13, [rax + PyStrObject.ob_size]

    lea rdi, [r13 + 64]
    call ap_malloc
    mov [rbp - SF_STATE], rax
    mov qword [rbp - SF_STATE + 8], 0
    lea rax, [r13 + 64]
    mov [rbp - SF_STATE + 16], rax

    xor r15d, r15d              ; cursor
    mov qword [rbp - SF_AUTO], 0

.fm_loop:
    cmp r15, r13
    jge .fm_done
    movzx eax, byte [r12 + r15]
    cmp al, '{'
    je .fm_open
    cmp al, '}'
    je .fm_close
.fm_literal:
    lea rdi, [rbp - SF_STATE]
    lea rsi, [r12 + r15]
    mov edx, 1
    call fmtbuf_append
    inc r15
    jmp .fm_loop

.fm_close:
    ; "}}" is a literal brace; a lone one is an error, as in CPython.
    lea rcx, [r15 + 1]
    cmp rcx, r13
    jge .fm_lone_brace
    cmp byte [r12 + rcx], '}'
    jne .fm_lone_brace
    lea rdi, [rbp - SF_STATE]
    lea rsi, [r12 + r15]
    mov edx, 1
    call fmtbuf_append
    add r15, 2
    jmp .fm_loop

.fm_open:
    lea rcx, [r15 + 1]
    cmp rcx, r13
    jge .fm_unterminated
    cmp byte [r12 + rcx], '{'
    jne .fm_field
    lea rdi, [rbp - SF_STATE]
    lea rsi, [r12 + r15]
    mov edx, 1
    call fmtbuf_append
    add r15, 2
    jmp .fm_loop

.fm_field:
    inc r15
    mov [rbp - SF_FSTART], r15
    mov qword [rbp - SF_CONV], 0
    mov qword [rbp - SF_SSTART], 0
    mov qword [rbp - SF_SEND], 0
    xor r14d, r14d              ; bracket depth
.fm_scan_field:
    cmp r15, r13
    jge .fm_unterminated
    movzx eax, byte [r12 + r15]
    cmp al, '['
    jne .fm_scan_not_open
    inc r14
    jmp .fm_scan_next
.fm_scan_not_open:
    cmp al, ']'
    jne .fm_scan_check_end
    dec r14
    jmp .fm_scan_next
.fm_scan_check_end:
    test r14, r14
    jnz .fm_scan_next
    cmp al, '!'
    je .fm_field_end
    cmp al, ':'
    je .fm_field_end
    cmp al, '}'
    je .fm_field_end
.fm_scan_next:
    inc r15
    jmp .fm_scan_field

.fm_field_end:
    mov [rbp - SF_FEND], r15
    movzx eax, byte [r12 + r15]
    cmp al, '!'
    jne .fm_after_conv
    inc r15
    cmp r15, r13
    jge .fm_unterminated
    movzx eax, byte [r12 + r15]
    mov [rbp - SF_CONV], rax
    inc r15
.fm_after_conv:
    cmp r15, r13
    jge .fm_unterminated
    movzx eax, byte [r12 + r15]
    cmp al, ':'
    jne .fm_after_spec
    inc r15
    mov [rbp - SF_SSTART], r15
    xor r14d, r14d
.fm_scan_spec:
    cmp r15, r13
    jge .fm_unterminated
    movzx eax, byte [r12 + r15]
    cmp al, '{'
    jne .fm_spec_not_open
    inc r14
    jmp .fm_spec_next
.fm_spec_not_open:
    cmp al, '}'
    jne .fm_spec_next
    test r14, r14
    jz .fm_spec_end
    dec r14
.fm_spec_next:
    inc r15
    jmp .fm_scan_spec
.fm_spec_end:
    mov [rbp - SF_SEND], r15
.fm_after_spec:
    cmp r15, r13
    jge .fm_unterminated
    cmp byte [r12 + r15], '}'
    jne .fm_unterminated
    inc r15                     ; past the closing brace

    mov rdi, [rbp - SF_ARGS]
    mov rsi, [rbp - SF_NPOS]
    mov rdx, [rbp - SF_KWN]
    mov rcx, r12
    add rcx, [rbp - SF_FSTART]
    mov r8, [rbp - SF_FEND]
    sub r8, [rbp - SF_FSTART]
    lea r9, [rbp - SF_AUTO]
    call fm_resolve_field
    mov [rbp - SF_VALUE], rax

    ; !r and !a render the repr; !s the str.  Anything else is left alone.
    mov rcx, [rbp - SF_CONV]
    test rcx, rcx
    jz .fm_no_conv
    cmp rcx, 'r'
    je .fm_conv_repr
    cmp rcx, 'a'
    je .fm_conv_repr
    cmp rcx, 's'
    jne .fm_no_conv
    mov rdi, [rbp - SF_VALUE]
    call obj_str
    jmp .fm_conv_done
.fm_conv_repr:
    mov rdi, [rbp - SF_VALUE]
    call obj_repr
.fm_conv_done:
    push rax
    mov rdi, [rbp - SF_VALUE]
    DECREF_V rdi, rcx
    pop rax
    mov [rbp - SF_VALUE], rax
.fm_no_conv:

    ; The spec, as a str, handed to the formatter f-strings use.
    mov rcx, [rbp - SF_SEND]
    sub rcx, [rbp - SF_SSTART]
    jle .fm_plain_str
    mov rdi, r12
    add rdi, [rbp - SF_SSTART]
    mov rsi, rcx
    call str_new_heap
    push rax
    mov rdi, [rbp - SF_VALUE]
    mov rsi, rax
    extern format_apply_spec
    call format_apply_spec
    mov r14, rax
    pop rdi
    call obj_decref
    jmp .fm_have_text

.fm_plain_str:
    mov rdi, [rbp - SF_VALUE]
    call obj_str
    mov r14, rax

.fm_have_text:
    mov rdi, [rbp - SF_VALUE]
    DECREF_V rdi, rcx
    test r14, r14
    jz .fm_loop
    lea rdi, [rbp - SF_STATE]
    lea rsi, [r14 + PyStrObject.data]
    mov rdx, [r14 + PyStrObject.ob_size]
    call fmtbuf_append
    mov rdi, r14
    call obj_decref
    jmp .fm_loop

.fm_lone_brace:
    RAISE exc_ValueError_type, "Single '}' encountered in format string"

.fm_unterminated:
    RAISE exc_ValueError_type, "Single '{' encountered in format string"

.fm_done:
    mov rdi, [rbp - SF_STATE]
    mov rsi, [rbp - SF_STATE + 8]
    call str_new_heap
    push rax
    mov rdi, [rbp - SF_STATE]
    call ap_free
    pop rax
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC str_method_format

;; ============================================================================
;; fm_resolve_field(rdi = args, rsi = npos, rdx = kw_names or 0,
;;                  rcx = field bytes, r8 = field length, r9 = &auto counter)
;;   -> rax = the argument the field names, a new reference
;; Empty means the next positional, all digits an explicit one, anything else
;; a keyword.
;; ============================================================================
RF_ARGS  equ 8
RF_NPOS  equ 16
RF_KWN   equ 24
RF_NAME  equ 32
RF_LEN   equ 40
RF_AUTO  equ 48
RF_FRAME equ 64             ; + 2 pushes = 80
DEF_FUNC_LOCAL fm_resolve_field, RF_FRAME
    push rbx
    push r12
    mov [rbp - RF_ARGS], rdi
    mov [rbp - RF_NPOS], rsi
    mov [rbp - RF_KWN], rdx
    mov [rbp - RF_NAME], rcx
    mov [rbp - RF_LEN], r8
    mov [rbp - RF_AUTO], r9

    test r8, r8
    jz .rf_auto

    ; All digits?
    xor r12d, r12d              ; the parsed index
    xor ecx, ecx
.rf_digits:
    cmp rcx, [rbp - RF_LEN]
    jge .rf_positional
    mov rdx, [rbp - RF_NAME]
    movzx eax, byte [rdx + rcx]
    cmp al, '0'
    jb .rf_keyword
    cmp al, '9'
    ja .rf_keyword
    imul r12, r12, 10
    sub eax, '0'
    add r12, rax
    inc rcx
    jmp .rf_digits

.rf_auto:
    mov rax, [rbp - RF_AUTO]
    mov r12, [rax]
    inc qword [rax]
.rf_positional:
    cmp r12, [rbp - RF_NPOS]
    jge .rf_index_error
    mov rax, [rbp - RF_ARGS]
    lea rcx, [r12 + 1]          ; args[0] is self
    mov rax, [rax + rcx*8]
    INCREF_V rax, rcx
    pop r12
    pop rbx
    leave
    ret

.rf_keyword:
    mov rax, [rbp - RF_KWN]
    test rax, rax
    jz .rf_key_error
    mov rbx, [rax + PyTupleObject.ob_size]
    xor r12d, r12d
.rf_kw_scan:
    cmp r12, rbx
    jge .rf_key_error
    mov rax, [rbp - RF_KWN]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rdi, [rax + r12*8]
    add rdi, PyStrObject.data
    mov rsi, [rbp - RF_NAME]
    mov rdx, [rbp - RF_LEN]
    call fm_name_equals
    test eax, eax
    jnz .rf_kw_found
    inc r12
    jmp .rf_kw_scan
.rf_kw_found:
    ; The keyword values sit after the positional ones.
    mov rax, [rbp - RF_ARGS]
    mov rcx, [rbp - RF_NPOS]
    add rcx, r12
    inc rcx                     ; past self
    mov rax, [rax + rcx*8]
    INCREF_V rax, rcx
    pop r12
    pop rbx
    leave
    ret

.rf_index_error:
    RAISE exc_IndexError_type, "Replacement index out of range for positional args tuple"
.rf_key_error:
    RAISE exc_KeyError_type, "format() got no such keyword argument"
END_FUNC fm_resolve_field

;; fm_name_equals(rdi = NUL-terminated name, rsi = bytes, rdx = length) -> eax
DEF_FUNC_LOCAL fm_name_equals
    xor ecx, ecx
.ne_loop:
    cmp rcx, rdx
    jge .ne_at_end
    movzx eax, byte [rdi + rcx]
    test al, al
    jz .ne_no
    movzx r8d, byte [rsi + rcx]
    cmp al, r8b
    jne .ne_no
    inc rcx
    jmp .ne_loop
.ne_at_end:
    cmp byte [rdi + rcx], 0
    jne .ne_no
    mov eax, 1
    leave
    ret
.ne_no:
    xor eax, eax
    leave
    ret
END_FUNC fm_name_equals

;; ============================================================================
;; str_method_format_map(args, nargs) -> formatted string
;; args[0]=self (format string), args[1]=mapping (dict)
;; Replaces {key} with mapping[key].
;; ============================================================================
FM_ARGS   equ 8
FM_MAP    equ 16
FM_BUF    equ 24
FM_USED   equ 32
FM_CAP    equ 40
FM_FRAME  equ 48            ; + 5 pushes = 88, not 16-aligned

DEF_FUNC str_method_format_map, FM_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .fmap_error

    mov [rbp - FM_ARGS], rdi
    mov rax, [rdi]              ; self = format string
    mov rcx, [rdi + 8]         ; mapping dict
    mov [rbp - FM_MAP], rcx

    lea r12, [rax + PyStrObject.data]   ; r12 = fmt data
    mov r13d, [rax + PyStrObject.ob_size] ; r13 = fmt len

    ; Allocate output buffer
    lea rdi, [r13 + 64]
    call ap_malloc
    mov [rbp - FM_BUF], rax
    mov qword [rbp - FM_USED], 0
    lea rax, [r13 + 64]
    mov [rbp - FM_CAP], rax

    xor ecx, ecx               ; source index

.fmap_loop:
    cmp ecx, r13d
    jge .fmap_done
    movzx eax, byte [r12 + rcx]
    cmp al, '{'
    je .fmap_brace
    cmp al, '}'
    je .fmap_close_brace

    ; Regular char — append
    push rcx
    mov rdi, [rbp - FM_BUF]
    mov rax, [rbp - FM_USED]
    movzx edx, byte [r12 + rcx]
    mov [rdi + rax], dl
    inc qword [rbp - FM_USED]
    pop rcx
    inc ecx
    jmp .fmap_loop

.fmap_brace:
    inc ecx
    cmp ecx, r13d
    jge .fmap_done
    movzx eax, byte [r12 + rcx]
    cmp al, '{'
    je .fmap_literal_open

    ; Extract key name until '}'
    mov r14d, ecx              ; key start
.fmap_key_scan:
    cmp ecx, r13d
    jge .fmap_done
    movzx eax, byte [r12 + rcx]
    cmp al, '}'
    je .fmap_have_key
    inc ecx
    jmp .fmap_key_scan

.fmap_have_key:
    ; Key is from r14 to ecx (exclusive)
    push rcx
    inc ecx                     ; skip '}'
    push rcx                    ; save next source pos

    ; Create key string
    lea rdi, [r12 + r14]
    mov esi, ecx
    dec esi
    sub esi, r14d               ; key length
    movzx esi, si               ; zero-extend
    call str_new_heap
    push rax                    ; save key str

    ; Look up in mapping: dict_get(dict, key, key_tag)
    mov rdi, [rbp - FM_MAP]
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    ; rax = value payload, edx = value tag
    push rax
    push rdx

    ; DECREF key
    mov rdi, [rsp + 16]         ; key str
    call obj_decref

    ; Convert value to string
    pop rsi                     ; value tag
    pop rdi                     ; value payload
    V_PACK rdi, rsi
    call obj_str
    ; rax = result payload, edx = tag
    push rax                    ; save str obj for DECREF

    ; Copy string data to buffer
    mov edx, [rax + PyStrObject.ob_size]
    ; Ensure buffer space
    mov rdi, [rbp - FM_USED]
    add rdi, rdx
    cmp rdi, [rbp - FM_CAP]
    jbe .fmap_copy_ok
    mov rdi, [rbp - FM_CAP]
    shl rdi, 1
    add rdi, rdx
    mov [rbp - FM_CAP], rdi
    push rdx
    mov rsi, rdi
    mov rdi, [rbp - FM_BUF]
    call ap_realloc
    mov [rbp - FM_BUF], rax
    pop rdx
    mov rax, [rsp]              ; re-read str obj
.fmap_copy_ok:
    test edx, edx
    jz .fmap_val_done
    push rdx
    mov rdi, [rbp - FM_BUF]
    add rdi, [rbp - FM_USED]
    lea rsi, [rax + PyStrObject.data]
    movzx edx, dx
    call ap_memcpy
    pop rdx
    movzx eax, dx
    add [rbp - FM_USED], rax

.fmap_val_done:
    ; DECREF temp str
    pop rdi
    call obj_decref
    pop rax                     ; discard saved key str slot
    pop rcx                     ; next source pos
    pop rax                     ; discard saved old ecx
    jmp .fmap_loop

.fmap_literal_open:
    push rcx
    mov rdi, [rbp - FM_BUF]
    mov rax, [rbp - FM_USED]
    mov byte [rdi + rax], '{'
    inc qword [rbp - FM_USED]
    pop rcx
    inc ecx
    jmp .fmap_loop

.fmap_close_brace:
    inc ecx
    cmp ecx, r13d
    jge .fmap_done
    movzx eax, byte [r12 + rcx]
    cmp al, '}'
    jne .fmap_loop
    push rcx
    mov rdi, [rbp - FM_BUF]
    mov rax, [rbp - FM_USED]
    mov byte [rdi + rax], '}'
    inc qword [rbp - FM_USED]
    pop rcx
    inc ecx
    jmp .fmap_loop

.fmap_done:
    mov rdi, [rbp - FM_BUF]
    mov rsi, [rbp - FM_USED]
    call str_new_heap
    push rax

    mov rdi, [rbp - FM_BUF]
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

.fmap_error:
    RAISE exc_TypeError_type, "format_map() takes exactly one argument"
END_FUNC str_method_format_map

;; ============================================================================
;; str_method_lstrip(args, nargs) -> new string with the left end stripped
;; args[0]=self, args[1]=chars (optional)
;; ============================================================================
DEF_FUNC_BARE str_method_lstrip
    mov edx, 1
    jmp str_strip_impl
END_FUNC str_method_lstrip

;; ============================================================================
;; str_method_rstrip(args, nargs) -> new string with the right end stripped
;; args[0]=self, args[1]=chars (optional)
;; ============================================================================
DEF_FUNC_BARE str_method_rstrip
    mov edx, 2
    jmp str_strip_impl
END_FUNC str_method_rstrip

;; ============================================================================
;; str_method_rsplit(args, nargs) -> list
;; Like split but from right. args[0]=self, args[1]=sep (optional)
;; For simplicity, implements same as split (no maxsplit from right)
;; ============================================================================
DEF_FUNC_BARE str_method_rsplit
    mov edx, 1                  ; scan from the right
    jmp str_split_impl
END_FUNC str_method_rsplit

section .rodata
empty_str_cstr: db 0
