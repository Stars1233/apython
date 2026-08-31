; methods/str_pred.asm - str: the is* predicates, case mapping, justification
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
extern str_search_window
extern ap_memfind
extern str_byte_to_cp
extern str_find_impl
extern ap_malloc
extern ap_free
extern ap_memcpy
extern ap_memset
extern str_new_heap
extern bool_true
extern bool_false
extern int_from_i64
extern int_to_i64
extern raise_exception
extern exc_TypeError_type
extern str_byte_to_cp
extern str_type

; Set entry layout constants (must match set.asm)

; --- moved to a sibling file by the split ---

section .text

;; ============================================================================
;; str_method_count(args, nargs) -> SmallInt count of occurrences
;; args[0]=self, args[1]=sub
;; ============================================================================
;; start and end were ignored and the scan used the C-string ap_strstr, so
;; "abcabc".count("b", 3) was 2 and "a\x00b".count("b") was 0.  Counting an
;; empty needle over a window gives one position per code point plus one, which
;; is what CPython reports.
CNT_ARGS  equ 8
CNT_NARGS equ 16
CNT_SELF  equ 24
CNT_N     equ 32
CNT_WPTR  equ 56            ; the 3-word window: 56, 48, 40
CNT_WLEN  equ 48
CNT_WOFF  equ 40
CNT_FRAME equ 64            ; + 2 pushes = 80
DEF_FUNC str_method_count, CNT_FRAME
    push rbx
    push r12
    mov [rbp - CNT_ARGS], rdi
    mov [rbp - CNT_NARGS], rsi

    mov rax, [rdi + 8]              ; args[1]
    V_TEST_PTR rax, rcx
    ja .count_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .count_type_error

    mov rcx, [rdi]
    mov [rbp - CNT_SELF], rcx
    mov qword [rbp - CNT_N], 0

    mov rdi, rcx
    mov rsi, [rbp - CNT_ARGS]
    mov rdx, [rbp - CNT_NARGS]
    lea rcx, [rbp - CNT_WPTR]
    call str_search_window
    test eax, eax
    jz .count_done                  ; nothing can match: zero

    mov rbx, [rbp - CNT_WPTR]       ; rbx = cursor into the window
    mov r12, [rbp - CNT_WLEN]       ; r12 = bytes left

    mov rax, [rbp - CNT_ARGS]
    mov rax, [rax + 8]
    mov rax, [rax + PyStrObject.ob_size]
    test rax, rax
    jz .count_empty_sub

.count_scan:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CNT_ARGS]
    mov rdx, [rdx + 8]
    mov rcx, [rdx + PyStrObject.ob_size]
    lea rdx, [rdx + PyStrObject.data]
    call ap_memfind
    test rax, rax
    jz .count_done

    inc qword [rbp - CNT_N]
    ; Advance past the match; non-overlapping, as CPython counts.
    mov rdx, [rbp - CNT_ARGS]
    mov rdx, [rdx + 8]
    mov rdx, [rdx + PyStrObject.ob_size]
    add rax, rdx
    sub r12, rax
    add r12, rbx                    ; bytes left = old_left - (rax - rbx)
    mov rbx, rax
    jmp .count_scan

.count_empty_sub:
    ; One position before each code point in the window, plus one at the end.
    mov rdi, [rbp - CNT_SELF]
    mov rax, [rdi + PyStrObject.ob_size]
    cmp rax, [rdi + PyStrObject.ob_length]
    jne .count_empty_walk
    mov rax, r12                    ; ASCII: one code point per byte
    inc rax
    mov [rbp - CNT_N], rax
    jmp .count_done
.count_empty_walk:
    ; Non-ASCII: convert both ends of the window to code point indices.
    mov rsi, [rbp - CNT_WOFF]
    call str_byte_to_cp
    mov rbx, rax
    mov rdi, [rbp - CNT_SELF]
    mov rsi, [rbp - CNT_WOFF]
    add rsi, r12
    call str_byte_to_cp
    sub rax, rbx
    inc rax
    mov [rbp - CNT_N], rax

.count_done:
    mov rdi, [rbp - CNT_N]
    call int_from_i64
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.count_type_error:
    RAISE exc_TypeError_type, "must be str, not other type"
END_FUNC str_method_count

;; ============================================================================
;; str_method_index(args, nargs) -> SmallInt index (raises ValueError if not found)
;; args[0]=self, args[1]=substr
;; ============================================================================
DEF_FUNC_BARE str_method_index
    mov edx, 2                  ; forward, raise on a miss
    jmp str_find_impl
END_FUNC str_method_index

;; ============================================================================
;; str_method_rfind(args, nargs) -> SmallInt index or -1
;; args[0]=self, args[1]=substr
;; Find rightmost occurrence of substr in self.
;; ============================================================================
DEF_FUNC_BARE str_method_rfind
    mov edx, 1                  ; reverse
    jmp str_find_impl
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
    RET_TRUE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isdigit_false:
    RET_FALSE
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
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret
.false:
    RET_FALSE
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
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret
.false:
    RET_FALSE
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
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret
.false:
    RET_FALSE
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
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret
.false:
    RET_FALSE
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
    RET_TRUE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isalpha_false:
    RET_FALSE
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
    RET_TRUE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isalnum_false:
    RET_FALSE
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
    cmp sil, 0x0d           ; tab(09), newline(0A), VT(0B), FF(0C), CR(0D)
    ja .isspace_false
.isspace_next:
    inc rdx
    jmp .isspace_loop

.isspace_true:
    RET_TRUE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isspace_false:
    RET_FALSE
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

    RET_TRUE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isupper_false:
    RET_FALSE
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

    RET_TRUE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.islower_false:
    RET_FALSE
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
