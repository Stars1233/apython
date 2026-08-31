; format.asm - The format-spec mini-language.
;
; builtin_format_fn ignored its spec entirely and fell through to str(), and
; op_format_value had a partial float path that handled only precision and a
; type letter.  So format(255, "08b") returned "255", f"{5:>5}" returned "5",
; and f"{1234:_}" returned "1234" -- confident wrong answers on some of the
; most common formatting there is.
;
; The grammar implemented here is CPython's:
;
;   [[fill]align][sign][#][0][width][grouping][.precision][type]
;
; align  < > ^ =        fill defaults to a space, or to '0' after a leading 0
; sign   + - space      only meaningful for numbers
; #      alternate form: the 0b/0o/0x prefix
; group  , or _         inserted every three digits
; type   b o x X d n    integer
;        e E f F g G %  float
;        s              string
;
; The body is rendered first and padded afterwards, which is what makes '='
; alignment (pad between the sign and the digits) fall out naturally.

%include "macros.inc"
%include "object.inc"

extern str_new_heap
extern ap_malloc
extern obj_str
extern obj_as_index
extern int_fits_i64
extern float_format_spec
extern ap_memcpy
extern obj_decref
extern raise_exception
extern exc_ValueError_type
extern str_type
extern int_type
extern bool_type
extern float_type
extern value_type

section .text

;; Parsed spec, kept in the caller's frame.
FS_FILL   equ 8          ; fill character
FS_ALIGN  equ 16         ; '<' '>' '^' '=' or 0 when unset
FS_SIGN   equ 24         ; '+' '-' ' '
FS_ALT    equ 32         ; the # flag
FS_ZERO   equ 40         ; a leading 0 was given
FS_WIDTH  equ 48
FS_GROUP  equ 56         ; ',' '_' or 0
FS_PREC   equ 64         ; -1 when unset
FS_TYPE   equ 72         ; type letter, or 0
FS_VALUE  equ 80
FS_BODY   equ 88         ; rendered body, a str object
FS_SIGNCH equ 96         ; the sign actually emitted, or 0
FS_SPECLEN equ 104       ; length of the spec as given
FS_FRAME  equ 112

;; ============================================================================
;; format_apply_spec(rdi = value Value, rsi = spec str) -> Value (a str)
;; ============================================================================
DEF_FUNC format_apply_spec, FS_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - FS_VALUE], rdi
    mov qword [rbp - FS_BODY], 0
    mov qword [rbp - FS_SIGNCH], 0

    ; ---- defaults ----------------------------------------------------------
    mov qword [rbp - FS_FILL], ' '
    mov qword [rbp - FS_ALIGN], 0
    mov qword [rbp - FS_SIGN], '-'
    mov qword [rbp - FS_ALT], 0
    mov qword [rbp - FS_ZERO], 0
    mov qword [rbp - FS_WIDTH], 0
    mov qword [rbp - FS_GROUP], 0
    mov qword [rbp - FS_PREC], -1
    mov qword [rbp - FS_TYPE], 0

    mov rbx, rsi                        ; spec str
    mov r12, [rbx + PyStrObject.ob_size]
    mov [rbp - FS_SPECLEN], r12
    lea r13, [rbx + PyStrObject.data]
    xor r14d, r14d                      ; position

    ; ---- [[fill]align] -----------------------------------------------------
    ; A two-character lookahead: any character followed by an alignment code
    ; is a fill.
    mov rax, r12
    sub rax, r14
    cmp rax, 2
    jl .fs_try_align_only
    movzx ecx, byte [r13 + r14 + 1]
    call .fs_is_align
    test eax, eax
    jz .fs_try_align_only
    movzx ecx, byte [r13 + r14]
    mov [rbp - FS_FILL], rcx
    movzx ecx, byte [r13 + r14 + 1]
    mov [rbp - FS_ALIGN], rcx
    add r14, 2
    jmp .fs_after_align

.fs_try_align_only:
    cmp r14, r12
    jge .fs_after_align
    movzx ecx, byte [r13 + r14]
    call .fs_is_align
    test eax, eax
    jz .fs_after_align
    movzx ecx, byte [r13 + r14]
    mov [rbp - FS_ALIGN], rcx
    inc r14

.fs_after_align:
    ; ---- [sign] ------------------------------------------------------------
    cmp r14, r12
    jge .fs_after_sign
    movzx ecx, byte [r13 + r14]
    cmp cl, '+'
    je .fs_take_sign
    cmp cl, '-'
    je .fs_take_sign
    cmp cl, ' '
    jne .fs_after_sign
.fs_take_sign:
    mov [rbp - FS_SIGN], rcx
    inc r14

.fs_after_sign:
    ; ---- [#] ---------------------------------------------------------------
    cmp r14, r12
    jge .fs_after_alt
    cmp byte [r13 + r14], '#'
    jne .fs_after_alt
    mov qword [rbp - FS_ALT], 1
    inc r14

.fs_after_alt:
    ; ---- [0] ---------------------------------------------------------------
    cmp r14, r12
    jge .fs_after_zero
    cmp byte [r13 + r14], '0'
    jne .fs_after_zero
    mov qword [rbp - FS_ZERO], 1
    mov qword [rbp - FS_FILL], '0'
    cmp qword [rbp - FS_ALIGN], 0
    jne .fs_zero_taken
    mov qword [rbp - FS_ALIGN], '='
.fs_zero_taken:
    inc r14

.fs_after_zero:
    ; ---- [width] -----------------------------------------------------------
    xor r15d, r15d
.fs_width_loop:
    cmp r14, r12
    jge .fs_after_width
    movzx ecx, byte [r13 + r14]
    cmp cl, '0'
    jb .fs_after_width
    cmp cl, '9'
    ja .fs_after_width
    imul r15, r15, 10
    sub rcx, '0'
    add r15, rcx
    inc r14
    jmp .fs_width_loop
.fs_after_width:
    mov [rbp - FS_WIDTH], r15

    ; ---- [grouping] --------------------------------------------------------
    cmp r14, r12
    jge .fs_after_group
    movzx ecx, byte [r13 + r14]
    cmp cl, ','
    je .fs_take_group
    cmp cl, '_'
    jne .fs_after_group
.fs_take_group:
    mov [rbp - FS_GROUP], rcx
    inc r14

.fs_after_group:
    ; ---- [.precision] ------------------------------------------------------
    cmp r14, r12
    jge .fs_after_prec
    cmp byte [r13 + r14], '.'
    jne .fs_after_prec
    inc r14
    xor r15d, r15d
.fs_prec_loop:
    cmp r14, r12
    jge .fs_prec_done
    movzx ecx, byte [r13 + r14]
    cmp cl, '0'
    jb .fs_prec_done
    cmp cl, '9'
    ja .fs_prec_done
    imul r15, r15, 10
    sub rcx, '0'
    add r15, rcx
    inc r14
    jmp .fs_prec_loop
.fs_prec_done:
    mov [rbp - FS_PREC], r15

.fs_after_prec:
    ; ---- [type] ------------------------------------------------------------
    cmp r14, r12
    jge .fs_parsed
    movzx ecx, byte [r13 + r14]
    mov [rbp - FS_TYPE], rcx
    inc r14
    cmp r14, r12
    jne .fs_bad_spec

.fs_parsed:
    ; ---- render the body ---------------------------------------------------
    mov rdi, [rbp - FS_VALUE]
    call value_type
    mov r15, rax                        ; the value's type

    mov rcx, [rbp - FS_TYPE]
    cmp rcx, 's'
    je .fs_body_str
    test rcx, rcx
    jnz .fs_typed

    ; No type letter: a str formats as a string, a number as itself.
    lea rax, [rel str_type]
    cmp r15, rax
    je .fs_body_str
    lea rax, [rel float_type]
    cmp r15, rax
    je .fs_body_float
    lea rax, [rel int_type]
    cmp r15, rax
    je .fs_body_int
    lea rax, [rel bool_type]
    cmp r15, rax
    je .fs_body_int
    ; Anything else uses object.__format__, which accepts only an empty
    ; spec.  format(None, ">5") padded None instead of raising.
    cmp qword [rbp - FS_SPECLEN], 0
    jne .fs_unsupported
    jmp .fs_body_str

.fs_bad_numeric_type:
    ; A numeric type letter on a non-number: format("abc", "f") converted the
    ; string through float_to_f64 and printed 0.000000.
    extern exc_ValueError_type
    RAISE exc_ValueError_type, "Unknown format code for object of type 'str'"
    ud2

.fs_unsupported:
    mov rsi, [rbp - FS_VALUE]
    CSTRING rdi, `unsupported format string passed to \x01.__format__`
    extern raise_type_error_with_name
    call raise_type_error_with_name

.fs_typed:
    ; A numeric type letter needs a number.
    cmp rcx, 's'
    je .fs_body_str
    lea rax, [rel str_type]
    cmp r15, rax
    je .fs_bad_numeric_type
    cmp rcx, 'b'
    je .fs_body_int
    cmp rcx, 'o'
    je .fs_body_int
    cmp rcx, 'x'
    je .fs_body_int
    cmp rcx, 'X'
    je .fs_body_int
    cmp rcx, 'd'
    je .fs_body_int
    cmp rcx, 'n'
    je .fs_body_int
    cmp rcx, 'c'
    je .fs_body_int
    jmp .fs_body_float

.fs_body_str:
    mov rdi, [rbp - FS_VALUE]
    call obj_str
    V_UNPACK rax, rdx
    test edx, edx
    jz .fs_failed
    mov [rbp - FS_BODY], rax
    ; A precision truncates a string, and it counts characters: cutting at a
    ; byte offset would split a multi-byte one in half.
    mov rcx, [rbp - FS_PREC]
    cmp rcx, 0
    jl .fs_pad
    cmp rcx, [rax + PyStrObject.ob_length]
    jge .fs_pad
    push rax
    mov rdi, rax
    mov rsi, rcx
    extern str_cp_offset
    call str_cp_offset
    mov rsi, rax
    mov rdi, [rsp]
    add rdi, PyStrObject.data
    call str_new_heap
    mov [rbp - FS_BODY], rax
    pop rdi
    call obj_decref
    jmp .fs_pad

.fs_body_float:
    call format_float_body
    mov [rbp - FS_BODY], rax
    jmp .fs_pad

.fs_body_int:
    call format_int_body
    mov [rbp - FS_BODY], rax
    jmp .fs_pad

    ; ---- pad to width ------------------------------------------------------
.fs_pad:
    ; A width counts characters.  r12 stays the body's byte length -- it is
    ; what gets copied -- but the comparison and the padding count are in code
    ; points, and the buffer is the body's bytes plus that many ASCII pads.
    mov rbx, [rbp - FS_BODY]
    mov r12, [rbx + PyStrObject.ob_size]
    mov rax, [rbx + PyStrObject.ob_length]
    mov r13, [rbp - FS_WIDTH]
    cmp rax, r13
    jge .fs_return_body

    mov r14, r13
    sub r14, rax                        ; total padding, in characters
    mov r13, r12
    add r13, r14                        ; the buffer, in bytes

    ; Default alignment: '>' for numbers, '<' for everything else.
    mov rcx, [rbp - FS_ALIGN]
    test rcx, rcx
    jnz .fs_have_align
    mov rcx, '<'
    mov rax, [rbp - FS_TYPE]
    test rax, rax
    jz .fs_align_by_value
    cmp rax, 's'
    je .fs_have_align
    mov rcx, '>'
    jmp .fs_have_align
.fs_align_by_value:
    mov rdi, [rbp - FS_VALUE]
    call value_type
    lea rdx, [rel str_type]
    cmp rax, rdx
    je .fs_have_align_lt
    mov rcx, '>'
    jmp .fs_have_align
.fs_align_by_value_str:
.fs_have_align_lt:
    mov rcx, '<'
.fs_have_align:
    mov [rbp - FS_ALIGN], rcx

    ; Build the padded result.
    mov rdi, r13
    add rdi, PyStrObject.data + 8
    call ap_malloc
    mov r15, rax
    mov qword [r15 + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [r15 + PyObject.ob_type], rcx
    mov qword [r15 + PyStrObject.ob_hash], -1
    mov [r15 + PyStrObject.ob_size], r13
    mov [r15 + PyStrObject.ob_length], r13   ; corrected once the bytes are in

    mov rcx, [rbp - FS_ALIGN]
    cmp rcx, '<'
    je .fs_pad_left_aligned
    cmp rcx, '^'
    je .fs_pad_center
    cmp rcx, '='
    je .fs_pad_after_sign

    ; '>' : all padding first
    lea rdi, [r15 + PyStrObject.data]
    mov rsi, r14
    call .fs_fill_run
    lea rdi, [r15 + PyStrObject.data]
    add rdi, r14
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy
    jmp .fs_pad_done

.fs_pad_left_aligned:
    lea rdi, [r15 + PyStrObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy
    lea rdi, [r15 + PyStrObject.data]
    add rdi, r12
    mov rsi, r14
    call .fs_fill_run
    jmp .fs_pad_done

.fs_pad_center:
    mov rax, r14
    shr rax, 1                          ; left half, rounded down
    push rax
    lea rdi, [r15 + PyStrObject.data]
    mov rsi, rax
    call .fs_fill_run
    pop rax
    push rax
    lea rdi, [r15 + PyStrObject.data]
    add rdi, rax
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy
    pop rax
    lea rdi, [r15 + PyStrObject.data]
    add rdi, rax
    add rdi, r12
    mov rsi, r14
    sub rsi, rax
    call .fs_fill_run
    jmp .fs_pad_done

.fs_pad_after_sign:
    ; Padding goes between the sign (and any 0x prefix) and the digits.
    mov r8, [rbp - FS_SIGNCH]
    lea rdi, [r15 + PyStrObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r8
    test r8, r8
    jz .fs_eq_no_prefix
    call ap_memcpy
.fs_eq_no_prefix:
    lea rdi, [r15 + PyStrObject.data]
    add rdi, r8
    mov rsi, r14
    call .fs_fill_run
    lea rdi, [r15 + PyStrObject.data]
    add rdi, r8
    add rdi, r14
    lea rsi, [rbx + PyStrObject.data]
    add rsi, r8
    mov rdx, r12
    sub rdx, r8
    call ap_memcpy

.fs_pad_done:
    lea rcx, [r15 + PyStrObject.data]
    add rcx, r13
    mov qword [rcx], 0                  ; NUL plus padding for ap_strcmp
    mov rdi, r15
    extern str_set_length
    call str_set_length
    mov rdi, rbx
    call obj_decref
    mov rax, r15
    jmp .fs_return

.fs_return_body:
    mov rax, [rbp - FS_BODY]

.fs_return:
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.fs_failed:
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.fs_bad_spec:
    RAISE exc_ValueError_type, "Invalid format specifier"

;; rdi = destination, rsi = count.  Writes the fill character.
.fs_fill_run:
    test rsi, rsi
    jle .fs_fill_done
    mov rax, [rbp - FS_FILL]
    xor ecx, ecx
.fs_fill_loop:
    mov [rdi + rcx], al
    inc rcx
    cmp rcx, rsi
    jl .fs_fill_loop
.fs_fill_done:
    ret

;; ecx = character; returns eax = 1 when it is an alignment code.
.fs_is_align:
    cmp cl, '<'
    je .fs_align_yes
    cmp cl, '>'
    je .fs_align_yes
    cmp cl, '^'
    je .fs_align_yes
    cmp cl, '='
    je .fs_align_yes
    xor eax, eax
    ret
.fs_align_yes:
    mov eax, 1
    ret
END_FUNC format_apply_spec

;; ============================================================================
;; format_int_body -> rax = str, the digits with sign, prefix and grouping
;;
;; Reads the parsed spec out of the caller's frame, so it is a local helper of
;; format_apply_spec rather than a general entry point.  FS_SIGNCH is set to
;; the number of leading characters that '=' alignment must keep in front of
;; the padding.
;; ============================================================================
; The digit buffer is indexed upward from its base, so its *base* must sit
; low enough that the whole thing stays below rbp: [rbp-136, rbp-8).  Putting
; the base at rbp-8 let nine digits or more run over the saved registers.
FIB_BUF   equ 136        ; 128 bytes of digits, for a value that fits int64
FIB_LEN   equ 144
FIB_NEG   equ 152
FIB_HEAP  equ 160        ; heap digit buffer to free, or 0 (wide values)
FIB_OUTSZ equ 168        ; bytes reserved for the assembled output
FIB_FRAME equ 192

DEF_FUNC_LOCAL format_int_body, FIB_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r15, rbp                        ; our frame

    ; The caller's frame holds the spec.  rbp of format_apply_spec is the
    ; saved rbp at [rbp].
    mov r14, [rbp]                      ; caller's rbp
    mov qword [rbp - FIB_HEAP], 0

    ; Value -> i64.  A value too wide for int64 is out of scope for the
    ; non-decimal bases here; base 10 goes through str() instead.
    mov rdi, [r14 - FS_VALUE]
    V_UNPACK rdi, rdx
    push rdi
    push rdx
    call int_fits_i64
    pop rdx
    pop rdi
    test eax, eax
    jz .fib_wide
    call obj_as_index
    mov r12, rax                        ; the value

    ; base and digit set from the type letter
    mov r13, 10
    mov rcx, [r14 - FS_TYPE]
    cmp rcx, 'b'
    je .fib_base2
    cmp rcx, 'o'
    je .fib_base8
    cmp rcx, 'x'
    je .fib_base16
    cmp rcx, 'X'
    je .fib_base16
    jmp .fib_have_base
.fib_base2:
    mov r13, 2
    jmp .fib_have_base
.fib_base8:
    mov r13, 8
    jmp .fib_have_base
.fib_base16:
    mov r13, 16
.fib_have_base:

    mov qword [rbp - FIB_NEG], 0
    test r12, r12
    jns .fib_positive
    mov qword [rbp - FIB_NEG], 1
    neg r12
.fib_positive:

    ; Digits, least significant first, into a scratch buffer.
    lea rbx, [rbp - FIB_BUF]
    xor ecx, ecx
    test r12, r12
    jnz .fib_digits
    mov byte [rbx], '0'
    mov ecx, 1
    jmp .fib_digits_done
.fib_digits:
    mov rax, r12
    xor edx, edx
    div r13
    mov r12, rax
    cmp dl, 10
    jb .fib_digit_num
    add dl, 'a' - 10
    mov rax, [r14 - FS_TYPE]
    cmp rax, 'X'
    jne .fib_digit_store
    add dl, 'A' - 'a'
    jmp .fib_digit_store
.fib_digit_num:
    add dl, '0'
.fib_digit_store:
    mov [rbx + rcx], dl
    inc rcx
    test r12, r12
    jnz .fib_digits
.fib_digits_done:
    mov [rbp - FIB_LEN], rcx

.fib_assemble:
    ; Assemble: sign, prefix, grouped digits (reversed).  The output is sized
    ; from the digit count -- grouping adds one separator per three digits --
    ; because a fixed 256-byte buffer overflowed the stack on any value with
    ; more than about 250 digits.
    mov rax, [rbp - FIB_LEN]
    mov rcx, rax
    shr rcx, 1
    add rax, rcx
    add rax, 64
    and rax, -16
    mov [rbp - FIB_OUTSZ], rax
    sub rsp, rax
    mov rdi, rsp
    xor r8d, r8d                        ; output length

    ; sign
    cmp qword [rbp - FIB_NEG], 0
    je .fib_sign_positive
    mov byte [rdi], '-'
    inc r8
    jmp .fib_sign_done
.fib_sign_positive:
    mov rax, [r14 - FS_SIGN]
    cmp rax, '+'
    je .fib_sign_plus
    cmp rax, ' '
    je .fib_sign_space
    jmp .fib_sign_done
.fib_sign_plus:
    mov byte [rdi], '+'
    inc r8
    jmp .fib_sign_done
.fib_sign_space:
    mov byte [rdi], ' '
    inc r8
.fib_sign_done:

    ; alternate-form prefix
    cmp qword [r14 - FS_ALT], 0
    je .fib_prefix_done
    mov rax, [r14 - FS_TYPE]
    cmp rax, 'b'
    je .fib_pfx_b
    cmp rax, 'o'
    je .fib_pfx_o
    cmp rax, 'x'
    je .fib_pfx_x
    cmp rax, 'X'
    je .fib_pfx_X
    jmp .fib_prefix_done
.fib_pfx_b:
    mov byte [rdi + r8], '0'
    mov byte [rdi + r8 + 1], 'b'
    add r8, 2
    jmp .fib_prefix_done
.fib_pfx_o:
    mov byte [rdi + r8], '0'
    mov byte [rdi + r8 + 1], 'o'
    add r8, 2
    jmp .fib_prefix_done
.fib_pfx_x:
    mov byte [rdi + r8], '0'
    mov byte [rdi + r8 + 1], 'x'
    add r8, 2
    jmp .fib_prefix_done
.fib_pfx_X:
    mov byte [rdi + r8], '0'
    mov byte [rdi + r8 + 1], 'X'
    add r8, 2
.fib_prefix_done:
    mov [rbp - FIB_NEG], r8             ; reuse: chars before the digits

    ; digits, most significant first, with grouping every three
    mov rcx, [rbp - FIB_LEN]
    mov r9, [r14 - FS_GROUP]
.fib_emit:
    test rcx, rcx
    jz .fib_emit_done
    dec rcx
    movzx eax, byte [rbx + rcx]
    mov [rdi + r8], al
    inc r8
    test r9, r9
    jz .fib_emit
    test rcx, rcx
    jz .fib_emit
    mov rax, rcx
    xor edx, edx
    mov r10, 3
    div r10
    test rdx, rdx
    jnz .fib_emit
    mov rax, r9
    mov [rdi + r8], al
    inc r8
    jmp .fib_emit
.fib_emit_done:

    mov rsi, r8
    mov r13, [rbp - FIB_NEG]
    call str_new_heap
    add rsp, [rbp - FIB_OUTSZ]
    mov [r14 - FS_SIGNCH], r13
    mov rcx, [rbp - FIB_HEAP]
    test rcx, rcx
    jz .fib_no_heap
    push rax
    mov rdi, rcx
    call ap_free
    pop rax
.fib_no_heap:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.fib_wide:
    ; Too wide for int64.  Decimal comes straight from str(); the other bases
    ; used to be refused outright ("integer too large for this format"), so
    ; f"{2**70:x}" raised.  int_base_str renders any of them through GMP; the
    ; digits are reversed into FIB_BUF so the assembly stage below -- sign,
    ; prefix, grouping, padding -- runs unchanged.
    mov rax, [r14 - FS_TYPE]
    xor edx, edx                        ; uppercase?
    mov esi, 10
    test rax, rax
    jz .fib_wb_go
    cmp rax, 'd'
    je .fib_wb_go
    cmp rax, 'n'
    je .fib_wb_go
    mov esi, 2
    cmp rax, 'b'
    je .fib_wb_go
    mov esi, 8
    cmp rax, 'o'
    je .fib_wb_go
    mov esi, 16
    cmp rax, 'x'
    je .fib_wb_go
    mov edx, 1
    cmp rax, 'X'
    je .fib_wb_go
    jmp .fib_wide_error
.fib_wb_go:
    mov rdi, [r14 - FS_VALUE]
    extern int_base_str
    call int_base_str
    mov r12, rax                        ; the C string

    mov qword [rbp - FIB_NEG], 0
    mov rsi, r12
    cmp byte [rsi], '-'
    jne .fib_wb_scan
    mov qword [rbp - FIB_NEG], 1
    inc rsi
.fib_wb_scan:
    xor ecx, ecx
.fib_wb_len:
    cmp byte [rsi + rcx], 0
    je .fib_wb_reverse
    inc rcx
    jmp .fib_wb_len
.fib_wb_reverse:
    ; The digits go least-significant first into FIB_BUF, which holds 128 --
    ; enough for any int64 but not for a GMP value, whose string is
    ; unbounded.  format(10**140, 'd') wrote past the saved rbp.
    lea rbx, [rbp - FIB_BUF]
    cmp rcx, 120
    jb .fib_wb_have_buf
    push rcx
    push rsi
    lea rdi, [rcx + 16]
    extern ap_malloc
    call ap_malloc
    mov [rbp - FIB_HEAP], rax
    mov rbx, rax
    pop rsi
    pop rcx
.fib_wb_have_buf:
    xor edx, edx
.fib_wb_rev_loop:
    cmp rdx, rcx
    jge .fib_wb_reversed
    mov r8, rcx
    sub r8, rdx
    dec r8
    mov al, [rsi + r8]
    mov [rbx + rdx], al
    inc rdx
    jmp .fib_wb_rev_loop
.fib_wb_reversed:
    mov [rbp - FIB_LEN], rcx
    push rcx
    mov rdi, r12
    extern ap_free
    call ap_free
    pop rcx
    jmp .fib_assemble

.fib_wide_error:
    extern exc_OverflowError_type
    RAISE exc_OverflowError_type, "%c arg not in range(0x110000)"
    ud2
END_FUNC format_int_body

;; ============================================================================
;; format_float_body -> rax = str
;;
;; Delegates the digits to float_format_spec, which already knows how to
;; render a double to a precision and a type letter, then leaves padding to
;; the caller.
;; ============================================================================
FFB_SPEC  equ 8          ; the synthesised ".<prec><type>" spec
FFB_FRAME equ 48

DEF_FUNC_LOCAL format_float_body, FFB_FRAME
    push rbx
    push r12
    mov r12, [rbp]                      ; caller's rbp

    ; The value has to be a float for float_format_spec; an int with a float
    ; type letter is converted first, as CPython does.
    mov rdi, [r12 - FS_VALUE]
    V_UNPACK rdi, rdx
    cmp edx, TAG_FLOAT
    je .ffb_have_double
    ; An int with a float type letter is converted first, as CPython does.
    mov rsi, rdx                        ; the tag
    extern float_to_f64
    call float_to_f64
    movq rdi, xmm0                      ; raw double bits

.ffb_have_double:
    ; Build ".<precision><type>" in a small buffer.
    lea rbx, [rbp - FFB_SPEC]
    xor ecx, ecx
    mov rax, [r12 - FS_PREC]
    cmp rax, 0
    jge .ffb_have_prec
    ; e, f and g default to six digits; only a bare spec means repr.  Without
    ; this f"{1.5:f}" was "1.5" rather than "1.500000".
    mov rdx, [r12 - FS_TYPE]
    cmp rdx, 'e'
    je .ffb_default_prec
    cmp rdx, 'E'
    je .ffb_default_prec
    cmp rdx, 'f'
    je .ffb_default_prec
    cmp rdx, 'F'
    je .ffb_default_prec
    cmp rdx, 'g'
    je .ffb_default_prec
    cmp rdx, 'G'
    je .ffb_default_prec
    cmp rdx, '%'
    jne .ffb_no_prec
.ffb_default_prec:
    mov rax, 6
.ffb_have_prec:
    mov byte [rbx], '.'
    mov ecx, 1
    ; CPython caps a float precision well below this; anything past three
    ; digits is not representable in the underlying formatter either.  Two
    ; digits used to be assumed, so ".100f" became ".10" plus a stray '0'
    ; that was read as the type letter.
    cmp rax, 999
    jle .ffb_prec_ok
    mov rax, 999
.ffb_prec_ok:
    mov r8, rax
    mov r9, 100
    xor edx, edx
    mov rax, r8
    div r9                              ; rax = hundreds, rdx = rest
    test rax, rax
    jz .ffb_prec_tens
    add al, '0'
    mov [rbx + rcx], al
    inc rcx
    mov r11d, 1                         ; a leading digit was emitted
    jmp .ffb_prec_have_h
.ffb_prec_tens:
    xor r11d, r11d
.ffb_prec_have_h:
    mov rax, rdx
    xor edx, edx
    mov r9, 10
    div r9                              ; rax = tens, rdx = units
    mov r10, rdx                        ; keep the units
    test rax, rax
    jnz .ffb_prec_emit_tens
    test r11d, r11d
    jz .ffb_one_digit
.ffb_prec_emit_tens:
    add al, '0'
    mov [rbx + rcx], al
    inc rcx
.ffb_one_digit:
    mov rax, r10
    add al, '0'
    mov [rbx + rcx], al
    inc rcx
.ffb_no_prec:
    mov rax, [r12 - FS_TYPE]
    test rax, rax
    jnz .ffb_have_type
    mov rax, 'r'                        ; float_format_spec's repr default
.ffb_have_type:
    mov [rbx + rcx], al
    inc rcx

    mov rsi, rbx
    mov rdx, rcx
    call float_format_spec
    V_UNPACK rax, rdx

    ; A leading '-' is what '=' alignment keeps in front of the padding.
    mov qword [r12 - FS_SIGNCH], 0
    cmp qword [rax + PyStrObject.ob_size], 0
    jle .ffb_done
    cmp byte [rax + PyStrObject.data], '-'
    je .ffb_negative

    ; float_format_spec knows nothing about the sign flag, so a '+' or a
    ; leading space has to be put on here: "%+.1f" % 1.25 was "1.2".
    mov rcx, [r12 - FS_SIGN]
    cmp rcx, '+'
    je .ffb_add_sign
    cmp rcx, ' '
    jne .ffb_done
.ffb_add_sign:
    mov rbx, rax                    ; the unsigned digits
    mov rsi, [rbx + PyStrObject.ob_size]
    lea rdi, [rsi + PyStrObject.data + 9]
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rdx, [rel str_type]
    mov [rax + PyObject.ob_type], rdx
    mov qword [rax + PyStrObject.ob_hash], -1
    mov rcx, [rbx + PyStrObject.ob_size]
    inc rcx
    mov [rax + PyStrObject.ob_size], rcx
    mov [rax + PyStrObject.ob_length], rcx
    mov rcx, [r12 - FS_SIGN]
    mov [rax + PyStrObject.data], cl
    push rax
    lea rdi, [rax + PyStrObject.data + 1]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, [rbx + PyStrObject.ob_size]
    call ap_memcpy
    pop rax
    mov rcx, [rax + PyStrObject.ob_size]
    mov qword [rax + PyStrObject.data + rcx], 0
    push rax
    mov rdi, rbx
    call obj_decref
    pop rax

.ffb_negative:
    mov qword [r12 - FS_SIGNCH], 1

.ffb_done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC format_float_body
