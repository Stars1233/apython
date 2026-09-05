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
FS_OWNED  equ 112        ; a box V_PACK made for a wide int subclass, or 0
FS_FRAME  equ 120           ; + 5 pushes = 160, 16-aligned

; The widest field this will build.  See .fs_after_width.
FS_MAX_WIDTH equ 0x10000000

;; ============================================================================
;; format_apply_spec(rdi = value Value, rsi = spec str) -> Value (a str)
;; ============================================================================
DEF_FUNC format_apply_spec, FS_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    ; Zeroed here and not where it is set: every arm reaches the release in
    ; .fs_body_int, and only the int-subclass arm assigns it, so a frame slot
    ; left holding whatever was on the stack was handed to obj_decref.
    mov qword [rbp - FS_OWNED], 0

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
    jo .fs_too_many_digits
    sub rcx, '0'
    add r15, rcx
    jo .fs_too_many_digits
    inc r14
    jmp .fs_width_loop
.fs_after_width:
    ; A width is padding, and padding is a buffer.  CPython has no cap and
    ; simply asks the allocator, which answers MemoryError; ap_malloc has no
    ; way to answer at all -- it calls fatal_error -- so `"%*d" % (2**40, 5)`
    ; printed "Fatal: out of memory" and ended the process.  The cap is
    ; list_repeat's, 256M, and the divergence it buys is a MemoryError where
    ; CPython would have spent a quarter of a gigabyte building a field of
    ; spaces.
    cmp r15, FS_MAX_WIDTH
    ja .fs_width_too_big
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
    jo .fs_too_many_digits
    sub rcx, '0'
    add r15, rcx
    jo .fs_too_many_digits
    inc r14
    jmp .fs_prec_loop
.fs_prec_done:
    ; CPython's precision is a C int, and it says so.
    cmp r15, 0x7FFFFFFF
    ja .fs_prec_too_big
    ; And a precision is a buffer, the same as a width: float_format_spec
    ; renders through snprintf, whose answer is an int, and a precision near
    ; INT_MAX makes it return -1 rather than a length.  The cap is the width's
    ; and the divergence is the same shape -- a MemoryError where CPython
    ; would have spent two gigabytes on decimal places.
    cmp r15, FS_MAX_WIDTH
    ja .fs_width_too_big
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

    ; A subclass formats as its base does: every arm below compares r15
    ; against an exact type, so format(F(2.5), ".2f") for `class F(float)`
    ; found no arm and fell out as "unsupported format string passed to
    ; object.__format__".  The family flags say which base to answer as.
    ; The family flags live on the BASE types too, so that a subclass
    ; inherits them: float_type itself carries TYPE_FLAG_FLOAT_SUBCLASS.
    ; Anything keyed off the flag alone therefore fires for the exact type as
    ; well, and unwrapping an immediate as if it were an instance reads a
    ; NaN-boxed double as an address.  Ask whether it IS one of the four
    ; first, and only then whether it derives from one.
    test r15, r15
    jz .fs_family_done
    lea rax, [rel complex_type]
    cmp r15, rax
    je .fs_family_done
    lea rax, [rel float_type]
    cmp r15, rax
    je .fs_family_done
    lea rax, [rel int_type]
    cmp r15, rax
    je .fs_family_done
    lea rax, [rel str_type]
    cmp r15, rax
    je .fs_family_done
    extern bool_type
    mov rdx, [r15 + PyTypeObject.tp_flags]
    lea rax, [rel bool_type]
    cmp r15, rax
    je .fs_bool

    test rdx, TYPE_FLAG_COMPLEX_SUBCLASS
    jz .fs_not_complex_sub
    lea r15, [rel complex_type]     ; complex_to_parts unwraps the value
    jmp .fs_family_done
.fs_not_complex_sub:
    test rdx, TYPE_FLAG_FLOAT_SUBCLASS
    jz .fs_not_float_sub
    lea r15, [rel float_type]
    ; The double lives inline in the instance, and the float arm below wants
    ; an immediate.
    mov rdi, [rbp - FS_VALUE]
    mov rax, [rdi + PyFloatObject.value]
    V_FROM_F64 rax, rcx
    mov [rbp - FS_VALUE], rax
    jmp .fs_family_done
.fs_not_int_sub:
    test rdx, TYPE_FLAG_STR_SUBCLASS
    jz .fs_family_done
    lea r15, [rel str_type]         ; a str subclass has str's layout
    jmp .fs_family_done
.fs_bool:
    ; A COMPLETELY empty spec is object.__format__, which is str(): CPython
    ; makes format(True) "True" and format(True, ">5") "    1", and this
    ; took the int path either way -- so f"{True}" printed 1.
    cmp qword [rbp - FS_SPECLEN], 0
    je .fs_body_str
    ; Otherwise bool formats as an int, which is what CPython does:
    ; format(True, "d") is "1".  Its value is a singleton, not an int, so it is unwrapped too.
    ; tp_flags is loaded BEFORE the jump here, because this falls straight
    ; into a test of rdx and nothing else on this path writes it -- it held
    ; whatever the caller had left in it.
    lea r15, [rel int_type]
.fs_not_float_sub:
    test rdx, TYPE_FLAG_INT_SUBCLASS
    jz .fs_not_int_sub
    lea r15, [rel int_type]
    mov rdi, [rbp - FS_VALUE]
    mov edx, TAG_PTR
    extern int_unwrap
    call int_unwrap
    ; V_PACK on a SmallInt outside +-2^50 ALLOCATES, and FS_VALUE is a slot
    ; this function neither owns nor releases: one boxed int leaked per
    ; format() of such a subclass instance.  Note whether it boxed, and give
    ; the box back once the body has been rendered.
    V_PACK rdi, rdx
    mov [rbp - FS_VALUE], rdi
    V_TEST_PTR rdi, rax
    ja .fs_family_done          ; still an immediate: nothing was allocated
    mov [rbp - FS_OWNED], rdi
.fs_family_done:

    mov rcx, [rbp - FS_TYPE]
    ; complex is asked first: format(1+2j, 's') is a ValueError in CPython, so
    ; it must not reach the 's' short-circuit below.
    extern complex_type
    lea rax, [rel complex_type]
    cmp r15, rax
    je .fs_typed
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
    extern complex_type
    lea rax, [rel complex_type]
    cmp r15, rax
    je .fs_body_complex
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
    ; string through float_to_f64 and printed 0.000000.  CPython names both
    ; the code and the type, and this named neither.
    jmp .fs_unknown_code

.fs_unknown_code:
    ; "Unknown format code 'x' for object of type 'str'" -- the code is one
    ; character, so the buffer is built here rather than in a raiser.
    sub rsp, 128
    mov rdi, rsp
    CSTRING rsi, "Unknown format code '"
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rcx, [rbp - FS_TYPE]
    mov [rax], cl
    mov byte [rax + 1], 0
    lea rdi, [rax + 1]
    CSTRING rsi, "' for object of type '"
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [r15 + PyTypeObject.tp_name]
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, "'"
    call rbt_append_cstr
    extern exc_ValueError_type
    lea rdi, [rel exc_ValueError_type]
    mov rsi, rsp
    extern raise_exception
    call raise_exception
    ud2

.fs_unsupported:
    mov rsi, [rbp - FS_VALUE]
    CSTRING rdi, `unsupported format string passed to \x01.__format__`
    extern raise_type_error_with_name
    call raise_type_error_with_name

.fs_typed:
    ; complex is checked before the 's' arm: format(1+2j, 's') is a ValueError
    ; in CPython, not a string.  The accepted letters are exactly e E f F g G n.
    lea rax, [rel complex_type]
    cmp r15, rax
    jne .fs_typed_not_complex
    test rcx, rcx
    jz .fs_body_complex             ; no type letter: repr, handled there
    cmp rcx, 'e'
    je .fs_body_complex
    cmp rcx, 'E'
    je .fs_body_complex
    cmp rcx, 'f'
    je .fs_body_complex
    cmp rcx, 'F'
    je .fs_body_complex
    cmp rcx, 'g'
    je .fs_body_complex
    cmp rcx, 'G'
    je .fs_body_complex
    cmp rcx, 'n'
    je .fs_body_complex
    jmp .fs_unknown_code
.fs_typed_not_complex:
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
    mov rdi, [rbp - FS_OWNED]
    test rdi, rdi
    jz .fs_pad
    mov qword [rbp - FS_OWNED], 0
    push rax
    sub rsp, 8
    call obj_decref             ; the box V_PACK made for a wide subclass
    add rsp, 8
    pop rax
    jmp .fs_pad

.fs_body_complex:
    ; CPython rejects these two for a complex before it formats anything, and
    ; the messages are its own.
    cmp qword [rbp - FS_ZERO], 0
    jne .fs_complex_zero_pad
    cmp qword [rbp - FS_ALIGN], '='
    je .fs_complex_equals_align
    call format_complex_body
    mov [rbp - FS_BODY], rax
    jmp .fs_pad

.fs_too_many_digits:
    RAISE exc_ValueError_type, "Too many decimal digits in format string"
.fs_prec_too_big:
    RAISE exc_ValueError_type, "precision too big"
.fs_width_too_big:
    extern exc_MemoryError_type
    RAISE exc_MemoryError_type, ""
.fs_complex_zero_pad:
    RAISE exc_ValueError_type, "Zero padding is not allowed in complex format specifier"
.fs_complex_equals_align:
    RAISE exc_ValueError_type, "'=' alignment flag is not allowed in complex format specifier"

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
FIB_FRAME equ 200            ; + 5 pushes = 240, 16-aligned

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
FFB_ADDDOT equ 8         ; 1 when an empty type needs its ".0" put back
FFB_PCT   equ 16         ; 1 when the type letter was '%'
; The synthesised ".<prec><type>" spec.  32 bytes, because the precision is
; whatever fits a C int and that is ten digits; four used to be assumed, and
; the cap that kept it to three was the reason format(1.0, ".5000f") came back
; with a thousand decimal places instead of five thousand.
FFB_SPEC  equ 48
FFB_FRAME equ 64            ; + 2 pushes = 80, 16-aligned

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
    ; The '%' type is 'f' applied to a hundred times the value, with a '%'
    ; put on the end.  Neither half was done: the letter reached
    ; float_format_spec, matched none of its six, and fell to the %g default,
    ; so format(1/3, ".2%") was "0.33" -- not the right number, and not
    ; carrying the sign that says what it is.
    mov qword [rbp - FFB_PCT], 0
    mov rax, [r12 - FS_TYPE]
    cmp rax, '%'
    jne .ffb_not_pct
    mov qword [rbp - FFB_PCT], 1
    mov qword [r12 - FS_TYPE], 'f'      ; and 'f' pads the same way '%' does
    movq xmm0, rdi
    mulsd xmm0, [rel ffb_hundred]
    movq rdi, xmm0
.ffb_not_pct:

    ; An empty type letter is repr, not %g.  format_float_body used to write a
    ; one-byte spec "r" on the strength of a comment claiming
    ; float_format_spec had a repr default; it has none, so the letter was
    ; ignored and the defaults %.6g rendered format(1.0, "") as "1".
    ;
    ; CPython's rule: with no precision an empty type is exactly repr(x); with
    ; one it is 'g' but with at least one digit after the point, so
    ; format(1.0, ".3") is "1.0" rather than "1".
    mov qword [rbp - FFB_ADDDOT], 0
    mov rax, [r12 - FS_TYPE]
    test rax, rax
    jnz .ffb_build_spec
    mov rax, [r12 - FS_PREC]
    cmp rax, 0
    jge .ffb_empty_with_prec
    extern float_repr
    call float_repr             ; rdi = the raw bits, still
    jmp .ffb_have_string
.ffb_empty_with_prec:
    mov qword [rbp - FFB_ADDDOT], 1

.ffb_build_spec:
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
    ; However many digits it takes.  float_format_spec renders through
    ; snprintf and falls back to a heap buffer of whatever size snprintf
    ; names, so the number here is the only limit -- and the spec parser has
    ; already refused anything that does not fit a C int.  Three digits used
    ; to be all this could write, and rather than say so it silently used 999
    ; instead: format(1.0, ".5000f") came back one thousand places long.
    mov r8, rax
    mov r9, 10
    xor r10d, r10d                      ; digits on the machine stack
.ffb_prec_split:
    xor edx, edx
    mov rax, r8
    div r9                              ; rax = quotient, rdx = digit
    mov r8, rax
    add rdx, '0'
    push rdx
    inc r10
    test r8, r8
    jnz .ffb_prec_split
.ffb_prec_emit:                         ; least significant went on last
    pop rax
    mov [rbx + rcx], al
    inc rcx
    dec r10
    jnz .ffb_prec_emit
.ffb_no_prec:
    mov rax, [r12 - FS_TYPE]
    test rax, rax
    jnz .ffb_have_type
    mov rax, 'g'                        ; an empty type, with a precision
.ffb_have_type:
    mov [rbx + rcx], al
    inc rcx

    mov rsi, rbx
    mov rdx, rcx
    call float_format_spec
    V_UNPACK rax, rdx

    cmp qword [rbp - FFB_PCT], 0
    jne .ffb_add_pct
    cmp qword [rbp - FFB_ADDDOT], 0
    je .ffb_have_string

    ; Put the ".0" back when %g dropped it.  A point, an exponent, or the
    ; letters of inf and nan all mean there is nothing to put back.
    mov rbx, rax
    mov rcx, [rbx + PyStrObject.ob_size]
    xor esi, esi
.ffb_dot_scan:
    cmp rsi, rcx
    jge .ffb_dot_append
    movzx eax, byte [rbx + PyStrObject.data + rsi]
    cmp al, '.'
    je .ffb_dot_none
    cmp al, 'e'
    je .ffb_dot_none
    cmp al, 'E'
    je .ffb_dot_none
    cmp al, 'n'                         ; nan
    je .ffb_dot_none
    cmp al, 'i'                         ; inf
    je .ffb_dot_none
    inc rsi
    jmp .ffb_dot_scan
.ffb_dot_none:
    mov rax, rbx
    jmp .ffb_have_string
.ffb_dot_append:
    lea rdi, [rcx + PyStrObject.data + 9]
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rdx, [rel str_type]
    mov [rax + PyObject.ob_type], rdx
    mov qword [rax + PyStrObject.ob_hash], -1
    mov rcx, [rbx + PyStrObject.ob_size]
    lea rdx, [rcx + 2]
    mov [rax + PyStrObject.ob_size], rdx
    mov [rax + PyStrObject.ob_length], rdx
    push rax
    lea rdi, [rax + PyStrObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, rcx
    call ap_memcpy
    pop rax
    mov rcx, [rbx + PyStrObject.ob_size]
    mov byte [rax + PyStrObject.data + rcx], '.'
    mov byte [rax + PyStrObject.data + rcx + 1], '0'
    mov byte [rax + PyStrObject.data + rcx + 2], 0
    push rax
    mov rdi, rbx
    call obj_decref
    pop rax

    jmp .ffb_have_string

.ffb_add_pct:
    ; The '%' on the end.  It goes over the NUL that terminates the rendered
    ; body -- a str always has one -- and str_new_heap copies from there with
    ; an explicit length, so the body is released untouched a line later.
    mov rbx, rax
    mov rcx, [rbx + PyStrObject.ob_size]
    mov byte [rbx + PyStrObject.data + rcx], '%'
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [rcx + 1]
    call str_new_heap
    push rax
    mov rdi, rbx
    call obj_decref
    pop rax

.ffb_have_string:
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

;; ============================================================================
;; format_complex_body() -> rax = PyStrObject*
;;
;; Reached with the caller's format_apply_spec frame live, the same way
;; format_float_body is: r12 addresses it and the FS_* slots are read through
;; that.
;;
;; With no type letter the body is exactly repr(z) -- CPython's
;; format_complex_internal sets type 'r' and precision 0 and applies the same
;; skip-the-real-part rule, which is what repr already is.  With a letter, both
;; halves go through float_format_spec with the same synthesised ".<prec><t>"
;; spec and are joined as `re` + signed(`im`) + "j", with no parentheses.
;;
;; The sign flag applies to the real part only; the imaginary part always
;; carries its own explicit sign.
;; ============================================================================
FCB_SPEC    equ 16          ; the synthesised ".<prec><type>", at most 6 bytes
FCB_SPECLEN equ 24
FCB_RE      equ 32          ; the real part's rendered str
FCB_IM      equ 40          ; the imaginary part's
FCB_SELF    equ 48
FCB_OUT     equ 56
FCB_FRAME   equ 64          ; + 2 pushes = 80
DEF_FUNC_LOCAL format_complex_body, FCB_FRAME
    push rbx
    push r12
    mov r12, [rbp]                      ; the caller's rbp
    mov rax, [r12 - FS_VALUE]
    mov [rbp - FCB_SELF], rax

    mov rax, [r12 - FS_TYPE]
    test rax, rax
    jz .fcb_repr                        ; an empty spec is repr, exactly

    ; ---- build ".<prec><type>" ---------------------------------------------
    lea rbx, [rbp - FCB_SPEC]
    mov rax, [r12 - FS_PREC]
    cmp rax, 0
    jge .fcb_prec_given
    mov rax, 6                          ; e E f F g G n all default to six
.fcb_prec_given:
    cmp rax, 999
    jle .fcb_prec_ok
    mov rax, 999
.fcb_prec_ok:
    mov byte [rbx], '.'
    mov ecx, 1
    xor r8d, r8d                        ; a digit has been emitted
    mov r9, 100
    xor edx, edx
    div r9                              ; rax = hundreds, rdx = rest
    test rax, rax
    jz .fcb_tens
    add al, '0'
    mov [rbx + rcx], al
    inc rcx
    mov r8d, 1
.fcb_tens:
    mov rax, rdx
    xor edx, edx
    mov r9, 10
    div r9                              ; rax = tens, rdx = units
    mov r10, rdx
    test rax, rax
    jnz .fcb_emit_tens
    test r8d, r8d
    jz .fcb_units
.fcb_emit_tens:
    add al, '0'
    mov [rbx + rcx], al
    inc rcx
.fcb_units:
    mov rax, r10
    add al, '0'
    mov [rbx + rcx], al
    inc rcx
    ; 'n' is 'g' with locale grouping, which we do not do; format it as 'g'.
    mov rax, [r12 - FS_TYPE]
    cmp rax, 'n'
    jne .fcb_type_ok
    mov rax, 'g'
.fcb_type_ok:
    mov [rbx + rcx], al
    inc rcx
    mov [rbp - FCB_SPECLEN], rcx

    ; ---- render both halves ------------------------------------------------
    mov rax, [rbp - FCB_SELF]
    mov rdi, [rax + PyComplexObject.cval_real]
    lea rsi, [rbp - FCB_SPEC]
    mov rdx, [rbp - FCB_SPECLEN]
    extern float_format_spec
    call float_format_spec
    V_UNPACK rax, rdx
    mov [rbp - FCB_RE], rax

    mov rax, [rbp - FCB_SELF]
    mov rdi, [rax + PyComplexObject.cval_imag]
    lea rsi, [rbp - FCB_SPEC]
    mov rdx, [rbp - FCB_SPECLEN]
    call float_format_spec
    V_UNPACK rax, rdx
    mov [rbp - FCB_IM], rax

    ; ---- join: [sign] re [+] im 'j' ----------------------------------------
    ; Room for both halves, an explicit sign on each, the 'j' and a NUL.
    mov rdi, [rbp - FCB_RE]
    mov rdi, [rdi + PyStrObject.ob_size]
    mov rax, [rbp - FCB_IM]
    add rdi, [rax + PyStrObject.ob_size]
    add rdi, 8
    extern ap_malloc
    call ap_malloc
    mov [rbp - FCB_OUT], rax
    mov rbx, rax                        ; rbx = write cursor

    ; The sign flag is the real part's; float_format_spec knows nothing of it.
    mov rax, [rbp - FCB_RE]
    cmp qword [rax + PyStrObject.ob_size], 0
    jle .fcb_no_sign
    cmp byte [rax + PyStrObject.data], '-'
    je .fcb_no_sign
    mov rcx, [r12 - FS_SIGN]
    cmp rcx, '+'
    je .fcb_put_sign
    cmp rcx, ' '
    jne .fcb_no_sign
.fcb_put_sign:
    mov [rbx], cl
    inc rbx
.fcb_no_sign:
    mov rdi, rbx
    mov rax, [rbp - FCB_RE]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    add rbx, rdx
    extern ap_memcpy
    call ap_memcpy

    ; The imaginary part is always signed.
    mov rax, [rbp - FCB_IM]
    cmp qword [rax + PyStrObject.ob_size], 0
    jle .fcb_im_signed
    cmp byte [rax + PyStrObject.data], '-'
    je .fcb_im_signed
    mov byte [rbx], '+'
    inc rbx
.fcb_im_signed:
    mov rdi, rbx
    mov rax, [rbp - FCB_IM]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    add rbx, rdx
    call ap_memcpy
    mov byte [rbx], 'j'
    inc rbx

    ; ---- wrap up -----------------------------------------------------------
    mov rdi, [rbp - FCB_OUT]
    mov rsi, rbx
    sub rsi, rdi                        ; the byte length
    extern str_new_heap
    call str_new_heap
    mov rbx, rax                        ; the finished body

    mov rdi, [rbp - FCB_OUT]
    extern ap_free
    call ap_free
    mov rdi, [rbp - FCB_RE]
    extern obj_decref
    call obj_decref
    mov rdi, [rbp - FCB_IM]
    call obj_decref

    ; '=' alignment is rejected for a complex, so there is no sign to hoist.
    mov qword [r12 - FS_SIGNCH], 0
    mov rax, rbx
    pop r12
    pop rbx
    leave
    ret

.fcb_repr:
    mov rdi, [rbp - FCB_SELF]
    extern complex_repr
    call complex_repr
    V_UNPACK rax, rdx
    mov qword [r12 - FS_SIGNCH], 0
    pop r12
    pop rbx
    leave
    ret
END_FUNC format_complex_body


;; ############################################################################
;;            %-FORMATTING: CHECKING THE ARGUMENT AGAINST THE CONVERSION
;; ############################################################################
;;
;; Every numeric conversion in str_mod used to format whatever it was handed,
;; so "%d" % "x" answered 'x' and "%i" % [] answered '[]' -- a wrong answer
;; with nothing to say anything was wrong.  This is the check, and the
;; coercion that goes with it: %d takes a float and truncates, %f takes an int
;; and widens, and both take an object that offers __index__ or __float__.

extern int_is_integer
extern int_from_i64
extern int_float
extern float_int
extern float_type
extern obj_as_index
extern dunder_lookup
extern dunder_call_1
extern type_is_subtype
extern raise_type_error_with_name

FPC_VAL   equ 8
FPC_CONV  equ 16
FPC_FRAME equ 32            ; + 0 pushes = 32

;; ============================================================================
;; fmt_percent_coerce(rdi = the argument Value, esi = the conversion character)
;;   -> rax = a Value the conversion can use, edx = 1 when it is a NEW
;;      reference the caller must release with DECREF_V
;; Raises TypeError when the argument cannot be used at all.
;; ============================================================================
global fmt_percent_coerce
DEF_FUNC fmt_percent_coerce, FPC_FRAME
    mov [rbp - FPC_VAL], rdi
    mov [rbp - FPC_CONV], rsi

    mov eax, esi
    cmp al, 'd'
    je .fpc_int_like
    cmp al, 'i'
    je .fpc_int_like
    cmp al, 'u'
    je .fpc_int_like
    cmp al, 'x'
    je .fpc_int_strict
    cmp al, 'X'
    je .fpc_int_strict
    cmp al, 'o'
    je .fpc_int_strict
    cmp al, 'b'
    je .fpc_int_strict
    cmp al, 'e'
    je .fpc_float
    cmp al, 'E'
    je .fpc_float
    cmp al, 'f'
    je .fpc_float
    cmp al, 'F'
    je .fpc_float
    cmp al, 'g'
    je .fpc_float
    cmp al, 'G'
    je .fpc_float

.fpc_pass:
    mov rax, [rbp - FPC_VAL]
    xor edx, edx
    leave
    ret

;; %d, %i and %u: an integer, a float truncated toward zero, or __index__.
.fpc_int_like:
    call .fpc_arg_is_int
    test eax, eax
    jnz .fpc_pass
    call .fpc_arg_is_float
    test eax, eax
    jz .fpc_int_dunder
    mov rdi, [rbp - FPC_VAL]
    call float_int
    mov edx, 1
    leave
    ret

;; %x, %X, %o and %b: an integer only.  A float is a TypeError, not a
;; truncation -- CPython is strict here and lax for %d.
.fpc_int_strict:
    call .fpc_arg_is_int
    test eax, eax
    jnz .fpc_pass

.fpc_int_dunder:
    mov rdi, [rbp - FPC_VAL]
    V_TEST_PTR rdi, rax
    ja .fpc_int_bad
    test rdi, rdi
    jz .fpc_int_bad
    mov rdi, [rdi + PyObject.ob_type]
    lea rsi, [rel fpc_name_index]
    call dunder_lookup
    test rax, rax
    jz .fpc_int_bad
    mov rdi, [rbp - FPC_VAL]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rdi, rax
    call int_from_i64
    V_PACK rax, rdx
    mov edx, 1
    leave
    ret

;; %e, %f, %g and their uppercase forms: a real number, or __float__.
.fpc_float:
    call .fpc_arg_is_float
    test eax, eax
    jnz .fpc_pass
    call .fpc_arg_is_int
    test eax, eax
    jz .fpc_float_dunder
    mov rdi, [rbp - FPC_VAL]
    call int_float
    mov edx, 1
    leave
    ret

.fpc_float_dunder:
    mov rdi, [rbp - FPC_VAL]
    V_TEST_PTR rdi, rax
    ja .fpc_float_bad
    test rdi, rdi
    jz .fpc_float_bad
    mov rdi, [rdi + PyObject.ob_type]
    lea rsi, [rel fpc_name_float]
    call dunder_lookup
    test rax, rax
    jz .fpc_float_bad
    mov rdi, [rbp - FPC_VAL]
    lea rsi, [rel fpc_name_float]
    call dunder_call_1
    mov edx, 1
    leave
    ret

;; .fpc_arg_is_int -> eax = 1 when the argument is an int, a bool or an int
;; subclass instance.  Reads the caller's slot, so it is not standalone.
.fpc_arg_is_int:
    sub rsp, 8
    mov rdi, [rbp - FPC_VAL]
    V_UNPACK rdi, rdx
    call int_is_integer
    add rsp, 8
    ret

;; .fpc_arg_is_float -> eax = 1 for a float immediate, a float, or a subclass.
.fpc_arg_is_float:
    sub rsp, 8
    mov rdi, [rbp - FPC_VAL]
    V_IS_FLOAT rdi, rax
    jb .fpc_aif_yes             ; CF=1 is the float immediate
    V_TEST_PTR rdi, rax
    ja .fpc_aif_no
    test rdi, rdi
    jz .fpc_aif_no
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel float_type]
    cmp rax, rcx
    je .fpc_aif_yes
    mov rdi, rax
    lea rsi, [rel float_type]
    call type_is_subtype
    test eax, eax
    jz .fpc_aif_no
.fpc_aif_yes:
    mov eax, 1
    add rsp, 8
    ret
.fpc_aif_no:
    xor eax, eax
    add rsp, 8
    ret

;; The messages carry the conversion character, so they are assembled rather
;; than picked from a list.  \x01 is raise_type_error_with_name's placeholder
;; for the argument's type name.
.fpc_int_bad:
    lea rsi, [rel fpc_msg_real]
    cmp qword [rbp - FPC_CONV], 'd'
    je .fpc_bad_build
    cmp qword [rbp - FPC_CONV], 'i'
    je .fpc_bad_build
    cmp qword [rbp - FPC_CONV], 'u'
    je .fpc_bad_build
    lea rsi, [rel fpc_msg_integer]
.fpc_bad_build:
    lea rdi, [rel fpc_msgbuf]
    mov rax, [rbp - FPC_CONV]
    mov byte [rdi], '%'
    mov [rdi + 1], al
    add rdi, 2
    xor ecx, ecx
.fpc_bad_copy:
    mov al, [rsi + rcx]
    mov [rdi + rcx], al
    test al, al
    jz .fpc_bad_raise
    inc rcx
    jmp .fpc_bad_copy
.fpc_bad_raise:
    lea rdi, [rel fpc_msgbuf]
    mov rsi, [rbp - FPC_VAL]
    call raise_type_error_with_name

.fpc_float_bad:
    lea rdi, [rel fpc_msg_notreal]
    mov rsi, [rbp - FPC_VAL]
    call raise_type_error_with_name

section .rodata
align 8
ffb_hundred:     dq 100.0
fpc_name_index:  db "__index__", 0
fpc_name_float:  db "__float__", 0
fpc_msg_real:    db ` format: a real number is required, not \x01`, 0
fpc_msg_integer: db ` format: an integer is required, not \x01`, 0
fpc_msg_notreal: db `must be real number, not \x01`, 0

section .bss
fpc_msgbuf: resb 96

section .text
END_FUNC fmt_percent_coerce
