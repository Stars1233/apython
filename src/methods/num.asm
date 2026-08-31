; methods/num.asm - int and float methods
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
extern ap_malloc
extern ap_free
extern ap_strcmp
extern obj_decref
extern str_new_heap
extern tuple_new
extern obj_call_n
extern int_to_i64
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
extern int_type
extern bytes_new
extern bytes_type
extern bool_false
extern bool_true

; Set entry layout constants (must match set.asm)

; --- moved to a sibling file by the split ---

section .text

;; ############################################################################
;;                       INT METHODS
;; ############################################################################

;; ============================================================================
;; HELPER: int_method_self_to_i64
;; Extract raw i64 from self, handling both SmallInt and heap int (subclasses).
;; Input: rdi = args pointer (args[0] = self)
;; Output: rax = raw i64
;; Clobbers: rcx, rdx
;; ============================================================================
DEF_FUNC int_method_self_to_i64
    mov rax, [rdi]              ; args[0] = self
    V_UNPACK rax, rdx
    cmp edx, TAG_SMALLINT
    jne .imsi_heap
    leave
    ret
.imsi_heap:
    ; TAG_PTR: heap int (subclass) — use int_to_i64
    mov rdi, [rdi]              ; heap int ptr
    call int_to_i64
    leave
    ret
END_FUNC int_method_self_to_i64

;; ============================================================================
;; int_method_bit_length(args, nargs) -> SmallInt
;; args[0] = self (SmallInt or heap int subclass)
;; Returns number of bits needed to represent abs(self), excluding sign and
;; leading zeros. bit_length(0) = 0.
;; ============================================================================
DEF_FUNC int_method_bit_length
    ; A value too large for int64 has to be measured on its mpz: going
    ; through int_to_i64 truncated it, so (2**63).bit_length() was 0.
    mov rax, [rdi]
    V_UNPACK rax, rdx
    cmp edx, TAG_SMALLINT
    je .ibl_small
    cmp edx, TAG_PTR
    jne .ibl_small
    cmp qword [rax + PyIntObject.compact], 0
    jne .ibl_small

    push rbx
    mov rbx, rax
    lea rdi, [rbx + PyIntObject.mpz]
    xor esi, esi
    extern __gmpz_cmp_si
    call __gmpz_cmp_si wrt ..plt
    test eax, eax
    jz .ibl_mpz_zero
    lea rdi, [rbx + PyIntObject.mpz]
    mov esi, 2
    extern __gmpz_sizeinbase
    call __gmpz_sizeinbase wrt ..plt
    pop rbx
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.ibl_mpz_zero:
    pop rbx
    xor eax, eax
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx
    ret

.ibl_small:
    call int_method_self_to_i64

    ; abs(self)
    mov rcx, rax
    neg rcx
    cmovs rcx, rax              ; rcx = abs(self)

    ; bit_length = 0 for 0
    test rcx, rcx
    jz .ibl_zero

    ; bsr finds highest set bit (0-indexed)
    bsr rax, rcx
    inc rax                     ; bit_length = highest_bit + 1
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.ibl_zero:
    xor eax, eax
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC int_method_bit_length

;; ============================================================================
;; int_method_bit_count(args, nargs) -> SmallInt
;; Returns number of ones in the binary representation of abs(self).
;; ============================================================================
DEF_FUNC int_method_bit_count
    call int_method_self_to_i64

    ; abs(self)
    mov rcx, rax
    neg rcx
    cmovs rcx, rax              ; rcx = abs(self)

    ; popcnt counts 1 bits
    popcnt rax, rcx
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC int_method_bit_count


;; ============================================================================
;; int_method_conjugate(args, nargs) -> SmallInt
;; ============================================================================
DEF_FUNC int_method_conjugate
    call int_method_self_to_i64
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC int_method_conjugate



;; ============================================================================
;; int_method_to_bytes(args, nargs) -> bytes
;; args[0]=self, args[1]=length, args[2]=byteorder ("big" or "little")
;; Optional kwarg: signed=False (via kw_names_pending)
;; ============================================================================

ITB_SELF  equ 8
ITB_LEN   equ 16
ITB_SIGN  equ 24
ITB_FRAME equ 32

DEF_FUNC int_method_to_bytes, ITB_FRAME
    push rbx
    push r12

    mov qword [rbp - ITB_SIGN], 0   ; signed = False

    ; Extract self value
    mov rbx, rdi
    call int_method_self_to_i64
    mov [rbp - ITB_SELF], rax       ; self i64

    ; Extract length arg
    mov r12, [rbx + 8]             ; args[1]
    V_UNPACK r12, rdx
    cmp edx, TAG_SMALLINT
    jne .itb_error
    mov [rbp - ITB_LEN], r12

    ; Extract byteorder arg
    mov rcx, [rbx + 16]            ; args[2] payload (str)
    V_UNPACK rcx, rdx       ; args[2]
    cmp edx, TAG_PTR
    jne .itb_error

    ; Check for "big" or "little"
    ; rcx = byteorder str obj
    push rcx                        ; save for comparison

    ; Compare with "big"
    lea rdi, [rcx + PyStrObject.data]
    CSTRING rsi, "big"
    call ap_strcmp
    pop rcx
    test eax, eax
    jz .itb_big

    push rcx
    lea rdi, [rcx + PyStrObject.data]
    CSTRING rsi, "little"
    call ap_strcmp
    pop rcx
    test eax, eax
    jz .itb_little

    jmp .itb_order_error

.itb_big:
    ; Big-endian: MSB first
    mov rdi, r12                    ; length
    call bytes_new
    mov rbx, rax

    ; Fill from end to start
    mov rax, [rbp - ITB_SELF]
    mov rcx, r12
.itb_big_loop:
    test rcx, rcx
    jz .itb_return
    dec rcx
    mov [rbx + PyBytesObject.data + rcx], al
    shr rax, 8
    jmp .itb_big_loop

.itb_little:
    ; Little-endian: LSB first
    mov rdi, r12
    call bytes_new
    mov rbx, rax

    mov rax, [rbp - ITB_SELF]
    xor ecx, ecx
.itb_little_loop:
    cmp rcx, r12
    jge .itb_return
    mov [rbx + PyBytesObject.data + rcx], al
    shr rax, 8
    inc rcx
    jmp .itb_little_loop

.itb_return:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.itb_error:
    RAISE exc_TypeError_type, "to_bytes() requires (length, byteorder) arguments"

.itb_order_error:
    RAISE exc_ValueError_type, "byteorder must be 'little' or 'big'"
END_FUNC int_method_to_bytes

;; ============================================================================
;; int_classmethod_from_bytes(args, nargs) -> SmallInt
;; args[0]=cls (type), args[1]=bytes, args[2]=byteorder ("big" or "little")
;; This is a classmethod: cls is passed as first arg.
;; ============================================================================

IFB_BYTES equ 8
IFB_CLS   equ 16
IFB_VAL   equ 24
IFB_OWNED equ 32
IFB_NARGS equ 40
IFB_ARGS  equ 48
IFB_FRAME equ 64          ; + 2 pushes = 80

DEF_FUNC int_classmethod_from_bytes, IFB_FRAME
    push rbx
    push r12

    cmp rsi, 2
    jl .ifb_error
    mov [rbp - IFB_ARGS], rdi       ; the conversion below clobbers rdi
    mov rax, [rdi]
    mov [rbp - IFB_CLS], rax        ; cls, for a subclass result
    mov qword [rbp - IFB_OWNED], 0

    ; args[1] is any iterable of ints, not only a bytes: ipaddress passes a
    ; map object.  Anything that is not already a bytes goes through bytes()
    ; first, which is where CPython's own conversion lives too.
    mov rax, [rdi + 8]
    V_TEST_PTR rax, rcx
    ja .ifb_convert
    test rax, rax
    jz .ifb_convert
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    je .ifb_have_bytes
.ifb_convert:
    mov [rbp - IFB_VAL], rax
    mov [rbp - IFB_NARGS], rsi      ; a push here would misalign the call
    lea rdi, [rel bytes_type]
    lea rsi, [rbp - IFB_VAL]
    mov edx, 1
    extern obj_call_n
    call obj_call_n
    mov rsi, [rbp - IFB_NARGS]
    test rax, rax
    jz .ifb_failed
    mov [rbp - IFB_OWNED], rax
.ifb_have_bytes:
    mov [rbp - IFB_BYTES], rax

    ; args[2] = byteorder.  It has defaulted to 'big' since 3.11, and reading
    ; it unconditionally walked off the end of the argument array -- which is
    ; how ipaddress calls it.
    cmp rsi, 3
    jl .ifb_big
    mov rdi, [rbp - IFB_ARGS]
    mov rcx, [rdi + 16]            ; payload
    V_UNPACK rcx, rdx       ; args[2]
    cmp edx, TAG_PTR
    jne .ifb_error
    push rcx

    lea rdi, [rcx + PyStrObject.data]
    CSTRING rsi, "big"
    call ap_strcmp
    pop rcx
    test eax, eax
    jz .ifb_big

    push rcx
    lea rdi, [rcx + PyStrObject.data]
    CSTRING rsi, "little"
    call ap_strcmp
    pop rcx
    test eax, eax
    jz .ifb_little

    jmp .ifb_order_error

.ifb_big:
    ; Big-endian: MSB first
    mov rax, [rbp - IFB_BYTES]
    mov rcx, [rax + PyBytesObject.ob_size]
    lea rsi, [rax + PyBytesObject.data]
    xor r12, r12                    ; result = 0
    xor edx, edx                   ; index
.ifb_big_loop:
    cmp rdx, rcx
    jge .ifb_return
    shl r12, 8
    movzx eax, byte [rsi + rdx]
    or r12, rax
    inc rdx
    jmp .ifb_big_loop

.ifb_little:
    ; Little-endian: LSB first
    mov rax, [rbp - IFB_BYTES]
    mov rcx, [rax + PyBytesObject.ob_size]
    lea rsi, [rax + PyBytesObject.data]
    xor r12, r12
    mov rdx, rcx
    dec rdx
.ifb_little_loop:
    test rdx, rdx
    js .ifb_return
    shl r12, 8
    movzx eax, byte [rsi + rdx]
    or r12, rax
    dec rdx
    jmp .ifb_little_loop

.ifb_return:
    mov rdi, [rbp - IFB_OWNED]
    test rdi, rdi
    jz .ifb_no_owned
    mov qword [rbp - IFB_OWNED], 0
    call obj_decref
.ifb_no_owned:
    ; A classmethod builds an instance of the class it was reached through:
    ; `I.from_bytes(b)` is an I, not an int.
    mov rax, [rbp - IFB_CLS]
    V_TEST_PTR rax, rcx
    ja .ifb_plain
    test rax, rax
    jz .ifb_plain
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .ifb_plain
    mov rcx, [rax + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_INT_SUBCLASS
    jz .ifb_plain
    mov rax, r12
    V_PACK_I64 rax, rcx
    mov [rbp - IFB_VAL], rax
    mov rdi, [rbp - IFB_CLS]
    lea rsi, [rbp - IFB_VAL]
    mov edx, 1
    extern int_sub_new
    call int_sub_new
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.ifb_plain:
    mov rax, r12
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.ifb_failed:
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.ifb_error:
    RAISE exc_TypeError_type, "from_bytes() requires (bytes, byteorder) arguments"

.ifb_order_error:
    RAISE exc_ValueError_type, "byteorder must be 'little' or 'big'"
END_FUNC int_classmethod_from_bytes




;; ############################################################################
;;                       FLOAT METHODS
;; ############################################################################

;; ============================================================================
;; float_method_is_integer(args, nargs) -> Bool
;; args[0] = self (Float: payload=raw double bits, tag=TAG_FLOAT)
;; Returns True if float has no fractional part.
;; ============================================================================

DEF_FUNC float_method_is_integer
    mov rax, [rdi]              ; args[0] = self
    V_TO_F64 rax                ; raw double bits
    movq xmm0, rax

    ; Check for inf/nan — not integer
    movq rax, xmm0
    mov rcx, 0x7ff0000000000000  ; inf exponent mask
    and rax, rcx
    cmp rax, rcx
    je .fii_false               ; inf or nan

    ; Compare floor(x) == x
    roundsd xmm1, xmm0, 1      ; xmm1 = floor(xmm0) (round toward -inf)
    ucomisd xmm0, xmm1
    jp .fii_false               ; NaN
    jne .fii_false              ; not equal

    ; True
    mov eax, 1
    RET_BOOL_RAX
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.fii_false:
    xor eax, eax
    RET_BOOL_RAX
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC float_method_is_integer

;; ============================================================================
;; float_method_conjugate(args, nargs) -> Float (return self)
;; ============================================================================
DEF_FUNC_BARE float_method_conjugate
    mov rax, [rdi]              ; args[0] = self
    V_TO_F64 rax                ; raw double bits
    mov edx, TAG_FLOAT
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC float_method_conjugate





;; ============================================================================
;; float_method_as_integer_ratio(args, nargs) -> 2-tuple (numerator, denominator)
;; Extract IEEE 754 mantissa/exponent and return (n, d) as SmallInts.
;; ============================================================================
extern exc_OverflowError_type

FIR_FRAME equ 8
DEF_FUNC float_method_as_integer_ratio, FIR_FRAME
    push rbx

    mov rax, [rdi]              ; args[0] = self
    V_TO_F64 rax                ; raw double bits

    ; Check for inf/nan
    mov rcx, rax
    mov rdx, 0x7ff0000000000000
    and rcx, rdx
    cmp rcx, rdx
    je .fir_error

    ; Check for zero
    mov rcx, rax
    btr rcx, 63                 ; clear sign
    test rcx, rcx
    jz .fir_zero

    ; Extract sign, exponent, mantissa from IEEE 754
    ; sign = bit 63, exponent = bits 62-52 (biased), mantissa = bits 51-0
    mov r8, rax                 ; save original bits
    mov rcx, rax
    shr rcx, 52
    and ecx, 0x7ff              ; biased exponent
    sub ecx, 1023               ; unbiased exponent
    sub ecx, 52                 ; adjust for mantissa bits

    ; mantissa with implicit 1 bit
    mov rax, r8
    mov rdx, 0x000fffffffffffff
    and rax, rdx
    bts rax, 52                 ; set implicit bit (bit 52)

    ; Reduce: strip trailing zeros from mantissa (common factor of 2)
    ; This makes the fraction fully reduced
    tzcnt rdx, rax              ; count trailing zeros
    mov cl, dl
    shr rax, cl                 ; mantissa >>= trailing_zeros

    ; Reload exponent (ecx was clobbered by cl usage)
    mov rcx, r8
    shr rcx, 52
    and ecx, 0x7ff
    sub ecx, 1023
    sub ecx, 52
    add ecx, edx               ; adjust exponent by trailing zeros stripped

    ; Apply sign
    bt r8, 63
    jnc .fir_positive
    neg rax
.fir_positive:

    ; Now: value = rax * 2^ecx
    ; If ecx >= 0: numerator = rax << ecx, denominator = 1
    ; If ecx < 0: numerator = rax, denominator = 1 << (-ecx)
    test ecx, ecx
    js .fir_neg_exp

    ; Positive exponent: shift numerator left
    cmp ecx, 62                 ; limit to prevent overflow
    ja .fir_error
    mov cl, cl
    shl rax, cl
    push rax                    ; numerator

    ; Build 2-tuple (numerator=rax, denominator=1)
    mov rdi, 2
    call tuple_new
    mov rbx, rax
    pop rcx                     ; numerator

    mov r9, [rbx + PyTupleObject.ob_item]
    V_PACK_I64 rcx, r10
    mov [r9], rcx
    mov rcx, 1
    V_PACK_I64 rcx, r10
    mov [r9 + 8], rcx

    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.fir_neg_exp:
    ; Negative exponent
    neg ecx
    cmp ecx, 62
    ja .fir_error
    push rax                    ; save numerator
    mov rdx, 1
    shl rdx, cl                 ; denominator = 1 << (-ecx)
    push rdx                    ; save denominator

    mov rdi, 2
    call tuple_new
    mov rbx, rax
    pop rdx                     ; denominator
    pop rcx                     ; numerator

    mov r9, [rbx + PyTupleObject.ob_item]
    V_PACK_I64 rcx, r10
    mov [r9], rcx
    V_PACK_I64 rdx, r10
    mov [r9 + 8], rdx

    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.fir_zero:
    ; Return (0, 1)
    mov rdi, 2
    call tuple_new
    mov rbx, rax

    mov r9, [rbx + PyTupleObject.ob_item]
    xor ecx, ecx
    V_PACK_I64 rcx, r10
    mov [r9], rcx
    mov rcx, 1
    V_PACK_I64 rcx, r10
    mov [r9 + 8], rcx

    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.fir_error:
    RAISE exc_OverflowError_type, "cannot convert float infinity or NaN to integer ratio"
END_FUNC float_method_as_integer_ratio

;; ============================================================================
;; float_method_hex(args, nargs) -> str
;; Format double as '0x1.XXXXp+YY' hex string.
;; ============================================================================
FH_BUF    equ 8
FH_FRAME  equ 16

DEF_FUNC float_method_hex, FH_FRAME
    push rbx
    push r12

    mov rax, [rdi]              ; args[0] = self
    V_TO_F64 rax                ; raw double bits
    mov rbx, rax                ; save bits

    ; Allocate temp buffer (64 bytes is enough for any hex float)
    mov edi, 64
    call ap_malloc
    mov [rbp - FH_BUF], rax
    mov r12, rax                ; write pointer

    ; Check sign
    bt rbx, 63
    jnc .fh_nosign
    mov byte [r12], '-'
    inc r12
.fh_nosign:

    ; Clear sign for analysis
    mov rax, rbx
    btr rax, 63

    ; Check for zero
    test rax, rax
    jz .fh_zero

    ; Check for inf
    mov rcx, 0x7ff0000000000000
    cmp rax, rcx
    je .fh_inf

    ; Check for NaN
    mov rdx, rax
    and rdx, rcx
    cmp rdx, rcx
    je .fh_nan

    ; Normal float: extract exponent and mantissa
    mov rdx, rax
    shr rdx, 52
    and edx, 0x7ff              ; biased exponent
    sub edx, 1023               ; unbiased

    mov rcx, rax
    mov r8, 0x000fffffffffffff
    and rcx, r8                 ; mantissa bits (52 bits)

    ; Write "0x1."
    mov byte [r12], '0'
    mov byte [r12+1], 'x'
    mov byte [r12+2], '1'
    mov byte [r12+3], '.'
    add r12, 4

    ; Convert mantissa to 13 hex digits (52 bits / 4 = 13 digits)
    ; Write hex digits from high nibble to low
    mov rax, rcx
    mov ecx, 13                 ; 13 hex digits
    mov r8d, 48                 ; shift = 48 (start from high)
.fh_hex_loop:
    test ecx, ecx
    jz .fh_hex_done
    push rcx
    mov cl, r8b
    mov rdx, rax
    shr rdx, cl
    and edx, 0x0f
    pop rcx
    cmp edx, 10
    jb .fh_digit
    add edx, ('a' - 10)
    jmp .fh_store_digit
.fh_digit:
    add edx, '0'
.fh_store_digit:
    mov [r12], dl
    inc r12
    sub r8d, 4
    dec ecx
    jmp .fh_hex_loop

.fh_hex_done:

    ; Write 'p' and exponent
    mov byte [r12], 'p'
    inc r12

    ; edx = unbiased exponent (stored in [rsp area])
    ; We need to reload it; it was in edx before hex loop
    ; Actually we lost edx. Let's recompute.
    mov rax, rbx
    btr rax, 63
    shr rax, 52
    and eax, 0x7ff
    sub eax, 1023

    ; Write sign of exponent
    test eax, eax
    js .fh_exp_neg
    mov byte [r12], '+'
    inc r12
    jmp .fh_exp_write
.fh_exp_neg:
    mov byte [r12], '-'
    inc r12
    neg eax
.fh_exp_write:
    ; Convert exponent to decimal string
    ; eax = absolute exponent value
    ; Use simple div loop
    push r12                    ; save start of exponent digits
    mov ecx, 10
    xor r8d, r8d               ; digit count
    test eax, eax
    jnz .fh_exp_digits
    ; Zero exponent
    mov byte [r12], '0'
    inc r12
    jmp .fh_exp_done
.fh_exp_digits:
    ; Push digits in reverse
    xor edx, edx
    div ecx                     ; eax = quotient, edx = remainder
    push rdx
    inc r8d
    test eax, eax
    jnz .fh_exp_digits
    ; Pop digits into buffer
.fh_exp_pop:
    test r8d, r8d
    jz .fh_exp_done
    pop rax
    add eax, '0'
    mov [r12], al
    inc r12
    dec r8d
    jmp .fh_exp_pop
.fh_exp_done:
    pop rax                     ; discard saved start pos

    ; Create string from buffer
    mov rdi, [rbp - FH_BUF]
    mov rsi, r12
    sub rsi, rdi                ; length
    call str_new_heap
    push rax

    mov rdi, [rbp - FH_BUF]
    call ap_free

    pop rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.fh_zero:
    ; Write "0x0.0p+0"
    mov byte [r12], '0'
    mov byte [r12+1], 'x'
    mov byte [r12+2], '0'
    mov byte [r12+3], '.'
    mov byte [r12+4], '0'
    mov byte [r12+5], 'p'
    mov byte [r12+6], '+'
    mov byte [r12+7], '0'
    add r12, 8
    jmp .fh_make_str

.fh_inf:
    mov byte [r12], 'i'
    mov byte [r12+1], 'n'
    mov byte [r12+2], 'f'
    add r12, 3
    jmp .fh_make_str

.fh_nan:
    mov byte [r12], 'n'
    mov byte [r12+1], 'a'
    mov byte [r12+2], 'n'
    add r12, 3

.fh_make_str:
    mov rdi, [rbp - FH_BUF]
    mov rsi, r12
    sub rsi, rdi
    call str_new_heap
    push rax

    mov rdi, [rbp - FH_BUF]
    call ap_free

    pop rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC float_method_hex

;; ============================================================================
;; float_classmethod_fromhex(args, nargs) -> Float
;; args[0]=cls (type), args[1]=hex string like "0x1.XXXXp+YY"
;; Parses hex float string and returns TAG_FLOAT.
;; ============================================================================
FFH_STR   equ 8
FFH_FRAME equ 16

DEF_FUNC float_classmethod_fromhex, FFH_FRAME
    push rbx
    push r12
    push r13

    ; Get string arg
    mov rcx, [rdi + 8]            ; args[1] payload
    mov [rbp - FFH_STR], rcx
    lea r12, [rcx + PyStrObject.data]  ; r12 = string data

    ; Parse: optional '-', '0x', mantissa '1.XXXX', 'p', exponent
    xor r13d, r13d                  ; r13 = sign (0 = positive)
    xor ebx, ebx                   ; current position

    ; Check for sign
    movzx eax, byte [r12]
    cmp al, '-'
    jne .ffh_check_plus
    mov r13d, 1
    inc ebx
    jmp .ffh_check_0x
.ffh_check_plus:
    cmp al, '+'
    jne .ffh_check_0x
    inc ebx

.ffh_check_0x:
    ; Expect '0x' or '0X'
    cmp byte [r12 + rbx], '0'
    jne .ffh_parse_error
    inc ebx
    movzx eax, byte [r12 + rbx]
    or al, 0x20                     ; lowercase
    cmp al, 'x'
    jne .ffh_parse_error
    inc ebx

    ; Parse integer part (digits before '.')
    xor ecx, ecx                   ; mantissa = 0 (as integer, shifted later)
    ; Parse hex digits
.ffh_int_digits:
    movzx eax, byte [r12 + rbx]
    call .ffh_hex_val               ; eax = hex value or -1
    cmp eax, -1
    je .ffh_int_done
    shl rcx, 4
    or rcx, rax
    inc ebx
    jmp .ffh_int_digits
.ffh_int_done:

    ; Check for '.'
    xor r8d, r8d                    ; frac_bits = 0 (count of hex digits after .)
    cmp byte [r12 + rbx], '.'
    jne .ffh_check_p
    inc ebx

    ; Parse fractional hex digits
.ffh_frac_digits:
    movzx eax, byte [r12 + rbx]
    push rcx
    push r8
    call .ffh_hex_val
    pop r8
    pop rcx
    cmp eax, -1
    je .ffh_check_p
    shl rcx, 4
    or rcx, rax
    inc r8d
    inc ebx
    jmp .ffh_frac_digits

.ffh_check_p:
    ; rcx = combined mantissa, r8d = fractional hex digits
    ; Expect 'p' or 'P'
    movzx eax, byte [r12 + rbx]
    or al, 0x20
    cmp al, 'p'
    jne .ffh_parse_error
    inc ebx

    ; Parse exponent (decimal, with optional sign)
    xor r9d, r9d                    ; exp_sign = 0
    movzx eax, byte [r12 + rbx]
    cmp al, '-'
    jne .ffh_exp_check_plus
    mov r9d, 1
    inc ebx
    jmp .ffh_exp_digits
.ffh_exp_check_plus:
    cmp al, '+'
    jne .ffh_exp_digits
    inc ebx

.ffh_exp_digits:
    xor r10d, r10d                  ; exponent value
.ffh_exp_loop:
    movzx eax, byte [r12 + rbx]
    sub al, '0'
    cmp al, 9
    ja .ffh_exp_done
    imul r10d, 10
    movzx eax, al
    add r10d, eax
    inc ebx
    jmp .ffh_exp_loop
.ffh_exp_done:
    test r9d, r9d
    jz .ffh_compute
    neg r10d

.ffh_compute:
    ; rcx = mantissa bits, r8d = fractional hex digits, r10d = exponent
    ; Actual exponent = r10d - (r8d * 4)  [each hex digit = 4 bits]
    mov eax, r8d
    shl eax, 2                      ; * 4
    sub r10d, eax                   ; adjusted exponent

    ; Convert to double: value = mantissa * 2^exponent
    ; Use integer -> double conversion then ldexp
    cvtsi2sd xmm0, rcx             ; mantissa as double

    ; Apply exponent via repeated multiply/divide by 2
    test r10d, r10d
    jz .ffh_apply_sign
    js .ffh_neg_exp_apply

    ; Positive exponent: multiply by 2^exp
.ffh_pos_exp:
    ; Use a loop to multiply by 2 for each bit
    mov ecx, r10d
.ffh_mul_loop:
    test ecx, ecx
    jz .ffh_apply_sign
    addsd xmm0, xmm0              ; xmm0 *= 2
    dec ecx
    jmp .ffh_mul_loop

.ffh_neg_exp_apply:
    neg r10d
    mov ecx, r10d
    mov rax, 0x3ff0000000000000    ; 1.0
    movq xmm1, rax
    mov rax, 0x4000000000000000    ; 2.0
    movq xmm2, rax
.ffh_div_loop:
    test ecx, ecx
    jz .ffh_apply_sign
    divsd xmm0, xmm2              ; xmm0 /= 2
    dec ecx
    jmp .ffh_div_loop

.ffh_apply_sign:
    test r13d, r13d
    jz .ffh_return
    ; Negate
    mov rax, 0x8000000000000000
    movq xmm1, rax
    xorpd xmm0, xmm1

.ffh_return:
    movq rax, xmm0
    mov edx, TAG_FLOAT
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

; Local helper: convert hex char in al to value in eax, or -1
.ffh_hex_val:
    movzx eax, byte [r12 + rbx]
    cmp al, '0'
    jb .ffh_hv_bad
    cmp al, '9'
    ja .ffh_hv_alpha
    sub eax, '0'
    ret
.ffh_hv_alpha:
    or al, 0x20                     ; lowercase
    cmp al, 'a'
    jb .ffh_hv_bad
    cmp al, 'f'
    ja .ffh_hv_bad
    sub eax, 'a'
    add eax, 10
    ret
.ffh_hv_bad:
    mov eax, -1
    ret

.ffh_parse_error:
    RAISE exc_ValueError_type, "invalid hexadecimal floating-point string"
END_FUNC float_classmethod_fromhex
