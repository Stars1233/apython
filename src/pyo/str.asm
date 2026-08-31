; str_obj.asm - String type
; Phase 8: full string operations

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern none_singleton
extern ap_malloc
extern ap_free
extern ap_strlen
extern ap_memcpy
extern ap_strcmp
extern bool_true
extern bool_false
extern int_from_i64
extern int_to_i64
extern fatal_error
extern raise_exception
extern exc_IndexError_type
extern exc_TypeError_type
extern int_type
extern obj_as_index
extern int_fits_i64
extern exc_OverflowError_type
extern slice_type
extern slice_indices
extern type_type
extern obj_dealloc


; ----------------------------------------------------------------------------
; str_cp_width(rdi = bytes, rsi = byte length, rdx = offset) -> rax = width
;
; How many bytes the code point starting at `offset` occupies.  Every walk
; over a string has to agree on this or the two index spaces drift apart: a
; lead byte whose continuation bytes are missing, or a stray continuation byte
; with no lead, is one code point of one byte, so a string that is not valid
; UTF-8 has exactly as many code points as it has bytes and behaves the way it
; did before there were two lengths at all.  bytes.decode() does not validate,
; so such a string is reachable.
; ----------------------------------------------------------------------------
global str_cp_width
DEF_FUNC_BARE str_cp_width
    movzx ecx, byte [rdi + rdx]
    cmp cl, 0x80
    jb .one                         ; ASCII
    cmp cl, 0xC0
    jb .one                         ; a continuation byte with no lead
    mov eax, 2
    cmp cl, 0xE0
    jb .have_width
    mov eax, 3
    cmp cl, 0xF0
    jb .have_width
    mov eax, 4
    cmp cl, 0xF8
    jb .have_width
    jmp .one                        ; 0xF8..0xFF is not a lead byte

.have_width:
    ; Truncate at the end of the string, or at the first byte that is not a
    ; continuation -- a sequence that was cut short is not one code point.
    mov r8, rsi
    sub r8, rdx                     ; bytes remaining
    cmp rax, r8
    jle .check_cont
    mov rax, r8
.check_cont:
    mov r9d, 1
.cont_loop:
    cmp r9, rax
    jge .done
    lea r10, [rdx + r9]
    movzx ecx, byte [rdi + r10]
    and cl, 0xC0
    cmp cl, 0x80
    jne .truncate
    inc r9
    jmp .cont_loop
.truncate:
    mov rax, r9
.done:
    ret
.one:
    mov eax, 1
    ret
END_FUNC str_cp_width

; ----------------------------------------------------------------------------
; str_count_codepoints(rdi = bytes, rsi = byte length) -> rax = code points
; ----------------------------------------------------------------------------
global str_count_codepoints
DEF_FUNC str_count_codepoints
    push rbx
    push r12
    push r13
    push r14
    mov rbx, rdi
    mov r12, rsi
    xor r13d, r13d                  ; byte cursor
    xor r14d, r14d                  ; code points
.scan:
    cmp r13, r12
    jge .done
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call str_cp_width
    add r13, rax
    inc r14
    jmp .scan
.done:
    mov rax, r14
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_count_codepoints

; ----------------------------------------------------------------------------
; str_set_length(rdi = PyStrObject*) -- fill ob_length from the bytes.
; ----------------------------------------------------------------------------
global str_set_length
DEF_FUNC str_set_length
    push rbx
    mov rbx, rdi
    mov rsi, [rbx + PyStrObject.ob_size]
    lea rdi, [rbx + PyStrObject.data]
    call str_count_codepoints
    mov [rbx + PyStrObject.ob_length], rax
    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC str_set_length


; ----------------------------------------------------------------------------
; str_byte_to_cp(rdi = PyStrObject*, rsi = byte offset) -> rax = code point index
; The inverse of str_cp_offset, for the methods that search in bytes and have
; to report a position in code points.
; ----------------------------------------------------------------------------
global str_byte_to_cp
DEF_FUNC str_byte_to_cp
    mov rax, [rdi + PyStrObject.ob_size]
    cmp rax, [rdi + PyStrObject.ob_length]
    je .ascii                       ; one byte per code point
    test rsi, rsi
    js .ascii                       ; -1 means "not found"; pass it through

    push rbx
    push r12
    push r13
    push r14
    mov r12, [rdi + PyStrObject.ob_size]
    lea rbx, [rdi + PyStrObject.data]
    mov r14, rsi                    ; the byte offset asked about
    xor r13d, r13d                  ; byte cursor
    xor eax, eax                    ; code points seen
.walk:
    cmp r13, r14
    jge .done
    cmp r13, r12
    jge .done
    push rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call str_cp_width
    add r13, rax
    pop rax
    inc rax
    jmp .walk
.done:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.ascii:
    mov rax, rsi
    leave
    ret
END_FUNC str_byte_to_cp

; ----------------------------------------------------------------------------
; str_cp_offset(rdi = PyStrObject*, rsi = code point index) -> rax = byte offset
; The index is not bounds-checked; an index at or past the end gives ob_size.
; ----------------------------------------------------------------------------
global str_cp_offset
DEF_FUNC str_cp_offset
    mov rax, [rdi + PyStrObject.ob_size]
    cmp rax, [rdi + PyStrObject.ob_length]
    je .ascii                       ; one byte per code point

    push rbx
    push r12
    push r13
    push r14
    mov r12, [rdi + PyStrObject.ob_size]
    lea rbx, [rdi + PyStrObject.data]
    mov r14, rsi                    ; the code point index asked about
    xor r13d, r13d                  ; byte cursor
    xor eax, eax                    ; code points seen
.walk:
    cmp rax, r14
    jge .done
    cmp r13, r12
    jge .done
    push rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call str_cp_width
    add r13, rax
    pop rax
    inc rax
    jmp .walk
.done:
    mov rax, r13
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.ascii:
    mov rax, rsi
    cmp rax, [rdi + PyStrObject.ob_size]
    jle .ascii_done
    mov rax, [rdi + PyStrObject.ob_size]
.ascii_done:
    leave
    ret
END_FUNC str_cp_offset


; ----------------------------------------------------------------------------
; codec_id(rdi = encoding str, or 0 for the default) -> eax
;   0 = utf-8, 1 = ascii, 2 = latin-1.  Raises LookupError for anything else.
;
; The three codecs the interpreter can do itself.  Everything else goes
; through the codecs module, which is Python and cannot be reached from here.
; ----------------------------------------------------------------------------
CI_BUF   equ 48
CI_FRAME equ 64
global codec_id
DEF_FUNC codec_id, CI_FRAME
    push rbx
    ; ap_strcmp compares eight bytes at a time, so the buffer has to be zeroed
    ; past the terminator or it reads uninitialised stack.
    mov qword [rbp - CI_BUF], 0
    mov qword [rbp - CI_BUF + 8], 0
    mov qword [rbp - CI_BUF + 16], 0
    mov qword [rbp - CI_BUF + 24], 0
    mov qword [rbp - CI_BUF + 32], 0
    mov qword [rbp - CI_BUF + 40], 0
    test rdi, rdi
    jz .ci_utf8
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .ci_unknown
    mov rcx, [rdi + PyStrObject.ob_size]
    test rcx, rcx
    jz .ci_utf8
    cmp rcx, 31
    ja .ci_unknown

    ; Normalise: lowercase, and '_' for '-', as CPython's normalizestring does.
    lea rbx, [rbp - CI_BUF]
    xor edx, edx
.ci_norm:
    cmp rdx, rcx
    jge .ci_norm_done
    movzx eax, byte [rdi + PyStrObject.data + rdx]
    cmp al, '-'
    jne .ci_not_dash
    mov al, '_'
    jmp .ci_store
.ci_not_dash:
    cmp al, 'A'
    jb .ci_store
    cmp al, 'Z'
    ja .ci_store
    add al, 32
.ci_store:
    mov [rbx + rdx], al
    inc rdx
    jmp .ci_norm
.ci_norm_done:
    mov byte [rbx + rdx], 0

    mov rdi, rbx
    CSTRING rsi, "utf_8"
    call ap_strcmp
    test eax, eax
    jz .ci_utf8
    mov rdi, rbx
    CSTRING rsi, "utf8"
    call ap_strcmp
    test eax, eax
    jz .ci_utf8
    mov rdi, rbx
    CSTRING rsi, "ascii"
    call ap_strcmp
    test eax, eax
    jz .ci_ascii
    mov rdi, rbx
    CSTRING rsi, "us_ascii"
    call ap_strcmp
    test eax, eax
    jz .ci_ascii
    mov rdi, rbx
    CSTRING rsi, "latin_1"
    call ap_strcmp
    test eax, eax
    jz .ci_latin1
    mov rdi, rbx
    CSTRING rsi, "latin1"
    call ap_strcmp
    test eax, eax
    jz .ci_latin1
    mov rdi, rbx
    CSTRING rsi, "iso_8859_1"
    call ap_strcmp
    test eax, eax
    jz .ci_latin1
    mov rdi, rbx
    CSTRING rsi, "iso8859_1"
    call ap_strcmp
    test eax, eax
    jz .ci_latin1
    jmp .ci_unknown

.ci_utf8:
    xor eax, eax
    pop rbx
    leave
    ret
.ci_ascii:
    mov eax, 1
    pop rbx
    leave
    ret
.ci_latin1:
    mov eax, 2
    pop rbx
    leave
    ret
.ci_unknown:
    extern exc_LookupError_type
    lea rdi, [rel exc_LookupError_type]
    CSTRING rsi, "unknown encoding"
    call raise_exception
END_FUNC codec_id

; str_from_cstr_heap(const char *cstr) -> (rax=PyStrObject*, edx=TAG_PTR)
; Always heap-allocates. For struct fields that need a real pointer.
DEF_FUNC str_from_cstr_heap
    push rbx
    push r12

    mov rbx, rdi            ; save cstr

    ; Get string length
    call ap_strlen
    mov r12, rax             ; r12 = length

    ; Allocate: PyStrObject header + length + 8 (null + padding for 8-byte strcmp)
    lea rdi, [rax + PyStrObject.data + 8]
    call ap_malloc
    ; rax = new PyStrObject*

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyStrObject.ob_size], r12
    mov qword [rax + PyStrObject.ob_hash], -1  ; not computed

    ; Copy string data
    push rax                 ; save obj ptr
    lea rdi, [rax + PyStrObject.data]
    mov rsi, rbx             ; source = cstr
    lea rdx, [r12 + 1]      ; length + null
    call ap_memcpy
    pop rax                  ; restore obj ptr

    ; Zero-fill 8 bytes at NUL terminator for ap_strcmp 8-byte reads
    mov qword [rax + PyStrObject.data + r12], 0

    mov rdi, rax
    call str_set_length

    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_from_cstr_heap

; str_from_cstr(const char *cstr) -> (rax=payload, edx=tag)
; Creates a string from a C string. Always returns heap TAG_PTR.
DEF_FUNC_BARE str_from_cstr
    jmp str_from_cstr_heap
END_FUNC str_from_cstr

; str_new_heap(const char *data, int64_t len) -> (rax=PyStrObject*, edx=TAG_PTR)
; Always heap-allocates. For struct fields and internal use.
DEF_FUNC str_new_heap
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; save data ptr
    mov r12, rsi            ; save length

    ; Allocate: header + length + 8 (null + padding for 8-byte strcmp)
    lea rdi, [r12 + PyStrObject.data + 8]
    call ap_malloc
    mov r13, rax             ; r13 = new PyStrObject*

    ; Fill header
    mov qword [r13 + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [r13 + PyObject.ob_type], rcx
    mov [r13 + PyStrObject.ob_size], r12
    mov qword [r13 + PyStrObject.ob_hash], -1

    ; Copy data
    lea rdi, [r13 + PyStrObject.data]
    mov rsi, rbx
    mov rdx, r12
    call ap_memcpy

    ; Zero-fill 8 bytes at NUL position for ap_strcmp 8-byte reads
    mov qword [r13 + PyStrObject.data + r12], 0

    mov rdi, r13
    call str_set_length

    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_new_heap

; str_new(const char *data, int64_t len) -> (rax=payload, edx=tag)
; Creates a string from data with given length. Always returns heap TAG_PTR.
DEF_FUNC_BARE str_new
    jmp str_new_heap         ; tail-call heap path
END_FUNC str_new

; str_dealloc(PyObject *self)
DEF_FUNC_BARE str_dealloc
    ; String data is inline, just free the object
    jmp ap_free
END_FUNC str_dealloc

;; ============================================================================
;; str_repr(PyObject *self) -> PyObject*
;; Returns string with surrounding single quotes: 'hello'
;; ============================================================================
DEF_FUNC str_repr
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; rbx = self
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = src length

    ; Allocate worst case: header + 2 quotes + 2*length + 8 (NUL padding)
    lea rdi, [r12*2 + PyStrObject.data + 10]
    call ap_malloc
    mov r13, rax             ; r13 = new str

    ; Fill header
    mov qword [r13 + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [r13 + PyObject.ob_type], rcx
    mov qword [r13 + PyStrObject.ob_hash], -1

    ; Pick the delimiter as CPython does: a single quote normally, a double
    ; quote when the text contains ' and no ", so the quote inside needs no
    ; backslash.  This always used ' and escaped, so repr("a'b") came out as
    ; 'a\'b' where CPython gives "a'b".
    push r14
    mov r14d, 0x27
    xor eax, eax                ; saw a single quote?
    xor ecx, ecx
.sr_scan:
    cmp rcx, r12
    jge .sr_scan_done
    movzx edx, byte [rbx + PyStrObject.data + rcx]
    cmp dl, 0x22                ; a double quote rules the switch out
    je .sr_scan_keep
    cmp dl, 0x27
    jne .sr_scan_next
    mov eax, 1
.sr_scan_next:
    inc rcx
    jmp .sr_scan
.sr_scan_done:
    test eax, eax
    jz .sr_scan_keep
    mov r14d, 0x22
.sr_scan_keep:

    ; Write opening quote
    mov [r13 + PyStrObject.data], r14b

    ; Copy with escaping: rsi=src, rdi=dst, rcx=src index
    lea rsi, [rbx + PyStrObject.data]
    lea rdi, [r13 + PyStrObject.data + 1]
    xor ecx, ecx

.sr_loop:
    cmp rcx, r12
    jge .sr_done
    movzx eax, byte [rsi + rcx]

    cmp al, 10               ; newline
    je .sr_esc_n
    cmp al, 13               ; carriage return
    je .sr_esc_r
    cmp al, 9                ; tab
    je .sr_esc_t
    cmp al, 0x5C             ; backslash
    je .sr_esc_bs
    cmp eax, r14d            ; the delimiter in use
    je .sr_esc_sq

    ; Normal character
    mov [rdi], al
    inc rdi
    inc rcx
    jmp .sr_loop

.sr_esc_n:
    mov byte [rdi], 0x5C     ; backslash
    mov byte [rdi + 1], 'n'
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_r:
    mov byte [rdi], 0x5C
    mov byte [rdi + 1], 'r'
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_t:
    mov byte [rdi], 0x5C
    mov byte [rdi + 1], 't'
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_bs:
    mov byte [rdi], 0x5C
    mov byte [rdi + 1], 0x5C
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_sq:
    mov byte [rdi], 0x5C
    mov [rdi + 1], r14b      ; the delimiter in use
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_done:
    ; Write closing quote and null
    mov [rdi], r14b
    mov qword [rdi + 1], 0  ; 8-byte zero-fill for ap_strcmp

    ; Calculate actual ob_size: (rdi - data_start) + 1 for closing quote
    lea rax, [r13 + PyStrObject.data]
    sub rdi, rax               ; rdi = chars written including open quote
    inc rdi                    ; + closing quote
    mov [r13 + PyStrObject.ob_size], rdi

    mov rdi, r13
    call str_set_length
    mov rax, r13
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_repr

;; ============================================================================
;; str_str(PyObject *self) -> PyObject*
;; tp_str: returns self with INCREF (no quotes)
;; ============================================================================
DEF_FUNC_BARE str_str
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC str_str

;; ============================================================================
;; str_hash(PyObject *self) -> int64
;; FNV-1a hash
;; ============================================================================
DEF_FUNC str_hash

    ; Check cached hash
    mov rax, [rdi + PyStrObject.ob_hash]
    cmp rax, -1
    jne .done

    ; Compute FNV-1a
    mov rcx, [rdi + PyStrObject.ob_size]
    lea rsi, [rdi + PyStrObject.data]
    mov rax, 0xcbf29ce484222325     ; FNV offset basis
    mov rdx, 0x100000001b3          ; FNV prime
    ; 4x unrolled FNV-1a loop
align 16
.loop4:
    cmp rcx, 4
    jb .tail
    movzx r8d, byte [rsi]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+1]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+2]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+3]
    xor rax, r8
    imul rax, rdx
    add rsi, 4
    sub rcx, 4
    jmp .loop4
.tail:
    test rcx, rcx
    jz .store
    movzx r8d, byte [rsi]
    xor rax, r8
    imul rax, rdx
    inc rsi
    dec rcx
    jmp .tail
.store:
    ; Ensure hash is never -1
    cmp rax, -1
    jne .cache
    mov rax, -2
.cache:
    mov [rdi + PyStrObject.ob_hash], rax
.done:
    leave
    ret
END_FUNC str_hash

;; ============================================================================
;; str_concat(PyObject *a, PyObject *b, ?, ecx=right_tag) -> (rax,edx) fat value
;; String concatenation via nb_add.
;; Binary op handler passes right_tag in ecx. Direct callers must set ecx=TAG_PTR.
;; ============================================================================
DEF_FUNC str_concat
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    ; Check right tag first — non-TAG_PTR means not a heap string
    cmp ecx, TAG_PTR
    jne .concat_type_error
    ; Verify right operand is a string (ob_type == str_type)
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_STR_TYPE rax, rdx, .concat_type_error

    push rbx
    push r12
    push r13

    mov rbx, rdi            ; a
    mov r12, rsi            ; b

    ; Get lengths
    mov r13, [rbx + PyStrObject.ob_size]   ; len_a
    add r13, [r12 + PyStrObject.ob_size]   ; total length

    ; Allocate new string (+ 8 for NUL padding for 8-byte strcmp)
    lea rdi, [r13 + PyStrObject.data + 8]
    call ap_malloc
    push rax                ; save new str

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyStrObject.ob_size], r13
    mov qword [rax + PyStrObject.ob_hash], -1
    ; Concatenating whole strings concatenates their code points too.
    mov rcx, [rbx + PyStrObject.ob_length]
    add rcx, [r12 + PyStrObject.ob_length]
    mov [rax + PyStrObject.ob_length], rcx

    ; Copy first string
    lea rdi, [rax + PyStrObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, [rbx + PyStrObject.ob_size]
    call ap_memcpy

    ; Copy second string
    mov rax, [rsp]          ; reload new str
    mov rcx, [rbx + PyStrObject.ob_size]
    lea rdi, [rax + PyStrObject.data + rcx]
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, [r12 + PyStrObject.ob_size]
    call ap_memcpy

    ; Zero-fill 8 bytes at NUL position for ap_strcmp
    pop rax
    mov qword [rax + PyStrObject.data + r13], 0

    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.concat_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "can only concatenate str (not other type) to str"
    call raise_exception
END_FUNC str_concat

;; ============================================================================
;; str_repeat(PyObject *str_obj, PyObject *int_obj) -> PyObject*
;; String repetition via nb_multiply
;; ============================================================================
DEF_FUNC str_repeat
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; str
    mov rdi, rsi            ; int (count payload)
    mov edx, ecx            ; count tag (right operand)
    ; A count too large for int64 truncates through __gmpz_get_si, so
    ; "a" * (2**64) quietly returned "".
    ; The count must be an index.  int_fits_i64 and int_to_i64 both read
    ; PyIntObject fields, so a str or a float count was dereferenced as one:
    ; "a" * "2" segfaulted and [1] * None reported an OverflowError.
    push rdi
    push rdx
    mov rsi, rdi
    mov rcx, rdx
    V_PACK rsi, rcx
    extern seq_repeat_check_count
    call seq_repeat_check_count
    pop rdx
    pop rdi
    push rdi
    push rdx
    call int_fits_i64
    pop rdx
    pop rdi
    test eax, eax
    jz .srep_overflow
    call int_to_i64
    mov r12, rax             ; r12 = repeat count

    ; Clamp negative to 0
    test r12, r12
    jg .positive
    xor r12d, r12d
.positive:

    mov r13, [rbx + PyStrObject.ob_size]   ; r13 = str length
    mov r14, r13
    imul r14, r12                           ; r14 = total length
    ; ("a"*16) * (2**60) wrapped to 0, allocated 40 bytes, and then ran the
    ; copy loop 2**60 times into it.
    jo .srep_overflow
    cmp r14, 0x10000000                     ; 256M bytes
    ja .srep_overflow

    ; Allocate new string (+ 8 for NUL padding for 8-byte strcmp)
    lea rdi, [r14 + PyStrObject.data + 8]
    call ap_malloc
    push rax                ; save

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyStrObject.ob_size], r14
    mov qword [rax + PyStrObject.ob_hash], -1
    mov rcx, [rbx + PyStrObject.ob_length]
    imul rcx, r12
    mov [rax + PyStrObject.ob_length], rcx

    ; Copy str r12 times
    lea rdi, [rax + PyStrObject.data]
    xor ecx, ecx            ; ecx = iteration counter
.repeat_loop:
    cmp rcx, r12
    jge .repeat_done
    push rcx
    push rdi
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r13
    call ap_memcpy
    pop rdi
    pop rcx
    add rdi, r13
    inc rcx
    jmp .repeat_loop

.repeat_done:
    ; Zero-fill 8 bytes at NUL position for ap_strcmp
    pop rax
    mov qword [rax + PyStrObject.data + r14], 0
    mov edx, TAG_PTR

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
.srep_overflow:
    lea rdi, [rel exc_OverflowError_type]
    CSTRING rsi, "repeated string is too long"
    call raise_exception
END_FUNC str_repeat

;; ============================================================================
;; str_mod(PyStrObject *fmt, PyObject *args) -> PyStrObject*
;; nb_remainder: implements "fmt % args" string formatting
;; Handles: %s, %d, %i, %r, %f, %%
;; args can be a single value or a tuple
;; ============================================================================
extern obj_str
extern exc_ValueError_type
extern obj_repr
extern tuple_type
extern dict_type
extern obj_decref

; str_mod stack offsets
SM_FMT     equ 8
SM_ARGS    equ 16
SM_BUF     equ 24
SM_CAP     equ 32
SM_ISTUPLE equ 40
SM_NARGS   equ 48
SM_ATAG    equ 56
SM_KEYVAL  equ 64        ; value picked out by a %(name)s mapping key, or 0
SM_HASKEY  equ 72
SM_SPECST  equ 80        ; start of the flags/width/precision text
SM_POS     equ 88        ; input position, across calls
SM_SPEC    equ 128       ; 40 bytes of translated format spec, [rbp-128, rbp-88)
SM_CONV    equ 136
SM_SPECOBJ equ 144
SM_VALUE   equ 152
SM_PIECE   equ 160
SM_OWNVAL  equ 168
SM_ISMAP   equ 176       ; the right operand is a mapping: %(name)s, no arity check
SM_FRAME   equ 184

DEF_FUNC str_mod, SM_FRAME
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    ; Stack layout:
    ; [rbp-SM_FMT]     = fmt string
    ; [rbp-SM_ARGS]    = args (single value or tuple)
    ; [rbp-SM_BUF]     = heap buffer ptr
    ; [rbp-SM_CAP]     = buffer capacity
    ; [rbp-SM_ISTUPLE] = is_tuple (bool)
    ; [rbp-SM_NARGS]   = nargs (int)
    ; r13 = buffer ptr, r14 = output pos, r15 = arg index

    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp-SM_FMT], rdi      ; fmt
    mov [rbp-SM_ARGS], rsi     ; args
    mov [rbp-SM_ATAG], rcx     ; args tag

    ; Determine if args is a tuple
    ; rcx = right_tag (args tag) from op_binary_op caller
    mov qword [rbp-SM_ISTUPLE], 0  ; is_tuple = false
    mov qword [rbp-SM_ISMAP], 0
    mov qword [rbp-SM_NARGS], 1   ; nargs = 1 (single value)
    cmp ecx, TAG_PTR
    jne .sm_not_tuple           ; non-heap → single value (SmallInt/Float/Bool/None)
    ; A mapping is addressed by key, so it has no argument count to check.
    push rsi
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_DICT_TYPE rax, rcx, .sm_not_map
    mov qword [rbp-SM_ISMAP], 1
.sm_not_map:
    pop rsi
    mov rcx, [rbp-SM_ATAG]
    ; A tuple SUBCLASS is a tuple here.  The exact-type test this replaces took
    ; one for a single value, so `'(x=%r, y=%r)' % self` -- which is what
    ; collections.namedtuple's __repr__ is -- read one argument for two
    ; conversions and walked off the end of the list.
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_TUPLE_TYPE rax, rcx, .sm_not_tuple
    mov qword [rbp-SM_ISTUPLE], 1  ; is_tuple = true
    mov rax, [rsi + PyTupleObject.ob_size]
    mov [rbp-SM_NARGS], rax    ; nargs = tuple size
.sm_not_tuple:

    ; Allocate initial heap buffer (8192 bytes)
    extern ap_malloc, ap_free, ap_realloc
    mov edi, 8192
    call ap_malloc
    mov r13, rax               ; r13 = output buffer
    mov [rbp-SM_BUF], rax
    mov qword [rbp-SM_CAP], 8192
    xor r14d, r14d             ; r14 = output pos
    xor r15d, r15d             ; r15 = arg index
    mov qword [rbp-SM_HASKEY], 0

    ; Walk format string
    mov rbx, [rbp-SM_FMT]     ; fmt string
    mov r12, [rbx + PyStrObject.ob_size]  ; fmt length
    lea rbx, [rbx + PyStrObject.data]     ; fmt data
    xor ecx, ecx               ; input pos

.sm_loop:
    cmp rcx, r12
    jge .sm_done

    movzx eax, byte [rbx + rcx]
    cmp al, '%'
    je .sm_format
    ; Regular char: ensure 1 byte of space
    push rcx
    lea rdi, [r14 + 1]
    call .sm_ensure_cap
    pop rcx
    ; Copy char to output
    movzx eax, byte [rbx + rcx]
    mov [r13 + r14], al
    inc r14
    inc rcx
    jmp .sm_loop

.sm_format:
    ; '%' found — skip optional format spec, then dispatch on conversion char
    ; Format: %[flags][width][.precision]conversion
    ; Flags: -, +, 0, #, space
    ; Width: digits
    ; Precision: . followed by digits
    inc rcx
    cmp rcx, r12
    jge .sm_done

    ; %(name)s -- a mapping key.  This was never parsed, so the whole
    ; directive was copied through and "%(a)s" % {"a": 1} returned itself.
    mov qword [rbp-SM_HASKEY], 0
    cmp byte [rbx + rcx], '('
    jne .sm_mark_spec
    inc rcx
    mov r8, rcx                     ; start of the key
.sm_key_scan:
    cmp rcx, r12
    jge .sm_key_unterminated
    cmp byte [rbx + rcx], ')'
    je .sm_key_end
    inc rcx
    jmp .sm_key_scan
.sm_key_end:
    ; Build the key string and look it up in the mapping.
    push rcx
    push r8
    lea rdi, [rbx + r8]
    mov rsi, rcx
    sub rsi, r8
    call str_new_heap
    pop r8
    pop rcx
    push rcx
    push rax                        ; the key, ours to release
    mov rdi, [rbp-SM_ARGS]
    mov rsi, rax
    extern dict_get
    call dict_get
    mov r9, rax
    pop rdi
    push r9
    call obj_decref
    pop r9
    pop rcx
    test r9, r9
    jz .sm_key_error
    mov [rbp-SM_KEYVAL], r9
    mov qword [rbp-SM_HASKEY], 1
    inc rcx                         ; step past ')'

.sm_mark_spec:
    ; Remember where the flags start.  This used to sit on .sm_skip_flags
    ; itself, which .sm_skip_one jumps back to once per flag -- so the marker
    ; ended up *after* the flags and "%-5s" looked like it had none.
    mov [rbp-SM_SPECST], rcx

.sm_skip_flags:
    movzx eax, byte [rbx + rcx]
    cmp al, '-'
    je .sm_skip_one
    cmp al, '+'
    je .sm_skip_one
    cmp al, '0'
    je .sm_skip_one
    cmp al, '#'
    je .sm_skip_one
    cmp al, ' '
    je .sm_skip_one
    jmp .sm_skip_width
.sm_skip_one:
    inc rcx
    cmp rcx, r12
    jge .sm_done
    jmp .sm_skip_flags

.sm_skip_width:
    movzx eax, byte [rbx + rcx]
    cmp al, '0'
    jb .sm_check_dot
    cmp al, '9'
    ja .sm_check_dot
    inc rcx
    cmp rcx, r12
    jge .sm_done
    jmp .sm_skip_width

.sm_check_dot:
    cmp al, '.'
    jne .sm_dispatch
    inc rcx                    ; skip '.'
    cmp rcx, r12
    jge .sm_done
.sm_skip_prec:
    movzx eax, byte [rbx + rcx]
    cmp al, '0'
    jb .sm_dispatch
    cmp al, '9'
    ja .sm_dispatch
    inc rcx
    cmp rcx, r12
    jge .sm_done
    jmp .sm_skip_prec

.sm_dispatch:
    ; A directive carrying flags, width or precision was skipped outright,
    ; so "%5s" % "x" returned "x".  Those go through the format-spec engine;
    ; a bare %s or %d keeps the direct path below.
    mov rax, [rbp-SM_SPECST]
    cmp rax, rcx
    jne .sm_use_spec
    ; The direct path below never learned %X, %o or %b, so those went out
    ; literally even with no flags; and its %x handled only an int immediate,
    ; printing "0" for a heap int.  All four go through the spec engine.
    movzx eax, byte [rbx + rcx]
    cmp al, 'X'
    je .sm_use_spec
    cmp al, 'x'
    je .sm_use_spec
    cmp al, 'o'
    je .sm_use_spec
    cmp al, 'b'
    je .sm_use_spec
    ; %e, %g and their uppercase forms went out literally, and %f fell back
    ; to str(), so "%f" % 1.5 was "1.5" rather than "1.500000".
    cmp al, 'e'
    je .sm_use_spec
    cmp al, 'E'
    je .sm_use_spec
    cmp al, 'f'
    je .sm_use_spec
    cmp al, 'F'
    je .sm_use_spec
    cmp al, 'g'
    je .sm_use_spec
    cmp al, 'G'
    je .sm_use_spec
    jmp .sm_dispatch_plain
.sm_use_spec:
    mov [rbp-SM_POS], rcx
    call .sm_spec_conv
    mov rcx, [rbp-SM_POS]
    jmp .sm_loop

.sm_dispatch_plain:
    movzx eax, byte [rbx + rcx]
    inc rcx                    ; consume conversion char

    cmp al, '%'
    je .sm_percent
    cmp al, 's'
    je .sm_str
    cmp al, 'd'
    je .sm_int
    cmp al, 'i'
    je .sm_int
    cmp al, 'r'
    je .sm_repr
    cmp al, 'f'
    je .sm_str                 ; %f: use str() for now (float.__str__)
    cmp al, 'x'
    je .sm_hex
    ; Unknown: just output the char
    mov byte [r13 + r14], '%'
    inc r14
    mov [r13 + r14], al
    inc r14
    jmp .sm_loop

.sm_percent:
    mov byte [r13 + r14], '%'
    inc r14
    jmp .sm_loop

.sm_str:
    ; Get next arg
    push rcx
    call .sm_get_arg
    ; rax = arg payload, rdx = arg tag
    mov rdi, rax
    mov rsi, rdx               ; tag for obj_str
    V_PACK rdi, rsi
    call obj_str
    ; rax = str result
    jmp .sm_copy_str

.sm_int:
    push rcx
    call .sm_get_arg
    ; If TAG_BOOL, convert to TAG_SMALLINT so we get "0"/"1" not "False"/"True"
    ; If TAG_PTR pointing to bool_type, extract 0/1 as SmallInt
    cmp edx, TAG_PTR
    jne .sm_int_go
    test rax, rax
    jz .sm_int_go
    mov rcx, [rax + PyObject.ob_type]
    extern bool_type
    lea r8, [rel bool_type]
    cmp rcx, r8
    jne .sm_int_go
    ; bool singleton → extract 0/1 by comparing with bool_true
    extern bool_true
    lea rcx, [rel bool_true]
    xor edi, edi
    cmp rax, rcx
    setne dil                  ; wait, True=1 so sete
    xor edi, edi
    cmp rax, rcx
    sete dil                   ; rdi = 1 if True, 0 if False
    mov rax, rdi
    mov edx, TAG_SMALLINT
    jmp .sm_int_go
.sm_int_from_bool:
    ; TAG_BOOL payload is 0 or 1
    mov edx, TAG_SMALLINT
.sm_int_go:
    mov rdi, rax
    mov rsi, rdx               ; tag for obj_str (64-bit)
    V_PACK rdi, rsi
    call obj_str               ; int.__str__ = int_repr
    jmp .sm_copy_str

.sm_repr:
    push rcx
    call .sm_get_arg
    mov rdi, rax
    mov rsi, rdx               ; tag for obj_repr (64-bit)
    V_PACK rdi, rsi
    call obj_repr
    jmp .sm_copy_str

.sm_hex:
    ; %x: format integer as lowercase hex
    push rcx
    call .sm_get_arg
    ; Convert TAG_BOOL to TAG_SMALLINT
    ; Handle TAG_PTR bool singletons
    cmp edx, TAG_PTR
    jne .sm_hex_go
    test rax, rax
    jz .sm_hex_go
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel bool_type]
    cmp rcx, r8
    jne .sm_hex_go
    lea rcx, [rel bool_true]
    xor edi, edi
    cmp rax, rcx
    sete dil
    mov rax, rdi
    mov edx, TAG_SMALLINT
    jmp .sm_hex_go
.sm_hex_from_bool:
    mov edx, TAG_SMALLINT
.sm_hex_go:
    ; Only handle SmallInt for now
    cmp edx, TAG_SMALLINT
    jne .sm_hex_zero
    mov rdi, rax               ; value
    ; Format into stack buffer (max 16 hex digits + null)
    sub rsp, 24                ; temp buffer
    mov rsi, rsp
    call .sm_format_hex        ; rsi = buffer, returns length in rax
    ; Copy result to output
    mov rcx, rax               ; length
    mov rsi, rsp               ; buffer
    lea rdi, [r14 + rcx + 1]
    push rcx
    push rsi
    call .sm_ensure_cap
    pop rsi
    pop rcx
    xor edx, edx
.sm_hex_copy:
    cmp rdx, rcx
    jge .sm_hex_done
    movzx eax, byte [rsi + rdx]
    mov [r13 + r14], al
    inc r14
    inc rdx
    jmp .sm_hex_copy
.sm_hex_done:
    add rsp, 24
    pop rcx
    jmp .sm_loop

.sm_hex_zero:
    ; Non-SmallInt: just output "0"
    lea rdi, [r14 + 2]
    call .sm_ensure_cap
    mov byte [r13 + r14], '0'
    inc r14
    pop rcx
    jmp .sm_loop

; .sm_format_hex: format unsigned int rdi as hex into buffer rsi
; Returns length in rax. Buffer must be >= 17 bytes.
.sm_format_hex:
    push rbx
    mov rax, rdi
    test rax, rax
    jnz .hex_nonzero
    mov byte [rsi], '0'
    mov rax, 1
    pop rbx
    ret
.hex_nonzero:
    ; Write digits in reverse into temp area, then reverse
    xor ecx, ecx              ; digit count
    mov rbx, rsi              ; save buffer start
    lea rdi, [rsi + 16]       ; write from end of temp area backward
.hex_digit_loop:
    test rax, rax
    jz .hex_reverse
    mov rdx, rax
    and edx, 0xF
    cmp dl, 10
    jb .hex_dec_digit
    add dl, ('a' - 10)
    jmp .hex_store
.hex_dec_digit:
    add dl, '0'
.hex_store:
    dec rdi
    mov [rdi], dl
    shr rax, 4
    inc ecx
    jmp .hex_digit_loop
.hex_reverse:
    ; Copy from [rdi] to [rbx], ecx chars
    mov rax, rcx               ; return length
    xor edx, edx
.hex_copy_loop:
    cmp edx, ecx
    jge .hex_fmt_done
    movzx esi, byte [rdi + rdx]
    mov [rbx + rdx], sil
    inc edx
    jmp .hex_copy_loop
.hex_fmt_done:
    pop rbx
    ret

.sm_copy_str:
    ; rax = str payload (heap PyStrObject*)
    push rax                   ; save for DECREF
    mov rcx, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    ; Ensure enough space for the entire string
    push rcx
    push rsi
    lea rdi, [r14 + rcx + 1]  ; need pos + len + 1 for null
    call .sm_ensure_cap
    pop rsi
    pop rcx
    ; Copy chars (memcpy-style)
    xor edx, edx
.sm_copy_loop:
    cmp rdx, rcx
    jge .sm_copy_done
    movzx eax, byte [rsi + rdx]
    mov [r13 + r14], al
    inc r14
    inc rdx
    jmp .sm_copy_loop
.sm_copy_done:
    pop rdi                    ; DECREF temp str
    DECREF_REG rdi
    pop rcx                    ; restore input pos
    jmp .sm_loop

.sm_get_arg:
    ; Get arg at index r15, increment r15
    ; Returns arg payload in rax, tag in rdx (borrowed ref)
    cmp qword [rbp-SM_HASKEY], 1
    jne .sm_arg_positional
    mov rax, [rbp-SM_KEYVAL]
    V_UNPACK rax, rdx
    mov qword [rbp-SM_HASKEY], 0
    ret
.sm_arg_positional:
    cmp qword [rbp-SM_ISTUPLE], 1
    je .sm_arg_tuple
    ; Single value
    mov rax, [rbp-SM_ARGS]
    mov rdx, [rbp-SM_ATAG]
    inc r15
    ret
.sm_arg_tuple:
    mov rax, [rbp-SM_ARGS]     ; tuple
    mov rdx, r15
    cmp rdx, [rax + PyTupleObject.ob_size]
    jge .sm_arg_none
    mov rcx, [rax + PyTupleObject.ob_item]       ; payloads
    mov rax, [rcx + rdx*8]                       ; arg payload
    V_UNPACK rax, rdx
    inc r15
    ret
.sm_arg_none:
    ; Past the end of the argument list.  Substituting None here quietly
    ; formatted a missing argument as "None"; the format string is wrong and
    ; Python says so.
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "not enough arguments for format string"
    call raise_exception

;; .sm_ensure_cap — ensure buffer can hold rdi bytes total
;; rdi = required capacity. Preserves r14, r15, rbx, r12. Updates r13.
.sm_ensure_cap:
    cmp rdi, [rbp-SM_CAP]
    jbe .sm_cap_ok
    ; Double capacity until sufficient
    mov rax, [rbp-SM_CAP]
.sm_grow_loop:
    shl rax, 1
    cmp rdi, rax
    ja .sm_grow_loop
    ; rax = new capacity
    mov [rbp-SM_CAP], rax
    mov rdi, r13               ; old ptr
    mov rsi, rax               ; new size
    call ap_realloc
    mov r13, rax
    mov [rbp-SM_BUF], rax
.sm_cap_ok:
    ret

.sm_done:
    ; Every argument must have been consumed.  A single non-tuple value counts
    ; as one; a mapping is addressed by key and has no count to check.
    cmp qword [rbp-SM_HASKEY], 1
    je .sm_arity_ok
    cmp qword [rbp-SM_ISMAP], 1
    je .sm_arity_ok
    cmp r15, [rbp-SM_NARGS]
    jb .sm_too_many
.sm_arity_ok:

    ; Null-terminate and create string
    mov byte [r13 + r14], 0

    push r13                   ; save buffer ptr for free
    mov rdi, r13
    mov rsi, r14
    call str_new_heap
    mov rbx, rax               ; save result

    pop rdi                    ; free heap buffer
    call ap_free

    mov rax, rbx               ; return result
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    ret
.sm_too_many:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "not all arguments converted during string formatting"
    call raise_exception

.sm_key_unterminated:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "incomplete format key"
    call raise_exception

.sm_key_error:
    extern exc_KeyError_type
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "format key not found"
    call raise_exception
;; Format one directive through format_apply_spec.  On entry SM_POS is the
;; index of the conversion character and SM_SPECST the start of the flags;
;; on exit SM_POS is just past it.  r13 (buffer), r14 (output position),
;; r15 (argument index), rbx and r12 belong to the caller's loop, so
;; everything here lives in frame slots.
.sm_spec_conv:
    mov r8, [rbp-SM_POS]
    movzx r9d, byte [rbx + r8]      ; the conversion character
    mov [rbp-SM_CONV], r9
    inc r8
    mov [rbp-SM_POS], r8

    ; Alignment first: '-' means left, and % right-aligns everything else,
    ; including strings -- unlike format(), whose default for str is left.
    lea rdi, [rbp-SM_SPEC]
    xor r10d, r10d
    mov rax, [rbp-SM_SPECST]
    xor r11d, r11d
.sm_sc_seek_minus:
    cmp rax, [rbp-SM_POS]
    jge .sm_sc_seek_done
    cmp byte [rbx + rax], '-'
    jne .sm_sc_seek_next
    mov r11d, 1
.sm_sc_seek_next:
    inc rax
    jmp .sm_sc_seek_minus
.sm_sc_seek_done:
    mov byte [rdi], '>'
    test r11d, r11d
    jz .sm_sc_numeric_zero
    mov byte [rdi], '<'
    jmp .sm_sc_align_done

.sm_sc_numeric_zero:
    ; A '0' flag on a numeric conversion pads between the sign and the
    ; digits, which is '=' alignment; '>' put the zeros in front of the sign,
    ; so "%05d" % -42 came out "00-42".
    mov rcx, [rbp-SM_CONV]
    cmp cl, 's'
    je .sm_sc_align_done
    cmp cl, 'r'
    je .sm_sc_align_done
    mov rax, [rbp-SM_SPECST]
.sm_sc_flagskip:
    cmp rax, [rbp-SM_POS]
    jge .sm_sc_align_done
    movzx ecx, byte [rbx + rax]
    cmp cl, '+'
    je .sm_sc_flagnext
    cmp cl, ' '
    je .sm_sc_flagnext
    cmp cl, '#'
    je .sm_sc_flagnext
    cmp cl, '0'
    jne .sm_sc_align_done
    mov byte [rdi], '='
    jmp .sm_sc_align_done
.sm_sc_flagnext:
    inc rax
    jmp .sm_sc_flagskip

.sm_sc_align_done:
    mov r10d, 1

    ; Then the flags, width and precision verbatim, minus the '-'.
    mov rax, [rbp-SM_SPECST]
.sm_sc_copy:
    mov rcx, [rbp-SM_POS]
    dec rcx
    cmp rax, rcx
    jge .sm_sc_copy_done
    movzx ecx, byte [rbx + rax]
    cmp cl, '-'
    je .sm_sc_copy_next
    ; A '0' flag means nothing for %s and %r; CPython pads those with spaces.
    cmp cl, '0'
    jne .sm_sc_copy_keep
    cmp r10d, 1
    jne .sm_sc_copy_keep        ; a digit of the width, not the flag
    mov rcx, [rbp-SM_CONV]
    cmp cl, 's'
    je .sm_sc_copy_next
    cmp cl, 'r'
    je .sm_sc_copy_next
    movzx ecx, byte [rbx + rax]
.sm_sc_copy_keep:
    cmp r10, 36                 ; the spec buffer is 40 bytes and grows up
    jge .sm_sc_copy_done
    mov [rdi + r10], cl
    inc r10
.sm_sc_copy_next:
    inc rax
    jmp .sm_sc_copy
.sm_sc_copy_done:

    ; The conversion letter, mapped onto a spec type.
    mov rcx, [rbp-SM_CONV]
    cmp cl, 'i'
    jne .sm_sc_not_i
    mov cl, 'd'
.sm_sc_not_i:
    cmp cl, 'r'
    jne .sm_sc_store_type
    mov cl, 's'                     ; repr is applied to the value below
.sm_sc_store_type:
    mov [rdi + r10], cl
    inc r10

    lea rdi, [rbp-SM_SPEC]
    mov rsi, r10
    call str_new_heap
    mov [rbp-SM_SPECOBJ], rax

    call .sm_get_arg
    V_PACK rax, rdx
    mov [rbp-SM_VALUE], rax
    mov rcx, [rbp-SM_CONV]
    cmp cl, 'r'
    jne .sm_sc_have_value
    mov rdi, rax
    call obj_repr
    V_UNPACK rax, rdx
    V_PACK rax, rdx
    mov [rbp-SM_VALUE], rax
    mov qword [rbp-SM_OWNVAL], 1
    jmp .sm_sc_format
.sm_sc_have_value:
    mov qword [rbp-SM_OWNVAL], 0

.sm_sc_format:
    mov rdi, [rbp-SM_VALUE]
    mov rsi, [rbp-SM_SPECOBJ]
    extern format_apply_spec
    call format_apply_spec
    V_UNPACK rax, rdx
    mov [rbp-SM_PIECE], rax

    mov rdi, [rbp-SM_SPECOBJ]
    call obj_decref
    cmp qword [rbp-SM_OWNVAL], 0
    je .sm_sc_no_own
    mov rdi, [rbp-SM_VALUE]
    call obj_decref
.sm_sc_no_own:

    ; Append the piece to the caller's buffer, advancing its position.
    mov rax, [rbp-SM_PIECE]
    mov r8, [rax + PyStrObject.ob_size]
    lea rdi, [r14 + r8]
    call .sm_ensure_cap
    mov rax, [rbp-SM_PIECE]
    mov r8, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor ecx, ecx
.sm_sc_append:
    cmp rcx, r8
    jge .sm_sc_appended
    movzx eax, byte [rsi + rcx]
    mov [r13 + r14], al
    inc r14
    inc rcx
    jmp .sm_sc_append
.sm_sc_appended:
    mov rdi, [rbp-SM_PIECE]
    call obj_decref
    ret

END_FUNC str_mod

;; ============================================================================
;; str_compare(left, right, op, left_tag, right_tag) -> (rax,edx) fat bool
;; Rich comparison for strings. Both operands are heap PyStrObject*.
;; Caller convention: rdi=left, rsi=right, edx=op, rcx=left_tag, r8=right_tag
;; Note: r8 may be unset by callers like max/min (rsi is always a valid heap
;; string in that case, so the TAG_RC_BIT guard is conservative-safe).
;; ============================================================================

DEF_FUNC str_compare
    V_UNPACK rdi, rcx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, r8            ; right Value -> (payload, tag)
    push rbx

    mov ebx, edx            ; save op

    ; --- Resolve right operand to a data pointer (-> rsi) ---
    ; Non-string guard: TAG_RC_BIT (bit 8) is set only for TAG_PTR (0x105).
    ; Non-pointer tags (0-4) and unset r8 from max/min: if TAG_RC_BIT clear
    ; → not a string.
    test r8d, TAG_RC_BIT
    jz .not_string
    ; Heap pointer — verify ob_type == str_type
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_STR_TYPE rax, rdx, .not_string
    lea rsi, [rsi + PyStrObject.data]

    ; --- Resolve left operand to a data pointer (-> rdi) ---
    ; Heap str — no type check needed (caller dispatched via str_type)
    lea rdi, [rdi + PyStrObject.data]

    ; --- Compare the two null-terminated data pointers ---
    call ap_strcmp
    ; eax = strcmp result

    ; Dispatch on comparison op (ebx)
    cmp ebx, PY_NE
    je .do_ne
    cmp ebx, PY_EQ
    je .do_eq
    cmp ebx, PY_LT
    je .do_lt
    cmp ebx, PY_GT
    je .do_gt
    cmp ebx, PY_LE
    je .do_le
    ; fall through: PY_GE
    test eax, eax
    jge .ret_true
    jmp .ret_false
.do_lt:
    test eax, eax
    js .ret_true
    jmp .ret_false
.do_le:
    test eax, eax
    jle .ret_true
    jmp .ret_false
.do_eq:
    test eax, eax
    jz .ret_true
    jmp .ret_false
.do_ne:
    test eax, eax
    jnz .ret_true
    jmp .ret_false
.do_gt:
    test eax, eax
    jg .ret_true
    jmp .ret_false

.not_string:
    ; Right operand is not a string.
    ; EQ → False, NE → True, ordering → NotImplemented (NULL)
    cmp ebx, PY_EQ
    je .ret_false
    cmp ebx, PY_NE
    je .ret_true
    ; Ordering comparison with non-string → return NotImplemented (NULL)
    RET_NULL
    pop rbx
    leave
    ret

.ret_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC str_compare

;; ============================================================================
;; str_len(PyObject *self) -> int64_t
;; sq_length: returns ob_size
;; ============================================================================
DEF_FUNC_BARE str_len
    ; Code points, which is what Python counts.  Equal to the byte length for
    ; anything ASCII, which is nearly everything.
    mov rax, [rdi + PyStrObject.ob_length]
    ret
END_FUNC str_len

;; ============================================================================
;; str_getitem(PyObject *self, int64_t index) -> PyObject*
;; sq_item: return single-char string at index
;; ============================================================================
DEF_FUNC str_getitem
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; self
    mov r12, rsi            ; index, in code points

    ; Handle negative index
    test r12, r12
    jns .positive
    add r12, [rbx + PyStrObject.ob_length]
.positive:

    ; Bounds check
    cmp r12, [rbx + PyStrObject.ob_length]
    jge .index_error
    cmp r12, 0
    jl .index_error

    ; Where the code point starts, and how many bytes it occupies.
    mov rdi, rbx
    mov rsi, r12
    call str_cp_offset
    mov r13, rax
    mov rdi, rbx
    lea rsi, [r12 + 1]
    call str_cp_offset
    sub rax, r13            ; the width of this one code point

    lea rdi, [rbx + PyStrObject.data]
    add rdi, r13
    mov rsi, rax
    call str_new

    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "string index out of range"
    call raise_exception
END_FUNC str_getitem

;; ============================================================================
;; str_subscript(PyObject *self, PyObject *key) -> PyObject*
;; mp_subscript: index with int or slice key (for BINARY_SUBSCR)
;; ============================================================================
DEF_FUNC str_subscript
    V_UNPACK rsi, rdx           ; key Value -> (payload, tag)
    push rbx

    mov rbx, rdi            ; save self

    ; Check if key is a SmallInt (edx = key tag from caller)
    cmp edx, TAG_SMALLINT
    je .ss_int               ; SmallInt -> int path
    cmp edx, TAG_PTR            ; a float key is neither: classify
    jne .ss_type_error          ; fully before dereferencing, or raw
                                ; f64 bits get used as an address
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .ss_slice

.ss_int:
    ; obj_as_index covers int, bool, an int subclass and __index__, and
    ; raises for anything else -- int_to_i64 would read PyIntObject.compact
    ; off whatever it was given.
    mov rdi, rsi
    call obj_as_index
    mov rsi, rax

    ; Call str_getitem — already returns a Value
    mov rdi, rbx
    call str_getitem

    pop rbx
    leave
    ret

.ss_slice:
    mov rdi, rbx
    ; rsi = slice
    call str_getslice
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ss_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "string indices must be integers"
    call raise_exception
END_FUNC str_subscript

;; ============================================================================
;; str_contains(rdi=self, rsi=substr Value) -> int (0/1)
;; sq_contains: check if substr is in self using strstr
;; ============================================================================
DEF_FUNC str_contains
    V_UNPACK rsi, rdx           ; decode the operand Value

    ; Validate substr is a string (TAG_PTR with ob_type == str_type)
    cmp edx, TAG_PTR
    jne .str_contains_type_error
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_STR_TYPE rax, rcx, .str_contains_type_error

    extern ap_strstr
    lea rdi, [rdi + PyStrObject.data]
    lea rsi, [rsi + PyStrObject.data]
    call ap_strstr
    test rax, rax
    setnz al
    movzx eax, al

    leave
    ret

.str_contains_type_error:
    extern exc_TypeError_type
    extern raise_exception
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "'in <string>' requires string as left operand"
    call raise_exception
END_FUNC str_contains

;; ============================================================================
;; str_bool(PyObject *self) -> int (0/1)
;; nb_bool: true if len > 0
;; ============================================================================
DEF_FUNC_BARE str_bool
    cmp qword [rdi + PyStrObject.ob_size], 0
    setne al
    movzx eax, al
    ret
END_FUNC str_bool

;; ============================================================================
;; str_getslice(PyStrObject *str, PySliceObject *slice) -> PyStrObject*
;; Creates a new string from a slice of the original.
;; ============================================================================
SGS_STR   equ 8
SGS_START equ 16
SGS_STEP  equ 24
SGS_LEN   equ 32
SGS_OUT   equ 40
SGS_POS   equ 48
SGS_I     equ 56
SGS_FRAME equ 64
DEF_FUNC str_getslice, SGS_FRAME
    push rbx
    push r12

    mov [rbp - SGS_STR], rdi
    mov rbx, rdi

    ; Slice indices are code-point indices, so the length handed to
    ; slice_indices is the code point count.  For an ASCII string that is the
    ; byte count and everything below reduces to the byte-wise version.
    mov rdi, rsi                ; the slice
    mov rsi, [rbx + PyStrObject.ob_length]
    call slice_indices
    mov [rbp - SGS_START], rax
    mov [rbp - SGS_STEP], rcx
    mov r12, rdx                ; stop

    ; slicelength
    test rcx, rcx
    jg .sgs_pos_step
    mov rax, [rbp - SGS_START]
    sub rax, r12
    jle .sgs_empty
    dec rax
    mov rcx, [rbp - SGS_STEP]
    neg rcx
    xor edx, edx
    div rcx
    inc rax
    jmp .sgs_have_len
.sgs_pos_step:
    mov rax, r12
    sub rax, [rbp - SGS_START]
    jle .sgs_empty
    dec rax
    xor edx, edx
    div qword [rbp - SGS_STEP]
    inc rax
    jmp .sgs_have_len
.sgs_empty:
    xor eax, eax
.sgs_have_len:
    mov [rbp - SGS_LEN], rax

    ; A contiguous slice is a byte range, so it can be copied whole.
    cmp qword [rbp - SGS_STEP], 1
    jne .sgs_general
    mov rdi, rbx
    mov rsi, [rbp - SGS_START]
    call str_cp_offset
    mov r12, rax
    mov rdi, rbx
    mov rsi, [rbp - SGS_START]
    add rsi, [rbp - SGS_LEN]
    call str_cp_offset
    sub rax, r12
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r12
    mov rsi, rax
    call str_new_heap
    jmp .sgs_ret

.sgs_general:
    ; A strided slice copies whole code points, so the result's byte length is
    ; not known in advance; the source's is an upper bound.
    mov rdi, [rbx + PyStrObject.ob_size]
    add rdi, PyStrObject.data + 8
    call ap_malloc
    mov [rbp - SGS_OUT], rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rdx, [rel str_type]
    mov [rax + PyObject.ob_type], rdx
    mov qword [rax + PyStrObject.ob_hash], -1

    mov qword [rbp - SGS_POS], 0
    mov qword [rbp - SGS_I], 0
.sgs_copy:
    mov rax, [rbp - SGS_I]
    cmp rax, [rbp - SGS_LEN]
    jge .sgs_finish
    imul rax, [rbp - SGS_STEP]
    add rax, [rbp - SGS_START]   ; the source code point index
    mov r12, rax
    mov rdi, rbx
    mov rsi, r12
    call str_cp_offset
    push rax
    mov rdi, rbx
    lea rsi, [r12 + 1]
    call str_cp_offset
    pop rcx
    sub rax, rcx                 ; rax = width, rcx = byte offset
    push rax
.sgs_bytes:
    test rax, rax
    jz .sgs_bytes_done
    movzx edx, byte [rbx + PyStrObject.data + rcx]
    mov r8, [rbp - SGS_OUT]
    mov r9, [rbp - SGS_POS]
    mov [r8 + PyStrObject.data + r9], dl
    inc qword [rbp - SGS_POS]
    inc rcx
    dec rax
    jmp .sgs_bytes
.sgs_bytes_done:
    pop rax
    inc qword [rbp - SGS_I]
    jmp .sgs_copy

.sgs_finish:
    mov rax, [rbp - SGS_OUT]
    mov rcx, [rbp - SGS_POS]
    mov [rax + PyStrObject.ob_size], rcx
    mov qword [rax + PyStrObject.data + rcx], 0
    mov rcx, [rbp - SGS_LEN]
    mov [rax + PyStrObject.ob_length], rcx

.sgs_ret:
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    ret
END_FUNC str_getslice

;; ============================================================================
;; String Iterator
;; ============================================================================

extern obj_decref
extern obj_incref
extern iter_self

;; str_tp_iter(PyStrObject *self) -> PyStrIterObject*
;; tp_iter for str type: create a new string iterator
;; ============================================================================
global str_tp_iter
DEF_FUNC str_tp_iter
    push rbx

    mov rbx, rdi               ; save str

    mov edi, PyStrIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyStrIterObject.it_seq], rbx
    mov qword [rax + PyStrIterObject.it_index], 0

    ; INCREF the string
    INCREF rbx

    pop rbx
    leave
    ret
END_FUNC str_tp_iter

;; str_iter_next(PyStrIterObject *self) -> PyObject* or NULL
;; Return next character as a 1-char string, or NULL if exhausted
;; ============================================================================
global str_iter_next
DEF_FUNC str_iter_next
    push rbx

    mov rbx, rdi                                      ; self (iter)
    mov rax, [rbx + PyStrIterObject.it_seq]            ; str
    mov rcx, [rbx + PyStrIterObject.it_index]          ; index

    ; Check bounds (byte index vs ob_size)
    cmp rcx, [rax + PyStrObject.ob_size]
    jge .si_exhausted

    ; One whole code point, however many bytes that is: the index is a byte
    ; offset, and iterating a byte at a time cut multi-byte characters apart.
    ; str_cp_width, so this walk agrees with the ones len() and indexing use.
    push rcx
    mov rsi, [rax + PyStrObject.ob_size]
    lea rdi, [rax + PyStrObject.data]
    mov rdx, rcx
    call str_cp_width
    pop rcx
    add rax, rcx                    ; the byte offset just past this code point
    mov [rbx + PyStrIterObject.it_index], rax
    mov rdx, [rbx + PyStrIterObject.it_seq]
    lea rdi, [rdx + PyStrObject.data]
    add rdi, rcx
    mov rsi, rax
    sub rsi, rcx
    call str_new

    pop rbx
    leave
    ret

.si_exhausted:
    RET_NULL
    pop rbx
    leave
    ret
END_FUNC str_iter_next

;; str_iter_dealloc(PyObject *self)
;; ============================================================================
global str_iter_dealloc
DEF_FUNC str_iter_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF the string
    mov rdi, [rbx + PyStrIterObject.it_seq]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC str_iter_dealloc

;; ============================================================================
;; Data section
;; ============================================================================
section .data

str_name: db "str", 0

; String number methods (for + and * operators)
align 8
str_number_methods:
    dq str_concat           ; nb_add          +0
    dq 0                    ; nb_subtract     +8
    dq str_repeat           ; nb_multiply     +16
    dq str_mod              ; nb_remainder    +24
    dq 0                    ; nb_divmod       +32
    dq 0                    ; nb_power        +40
    dq 0                    ; nb_negative     +48
    dq 0                    ; nb_positive     +56
    dq 0                    ; nb_absolute     +64
    dq str_bool             ; nb_bool         +72
    dq 0                    ; nb_invert       +80
    dq 0                    ; nb_lshift       +88
    dq 0                    ; nb_rshift       +96
    dq 0                    ; nb_and          +104
    dq 0                    ; nb_xor          +112
    dq 0                    ; nb_or           +120
    dq 0                    ; nb_int          +128
    dq 0                    ; nb_float        +136
    dq 0                    ; nb_floor_divide +144
    dq 0                    ; nb_true_divide  +152
    dq 0                    ; nb_index        +160
    dq 0                        ; nb_iadd         +168
    dq 0                        ; nb_isub         +176
    dq 0                        ; nb_imul         +184
    dq 0                        ; nb_irem         +192
    dq 0                        ; nb_ipow         +200
    dq 0                        ; nb_ilshift      +208
    dq 0                        ; nb_irshift      +216
    dq 0                        ; nb_iand         +224
    dq 0                        ; nb_ixor         +232
    dq 0                        ; nb_ior          +240
    dq 0                        ; nb_ifloor_divide +248
    dq 0                        ; nb_itrue_divide +256
    dq 0 ; nb_matmul
    dq 0 ; nb_imatmul

; String sequence methods
align 8
str_sequence_methods:
    dq str_len              ; sq_length       +0
    dq 0                    ; sq_concat       +8
    ; str_repeat is also nb_multiply, but it has to be here as well: the
    ; reflected form `3 * "ab"` reaches a sequence only through sq_repeat, and
    ; with this NULL the int's nb_multiply took the string and read its length.
    dq str_repeat           ; sq_repeat       +16
    dq str_getitem          ; sq_item         +24
    dq 0                    ; sq_ass_item     +32
    dq str_contains         ; sq_contains     +40
    dq 0                    ; sq_inplace_concat +48
    dq 0                    ; sq_inplace_repeat +56

; String mapping methods (for BINARY_SUBSCR with int key)
align 8
str_mapping_methods:
    dq str_len              ; mp_length       +0
    dq str_subscript         ; mp_subscript    +8
    dq 0                    ; mp_ass_subscript +16

; str type object
align 8
global str_type
str_type:
    dq 1                ; ob_refcnt
    dq type_type        ; ob_type
    dq str_name         ; tp_name
    dq PyStrObject.data ; tp_basicsize (minimum, without data)
    dq str_dealloc      ; tp_dealloc
    dq str_repr         ; tp_repr
    dq str_str          ; tp_str (returns self for strings, no quotes)
    dq str_hash         ; tp_hash
    dq 0                ; tp_call
    dq 0                ; tp_getattr
    dq 0                ; tp_setattr
    dq str_compare      ; tp_richcompare
    dq str_tp_iter      ; tp_iter
    dq 0                ; tp_iternext
    dq 0                ; tp_init
    dq 0                ; tp_new
    dq str_number_methods    ; tp_as_number
    dq str_sequence_methods  ; tp_as_sequence
    dq str_mapping_methods   ; tp_as_mapping
    dq 0                ; tp_base
    dq 0                ; tp_dict
    dq 0                ; tp_mro
    dq TYPE_FLAG_STR_SUBCLASS ; tp_flags
    dq 0                ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; str_iter type data
align 8
str_iter_name: db "str_iterator", 0

align 8
str_iter_type:
    dq 1                        ; ob_refcnt
    dq type_type                ; ob_type
    dq str_iter_name            ; tp_name
    dq PyStrIterObject_size     ; tp_basicsize
    dq str_iter_dealloc         ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq iter_self                ; tp_iter (return self)
    dq str_iter_next            ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
