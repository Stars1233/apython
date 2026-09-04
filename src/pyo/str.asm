; pyo/str.asm - String type
; Phase 8: full string operations

%include "macros.inc"
%include "object.inc"

extern none_singleton
extern ap_malloc
extern ap_free
extern ap_strlen
extern ap_memcpy
extern ap_strcmp
extern bool_true
extern bool_false
extern int_to_i64
extern int_unwrap
extern int_is_integer
extern fatal_error
extern raise_exception
extern exc_IndexError_type
extern exc_TypeError_type
extern int_type
extern obj_as_index
extern int_fits_i64
extern exc_OverflowError_type
extern exc_MemoryError_type
extern slice_type
extern slice_indices
extern type_type
extern obj_dealloc


;; ============================================================================
;; str_cp_width(rdi = bytes, rsi = byte length, rdx = offset) -> rax = width
;;
;; How many bytes the code point starting at `offset` occupies.  Every walk
;; over a string has to agree on this or the two index spaces drift apart: a
;; lead byte whose continuation bytes are missing, or a stray continuation byte
;; with no lead, is one code point of one byte, so a string that is not valid
;; UTF-8 has exactly as many code points as it has bytes and behaves the way it
;; did before there were two lengths at all.  bytes.decode() does not validate,
;; so such a string is reachable.
;; ============================================================================
DEF_FUNC_BARE str_cp_width
    movzx ecx, byte [rdi + rdx]
    cmp cl, 0x80
    jb .one                         ; ASCII
    cmp cl, 0xc0
    jb .one                         ; a continuation byte with no lead
    mov eax, 2
    cmp cl, 0xe0
    jb .have_width
    mov eax, 3
    cmp cl, 0xf0
    jb .have_width
    mov eax, 4
    cmp cl, 0xf8
    jb .have_width
    jmp .one                        ; 0xf8..0xff is not a lead byte

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
    and cl, 0xc0
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

;; ============================================================================
;; str_count_codepoints(rdi = bytes, rsi = byte length) -> rax = code points
;;
;; Runs on every string creation -- str_new_heap and str_from_cstr_heap both
;; call str_set_length -- so every slice, concat, repr and format pays it.  The
;; walk below issues a call per code point; for ASCII, which is nearly all of
;; them, the answer is just the byte count.  Establish that first, eight bytes
;; at a time, and only fall into the walk when a byte >= 0x80 turns up.
;;
;; str_byte_to_cp and str_cp_offset already had this short-circuit
;; (`cmp ob_size, ob_length`); it was never applied to the function that
;; *establishes* ob_length, which is the one that cannot assume it.
;; ============================================================================
DEF_FUNC str_count_codepoints
    push rbx
    push r12
    push r13
    push r14

    ; --- ASCII probe: no high bit anywhere means one code point per byte ---
    mov rax, rdi
    mov rcx, rsi
    mov rdx, 0x8080808080808080
.ascii_word:
    cmp rcx, 8
    jb .ascii_tail
    test rdx, [rax]
    jnz .walk_setup
    add rax, 8
    sub rcx, 8
    jmp .ascii_word
.ascii_tail:
    test rcx, rcx
    jz .all_ascii
    test byte [rax], 0x80
    jnz .walk_setup
    inc rax
    dec rcx
    jmp .ascii_tail
.all_ascii:
    mov rax, rsi                    ; one code point per byte
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.walk_setup:
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

;; ============================================================================
;; str_set_length(rdi = PyStrObject*) -- fill ob_length from the bytes.
;; ============================================================================
DEF_FUNC str_set_length, 8            ; 1 pushes, so rsp is 16-aligned
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


;; ============================================================================
;; str_byte_to_cp(rdi = PyStrObject*, rsi = byte offset) -> rax = code point index
;; The inverse of str_cp_offset, for the methods that search in bytes and have
;; to report a position in code points.
;; ============================================================================
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

;; ============================================================================
;; str_cp_at(rdi = PyStrObject*, rsi = code point index) -> rax = the code
;; point, or -1 when the index is out of range.
;;
;; str_cp_offset gives the byte offset; this decodes the UTF-8 sequence that
;; starts there.  builtin_ord had the only decoder in the tree and it insists
;; the character is the whole string.
;; ============================================================================
DEF_FUNC str_cp_at
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    cmp r12, [rbx + PyStrObject.ob_length]
    jae .sca_out_of_range
    test r12, r12
    js .sca_out_of_range

    mov rdi, rbx
    mov rsi, r12
    call str_cp_offset          ; rax = the byte offset
    lea rcx, [rbx + PyStrObject.data]
    add rcx, rax

    movzx eax, byte [rcx]
    test al, 0x80
    jz .sca_done                ; ASCII: the byte is the code point

    mov r8d, eax
    and r8d, 0xf8
    cmp r8d, 0xf0
    je .sca_four
    mov r8d, eax
    and r8d, 0xf0
    cmp r8d, 0xe0
    je .sca_three

    and eax, 0x1f
    shl eax, 6
    movzx edx, byte [rcx + 1]
    and edx, 0x3f
    or eax, edx
    jmp .sca_done
.sca_three:
    and eax, 0x0f
    shl eax, 12
    movzx edx, byte [rcx + 1]
    and edx, 0x3f
    shl edx, 6
    or eax, edx
    movzx edx, byte [rcx + 2]
    and edx, 0x3f
    or eax, edx
    jmp .sca_done
.sca_four:
    and eax, 0x07
    shl eax, 18
    movzx edx, byte [rcx + 1]
    and edx, 0x3f
    shl edx, 12
    or eax, edx
    movzx edx, byte [rcx + 2]
    and edx, 0x3f
    shl edx, 6
    or eax, edx
    movzx edx, byte [rcx + 3]
    and edx, 0x3f
    or eax, edx
.sca_done:
    pop r12
    pop rbx
    leave
    ret
.sca_out_of_range:
    mov rax, -1
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_cp_at

;; ============================================================================
;; str_cp_offset(rdi = PyStrObject*, rsi = code point index) -> rax = byte offset
;; The index is not bounds-checked; an index at or past the end gives ob_size.
;; ============================================================================
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

;; ============================================================================
;; str_search_window(rdi = self, rsi = args, rdx = nargs, rcx = out[3])
;;   [out +  0] = pointer to the first byte of the window
;;   [out +  8] = window length in bytes
;;   [out + 16] = the window's start as a byte offset into self
;;   -> eax = 1 when the window is usable, 0 when nothing can match in it
;;
;; The optional start/end arguments shared by find, rfind, index, rindex and
;; count, resolved once.  Every one of those methods used to ignore them
;; outright -- "abcabc".find("b", 3) was 1 -- because each read args[0] and
;; args[1] and stopped.
;;
;; They are *code point* indices and clamp like a slice: negative counts from
;; the end, end is capped at the length, and a start past the end matches
;; nothing at all.  The search itself runs over bytes, so both are converted
;; through str_cp_offset; for ASCII, which is the common case, that is the
;; identity and costs one compare.
;; ============================================================================
SSW_SELF  equ 8
SSW_ARGS  equ 16
SSW_NARGS equ 24
SSW_OUT   equ 32
SSW_START equ 40
SSW_END   equ 48
SSW_FRAME equ 48            ; + 0 pushes = 48
DEF_FUNC str_search_window, SSW_FRAME
    mov [rbp - SSW_SELF], rdi
    mov [rbp - SSW_ARGS], rsi
    mov [rbp - SSW_NARGS], rdx
    mov [rbp - SSW_OUT], rcx

    ; Defaults: the whole string.
    xor eax, eax
    mov [rbp - SSW_START], rax
    mov rax, [rdi + PyStrObject.ob_length]
    mov [rbp - SSW_END], rax

    cmp qword [rbp - SSW_NARGS], 3
    jl .ssw_bounds_done

    mov rdi, [rbp - SSW_ARGS]
    mov rdi, [rdi + 16]             ; args[2] = start
    IS_NONE rdi, rax
    je .ssw_start_done              ; None means the default, as in CPython
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rdi, [rbp - SSW_SELF]
    test rax, rax
    jns .ssw_start_set
    add rax, [rdi + PyStrObject.ob_length]
    jns .ssw_start_set
    xor eax, eax                    ; still negative: clamp to the start
.ssw_start_set:
    mov [rbp - SSW_START], rax
.ssw_start_done:

    cmp qword [rbp - SSW_NARGS], 4
    jl .ssw_bounds_done
    mov rdi, [rbp - SSW_ARGS]
    mov rdi, [rdi + 24]             ; args[3] = end
    IS_NONE rdi, rax
    je .ssw_bounds_done
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rdi, [rbp - SSW_SELF]
    test rax, rax
    jns .ssw_end_set
    add rax, [rdi + PyStrObject.ob_length]
    jns .ssw_end_set
    xor eax, eax
.ssw_end_set:
    cmp rax, [rdi + PyStrObject.ob_length]
    jle .ssw_end_store
    mov rax, [rdi + PyStrObject.ob_length]
.ssw_end_store:
    mov [rbp - SSW_END], rax

.ssw_bounds_done:
    mov rdi, [rbp - SSW_SELF]
    mov rax, [rbp - SSW_START]
    cmp rax, [rdi + PyStrObject.ob_length]
    jg .ssw_nothing                 ; start past the end
    cmp rax, [rbp - SSW_END]
    jg .ssw_nothing                 ; start past end: an empty window matches nothing

    mov rsi, rax
    call str_cp_offset              ; start, in bytes
    mov [rbp - SSW_START], rax
    mov rdi, [rbp - SSW_SELF]
    mov rsi, [rbp - SSW_END]
    call str_cp_offset              ; end, in bytes
    mov rsi, [rbp - SSW_START]
    sub rax, rsi                    ; window length

    mov rcx, [rbp - SSW_OUT]
    mov rdi, [rbp - SSW_SELF]
    lea rdx, [rdi + PyStrObject.data]
    add rdx, rsi
    mov [rcx], rdx
    mov [rcx + 8], rax
    mov [rcx + 16], rsi
    mov eax, 1
    leave
    ret

.ssw_nothing:
    xor eax, eax
    leave
    ret
END_FUNC str_search_window



;; ============================================================================
;; codec_error_id(rdi = the errors= argument, a Value, or 0) -> eax
;;   0 = strict (the default), 1 = ignore, 2 = replace, -1 = anything else
;;
;; The three handlers the interpreter implements itself.  CPython has a
;; registry; reaching it from here would mean calling into Python, and the
;; three below are what the interpreter's own decoding needs.
;; ============================================================================
CEI_BUF   equ 32
CEI_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC codec_error_id, CEI_FRAME
    mov qword [rbp - CEI_BUF], 0
    mov qword [rbp - CEI_BUF + 8], 0
    mov qword [rbp - CEI_BUF + 16], 0
    mov qword [rbp - CEI_BUF + 24], 0
    test rdi, rdi
    jz .cei_strict
    LOAD_NONE rax
    cmp rdi, rax
    je .cei_strict
    V_TEST_PTR rdi, rax
    ja .cei_unknown
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .cei_is_str
    ; A subclass has str's layout and str's data, and bytes_check_errors_type
    ; -- which validates this same argument one call earlier -- already
    ; accepts one.  Refusing it here made the two disagree:
    ; b"...".decode("utf-8", MyStr("ignore")) got past the type check and then
    ; came back as "unknown error handler name 'ignore'".
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_STR_SUBCLASS
    jz .cei_unknown
.cei_is_str:
    mov rcx, [rdi + PyStrObject.ob_size]
    cmp rcx, 24
    ja .cei_unknown
    lea rsi, [rbp - CEI_BUF]
    xor edx, edx
.cei_copy:
    cmp rdx, rcx
    jge .cei_done
    movzx eax, byte [rdi + PyStrObject.data + rdx]
    mov [rsi + rdx], al
    inc rdx
    jmp .cei_copy
.cei_done:
    lea rdi, [rbp - CEI_BUF]
    CSTRING rsi, "strict"
    call ap_strcmp
    test eax, eax
    jz .cei_strict
    lea rdi, [rbp - CEI_BUF]
    CSTRING rsi, "ignore"
    call ap_strcmp
    test eax, eax
    jz .cei_ignore
    lea rdi, [rbp - CEI_BUF]
    CSTRING rsi, "replace"
    call ap_strcmp
    test eax, eax
    jz .cei_replace
.cei_unknown:
    mov eax, -1
    leave
    ret
.cei_strict:
    xor eax, eax
    leave
    ret
.cei_ignore:
    mov eax, 1
    leave
    ret
.cei_replace:
    mov eax, 2
    leave
    ret
END_FUNC codec_error_id

;; ============================================================================
;; codec_via_python(rdi = the object, a Value; rsi = the encoding str or 0;
;;                  rdx = the errors argument, a Value, or 0;
;;                  ecx = 0 to encode, 1 to decode)
;;   -> rax = payload, rdx = tag; (0, 0) with an exception pending on failure
;;
;; Everything the interpreter cannot spell itself.  `_codecs` is Python -- it
;; holds the registry, the cache, CPython's normalizestring, the search
;; functions and the six error handlers -- and this is how a builtin method
;; reaches it: lazily import the module, pull `encode` or `decode` out of its
;; dict, cache the callable for the process's life, and call it.
;;
;; The pattern is builtin_open_fn's, down to parking kw_names_pending across
;; the import: an import runs whole module bodies, and the keyword names of
;; the call that got us here are not theirs.  The import cannot happen at
;; startup, which is the other half of why it is done here and not there.
;;
;; A codec written in Python can itself call str.encode, so this has to be
;; re-entrant; it is, because the only state it keeps is the two cached
;; callables and neither is mutated after the first call.
;; ============================================================================
CVP_OBJ    equ 8
CVP_ENC    equ 16
CVP_ERR    equ 24
CVP_DIR    equ 32
CVP_ARGS   equ 56           ; three Values, the tp_call argument array
CVP_FRAME  equ 72            ; + 1 push = 80, 16-aligned
global codec_via_python
DEF_FUNC codec_via_python, CVP_FRAME
    push rbx
    mov [rbp - CVP_OBJ], rdi
    mov [rbp - CVP_ENC], rsi
    mov [rbp - CVP_ERR], rdx
    mov [rbp - CVP_DIR], rcx

    test ecx, ecx
    jz .cvp_encode
    mov rbx, [rel codec_decode_impl]
    jmp .cvp_have_impl
.cvp_encode:
    mov rbx, [rel codec_encode_impl]
.cvp_have_impl:
    test rbx, rbx
    jnz .cvp_call

    extern kw_names_pending
    mov rax, [rel kw_names_pending]
    push rax
    mov qword [rel kw_names_pending], 0

    CSTRING rdi, "_codecs"
    call str_from_cstr_heap
    push rax
    mov rdi, rax
    xor esi, esi
    xor edx, edx
    extern import_module
    call import_module
    mov rbx, rax
    pop rdi
    call obj_decref
    test rbx, rbx
    jz .cvp_import_failed

    ; Both callables come out at once: the module is imported either way, and
    ; caching only the one asked for means importing again for the other.
    push rbx
    CSTRING rdi, "encode"
    call str_from_cstr_heap
    push rax
    mov rdi, [rbx + PyModuleObject.mod_dict]
    mov rsi, rax
    call dict_get
    mov rbx, rax
    pop rdi
    call obj_decref
    test rbx, rbx
    jz .cvp_missing
    mov rdi, rbx
    call obj_incref
    mov [rel codec_encode_impl], rbx
    pop rbx

    push rbx
    CSTRING rdi, "decode"
    call str_from_cstr_heap
    push rax
    mov rdi, [rbx + PyModuleObject.mod_dict]
    mov rsi, rax
    call dict_get
    mov rbx, rax
    pop rdi
    call obj_decref
    test rbx, rbx
    jz .cvp_missing
    mov rdi, rbx
    call obj_incref
    mov [rel codec_decode_impl], rbx
    pop rbx

    pop rax
    mov [rel kw_names_pending], rax

    cmp qword [rbp - CVP_DIR], 0
    je .cvp_pick_encode
    mov rbx, [rel codec_decode_impl]
    jmp .cvp_call
.cvp_pick_encode:
    mov rbx, [rel codec_encode_impl]

.cvp_call:
    ; args = (obj, encoding, errors).  The two defaults are the ones CPython
    ; gives the same call, and they are built here rather than kept as
    ; constants because a str is a heap object.
    mov rax, [rbp - CVP_OBJ]
    mov [rbp - CVP_ARGS], rax

    mov rax, [rbp - CVP_ENC]
    test rax, rax
    jnz .cvp_have_enc
    CSTRING rdi, "utf-8"
    call str_from_cstr_heap
    mov [rbp - CVP_ENC], rax    ; ours to release below
    mov [rbp - CVP_ARGS + 8], rax
    jmp .cvp_errors
.cvp_have_enc:
    mov [rbp - CVP_ARGS + 8], rax
    mov qword [rbp - CVP_ENC], 0    ; borrowed: nothing to release

.cvp_errors:
    mov rax, [rbp - CVP_ERR]
    test rax, rax
    jz .cvp_default_err
    extern none_singleton
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .cvp_default_err
    mov [rbp - CVP_ARGS + 16], rax
    mov qword [rbp - CVP_ERR], 0
    jmp .cvp_invoke
.cvp_default_err:
    CSTRING rdi, "strict"
    call str_from_cstr_heap
    mov [rbp - CVP_ERR], rax
    mov [rbp - CVP_ARGS + 16], rax

.cvp_invoke:
    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_call]
    test rcx, rcx
    jz .cvp_missing_call
    mov rdi, rbx
    lea rsi, [rbp - CVP_ARGS]
    mov edx, 3
    call rcx
    V_UNPACK rax, rdx

.cvp_release:
    push rax
    push rdx
    mov rdi, [rbp - CVP_ENC]
    test rdi, rdi
    jz .cvp_no_enc_ref
    call obj_decref
.cvp_no_enc_ref:
    mov rdi, [rbp - CVP_ERR]
    test rdi, rdi
    jz .cvp_no_err_ref
    call obj_decref
.cvp_no_err_ref:
    pop rdx
    pop rax
    pop rbx
    leave
    ret

.cvp_missing_call:
    extern exc_TypeError_type
    SET_EXC exc_TypeError_type, "_codecs.encode is not callable"
    xor eax, eax
    xor edx, edx
    jmp .cvp_release

.cvp_missing:
    pop rbx
    add rsp, 8                  ; the parked keyword names
.cvp_import_failed:
    ; import_module leaves its own exception pending; if it somehow did not,
    ; say what was being looked for.
    extern current_exception
    cmp qword [rel current_exception], 0
    jne .cvp_failed_pending
    extern exc_LookupError_type
    SET_EXC exc_LookupError_type, "unknown encoding"
.cvp_failed_pending:
    xor eax, eax
    xor edx, edx
    mov qword [rbp - CVP_ENC], 0
    mov qword [rbp - CVP_ERR], 0
    pop rbx
    leave
    ret
END_FUNC codec_via_python

;; ============================================================================
;; codec_unknown_encoding(rdi = the encoding str, or 0) -- sets the LookupError
;; CPython raises, naming the encoding AS WRITTEN.  codec_id normalises before
;; it compares, and reporting the normalised form said "utf_16" for a lookup
;; of "utf-16".
;; ============================================================================
CUE_MSG   equ 264
CUE_FRAME equ 280            ; + 1 push = 288, 16-aligned
global codec_unknown_encoding
DEF_FUNC codec_unknown_encoding, CUE_FRAME
    push rbx
    mov rbx, rdi
    lea rdi, [rbp - CUE_MSG]
    CSTRING rsi, "unknown encoding: "
    extern rbt_append_cstr
    call rbt_append_cstr
    test rbx, rbx
    jz .cue_raise
    mov rdi, rax
    lea rsi, [rbx + PyStrObject.data]
    call rbt_append_cstr
.cue_raise:
    extern exc_LookupError_type
    lea rdi, [rel exc_LookupError_type]
    lea rsi, [rbp - CUE_MSG]
    extern set_exception
    call set_exception
    pop rbx
    leave
    ret
END_FUNC codec_unknown_encoding

;; ============================================================================
;; codec_id(rdi = encoding str, or 0 for the default) -> eax
;;   0 = utf-8, 1 = ascii, 2 = latin-1, -1 = something else
;;
;; The three codecs the interpreter can do itself.  Anything else is the
;; registry's business: codec_via_python hands the whole call to
;; `_codecs.encode` / `_codecs.decode`, which is where the search functions,
;; the cache and the error handlers all live.  This used to raise LookupError
;; here instead, so a name the registry would have found was refused before
;; anyone asked it.
;; ============================================================================
CI_BUF   equ 48
CI_MSG   equ 240            ; the "unknown encoding: x" message, built in place
CI_FRAME equ 264            ; + 1 push = 272, 16-aligned
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
    mov eax, -1
    pop rbx
    leave
    ret
END_FUNC codec_id

;; ============================================================================
;; str_from_cstr_heap(const char *cstr) -> (rax=PyStrObject*, edx=TAG_PTR)
;; Always heap-allocates. For struct fields that need a real pointer.
;; ============================================================================
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

;; ============================================================================
;; str_from_cstr(const char *cstr) -> (rax=payload, edx=tag)
;; Creates a string from a C string. Always returns heap TAG_PTR.
;; ============================================================================
DEF_FUNC_BARE str_from_cstr
    jmp str_from_cstr_heap
END_FUNC str_from_cstr

;; ============================================================================
;; str_new_heap(const char *data, int64_t len) -> (rax=PyStrObject*, edx=TAG_PTR)
;; Always heap-allocates. For struct fields and internal use.
;; ============================================================================
DEF_FUNC str_new_heap, 8            ; 3 pushes, so rsp is 16-aligned
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

;; ============================================================================
;; str_new(const char *data, int64_t len) -> (rax=payload, edx=tag)
;; Creates a string from data with given length. Always returns heap TAG_PTR.
;; ============================================================================
DEF_FUNC_BARE str_new
    jmp str_new_heap         ; tail-call heap path
END_FUNC str_new

;; ============================================================================
;; str_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_BARE str_dealloc
    ; String data is inline, just free the object
    jmp ap_free
END_FUNC str_dealloc

;; ============================================================================
;; str_repr(PyObject *self) -> PyObject*
;; Returns string with surrounding single quotes: 'hello'
;; ============================================================================
DEF_FUNC str_repr, 8            ; 3 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; rbx = self
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = src length

    ; Allocate worst case: header + 2 quotes + 4*length + 8 (NUL padding).
    ; Four, not two: a control character escapes to \xNN.
    lea rdi, [r12*4 + PyStrObject.data + 10]
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
    cmp al, 0x5c             ; backslash
    je .sr_esc_bs
    cmp eax, r14d            ; the delimiter in use
    je .sr_esc_sq
    cmp al, 0x20             ; the other C0 controls have no letter escape
    jb .sr_esc_hex
    cmp al, 0x7f             ; and neither does DEL
    je .sr_esc_hex
    cmp al, 0x80
    jae .sr_wide

    ; Normal character
    mov [rdi], al
    inc rdi
    inc rcx
    jmp .sr_loop

.sr_wide:
    ; Above ASCII, repr escapes exactly what is not printable -- CPython's
    ; rule, and the reason a non-breaking space comes back as an escape and an
    ; accented letter does not.  Every non-ASCII byte used to be copied
    ; through, so repr() of a string with a soft hyphen or a combining mark in
    ; it put the character itself in the output, invisibly.
    push rcx
    push rsi
    push rdi
    push r12                    ; four, so the calls stay 16-byte aligned
    mov rdi, rsi
    mov rsi, rcx
    extern ucase_utf8_get
    call ucase_utf8_get         ; eax = codepoint, ecx = its width
    push rax
    push rcx
    mov edi, eax
    extern uflags_of
    call uflags_of              ; clobbers r8 and r9, hence the stack
    pop r9                      ; the width
    pop r8                      ; the codepoint
    mov r10d, eax
    pop r12
    pop rdi
    pop rsi
    pop rcx
    test r10d, 256              ; UF_PRINTABLE
    jz .sr_esc_wide

    ; Printable: its bytes go straight across.
    xor eax, eax
.sr_wide_copy:
    cmp eax, r9d
    jge .sr_loop
    mov r10b, [rsi + rcx]
    mov [rdi], r10b
    inc rdi
    inc rcx
    inc eax
    jmp .sr_wide_copy

.sr_esc_wide:
    ; \xNN below 256, \uXXXX below 65536, \UXXXXXXXX above.
    lea r11, [rel sr_hexdigits]
    mov byte [rdi], 0x5c
    cmp r8d, 0x100
    jae .sr_esc_u
    mov byte [rdi + 1], 'x'
    mov r10d, 2
    jmp .sr_esc_digits
.sr_esc_u:
    cmp r8d, 0x10000
    jae .sr_esc_bigu
    mov byte [rdi + 1], 'u'
    mov r10d, 4
    jmp .sr_esc_digits
.sr_esc_bigu:
    mov byte [rdi + 1], 'U'
    mov r10d, 8
.sr_esc_digits:
    add rdi, 2
    mov eax, r10d               ; nibbles left, high one first
.sr_esc_digit:
    test eax, eax
    jz .sr_esc_wide_done
    dec eax
    push rcx
    mov ecx, eax
    shl ecx, 2
    mov edx, r8d
    shr edx, cl
    pop rcx
    and edx, 0x0f
    movzx edx, byte [r11 + rdx]
    mov [rdi], dl
    inc rdi
    jmp .sr_esc_digit
.sr_esc_wide_done:
    movsxd r9, r9d
    add rcx, r9
    jmp .sr_loop

.sr_esc_n:
    mov byte [rdi], 0x5c     ; backslash
    mov byte [rdi + 1], 'n'
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_r:
    mov byte [rdi], 0x5c
    mov byte [rdi + 1], 'r'
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_t:
    mov byte [rdi], 0x5c
    mov byte [rdi + 1], 't'
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_bs:
    mov byte [rdi], 0x5c
    mov byte [rdi + 1], 0x5c
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_sq:
    mov byte [rdi], 0x5c
    mov [rdi + 1], r14b      ; the delimiter in use
    add rdi, 2
    inc rcx
    jmp .sr_loop

.sr_esc_hex:
    ; \xNN, for a control character with no letter escape of its own.  These
    ; used to be copied through raw, so repr("\x00") emitted an actual NUL --
    ; unreadable, and not something eval() could read back.
    mov byte [rdi], 0x5c
    mov byte [rdi + 1], 'x'
    lea r8, [rel sr_hexdigits]
    mov edx, eax
    shr edx, 4
    and edx, 0x0f
    movzx edx, byte [r8 + rdx]
    mov [rdi + 2], dl
    mov edx, eax
    and edx, 0x0f
    movzx edx, byte [r8 + rdx]
    mov [rdi + 3], dl
    add rdi, 4
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

section .rodata
; The conversion characters `%` accepts, in CPython's order.  %b is bytes'
; alone; everything else is common to both.
sm_convs:       db "diouxXeEfFgGcrsa%", 0
sm_convs_bytes: db "diouxXeEfFgGcrsab%", 0

sr_hexdigits: db "0123456789abcdef"

section .text

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
    BINOP_REQUIRE_LEFT str_type, TYPE_FLAG_STR_SUBCLASS, 1
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
    mov rdi, rsi
    mov rsi, rcx
    VALUE_FOR_TYPE rdi, rsi
    mov rsi, rdi
    CSTRING rdi, `can only concatenate str (not "\x01") to str`
    extern raise_type_error_with_name
    call raise_type_error_with_name
END_FUNC str_concat

;; ============================================================================
;; str_repeat(PyObject *str_obj, PyObject *int_obj) -> rax = Value
;; String repetition via nb_multiply
;; ============================================================================
DEF_FUNC str_repeat
    BINOP_REQUIRE_LEFT str_type, TYPE_FLAG_STR_SUBCLASS, 1
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
    ja .srep_toobig

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
.srep_toobig:
    ; Too large to allocate is a MemoryError in CPython; only a count that
    ; does not fit an index is an OverflowError.  list and bytes have said so
    ; since they were written; str sent both cases to the one label.
    RAISE exc_MemoryError_type, ""
.srep_overflow:
    RAISE exc_OverflowError_type, "repeated string is too long"
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
SM_SPECCH  equ 184       ; the conversion as format() spells it: i and u are d
SM_ISBYTES equ 192       ; formatting a BYTES: %s means bytes, %r means b'x'
; Report a failure by RETURNING 0 with the exception set, rather than by
; raising.  bytes_mod asks for this: it holds a decoded copy of the format and
; a raise abandons the C stack, so the copy was leaked once per malformed
; `b"%d" % (1, 2)`.  The nb_remainder slot cannot use it -- a NULL from a
; number slot means "declined", and the interpreter would then look for a
; dunder instead of reporting the error.
SM_NORAISE equ 248
SM_KEYOBJ  equ 200       ; the %(name)s key, for the message when it is missing
SM_STARW   equ 208       ; a '*' width taken from the argument list
SM_STARWON equ 216       ; ...and whether there was one
SM_STARP   equ 224       ; a '*' precision, likewise
SM_STARPON equ 232
SM_SAWDOT  equ 240       ; the spec copier's cursor has passed the '.'
SM_FRAME   equ 256          ; + 0 pushes = 256; SM_NORAISE at 248 is the
                            ; last slot, and the frame is full

;; str_mod is the nb_remainder slot.  str_mod_impl is what bytes_mod calls, with
;; the flag that changes what half the conversions mean: %s on a bytes REQUIRES
;; a bytes-like where str's takes anything, %r has to answer b'x' and not 'x',
;; %b exists at all, and %c takes a byte.  bytes % used to reach here by
;; latin-1 decoding the format and every bytes-like argument up front, which
;; cannot express any of that -- the conversion is only known here, so the
;; argument is converted here.
DEF_FUNC_BARE str_mod
    xor ecx, ecx                ; the slot raises; only bytes_mod does not
    xor edx, edx
    jmp str_mod_impl
END_FUNC str_mod

global str_mod_impl
DEF_FUNC str_mod_impl, SM_FRAME
    mov [rbp-SM_ISBYTES], rdx
    mov [rbp-SM_NORAISE], rcx
    BINOP_REQUIRE_LEFT str_type, TYPE_FLAG_STR_SUBCLASS, 1
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
    ; CPython's test is PyMapping_Check -- anything with an mp_subscript --
    ; and not just a dict, which is why `"ab" % [1, 2]` is 'ab' there and was
    ; a TypeError here.  A tuple is excluded below (it is the argument list),
    ; and a str is excluded here (it is a single value).
    push rsi
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_STR_TYPE rax, rcx, .sm_map_check
    jmp .sm_not_map
.sm_map_check:
    ; A tuple has an mp_subscript too, and it is the argument list rather than
    ; a mapping -- treating one as a mapping skipped the arity check, so
    ; `"%s" % ("a", "b")` quietly formatted the first and dropped the second.
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_TUPLE_TYPE rax, rcx, .sm_map_not_tuple
    jmp .sm_not_map
.sm_map_not_tuple:
    ; In a BYTES format a bytes or bytearray argument is a single value, not a
    ; mapping, exactly as a str is for a str format.  Both have an
    ; mp_subscript, so without this b"ab" % b"cd" skipped the arity check and
    ; answered b'ab' instead of saying the argument was never converted.
    cmp qword [rbp-SM_ISBYTES], 0
    je .sm_map_not_bytes
    mov rdi, rsi
    extern bytes_mod_is_byteslike
    call bytes_mod_is_byteslike
    test eax, eax
    jnz .sm_not_map
.sm_map_not_bytes:
    mov rax, [rsi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .sm_not_map
    cmp qword [rax + PyMappingMethods.mp_subscript], 0
    je .sm_not_map
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
    ; A BYTES format is keyed by BYTES.  The format was decoded to a str to be
    ; scanned, so the key comes out of that str and has to go back -- without
    ; which b"%(a)s" % {b"a": b"x"} looked up "a" in a dict that has b"a".
    cmp qword [rbp-SM_ISBYTES], 0
    je .sm_key_have
    push rcx
    push rax
    mov rdi, rax
    extern bytes_latin1_from_str
    call bytes_latin1_from_str
    pop rdi
    push rax
    call obj_decref                 ; the str key
    pop rax
    pop rcx
.sm_key_have:
    push rcx
    push rax
    mov [rbp-SM_KEYOBJ], rax        ; the key, ours to release
    mov rdi, [rbp-SM_ARGS]
    mov rsi, rax
    call str_mod_subscript
    mov r9, rax
    pop rdi
    pop rcx
    ; Release the key only once the lookup has answered: the error path names
    ; it in the exception, and freeing it first left that reading freed memory.
    test r9, r9
    jz .sm_key_error
    push rcx
    push r9
    call obj_decref
    pop r9
    pop rcx
    mov [rbp-SM_KEYVAL], r9
    mov qword [rbp-SM_HASKEY], 1
    inc rcx                         ; step past ')'

.sm_mark_spec:
    mov qword [rbp-SM_STARWON], 0
    mov qword [rbp-SM_STARPON], 0
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
    cmp al, '*'
    je .sm_star_width
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
    cmp al, '*'
    je .sm_star_prec
    cmp al, '0'
    jb .sm_dispatch
    cmp al, '9'
    ja .sm_dispatch
    inc rcx
    cmp rcx, r12
    jge .sm_done
    jmp .sm_skip_prec

;; "%*d" % (6, 42) and "%.*g" % (3, x): the width, the precision or both come
;; from the argument list.  The whole family was unhandled -- '*' is not a
;; flag, not a digit and not '.', so the scanner stopped there, the conversion
;; went out with no width, and the width argument was still sitting in the
;; tuple when the arity check ran.  What that reported was "not all arguments
;; converted during string formatting", which names neither the directive nor
;; the reason.
.sm_star_width:
    push rcx
    call .sm_get_arg            ; rax = payload, rdx = tag
    call .sm_star_to_i64
    pop rcx
    mov [rbp-SM_STARW], rax
    mov qword [rbp-SM_STARWON], 1
    inc rcx
    cmp rcx, r12
    jge .sm_done
    movzx eax, byte [rbx + rcx] ; .sm_check_dot reads the character in al
    jmp .sm_check_dot

.sm_star_prec:
    push rcx
    call .sm_get_arg
    call .sm_star_to_i64
    pop rcx
    ; A negative precision is no precision at all -- CPython's
    ; "%.*g" % (-1, 1.5) is "%.0g" % 1.5, which is '2'.
    test rax, rax
    jns .sm_star_prec_keep
    xor eax, eax
.sm_star_prec_keep:
    mov [rbp-SM_STARP], rax
    mov qword [rbp-SM_STARPON], 1
    inc rcx
    cmp rcx, r12
    jge .sm_done
    jmp .sm_dispatch

;; .sm_star_to_i64(rax = payload, rdx = tag) -> rax = the number
;; CPython takes an int and nothing else here -- not a float, and not even an
;; object with __index__ -- and says "* wants int".
.sm_star_to_i64:
    push rcx
    mov rdi, rax
    call int_is_integer
    test eax, eax
    jz .sm_star_bad
    call int_unwrap             ; rdi, edx: a compact int flattens to a smallint
    cmp edx, TAG_SMALLINT
    jne .sm_star_big
    mov rax, rdi
    pop rcx
    ret
.sm_star_big:
    ; A width or precision that needs GMP is nonsense, but truncating it is
    ; still better than reading the pointer as a number.
    call int_to_i64
    pop rcx
    ret
.sm_star_bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "* wants int"
    jmp .sm_error

.sm_dispatch:
    ; In bytes mode every conversion takes the spec path, so the argument is
    ; converted in exactly one place.  %% is not a conversion and takes no
    ; argument, so it stays where it was.
    cmp qword [rbp-SM_ISBYTES], 0
    je .sm_dispatch_str
    movzx eax, byte [rbx + rcx]
    cmp al, '%'
    jne .sm_use_spec
    jmp .sm_dispatch_plain
.sm_dispatch_str:
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
    ; d, i and u too, so that one place checks the argument against the
    ; conversion.  The direct path below could not: it formatted whatever it
    ; was handed, which is how "%d" % "x" came to answer 'x'.
    cmp al, 'd'
    je .sm_use_spec
    cmp al, 'i'
    je .sm_use_spec
    cmp al, 'u'
    je .sm_use_spec
    ; %a and %c had no handler at all: the dispatcher's unknown-conversion arm
    ; printed them literally and consumed no argument, so "%c" % (65,) came
    ; back as "%c" and then complained that an argument was left over.
    cmp al, 'a'
    je .sm_use_spec
    cmp al, 'c'
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
    ; Unknown: CPython raises rather than echoing it.
    movzx edi, al
    lea rsi, [rcx - 1]
    jmp .sm_bad_conv

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
    and edx, 0xf
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
    ; CPython makes the keyed value the argument source and lets it be taken
    ; once: "%(a)*d" % {"a": 1} uses the 1 as the WIDTH and then has nothing
    ; left for the %d.  Without this the second fetch fell back to the mapping
    ; itself and complained about formatting a dict.  A mapping skips the
    ; arity check entirely, so counting here costs nothing else.
    inc r15
    ret
.sm_arg_positional:
    cmp qword [rbp-SM_ISTUPLE], 1
    je .sm_arg_tuple
    ; Single value.  A mapping counts as one too -- "%s" % {"a": 1} formats
    ; the dict -- but only once: a second unkeyed conversion has nothing left,
    ; which is what CPython reports as "not enough arguments".
    cmp qword [rbp-SM_ISMAP], 1
    jne .sm_arg_single
    test r15, r15
    jnz .sm_arg_none
.sm_arg_single:
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
    jmp .sm_error

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
    ; "bytes formatting" when that is what it is: bytes_mod goes through this
    ; function, and the message it produced named the wrong type.
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "not all arguments converted during string formatting"
    cmp qword [rbp-SM_ISBYTES], 0
    je .sm_error
    CSTRING rsi, "not all arguments converted during bytes formatting"
    jmp .sm_error

.sm_key_unterminated:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "incomplete format key"
    jmp .sm_error

;; .sm_bad_conv(rdi = the conversion character, rsi = its index) -- CPython's
;; wording, which names the character twice and says where it was.  Reached
;; from both dispatchers: the spec path validates against the table, and the
;; direct path used to print an unknown conversion LITERALLY and consume no
;; argument, so "%z" % (1,) answered "%z" and then complained about a leftover
;; argument.
.sm_bad_conv:
    push rdi
    push rsi
    lea rdi, [rel sm_convbuf]
    CSTRING rsi, "unsupported format character '"
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rcx, [rsp + 8]
    mov [rax], cl
    inc rax
    mov rdi, rax
    CSTRING rsi, "' (0x"
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rsp + 8]
    extern msg_append_hex2
    call msg_append_hex2
    mov rdi, rax
    CSTRING rsi, ") at index "
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rsp]
    extern msg_append_i64
    call msg_append_i64
    mov byte [rax], 0
    add rsp, 16
    lea rdi, [rel exc_ValueError_type]
    lea rsi, [rel sm_convbuf]
    jmp .sm_error

.sm_key_error:
    ; CPython names the key that was missing, as an ordinary dict lookup does.
    ; A fixed message said only that one was.  The key object was released
    ; just above, so this re-reads it -- it is still allocated, and its only
    ; use here is the message.
    lea rsp, [rbp - SM_FRAME - 40]      ; as .sm_error does, and for the same
    mov rdi, [rbp-SM_KEYOBJ]           ; reason
    extern set_key_error
    call set_key_error
    mov rdi, [rbp-SM_BUF]
    test rdi, rdi
    jz .sm_ke_freed
    mov qword [rbp-SM_BUF], 0
    call ap_free
.sm_ke_freed:
    cmp qword [rbp-SM_NORAISE], 0
    jne .sm_error_ret           ; the caller reads the pending exception
    extern current_exception
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    extern raise_exception_obj
    jmp raise_exception_obj     ; takes the reference, does not return

;; The one way out.  rdi = the exception type, rsi = the message; the buffer
;; goes back either way, because a raise abandons this frame and the free
;; below with it.
.sm_error:
    ; Some of these sites are subroutines of this function, reached with a
    ; `call` -- so the return address is still on the stack, and popping the
    ; five saved registers over it put a return address in r15.  RAISE could
    ; ignore that, because it abandoned the whole stack; returning cannot.
    lea rsp, [rbp - SM_FRAME - 40]      ; the five pushes, and nothing else
    cmp qword [rbp-SM_NORAISE], 0
    jne .sm_error_set
    push rdi
    push rsi
    mov rdi, [rbp-SM_BUF]
    test rdi, rdi
    jz .sm_error_freed
    mov qword [rbp-SM_BUF], 0
    call ap_free
.sm_error_freed:
    pop rsi
    pop rdi
    extern raise_exception
    call raise_exception        ; does not return
    ud2
.sm_error_set:
    extern set_exception
    call set_exception
.sm_error_installed:
    mov rdi, [rbp-SM_BUF]
    test rdi, rdi
    jz .sm_error_ret
    mov qword [rbp-SM_BUF], 0
    call ap_free
.sm_error_ret:
    xor eax, eax
    xor edx, edx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
;; Format one directive through format_apply_spec.  On entry SM_POS is the
;; index of the conversion character and SM_SPECST the start of the flags;
;; on exit SM_POS is just past it.  r13 (buffer), r14 (output position),
;; r15 (argument index), rbx and r12 belong to the caller's loop, so
;; everything here lives in frame slots.
.sm_spec_conv:
    mov r8, [rbp-SM_POS]
    movzx r9d, byte [rbx + r8]      ; the conversion character
    mov [rbp-SM_CONV], r9
    ; %i and %u are %d's spellings; format() knows only the one.  SM_CONV
    ; keeps the original, because the error messages name it.
    mov [rbp-SM_SPECCH], r9

    ; The conversions % understands.  Anything else was accepted and then
    ; formatted as though it had been %s, so `b"%z" % (1,)` answered b"1"
    ; where CPython raises and names the character.  %b is bytes' alone.
    push r8
    push r9
    lea rsi, [rel sm_convs]
    cmp qword [rbp-SM_ISBYTES], 0
    je .sm_sc_check
    lea rsi, [rel sm_convs_bytes]
.sm_sc_check:
    movzx ecx, r9b
.sm_sc_scan:
    movzx eax, byte [rsi]
    test al, al
    jz .sm_sc_bad
    cmp eax, ecx
    je .sm_sc_known
    inc rsi
    jmp .sm_sc_scan
.sm_sc_bad:
    mov rdi, [rsp]              ; the conversion character
    mov rsi, [rsp + 8]          ; its index in the format
    add rsp, 16
    jmp .sm_bad_conv
.sm_sc_known:
    pop r9
    pop r8

    cmp r9b, 'i'
    je .sm_sc_as_d
    cmp r9b, 'u'
    je .sm_sc_as_d
    cmp r9b, 'r'
    je .sm_sc_as_s
    cmp r9b, 'a'
    je .sm_sc_as_s
    cmp r9b, 'c'
    je .sm_sc_as_s
    cmp qword [rbp-SM_ISBYTES], 0
    je .sm_sc_conv_kept
    cmp r9b, 'b'
    je .sm_sc_as_s              ; %b is bytes', and only exists there
    cmp r9b, 's'
    je .sm_sc_as_s
    jmp .sm_sc_conv_kept
.sm_sc_as_d:
    mov qword [rbp-SM_SPECCH], 'd'
    jmp .sm_sc_conv_kept
.sm_sc_as_s:
    ; %r, %a and %c each build a string first; what is left is a str spec.
    mov qword [rbp-SM_SPECCH], 's'
.sm_sc_conv_kept:
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
    ; A '*' width that came out negative means left-alignment, as a literal
    ; '-' flag does: CPython's "%*d" % (-6, 42) is '42    '.
    cmp qword [rbp-SM_STARWON], 0
    je .sm_sc_align_from_flags
    cmp qword [rbp-SM_STARW], 0
    jge .sm_sc_align_from_flags
    mov r11d, 1
.sm_sc_align_from_flags:
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
    mov qword [rbp-SM_SAWDOT], 0
    mov rax, [rbp-SM_SPECST]
.sm_sc_copy:
    mov rcx, [rbp-SM_POS]
    dec rcx
    cmp rax, rcx
    jge .sm_sc_copy_done
    movzx ecx, byte [rbx + rax]
    cmp cl, '.'
    je .sm_sc_copy_dot
    cmp cl, '*'
    je .sm_sc_copy_star
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

.sm_sc_copy_dot:
    mov qword [rbp-SM_SAWDOT], 1
    jmp .sm_sc_copy_keep

;; A '*' in the source stands for a number taken from the argument list; the
;; format-spec engine downstream knows nothing about '*', so the digits go in
;; here.  Which of the two it is follows from whether the '.' has gone by.
.sm_sc_copy_star:
    push rax
    mov rax, [rbp-SM_STARW]
    cmp qword [rbp-SM_SAWDOT], 0
    je .sm_sc_star_have
    mov rax, [rbp-SM_STARP]
.sm_sc_star_have:
    ; The sign is not part of the spec: a negative width has already become
    ; left-alignment, and a negative precision has already become zero.
    test rax, rax
    jns .sm_sc_star_emit
    neg rax
.sm_sc_star_emit:
    push rbx
    push r12
    xor r12d, r12d              ; digits pushed
.sm_sc_star_split:
    xor edx, edx
    mov rbx, 10
    div rbx                     ; rax = quotient, rdx = digit
    add rdx, '0'
    push rdx
    inc r12
    test rax, rax
    jnz .sm_sc_star_split
.sm_sc_star_pop:
    pop rcx
    cmp r10, 36                 ; the spec buffer is 40 bytes and grows up
    jge .sm_sc_star_skip
    mov [rdi + r10], cl
    inc r10
.sm_sc_star_skip:
    dec r12
    jnz .sm_sc_star_pop
    pop r12
    pop rbx
    pop rax
    jmp .sm_sc_copy_next

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
    mov rcx, [rbp-SM_SPECCH]
    mov [rdi + r10], cl
    inc r10

    lea rdi, [rbp-SM_SPEC]
    mov rsi, r10
    call str_new_heap
    mov [rbp-SM_SPECOBJ], rax

    call .sm_get_arg
    V_PACK rax, rdx
    mov [rbp-SM_VALUE], rax
    mov qword [rbp-SM_OWNVAL], 0
    ; The three conversions a BYTES format spells differently come first: they
    ; take the argument as it is, and would not survive the numeric check.
    mov rcx, [rbp-SM_CONV]
    cmp qword [rbp-SM_ISBYTES], 0
    je .sm_sc_coerce
    cmp cl, 'b'
    je .sm_sc_bytes_like
    cmp cl, 's'
    je .sm_sc_bytes_like
    cmp cl, 'c'
    je .sm_sc_bytes_char

.sm_sc_coerce:
    ; The argument has to suit the conversion, and may need converting to it:
    ; %d takes a float and truncates, %f takes an int and widens, and both
    ; take anything offering __index__ or __float__.
    mov rdi, [rbp-SM_VALUE]
    mov rsi, [rbp-SM_CONV]
    extern fmt_percent_coerce
    call fmt_percent_coerce
    mov [rbp-SM_VALUE], rax
    mov [rbp-SM_OWNVAL], rdx
    mov rcx, [rbp-SM_CONV]
    cmp cl, 'r'
    je .sm_sc_repr
    cmp cl, 'a'
    je .sm_sc_ascii
    cmp cl, 'c'
    je .sm_sc_char
    jmp .sm_sc_have_value_owned

.sm_sc_repr:
    mov rdi, [rbp-SM_VALUE]
    call obj_repr
    V_UNPACK rax, rdx
    V_PACK rax, rdx
    mov [rbp-SM_VALUE], rax
    mov qword [rbp-SM_OWNVAL], 1
    jmp .sm_sc_format

.sm_sc_ascii:
    sub rsp, 16
    mov rax, [rbp-SM_VALUE]
    mov [rsp], rax
    mov rdi, rsp
    mov esi, 1
    extern builtin_ascii_fn
    call builtin_ascii_fn
    add rsp, 16
    mov [rbp-SM_VALUE], rax
    mov qword [rbp-SM_OWNVAL], 1
    jmp .sm_sc_format

.sm_sc_char:
    ; An integer becomes the character it numbers; a one-character string is
    ; already the answer.  Anything else, including a longer string, is not.
    mov rdi, [rbp-SM_VALUE]
    V_TEST_PTR rdi, rcx
    ja .sm_sc_char_int
    mov rcx, [rdi + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .sm_sc_char_int
    cmp qword [rdi + PyStrObject.ob_length], 1
    jne .sm_sc_char_bad
    INCREF rdi
    mov [rbp-SM_VALUE], rdi
    mov qword [rbp-SM_OWNVAL], 1
    jmp .sm_sc_format
.sm_sc_char_int:
    mov rdi, [rbp-SM_VALUE]
    V_UNPACK rdi, rdx
    extern int_is_integer
    call int_is_integer
    test eax, eax
    jz .sm_sc_char_bad
    sub rsp, 16
    mov rax, [rbp-SM_VALUE]
    mov [rsp], rax
    mov rdi, rsp
    mov esi, 1
    extern builtin_chr
    call builtin_chr
    add rsp, 16
    V_PACK rax, rdx
    mov [rbp-SM_VALUE], rax
    mov qword [rbp-SM_OWNVAL], 1
    jmp .sm_sc_format
.sm_sc_char_bad:
    RAISE exc_TypeError_type, "%c requires int or char"

;; %s and %b on a BYTES format: the argument must be bytes-like, and its bytes
;; go in unchanged.  Decoding it as latin-1 makes a str whose code points are
;; its bytes; bytes_mod re-encodes the result the same way, so the round trip
;; is exact.
.sm_sc_bytes_like:
    mov rdi, [rbp-SM_VALUE]
    extern bytes_mod_as_str
    call bytes_mod_as_str
    test rax, rax
    jz .sm_sc_bytes_bad
    mov [rbp-SM_VALUE], rax
    mov qword [rbp-SM_OWNVAL], 1
    jmp .sm_sc_format
.sm_sc_bytes_bad:
    mov rsi, [rbp-SM_VALUE]
    CSTRING rdi, `%b requires a bytes-like object, or an object that implements __bytes__, not '\x01'`
    extern raise_type_error_with_name
    call raise_type_error_with_name

;; %c on a BYTES format: an integer in range(256), or a single byte.
.sm_sc_bytes_char:
    mov rdi, [rbp-SM_VALUE]
    V_TEST_PTR rdi, rcx
    ja .sm_sc_bc_int
    mov rcx, [rdi + PyObject.ob_type]
    extern bytes_type
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    jne .sm_sc_bc_int
    cmp qword [rdi + PyBytesObject.ob_size], 1
    jne .sm_sc_bc_bad
    movzx edi, byte [rdi + PyBytesObject.data]
    jmp .sm_sc_bc_from_cp
.sm_sc_bc_int:
    mov rdi, [rbp-SM_VALUE]
    V_UNPACK rdi, rdx
    call int_is_integer
    test eax, eax
    jz .sm_sc_bc_bad
    mov rdi, [rbp-SM_VALUE]
    V_UNPACK rdi, rdx
    extern int_to_i64
    call int_to_i64
    cmp rax, 0
    jl .sm_sc_bc_range
    cmp rax, 255
    ja .sm_sc_bc_range
    mov rdi, rax
.sm_sc_bc_from_cp:
    ; One code point, which the re-encode turns back into the byte it names.
    ; It has to go in as UTF-8: a raw 0xff is not a str, and the re-encode
    ; read it as the lead byte of a sequence -- b"%c" % 255 came out b"\xc0".
    sub rsp, 32
    mov eax, edi
    mov rdi, rsp
    extern ucase_utf8_put
    call ucase_utf8_put         ; ecx = bytes written
    movsxd rsi, ecx
    mov rdi, rsp
    call str_new_heap
    add rsp, 32
    mov [rbp-SM_VALUE], rax
    mov qword [rbp-SM_OWNVAL], 1
    jmp .sm_sc_format
.sm_sc_bc_bad:
    RAISE exc_TypeError_type, "%c requires an integer in range(256) or a single byte"
.sm_sc_bc_range:
    RAISE exc_OverflowError_type, "%c arg not in range(256)"

.sm_sc_have_value_owned:

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
    ; DECREF_V and not obj_decref: what the argument check hands back may be
    ; an int or a float IMMEDIATE -- "%d" % 3.9 truncates to one -- and
    ; obj_decref would dereference it as a pointer.
    mov rdi, [rbp-SM_VALUE]
    DECREF_V rdi, rsi
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

END_FUNC str_mod_impl

;; ============================================================================
;; str_mod_subscript(rdi = the mapping, rsi = the key str) -> rax = Value, or 0
;;
;; `"%(a)s" % m` for any m with an mp_subscript, not only a dict -- the same
;; widening the operand classification got.  A dict keeps the direct lookup:
;; dict_get answers 0 for a miss where dict's own mp_subscript raises KeyError,
;; and str_mod's caller wants the former.
;;
;; The reference this hands back is borrowed for a dict and owned for anything
;; else, and str_mod treats it as borrowed throughout -- so a mapping of one's
;; own leaks one reference per key.  Releasing it here is not possible: the
;; value is read long after, and a raise anywhere between abandons the stack.
;; ============================================================================
DEF_FUNC_LOCAL str_mod_subscript
    mov rax, [rdi + PyObject.ob_type]
    extern dict_type
    lea rcx, [rel dict_type]
    cmp rax, rcx
    je .sms_dict
    REQUIRE_DICT_TYPE rax, rcx, .sms_generic
.sms_dict:
    extern dict_get
    call dict_get
    leave
    ret
.sms_generic:
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .sms_none
    mov rax, [rax + PyMappingMethods.mp_subscript]
    test rax, rax
    jz .sms_none
    call rax
    leave
    ret
.sms_none:
    xor eax, eax
    leave
    ret
END_FUNC str_mod_subscript

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
    ; Right operand is not a string: DECLINE, for every op.
    ;
    ; EQ used to answer False here and NE True, which is the right final
    ; answer but not this slot's to give -- declining is what lets the
    ; protocol ask the OTHER operand, and it only falls back to identity
    ; when that declines too.  `'a' == S()` for a class defining __eq__ was
    ; False where CPython calls S.__eq__, and by name str.__eq__('a', 1) was
    ; False where CPython says NotImplemented.
    RET_NULL
    pop rbx
    leave
    ret

.ret_true:
    RET_TRUE
    pop rbx
    leave
    ret
.ret_false:
    RET_FALSE
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
;; str_getitem(PyObject *self, int64_t index) -> rax = Value
;; sq_item: return single-char string at index
;; ============================================================================
DEF_FUNC str_getitem, 8            ; 3 pushes, so rsp is 16-aligned
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
    RAISE exc_IndexError_type, "string index out of range"
END_FUNC str_getitem

;; ============================================================================
;; str_subscript(PyObject *self, PyObject *key) -> rax = Value
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
    RAISE exc_TypeError_type, "string indices must be integers"
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

    ; Length-aware: ap_strstr stopped at the first NUL, so "b" in "a\x00b"
    ; was False.
    extern ap_memfind
    mov rcx, [rsi + PyStrObject.ob_size]
    lea rdx, [rsi + PyStrObject.data]
    mov rsi, [rdi + PyStrObject.ob_size]
    lea rdi, [rdi + PyStrObject.data]
    call ap_memfind
    test rax, rax
    setnz al
    movzx eax, al

    leave
    ret

.str_contains_type_error:
    extern exc_TypeError_type
    extern raise_exception
    RAISE exc_TypeError_type, "'in <string>' requires string as left operand"
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
SGS_FRAME equ 64            ; + 2 pushes = 80
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
DEF_FUNC str_tp_iter, 8            ; 1 pushes, so rsp is 16-aligned
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
DEF_FUNC str_iter_next, 8            ; 1 pushes, so rsp is 16-aligned
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
DEF_FUNC str_iter_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
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
    dq 0                        ; tp_tailslots

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
    dq 0                        ; tp_tailslots


section .data
align 8
codec_encode_impl: dq 0
codec_decode_impl: dq 0

section .bss
; The "unsupported format character" message, built in place.
sm_convbuf: resb 128
