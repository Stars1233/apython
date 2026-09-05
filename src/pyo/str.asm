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
extern obj_as_index_seq
extern int_fits_i64
extern exc_OverflowError_type
extern exc_MemoryError_type
extern slice_type
extern slice_indices
extern type_type
extern obj_dealloc
; the `%` operator, now pyo/str_mod.asm
extern str_mod


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
sr_hexdigits: db "0123456789abcdef"

extern dict_get
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
    mov rsi, rdi
    mov rcx, rdx
    V_PACK rsi, rcx
    ; Not a count at all: DECLINE rather than raise, so the protocol carries
    ; on to the right operand's __rmul__.  This raised, and `x * R()` for an R
    ; with an __rmul__ never reached it.  op_binary_op words the failure when
    ; nothing else answers either.
    mov rdi, rsi
    push rsi
    extern binop_is_count
    call binop_is_count
    pop rsi
    test eax, eax
    jz .srep_decline
    extern seq_repeat_count
    call seq_repeat_count    ; __index__ counts, and one too big to be an
    mov r12, rax             ; index is refused rather than truncated

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
.srep_decline:
    xor eax, eax
    xor edx, edx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
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
    jne .ss_int                 ; fully before dereferencing, or raw
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
    lea rsi, [rel ss_index_msg] ; ...and the refusal names the key's type
    call obj_as_index_seq
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

END_FUNC str_subscript

section .rodata
ss_index_msg: db `string indices must be integers, not '\x01'`, 0
section .text

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

