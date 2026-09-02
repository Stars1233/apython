; methods/str_parts.asm - str: taking a string apart and putting it back
;
; partition, splitlines, expandtabs, translate, maketrans, removeprefix,
; removesuffix and encode.
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
extern ap_memfind
extern str_find_impl
extern ap_malloc
extern ap_free
extern ap_memcpy
extern ap_memcmp
extern obj_decref
extern str_new_heap
extern obj_as_index
extern int_is_integer
extern str_type
extern list_new
extern list_append
extern tuple_new
extern dict_new
extern dict_get
extern dict_set
extern none_singleton
extern bool_true
extern bool_false
extern int_from_i64
extern int_to_i64
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type

; Set entry layout constants (must match set.asm)

; --- moved to a sibling file by the split ---
extern str_method_join

section .text

;; ============================================================================
;; str_method_rindex(args, nargs) -> int
;; Like rfind but raises ValueError if not found
;; args[0]=self, args[1]=substr
;; ============================================================================
DEF_FUNC_BARE str_method_rindex
    mov edx, 3                  ; reverse, raise on a miss
    jmp str_find_impl
END_FUNC str_method_rindex

;; ============================================================================
;; str_method_istitle(args, nargs) -> bool
;; ============================================================================
DEF_FUNC str_method_istitle
    push rbx
    push r12

    mov rbx, [rdi]
    mov r12, [rbx + PyStrObject.ob_size]

    ; Empty string → False
    test r12, r12
    jz .istitle_false

    xor ecx, ecx            ; i = 0
    mov r8d, 1               ; prev_sep = true
    xor r9d, r9d             ; seen_cased = false
.istitle_loop:
    cmp rcx, r12
    jge .istitle_check
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, 'A'
    jb .istitle_not_alpha
    cmp al, 'Z'
    jbe .istitle_upper
    cmp al, 'a'
    jb .istitle_not_alpha
    cmp al, 'z'
    ja .istitle_not_alpha
    ; lowercase char
    test r8d, r8d
    jnz .istitle_false        ; lowercase after separator → not title
    xor r8d, r8d
    mov r9d, 1
    inc rcx
    jmp .istitle_loop
.istitle_upper:
    test r8d, r8d
    jz .istitle_false         ; uppercase after alpha → not title
    xor r8d, r8d
    mov r9d, 1
    inc rcx
    jmp .istitle_loop
.istitle_not_alpha:
    mov r8d, 1                ; prev_sep = true
    inc rcx
    jmp .istitle_loop
.istitle_check:
    test r9d, r9d
    jz .istitle_false         ; no cased chars → False
    RET_TRUE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.istitle_false:
    RET_FALSE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_istitle

;; ============================================================================
;; str_method_partition(args, nargs) -> 3-tuple (before, sep, after)
;; args[0]=self, args[1]=sep
;; ============================================================================
PT_SELF   equ 8
PT_SEP    equ 16
PT_FRAME  equ 16            ; + 3 pushes = 40, not 16-aligned
DEF_FUNC str_method_partition, PT_FRAME
    push rbx
    push r12
    push r13

    mov rbx, [rdi]           ; self
    mov r12, [rdi + 8]      ; sep
    mov [rbp - PT_SELF], rbx
    mov [rbp - PT_SEP], r12

    ; Find sep in self.  Length-aware: ap_strstr stopped at the first NUL.
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, [rbx + PyStrObject.ob_size]
    lea rdx, [r12 + PyStrObject.data]
    mov rcx, [r12 + PyStrObject.ob_size]
    call ap_memfind
    test rax, rax
    jz .part_not_found

    ; Found: compute before, sep, after
    mov r13, rax             ; pointer to match
    lea rcx, [rbx + PyStrObject.data]
    sub r13, rcx             ; r13 = match index

    ; Create before string
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r13
    call str_new_heap
    push rax                 ; save before

    ; INCREF sep (reuse original)
    mov r12, [rbp - PT_SEP]
    INCREF r12

    ; Create after string
    mov rbx, [rbp - PT_SELF]
    mov rcx, [r12 + PyStrObject.ob_size]
    lea rax, [r13 + rcx]     ; after_start = match_idx + sep_len
    mov rdx, [rbx + PyStrObject.ob_size]
    sub rdx, rax              ; after_len = self_len - after_start
    lea rdi, [rbx + PyStrObject.data + rax]
    mov rsi, rdx
    call str_new_heap
    mov r13, rax             ; r13 = after

    ; Create 3-tuple
    mov rdi, 3
    call tuple_new
    mov rbx, rax             ; rbx = tuple

    mov r9, [rbx + PyTupleObject.ob_item]
    pop rcx                  ; before
    mov [r9], rcx
    mov [r9 + 8], r12
    mov [r9 + 16], r13

    mov rax, rbx
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.part_not_found:
    ; Return (self_copy, "", "")
    mov rbx, [rbp - PT_SELF]
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, [rbx + PyStrObject.ob_size]
    call str_new_heap
    push rax                 ; before = self copy

    ; Create two empty strings
    CSTRING rdi, ""
    xor esi, esi
    call str_new_heap
    push rax                 ; empty1

    CSTRING rdi, ""
    xor esi, esi
    call str_new_heap
    mov r13, rax             ; empty2

    mov rdi, 3
    call tuple_new
    mov rbx, rax

    mov r9, [rbx + PyTupleObject.ob_item]
    pop rcx                  ; empty1
    pop rax                  ; before
    mov [r9], rax
    mov [r9 + 8], rcx
    mov [r9 + 16], r13

    mov rax, rbx
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_partition

;; ============================================================================
;; str_method_rpartition(args, nargs) -> 3-tuple (before, sep, after)
;; Like partition but searches from right
;; ============================================================================
DEF_FUNC str_method_rpartition, PT_FRAME
    push rbx
    push r12
    push r13

    mov rbx, [rdi]           ; self
    mov r12, [rdi + 8]      ; sep
    mov [rbp - PT_SELF], rbx
    mov [rbp - PT_SEP], r12

    ; Search from right: find last occurrence
    mov r13, [rbx + PyStrObject.ob_size]
    mov rcx, [r12 + PyStrObject.ob_size]
    mov rax, r13
    sub rax, rcx              ; max start pos
    js .rpart_not_found

.rpart_loop:
    cmp rax, 0
    jl .rpart_not_found
    push rax
    push rcx
    lea rdi, [rbx + PyStrObject.data]
    add rdi, rax
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, rcx
    call ap_memcmp
    mov r8d, eax              ; save memcmp result
    pop rcx
    pop rax
    test r8d, r8d
    jz .rpart_found
    dec rax
    jmp .rpart_loop

.rpart_found:
    ; rax = match index
    mov r13, rax

    ; Create before string
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r13
    call str_new_heap
    push rax

    ; INCREF sep
    mov r12, [rbp - PT_SEP]
    INCREF r12

    ; Create after string
    mov rbx, [rbp - PT_SELF]
    mov rcx, [r12 + PyStrObject.ob_size]
    lea rax, [r13 + rcx]
    mov rdx, [rbx + PyStrObject.ob_size]
    sub rdx, rax
    lea rdi, [rbx + PyStrObject.data + rax]
    mov rsi, rdx
    call str_new_heap
    mov r13, rax

    mov rdi, 3
    call tuple_new
    mov rbx, rax

    mov r9, [rbx + PyTupleObject.ob_item]
    pop rcx
    mov [r9], rcx
    mov [r9 + 8], r12
    mov [r9 + 16], r13

    mov rax, rbx
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rpart_not_found:
    ; Return ("", "", self_copy)
    CSTRING rdi, ""
    xor esi, esi
    call str_new_heap
    push rax

    CSTRING rdi, ""
    xor esi, esi
    call str_new_heap
    push rax

    mov rbx, [rbp - PT_SELF]
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, [rbx + PyStrObject.ob_size]
    call str_new_heap
    mov r13, rax

    mov rdi, 3
    call tuple_new
    mov rbx, rax

    mov r9, [rbx + PyTupleObject.ob_item]
    pop rcx                  ; empty2
    pop rax                  ; empty1
    mov [r9], rax
    mov [r9 + 8], rcx
    mov [r9 + 16], r13

    mov rax, rbx
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_rpartition

;; ============================================================================
;; str_method_expandtabs(args, nargs) -> new string
;; args[0]=self, args[1]=tabsize (optional, default 8)
;; ============================================================================
ET_TAB    equ 8
ET_BUF    equ 16
ET_RES    equ 24
ET_FRAME  equ 32            ; + 4 pushes = 64
DEF_FUNC str_method_expandtabs, ET_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]           ; self
    mov r12, [rbx + PyStrObject.ob_size]

    ; Get tabsize (default 8)
    mov r13, 8
    cmp rsi, 2
    jl .et_have_tab
    mov rax, rdi
    mov rdi, [rax + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call int_to_i64
    mov r13, rax
.et_have_tab:
    mov [rbp - ET_TAB], r13

    ; First pass: compute output length
    xor ecx, ecx            ; i
    xor r14d, r14d           ; col
    xor r8d, r8d             ; out_len
.et_len_loop:
    cmp rcx, r12
    jge .et_len_done
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, 9                ; '\t'
    je .et_len_tab
    cmp al, 10               ; '\n'
    je .et_len_nl
    cmp al, 13               ; '\r'
    je .et_len_nl
    inc r14                  ; col++
    inc r8                   ; out_len++
    inc rcx
    jmp .et_len_loop
.et_len_tab:
    ; spaces = tabsize - (col % tabsize)
    test r13, r13
    jz .et_len_tab_zero
    mov rax, r14
    xor edx, edx
    div r13                  ; rdx = col % tabsize
    mov rax, r13
    sub rax, rdx             ; spaces
    add r8, rax
    add r14, rax
    inc rcx
    jmp .et_len_loop
.et_len_tab_zero:
    inc rcx
    jmp .et_len_loop
.et_len_nl:
    inc r8
    xor r14d, r14d           ; reset col
    inc rcx
    jmp .et_len_loop
.et_len_done:

    ; Allocate output buffer
    mov rdi, r8
    call ap_malloc
    mov [rbp - ET_BUF], rax
    mov r9, rax              ; r9 = output buffer

    ; Second pass: fill output
    mov r13, [rbp - ET_TAB]
    xor ecx, ecx            ; i (input)
    xor r14d, r14d           ; col
    xor r8d, r8d             ; j (output)
.et_fill_loop:
    cmp rcx, r12
    jge .et_fill_done
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, 9
    je .et_fill_tab
    cmp al, 10
    je .et_fill_nl
    cmp al, 13
    je .et_fill_nl
    mov [r9 + r8], al
    inc r14
    inc r8
    inc rcx
    jmp .et_fill_loop
.et_fill_tab:
    test r13, r13
    jz .et_fill_tab_skip
    mov rax, r14
    xor edx, edx
    div r13
    mov rax, r13
    sub rax, rdx             ; spaces
    ; Fill spaces
    mov r10, rax
.et_fill_spaces:
    test r10, r10
    jz .et_fill_tab_skip
    mov byte [r9 + r8], ' '
    inc r8
    inc r14
    dec r10
    jmp .et_fill_spaces
.et_fill_tab_skip:
    inc rcx
    jmp .et_fill_loop
.et_fill_nl:
    mov [r9 + r8], al
    inc r8
    xor r14d, r14d
    inc rcx
    jmp .et_fill_loop
.et_fill_done:
    ; Create str from buffer
    mov rdi, [rbp - ET_BUF]
    mov rsi, r8
    call str_new_heap
    mov [rbp - ET_RES], rax

    ; Free temp buffer
    mov rdi, [rbp - ET_BUF]
    call ap_free

    mov rax, [rbp - ET_RES]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_expandtabs

;; ============================================================================
;; str_method_splitlines(args, nargs) -> list of lines
;; args[0]=self, args[1]=keepends (optional bool, default False)
;; ============================================================================
SL_STEP  equ 8           ; how far past the break the next line starts
SL_FRAME equ 16             ; + 4 pushes = 48
DEF_FUNC str_method_splitlines, SL_FRAME
    push rbx
    push r12
    push r13
    push r14
    mov qword [rbp - SL_STEP], 1

    mov rbx, [rdi]           ; self
    mov r12, [rbx + PyStrObject.ob_size]

    ; Get keepends flag
    xor r14d, r14d           ; default: don't keep
    cmp rsi, 2
    jl .sl_have_keep
    ; Check args[1] - bool_true means keep
    lea rax, [rel bool_true]
    cmp qword [rdi + 8], rax
    sete r14b
.sl_have_keep:

    ; Create result list
    xor edi, edi
    call list_new
    mov r13, rax             ; result list

    ; Scan for line breaks
    xor ecx, ecx            ; i = start of current line
    xor r8d, r8d             ; j = scanner
.sl_loop:
    cmp r8, r12
    jge .sl_last

    movzx eax, byte [rbx + PyStrObject.data + r8]
    cmp al, 10               ; '\n'
    je .sl_found
    cmp al, 13               ; '\r'
    je .sl_found_cr
    inc r8
    jmp .sl_loop

.sl_found_cr:
    ; Check for \r\n
    lea rax, [r8 + 1]
    cmp rax, r12
    jge .sl_found            ; no more chars after \r
    movzx eax, byte [rbx + PyStrObject.data + rax]
    cmp al, 10
    jne .sl_found            ; not \r\n, just \r
    ; \r\n: end_pos = r8 + 2
    test r14d, r14d
    jz .sl_no_keep_crlf
    ; keepends: include \r\n.  The shared tail advances by one, so the \n
    ; was seen again as its own line break and produced a spurious entry.
    mov qword [rbp - SL_STEP], 2
    lea rdx, [r8 + 2]
    sub rdx, rcx
    jmp .sl_emit_line
.sl_no_keep_crlf:
    mov rdx, r8
    sub rdx, rcx
    push rcx
    push r8
    lea rdi, [rbx + PyStrObject.data + rcx]
    mov rsi, rdx
    call str_new_heap
    push rax
    mov rdi, r13
    mov rsi, rax
    V_PACK rsi, rdx         ; list_append takes a Value
    call list_append
    pop rdi
    call obj_decref
    pop r8
    pop rcx
    lea rcx, [r8 + 2]        ; skip \r\n
    lea r8, [r8 + 2]
    jmp .sl_loop

.sl_found:
    ; Line break at r8
    test r14d, r14d
    jz .sl_no_keep
    ; keepends: include the newline char
    lea rdx, [r8 + 1]
    sub rdx, rcx
    jmp .sl_emit_line
.sl_no_keep:
    mov rdx, r8
    sub rdx, rcx
.sl_emit_line:
    push rcx
    push r8
    lea rdi, [rbx + PyStrObject.data + rcx]
    mov rsi, rdx
    call str_new_heap
    push rax
    mov rdi, r13
    mov rsi, rax
    V_PACK rsi, rdx         ; list_append takes a Value
    call list_append
    pop rdi
    call obj_decref
    pop r8
    pop rcx
    add r8, [rbp - SL_STEP]
    mov rcx, r8
    mov qword [rbp - SL_STEP], 1
    jmp .sl_loop

.sl_last:
    ; Remaining text after last newline
    cmp rcx, r12
    jge .sl_done
    mov rdx, r12
    sub rdx, rcx
    lea rdi, [rbx + PyStrObject.data + rcx]
    mov rsi, rdx
    call str_new_heap
    push rax
    mov rdi, r13
    mov rsi, rax
    V_PACK rsi, rdx         ; list_append takes a Value
    call list_append
    pop rdi
    call obj_decref

.sl_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_splitlines

;; ============================================================================
;; str_method_translate(args, nargs) -> a new string
;; args[0] = self, args[1] = the table
;;
;; The table is anything subscriptable, not only a dict.  This used to hand
;; whatever it was given straight to dict_get, with no type check: a list read
;; its fields as a dict's and the interpreter segfaulted on
;; `"abc".translate([None] * 200)`.  That is not a contrived call --
;; re.escape() is `pattern.translate(_special_chars_map)`, and fnmatch builds
;; every pattern through it.
;;
;; CPython's rule, which this now follows: look the ordinal up; a LookupError
;; means leave the character alone -- that is what a short list and a str
;; table both give -- None deletes it, an int is an ordinal and a str is
;; substituted whole.  A table with no subscript at all is a TypeError.
;;
;; A dict still goes through dict_get, which is the common case and cannot
;; raise; everything else goes through mp_subscript.
;; trn_decode_cp(rdi = bytes, rsi = how many are left) -> rax = code point,
;;   rdx = its length in bytes
;; A malformed byte is passed through as itself, one byte wide: this is
;; decoding a str that is already valid, so the fallback is only a guard.
DEF_FUNC_BARE trn_decode_cp
    movzx eax, byte [rdi]
    mov edx, 1
    cmp al, 0x80
    jb .tdc_done
    cmp al, 0xc2
    jb .tdc_done
    cmp al, 0xe0
    jb .tdc_two
    cmp al, 0xf0
    jb .tdc_three
    cmp al, 0xf5
    jb .tdc_four
    jmp .tdc_done
.tdc_two:
    cmp rsi, 2
    jl .tdc_done
    and eax, 0x1f
    shl eax, 6
    movzx ecx, byte [rdi + 1]
    and ecx, 0x3f
    or eax, ecx
    mov edx, 2
    ret
.tdc_three:
    cmp rsi, 3
    jl .tdc_done
    and eax, 0x0f
    shl eax, 12
    movzx ecx, byte [rdi + 1]
    and ecx, 0x3f
    shl ecx, 6
    or eax, ecx
    movzx ecx, byte [rdi + 2]
    and ecx, 0x3f
    or eax, ecx
    mov edx, 3
    ret
.tdc_four:
    cmp rsi, 4
    jl .tdc_done
    and eax, 0x07
    shl eax, 18
    movzx ecx, byte [rdi + 1]
    and ecx, 0x3f
    shl ecx, 12
    or eax, ecx
    movzx ecx, byte [rdi + 2]
    and ecx, 0x3f
    shl ecx, 6
    or eax, ecx
    movzx ecx, byte [rdi + 3]
    and ecx, 0x3f
    or eax, ecx
    mov edx, 4
    ret
.tdc_done:
    ret
END_FUNC trn_decode_cp

;; trn_encode_cp(rdi = code point, rsi = a buffer of at least 5 bytes)
;;   -> rax = how many bytes it wrote, NUL-terminated
DEF_FUNC_BARE trn_encode_cp
    cmp rdi, 0x80
    jb .tec_one
    cmp rdi, 0x800
    jb .tec_two
    cmp rdi, 0x10000
    jb .tec_three
    mov rax, rdi
    shr rax, 18
    or al, 0xf0
    mov [rsi], al
    mov rax, rdi
    shr rax, 12
    and al, 0x3f
    or al, 0x80
    mov [rsi + 1], al
    mov rax, rdi
    shr rax, 6
    and al, 0x3f
    or al, 0x80
    mov [rsi + 2], al
    mov rax, rdi
    and al, 0x3f
    or al, 0x80
    mov [rsi + 3], al
    mov byte [rsi + 4], 0
    mov eax, 4
    ret
.tec_one:
    mov rax, rdi
    mov [rsi], al
    mov byte [rsi + 1], 0
    mov eax, 1
    ret
.tec_two:
    mov rax, rdi
    shr rax, 6
    or al, 0xc0
    mov [rsi], al
    mov rax, rdi
    and al, 0x3f
    or al, 0x80
    mov [rsi + 1], al
    mov byte [rsi + 2], 0
    mov eax, 2
    ret
.tec_three:
    mov rax, rdi
    shr rax, 12
    or al, 0xe0
    mov [rsi], al
    mov rax, rdi
    shr rax, 6
    and al, 0x3f
    or al, 0x80
    mov [rsi + 1], al
    mov rax, rdi
    and al, 0x3f
    or al, 0x80
    mov [rsi + 2], al
    mov byte [rsi + 3], 0
    mov eax, 3
    ret
END_FUNC trn_encode_cp

;; ============================================================================
TRN_SELF  equ 8
TRN_TAB   equ 16
TRN_LIST  equ 24            ; the pieces, joined at the end
TRN_SUB   equ 32            ; the table's mp_subscript, or 0 when it is a dict
TRN_I     equ 40            ; the index into self
TRN_N     equ 48            ; self's length in bytes
TRN_EXC   equ 56            ; current_exception, to tell a raise from a miss
TRN_CH    equ 72            ; one character, built on the stack: up to four
                            ; UTF-8 bytes and a NUL
TRN_LEN   equ 80            ; a bounded table's length, or -1
TRN_CHLEN equ 88            ; how many bytes the current character occupies
TRN_HEAP  equ 96            ; 1 when the table is a user class: ask __getitem__
                            ; through dunder_call_2, which comes back
TRN_FRAME equ 104           ; + 2 pushes = 120, 16-aligned

extern bytearray_type
extern bytes_type
extern list_type
extern tuple_type
extern type_is_subtype
extern dict_type
extern current_exception
extern dunder_call_2
extern obj_dealloc
extern raise_type_error_with_name

DEF_FUNC str_method_translate, TRN_FRAME
    push rbx
    push r12

    cmp rsi, 2
    jl .trn_argerr
    mov rax, [rdi]
    mov [rbp - TRN_SELF], rax
    mov rcx, [rax + PyStrObject.ob_size]
    mov [rbp - TRN_N], rcx
    mov rax, [rdi + 8]
    mov [rbp - TRN_TAB], rax

    ; Resolve how to look a key up, once, before the loop.
    ;
    ; A raise from inside a subscript cannot be caught here: raise_exception
    ; tail-jumps into eval_exception_unwind, which resumes the eval loop from
    ; saved globals rather than returning through the C stack, so a `call` to
    ; a slot that raises never comes back.  That rules out CPython's "try the
    ; lookup and treat LookupError as a miss".
    ;
    ; So the bound is checked BEFORE the call instead, which needs no catching
    ; and gives the same answer for every table that has one: a list, tuple,
    ; str, bytes or bytearray shorter than the ordinal simply leaves the
    ; character alone.  A dict has a length too, but it is an entry count and
    ; not an index bound, so a dict keeps its own path -- dict_get reports a
    ; miss with a NULL and never raises.
    mov qword [rbp - TRN_SUB], 0
    mov qword [rbp - TRN_LEN], -1   ; -1 = no bound to check
    mov qword [rbp - TRN_HEAP], 0
    V_TEST_PTR rax, rcx
    ja .trn_not_subscriptable       ; an immediate has no subscript
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel dict_type]
    cmp rcx, rdx
    je .trn_have_lookup             ; a dict: TRN_SUB stays 0
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_DICT_SUBCLASS
    jz .trn_not_dict
    ; A subclass takes the dict fast path only if it has not overridden
    ; __getitem__.  Taking it on the flag alone meant dict_get answered from
    ; the entries and the subclass's own __getitem__ was never called --
    ; CPython asks the object, and a LookupError from it is the miss.
    mov rdx, [rcx + PyTypeObject.tp_as_mapping]
    test rdx, rdx
    jz .trn_have_lookup
    mov rdx, [rdx + PyMappingMethods.mp_subscript]
    lea r8, [rel dict_type]
    mov r8, [r8 + PyTypeObject.tp_as_mapping]
    test r8, r8
    jz .trn_have_lookup
    cmp rdx, [r8 + PyMappingMethods.mp_subscript]
    je .trn_have_lookup             ; dict's own: the fast path is right
.trn_not_dict:

    mov rdx, [rcx + PyTypeObject.tp_as_mapping]
    test rdx, rdx
    jz .trn_not_subscriptable
    mov rdx, [rdx + PyMappingMethods.mp_subscript]
    test rdx, rdx
    jz .trn_not_subscriptable
    mov [rbp - TRN_SUB], rdx
    ; A user class's mp_subscript is a slot wrapper, and a slot wrapper that
    ; raises does not come back -- it jumps to the unwinder.  CPython's rule
    ; is that a LookupError from the table means "not in it", which needs a
    ; call that returns.  dunder_call_2 is that call, so a heaptype table is
    ; asked through __getitem__ by name instead.
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .trn_sub_ready
    mov qword [rbp - TRN_HEAP], 1
.trn_sub_ready:

    ; A builtin sequence: take its length as the bound.  Only these, by name:
    ; a user class with both __len__ and __getitem__ gets an sq_length too,
    ; and for a mapping its length is not an index bound either.
    lea rdx, [rel list_type]
    cmp rcx, rdx
    je .trn_bounded
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    je .trn_bounded
    lea rdx, [rel str_type]
    cmp rcx, rdx
    je .trn_bounded
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    je .trn_bounded
    lea rdx, [rel bytearray_type]
    cmp rcx, rdx
    je .trn_bounded
    test qword [rcx + PyTypeObject.tp_flags], \
              TYPE_FLAG_LIST_SUBCLASS | TYPE_FLAG_TUPLE_SUBCLASS | \
              TYPE_FLAG_STR_SUBCLASS
    jz .trn_have_lookup
.trn_bounded:
    mov rdx, [rcx + PyTypeObject.tp_as_sequence]
    test rdx, rdx
    jz .trn_have_lookup
    mov rdx, [rdx + PySequenceMethods.sq_length]
    test rdx, rdx
    jz .trn_have_lookup
    mov rdi, rax
    call rdx
    mov [rbp - TRN_LEN], rax

.trn_have_lookup:
    xor edi, edi
    call list_new
    test rax, rax
    jz .trn_fail
    mov [rbp - TRN_LIST], rax
    mov qword [rbp - TRN_I], 0
    DUNDER_EXC_SAVE [rbp - TRN_EXC]

.trn_loop:
    mov rcx, [rbp - TRN_I]
    cmp rcx, [rbp - TRN_N]
    jge .trn_join
    ; A CODE POINT, not a byte.  Reading one byte took the lead byte of a
    ; UTF-8 sequence as the ordinal, so "é".translate({233: "X"}) mapped
    ; nothing and translating by the lead byte instead produced a string that
    ; was not valid UTF-8 at all.
    mov rax, [rbp - TRN_SELF]
    lea rdi, [rax + PyStrObject.data]
    add rdi, rcx
    mov rsi, [rbp - TRN_N]
    sub rsi, rcx
    call trn_decode_cp          ; rax = the code point, rdx = its byte count
    mov rbx, rax
    mov [rbp - TRN_CHLEN], rdx

    ; The key is the ordinal, as an int Value.
    mov rdi, rbx
    V_PACK_I64 rdi, rcx
    mov rsi, rdi
    mov rdi, [rbp - TRN_TAB]
    mov rcx, [rbp - TRN_SUB]
    test rcx, rcx
    jnz .trn_call_sub
    call dict_get                   ; a miss is a NULL Value, never a raise
    test rax, rax
    jz .trn_keep
    ; dict_get hands back a BORROWED reference where mp_subscript hands back
    ; an owned one.  Taking one here makes the two paths the same below --
    ; without it the release after list_append frees the table's own value,
    ; and the next lookup of it reads freed memory.  The version before this
    ; one had the same bug and only a dict path to hit it with.
    INCREF_V rax, rcx
    jmp .trn_have_value

.trn_call_sub:
    ; Past the end of a bounded table: a miss, without asking.
    mov rdx, [rbp - TRN_LEN]
    cmp rdx, 0
    jl .trn_do_sub
    cmp rbx, rdx
    jge .trn_keep
.trn_do_sub:
    cmp qword [rbp - TRN_HEAP], 0
    jne .trn_do_dunder
    call rcx
    test rax, rax
    jz .trn_sub_null
    jmp .trn_have_value

.trn_do_dunder:
    ; rdi = table, rsi = the ordinal as a Value.  dunder_call_2 wants the
    ; argument split, and answers (0, TAG_NULL) for a miss or a raise.
    mov rdi, [rbp - TRN_TAB]
    mov rsi, rbx
    V_PACK_I64 rsi, rcx
    V_UNPACK rsi, rcx
    extern dunder_getitem
    lea rdx, [rel dunder_getitem]
    call dunder_call_2
    V_PACK rax, rdx
    test rax, rax
    jz .trn_sub_null
    jmp .trn_have_value

.trn_sub_null:
    ; NULL is a miss, or a raise.  The snapshot at the top of the loop was
    ; being taken and never compared, so every raise looked like a miss and
    ; the exception was left to surface somewhere unrelated.  CPython reads a
    ; LookupError as "not in the table" and leaves the character alone;
    ; anything else propagates.
    EXC_RAISED_SINCE [rbp - TRN_EXC], rcx, .trn_sub_raised
    jmp .trn_keep

.trn_sub_raised:
    extern exc_LookupError_type
    extern exc_isinstance
    mov rdi, [rel current_exception]
    lea rsi, [rel exc_LookupError_type]
    call exc_isinstance
    test eax, eax
    jz .trn_fail                    ; not a lookup miss: let it out
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    call obj_decref
    jmp .trn_keep

.trn_have_value:
    ; rax = the mapped value, owned.  None deletes, an int is an ordinal, a
    ; str is substituted whole.
    mov r12, rax
    IS_NONE rax, rcx
    je .trn_drop
    ; int_is_integer, not V_IS_INT: the mapped value may be a heap int, a bool
    ; or an int subclass, and an immediate-only test called all three "not an
    ; integer".  Under INT_STRESS=1 every ordinal above 7 is one of them.
    mov rdi, rax
    V_UNPACK rdi, rdx
    call int_is_integer
    test eax, eax
    jnz .trn_ordinal
    mov rax, r12
    V_TEST_PTR rax, rcx
    ja .trn_bad_value
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    je .trn_append_r12
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_STR_SUBCLASS
    jz .trn_bad_value

.trn_append_r12:
    mov rdi, [rbp - TRN_LIST]
    mov rsi, r12
    call list_append
    mov rdi, r12
    DECREF_V rdi, rcx
    jmp .trn_next

.trn_ordinal:
    ; An ordinal: render it as one character, in UTF-8.  It used to be
    ; refused above 0x7f, which CPython allows.
    mov rdi, r12
    V_UNPACK rdi, rdx
    call obj_as_index           ; not V_TO_I64: it may be a heap int
    cmp rax, 0
    jl .trn_range
    cmp rax, 0x10ffff
    jg .trn_range
    mov rdi, rax
    lea rsi, [rbp - TRN_CH]
    call trn_encode_cp          ; rax = how many bytes it wrote
    lea rdi, [rbp - TRN_CH]
    mov rsi, rax
    call str_new_heap
    mov rbx, rax
    mov rdi, [rbp - TRN_LIST]
    mov rsi, rax
    call list_append
    mov rdi, rbx
    call obj_decref
    mov rdi, r12
    DECREF_V rdi, rcx
    jmp .trn_next

.trn_drop:
    mov rdi, r12
    DECREF_V rdi, rcx
    jmp .trn_next

.trn_keep:
    ; Not in the table: the original character survives, all of its bytes.
    mov rax, [rbp - TRN_SELF]
    lea rdi, [rax + PyStrObject.data]
    add rdi, [rbp - TRN_I]
    mov rsi, [rbp - TRN_CHLEN]
    call str_new_heap
    mov rbx, rax
    mov rdi, [rbp - TRN_LIST]
    mov rsi, rax
    call list_append
    mov rdi, rbx
    call obj_decref

.trn_next:
    mov rax, [rbp - TRN_CHLEN]
    add [rbp - TRN_I], rax      ; past the whole character, not one byte
    jmp .trn_loop

.trn_join:
    CSTRING rdi, ""
    xor esi, esi
    call str_new_heap
    mov rbx, rax
    sub rsp, 16
    mov [rsp], rbx
    mov rax, [rbp - TRN_LIST]
    mov [rsp + 8], rax
    mov rdi, rsp
    mov esi, 2
    call str_method_join
    add rsp, 16
    mov r12, rax
    mov rdi, rbx
    call obj_decref
    mov rdi, [rbp - TRN_LIST]
    call obj_decref
    mov rax, r12
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.trn_range:
    ; The mapped value and the list of pieces are both ours, and RAISE
    ; abandons the C stack -- .trn_bad_value two lines down releases them and
    ; this one did not.
    mov rdi, r12
    DECREF_V rdi, rcx
    mov rdi, [rbp - TRN_LIST]
    call obj_decref
    RAISE exc_ValueError_type, "character mapping must be in range(0x110000)"
.trn_bad_value:
    mov rdi, r12
    DECREF_V rdi, rcx
    mov rdi, [rbp - TRN_LIST]
    call obj_decref
    RAISE exc_TypeError_type, "character mapping must return integer, None or str"

.trn_fail:
    mov rdi, [rbp - TRN_LIST]
    test rdi, rdi
    jz .trn_fail_out
    call obj_decref
.trn_fail_out:
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret

.trn_not_subscriptable:
    mov rsi, [rbp - TRN_TAB]
    CSTRING rdi, `'\x01' object is not subscriptable`
    call raise_type_error_with_name

.trn_argerr:
    RAISE exc_TypeError_type, "translate() takes exactly one argument"
END_FUNC str_method_translate

;; ============================================================================
;; str_staticmethod_maketrans(args, nargs) -> dict
;; 2-arg form: maketrans(x, y) where x and y are strings of equal length
;; Returns dict mapping ord(x[i]) -> ord(y[i])
;; Note: called as staticmethod, so no 'self' arg.
;; ============================================================================
SMT_FROM  equ 8
SMT_TO    equ 16
SMT_NARGS equ 24
SMT_ARGS  equ 32
SMT_KEY   equ 40            ; the code point being mapped
SMT_TOPOS equ 48            ; the TO cursor, which advances at its own rate
SMT_FRAME equ 56            ; + 3 pushes = 80, 16-byte aligned

DEF_FUNC str_staticmethod_maketrans, SMT_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 2
    jl .smt_error
    cmp rsi, 3
    jg .smt_error
    mov [rbp - SMT_NARGS], rsi
    mov [rbp - SMT_ARGS], rdi

    ; Get from and to strings
    mov rcx, [rdi]                 ; args[0] payload (from str)
    mov [rbp - SMT_FROM], rcx

    mov rcx, [rdi + 8]            ; args[1] payload (to str)
    mov [rbp - SMT_TO], rcx

    ; Equal lengths in CODE POINTS, and a table keyed by code point.  Both
    ; used ob_size, which is a byte count: "áâ" is four bytes and two
    ; characters, so str.maketrans("ab", "áâ") reported unequal lengths, and
    ; a pair that did match in bytes built a table keyed on UTF-8 fragments
    ; that the code-point-based translate could never look up.
    mov rax, [rbp - SMT_FROM]
    mov rcx, [rbp - SMT_TO]
    mov r12, [rax + PyStrObject.ob_length]
    cmp r12, [rcx + PyStrObject.ob_length]
    jne .smt_len_error

    ; Create result dict
    call dict_new
    mov rbx, rax                    ; result dict

    ; For each character, map ord(from[i]) -> ord(to[i])
    xor r13d, r13d                  ; the FROM byte cursor
    mov qword [rbp - SMT_TOPOS], 0  ; and the TO one, which advances at its
                                    ; own rate: the two strings need not use
                                    ; the same number of bytes per character
.smt_loop:
    mov rax, [rbp - SMT_FROM]
    cmp r13, [rax + PyStrObject.ob_size]
    jge .smt_done

    lea rdi, [rax + PyStrObject.data]
    add rdi, r13
    mov rsi, [rax + PyStrObject.ob_size]
    sub rsi, r13
    call trn_decode_cp              ; rax = code point, rdx = its byte count
    mov [rbp - SMT_KEY], rax
    add r13, rdx

    mov rax, [rbp - SMT_TO]
    lea rdi, [rax + PyStrObject.data]
    add rdi, [rbp - SMT_TOPOS]
    mov rsi, [rax + PyStrObject.ob_size]
    sub rsi, [rbp - SMT_TOPOS]
    call trn_decode_cp
    add [rbp - SMT_TOPOS], rdx

    mov rdx, rax                    ; value = the TO ordinal
    mov rsi, [rbp - SMT_KEY]        ; key   = the FROM ordinal
    V_PACK_I64 rdx, rcx             ; dict_set takes Values
    V_PACK_I64 rsi, r8
    mov rdi, rbx
    call dict_set
    jmp .smt_loop

.smt_done:
    ; The third argument, when there is one: every character in it maps to
    ; None, which str.translate reads as "delete".  os.path and shlex both
    ; build their tables this way.
    cmp qword [rbp - SMT_NARGS], 3
    jne .smt_finish
    mov rax, [rbp - SMT_ARGS]
    mov rax, [rax + 16]
    mov [rbp - SMT_TO], rax
    xor r13d, r13d
.smt_del_loop:
    mov rcx, [rbp - SMT_TO]
    cmp r13, [rcx + PyStrObject.ob_size]
    jge .smt_finish
    lea rdi, [rcx + PyStrObject.data]
    add rdi, r13
    mov rsi, [rcx + PyStrObject.ob_size]
    sub rsi, r13
    call trn_decode_cp              ; by code point here too
    add r13, rdx
    mov rsi, rax
    V_PACK_I64 rsi, r8
    mov rdi, rbx
    LOAD_NONE rdx
    call dict_set
    jmp .smt_del_loop

.smt_finish:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smt_error:
    RAISE exc_TypeError_type, "maketrans requires 2 or 3 string arguments"

.smt_len_error:
    RAISE exc_ValueError_type, "maketrans arguments must have equal length"
END_FUNC str_staticmethod_maketrans

;; args[0]=self, args[1]=prefix
;; If self starts with prefix, return self[len(prefix):], else return self.
;; ============================================================================
DEF_FUNC str_method_removeprefix
    push rbx
    push r12
    push r13
    push r14

    ; Validate args[1] is a string
    mov rax, [rdi + 8]         ; args[1]
    V_TEST_PTR rax, rcx
    ja .rp_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .rp_type_error

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]     ; prefix
    mov r13, [rbx + PyStrObject.ob_size]   ; self len
    mov r14, [r12 + PyStrObject.ob_size]   ; prefix len

    ; If prefix longer than self, return self (INCREF)
    cmp r14, r13
    jg .rmpfx_return_self

    ; Compare first prefix_len bytes
    xor ecx, ecx
.rmpfx_cmp:
    cmp rcx, r14
    jge .rmpfx_match
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, [r12 + PyStrObject.data + rcx]
    jne .rmpfx_return_self
    inc rcx
    jmp .rmpfx_cmp

.rmpfx_match:
    ; Prefix matches - return str_new(data+preflen, len-preflen)
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r14
    mov rsi, r13
    sub rsi, r14
    call str_new_heap
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rmpfx_return_self:
    mov rax, rbx
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rp_type_error:
    RAISE exc_TypeError_type, "must be str, not other type"
END_FUNC str_method_removeprefix

;; ============================================================================
;; str_method_removesuffix(args, nargs) -> new string
;; args[0]=self, args[1]=suffix
;; If self ends with suffix, return self[:len(self)-len(suffix)], else return self.
;; ============================================================================
DEF_FUNC str_method_removesuffix
    push rbx
    push r12
    push r13
    push r14

    ; Validate args[1] is a string
    mov rax, [rdi + 8]         ; args[1]
    V_TEST_PTR rax, rcx
    ja .rs_type_error
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_STR_TYPE rcx, rdx, .rs_type_error

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]     ; suffix
    mov r13, [rbx + PyStrObject.ob_size]   ; self len
    mov r14, [r12 + PyStrObject.ob_size]   ; suffix len

    ; If suffix longer than self, return self (INCREF)
    cmp r14, r13
    jg .rmsfx_return_self

    ; If suffix is empty, return self (INCREF)
    test r14, r14
    jz .rmsfx_return_self

    ; Compare last suffix_len bytes of self with suffix
    mov rcx, r13
    sub rcx, r14            ; offset = self_len - suffix_len
    xor edx, edx
.rmsfx_cmp:
    cmp rdx, r14
    jge .rmsfx_match
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, [r12 + PyStrObject.data + rdx]
    jne .rmsfx_return_self
    inc rcx
    inc rdx
    jmp .rmsfx_cmp

.rmsfx_match:
    ; Suffix matches - return str_new(data, len-suffixlen)
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r13
    sub rsi, r14
    call str_new_heap
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rmsfx_return_self:
    mov rax, rbx
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rs_type_error:
    RAISE exc_TypeError_type, "must be str, not other type"
END_FUNC str_method_removesuffix

;; ============================================================================
;; str_method_encode(args, nargs) -> bytes
;; args[0]=self, args[1]=encoding (optional, default 'utf-8')
;; For now, supports 'utf-8' and 'ascii' — both just copy raw bytes.
;; ============================================================================
SE_SELF  equ 8
SE_LEN   equ 16
SE_OUT   equ 24
SE_POS   equ 32
SE_ERRS  equ 40
SE_EID   equ 48             ; 0 strict, 1 ignore, 2 replace
SE_ARGS  equ 56
SE_NARGS equ 64
SE_FRAME equ 80             ; + 2 pushes = 96
DEF_FUNC str_method_encode, SE_FRAME
    push rbx
    push r12
    ; args[0] = self, args[1] = encoding, args[2] = errors
    mov [rbp - SE_ARGS], rdi
    mov [rbp - SE_NARGS], rsi
    mov rbx, [rdi]
    mov [rbp - SE_SELF], rbx
    mov r12, [rbx + PyStrObject.ob_size]
    mov [rbp - SE_LEN], r12
    mov qword [rbp - SE_ERRS], 0

    ; encode([encoding[, errors]]).  An encoding that is not a str is a
    ; TypeError in CPython, not a silent fall back to utf-8.
    cmp rsi, 3
    jg .se_too_many
    xor eax, eax
    cmp rsi, 2
    jl .se_have_enc
    mov rax, [rdi + 8]
    extern none_singleton
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .se_default_enc
    V_TEST_PTR rax, rcx
    ja .se_bad_enc
    test rax, rax
    jz .se_bad_enc
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .se_bad_enc
    jmp .se_have_enc
.se_default_enc:
    xor eax, eax
.se_have_enc:
    ; args[2] = errors.  It was never read at all: SE_ERRS stayed 0, so every
    ; failure was strict whatever was asked for.
    cmp qword [rbp - SE_NARGS], 3
    jl .se_no_errors
    mov rcx, [rbp - SE_ARGS]
    mov rcx, [rcx + 16]
    mov [rbp - SE_ERRS], rcx
.se_no_errors:
    mov rdi, rax
    extern codec_id
    call codec_id
    cmp eax, 1
    je .se_ascii
    cmp eax, 2
    je .se_latin1

.se_utf8:
    ; The bytes are already UTF-8.
    mov rdi, r12
    extern bytes_new
    call bytes_new
    push rax
    lea rdi, [rax + PyBytesObject.data]
    mov rbx, [rbp - SE_SELF]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    extern ap_memcpy
    call ap_memcpy
    pop rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.se_ascii:
    ; Every byte of a valid ASCII string is below 0x80, and a multi-byte
    ; character is exactly the case that is not encodable.  The errors=
    ; argument was parked in SE_ERRS and never looked up, so "ignore" and
    ; "replace" both raised and an unknown handler name was never reported as
    ; a LookupError either.
    mov rbx, [rbp - SE_SELF]
    xor ecx, ecx
.se_ascii_scan:
    cmp rcx, r12
    jge .se_utf8
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    test al, 0x80
    jnz .se_ascii_needs_handler
    inc rcx
    jmp .se_ascii_scan

.se_ascii_needs_handler:
    ; The handler is looked up only once something actually fails, which is
    ; also when CPython reports an unknown name: "ab".encode("ascii", "bogus")
    ; succeeds there.
    mov rdi, [rbp - SE_ERRS]
    extern codec_error_id
    call codec_error_id         ; 0 strict, 1 ignore, 2 replace, -1 unknown
    cmp eax, -1
    je .se_bad_errors
    mov [rbp - SE_EID], rax
    test eax, eax
    jz .se_not_encodable

.se_ascii_handled:
    ; One byte out per code point at most, so the code point count is an upper
    ; bound on the result.
    mov rbx, [rbp - SE_SELF]
    mov rdi, [rbx + PyStrObject.ob_length]
    call bytes_new
    mov [rbp - SE_OUT], rax
    mov qword [rbp - SE_POS], 0
    xor ecx, ecx
.se_ah_loop:
    cmp rcx, [rbp - SE_LEN]
    jge .se_ah_done
    mov rbx, [rbp - SE_SELF]
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    test al, 0x80
    jz .se_ah_emit

    ; A character that does not fit: skip its whole UTF-8 sequence, then
    ; either drop it or write a '?', which is what CPython's replace does on
    ; the encode side.
    inc rcx
.se_ah_skip:
    cmp rcx, [rbp - SE_LEN]
    jge .se_ah_after
    movzx edx, byte [rbx + PyStrObject.data + rcx]
    and edx, 0xc0
    cmp edx, 0x80
    jne .se_ah_after
    inc rcx
    jmp .se_ah_skip
.se_ah_after:
    cmp qword [rbp - SE_EID], 2
    jne .se_ah_loop             ; ignore
    mov rdx, [rbp - SE_OUT]
    mov r8, [rbp - SE_POS]
    mov byte [rdx + PyBytesObject.data + r8], '?'
    inc qword [rbp - SE_POS]
    jmp .se_ah_loop

.se_ah_emit:
    mov rdx, [rbp - SE_OUT]
    mov r8, [rbp - SE_POS]
    mov [rdx + PyBytesObject.data + r8], al
    inc qword [rbp - SE_POS]
    inc rcx
    jmp .se_ah_loop

.se_ah_done:
    mov rax, [rbp - SE_OUT]
    mov rcx, [rbp - SE_POS]
    mov [rax + PyBytesObject.ob_size], rcx
    mov byte [rax + PyBytesObject.data + rcx], 0
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.se_bad_errors:
    extern exc_LookupError_type
    RAISE exc_LookupError_type, "unknown error handler name"

.se_latin1:
    ; One byte per code point, for the code points that fit in one.
    mov rdi, [rbx + PyStrObject.ob_length]
    call bytes_new
    mov [rbp - SE_OUT], rax
    mov qword [rbp - SE_POS], 0
    xor ecx, ecx                    ; byte cursor into the source
.se_l1_loop:
    cmp rcx, [rbp - SE_LEN]
    jge .se_l1_done
    mov rbx, [rbp - SE_SELF]
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    test al, 0x80
    jz .se_l1_emit
    ; A two-byte form can reach U+00FF; anything wider cannot be Latin-1.
    mov edx, eax
    and edx, 0xe0
    cmp edx, 0xc0
    jne .se_not_encodable
    and eax, 0x1f
    cmp eax, 3
    ja .se_not_encodable
    shl eax, 6
    inc rcx
    movzx edx, byte [rbx + PyStrObject.data + rcx]
    and edx, 0x3f
    or eax, edx
.se_l1_emit:
    mov rdx, [rbp - SE_OUT]
    mov r8, [rbp - SE_POS]
    mov [rdx + PyBytesObject.data + r8], al
    inc qword [rbp - SE_POS]
    inc rcx
    jmp .se_l1_loop
.se_l1_done:
    mov rax, [rbp - SE_OUT]
    mov rcx, [rbp - SE_POS]
    mov [rax + PyBytesObject.ob_size], rcx
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.se_bad_enc:
    extern raise_type_error_with_name
    CSTRING rdi, `encode() argument 'encoding' must be str, not \x01`
    mov rsi, rax
    call raise_type_error_with_name
.se_too_many:
    RAISE exc_TypeError_type, "encode() takes at most 2 arguments"

.se_not_encodable:
    extern exc_UnicodeEncodeError_type
    RAISE exc_UnicodeEncodeError_type, "character not in range for this encoding"
END_FUNC str_method_encode
