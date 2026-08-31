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
;; str_method_translate(args, nargs) -> new string
;; args[0]=self, args[1]=table (dict mapping ordinals to ordinals/strings/None)
;; ============================================================================
DEF_FUNC str_method_translate
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]           ; self
    mov r12, [rbx + PyStrObject.ob_size]
    mov r14, [rdi + 8]      ; table (dict)

    ; Build result: for each char, look up ord(char) in table
    xor edi, edi
    call list_new
    mov r13, rax             ; result list (of chars/strings)

    xor ecx, ecx
.tr_loop:
    cmp rcx, r12
    jge .tr_join

    ; Get ordinal of current char
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    push rcx

    ; Look up in table: dict_get(table, ord_key)
    ; Create SmallInt key
    movzx edi, al
    call int_from_i64
    ; rax = SmallInt payload, edx = TAG_SMALLINT
    push rax
    push rdx
    mov rdi, r14
    mov rsi, rax
    mov edx, edx
    V_PACK rsi, rdx           ; dict_get/del take a key Value
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop r8                   ; original key tag
    pop r9                   ; original key payload
    test edx, edx
    jz .tr_not_found

    ; Found: check what the value is
    ; If None: skip char (delete)
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .tr_delete

    ; If SmallInt: character ordinal
    cmp edx, TAG_SMALLINT
    je .tr_ord

    ; Else: it's a string, append it
    push rax
    mov rdi, r13
    mov rsi, rax
    V_PACK rsi, rdx         ; list_append takes a Value
    call list_append
    pop rdi
    call obj_decref
    pop rcx
    inc rcx
    jmp .tr_loop

.tr_ord:
    ; Convert ordinal to 1-char string
    push rax
    sub rsp, 8
    mov [rsp], al
    mov byte [rsp + 1], 0
    mov rdi, rsp
    mov rsi, 1
    call str_new_heap
    add rsp, 8
    push rax
    mov rdi, r13
    mov rsi, rax
    V_PACK rsi, rdx         ; list_append takes a Value
    call list_append
    pop rdi
    call obj_decref
    pop rax                  ; discard saved ordinal
    pop rcx
    inc rcx
    jmp .tr_loop

.tr_not_found:
    ; Not in table: keep original char
    movzx eax, byte [rbx + PyStrObject.data + rcx]  ; rcx is on stack
    ; Wait, rcx was pushed. Let me get it from stack.
    mov rcx, [rsp]           ; peek at saved rcx
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    sub rsp, 8
    mov [rsp], al
    mov byte [rsp + 1], 0
    mov rdi, rsp
    mov rsi, 1
    call str_new_heap
    add rsp, 8
    push rax
    mov rdi, r13
    mov rsi, rax
    V_PACK rsi, rdx         ; list_append takes a Value
    call list_append
    pop rdi
    call obj_decref
    pop rcx
    inc rcx
    jmp .tr_loop

.tr_delete:
    ; Skip this character (mapped to None)
    pop rcx
    inc rcx
    jmp .tr_loop

.tr_join:
    ; Join all pieces: "".join(result_list)
    CSTRING rdi, ""
    xor esi, esi
    call str_new_heap
    push rax                 ; empty sep

    ; Build args for join: [sep, list]
    sub rsp, 16
    mov rax, [rsp + 16]     ; sep
    mov [rsp], rax
    mov [rsp + 8], r13
    mov rdi, rsp
    mov rsi, 2
    call str_method_join
    V_UNPACK rax, rdx           ; str_method_join returns a Value
    add rsp, 16
    push rax
    push rdx

    ; Cleanup: DECREF sep and list
    mov rdi, [rsp + 16]     ; sep
    call obj_decref
    mov rdi, r13
    call obj_decref

    pop rdx
    pop rax
    add rsp, 8              ; sep ptr

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC str_method_translate

;; ============================================================================
;; str_staticmethod_maketrans(args, nargs) -> dict
;; 2-arg form: maketrans(x, y) where x and y are strings of equal length
;; Returns dict mapping ord(x[i]) -> ord(y[i])
;; Note: called as staticmethod, so no 'self' arg.
;; ============================================================================
SMT_FROM  equ 8
SMT_TO    equ 16
SMT_FRAME equ 24            ; + 3 pushes = 48

DEF_FUNC str_staticmethod_maketrans, SMT_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 2
    jne .smt_error

    ; Get from and to strings
    mov rcx, [rdi]                 ; args[0] payload (from str)
    mov [rbp - SMT_FROM], rcx

    mov rcx, [rdi + 8]            ; args[1] payload (to str)
    mov [rbp - SMT_TO], rcx

    ; Check equal lengths
    mov rax, [rbp - SMT_FROM]
    mov rcx, [rbp - SMT_TO]
    mov r12, [rax + PyStrObject.ob_size]
    cmp r12, [rcx + PyStrObject.ob_size]
    jne .smt_len_error

    ; Create result dict
    call dict_new
    mov rbx, rax                    ; result dict

    ; For each character position, map ord(from[i]) -> ord(to[i])
    xor r13d, r13d                  ; index
.smt_loop:
    cmp r13, r12
    jge .smt_done

    ; Get from char ordinal
    mov rax, [rbp - SMT_FROM]
    movzx edi, byte [rax + PyStrObject.data + r13]
    ; Get to char ordinal
    mov rax, [rbp - SMT_TO]
    movzx esi, byte [rax + PyStrObject.data + r13]

    ; dict_set(dict, key=ord_from, value=ord_to, value_tag=SMALLINT, key_tag=SMALLINT)
    push r13
    mov rdi, rbx                    ; dict
    ; rsi already = to ordinal (value becomes SmallInt)
    mov rdx, rsi                    ; value = to ordinal
    movzx esi, byte [rax + PyStrObject.data + r13] ; recalc — but we need from ordinal as key
    ; Actually: rdi=dict, rsi=key, rdx=value, rcx=value_tag, r8=key_tag
    mov rcx, [rbp - SMT_FROM]
    movzx esi, byte [rcx + PyStrObject.data + r13]  ; key = from ordinal
    mov rax, [rbp - SMT_TO]
    movzx edx, byte [rax + PyStrObject.data + r13]  ; value = to ordinal
    V_PACK_I64 rdx, rcx      ; dict_set takes Values
    V_PACK_I64 rsi, r8       ; dict_set takes Values
    call dict_set
    pop r13

    inc r13
    jmp .smt_loop

.smt_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smt_error:
    RAISE exc_TypeError_type, "maketrans requires 2 string arguments"

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
SE_FRAME equ 48             ; + 2 pushes = 64
DEF_FUNC str_method_encode, SE_FRAME
    push rbx
    push r12
    ; args[0] = self, args[1] = encoding, args[2] = errors
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
    ; character is exactly the case that is not encodable.
    xor ecx, ecx
.se_ascii_scan:
    cmp rcx, r12
    jge .se_utf8
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    test al, 0x80
    jnz .se_not_encodable
    inc rcx
    jmp .se_ascii_scan

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
