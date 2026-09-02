; comptest.asm - Self-test for the Python source compiler
;
; Reachable via `./apython --selftest-compile`, and run by tests/run_tests.sh
; before any Python-level test.  The point is ordering: a bug in an encoder --
; a wrong CACHE count, a mis-encoded exception table, a non-converging
; EXTENDED_ARG fixpoint -- produces Python-level symptoms that look nothing
; like their cause.  Checking the encoders directly, against the very decoders
; the interpreter will use, keeps those bugs from masquerading as language bugs.
;
; compile_selftest() -> rax = 0 on success, else a case id of the form
;                       (group_number * 1000 + case_index + 1).

%include "macros.inc"
%include "object.inc"
%include "compiler.inc"

extern sys_write

extern arena_alloc
extern arena_free
extern arena_init
extern buf_free
extern buf_init
extern buf_push_u32
extern buf_push_u8
extern buf_reserve

extern ap_strlen
extern comp_free
extern comp_init
extern lex_run

; --- Named frame-layout constants ---
CT_BUF   equ 32            ; a Buf, 32 bytes
CT_ARENA equ 64            ; an Arena, 32 bytes
CT_P0    equ 80
CT_P1    equ 88
; Frame sizes are chosen so that (frame + 8*pushes) is a multiple of 16, which
; is what leaves rsp 16-aligned at every call.  The two groups push a different
; number of registers, so they need different frames.
CT1_FRAME equ 112       ; + 2 pushes = 128
CT2_FRAME equ 104       ; + 3 pushes = 128
CT3_TOK   equ 8
CT3_FRAME equ 8         ; + 5 pushes = 48

section .text

;; ============================================================================
;; ct_group1_buf() -> rax = 0 on success, else the failing case index + 1
;;
;; Buf invariants.  The one that matters is growth across a realloc boundary:
;; every element written before the grow must survive it, because the token
;; array and the instruction stream are both built by unbounded appending.
;; ============================================================================
DEF_FUNC_LOCAL ct_group1_buf, CT1_FRAME
    push rbx
    push r12

    ; --- case 1: a fresh Buf allocates nothing ---
    lea rdi, [rbp - CT_BUF]
    mov esi, 4
    call buf_init
    mov eax, 1
    cmp qword [rbp - CT_BUF + Buf.data], 0
    jne .fail
    cmp qword [rbp - CT_BUF + Buf.cap], 0
    jne .fail

    ; --- case 2: push 4096 u32s, well past several doublings ---
    xor ebx, ebx
.push_loop:
    lea rdi, [rbp - CT_BUF]
    mov esi, ebx
    add esi, 0x1000                     ; a value distinguishable from the index
    call buf_push_u32
    inc ebx
    cmp ebx, 4096
    jb .push_loop

    mov eax, 2
    cmp qword [rbp - CT_BUF + Buf.len], 4096
    jne .fail

    ; --- case 3: every element survived the reallocs ---
    mov eax, 3
    mov r12, [rbp - CT_BUF + Buf.data]
    xor ebx, ebx
.check_loop:
    mov edx, [r12 + rbx*4]
    lea ecx, [rbx + 0x1000]
    cmp edx, ecx
    jne .fail
    inc ebx
    cmp ebx, 4096
    jb .check_loop

    ; --- case 4: capacity is a power-of-two doubling from BUF_MIN_CAP ---
    mov eax, 4
    cmp qword [rbp - CT_BUF + Buf.cap], 4096
    jb .fail

    ; --- case 5: buf_reserve hands back a pointer to the first new element
    ;             and advances len by exactly n ---
    lea rdi, [rbp - CT_BUF]
    mov esi, 10
    call buf_reserve
    mov r12, rax
    mov eax, 5
    cmp qword [rbp - CT_BUF + Buf.len], 4106
    jne .fail
    mov rdx, [rbp - CT_BUF + Buf.data]
    lea rdx, [rdx + 4096*4]
    cmp r12, rdx
    jne .fail

    lea rdi, [rbp - CT_BUF]
    call buf_free

    ; --- case 6: buf_free resets to the empty state, so a Buf can be reused ---
    mov eax, 6
    cmp qword [rbp - CT_BUF + Buf.data], 0
    jne .fail
    cmp qword [rbp - CT_BUF + Buf.len], 0
    jne .fail

    ; --- case 7: byte pushes, the shape the three emitters use ---
    lea rdi, [rbp - CT_BUF]
    mov esi, 1
    call buf_init
    xor ebx, ebx
.byte_loop:
    lea rdi, [rbp - CT_BUF]
    mov esi, ebx
    and esi, 0xff
    call buf_push_u8
    inc ebx
    cmp ebx, 1000
    jb .byte_loop
    mov eax, 7
    cmp qword [rbp - CT_BUF + Buf.len], 1000
    jne .fail
    mov r12, [rbp - CT_BUF + Buf.data]
    movzx edx, byte [r12 + 999]
    cmp edx, 999 & 0xff
    jne .fail
    lea rdi, [rbp - CT_BUF]
    call buf_free

    xor eax, eax
.fail:
    pop r12
    pop rbx
    leave
    ret
END_FUNC ct_group1_buf

;; ============================================================================
;; ct_group2_arena() -> rax = 0 on success, else the failing case index + 1
;;
;; Arena invariants.  Alignment and chunk-spanning are what matter: AST nodes
;; are 32 bytes and are addressed by index off a chunk pointer, so a misaligned
;; or overlapping allocation corrupts the parse silently.
;; ============================================================================
DEF_FUNC_LOCAL ct_group2_arena, CT2_FRAME
    push rbx
    push r12
    push r13

    lea rdi, [rbp - CT_ARENA]
    call arena_init

    ; --- case 1: allocations are 8-byte aligned and never overlap ---
    ; Ask for a deliberately awkward 13 bytes, many times, so the rounding is
    ; exercised rather than accidentally satisfied.
    mov eax, 1
    xor ebx, ebx
    xor r13, r13                        ; r13 = previous allocation
.alloc_loop:
    lea rdi, [rbp - CT_ARENA]
    mov esi, 13
    call arena_alloc
    test al, 7
    jnz .fail                           ; misaligned
    test r13, r13
    jz .no_prev
    ; The bump pointer only moves forward within a chunk; across a chunk
    ; boundary the addresses are unrelated, so only check the ordering when
    ; the two are close enough to be in the same chunk.
    mov rdx, rax
    sub rdx, r13
    cmp rdx, ARENA_CHUNK
    jae .no_prev
    cmp rdx, 16                         ; 13 rounded up to 8 is 16
    jb .fail
.no_prev:
    mov r13, rax
    inc ebx
    cmp ebx, 20000                      ; enough to span several chunks
    jb .alloc_loop

    ; --- case 2: writes to distinct allocations do not alias ---
    mov eax, 2
    lea rdi, [rbp - CT_ARENA]
    mov esi, 64
    call arena_alloc
    mov [rbp - CT_P0], rax
    mov rcx, 0xaaaaaaaaaaaaaaaa
    mov [rax], rcx
    lea rdi, [rbp - CT_ARENA]
    mov esi, 64
    call arena_alloc
    mov [rbp - CT_P1], rax
    mov rcx, 0xbbbbbbbbbbbbbbbb
    mov [rax], rcx
    mov rdx, [rbp - CT_P0]
    mov rcx, 0xaaaaaaaaaaaaaaaa
    cmp qword [rdx], rcx
    jne .fail

    ; --- case 3: an allocation larger than a chunk still works ---
    mov eax, 3
    lea rdi, [rbp - CT_ARENA]
    mov esi, ARENA_CHUNK * 4
    call arena_alloc
    test rax, rax
    jz .fail
    ; write both ends, to prove the whole span is really ours
    mov rcx, 0x1122334455667788
    mov [rax], rcx
    mov rdx, ARENA_CHUNK * 4 - 8
    mov rcx, 0x8877665544332211
    mov [rax + rdx], rcx
    mov rcx, 0x1122334455667788
    cmp qword [rax], rcx
    jne .fail

    ; --- case 4: arena_free empties it, and it is reusable afterwards ---
    lea rdi, [rbp - CT_ARENA]
    call arena_free
    mov eax, 4
    cmp qword [rbp - CT_ARENA + Arena.chunks], 0
    jne .fail
    lea rdi, [rbp - CT_ARENA]
    mov esi, 32
    call arena_alloc
    test rax, rax
    jz .fail
    lea rdi, [rbp - CT_ARENA]
    call arena_free

    xor eax, eax
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC ct_group2_arena

;; ============================================================================
;; ct_group3_lex() -> rax = 0 on success, else the failing case index + 1
;;
;; Each case is a source string and the exact sequence of token kinds it must
;; produce.  Comparing kinds rather than text is deliberate: it pins down the
;; decisions that are easy to get subtly wrong and hard to see later -- maximal
;; munch, where NEWLINE is and is not emitted, and whether a blank or
;; comment-only line inside a suite disturbs the indent stack.
;; ============================================================================
DEF_FUNC_LOCAL ct_group3_lex, CT3_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    lea rbx, [rel lex_cases]
    xor r15d, r15d                      ; case index
.case_loop:
    mov r12, [rbx]                      ; source
    test r12, r12
    jz .all_ok
    mov r13, [rbx + 8]                  ; expected kinds
    inc r15d

    mov rdi, r12
    call ap_strlen
    mov rdx, rax
    lea rdi, [rel ct_comp]
    mov rsi, r12
    xor ecx, ecx                        ; filename: none
    xor r8d, r8d                        ; CMODE_EXEC
    call comp_init

    lea rdi, [rel ct_comp]
    xor esi, esi                        ; the whole source, not a span
    xor edx, edx
    xor ecx, ecx
    call lex_run
    test eax, eax
    jz .case_fail                       ; the lexer reported an error

    ; Walk the produced tokens against the expected kinds.
    lea rax, [rel ct_comp]
    mov r14, [rax + Comp.tokens + Buf.data]
    mov r8, [rax + Comp.tokens + Buf.len]
    xor ecx, ecx
.tok_loop:
    movzx edx, byte [r13 + rcx]
    cmp edx, 0xff
    je .expect_end
    cmp rcx, r8
    jae .case_fail                      ; ran out of tokens early
    mov rax, rcx
    shl rax, TOKEN_SHIFT
    movzx eax, word [r14 + rax + Token.kind]
    cmp eax, edx
    jne .case_fail
    inc rcx
    jmp .tok_loop
.expect_end:
    cmp rcx, r8
    jne .case_fail                      ; produced more tokens than expected

    lea rdi, [rel ct_comp]
    call comp_free
    add rbx, 16
    jmp .case_loop

.case_fail:
    ; Report the token index as well as the case: "wrong token 4 of case 9" is
    ; a diagnosis, where "case 9 failed" is the start of another investigation.
    mov [rbp - CT3_TOK], rcx
    lea rdi, [rel ct_comp]
    call comp_free
    mov eax, r15d
    imul eax, 100
    mov rcx, [rbp - CT3_TOK]
    lea eax, [rax + rcx + 1]
    jmp .ret
.all_ok:
    xor eax, eax
.ret:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC ct_group3_lex

;; ============================================================================
;; compile_selftest() -> rax = 0, or a failure id: group * 1000000 plus the
;; group's own detail code.  Group 3 encodes case * 100 + token index + 1, so
;; 3000901 reads as "group 3, case 9, token 0".
;; ============================================================================
DEF_FUNC compile_selftest
    call ct_group1_buf
    test eax, eax
    jz .g2
    add eax, 1000000
    leave
    ret
.g2:
    call ct_group2_arena
    test eax, eax
    jz .g3
    add eax, 2000000
    leave
    ret
.g3:
    call ct_group3_lex
    test eax, eax
    jz .ok
    add eax, 3000000
    leave
    ret
.ok:
    xor eax, eax
    leave
    ret
END_FUNC compile_selftest

;; ============================================================================
;; compile_selftest_main() -> exit status in rax
;; Prints a one-line verdict, mirroring value_selftest_main.
;; ============================================================================
DEF_FUNC compile_selftest_main, 8
    push rbx
    call compile_selftest
    mov rbx, rax
    test rbx, rbx
    jnz .fail

    mov edi, 1
    lea rsi, [rel ct_msg_ok]
    mov edx, ct_msg_ok_len
    call sys_write
    xor eax, eax
    pop rbx
    leave
    ret

.fail:
    mov edi, 1
    lea rsi, [rel ct_msg_fail]
    mov edx, ct_msg_fail_len
    call sys_write

    ; Render the case id as decimal, backwards from the end of the buffer.
    lea rsi, [rel ct_fail_buf + 31]
    mov byte [rsi], 10
    mov rax, rbx
    mov ecx, 10
.digit:
    xor edx, edx
    div rcx
    add dl, '0'
    dec rsi
    mov [rsi], dl
    test rax, rax
    jnz .digit

    lea rdx, [rel ct_fail_buf + 32]
    sub rdx, rsi
    mov edi, 1
    call sys_write
    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC compile_selftest_main

section .rodata

align 8
lex_cases:
    dq lx_src_arith,    lx_exp_arith
    dq lx_src_call,     lx_exp_call
    dq lx_src_indent,   lx_exp_indent
    dq lx_src_blank,    lx_exp_blank
    dq lx_src_implicit, lx_exp_implicit
    dq lx_src_explicit, lx_exp_explicit
    dq lx_src_munch,    lx_exp_munch
    dq lx_src_numbers,  lx_exp_numbers
    dq lx_src_strings,  lx_exp_strings
    dq lx_src_noprefix, lx_exp_noprefix
    dq lx_src_triple,   lx_exp_triple
    dq lx_src_kwnear,   lx_exp_kwnear
    dq lx_src_noeol,    lx_exp_noeol
    dq lx_src_empty,    lx_exp_empty
    dq lx_src_deep,     lx_exp_deep
    dq 0, 0

lx_src_arith:    db "1 + 2*3", 0
lx_exp_arith:    db TOK_NUMBER, TOK_PLUS, TOK_NUMBER, TOK_STAR, TOK_NUMBER
                 db TOK_NEWLINE, TOK_ENDMARKER, 0xff

lx_src_call:     db "x = foo(1, 'a')", 0
lx_exp_call:     db TOK_NAME, TOK_EQUAL, TOK_NAME, TOK_LPAR, TOK_NUMBER
                 db TOK_COMMA, TOK_STRING, TOK_RPAR, TOK_NEWLINE, TOK_ENDMARKER, 0xff

lx_src_indent:   db "if x:", 10, "    y = 1", 10, "z = 2", 10, 0
lx_exp_indent:   db TOK_IF, TOK_NAME, TOK_COLON, TOK_NEWLINE
                 db TOK_INDENT, TOK_NAME, TOK_EQUAL, TOK_NUMBER, TOK_NEWLINE
                 db TOK_DEDENT, TOK_NAME, TOK_EQUAL, TOK_NUMBER, TOK_NEWLINE
                 db TOK_ENDMARKER, 0xff

; A blank line and a comment-only line inside a suite must leave the indent
; stack alone and emit nothing at all.
lx_src_blank:    db "if a:", 10, "    b", 10, 10, "    # note", 10, "    c", 10, 0
lx_exp_blank:    db TOK_IF, TOK_NAME, TOK_COLON, TOK_NEWLINE
                 db TOK_INDENT, TOK_NAME, TOK_NEWLINE
                 db TOK_NAME, TOK_NEWLINE
                 db TOK_DEDENT, TOK_ENDMARKER, 0xff

; Inside brackets a newline is implicit continuation: no NEWLINE token.
lx_src_implicit: db "f(1,", 10, "  2)", 10, 0
lx_exp_implicit: db TOK_NAME, TOK_LPAR, TOK_NUMBER, TOK_COMMA, TOK_NUMBER
                 db TOK_RPAR, TOK_NEWLINE, TOK_ENDMARKER, 0xff

lx_src_explicit: db "a = 1 + \", 10, "    2", 10, 0
lx_exp_explicit: db TOK_NAME, TOK_EQUAL, TOK_NUMBER, TOK_PLUS, TOK_NUMBER
                 db TOK_NEWLINE, TOK_ENDMARKER, 0xff

; Maximal munch: every one of these must take the longest operator, not the
; shortest.  op_table is sorted longest-first per first byte to make that free.
lx_src_munch:    db "a**=b//=c<<=d>>=e...f->g:=h!=i<=j>=k==l@=m", 0
lx_exp_munch:    db TOK_NAME, TOK_DOUBLESTAREQUAL, TOK_NAME, TOK_DOUBLESLASHEQUAL
                 db TOK_NAME, TOK_LEFTSHIFTEQUAL, TOK_NAME, TOK_RIGHTSHIFTEQUAL
                 db TOK_NAME, TOK_ELLIPSIS, TOK_NAME, TOK_RARROW, TOK_NAME
                 db TOK_COLONEQUAL, TOK_NAME, TOK_NOTEQUAL, TOK_NAME
                 db TOK_LESSEQUAL, TOK_NAME, TOK_GREATEREQUAL, TOK_NAME
                 db TOK_EQEQUAL, TOK_NAME, TOK_ATEQUAL, TOK_NAME
                 db TOK_NEWLINE, TOK_ENDMARKER, 0xff

lx_src_numbers:  db "0x1f 0b101 0o17 1_000 1.5 1e10 1.5e-3 2j .5 1.", 0
lx_exp_numbers:  db TOK_NUMBER, TOK_NUMBER, TOK_NUMBER, TOK_NUMBER, TOK_NUMBER
                 db TOK_NUMBER, TOK_NUMBER, TOK_NUMBER, TOK_NUMBER, TOK_NUMBER
                 db TOK_NEWLINE, TOK_ENDMARKER, 0xff

lx_src_strings:  db "b'x' rb", 34, "y", 34, " f'z' u'w' R'q'", 0
lx_exp_strings:  db TOK_STRING, TOK_STRING, TOK_STRING, TOK_STRING, TOK_STRING
                 db TOK_NEWLINE, TOK_ENDMARKER, 0xff

; An identifier that is not a legal string prefix stays a name, so this is a
; name followed by a string -- which is why `print'x'` is a syntax error in
; Python 3 rather than a mysterious literal.
lx_src_noprefix: db "print'x' xy'z'", 0
lx_exp_noprefix: db TOK_NAME, TOK_STRING, TOK_NAME, TOK_STRING
                 db TOK_NEWLINE, TOK_ENDMARKER, 0xff

lx_src_triple:   db "s = ", 34, 34, 34, "a", 10, "b", 34, 34, 34, 10, "t = 1", 10, 0
lx_exp_triple:   db TOK_NAME, TOK_EQUAL, TOK_STRING, TOK_NEWLINE
                 db TOK_NAME, TOK_EQUAL, TOK_NUMBER, TOK_NEWLINE
                 db TOK_ENDMARKER, 0xff

; Keywords and the identifiers that merely start like them.
lx_src_kwnear:   db "is not in iss no lambda lambda_ None Nones", 0
lx_exp_kwnear:   db TOK_IS, TOK_NOT, TOK_IN, TOK_NAME, TOK_NAME, TOK_LAMBDA
                 db TOK_NAME, TOK_NONE, TOK_NAME
                 db TOK_NEWLINE, TOK_ENDMARKER, 0xff

; A file that does not end in a newline still gets one, or the parser would
; see a statement that never terminates.
lx_src_noeol:    db "x", 0
lx_exp_noeol:    db TOK_NAME, TOK_NEWLINE, TOK_ENDMARKER, 0xff

lx_src_empty:    db "", 0
lx_exp_empty:    db TOK_ENDMARKER, 0xff

; Nested suites must unwind every level at end of input.
lx_src_deep:     db "if a:", 10, "  if b:", 10, "    c", 10, 0
lx_exp_deep:     db TOK_IF, TOK_NAME, TOK_COLON, TOK_NEWLINE, TOK_INDENT
                 db TOK_IF, TOK_NAME, TOK_COLON, TOK_NEWLINE, TOK_INDENT
                 db TOK_NAME, TOK_NEWLINE
                 db TOK_DEDENT, TOK_DEDENT, TOK_ENDMARKER, 0xff

ct_msg_ok:   db "compiler selftest: ok", 10
ct_msg_ok_len equ $ - ct_msg_ok
ct_msg_fail: db "compiler selftest FAILED, case "
ct_msg_fail_len equ $ - ct_msg_fail

section .bss
ct_fail_buf: resb 32
align 16
ct_comp:     resb Comp_size

ASM_INIT
