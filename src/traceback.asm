; ============================================================================
; traceback.asm -- the code object's side tables, and traceback rendering
;
; Both compressed tables CPython hangs off a code object are decoded here:
; co_linetable, for the line a traceback names, and co_exceptiontable, for
; the handler the unwinder jumps to.  They are different encodings of the
; same idea -- a varint stream indexed by instruction offset -- and the two
; varint readers below are siblings.
;
; Python 3.12 stores locations in co_linetable using the PEP 626 format: each
; entry begins with a byte 0x80 | (code << 3) | (length - 1), where `length`
; is how many code units the entry covers and `code` selects what follows.
; Only the line delta matters here; the column fields are decoded far enough
; to be skipped.
;
;   code 0-9   short form, one trailing byte, line delta 0
;   code 10-12 one-line form, two trailing bytes, line delta = code - 10
;   code 13    no columns, one signed varint line delta
;   code 14    long form, signed varint line delta then three varints
;   code 15    no location at all
;
; The renderer walks the chain a raise builds -- newest entry at the head, so
; tb_next order is outermost first, "most recent call last" -- and prints what
; CPython's default excepthook prints, including the __cause__ / __context__
; preamble.
; ============================================================================

%include "object.inc"
%include "macros.inc"
%include "value.inc"

extern sys_write
extern sys_open
extern sys_read
extern sys_close
extern obj_str
extern exc_is_syntax
extern str_type
extern tuple_type
extern obj_decref
extern str_type
extern traceback_type
extern ap_malloc

TB_CHUNK equ 4096
TB_LINE  equ 1024
TB_PATH  equ 4096

section .text

; ----------------------------------------------------------------------------
; tb_read_varint -- reads a PEP 626 varint from [r8], advancing r8.
; Result in ecx.  rax, rdx, rsi, r9, r10 are preserved; r11 is clobbered.
; ----------------------------------------------------------------------------
DEF_FUNC_BARE tb_read_varint
    push rax
    push rdx
    xor eax, eax                    ; accumulated value
    xor edx, edx                    ; shift
.rv_loop:
    movzx r11d, byte [r8]
    inc r8
    and r11d, 63
    push rcx
    mov ecx, edx
    shl r11, cl
    pop rcx
    or rax, r11
    add edx, 6
    movzx r11d, byte [r8 - 1]
    test r11d, 64
    jnz .rv_loop
    mov ecx, eax
    pop rdx
    pop rax
    ret
END_FUNC tb_read_varint

; tb_read_svarint -- zig-zag signed varint; result in ecx.
DEF_FUNC_BARE tb_read_svarint
    call tb_read_varint
    mov r11d, ecx
    shr ecx, 1
    test r11d, 1
    jz .sv_done
    neg ecx
.sv_done:
    ret
END_FUNC tb_read_svarint

; ----------------------------------------------------------------------------
; code_addr2line(rdi = PyCodeObject*, rsi = instruction offset in code units)
;   -> eax = line number, or 0 when the table does not cover the offset
; ----------------------------------------------------------------------------
global code_addr2line
DEF_FUNC_BARE code_addr2line
    mov r8, [rdi + PyCodeObject.co_linetable]
    test r8, r8
    jz .a2l_none
    mov eax, [rdi + PyCodeObject.co_firstlineno]    ; running line
    mov r9, [r8 + PyBytesObject.ob_size]
    lea r8, [r8 + PyBytesObject.data]               ; cursor
    add r9, r8                                      ; end
    xor r10d, r10d                                  ; code-unit offset of entry

.a2l_entry:
    cmp r8, r9
    jae .a2l_none
    movzx ecx, byte [r8]
    inc r8
    test cl, 0x80
    jz .a2l_none                                    ; desynchronised
    mov edx, ecx
    and edx, 7
    inc edx                                         ; edx = length in units
    shr ecx, 3
    and ecx, 0x0F                                   ; ecx = code

    cmp ecx, 15
    je .a2l_check                                   ; no location, delta 0
    cmp ecx, 14
    je .a2l_long
    cmp ecx, 13
    je .a2l_nocol
    cmp ecx, 10
    jb .a2l_short
    sub ecx, 10                                     ; one-line form
    add eax, ecx
    add r8, 2
    jmp .a2l_check

.a2l_short:
    inc r8
    jmp .a2l_check

.a2l_nocol:
    call tb_read_svarint
    add eax, ecx
    jmp .a2l_check

.a2l_long:
    call tb_read_svarint
    push rcx                                        ; line delta
    call tb_read_varint                             ; end line delta
    call tb_read_varint                             ; start column + 1
    call tb_read_varint                             ; end column + 1
    pop rcx
    add eax, ecx

.a2l_check:
    cmp rsi, r10
    jb .a2l_advance
    lea r11, [r10 + rdx]
    cmp rsi, r11
    jae .a2l_advance
    ret                                             ; eax = line

.a2l_advance:
    add r10, rdx
    jmp .a2l_entry

.a2l_none:
    xor eax, eax
    ret
END_FUNC code_addr2line

; ----------------------------------------------------------------------------
; traceback_here(rdi = exception, rsi = code object, rdx = lasti in code units)
; Prepends a frame to the exception's traceback, as PyTraceBack_Here does.
; ----------------------------------------------------------------------------
TH_EXC   equ 8
TH_CODE  equ 16
TH_LASTI equ 24
TH_TB    equ 32
TH_FRAME equ 32
global traceback_here
DEF_FUNC traceback_here, TH_FRAME
    mov [rbp - TH_EXC], rdi
    mov [rbp - TH_CODE], rsi
    mov [rbp - TH_LASTI], rdx

    mov edi, PyTracebackObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel traceback_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyTracebackObject.tb_lineno], 0

    mov rcx, [rbp - TH_EXC]
    mov rdx, [rcx + PyExceptionObject.exc_tb]
    mov [rax + PyTracebackObject.tb_next], rdx      ; adopts the old chain
    mov rdx, [rbp - TH_CODE]
    mov [rax + PyTracebackObject.tb_code], rdx
    mov rdx, [rbp - TH_LASTI]
    mov [rax + PyTracebackObject.tb_lasti], rdx
    mov [rcx + PyExceptionObject.exc_tb], rax
    mov [rbp - TH_TB], rax

    mov rdi, [rbp - TH_CODE]
    test rdi, rdi
    jz .th_done
    ; The code object must outlive the frame it came from.
    inc qword [rdi + PyObject.ob_refcnt]
    mov rsi, [rbp - TH_LASTI]
    call code_addr2line
    movsx rax, eax
    mov rcx, [rbp - TH_TB]
    mov [rcx + PyTracebackObject.tb_lineno], rax
.th_done:
    leave
    ret
END_FUNC traceback_here

; ----------------------------------------------------------------------------
; tb_write(rdi = buf, rsi = len) -- stderr
; ----------------------------------------------------------------------------
DEF_FUNC_BARE tb_write
    mov rdx, rsi
    mov rsi, rdi
    mov edi, 2
    jmp sys_write
END_FUNC tb_write

; tb_write_cstr(rdi = NUL-terminated string)
DEF_FUNC tb_write_cstr
    push rbx
    mov rbx, rdi
    xor ecx, ecx
.len:
    cmp byte [rbx + rcx], 0
    je .have
    inc rcx
    jmp .len
.have:
    mov rdi, rbx
    mov rsi, rcx
    call tb_write
    pop rbx
    leave
    ret
END_FUNC tb_write_cstr

; tb_write_str(rdi = PyStrObject* or NULL) -- writes nothing for a non-str
DEF_FUNC tb_write_str
    test rdi, rdi
    jz .ws_out
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .ws_out
    mov rsi, [rdi + PyStrObject.ob_size]
    add rdi, PyStrObject.data
    call tb_write
.ws_out:
    leave
    ret
END_FUNC tb_write_str

; tb_write_dec(rdi = signed value)
TD_BUF   equ 32
TD_FRAME equ 48
DEF_FUNC tb_write_dec, TD_FRAME
    mov rax, rdi
    lea rcx, [rbp - TD_BUF]
    add rcx, 24                     ; one past the digit area
    mov r8, rcx
    xor r9d, r9d
    test rax, rax
    jns .td_pos
    mov r9d, 1
    neg rax
.td_pos:
    mov r10, 10
.td_loop:
    xor edx, edx
    div r10
    add dl, '0'
    dec rcx
    mov [rcx], dl
    test rax, rax
    jnz .td_loop
    test r9d, r9d
    jz .td_out
    dec rcx
    mov byte [rcx], '-'
.td_out:
    mov rdi, rcx
    mov rsi, r8
    sub rsi, rcx
    call tb_write
    leave
    ret
END_FUNC tb_write_dec

; ----------------------------------------------------------------------------
; tb_write_source(rdi = filename str, rsi = line number)
; Prints the source line stripped and indented four spaces.  A file that
; cannot be opened simply produces nothing, which is what CPython does.
; The file is scanned in chunks, so a large source costs no extra memory and
; the frame stays small enough to use while unwinding.
; ----------------------------------------------------------------------------
TS_FD    equ 8
TS_TGT   equ 16
TS_CUR   equ 24
TS_LEN   equ 32
TS_PATH  equ 48 + TB_PATH
TS_LBUF  equ TS_PATH + TB_LINE
TS_CBUF  equ TS_LBUF + TB_CHUNK
TS_FRAME equ TS_CBUF + 16
DEF_FUNC tb_write_source, TS_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - TS_TGT], rsi
    mov qword [rbp - TS_CUR], 1
    mov qword [rbp - TS_LEN], 0
    mov qword [rbp - TS_FD], -1

    test rsi, rsi
    jle .ts_out
    test rdi, rdi
    jz .ts_out
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .ts_out
    mov rdx, [rdi + PyStrObject.ob_size]
    cmp rdx, TB_PATH - 1
    jae .ts_out

    ; Copy the filename into a NUL-terminated buffer for open(2)
    lea rcx, [rbp - TS_PATH]
    lea rsi, [rdi + PyStrObject.data]
    xor eax, eax
.ts_copy:
    cmp rax, rdx
    jge .ts_copied
    mov r8b, [rsi + rax]
    mov [rcx + rax], r8b
    inc rax
    jmp .ts_copy
.ts_copied:
    mov byte [rcx + rax], 0

    lea rdi, [rbp - TS_PATH]
    xor esi, esi                    ; O_RDONLY
    xor edx, edx
    call sys_open
    test eax, eax
    js .ts_out
    movsx rax, eax
    mov [rbp - TS_FD], rax

.ts_read:
    mov rdi, [rbp - TS_FD]
    lea rsi, [rbp - TS_CBUF]
    mov edx, TB_CHUNK
    call sys_read
    test rax, rax
    jle .ts_flush
    mov r12, rax                    ; bytes read
    xor rbx, rbx                    ; index
.ts_scan:
    cmp rbx, r12
    jge .ts_read
    lea rax, [rbp - TS_CBUF]
    movzx r13d, byte [rax + rbx]
    inc rbx
    cmp r13b, 10
    je .ts_newline
    mov rax, [rbp - TS_CUR]
    cmp rax, [rbp - TS_TGT]
    jne .ts_scan
    mov rax, [rbp - TS_LEN]
    cmp rax, TB_LINE - 1
    jae .ts_scan
    lea rcx, [rbp - TS_LBUF]
    mov [rcx + rax], r13b
    inc rax
    mov [rbp - TS_LEN], rax
    jmp .ts_scan
.ts_newline:
    mov rax, [rbp - TS_CUR]
    cmp rax, [rbp - TS_TGT]
    je .ts_flush
    inc rax
    mov [rbp - TS_CUR], rax
    jmp .ts_scan

.ts_flush:
    mov rax, [rbp - TS_CUR]
    cmp rax, [rbp - TS_TGT]
    jne .ts_close
    mov rdx, [rbp - TS_LEN]
    test rdx, rdx
    jz .ts_close
    ; Strip leading whitespace, then trailing.
    xor rcx, rcx
.ts_lstrip:
    cmp rcx, rdx
    jge .ts_close
    lea rax, [rbp - TS_LBUF]
    mov r8b, [rax + rcx]
    cmp r8b, ' '
    je .ts_lnext
    cmp r8b, 9
    je .ts_lnext
    cmp r8b, 13
    je .ts_lnext
    jmp .ts_lstripped
.ts_lnext:
    inc rcx
    jmp .ts_lstrip
.ts_lstripped:
.ts_rstrip:
    cmp rdx, rcx
    jle .ts_close
    lea rax, [rbp - TS_LBUF]
    mov r8b, [rax + rdx - 1]
    cmp r8b, ' '
    je .ts_rnext
    cmp r8b, 9
    je .ts_rnext
    cmp r8b, 13
    je .ts_rnext
    jmp .ts_emit
.ts_rnext:
    dec rdx
    jmp .ts_rstrip
.ts_emit:
    push rcx
    push rdx
    CSTRING rdi, "    "
    call tb_write_cstr
    pop rdx
    pop rcx
    lea rdi, [rbp - TS_LBUF]
    add rdi, rcx
    mov rsi, rdx
    sub rsi, rcx
    call tb_write
    CSTRING rdi, `\n`
    call tb_write_cstr

.ts_close:
    mov rdi, [rbp - TS_FD]
    cmp rdi, 0
    jl .ts_out
    call sys_close
.ts_out:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tb_write_source

; tb_print_repeated(rdi = run length) -- the elision line CPython prints
DEF_FUNC tb_print_repeated
    push rbx
    lea rbx, [rdi - TB_RECURSIVE_CUTOFF]
    CSTRING rdi, "  [Previous line repeated "
    call tb_write_cstr
    mov rdi, rbx
    call tb_write_dec
    CSTRING rdi, " more time"
    call tb_write_cstr
    cmp rbx, 1
    je .pr_one
    CSTRING rdi, "s"
    call tb_write_cstr
.pr_one:
    CSTRING rdi, `]\n`
    call tb_write_cstr
    pop rbx
    leave
    ret
END_FUNC tb_print_repeated

; ----------------------------------------------------------------------------
; traceback_print(rdi = exception)
; Prints the CPython-shaped report for an uncaught exception, on stderr.
; ----------------------------------------------------------------------------
TP_EXC   equ 8
TP_TB    equ 16
TP_TMP   equ 24
TP_LASTC equ 32          ; code object of the previous entry
TP_LASTL equ 40          ; line number of the previous entry
TP_CNT   equ 48          ; length of the current run of identical entries
TP_FRAME equ 64
TB_RECURSIVE_CUTOFF equ 3
TB_SEEN_MAX equ 64
global traceback_print
DEF_FUNC traceback_print
    mov qword [rel tb_seen_n], 0
    mov rsi, rdi
    xor edi, edi
    call tb_print_one
    leave
    ret
END_FUNC traceback_print

; tb_print_one(rsi = exception) -- the body; tb_seen guards against a cycle in
; the __cause__ / __context__ chain, which `raise e from e` otherwise turns
; into unbounded recursion.
DEF_FUNC tb_print_one, TP_FRAME
    push rbx
    mov rdi, rsi
    test rdi, rdi
    jz .tp_out
    mov [rbp - TP_EXC], rdi

    ; Already reported? then this is a cycle; stop here.
    mov rcx, [rel tb_seen_n]
    xor edx, edx
    lea r8, [rel tb_seen]
.tp_seen_scan:
    cmp rdx, rcx
    jge .tp_seen_add
    cmp [r8 + rdx*8], rdi
    je .tp_skip
    inc rdx
    jmp .tp_seen_scan
.tp_seen_add:
    cmp rcx, TB_SEEN_MAX
    jae .tp_seen_done
    mov [r8 + rcx*8], rdi
    inc rcx
    mov [rel tb_seen_n], rcx
.tp_seen_done:

    ; A __cause__ or __context__ is reported first, then the linking sentence.
    mov rax, [rdi + PyExceptionObject.exc_cause]
    test rax, rax
    jnz .tp_cause
    ; `raise X from ...` suppresses the implicit context.
    cmp qword [rdi + PyExceptionObject.exc_suppress], 0
    jne .tp_header
    mov rax, [rdi + PyExceptionObject.exc_context]
    test rax, rax
    jz .tp_header
    mov rsi, rax
    call tb_print_one
    test eax, eax
    jz .tp_header
    CSTRING rdi, `\nDuring handling of the above exception, another exception occurred:\n\n`
    call tb_write_cstr
    jmp .tp_header
.tp_cause:
    mov rsi, rax
    call tb_print_one
    test eax, eax
    jz .tp_header
    CSTRING rdi, `\nThe above exception was the direct cause of the following exception:\n\n`
    call tb_write_cstr

.tp_header:
    mov rdi, [rbp - TP_EXC]
    mov rax, [rdi + PyExceptionObject.exc_tb]
    test rax, rax
    jz .tp_body
    mov [rbp - TP_TB], rax
    mov qword [rbp - TP_LASTC], 0
    mov qword [rbp - TP_LASTL], -1
    mov qword [rbp - TP_CNT], 0
    CSTRING rdi, `Traceback (most recent call last):\n`
    call tb_write_cstr

.tp_frame:
    mov rbx, [rbp - TP_TB]
    test rbx, rbx
    jz .tp_tail_repeat

    ; Collapse a run of identical frames the way CPython does: print the
    ; first three, then "[Previous line repeated N more times]".  Without it
    ; a RecursionError printed a thousand copies of one line.
    mov rax, [rbx + PyTracebackObject.tb_code]
    cmp rax, [rbp - TP_LASTC]
    jne .tp_newrun
    mov rax, [rbx + PyTracebackObject.tb_lineno]
    cmp rax, [rbp - TP_LASTL]
    je .tp_samerun
.tp_newrun:
    mov rdi, [rbp - TP_CNT]
    cmp rdi, TB_RECURSIVE_CUTOFF
    jle .tp_run_reset
    push rbx
    call tb_print_repeated
    pop rbx
.tp_run_reset:
    mov rax, [rbx + PyTracebackObject.tb_code]
    mov [rbp - TP_LASTC], rax
    mov rax, [rbx + PyTracebackObject.tb_lineno]
    mov [rbp - TP_LASTL], rax
    mov qword [rbp - TP_CNT], 0
.tp_samerun:
    inc qword [rbp - TP_CNT]
    cmp qword [rbp - TP_CNT], TB_RECURSIVE_CUTOFF
    jg .tp_next

    push rbx
    CSTRING rdi, `  File "`
    call tb_write_cstr
    pop rbx
    mov rax, [rbx + PyTracebackObject.tb_code]
    test rax, rax
    jz .tp_noname
    push rbx
    mov rdi, [rax + PyCodeObject.co_filename]
    call tb_write_str
    pop rbx
    push rbx
    CSTRING rdi, `", line `
    call tb_write_cstr
    pop rbx
    push rbx
    mov rdi, [rbx + PyTracebackObject.tb_lineno]
    call tb_write_dec
    pop rbx
    push rbx
    CSTRING rdi, ", in "
    call tb_write_cstr
    pop rbx
    push rbx
    mov rax, [rbx + PyTracebackObject.tb_code]
    mov rdi, [rax + PyCodeObject.co_name]
    call tb_write_str
    pop rbx
    push rbx
    CSTRING rdi, `\n`
    call tb_write_cstr
    pop rbx
    mov rax, [rbx + PyTracebackObject.tb_code]
    mov rdi, [rax + PyCodeObject.co_filename]
    mov rsi, [rbx + PyTracebackObject.tb_lineno]
    push rbx
    call tb_write_source
    pop rbx
    jmp .tp_next
.tp_noname:
    push rbx
    CSTRING rdi, `?", line ?\n`
    call tb_write_cstr
    pop rbx
.tp_next:
    mov rax, [rbx + PyTracebackObject.tb_next]
    mov [rbp - TP_TB], rax
    jmp .tp_frame

.tp_tail_repeat:
    mov rdi, [rbp - TP_CNT]
    cmp rdi, TB_RECURSIVE_CUTOFF
    jle .tp_body
    call tb_print_repeated

.tp_body:
    ; A syntax error shows where it happened before it says what it was, the
    ; way CPython does: the file and line, the source of that line, and a caret
    ; under the column.  Its args carry (msg, (filename, lineno, offset, text)).
    mov rdi, [rbp - TP_EXC]
    call tb_syntax_header

    ; "TypeName: str(exc)", with the colon omitted when str(exc) is empty.
    mov rdi, [rbp - TP_EXC]
    mov rax, [rdi + PyObject.ob_type]
    mov rdi, [rax + PyTypeObject.tp_name]
    call tb_write_cstr

    mov rdi, [rbp - TP_EXC]
    call obj_str
    V_UNPACK rax, rdx
    test rax, rax
    jz .tp_newline
    mov [rbp - TP_TMP], rax
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .tp_release
    cmp qword [rax + PyStrObject.ob_size], 0
    je .tp_release
    CSTRING rdi, ": "
    call tb_write_cstr
    mov rdi, [rbp - TP_TMP]
    call tb_write_str
.tp_release:
    mov rdi, [rbp - TP_TMP]
    call obj_decref

.tp_newline:
    CSTRING rdi, `\n`
    call tb_write_cstr
    mov eax, 1                      ; something was printed
    pop rbx
    leave
    ret
.tp_skip:
.tp_out:
    xor eax, eax                    ; nothing printed: NULL, or a cycle
    pop rbx
    leave
    ret
END_FUNC tb_print_one

;; ============================================================================
;; tb_syntax_header(PyObject *exc)
;; The File/line/source/caret block a syntax error is printed with.  Does
;; nothing for anything else, or for a syntax error with no location.
;; ============================================================================
SH_INNER equ 16
SH_TEXT  equ 24
SH_COL   equ 32
SH_I     equ 40
SH_LEN   equ 48           ; bytes of source text actually written
SH_HASCOL equ 56          ; whether args[1][2] gave us a usable column
SH_FRAME equ 72           ; + 1 push = 80
DEF_FUNC tb_syntax_header, SH_FRAME
    push rbx
    mov rbx, rdi
    call exc_is_syntax
    test eax, eax
    jz .done

    ; args is whatever the raise supplied, so every step has to be checked:
    ; `raise SyntaxError("m", (1, 2, 3, 4))` is legal Python and would
    ; otherwise print the integer 1 as if it were a filename string.
    mov rax, [rbx + PyExceptionObject.exc_args]
    V_TEST_PTR rax, rcx
    ja .done
    test rax, rax
    jz .done
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    jne .done
    cmp qword [rax + PyTupleObject.ob_size], 2
    jl .done
    mov rax, [rax + PyTupleObject.ob_item]
    mov rax, [rax + 8]
    V_TEST_PTR rax, rcx
    ja .done
    test rax, rax
    jz .done
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    jne .done
    cmp qword [rax + PyTupleObject.ob_size], 4
    jl .done
    mov [rbp - SH_INNER], rax
    mov rax, [rax + PyTupleObject.ob_item]

    ; The filename must be a str and the line number an int.
    mov rcx, [rax]
    V_TEST_PTR rcx, rdx
    ja .done
    test rcx, rcx
    jz .done
    mov rdx, [rcx + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rdx, rcx
    jne .done
    mov rcx, [rax + 8]
    V_IS_INT rcx, rdx
    jb .done

    push rax
    CSTRING rdi, `  File "`
    call tb_write_cstr
    pop rax
    push rax
    mov rdi, [rax]
    call tb_write_str
    pop rax
    push rax
    CSTRING rdi, `", line `
    call tb_write_cstr
    pop rax
    push rax
    mov rdi, [rax + 8]
    V_TO_I64 rdi
    call tb_write_dec
    pop rax
    push rax
    CSTRING rdi, `\n`
    call tb_write_cstr
    pop rax

    ; The source line, indented four spaces and stripped of leading blanks the
    ; way CPython prints it, then a caret under the offending column.
    mov rcx, [rax + 24]
    mov [rbp - SH_TEXT], rcx
    ; The offset is checked like the line number is.  It is legally None --
    ; that is what CPython puts there when the column is unknown -- and None is
    ; a heap pointer, so subtracting the int bias from it gave about 2^50 and
    ; the caret loop below wrote that many spaces, one write() each.
    mov rcx, [rax + 16]
    mov qword [rbp - SH_HASCOL], 1
    V_IS_INT rcx, rdx
    jae .have_col
    ; No usable column: CPython prints the source line and no caret at all.
    mov qword [rbp - SH_HASCOL], 0
    xor ecx, ecx
    jmp .stash_col
.have_col:
    V_TO_I64 rcx
.stash_col:
    mov [rbp - SH_COL], rcx
    mov rax, [rbp - SH_TEXT]
    V_TEST_PTR rax, rcx
    ja .done
    test rax, rax
    jz .done
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .done

    ; Skip the leading whitespace, and take the caret's column with it.
    xor ecx, ecx
.skip:
    cmp rcx, [rax + PyStrObject.ob_size]
    jae .have_skip
    mov dl, [rax + PyStrObject.data + rcx]
    cmp dl, ' '
    je .skip_next
    cmp dl, 9
    jne .have_skip
.skip_next:
    inc rcx
    jmp .skip
.have_skip:
    mov [rbp - SH_I], rcx

    push rax
    CSTRING rdi, "    "
    call tb_write_cstr
    pop rax
    mov rdi, rax
    add rdi, PyStrObject.data
    add rdi, [rbp - SH_I]
    mov rsi, [rax + PyStrObject.ob_size]
    sub rsi, [rbp - SH_I]
    mov qword [rbp - SH_LEN], 0
    ; Trim the trailing newline; the caret line supplies its own.
    cmp rsi, 0
    jle .no_text
    cmp byte [rdi + rsi - 1], 10
    jne .write_text
    dec rsi
.write_text:
    mov [rbp - SH_LEN], rsi
    call tb_write
    CSTRING rdi, `\n`
    call tb_write_cstr
.no_text:

    ; The caret, under the column.  The offset is one-based and the leading
    ; whitespace has already been dropped, so both come off it.
    cmp qword [rbp - SH_HASCOL], 0
    je .done
    CSTRING rdi, "    "
    call tb_write_cstr
    mov rcx, [rbp - SH_COL]
    dec rcx
    sub rcx, [rbp - SH_I]
    jns .pad
    xor ecx, ecx
.pad:
    ; A caret past the end of the line is not a caret.  args[1][2] is whatever
    ; the raiser put there -- a Python program may raise SyntaxError with any
    ; offset it likes -- and the loop below writes one space per column.
    cmp rcx, [rbp - SH_LEN]
    jbe .pad_ok
    mov rcx, [rbp - SH_LEN]
.pad_ok:
    mov [rbp - SH_I], rcx
.pad_loop:
    cmp qword [rbp - SH_I], 0
    jle .caret
    CSTRING rdi, " "
    call tb_write_cstr
    dec qword [rbp - SH_I]
    jmp .pad_loop
.caret:
    CSTRING rdi, `^\n`
    call tb_write_cstr
.done:
    pop rbx
    leave
    ret
END_FUNC tb_syntax_header

section .bss
tb_seen:   resq TB_SEEN_MAX
tb_seen_n: resq 1
section .text

;; ============================================================================
;; (was src/except.asm)
;; ============================================================================

section .text

; exc_table_find_handler(PyCodeObject *code, int bytecode_offset_halfwords)
;   -> rax = handler target (in halfwords), rdx = stack depth, rcx = push_lasti
;   -> rax = -1 if no handler found
;
; bytecode_offset_halfwords = (rbx - &code.co_code) / 2
;
; rdi = code object
; esi = bytecode offset in instruction units (halfwords, i.e., 2-byte units)
DEF_FUNC exc_table_find_handler
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12d, esi           ; r12d = target offset (in instruction units)

    ; Get co_exceptiontable (PyBytesObject*)
    mov rax, [rdi + PyCodeObject.co_exceptiontable]
    test rax, rax
    jz .not_found

    ; Get table data pointer and size
    mov r13, [rax + PyBytesObject.ob_size]  ; r13 = table length
    test r13, r13
    jz .not_found
    lea r14, [rax + PyBytesObject.data]     ; r14 = table data start
    xor r15d, r15d                          ; r15 = current position in table

.scan_entry:
    cmp r15, r13
    jge .not_found

    ; Read start (unsigned varint)
    call .read_varint
    mov ebx, eax            ; ebx = start

    ; Read length (unsigned varint)
    ; NOTE: .read_varint clobbers ecx/esi, so use r8/r9 for length/target
    call .read_varint
    mov r8d, eax            ; r8d = length (safe from .read_varint)

    ; Read target (unsigned varint)
    call .read_varint
    mov r9d, eax            ; r9d = target (safe from .read_varint)

    ; Read depth_lasti (unsigned varint)
    call .read_varint
    ; eax = depth_lasti: depth = eax >> 1, push_lasti = eax & 1
    mov edi, eax            ; edi = depth_lasti

    ; Check if bytecode_offset is in range [start, start+length)
    cmp r12d, ebx
    jb .scan_entry           ; offset < start, try next
    lea edx, [ebx + r8d]    ; edx = start + length
    cmp r12d, edx
    jge .scan_entry          ; offset >= start + length, try next

    ; Found a matching handler!
    ; Return: rax = target, edx = depth, ecx = push_lasti
    mov eax, r9d            ; handler target in instruction units
    mov ecx, edi
    and ecx, 1              ; push_lasti flag
    shr edi, 1
    mov edx, edi            ; stack depth

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    mov rax, -1
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; Internal: read an unsigned varint from table at r14+r15
; Returns value in eax, advances r15
; Big-endian 6-bit chunks: first byte = high bits, bit 6 = continue
; Algorithm: val = b & 63; while (b & 64) { val <<= 6; b = next; val |= b & 63; }
.read_varint:
    cmp r15, r13
    jge .varint_zero         ; safety: don't read past end
    movzx edx, byte [r14 + r15]
    inc r15
    mov eax, edx
    and eax, 0x3F           ; initial value = bits 0-5 of first byte

.varint_loop:
    test edx, 0x40          ; check continue bit
    jz .varint_done
    cmp r15, r13
    jge .varint_done         ; safety
    shl eax, 6              ; shift accumulated value LEFT
    movzx edx, byte [r14 + r15]
    inc r15
    mov ecx, edx
    and ecx, 0x3F
    or eax, ecx             ; OR in new 6 bits at bottom
    jmp .varint_loop

.varint_zero:
    xor eax, eax
.varint_done:
    ret
END_FUNC exc_table_find_handler
