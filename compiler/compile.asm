; compile.asm - Driver for the Python source compiler
;
; Owns the lifetime of a compilation: set up the Comp state, normalize the
; source, run the passes, and dispose of everything however it ended.
;
; The disposal rule is the reason this file exists as a separate layer.  No
; pass may call raise_exception (see compiler.inc), so a failing pass records
; its error and returns 0; every buffer is released here, and only then is the
; recorded error turned into a real exception.  A compiler that raised from
; inside the parser would strand every allocation the parse had made.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "errcodes.inc"
%include "opcodes.inc"
%include "value.inc"
%include "compiler.inc"

extern ap_free
extern ap_malloc
extern ap_memcpy
extern ap_memset
extern buf_free
extern buf_init
extern buf_reserve
extern comp_error
extern exc_SyntaxError_type
extern exc_IndentationError_type
extern exc_TabError_type
extern tuple_new
extern obj_decref
extern obj_dealloc

extern lex_run
extern sym_build
extern sym_finalize
extern sym_free_all
extern str_new_heap

extern asm_assemble
extern ast_obj
extern ast_obj_at
extern cg_const
extern cg_emit
extern none_singleton
extern cg_expr
extern cg_unit_free
extern cg_unit_init
extern current_exception
extern exc_from_cstr
extern exc_set_context
extern obj_decref
extern par_expect
extern par_expr
extern par_module
extern cg_body
extern par_kind
extern par_advance
extern str_from_cstr_heap

; --- Named frame-layout constants ---
CI_COMP  equ 8
CI_SRC   equ 16
CI_LEN   equ 24
CI_FILE  equ 32
CI_MODE  equ 40
CI_FRAME equ 40          ; + 1 push = 48

section .text

;; ============================================================================
;; comp_src_normalize(const char *src, int64_t len, int64_t *out_len)
;;   -> char * (owned), NUL-terminated with CODE-style tail padding
;;
;; Three normalizations, all of which the rest of the compiler then gets to
;; ignore:
;;   - a UTF-8 BOM is dropped;
;;   - CRLF and a lone CR both become LF, so the lexer has one line ending;
;;   - the buffer carries 8 trailing NUL bytes, because the keyword matcher
;;     reads 8 bytes at once and the operator matcher 4, either of which can
;;     start on the last byte of the source.
;; ============================================================================
SN_OUT   equ 8
SN_DST   equ 16
SN_FRAME equ 24          ; + 3 pushes = 48
DEF_FUNC comp_src_normalize, SN_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - SN_OUT], rdx
    mov rbx, rdi                        ; source
    mov r12, rsi                        ; length

    ; Drop a UTF-8 byte-order mark.
    cmp r12, 3
    jb .no_bom
    cmp byte [rbx], 0xEF
    jne .no_bom
    cmp byte [rbx + 1], 0xBB
    jne .no_bom
    cmp byte [rbx + 2], 0xBF
    jne .no_bom
    add rbx, 3
    sub r12, 3
.no_bom:

    lea rdi, [r12 + 8]
    call ap_malloc
    mov [rbp - SN_DST], rax
    mov r13, rax                        ; write cursor

    xor ecx, ecx                        ; read index
.copy_loop:
    cmp rcx, r12
    jae .copy_done
    movzx eax, byte [rbx + rcx]
    cmp al, 13                          ; CR
    jne .copy_plain
    ; CRLF collapses to one LF; a lone CR becomes one LF.
    inc rcx
    cmp rcx, r12
    jae .emit_lf
    cmp byte [rbx + rcx], 10
    jne .emit_lf
    inc rcx
.emit_lf:
    mov byte [r13], 10
    inc r13
    jmp .copy_loop
.copy_plain:
    mov [r13], al
    inc r13
    inc rcx
    jmp .copy_loop
.copy_done:

    mov rax, r13
    sub rax, [rbp - SN_DST]
    mov rdx, [rbp - SN_OUT]
    mov [rdx], rax                      ; the normalized length

    mov rdi, r13                        ; zero the 8-byte tail
    xor esi, esi
    mov edx, 8
    call ap_memset

    mov rax, [rbp - SN_DST]
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC comp_src_normalize

;; ============================================================================
;; comp_init(Comp *c, const char *src, int64_t len, PyStrObject *filename,
;;           int mode)
;; The filename reference is borrowed; the caller outlives the compilation.
;; ============================================================================
DEF_FUNC comp_init, CI_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - CI_SRC], rsi
    mov [rbp - CI_LEN], rdx
    mov [rbp - CI_FILE], rcx
    mov [rbp - CI_MODE], r8

    mov rdi, rbx
    xor esi, esi
    mov edx, Comp_size
    call ap_memset

    mov rdi, [rbp - CI_SRC]
    mov rsi, [rbp - CI_LEN]
    lea rdx, [rbx + Comp.srclen]
    call comp_src_normalize
    mov [rbx + Comp.src], rax

    mov rax, [rbp - CI_FILE]
    mov [rbx + Comp.filename], rax
    mov rax, [rbp - CI_MODE]
    mov [rbx + Comp.mode], eax

    lea rdi, [rbx + Comp.tokens]
    mov esi, Token_size
    call buf_init
    lea rdi, [rbx + Comp.nodes]
    mov esi, AstNode_size
    call buf_init
    lea rdi, [rbx + Comp.children]
    mov esi, 4
    call buf_init
    lea rdi, [rbx + Comp.pending]
    mov esi, 4
    call buf_init
    lea rdi, [rbx + Comp.objs]
    mov esi, 8
    call buf_init
    lea rdi, [rbx + Comp.scopes]
    mov esi, Scope_size
    call buf_init

    ; Reserve node 0 as the null node, so a 0 index reads as "absent" without
    ; any call site needing a separate presence flag.
    lea rdi, [rbx + Comp.nodes]
    mov esi, 1
    call buf_reserve
    mov rdi, rax
    xor esi, esi
    mov edx, AstNode_size
    call ap_memset

    ; Reserve objs[0] for the same reason.  Without it the first literal in a
    ; compilation gets index 0, and every caller that tests an object index for
    ; zero reads it as a failure -- which is exactly what `import sys` hit.
    lea rdi, [rbx + Comp.objs]
    mov esi, 1
    call buf_reserve
    mov qword [rax], 0

    pop rbx
    leave
    ret
END_FUNC comp_init

;; ============================================================================
;; comp_free(Comp *c)
;; Releases everything a compilation allocated, on the success and failure
;; paths alike.  comp.objs is the only refcounted holding, which is exactly why
;; every PyObject the front end builds is put there.
;; ============================================================================
DEF_FUNC comp_free, 8
    push rbx
    push r12
    push r13
    mov rbx, rdi

    ; comp.objs holds Values, not pointers.  A small int and a float are
    ; immediates rather than heap objects, so releasing them has to go through
    ; DECREF_V -- which is NULL-safe and a no-op for anything that is not a
    ; pointer.  Using obj_decref here would treat the bits of 1.5 as an
    ; address.
    mov r12, [rbx + Comp.objs + Buf.data]
    mov r13, [rbx + Comp.objs + Buf.len]
.obj_loop:
    test r13, r13
    jz .objs_done
    dec r13
    mov rax, [r12 + r13*8]
    DECREF_V rax, rcx
    mov r12, [rbx + Comp.objs + Buf.data]   ; obj_dealloc clobbers caller-saved
    jmp .obj_loop
.objs_done:

    mov rdi, rbx
    call sym_free_all
    lea rdi, [rbx + Comp.scopes]
    call buf_free
    lea rdi, [rbx + Comp.objs]
    call buf_free
    lea rdi, [rbx + Comp.pending]
    call buf_free
    lea rdi, [rbx + Comp.children]
    call buf_free
    lea rdi, [rbx + Comp.nodes]
    call buf_free
    lea rdi, [rbx + Comp.tokens]
    call buf_free

    mov rdi, [rbx + Comp.src]
    call ap_free
    mov qword [rbx + Comp.src], 0

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC comp_free

;; ============================================================================
;; comp_intern(const char *s, int64_t len) -> rax = owned PyStrObject*
;;
;; Every identifier and string the front end builds goes through here.  It is a
;; single entry point on purpose: a PyStrObject carries two lengths -- ob_size
;; in bytes and ob_length in code points -- and a constructor that set only the
;; first would give non-ASCII identifiers and literals a silently wrong len().
;; str_new_heap sets both.
;;
;; No dedup yet.  Repeated identifiers each get their own object; the name and
;; const tables downstream do the deduplication that actually matters, because
;; that is what decides co_names indices.
;; ============================================================================
DEF_FUNC_BARE comp_intern
    jmp str_new_heap
END_FUNC comp_intern

;; ============================================================================
;; comp_set_pending(Comp *c)
;;
;; Turn the recorded error into a real exception object and make it the pending
;; one -- WITHOUT unwinding.  raise_exception would tail-jump into
;; eval_exception_unwind, which calls fatal_error when there is no interpreter
;; frame, and `./apython foo.py` compiles before any frame exists.  Setting
;; current_exception directly leaves the caller free to clean up and return
;; a failure the ordinary way.
;; ============================================================================
DEF_FUNC comp_set_pending, 8
    push rbx
    mov rbx, rdi
    cmp dword [rbx + Comp.err + CompErr.set], 0
    je .none

    mov rdi, [rbx + Comp.err + CompErr.type]
    mov rsi, [rbx + Comp.err + CompErr.msg]
    call exc_from_cstr

    ; A syntax error carries where it happened, in CPython's shape:
    ; args = (msg, (filename, lineno, offset, text)).  The traceback printer
    ; reads that tuple to produce the File/line/caret block, and str() reads it
    ; for the "(file, line N)" suffix; without it a syntax error is a bare
    ; message with nothing to locate it by.
    push rax
    mov rdi, rbx
    mov rsi, rax
    call comp_attach_location
    pop rax

    ; Chain onto whatever was already being handled, as a raise would.
    mov rsi, [rel current_exception]
    test rsi, rsi
    jz .no_prev
    push rax
    mov rdi, rax
    call exc_set_context
    mov rdi, [rel current_exception]
    call obj_decref
    pop rax
.no_prev:
    mov [rel current_exception], rax
.none:
    pop rbx
    leave
    ret
END_FUNC comp_set_pending

;; ============================================================================
;; par_eval_root(Comp *c) -> rax = the root expression node, 0 on error
;; The `eval` start symbol: one expression, then optional blank lines, then end
;; of input.  Trailing junk is an error here rather than being ignored, which
;; is what makes eval("1 2") a syntax error.
;; ============================================================================
DEF_FUNC par_eval_root, 16      ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi

    mov esi, 0                          ; BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov r12, rax

.skip_newlines:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NEWLINE
    jne .at_end
    mov rdi, rbx
    call par_advance
    jmp .skip_newlines

.at_end:
    mov rdi, rbx
    mov esi, TOK_ENDMARKER
    CSTRING rdx, "invalid syntax"
    call par_expect
    test eax, eax
    jz .fail
    mov rax, r12
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_eval_root

;; ============================================================================
;; compile_source(const char *src, int64_t len, PyStrObject *filename, int mode)
;;   -> rax = PyCodeObject*, or 0 with the exception already pending
;;
;; The whole pipeline, and the only place that disposes of it.  Every pass
;; below returns a failure rather than raising, so there is exactly one exit
;; path that frees the buffers and exactly one that turns a recorded error into
;; an exception.
;; ============================================================================
CS_SRC   equ 8
CS_LEN   equ 16
CS_FILE  equ 24
CS_MODE  equ 32
CS_ROOT  equ 40
CS_CODE  equ 48
; The CompUnit is a large struct living in this frame, so its offset is derived
; from its size rather than written out: a hand-picked number silently overlaps
; the scalar slots above the first time the struct grows, and the symptom is a
; field reading as garbage rather than anything that looks like a layout bug.
CS_UNIT  equ 48 + CompUnit_size
CS_COMP  equ CS_UNIT + 8
CS_FRAME equ ((CS_COMP + 15) / 16) * 16 + 8      ; + 3 pushes = 16-aligned
DEF_FUNC compile_source, CS_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - CS_SRC], rdi
    mov [rbp - CS_LEN], rsi
    mov [rbp - CS_FILE], rdx
    mov [rbp - CS_MODE], rcx
    mov qword [rbp - CS_CODE], 0

    mov edi, Comp_size
    call ap_malloc
    mov rbx, rax
    mov [rbp - CS_COMP], rax

    mov rdi, rbx
    mov rsi, [rbp - CS_SRC]
    mov rdx, [rbp - CS_LEN]
    mov rcx, [rbp - CS_FILE]
    mov r8, [rbp - CS_MODE]
    call comp_init

    lea r12, [rbp - CS_UNIT]
    mov rdi, r12
    mov rsi, [rbp - CS_FILE]
    lea rdx, [rel cs_module_name]
    call cs_unit_setup

    mov rdi, rbx
    xor esi, esi                        ; the whole source
    xor edx, edx
    xor ecx, ecx
    call lex_run
    test eax, eax
    jz .failed

    cmp qword [rbp - CS_MODE], CMODE_EVAL
    je .parse_eval
    mov rdi, rbx
    call par_module
    jmp .parsed
.parse_eval:
    mov rdi, rbx
    call par_eval_root
.parsed:
    test rax, rax
    jz .failed
    mov [rbp - CS_ROOT], rax

    ; The symbol table has to run before anything is emitted: it is what
    ; decides whether each name is a fast local, a cell, a free variable or a
    ; global, and no emitter can answer that from the syntax alone.
    mov rdi, rbx
    mov rsi, [rbp - CS_ROOT]
    mov rdx, [rbp - CS_MODE]
    call sym_build
    test eax, eax
    jz .failed
    mov [r12 + CompUnit.scope], eax
    mov [r12 + CompUnit.comp], rbx
    mov [rbx + Comp.cur_scope], eax
    mov rdi, rbx
    mov rsi, rax
    xor edx, edx                        ; a module has no parameters
    call sym_finalize
    test eax, eax
    jz .failed

    ; RESUME must be the first instruction of every code object: the frame
    ; setup, the eval breaker and the tracing hook all key off it.  It carries
    ; no source position, hence IF_NOLINE.
    mov rdi, r12
    mov esi, OP_RESUME
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE

    cmp qword [rbp - CS_MODE], CMODE_EVAL
    je .gen_eval

    ; A leading string literal is the module's __doc__, not a statement whose
    ; value is thrown away.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CS_ROOT]
    extern cg_docstring
    call cg_docstring
    test eax, eax
    jz .failed

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CS_ROOT]
    call cg_body
    test eax, eax
    jz .failed
    ; A module body has no value of its own: it always returns None, and exec()
    ; discards even that.
    mov rdi, r12
    mov rsi, [rbp - CS_ROOT]
    call cs_return_none
    jmp .assemble

.gen_eval:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CS_ROOT]
    call cg_expr
    test eax, eax
    jz .failed
    ; An eval-mode code object returns the expression's own value.
    mov rdi, r12
    mov esi, OP_RETURN_VALUE
    xor edx, edx
    mov ecx, 1
    call cg_emit

.assemble:

    mov rdi, rbx
    mov rsi, r12
    call asm_assemble
    mov [rbp - CS_CODE], rax
    test rax, rax
    jz .failed

.cleanup:
    mov rdi, r12
    call cg_unit_free
    mov rdi, [rbp - CS_UNIT + CompUnit.name]
    call obj_decref
    mov rdi, rbx
    call comp_free
    mov rdi, rbx
    call ap_free
    mov rax, [rbp - CS_CODE]
    pop r13
    pop r12
    pop rbx
    leave
    ret
.failed:
    mov rdi, rbx
    call comp_set_pending
    mov qword [rbp - CS_CODE], 0
    jmp .cleanup
END_FUNC compile_source

;; ============================================================================
;; cs_unit_setup(CompUnit *u, PyStrObject *filename, const char *name)
;; Wraps cg_unit_init, interning the code object's name.  The unit borrows the
;; filename from the caller and owns the name it makes here.
;; ============================================================================
DEF_FUNC cs_unit_setup, 16      ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    mov rdi, rdx
    call str_from_cstr_heap
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_unit_init
    pop r12
    pop rbx
    leave
    ret
END_FUNC cs_unit_setup

;; ============================================================================
;; cs_return_none(CompUnit *u, uint32_t root)
;; The implicit `return None` every module body ends with.
;; ============================================================================
DEF_FUNC cs_return_none, 8
    push rbx
    mov rbx, rdi
    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, rbx
    call cg_const
    mov rdx, rax
    mov rdi, rbx
    mov esi, OP_RETURN_CONST
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
    pop rbx
    leave
    ret
END_FUNC cs_return_none

;; ============================================================================
;; comp_intern_cstr(Comp *c, const char *s) -> rax = a borrowed PyStrObject*
;;
;; For names the compiler invents rather than reads from the source --
;; __module__, __qualname__, the leading component of a dotted import.  They go
;; into comp.objs like every other literal, because CompUnit.names holds
;; BORROWED references: a string created and released at the call site leaves a
;; dangling pointer in co_names, and the failure surfaces as a wild jump inside
;; dict_lookup at run time.
;; ============================================================================
DEF_FUNC comp_intern_cstr, 16
    push rbx
    push r12
    mov rbx, rdi
    mov rdi, rsi
    call str_from_cstr_heap
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov rdi, rbx
    mov rsi, rax
    call ast_obj_at
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC comp_intern_cstr

;; ============================================================================
;; comp_intern_keep(Comp *c, const char *s, int64_t len)
;;   -> rax = a borrowed PyStrObject*
;;
;; comp_intern_cstr for a counted slice: the components of a dotted import
;; name.  Same reason it exists -- CompUnit.names holds BORROWED references,
;; so a string created and released at the call site leaves a dangling pointer
;; in co_names and the failure surfaces as a wild jump inside dict_lookup.
;; ============================================================================
DEF_FUNC comp_intern_keep, 16
    push rbx
    push r12
    mov rbx, rdi
    mov rdi, rsi
    mov rsi, rdx
    call comp_intern
    test rax, rax
    jz .ik_fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov rdi, rbx
    mov rsi, rax
    call ast_obj_at
    pop r12
    pop rbx
    leave
    ret
.ik_fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC comp_intern_keep

;; ============================================================================
;; comp_empty_string(Comp *c) -> rax = a borrowed empty PyStrObject*
;; `from . import x` has no module name, but IMPORT_NAME still needs one.
;; ============================================================================
DEF_FUNC comp_empty_string, 8
    push rbx
    mov rbx, rdi
    lea rdi, [rel cs_empty]
    call str_from_cstr_heap
    mov rdi, rbx
    mov rsi, rax
    call ast_obj                        ; parked in comp.objs, so it is freed
    mov rdi, rbx
    mov rsi, rax
    call ast_obj_at
    pop rbx
    leave
    ret
END_FUNC comp_empty_string

section .rodata
cs_empty: db "", 0

cs_module_name: db "<module>", 0


section .text

;; ============================================================================
;; comp_lex_span(Comp *c, const char *start, const char *end, int lineno)
;;   -> rax = the token index the span's tokens start at, or -1
;;
;; Appends a span's tokens to the array and hands back where they begin, so a
;; caller can point the parser's cursor at them.  The lexer state is saved and
;; restored around it: an f-string's field is lexed in the middle of a file
;; whose own indent stack and paren depth must survive.
;; ============================================================================
CLS_COMP  equ 8
CLS_START equ 16
CLS_END   equ 24
CLS_LINE  equ 32
CLS_IDX   equ 40
CLS_SAVE  equ 48 + Lexer_size
CLS_FRAME equ ((CLS_SAVE + 15) / 16) * 16 + 8      ; + 1 push = 16-aligned
DEF_FUNC comp_lex_span, CLS_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - CLS_START], rsi
    mov [rbp - CLS_END], rdx
    mov [rbp - CLS_LINE], rcx

    mov rax, [rbx + Comp.tokens + Buf.len]
    mov [rbp - CLS_IDX], rax

    lea rdi, [rbp - CLS_SAVE]
    lea rsi, [rbx + Comp.lex]
    mov edx, Lexer_size
    call ap_memcpy

    mov rdi, rbx
    mov rsi, [rbp - CLS_START]
    mov rdx, [rbp - CLS_END]
    mov rcx, [rbp - CLS_LINE]
    call lex_run
    push rax
    lea rdi, [rbx + Comp.lex]
    lea rsi, [rbp - CLS_SAVE]
    mov edx, Lexer_size
    call ap_memcpy
    pop rax
    test eax, eax
    jz .fail
    mov rax, [rbp - CLS_IDX]
    pop rbx
    leave
    ret
.fail:
    mov rax, -1
    pop rbx
    leave
    ret
END_FUNC comp_lex_span

;; ============================================================================
;; comp_attach_location(Comp *c, PyExceptionObject *exc)
;; Replace the exception's args with (msg, (filename, lineno, offset, text)).
;; Best-effort: on any allocation failure the bare message is left alone.
;; ============================================================================
AL_COMP  equ 8
AL_EXC   equ 16
AL_INNER equ 24
AL_OUTER equ 32
AL_TEXT  equ 40
AL_FRAME equ 56           ; + 3 pushes = 80
DEF_FUNC comp_attach_location, AL_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi

    ; Only the syntax errors carry a position.
    mov rax, [rbx + Comp.err + CompErr.type]
    lea rcx, [rel exc_SyntaxError_type]
    cmp rax, rcx
    je .go
    lea rcx, [rel exc_IndentationError_type]
    cmp rax, rcx
    je .go
    lea rcx, [rel exc_TabError_type]
    cmp rax, rcx
    jne .done
.go:
    mov edi, 4
    call tuple_new
    test rax, rax
    jz .done
    mov [rbp - AL_INNER], rax

    mov rdx, [rax + PyTupleObject.ob_item]
    mov rcx, [rbx + Comp.filename]
    test rcx, rcx
    jnz .have_file
    lea rcx, [rel none_singleton]
.have_file:
    INCREF rcx
    mov [rdx], rcx

    mov rsi, [rbp - AL_INNER]
    mov rsi, [rsi + PyTupleObject.ob_item]
    mov ecx, [rbx + Comp.err + CompErr.lineno]
    V_PACK_I64 rcx, rdx
    mov [rsi + 8], rcx
    ; CPython's offset is one-based; the column recorded here is not.
    mov ecx, [rbx + Comp.err + CompErr.col]
    inc rcx
    V_PACK_I64 rcx, rdx
    mov [rsi + 16], rcx

    mov rdi, rbx
    mov esi, [rbx + Comp.err + CompErr.lineno]
    call comp_line_text
    test rax, rax
    jnz .have_text
    ; A slot in a tuple must hold a real Value: None, not NULL.  A NULL there
    ; is not an empty string, it is a hole that anything reading the tuple
    ; walks straight into.
    lea rax, [rel none_singleton]
    INCREF rax
.have_text:
    mov rdx, [rbp - AL_INNER]
    mov rdx, [rdx + PyTupleObject.ob_item]
    mov [rdx + 24], rax

    mov edi, 2
    call tuple_new
    test rax, rax
    jz .free_inner
    mov [rbp - AL_OUTER], rax
    mov rdx, [rax + PyTupleObject.ob_item]

    ; args[0] is the message the exception already carries.
    mov rcx, [r12 + PyExceptionObject.exc_args]
    test rcx, rcx
    jz .no_msg
    cmp qword [rcx + PyTupleObject.ob_size], 0
    jle .no_msg
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov rcx, [rcx]
    INCREF_V rcx, r8
    mov [rdx], rcx
    jmp .have_msg
.no_msg:
    mov qword [rdx], 0
.have_msg:
    mov rcx, [rbp - AL_INNER]
    mov [rdx + 8], rcx

    mov rdi, [r12 + PyExceptionObject.exc_args]
    mov rax, [rbp - AL_OUTER]
    mov [r12 + PyExceptionObject.exc_args], rax
    test rdi, rdi
    jz .done
    call obj_decref
    jmp .done

.free_inner:
    mov rdi, [rbp - AL_INNER]
    call obj_decref
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC comp_attach_location

;; ============================================================================
;; comp_line_text(Comp *c, int lineno) -> PyStrObject*, or 0
;; The source of one line, newline included, as CPython's SyntaxError.text is.
;; ============================================================================
LT_COMP  equ 8
LT_LINE  equ 16
LT_POS   equ 24
LT_START equ 32
LT_FRAME equ 40           ; + 1 push = 48
DEF_FUNC comp_line_text, LT_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - LT_LINE], rsi
    cmp rsi, 1
    jl .none
    mov rax, [rbx + Comp.src]
    test rax, rax
    jz .none

    ; Walk to the start of the wanted line.
    xor ecx, ecx                        ; byte position
    mov edx, 1                          ; current line number
.scan:
    cmp rdx, [rbp - LT_LINE]
    jae .found
    cmp rcx, [rbx + Comp.srclen]
    jae .none
    cmp byte [rax + rcx], 10
    jne .scan_next
    inc rdx
.scan_next:
    inc rcx
    jmp .scan
.found:
    mov [rbp - LT_START], rcx
    ; And to its end, keeping the newline the way CPython does.
.end_scan:
    cmp rcx, [rbx + Comp.srclen]
    jae .have_end
    inc rcx
    cmp byte [rax + rcx - 1], 10
    jne .end_scan
.have_end:
    mov rdx, rcx
    sub rdx, [rbp - LT_START]
    jz .none
    add rax, [rbp - LT_START]
    mov rdi, rax
    mov rsi, rdx
    call str_new_heap
    pop rbx
    leave
    ret
.none:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC comp_line_text

ASM_INIT
