; codegen_func.asm - Functions, lambdas and closures
;
; A nested function gets its own CompUnit, is assembled to a complete code
; object, and is stored as a constant of the enclosing one; MAKE_FUNCTION turns
; that constant into a function at run time.
;
; Closures are the part with no room for approximation.  A local that some
; nested block reads has to live in a cell so both frames share one storage
; location, and three opcodes have to agree about where those cells are:
; MAKE_CELL boxes a slot in place, LOAD_CLOSURE reads a cell out of the
; enclosing frame, and COPY_FREE_VARS writes the incoming ones into the LAST
; nfree slots of localsplus -- it computes the destination as
; nlocalsplus - oparg and nothing else tells it where they are.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern ast_obj_at
extern asm_assemble
extern cg_body
extern cg_const
extern cg_emit
extern cg_expr
extern cg_name
extern cg_unit_free
extern cg_unit_init
extern comp_error
extern cg_unwind_finallys
extern comp_intern
extern comp_intern_cstr
extern obj_decref
extern str_from_cstr_heap
extern str_type
extern sym_at
extern cg_call_args_only
extern sym_finalize
extern sym_scope_of
extern sym_flags_of
extern sym_is_function_like
extern sym_lp_index
extern sym_scope_of

extern exc_SyntaxError_type

; --- Named frame-layout constants ---
CF_COMP   equ 8
CF_PARENT equ 16
CF_NODE   equ 24
CF_SCOPE  equ 32
CF_LINE   equ 40
CF_ARGS   equ 48
CF_CODE   equ 56
CF_NAME   equ 64
CF_I      equ 72
CF_N      equ 80
CF_FLAGS  equ 88
CF_UNIT   equ 96 + CompUnit_size
CF_FRAME  equ ((CF_UNIT + 15) / 16) * 16 + 8      ; + 3 pushes = 16-aligned

section .text

;; ============================================================================
;; cg_nameop(Comp *c, CompUnit *u, PyStrObject *name, int ctx, int want_null)
;;   -> rax = 1 ok, 0 error
;;
;; The single place a name turns into an opcode.  Everything it needs comes
;; from the symbol table, because nothing about the syntax says which of
;; LOAD_FAST, LOAD_NAME, LOAD_GLOBAL or LOAD_DEREF is right: `x` in a function
;; is a fast local if the function assigns it anywhere at all, including after
;; the use.
;;
;; The negative rule matters as much as the positive ones.  Module and class
;; blocks are not function-like, so a local there is still LOAD_NAME -- it
;; lives in the frame's locals mapping, which is the dict exec(src, d) passes.
;; ============================================================================
CN2_COMP  equ 8
CN2_UNIT  equ 16
CN2_NAME  equ 24
CN2_CTX   equ 32
CN2_NULL  equ 40
CN2_SCOPE equ 48
CN2_SLOT  equ 56
CN2_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC cg_nameop, CN2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - CN2_NAME], rdx
    mov [rbp - CN2_CTX], rcx
    mov [rbp - CN2_NULL], r8

    mov r13d, [r12 + CompUnit.scope]
    mov [rbp - CN2_SCOPE], r13

    mov rdi, rbx
    mov rsi, r13
    mov rdx, [rbp - CN2_NAME]
    call sym_scope_of
    mov r8d, eax

    cmp r8d, SYM_CELL
    je .deref
    cmp r8d, SYM_FREE
    je .deref
    cmp r8d, SYM_GLOBAL_EXPLICIT
    je .global
    cmp r8d, SYM_LOCAL
    je .maybe_fast
    ; SYM_GLOBAL_IMPLICIT, or a name the table never saw.
    mov rdi, rbx
    mov rsi, r13
    call sym_is_function_like
    test eax, eax
    jnz .global
    jmp .by_name

.maybe_fast:
    mov rdi, rbx
    mov rsi, r13
    call sym_is_function_like
    test eax, eax
    jz .by_name

    ; A fast local: the oparg is its localsplus slot.
    mov rdi, rbx
    mov rsi, [rbp - CN2_SCOPE]
    mov rdx, [rbp - CN2_NAME]
    call sym_lp_index
    cmp eax, -1
    je .missing
    mov edx, eax
    cmp qword [rbp - CN2_CTX], CTX_LOAD
    jne .fast_store
    ; A local that is deleted anywhere in the block -- or bound by an
    ; `except E as e`, which deletes it at the end of the clause -- may be
    ; empty here.  LOAD_FAST would hand back whatever the slot holds; only
    ; LOAD_FAST_CHECK raises for it.
    mov [rbp - CN2_SLOT], rdx
    mov rdi, rbx
    mov rsi, [rbp - CN2_SCOPE]
    mov rdx, [rbp - CN2_NAME]
    call sym_flags_of
    mov esi, OP_LOAD_FAST
    test eax, DEF_UNBOUND
    jz .fast_load
    mov esi, OP_LOAD_FAST_CHECK
.fast_load:
    mov rdx, [rbp - CN2_SLOT]
    jmp .emit
.fast_store:
    mov esi, OP_STORE_FAST
    cmp qword [rbp - CN2_CTX], CTX_STORE
    je .emit
    mov esi, OP_DELETE_FAST
    jmp .emit

.deref:
    mov rdi, rbx
    mov rsi, [rbp - CN2_SCOPE]
    mov rdx, [rbp - CN2_NAME]
    call sym_lp_index
    cmp eax, -1
    je .missing
    mov edx, eax
    mov esi, OP_LOAD_DEREF
    cmp qword [rbp - CN2_CTX], CTX_LOAD
    je .emit
    mov esi, OP_STORE_DEREF
    cmp qword [rbp - CN2_CTX], CTX_STORE
    je .emit
    mov esi, OP_DELETE_DEREF
    jmp .emit

.global:
    mov rdi, r12
    mov rsi, [rbp - CN2_NAME]
    call cg_name
    mov edx, eax
    mov esi, OP_STORE_GLOBAL
    cmp qword [rbp - CN2_CTX], CTX_STORE
    je .emit
    mov esi, OP_DELETE_GLOBAL
    cmp qword [rbp - CN2_CTX], CTX_DEL
    je .emit
    ; LOAD_GLOBAL's oparg is the name index shifted left, with bit 0 asking the
    ; interpreter to push a NULL alongside -- which is how a call's empty self
    ; slot gets filled without a separate PUSH_NULL.
    shl edx, 1
    or edx, [rbp - CN2_NULL]
    mov esi, OP_LOAD_GLOBAL
    jmp .emit

.by_name:
    mov rdi, r12
    mov rsi, [rbp - CN2_NAME]
    call cg_name
    mov edx, eax
    mov esi, OP_LOAD_NAME
    cmp qword [rbp - CN2_CTX], CTX_LOAD
    je .emit
    mov esi, OP_STORE_NAME
    cmp qword [rbp - CN2_CTX], CTX_STORE
    je .emit
    mov esi, OP_DELETE_NAME

.emit:
    mov rdi, r12
    xor ecx, ecx
    mov ecx, [r12 + CompUnit.curline]
    call cg_emit
    mov eax, 1
    jmp .ret

.missing:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "internal error: name has no local slot"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_nameop

;; ============================================================================
;; cg_function(Comp *c, CompUnit *parent, uint32_t node, int is_lambda)
;;   -> rax = 1 ok, 0 error
;;
;; Compile a nested function into its own code object, push it as a constant of
;; the enclosing unit, and emit MAKE_FUNCTION.  The order the operands are
;; pushed in is fixed by the opcode: defaults, keyword defaults, annotations,
;; closure, then the code object on top.
;; ============================================================================
DEF_FUNC cg_function, CF_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - CF_PARENT], rsi
    mov r13, rdx
    mov [rbp - CF_NODE], rdx
    mov [rbp - CF_FLAGS], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    movzx ecx, word [rax + AstNode.flags]   ; the scope the symbol table made
    mov [rbp - CF_SCOPE], rcx
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CF_LINE], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - CF_ARGS], rcx

    ; Fix this scope's variable layout before anything is emitted into it.
    mov rax, [rbp - CF_SCOPE]
    mov [rbx + Comp.cur_scope], eax
    mov rdi, rbx
    mov rsi, [rbp - CF_SCOPE]
    mov rdx, [rbp - CF_ARGS]
    call sym_finalize
    test eax, eax
    jz .fail

    ; --- defaults are evaluated HERE, in the enclosing scope ---
    mov rdi, rbx
    mov rsi, [rbp - CF_PARENT]
    mov rdx, [rbp - CF_ARGS]
    call cg_defaults
    cmp rax, -1
    je .fail
    mov [rbp - CF_I], rax               ; the MAKE_FUNCTION flag bits so far

    ; --- the closure tuple, if the body captured anything ---
    mov rdi, rbx
    mov rsi, [rbp - CF_PARENT]
    mov rdx, [rbp - CF_SCOPE]
    mov rcx, [rbp - CF_LINE]
    call cg_closure_tuple
    cmp rax, -1
    je .fail
    or [rbp - CF_I], rax

    ; --- compile the body into a unit of its own ---
    mov rdi, rbx
    lea rsi, [rbp - CF_UNIT]
    mov rdx, r13
    mov rcx, [rbp - CF_FLAGS]
    call cg_compile_body
    mov [rbp - CF_CODE], rax
    test rax, rax
    jz .fail

    ; The finished code object becomes a constant of the enclosing unit.
    mov rdi, [rbp - CF_PARENT]
    mov rsi, [rbp - CF_CODE]
    call cg_const
    mov rdx, rax
    mov rdi, [rbp - CF_PARENT]
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CF_LINE]
    call cg_emit

    mov rdi, [rbp - CF_PARENT]
    mov esi, OP_MAKE_FUNCTION
    mov rdx, [rbp - CF_I]
    mov rcx, [rbp - CF_LINE]
    call cg_emit

    ; Restore the enclosing scope for whatever follows.
    mov rax, [rbp - CF_PARENT]
    mov eax, [rax + CompUnit.scope]
    mov [rbx + Comp.cur_scope], eax
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_function

;; ============================================================================
;; cg_defaults(Comp *c, CompUnit *u, uint32_t args) -> rax = MAKE_FUNCTION bits
;;   or -1 on error.
;;
;; Positional defaults become one tuple; keyword-only defaults become a dict of
;; name to value.  Both are built in the DEFINING scope, which is why
;; `def f(x=n)` captures n as it is now rather than at call time.
;; ============================================================================
CD2_COMP  equ 8
CD2_UNIT  equ 16
CD2_ARGS  equ 24
CD2_I     equ 32
CD2_N     equ 40
CD2_NPOS  equ 48
CD2_NKW   equ 56
CD2_LINE  equ 64
CD2_BITS  equ 72
CD2_EXTRA equ 80
CD2_FRAME equ 88          ; + 3 pushes = 112
DEF_FUNC cg_defaults, CD2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    ; The unit and the arguments node go in frame slots, not registers: r12 is
    ; reused below as a counter and r13 as a scratch node.
    mov [rbp - CD2_UNIT], rsi
    mov [rbp - CD2_ARGS], rdx
    mov r13, rdx
    mov qword [rbp - CD2_BITS], 0
    test r13, r13
    jz .done

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CD2_LINE], rcx
    mov ecx, [rax + AstNode.a]          ; the AST_EXTRA counts node
    mov [rbp - CD2_EXTRA], rcx
    mov rdi, rbx
    mov rsi, rcx
    call ast_at
    mov ecx, [rax + AstNode.a]
    mov [rbp - CD2_NPOS], rcx
    mov ecx, [rax + AstNode.c]
    mov [rbp - CD2_NKW], rcx

    ; --- positional defaults, as a tuple ---
    xor r12d, r12d                      ; how many there are
    mov qword [rbp - CD2_I], 0
.pos_loop:
    mov rax, [rbp - CD2_I]
    cmp rax, [rbp - CD2_NPOS]
    jae .pos_done
    call .arg_at
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov ecx, [rax + AstNode.c]
    test ecx, ecx
    jz .pos_next                        ; no default on this parameter
    mov edx, ecx
    mov rdi, rbx
    mov rsi, [rbp - CD2_UNIT]
    call cg_expr
    test eax, eax
    jz .fail
    inc r12
.pos_next:
    inc qword [rbp - CD2_I]
    jmp .pos_loop
.pos_done:
    test r12, r12
    jz .kwdefaults
    mov rdi, [rbp - CD2_UNIT]
    mov esi, OP_BUILD_TUPLE
    mov rdx, r12
    mov rcx, [rbp - CD2_LINE]
    call cg_emit
    or qword [rbp - CD2_BITS], MAKE_FUNC_DEFAULTS

.kwdefaults:
    ; --- keyword-only defaults, as a dict of name to value ---
    xor r12d, r12d
    mov rax, [rbp - CD2_NPOS]
    mov [rbp - CD2_I], rax
    mov rcx, [rbp - CD2_NPOS]
    add rcx, [rbp - CD2_NKW]
    mov [rbp - CD2_N], rcx
.kw_loop:
    mov rax, [rbp - CD2_I]
    cmp rax, [rbp - CD2_N]
    jae .kw_done
    call .arg_at
    mov r13, rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov ecx, [rax + AstNode.c]
    test ecx, ecx
    jz .kw_next
    ; the parameter's name, as a constant
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, [rbp - CD2_UNIT]
    mov rsi, rax
    call cg_const
    mov rdx, rax
    mov rdi, [rbp - CD2_UNIT]
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CD2_LINE]
    call cg_emit
    ; then its default
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.c]
    mov rdi, rbx
    mov rsi, [rbp - CD2_UNIT]
    call cg_expr
    test eax, eax
    jz .fail
    inc r12
.kw_next:
    inc qword [rbp - CD2_I]
    jmp .kw_loop
.kw_done:
    test r12, r12
    jz .done
    mov rdi, [rbp - CD2_UNIT]
    mov esi, OP_BUILD_MAP
    mov rdx, r12
    mov rcx, [rbp - CD2_LINE]
    call cg_emit
    or qword [rbp - CD2_BITS], MAKE_FUNC_KWDEFAULTS

.done:
    mov rax, [rbp - CD2_BITS]
    jmp .ret
.fail:
    mov rax, -1
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret

; Local: the i'th parameter node of the arguments list.
.arg_at:
    sub rsp, 8
    mov rdi, rbx
    mov rsi, [rbp - CD2_ARGS]
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CD2_I]
    mov rdi, rbx
    call ast_child
    add rsp, 8
    ret
END_FUNC cg_defaults

;; ============================================================================
;; cg_closure_tuple(Comp *c, CompUnit *u, uint32_t scope, int line)
;;   -> rax = MAKE_FUNC_CLOSURE, 0 when the body captured nothing, -1 on error
;;
;; One LOAD_CLOSURE per free variable, in the child's freevars order, built
;; into a tuple.  The slot each one is read from is the ENCLOSING scope's --
;; where the name is a cell -- while the order is the child's, because
;; COPY_FREE_VARS drops them into the child's last nfree slots positionally.
;; ============================================================================
CC3_COMP  equ 8
CC3_UNIT  equ 16
CC3_SCOPE equ 24
CC3_LINE  equ 32
CC3_I     equ 40
CC3_N     equ 48
CC3_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC cg_closure_tuple, CC3_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - CC3_SCOPE], rdx
    mov [rbp - CC3_LINE], rcx

    mov rdi, rbx
    mov rsi, [rbp - CC3_SCOPE]
    call sym_at
    mov r13, rax
    mov rcx, [r13 + Scope.freevars + Buf.len]
    mov [rbp - CC3_N], rcx
    test rcx, rcx
    jz .none

    mov qword [rbp - CC3_I], 0
.loop:
    mov rax, [rbp - CC3_I]
    cmp rax, [rbp - CC3_N]
    jae .build
    mov rdi, rbx
    mov rsi, [rbp - CC3_SCOPE]
    call sym_at
    mov rdx, [rax + Scope.freevars + Buf.data]
    mov rcx, [rbp - CC3_I]
    mov rdx, [rdx + rcx*8]              ; the name
    mov rdi, rbx
    mov esi, [r12 + CompUnit.scope]     ; look it up in the ENCLOSING scope
    call sym_lp_index
    cmp eax, -1
    je .not_found
    mov edx, eax
    mov rdi, r12
    mov esi, OP_LOAD_CLOSURE
    mov rcx, [rbp - CC3_LINE]
    call cg_emit
    inc qword [rbp - CC3_I]
    jmp .loop
.build:
    mov rdi, r12
    mov esi, OP_BUILD_TUPLE
    mov rdx, [rbp - CC3_N]
    mov rcx, [rbp - CC3_LINE]
    call cg_emit
    mov eax, MAKE_FUNC_CLOSURE
    jmp .ret
.none:
    xor eax, eax
    jmp .ret
.not_found:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "internal error: free variable has no cell in the enclosing scope"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    mov rax, -1
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_closure_tuple


;; ============================================================================
;; cg_compile_body(Comp *c, CompUnit *u, uint32_t node, int is_lambda)
;;   -> rax = PyCodeObject*, or 0
;;
;; The nested unit's prologue is fixed and its order matters:
;;
;;     MAKE_CELL i         for each cell, in ascending localsplus order
;;     COPY_FREE_VARS n    if the body captured anything
;;     RESUME 0
;;
;; MAKE_CELL boxes whatever is already in the slot, so a parameter that is also
;; a cell is wrapped in place after func_call has bound it -- it is not moved.
;; COPY_FREE_VARS writes into the last n slots and derives that from
;; nlocalsplus, which is why the layout puts free variables last.
;; ============================================================================
CB_COMP   equ 8
CB_UNIT   equ 16
CB_NODE   equ 24
CB_LAMBDA equ 32
CB_SCOPE  equ 40
CB_LINE   equ 48
CB_I      equ 56
CB_N      equ 64
CB_NAME   equ 72
CB_ARGS   equ 80
CB_FRAME  equ 88          ; + 3 pushes = 112
;; ============================================================================
;; cg_docstring(Comp *c, CompUnit *u, uint32_t body) -> rax = 1 ok, 0 error
;;
;; A module or class body whose first statement is a bare string literal binds
;; it as __doc__.  A function needs nothing here -- func_doc reads co_consts[0],
;; which the same statement fills -- but a module and a class have to store it
;; by name, and without that `C.__doc__` was None for every documented class.
;; Anything else as the first statement, including an expression that merely
;; begins with a string, leaves __doc__ alone.
;; ============================================================================
CDS_COMP  equ 8
CDS_UNIT  equ 16
CDS_LINE  equ 24
CDS_FRAME equ 40          ; + 1 push = 56
global cg_docstring
DEF_FUNC cg_docstring, CDS_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - CDS_UNIT], rsi

    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    movzx ecx, word [rax + AstNode.nchild]
    test ecx, ecx
    jz .cds_none
    mov rsi, rax
    xor edx, edx
    mov rdi, rbx
    call ast_child                      ; the first statement
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_EXPR_STMT
    jne .cds_none
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CDS_LINE], rcx
    mov edx, [rax + AstNode.a]          ; the expression
    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_CONST
    jne .cds_none
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    test rax, rax
    jz .cds_none
    ; A constant is a Value: `class C: 42` puts an immediate int here, and
    ; reading ob_type off one dereferences the number.
    V_TEST_PTR rax, rcx
    ja .cds_none
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .cds_none

    ; LOAD_CONST <the string>; STORE_NAME __doc__
    mov rdi, [rbp - CDS_UNIT]
    mov rsi, rax
    call cg_const
    mov rdx, rax
    mov rdi, [rbp - CDS_UNIT]
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CDS_LINE]
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE

    mov rdi, rbx
    lea rsi, [rel cg_doc_dunder]
    call comp_intern_cstr
    test rax, rax
    jz .cds_fail
    mov rdi, [rbp - CDS_UNIT]
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, [rbp - CDS_UNIT]
    mov esi, OP_STORE_NAME
    mov rcx, [rbp - CDS_LINE]
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE

.cds_none:
    mov eax, 1
    pop rbx
    leave
    ret
.cds_fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC cg_docstring

DEF_FUNC cg_compile_body, CB_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - CB_LAMBDA], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    movzx ecx, word [rax + AstNode.flags]
    mov [rbp - CB_SCOPE], rcx
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CB_LINE], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - CB_ARGS], rcx

    ; The function's name, for co_name and tracebacks.
    cmp qword [rbp - CB_LAMBDA], 1
    je .lambda_name
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    INCREF rax
    jmp .have_name
.lambda_name:
    lea rdi, [rel cg_lambda_name]
    call str_from_cstr_heap
.have_name:
    mov [rbp - CB_NAME], rax

    mov rdi, r12
    mov rax, [rbp - CB_UNIT]
    mov rsi, [rbx + Comp.filename]
    mov rdx, [rbp - CB_NAME]
    call cg_unit_init
    mov rax, [rbp - CB_SCOPE]
    mov [r12 + CompUnit.scope], eax
    mov [r12 + CompUnit.comp], rbx
    mov rax, [rbp - CB_LINE]
    mov [r12 + CompUnit.firstline], eax
    mov [r12 + CompUnit.curline], eax

    ; A function-like scope gets its own fast locals.  A class body does not:
    ; co_flags is 0 there, and every name goes through the mapping
    ; __build_class__ hands it -- which is what makes class attributes work.
    mov dword [r12 + CompUnit.flags], CO_OPTIMIZED | CO_NEWLOCALS
    cmp qword [rbp - CB_LAMBDA], 2
    jne .not_class_flags
    mov dword [r12 + CompUnit.flags], 0
    mov qword [rbp - CB_ARGS], 0
.not_class_flags:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CB_ARGS]
    call cg_set_arg_counts
    test eax, eax
    jz .fail

    ; --- prologue ---
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CB_SCOPE]
    call cg_cell_prologue
    test eax, eax
    jz .fail

    ; A generator announces itself before RESUME.  The opcode is the trigger,
    ; not the flag -- op_return_generator does not consult co_flags -- so both
    ; have to be set, or the body runs eagerly on the first call.
    mov rdi, rbx
    mov rsi, [rbp - CB_SCOPE]
    call sym_at
    mov edx, [rax + Scope.flags]
    test edx, SCF_GENERATOR | SCF_COROUTINE
    jz .not_generator
    ; The three kinds are mutually exclusive in co_flags, and which one this is
    ; falls out of the two scope bits: `yield` alone is a generator, `await`
    ; alone a coroutine, and both together an async generator -- which carries
    ; neither of the other two flags.
    mov ecx, CO_GENERATOR
    test edx, SCF_COROUTINE
    jz .have_kind
    mov ecx, CO_COROUTINE
    test edx, SCF_GENERATOR
    jz .have_kind
    mov ecx, CO_ASYNC_GENERATOR
.have_kind:
    or dword [r12 + CompUnit.flags], ecx
    mov rdi, r12
    mov esi, OP_RETURN_GENERATOR
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
.not_generator:

    mov rdi, r12
    mov esi, OP_RESUME
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE

    ; A class body opens by recording where it came from, which is what makes
    ; C.__module__ and C.__qualname__ exist.
    cmp qword [rbp - CB_LAMBDA], 2
    jne .body_start
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CB_NAME]
    mov rcx, [rbp - CB_LINE]
    call cg_class_prologue
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_docstring
    test eax, eax
    jz .fail
.body_start:

    ; --- body ---
    cmp qword [rbp - CB_LAMBDA], 1
    je .lambda_body
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_body
    test eax, eax
    jz .fail
    ; A class body whose methods needed the class returns the cell holding it,
    ; under the name __build_class__ looks for.  Anything else -- including a
    ; class body with no such method -- falls off the end returning None.
    cmp qword [rbp - CB_LAMBDA], 2
    jne .plain_return
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CB_SCOPE]
    mov rcx, [rbp - CB_LINE]
    call cg_classcell_epilogue
    cmp rax, -1
    je .fail
    test rax, rax
    jnz .assemble
.plain_return:
    mov rdi, r12
    call cg_return_none
    jmp .assemble

.lambda_body:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.c]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_RETURN_VALUE
    xor edx, edx
    mov rcx, [rbp - CB_LINE]
    call cg_emit

.assemble:
    mov rdi, rbx
    mov rsi, r12
    call asm_assemble
    mov [rbp - CB_I], rax
    push rax
    mov rdi, r12
    call cg_unit_free
    mov rdi, [rbp - CB_NAME]
    call obj_decref
    pop rax
    jmp .ret
.fail:
    mov rdi, r12
    call cg_unit_free
    mov rdi, [rbp - CB_NAME]
    call obj_decref
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_compile_body

;; ============================================================================
;; cg_set_arg_counts(Comp *c, CompUnit *u, uint32_t args) -> rax = 1
;; Copy the parameter counts onto the unit and set CO_VARARGS / CO_VARKEYWORDS.
;; func_call reads all four to place arguments, so a wrong count here is an
;; argument landing in the wrong slot rather than an error.
;; ============================================================================
DEF_FUNC cg_set_arg_counts, 16
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    mov dword [r12 + CompUnit.argcount], 0
    mov dword [r12 + CompUnit.posonly], 0
    mov dword [r12 + CompUnit.kwonly], 0
    test rdx, rdx
    jz .done
    mov [rbp - 8], rdx
    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .no_vararg
    or dword [r12 + CompUnit.flags], CO_VARARGS
.no_vararg:
    mov rdi, rbx
    mov rsi, [rbp - 8]
    call ast_at
    mov ecx, [rax + AstNode.c]
    test ecx, ecx
    jz .no_varkw
    or dword [r12 + CompUnit.flags], CO_VARKEYWORDS
.no_varkw:
    mov rdi, rbx
    mov rsi, [rbp - 8]
    call ast_at
    mov esi, [rax + AstNode.a]          ; the AST_EXTRA counts node
    mov rdi, rbx
    call ast_at
    mov ecx, [rax + AstNode.a]
    mov [r12 + CompUnit.argcount], ecx
    mov ecx, [rax + AstNode.b]
    mov [r12 + CompUnit.posonly], ecx
    mov ecx, [rax + AstNode.c]
    mov [r12 + CompUnit.kwonly], ecx
.done:
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_set_arg_counts

;; ============================================================================
;; cg_cell_prologue(Comp *c, CompUnit *u, uint32_t scope) -> rax = 1 ok
;; ============================================================================
CP2_COMP  equ 8
CP2_UNIT  equ 16
CP2_SCOPE equ 24
CP2_I     equ 32
CP2_N     equ 40
CP2_FRAME equ 40          ; + 3 pushes = 64
DEF_FUNC cg_cell_prologue, CP2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - CP2_SCOPE], rdx

    mov rdi, rbx
    mov rsi, rdx
    call sym_at
    mov r13, rax
    mov rcx, [r13 + Scope.cellvars + Buf.len]
    mov [rbp - CP2_N], rcx
    mov qword [rbp - CP2_I], 0
.cell_loop:
    mov rax, [rbp - CP2_I]
    cmp rax, [rbp - CP2_N]
    jae .frees
    mov rdi, rbx
    mov rsi, [rbp - CP2_SCOPE]
    call sym_at
    mov rdx, [rax + Scope.cellvars + Buf.data]
    mov rcx, [rbp - CP2_I]
    mov rdx, [rdx + rcx*8]
    mov rdi, rbx
    mov rsi, [rbp - CP2_SCOPE]
    call sym_lp_index
    cmp eax, -1
    je .next_cell
    mov edx, eax
    mov rdi, r12
    mov esi, OP_MAKE_CELL
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
.next_cell:
    inc qword [rbp - CP2_I]
    jmp .cell_loop

.frees:
    mov rdi, rbx
    mov rsi, [rbp - CP2_SCOPE]
    call sym_at
    mov rcx, [rax + Scope.freevars + Buf.len]
    test rcx, rcx
    jz .done
    mov rdi, r12
    mov esi, OP_COPY_FREE_VARS
    mov rdx, rcx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
.done:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_cell_prologue

;; ============================================================================
;; cg_return_none(CompUnit *u)
;; ============================================================================
DEF_FUNC cg_return_none, 16
    push rbx
    push r12
    mov rbx, rdi
    extern none_singleton
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
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_return_none

;; ============================================================================
;; cg_s_functiondef / cg_s_lambda_expr / cg_s_return
;; ============================================================================
CSF_COMP  equ 8
CSF_UNIT  equ 16
CSF_NODE  equ 24
CSF_LINE  equ 32
CSF_FRAME equ 40          ; + 3 pushes = 64
DEF_FUNC cg_s_functiondef, CSF_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CSF_LINE], rcx
    mov [r12 + CompUnit.curline], ecx

    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    xor ecx, ecx                        ; not a lambda
    call cg_function
    test eax, eax
    jz .fail

    ; Bind the function to its name in the defining scope.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_functiondef

DEF_FUNC cg_e_lambda, CSF_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [r12 + CompUnit.curline], ecx
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov ecx, 1                          ; a lambda
    call cg_function
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_lambda

DEF_FUNC cg_s_return, CSF_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CSF_LINE], rcx
    mov [r12 + CompUnit.curline], ecx
    mov ecx, [rax + AstNode.a]
    test ecx, ecx
    jz .bare

    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    ; A return leaving a try/finally has to run the finally body first, and
    ; the value it is returning is already on the stack while that happens.
    mov rdi, rbx
    mov rsi, r12
    xor edx, edx
    mov ecx, 1                          ; the return value is on top
    mov r8d, 1                          ; and every enclosing loop is left
    call cg_unwind_finallys
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_RETURN_VALUE
    xor edx, edx
    mov rcx, [rbp - CSF_LINE]
    call cg_emit
    mov eax, 1
    jmp .ret
.bare:
    mov rdi, rbx
    mov rsi, r12
    xor edx, edx
    xor ecx, ecx                        ; nothing on the stack yet
    mov r8d, 1                          ; every enclosing loop is left
    call cg_unwind_finallys
    test eax, eax
    jz .fail
    mov rdi, r12
    call cg_return_none
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_return

;; ============================================================================
;; cg_s_classdef - `class C(bases): body`
;;
;;     PUSH_NULL; LOAD_BUILD_CLASS
;;     <the body as a function>; LOAD_CONST 'C'; <bases...>; CALL 2+n
;;     STORE the name
;;
;; __build_class__ takes the body as a function and calls it with a fresh
;; mapping as its locals; that is why the body compiles to a code object with
;; co_flags 0 and LOAD_NAME semantics rather than fast locals.
;; ============================================================================
CC4_COMP  equ 8
CC4_UNIT  equ 16
CC4_NODE  equ 24
CC4_LINE  equ 32
CC4_SCOPE equ 40
CC4_CODE  equ 48
CC4_NAME  equ 56
CC4_BASES equ 64
CC4_NARGS equ 72
CC4_UNIT2 equ 80 + CompUnit_size
CC4_FRAME equ ((CC4_UNIT2 + 15) / 16) * 16 + 8      ; + 3 pushes = 16-aligned
DEF_FUNC cg_class_value, CC4_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CC4_LINE], rcx
    mov [r12 + CompUnit.curline], ecx
    movzx ecx, word [rax + AstNode.flags]
    mov [rbp - CC4_SCOPE], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - CC4_BASES], rcx

    mov rdi, r12
    mov esi, OP_PUSH_NULL
    xor edx, edx
    mov rcx, [rbp - CC4_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_LOAD_BUILD_CLASS
    xor edx, edx
    mov rcx, [rbp - CC4_LINE]
    call cg_emit

    ; The body, compiled as a function of no arguments.
    mov rax, [rbp - CC4_SCOPE]
    mov [rbx + Comp.cur_scope], eax
    mov rdi, rbx
    mov rsi, [rbp - CC4_SCOPE]
    xor edx, edx
    call sym_finalize
    test eax, eax
    jz .fail

    mov rdi, rbx
    lea rsi, [rbp - CC4_UNIT2]
    mov rdx, r13
    mov ecx, 2                          ; a class body
    call cg_compile_body
    mov [rbp - CC4_CODE], rax
    test rax, rax
    jz .fail

    ; A class body needs a closure like any other nested block.  Its methods'
    ; free variables resolve past the class scope to the enclosing function,
    ; and the body carries them through -- so without this its COPY_FREE_VARS
    ; had nothing to copy, and `def f(): class M: def g(self): return M` read
    ; an empty cell.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CC4_SCOPE]
    mov rcx, [rbp - CC4_LINE]
    call cg_closure_tuple
    cmp rax, -1
    je .fail
    mov [rbp - CC4_NAME], rax

    mov rdi, r12
    mov rsi, [rbp - CC4_CODE]
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CC4_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_MAKE_FUNCTION
    mov rdx, [rbp - CC4_NAME]
    mov rcx, [rbp - CC4_LINE]
    call cg_emit

    ; The class name, as a constant: __build_class__ takes it as an argument,
    ; quite separately from the name the result is bound to.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CC4_LINE]
    call cg_emit

    mov qword [rbp - CC4_NARGS], 2
    cmp qword [rbp - CC4_BASES], 0
    je .call

    ; The bases were parsed as a call's argument list; emit them the same way.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CC4_BASES]
    call cg_call_args_only
    cmp rax, -1
    je .fail
    add [rbp - CC4_NARGS], rax

.call:
    mov rdi, r12
    mov esi, OP_CALL
    mov rdx, [rbp - CC4_NARGS]
    mov rcx, [rbp - CC4_LINE]
    call cg_emit
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
.ret:
    ; Restore the enclosing scope for whatever follows -- through rcx, because
    ; eax is the return value.  Reading the scope into eax here reported every
    ; failure as a success, since a nested scope's index is never 0.
    mov ecx, [r12 + CompUnit.scope]
    mov [rbx + Comp.cur_scope], ecx
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_class_value

;; ============================================================================
;; cg_s_classdef - build the class and bind its name.
;; ============================================================================
DEF_FUNC cg_s_classdef, CSF_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    call cg_class_value
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_classdef

;; ============================================================================
;; cg_s_decorated - `@d1 @d2 def f(): ...`
;;
;; The decorators are evaluated top to bottom and applied bottom to top, so the
;; callables are pushed in source order and the calls come out reversed.
;; ============================================================================
CD3_COMP  equ 8
CD3_UNIT  equ 16
CD3_NODE  equ 24
CD3_LINE  equ 32
CD3_I     equ 40
CD3_N     equ 48
CD3_TGT   equ 56
CD3_NAME  equ 64
CD3_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC cg_s_decorated, CD3_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CD3_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CD3_N], rcx
    mov ecx, [rax + AstNode.a]
    mov [rbp - CD3_TGT], rcx

    ; Push each decorator, in source order, with its NULL self slot.
    mov qword [rbp - CD3_I], 0
.push_loop:
    mov rax, [rbp - CD3_I]
    cmp rax, [rbp - CD3_N]
    jae .make
    mov rdi, r12
    mov esi, OP_PUSH_NULL
    xor edx, edx
    mov rcx, [rbp - CD3_LINE]
    call cg_emit
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CD3_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    inc qword [rbp - CD3_I]
    jmp .push_loop

.make:
    ; Build the function or class itself, WITHOUT binding its name: the
    ; decorators run first and the result of the last one is what gets bound.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    movzx eax, byte [rax + AstNode.kind]
    cmp eax, AST_CLASSDEF
    je .class_target

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CD3_TGT]
    xor ecx, ecx
    call cg_function
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, [rbp - CD3_TGT]
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov [rbp - CD3_NAME], rax
    jmp .apply

.class_target:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CD3_TGT]
    call cg_class_value
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, [rbp - CD3_TGT]
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov [rbp - CD3_NAME], rax

.apply:
    ; One CALL per decorator, innermost first.
    mov rax, [rbp - CD3_N]
    mov [rbp - CD3_I], rax
.apply_loop:
    cmp qword [rbp - CD3_I], 0
    je .bind
    mov rdi, r12
    mov esi, OP_CALL
    mov edx, 1
    mov rcx, [rbp - CD3_LINE]
    call cg_emit
    dec qword [rbp - CD3_I]
    jmp .apply_loop

.bind:
    mov rdx, [rbp - CD3_NAME]
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_decorated

;; ============================================================================
;; cg_class_prologue(Comp *c, CompUnit *u, PyStrObject *name, int line)
;;     LOAD_NAME __name__ ; STORE_NAME __module__
;;     LOAD_CONST 'C'     ; STORE_NAME __qualname__
;; ============================================================================
CQ_COMP  equ 8
CQ_UNIT  equ 16
CQ_NAME  equ 24
CQ_LINE  equ 32
CQ_FRAME equ 40           ; + 3 pushes = 64
DEF_FUNC cg_class_prologue, CQ_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - CQ_NAME], rdx
    mov [rbp - CQ_LINE], rcx

    mov rdi, rbx
    lea rsi, [rel cg_name_dunder]
    call comp_intern_cstr
    test rax, rax
    jz .fail
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_NAME
    mov rcx, [rbp - CQ_LINE]
    call cg_emit

    mov rdi, rbx
    lea rsi, [rel cg_module_dunder]
    call comp_intern_cstr
    test rax, rax
    jz .fail
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_NAME
    mov rcx, [rbp - CQ_LINE]
    call cg_emit

    mov rdi, r12
    mov rsi, [rbp - CQ_NAME]
    INCREF rsi
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CQ_LINE]
    call cg_emit

    mov rdi, rbx
    lea rsi, [rel cg_qualname_dunder]
    call comp_intern_cstr
    test rax, rax
    jz .fail
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_NAME
    mov rcx, [rbp - CQ_LINE]
    call cg_emit

    mov eax, 1
    jmp .cq_ret
.fail:
    xor eax, eax
.cq_ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_class_prologue

;; ============================================================================
;; cg_classcell_epilogue(Comp *c, CompUnit *u, uint32_t scope, int line)
;;   -> rax = 1 if it emitted a return, 0 if there was nothing to do, -1 error
;;
;;     LOAD_CLOSURE __class__ ; COPY 1 ; STORE_NAME __classcell__ ; RETURN_VALUE
;;
;; __build_class__ looks for __classcell__ in the namespace the body produced
;; and fills the cell with the finished class, which is what a method's
;; __class__ free variable then reads.
;; ============================================================================
CE3_COMP  equ 8
CE3_UNIT  equ 16
CE3_SCOPE equ 24
CE3_LINE  equ 32
CE3_FRAME equ 40          ; + 3 pushes = 64
DEF_FUNC cg_classcell_epilogue, CE3_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - CE3_SCOPE], rdx
    mov [rbp - CE3_LINE], rcx

    mov rdi, rbx
    lea rsi, [rel cg_class_dunder]
    call comp_intern_cstr
    test rax, rax
    jz .none
    mov r13, rax
    mov rdi, rbx
    mov rsi, [rbp - CE3_SCOPE]
    mov rdx, r13
    call sym_scope_of
    cmp eax, SYM_CELL
    jne .none

    mov rdi, rbx
    mov rsi, [rbp - CE3_SCOPE]
    mov rdx, r13
    call sym_lp_index
    cmp eax, -1
    je .none
    mov edx, eax
    mov rdi, r12
    mov esi, OP_LOAD_CLOSURE
    mov rcx, [rbp - CE3_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - CE3_LINE]
    call cg_emit

    mov rdi, rbx
    lea rsi, [rel cg_classcell_dunder]
    call comp_intern_cstr
    test rax, rax
    jz .oops
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_NAME
    mov rcx, [rbp - CE3_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_RETURN_VALUE
    xor edx, edx
    mov rcx, [rbp - CE3_LINE]
    call cg_emit
    mov eax, 1
    jmp .ret
.oops:
    mov rax, -1
    jmp .ret
.none:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_classcell_epilogue

section .rodata
cg_class_dunder:     db "__class__", 0
cg_classcell_dunder: db "__classcell__", 0

cg_name_dunder:     db "__name__", 0
cg_module_dunder:   db "__module__", 0
cg_qualname_dunder: db "__qualname__", 0
cg_doc_dunder:      db "__doc__", 0

cg_lambda_name: db "<lambda>", 0


ASM_INIT
