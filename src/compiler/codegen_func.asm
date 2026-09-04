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
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_at
extern comp_keep
extern ast_child
extern ast_obj_at
extern ast_obj
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
extern obj_incref
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
CF_PARENT equ 16
CF_NODE   equ 24
CF_SCOPE  equ 32
CF_LINE   equ 40
CF_ARGS   equ 48
CF_CODE   equ 56
CF_I      equ 72
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
    ; Which of the two load opcodes: LOAD_FAST hands back whatever the slot
    ; holds, and an EMPTY slot is a NULL Value with no exception set -- which
    ; print() silently skips, obj_repr calls "object has no repr", and
    ; op_pop_jump_if_false dereferences.  `def f(): x; x = 1` read as nothing
    ; and `if x:` segfaulted.
    ;
    ; DEF_UNBOUND alone was the test, and it only marks a name that is
    ; DELETED somewhere or bound by an `except E as e`.  It says nothing
    ; about a name simply read before its assignment, which is the ordinary
    ; typo.  CPython decides this with a definite-assignment analysis over
    ; the CFG and falls back to the checked form whenever it cannot prove
    ; boundness; the sound approximation here is that a PARAMETER is always
    ; bound on entry and every other local might not be.
    ;
    ; The check never raises where CPython's LOAD_FAST would have succeeded:
    ; it fires only on an actually-empty slot, which is exactly the case
    ; CPython raises for too.  It costs one test and one branch.
    mov [rbp - CN2_SLOT], rdx
    mov rdi, rbx
    mov rsi, [rbp - CN2_SCOPE]
    mov rdx, [rbp - CN2_NAME]
    call sym_flags_of
    mov esi, OP_LOAD_FAST_CHECK
    test eax, DEF_PARAM
    jz .fast_load
    test eax, DEF_UNBOUND
    jnz .fast_load              ; a parameter that is deleted can still be empty
    mov esi, OP_LOAD_FAST
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
    ; CompUnit.consts holds a BORROWED pointer; the arena owns this reference.
    mov rdi, rbx
    mov rsi, rax
    call ast_obj

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
CB_UNIT   equ 16
CB_LAMBDA equ 32
CB_SCOPE  equ 40
CB_LINE   equ 48
CB_I      equ 56
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
CDS_UNIT  equ 16
CDS_LINE  equ 24
CDS_FRAME equ 40          ; + 1 push = 56
DEF_FUNC cg_docstring, CDS_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - CDS_UNIT], rsi

    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    mov ecx, [rax + AstNode.nchild]     ; a dword field
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

;; ============================================================================
;; cg_seed_doc_const(Comp *c, CompUnit *u, uint32_t body) -> rax = 1 ok, 0 error
;;
;; Put the docstring, or None, at co_consts[0].  Called before anything else
;; emits a constant, so cg_const appends it at index zero -- which is the slot
;; func_doc reads.
;; ============================================================================
CSD_UNIT  equ 16
CSD_FRAME equ 40          ; + 1 push = 48, 16-aligned
DEF_FUNC cg_seed_doc_const, CSD_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - CSD_UNIT], rsi

    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    test ecx, ecx
    jz .csd_none
    mov rsi, rax
    xor edx, edx
    mov rdi, rbx
    call ast_child
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_EXPR_STMT
    jne .csd_none
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_CONST
    jne .csd_none
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    test rax, rax
    jz .csd_none
    V_TEST_PTR rax, rcx
    ja .csd_none
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .csd_none
    mov rsi, rax
    jmp .csd_emit

.csd_none:
    extern none_singleton
    lea rsi, [rel none_singleton]
.csd_emit:
    mov rdi, [rbp - CSD_UNIT]
    call cg_const
    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC cg_seed_doc_const

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

    ; The qualified name, which needs the ENCLOSING unit and so has to be
    ; built here rather than in cg_unit_init.
    mov rdi, rbx
    mov rsi, r12
    mov edx, [rbp - CB_SCOPE]
    extern cg_set_qualname
    call cg_set_qualname
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

    ; A function reserves co_consts[0] for its docstring, or None when it has
    ; none.  CPython's compiler does, and func_doc reads that slot -- so a
    ; function whose first constant happened to be a string reported it as
    ; __doc__: `def f(): x = "s"; return x` answered 's', and so did
    ; `lambda: "s"`.  A lambda has no docstring, so it always seeds None; a
    ; class stores __doc__ by name below rather than out of the tuple, and
    ; wants no reserved slot at all.
    cmp qword [rbp - CB_LAMBDA], 2
    je .no_doc_slot
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_seed_doc_const
    test eax, eax
    jz .fail
.no_doc_slot:

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
    ; ...and __annotations__ before anything annotates into it.
    mov rdi, rbx
    mov rsi, r13
    extern cg_has_annotation_body
    call cg_has_annotation_body
    test eax, eax
    jz .body_start
    mov rdi, r12
    mov esi, OP_SETUP_ANNOTATIONS
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
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
CSA_ARGS  equ 8              ; the arguments node, across the ast_at calls
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
    mov [rbp - CSA_ARGS], rdx
    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .no_vararg
    or dword [r12 + CompUnit.flags], CO_VARARGS
.no_vararg:
    mov rdi, rbx
    mov rsi, [rbp - CSA_ARGS]
    call ast_at
    mov ecx, [rax + AstNode.c]
    test ecx, ecx
    jz .no_varkw
    or dword [r12 + CompUnit.flags], CO_VARKEYWORDS
.no_varkw:
    mov rdi, rbx
    mov rsi, [rbp - CSA_ARGS]
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

    ; PEP 695: `def f[T]` is the def wrapped in a nullary function that binds
    ; T and hands back the result.  cg_generic_wrap emits both the wrapper and
    ; the call, and answers 0 when there are no brackets at all.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    xor ecx, ecx                        ; not a class
    call cg_generic_wrap
    cmp rax, -1
    je .fail
    test rax, rax
    jnz .csf_bind

    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    xor ecx, ecx                        ; not a lambda
    call cg_function
    test eax, eax
    jz .fail

.csf_bind:
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
    mov rdi, rbx
    mov rsi, rax
    call ast_obj

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
    mov ecx, 1                          ; a class
    call cg_generic_wrap
    cmp rax, -1
    je .fail
    test rax, rax
    jnz .csc_bind
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_class_value
    test eax, eax
    jz .fail
.csc_bind:
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

    ; The caller owns CQ_NAME and releases it, and cg_const stores a borrowed
    ; reference, so co_consts needs an owner of its own: the arena, which
    ; comp_free releases.  The bare INCREF that used to stand here gave it a
    ; reference and nobody to drop it.
    ; The QUALIFIED name, not the bare one: a class's __qualname__ comes from
    ; this store, not from its code object, so `class B` inside `class A`
    ; reported "B" where CPython reports "A.B".  cg_set_qualname has already
    ; put the qualified form on the class body's unit, and it is arena-owned,
    ; so it needs no keeping of its own; the bare name is the fallback for a
    ; class directly inside the module, where the two are the same anyway.
    mov rsi, [r12 + CompUnit.qualname]
    test rsi, rsi
    jnz .cq_have_qual
    mov rdi, rbx
    mov rsi, [rbp - CQ_NAME]
    INCREF rsi
    call comp_keep
    mov rsi, rax
.cq_have_qual:
    mov rdi, r12
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

section .text

;; ============================================================================
;; cg_generic_wrap(Comp *c, CompUnit *u, uint32_t node, int is_class)
;;   -> rax = 1 when it emitted a wrapper, 0 when there are no type parameters
;;      (and -1 on error)
;;
;;     PUSH_NULL
;;     LOAD_CONST <generic parameters of f>
;;     MAKE_FUNCTION
;;     CALL 0
;;
;; PEP 695 wraps a `def f[T]` or a `class C[T]` in a nullary function that
;; binds the parameters and returns the thing it defined.  That is what gives
;; the def a scope in which T exists -- so a default, an annotation or a base
;; may name one -- and what sets __type_params__ without any statement doing
;; it.  The parameters are ordinary locals of that wrapper, cells when
;; something inside reaches them.
;; ============================================================================
CGW_LINE  equ 32
CGW_SCOPE equ 40
CGW_CODE  equ 48
CGW_NAME  equ 56
CGW_CLASS equ 64
CGW_TP    equ 72
CGW_UNIT2 equ 96 + CompUnit_size
CGW_FRAME equ ((CGW_UNIT2 + 15) / 16) * 16 + 8      ; + 3 pushes = 16-aligned
global cg_generic_wrap
DEF_FUNC cg_generic_wrap, CGW_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - CGW_CLASS], rcx

    mov rdi, rbx
    mov esi, r13d
    extern ast_typeparams_at
    call ast_typeparams_at
    test eax, eax
    jz .cgw_none
    mov [rbp - CGW_TP], rax

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CGW_LINE], rcx
    mov [r12 + CompUnit.curline], ecx

    mov rdi, rbx
    mov rsi, [rbp - CGW_TP]
    call ast_at
    movzx ecx, word [rax + AstNode.flags]
    mov [rbp - CGW_SCOPE], rcx

    ; The wrapper's name is CPython's: "<generic parameters of f>".  A def and
    ; a class hold theirs as an object index; a type alias holds an AST_NAME
    ; node, whose own .a is the index.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.a]
    cmp byte [rax + AstNode.kind], AST_TYPEALIAS
    jne .cgw_have_name_idx
    mov rdi, rbx
    call ast_at
    mov esi, [rax + AstNode.a]
.cgw_have_name_idx:
    mov rdi, rbx
    call ast_obj_at
    mov rdi, rax
    call cg_generic_name
    test rax, rax
    jz .cgw_fail
    mov [rbp - CGW_NAME], rax

    mov rax, [rbp - CGW_SCOPE]
    mov [rbx + Comp.cur_scope], eax
    mov rdi, rbx
    mov rsi, [rbp - CGW_SCOPE]
    xor edx, edx
    extern sym_finalize
    call sym_finalize
    test eax, eax
    jz .cgw_free_name

    mov rdi, rbx
    lea rsi, [rbp - CGW_UNIT2]
    mov rdx, r13
    mov rcx, [rbp - CGW_NAME]
    mov r8, [rbp - CGW_CLASS]
    call cg_generic_body
    mov [rbp - CGW_CODE], rax
    test rax, rax
    jz .cgw_free_name
    mov rdi, rbx
    mov rsi, rax
    extern ast_obj
    call ast_obj
    mov rdi, [rbp - CGW_NAME]
    call obj_decref

    ; Back outside for the call that runs it.
    mov eax, [r12 + CompUnit.scope]
    mov [rbx + Comp.cur_scope], eax

    mov rdi, r12
    mov esi, OP_PUSH_NULL
    xor edx, edx
    mov rcx, [rbp - CGW_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CGW_SCOPE]
    mov rcx, [rbp - CGW_LINE]
    extern cg_closure_tuple
    call cg_closure_tuple
    cmp rax, -1
    je .cgw_fail
    mov [rbp - CGW_SCOPE], rax          ; the MAKE_FUNCTION flags

    mov rdi, r12
    mov rsi, [rbp - CGW_CODE]
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CGW_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_MAKE_FUNCTION
    mov rdx, [rbp - CGW_SCOPE]
    mov rcx, [rbp - CGW_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_CALL
    xor edx, edx
    mov rcx, [rbp - CGW_LINE]
    call cg_emit

    mov eax, 1
    jmp .cgw_ret
.cgw_none:
    xor eax, eax
    jmp .cgw_ret
.cgw_free_name:
    mov rdi, [rbp - CGW_NAME]
    call obj_decref
.cgw_fail:
    mov rax, -1
.cgw_ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_generic_wrap

;; ============================================================================
;; cg_generic_name(rdi = the def's or class's name str)
;;   -> rax = "<generic parameters of NAME>", owned, or 0
;; ============================================================================
CGN_BUF   equ 208
CGN_FRAME equ 216           ; + 1 push = 224, 16-aligned
DEF_FUNC_LOCAL cg_generic_name, CGN_FRAME
    push rbx
    mov rbx, rdi
    lea rdi, [rbp - CGN_BUF]
    CSTRING rsi, "<generic parameters of "
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rbx + PyStrObject.data]
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, ">"
    call rbt_append_cstr
    lea rdi, [rbp - CGN_BUF]
    extern str_from_cstr_heap
    call str_from_cstr_heap
    pop rbx
    leave
    ret
END_FUNC cg_generic_name

;; ============================================================================
;; cg_generic_body(Comp *c, CompUnit *u, uint32_t node, PyStrObject *name,
;;                 int is_class) -> PyCodeObject*, or 0
;;
;;     RESUME 0
;;     for each parameter:
;;         LOAD_CONST 'T'
;;         [LOAD_CONST <thunk>; MAKE_FUNCTION; CALL_INTRINSIC_2 2 or 3]
;;         [CALL_INTRINSIC_1 7, 8 or 9]
;;         COPY 1; <store T>
;;     BUILD_TUPLE n
;;     <the def or the class>
;;     SWAP 2; CALL_INTRINSIC_2 4          a function takes its parameters
;;     COPY 1; SWAP 3; SWAP 2; STORE_ATTR  a class is given them by name
;;     RETURN_VALUE
;;
;; The two differ because CPython's class form threads the tuple through a
;; cell so the class BODY can see it and pass Generic[T] as a base.  Nothing
;; here consumes Generic, so the tuple is set on the finished class instead --
;; which gives the same __type_params__ and leaves the MRO without the extra
;; base.  DIVERGENCES.md records that.
;; ============================================================================
CGB_LINE  equ 32
CGB_SCOPE equ 40
CGB_CODE  equ 48
CGB_NAME  equ 56
CGB_CLASS equ 64
CGB_TP    equ 72
CGB_I     equ 80
CGB_N     equ 88
CGB_PARAM equ 96
CGB_NAMEOBJ equ 104         ; the alias's own name, for INTRINSIC_TYPEALIAS
CGB_FRAME equ 120           ; + 3 pushes = 144, 16-aligned
DEF_FUNC_LOCAL cg_generic_body, CGB_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - CGB_NAME], rcx
    mov [rbp - CGB_CLASS], r8
    mov qword [rbp - CGB_NAMEOBJ], 0
    cmp r8, 2
    jne .cgb_not_alias
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov [rbp - CGB_NAMEOBJ], rax
.cgb_not_alias:

    mov rdi, rbx
    mov esi, r13d
    call ast_typeparams_at
    mov [rbp - CGB_TP], rax

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CGB_LINE], rcx

    mov rdi, rbx
    mov rsi, [rbp - CGB_TP]
    call ast_at
    movzx ecx, word [rax + AstNode.flags]
    mov [rbp - CGB_SCOPE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CGB_N], rcx
    mov qword [rbp - CGB_I], 0

    mov rdi, r12
    mov rsi, [rbx + Comp.filename]
    mov rdx, [rbp - CGB_NAME]
    extern cg_unit_init
    call cg_unit_init
    mov rax, [rbp - CGB_SCOPE]
    mov [r12 + CompUnit.scope], eax
    mov [r12 + CompUnit.comp], rbx
    mov rax, [rbp - CGB_LINE]
    mov [r12 + CompUnit.firstline], eax
    mov [r12 + CompUnit.curline], eax
    mov dword [r12 + CompUnit.flags], CO_OPTIMIZED | CO_NEWLOCALS | CO_NESTED

    ; The same prologue every other nested unit gets: MAKE_CELL for each cell
    ; this scope owns, COPY_FREE_VARS for the ones it borrows.  Without it a
    ; STORE_DEREF writes through a slot that was never boxed.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CGB_SCOPE]
    extern cg_cell_prologue
    call cg_cell_prologue
    test eax, eax
    jz .cgb_fail

    mov rdi, r12
    mov esi, OP_RESUME
    xor edx, edx
    mov rcx, [rbp - CGB_LINE]
    call cg_emit

.cgb_loop:
    mov rcx, [rbp - CGB_I]
    cmp rcx, [rbp - CGB_N]
    jge .cgb_built
    mov rdi, rbx
    mov rsi, [rbp - CGB_TP]
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CGB_I]
    mov rdi, rbx
    call ast_child
    mov [rbp - CGB_PARAM], rax

    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    call cg_typeparam
    test eax, eax
    jz .cgb_fail
    inc qword [rbp - CGB_I]
    jmp .cgb_loop

.cgb_built:
    mov rdi, r12
    mov esi, OP_BUILD_TUPLE
    mov rdx, [rbp - CGB_N]
    mov rcx, [rbp - CGB_LINE]
    call cg_emit

    ; The def, the class or the alias itself, emitted into this unit rather
    ; than the one outside -- which is the whole point of the wrapper.
    cmp qword [rbp - CGB_CLASS], 2
    je .cgb_alias
    cmp qword [rbp - CGB_CLASS], 0
    jne .cgb_class
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    xor ecx, ecx                        ; not a lambda
    call cg_function
    test eax, eax
    jz .cgb_fail
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - CGB_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_CALL_INTRINSIC_2
    mov edx, 4                          ; SET_FUNCTION_TYPE_PARAMS
    mov rcx, [rbp - CGB_LINE]
    call cg_emit
    jmp .cgb_return

.cgb_class:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_class_value
    test eax, eax
    jz .cgb_fail
    ; [params, cls] -> [cls, params, cls] -> STORE_ATTR leaves [cls]
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - CGB_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 3
    mov rcx, [rbp - CGB_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - CGB_LINE]
    call cg_emit
    mov rdi, rbx
    lea rsi, [rel cg_type_params_name]
    call comp_intern_cstr
    test rax, rax
    jz .cgb_fail
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_ATTR
    mov rcx, [rbp - CGB_LINE]
    call cg_emit
    jmp .cgb_return

.cgb_alias:
    ; [params] -> [name, params, valuefunc] -> BUILD_TUPLE 3.  The name goes
    ; UNDER the tuple, so it is rotated in rather than pushed first: the
    ; parameters had to be built before anything could name them.
    mov rdi, r12
    mov rsi, [rbp - CGB_NAMEOBJ]
    INCREF rsi
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CGB_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - CGB_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    extern cg_typealias_func
    call cg_typealias_func
    test eax, eax
    jz .cgb_fail

    mov rdi, r12
    mov esi, OP_BUILD_TUPLE
    mov edx, 3
    mov rcx, [rbp - CGB_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_CALL_INTRINSIC_1
    mov edx, 11                         ; INTRINSIC_TYPEALIAS
    mov rcx, [rbp - CGB_LINE]
    call cg_emit

.cgb_return:
    mov rdi, r12
    mov esi, OP_RETURN_VALUE
    xor edx, edx
    mov rcx, [rbp - CGB_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r12
    extern asm_assemble
    call asm_assemble
    mov [rbp - CGB_CODE], rax
    mov rdi, r12
    extern cg_unit_free
    call cg_unit_free
    mov rax, [rbp - CGB_CODE]
    jmp .cgb_ret
.cgb_fail:
    mov rdi, r12
    call cg_unit_free
    xor eax, eax
.cgb_ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_generic_body

;; ============================================================================
;; cg_typeparam(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;;
;;     LOAD_CONST 'T'
;;     [LOAD_CONST <bound thunk>; MAKE_FUNCTION; CALL_INTRINSIC_2 2 or 3]
;;     [CALL_INTRINSIC_1 7, 8 or 9]
;;     COPY 1
;;     <store T>
;;
;; One type parameter, left on the stack and also bound in the wrapper so the
;; def below can name it.  A bound is a nullary function rather than an
;; expression, so `def f[T: S, S]` may name a parameter declared after it --
;; the same laziness a type alias's value has, and for the same reason.
;; ============================================================================
CTP_LINE  equ 32
CTP_NAME  equ 40
CTP_BOUND equ 48
CTP_KIND  equ 56
CTP_CODE  equ 64
CTP_SCOPE equ 72            ; the bound thunk's scope, for its closure
CTP_UNIT2 equ 96 + CompUnit_size
CTP_FRAME equ ((CTP_UNIT2 + 15) / 16) * 16 + 8      ; + 3 pushes = 16-aligned
DEF_FUNC_LOCAL cg_typeparam, CTP_FRAME
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
    mov [rbp - CTP_LINE], rcx
    mov [r12 + CompUnit.curline], ecx
    movzx ecx, byte [rax + AstNode.kind]
    mov [rbp - CTP_KIND], rcx
    movzx ecx, word [rax + AstNode.flags]
    mov [rbp - CTP_SCOPE], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - CTP_BOUND], rcx
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov [rbp - CTP_NAME], rax

    ; LOAD_CONST 'T'
    mov rdi, r12
    mov rsi, [rbp - CTP_NAME]
    INCREF rsi
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CTP_LINE]
    call cg_emit

    cmp qword [rbp - CTP_BOUND], 0
    je .ctp_plain

    ; The bound, as a nullary function compiled in its own scope.
    mov rdi, rbx
    lea rsi, [rbp - CTP_UNIT2]
    mov rdx, r13
    call cg_bound_thunk
    mov [rbp - CTP_CODE], rax
    test rax, rax
    jz .ctp_fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj

    ; A bound may name a parameter declared after it, which makes that
    ; parameter a cell of the wrapper and the thunk a closure over it -- so
    ; the closure tuple is built before the code constant, as MAKE_FUNCTION
    ; wants, and the flags come from it.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CTP_SCOPE]
    mov rcx, [rbp - CTP_LINE]
    call cg_closure_tuple
    cmp rax, -1
    je .ctp_fail
    push rax
    mov rdi, r12
    mov rsi, [rbp - CTP_CODE]
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CTP_LINE]
    call cg_emit
    pop rdx
    mov rdi, r12
    mov esi, OP_MAKE_FUNCTION
    mov rcx, [rbp - CTP_LINE]
    call cg_emit

    ; A tuple literal is a constraint list; anything else is a bound.  That is
    ; the same test CPython makes, and it is syntactic: `T: (int, str)` names
    ; two alternatives, `T: Tuple[int, str]` names one bound.
    mov rdi, rbx
    mov rsi, [rbp - CTP_BOUND]
    call ast_at
    mov edx, 2                          ; TYPEVAR_WITH_BOUND
    cmp byte [rax + AstNode.kind], AST_TUPLE
    jne .ctp_have_sel
    mov edx, 3                          ; TYPEVAR_WITH_CONSTRAINTS
.ctp_have_sel:
    mov rdi, r12
    mov esi, OP_CALL_INTRINSIC_2
    mov rcx, [rbp - CTP_LINE]
    call cg_emit
    jmp .ctp_bind

.ctp_plain:
    mov edx, 7                          ; INTRINSIC_TYPEVAR
    cmp qword [rbp - CTP_KIND], AST_PARAMSPEC
    jne .ctp_not_paramspec
    mov edx, 8
.ctp_not_paramspec:
    cmp qword [rbp - CTP_KIND], AST_TYPEVARTUPLE
    jne .ctp_have_one
    mov edx, 9
.ctp_have_one:
    mov rdi, r12
    mov esi, OP_CALL_INTRINSIC_1
    mov rcx, [rbp - CTP_LINE]
    call cg_emit

.ctp_bind:
    ; The parameter stays on the stack for BUILD_TUPLE and is also bound, so
    ; the def below -- and any bound after it -- can name it.
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - CTP_LINE]
    call cg_emit
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CTP_NAME]
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    test eax, eax
    jz .ctp_fail
    mov eax, 1
    jmp .ctp_ret
.ctp_fail:
    xor eax, eax
.ctp_ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_typeparam

;; ============================================================================
;; cg_bound_thunk(Comp *c, CompUnit *u, uint32_t node) -> PyCodeObject*, or 0
;;
;;     RESUME 0; <the bound>; RETURN_VALUE
;;
;; A nullary function named after the parameter, which is what a traceback
;; through a bound that raises shows.
;; ============================================================================
CBT_LINE  equ 32
CBT_SCOPE equ 40
CBT_CODE  equ 48
CBT_NAME  equ 56
CBT_FRAME equ 72            ; + 3 pushes = 96, 16-aligned
DEF_FUNC_LOCAL cg_bound_thunk, CBT_FRAME
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
    mov [rbp - CBT_LINE], rcx
    movzx ecx, word [rax + AstNode.flags]
    mov [rbp - CBT_SCOPE], rcx
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov [rbp - CBT_NAME], rax
    mov rdi, rax
    call obj_incref

    mov rax, [rbp - CBT_SCOPE]
    mov [rbx + Comp.cur_scope], eax
    mov rdi, rbx
    mov rsi, [rbp - CBT_SCOPE]
    xor edx, edx
    call sym_finalize
    test eax, eax
    jz .cbt_fail_name

    mov rdi, r12
    mov rsi, [rbx + Comp.filename]
    mov rdx, [rbp - CBT_NAME]
    call cg_unit_init
    mov rax, [rbp - CBT_SCOPE]
    mov [r12 + CompUnit.scope], eax
    mov [r12 + CompUnit.comp], rbx
    mov rax, [rbp - CBT_LINE]
    mov [r12 + CompUnit.firstline], eax
    mov [r12 + CompUnit.curline], eax
    mov dword [r12 + CompUnit.flags], CO_OPTIMIZED | CO_NEWLOCALS | CO_NESTED

    ; The same prologue every other nested unit gets: MAKE_CELL for each cell
    ; this scope owns, COPY_FREE_VARS for the ones it borrows.  Without it a
    ; STORE_DEREF writes through a slot that was never boxed.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CBT_SCOPE]
    extern cg_cell_prologue
    call cg_cell_prologue
    test eax, eax
    jz .cbt_fail

    mov rdi, r12
    mov esi, OP_RESUME
    xor edx, edx
    mov rcx, [rbp - CBT_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .cbt_fail

    mov rdi, r12
    mov esi, OP_RETURN_VALUE
    xor edx, edx
    mov rcx, [rbp - CBT_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r12
    call asm_assemble
    mov [rbp - CBT_CODE], rax
    mov rdi, r12
    call cg_unit_free
    mov rdi, [rbp - CBT_NAME]
    call obj_decref
    ; The enclosing scope is the wrapper's; the caller put it there.
    mov rax, [rbp - CBT_CODE]
    jmp .cbt_ret
.cbt_fail:
    mov rdi, r12
    call cg_unit_free
.cbt_fail_name:
    mov rdi, [rbp - CBT_NAME]
    call obj_decref
    xor eax, eax
.cbt_ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_bound_thunk

section .rodata
cg_type_params_name: db "__type_params__", 0
section .text
