; symtab.asm - Scope analysis
;
; Two passes over the tree.  The first records what each block binds and what
; it uses; the second classifies every name as local, cell, free, explicit
; global or implicit global.  That classification is the whole reason this file
; exists: it is what decides whether a name compiles to LOAD_FAST, LOAD_NAME,
; LOAD_GLOBAL or LOAD_DEREF, and none of those can be chosen from the syntax
; alone.  `x` in a function is a local if the function assigns it ANYWHERE,
; including after the use.
;
; The rule that matters most is a negative one.  Module and class blocks are
; not function-like, so nothing in them is ever a fast local: every name goes
; through the frame's locals mapping.  That mapping is exactly the dict handed
; to exec(src, d), which is why exec can return results through it -- and why
; getting this wrong would break every caller of exec while leaving functions
; looking fine.
;
; The name maps are ordinary PyDictObjects.  Collision handling, resizing and
; refcounting come for free, and they are the reason this file is a few hundred
; lines rather than a few thousand.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern ast_obj_at
extern buf_free
extern buf_init
extern buf_push_ptr
extern buf_reserve
extern comp_error
extern comp_intern_cstr

extern dict_get
extern dict_new
extern dict_set
extern obj_decref
extern obj_dealloc

extern exc_SyntaxError_type

; --- Named frame-layout constants ---
SN_COMP  equ 8
SN_PARENT equ 16
SN_KIND  equ 24
SN_NODE  equ 32
SN_IDX   equ 40
SN_FRAME equ 40          ; + 3 pushes = 64

section .text

;; ============================================================================
;; sym_at(Comp *c, uint32_t idx) -> Scope*
;; Valid until the next sym_new; recompute rather than cache.
;; ============================================================================
DEF_FUNC_BARE sym_at
    mov rax, rsi
    imul rax, rax, Scope_size
    add rax, [rdi + Comp.scopes + Buf.data]
    ret
END_FUNC sym_at

;; ============================================================================
;; sym_new(Comp *c, uint32_t parent, int kind, uint32_t node) -> scope index
;; ============================================================================
DEF_FUNC sym_new, SN_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - SN_PARENT], rsi
    mov [rbp - SN_KIND], rdx
    mov [rbp - SN_NODE], rcx

    ; Index 0 is reserved so a zero parent reads as "no enclosing scope".
    cmp qword [rbx + Comp.scopes + Buf.len], 0
    jne .have_zero
    lea rdi, [rbx + Comp.scopes]
    mov esi, 1
    call buf_reserve
    mov rdi, rax
    xor esi, esi
    mov edx, Scope_size
    call sym_zero
.have_zero:

    lea rdi, [rbx + Comp.scopes]
    mov esi, 1
    call buf_reserve
    mov r12, rax
    mov rdi, r12
    xor esi, esi
    mov edx, Scope_size
    call sym_zero

    mov rax, [rbp - SN_PARENT]
    mov [r12 + Scope.parent], eax
    mov rax, [rbp - SN_KIND]
    mov [r12 + Scope.kind], eax
    mov rax, [rbp - SN_NODE]
    mov [r12 + Scope.node], eax

    call dict_new
    mov [r12 + Scope.symbols], rax

    lea rdi, [r12 + Scope.varnames]
    mov esi, 8
    call buf_init
    lea rdi, [r12 + Scope.cellvars]
    mov esi, 8
    call buf_init
    lea rdi, [r12 + Scope.freevars]
    mov esi, 8
    call buf_init
    lea rdi, [r12 + Scope.localsplus]
    mov esi, 8
    call buf_init
    lea rdi, [r12 + Scope.children]
    mov esi, 4
    call buf_init

    mov r13, [rbx + Comp.scopes + Buf.len]
    dec r13

    ; Link into the parent, and inherit the nested flag so CO_NESTED is right.
    mov rax, [rbp - SN_PARENT]
    test rax, rax
    jz .no_parent
    mov rdi, rbx
    mov rsi, rax
    call sym_at
    lea rdi, [rax + Scope.children]
    mov rsi, r13
    call sym_push_u32
.no_parent:
    mov rax, r13
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_new

;; ============================================================================
;; sym_zero(void *p, int unused, size_t n) - a local memset, to avoid pulling
;; the whole memory interface into this file for one call.
;; ============================================================================
DEF_FUNC_BARE sym_zero
    xor eax, eax
.loop:
    test rdx, rdx
    jz .done
    mov [rdi], al
    inc rdi
    dec rdx
    jmp .loop
.done:
    ret
END_FUNC sym_zero

DEF_FUNC_BARE sym_push_u32
    extern buf_push_u32
    jmp buf_push_u32
END_FUNC sym_push_u32

;; ============================================================================
;; sym_free_all(Comp *c) - release every scope's maps and buffers.
;; ============================================================================
DEF_FUNC sym_free_all, 8
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r13, [rbx + Comp.scopes + Buf.len]
    mov r12, 1                          ; index 0 is the reserved blank
.loop:
    cmp r12, r13
    jae .done
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    push rax
    mov rdi, [rax + Scope.symbols]
    test rdi, rdi
    jz .no_dict
    call obj_decref
.no_dict:
    pop rax
    mov qword [rax + Scope.symbols], 0
    push rax
    lea rdi, [rax + Scope.varnames]
    call buf_free
    mov rax, [rsp]
    lea rdi, [rax + Scope.cellvars]
    call buf_free
    mov rax, [rsp]
    lea rdi, [rax + Scope.freevars]
    call buf_free
    mov rax, [rsp]
    lea rdi, [rax + Scope.localsplus]
    call buf_free
    pop rax
    lea rdi, [rax + Scope.children]
    call buf_free
    inc r12
    jmp .loop
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_free_all

;; ============================================================================
;; sym_get(Comp *c, uint32_t scope, PyStrObject *name) -> eax = flags, 0 absent
;; ============================================================================
DEF_FUNC sym_get, 8
    push rbx
    mov rbx, rsi
    call sym_at
    mov rdi, [rax + Scope.symbols]
    mov rsi, rdx
    call dict_get
    test rax, rax
    jz .absent
    V_TO_I64 rax
    pop rbx
    leave
    ret
.absent:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC sym_get

;; ============================================================================
;; sym_set(Comp *c, uint32_t scope, PyStrObject *name, uint32_t flags)
;; ============================================================================
SS_FLAGS equ 8
SS_NAME  equ 16
SS_FRAME equ 16          ; + 2 pushes = 32
DEF_FUNC sym_set, SS_FRAME
    push rbx
    push r12
    mov [rbp - SS_FLAGS], rcx
    mov [rbp - SS_NAME], rdx
    call sym_at
    mov r12, [rax + Scope.symbols]
    mov rax, [rbp - SS_FLAGS]
    V_PACK_I64 rax, rcx
    mov rdi, r12
    mov rsi, [rbp - SS_NAME]
    mov rdx, rax
    call dict_set
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_set

;; ============================================================================
;; sym_add(Comp *c, uint32_t scope, PyStrObject *name, uint32_t add_flags)
;; Fold new flags into whatever the name already carries in this block.
;; ============================================================================
SA2_ADD   equ 8
SA2_NAME  equ 16
SA2_SCOPE equ 24
SA2_COMP  equ 32
SA2_FRAME equ 40          ; + 1 push = 48
DEF_FUNC sym_add, SA2_FRAME
    push rbx
    mov [rbp - SA2_COMP], rdi
    mov [rbp - SA2_SCOPE], rsi
    mov [rbp - SA2_NAME], rdx
    mov [rbp - SA2_ADD], rcx
    mov rbx, rdi
    call sym_get
    or rax, [rbp - SA2_ADD]
    mov rdi, rbx
    mov rsi, [rbp - SA2_SCOPE]
    mov rdx, [rbp - SA2_NAME]
    mov rcx, rax
    call sym_set
    pop rbx
    leave
    ret
END_FUNC sym_add

;; ============================================================================
;; sym_visit(Comp *c, uint32_t scope, uint32_t node) -> rax = 1 ok, 0 error
;;
;; Pass one.  Walks the tree recording bindings and uses.  It is a single
;; generic walk rather than one visitor per node kind: almost every node just
;; needs its children visited, and the handful that bind something -- targets,
;; parameters, imports, def, lambda -- are picked out by kind here.
;; ============================================================================
SV_COMP  equ 8
SV_SCOPE equ 16
SV_NODE  equ 24
SV_I     equ 32
SV_N     equ 40
SV_NPTR  equ 48
SV_KIND  equ 56
SV_NAME  equ 64          ; the walrus target, across the scope walk
SV_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC sym_visit, SV_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    test r13, r13
    jz .ok

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov [rbp - SV_NPTR], rax
    movzx eax, byte [rax + AstNode.kind]
    mov [rbp - SV_KIND], rax

    cmp eax, AST_NAME
    je .name
    cmp eax, AST_GLOBAL
    je .global_decl
    cmp eax, AST_NONLOCAL
    je .nonlocal_decl
    cmp eax, AST_FUNCTIONDEF
    je .funcdef
    cmp eax, AST_LAMBDA
    je .lambda
    cmp eax, AST_CLASSDEF
    je .classdef
    cmp eax, AST_LISTCOMP
    je .comprehension
    cmp eax, AST_SETCOMP
    je .comprehension
    cmp eax, AST_DICTCOMP
    je .comprehension
    cmp eax, AST_GENEXP
    je .comprehension
    cmp eax, AST_ALIAS
    je .alias
    cmp eax, AST_ARG
    je .arg
    cmp eax, AST_HANDLER
    je .handler
    cmp eax, AST_NAMEDEXPR
    je .namedexpr
    cmp eax, AST_PAT_CAPTURE
    je .pat_capture
    cmp eax, AST_PAT_AS
    je .pat_as
    cmp eax, AST_PAT_KEYWORD
    je .pat_keyword
    cmp eax, AST_PAT_MAPPING
    je .pat_mapping
    cmp eax, AST_PAT_VALUE
    je .children
    cmp eax, AST_COMPARE
    je .compare
    cmp eax, AST_YIELD
    je .mark_generator
    cmp eax, AST_YIELDFROM
    je .mark_generator
    cmp eax, AST_AWAIT
    je .mark_coroutine
    cmp eax, AST_FOR
    je .maybe_async
    cmp eax, AST_WITH
    je .maybe_async
    jmp .children

;; A bare name: a use, or a binding, depending on the context the parser set.
.name:
    mov rax, [rbp - SV_NPTR]
    movzx ecx, byte [rax + AstNode.subkind]
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    push rcx
    call ast_obj_at
    pop rcx
    mov rdx, rax
    mov r8d, DEF_USE
    cmp ecx, CTX_LOAD
    je .name_flags
    mov r8d, DEF_LOCAL
    cmp ecx, CTX_DEL
    jne .name_flags
    or r8d, DEF_UNBOUND
.name_flags:
    mov rdi, rbx
    mov rsi, r12
    mov rcx, r8
    call sym_add
    jmp .ok

;; `global x` / `nonlocal x`: a declaration about a name, not a binding of it.
.global_decl:
    mov r8d, DEF_GLOBAL
    jmp .decl_common
.nonlocal_decl:
    mov r8d, DEF_NONLOCAL
.decl_common:
    mov [rbp - SV_KIND], r8
    mov rax, [rbp - SV_NPTR]
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SV_N], rcx
    mov qword [rbp - SV_I], 0
.decl_loop:
    mov rax, [rbp - SV_I]
    cmp rax, [rbp - SV_N]
    jae .ok
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SV_I]
    mov rdi, rbx
    call ast_child
    mov rsi, rax
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov rcx, [rbp - SV_KIND]
    call sym_add
    inc qword [rbp - SV_I]
    jmp .decl_loop

;; `import a` and `from m import a as b` bind a name in this block.
.alias:
    ; The name the import BINDS, which the parser worked out: the asname, or
    ; the mangled leading component of the dotted name.  Binding .a instead
    ; recorded a local literally called "a.b" for `import a.b`, where CPython
    ; binds `a` -- so the symbol table and the store disagreed about the name.
    mov rax, [rbp - SV_NPTR]
    mov ecx, [rax + AstNode.c]
    test ecx, ecx
    jz .ok                              ; `import *` binds nothing statically
.alias_name:
    mov esi, ecx
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, DEF_IMPORT | DEF_LOCAL
    call sym_add
    jmp .ok

;; A parameter binds in the function's own scope; its default is evaluated in
;; the enclosing one, and was already visited there.
.arg:
    mov rax, [rbp - SV_NPTR]
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, DEF_PARAM | DEF_LOCAL
    call sym_add
    jmp .ok

;; `except E as e:` binds e in this block, exactly as an assignment would --
;; and it is a local in a function, not a global.  Nothing else visits the
;; name, because it hangs off .b rather than the child list.
.handler:
    mov rax, [rbp - SV_NPTR]
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .children
    mov esi, ecx
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, DEF_LOCAL | DEF_UNBOUND
    call sym_add
    jmp .children

;; A pattern's names are bindings, not uses: `case x` stores into x.  They live
;; in .a or .b as OBJECT indices, so the generic walk must not follow them --
;; the two index spaces collide, and following one lands on an unrelated node.
;; PEP 572: a walrus inside a comprehension binds in the scope the
;; comprehension appears in, not in the comprehension itself.  Ours are
;; compiled as nested functions, so the target has to become a cell of the
;; enclosing function -- or a global at module level -- and be declared in
;; every comprehension scope in between.  Without that, `[y := i for i in r]`
;; left y visible only inside the comprehension.
.namedexpr:
    mov rax, [rbp - SV_NPTR]
    mov edx, [rax + AstNode.b]          ; the value first
    call .visit_field
    test eax, eax
    jz .fail

    ; Find the nearest enclosing scope that is not a comprehension.
    mov [rbp - SV_N], r12               ; the scope the target belongs to
.ne_climb:
    mov rdi, rbx
    mov rsi, [rbp - SV_N]
    call sym_at
    cmp dword [rax + Scope.kind], SCOPE_COMP
    jne .ne_found
    mov ecx, [rax + Scope.parent]
    test ecx, ecx
    jz .ne_found
    mov [rbp - SV_N], rcx
    jmp .ne_climb
.ne_found:
    mov rdi, rbx
    mov rsi, [rbp - SV_N]
    call sym_at
    xor r13d, r13d
    cmp dword [rax + Scope.kind], SCOPE_FUNCTION
    je .ne_have_kind
    mov r13d, 1                         ; module or class: a global
.ne_have_kind:
    mov [rbp - SV_I], r13

    ; The name itself.
    mov rax, [rbp - SV_NPTR]
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov [rbp - SV_NAME], rax

    ; Bind it where it belongs.
    mov rdi, rbx
    mov rsi, [rbp - SV_N]
    mov rdx, [rbp - SV_NAME]
    mov ecx, DEF_LOCAL
    call sym_add

    ; ...and declare it in each comprehension between here and there.
    mov r13, r12
.ne_decl:
    cmp r13, [rbp - SV_N]
    je .ok
    mov ecx, DEF_NONLOCAL
    cmp qword [rbp - SV_I], 0
    je .ne_decl_kind
    mov ecx, DEF_GLOBAL
.ne_decl_kind:
    push rcx
    mov rdi, rbx
    mov rsi, r13
    mov rdx, [rbp - SV_NAME]
    pop rcx
    call sym_add
    mov rdi, rbx
    mov rsi, r13
    call sym_at
    mov ecx, [rax + Scope.parent]
    test ecx, ecx
    jz .ok
    mov r13, rcx
    jmp .ne_decl

.pat_capture:
    mov rax, [rbp - SV_NPTR]
    mov ecx, [rax + AstNode.a]
    test ecx, ecx
    jz .ok                              ; the `_` wildcard binds nothing
    jmp .bind_pat_name
.pat_as:
    ; .a is the inner pattern (a node), .b the bound name (an object).
    mov rax, [rbp - SV_NPTR]
    mov edx, [rax + AstNode.a]
    test edx, edx
    jz .as_name
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
.as_name:
    mov rax, [rbp - SV_NPTR]
    mov ecx, [rax + AstNode.b]
    jmp .bind_pat_name
.pat_keyword:
    ; .a is the keyword's name (an object); .b is the sub-pattern.
    mov rax, [rbp - SV_NPTR]
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    jmp .ret
.pat_mapping:
    ; .b is the **rest name, an object; the child list is visited as usual.
    mov rax, [rbp - SV_NPTR]
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .children
    push rcx
    mov rax, [rbp - SV_NPTR]
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SV_N], rcx
    pop rcx
    mov esi, ecx
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, DEF_LOCAL
    call sym_add
    jmp .children
.bind_pat_name:
    mov esi, ecx
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, DEF_LOCAL
    call sym_add
    jmp .ok

;; `def f(...)`: f binds here; the body gets its own scope.
.funcdef:
    mov rax, [rbp - SV_NPTR]
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, DEF_LOCAL
    call sym_add
    ; The defaults belong to the enclosing scope, so visit the parameter list
    ; here before descending.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call sym_visit_defaults
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov ecx, SCOPE_FUNCTION
    call sym_enter_function
    jmp .ret

.comprehension:
    ; A comprehension compiles to a nested function taking the outermost
    ; iterable as its one argument, so the iterable itself is evaluated in the
    ; ENCLOSING scope while everything else belongs to the new one.  That is
    ; also what keeps the loop variable from leaking, without any special rule.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    xor edx, edx
    mov rdi, rbx
    call ast_child                      ; the first clause
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov edx, [rax + AstNode.b]          ; its iterable
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call sym_enter_comp
    jmp .ret

.classdef:
    ; The class name binds in the enclosing block; the body gets a scope of its
    ; own, which is NOT function-like -- so nothing in it is a fast local, and
    ; its names are invisible to the methods defined inside it.
    mov rax, [rbp - SV_NPTR]
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, DEF_LOCAL
    call sym_add
    ; The bases and keywords are evaluated in the enclosing scope.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]
    test edx, edx
    jz .class_body
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
.class_body:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov ecx, SCOPE_CLASS
    call sym_enter_function
    jmp .ret

.lambda:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call sym_visit_defaults
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov ecx, SCOPE_LAMBDA
    call sym_enter_function
    jmp .ret

.mark_generator:
    ; Any yield anywhere in a function makes the whole function a generator --
    ; the flag belongs to the scope, not to the statement.
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    or dword [rax + Scope.flags], SCF_GENERATOR
    jmp .children

;; `await` makes the enclosing block a coroutine, exactly as `yield` makes it a
;; generator.  A block that has both is an async generator; the two flags are
;; independent here and only combine in the code generator.
.mark_coroutine:
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    or dword [rax + Scope.flags], SCF_COROUTINE
    jmp .children

;; `async for` and `async with` carry the same implication as `await`; a plain
;; one does not, so the bit has to be read before deciding.
.maybe_async:
    mov rax, [rbp - SV_NPTR]
    cmp byte [rax + AstNode.subkind], 0
    jne .mark_coroutine
    jmp .children

;; A comparison chain interleaves operator CODES with operand nodes in its
;; child list, so only the odd slots are nodes.  Walking the whole list treats
;; a code like CMPOP_NE as a node index and recurses into whatever happens to
;; live there.
.compare:
    mov rax, [rbp - SV_NPTR]
    mov edx, [rax + AstNode.a]          ; the left operand
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.nchild]     ; pairs, not entries
    shl rcx, 1
    mov [rbp - SV_N], rcx
    mov qword [rbp - SV_I], 1           ; the first operand, skipping its op
.cmp_loop:
    mov rax, [rbp - SV_I]
    cmp rax, [rbp - SV_N]
    jae .ok
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SV_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    add qword [rbp - SV_I], 2
    jmp .cmp_loop

;; Everything else: visit whatever it points at.
.children:
    mov rax, [rbp - SV_NPTR]
    mov edx, [rax + AstNode.a]
    call .visit_field
    test eax, eax
    jz .fail
    ; Three pattern kinds keep something that is not a node index in .b: a
    ; sequence pattern's star position, a class pattern's positional count,
    ; and a mapping pattern's object index for **rest.  Walking one as a node
    ; visits whatever sits at that index in the *node* arena -- the two index
    ; spaces overlap freely -- and adds bindings from it.
    mov rax, [rbp - SV_KIND]
    cmp eax, AST_PAT_SEQUENCE
    je .skip_b
    cmp eax, AST_PAT_MAPPING
    je .skip_b
    cmp eax, AST_PAT_CLASS
    je .skip_b
    mov rax, [rbp - SV_NPTR]
    mov edx, [rax + AstNode.b]
    call .visit_field
    test eax, eax
    jz .fail
.skip_b:
    mov rax, [rbp - SV_NPTR]
    mov edx, [rax + AstNode.c]
    call .visit_field
    test eax, eax
    jz .fail

    ; AST_FOR keeps its `else` block in clist, with nchild left at 0, so the
    ; list walk below never reaches it.  The whole else body therefore went
    ; unvisited: a def in one got no scope, and compiling it segfaulted.
    mov rax, [rbp - SV_KIND]
    cmp eax, AST_FOR
    jne .not_for_else
    mov rax, [rbp - SV_NPTR]
    mov edx, [rax + AstNode.clist]
    call .visit_field
    test eax, eax
    jz .fail
.not_for_else:

    ; The a/b/c fields of some kinds hold object indices rather than nodes, so
    ; only kinds whose children really are nodes reach the list walk below.
    mov rax, [rbp - SV_KIND]
    cmp eax, AST_CONST
    je .ok
    cmp eax, AST_ALIAS
    je .ok

    mov rax, [rbp - SV_NPTR]
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SV_N], rcx
    mov qword [rbp - SV_I], 0
.child_loop:
    mov rax, [rbp - SV_I]
    cmp rax, [rbp - SV_N]
    jae .ok
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SV_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    inc qword [rbp - SV_I]
    jmp .child_loop

.ok:
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
    jmp .ret

; Local: visit one node-valued field, if it is a node at all.
.visit_field:
    sub rsp, 8
    test edx, edx
    jz .field_none
    ; Kinds whose a/b/c are object indices, not node indices.
    mov rax, [rbp - SV_KIND]
    cmp eax, AST_CONST
    je .field_none
    cmp eax, AST_NAME
    je .field_none
    cmp eax, AST_ATTRIBUTE
    je .field_attr
    cmp eax, AST_KEYWORD
    je .field_kw
    cmp eax, AST_IMPORTFROM
    je .field_none
    cmp eax, AST_CLASSDEF
    je .field_none
    cmp eax, AST_HANDLER
    je .field_handler
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    add rsp, 8
    ret
.field_handler:
    ; a is the exception type (a node); b is the bound name (an object).
    ; Walking b as a node is not a no-op: object indices and node indices come
    ; from different arenas and collide freely, so `except E as e` would visit
    ; whatever node happens to sit at e's object index -- and if that node is a
    ; function or a lambda, it gets a SECOND scope, overwriting the scope index
    ; stamped on it by the first.  The real owner then reads someone else's
    ; scope, and its closure comes out empty.
    mov rax, [rbp - SV_NPTR]
    cmp edx, [rax + AstNode.a]
    jne .field_none
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    add rsp, 8
    ret
.field_attr:
    ; a is the value (a node); b is the attribute name (an object).
    mov rax, [rbp - SV_NPTR]
    cmp edx, [rax + AstNode.a]
    jne .field_none
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    add rsp, 8
    ret
.field_kw:
    ; a is the keyword name (an object); b is the value (a node).
    mov rax, [rbp - SV_NPTR]
    cmp edx, [rax + AstNode.b]
    jne .field_none
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    add rsp, 8
    ret
.field_none:
    mov eax, 1
    add rsp, 8
    ret

.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_visit


;; ============================================================================
;; sym_visit_defaults(Comp *c, uint32_t scope, uint32_t fn) -> 1 ok, 0 error
;; Default values and annotations are evaluated in the ENCLOSING scope, at the
;; point the function is defined -- which is why `def f(x=n)` captures n's value
;; then rather than at call time.
;; ============================================================================
SD_COMP  equ 8
SD_SCOPE equ 16
SD_FN    equ 24
SD_I     equ 32
SD_N     equ 40
SD_ARGS  equ 48
SD_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC sym_visit_defaults, SD_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.b]          ; the AST_ARGUMENTS node
    mov [rbp - SD_ARGS], rcx
    test ecx, ecx
    jz .ok
    mov rdi, rbx
    mov rsi, rcx
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SD_N], rcx
    mov qword [rbp - SD_I], 0
.loop:
    mov rax, [rbp - SD_I]
    cmp rax, [rbp - SD_N]
    jae .ok
    mov rdi, rbx
    mov rsi, [rbp - SD_ARGS]
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SD_I]
    mov rdi, rbx
    call ast_child
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov edx, [rax + AstNode.c]          ; the default expression
    test edx, edx
    jz .next
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
.next:
    inc qword [rbp - SD_I]
    jmp .loop
.ok:
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
END_FUNC sym_visit_defaults

;; ============================================================================
;; sym_enter_function(Comp *c, uint32_t parent, uint32_t fn, int kind)
;;   -> rax = 1 ok, 0 error
;; Create the function's own scope, bind its parameters there, and walk its
;; body.  The scope index is stored back on the AST node so codegen can find it
;; without repeating the walk.
;; ============================================================================
SE_COMP  equ 8
SE_PARENT equ 16
SE_FN    equ 24
SE_KIND  equ 32
SE_SCOPE equ 40
SE_I     equ 48
SE_N     equ 56
SE_ARGS  equ 64
SE_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC sym_enter_function, SE_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - SE_PARENT], rsi
    mov r13, rdx
    mov [rbp - SE_FN], rdx
    mov [rbp - SE_KIND], rcx

    mov rdi, rbx
    mov rsi, [rbp - SE_PARENT]
    mov rdx, [rbp - SE_KIND]
    mov rcx, r13
    call sym_new
    mov r12, rax
    mov [rbp - SE_SCOPE], rax

    ; Record the scope on the node; the code generator reads it back from here.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov [rax + AstNode.flags], r12w

    ; `async def` is a property of the block itself, not of anything inside it,
    ; so it is stamped here rather than discovered by the walk.
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_FUNCTIONDEF
    jne .not_async
    cmp byte [rax + AstNode.subkind], 0
    je .not_async
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    or dword [rax + Scope.flags], SCF_COROUTINE
.not_async:

    ; Parameters bind in the new scope, in signature order.  A class body has
    ; none: its .b is the base list, which belongs to the enclosing scope.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_CLASSDEF
    je .no_params
    mov ecx, [rax + AstNode.b]
    mov [rbp - SE_ARGS], rcx
    test ecx, ecx
    jz .body
    jmp .have_args
.no_params:
    mov qword [rbp - SE_ARGS], 0
    jmp .body
.have_args:
    mov rdi, rbx
    mov rsi, rcx
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SE_N], rcx
    mov qword [rbp - SE_I], 0
.param_loop:
    mov rax, [rbp - SE_I]
    cmp rax, [rbp - SE_N]
    jae .star_params
    mov rdi, rbx
    mov rsi, [rbp - SE_ARGS]
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SE_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call sym_visit                      ; an AST_ARG binds its name
    test eax, eax
    jz .fail
    inc qword [rbp - SE_I]
    jmp .param_loop

.star_params:
    ; *args and **kwargs hang off .b and .c rather than the child list, because
    ; localsplus puts them after the keyword-only slots.  They still bind names
    ; in this scope, so they need visiting too -- without this, the body sees
    ; `a` in `def f(*a)` as an undefined global.
    mov rdi, rbx
    mov rsi, [rbp - SE_ARGS]
    call ast_at
    mov edx, [rax + AstNode.b]
    test edx, edx
    jz .star_kw
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
.star_kw:
    mov rdi, rbx
    mov rsi, [rbp - SE_ARGS]
    call ast_at
    mov edx, [rax + AstNode.c]
    test edx, edx
    jz .body
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    jmp .body

.body:
    ; A lambda's body is one expression in .c; a def's is a statement list.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_LAMBDA
    je .lambda_body

    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SE_N], rcx
    mov qword [rbp - SE_I], 0
.stmt_loop:
    mov rax, [rbp - SE_I]
    cmp rax, [rbp - SE_N]
    jae .ok
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SE_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    inc qword [rbp - SE_I]
    jmp .stmt_loop

.lambda_body:
    mov edx, [rax + AstNode.c]
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
.ok:
    ; A method that mentions `super` or `__class__` needs the class itself, and
    ; gets it through a cell the class body leaves behind.  Zero-argument
    ; super() is exactly this and nothing else: the name is implicit, so the
    ; symbol table has to add the use that the source never wrote.
    mov rdi, rbx
    mov rsi, r12
    call sym_note_super
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
END_FUNC sym_enter_function

;; ============================================================================
;; sym_is_function_like(Comp *c, uint32_t scope) -> rax = 1 or 0
;; Module and class blocks are NOT function-like.  Everything downstream of
;; that answer -- fast locals, LOAD_GLOBAL, whether a name can be a cell --
;; turns on it, so it lives in one place.
;; ============================================================================
DEF_FUNC sym_is_function_like, 16
    call sym_at
    mov ecx, [rax + Scope.kind]
    mov eax, 1
    cmp ecx, SCOPE_FUNCTION
    je .yes
    cmp ecx, SCOPE_LAMBDA
    je .yes
    cmp ecx, SCOPE_COMP
    je .yes
    xor eax, eax
.yes:
    leave
    ret
END_FUNC sym_is_function_like

;; ============================================================================
;; sym_analyze(Comp *c, uint32_t scope) -> rax = 1 ok, 0 error
;;
;; Pass two, post-order.  Classify every name this block mentions, then walk the
;; children, then promote to cells whatever a child turned out to need.
;;
;; The order of the tests follows CPython's analyze_name, and it is an order
;; rather than a set of independent rules: an explicit `global` wins over a
;; binding in the same block, a binding wins over an enclosing one, and only a
;; name with no local answer at all looks outward.
;; ============================================================================
SAN_COMP  equ 8
SAN_SCOPE equ 16
SAN_I     equ 24
SAN_N     equ 32
SAN_KEYS  equ 40
SAN_FLAGS equ 48
SAN_NAME  equ 56
SAN_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC sym_analyze, SAN_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi

    ; --- classify this block's own names ---
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov r13, [rax + Scope.symbols]
    ; Walk the dense entry array: it is in insertion order, which is what makes
    ; co_varnames and co_names deterministic rather than hash-order.
    mov rax, [r13 + PyDictObject.dk_nentries]
    mov [rbp - SAN_N], rax
    mov qword [rbp - SAN_I], 0
.name_loop:
    mov rax, [rbp - SAN_I]
    cmp rax, [rbp - SAN_N]
    jae .children

    mov rdi, r13
    mov rsi, [rbp - SAN_I]
    call sym_dict_key_at
    test rax, rax
    jz .next_name
    mov [rbp - SAN_NAME], rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    call sym_get
    mov [rbp - SAN_FLAGS], rax

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SAN_NAME]
    mov rcx, [rbp - SAN_FLAGS]
    call sym_classify
    test eax, eax
    jz .fail
.next_name:
    inc qword [rbp - SAN_I]
    jmp .name_loop

.children:
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov ecx, [rax + Scope.children + Buf.len]
    mov [rbp - SAN_N], rcx
    mov qword [rbp - SAN_I], 0
.child_loop:
    mov rax, [rbp - SAN_I]
    cmp rax, [rbp - SAN_N]
    jae .cells
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov rdx, [rax + Scope.children + Buf.data]
    mov rcx, [rbp - SAN_I]
    mov esi, [rdx + rcx*4]
    mov rdi, rbx
    call sym_analyze
    test eax, eax
    jz .fail
    inc qword [rbp - SAN_I]
    jmp .child_loop

.cells:
    mov rdi, rbx
    mov rsi, r12
    call sym_promote_cells
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_analyze


;; ============================================================================
;; sym_dict_key_at(PyDictObject *d, uint64_t i) -> rax = key Value, or 0
;; The i'th slot of the dense entry array; 0 for a hole.
;; ============================================================================
DEF_FUNC_BARE sym_dict_key_at
    mov rax, [rdi + PyDictObject.entries]
    imul rsi, rsi, DictEntry_size
    mov rax, [rax + rsi + DictEntry.key]
    ret
END_FUNC sym_dict_key_at

;; ============================================================================
;; sym_binds(Comp *c, uint32_t scope, PyStrObject *name) -> rax = 1 or 0
;; Does this block bind the name in a way a nested block could capture?
;;
;; A `global` or `nonlocal` declaration says no, even though the assignment
;; that follows it still stamps DEF_LOCAL on the name -- sym_visit adds that
;; for every store target without looking at the declarations.  `global x;
;; x = 1` writes the module dict, so there is no cell here to close over, and
;; answering yes made the nested block a closure over a slot nobody had
;; allocated: "free variable has no cell in the enclosing scope".  CPython's
;; analyze_name puts a DEF_GLOBAL name in `global` and a DEF_NONLOCAL one in
;; `free`, never in `local` -- and `local` is the only thing that feeds the
;; `bound` set the children are told they may capture.
;; ============================================================================
DEF_FUNC sym_binds, 16
    call sym_get
    mov ecx, eax                        ; the raw flags
    xor eax, eax
    test ecx, DEF_GLOBAL | DEF_NONLOCAL
    jnz .done
    test ecx, DEF_LOCAL | DEF_PARAM | DEF_IMPORT
    setnz al
.done:
    leave
    ret
END_FUNC sym_binds

;; ============================================================================
;; sym_enclosing_binds(Comp *c, uint32_t scope, PyStrObject *name)
;;   -> rax = the binding scope index, or 0
;;
;; Walk outward for a FUNCTION-LIKE block that binds the name.  Class blocks
;; are skipped deliberately: a class attribute is invisible to methods defined
;; in the class, which is why `class C: x = 1` does not put x in scope for
;; `def m(self): return x`.
;; ============================================================================
SEB_COMP  equ 8
SEB_NAME  equ 16
SEB_CUR   equ 24
SEB_FRAME equ 32          ; + 2 pushes = 40
DEF_FUNC sym_enclosing_binds, SEB_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov [rbp - SEB_NAME], rdx
    mov r12, rsi
.loop:
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov ecx, [rax + Scope.parent]
    test ecx, ecx
    jz .none
    mov r12, rcx
    mov [rbp - SEB_CUR], rcx

    mov rdi, rbx
    mov rsi, r12
    call sym_is_function_like
    test eax, eax
    jnz .function_block
    ; A class block provides nothing to nested blocks -- with one exception.
    ; `__class__` is visible to them, which is what makes zero-argument super()
    ; and an explicit __class__ reference work inside a method.
    mov rdi, rbx
    lea rsi, [rel sym_class_name]
    call comp_intern_cstr
    test rax, rax
    jz .loop
    mov rdi, rax
    mov rsi, [rbp - SEB_NAME]
    call sym_str_eq
    test eax, eax
    jz .loop
    jmp .check_binds

.function_block:
    ; A `global x` here ends the walk.  CPython's analyze_name discards the
    ; name from the `bound` set it hands its children, so a binding further out
    ; is invisible from inside this function: in
    ; `def f(): x=1; def g(): global x; def h(): return x`, h reads the
    ; module's x and f gets no cell at all.  A class block is deliberately not
    ; treated this way -- analyze_block copies `bound` for the children before
    ; it looks at the class body's own declarations -- and never reaches here
    ; except for __class__.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SEB_NAME]
    call sym_get
    test eax, DEF_GLOBAL
    jnz .none

.check_binds:

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SEB_NAME]
    call sym_binds
    test eax, eax
    jz .loop
    mov rax, r12
    pop r12
    pop rbx
    leave
    ret
.none:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_enclosing_binds

;; ============================================================================
;; sym_classify(Comp *c, uint32_t scope, PyStrObject *name, uint32_t flags)
;;   -> rax = 1 ok, 0 error
;;
;; The order below is CPython's analyze_name, and it is an order rather than a
;; set of independent rules: an explicit `global` beats a binding in the same
;; block, a binding beats an enclosing one, and only a name with no local
;; answer at all looks outward.
;; ============================================================================
SCL_COMP  equ 8
SCL_SCOPE equ 16
SCL_NAME  equ 24
SCL_FLAGS equ 32
SCL_FRAME equ 40          ; + 1 push = 48
DEF_FUNC sym_classify, SCL_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - SCL_SCOPE], rsi
    mov [rbp - SCL_NAME], rdx
    mov [rbp - SCL_FLAGS], rcx

    ; Already classified (a cell promoted by a nested block) -- leave it alone.
    test ecx, SCOPE_MASK
    jnz .ok

    test ecx, DEF_GLOBAL
    jnz .explicit_global
    test ecx, DEF_NONLOCAL
    jnz .nonlocal
    test ecx, DEF_LOCAL | DEF_PARAM | DEF_IMPORT
    jnz .local

    ; Not bound here: an enclosing function-like block may bind it.
    mov rdi, rbx
    mov rsi, [rbp - SCL_SCOPE]
    mov rdx, [rbp - SCL_NAME]
    call sym_enclosing_binds
    test eax, eax
    jnz .free
    mov ecx, SYM_GLOBAL_IMPLICIT
    jmp .store

.explicit_global:
    test dword [rbp - SCL_FLAGS], DEF_NONLOCAL
    jnz .both
    mov ecx, SYM_GLOBAL_EXPLICIT
    jmp .store

.nonlocal:
    mov rdi, rbx
    mov rsi, [rbp - SCL_SCOPE]
    mov rdx, [rbp - SCL_NAME]
    call sym_enclosing_binds
    test eax, eax
    jz .no_binding
    mov ecx, SYM_FREE
    jmp .store

.local:
    mov ecx, SYM_LOCAL
    jmp .store
.free:
    mov ecx, SYM_FREE

.store:
    shl ecx, SCOPE_SHIFT
    or rcx, [rbp - SCL_FLAGS]
    mov rdi, rbx
    mov rsi, [rbp - SCL_SCOPE]
    mov rdx, [rbp - SCL_NAME]
    call sym_set
.ok:
    mov eax, 1
    pop rbx
    leave
    ret

.both:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "name is nonlocal and global"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    xor eax, eax
    pop rbx
    leave
    ret
.no_binding:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "no binding for nonlocal found"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC sym_classify

;; ============================================================================
;; sym_promote_cells(Comp *c, uint32_t scope) -> rax = 1
;;
;; A local that some nested block referenced as free has to live in a cell
;; rather than a fast slot, because the two frames must share one storage
;; location.  This is the only place LOCAL turns into CELL, and it runs after
;; the children are analyzed for exactly that reason.
;; ============================================================================
SPC_COMP  equ 8
SPC_SCOPE equ 16
SPC_CI    equ 24
SPC_CN    equ 32
SPC_NI    equ 40
SPC_NN    equ 48
SPC_CHILD equ 56
SPC_NAME  equ 64
SPC_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC sym_promote_cells, SPC_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi

    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov ecx, [rax + Scope.children + Buf.len]
    mov [rbp - SPC_CN], rcx
    mov qword [rbp - SPC_CI], 0

.child_loop:
    mov rax, [rbp - SPC_CI]
    cmp rax, [rbp - SPC_CN]
    jae .done
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov rdx, [rax + Scope.children + Buf.data]
    mov rcx, [rbp - SPC_CI]
    mov eax, [rdx + rcx*4]
    mov [rbp - SPC_CHILD], rax

    mov rdi, rbx
    mov rsi, rax
    call sym_at
    mov r13, [rax + Scope.symbols]
    mov rax, [r13 + PyDictObject.dk_nentries]
    mov [rbp - SPC_NN], rax
    mov qword [rbp - SPC_NI], 0

.name_loop:
    mov rax, [rbp - SPC_NI]
    cmp rax, [rbp - SPC_NN]
    jae .next_child
    mov rdi, r13
    mov rsi, rax
    call sym_dict_key_at
    test rax, rax
    jz .next_name
    mov [rbp - SPC_NAME], rax

    ; Is it free in the child?
    mov rdi, rbx
    mov rsi, [rbp - SPC_CHILD]
    mov rdx, rax
    call sym_get
    mov ecx, eax                        ; the child's raw flags
    shr eax, SCOPE_SHIFT
    and eax, 7
    cmp eax, SYM_FREE
    je .free_in_child
    ; A class body that binds a name some block inside it captured keeps that
    ; name LOCAL -- its own stores must stay STORE_NAME -- and records the
    ; capture in DEF_FREE_CLASS instead.  It is still free as far as WE are
    ; concerned: the cell it needs is ours to make.  CPython gets this without
    ; a special case, because analyze_block merges a child's free set into its
    ; own and hands that up regardless of how its symbols resolved.
    test ecx, DEF_FREE_CLASS
    jz .next_name
.free_in_child:

    ; Then it must be a cell here, if we are the block that binds it.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SPC_NAME]
    call sym_get
    mov ecx, eax
    shr eax, SCOPE_SHIFT
    and eax, 7
    cmp eax, SYM_LOCAL
    je .bound_here
    cmp eax, SYM_CELL
    je .next_name
    cmp eax, SYM_FREE
    je .next_name
    ; We do not bind it, so it comes from further out and has to travel through
    ; this block as a free variable of its own.  Without this a two-level
    ; closure breaks: in `def f(): a=1; def g(): def h(): return a`, g never
    ; mentions a, so nothing else would ever mark it free there -- and h would
    ; have no cell to read.
    ; A class block travels too, even though it is not function-like and holds
    ; no fast locals of its own: a method's free variable resolves past the
    ; class body to the enclosing function, and the class body carries it
    ; through in co_freevars so it can hand it on.  Without this,
    ; `def f(): class M: def g(self): return M` had nowhere to put M.
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    cmp dword [rax + Scope.kind], SCOPE_CLASS
    je .class_travels
    mov rdi, rbx
    mov rsi, r12
    call sym_is_function_like
    test eax, eax
    jz .next_name
    ; An explicit `global` is the end of the line: the name is not ours to pass
    ; on, and making it free compiles `global x; x = 1` to a STORE_DEREF into
    ; an enclosing function's cell.  sym_enclosing_binds already stops at a
    ; block that declares the name global, so no child should reach here with
    ; it free; this is the belt to that suspenders.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SPC_NAME]
    call sym_get
    test eax, DEF_GLOBAL
    jnz .next_name
    jmp .travels

.class_travels:
    ; A class body that declares the name global still has to carry it: the
    ; declaration governs the class body's own stores, not a method's read.
    ; `def f(): x = 1` / `class C: global x; x = 9; def m(self): return x`
    ; compiles the 9 to STORE_GLOBAL and still gives C a free x for m's
    ; closure.  CPython's update_symbols sets DEF_FREE_CLASS for a class-block
    ; name that is DEF_BOUND *or* DEF_GLOBAL.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SPC_NAME]
    call sym_get
    test eax, DEF_GLOBAL
    jnz .class_free
.travels:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SPC_NAME]
    call sym_enclosing_binds
    test eax, eax
    jz .next_name
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SPC_NAME]
    call sym_get
    mov ecx, eax
    and ecx, ~SCOPE_MASK
    or ecx, SYM_FREE << SCOPE_SHIFT
    jmp .store_scope
.bound_here:
    ; A function-like block boxes the name: the two frames have to share one
    ; storage location, and a fast slot cannot be shared.
    ;
    ; A class body must not.  Its names live in the mapping __build_class__
    ; hands it, and `name = "H"` has to reach the class dict as STORE_NAME.
    ; Promoting it here compiled that store to STORE_DEREF, so H.name did not
    ; exist and the method read the class body's value rather than the
    ; enclosing function's.  CPython runs analyze_cells for a FunctionBlock
    ; only; a ClassBlock runs drop_class_free instead.
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    cmp dword [rax + Scope.kind], SCOPE_CLASS
    jne .make_cell
    ; ...with one exception, and it is drop_class_free's own: __class__ comes
    ; OUT of the free set and gets a real cell, which is what
    ; LOAD_CLOSURE __class__ / COPY 1 / STORE_NAME __classcell__ hands to
    ; __build_class__ and what zero-argument super() reads back.
    mov rdi, rbx
    mov rsi, [rbp - SPC_NAME]
    call sym_is_class_name
    test eax, eax
    jnz .make_cell

.class_free:
    ; Keep the scope, add the flag.  sym_finalize reads it and lists the name
    ; among the free variables anyway -- that trailing slot is the one the
    ; LOAD_CLOSURE inside the class body will name.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SPC_NAME]
    call sym_get
    mov ecx, eax
    or ecx, DEF_FREE_CLASS
    jmp .store_scope

.make_cell:
    ; ecx has not survived the calls above; re-read rather than rely on it.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SPC_NAME]
    call sym_get
    mov ecx, eax
    and ecx, ~SCOPE_MASK
    or ecx, SYM_CELL << SCOPE_SHIFT
.store_scope:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SPC_NAME]
    call sym_set
.next_name:
    inc qword [rbp - SPC_NI]
    jmp .name_loop
.next_child:
    inc qword [rbp - SPC_CI]
    jmp .child_loop
.done:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_promote_cells

;; ============================================================================
;; sym_build(Comp *c, uint32_t root, int mode) -> rax = the module scope, 0 on error
;; ============================================================================
SB_COMP  equ 8
SB_MODE  equ 16
SB_SCOPE equ 24
SB_I     equ 32
SB_N     equ 40
SB_FRAME equ 40           ; + 3 pushes = 64
DEF_FUNC sym_build, SB_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r13, rsi
    mov [rbp - SB_MODE], rdx

    mov rdi, rbx
    xor esi, esi                        ; no parent
    mov edx, SCOPE_MODULE
    mov rcx, r13
    call sym_new
    mov r12, rax
    mov [rbp - SB_SCOPE], rax

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov [rax + AstNode.flags], r12w

    ; `async def` is a property of the block itself, not of anything inside it,
    ; so it is stamped here rather than discovered by the walk.
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_FUNCTIONDEF
    jne .not_async
    cmp byte [rax + AstNode.subkind], 0
    je .not_async
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    or dword [rax + Scope.flags], SCF_COROUTINE
.not_async:

    ; In eval mode the root is a single expression, not a statement list.
    ; Walking its children as statements would visit nothing at all -- and a
    ; top-level `lambda a: ...` would have no parameters bound anywhere.
    cmp qword [rbp - SB_MODE], CMODE_EVAL
    jne .as_module
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call sym_visit
    test eax, eax
    jz .fail
    jmp .analyze

.as_module:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SB_N], rcx
    mov qword [rbp - SB_I], 0
.loop:
    mov rax, [rbp - SB_I]
    cmp rax, [rbp - SB_N]
    jae .analyze
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SB_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    inc qword [rbp - SB_I]
    jmp .loop

.analyze:
    mov rdi, rbx
    mov rsi, r12
    call sym_analyze
    test eax, eax
    jz .fail
    mov rax, r12
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_build

;; ============================================================================
;; sym_scope_of(Comp *c, uint32_t scope, PyStrObject *name) -> eax = SYM_*
;; The answer the code generator asks for at every name.
;; ============================================================================
DEF_FUNC sym_scope_of, 16
    call sym_get
    shr eax, SCOPE_SHIFT
    and eax, 7
    leave
    ret
END_FUNC sym_scope_of

;; ============================================================================
;; sym_flags_of(Comp *c, uint32_t scope, PyStrObject *name) -> eax = DEF_*
;; The raw flags, for the one question the resolved scope does not answer:
;; whether the name can be unbound where it is read.
;; ============================================================================
DEF_FUNC sym_flags_of, 16
    call sym_get
    leave
    ret
END_FUNC sym_flags_of


;; ============================================================================
;; sym_finalize(Comp *c, uint32_t scope, uint32_t argsnode) -> rax = 1
;;
;; Fix the variable layout.  Order is not cosmetic here: func_call places the
;; *args tuple at co_argcount + co_kwonlyargcount and matches keywords by
;; scanning a window of co_localsplusnames, and COPY_FREE_VARS computes its
;; destination as nlocalsplus - nfree.  So:
;;
;;   varnames    parameters in signature order, then the other locals
;;   localsplus  varnames, then cells that are not already parameters,
;;               then free variables LAST
;;
;; A parameter that is also a cell keeps its parameter slot and is boxed in
;; place by MAKE_CELL, rather than being moved.
;; ============================================================================
SF_COMP  equ 8
SF_SCOPE equ 16
SF_ARGS  equ 24
SF_I     equ 32
SF_N     equ 40
SF_SYMS  equ 48
SF_NAME  equ 56
SF_FRAME equ 56           ; + 3 pushes = 80
DEF_FUNC sym_finalize, SF_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - SF_ARGS], rdx

    ; --- parameters, in signature order ---
    mov rdx, [rbp - SF_ARGS]
    test rdx, rdx
    jz .other_locals
    mov rdi, rbx
    mov rsi, rdx
    call sym_params_into
    test eax, eax
    jz .fail

.other_locals:
    ; --- every other local, in the order the block first mentioned it ---
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov r13, [rax + Scope.symbols]
    mov rax, [r13 + PyDictObject.dk_nentries]
    mov [rbp - SF_N], rax
    mov qword [rbp - SF_I], 0
.local_loop:
    mov rax, [rbp - SF_I]
    cmp rax, [rbp - SF_N]
    jae .cells
    mov rdi, r13
    mov rsi, rax
    call sym_dict_key_at
    test rax, rax
    jz .local_next
    mov [rbp - SF_NAME], rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    call sym_scope_of
    cmp eax, SYM_LOCAL
    je .maybe_local
    cmp eax, SYM_CELL
    jne .local_next
    jmp .add_local
.maybe_local:
    ; A class-body local that a nested block captured belongs among the free
    ; variables instead, and in exactly ONE localsplus slot: COPY_FREE_VARS
    ; fills the last nfree slots, and the LOAD_CLOSURE emitted inside the class
    ; body names that slot.  Listing it here as well would leave sym_lp_index
    ; an earlier, permanently empty slot to find first.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SF_NAME]
    call sym_flags_of
    test eax, DEF_FREE_CLASS
    jnz .local_next
.add_local:
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    lea rdi, [rax + Scope.varnames]
    mov rsi, [rbp - SF_NAME]
    call sym_add_unique
.local_next:
    inc qword [rbp - SF_I]
    jmp .local_loop

.cells:
    ; --- cells and free variables, in the same insertion order ---
    mov qword [rbp - SF_I], 0
.cell_loop:
    mov rax, [rbp - SF_I]
    cmp rax, [rbp - SF_N]
    jae .build_lp
    mov rdi, r13
    mov rsi, rax
    call sym_dict_key_at
    test rax, rax
    jz .cell_next
    mov [rbp - SF_NAME], rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    call sym_scope_of
    cmp eax, SYM_CELL
    je .add_cell
    cmp eax, SYM_FREE
    je .add_free
    ; A class-body name the blocks inside it captured: LOCAL (or explicitly
    ; global) for the code generator, free for the layout.  CPython's
    ; dictbytype(symbols, FREE, DEF_FREE_CLASS, ncells) lands the same name in
    ; co_freevars the same way.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SF_NAME]
    call sym_flags_of
    test eax, DEF_FREE_CLASS
    jnz .add_free
    jmp .cell_next
.add_cell:
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    lea rdi, [rax + Scope.cellvars]
    mov rsi, [rbp - SF_NAME]
    call sym_add_unique
    jmp .cell_next
.add_free:
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    lea rdi, [rax + Scope.freevars]
    mov rsi, [rbp - SF_NAME]
    call sym_add_unique
.cell_next:
    inc qword [rbp - SF_I]
    jmp .cell_loop

.build_lp:
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov r13, rax
    mov rcx, [r13 + Scope.varnames + Buf.len]
    mov [r13 + Scope.nlocals], ecx

    ; localsplus = varnames, then the cells not already among them, then the
    ; free variables.  The free ones MUST come last: COPY_FREE_VARS writes into
    ; the final nfree slots and nothing else tells it where they are.
    xor ecx, ecx
.lp_vars:
    cmp rcx, [r13 + Scope.varnames + Buf.len]
    jae .lp_cells
    mov rdx, [r13 + Scope.varnames + Buf.data]
    mov rsi, [rdx + rcx*8]
    push rcx
    lea rdi, [r13 + Scope.localsplus]
    call buf_push_ptr
    pop rcx
    inc rcx
    jmp .lp_vars
.lp_cells:
    xor ecx, ecx
    mov dword [r13 + Scope.ncells], 0
.lp_cell_loop:
    cmp rcx, [r13 + Scope.cellvars + Buf.len]
    jae .lp_frees
    mov rdx, [r13 + Scope.cellvars + Buf.data]
    mov rsi, [rdx + rcx*8]
    push rcx
    lea rdi, [r13 + Scope.localsplus]
    call sym_add_unique                 ; a cell that is also a parameter stays put
    pop rcx
    inc rcx
    jmp .lp_cell_loop
.lp_frees:
    mov rax, [r13 + Scope.localsplus + Buf.len]
    sub rax, [r13 + Scope.varnames + Buf.len]
    mov [r13 + Scope.ncells], eax       ; cells that needed a new slot
    xor ecx, ecx
.lp_free_loop:
    cmp rcx, [r13 + Scope.freevars + Buf.len]
    jae .lp_done
    mov rdx, [r13 + Scope.freevars + Buf.data]
    mov rsi, [rdx + rcx*8]
    push rcx
    lea rdi, [r13 + Scope.localsplus]
    call buf_push_ptr
    pop rcx
    inc rcx
    jmp .lp_free_loop
.lp_done:
    mov rcx, [r13 + Scope.freevars + Buf.len]
    mov [r13 + Scope.nfrees], ecx
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
END_FUNC sym_finalize

;; ============================================================================
;; sym_add_unique(Buf *b, uint64_t value)
;; Append unless the buffer already holds an equal string.
;; ============================================================================
DEF_FUNC sym_add_unique, 8
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    xor r13d, r13d
.scan:
    cmp r13, [rbx + Buf.len]
    jae .append
    mov rax, [rbx + Buf.data]
    mov rdi, [rax + r13*8]
    mov rsi, r12
    call sym_str_eq
    test eax, eax
    jnz .done
    inc r13
    jmp .scan
.append:
    mov rdi, rbx
    mov rsi, r12
    call buf_push_ptr
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_add_unique

;; ============================================================================
;; sym_str_eq(PyStrObject *a, PyStrObject *b) -> rax = 1 or 0
;; ============================================================================
DEF_FUNC sym_str_eq, 8
    push rbx
    mov rbx, rsi
    cmp rdi, rsi
    je .same
    mov rax, [rdi + PyStrObject.ob_size]
    cmp rax, [rsi + PyStrObject.ob_size]
    jne .differ
    mov rdx, rax
    lea rdi, [rdi + PyStrObject.data]
    lea rsi, [rsi + PyStrObject.data]
    extern ap_memcmp
    call ap_memcmp
    test eax, eax
    jnz .differ
.same:
    mov eax, 1
    pop rbx
    leave
    ret
.differ:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC sym_str_eq

;; ============================================================================
;; sym_is_class_name(Comp *c, PyStrObject *name) -> rax = 1 if it is __class__
;;
;; The one name a class body may hold in a cell.  An interning failure answers
;; no, which is the same direction sym_enclosing_binds takes for it.
;; ============================================================================
DEF_FUNC sym_is_class_name, 8
    push rbx
    mov rbx, rsi
    lea rsi, [rel sym_class_name]
    call comp_intern_cstr
    test rax, rax
    jz .no
    mov rdi, rax
    mov rsi, rbx
    call sym_str_eq
    pop rbx
    leave
    ret
.no:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC sym_is_class_name

;; ============================================================================
;; sym_params_into(Comp *c, uint32_t argsnode) -> rax = 1 ok
;; Append the parameter names to the scope's varnames, in the order
;; co_varnames requires: positional, keyword-only, *args, **kwargs.
;; ============================================================================
SPI_COMP  equ 8
SPI_ARGS  equ 16
SPI_SCOPE equ 24
SPI_I     equ 32
SPI_N     equ 40
SPI_FRAME equ 40          ; + 3 pushes = 64
DEF_FUNC sym_params_into, SPI_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r13, rsi

    ; The scope was left as the current one by the caller.
    mov rdi, rbx
    mov esi, [rbx + Comp.cur_scope]
    call sym_at
    mov r12, rax

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SPI_N], rcx
    mov qword [rbp - SPI_I], 0
.loop:
    mov rax, [rbp - SPI_I]
    cmp rax, [rbp - SPI_N]
    jae .vararg
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SPI_I]
    mov rdi, rbx
    call ast_child
    call .name_of
    lea rdi, [r12 + Scope.varnames]
    mov rsi, rax
    call sym_add_unique
    inc qword [rbp - SPI_I]
    jmp .loop

.vararg:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .varkw
    mov rax, rcx
    call .name_of
    lea rdi, [r12 + Scope.varnames]
    mov rsi, rax
    call sym_add_unique
.varkw:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.c]
    test ecx, ecx
    jz .ok
    mov rax, rcx
    call .name_of
    lea rdi, [r12 + Scope.varnames]
    mov rsi, rax
    call sym_add_unique
.ok:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret

; Local: the PyStrObject of the AST_ARG node in rax.
.name_of:
    sub rsp, 8
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    add rsp, 8
    ret
END_FUNC sym_params_into

;; ============================================================================
;; sym_lp_index(Comp *c, uint32_t scope, PyStrObject *name) -> eax = index, -1
;; The localsplus slot a name occupies.
;; ============================================================================
DEF_FUNC sym_lp_index, 8
    push rbx
    push r12
    push r13
    mov rbx, rdx
    call sym_at
    mov r12, rax
    xor r13d, r13d
.scan:
    cmp r13, [r12 + Scope.localsplus + Buf.len]
    jae .none
    mov rax, [r12 + Scope.localsplus + Buf.data]
    mov rdi, [rax + r13*8]
    mov rsi, rbx
    call sym_str_eq
    test eax, eax
    jnz .found
    inc r13
    jmp .scan
.found:
    mov eax, r13d
    jmp .ret
.none:
    mov eax, -1
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_lp_index


;; ============================================================================
;; sym_note_super(Comp *c, uint32_t scope) -> rax = 1
;; If this block mentions `super` or `__class__` and sits directly inside a
;; class body, record a use of `__class__` here and a binding of it there.
;; ============================================================================
SNS_COMP  equ 8
SNS_SCOPE equ 16
SNS_PARENT equ 24
SNS_NAME  equ 32
SNS_FRAME equ 48          ; + 2 pushes = 64
DEF_FUNC sym_note_super, SNS_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi

    mov rdi, rbx
    mov rsi, r12
    call sym_at
    mov ecx, [rax + Scope.parent]
    mov [rbp - SNS_PARENT], rcx
    test ecx, ecx
    jz .done
    mov rdi, rbx
    mov rsi, rcx
    call sym_at
    cmp dword [rax + Scope.kind], SCOPE_CLASS
    jne .done

    ; Does the block mention either name?
    mov rdi, rbx
    lea rsi, [rel sym_super_name]
    call comp_intern_cstr
    test rax, rax
    jz .done
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call sym_get
    test eax, eax
    jnz .needs_class

    mov rdi, rbx
    lea rsi, [rel sym_class_name]
    call comp_intern_cstr
    test rax, rax
    jz .done
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call sym_get
    test eax, eax
    jz .done

.needs_class:
    mov rdi, rbx
    lea rsi, [rel sym_class_name]
    call comp_intern_cstr
    test rax, rax
    jz .done
    mov [rbp - SNS_NAME], rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    mov ecx, DEF_USE
    call sym_add
    mov rdi, rbx
    mov rsi, [rbp - SNS_PARENT]
    mov rdx, [rbp - SNS_NAME]
    mov ecx, DEF_LOCAL
    call sym_add
.done:
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_note_super

;; ============================================================================
;; sym_enter_comp(Comp *c, uint32_t parent, uint32_t node) -> 1 ok, 0 error
;; The comprehension's own scope: one parameter named `.0` for the outermost
;; iterable, then the targets, conditions and element.
;; ============================================================================
SEC_COMP  equ 8
SEC_PARENT equ 16
SEC_NODE  equ 24
SEC_SCOPE equ 32
SEC_I     equ 40
SEC_N     equ 48
SEC_CL    equ 56
SEC_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC sym_enter_comp, SEC_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - SEC_PARENT], rsi
    mov r13, rdx
    mov [rbp - SEC_NODE], rdx

    mov rdi, rbx
    mov rsi, [rbp - SEC_PARENT]
    mov edx, SCOPE_COMP
    mov rcx, r13
    call sym_new
    mov r12, rax
    mov [rbp - SEC_SCOPE], rax
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov [rax + AstNode.flags], r12w

    ; `async def` is a property of the block itself, not of anything inside it,
    ; so it is stamped here rather than discovered by the walk.
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_FUNCTIONDEF
    jne .not_async
    cmp byte [rax + AstNode.subkind], 0
    je .not_async
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    or dword [rax + Scope.flags], SCF_COROUTINE
.not_async:

    ; The implicit parameter.  CPython calls it `.0`, which no source can name.
    mov rdi, rbx
    lea rsi, [rel sym_dot_zero]
    call comp_intern_cstr
    test rax, rax
    jz .fail
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, DEF_PARAM | DEF_LOCAL
    call sym_add

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SEC_N], rcx
    mov qword [rbp - SEC_I], 0
.clause_loop:
    mov rax, [rbp - SEC_I]
    cmp rax, [rbp - SEC_N]
    jae .element
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SEC_I]
    mov rdi, rbx
    call ast_child
    mov [rbp - SEC_CL], rax

    ; `async for` makes the comprehension's own function a coroutine, exactly
    ; as it would any other block.  Nothing visits the clause node itself --
    ; this loop takes it apart field by field -- so the bit is read here.
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    cmp byte [rax + AstNode.subkind], 0
    je .not_async_clause
    mov rdi, rbx
    mov rsi, r12
    call sym_at
    or dword [rax + Scope.flags], SCF_COROUTINE
.not_async_clause:

    ; The target binds in this scope; the conditions are evaluated here too.
    mov rdi, rbx
    mov rsi, [rbp - SEC_CL]
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail

    ; Every iterable but the outermost is evaluated inside the comprehension.
    cmp qword [rbp - SEC_I], 0
    je .conds
    mov rdi, rbx
    mov rsi, [rbp - SEC_CL]
    call ast_at
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
.conds:
    mov rdi, rbx
    mov rsi, [rbp - SEC_CL]
    call ast_at
    mov rsi, rax
    mov rdi, rbx
    mov rdx, r12
    call sym_visit_children
    test eax, eax
    jz .fail
    inc qword [rbp - SEC_I]
    jmp .clause_loop

.element:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]          ; a dict comprehension's value
    test edx, edx
    jz .ok
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
.ok:
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
END_FUNC sym_enter_comp

;; ============================================================================
;; sym_visit_children(Comp *c, AstNode *n, uint32_t scope) -> 1 ok, 0 error
;; Visit a node's child list only.
;; ============================================================================
SVC_COMP  equ 8
SVC_NODE  equ 16
SVC_SCOPE equ 24
SVC_I     equ 32
SVC_N     equ 40
SVC_CLIST equ 48
SVC_FRAME equ 48          ; + 2 pushes = 64
DEF_FUNC sym_visit_children, SVC_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rdx
    mov ecx, [rsi + AstNode.nchild]
    mov [rbp - SVC_N], rcx
    mov ecx, [rsi + AstNode.clist]
    mov [rbp - SVC_CLIST], rcx
    mov qword [rbp - SVC_I], 0
.loop:
    mov rax, [rbp - SVC_I]
    cmp rax, [rbp - SVC_N]
    jae .ok
    mov rdx, [rbx + Comp.children + Buf.data]
    mov rcx, [rbp - SVC_CLIST]
    add rcx, rax
    mov edx, [rdx + rcx*4]
    mov rdi, rbx
    mov rsi, r12
    call sym_visit
    test eax, eax
    jz .fail
    inc qword [rbp - SVC_I]
    jmp .loop
.ok:
    mov eax, 1
.fail:
    pop r12
    pop rbx
    leave
    ret
END_FUNC sym_visit_children

section .rodata
sym_dot_zero: db ".0", 0

sym_super_name: db "super", 0
sym_class_name: db "__class__", 0


ASM_INIT
