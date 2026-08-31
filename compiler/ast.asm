; ast.asm - The AST, and the growable buffers it lives in
;
; A node is 32 bytes in a Buf, addressed by u32 index; the tree has no
; allocator of its own beyond that.  Buf and the bump Arena are here because
; the node representation is not separable from them -- ast.asm's entire
; extern list was buf_reserve, buf_push_u32 and buf_push_ptr.
;
; Nodes are fixed 32-byte records in a Buf, addressed by u32 index rather than
; by pointer.  The Buf is reallocated as it grows, so a pointer taken before a
; node is added is stale afterwards; an index never is.  Node 0 is reserved as
; the null node by comp_init, which lets a 0 field read as "absent" with no
; separate presence flag anywhere.
;
; Variable-arity children (call arguments, statement bodies, list elements) go
; in a second u32 array.  They cannot be appended to it directly, because
; nested constructs are under construction at the same time -- f(a, [b, c], d)
; has three lists open at once.  So they are staged on a LIFO pending stack and
; copied into place when the construct closes:
;
;     mark = ast_mark(c)
;     ... ast_push(c, child) for each ...
;     ast_commit(c, mark)      -> (offset, count) stored in .clist / .nchild
;
; Recursive descent always closes lists in the order it opened them, so LIFO is
; exactly right and the staging never needs a search.

%include "macros.inc"
%include "object.inc"
%include "compiler.inc"

; --- Named frame-layout constants ---
AM_KIND  equ 16
AM_SUB   equ 24
AM_LINE  equ 32
AM_A     equ 40
AM_B     equ 48
AM_FRAME equ 56          ; + 1 push = 64

section .text

;; ============================================================================
;; ast_at(Comp *c, uint32_t idx) -> AstNode*
;; Valid only until the next ast_make; recompute rather than cache.
;; ============================================================================
DEF_FUNC_BARE ast_at
    mov rax, [rdi + Comp.nodes + Buf.data]
    shl rsi, AST_SHIFT
    add rax, rsi
    ret
END_FUNC ast_at

;; ============================================================================
;; ast_make(Comp *c, int kind, int subkind, int lineno, uint32_t a, uint32_t b)
;;   -> rax = the new node's index
;;
;; .c, .clist and .nchild are left zero; the few node kinds that use them fill
;; them in through ast_at once their children are known.
;; ============================================================================
DEF_FUNC ast_make, AM_FRAME
    push rbx
    mov [rbp - AM_KIND], rsi
    mov [rbp - AM_SUB], rdx
    mov [rbp - AM_LINE], rcx
    mov [rbp - AM_A], r8
    mov [rbp - AM_B], r9
    mov rbx, rdi

    lea rdi, [rbx + Comp.nodes]
    mov esi, 1
    call buf_reserve                    ; rax = AstNode*

    mov rdx, [rbp - AM_KIND]
    mov [rax + AstNode.kind], dl
    mov rdx, [rbp - AM_SUB]
    mov [rax + AstNode.subkind], dl
    mov word [rax + AstNode.flags], 0
    mov rdx, [rbp - AM_LINE]
    mov [rax + AstNode.lineno], edx
    mov rdx, [rbp - AM_A]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - AM_B]
    mov [rax + AstNode.b], edx
    mov dword [rax + AstNode.c], 0
    mov dword [rax + AstNode.clist], 0
    mov dword [rax + AstNode.nchild], 0
    mov dword [rax + AstNode.col], 0

    ; The index is one less than the new length, since buf_reserve appended it.
    mov rax, [rbx + Comp.nodes + Buf.len]
    dec rax
    pop rbx
    leave
    ret
END_FUNC ast_make

;; ============================================================================
;; ast_mark(Comp *c) -> rax = the current height of the pending stack
;; ============================================================================
DEF_FUNC_BARE ast_mark
    mov rax, [rdi + Comp.pending + Buf.len]
    ret
END_FUNC ast_mark

;; ============================================================================
;; ast_push(Comp *c, uint32_t node)
;; ============================================================================
DEF_FUNC_BARE ast_push
    lea rdi, [rdi + Comp.pending]
    jmp buf_push_u32
END_FUNC ast_push

;; ============================================================================
;; ast_commit(Comp *c, uint64_t mark) -> rax = child-list offset, rdx = count
;;
;; Moves everything pushed since `mark` out of the pending stack and into the
;; child-list array, leaving the stack as it was.  An empty list commits to
;; offset 0 with count 0, which is indistinguishable from "no children" -- and
;; that is fine, because .nchild is what any reader consults.
;; ============================================================================
AC_MARK  equ 8
AC_OFF   equ 16
AC_CNT   equ 24
AC_FRAME equ 24          ; + 3 pushes = 48
DEF_FUNC ast_commit, AC_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - AC_MARK], rsi

    mov r13, [rbx + Comp.pending + Buf.len]
    sub r13, rsi                        ; r13 = how many were staged
    jz .empty

    mov [rbp - AC_CNT], r13
    mov rax, [rbx + Comp.children + Buf.len]
    mov [rbp - AC_OFF], rax             ; the offset this list will live at

    lea rdi, [rbx + Comp.children]
    mov rsi, r13
    call buf_reserve
    mov r12, rax                        ; destination

    ; Source is read after the reserve, because growing children cannot move
    ; pending -- but reading it afterwards costs nothing and keeps the two
    ; buffers' lifetimes independent.
    mov rax, [rbp - AC_MARK]
    mov rsi, [rbx + Comp.pending + Buf.data]
    lea rsi, [rsi + rax*4]

    xor ecx, ecx
.copy:
    mov edx, [rsi + rcx*4]
    mov [r12 + rcx*4], edx
    inc rcx
    cmp rcx, r13
    jb .copy

    mov rax, [rbp - AC_MARK]
    mov [rbx + Comp.pending + Buf.len], rax     ; pop the staging area

    mov rax, [rbp - AC_OFF]
    mov rdx, [rbp - AC_CNT]
    pop r13
    pop r12
    pop rbx
    leave
    ret

.empty:
    mov [rbx + Comp.pending + Buf.len], rsi
    xor eax, eax
    xor edx, edx
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC ast_commit

;; ============================================================================
;; ast_child(Comp *c, AstNode *n, uint64_t i) -> rax = the i'th child index
;; ============================================================================
DEF_FUNC_BARE ast_child
    mov eax, [rsi + AstNode.clist]
    add rax, rdx
    mov rdx, [rdi + Comp.children + Buf.data]
    mov eax, [rdx + rax*4]
    ret
END_FUNC ast_child

;; ============================================================================
;; ast_obj(Comp *c, Value v) -> rax = index into comp.objs
;;
;; Takes ownership.  A Value, not a pointer: an int inside +-2^50 and every
;; float are immediates rather than heap objects, and comp_free releases the
;; table with DECREF_V accordingly.
;;
;; Everything the front end allocates goes through here, so comp_free has one
;; loop that releases the lot however the compilation ended -- which is what
;; lets the parser abandon a deeply nested construct without unwinding
;; anything by hand.
;; ============================================================================
DEF_FUNC ast_obj, 8            ; the 8 pads the push to a 16-aligned rsp
    push rbx
    mov rbx, rdi
    lea rdi, [rbx + Comp.objs]
    call buf_push_ptr
    mov rax, [rbx + Comp.objs + Buf.len]
    dec rax
    pop rbx
    leave
    ret
END_FUNC ast_obj

;; ============================================================================
;; ast_obj_at(Comp *c, uint32_t idx) -> rax = the borrowed Value
;; ============================================================================
DEF_FUNC_BARE ast_obj_at
    mov rax, [rdi + Comp.objs + Buf.data]
    mov rax, [rax + rsi*8]
    ret
END_FUNC ast_obj_at

;; ============================================================================
;; ast_set_ctx(Comp *c, uint32_t node, int ctx) -> rax = 1 ok, 0 not assignable
;;
;; Python parses an assignment target as an ordinary expression and only then
;; decides it was a target -- `a, b = t` and `a, b` are the same production
;; until the `=` appears.  This walks the parsed expression and re-marks it,
;; which is also where "cannot assign to a literal" is discovered: a node kind
;; with no store form simply fails here.
;;
;; Only the container kinds recurse: `[a, b] = t` and `(a, b) = t` both unpack,
;; while `f(x) = 1` does not, because a call has no store form.
;; ============================================================================
SC_CTX   equ 24
SC_I     equ 32
SC_N     equ 40
SC_FRAME equ 40          ; + 3 pushes = 64
DEF_FUNC ast_set_ctx, SC_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbp - SC_CTX], rdx

    test r12, r12
    jz .bad
    mov rdi, rbx
    mov rsi, r12
    call ast_at
    mov r13, rax
    movzx eax, byte [r13 + AstNode.kind]

    cmp eax, AST_NAME
    je .simple
    cmp eax, AST_ATTRIBUTE
    je .simple
    cmp eax, AST_SUBSCRIPT
    je .simple
    cmp eax, AST_TUPLE
    je .container
    cmp eax, AST_LIST
    je .container
    cmp eax, AST_STARRED
    je .starred
    jmp .bad

.simple:
    mov rdx, [rbp - SC_CTX]
    mov [r13 + AstNode.subkind], dl
    mov eax, 1
    jmp .ret

.starred:
    mov rdx, [rbp - SC_CTX]
    mov [r13 + AstNode.subkind], dl
    mov edx, [r13 + AstNode.a]
    mov rdi, rbx
    mov rsi, rdx
    mov rdx, [rbp - SC_CTX]
    call ast_set_ctx
    jmp .ret

.container:
    mov rdx, [rbp - SC_CTX]
    mov [r13 + AstNode.subkind], dl
    mov ecx, [r13 + AstNode.nchild]
    mov [rbp - SC_N], rcx
    mov qword [rbp - SC_I], 0
.loop:
    mov rax, [rbp - SC_I]
    cmp rax, [rbp - SC_N]
    jae .ok
    mov rdi, rbx
    mov rsi, r12
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SC_I]
    mov rdi, rbx
    call ast_child
    mov rsi, rax
    mov rdi, rbx
    mov rdx, [rbp - SC_CTX]
    call ast_set_ctx
    test eax, eax
    jz .bad
    inc qword [rbp - SC_I]
    jmp .loop
.ok:
    mov eax, 1
    jmp .ret
.bad:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC ast_set_ctx

;; ============================================================================
;; (was compiler/arena.asm)
;; ============================================================================

section .text

extern ap_malloc
extern ap_realloc
extern ap_free

; --- Named frame-layout constants ---

; buf_push_* save their two arguments across the buf_grow call: buf_grow
; clobbers rdi (it passes Buf.data to ap_realloc), and a bare push/pop pair
; would leave rsp misaligned for libc's SSE stores.
BP_BUF   equ 8
BP_VAL   equ 16
BP_FRAME equ 16

section .text

;; ============================================================================
;; buf_init(Buf *b, size_t elsz)
;; Start empty with no allocation.  The first buf_reserve does the malloc, so
;; a Buf that is never written costs nothing.
;; ============================================================================
DEF_FUNC_BARE buf_init
    mov qword [rdi + Buf.data], 0
    mov qword [rdi + Buf.len], 0
    mov qword [rdi + Buf.cap], 0
    mov [rdi + Buf.elsz], rsi
    ret
END_FUNC buf_init

;; ============================================================================
;; buf_free(Buf *b)
;; Release the storage and reset to empty.  Does not touch element contents --
;; a Buf holding owned references must be drained by its owner first.
;; ============================================================================
DEF_FUNC buf_free, 8            ; the 8 pads to a 16-aligned rsp at the call
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + Buf.data]
    call ap_free
    mov qword [rbx + Buf.data], 0
    mov qword [rbx + Buf.len], 0
    mov qword [rbx + Buf.cap], 0
    pop rbx
    leave
    ret
END_FUNC buf_free

;; ============================================================================
;; buf_grow(Buf *b, size_t need)
;; Ensure capacity for at least `need` elements.  Doubles, with a floor of
;; BUF_MIN_CAP, so appending n elements costs O(n) copies total.
;; ============================================================================
DEF_FUNC buf_grow
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi                        ; r12 = needed capacity

    cmp r12, [rbx + Buf.cap]
    jbe .done                           ; already large enough

    mov rax, [rbx + Buf.cap]
    test rax, rax
    jnz .have_cap
    mov eax, BUF_MIN_CAP
.have_cap:
.double:
    cmp rax, r12
    jae .got_cap
    add rax, rax                        ; cap *= 2
    jmp .double
.got_cap:
    mov r12, rax                        ; r12 = new capacity

    mov rdi, [rbx + Buf.data]
    mov rsi, r12
    imul rsi, [rbx + Buf.elsz]
    call ap_realloc                      ; ap_realloc(NULL, n) == malloc(n)
    mov [rbx + Buf.data], rax
    mov [rbx + Buf.cap], r12

.done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC buf_grow

;; ============================================================================
;; buf_reserve(Buf *b, size_t n) -> void *first_new_element
;; Append n uninitialised elements and return a pointer to the first.  The
;; pointer is invalidated by the next reserve, so callers must not hold it
;; across one -- store an index instead.
;; ============================================================================
DEF_FUNC buf_reserve
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi

    mov rsi, [rbx + Buf.len]
    add rsi, r12
    call buf_grow                        ; rdi is still b here

    mov rax, [rbx + Buf.len]
    mov rdx, rax
    add rdx, r12
    mov [rbx + Buf.len], rdx             ; len += n

    imul rax, [rbx + Buf.elsz]
    add rax, [rbx + Buf.data]            ; &data[old_len]

    pop r12
    pop rbx
    leave
    ret
END_FUNC buf_reserve

;; ============================================================================
;; buf_push_u32(Buf *b, uint32_t v)
;; The child-list and label arenas are dense u32 arrays; this is their hot
;; path, so it inlines the capacity check rather than calling buf_reserve.
;; ============================================================================
DEF_FUNC buf_push_u32, BP_FRAME
    mov rax, [rdi + Buf.len]
    cmp rax, [rdi + Buf.cap]
    jb .fast
    mov [rbp - BP_BUF], rdi
    mov [rbp - BP_VAL], rsi
    lea rsi, [rax + 1]
    call buf_grow
    mov rdi, [rbp - BP_BUF]
    mov rsi, [rbp - BP_VAL]
    mov rax, [rdi + Buf.len]
.fast:
    mov rdx, [rdi + Buf.data]
    mov [rdx + rax*4], esi
    inc rax
    mov [rdi + Buf.len], rax
    leave
    ret
END_FUNC buf_push_u32

;; ============================================================================
;; buf_push_u8(Buf *b, uint8_t v)
;; Used by the bytecode, exception-table and line-table emitters.
;; ============================================================================
DEF_FUNC buf_push_u8, BP_FRAME
    mov rax, [rdi + Buf.len]
    cmp rax, [rdi + Buf.cap]
    jb .fast
    mov [rbp - BP_BUF], rdi
    mov [rbp - BP_VAL], rsi
    lea rsi, [rax + 1]
    call buf_grow
    mov rdi, [rbp - BP_BUF]
    mov rsi, [rbp - BP_VAL]
    mov rax, [rdi + Buf.len]
.fast:
    mov rdx, [rdi + Buf.data]
    mov [rdx + rax], sil
    inc rax
    mov [rdi + Buf.len], rax
    leave
    ret
END_FUNC buf_push_u8

;; ============================================================================
;; buf_push_ptr(Buf *b, void *v)
;; The object table (comp.objs) is a Buf of owned PyObject*.
;; ============================================================================
DEF_FUNC buf_push_ptr, BP_FRAME
    mov rax, [rdi + Buf.len]
    cmp rax, [rdi + Buf.cap]
    jb .fast
    mov [rbp - BP_BUF], rdi
    mov [rbp - BP_VAL], rsi
    lea rsi, [rax + 1]
    call buf_grow
    mov rdi, [rbp - BP_BUF]
    mov rsi, [rbp - BP_VAL]
    mov rax, [rdi + Buf.len]
.fast:
    mov rdx, [rdi + Buf.data]
    mov [rdx + rax*8], rsi
    inc rax
    mov [rdi + Buf.len], rax
    leave
    ret
END_FUNC buf_push_ptr

;; ============================================================================
;; arena_init(Arena *a)
;; ============================================================================
DEF_FUNC_BARE arena_init
    mov qword [rdi + Arena.cur], 0
    mov qword [rdi + Arena.end], 0
    mov qword [rdi + Arena.chunks], 0
    mov qword [rdi + Arena.total], 0
    ret
END_FUNC arena_init

;; ============================================================================
;; arena_alloc(Arena *a, size_t n) -> void *
;; Bump allocate, 8-byte aligned.  The fast path is five instructions; the slow
;; path links a fresh chunk, sized to whichever is larger of ARENA_CHUNK and
;; the request itself, so an oversized single allocation still works.
;; ============================================================================
DEF_FUNC arena_alloc, 8         ; lint: pushes=3 -- the slow path's, at .new_chunk
    add rsi, 7
    and rsi, -8                         ; round the request up to 8

    mov rax, [rdi + Arena.cur]
    lea rdx, [rax + rsi]
    cmp rdx, [rdi + Arena.end]
    ja .new_chunk
    mov [rdi + Arena.cur], rdx
    add [rdi + Arena.total], rsi
    leave
    ret

.new_chunk:
    push rbx
    push r12
    push r13
    mov rbx, rdi                        ; arena
    mov r12, rsi                        ; rounded request

    mov r13, ARENA_CHUNK
    lea rax, [r12 + 16]                 ; payload + link header
    cmp r13, rax
    jae .have_size
    mov r13, rax
.have_size:
    mov rdi, r13
    call ap_malloc
    ; chunk layout: [0]=next [8]=size [16..]=payload
    mov rdx, [rbx + Arena.chunks]
    mov [rax], rdx
    mov [rax + 8], r13
    mov [rbx + Arena.chunks], rax

    lea rdx, [rax + 16]                 ; payload start
    lea rcx, [rax + r13]                ; chunk end
    mov [rbx + Arena.end], rcx

    lea rcx, [rdx + r12]
    mov [rbx + Arena.cur], rcx
    add [rbx + Arena.total], r12
    mov rax, rdx                        ; return the payload start

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC arena_alloc

;; ============================================================================
;; arena_free(Arena *a)
;; Walk the chunk list and release everything.  This is the whole point of the
;; arena: one call disposes of every AST node and symbol the parse produced,
;; however the compilation ended.
;; ============================================================================
DEF_FUNC arena_free
    push rbx
    push r12
    mov rbx, rdi
    mov r12, [rbx + Arena.chunks]
.loop:
    test r12, r12
    jz .done
    mov rdi, r12
    mov r12, [r12]                      ; next, before the free
    call ap_free
    jmp .loop
.done:
    mov qword [rbx + Arena.cur], 0
    mov qword [rbx + Arena.end], 0
    mov qword [rbx + Arena.chunks], 0
    pop r12
    pop rbx
    leave
    ret
END_FUNC arena_free

ASM_INIT
