; arena.asm - Growable buffers and bump arenas for the Python source compiler
;
; Two allocation shapes, because the compiler has exactly two needs:
;
;   Buf   - a growable array of fixed-size elements, doubling on demand.  The
;           token stream, the child-list arena, the instruction stream and the
;           three byte emitters are all Bufs.
;   Arena - bump allocation released wholesale.  AST nodes and symbol table
;           entries are never freed individually, because a compilation is a
;           bounded episode: allocate freely, drop the lot at the end.
;
; ap_malloc and ap_realloc are fatal on OOM and never return NULL, so nothing
; in this file has a failure path.

%include "macros.inc"
%include "object.inc"
%include "compiler.inc"

extern ap_malloc
extern ap_realloc
extern ap_free
extern ap_memcpy
extern ap_memset

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
