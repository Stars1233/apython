; alloc.asm - Where memory comes from
;
; ap_malloc, ap_free and ap_realloc are the WHOLE funnel.  Nothing else in the
; tree calls libc malloc, free or realloc -- GMP and zlib allocate and free
; their own -- so every byte the interpreter owns passes through these three,
; and they are free to be something other than a libc wrapper.
;
; Today they are still that wrapper.  This file exists first as a pure move
; out of runtime.asm, so that the commit which makes them a pool allocator is
; a diff of the allocator and not a diff of where the allocator lives.
;
; Below the object model, like the rest of runtime.asm was: nothing here can
; allocate a Python object or raise a Python exception, and out of memory is
; fatal_error rather than a return of NULL -- which is why not one of the 183
; call sites in the tree has a failure path.

%include "macros.inc"
%include "object.inc"

extern fatal_error

extern malloc
extern free
extern realloc

section .text
;; ============================================================================
;; ap_malloc(rdi = size) -> rax = a block of at least that many bytes
;;
;; Never returns NULL: out of memory is fatal_error, which is why no call site
;; in the tree has a failure path.  16-byte aligned, as glibc's is.
;; ============================================================================
DEF_FUNC ap_malloc, 16           ; 0 pushes, so rsp is 16-aligned
    ; No register is saved here: the size used to be parked in rbx "for the
    ; error case" and no path ever read it back, so every allocation in the
    ; interpreter paid a push, a mov and a pop for nothing.
    call malloc wrt ..plt
    test rax, rax
    jz .oom
    leave
    ret
.oom:
    lea rdi, [rel ap_oom_msg]
    call fatal_error        ; never returns
END_FUNC ap_malloc

;; ============================================================================
;; ap_free(rdi = ptr) -> void
;;
;; NULL-safe, and a pure tail call, so it preserves the caller's alignment.
;; ============================================================================
DEF_FUNC_BARE ap_free
    test rdi, rdi
    jz .null
    jmp free wrt ..plt
.null:
    ret
END_FUNC ap_free

;; ============================================================================
;; ap_realloc(rdi = ptr, rsi = size) -> rax = the resized block
;;
;; ap_realloc(NULL, n) is malloc(n), which src/compiler/ast.asm's Buf grower
;; relies on.  Never returns NULL, as ap_malloc does not.
;; ============================================================================
DEF_FUNC ap_realloc, 16           ; 0 pushes, so rsp is 16-aligned
    ; As in ap_malloc: the saved size was never read on the error path.
    call realloc wrt ..plt
    test rax, rax
    jz .oom
    leave
    ret
.oom:
    lea rdi, [rel ap_oom_msg]
    call fatal_error        ; never returns
END_FUNC ap_realloc

section .rodata
ap_oom_msg: db "Fatal: out of memory", 0
