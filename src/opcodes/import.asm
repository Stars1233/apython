; opcodes/import.asm - IMPORT_NAME and IMPORT_FROM opcode handlers

%include "macros.inc"
%include "object.inc"

extern eval_dispatch
extern eval_saved_r13
extern import_module
extern obj_decref
extern raise_exception
extern exc_ImportError_type
extern eval_co_names
extern eval_saved_rbx
extern obj_dealloc
extern opcode_table
extern opcode_dispatch_table

;; ============================================================================
;; op_import_name - Opcode 108: IMPORT_NAME
;;
;; Stack in: [level, fromlist] (TOS = fromlist, TOS1 = level)
;; Stack out: [module]
;;
;; ecx = arg = index into co_names for module name
;; ============================================================================
DEF_FUNC_BARE op_import_name
    ; Get module name from co_names[ecx] (payload array: 8-byte stride)
    shl ecx, 3
    LOAD_CO_NAMES rax
    mov rax, [rax + rcx]       ; name_str from co_names

    ; Pop fromlist (TOS)
    VPOP_VAL rsi, r8            ; fromlist payload+tag

    ; Pop level (TOS1)
    VPOP_VAL rdx, rcx           ; level payload+tag

    ; Save name, fromlist (payload+tag), and level for later.  FOUR pushes,
    ; deliberately: a handler is jumped to rather than called, so rsp arrives
    ; 16-aligned and every call below has to be made at an even push count.
    push rax                    ; name
    push r8                     ; fromlist tag
    push rsi                    ; fromlist payload
    push rdx                    ; level

    ; The two operands are ours now.  eval_saved_r13 was captured by
    ; eval_dispatch *before* those pops, so a non-local unwind out of
    ; import_module would have walked the value stack back down over both
    ; slots and DECREF'd them a second time -- a double free of the
    ; fromlist tuple on every failing import.
    mov [rel eval_saved_r13], r13

    ; Decode level from SmallInt
    cmp ecx, TAG_SMALLINT
    je .decode_smallint
    ; Not a SmallInt — assume 0
    xor edx, edx
    jmp .do_import

.decode_smallint:
    ; rdx already holds the raw integer payload (no decoding needed for fat values)

.do_import:
    ; A relative import is resolved against the importing module's package
    ; before anything else happens; import_module only knows absolute names.
    test rdx, rdx
    jz .absolute
    push rax
    sub rsp, 8                  ; pad: an odd push makes the call below odd
    mov rdi, rax                ; the name as written
    mov rsi, [r12 + PyFrame.globals]
    extern import_resolve_relative
    call import_resolve_relative
    mov rdx, rax                ; the resolved name, owned
    add rsp, 8
    pop rax
    mov [rsp + 24], rdx         ; keep it where the saved name lives
    mov rax, rdx
    mov rdx, 0                  ; it is absolute now
    mov r15d, 1                 ; and the name is ours to release
    jmp .have_name
.absolute:
    xor r15d, r15d
.have_name:
    ; The flag lives in r15, which the register convention leaves free for a
    ; handler and which is callee-saved, so it survives import_module.  It
    ; used to be a FIFTH push, and that is what misaligned this call -- and
    ; with it every frame the imported module ran, since the misalignment
    ; propagates down the whole nested interpreter stack.

    ; import_module(name_str, fromlist, level)
    mov rdi, rax                ; name
    mov rsi, [rsp + 8]          ; fromlist
    ; rdx = level (already set)
    call import_module
    ; rax = module (new reference)

    test r15d, r15d
    jz .no_resolved_name
    push rax
    sub rsp, 8                  ; pad, as above
    mov rdi, [rsp + 40]         ; the resolved name we built
    call obj_decref
    add rsp, 8
    pop rax
.no_resolved_name:

    add rsp, 8                  ; discard level (SmallInt, no refcount)

    ; DECREF fromlist (may be None/TAG_NONE — use fat DECREF_VAL)
    pop rdi                     ; fromlist payload
    pop rsi                     ; fromlist tag
    push rax                    ; save module across DECREF
    DECREF_VAL rdi, rsi
    pop rax                     ; restore module

    add rsp, 8                  ; pop saved name (borrowed ref, no DECREF)

    ; Push module onto value stack
    test rax, rax
    jz .import_failed
    VPUSH_PTR rax
    DISPATCH

.import_failed:
    ; import_module raises for a module it cannot find, but returns NULL for
    ; one whose body raised -- that exception is already pending and must be
    ; propagated, not replaced.
    extern current_exception
    extern eval_exception_unwind
    cmp qword [rel current_exception], 0
    jne .propagate_import_exc
    RAISE exc_ImportError_type, "import failed"

.propagate_import_exc:
    mov [rel eval_saved_r13], r13
    jmp eval_exception_unwind
END_FUNC op_import_name

;; ============================================================================
;; op_import_from - Opcode 109: IMPORT_FROM
;;
;; Stack in: [module] (TOS = module, NOT popped)
;; Stack out: [module, attr]
;;
;; ecx = arg = index into co_names for attribute name
;;
;; If attr not found on module, tries importing <pkg_name>.<attr_name>
;; as a submodule (CPython submodule fallback).
;; ============================================================================
extern dict_get
extern dict_set
extern obj_incref
extern str_from_cstr_heap
extern str_concat
extern import_find_and_load

IF_ATTR  equ 8
IF2_MOD  equ 16
IF2_SUB  equ 24             ; the submodule the fallback loaded
IF2_T1   equ 40             ; the two temporaries the dotted-name concat used
IF2_T2   equ 48             ; to push and pop one at a time
IF2_FRAME equ 56            ; 40 was the aligned size; +16 keeps the parity

DEF_FUNC op_import_from, IF2_FRAME
    ; Get attribute name from co_names[ecx] (payload array: 8-byte stride)
    shl ecx, 3
    LOAD_CO_NAMES rsi
    mov rsi, [rsi + rcx]       ; attr name_str
    mov [rbp - IF_ATTR], rsi

    ; Peek module (TOS, don't pop)
    VPEEK rdi
    mov [rbp - IF2_MOD], rdi

    ; Get module's type and tp_getattr
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_getattr]
    test rax, rax
    jz .if_no_getattr

    ; Call tp_getattr(module, name_str)
    call rax
    V_UNPACK rax, rdx           ; tp_getattr returns a Value
    test edx, edx
    jnz .if_got_attr

    ; tp_getattr returned NULL — try dict_get directly
    mov rdi, [rbp - IF2_MOD]
    mov rdi, [rdi + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .if_try_submodule
    mov rsi, [rbp - IF_ATTR]
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .if_found_in_dict
    jmp .if_try_submodule

.if_no_getattr:
    ; No tp_getattr — try dict_get on module dict
    mov rdi, [rbp - IF2_MOD]
    mov rdi, [rdi + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .if_try_submodule
    mov rsi, [rbp - IF_ATTR]
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .if_found_in_dict
    jmp .if_try_submodule

.if_found_in_dict:
    INCREF_VAL rax, edx
    VPUSH_VAL rax, rdx
    leave
    DISPATCH

.if_got_attr:
    VPUSH_VAL rax, rdx
    leave
    DISPATCH

.if_try_submodule:
    ; Submodule fallback: construct "<pkg_name>.<attr_name>" and try importing
    ; Get module's __name__ from its dict
    mov rdi, [rbp - IF2_MOD]
    mov rdi, [rdi + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .if_error

    ; The attribute is not there, so it may be a SUBMODULE the package's own
    ; body never bound.  import_submodule_attr builds the dotted name, loads
    ; it, and binds it on the package -- which is also what IMPORT_STAR needs,
    ; and used not to have.
    mov rdi, [rbp - IF2_MOD]
    mov rsi, [rbp - IF_ATTR]
    extern import_submodule_attr
    call import_submodule_attr
    test rax, rax
    jz .if_error
    mov [rbp - IF2_SUB], rax

    ; import_find_and_load hands back a BORROWED reference -- sys.modules owns
    ; it -- and the value stack owns what it holds.
    mov rdi, [rbp - IF2_SUB]
    call obj_incref
    mov rax, [rbp - IF2_SUB]
    VPUSH_PTR rax
    leave
    DISPATCH

.if_error:
    ; A submodule whose body raised leaves its exception pending; reporting
    ; "cannot import name" over it would hide the real cause.
    cmp qword [rel current_exception], 0
    jne .propagate_from_exc
    RAISE exc_ImportError_type, "cannot import name"

.propagate_from_exc:
    mov [rel eval_saved_r13], r13
    leave
    jmp eval_exception_unwind
END_FUNC op_import_from

section .rodata
if_dunder_name: db "__name__", 0
if_dot_str: db ".", 0
section .text
