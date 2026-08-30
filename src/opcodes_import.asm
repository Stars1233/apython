; opcodes_import.asm - IMPORT_NAME and IMPORT_FROM opcode handlers

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"

extern eval_dispatch
extern eval_saved_rbx
extern eval_saved_r13
extern eval_co_names
extern opcode_table
extern import_module
extern obj_decref
extern obj_dealloc
extern raise_exception
extern exc_ImportError_type

; ============================================================================
; op_import_name - Opcode 108: IMPORT_NAME
;
; Stack in: [level, fromlist] (TOS = fromlist, TOS1 = level)
; Stack out: [module]
;
; ecx = arg = index into co_names for module name
; ============================================================================
DEF_FUNC_BARE op_import_name
    ; Get module name from co_names[ecx] (payload array: 8-byte stride)
    shl ecx, 3
    LOAD_CO_NAMES rax
    mov rax, [rax + rcx]       ; name_str from co_names

    ; Pop fromlist (TOS)
    VPOP_VAL rsi, r8            ; fromlist payload+tag

    ; Pop level (TOS1)
    VPOP_VAL rdx, rcx           ; level payload+tag

    ; Save name, fromlist (payload+tag), and level for later
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
    mov rdi, rax                ; the name as written
    mov rsi, [r12 + PyFrame.globals]
    extern import_resolve_relative
    call import_resolve_relative
    mov rdx, rax                ; the resolved name, owned
    pop rax
    mov [rsp + 24], rdx         ; keep it where the saved name lives
    mov rax, rdx
    mov rdx, 0                  ; it is absolute now
    mov r9d, 1                  ; and the name is ours to release
    jmp .have_name
.absolute:
    xor r9d, r9d
.have_name:
    push r9

    ; import_module(name_str, fromlist, level)
    mov rdi, rax                ; name
    mov rsi, [rsp + 16]        ; fromlist
    ; rdx = level (already set)
    call import_module
    ; rax = module (new reference)

    pop r9
    test r9d, r9d
    jz .no_resolved_name
    push rax
    mov rdi, [rsp + 32]         ; the resolved name we built
    call obj_decref
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
    lea rdi, [rel exc_ImportError_type]
    CSTRING rsi, "import failed"
    call raise_exception

.propagate_import_exc:
    mov [rel eval_saved_r13], r13
    jmp eval_exception_unwind
END_FUNC op_import_name

; ============================================================================
; op_import_from - Opcode 109: IMPORT_FROM
;
; Stack in: [module] (TOS = module, NOT popped)
; Stack out: [module, attr]
;
; ecx = arg = index into co_names for attribute name
;
; If attr not found on module, tries importing <pkg_name>.<attr_name>
; as a submodule (CPython submodule fallback).
; ============================================================================
extern dict_get
extern str_from_cstr_heap
extern str_concat
extern import_find_and_load

IF_ATTR  equ 8
IF2_MOD  equ 16
IF2_FRAME equ 16

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

    ; Look up "__name__" in module dict (heap — dict key, DECREFed)
    lea rdi, [rel if_dunder_name]
    call str_from_cstr_heap
    push rax                    ; save __name__ str key
    mov rdi, [rbp - IF2_MOD]
    mov rdi, [rdi + PyModuleObject.mod_dict]
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    mov rcx, rax                ; rcx = pkg_name str (or NULL)
    pop rdi                     ; __name__ str key
    push rcx                    ; save pkg_name
    call obj_decref             ; DECREF __name__ key
    pop rcx                     ; restore pkg_name
    test rcx, rcx
    jz .if_error

    ; Concat: pkg_name + "." + attr_name
    ; First: pkg_name + "."
    push rcx                    ; save pkg_name
    lea rdi, [rel if_dot_str]
    call str_from_cstr_heap
    pop rdi                     ; rdi = pkg_name
    mov rsi, rax                ; rsi = "."
    push rsi                    ; save dot str for decref
    mov ecx, TAG_PTR            ; right_tag (heap str guaranteed)
    call str_concat             ; rax = pkg_name + "."
    pop rdi                     ; dot str
    push rax                    ; save intermediate
    call obj_decref             ; DECREF "."
    pop rdi                     ; rdi = "pkg."
    mov rsi, [rbp - IF_ATTR]    ; rsi = attr_name
    push rdi                    ; save "pkg." for decref
    mov ecx, TAG_PTR            ; right_tag (heap str guaranteed)
    call str_concat             ; rax = "pkg.attr"
    pop rdi                     ; "pkg."
    push rax                    ; save full name
    call obj_decref             ; DECREF "pkg."

    ; Try import_find_and_load with full dotted name
    pop rdi                     ; rdi = "pkg.attr" str
    push rdi                    ; save for decref
    call import_find_and_load
    mov rcx, rax                ; rcx = submodule (or NULL)
    pop rdi                     ; full name str
    push rcx                    ; save submodule
    call obj_decref             ; DECREF full name
    pop rax                     ; restore submodule

    test rax, rax
    jz .if_error

    ; Got the submodule — push it
    VPUSH_PTR rax
    leave
    DISPATCH

.if_error:
    ; A submodule whose body raised leaves its exception pending; reporting
    ; "cannot import name" over it would hide the real cause.
    cmp qword [rel current_exception], 0
    jne .propagate_from_exc
    lea rdi, [rel exc_ImportError_type]
    CSTRING rsi, "cannot import name"
    call raise_exception

.propagate_from_exc:
    mov [rel eval_saved_r13], r13
    leave
    jmp eval_exception_unwind
END_FUNC op_import_from

section .rodata
if_dunder_name: db "__name__", 0
if_dot_str: db ".", 0
section .text
