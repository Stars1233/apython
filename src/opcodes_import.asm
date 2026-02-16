; opcodes_import.asm - IMPORT_NAME and IMPORT_FROM opcode handlers

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"

extern eval_dispatch
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
    ; Get module name from co_names[ecx]
    mov rax, [r15 + rcx * 8]   ; name_str from co_names

    ; Pop fromlist (TOS)
    VPOP rsi                    ; fromlist

    ; Pop level (TOS1)
    VPOP rdx                    ; level (SmallInt)

    ; Save name and fromlist for later
    push rax                    ; name
    push rsi                    ; fromlist
    push rdx                    ; level

    ; Decode level from SmallInt
    test rdx, rdx
    js .decode_smallint
    ; Not a SmallInt — assume 0
    xor edx, edx
    jmp .do_import

.decode_smallint:
    shl rdx, 1
    sar rdx, 1

.do_import:
    ; import_module(name_str, fromlist, level)
    mov rdi, rax                ; name
    mov rsi, [rsp + 8]         ; fromlist
    ; rdx = level (already set)
    call import_module
    ; rax = module (new reference)

    ; DECREF level (SmallInt, no-op typically)
    pop rdi                     ; level
    DECREF rdi

    ; DECREF fromlist
    pop rdi                     ; fromlist
    DECREF rdi

    add rsp, 8                  ; pop saved name (borrowed ref, no DECREF)

    ; Push module onto value stack
    test rax, rax
    jz .import_failed
    VPUSH rax
    DISPATCH

.import_failed:
    ; Should not reach here — import_module raises on failure
    lea rdi, [rel exc_ImportError_type]
    CSTRING rsi, "import failed"
    call raise_exception
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
extern str_from_cstr
extern str_concat
extern import_find_and_load

IF_ATTR  equ 8
IF2_MOD  equ 16
IF2_FRAME equ 16

DEF_FUNC op_import_from, IF2_FRAME
    ; Get attribute name from co_names[ecx]
    mov rsi, [r15 + rcx * 8]   ; attr name_str
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
    test rax, rax
    jnz .if_got_attr

    ; tp_getattr returned NULL — try dict_get directly
    mov rdi, [rbp - IF2_MOD]
    mov rdi, [rdi + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .if_try_submodule
    mov rsi, [rbp - IF_ATTR]
    call dict_get
    test rax, rax
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
    test rax, rax
    jnz .if_found_in_dict
    jmp .if_try_submodule

.if_found_in_dict:
    inc qword [rax + PyObject.ob_refcnt]
    VPUSH rax
    leave
    DISPATCH

.if_got_attr:
    VPUSH rax
    leave
    DISPATCH

.if_try_submodule:
    ; Submodule fallback: construct "<pkg_name>.<attr_name>" and try importing
    ; Get module's __name__ from its dict
    mov rdi, [rbp - IF2_MOD]
    mov rdi, [rdi + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .if_error

    ; Look up "__name__" in module dict
    lea rdi, [rel if_dunder_name]
    call str_from_cstr
    push rax                    ; save __name__ str key
    mov rdi, [rbp - IF2_MOD]
    mov rdi, [rdi + PyModuleObject.mod_dict]
    mov rsi, rax
    call dict_get
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
    call str_from_cstr
    pop rdi                     ; rdi = pkg_name
    mov rsi, rax                ; rsi = "."
    push rsi                    ; save dot str for decref
    call str_concat             ; rax = pkg_name + "."
    pop rdi                     ; dot str
    push rax                    ; save intermediate
    call obj_decref             ; DECREF "."
    pop rdi                     ; rdi = "pkg."
    mov rsi, [rbp - IF_ATTR]    ; rsi = attr_name
    push rdi                    ; save "pkg." for decref
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
    VPUSH rax
    leave
    DISPATCH

.if_error:
    lea rdi, [rel exc_ImportError_type]
    CSTRING rsi, "cannot import name"
    call raise_exception
END_FUNC op_import_from

section .rodata
if_dunder_name: db "__name__", 0
if_dot_str: db ".", 0
section .text
