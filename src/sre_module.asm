; sre_module.asm - _sre built-in module for apython
; Provides _sre.compile() and SRE constants needed by the re module.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"
%include "sre.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_incref
extern obj_dealloc
extern str_from_cstr_heap
extern int_from_i64
extern dict_new
extern dict_set
extern dict_get
extern module_new
extern builtin_func_new
extern none_singleton
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
extern list_type
extern tuple_type

; From sre_pattern.asm
extern sre_pattern_type

; ============================================================================
; sre_compile_func(PyObject **args, int64_t nargs) -> (rax, edx)
; _sre.compile(pattern, flags, code, groups, groupindex, indexgroup)
;
; args is a fat value array (16-byte stride):
;   args[0] = pattern (str)
;   args[1] = flags (int)
;   args[2] = code (list of ints)
;   args[3] = groups (int)
;   args[4] = groupindex (dict)
;   args[5] = indexgroup (tuple)
; ============================================================================
SC_ARGS     equ 8
SC_NARGS    equ 16
SC_PATTERN  equ 24
SC_FLAGS    equ 32
SC_CODE     equ 40
SC_GROUPS   equ 48
SC_GINDEX   equ 56
SC_IGROUP   equ 64
SC_CODEBUF  equ 72
SC_CODELEN  equ 80
SC_FRAME    equ 80

DEF_FUNC sre_compile_func, SC_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - SC_ARGS], rdi
    mov [rbp - SC_NARGS], rsi

    ; Validate nargs >= 6
    cmp rsi, 6
    jb .compile_error_args

    ; Extract args (fat values: 16-byte stride)
    ; args[0] = pattern string
    mov rax, [rdi]              ; payload
    mov [rbp - SC_PATTERN], rax

    ; args[1] = flags (SmallInt)
    mov rax, [rdi + 16]        ; payload
    mov [rbp - SC_FLAGS], rax

    ; args[2] = code (list of ints)
    mov rax, [rdi + 32]        ; payload
    mov [rbp - SC_CODE], rax

    ; args[3] = groups (SmallInt)
    mov rax, [rdi + 48]        ; payload
    mov [rbp - SC_GROUPS], rax

    ; args[4] = groupindex (dict or None)
    mov rax, [rdi + 64]        ; payload
    mov rcx, [rdi + 72]        ; tag
    cmp ecx, TAG_NONE
    jne .have_groupindex
    xor eax, eax               ; NULL for None
.have_groupindex:
    mov [rbp - SC_GINDEX], rax

    ; args[5] = indexgroup (tuple or None)
    mov rax, [rdi + 80]        ; payload
    mov rcx, [rdi + 88]        ; tag
    cmp ecx, TAG_NONE
    jne .have_indexgroup
    xor eax, eax
.have_indexgroup:
    mov [rbp - SC_IGROUP], rax

    ; Convert code list to u32 array
    ; Get list size
    mov r12, [rbp - SC_CODE]   ; code list
    mov r13, [r12 + PyListObject.ob_size]  ; code length
    mov [rbp - SC_CODELEN], r13

    ; Allocate u32 array
    lea rdi, [r13*4]
    call ap_malloc
    mov r14, rax               ; r14 = u32* code buffer
    mov [rbp - SC_CODEBUF], r14

    ; Iterate code list, extract each SmallInt as u32
    mov r15, [r12 + PyListObject.ob_item]  ; fat value array
    xor ecx, ecx               ; index
.code_loop:
    cmp rcx, r13
    jge .code_done
    ; Fat value at r15 + index*16
    mov rax, rcx
    shl rax, 4
    mov rax, [r15 + rax]       ; payload (SmallInt value)
    mov [r14 + rcx*4], eax    ; store as u32
    inc rcx
    jmp .code_loop
.code_done:

    ; Allocate SRE_PatternObject
    mov edi, SRE_PatternObject_size
    call ap_malloc
    mov rbx, rax               ; rbx = pattern object

    ; Initialize
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel sre_pattern_type]
    mov [rbx + PyObject.ob_type], rax

    ; Set fields
    mov rax, [rbp - SC_PATTERN]
    mov [rbx + SRE_PatternObject.pattern], rax
    inc qword [rax + PyObject.ob_refcnt]  ; INCREF pattern string

    mov rax, [rbp - SC_FLAGS]
    mov [rbx + SRE_PatternObject.flags], eax

    mov rax, [rbp - SC_GROUPS]
    mov [rbx + SRE_PatternObject.groups], eax

    mov rax, [rbp - SC_GINDEX]
    mov [rbx + SRE_PatternObject.groupindex], rax
    test rax, rax
    jz .no_gi_incref
    inc qword [rax + PyObject.ob_refcnt]
.no_gi_incref:

    mov rax, [rbp - SC_IGROUP]
    mov [rbx + SRE_PatternObject.indexgroup], rax
    test rax, rax
    jz .no_ig_incref
    inc qword [rax + PyObject.ob_refcnt]
.no_ig_incref:

    mov [rbx + SRE_PatternObject.code], r14
    mov [rbx + SRE_PatternObject.code_len], r13

    ; Return pattern object
    mov rax, rbx
    mov edx, TAG_PTR

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.compile_error_args:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "_sre.compile() requires 6 arguments"
    call raise_exception
END_FUNC sre_compile_func

; ============================================================================
; sre_ascii_iscased_func(PyObject **args, int64_t nargs) -> (rax, edx)
; ============================================================================
DEF_FUNC sre_ascii_iscased_func
    cmp rsi, 1
    jne .aic_error
    mov rax, [rdi]              ; char value (SmallInt payload)
    ; A-Z or a-z?
    cmp eax, 'A'
    jb .aic_false
    cmp eax, 'Z'
    jbe .aic_true
    cmp eax, 'a'
    jb .aic_false
    cmp eax, 'z'
    jbe .aic_true
.aic_false:
    xor eax, eax
    mov edx, TAG_BOOL
    leave
    ret
.aic_true:
    mov eax, 1
    mov edx, TAG_BOOL
    leave
    ret
.aic_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "ascii_iscased() takes 1 argument"
    call raise_exception
END_FUNC sre_ascii_iscased_func

; ============================================================================
; sre_ascii_tolower_func(PyObject **args, int64_t nargs) -> (rax, edx)
; ============================================================================
DEF_FUNC sre_ascii_tolower_func
    cmp rsi, 1
    jne .atl_error
    mov eax, [rdi]              ; char value
    cmp eax, 'A'
    jb .atl_done
    cmp eax, 'Z'
    ja .atl_done
    add eax, 32
.atl_done:
    RET_TAG_SMALLINT
    leave
    ret
.atl_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "ascii_tolower() takes 1 argument"
    call raise_exception
END_FUNC sre_ascii_tolower_func

; ============================================================================
; sre_unicode_iscased_func(PyObject **args, int64_t nargs) -> (rax, edx)
; ============================================================================
DEF_FUNC sre_unicode_iscased_func
    cmp rsi, 1
    jne .uic_error
    mov eax, [rdi]
    ; Check if char has a case mapping
    cmp eax, 'A'
    jb .uic_check_ext
    cmp eax, 'Z'
    jbe .uic_true
    cmp eax, 'a'
    jb .uic_check_ext
    cmp eax, 'z'
    jbe .uic_true
.uic_check_ext:
    ; Extended Latin
    cmp eax, 0xC0
    jb .uic_false
    cmp eax, 0xFF
    jbe .uic_true
    ; Basic check for common Unicode ranges
    cmp eax, 0x100
    jb .uic_false
    ; Simplified: assume cased if in Latin Extended or other letter ranges
    mov edx, 1
    mov eax, 1
    mov edx, TAG_BOOL
    leave
    ret
.uic_false:
    xor eax, eax
    mov edx, TAG_BOOL
    leave
    ret
.uic_true:
    mov eax, 1
    mov edx, TAG_BOOL
    leave
    ret
.uic_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unicode_iscased() takes 1 argument"
    call raise_exception
END_FUNC sre_unicode_iscased_func

; ============================================================================
; sre_unicode_tolower_func(PyObject **args, int64_t nargs) -> (rax, edx)
; ============================================================================
DEF_FUNC sre_unicode_tolower_func
    cmp rsi, 1
    jne .utl_error
    mov eax, [rdi]
    cmp eax, 'A'
    jb .utl_done
    cmp eax, 'Z'
    jbe .utl_lower
    cmp eax, 0xC0
    jb .utl_done
    cmp eax, 0xD6
    jbe .utl_lower
    cmp eax, 0xD8
    jb .utl_done
    cmp eax, 0xDE
    jbe .utl_lower
    jmp .utl_done
.utl_lower:
    add eax, 32
.utl_done:
    RET_TAG_SMALLINT
    leave
    ret
.utl_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unicode_tolower() takes 1 argument"
    call raise_exception
END_FUNC sre_unicode_tolower_func

; ============================================================================
; sre_getlower_func(PyObject **args, int64_t nargs) -> (rax, edx)
; _sre.getlower(char, flags) — same as tolower with flag check
; ============================================================================
DEF_FUNC sre_getlower_func
    cmp rsi, 2
    jne .gl_error
    mov eax, [rdi]             ; char
    mov ecx, [rdi + 16]       ; flags
    ; If ASCII flag, use ASCII tolower
    test ecx, SRE_FLAG_ASCII
    jnz .gl_ascii
    ; Default: unicode tolower
    cmp eax, 'A'
    jb .gl_done
    cmp eax, 'Z'
    ja .gl_check_ext
    add eax, 32
    jmp .gl_done
.gl_check_ext:
    cmp eax, 0xC0
    jb .gl_done
    cmp eax, 0xD6
    jbe .gl_do_lower
    cmp eax, 0xD8
    jb .gl_done
    cmp eax, 0xDE
    ja .gl_done
.gl_do_lower:
    add eax, 32
    jmp .gl_done
.gl_ascii:
    cmp eax, 'A'
    jb .gl_done
    cmp eax, 'Z'
    ja .gl_done
    add eax, 32
.gl_done:
    RET_TAG_SMALLINT
    leave
    ret
.gl_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "getlower() takes 2 arguments"
    call raise_exception
END_FUNC sre_getlower_func

; ============================================================================
; sre_getcodesize_func(PyObject **args, int64_t nargs) -> (rax, edx)
; _sre.getcodesize() -> 4
; ============================================================================
DEF_FUNC sre_getcodesize_func
    mov eax, SRE_CODESIZE
    RET_TAG_SMALLINT
    leave
    ret
END_FUNC sre_getcodesize_func

; ============================================================================
; Helper macro for adding a function to module dict
; ============================================================================
; (inline in module_create below)

; ============================================================================
; sre_module_create() -> PyObject*
; Creates and returns the _sre module with all functions and constants.
; ============================================================================
SMC_DICT    equ 8
SMC_FRAME   equ 8

DEF_FUNC sre_module_create, SMC_FRAME
    push rbx
    push r12
    push r13

    ; Create module dict
    call dict_new
    mov r12, rax               ; r12 = module dict

    ; --- Add compile function ---
    lea rdi, [rel sre_compile_func]
    lea rsi, [rel sm_compile_name]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_compile_name]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- Add ascii_iscased ---
    lea rdi, [rel sre_ascii_iscased_func]
    lea rsi, [rel sm_ascii_iscased]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_ascii_iscased]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- Add ascii_tolower ---
    lea rdi, [rel sre_ascii_tolower_func]
    lea rsi, [rel sm_ascii_tolower]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_ascii_tolower]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- Add unicode_iscased ---
    lea rdi, [rel sre_unicode_iscased_func]
    lea rsi, [rel sm_unicode_iscased]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_unicode_iscased]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- Add unicode_tolower ---
    lea rdi, [rel sre_unicode_tolower_func]
    lea rsi, [rel sm_unicode_tolower]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_unicode_tolower]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- Add getlower ---
    lea rdi, [rel sre_getlower_func]
    lea rsi, [rel sm_getlower]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_getlower]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- Add getcodesize ---
    lea rdi, [rel sre_getcodesize_func]
    lea rsi, [rel sm_getcodesize]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_getcodesize]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- Add constants ---
    ; MAGIC
    lea rdi, [rel sm_MAGIC]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, SRE_MAGIC
    mov ecx, TAG_SMALLINT
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; CODESIZE
    lea rdi, [rel sm_CODESIZE]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, SRE_CODESIZE
    mov ecx, TAG_SMALLINT
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; MAXREPEAT
    lea rdi, [rel sm_MAXREPEAT]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, SRE_MAXREPEAT
    mov ecx, TAG_SMALLINT
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; MAXGROUPS
    lea rdi, [rel sm_MAXGROUPS]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, SRE_MAXGROUPS
    mov ecx, TAG_SMALLINT
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; copyright
    lea rdi, [rel sm_copyright]
    call str_from_cstr_heap
    push rax                    ; save key
    lea rdi, [rel sm_copyright_val]
    call str_from_cstr_heap
    mov rdx, rax                ; value
    mov rdi, r12                ; dict
    pop rsi                     ; key
    push rdx                    ; save value for DECREF
    push rsi                    ; save key for DECREF
    mov ecx, TAG_PTR            ; value tag
    mov r8d, TAG_PTR            ; key tag
    call dict_set
    pop rdi
    call obj_decref             ; DECREF key
    pop rdi
    call obj_decref             ; DECREF value

    ; Create module object
    lea rdi, [rel sm_sre_name]
    call str_from_cstr_heap
    mov rdi, rax
    mov rsi, r12
    call module_new
    ; rax = _sre module

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_module_create

; ============================================================================
; Data
; ============================================================================
section .rodata

sm_sre_name:        db "_sre", 0
sm_compile_name:    db "compile", 0
sm_ascii_iscased:   db "ascii_iscased", 0
sm_ascii_tolower:   db "ascii_tolower", 0
sm_unicode_iscased: db "unicode_iscased", 0
sm_unicode_tolower: db "unicode_tolower", 0
sm_getlower:        db "getlower", 0
sm_getcodesize:     db "getcodesize", 0

sm_MAGIC:           db "MAGIC", 0
sm_CODESIZE:        db "CODESIZE", 0
sm_MAXREPEAT:       db "MAXREPEAT", 0
sm_MAXGROUPS:       db "MAXGROUPS", 0
sm_copyright:       db "copyright", 0
sm_copyright_val:   db "_sre for apython", 0
