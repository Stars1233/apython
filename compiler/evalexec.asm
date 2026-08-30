; evalexec.asm - The eval() and compile() builtins
;
; These replace the stub at src/builtins_extra.asm, which parsed a single
; integer literal and raised ValueError for anything else.
;
; The globals/locals rules are CPython's, and they matter more than they look:
; when globals is given but locals is not, locals becomes globals, which is
; what makes eval(expr, ns) evaluate names against ns.  __builtins__ is
; injected into globals only when absent, so a caller that supplies its own --
; collections.namedtuple passes {} deliberately -- keeps it.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "value.inc"
%include "builtins.inc"
%include "frame.inc"
%include "compiler.inc"

extern builtins_dict_global
extern compile_source
extern current_exception
extern dict_get
extern dict_set
extern dict_type
extern eval_frame
extern eval_saved_r12
extern frame_free
extern frame_new
extern obj_decref
extern none_singleton
extern obj_incref
extern raise_exception
extern str_from_cstr_heap
extern str_type
extern code_type

extern exc_TypeError_type
extern exc_ValueError_type

; --- Named frame-layout constants ---
EV_ARGS  equ 8
EV_NARGS equ 16
EV_CODE  equ 24
EV_GLOB  equ 32
EV_LOC   equ 40
EV_BLT   equ 48
EV_FRAME equ 56          ; + 3 pushes = 80

section .text

;; ============================================================================
;; ev_resolve_ns(Comp-less) - shared globals/locals resolution
;;   rdi = args, rsi = nargs
;;   -> rax = globals, rdx = locals, or rax = 0 on a type error already raised
;;
;; Defaults follow CPython exactly:
;;   neither given  -> the calling frame's globals and locals
;;   globals only   -> locals = globals
;;   both given     -> used as they are
;; ============================================================================
NS_ARGS  equ 8
NS_NARGS equ 16
NS_GLOB  equ 24
NS_LOC   equ 32
NS_FRAME equ 40          ; + 1 push = 48
DEF_FUNC ev_resolve_ns, NS_FRAME
    push rbx
    mov [rbp - NS_ARGS], rdi
    mov [rbp - NS_NARGS], rsi
    mov qword [rbp - NS_GLOB], 0
    mov qword [rbp - NS_LOC], 0

    cmp rsi, 2
    jb .defaults
    mov rax, [rdi + 8]
    lea rcx, [rel none_singleton]       ; a pointer is its own Value
    cmp rax, rcx
    je .check_locals
    mov [rbp - NS_GLOB], rax
.check_locals:
    cmp qword [rbp - NS_NARGS], 3
    jb .defaults
    mov rdi, [rbp - NS_ARGS]
    mov rax, [rdi + 16]
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .defaults
    mov [rbp - NS_LOC], rax

.defaults:
    cmp qword [rbp - NS_GLOB], 0
    jne .have_globals
    ; Nothing supplied: fall back to the calling frame, the same source
    ; globals() and locals() use.
    mov rax, [rel eval_saved_r12]
    test rax, rax
    jz .no_frame
    mov rdx, [rax + PyFrame.globals]
    mov [rbp - NS_GLOB], rdx
    cmp qword [rbp - NS_LOC], 0
    jne .have_globals
    mov rdx, [rax + PyFrame.locals]
    test rdx, rdx
    jnz .use_frame_locals
    mov rdx, [rax + PyFrame.globals]
.use_frame_locals:
    mov [rbp - NS_LOC], rdx
    jmp .have_globals

.have_globals:
    ; With globals given but no locals, the two are the same mapping.  This is
    ; the case eval(expr, ns) takes, and it is why names resolve against ns.
    cmp qword [rbp - NS_LOC], 0
    jne .check_type
    mov rax, [rbp - NS_GLOB]
    mov [rbp - NS_LOC], rax

.check_type:
    mov rax, [rbp - NS_GLOB]
    test rax, rax
    jz .no_frame
    mov rdx, [rax + PyObject.ob_type]
    lea rcx, [rel dict_type]
    cmp rdx, rcx
    jne .globals_not_dict

    mov rax, [rbp - NS_GLOB]
    mov rdx, [rbp - NS_LOC]
    pop rbx
    leave
    ret

.globals_not_dict:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "globals must be a real dict"
    call raise_exception
.no_frame:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "eval must be given globals and locals when called without a frame"
    call raise_exception
END_FUNC ev_resolve_ns

;; ============================================================================
;; ev_inject_builtins(PyObject *globals) -> rax = the builtins mapping to use
;;
;; Only injected when absent -- a caller that provides its own __builtins__ is
;; making a deliberate choice about what the evaluated code can reach.  The
;; value that ends up there is also what the frame gets, so honouring it costs
;; nothing and is what makes a restricted namespace actually restricted.
;; ============================================================================
DEF_FUNC ev_inject_builtins, 16 ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi
    lea rdi, [rel ev_builtins_name]
    call str_from_cstr_heap
    mov r12, rax

    mov rdi, rbx
    mov rsi, r12
    call dict_get
    test rax, rax
    jnz .present

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rel builtins_dict_global]
    call dict_set
    mov rax, [rel builtins_dict_global]
.present:
    mov rbx, rax
    mov rdi, r12
    call obj_decref
    mov rax, rbx

    ; The frame's builtins slot has to be a dict; anything else falls back to
    ; the interpreter's own.
    test rax, rax
    jz .fallback
    mov rdx, [rax + PyObject.ob_type]
    lea rcx, [rel dict_type]
    cmp rdx, rcx
    je .done
.fallback:
    mov rax, [rel builtins_dict_global]
.done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC ev_inject_builtins

;; ============================================================================
;; ev_run_code(PyCodeObject *code, globals, locals, builtins) -> Value
;; The module-body pattern from src/import.asm: build a frame, run it, free it.
;; ============================================================================
RC_CODE  equ 8
RC_FRAME_P equ 16
RC_RET   equ 24
RC_FRAME equ 24          ; + 1 push = 32
DEF_FUNC ev_run_code, RC_FRAME
    push rbx
    mov [rbp - RC_CODE], rdi
    call frame_new                      ; (code, globals, builtins, locals)
    mov [rbp - RC_FRAME_P], rax
    mov rbx, rax

    mov rdi, rbx
    call eval_frame
    mov [rbp - RC_RET], rax

    mov rdi, rbx
    call frame_free

    mov rax, [rbp - RC_RET]
    pop rbx
    leave
    ret
END_FUNC ev_run_code

;; ============================================================================
;; builtin_eval_fn(args, nargs) -> Value
;; ============================================================================
DEF_FUNC builtin_eval_fn, EV_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - EV_ARGS], rdi
    mov [rbp - EV_NARGS], rsi

    test rsi, rsi
    jz .bad_nargs
    cmp rsi, 3
    ja .bad_nargs

    call ev_resolve_ns
    mov [rbp - EV_GLOB], rax
    mov [rbp - EV_LOC], rdx

    mov rdi, rax
    call ev_inject_builtins
    mov [rbp - EV_BLT], rax

    mov rdi, [rbp - EV_ARGS]
    mov rbx, [rdi]                      ; the source argument
    test rbx, rbx
    jz .bad_source
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel code_type]
    cmp rax, rcx
    je .have_code
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .bad_source

    ; eval() strips leading spaces and tabs from a source string -- and only
    ; those two, and only at the front.  That is why eval("  1+2") works while
    ; exec("  x=1") is an IndentationError.
    lea r12, [rbx + PyStrObject.data]
    mov r13, [rbx + PyStrObject.ob_size]
.strip:
    test r13, r13
    jz .stripped
    movzx eax, byte [r12]
    cmp al, ' '
    je .strip_one
    cmp al, 9
    jne .stripped
.strip_one:
    inc r12
    dec r13
    jmp .strip
.stripped:

    lea rdi, [rel ev_string_name]
    call str_from_cstr_heap
    mov [rbp - EV_CODE], rax            ; parked: the filename, "<string>"

    mov rdi, r12
    mov rsi, r13
    mov rdx, [rbp - EV_CODE]
    mov ecx, CMODE_EVAL
    call compile_source
    mov r12, rax                        ; the code object, or 0
    mov rdi, [rbp - EV_CODE]
    call obj_decref                     ; the code object took its own reference
    test r12, r12
    jz .propagate
    mov [rbp - EV_CODE], r12
    jmp .run

.have_code:
    INCREF rbx
    mov [rbp - EV_CODE], rbx

.run:
    mov rdi, [rbp - EV_CODE]
    mov rsi, [rbp - EV_GLOB]
    mov rdx, [rbp - EV_BLT]
    mov rcx, [rbp - EV_LOC]
    call ev_run_code
    mov rbx, rax
    mov rdi, [rbp - EV_CODE]
    call obj_decref
    mov rax, rbx
    pop r13
    pop r12
    pop rbx
    leave
    ret

.propagate:
    ; compile_source has already made the exception pending; a NULL Value is
    ; how a builtin reports that, and op_call unwinds from there.
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bad_nargs:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "eval() takes 1 to 3 arguments"
    call raise_exception
.bad_source:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "eval() arg 1 must be a string or code object"
    call raise_exception
END_FUNC builtin_eval_fn

;; ============================================================================
;; builtin_compile_fn(args, nargs) -> Value
;;   compile(source, filename, mode[, flags[, dont_inherit[, optimize]]])
;;
;; flags, dont_inherit and optimize are accepted and ignored: there are no
;; __future__ features to inherit here and no optimization levels to select.
;; Only "eval" mode is implemented so far; "exec" and "single" report it
;; plainly rather than producing something that half works.
;; ============================================================================
CO_ARGS  equ 8
CO_NARGS equ 16
CO_MODE  equ 24
CO_FRAME equ 24          ; + 3 pushes = 48
DEF_FUNC builtin_compile_fn, CO_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - CO_ARGS], rdi
    mov [rbp - CO_NARGS], rsi

    cmp rsi, 3
    jb .bad_nargs
    cmp rsi, 6
    ja .bad_nargs

    mov rbx, [rdi]                      ; source
    mov r12, [rdi + 8]                  ; filename
    mov r13, [rdi + 16]                 ; mode

    test rbx, rbx
    jz .bad_source
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .bad_source
    test r12, r12
    jz .bad_filename
    mov rax, [r12 + PyObject.ob_type]
    cmp rax, rcx
    jne .bad_filename
    test r13, r13
    jz .bad_mode
    mov rax, [r13 + PyObject.ob_type]
    cmp rax, rcx
    jne .bad_mode

    ; Only "eval" for now.
    cmp qword [r13 + PyStrObject.ob_size], 4
    jne .unsupported_mode
    mov eax, [r13 + PyStrObject.data]
    cmp eax, 'eval'
    jne .unsupported_mode

    lea rdi, [rbx + PyStrObject.data]
    mov rsi, [rbx + PyStrObject.ob_size]
    mov rdx, r12
    mov ecx, CMODE_EVAL
    call compile_source
    test rax, rax
    jz .propagate
    pop r13
    pop r12
    pop rbx
    leave
    ret

.propagate:
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bad_nargs:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "compile() takes 3 to 6 arguments"
    call raise_exception
.bad_source:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "compile() arg 1 must be a string"
    call raise_exception
.bad_filename:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "compile() arg 2 must be a string"
    call raise_exception
.bad_mode:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "compile() arg 3 must be a string"
    call raise_exception
.unsupported_mode:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "compile() mode must be 'eval' (only eval is implemented so far)"
    call raise_exception
END_FUNC builtin_compile_fn

section .rodata
ev_builtins_name: db "__builtins__", 0
ev_string_name:   db "<string>", 0

ASM_INIT
