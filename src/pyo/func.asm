; func_obj.asm - Function object type for apython
; Implements PyFuncObject: creation, calling, deallocation, and type descriptor

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern str_from_cstr
extern eval_frame
extern frame_new
extern frame_free

; ---------------------------------------------------------------------------
; func_new(PyCodeObject *code, PyObject *globals) -> PyFuncObject*
; Allocate and initialize a new function object.
; rdi = code object, rsi = globals dict
; ---------------------------------------------------------------------------
global func_new
func_new:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14                ; padding for 16-byte stack alignment

    mov rbx, rdi            ; rbx = code
    mov r12, rsi            ; r12 = globals

    ; Allocate PyFuncObject
    mov edi, PyFuncObject_size
    call ap_malloc
    mov r13, rax            ; r13 = new func object

    ; ob_refcnt = 1
    mov qword [r13 + PyObject.ob_refcnt], 1

    ; ob_type = &func_type
    lea rax, [rel func_type]
    mov [r13 + PyObject.ob_type], rax

    ; func_code = code; INCREF code
    mov [r13 + PyFuncObject.func_code], rbx
    INCREF rbx

    ; func_globals = globals; INCREF globals
    mov [r13 + PyFuncObject.func_globals], r12
    INCREF r12

    ; func_name = code->co_name; INCREF name
    mov rax, [rbx + PyCodeObject.co_name]
    mov [r13 + PyFuncObject.func_name], rax
    INCREF rax

    ; func_defaults = NULL
    mov qword [r13 + PyFuncObject.func_defaults], 0

    ; func_closure = NULL
    mov qword [r13 + PyFuncObject.func_closure], 0

    ; func_kwdefaults = NULL
    mov qword [r13 + PyFuncObject.func_kwdefaults], 0

    mov rax, r13            ; return func object
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ---------------------------------------------------------------------------
; func_call(PyFuncObject *callable, PyObject **args_ptr, int nargs) -> PyObject*
; tp_call implementation for function objects.
; rdi = function object, rsi = pointer to args array, edx = nargs
;
; r12 still holds the CALLER's frame pointer (callee-saved, set by eval loop,
; preserved through op_call).
; ---------------------------------------------------------------------------
global func_call
func_call:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8              ; align stack to 16 bytes (5 pushes + push rbp = 48, +8 = 56, 56 mod 16 = 8, need -8 more)

    mov rbx, rdi            ; rbx = function object
    mov r13, r12            ; r13 = caller's frame (r12 on entry from eval loop)
    mov r14, rsi            ; r14 = args_ptr
    mov r15d, edx           ; r15d = nargs

    ; Get builtins from caller's frame
    mov rdx, [r13 + PyFrame.builtins]  ; rdx = builtins

    ; Create new frame: frame_new(code, globals, builtins, locals=NULL)
    mov rdi, [rbx + PyFuncObject.func_code]     ; rdi = code
    mov rsi, [rbx + PyFuncObject.func_globals]   ; rsi = globals
    ; rdx = builtins (already set)
    xor ecx, ecx            ; rcx = locals = NULL
    call frame_new
    mov r12, rax            ; r12 = new frame

    ; Bind positional args to frame->localsplus[0..nargs-1]
    ; INCREF each arg as we store it
    xor ecx, ecx            ; ecx = loop index
    test r15d, r15d
    jz .args_done

.bind_args:
    mov rax, [r14 + rcx*8]             ; rax = args[i]
    mov [r12 + PyFrame.localsplus + rcx*8], rax
    INCREF rax
    inc ecx
    cmp ecx, r15d
    jb .bind_args

.args_done:
    ; Call eval_frame(frame)
    mov rdi, r12
    call eval_frame
    mov r13, rax            ; r13 = return value

    ; Free the frame
    mov rdi, r12
    call frame_free

    ; Return result
    mov rax, r13

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ---------------------------------------------------------------------------
; func_dealloc(PyFuncObject *self)
; Releases references to internal objects and frees the function.
; rdi = function object
; ---------------------------------------------------------------------------
global func_dealloc
func_dealloc:
    push rbp
    mov rbp, rsp
    push rbx
    push r12                ; alignment padding (3 pushes = RSP aligned)

    mov rbx, rdi            ; rbx = func object

    ; DECREF func_code
    mov rdi, [rbx + PyFuncObject.func_code]
    call obj_decref

    ; DECREF func_globals
    mov rdi, [rbx + PyFuncObject.func_globals]
    call obj_decref

    ; DECREF func_name
    mov rdi, [rbx + PyFuncObject.func_name]
    call obj_decref

    ; XDECREF func_defaults (may be NULL)
    mov rdi, [rbx + PyFuncObject.func_defaults]
    test rdi, rdi
    jz .no_defaults
    call obj_decref
.no_defaults:

    ; XDECREF func_closure (may be NULL)
    mov rdi, [rbx + PyFuncObject.func_closure]
    test rdi, rdi
    jz .no_closure
    call obj_decref
.no_closure:

    ; XDECREF func_kwdefaults (may be NULL)
    mov rdi, [rbx + PyFuncObject.func_kwdefaults]
    test rdi, rdi
    jz .no_kwdefaults
    call obj_decref
.no_kwdefaults:

    ; Free the function object itself
    mov rdi, rbx
    call ap_free

    pop r12
    pop rbx
    pop rbp
    ret

; ---------------------------------------------------------------------------
; func_repr(PyFuncObject *self) -> PyStrObject*
; Returns the string "<function>"
; rdi = function object
; ---------------------------------------------------------------------------
global func_repr
func_repr:
    push rbp
    mov rbp, rsp
    lea rdi, [rel func_repr_str]
    call str_from_cstr
    pop rbp
    ret

; ---------------------------------------------------------------------------
; Data section
; ---------------------------------------------------------------------------
section .data

func_name_str:  db "function", 0
func_repr_str:  db "<function>", 0

; func_type - Type object for function objects
align 8
global func_type
func_type:
    dq 1                    ; ob_refcnt (immortal)
    dq 0                    ; ob_type (simplified: no metatype)
    dq func_name_str        ; tp_name
    dq PyFuncObject_size    ; tp_basicsize
    dq func_dealloc         ; tp_dealloc
    dq func_repr            ; tp_repr
    dq func_repr            ; tp_str
    dq 0                    ; tp_hash
    dq func_call            ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq 0                    ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases
