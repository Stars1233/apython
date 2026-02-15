; func_obj.asm - Function object type for apython
; Implements PyFuncObject: creation, calling, deallocation, and type descriptor

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern str_from_cstr
extern eval_frame
extern frame_new
extern frame_free
extern tuple_new
extern obj_incref

; CO_FLAGS
CO_VARARGS equ 0x04

; ---------------------------------------------------------------------------
; func_new(PyCodeObject *code, PyObject *globals) -> PyFuncObject*
; Allocate and initialize a new function object.
; rdi = code object, rsi = globals dict
; ---------------------------------------------------------------------------
DEF_FUNC func_new
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
    leave
    ret
END_FUNC func_new

; ---------------------------------------------------------------------------
; func_call(PyFuncObject *callable, PyObject **args_ptr, int nargs) -> PyObject*
; tp_call implementation for function objects.
; rdi = function object, rsi = pointer to args array, edx = nargs
;
; r12 still holds the CALLER's frame pointer (callee-saved, set by eval loop,
; preserved through op_call).
; ---------------------------------------------------------------------------
DEF_FUNC func_call
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

    ; Store function object in frame for COPY_FREE_VARS
    mov [r12 + PyFrame.func_obj], rbx

    ; Get co_argcount and co_flags from code object
    mov rdi, [rbx + PyFuncObject.func_code]
    mov eax, [rdi + PyCodeObject.co_argcount]   ; eax = co_argcount
    mov ecx, [rdi + PyCodeObject.co_flags]      ; ecx = co_flags

    ; Bind positional args: localsplus[0..min(nargs, co_argcount)-1]
    push rax                ; save co_argcount
    push rcx                ; save co_flags

    ; Determine how many regular args to bind
    cmp r15d, eax
    cmovb eax, r15d         ; eax = min(nargs, co_argcount)
    xor ecx, ecx
    test eax, eax
    jz .regular_args_done

.bind_args:
    mov rdx, [r14 + rcx*8]             ; rdx = args[i]
    mov [r12 + PyFrame.localsplus + rcx*8], rdx
    INCREF rdx
    inc ecx
    cmp ecx, eax
    jb .bind_args

.regular_args_done:
    pop rcx                 ; rcx = co_flags
    pop rax                 ; rax = co_argcount

    ; Check CO_VARARGS
    test ecx, CO_VARARGS
    jz .args_done

    ; Pack excess args into a tuple at localsplus[co_argcount]
    ; Number of excess args = max(0, nargs - co_argcount)
    mov ecx, r15d           ; nargs
    sub ecx, eax            ; excess = nargs - co_argcount
    jle .empty_varargs

    ; Create tuple of excess args
    push rax                ; save co_argcount (= localsplus index for *args)
    movsx rdi, ecx          ; tuple size = excess
    push rdi                ; save excess count
    call tuple_new          ; rax = new tuple
    pop rcx                 ; rcx = excess count
    pop rdx                 ; rdx = co_argcount

    ; Fill tuple: tuple.ob_item[i] = args[co_argcount + i], INCREF each
    xor esi, esi
.fill_varargs:
    cmp esi, ecx
    jge .store_varargs_tuple
    lea edi, [edx + esi]    ; index into args = co_argcount + i
    mov r8, [r14 + rdi*8]   ; r8 = args[co_argcount + i]
    mov [rax + PyTupleObject.ob_item + rsi*8], r8
    push rax
    push rcx
    push rdx
    push rsi
    mov rdi, r8
    call obj_incref
    pop rsi
    pop rdx
    pop rcx
    pop rax
    inc esi
    jmp .fill_varargs

.store_varargs_tuple:
    ; Store tuple at localsplus[co_argcount]
    mov [r12 + PyFrame.localsplus + rdx*8], rax
    jmp .args_done

.empty_varargs:
    ; No excess args - create empty tuple
    push rax                ; save co_argcount
    xor edi, edi            ; size = 0
    call tuple_new
    pop rdx                 ; rdx = co_argcount
    mov [r12 + PyFrame.localsplus + rdx*8], rax

.args_done:
    ; Call eval_frame(frame)
    mov rdi, r12
    call eval_frame
    mov r13, rax            ; r13 = return value

    ; Free the frame (unless generator owns it: instr_ptr != 0)
    cmp qword [r12 + PyFrame.instr_ptr], 0
    jne .skip_frame_free
    mov rdi, r12
    call frame_free
.skip_frame_free:

    ; Return result
    mov rax, r13

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC func_call

; ---------------------------------------------------------------------------
; func_dealloc(PyFuncObject *self)
; Releases references to internal objects and frees the function.
; rdi = function object
; ---------------------------------------------------------------------------
DEF_FUNC func_dealloc
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
    leave
    ret
END_FUNC func_dealloc

; ---------------------------------------------------------------------------
; func_repr(PyFuncObject *self) -> PyStrObject*
; Returns the string "<function>"
; rdi = function object
; ---------------------------------------------------------------------------
DEF_FUNC_BARE func_repr
    lea rdi, [rel func_repr_str]
    jmp str_from_cstr
END_FUNC func_repr

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
