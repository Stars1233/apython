; generator.asm - Generator object implementation
; Phase 10: suspendable frames via RETURN_GENERATOR / YIELD_VALUE

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_incref
extern eval_frame
extern frame_free
extern none_singleton
extern str_from_cstr
extern obj_dealloc
extern type_type

;; ============================================================================
;; gen_new(PyFrame *frame) -> PyGenObject*
;; Create a new generator object that owns the given frame.
;; rdi = frame (ownership transfers to generator)
;; ============================================================================
DEF_FUNC gen_new
    push rbx
    push r12

    mov rbx, rdi               ; rbx = frame

    mov edi, PyGenObject_size
    call ap_malloc
    mov r12, rax               ; r12 = gen object

    mov qword [r12 + PyObject.ob_refcnt], 1
    lea rcx, [rel gen_type]
    mov [r12 + PyObject.ob_type], rcx
    mov [r12 + PyGenObject.gi_frame], rbx
    mov qword [r12 + PyGenObject.gi_running], 0

    ; Copy code from frame and INCREF it
    mov rdx, [rbx + PyFrame.code]
    mov [r12 + PyGenObject.gi_code], rdx
    mov rdi, rdx
    call obj_incref

    ; gi_name = NULL (not critical)
    mov qword [r12 + PyGenObject.gi_name], 0

    mov rax, r12               ; return gen object
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_new

;; ============================================================================
;; gen_iternext(PyGenObject *self) -> PyObject* or NULL
;; Resume the generator. Push None as sent value, call eval_frame.
;; Returns yielded value, or NULL if generator is exhausted.
;; rdi = generator
;; ============================================================================
DEF_FUNC gen_iternext
    push rbx
    push r12

    mov rbx, rdi               ; rbx = generator

    ; Check if generator is exhausted
    mov r12, [rbx + PyGenObject.gi_frame]
    test r12, r12
    jz .exhausted

    ; Check if already running (recursive call)
    cmp qword [rbx + PyGenObject.gi_running], 1
    je .running_error

    ; Mark as running
    mov qword [rbx + PyGenObject.gi_running], 1

    ; Push None as the "sent" value onto the frame's value stack
    ; The value stack top is at frame->stack_ptr
    mov rax, [r12 + PyFrame.stack_ptr]
    lea rcx, [rel none_singleton]
    mov [rax], rcx
    add rax, 8
    mov [r12 + PyFrame.stack_ptr], rax

    ; INCREF None (it's now on the value stack)
    mov rdi, rcx
    call obj_incref

    ; Resume execution
    mov rdi, r12
    call eval_frame
    ; rax = yielded/returned value

    mov r12, rax               ; save return value

    ; Mark as not running
    mov qword [rbx + PyGenObject.gi_running], 0

    ; Check if generator returned (vs yielded)
    ; If frame->instr_ptr == 0, generator returned (exhausted)
    mov rdi, [rbx + PyGenObject.gi_frame]
    cmp qword [rdi + PyFrame.instr_ptr], 0
    jne .yielded

    ; Generator is exhausted: free frame, set gi_frame = NULL
    call frame_free
    mov qword [rbx + PyGenObject.gi_frame], 0

    ; DECREF the return value (we don't use it, return NULL for StopIteration)
    mov rdi, r12
    test rdi, rdi
    jz .return_null
    DECREF_REG rdi

.return_null:
    xor eax, eax              ; return NULL (StopIteration)
    pop r12
    pop rbx
    leave
    ret

.yielded:
    ; Return the yielded value
    mov rax, r12
    pop r12
    pop rbx
    leave
    ret

.exhausted:
    xor eax, eax              ; return NULL
    pop r12
    pop rbx
    leave
    ret

.running_error:
    xor eax, eax              ; return NULL
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_iternext

;; ============================================================================
;; gen_dealloc(PyObject *self)
;; Free generator: free frame if still held, DECREF code.
;; ============================================================================
DEF_FUNC gen_dealloc
    push rbx

    mov rbx, rdi

    ; Free frame if still held
    mov rdi, [rbx + PyGenObject.gi_frame]
    test rdi, rdi
    jz .no_frame
    call frame_free
.no_frame:

    ; DECREF code object
    mov rdi, [rbx + PyGenObject.gi_code]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC gen_dealloc

;; ============================================================================
;; gen_iter_self(PyObject *self) -> self with INCREF
;; tp_iter for generator: return self
;; ============================================================================
gen_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC gen_iter_self

;; ============================================================================
;; gen_repr(PyObject *self) -> PyStrObject*
;; ============================================================================
gen_repr:
    lea rdi, [rel gen_repr_str]
    jmp str_from_cstr
END_FUNC gen_repr

;; ============================================================================
;; Data section
;; ============================================================================
section .data

gen_name_str:  db "generator", 0
gen_repr_str:  db "<generator>", 0

align 8
global gen_type
gen_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq gen_name_str             ; tp_name
    dq PyGenObject_size         ; tp_basicsize
    dq gen_dealloc              ; tp_dealloc
    dq gen_repr                 ; tp_repr
    dq gen_repr                 ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq gen_iter_self            ; tp_iter (return self)
    dq gen_iternext             ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
