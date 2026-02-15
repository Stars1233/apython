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
extern ap_strcmp
extern raise_exception
extern exc_TypeError_type
extern exc_StopIteration_type
extern method_new
extern builtin_func_new

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
    ; The value stack top is at frame->stack_ptr (16 bytes/slot)
    mov rax, [r12 + PyFrame.stack_ptr]
    lea rcx, [rel none_singleton]
    mov [rax], rcx
    mov qword [rax + 8], TAG_PTR    ; None singleton is a heap pointer
    add rax, 16
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
;; gen_send(PyGenObject *gen, PyObject *value) -> PyObject*
;; Resume generator with a sent value. Returns yielded value or NULL.
;; rdi = generator, rsi = value to send
;; ============================================================================
global gen_send
DEF_FUNC gen_send
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; rbx = generator
    mov r13, rsi               ; r13 = value to send

    ; Check if generator is exhausted
    mov r12, [rbx + PyGenObject.gi_frame]
    test r12, r12
    jz .gs_exhausted

    ; Check if already running
    cmp qword [rbx + PyGenObject.gi_running], 1
    je .gs_error

    ; Mark as running
    mov qword [rbx + PyGenObject.gi_running], 1

    ; Push sent value onto the frame's value stack (16 bytes/slot)
    mov rax, [r12 + PyFrame.stack_ptr]
    mov [rax], r13
    ; Auto-classify tag for sent value
    test r13, r13
    js .gs_tag_smallint
    jz .gs_tag_null
    mov qword [rax + 8], TAG_PTR
    jmp .gs_tag_done
.gs_tag_smallint:
    mov qword [rax + 8], TAG_SMALLINT
    jmp .gs_tag_done
.gs_tag_null:
    mov qword [rax + 8], TAG_NULL
.gs_tag_done:
    add rax, 16
    mov [r12 + PyFrame.stack_ptr], rax

    ; INCREF sent value
    mov rdi, r13
    call obj_incref

    ; Resume execution
    mov rdi, r12
    call eval_frame
    mov r12, rax               ; save return value

    ; Mark as not running
    mov qword [rbx + PyGenObject.gi_running], 0

    ; Check if exhausted
    mov rdi, [rbx + PyGenObject.gi_frame]
    cmp qword [rdi + PyFrame.instr_ptr], 0
    jne .gs_yielded

    ; Exhausted: free frame
    call frame_free
    mov qword [rbx + PyGenObject.gi_frame], 0

    ; DECREF return value, signal StopIteration
    mov rdi, r12
    test rdi, rdi
    jz .gs_stop
    DECREF_REG rdi
.gs_stop:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret

.gs_yielded:
    mov rax, r12
    pop r13
    pop r12
    pop rbx
    leave
    ret

.gs_exhausted:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret

.gs_error:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_send

;; ============================================================================
;; gen_close(PyGenObject *gen) -> None
;; Close the generator by marking it as exhausted.
;; rdi = generator
;; ============================================================================
DEF_FUNC gen_close
    push rbx
    mov rbx, rdi

    ; Free frame if present
    mov rdi, [rbx + PyGenObject.gi_frame]
    test rdi, rdi
    jz .gc_done
    call frame_free
    mov qword [rbx + PyGenObject.gi_frame], 0

.gc_done:
    lea rax, [rel none_singleton]
    mov rdi, rax
    push rax
    call obj_incref
    pop rax

    pop rbx
    leave
    ret
END_FUNC gen_close

;; ============================================================================
;; gen_getattr(PyGenObject *self, PyObject *name) -> PyObject*
;; Attribute lookup for generators: handles send, close, throw
;; ============================================================================
DEF_FUNC gen_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self
    mov r12, rsi               ; name

    lea rdi, [r12 + PyStrObject.data]

    ; Check "send"
    CSTRING rsi, "send"
    call ap_strcmp
    test eax, eax
    jz .gga_send

    ; Check "close"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "close"
    call ap_strcmp
    test eax, eax
    jz .gga_close

    ; Check "throw"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "throw"
    call ap_strcmp
    test eax, eax
    jz .gga_throw

    ; Not found
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.gga_send:
    ; Return raw builtin — LOAD_ATTR handles binding via flag
    call _get_gen_send_builtin
    mov rdi, rax
    call obj_incref
    pop r12
    pop rbx
    leave
    ret

.gga_close:
    call _get_gen_close_builtin
    mov rdi, rax
    call obj_incref
    pop r12
    pop rbx
    leave
    ret

.gga_throw:
    ; For now, throw acts like close
    call _get_gen_close_builtin
    mov rdi, rax
    call obj_incref
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_getattr

;; ============================================================================
;; Builtin implementations for gen.send() and gen.close()
;; These follow the builtin calling convention: (args, nargs)
;; args[0] = self (generator), remaining args follow
;; ============================================================================

;; _gen_send_impl(args, nargs) — gen.send(value)
DEF_FUNC _gen_send_impl
    cmp rsi, 2
    jne .gsi_error

    mov rsi, [rdi + 8]         ; value = args[1]
    mov rdi, [rdi]             ; gen = args[0]
    call gen_send
    test rax, rax
    jnz .gsi_ret

    ; StopIteration
    lea rdi, [rel exc_StopIteration_type]
    CSTRING rsi, "generator exhausted"
    call raise_exception

.gsi_ret:
    leave
    ret

.gsi_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "send() takes exactly one argument"
    call raise_exception
END_FUNC _gen_send_impl

;; _gen_close_impl(args, nargs) — gen.close()
DEF_FUNC _gen_close_impl
    mov rdi, [rdi]             ; gen = args[0]
    call gen_close
    leave
    ret
END_FUNC _gen_close_impl

;; Lazy-init helpers for gen method builtins
DEF_FUNC_LOCAL _get_gen_send_builtin
    mov rax, [rel _gen_send_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _gen_send_impl]
    CSTRING rsi, "send"
    call builtin_func_new
    mov [rel _gen_send_cache], rax
.ret:
    leave
    ret
END_FUNC _get_gen_send_builtin

DEF_FUNC_LOCAL _get_gen_close_builtin
    mov rax, [rel _gen_close_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _gen_close_impl]
    CSTRING rsi, "close"
    call builtin_func_new
    mov [rel _gen_close_cache], rax
.ret:
    leave
    ret
END_FUNC _get_gen_close_builtin

;; ============================================================================
;; Data section
;; ============================================================================
section .data

gen_name_str:  db "generator", 0
gen_repr_str:  db "<generator>", 0

; Cached builtin singletons for gen methods
_gen_send_cache: dq 0
_gen_close_cache: dq 0

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
    dq gen_getattr              ; tp_getattr (.send, .close, .throw)
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
