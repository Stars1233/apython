; tracebackobj.asm - the traceback OBJECT: what `e.__traceback__` is
;
; Split out of exception.asm, which had grown past the 100k cap for a
; hand-written file.  The seam is the one that file already had: everything
; here is the type a traceback IS -- allocation, the three attributes a
; program reads off one, and the type object -- while raising, chaining and
; the exception hierarchy stay behind.  RENDERING a traceback is a third
; thing again and lives in src/traceback.asm, beside both of a code object's
; side tables.

%include "macros.inc"
%include "object.inc"
%include "value.inc"

ASM_INIT

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_incref
extern int_from_i64
extern str_from_cstr
extern ap_strcmp
extern str_type
extern type_type
extern none_singleton
extern raise_exception
extern exc_AttributeError_type

section .text

;; ============================================================================
;; Traceback support
;; ============================================================================

;; ============================================================================
;; traceback_new() -> PyTracebackObject*
;; Allocates a new traceback with tb_next=NULL, tb_lineno=0.
;; ============================================================================
DEF_FUNC traceback_new
    mov edi, PyTracebackObject_size
    call ap_malloc
    mov qword [rax + PyTracebackObject.ob_refcnt], 1
    lea rcx, [rel traceback_type]
    mov [rax + PyTracebackObject.ob_type], rcx
    mov qword [rax + PyTracebackObject.tb_next], 0
    mov qword [rax + PyTracebackObject.tb_lineno], 0
    mov qword [rax + PyTracebackObject.tb_code], 0
    mov qword [rax + PyTracebackObject.tb_lasti], 0
    leave
    ret
END_FUNC traceback_new

;; ============================================================================
;; traceback_dealloc(PyTracebackObject *tb) -> void
;; XDECREF tb_next, free self.
;; ============================================================================
DEF_FUNC traceback_dealloc
    push rbx
    push r12
    mov rbx, rdi
.td_node:
    ; Iterative, not recursive: a traceback chain is as deep as the call
    ; stack was, and freeing it recursively would overflow on exactly the
    ; deep-recursion case that produced it.
    mov rdi, [rbx + PyTracebackObject.tb_code]
    test rdi, rdi
    jz .td_no_code
    mov qword [rbx + PyTracebackObject.tb_code], 0
    call obj_decref
.td_no_code:
    mov r12, [rbx + PyTracebackObject.tb_next]
    mov rdi, rbx
    call ap_free
    test r12, r12
    jz .td_done
    dec qword [r12 + PyTracebackObject.ob_refcnt]
    jnz .td_done                   ; still referenced elsewhere
    mov rbx, r12
    jmp .td_node
.td_done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC traceback_dealloc

;; ============================================================================
;; traceback_getattr(PyTracebackObject *tb, PyStrObject *name) -> (rax, edx)
;; Handles tb_lineno, tb_next, tb_frame attributes.
;; ============================================================================
DEF_FUNC traceback_getattr
    push rbx
    push r12

    mov rbx, rdi            ; tb
    mov r12, rsi            ; name str

    ; Check "tb_lineno"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "tb_lineno"
    call ap_strcmp
    test eax, eax
    jz .tb_get_lineno

    ; Check "tb_next"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "tb_next"
    call ap_strcmp
    test eax, eax
    jz .tb_get_next

    ; Check "tb_frame"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "tb_frame"
    call ap_strcmp
    test eax, eax
    jz .tb_get_frame

    ; Check "tb_lasti"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "tb_lasti"
    call ap_strcmp
    test eax, eax
    jz .tb_get_lasti

    ; Not found
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.tb_get_lineno:
    mov rax, [rbx + PyTracebackObject.tb_lineno]
    mov edx, TAG_SMALLINT
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.tb_get_next:
    mov rax, [rbx + PyTracebackObject.tb_next]
    test rax, rax
    jz .tb_return_none
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.tb_get_frame:
    ; A snapshot built from what the entry records.  This answered None, and
    ; CPython's traceback.py reads tb_frame.f_code on every entry -- so
    ; importing anything that formats a traceback died on the None.
    mov rdi, [rbx + PyTracebackObject.tb_code]
    mov rsi, [rbx + PyTracebackObject.tb_lineno]
    mov rdx, [rbx + PyTracebackObject.tb_lasti]
    extern frameobj_from_code
    call frameobj_from_code
    test rax, rax
    jz .tb_return_none
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tb_get_lasti:
    ; Stored in code units, which is what the line and column tables are
    ; indexed by; CPython's attribute is a BYTE offset into co_code, and
    ; anything that indexes co_code with it -- dis, traceback -- reads it
    ; that way.
    mov rax, [rbx + PyTracebackObject.tb_lasti]
    add rax, rax
    mov edx, TAG_SMALLINT
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tb_return_none:
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC traceback_getattr

section .data
; Traceback type object (immortal)
align 8
global traceback_type
traceback_type:
    dq 1                    ; ob_refcnt (immortal)
    dq type_type            ; ob_type
    dq tb_type_name         ; tp_name
    dq PyTracebackObject_size ; tp_basicsize
    dq traceback_dealloc    ; tp_dealloc
    dq 0                    ; tp_repr
    dq 0                    ; tp_str
    dq 0                    ; tp_hash
    dq 0                    ; tp_call
    dq traceback_getattr    ; tp_getattr
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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots
tb_type_name: db "traceback", 0
