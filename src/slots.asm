; slots.asm - Install real type slots on a heaptype from its Python dunders.
;
; __build_class__ leaves every tp_as_*, tp_iter, tp_iternext, tp_hash, tp_call
; and tp_richcompare at zero.  Dispatch to a user class was therefore wired
; ad-hoc, one operation at a time, wherever somebody remembered: of the 163
; slot reads in the tree, 130 have no dunder fallback at all.  The ones nobody
; wired are simply absent -- sorted(MyIterator()) called through a NULL
; tp_iternext, any(MyIterable()) raised TypeError, and -obj dereferenced a
; NULL tp_as_number.
;
; This is CPython's answer: at class creation, install a small wrapper into
; the slot for each dunder the class defines.  Every slot reader then becomes
; correct with no edit -- including the 23 readers of tp_iter across 11 files
; -- and the ad-hoc fallbacks become dead weight rather than load-bearing.
;
; A wrapper cannot signal failure the way CPython's can, because most callers
; here do not check the result: get_iterator does `call rax` and immediately
; dereferences what comes back.  So a wrapper whose dunder raises re-enters
; the interpreter's unwinder directly, exactly as raise_exception does, and
; never returns to its caller.  The one exception is tp_iternext, where NULL
; is the ordinary "exhausted" answer and every caller already handles it.

%include "macros.inc"
%include "object.inc"
%include "types.inc"

; One row of the dunder-to-slot table.
struc SlotEntry
    .name:    resq 1        ; dunder name, a C string
    .offset:  resq 1        ; byte offset of the slot within PyTypeObject
    .wrapper: resq 1        ; function to install there
endstruc

extern dunder_lookup
extern dunder_call_1
extern dunder_iter
extern dunder_next
extern current_exception
extern eval_exception_unwind
extern exc_StopIteration_type
extern obj_decref

section .text

;; ============================================================================
;; slot_reraise - resume unwinding with the exception the dunder left pending.
;;
;; Does not return.  If somehow nothing is pending, there is no coherent value
;; to hand back either, so report it rather than continue with a NULL.
;; ============================================================================
DEF_FUNC_LOCAL slot_reraise
    cmp qword [rel current_exception], 0
    je .no_exc
    leave
    jmp eval_exception_unwind
.no_exc:
    extern raise_exception
    extern exc_RuntimeError_type
    lea rdi, [rel exc_RuntimeError_type]
    CSTRING rsi, "slot wrapper failed without an exception"
    call raise_exception
END_FUNC slot_reraise

;; ============================================================================
;; slot_tp_iter(rdi = self) -> rax = iterator, a raw pointer
;;
;; get_iterator does `call rax` and then reads ob_type off the result without
;; a NULL check, so this must either return an object or not return.
;; ============================================================================
DEF_FUNC slot_tp_iter
    lea rsi, [rel dunder_iter]
    call dunder_call_1
    V_UNPACK rax, rdx
    test edx, edx
    jz .failed
    leave
    ret
.failed:
    call slot_reraise           ; does not return
END_FUNC slot_tp_iter

;; ============================================================================
;; slot_tp_iternext(rdi = self) -> Value, or NULL when exhausted
;;
;; NULL is the ordinary answer here, so this mirrors call_iternext: a
;; StopIteration is swallowed and reported as exhaustion, and any other
;; exception is left pending for the caller to notice.
;; ============================================================================
DEF_FUNC slot_tp_iternext
    lea rsi, [rel dunder_next]
    call dunder_call_1
    V_UNPACK rax, rdx
    test edx, edx
    jnz .got_value

    mov rax, [rel current_exception]
    test rax, rax
    jz .exhausted
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel exc_StopIteration_type]
    cmp rcx, rdx
    jne .exhausted              ; a different exception: leave it pending
    mov rdi, rax
    mov qword [rel current_exception], 0
    call obj_decref

.exhausted:
    RET_NULL
    leave
    ret

.got_value:
    V_PACK rax, rdx
    leave
    ret
END_FUNC slot_tp_iternext

;; ============================================================================
;; type_install_slots(rdi = heaptype)
;;
;; Fill the type's slots from the dunders it defines.  Called once at class
;; creation, and again by type_setattr when a dunder is assigned afterwards,
;; so `C.__iter__ = f` takes effect the way it does in CPython.
;; ============================================================================
TIS_TYPE  equ 8
TIS_ENTRY equ 16
TIS_FRAME equ 16

global type_install_slots
DEF_FUNC type_install_slots, TIS_FRAME
    push rbx
    push r12

    mov [rbp - TIS_TYPE], rdi
    lea rbx, [rel slot_table]

.next_entry:
    mov rax, [rbx + SlotEntry.name]
    test rax, rax
    jz .done

    mov [rbp - TIS_ENTRY], rbx
    mov rdi, [rbp - TIS_TYPE]
    mov rsi, rax
    call dunder_lookup          ; walks the MRO; returns a Value
    V_UNPACK rax, rdx
    mov rbx, [rbp - TIS_ENTRY]
    test edx, edx
    jz .skip                    ; the class does not define this dunder
    ; A dunder explicitly set to None disables the protocol in Python, so
    ; leave the slot empty rather than installing a wrapper that would call
    ; None.
    extern none_singleton
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .skip

    mov rcx, [rbp - TIS_TYPE]
    mov rax, [rbx + SlotEntry.offset]
    mov rdx, [rbx + SlotEntry.wrapper]
    mov [rcx + rax], rdx

.skip:
    add rbx, SlotEntry_size
    jmp .next_entry

.done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_install_slots

section .rodata

sl_iter_name: db "__iter__", 0
sl_next_name: db "__next__", 0

align 8
slot_table:
    dq sl_iter_name, PyTypeObject.tp_iter,     slot_tp_iter
    dq sl_next_name, PyTypeObject.tp_iternext, slot_tp_iternext
    dq 0, 0, 0
