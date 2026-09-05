; frameobj.asm - the snapshot sys._getframe() hands back.
;
; A PyFrame is pooled and recycled: eval_frame takes one from a free list and
; puts it back on return, so a Python object holding a PyFrame* would be
; looking at a frame belonging to some later call.  CPython's frame objects
; are the frames; here they cannot be.
;
; So this is a SNAPSHOT.  _getframe walks the live chain once and copies what
; a caller can read -- the code object, the globals, the locals, and the line
; the frame is on -- into ordinary objects that outlive the frames.  What it
; cannot offer is anything that changes after the snapshot: f_lineno is where
; the frame was when _getframe ran, and there is no f_trace and no writing
; back into the frame.
;
; That is enough for what the stdlib actually does with it: warnings.warn
; reads f_globals to find the module a warning came from, and walks f_back to
; skip its own frames, which is what nine modules come in behind.

%include "macros.inc"
%include "object.inc"

ASM_INIT

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_incref
extern type_type
extern dict_new
extern str_from_cstr_heap
extern ap_strcmp
extern int_from_i64
extern none_singleton
extern raise_exception
extern exc_ValueError_type
extern exc_TypeError_type
extern eval_saved_r12
extern code_addr2line
extern obj_as_index
extern str_type

;; FRAMEOBJ_DROP field -- release one owned field during dealloc.
%macro FRAMEOBJ_DROP 1
    mov rdi, [rbx + PyFrameObject.%1]
    test rdi, rdi
    jz %%skip
    mov qword [rbx + PyFrameObject.%1], 0
    call obj_decref
%%skip:
%endmacro

;; FRAMEOBJ_ATTR name, field -- one object-valued attribute of the snapshot.
%macro FRAMEOBJ_ATTR 2
    mov rdi, [rbp - FOG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, %1
    call ap_strcmp
    test eax, eax
    jnz %%next
    mov rax, [rbp - FOG_SELF]
    mov rax, [rax + PyFrameObject.%2]
    test rax, rax
    jnz %%have
    LOAD_NONE rax
%%have:
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
%%next:
%endmacro

section .text

;; ============================================================================
;; frameobj_new(rdi = a live PyFrame*) -> rax = the snapshot, or 0
;;
;; f_back is filled in by the caller, which walks the chain outward.
;; ============================================================================
FON_FRAME_IN equ 8
FON_OBJ      equ 16
FON_FRAME    equ 40            ; + 1 push = 48, 16-aligned
DEF_FUNC frameobj_new, FON_FRAME
    push rbx
    mov [rbp - FON_FRAME_IN], rdi

    mov edi, PyFrameObject_size
    call ap_malloc
    test rax, rax
    jz .fon_fail
    mov rbx, rax
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rcx, [rel frame_object_type]
    mov [rbx + PyObject.ob_type], rcx
    mov qword [rbx + PyFrameObject.f_back], 0

    mov rdi, [rbp - FON_FRAME_IN]
    mov rax, [rdi + PyFrame.code]
    mov [rbx + PyFrameObject.f_code], rax
    test rax, rax
    jz .fon_no_code
    INCREF rax
.fon_no_code:

    mov rdi, [rbp - FON_FRAME_IN]
    mov rax, [rdi + PyFrame.globals]
    test rax, rax
    jnz .fon_have_globals
    LOAD_NONE rax
.fon_have_globals:
    mov [rbx + PyFrameObject.f_globals], rax
    INCREF rax

    mov rdi, [rbp - FON_FRAME_IN]
    mov rax, [rdi + PyFrame.locals]
    test rax, rax
    jnz .fon_have_locals
    ; A frame using fast locals has no dict; CPython builds one on demand and
    ; this hands back the globals, which is what locals() does at module
    ; scope and the closest honest answer here.
    mov rax, [rbx + PyFrameObject.f_globals]
.fon_have_locals:
    mov [rbx + PyFrameObject.f_locals], rax
    INCREF rax

    mov rdi, [rbp - FON_FRAME_IN]
    mov rax, [rdi + PyFrame.builtins]
    test rax, rax
    jnz .fon_have_builtins
    LOAD_NONE rax
.fon_have_builtins:
    mov [rbx + PyFrameObject.f_builtins], rax
    INCREF rax

    ; The line, from the instruction pointer and the code object's line
    ; table.  instr_ptr is an absolute address into co_code; addr2line wants
    ; the offset in code units.
    mov qword [rbx + PyFrameObject.f_lineno], 0
    mov qword [rbx + PyFrameObject.f_lasti], -1
    mov rdi, [rbx + PyFrameObject.f_code]
    test rdi, rdi
    jz .fon_done
    ; Where the frame is NOW: the innermost one is the interpreter's, whose
    ; IP lives in eval_saved_rbx; an outer one was left at the call it made,
    ; recorded in call_ip; a suspended generator's is instr_ptr.  Reading
    ; only instr_ptr answered 0 for every running frame, so f_lineno was 0
    ; and f_lasti -1 for all of them.
    mov rcx, [rbp - FON_FRAME_IN]
    extern eval_saved_r12
    mov rax, [rel eval_saved_r12]
    cmp rax, rcx
    jne .fon_not_current
    extern eval_saved_rbx
    mov rsi, [rel eval_saved_rbx]
    jmp .fon_have_ip
.fon_not_current:
    mov rsi, [rcx + PyFrame.call_ip]
    test rsi, rsi
    jnz .fon_have_ip
    mov rsi, [rcx + PyFrame.instr_ptr]
.fon_have_ip:
    test rsi, rsi
    jz .fon_done
    lea rax, [rdi + PyCodeObject.co_code]
    sub rsi, rax                ; the byte offset
    js .fon_done
    ; f_lasti is a BYTE offset into co_code, as CPython's is; the line table
    ; is indexed in code units, so the two are kept apart here.
    mov [rbx + PyFrameObject.f_lasti], rsi
    shr rsi, 1                  ; code units are two bytes
    call code_addr2line
    mov [rbx + PyFrameObject.f_lineno], rax

.fon_done:
    mov rax, rbx
    pop rbx
    leave
    ret
.fon_fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC frameobj_new

;; ============================================================================
;; frameobj_from_code(rdi = a code object or 0, rsi = the line, rdx = lasti)
;;   -> rax = a snapshot, or 0
;;
;; What a traceback entry can offer for tb_frame.  A traceback records the
;; code and the line and NOT the frame -- by the time anything looks, the
;; frame has been recycled -- so the globals are an empty dict rather than a
;; lie about which module it was.  CPython's traceback.py reads f_code and
;; f_globals; the first is right, and the second only costs it the source
;; line it would have looked up.
;; ============================================================================
FFC_CODE  equ 8
FFC_LINE  equ 16
FFC_LASTI equ 24
FFC_OBJ   equ 32
FFC_FRAME equ 48            ; + 0 pushes = 48
DEF_FUNC frameobj_from_code, FFC_FRAME
    mov [rbp - FFC_CODE], rdi
    mov [rbp - FFC_LINE], rsi
    mov [rbp - FFC_LASTI], rdx

    mov edi, PyFrameObject_size
    call ap_malloc
    test rax, rax
    jz .ffc_fail
    mov [rbp - FFC_OBJ], rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel frame_object_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyFrameObject.f_back], 0
    mov rcx, [rbp - FFC_CODE]
    mov [rax + PyFrameObject.f_code], rcx
    test rcx, rcx
    jz .ffc_no_code
    INCREF rcx
.ffc_no_code:
    mov rcx, [rbp - FFC_LINE]
    mov [rax + PyFrameObject.f_lineno], rcx
    ; The caller passes code units, as the traceback stores them; the
    ; attribute is a byte offset, as CPython's is.
    mov rcx, [rbp - FFC_LASTI]
    add rcx, rcx
    mov [rax + PyFrameObject.f_lasti], rcx

    call dict_new
    test rax, rax
    jz .ffc_drop
    mov rcx, [rbp - FFC_OBJ]
    mov [rcx + PyFrameObject.f_globals], rax
    INCREF rax
    mov [rcx + PyFrameObject.f_locals], rax
    LOAD_NONE rax
    INCREF rax
    mov [rcx + PyFrameObject.f_builtins], rax
    mov rax, rcx
    leave
    ret
.ffc_drop:
    mov rdi, [rbp - FFC_OBJ]
    call ap_free
.ffc_fail:
    xor eax, eax
    leave
    ret
END_FUNC frameobj_from_code

;; ============================================================================
;; sys._getframe([depth]) -> a snapshot of the frame `depth` levels up
;;
;; Depth 0 is the caller's frame: a builtin has no Python frame of its own for
;; it to count.  The whole chain from there outward is snapshotted, because
;; f_back has to keep working after the live frames are gone.
;; ============================================================================
SGF_DEPTH equ 8
SGF_HEAD  equ 16
SGF_PREV  equ 24
SGF_FRAME equ 40            ; + 1 push = 48, 16-aligned
DEF_FUNC sys_getframe_func, SGF_FRAME
    push rbx
    mov qword [rbp - SGF_DEPTH], 0
    test rsi, rsi
    jz .sgf_have_depth
    mov rdi, [rdi]
    V_UNPACK rdi, rdx
    call obj_as_index
    ; CPython treats a negative depth as zero rather than refusing it.
    test rax, rax
    js .sgf_have_depth
    mov [rbp - SGF_DEPTH], rax
.sgf_have_depth:

    mov rbx, [rel eval_saved_r12]
    test rbx, rbx
    jz .sgf_too_deep
.sgf_descend:
    cmp qword [rbp - SGF_DEPTH], 0
    je .sgf_at_frame
    mov rbx, [rbx + PyFrame.prev_frame]
    test rbx, rbx
    jz .sgf_too_deep
    dec qword [rbp - SGF_DEPTH]
    jmp .sgf_descend

.sgf_at_frame:
    ; Snapshot this frame and every one outside it, linking them by f_back.
    mov qword [rbp - SGF_HEAD], 0
    mov qword [rbp - SGF_PREV], 0
.sgf_walk:
    test rbx, rbx
    jz .sgf_walked
    mov rdi, rbx
    call frameobj_new
    test rax, rax
    jz .sgf_failed
    cmp qword [rbp - SGF_HEAD], 0
    jne .sgf_link
    mov [rbp - SGF_HEAD], rax
    jmp .sgf_advance
.sgf_link:
    mov rcx, [rbp - SGF_PREV]
    mov [rcx + PyFrameObject.f_back], rax   ; takes over the reference
.sgf_advance:
    mov [rbp - SGF_PREV], rax
    mov rbx, [rbx + PyFrame.prev_frame]
    jmp .sgf_walk

.sgf_walked:
    mov rax, [rbp - SGF_HEAD]
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sgf_failed:
    mov rdi, [rbp - SGF_HEAD]
    test rdi, rdi
    jz .sgf_null
    call obj_decref
.sgf_null:
    xor eax, eax
    pop rbx
    leave
    ret

.sgf_too_deep:
    pop rbx
    RAISE exc_ValueError_type, "call stack is not deep enough"
END_FUNC sys_getframe_func

;; ============================================================================
;; sys._getframemodulename([depth]) -> the __name__ of that frame's globals
;;
;; What warnings._deprecated actually wants; CPython added it in 3.12 for
;; exactly this, so the common case need not build a frame object at all.
;; ============================================================================
SGM_FRAME equ 24            ; + 1 push = 32, 16-aligned
DEF_FUNC sys_getframemodulename_func, SGM_FRAME
    push rbx
    xor ecx, ecx
    test rsi, rsi
    jz .sgm_have_depth
    push rcx
    mov rdi, [rdi]
    V_UNPACK rdi, rdx
    call obj_as_index
    pop rcx
    mov rcx, rax
.sgm_have_depth:
    mov rbx, [rel eval_saved_r12]
.sgm_descend:
    test rbx, rbx
    jz .sgm_none
    test rcx, rcx
    jz .sgm_at_frame
    mov rbx, [rbx + PyFrame.prev_frame]
    dec rcx
    jmp .sgm_descend
.sgm_at_frame:
    mov rdi, [rbx + PyFrame.globals]
    test rdi, rdi
    jz .sgm_none
    CSTRING rsi, "__name__"
    push rdi
    sub rsp, 8
    mov rdi, rsi
    call str_from_cstr_heap
    add rsp, 8
    pop rdi
    push rax
    sub rsp, 8
    mov rsi, rax
    extern dict_get
    call dict_get
    add rsp, 8
    pop rcx
    push rax
    sub rsp, 8
    mov rdi, rcx
    call obj_decref             ; the key
    add rsp, 8
    pop rax
    test rax, rax
    jz .sgm_none
    INCREF_V rax, rdx
    pop rbx
    leave
    ret
.sgm_none:
    LOAD_NONE rax
    INCREF rax
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC sys_getframemodulename_func

;; ============================================================================
;; The frame snapshot's own behaviour.
;; ============================================================================
DEF_FUNC frameobj_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    FRAMEOBJ_DROP f_back
    FRAMEOBJ_DROP f_code
    FRAMEOBJ_DROP f_globals
    FRAMEOBJ_DROP f_locals
    FRAMEOBJ_DROP f_builtins
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC frameobj_dealloc

FOG_SELF  equ 8
FOG_NAME  equ 16
FOG_FRAME equ 32            ; + 0 pushes = 32
DEF_FUNC frameobj_getattr, FOG_FRAME
    mov [rbp - FOG_SELF], rdi
    mov [rbp - FOG_NAME], rsi
    lea rdi, [rsi + PyStrObject.data]

    FRAMEOBJ_ATTR "f_back",     f_back
    FRAMEOBJ_ATTR "f_code",     f_code
    FRAMEOBJ_ATTR "f_globals",  f_globals
    FRAMEOBJ_ATTR "f_locals",   f_locals
    FRAMEOBJ_ATTR "f_builtins", f_builtins

    lea rdi, [rsi + PyStrObject.data]
    mov rdi, [rbp - FOG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "f_lineno"
    call ap_strcmp
    test eax, eax
    jz .fog_lineno
    mov rdi, [rbp - FOG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "f_lasti"
    call ap_strcmp
    test eax, eax
    jz .fog_lasti

    ; Unknown: NULL, so the caller decides -- the contract every other
    ; tp_getattr here keeps.
    RET_NULL
    leave
    V_PACK rax, rdx
    ret

.fog_lineno:
    mov rax, [rbp - FOG_SELF]
    mov rdi, [rax + PyFrameObject.f_lineno]
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret
.fog_lasti:
    mov rax, [rbp - FOG_SELF]
    mov rdi, [rax + PyFrameObject.f_lasti]
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret
END_FUNC frameobj_getattr

DEF_FUNC frameobj_repr
    CSTRING rdi, "<frame object>"
    extern str_from_cstr
    call str_from_cstr
    leave
    ret
END_FUNC frameobj_repr

section .data
align 8
global frame_object_type
frame_object_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq fo_name                  ; tp_name
    dq PyFrameObject_size       ; tp_basicsize
    dq frameobj_dealloc         ; tp_dealloc
    dq frameobj_repr            ; tp_repr
    dq frameobj_repr            ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq frameobj_getattr         ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset
    dq 0                        ; tp_tailslots

section .rodata
fo_name: db "frame", 0
