; weakrefmod.asm - the _weakref module
;
; weakref.py, _weakrefset.py and collections/__init__.py all import from here,
; so nothing downstream of collections could load without it: functools, enum,
; re, inspect, typing, dataclasses, copy.
;
; The references are real.  Rather than give every object a weakref list slot
; -- which would mean a new field in PyTypeObject and an edit to every static
; type table -- the links live in one side table keyed by the referent's
; address, and obj_dealloc consults it.  The table is empty in the common case
; and the check is one compare against a counter, so a program that never
; makes a weak reference pays nothing.
;
; The table owns its reference objects; a reference borrows its referent and
; has that pointer zeroed when the referent dies, before any callback runs.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "opcodes.inc"

extern ap_malloc
extern ap_free
extern gc_alloc
extern obj_incref
extern obj_decref
extern obj_dealloc
extern obj_hash
extern obj_repr
extern obj_richcompare_bool
extern dict_new
extern dict_get
extern dict_set
extern dict_del
extern dict_type
extern list_new
extern list_append
extern list_type
extern str_from_cstr_heap
extern str_new_heap
extern module_new
extern builtin_func_new
extern type_type
extern raise_exception
extern exc_TypeError_type
extern bool_true
extern bool_false
extern none_singleton
extern current_exception
extern v_int_bias

section .text

; ----------------------------------------------------------------------------
; weakref_table_get() -> rax = the side table, creating it on first use
; ----------------------------------------------------------------------------
DEF_FUNC_LOCAL weakref_table_get
    mov rax, [rel weakref_table]
    test rax, rax
    jnz .have
    call dict_new
    mov [rel weakref_table], rax
.have:
    leave
    ret
END_FUNC weakref_table_get

; ----------------------------------------------------------------------------
; weakref_chain(rdi = referent) -> rax = the list of references, or 0
; ----------------------------------------------------------------------------
DEF_FUNC_LOCAL weakref_chain
    push rbx
    mov rbx, rdi
    mov rax, [rel weakref_table]
    test rax, rax
    jz .none
    mov rdi, rax
    mov rsi, rbx
    add rsi, [rel v_int_bias]   ; a user address always fits an int immediate
    call dict_get
    pop rbx
    leave
    ret
.none:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC weakref_chain

; ----------------------------------------------------------------------------
; weakref_clear_for(rdi = object about to be freed)
;
; Called from obj_dealloc, and only when the table is non-empty.  Every
; reference to the object is emptied before any callback runs, so a callback
; that looks at the reference sees a dead one, as CPython guarantees.
; ----------------------------------------------------------------------------
WC_OBJ   equ 8
WC_LIST  equ 16
WC_IDX   equ 24
WC_EXC   equ 32
WC_FRAME equ 48
global weakref_clear_for
DEF_FUNC weakref_clear_for, WC_FRAME
    push rbx
    push r12
    mov [rbp - WC_OBJ], rdi

    call weakref_chain
    test rax, rax
    jz .done
    mov [rbp - WC_LIST], rax
    mov rdi, rax
    call obj_incref             ; hold it past the table entry going away

    ; Empty every reference first.
    mov rax, [rbp - WC_LIST]
    mov rbx, [rax + PyListObject.ob_size]
    mov r12, [rax + PyListObject.ob_item]
    xor ecx, ecx
.clear_loop:
    cmp rcx, rbx
    jge .clear_done
    mov rdx, [r12 + rcx*8]
    test rdx, rdx
    jz .clear_next
    mov qword [rdx + PyWeakRefObject.wr_object], 0
.clear_next:
    inc rcx
    jmp .clear_loop
.clear_done:

    ; Drop the table entry.  That releases the table's references, so a
    ; reference nobody else holds is freed here.
    mov rdi, [rel weakref_table]
    test rdi, rdi
    jz .no_entry
    mov rsi, [rbp - WC_OBJ]
    add rsi, [rel v_int_bias]
    call dict_del
    mov rax, [rel weakref_table]
    mov rax, [rax + PyDictObject.ob_size]
    mov [rel weakref_live], rax
.no_entry:

    ; Then the callbacks, on whichever references are still alive.
    mov qword [rbp - WC_IDX], 0
.cb_loop:
    mov rax, [rbp - WC_LIST]
    mov rcx, [rbp - WC_IDX]
    cmp rcx, [rax + PyListObject.ob_size]
    jge .cb_done
    mov rdx, [rax + PyListObject.ob_item]
    mov rbx, [rdx + rcx*8]
    inc qword [rbp - WC_IDX]
    test rbx, rbx
    jz .cb_loop
    cmp qword [rbx + PyObject.ob_refcnt], 0
    jle .cb_loop
    mov r12, [rbx + PyWeakRefObject.wr_callback]
    test r12, r12
    jz .cb_loop
    mov rax, [r12 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .cb_loop

    ; A callback that raises has no caller to hand the exception to; report
    ; nothing and put back whatever was being handled, as __del__ does.
    DUNDER_EXC_SAVE [rbp - WC_EXC]
    inc qword [rbx + PyObject.ob_refcnt]
    sub rsp, 16
    mov [rsp], rbx
    mov rdi, r12
    mov rsi, rsp
    mov edx, 1
    call rax
    add rsp, 16
    V_UNPACK rax, rdx
    test edx, edx
    jz .cb_no_result
    DECREF_VAL rax, rdx
.cb_no_result:
    dec qword [rbx + PyObject.ob_refcnt]
    DUNDER_RAISED [rbp - WC_EXC], .cb_raised
.cb_resume:
    jmp .cb_loop

.cb_raised:
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    call obj_decref
    mov rax, [rbp - WC_EXC]
    mov [rel current_exception], rax
    jmp .cb_resume

.cb_done:
    mov rdi, [rbp - WC_LIST]
    call obj_decref
.done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC weakref_clear_for

; ----------------------------------------------------------------------------
; weakref_make(rdi = type, rsi = referent, rdx = callback or 0) -> rax = ref
; ----------------------------------------------------------------------------
WM_TYPE  equ 8
WM_OBJ   equ 16
WM_CB    equ 24
WM_REF   equ 32
WM_FRAME equ 48
DEF_FUNC_LOCAL weakref_make, WM_FRAME
    push rbx
    mov [rbp - WM_TYPE], rdi
    mov [rbp - WM_OBJ], rsi
    mov [rbp - WM_CB], rdx

    ; Without a callback the reference is shared, so `ref(x) is ref(x)`, as
    ; CPython does.  With one it must be distinct: each callback fires once.
    test rdx, rdx
    jnz .fresh
    mov rdi, rsi
    call weakref_chain
    test rax, rax
    jz .fresh
    mov rcx, [rax + PyListObject.ob_size]
    mov rdx, [rax + PyListObject.ob_item]
    xor r8d, r8d
.reuse_scan:
    cmp r8, rcx
    jge .fresh
    mov rbx, [rdx + r8*8]
    test rbx, rbx
    jz .reuse_next
    cmp qword [rbx + PyWeakRefObject.wr_callback], 0
    jne .reuse_next
    mov rax, [rbx + PyObject.ob_type]
    cmp rax, [rbp - WM_TYPE]
    jne .reuse_next
    mov rdi, rbx
    call obj_incref
    mov rax, rbx
    pop rbx
    leave
    ret
.reuse_next:
    inc r8
    jmp .reuse_scan

.fresh:
    mov rdi, [rbp - WM_TYPE]
    mov rdi, [rdi + PyTypeObject.tp_basicsize]
    cmp rdi, PyWeakRefObject_size
    jae .size_ok
    mov edi, PyWeakRefObject_size
.size_ok:
    call ap_malloc
    mov rbx, rax
    mov qword [rbx + PyObject.ob_refcnt], 1
    mov rax, [rbp - WM_TYPE]
    mov [rbx + PyObject.ob_type], rax
    inc qword [rax + PyObject.ob_refcnt]
    mov rax, [rbp - WM_OBJ]
    mov [rbx + PyWeakRefObject.wr_object], rax
    mov rax, [rbp - WM_CB]
    mov [rbx + PyWeakRefObject.wr_callback], rax
    test rax, rax
    jz .no_cb
    mov rdi, rax
    call obj_incref
.no_cb:
    mov qword [rbx + PyWeakRefObject.wr_hash], -1

    ; A subclass may have an instance dict beyond our fields; zero it.
    mov rax, [rbp - WM_TYPE]
    mov rcx, [rax + PyTypeObject.tp_dictoffset]
    test rcx, rcx
    jz .no_dict
    mov qword [rbx + rcx], 0
.no_dict:

    ; Register in the side table.
    call weakref_table_get
    mov [rbp - WM_REF], rax
    mov rdi, rax
    mov rsi, [rbp - WM_OBJ]
    add rsi, [rel v_int_bias]
    call dict_get
    test rax, rax
    jnz .have_chain
    xor edi, edi
    call list_new
    push rax
    mov rdi, [rbp - WM_REF]
    mov rsi, [rbp - WM_OBJ]
    add rsi, [rel v_int_bias]
    mov rdx, [rsp]
    call dict_set
    pop rax
    push rax
    mov rdi, rax
    call obj_decref             ; the table holds it now
    pop rax
.have_chain:
    mov rdi, rax
    mov rsi, rbx
    call list_append
    mov rax, [rel weakref_table]
    mov rax, [rax + PyDictObject.ob_size]
    mov [rel weakref_live], rax

    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC weakref_make

; ----------------------------------------------------------------------------
; ref_dealloc / ref_call / ref_repr / ref_hash / ref_richcompare
; ----------------------------------------------------------------------------
DEF_FUNC_LOCAL ref_dealloc
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyWeakRefObject.wr_callback]
    test rdi, rdi
    jz .no_cb
    mov qword [rbx + PyWeakRefObject.wr_callback], 0
    call obj_decref
.no_cb:
    mov rdi, [rbx + PyObject.ob_type]
    test rdi, rdi
    jz .free
    call obj_decref
.free:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC ref_dealloc

; Calling a reference yields the referent, or None once it is gone.
DEF_FUNC ref_deref
    mov rax, [rdi + PyWeakRefObject.wr_object]
    test rax, rax
    jz .dead
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.dead:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC ref_deref

RR_SELF  equ 8
RR_BUF   equ 1040
RR_FRAME equ 1056
DEF_FUNC_LOCAL ref_repr, RR_FRAME
    push rbx
    push r12
    mov [rbp - RR_SELF], rdi
    lea rbx, [rbp - RR_BUF]
    xor r12d, r12d
    mov rax, [rdi + PyWeakRefObject.wr_object]
    test rax, rax
    jz .dead
    CSTRING rsi, "<weakref at 0x0; to '"
    jmp .copy_prefix
.dead:
    CSTRING rsi, "<weakref at 0x0; dead>"
.copy_prefix:
    movzx eax, byte [rsi]
    test al, al
    jz .prefix_done
    inc rsi
    mov [rbx + r12], al
    inc r12
    jmp .copy_prefix
.prefix_done:
    mov rax, [rbp - RR_SELF]
    mov rax, [rax + PyWeakRefObject.wr_object]
    test rax, rax
    jz .finish
    mov rsi, [rax + PyObject.ob_type]
    mov rsi, [rsi + PyTypeObject.tp_name]
.copy_name:
    movzx eax, byte [rsi]
    test al, al
    jz .name_done
    cmp r12, RR_BUF - 8
    jae .name_done
    inc rsi
    mov [rbx + r12], al
    inc r12
    jmp .copy_name
.name_done:
    mov byte [rbx + r12], 0x27      ; a closing quote
    mov byte [rbx + r12 + 1], '>'
    add r12, 2
.finish:
    mov rdi, rbx
    mov rsi, r12
    call str_new_heap
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC ref_repr

; A live reference hashes as its referent and caches the result, so it keeps
; the same hash after the referent dies -- which is what lets a WeakSet find
; and discard a dead entry.
DEF_FUNC_LOCAL ref_hash
    push rbx
    mov rbx, rdi
    mov rax, [rbx + PyWeakRefObject.wr_hash]
    cmp rax, -1
    jne .cached
    mov rdi, [rbx + PyWeakRefObject.wr_object]
    test rdi, rdi
    jz .dead
    call obj_hash
    mov [rbx + PyWeakRefObject.wr_hash], rax
.cached:
    pop rbx
    leave
    ret
.dead:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "weak object has gone away"
    call raise_exception
END_FUNC ref_hash

; Two live references compare as their referents do; a dead one only ever
; equals itself.
RC_SELF  equ 8
RC_OTHER equ 16
RC_OP    equ 24
RC_FRAME equ 32
DEF_FUNC_LOCAL ref_richcompare, RC_FRAME
    mov [rbp - RC_SELF], rdi
    mov [rbp - RC_OTHER], rsi
    mov [rbp - RC_OP], rdx
    cmp edx, CMP_EQ
    je .eq_or_ne
    cmp edx, CMP_NE
    je .eq_or_ne
    lea rax, [rel notimpl_ref]
    mov rax, [rax]
    jmp .out_ptr

.eq_or_ne:
    V_TEST_PTR rsi, rax
    ja .not_equal
    test rsi, rsi
    jz .not_equal
    mov rax, [rsi + PyObject.ob_type]
    cmp rax, [rdi + PyObject.ob_type]
    jne .not_equal
    mov rdi, [rdi + PyWeakRefObject.wr_object]
    mov rsi, [rsi + PyWeakRefObject.wr_object]
    test rdi, rdi
    jz .by_identity
    test rsi, rsi
    jz .by_identity
    call obj_richcompare_bool
    test eax, eax
    jz .not_equal
    jmp .equal
.by_identity:
    mov rax, [rbp - RC_SELF]
    cmp rax, [rbp - RC_OTHER]
    jne .not_equal
.equal:
    cmp qword [rbp - RC_OP], CMP_EQ
    je .true
    jmp .false
.not_equal:
    cmp qword [rbp - RC_OP], CMP_EQ
    je .false
.true:
    lea rax, [rel bool_true]
    jmp .out_ptr
.false:
    lea rax, [rel bool_false]
.out_ptr:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC ref_richcompare

; ----------------------------------------------------------------------------
; ref(object[, callback]) -- tp_new for ReferenceType and its subclasses
; ----------------------------------------------------------------------------
DEF_FUNC ref_construct
    ; rdi = type, rsi = args, rdx = nargs
    cmp rdx, 1
    jl .bad
    push rbx
    mov rbx, rdi
    mov rdi, [rsi]
    V_TEST_PTR rdi, rax
    ja .not_referenceable
    test rdi, rdi
    jz .not_referenceable
    xor ecx, ecx
    cmp rdx, 2
    jl .no_cb
    mov rcx, [rsi + 8]
    lea rax, [rel none_singleton]
    cmp rcx, rax
    jne .have_cb
    xor ecx, ecx
.have_cb:
.no_cb:
    mov rsi, rdi
    mov rdx, rcx
    mov rdi, rbx
    call weakref_make
    mov edx, TAG_PTR
    pop rbx
    V_PACK rax, rdx
    leave
    ret
.not_referenceable:
    pop rbx
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "cannot create weak reference to this object"
    call raise_exception
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "ref expected at least 1 argument"
    call raise_exception
END_FUNC ref_construct

; ----------------------------------------------------------------------------
; proxy objects: the same structure, forwarding attribute access
; ----------------------------------------------------------------------------
DEF_FUNC_LOCAL proxy_referent
    mov rax, [rdi + PyWeakRefObject.wr_object]
    test rax, rax
    jz .dead
    leave
    ret
.dead:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "weakly-referenced object no longer exists"
    call raise_exception
END_FUNC proxy_referent

DEF_FUNC proxy_getattr
    push rbx
    mov rbx, rsi
    call proxy_referent
    mov rdi, rax
    mov rsi, rbx
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_getattr]
    test rax, rax
    jz .via_dict
    call rax
    pop rbx
    leave
    ret
.via_dict:
    mov rax, [rdi + PyObject.ob_type]
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .none
    mov rsi, rbx
    call dict_get
    test rax, rax
    jz .none
    INCREF_V rax, rcx
    pop rbx
    leave
    ret
.none:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC proxy_getattr

DEF_FUNC proxy_setattr
    push rbx
    push r12
    mov rbx, rsi
    mov r12, rdx
    call proxy_referent
    mov rdi, rax
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_setattr]
    test rax, rax
    jz .no_setattr
    mov rsi, rbx
    mov rdx, r12
    call rax
    pop r12
    pop rbx
    leave
    ret
.no_setattr:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "proxy object does not support attribute assignment"
    call raise_exception
END_FUNC proxy_setattr

DEF_FUNC proxy_repr
    call proxy_referent
    mov rdi, rax
    call obj_repr
    leave
    ret
END_FUNC proxy_repr

DEF_FUNC proxy_call
    push rbx
    push r12
    mov rbx, rsi
    mov r12, rdx
    call proxy_referent
    mov rdi, rax
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_callable
    mov rsi, rbx
    mov rdx, r12
    call rax
    pop r12
    pop rbx
    leave
    ret
.not_callable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "proxy object is not callable"
    call raise_exception
END_FUNC proxy_call

; ----------------------------------------------------------------------------
; Module functions
; ----------------------------------------------------------------------------
DEF_FUNC wr_proxy_func
    ; proxy(object[, callback])
    cmp rsi, 1
    jl .bad
    push rbx
    mov rbx, rdi
    mov rdi, [rbx]
    V_TEST_PTR rdi, rax
    ja .bad_pop
    test rdi, rdi
    jz .bad_pop
    ; A callable referent gets CallableProxyType, as CPython does.
    mov rax, [rdi + PyObject.ob_type]
    cmp qword [rax + PyTypeObject.tp_call], 0
    je .plain
    lea rax, [rel callableproxy_type]
    jmp .have_type
.plain:
    lea rax, [rel proxy_type]
.have_type:
    mov r8, rax
    xor ecx, ecx
    cmp rsi, 2
    jl .no_cb
    mov rcx, [rbx + 8]
    lea rax, [rel none_singleton]
    cmp rcx, rax
    jne .no_cb
    xor ecx, ecx
.no_cb:
    mov rsi, rdi
    mov rdx, rcx
    mov rdi, r8
    call weakref_make
    mov edx, TAG_PTR
    pop rbx
    V_PACK rax, rdx
    leave
    ret
.bad_pop:
    pop rbx
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "cannot create weak proxy to this object"
    call raise_exception
END_FUNC wr_proxy_func

DEF_FUNC wr_getweakrefcount_func
    cmp rsi, 1
    jl .zero
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .zero
    test rdi, rdi
    jz .zero
    call weakref_chain
    test rax, rax
    jz .zero
    mov rax, [rax + PyListObject.ob_size]
    add rax, [rel v_int_bias]
    leave
    ret
.zero:
    mov rax, [rel v_int_bias]
    leave
    ret
END_FUNC wr_getweakrefcount_func

DEF_FUNC wr_getweakrefs_func
    cmp rsi, 1
    jl .empty
    push rbx
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .empty_pop
    test rdi, rdi
    jz .empty_pop
    call weakref_chain
    test rax, rax
    jz .empty_pop
    mov rbx, rax
    xor edi, edi
    call list_new
    push rax
    mov rcx, [rbx + PyListObject.ob_size]
    mov rdx, [rbx + PyListObject.ob_item]
    xor r8d, r8d
.copy:
    cmp r8, rcx
    jge .copied
    push rcx
    push rdx
    push r8
    mov rdi, [rsp + 24]
    mov rsi, [rdx + r8*8]
    call list_append
    pop r8
    pop rdx
    pop rcx
    inc r8
    jmp .copy
.copied:
    pop rax
    mov edx, TAG_PTR
    pop rbx
    V_PACK rax, rdx
    leave
    ret
.empty_pop:
    pop rbx
.empty:
    xor edi, edi
    call list_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC wr_getweakrefs_func

; _remove_dead_weakref(dict, key): drop dict[key] if it is a reference whose
; referent has gone.  WeakValueDictionary calls this from its callback.
DEF_FUNC wr_remove_dead_func
    cmp rsi, 2
    jl .none
    push rbx
    mov rbx, rdi
    mov rdi, [rbx]
    V_TEST_PTR rdi, rax
    ja .none_pop
    mov rsi, [rbx + 8]
    push rdi
    push rsi
    call dict_get
    pop rsi
    pop rdi
    test rax, rax
    jz .none_pop
    V_TEST_PTR rax, rcx
    ja .none_pop
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel weakref_type]
    cmp rcx, rdx
    jne .none_pop
    cmp qword [rax + PyWeakRefObject.wr_object], 0
    jne .none_pop
    call dict_del
.none_pop:
    pop rbx
.none:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC wr_remove_dead_func

; ============================================================================
; Module construction
; ============================================================================
%macro WR_ADD_FUNC 2
    lea rdi, [rel %1]
    lea rsi, [rel %2]
    call builtin_func_new
    push rax
    lea rdi, [rel %2]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref
%endmacro

%macro WR_ADD_TYPE 2
    lea rdi, [rel %2]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    lea rdx, [rel %1]
    call dict_set
    pop rdi
    call obj_decref
%endmacro

WRM_FRAME equ 8
global weakref_module_create
DEF_FUNC weakref_module_create, WRM_FRAME
    push rbx
    push r12

    call dict_new
    mov r12, rax

    WR_ADD_FUNC wr_proxy_func,           wrm_proxy
    WR_ADD_FUNC wr_getweakrefcount_func, wrm_getweakrefcount
    WR_ADD_FUNC wr_getweakrefs_func,     wrm_getweakrefs
    WR_ADD_FUNC wr_remove_dead_func,     wrm_remove_dead

    WR_ADD_TYPE weakref_type,       wrm_ref
    WR_ADD_TYPE weakref_type,       wrm_ReferenceType
    WR_ADD_TYPE proxy_type,         wrm_ProxyType
    WR_ADD_TYPE callableproxy_type, wrm_CallableProxyType

    lea rdi, [rel wrm_name]
    call str_from_cstr_heap
    push rax
    mov rdi, rax
    mov rsi, r12
    call module_new
    mov rbx, rax
    pop rdi
    call obj_decref
    mov rdi, r12
    call obj_decref
    mov rax, rbx

    pop r12
    pop rbx
    leave
    ret
END_FUNC weakref_module_create

section .data
align 8
weakref_table: dq 0
global weakref_live
weakref_live:  dq 0
notimpl_ref:   dq notimpl_singleton
extern notimpl_singleton

align 8
wrm_ref_name:   db "weakref", 0
wrm_proxy_name: db "weakproxy", 0
wrm_cproxy_name: db "weakcallableproxy", 0

align 8
global weakref_type
weakref_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq wrm_ref_name             ; tp_name
    dq PyWeakRefObject_size     ; tp_basicsize
    dq ref_dealloc              ; tp_dealloc
    dq ref_repr                 ; tp_repr
    dq ref_repr                 ; tp_str
    dq ref_hash                 ; tp_hash
    dq ref_deref                ; tp_call — calling a reference derefs it
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq ref_richcompare          ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq ref_construct            ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_BASETYPE       ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset

align 8
global proxy_type
proxy_type:
    dq 1
    dq type_type
    dq wrm_proxy_name
    dq PyWeakRefObject_size
    dq ref_dealloc
    dq proxy_repr
    dq proxy_repr
    dq 0                        ; tp_hash — a proxy is unhashable
    dq 0                        ; tp_call
    dq proxy_getattr
    dq proxy_setattr
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0

align 8
global callableproxy_type
callableproxy_type:
    dq 1
    dq type_type
    dq wrm_cproxy_name
    dq PyWeakRefObject_size
    dq ref_dealloc
    dq proxy_repr
    dq proxy_repr
    dq 0
    dq proxy_call
    dq proxy_getattr
    dq proxy_setattr
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0
    dq 0

section .rodata
wrm_name:              db "_weakref", 0
wrm_proxy:             db "proxy", 0
wrm_getweakrefcount:   db "getweakrefcount", 0
wrm_getweakrefs:       db "getweakrefs", 0
wrm_remove_dead:       db "_remove_dead_weakref", 0
wrm_ref:               db "ref", 0
wrm_ReferenceType:     db "ReferenceType", 0
wrm_ProxyType:         db "ProxyType", 0
wrm_CallableProxyType: db "CallableProxyType", 0
