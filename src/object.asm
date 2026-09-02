; object.asm - PyObject base operations
; Allocation, reference counting, type dispatch for repr/str/hash/bool
; Fat-value aware: functions accept (payload, tag) pairs

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern ap_free
extern str_from_cstr
extern dict_get
extern dict_new
extern ap_strcmp
extern none_singleton
extern bool_false
extern bool_true
extern int_repr
extern int_type
extern current_exception
extern eval_saved_r13
extern eval_exception_unwind
extern int_to_i64
extern float_type
extern float_repr
extern none_repr
extern bool_repr
extern type_getattr
extern type_setattr
extern type_call


; obj_incref(PyObject *obj)
; Increment reference count; NULL-safe.
; Callers must only pass heap pointers (not SmallInts).
DEF_FUNC_BARE obj_incref
    test rdi, rdi
    jz .skip
    inc qword [rdi + PyObject.ob_refcnt]
.skip:
    ret
END_FUNC obj_incref

; obj_decref(PyObject *obj)
; Decrement reference count; deallocate if zero; NULL-safe.
; Callers must only pass heap pointers (not SmallInts).
DEF_FUNC_BARE obj_decref
    test rdi, rdi
    jz .skip
    dec qword [rdi + PyObject.ob_refcnt]
    jnz .skip
    ; refcount hit zero - deallocate
    jmp obj_dealloc
.skip:
    ret
END_FUNC obj_decref

; The trashcan.
;
; obj_decref -> obj_dealloc -> tp_dealloc -> obj_decref is one machine frame
; per level of a nested structure, and nothing bounded it: a list nested 200k
; deep walked the stack off its guard page the moment it was dropped, and the
; only symptom was SIGSEGV.  Past a nesting limit the object is set aside
; instead, on a chain threaded through its own ob_refcnt -- which is zero by
; definition here, and read by nothing until the object is picked back up --
; and the outermost dealloc frees the chain iteratively.
;
; This is CPython's Py_TRASHCAN, put in the one place every deallocation
; already funnels through rather than in each type's tp_dealloc.  The limit is
; CPython's Py_TRASHCAN_HEADROOM, and it has to be well above 1: the drain
; below runs at nesting 1, so a smaller one would deposit every child of every
; drained object and make no progress in the ordinary case.
TRASH_LIMIT equ 50

section .bss
trash_nesting: resq 1
trash_later:   resq 1

section .text

; obj_dealloc(PyObject *obj)
; Calls type's tp_dealloc if present, else just frees
DEF_FUNC_BARE obj_dealloc

    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 8                  ; the calls below want a 16-byte rsp
    mov rbx, rdi

    cmp qword [rel trash_nesting], TRASH_LIMIT
    jl .td_enter

    ; Too deep: set it aside for the outermost dealloc.  It has to leave the
    ; collector's lists first -- its tp_dealloc has not run, so nothing has
    ; untracked it, and a collection during the drain would otherwise walk an
    ; object whose refcount is already zero.  gc_untrack is idempotent, so the
    ; untrack inside tp_dealloc still runs harmlessly later.
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .td_no_gc
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .td_no_gc
    extern gc_untrack
    mov rdi, rbx
    call gc_untrack
.td_no_gc:
    mov rax, [rel trash_later]
    mov [rbx + PyObject.ob_refcnt], rax
    mov [rel trash_later], rbx
    jmp .td_out

.td_enter:
    inc qword [rel trash_nesting]

    ; Weak references to this object have to be emptied, and their callbacks
    ; run, before it is freed.  The links live in a side table rather than in
    ; the object, so the check is one compare against a counter that stays
    ; zero in a program that makes no weak references.
    extern weakref_live
    cmp qword [rel weakref_live], 0
    je .no_weakrefs
    extern weakref_clear_for
    mov rdi, rbx
    call weakref_clear_for
.no_weakrefs:

    ; Get type's tp_dealloc
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .just_free
    mov rax, [rax + PyTypeObject.tp_dealloc]
    test rax, rax
    jz .just_free

    ; Call tp_dealloc(obj)
    mov rdi, rbx
    call rax
    jmp .td_leave

.just_free:
    mov rdi, rbx
    call ap_free

.td_leave:
    dec qword [rel trash_nesting]
    jnz .td_out
    cmp qword [rel trash_later], 0
    je .td_out

    ; The outermost dealloc empties the chain, one object at a time.  The
    ; nesting stays at 1 for the whole drain, so each object's own children go
    ; on the chain rather than onto the machine stack once they are deep
    ; enough -- which is what keeps this loop, and not the stack, bounded.
.td_drain:
    inc qword [rel trash_nesting]
.td_drain_loop:
    mov rbx, [rel trash_later]
    test rbx, rbx
    jz .td_drain_done
    mov rax, [rbx + PyObject.ob_refcnt]
    mov [rel trash_later], rax
    mov qword [rbx + PyObject.ob_refcnt], 0
    mov rdi, rbx
    call obj_dealloc
    jmp .td_drain_loop
.td_drain_done:
    dec qword [rel trash_nesting]

.td_out:
    add rsp, 8
    pop rbx
    pop rbp
.bail:
    ret
END_FUNC obj_dealloc

; obj_repr(rdi=value) -> PyObject* (string)
; Decodes the Value, then dispatches: int immediate → int_repr, pointer → tp_repr.
DEF_FUNC obj_repr
    V_UNPACK rdi, rsi

    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_FLOAT
    je .float_tag

    ; TAG_PTR: use tp_repr
    test rdi, rdi
    jz .null_obj

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .no_repr
    mov rax, [rax + PyTypeObject.tp_repr]
    test rax, rax
    jz .no_repr

    ; tail-call tp_repr(obj, tag)
    mov edx, esi               ; pass tag for tag-aware repr (e.g., int_repr)
    leave
    jmp rax

.smallint:
    ; rdi = raw int value — int_repr checks edx for TAG_SMALLINT
    RET_TAG_SMALLINT
    call int_repr
    leave
    ret

.float_tag:
    ; rdi = raw double bits.  float_repr reads edx to tell these from a
    ; float subclass instance, which reaches it as a pointer.
    mov edx, TAG_FLOAT
    call float_repr
    leave
    ret

.none_tag:
    call none_repr
    leave
    ret

.bool_tag:
    test rdi, rdi
    jz .bool_false_repr
    lea rdi, [rel bool_true]
    call bool_repr
    leave
    ret
.bool_false_repr:
    lea rdi, [rel bool_false]
    call bool_repr
    leave
    ret

.null_obj:
.no_repr:
    ; Return a NULL *Value*, not just a zero payload: callers test the tag,
    ; and leaving edx stale made print() dereference the NULL.
    RET_NULL
    leave
    ret
END_FUNC obj_repr

; obj_str(rdi=value) -> PyObject* (string)
; Decodes the Value, then dispatches: int immediate → int_repr, pointer → tp_str
; falling back to tp_repr.
DEF_FUNC obj_str
    V_UNPACK rdi, rsi
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi               ; save tag

    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_FLOAT
    je .float_tag

    ; TAG_PTR path
    test rdi, rdi
    jz .fallback

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .fallback

    mov rax, [rax + PyTypeObject.tp_str]
    test rax, rax
    jz .fallback

    mov rdi, rbx
    mov edx, r12d              ; tag for tp_str (e.g., int_repr checks edx)
    call rax
    pop r12
    pop rbx
    leave
    ret

.smallint:
    ; SmallInt: delegate to int_repr
    mov rdi, rbx
    RET_TAG_SMALLINT
    call int_repr
    pop r12
    pop rbx
    leave
    ret

.float_tag:
    ; rbx = raw double bits; see the note in obj_repr.
    mov rdi, rbx
    mov edx, TAG_FLOAT
    call float_repr
    pop r12
    pop rbx
    leave
    ret

.none_tag:
    call none_repr
    pop r12
    pop rbx
    leave
    ret

.bool_tag:
    test rbx, rbx
    jz .bool_false_str
    lea rdi, [rel bool_true]
    call bool_repr                 ; bool tp_str = bool_repr
    pop r12
    pop rbx
    leave
    ret
.bool_false_str:
    lea rdi, [rel bool_false]
    call bool_repr
    pop r12
    pop rbx
    leave
    ret

.fallback:
    mov rdi, rbx
    mov rsi, r12
    V_PACK rdi, rsi
    call obj_repr
    pop r12
    pop rbx
    leave
    ret
END_FUNC obj_str

; obj_as_index(rdi = payload, edx = tag) -> rax = int64
;
; Convert a Value to a C index, or raise TypeError.  Callers used to hand
; whatever they were given straight to int_to_i64, which reads
; PyIntObject.compact unconditionally: a float's payload is raw IEEE bits, so
; range(1.5) dereferenced 0x3ff8000000000000, and None's fields decoded as a
; garbage length, so range(None) hung.
;
; Takes the same (payload, tag) pair as int_to_i64 so a call site changes by
; one word.  This is where the __index__ protocol belongs once heaptypes
; carry real slots.
DEF_FUNC obj_as_index
    cmp edx, TAG_SMALLINT
    je .oai_immediate
    cmp edx, TAG_PTR
    jne .oai_error
    mov rax, [rdi + PyObject.ob_type]
    REQUIRE_INT_TYPE rax, rcx, .oai_try_dunder
    call int_to_i64
    leave
    ret

.oai_immediate:
    mov rax, rdi
    leave
    ret

.oai_try_dunder:
    ; Not an int, but __index__ makes an object usable wherever one is
    ; wanted -- as a subscript, a repetition count, a slice bound, or an
    ; argument to hex().  This is the single place all of those converge.
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .oai_error
    mov rax, [rax + PyNumberMethods.nb_index]
    test rax, rax
    jz .oai_error
    call rax                    ; nb_index returns a Value
    V_UNPACK rax, rdx
    ; __index__ must itself return an int; one level only, so a class whose
    ; __index__ returns another such class is an error rather than a loop.
    cmp edx, TAG_SMALLINT
    je .oai_dunder_done
    cmp edx, TAG_PTR
    jne .oai_bad_index
    mov rcx, [rax + PyObject.ob_type]
    REQUIRE_INT_TYPE rcx, rsi, .oai_bad_index
    mov rdi, rax
    call int_to_i64
    leave
    ret
.oai_dunder_done:
    leave
    ret

.oai_bad_index:
    RAISE exc_TypeError_type, "__index__ returned non-int"

.oai_error:
    RAISE exc_TypeError_type, "object cannot be interpreted as an integer"
END_FUNC obj_as_index

; value_number_methods(rdi = payload, edx = tag) -> rax = PyNumberMethods*, or 0
;
; Resolve a Value's numeric protocol table, immediates included.  Callers that
; want an arithmetic slot need this rather than assuming int: builtin_divmod
; called int_floordiv unconditionally, so divmod(1.5, 1.5) crashed even though
; 1.5 // 1.5 has always worked.
DEF_FUNC_BARE value_number_methods
    cmp edx, TAG_SMALLINT
    je .vnm_int
    cmp edx, TAG_FLOAT
    je .vnm_float
    cmp edx, TAG_PTR
    jne .vnm_none
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    ret
.vnm_int:
    lea rax, [rel int_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    ret
.vnm_float:
    lea rax, [rel float_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    ret
.vnm_none:
    xor eax, eax
    ret
END_FUNC value_number_methods

; value_type(rdi = Value) -> rax = PyTypeObject*, or 0 for a NULL Value
;
; Resolve a Value's type, immediates included.  Several places open-code this
; three-way test; having it once keeps them from disagreeing.
DEF_FUNC_BARE value_type
    V_IS_INT rdi, rax
    jae .vt_int
    V_IS_FLOAT rdi, rax
    jb .vt_float
    test rdi, rdi
    jz .vt_null
    mov rax, [rdi + PyObject.ob_type]
    ; A heaptype's metatype is an internal split -- it exists only so that
    ; heaptypes get a tp_dealloc that static types must not have.  CPython
    ; has one `type`, and `type(C) is type` for an ordinary class, so report
    ; the one the language defines.
    extern user_type_metatype
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    jne .vt_done
    lea rax, [rel type_type]
.vt_done:
    ret
.vt_int:
    lea rax, [rel int_type]
    ret
.vt_float:
    lea rax, [rel float_type]
    ret
.vt_null:
    xor eax, eax
    ret
END_FUNC value_type

; raise_type_error_with_name(rdi = template C string with a single %s marker
;                            written as \x01, rsi = Value whose type to name)
; Composes the message into a static buffer and raises TypeError.  Does not
; return.
RTN_BUFSZ equ 160

section .rodata
rbt_open:    db ": '", 0
rbt_and:     db "' and '", 0
rbt_close:   db "'", 0
rbt_unknown: db "object", 0
mah_digits:  db "0123456789abcdef", 0

section .bss
rbt_buf: resb 320   ; two 80-char type names plus the prefix and separators

section .text
DEF_FUNC raise_type_error_with_name
    push rbx
    push r12
    mov rbx, rdi
    mov rdi, rsi
    call value_type
    mov r12, rax                    ; type, or 0
    jmp rtn_compose
END_FUNC raise_type_error_with_name

; raise_type_error_with_typename(rdi = the same template, rsi = a type object)
; For a caller that has released the object it is complaining about and kept
; only its type -- the object is gone by then, and reading ob_type off freed
; memory is exactly the bug this message is reporting.
DEF_FUNC raise_type_error_with_typename
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
rtn_compose:

    lea rdi, [rel rtn_buf]
    xor ecx, ecx
.rtn_copy:
    movzx eax, byte [rbx]
    test al, al
    jz .rtn_end
    inc rbx
    cmp al, 1
    je .rtn_insert
    cmp rcx, RTN_BUFSZ - 2
    jae .rtn_copy
    mov [rdi + rcx], al
    inc rcx
    jmp .rtn_copy
.rtn_insert:
    test r12, r12
    jz .rtn_copy
    mov rsi, [r12 + PyTypeObject.tp_name]
.rtn_name:
    movzx eax, byte [rsi]
    test al, al
    jz .rtn_copy
    inc rsi
    cmp rcx, RTN_BUFSZ - 2
    jae .rtn_copy
    mov [rdi + rcx], al
    inc rcx
    jmp .rtn_name
.rtn_end:
    mov byte [rdi + rcx], 0
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel rtn_buf]
    extern exc_TypeError_type
    extern raise_exception
    call raise_exception
    ud2
END_FUNC raise_type_error_with_typename

; raise_binop_type_error(rdi = left Value, rsi = right Value,
;                        rdx = prefix C string) -> never returns
; "<prefix>: 'int' and 'complex'", which is how CPython words every binary
; operator's TypeError.  With two operands the bare prefix does not say which
; one was wrong.
RBT_LEFT  equ 8
RBT_RIGHT equ 16
RBT_OPEN  equ 24
RBT_FRAME equ 40            ; + 1 push = 48, 16-aligned

DEF_FUNC_BARE raise_binop_type_error
    lea rcx, [rel rbt_open]     ; the default opener, ": '"
    jmp raise_binop_type_error_ex
END_FUNC raise_binop_type_error

; raise_binop_type_error_ex(rdi = left Value, rsi = right Value,
;                           rdx = prefix C string, rcx = opener C string)
;   -> never returns
; The opener is what sits between the prefix and the first type name.  A
; binary operator wants ": '", and COMPARE_OP wants " of '", because CPython
; words that one "'<' not supported between instances of 'int' and 'str'".
DEF_FUNC raise_binop_type_error_ex, RBT_FRAME
    push rbx
    mov [rbp - RBT_LEFT], rdi
    mov [rbp - RBT_RIGHT], rsi
    mov rbx, rdx
    mov [rbp - RBT_OPEN], rcx

    lea rdi, [rel rbt_buf]
    mov rsi, rbx
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RBT_OPEN]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RBT_LEFT]
    call rbt_typename
    mov rdi, rax
    lea rsi, [rel rbt_and]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RBT_RIGHT]
    call rbt_typename
    mov rdi, rax
    lea rsi, [rel rbt_close]
    call rbt_append_cstr

    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel rbt_buf]
    call raise_exception
    ud2
END_FUNC raise_binop_type_error_ex

; The cap and the buffer have to agree: 40 (prefix) + 3 + 80 + 7 + 80 + 1 + a
; NUL is 212, which overran a 192-byte buffer and wrote into the globals
; after it -- one of them being attr_error_pending, so an over-long type name
; in a divmod TypeError made the NEXT attribute error re-raise this one.
;; rbt_append_cstr(rdi = dest, rsi = src cstr) -> rax = the NUL it wrote.
;; Bounded at 80 bytes per field; the callers' buffers are sized for that.
global rbt_append_cstr
DEF_FUNC rbt_append_cstr
    xor ecx, ecx
.rbtc_loop:
    cmp rcx, 80
    jge .rbtc_done
    mov al, [rsi + rcx]
    test al, al
    jz .rbtc_done
    mov [rdi + rcx], al
    inc rcx
    jmp .rbtc_loop
.rbtc_done:
    lea rax, [rdi + rcx]
    mov byte [rax], 0
    leave
    ret
END_FUNC rbt_append_cstr

DEF_FUNC_LOCAL rbt_typename     ; (rdi = dest, rsi = a Value) -> rax = the NUL
    push rbx
    mov rbx, rdi
    mov rdi, rsi
    call value_type
    test rax, rax
    jz .rbtt_unknown
    mov rsi, [rax + PyTypeObject.tp_name]
    jmp .rbtt_have
.rbtt_unknown:
    lea rsi, [rel rbt_unknown]
.rbtt_have:
    mov rdi, rbx
    call rbt_append_cstr
    pop rbx
    leave
    ret
END_FUNC rbt_typename

;; ============================================================================
;; msg_append_i64(rdi = dest, rsi = the number) -> rax = the NUL it wrote
;;
;; The one thing a message could not carry.  Six file-local near-duplicates of
;; this exist -- in build.asm, bytes.asm twice, iomod.asm, structseq.asm and
;; func.asm -- and none of them is reachable from another file, which is why
;; "attempt to assign sequence of wrong size to extended slice" does not name
;; either size.  Signed, unlike most of those.
;; ============================================================================
DEF_FUNC msg_append_i64
    push rbx
    push r12
    mov rbx, rdi                ; dest
    mov rax, rsi

    sub rsp, 40
    lea rcx, [rsp + 32]
    mov byte [rcx], 0
    xor r12d, r12d              ; negative?
    test rax, rax
    jns .mai_digits
    mov r12d, 1
    neg rax
.mai_digits:
    test rax, rax
    jnz .mai_loop
    dec rcx
    mov byte [rcx], '0'
    jmp .mai_emit
.mai_loop:
    test rax, rax
    jz .mai_emit
    xor edx, edx
    mov r8, 10
    div r8
    add dl, '0'
    dec rcx
    mov [rcx], dl
    jmp .mai_loop
.mai_emit:
    test r12d, r12d
    jz .mai_copy
    dec rcx
    mov byte [rcx], '-'
.mai_copy:
    mov rdi, rbx
    mov rsi, rcx
    call rbt_append_cstr
    add rsp, 40
    pop r12
    pop rbx
    leave
    ret
END_FUNC msg_append_i64

;; ============================================================================
;; msg_append_hex2(rdi = dest, esi = a byte) -> rax = the NUL it wrote
;; Two lowercase hex digits, which is how CPython spells the byte in a
;; UnicodeDecodeError.
;; ============================================================================
DEF_FUNC msg_append_hex2
    movzx esi, sil
    mov eax, esi
    shr eax, 4
    lea rcx, [rel mah_digits]
    movzx eax, byte [rcx + rax]
    mov [rdi], al
    mov eax, esi
    and eax, 15
    movzx eax, byte [rcx + rax]
    mov [rdi + 1], al
    mov byte [rdi + 2], 0
    lea rax, [rdi + 2]
    leave
    ret
END_FUNC msg_append_hex2

;; ============================================================================
;; msg_append_escaped_cp(rdi = dest, rsi = a str, rdx = index in code points)
;;   -> rax = the NUL it wrote
;;
;; One character, quoted and escaped the way CPython writes it in a
;; UnicodeEncodeError: '\xNN', '\uNNNN' or '\UNNNNNNNN' by magnitude.
;; ============================================================================
MAE_DEST equ 8
MAE_CP   equ 16
MAE_FRAME equ 32            ; + 1 push = 40, not 16-aligned

DEF_FUNC msg_append_escaped_cp, MAE_FRAME
    push rbx
    mov [rbp - MAE_DEST], rdi
    mov rbx, rdi

    ; The code point at that index.  A str keeps UTF-8, so the byte offset has
    ; to be found first; an ASCII string is its own index.
    mov rdi, rsi
    mov rsi, rdx
    extern str_cp_at
    call str_cp_at
    mov [rbp - MAE_CP], rax

    mov byte [rbx], 39          ; a single quote
    lea rdi, [rbx + 1]
    ; Always escaped, printable or not: CPython writes even 'Z' as '\x5a' in
    ; a UnicodeEncodeError.
.mae_escape:
    mov byte [rdi], 92          ; a backslash
    inc rdi
    mov rax, [rbp - MAE_CP]
    cmp rax, 0x100
    jb .mae_x
    cmp rax, 0x10000
    jb .mae_u
    mov byte [rdi], 'U'
    inc rdi
    mov ecx, 8
    jmp .mae_digits
.mae_u:
    mov byte [rdi], 'u'
    inc rdi
    mov ecx, 4
    jmp .mae_digits
.mae_x:
    mov byte [rdi], 'x'
    inc rdi
    mov ecx, 2
.mae_digits:
    ; ecx nibbles, most significant first
    mov rax, [rbp - MAE_CP]
    lea r8, [rel mah_digits]
.mae_digit_loop:
    dec ecx
    mov r9, rax
    mov r10d, ecx
    shl r10d, 2
    mov r11, rcx
    mov ecx, r10d
    shr r9, cl
    mov rcx, r11
    and r9, 15
    movzx r9d, byte [r8 + r9]
    mov [rdi], r9b
    inc rdi
    test ecx, ecx
    jnz .mae_digit_loop

.mae_close:
    mov byte [rdi], 39
    mov byte [rdi + 1], 0
    lea rax, [rdi + 1]
    pop rbx
    leave
    ret
END_FUNC msg_append_escaped_cp

; raise_value_error_with_repr(rdi = prefix C string, rsi = the object Value)
;   -> never returns
;
; ValueError("<prefix><repr(obj)>"), which CPython writes as "%s: %R".  int's
; own copy of this is inline and stays there, because its prefix carries the
; base; float's message had simply lost the value it could not convert, and
; complex's underscore rule needs the same shape.
RVR_PREFIX equ 8
RVR_OBJ    equ 16
RVR_REPR   equ 24
RVR_FULL   equ 32
RVR_FRAME  equ 32           ; + 0 pushes = 32

extern str_from_cstr_heap
extern str_concat
extern exc_new
extern exc_ValueError_type
extern raise_exception_obj

DEF_FUNC raise_value_error_with_repr, RVR_FRAME
    mov [rbp - RVR_OBJ], rsi
    call str_from_cstr_heap         ; rdi still holds the prefix
    mov [rbp - RVR_PREFIX], rax

    mov rdi, [rbp - RVR_OBJ]
    call obj_repr
    test rax, rax
    jnz .rvr_have_repr
    ; repr itself raised.  Let that exception stand rather than replacing it
    ; with one about a message we could not build.
    mov rdi, [rbp - RVR_PREFIX]
    call obj_decref
    leave
    jmp eval_exception_unwind

.rvr_have_repr:
    mov [rbp - RVR_REPR], rax
    mov rdi, [rbp - RVR_PREFIX]
    mov rsi, rax
    mov ecx, TAG_PTR
    call str_concat
    mov [rbp - RVR_FULL], rax

    mov rdi, [rbp - RVR_PREFIX]
    call obj_decref
    mov rdi, [rbp - RVR_REPR]
    call obj_decref

    lea rdi, [rel exc_ValueError_type]
    mov rsi, [rbp - RVR_FULL]
    mov edx, TAG_PTR
    call exc_new
    mov [rbp - RVR_PREFIX], rax     ; the exception; that slot is free now
    mov rdi, [rbp - RVR_FULL]
    call obj_decref                 ; exc_new took its own reference

    mov rdi, [rbp - RVR_PREFIX]
    leave
    jmp raise_exception_obj         ; chains and unwinds; takes the reference
END_FUNC raise_value_error_with_repr

section .bss
; Set by instance_getattr when __getattr__ raised an AttributeError and it
; handed the exception back rather than unwinding.  Cleared on entry to every
; instance_getattr, so it cannot survive a lookup, and consumed by
; raise_no_attribute.
global attr_error_pending
attr_error_pending: resq 1

rtn_buf: resb RTN_BUFSZ
section .text

; seq_repeat_check_count(rsi = count Value) -- raises TypeError unless the
; count is an int (or a bool, which is one).  Does not return on failure.
DEF_FUNC_BARE seq_repeat_check_count
    V_IS_INT rsi, rax
    jae .src_ok
    V_TEST_PTR rsi, rax
    ja .src_bad
    test rsi, rsi
    jz .src_bad
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .src_ok
    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .src_ok
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jnz .src_ok
.src_bad:
    CSTRING rdi, `can't multiply sequence by non-int of type '\x01'`
    jmp raise_type_error_with_name
.src_ok:
    ret
END_FUNC seq_repeat_check_count

; raise_no_attribute(rdi = object Value, rsi = attribute-name str, edx = 1 for
; a set, 0 for a get) -- raises the AttributeError CPython raises.  Does not
; return.
RNA_NAME equ 16
RNA_FRAME equ 16            ; + 2 pushes = 32
extern str_type
DEF_FUNC raise_no_attribute, RNA_FRAME
    push rbx
    push r12
    ; A __getattr__ that raised AttributeError already said what it wanted
    ; said.  Replacing it here with a generic message threw that away, so
    ; instance_getattr hands it over with this flag rather than unwinding --
    ; which would skip getattr()'s and hasattr()'s own frames.
    cmp qword [rel attr_error_pending], 0
    je .rna_fresh
    mov qword [rel attr_error_pending], 0
    cmp qword [rel current_exception], 0
    je .rna_fresh
    pop r12
    pop rbx
    leave
    mov [rel eval_saved_r13], r13
    jmp eval_exception_unwind
.rna_fresh:
    mov [rbp - RNA_NAME], rsi
    call value_type
    mov r12, rax

    lea rbx, [rel rtn_buf]
    xor ecx, ecx
    mov byte [rbx], 39                  ; '
    inc rcx
    test r12, r12
    jz .rna_after_type
    mov rsi, [r12 + PyTypeObject.tp_name]
.rna_type:
    movzx eax, byte [rsi]
    test al, al
    jz .rna_after_type
    inc rsi
    cmp rcx, RTN_BUFSZ - 2
    jae .rna_after_type
    mov [rbx + rcx], al
    inc rcx
    jmp .rna_type
.rna_after_type:
    CSTRING rsi, `' object has no attribute '`
.rna_mid:
    movzx eax, byte [rsi]
    test al, al
    jz .rna_name
    inc rsi
    cmp rcx, RTN_BUFSZ - 2
    jae .rna_name
    mov [rbx + rcx], al
    inc rcx
    jmp .rna_mid
.rna_name:
    mov rsi, [rbp - RNA_NAME]
    test rsi, rsi
    jz .rna_close
    mov rax, [rsi + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rax, rdx
    jne .rna_close
    mov rdx, [rsi + PyStrObject.ob_size]
    lea rsi, [rsi + PyStrObject.data]
    xor eax, eax
.rna_name_copy:
    cmp rax, rdx
    jge .rna_close
    cmp rcx, RTN_BUFSZ - 3
    jae .rna_close
    mov r8b, [rsi + rax]
    mov [rbx + rcx], r8b
    inc rcx
    inc rax
    jmp .rna_name_copy
.rna_close:
    mov byte [rbx + rcx], 39            ; '
    inc rcx
    mov byte [rbx + rcx], 0
    lea rdi, [rel exc_AttributeError_type]
    extern exc_AttributeError_type
    mov rsi, rbx
    call raise_exception
    ud2
END_FUNC raise_no_attribute

;; ============================================================================
;; obj_generic_attr(rdi = object Value, rsi = name str) -> Value, or 0
;;
;; The attributes every object has regardless of type.  They used to be
;; nobody's job: each tp_getattr special-cased its own names and there was no
;; shared tail, so `(5).__class__` and `obj.__dict__` were AttributeErrors on
;; every type in the tree.  Called from the miss path of the attribute
;; lookups, so a type that defines one of these itself still wins.
;;
;; Returns a new reference, or 0 when the name is not one of these.
;; ============================================================================
OGA_OBJ   equ 8
OGA_NAME  equ 16
OGA_FRAME equ 32            ; + 1 push = 40, not 16-aligned
DEF_FUNC obj_generic_attr, OGA_FRAME
    push rbx
    mov [rbp - OGA_OBJ], rdi
    mov [rbp - OGA_NAME], rsi

    test rsi, rsi
    jz .oga_none
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .oga_none

    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__class__"
    call ap_strcmp
    test eax, eax
    jz .oga_class

    mov rdi, [rbp - OGA_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "__dict__"
    call ap_strcmp
    test eax, eax
    jz .oga_dict

.oga_none:
    xor eax, eax
    pop rbx
    leave
    ret

.oga_class:
    ; Every value has a type, including the immediates.
    mov rdi, [rbp - OGA_OBJ]
    call value_type
    test rax, rax
    jz .oga_none
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
    pop rbx
    leave
    ret

.oga_dict:
    ; Only an object with a real instance dict has one.  tp_dictoffset is 0
    ; for every static type and for the layouts that cannot host a dict.
    mov rdi, [rbp - OGA_OBJ]
    V_TEST_PTR rdi, rax
    ja .oga_none
    test rdi, rdi
    jz .oga_none
    ; A __slots__ class has none, whatever its tp_dictoffset says.  Its dict
    ; word is still in the layout, but nothing may put a dict there -- and
    ; this arm CREATED one on first read, which is how `__slots__` classes
    ; came to accept arbitrary attributes: asking for o.__dict__ gave them the
    ; dict they were supposed not to have.
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HAS_SLOTS
    jnz .oga_none
    LOAD_INST_DICT rbx, rdi, .oga_none
    test rbx, rbx
    jnz .oga_dict_have
    ; Not created yet: an instance gets its dict on first use, so asking for
    ; it has to create one or the attribute would come and go.
    call dict_new
    mov rbx, rax
    mov rdi, [rbp - OGA_OBJ]
    STORE_INST_DICT rdi, rbx, rcx, .oga_dict_have
.oga_dict_have:
    mov rdi, rbx
    call obj_incref
    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC obj_generic_attr

; obj_richcompare_bool(rdi = left Value, rsi = right Value, edx = op)
;   -> eax = 1 (true), 0 (false), or -1 (an exception is pending)
;
; CPython's PyObject_RichCompareBool, which is what every container search
; uses and what none of them used here.  Nine sites open-coded a comparison
; and treated a NULL result as "not equal" -- but NULL means either
; NotImplemented, in which case the reflected operand and then identity must
; be tried, or that the comparison raised, in which case it must propagate.
; None of them read current_exception, so a raising __eq__ inside `x in list`
; silently answered False.
;
; The identity shortcut comes first, as in CPython: a container holding an
; object finds it even if its __eq__ is broken or raises.
ORB_LEFT  equ 8
ORB_RIGHT equ 16
ORB_OP    equ 24
ORB_EXC   equ 32
ORB_RES   equ 40
ORB_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC obj_richcompare_bool, ORB_FRAME
    mov [rbp - ORB_LEFT], rdi
    mov [rbp - ORB_RIGHT], rsi
    mov [rbp - ORB_OP], rdx

    ; Hold a strong reference to both operands for the duration.  A
    ; comparison can run arbitrary Python: CPython's own
    ; test_count_index_remove_crashes has an __eq__ that clears the very list
    ; being searched, which frees the element the caller handed us as a
    ; borrowed slot reference (bpo-38610).  Doing it here rather than in each
    ; of the six search loops means no loop can forget.
    INCREF_V rdi, rax
    INCREF_V rsi, rax

    ; Identity: for == this is true and for != false, without consulting the
    ; type at all.  One compare, since a Value is one word.
    mov rdi, [rbp - ORB_LEFT]
    cmp rdi, [rbp - ORB_RIGHT]
    jne .orb_compare
    mov edx, [rbp - ORB_OP]
    cmp edx, PY_EQ
    je .orb_true
    cmp edx, PY_NE
    je .orb_false

.orb_compare:
    DUNDER_EXC_SAVE [rbp - ORB_EXC]

    ; Left operand's tp_richcompare.
    mov rdi, [rbp - ORB_LEFT]
    call value_type
    test rax, rax
    jz .orb_identity
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .orb_reflected
    mov rdi, [rbp - ORB_LEFT]
    mov rsi, [rbp - ORB_RIGHT]
    mov edx, [rbp - ORB_OP]
    call rax
    test rax, rax
    jnz .orb_have_result
    DUNDER_RAISED [rbp - ORB_EXC], .orb_error

.orb_reflected:
    ; NotImplemented from the left: try the right operand with the op
    ; reversed, which is how a subclass or a mixed-type comparison gets its
    ; say.
    mov rdi, [rbp - ORB_RIGHT]
    call value_type
    test rax, rax
    jz .orb_identity
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .orb_identity
    mov rdi, [rbp - ORB_RIGHT]
    mov rsi, [rbp - ORB_LEFT]
    mov edx, [rbp - ORB_OP]
    lea rcx, [rel orb_swap_table]
    movsxd rdx, edx
    mov edx, [rcx + rdx*4]      ; the reversed op
    call rax
    test rax, rax
    jnz .orb_have_result
    DUNDER_RAISED [rbp - ORB_EXC], .orb_error

.orb_identity:
    ; Neither side had an opinion.  Equality falls back to identity, which
    ; the fast path above already ruled out, so the answer is fixed.
    mov edx, [rbp - ORB_OP]
    cmp edx, PY_EQ
    je .orb_false
    cmp edx, PY_NE
    je .orb_true
    ; Name the operator the caller actually asked for.  A flat "unorderable
    ; types" here read differently from the identical failure raised by
    ; COMPARE_OP and by list.sort, and min()/max() go through this one.
    lea rax, [rel orb_unorderable_msgs]
    movsxd rdx, edx
    mov rsi, [rax + rdx*8]
    lea rdi, [rel exc_TypeError_type]
    ; set_exception, not raise_exception: this function holds a reference to
    ; both operands, and an unwind from here abandons the C stack and leaks
    ; them.  -1 is what the contract above already promises.
    extern set_exception
    call set_exception
    jmp .orb_error

.orb_have_result:
    mov [rbp - ORB_RES], rax    ; the result Value, owned
    mov rdi, rax
    call obj_is_true
    mov [rbp - ORB_OP], rax     ; the op is finished with; reuse the slot
    mov rdi, [rbp - ORB_RES]
    DECREF_V rdi, rdx
    mov rax, [rbp - ORB_OP]
    jmp .orb_done

.orb_true:
    mov eax, 1
    jmp .orb_done

.orb_false:
    xor eax, eax
    jmp .orb_done

.orb_error:
    mov eax, -1

.orb_done:
    mov [rbp - ORB_RES], rax
    mov rdi, [rbp - ORB_LEFT]
    DECREF_V rdi, rdx
    mov rdi, [rbp - ORB_RIGHT]
    DECREF_V rdi, rdx
    mov rax, [rbp - ORB_RES]
    leave
    ret
END_FUNC obj_richcompare_bool

; obj_binary_op(rdi = left Value, rsi = right Value, edx = op index, 0..12)
;   -> rax = result Value, or 0 with an exception pending
;
; CPython's PyNumber_Add and its siblings, made callable.  The whole protocol
; lived inside op_binary_op, which pops from r13 and leaves through DISPATCH,
; so no builtin could reach it: sum() hardcoded int_add/float_add and
; min()/max() hardcoded a type ladder.  Both then read a declining slot's NULL
; Value as the answer, and a NULL on the value stack surfaces as a failure in
; whatever runs next -- sum([1j, 2j]) reported "build_string expects str".
;
; The order is binary_op1's: the left type's slot, the right type's same slot,
; then the sequence fallback, then the dunder pair on a heaptype, then
; TypeError.  Only the non-inplace half, 0..12; nothing that reduces a
; sequence needs the other one.
OBO_LEFT  equ 8
OBO_RIGHT equ 16
OBO_OP    equ 24
OBO_OFF   equ 32
OBO_EXC   equ 40
OBO_FRAME equ 48            ; + 0 pushes = 48

extern binary_op_offsets
extern binop_dunder_table
extern binop_rdunder_table
extern dunder_call_2

DEF_FUNC obj_binary_op, OBO_FRAME
    mov [rbp - OBO_LEFT], rdi
    mov [rbp - OBO_RIGHT], rsi
    movsxd rdx, edx
    mov [rbp - OBO_OP], rdx
    lea rax, [rel binary_op_offsets]
    mov rax, [rax + rdx*8]
    mov [rbp - OBO_OFF], rax    ; the nb_* offset both slot tries use

    ; Hold a strong reference to both operands for the duration, as
    ; obj_richcompare_bool does and for the same reason: a slot or a dunder
    ; runs arbitrary Python, and the caller's operands are usually borrowed
    ; slots in an array that call can reach.
    INCREF_V rdi, rax
    INCREF_V rsi, rax

    DUNDER_EXC_SAVE [rbp - OBO_EXC]

    ; --- the left type's slot ---
    mov rdi, [rbp - OBO_LEFT]
    call value_type
    test rax, rax
    jz .obo_right_slot
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .obo_right_slot
    mov rcx, [rbp - OBO_OFF]
    mov rax, [rax + rcx]
    test rax, rax
    jz .obo_right_slot
    mov rdi, [rbp - OBO_LEFT]
    mov rsi, [rbp - OBO_RIGHT]
    call rax
    test rax, rax
    jnz .obo_done               ; a non-NULL Value is the answer
    DUNDER_RAISED [rbp - OBO_EXC], .obo_error

.obo_right_slot:
    ; The left slot declined.  The right type gets the same slot with the
    ; operands still in their original order -- the only route by which a
    ; numeric type the left side has never heard of can answer.
    mov rdi, [rbp - OBO_RIGHT]
    call value_type
    test rax, rax
    jz .obo_seq
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .obo_seq
    mov rcx, [rbp - OBO_OFF]
    mov rax, [rax + rcx]
    test rax, rax
    jz .obo_seq
    mov rdi, [rbp - OBO_LEFT]
    mov rsi, [rbp - OBO_RIGHT]
    call rax
    test rax, rax
    jnz .obo_done
    DUNDER_RAISED [rbp - OBO_EXC], .obo_error

.obo_seq:
    ; sq_concat for +, sq_repeat for *, off the left operand -- what makes
    ; sum(list_of_lists, []) work.
    mov rcx, [rbp - OBO_OP]
    cmp rcx, 0                  ; NB_ADD
    je .obo_seq_have_op
    cmp rcx, 5                  ; NB_MULTIPLY
    jne .obo_dunder
.obo_seq_have_op:
    mov rdi, [rbp - OBO_LEFT]
    call value_type
    test rax, rax
    jz .obo_dunder
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .obo_dunder
    mov rcx, [rbp - OBO_OP]
    test rcx, rcx
    jnz .obo_seq_repeat
    mov rax, [rax + PySequenceMethods.sq_concat]
    jmp .obo_seq_call
.obo_seq_repeat:
    mov rax, [rax + PySequenceMethods.sq_repeat]
.obo_seq_call:
    test rax, rax
    jz .obo_dunder
    mov rdi, [rbp - OBO_LEFT]
    mov rsi, [rbp - OBO_RIGHT]
    call rax
    test rax, rax
    jnz .obo_done
    DUNDER_RAISED [rbp - OBO_EXC], .obo_error

.obo_dunder:
    ; __add__ on the left, then __radd__ on the right.  A heaptype's binary
    ; dunders have no nb_* slot of their own -- slots.asm installs only the
    ; unary ones -- so this arm, not the two above, is what serves a user
    ; class.  The tag argument is TAG_PTR because V_PACK leaves a Value
    ; alone under it, which is what the operands already are.
    mov rdi, [rbp - OBO_LEFT]
    V_TEST_PTR rdi, rax
    ja .obo_rdunder          ; an immediate has no dunders
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .obo_rdunder
    mov rcx, [rbp - OBO_OP]
    lea rdx, [rel binop_dunder_table]
    mov rdx, [rdx + rcx*8]
    test rdx, rdx
    jz .obo_rdunder
    mov rsi, [rbp - OBO_RIGHT]
    mov ecx, TAG_PTR
    call dunder_call_2
    test rax, rax
    jnz .obo_done
    DUNDER_RAISED [rbp - OBO_EXC], .obo_error

.obo_rdunder:
    mov rdi, [rbp - OBO_RIGHT]
    V_TEST_PTR rdi, rax
    ja .obo_unsupported          ; an immediate has no dunders
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .obo_unsupported
    mov rcx, [rbp - OBO_OP]
    lea rdx, [rel binop_rdunder_table]
    mov rdx, [rdx + rcx*8]
    test rdx, rdx
    jz .obo_unsupported
    mov rsi, [rbp - OBO_LEFT]   ; reflected: the right operand is self
    mov ecx, TAG_PTR
    call dunder_call_2
    test rax, rax
    jnz .obo_done
    DUNDER_RAISED [rbp - OBO_EXC], .obo_error

.obo_unsupported:
    ; SET_EXC, not RAISE: .obo_done below still has to release both operands,
    ; and an unwind from here would never reach it.
    SET_EXC exc_TypeError_type, "unsupported operand type(s)"
    jmp .obo_error

.obo_error:
    xor eax, eax

.obo_done:
    mov [rbp - OBO_OP], rax     ; the op is finished with; reuse the slot
    mov rdi, [rbp - OBO_LEFT]
    DECREF_V rdi, rdx
    mov rdi, [rbp - OBO_RIGHT]
    DECREF_V rdi, rdx
    mov rax, [rbp - OBO_OP]
    leave
    ret
END_FUNC obj_binary_op

section .rodata
align 8
orb_unorderable_msgs:
    dq orb_msg_lt, orb_msg_le, orb_msg_eq, orb_msg_eq, orb_msg_gt, orb_msg_ge
orb_msg_lt: db "'<' not supported between instances", 0
orb_msg_le: db "'<=' not supported between instances", 0
orb_msg_gt: db "'>' not supported between instances", 0
orb_msg_ge: db "'>=' not supported between instances", 0
; == and != never reach the raise -- both fall back to identity above -- but
; the table is indexed by the op, so the two slots have to hold something.
orb_msg_eq: db "unorderable types", 0

align 4
orb_swap_table:
    dd PY_GT                    ; PY_LT reversed
    dd PY_GE                    ; PY_LE
    dd PY_EQ                    ; PY_EQ
    dd PY_NE                    ; PY_NE
    dd PY_LT                    ; PY_GT
    dd PY_LE                    ; PY_GE
section .text

; hash_not_implemented() -> never returns
; Used as tp_hash for unhashable types (dict, list, set).
; Raises TypeError("unhashable type").
DEF_FUNC hash_not_implemented
    extern raise_exception
    extern exc_TypeError_type
    RAISE exc_TypeError_type, "unhashable type"
END_FUNC hash_not_implemented

; obj_hash(rdi=value) -> int64
; Decodes the Value, then dispatches: int immediate → int_hash_i64, pointer → tp_hash.
DEF_FUNC obj_hash
    V_UNPACK rdi, rsi

    cmp esi, TAG_SMALLINT
    je .smallint_hash
    cmp esi, TAG_FLOAT
    je .float_hash

    ; TAG_PTR path
    test rdi, rdi
    jz .default_hash

    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .default_hash
    mov rax, [rax + PyTypeObject.tp_hash]
    test rax, rax
    jz .default_hash

    ; tail-call tp_hash(rdi=obj, edx=tag)
    ; tp_hash implementations (int_hash) forward edx to int_unwrap, so the
    ; tag MUST be supplied here -- leaving edx undefined makes int_unwrap
    ; take a random branch and int_hash return the object address.
    mov edx, esi
    leave
    jmp rax

.smallint_hash:
    ; Shared with int_hash / builtin_hash: sign(v) * (|v| mod 2^61-1).
    ; All three must agree or dict and set lookups silently break.
    extern int_hash_i64
    leave
    jmp int_hash_i64

.float_hash:
    ; Inline float: call float_hash for PEP-correct integer-float matching
    extern float_hash
    mov edx, TAG_FLOAT
    call float_hash
    leave
    ret

.bool_hash:
    ; Hash of bool: 0 for False, 1 for True (matches Python int hash)
    mov rax, rdi
    leave
    ret

.none_hash:
    ; Hash of None: constant (avoids -1 which is reserved error value)
    mov eax, 0x48ae2ce5
    leave
    ret

.default_hash:
    ; Default: hash is the object address
    mov rax, rdi
    leave
    ret
END_FUNC obj_hash

; obj_is_true(rdi=value) -> int (0 or 1)
; Decodes the Value, then dispatches: int immediate → value != 0, pointer → type-based.
DEF_FUNC_BARE obj_is_true
    V_UNPACK rdi, rsi

    cmp esi, TAG_SMALLINT
    je .smallint
    cmp esi, TAG_FLOAT
    je .float_tag

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; None is false (legacy — TAG_PTR none_singleton)
    lea rax, [rel none_singleton]
    cmp rbx, rax
    je .false

    ; bool False is false (legacy — TAG_PTR bool_false)
    lea rax, [rel bool_false]
    cmp rbx, rax
    je .false

    ; Check for nb_bool in type's number methods
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .true
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .check_seq_len
    mov rax, [rax + PyNumberMethods.nb_bool]
    test rax, rax
    jz .check_seq_len
    mov rdi, rbx
    ; nb_bool still takes (payload, tag): int_bool hands the pair straight to
    ; int_unwrap, and without the tag it read whatever the caller had left in
    ; edx.  When that happened to be TAG_SMALLINT it tested the POINTER,
    ; which is never zero -- so bool() of a heap-boxed 0 was True, while
    ; `not x` and `if x:` were right, because they go elsewhere.
    mov edx, TAG_PTR
    call rax
    pop rbx
    pop rbp
    ret

.check_seq_len:
    ; Check for sq_length in type's sequence methods
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_sequence]
    test rax, rax
    jz .check_map_len
    mov rax, [rax + PySequenceMethods.sq_length]
    test rax, rax
    jz .check_map_len
    mov rdi, rbx
    call rax
    test rax, rax
    jnz .true
    jmp .false

.check_map_len:
    ; Check for mp_length in type's mapping methods
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_mapping]
    test rax, rax
    jz .check_dunder_bool
    mov rax, [rax + PyMappingMethods.mp_length]
    test rax, rax
    jz .check_dunder_bool
    mov rdi, rbx
    call rax
    test rax, rax
    jnz .true
    jmp .false

.check_dunder_bool:
    ; Try __bool__ dunder on heaptype
    mov rax, [rbx + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .true                ; default: objects are truthy

    ; Look up __bool__ in type dict to check for None
    extern dunder_bool
    extern dunder_lookup
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel dunder_bool]
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .check_dunder_len       ; not found (TAG_NULL) → try __len__

    ; Check if __bool__ is None → TypeError
    ; Handle both inline (0, TAG_NONE) and pointer (none_singleton, TAG_PTR) forms
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .dunder_bool_none_error

    ; Call __bool__ via dunder_call_1
    extern dunder_call_1
    mov rdi, rbx
    lea rsi, [rel dunder_bool]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx              ; TAG_NULL = call failed
    jz .check_dunder_len

    ; __bool__ returned a result — must be bool
    ; Check TAG_PTR pointing to bool_type
    cmp edx, TAG_PTR
    jne .dunder_bool_type_error
    test rax, rax
    jz .dunder_bool_type_error
    mov rcx, [rax + PyObject.ob_type]
    extern bool_type
    lea r8, [rel bool_type]
    cmp rcx, r8
    jne .dunder_bool_type_error
    ; TAG_PTR bool singleton: convert to 0/1
    lea rcx, [rel bool_true]
    cmp rax, rcx
    sete al
    movzx eax, al
    pop rbx
    pop rbp
    ret

.dunder_bool_is_bool:
    ; Result is TAG_BOOL: rax payload is 0 or 1
    pop rbx
    pop rbp
    ret

.dunder_bool_none_error:
    extern raise_exception
    extern exc_TypeError_type
    RAISE exc_TypeError_type, "cannot interpret 'NoneType' object as an integer"

.dunder_bool_type_error:
    ; __bool__ didn't return bool — DECREF result and raise TypeError
    ; rax=payload, edx=tag from dunder_call_1
    mov rdi, rax
    mov esi, edx
    DECREF_VAL rdi, rsi
    RAISE exc_TypeError_type, "__bool__ should return bool, returned non-bool"

.check_dunder_len:
    ; Try __len__ dunder
    extern dunder_len
    mov rdi, rbx
    lea rsi, [rel dunder_len]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx              ; TAG_NULL = not found
    jz .true                ; no __len__ → truthy by default

    ; __len__ returned a result — check for negative
    cmp edx, TAG_SMALLINT
    jne .len_check_ptr
    ; SmallInt: check if negative
    test rax, rax
    js .len_negative_error
    ; Non-negative SmallInt: truthy if != 0
    test rax, rax
    setnz al
    movzx eax, al
    pop rbx
    pop rbp
    ret

.len_check_ptr:
    ; Non-SmallInt result: use obj_is_true
    push rdx                   ; save tag
    push rax                   ; save payload
    mov rdi, rax
    mov rsi, rdx
    V_PACK rdi, rsi
    call obj_is_true
    mov ecx, eax
    pop rdi                    ; payload
    pop rsi                    ; tag
    DECREF_VAL rdi, rsi
    mov eax, ecx
    pop rbx
    pop rbp
    ret

.len_negative_error:
    extern exc_ValueError_type
    RAISE exc_ValueError_type, "__len__() should return >= 0"

.false:
    xor eax, eax
    pop rbx
    pop rbp
    ret

.true:
    mov eax, 1
    pop rbx
    pop rbp
    ret

.smallint:
    ; SmallInt is true iff raw value != 0
    test rdi, rdi
    setnz al
    movzx eax, al
    ret

.float_tag:
    ; Inline float: true iff not 0.0 and not -0.0
    movq xmm0, rdi
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    setne al
    setp cl                    ; NaN is truthy
    or al, cl
    movzx eax, al
    ret

.bool_tag:
    ; TAG_BOOL: payload = 0 (False) or 1 (True)
    mov eax, edi
    and eax, 1
    ret

.none_tag:
    ; TAG_NONE: always false
    xor eax, eax
    ret
END_FUNC obj_is_true


;; ============================================================================
;; type_repr(PyObject *type_obj) -> PyStrObject*
;; Formats "<class 'name'>" for a type object.
;; ============================================================================
TR_TYPE  equ 8
TR_LEN   equ 16
TR_BUF   equ 272            ; 256 bytes, [rbp-272, rbp-16)
TR_FRAME equ 288            ; + 2 pushes = 304
DEF_FUNC type_repr, TR_FRAME
    push rbx
    push r12
    mov [rbp - TR_TYPE], rdi

    mov rax, [rdi + PyTypeObject.tp_name]  ; C string pointer
    test rax, rax
    jz .type_repr_unknown

    lea rbx, [rbp - TR_BUF]
    CSTRING rsi, `<class '`
    xor r12d, r12d
.tr_open:
    movzx eax, byte [rsi]
    test al, al
    jz .tr_module
    inc rsi
    mov [rbx + r12], al
    inc r12
    jmp .tr_open

.tr_module:
    ; CPython qualifies a class with its module: <class '__main__.C'>.  Only
    ; the bare name was printed, so every class repr differed from CPython's.
    ; Builtins live in "builtins" and are shown unqualified.
    mov rdi, [rbp - TR_TYPE]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tr_name
    mov [rbp - TR_LEN], rdi         ; the type dict
    CSTRING rdi, "__module__"
    call str_from_cstr
    mov rsi, rax
    mov rdi, [rbp - TR_LEN]
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .tr_name
    cmp edx, TAG_PTR
    jne .tr_name
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .tr_name
    mov rdx, [rax + PyStrObject.ob_size]
    test rdx, rdx
    jz .tr_name
    cmp rdx, 8
    jne .tr_copy_module
    lea rdi, [rax + PyStrObject.data]
    CSTRING rsi, "builtins"
    push rax
    push rdx
    call ap_strcmp
    pop rdx
    pop rax
    test eax, eax
    jz .tr_name                 ; module is "builtins": leave it off
.tr_copy_module:
    lea rsi, [rax + PyStrObject.data]
    xor ecx, ecx
.tr_mod_loop:
    cmp rcx, rdx
    jge .tr_mod_done
    cmp r12, 200
    jae .tr_mod_done
    mov al, [rsi + rcx]
    mov [rbx + r12], al
    inc r12
    inc rcx
    jmp .tr_mod_loop
.tr_mod_done:
    mov byte [rbx + r12], '.'
    inc r12

.tr_name:
    mov rax, [rbp - TR_TYPE]
    mov rsi, [rax + PyTypeObject.tp_name]
.tr_name_loop:
    movzx eax, byte [rsi]
    test al, al
    jz .tr_close
    inc rsi
    cmp r12, 250
    jae .tr_close
    mov [rbx + r12], al
    inc r12
    jmp .tr_name_loop

.tr_close:
    mov byte [rbx + r12], 0x27
    mov byte [rbx + r12 + 1], '>'
    mov byte [rbx + r12 + 2], 0
    mov rdi, rbx
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret

.type_repr_unknown:
    lea rdi, [rel type_repr_unknown_str]
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_repr

section .rodata
align 8
extern union_type_or
global type_number_methods
type_number_methods:
    times 15 dq 0
    dq union_type_or          ; nb_or (+120): `int | str` builds a UnionType
    times 20 dq 0

section .rodata
obj_print_newline: db 10
obj_print_null_str: db "<NULL>", 10
type_repr_unknown_str: db "<class '?'>", 0
type_type_name: db "type", 0

section .data
align 8
global type_type
type_type:
    dq 1                      ; ob_refcnt (immortal)
    dq type_type              ; ob_type (self-referential)
    dq type_type_name         ; tp_name
    dq TYPE_OBJECT_SIZE       ; tp_basicsize
    dq 0                      ; tp_dealloc
    dq type_repr              ; tp_repr
    dq type_repr              ; tp_str
    dq 0                      ; tp_hash
    dq type_call              ; tp_call — calling a type creates instances
    dq type_getattr           ; tp_getattr — __name__, tp_dict lookups
    dq type_setattr           ; tp_setattr
    dq 0                      ; tp_richcompare
    dq 0                      ; tp_iter
    dq 0                      ; tp_iternext
    dq 0                      ; tp_init
    dq 0                      ; tp_new
    dq type_number_methods    ; tp_as_number -- PEP 604: int | str
    dq 0                      ; tp_as_sequence
    dq 0                      ; tp_as_mapping
    dq 0                      ; tp_base
    dq 0                      ; tp_dict
    dq 0                      ; tp_mro
    dq TYPE_FLAG_METATYPE     ; tp_flags — instances of `type` are classes
    dq 0                      ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
