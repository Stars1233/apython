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

; Where a slot lives: directly in PyTypeObject, or in one of the three
; method tables it points at.  A table is allocated for a type only when it
; defines at least one dunder that belongs in it.
SLOT_DIRECT   equ 0
SLOT_NUMBER   equ 1
SLOT_SEQUENCE equ 2
SLOT_MAPPING  equ 3

; One row of the dunder-to-slot table.
struc SlotEntry
    .name:    resq 1        ; dunder name, a C string
    .kind:    resq 1        ; SLOT_DIRECT or which method table
    .offset:  resq 1        ; byte offset within PyTypeObject or that table
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
extern obj_as_index
extern ap_malloc
extern raise_exception
extern exc_TypeError_type

section .text

;; ============================================================================
;; slot_ensure_table(rdi = type, esi = kind) -> rax = the method table
;;
;; Allocate the method table on first use and hang it off the type.  A
;; heaptype starts with all three pointers NULL, which is exactly what
;; "implements no numeric/sequence/mapping protocol" means, so they must stay
;; NULL unless the class actually defines something.
;; ============================================================================
DEF_FUNC_LOCAL slot_ensure_table
    push rbx
    push r12
    push r13
    push r14
    mov rbx, rdi

    cmp esi, SLOT_NUMBER
    je .set_number
    cmp esi, SLOT_SEQUENCE
    je .set_sequence
    mov r13, PyTypeObject.tp_as_mapping
    mov r14, PyMappingMethods_size
    jmp .go
.set_number:
    mov r13, PyTypeObject.tp_as_number
    mov r14, PyNumberMethods_size
    jmp .go
.set_sequence:
    mov r13, PyTypeObject.tp_as_sequence
    mov r14, PySequenceMethods_size

.go:
    mov rax, [rbx + r13]
    test rax, rax
    jz .fresh

    ; The table may be an *ancestor's*: __build_class__ inherits the protocol
    ; slots of a builtin base by copying the pointer.  Writing a wrapper
    ; through it patched the builtin's own static table, so one `class
    ; MyInt(int)` with a __neg__ gave every int in the process that __neg__.
    ;
    ; The question has to be asked of the whole MRO, not just tp_base.  With
    ; multiple inheritance the table comes from whichever base supplied the
    ; layout, which need not be tp_base: `class IntFlag(int, ReprEnum, Flag)`
    ; inherits int's PyNumberMethods while its tp_base is elsewhere, so a
    ; tp_base-only test found no sharing and installed Flag's __invert__ into
    ; int's own static table.  Every plain `~n` in the process then went to
    ; slot_nb_invert, which dereferences its operand -- and an int immediate
    ; is not a pointer.  Importing enum and defining any IntFlag was enough.
    mov r12, rax                    ; the candidate table
    mov rcx, rbx                    ; MRO walker, starting at this type
.share_scan:
    MRO_NEXT rcx, rbx               ; clobbers rax; r12 holds the table
    test rcx, rcx
    jz .not_shared                  ; nothing above shares it: already ours
    cmp r12, [rcx + r13]
    jne .share_scan
    jmp .copy_shared

.not_shared:
    mov rax, r12                    ; type_mro_next returned 0 into rax
    jmp .have

.copy_shared:                       ; shared with an ancestor: copy first
    mov rdi, r14
    call .alloc_zeroed
    push rax
    mov rdi, rax
    mov rsi, r12
    mov rdx, r14
    extern ap_memcpy
    call ap_memcpy
    pop rax
    mov [rbx + r13], rax
    jmp .have

.fresh:
    mov rdi, r14
    call .alloc_zeroed
    mov [rbx + r13], rax

.have:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.alloc_zeroed:
    push rdi
    call ap_malloc
    pop rcx
    push rax
    mov rdi, rax
    shr rcx, 3
    xor eax, eax
    rep stosq
    pop rax
    ret
END_FUNC slot_ensure_table

;; ============================================================================
;; slot_tp_hash(rdi = self, edx = tag) -> rax = i64 hash
;; ============================================================================
DEF_FUNC slot_tp_hash
    lea rsi, [rel sl_hash_name]
    call dunder_call_1
    V_UNPACK rax, rdx
    test edx, edx
    jz .failed
    ; __hash__ must return an int; obj_as_index raises otherwise.
    push rax
    push rdx
    mov rdi, rax
    call obj_as_index
    add rsp, 16
    leave
    ret
.failed:
    call slot_reraise
END_FUNC slot_tp_hash

;; ============================================================================
;; Unary numeric slots.  Each is called as nb_xxx(rdi = operand Value) and
;; returns a Value.  Before this, -obj and ~obj on a user class dereferenced
;; a NULL tp_as_number, and +obj and abs(obj) were simply ignored.
;; ============================================================================
%macro DEF_UNARY_SLOT 2         ; %1 = function name, %2 = dunder name symbol
DEF_FUNC %1
    lea rsi, [rel %2]
    call dunder_call_1
    V_UNPACK rax, rdx
    test edx, edx
    jz %%failed
    V_PACK rax, rdx
    leave
    ret
%%failed:
    call slot_reraise
END_FUNC %1
%endmacro

;; ============================================================================
;; slot_nb_bool(rdi = self) -> eax = 0 or 1
;;
;; obj_is_true consults nb_bool, then sq_length, then mp_length, and only then
;; the __bool__ dunder -- so once __len__ reached a length slot it shadowed
;; __bool__, which is the wrong priority.  Installing nb_bool puts it back.
;; ============================================================================
DEF_FUNC slot_nb_bool
    lea rsi, [rel sl_bool_name]
    call dunder_call_1
    V_UNPACK rax, rdx
    test edx, edx
    jz .failed

    extern bool_true
    extern bool_false
    push rax
    push rdx
    cmp edx, TAG_PTR
    jne .not_bool
    lea rcx, [rel bool_true]
    cmp rax, rcx
    je .is_true
    lea rcx, [rel bool_false]
    cmp rax, rcx
    jne .not_bool
    pop rdx
    pop rdi
    call obj_decref
    xor eax, eax
    leave
    ret
.is_true:
    pop rdx
    pop rdi
    call obj_decref
    mov eax, 1
    leave
    ret
.not_bool:
    add rsp, 16
    RAISE exc_TypeError_type, "__bool__ should return bool"
.failed:
    call slot_reraise
END_FUNC slot_nb_bool

;; ============================================================================
;; slot_nb_index / slot_nb_int / slot_nb_float -- conversion protocols.
;; ============================================================================
DEF_UNARY_SLOT slot_nb_index, sl_index_name
DEF_UNARY_SLOT slot_nb_int,   sl_int_name
DEF_UNARY_SLOT slot_nb_float, sl_float_name

;; ============================================================================
;; slot_tp_richcompare(rdi = left Value, rsi = right Value, edx = op) -> Value
;;
;; NULL means NotImplemented, which is what every caller of tp_richcompare
;; already expects.  Installed when the class defines any of the six
;; comparison dunders, so dict and set key lookup -- which consult
;; tp_richcompare -- start seeing a user class's __eq__.
;; ============================================================================
DEF_FUNC slot_tp_richcompare
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi

    ; op -> dunder name
    extern cmp_dunder_table
    lea rax, [rel cmp_dunder_table]
    movsxd rcx, edx
    mov rdx, [rax + rcx*8]      ; the dunder name

    ; dunder_call_2 takes `other` as a payload plus its tag, not a Value.
    mov rdi, rbx
    mov rsi, r12
    V_UNPACK rsi, rcx
    extern dunder_call_3
    extern obj_dealloc
    extern dunder_call_2
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jz .rc_notimplemented

    ; A dunder answering NotImplemented is reported the same way a missing
    ; one is: NULL, so the caller tries the reflected operand.
    extern notimpl_singleton
    lea rcx, [rel notimpl_singleton]
    cmp rax, rcx
    je .rc_drop_notimpl

    V_PACK rax, rdx
    pop r12
    pop rbx
    leave
    ret

.rc_drop_notimpl:
    mov rdi, rax
    call obj_decref
.rc_notimplemented:
    RET_NULL
    pop r12
    pop rbx
    leave
    ret
END_FUNC slot_tp_richcompare

DEF_UNARY_SLOT slot_nb_negative, sl_neg_name
DEF_UNARY_SLOT slot_nb_positive, sl_pos_name
DEF_UNARY_SLOT slot_nb_invert,   sl_invert_name
DEF_UNARY_SLOT slot_nb_absolute, sl_abs_name

;; ============================================================================
;; slot_length(rdi = self) -> rax = i64
;;
;; Serves both mp_length and sq_length; builtin_len tries mapping first, and
;; GET_LEN in a match statement tries sequence first.
;; ============================================================================
DEF_FUNC slot_length
    lea rsi, [rel sl_len_name]
    call dunder_call_1
    V_UNPACK rax, rdx
    test edx, edx
    jz .failed
    push rax
    push rdx
    mov rdi, rax
    call obj_as_index
    add rsp, 16
    test rax, rax
    js .negative
    leave
    ret
.negative:
    extern exc_ValueError_type
    RAISE exc_ValueError_type, "__len__() should return >= 0"
.failed:
    call slot_reraise
END_FUNC slot_length

;; ============================================================================
;; slot_mp_subscript(rdi = self, rsi = key Value) -> Value
;; slot_mp_ass_subscript(rdi = self, rsi = key Value, rdx = value Value)
;;
;; type_from_parts hands a builtin subclass its base's method table by pointer,
;; so a dict subclass that defines __setitem__ inherits dict's slot and the
;; Python method is never reached: `d["a"] = 1` went straight into dict's
;; storage.  Installing these wrappers is what makes the override take effect
;; -- collections.OrderedDict and enum's _EnumDict are both built on it.
;;
;; A NULL value Value means deletion, which is __delitem__, the same convention
;; dict_ass_subscript uses.
;; ============================================================================
DEF_FUNC slot_mp_subscript
    mov rdx, rsi
    V_UNPACK rdx, rcx
    mov rsi, rdx
    lea rdx, [rel sl_getitem_name]
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jz .failed
    leave
    V_PACK rax, rdx
    ret
.failed:
    call slot_reraise           ; does not return
END_FUNC slot_mp_subscript

SAS_SELF  equ 8
SAS_KEY   equ 16
SAS_FRAME equ 24            ; + 1 push = 32
DEF_FUNC slot_mp_ass_subscript, SAS_FRAME
    push rbx
    mov [rbp - SAS_SELF], rdi
    mov [rbp - SAS_KEY], rsi
    mov rbx, rdx
    test rbx, rbx
    jz .delete

    ; __setitem__(self, key, value)
    mov rsi, [rbp - SAS_KEY]
    mov rdx, rbx
    lea rcx, [rel sl_setitem_name]
    mov r8d, TAG_PTR                    ; dunder_call_3 packs arg2 with this
    V_UNPACK rdx, r8
    call dunder_call_3
    V_UNPACK rax, rdx
    test edx, edx
    jz .failed
    mov rdi, rax
    DECREF_V rdi, rsi                   ; __setitem__ returns None
    xor eax, eax
    pop rbx
    leave
    ret

.delete:
    mov rdi, [rbp - SAS_SELF]
    mov rsi, [rbp - SAS_KEY]
    V_UNPACK rsi, rcx
    lea rdx, [rel sl_delitem_name]
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jz .failed
    mov rdi, rax
    DECREF_V rdi, rsi
    xor eax, eax
    pop rbx
    leave
    ret
.failed:
    call slot_reraise           ; does not return
END_FUNC slot_mp_ass_subscript

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
    RAISE exc_RuntimeError_type, "slot wrapper failed without an exception"
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
;; slot_is_object_default(rdi = the value a dunder lookup returned) -> eax 0/1
;; True when it is one of the implementations object itself supplies.
;; ============================================================================
DEF_FUNC_LOCAL slot_is_object_default
    V_TEST_PTR rdi, rax
    ja .no
    test rdi, rdi
    jz .no
    mov rax, [rdi + PyObject.ob_type]
    extern builtin_func_type
    lea rcx, [rel builtin_func_type]
    cmp rax, rcx
    jne .no
    mov rax, [rdi + PyBuiltinObject.func_ptr]
    lea rcx, [rel object_default_impls]
    xor edx, edx
.scan:
    mov rsi, [rcx + rdx*8]
    test rsi, rsi
    jz .no
    cmp rax, rsi
    je .yes
    inc rdx
    jmp .scan
.yes:
    mov eax, 1
    leave
    ret
.no:
    xor eax, eax
    leave
    ret
END_FUNC slot_is_object_default

;; ============================================================================
;; type_install_slots(rdi = heaptype)
;;
;; Fill the type's slots from the dunders it defines.  Called once at class
;; creation, and again by type_setattr when a dunder is assigned afterwards,
;; so `C.__iter__ = f` takes effect the way it does in CPython.
;; ============================================================================
TIS_TYPE  equ 8
TIS_ENTRY equ 16
TIS_FOUND equ 24
TIS_FRAME equ 32            ; + 2 pushes = 48

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
    mov [rbp - TIS_FOUND], rax
    ; A dunder explicitly set to None disables the protocol in Python, so
    ; leave the slot empty rather than installing a wrapper that would call
    ; None.
    extern none_singleton
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .skip
    ; object's own defaults are not a definition.  They live in
    ; object_type.tp_dict so that `MutableMapping.__ne__` and friends can be
    ; bound by name, but a builtin subclass that inherits one must keep the
    ; base type's C-level slot: installing a wrapper here would make
    ; `T((1,)) == (1,)` on a tuple subclass go through object's identity test
    ; instead of tuple's comparison.
    mov rdi, rax
    call slot_is_object_default
    mov rbx, [rbp - TIS_ENTRY]
    test eax, eax
    jnz .skip
    mov rax, [rbp - TIS_FOUND]

    mov rcx, [rbp - TIS_TYPE]
    mov rsi, [rbx + SlotEntry.kind]
    test rsi, rsi
    jnz .indirect
    mov rax, [rbx + SlotEntry.offset]
    mov rdx, [rbx + SlotEntry.wrapper]
    mov [rcx + rax], rdx
    jmp .skip

.indirect:
    mov rdi, rcx
    call slot_ensure_table      ; rax = the method table
    mov rbx, [rbp - TIS_ENTRY]
    mov rcx, [rbx + SlotEntry.offset]
    mov rdx, [rbx + SlotEntry.wrapper]
    mov [rax + rcx], rdx

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

sl_iter_name:   db "__iter__", 0
sl_next_name:   db "__next__", 0
sl_hash_name:   db "__hash__", 0
sl_neg_name:    db "__neg__", 0
sl_pos_name:    db "__pos__", 0
sl_invert_name: db "__invert__", 0
sl_abs_name:    db "__abs__", 0
sl_len_name:    db "__len__", 0
sl_bool_name:   db "__bool__", 0
sl_index_name:  db "__index__", 0
sl_int_name:    db "__int__", 0
sl_float_name:  db "__float__", 0
sl_eq_name:     db "__eq__", 0
sl_ne_name:     db "__ne__", 0
sl_lt_name:     db "__lt__", 0
sl_le_name:     db "__le__", 0
sl_gt_name:     db "__gt__", 0
sl_ge_name:     db "__ge__", 0
sl_getitem_name: db "__getitem__", 0
sl_setitem_name: db "__setitem__", 0
sl_delitem_name: db "__delitem__", 0

align 8
slot_table:
    dq sl_iter_name,   SLOT_DIRECT,   PyTypeObject.tp_iter,     slot_tp_iter
    dq sl_next_name,   SLOT_DIRECT,   PyTypeObject.tp_iternext, slot_tp_iternext
    dq sl_hash_name,   SLOT_DIRECT,   PyTypeObject.tp_hash,     slot_tp_hash
    dq sl_neg_name,    SLOT_NUMBER,   PyNumberMethods.nb_negative, slot_nb_negative
    dq sl_pos_name,    SLOT_NUMBER,   PyNumberMethods.nb_positive, slot_nb_positive
    dq sl_invert_name, SLOT_NUMBER,   PyNumberMethods.nb_invert,   slot_nb_invert
    dq sl_abs_name,    SLOT_NUMBER,   PyNumberMethods.nb_absolute, slot_nb_absolute
    dq sl_bool_name,   SLOT_NUMBER,   PyNumberMethods.nb_bool,     slot_nb_bool
    dq sl_index_name,  SLOT_NUMBER,   PyNumberMethods.nb_index,    slot_nb_index
    dq sl_int_name,    SLOT_NUMBER,   PyNumberMethods.nb_int,      slot_nb_int
    dq sl_float_name,  SLOT_NUMBER,   PyNumberMethods.nb_float,    slot_nb_float
    dq sl_len_name,    SLOT_MAPPING,  PyMappingMethods.mp_length,  slot_length
    dq sl_getitem_name, SLOT_MAPPING, PyMappingMethods.mp_subscript, slot_mp_subscript
    ; Either one installs the single assignment wrapper, which reads a NULL
    ; value as a deletion the way dict_ass_subscript does.
    dq sl_setitem_name, SLOT_MAPPING, PyMappingMethods.mp_ass_subscript, slot_mp_ass_subscript
    dq sl_delitem_name, SLOT_MAPPING, PyMappingMethods.mp_ass_subscript, slot_mp_ass_subscript
    dq sl_len_name,    SLOT_SEQUENCE, PySequenceMethods.sq_length, slot_length
    ; Any one of the six installs the single richcompare wrapper, which
    ; dispatches on the op it is handed.
    dq sl_eq_name,     SLOT_DIRECT,   PyTypeObject.tp_richcompare, slot_tp_richcompare
    dq sl_ne_name,     SLOT_DIRECT,   PyTypeObject.tp_richcompare, slot_tp_richcompare
    dq sl_lt_name,     SLOT_DIRECT,   PyTypeObject.tp_richcompare, slot_tp_richcompare
    dq sl_le_name,     SLOT_DIRECT,   PyTypeObject.tp_richcompare, slot_tp_richcompare
    dq sl_gt_name,     SLOT_DIRECT,   PyTypeObject.tp_richcompare, slot_tp_richcompare
    dq sl_ge_name,     SLOT_DIRECT,   PyTypeObject.tp_richcompare, slot_tp_richcompare
    dq 0, 0, 0, 0

section .data
align 8
extern object_method_eq
extern object_method_ne
extern object_method_hash
extern object_method_str
extern object_method_repr
extern object_method_init
extern object_method_lt
extern object_method_le
extern object_method_gt
extern object_method_ge
object_default_impls:
    dq object_method_eq, object_method_ne, object_method_hash
    dq object_method_str, object_method_repr, object_method_init
    ; The four orderings answer NotImplemented and nothing else.  They are
    ; here for the same reason __eq__ is: a builtin subclass finds them in the
    ; MRO before its base's own comparison, and installing a wrapper for one
    ; would make a list subclass sort by identity.
    dq object_method_lt, object_method_le, object_method_gt, object_method_ge
    dq 0
