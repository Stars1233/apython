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
extern dunder_lookup_owner
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


;; ============================================================================
;; DEF_BINARY_SLOT wrapper, dunder_name_symbol, nb_field
;;
;; The generic dispatcher for a binary operator slot:
;;   (rdi = left Value, rsi = right Value) -> rax = result Value, or NULL
;;
;; A NULL Value means NotImplemented.  That is already what a declining nb_
;; slot means to op_binary_op and obj_binary_op, so the protocol carries on
;; to the right operand and then to the reflected dunder.  A dunder that
;; RAISES cannot be reported that way -- a NULL would read as a decline and
;; the exception would surface later at an unrelated instruction -- so it goes
;; to slot_reraise, like every other wrapper here.
;;
;; The wrapper speaks for the LEFT operand only.  op_binary_op offers the pair
;; to the RIGHT type's slot as well, with the operands still in their original
;; order, and answering there would call the left object's __op__ -- the wrong
;; object entirely.  The identity test below is CPython's own, from SLOT1BIN:
;; "am I the slot this operand's type actually holds?"
;; ============================================================================
SB_LEFT  equ 8
SB_RIGHT equ 16
SB_EXC   equ 24
SB_FRAME equ 32             ; + 0 pushes = 32, 16-aligned

%macro DEF_BINARY_SLOT 3        ; %1 = wrapper, %2 = name symbol, %3 = nb field
DEF_FUNC %1, SB_FRAME
    mov [rbp - SB_LEFT], rdi
    mov [rbp - SB_RIGHT], rsi

    V_TEST_PTR rdi, rax
    ja %%decline                ; an immediate holds no slot of its own
    test rdi, rdi
    jz %%decline
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz %%decline
    mov rax, [rax + PyNumberMethods.%3]
    lea rcx, [rel %1]
    cmp rax, rcx
    jne %%decline               ; we are the RIGHT type's slot here

    DUNDER_EXC_SAVE [rbp - SB_EXC]
    mov rdi, [rbp - SB_LEFT]
    mov rsi, [rbp - SB_RIGHT]
    V_UNPACK rsi, rcx           ; dunder_call_2 wants (payload, tag)
    lea rdx, [rel %2]
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jz %%none_or_raised

    lea rcx, [rel notimpl_singleton]
    cmp rax, rcx
    je %%drop_notimpl
    V_PACK rax, rdx
    leave
    ret

%%drop_notimpl:
    mov rdi, rax                ; dunder_call_2 hands back an owned reference
    call obj_decref
%%decline:
    xor eax, eax                ; the NULL Value
    leave
    ret

%%none_or_raised:
    EXC_RAISED_SINCE [rbp - SB_EXC], rcx, %%raised
    xor eax, eax
    leave
    ret
%%raised:
    call slot_reraise           ; does not return
END_FUNC %1
%endmacro

DEF_UNARY_SLOT slot_nb_negative, sl_neg_name
DEF_UNARY_SLOT slot_nb_positive, sl_pos_name
DEF_UNARY_SLOT slot_nb_invert,   sl_invert_name
DEF_UNARY_SLOT slot_nb_absolute, sl_abs_name

; The binary operators, forward and in-place.  Reflected names get no
; wrapper: this one is one-directional, unlike CPython's SLOT1BIN, and
; op_binary_op's reflected-dunder arm already serves that direction.

DEF_BINARY_SLOT slot_nb_add, sl_add_name, nb_add
DEF_BINARY_SLOT slot_nb_sub, sl_sub_name, nb_subtract
DEF_BINARY_SLOT slot_nb_mul, sl_mul_name, nb_multiply
DEF_BINARY_SLOT slot_nb_mod, sl_mod_name, nb_remainder
DEF_BINARY_SLOT slot_nb_divmod, sl_divmod_name, nb_divmod
DEF_BINARY_SLOT slot_nb_pow, sl_pow_name, nb_power
DEF_BINARY_SLOT slot_nb_lshift, sl_lshift_name, nb_lshift
DEF_BINARY_SLOT slot_nb_rshift, sl_rshift_name, nb_rshift
DEF_BINARY_SLOT slot_nb_and, sl_and_name, nb_and
DEF_BINARY_SLOT slot_nb_xor, sl_xor_name, nb_xor
DEF_BINARY_SLOT slot_nb_or, sl_or_name, nb_or
DEF_BINARY_SLOT slot_nb_floordiv, sl_floordiv_name, nb_floor_divide
DEF_BINARY_SLOT slot_nb_truediv, sl_truediv_name, nb_true_divide
DEF_BINARY_SLOT slot_nb_matmul, sl_matmul_name, nb_matmul
DEF_BINARY_SLOT slot_nb_iadd, sl_iadd_name, nb_iadd
DEF_BINARY_SLOT slot_nb_isub, sl_isub_name, nb_isub
DEF_BINARY_SLOT slot_nb_imul, sl_imul_name, nb_imul
DEF_BINARY_SLOT slot_nb_imod, sl_imod_name, nb_irem
DEF_BINARY_SLOT slot_nb_ipow, sl_ipow_name, nb_ipow
DEF_BINARY_SLOT slot_nb_ilshift, sl_ilshift_name, nb_ilshift
DEF_BINARY_SLOT slot_nb_irshift, sl_irshift_name, nb_irshift
DEF_BINARY_SLOT slot_nb_iand, sl_iand_name, nb_iand
DEF_BINARY_SLOT slot_nb_ixor, sl_ixor_name, nb_ixor
DEF_BINARY_SLOT slot_nb_ior, sl_ior_name, nb_ior
DEF_BINARY_SLOT slot_nb_ifloordiv, sl_ifloordiv_name, nb_ifloor_divide
DEF_BINARY_SLOT slot_nb_itruediv, sl_itruediv_name, nb_itrue_divide
DEF_BINARY_SLOT slot_nb_imatmul, sl_imatmul_name, nb_imatmul

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
;; type_install_slots(rdi = heaptype)
;;
;; Fill the type's slots from the dunders it defines.  Called once at class
;; creation, and again by type_setattr when a dunder is assigned afterwards,
;; so `C.__iter__ = f` takes effect the way it does in CPython.
;; ============================================================================
TIS_TYPE  equ 8
TIS_ENTRY equ 16
TIS_FOUND equ 24
TIS_OWNER equ 32            ; the MRO entry whose tp_dict answered
TIS_FRAME equ 48            ; + 2 pushes = 64

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
    lea rdx, [rbp - TIS_OWNER]
    call dunder_lookup_owner    ; walks the MRO; returns a Value
    V_UNPACK rax, rdx
    mov rbx, [rbp - TIS_ENTRY]
    test edx, edx
    jz .skip                    ; the class does not define this dunder
    mov [rbp - TIS_FOUND], rax

    ; A dunder a BUILTIN base supplies is not a definition this class made,
    ; and a generic wrapper must not be installed over it.  type_from_parts
    ; has already given the subclass that base's real slot by pointer, so
    ; leaving the slot alone is not merely safe -- it is the same thing
    ; CPython does when update_one_slot recognises an inherited wrapper
    ; descriptor and installs the base's own C function, one indirection
    ; earlier.
    ;
    ; Without this, `class E(int): pass` finds int's own __add__ in the MRO
    ; and would get a wrapper over it -- and int.__add__ refuses a float, so
    ; E(1) + 2.5 would answer NotImplemented both ways round and raise, where
    ; int's nb_add coerces and CPython answers 3.5.
    ;
    ; TYPE_FLAG_HEAPTYPE is set on every class type_from_parts builds and on
    ; nothing static, so the test is one flag on the type that answered.
    mov rcx, [rbp - TIS_OWNER]
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .skip
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
    jmp .skip

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
sl_add_name: db "__add__", 0
sl_sub_name: db "__sub__", 0
sl_mul_name: db "__mul__", 0
sl_mod_name: db "__mod__", 0
sl_divmod_name: db "__divmod__", 0
sl_pow_name: db "__pow__", 0
sl_lshift_name: db "__lshift__", 0
sl_rshift_name: db "__rshift__", 0
sl_and_name: db "__and__", 0
sl_xor_name: db "__xor__", 0
sl_or_name: db "__or__", 0
sl_floordiv_name: db "__floordiv__", 0
sl_truediv_name: db "__truediv__", 0
sl_matmul_name: db "__matmul__", 0
sl_iadd_name: db "__iadd__", 0
sl_isub_name: db "__isub__", 0
sl_imul_name: db "__imul__", 0
sl_imod_name: db "__imod__", 0
sl_ipow_name: db "__ipow__", 0
sl_ilshift_name: db "__ilshift__", 0
sl_irshift_name: db "__irshift__", 0
sl_iand_name: db "__iand__", 0
sl_ixor_name: db "__ixor__", 0
sl_ior_name: db "__ior__", 0
sl_ifloordiv_name: db "__ifloordiv__", 0
sl_itruediv_name: db "__itruediv__", 0
sl_imatmul_name: db "__imatmul__", 0
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
;; ============================================================================
;; slot_binop_wrappers -- the wrapper installed for each NB_* op, indexed
;; exactly as arith.asm's binary_op_offsets is: 0..12 forward, 13..25
;; in-place.  op_binary_op reads it to answer one question it cannot get from
;; the slot alone, now that every heaptype overriding an operator holds the
;; same function there: is this type's own __op__ what the slot would call?
;; ============================================================================
global slot_binop_wrappers
slot_binop_wrappers:
    dq slot_nb_add
    dq slot_nb_and
    dq slot_nb_floordiv
    dq slot_nb_lshift
    dq slot_nb_matmul
    dq slot_nb_mul
    dq slot_nb_mod
    dq slot_nb_or
    dq slot_nb_pow
    dq slot_nb_rshift
    dq slot_nb_sub
    dq slot_nb_truediv
    dq slot_nb_xor
    dq slot_nb_iadd
    dq slot_nb_iand
    dq slot_nb_ifloordiv
    dq slot_nb_ilshift
    dq slot_nb_imatmul
    dq slot_nb_imul
    dq slot_nb_imod
    dq slot_nb_ior
    dq slot_nb_ipow
    dq slot_nb_irshift
    dq slot_nb_isub
    dq slot_nb_itruediv
    dq slot_nb_ixor

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
    
    ; The binary operators.  CPython maps each of these names to a sequence
    ; slot as well, but only the NUMERIC slotdef carries a generic
    ; dispatcher -- sq_concat and sq_repeat exist so list.__add__ is findable
    ; by name, not to be filled in on a subclass.  A row for either would
    ; answer `2 * L` with L.__mul__ where CPython answers list.__rmul__.

    dq sl_add_name, SLOT_NUMBER, PyNumberMethods.nb_add, slot_nb_add
    dq sl_sub_name, SLOT_NUMBER, PyNumberMethods.nb_subtract, slot_nb_sub
    dq sl_mul_name, SLOT_NUMBER, PyNumberMethods.nb_multiply, slot_nb_mul
    dq sl_mod_name, SLOT_NUMBER, PyNumberMethods.nb_remainder, slot_nb_mod
    dq sl_divmod_name, SLOT_NUMBER, PyNumberMethods.nb_divmod, slot_nb_divmod
    dq sl_pow_name, SLOT_NUMBER, PyNumberMethods.nb_power, slot_nb_pow
    dq sl_lshift_name, SLOT_NUMBER, PyNumberMethods.nb_lshift, slot_nb_lshift
    dq sl_rshift_name, SLOT_NUMBER, PyNumberMethods.nb_rshift, slot_nb_rshift
    dq sl_and_name, SLOT_NUMBER, PyNumberMethods.nb_and, slot_nb_and
    dq sl_xor_name, SLOT_NUMBER, PyNumberMethods.nb_xor, slot_nb_xor
    dq sl_or_name, SLOT_NUMBER, PyNumberMethods.nb_or, slot_nb_or
    dq sl_floordiv_name, SLOT_NUMBER, PyNumberMethods.nb_floor_divide, slot_nb_floordiv
    dq sl_truediv_name, SLOT_NUMBER, PyNumberMethods.nb_true_divide, slot_nb_truediv
    dq sl_matmul_name, SLOT_NUMBER, PyNumberMethods.nb_matmul, slot_nb_matmul
    dq sl_iadd_name, SLOT_NUMBER, PyNumberMethods.nb_iadd, slot_nb_iadd
    dq sl_isub_name, SLOT_NUMBER, PyNumberMethods.nb_isub, slot_nb_isub
    dq sl_imul_name, SLOT_NUMBER, PyNumberMethods.nb_imul, slot_nb_imul
    dq sl_imod_name, SLOT_NUMBER, PyNumberMethods.nb_irem, slot_nb_imod
    dq sl_ipow_name, SLOT_NUMBER, PyNumberMethods.nb_ipow, slot_nb_ipow
    dq sl_ilshift_name, SLOT_NUMBER, PyNumberMethods.nb_ilshift, slot_nb_ilshift
    dq sl_irshift_name, SLOT_NUMBER, PyNumberMethods.nb_irshift, slot_nb_irshift
    dq sl_iand_name, SLOT_NUMBER, PyNumberMethods.nb_iand, slot_nb_iand
    dq sl_ixor_name, SLOT_NUMBER, PyNumberMethods.nb_ixor, slot_nb_ixor
    dq sl_ior_name, SLOT_NUMBER, PyNumberMethods.nb_ior, slot_nb_ior
    dq sl_ifloordiv_name, SLOT_NUMBER, PyNumberMethods.nb_ifloor_divide, slot_nb_ifloordiv
    dq sl_itruediv_name, SLOT_NUMBER, PyNumberMethods.nb_itrue_divide, slot_nb_itruediv
    dq sl_imatmul_name, SLOT_NUMBER, PyNumberMethods.nb_imatmul, slot_nb_imatmul

    dq 0, 0, 0, 0

