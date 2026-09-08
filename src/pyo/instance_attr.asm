; pyo/instance_attr.asm - the attribute protocol for a heaptype instance
;
; tp_getattr and tp_setattr for anything __build_class__ built: the
; __getattribute__ hook, the descriptor and MRO resolution behind it, and the
; store side.  Split out of class.asm, which keeps the metatype, the
; constructor and the instance's lifecycle -- these are the part every
; attribute access in a Python program goes through, and the part worth
; reading on its own.
;
; type_refresh_attr_flags lives here too: it is the cold half of both hot
; checks, and it answers the same questions they ask.

%include "macros.inc"
%include "object.inc"
extern type_number_methods
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern raise_exception
extern none_singleton
extern obj_incref
extern dict_new
extern dict_get
extern dict_set
extern str_from_cstr
extern str_from_cstr_heap
extern ap_strcmp
extern type_repr
extern attr_error_pending
extern exc_AttributeError_type
extern exc_TypeError_type
extern func_type
extern type_type
extern kw_names_pending
extern eval_exception_unwind
extern sub_list_for_type
extern object_method_getattribute
extern dunder_lookup
extern dunder_call_2
extern builtin_func_type
extern current_exception
extern dict_type
extern tuple_type
extern int_type
extern str_type
extern staticmethod_type
extern method_new
extern instance_new
extern int_sub_new
extern str_sub_new
extern tuple_sub_fill
extern builtin_sub_init_base
extern classmethod_type
extern property_type
extern member_descr_type
extern getset_descr_type

; Kept in class.asm, which also uses them
extern base_slot
extern instance_getattr_skip
extern type_install_slots

section .text

;; ============================================================================
;; instance_getattr(rdi = instance, rsi = name str) -> rax = Value, or 0
;;
;; tp_getattr for a heaptype instance, and the entry point of the whole
;; attribute protocol -- which means `__getattribute__` first.  A class that
;; defines one intercepts EVERY access, found or not: `c.x` runs it even when
;; x is a plain class attribute.  It used to be ignored entirely, so the only
;; time a user's ran was when the name was also missing and __getattr__ would
;; have run anyway.
;;
;; object's own is the one every class inherits, and calling it would be an
;; infinite regress, so it is recognised and skipped -- and
;; object.__getattribute__, which a user's almost always delegates to, enters
;; at instance_getattr_default below to skip the hook the same way CPython's
;; slot dispatch does.
;; ============================================================================
IGA_SELF  equ 8
IGA_NAME  equ 16
IGA_WHERE equ 24        ; out-parameter: where the answer came from, or NULL
IGA_UNBOUND equ 32      ; 1 when the caller will take a method UNBOUND
IGA_FRAME equ 48            ; + 0 pushes = 48, 16-aligned
;; ============================================================================
;; instance_getattr_where(rdi = self, rsi = name, rdx = int64_t *from_inst_dict,
;;                        ecx = 1 to accept a method UNBOUND)
;;   -> rax = Value, and *rdx = 1 when the answer came out of the INSTANCE
;;      dict, or 2 when it is a function the caller asked for unbound
;;
;; The unbound form exists so that `c.m()` does not build a bound method for
;; op_load_attr to take apart two instructions later: method_new is a gc_alloc,
;; two increfs and a gc_track, and the wrapper is released again immediately.
;; It has to be asked for rather than implied by the out-pointer, because
;; obj_getattr_opt shares this entry and getattr(c, 'm') must still answer a
;; bound method.
;;
;; instance_getattr with one more thing said.  Where the answer came from
;; matters because the descriptor protocol applies to what a TYPE supplies and
;; not to what an instance happens to be holding: a property stored in an
;; instance dict is a property object, not a call to its getter, and an object
;; with __get__ stored there is itself.
;;
;; The same shape as type_getattr / type_getattr_meta, and for the same reason.
;; An out-parameter in the caller's frame is re-entrant; a global would be
;; clobbered by any lookup a descriptor makes on its way through.
;; ============================================================================
global instance_getattr_where
DEF_FUNC instance_getattr_where, IGA_FRAME
    mov [rbp - IGA_WHERE], rdx
    mov [rbp - IGA_UNBOUND], rcx
    jmp instance_getattr.iga_body
END_FUNC instance_getattr_where

;; ============================================================================
;; instance_getattr(rdi = self, rsi = name) -> rax = Value
;;
;; The tp_getattr slot: instance_getattr_where with nowhere to report.
;; ============================================================================
DEF_FUNC instance_getattr, IGA_FRAME
    mov qword [rbp - IGA_WHERE], 0
    mov qword [rbp - IGA_UNBOUND], 0
.iga_body:
    mov qword [rel attr_error_pending], 0
    mov [rbp - IGA_SELF], rdi
    mov [rbp - IGA_NAME], rsi
    test rdi, rdi
    jz .iga_default
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .iga_default
    ; object.__getattribute__ delegating back is not the hook running again:
    ; it asks for the ordinary resolution, and it says so by naming the object
    ; it is asking about.  CPython gets this from slot dispatch -- calling
    ; object's slot cannot reach the subclass's -- and this is the same thing
    ; said explicitly.
    mov rax, [rel instance_getattr_skip]
    cmp rax, rdi
    jne .iga_hook
    mov qword [rel instance_getattr_skip], 0
    jmp .iga_default
.iga_hook:
    mov rdi, [rdi + PyObject.ob_type]
    ; One bit answers what used to be a full MRO walk with a dict lookup per
    ; entry, on every attribute access -- 20-37% of the instructions in
    ; attribute-heavy code, spent almost always to conclude that the
    ; __getattribute__ found is object's own and there is nothing to run.
    ; See TYPE_FLAG_GETATTRIBUTE_OVERRIDDEN in object.inc for who maintains it.
    test qword [rdi + PyTypeObject.tp_flags], TYPE_FLAG_GETATTRIBUTE_OVERRIDDEN
    jz .iga_default
    lea rsi, [rel ig_getattribute_name]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .iga_default
    cmp edx, TAG_PTR
    jne .iga_default
    ; object's own is a builtin wrapping object_method_getattribute.  Anything
    ; else is a definition, and definitions run.
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel builtin_func_type]
    cmp rcx, r8
    jne .iga_call
    extern object_method_getattribute
    lea r8, [rel object_method_getattribute]
    cmp [rax + PyBuiltinObject.func_ptr], r8
    je .iga_default
.iga_call:
    mov rdi, [rbp - IGA_SELF]
    mov rsi, [rbp - IGA_NAME]
    lea rdx, [rel ig_getattribute_name]
    mov ecx, TAG_PTR
    call dunder_call_2          ; a (payload, tag) pair, not a Value
    V_UNPACK rax, rdx
    test edx, edx
    jz .iga_raised
    leave
    V_PACK rax, rdx
    ret
.iga_raised:
    ; Whatever __getattribute__ raised is the answer, including a KeyError or
    ; a TypeError.  Returning NULL bare would let raise_no_attribute replace
    ; it with a generic AttributeError; the flag is how instance_getattr
    ; already hands one over.
    cmp qword [rel current_exception], 0
    je .iga_raised_bare
    mov qword [rel attr_error_pending], 1
.iga_raised_bare:
    xor eax, eax
    leave
    ret
.iga_default:
    mov rdi, [rbp - IGA_SELF]
    mov rsi, [rbp - IGA_NAME]
    mov rdx, [rbp - IGA_WHERE]
    mov rcx, [rbp - IGA_UNBOUND]
    leave
    jmp instance_getattr_default
END_FUNC instance_getattr

;; ============================================================================
;; attr_is_data_descr(rdi = a Value) -> eax = 1 if it is a DATA descriptor
;;
;; CPython's rule is "its type defines __set__ or __delete__".  The three
;; builtin descriptor types are answered by identity; a user descriptor costs
;; two MRO walks, but only ever on the cold path that fills in a type's flags.
;;
;; staticmethod, classmethod and a plain function are deliberately NOT data
;; descriptors: they lose to an entry in the instance dict, and that is the
;; whole distinction this exists to draw.
;; ============================================================================
DEF_FUNC attr_is_data_descr, 8   ; 1 push below, so rsp stays 16-aligned
    push rbx
    V_TEST_PTR rdi, rax
    ja .aidd_no                 ; an immediate is not a descriptor
    test rdi, rdi
    jz .aidd_no
    mov rbx, [rdi + PyObject.ob_type]
    test rbx, rbx
    jz .aidd_no

    lea rax, [rel member_descr_type]
    cmp rbx, rax
    je .aidd_yes
    lea rax, [rel getset_descr_type]
    cmp rbx, rax
    je .aidd_yes
    lea rax, [rel property_type]
    cmp rbx, rax
    je .aidd_yes

    ; Anything else is one only if its own type says so.  This answer must be
    ; EXACT: instance_getattr_default calls it live at .found_type to decide
    ; whether the MRO entry outranks the instance dict, and a non-data
    ; descriptor that answered yes there would beat an instance attribute.
    ; The cached flag's scan wants a different question and asks
    ; attr_may_be_data_descr below.
    test qword [rbx + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .aidd_no
    mov rdi, rbx
    CSTRING rsi, "__set__"
    call dunder_lookup
    test rax, rax               ; dunder_lookup answers with a Value; 0 is the miss
    jnz .aidd_yes
    mov rdi, rbx
    CSTRING rsi, "__delete__"
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jnz .aidd_yes

.aidd_no:
    xor eax, eax
    pop rbx
    leave
    ret
.aidd_yes:
    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC attr_is_data_descr

;; ============================================================================
;; attr_may_be_data_descr(rdi = a Value) -> eax = 1 if it could EVER be one
;;
;; The question TYPE_FLAG_MRO_HAS_DATA_DESCR actually needs, which is not the
;; one attr_is_data_descr answers.  That flag is computed when a class is
;; created and when something is stored ON that class -- but a descriptor's
;; OWN type can gain `__set__` long afterwards, and that write touches neither
;; the class holding the descriptor nor any of its bases:
;;
;;     class Lazy:
;;         def __get__(self, o, t=None): return 1
;;     class B: pass
;;     B.l = Lazy()            ; refreshes B; Lazy has no __set__ yet
;;     Lazy.__set__ = ...      ; refreshes Lazy, which B knows nothing about
;;     b.l = 7                 ; must still call the setter
;;
;; With the exact test the flag stayed clear and that store went straight into
;; the instance dict.  Nothing short of re-scanning every class in the process
;; on `__set__` keeps an exact answer true, so the flag means "there MIGHT be
;; one here" and the walk it gates decides for real -- op_store_attr and
;; instance_getattr_default both make a live attr_is_data_descr check once
;; they get there.  A false positive costs an MRO walk; the false negative was
;; a wrong answer.
;;
;; The over-approximation is exactly as wide as the uncertainty.  A static
;; type cannot gain `__set__` -- setattr on one is refused -- so only a value
;; whose type is a HEAPTYPE is in doubt, and a class body holding an instance
;; of a user class is overwhelmingly the descriptor idiom already.  Functions,
;; ints, strs, staticmethod and classmethod have static types and answer no.
;; ============================================================================
DEF_FUNC attr_may_be_data_descr, 8   ; 1 push below, so rsp stays 16-aligned
    push rbx
    V_TEST_PTR rdi, rax
    ja .amdd_no                 ; an immediate is not a descriptor
    test rdi, rdi
    jz .amdd_no
    mov rbx, [rdi + PyObject.ob_type]
    test rbx, rbx
    jz .amdd_no
    test qword [rbx + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jnz .amdd_yes
    ; A static type is fixed, so the exact test is also the final one.
    pop rbx
    leave
    jmp attr_is_data_descr
.amdd_no:
    xor eax, eax
    pop rbx
    leave
    ret
.amdd_yes:
    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC attr_may_be_data_descr

;; ============================================================================
;; dict_has_data_descr(rdi = a dict) -> eax = 1 if any value is a data descr
;;
;; The dense entries array, holes skipped -- the same walk dict.copy() makes.
;; ============================================================================
DEF_FUNC dict_has_data_descr
    push rbx
    push r12
    push r13
    push r14
    mov rbx, rdi
    mov r13, [rbx + PyDictObject.capacity]
    xor r14d, r14d
.dhdd_loop:
    cmp r14, r13
    jge .dhdd_no
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r14, DICT_ENTRY_SIZE
    add rax, rcx
    mov r12, [rax + DictEntry.key]
    test r12, r12
    jz .dhdd_next
    mov rdi, [rax + DictEntry.value]
    call attr_may_be_data_descr
    test eax, eax
    jnz .dhdd_yes
.dhdd_next:
    inc r14
    jmp .dhdd_loop
.dhdd_no:
    xor eax, eax
    jmp .dhdd_out
.dhdd_yes:
    mov eax, 1
.dhdd_out:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_has_data_descr

;; ============================================================================
;; type_bump_version(rdi = a type, or NULL) -> nothing
;;
;; Give the type a version number nobody has held before, so that every cache
;; guarding on the old one misses.  The version is the high 32 bits of
;; tp_flags; see TYPE_VERSION_SHIFT in object.inc for why it lives there.
;;
;; Zero is the "never cached" sentinel, so the counter skips it on wraparound
;; the way dk_version does.  Wrapping is not a correctness question either
;; way -- a stale cache entry would have to survive 2^32 intervening class
;; mutations AND land on the one type that reused its number -- but skipping
;; zero keeps the sentinel meaning one thing.
;;
;; Every caller reaches this through type_refresh_attr_flags, which already
;; runs at class creation and from type_setattr and already pushes its answer
;; down every subclass.  That recursion is the whole reason the version can be
;; a single word: a change to a base invalidates its subclasses' caches
;; because the walk stamps each of them too.
;; ============================================================================
DEF_FUNC_BARE type_bump_version
    test rdi, rdi
    jz .tbv_out
    mov eax, [rel type_version_counter]
    inc eax
    jnz .tbv_store
    mov eax, 1                      ; wrapped; zero is not a version
.tbv_store:
    mov [rel type_version_counter], eax
    mov rdx, [rdi + PyTypeObject.tp_flags]
    mov edx, edx                    ; keep the flags, drop the old version
    shl rax, TYPE_VERSION_SHIFT
    or rdx, rax
    mov [rdi + PyTypeObject.tp_flags], rdx
.tbv_out:
    ret
END_FUNC type_bump_version

;; ============================================================================
;; type_refresh_attr_flags(rdi = a heaptype) -> nothing
;;
;; Ask, once, the two questions instance_getattr's fast path is not allowed to
;; ask per access:
;;
;;   - does anything in this MRO define a __getattribute__ that is not
;;     object's?                      TYPE_FLAG_GETATTRIBUTE_OVERRIDDEN
;;   - does anything in this MRO keep a DATA descriptor in its dict?
;;                                    TYPE_FLAG_MRO_HAS_DATA_DESCR
;;
;; Both answers are inherited, so both are pushed down every subclass.  That
;; is the whole reason the direct-subclass table has to be populated before
;; any user code runs -- a class registered later would sit outside this walk
;; and keep a stale bit forever.
;;
;; Called at class creation and from type_setattr.  Both are cold; this walks
;; the MRO and allocates nothing.
;; ============================================================================

DEF_FUNC type_refresh_attr_flags
    push rbx
    push r12
    push r13
    push r14
    test rdi, rdi
    jz .trg_out
    mov rbx, rdi

    ; Whatever this recomputes, it recomputes because the type or one of its
    ; bases changed -- so every cache keyed on the old version is now wrong.
    ; Stamping here rather than at the call sites means the walk over
    ; subclasses below invalidates them too, which is the property the caches
    ; rely on.
    call type_bump_version

    lea rsi, [rel ig_getattribute_name]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .trg_clear               ; nothing in the MRO at all
    cmp edx, TAG_PTR
    jne .trg_clear
    ; object's own is a builtin wrapping object_method_getattribute; anything
    ; else is a definition.  The same test instance_getattr makes.
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel builtin_func_type]
    cmp rcx, r8
    jne .trg_set
    lea r8, [rel object_method_getattribute]
    cmp [rax + PyBuiltinObject.func_ptr], r8
    je .trg_clear
.trg_set:
    or qword [rbx + PyTypeObject.tp_flags], TYPE_FLAG_GETATTRIBUTE_OVERRIDDEN
    jmp .trg_data_descr
.trg_clear:
    mov rax, TYPE_FLAG_GETATTRIBUTE_OVERRIDDEN
    not rax
    and [rbx + PyTypeObject.tp_flags], rax

.trg_data_descr:
    ; --- and the data-descriptor bit ---
    ; Reached whichever way the question above was answered.  It used to be
    ; reached only on the `clear` path, so a class that overrode
    ; __getattribute__ never had this bit computed at all -- and the two are
    ; independent: __getattribute__ has nothing to do with __setattr__, and
    ; op_store_attr consults this bit ALONE to decide whether a store needs to
    ; look for a property.  A class with both answered `c.p = 5` by putting
    ; `p` straight into the instance dict, so the setter never ran.
    ; The MRO's dicts, scanned for anything a data descriptor could be.  Cold,
    ; and the answer saves the hot path an MRO walk per attribute access.
    mov rax, TYPE_FLAG_MRO_HAS_DATA_DESCR
    not rax
    and [rbx + PyTypeObject.tp_flags], rax
    mov r12, rbx                        ; the MRO walker
.trg_mro:
    test r12, r12
    jz .trg_children
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .trg_mro_next
    call dict_has_data_descr
    test eax, eax
    jz .trg_mro_next
    or qword [rbx + PyTypeObject.tp_flags], TYPE_FLAG_MRO_HAS_DATA_DESCR
    jmp .trg_children
.trg_mro_next:
    MRO_NEXT r12, rbx
    jmp .trg_mro

.trg_children:
    mov rdi, rbx
    call sub_list_for_type
    test rax, rax
    jz .trg_out
    mov r12, [rax + SubList.items]
    mov r13, [rax + SubList.count]
    test r12, r12
    jz .trg_out
    xor r14d, r14d
.trg_loop:
    cmp r14, r13
    jge .trg_out
    mov rdi, [r12 + r14*8]
    test rdi, rdi
    jz .trg_next
    call type_refresh_attr_flags
.trg_next:
    inc r14
    jmp .trg_loop

.trg_out:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_refresh_attr_flags

;; ============================================================================
;; instance_getattr_default(PyInstanceObject *self, PyObject *name,
;;                          int64_t *from_inst_dict_or_null,
;;                          ecx = 1 to accept a method UNBOUND) -> Value
;; Look up an attribute on an instance, without the __getattribute__ hook.
;; 1. Check self->inst_dict — return raw value
;; 2. If not found, check type->tp_dict (walk tp_base chain)
;; 3. If found in type dict and callable, create bound method
;; 4. If found, INCREF and return
;; 5. If not found, __getattr__, then AttributeError
;;
;; rdi = instance, rsi = name (PyStrObject*)
;; Returns: owned reference to attribute value, or NULL
;; ============================================================================
IG_NAME   equ 8
IG_ORIGIN equ 16        ; the type the MRO walk started from
IG_DESCR1 equ 24        ; 1 when the MRO was consulted BEFORE the dict
IG_WHERE  equ 32        ; out-parameter: set to 1 for an instance-dict hit
IG_UNBOUND equ 40       ; 1 when the caller will take a method unbound
IG_FRAME  equ 56            ; + 3 pushes = 80, 16-aligned
global instance_getattr_default
DEF_FUNC instance_getattr_default, IG_FRAME
    push rbx
    push r12
    push r13
    mov qword [rel attr_error_pending], 0

    mov rbx, rdi                ; rbx = self (instance)
    mov r12, rsi                ; r12 = name
    mov [rbp - IG_NAME], rsi    ; r12 is reused as scratch further down
    mov qword [rbp - IG_DESCR1], 0
    ; rdx is the out-parameter, or NULL.  Everything but .found_inst leaves it
    ; at 0, so only an instance-dict hit reports one.
    mov [rbp - IG_WHERE], rdx
    mov [rbp - IG_UNBOUND], rcx
    test rdx, rdx
    jz .ig_no_where
    mov qword [rdx], 0
.ig_no_where:

    ; A DATA descriptor outranks the instance dict and a non-data one does
    ; not, so the correct order is MRO first -- and the fast order is instance
    ; dict first, which is what an ordinary `self.x` wants.  The flag says
    ; which classes actually need the slow order.  Almost none do.
    mov rax, [rbx + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_MRO_HAS_DATA_DESCR
    jz .inst_dict_first
    mov qword [rbp - IG_DESCR1], 1
    jmp .check_type_dict
.inst_dict_first:

    ; Check self's instance dict first; a type may have none at all.
    LOAD_INST_DICT rdi, rbx, .check_type_dict
    test rdi, rdi
    jz .check_type_dict
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .found_inst

.check_type_dict:

    ; Not in inst_dict -- ask the class what it defines for this name.  That
    ; is an MRO walk with a dict probe per entry, and the answer only changes
    ; when a class in the MRO is written to, so type_lookup_cached keeps it
    ; against the type's version.  It walks unchanged on a miss, and for a
    ; static type, which has no version.
    mov rdi, [rbx + PyObject.ob_type]
    mov [rbp - IG_ORIGIN], rdi
    mov rsi, r12
    extern type_lookup_cached
    call type_lookup_cached             ; rax/edx = value, rcx = the owner
    test edx, edx
    jnz .found_type

    ; Nothing in the MRO.  On the descriptor-first order that leaves the
    ; instance dict still unread.
    cmp qword [rbp - IG_DESCR1], 0
    je .not_found
    mov qword [rbp - IG_DESCR1], 0
    LOAD_INST_DICT rdi, rbx, .not_found
    test rdi, rdi
    jz .not_found
    mov rsi, [rbp - IG_NAME]
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .found_inst
    jmp .not_found

.found_inst:
    ; Found in instance dict — INCREF and return raw value.  Tell the caller,
    ; if it asked: this value gets no descriptor protocol run over it.
    mov rcx, [rbp - IG_WHERE]
    test rcx, rcx
    jz .found_inst_nowhere
    mov qword [rcx], 1
.found_inst_nowhere:
    mov r13, rax                ; save payload
    mov r12, rdx                ; save tag (name no longer needed)
    INCREF_VAL rax, edx         ; tag-aware INCREF (skips SmallInt/NULL)
    mov rax, r13
    mov rdx, r12                ; restore tag from dict_get
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_type:
    ; On the descriptor-first order the instance dict has not been consulted
    ; yet, and it outranks everything here except a data descriptor.
    cmp qword [rbp - IG_DESCR1], 0
    je .found_type_dispatch
    mov qword [rbp - IG_DESCR1], 0      ; ask once
    ; rax/rdx are an unpacked (payload, tag) pair here, not a Value: only a
    ; TAG_PTR payload is an address, and only an address can be a descriptor.
    cmp rdx, TAG_PTR
    jne .ft_beaten_by_inst
    push rax
    push rdx
    mov rdi, rax
    call attr_is_data_descr
    pop rdx
    pop rdi
    test eax, eax
    mov rax, rdi
    jnz .found_type_dispatch            ; data descriptor: it wins
.ft_beaten_by_inst:
    ; Not one.  If the instance has the name, that is the answer.
    push rax
    push rdx
    LOAD_INST_DICT rdi, rbx, .ft_no_inst
    test rdi, rdi
    jz .ft_no_inst
    mov rsi, [rbp - IG_NAME]
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .ft_no_inst
    add rsp, 16                         ; drop the saved type-dict answer
    jmp .found_inst
.ft_no_inst:
    pop rdx
    pop rax

.found_type_dispatch:
    ; Found in type dict — handle method binding.
    ; Descriptors (staticmethod, classmethod, property) are returned as-is
    ; for LOAD_ATTR to unwrap, since LOAD_ATTR knows the push convention.
    ; Member descriptors (slots) read from fixed instance offset.
    ; Regular callables are bound to the instance.
    mov r13, rax                ; r13 = attr (borrowed ref from dict_get)
    mov r12, rdx                ; r12 = attr tag (name no longer needed)
    cmp r12, TAG_PTR
    jne .found_type_raw         ; non-pointer — return as-is

    mov rcx, [rax + PyObject.ob_type]

    ; Check for member descriptor (slot) → read from instance offset
    extern member_descr_type
    lea rdx, [rel member_descr_type]
    cmp rcx, rdx
    je .found_slot

    ; A getset descriptor calls its getter.  int, float and complex register
    ; real/imag/numerator/denominator this way, and a subclass instance
    ; reaches them here rather than through the base's tp_getattr.
    extern getset_descr_type
    lea rdx, [rel getset_descr_type]
    cmp rcx, rdx
    je .found_getset

    ; Check for staticmethod/classmethod/property → return raw descriptor
    ; LOAD_ATTR handles unwrapping with the correct push convention
    lea rdx, [rel staticmethod_type]
    cmp rcx, rdx
    je .found_type_raw

    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    je .found_type_raw

    lea rdx, [rel property_type]
    cmp rcx, rdx
    je .found_type_raw

    ; Only bind func_type and builtin_func_type as methods
    ; Types, classes, and other callables are returned as-is
    lea rdx, [rel func_type]
    cmp rcx, rdx
    je .bind_method

    extern builtin_func_type
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    je .bind_method

    jmp .found_type_raw         ; not a function — return raw

.bind_method:
    ; A caller that is about to CALL this does not need the wrapper: it wants
    ; the function and the receiver, which is what op_load_attr takes the
    ; wrapper apart to get.  Answer with the function and say so, and the
    ; gc_alloc, the gc_track, the two increfs and the immediate dealloc all go.
    cmp qword [rbp - IG_UNBOUND], 0
    je .bind_method_wrapped
    mov rcx, [rbp - IG_WHERE]
    test rcx, rcx
    jz .bind_method_wrapped
    mov qword [rcx], 2          ; "a function, not bound"
    mov rax, r13
    INCREF rax
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.bind_method_wrapped:
    ; Function found in type dict — create bound method
    mov rdi, r13                ; func
    mov rsi, rbx                ; self (instance)
    call method_new
    ; rax = bound method (method_new INCREFs func and self)
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_slot:
    ; Member descriptor found — read the value out of the instance.
    ; A __slots__ value is instance storage exactly as a dict entry is, so it
    ; reports the same way: the descriptor protocol does not run over it.
    mov rcx, [rbp - IG_WHERE]
    test rcx, rcx
    jz .found_slot_nowhere
    mov qword [rcx], 1
.found_slot_nowhere:
    ; r13 = member descriptor, rbx = instance
    mov rcx, [r13 + PyMemberDescrObject.md_offset]
    SLOT_ADDR rdx, rbx, rcx
    mov rax, [rdx]             ; slot Value
    test rax, rax
    jz .slot_not_set            ; 0 = slot not set → AttributeError
    INCREF_V rax, rdx
    V_UNPACK rax, rdx
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_getset:
    ; getset_descr_get answers a Value, or never returns when the attribute
    ; has no getter at all.
    mov rdi, r13
    mov rsi, rbx
    extern getset_descr_get
    call getset_descr_get
    V_UNPACK rax, rdx
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_type_raw:
    ; Not callable, SmallInt, or descriptor — INCREF and return
    INCREF_VAL r13, r12         ; tag-aware INCREF
    mov rax, r13
    mov rdx, r12                ; restore tag from dict_get
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.slot_not_set:
    ; Slot exists but not initialized — raise AttributeError directly
    ; (must not return NULL or LOAD_ATTR fallback finds descriptor in tp_dict).
    ; CPython names the type and the attribute here as it does everywhere else.
    mov rdi, rbx
    mov rsi, [rbp - IG_NAME]
    extern raise_no_attribute
    call raise_no_attribute

.not_found:
    ; __class__ and __dict__ are part of ordinary resolution, not of the hook:
    ; CPython answers them from getsets on the type, so a class that defines
    ; __getattr__ never sees either name.  Asking the hook first made
    ; `self.__dict__` INSIDE a __getattr__ re-enter it -- which is how
    ; typing.py's _BaseGenericAlias.__getattr__ is written, and it recursed
    ; until the stack ran out, taking the whole typing module with it.
    mov rdi, rbx
    mov rsi, [rbp - IG_NAME]
    extern obj_generic_attr
    call obj_generic_attr
    test rax, rax
    jz .ig_ask_getattr
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.ig_ask_getattr:
    ; Ordinary lookup missed.  __getattr__ is Python's hook for exactly that
    ; -- it runs only when normal resolution fails -- and it was never
    ; consulted, so a class defining it got a bare AttributeError.
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel ig_getattr_name]
    call dunder_lookup
    test rax, rax               ; dunder_lookup answers with a Value; 0 is the miss
    jz .really_not_found
    ; A __getattr__ explicitly set to None is not absent.  CPython calls it and
    ; the call fails as "'NoneType' object is not callable"; treating it as
    ; absent turned that into a plain AttributeError, which is what a caller
    ; would then swallow.  dunder_call_2 raises it, so just let it through.

    ; dunder_call_2(self, name, "__getattr__", TAG_PTR)
    mov rdi, rbx
    mov rsi, [rbp - IG_NAME]    ; the attribute name str
    lea rdx, [rel ig_getattr_name]
    mov ecx, TAG_PTR
    extern dunder_call_2
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jz .getattr_raised          ; the slot is present, so NULL means it raised
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.getattr_raised:
    ; An AttributeError from __getattr__ is the protocol saying "absent", and
    ; getattr(o, n, default) and hasattr() have to be able to see that and
    ; answer.  Unwinding from here skips their native frames entirely, so they
    ; never got the chance.  Hand back NULL with the exception still pending
    ; and a flag saying so; raise_no_attribute propagates it rather than
    ; replacing it, so `o.missing` still reports what __getattr__ raised.
    ;
    ; Anything else is a genuine failure in the middle of a lookup and keeps
    ; unwinding, which is what it did before.
    mov rax, [rel current_exception]
    test rax, rax
    jz .getattr_unwind
    push rax
    mov rdi, [rax + PyObject.ob_type]
    lea rsi, [rel exc_AttributeError_type]
    extern type_is_subtype
    call type_is_subtype
    pop rcx
    test eax, eax
    jz .getattr_unwind
    mov qword [rel attr_error_pending], 1
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret
.getattr_unwind:
    leave
    jmp eval_exception_unwind

.really_not_found:
    ; A builtin base may answer through its own tp_getattr rather than a
    ; tp_dict entry -- bytes.decode and str.encode live there -- and a
    ; subclass inherits those.  Only the *base's* slot: this type's own is
    ; instance_getattr, which is where we already are.
    mov rdi, [rbx + PyObject.ob_type]
    mov rsi, PyTypeObject.tp_getattr
    call base_slot
    test rax, rax
    jz .no_base_getattr
    lea rcx, [rel instance_getattr]
    cmp rax, rcx
    je .no_base_getattr
    mov rdi, rbx
    mov rsi, [rbp - IG_NAME]
    call rax
    V_UNPACK rax, rdx
    test edx, edx
    jz .no_base_getattr
    ; It hands back an *unbound* builtin -- its own instances reach it through
    ; LOAD_ATTR's method form -- so bind it here, where the caller is about to
    ; be told the answer came from a heaptype and needs no self.
    cmp edx, TAG_PTR
    jne .base_getattr_done
    mov rcx, [rax + PyObject.ob_type]
    extern builtin_func_type
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    jne .base_getattr_ptr
    mov rdi, rax
    mov rsi, rbx
    push rax
    call method_new
    mov r13, rax
    pop rdi
    call obj_decref
    mov rax, r13
.base_getattr_ptr:
    mov edx, TAG_PTR
.base_getattr_done:
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.no_base_getattr:
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC instance_getattr_default

;; ============================================================================
;; instance_setattr(rdi = instance, rsi = name, rdx = value Value) -> nothing
;;
;; tp_setattr for a heaptype instance.  Walks the MRO for a data descriptor
;; first, then falls back to the instance dict, creating it if this family
;; has one and it has not been made yet.  A value of 0 means DELETE_ATTR.
;; Raises rather than returning a status.
;; ============================================================================
DEF_FUNC instance_setattr
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; instance
    mov r12, rsi                ; name
    mov r13, rdx                ; value Value

    ; Does the MRO define this name as a data descriptor?
    mov rax, [rbx + PyObject.ob_type]
    ; A slot, a property and a getset are all data descriptors, so the flag
    ; that says none of them is in this MRO says the lookup will find nothing.
    ; It is an over-approximation, so a clear flag is a real negative.
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_MRO_HAS_DATA_DESCR
    jz .sa_no_slot

    ; Asked through the type cache, not by walking.  This is the SECOND time
    ; the same MRO is searched for the same name on an ordinary store --
    ; op_store_attr asks first -- and as a raw dict_get per level it cost more
    ; than everything else the store does put together, every level of it
    ; finding nothing.  Same registers as the walk it replaces, and the
    ; reference is borrowed the same way.
    mov rdi, rax
    mov rsi, r12                ; name
    call type_lookup_cached     ; rax = payload, edx = tag
    mov r9, rax
    test edx, edx
    jz .sa_no_slot

.sa_found_type:
    ; Check if it's a member descriptor (r9 = the value found)
    cmp edx, TAG_PTR
    jne .sa_no_slot
    extern member_descr_type
    lea rcx, [rel member_descr_type]
    cmp [r9 + PyObject.ob_type], rcx
    je .sa_member

    ; A property is a data descriptor too, and this is the only road a DELETE
    ; takes: op_store_attr has a property fast path of its own, op_delete_attr
    ; has none and comes straight here.  So `del obj.prop` never reached the
    ; deleter -- it fell through to the instance dict and did nothing.
    lea rcx, [rel property_type]
    cmp [r9 + PyObject.ob_type], rcx
    jne .sa_check_getset
    mov rdi, r9
    mov rsi, rbx
    mov rdx, r13                ; the value Value; 0 means delete
    extern property_descr_set
    call property_descr_set
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_check_getset:
    ; A getset descriptor is a data descriptor: it takes precedence over the
    ; instance dict, so `I(5).real = 9` is the AttributeError CPython raises
    ; rather than a shadowing instance attribute.
    extern getset_descr_type
    lea rcx, [rel getset_descr_type]
    cmp [r9 + PyObject.ob_type], rcx
    jne .sa_no_slot
    mov rdi, r9
    mov rsi, rbx
    mov rdx, r13
    extern getset_descr_set
    call getset_descr_set
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_member:

    ; Member descriptor! Write the value into the slot
    mov rcx, [r9 + PyMemberDescrObject.md_offset]
    SLOT_ADDR rdx, rbx, rcx

    ; XDECREF old value at slot
    push rdx
    mov rdi, [rdx]             ; old Value
    XDECREF_V rdi, rsi
    pop rdx

    ; INCREF the new value and store it
    INCREF_V r13, r14
    mov [rdx], r13

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_no_slot:
    ; No slot found. Fall back to the instance dict.
    LOAD_INST_DICT rdi, rbx, .sa_no_dict_slot
    test rdi, rdi
    jnz .sa_have_dict

    ; inst_dict is NULL — check if __slots__ class (can't set arbitrary attrs)
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HAS_SLOTS
    jnz .sa_no_dict_error

    ; Regular class without __slots__ — create dict on the fly
    push r12
    push r13
    push r14
    call dict_new
    STORE_INST_DICT rbx, rax, rcx, .sa_no_dict_slot
    mov rdi, rax
    pop r14
    pop r13
    pop r12
    jmp .sa_dict_set

.sa_have_dict:
.sa_dict_set:
    ; A NULL value means DELETE, not "store a NULL".  dict_set was called
    ; either way, so `del obj.attr` left the key in the instance dict bound
    ; to a NULL Value: vars(obj) could not be repr'd, len(vars(obj)) still
    ; counted it, and deleting twice succeeded.
    test r13, r13
    jz .sa_dict_del
    mov rsi, r12                ; name
    mov rdx, r13                ; value
    call dict_set

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_dict_del:
    mov rsi, r12                ; name
    extern dict_del_opt
    call dict_del_opt           ; -1 when it was never there
    test eax, eax
    jnz .sa_del_missing
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sa_del_missing:
    mov rdi, rbx
    mov rsi, r12
    call raise_no_attribute     ; does not return

.sa_no_dict_error:
.sa_no_dict_slot:
    ; This type's instances have no dict slot -- a str subclass, or a class
    ; with __slots__ -- so there is nowhere to put the attribute.  The message
    ; names both the type and the attribute, as every other one here does; a
    ; bare "object has no attribute" said neither.
    mov rdi, rbx
    mov rsi, r12
    extern raise_no_attribute
    call raise_no_attribute
END_FUNC instance_setattr

section .rodata
ig_getattr_name: db "__getattr__", 0
ig_getattribute_name: db "__getattribute__", 0

section .bss
type_version_counter: resd 1
