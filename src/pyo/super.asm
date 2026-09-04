; super.asm - the super type
;
; `super` used to be a name with nothing behind it: a 27-quadword table whose
; only filled slots were tp_name and tp_basicsize, and whose ob_type was
; ITSELF -- so calling it found no tp_call on its metatype and raised "object
; is not callable".  It existed only because LOAD_SUPER_ATTR needs the name to
; be loadable; the opcode popped that value, discarded it, and did the MRO
; walk itself.  `super().m()` therefore worked and `s = super()` did not, and
; neither did storing one, passing one, or reaching one through getattr.
;
; This is the real type.  The lookup is shared with the opcode rather than
; written twice: super_check computes __self_class__, super_find walks the
; MRO, and super_lookup applies the descriptor protocol to what it finds.
; LOAD_SUPER_ATTR's attribute form is a call to super_lookup; its method form
; keeps arms of its own, because it pushes two values and leaves a function
; unbound on purpose.
;
; The zero-argument form reads the calling frame, as CPython's does: the first
; positional argument, and the __class__ cell the compiler creates for any
; method inside a class body that so much as mentions `super` (sym_note_super)
; and that type_from_parts fills through __classcell__.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

extern type_type
extern type_is_subtype
extern type_mro_next
extern dict_get
extern method_new
extern property_descr_get
extern property_type
extern staticmethod_type
extern classmethod_type
extern func_type
extern builtin_func_type
extern gc_alloc
extern gc_track
extern gc_dealloc
extern obj_incref
extern obj_decref
extern set_exception
extern str_from_cstr_heap
extern eval_saved_r12
extern exc_TypeError_type
extern exc_RuntimeError_type
extern none_singleton
extern value_type
extern exc_AttributeError_type

; Room for a message plus a type name; a tp_name is a literal in an assembled
; table, so this is a guard rather than a case that happens.
SUP_MSG_MAX equ 240

section .text

;; ============================================================================
;; sup_streq(rdi = a NUL-terminated string, rsi = another) -> eax = 0 if equal
;;
;; The attribute names below are interned, but comparing the bytes costs
;; nothing and does not depend on that being true of the argument.
;; ============================================================================
DEF_FUNC_LOCAL sup_streq
.loop:
    movzx eax, byte [rdi]
    movzx ecx, byte [rsi]
    cmp eax, ecx
    jne .differ
    test eax, eax
    jz .same
    inc rdi
    inc rsi
    jmp .loop
.same:
    xor eax, eax
    leave
    ret
.differ:
    mov eax, 1
    leave
    ret
END_FUNC sup_streq

;; ============================================================================
;; sup_error(rdi = the exception type, rsi = a template, rdx = the text to
;;           substitute for the byte 1 in it, or 0)
;;
;; CPython names the offending type and counts the arguments, and both messages
;; are tested against it.  raise_type_error_with_typename next door composes the
;; same shape but RAISES, and a tp_new has to report by returning instead: it is
;; called from type_call, which holds the argument array.
;; ============================================================================
DEF_FUNC_LOCAL sup_error
    push rbx
    push r12
    push r13
    push r14                    ; four pushes keep rsp 16-aligned at the call
    mov rbx, rsi                ; the template
    mov r12, rdx                ; the substitution
    mov r13, rdi                ; the exception type
    lea r14, [rel sup_msg_buf]
    xor ecx, ecx
.se_copy:
    movzx eax, byte [rbx]
    test al, al
    jz .se_end
    inc rbx
    cmp al, 1
    je .se_insert
    cmp rcx, SUP_MSG_MAX - 2
    jae .se_copy
    mov [r14 + rcx], al
    inc rcx
    jmp .se_copy
.se_insert:
    test r12, r12
    jz .se_copy
    push rsi
    mov rsi, r12
.se_ins_loop:
    movzx eax, byte [rsi]
    test al, al
    jz .se_ins_done
    inc rsi
    cmp rcx, SUP_MSG_MAX - 2
    jae .se_ins_done
    mov [r14 + rcx], al
    inc rcx
    jmp .se_ins_loop
.se_ins_done:
    pop rsi
    jmp .se_copy
.se_end:
    mov byte [r14 + rcx], 0
    mov rdi, r13
    mov rsi, r14
    call set_exception
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sup_error

;; ============================================================================
;; super_no_attribute(rdi = the attribute name) -- sets the AttributeError
;; LOAD_SUPER_ATTR reports, and returns.  A super OBJECT gets the same wording
;; for free: its tp_getattr answers "not here" and the generic path names the
;; type.  The opcode has no object to name, so it composes the same sentence.
;; ============================================================================
global super_no_attribute
DEF_FUNC super_no_attribute
    lea rdx, [rdi + PyStrObject.data]
    lea rdi, [rel exc_AttributeError_type]
    lea rsi, [rel sup_msg_noattr]
    call sup_error
    leave
    ret
END_FUNC super_no_attribute

;; ============================================================================
;; super_check(rdi = the class super was written with, rsi = the object)
;;   -> rax = __self_class__, borrowed, or 0 with a TypeError pending
;;
;; CPython's supercheck.  The object is either an instance of the class -- in
;; which case its type is what gets searched -- or a subclass of it, which is
;; what a classmethod's first argument is, and then the object IS the type to
;; search.  Anything else is a TypeError, worded as CPython words it.
;; ============================================================================
SC_CLASS equ 8
SC_OBJ   equ 16
SC_FRAME equ 32                 ; 16 used + 16 pad = 32, 16-aligned
global super_check
DEF_FUNC super_check, SC_FRAME
    mov [rbp - SC_CLASS], rdi
    mov [rbp - SC_OBJ], rsi

    ; A class that derives from the class: search that class's own MRO.
    mov rax, [rsi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .sc_instance
    mov rdi, rsi                        ; the candidate subclass
    mov rsi, [rbp - SC_CLASS]
    call type_is_subtype
    test eax, eax
    jz .sc_instance
    mov rax, [rbp - SC_OBJ]
    leave
    ret

.sc_instance:
    mov rdi, [rbp - SC_OBJ]
    mov rdi, [rdi + PyObject.ob_type]
    mov rsi, [rbp - SC_CLASS]
    call type_is_subtype
    test eax, eax
    jz .sc_bad
    mov rax, [rbp - SC_OBJ]
    mov rax, [rax + PyObject.ob_type]
    leave
    ret

.sc_bad:
    SET_EXC exc_TypeError_type, "super(type, obj): obj must be an instance or subtype of type"
    xor eax, eax
    leave
    ret
END_FUNC super_check

;; ============================================================================
;; super_find(rdi = __self_class__, rsi = __thisclass__, rdx = the name)
;;   -> rax = payload, rdx = tag, BORROWED from the tp_dict that holds it;
;;      (0, 0) when no class after __thisclass__ defines the name
;;
;; The search starts at the entry AFTER __thisclass__ in __self_class__'s MRO.
;; That is the whole point of super in a diamond: following __thisclass__'s
;; own tp_base chain reaches its bases and skips the sibling branch entirely.
;; ============================================================================
SF_ORIGIN equ 8
SF_NAME   equ 16
SF_FRAME  equ 32                ; 16 used + 16 pad = 32, 16-aligned
global super_find
DEF_FUNC super_find, SF_FRAME
    mov [rbp - SF_ORIGIN], rdi
    mov [rbp - SF_NAME], rdx
    call type_mro_next          ; rdi = origin, rsi = the class
    test rax, rax
    jz .sf_absent

.sf_walk:
    ; rcx tracks the current type for the .sf_next step, including the
    ; no-tp_dict path, which would otherwise fall through with rcx undefined.
    mov rcx, rax
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .sf_next

    push rax
    mov rsi, [rbp - SF_NAME]
    call dict_get
    V_UNPACK rax, rdx
    pop rcx
    test edx, edx               ; the tag, not the payload: a hit may be int 0
    jnz .sf_done

.sf_next:
    MRO_NEXT rcx, [rbp - SF_ORIGIN]
    mov rax, rcx
    test rax, rax
    jnz .sf_walk

.sf_absent:
    xor eax, eax
    xor edx, edx
.sf_done:
    leave
    ret
END_FUNC super_find

;; ============================================================================
;; super_lookup(rdi = __thisclass__, rsi = __self__, rdx = __self_class__,
;;              rcx = the name, r8 = int64_t *out_absent)
;;   -> rax = payload, rdx = tag, OWNED
;;
;; The attribute form: what `super(B, obj).name` evaluates to, and what
;; LOAD_SUPER_ATTR pushes when its low bit is clear.  Whatever the MRO walk
;; finds goes through the descriptor protocol, which is where the four cases
;; come from:
;;
;;   staticmethod   unwrapped, never bound
;;   classmethod    bound to __self_class__, not to the instance
;;   function       bound to __self__ -- but NOT when __self__ IS
;;                  __self_class__, which is CPython's rule and is why
;;                  `super(B, B).m` is the plain function there and was a
;;                  bound method here
;;   property       run, because `super().value` is the getter's answer and
;;                  not the descriptor
;;
;; and anything else is the class attribute as it stands.
;;
;; *out_absent is 1, with no exception set, when nothing on the MRO has the
;; name; the caller words that failure, because the opcode and the getattr
;; slot word it differently.  A (0, 0) result with *out_absent 0 is a getter
;; that raised.
;; ============================================================================
SL_TYPE     equ 8
SL_OBJ      equ 16
SL_OBJTYPE  equ 24
SL_NAME     equ 32
SL_ATTR     equ 40
SL_TAG      equ 48
SL_BIND     equ 56
SL_ABSENT   equ 64
SL_FRAME    equ 80              ; 64 used + 16 pad = 80, 16-aligned
global super_lookup
DEF_FUNC super_lookup, SL_FRAME
    mov [rbp - SL_TYPE], rdi
    mov [rbp - SL_OBJ], rsi
    mov [rbp - SL_OBJTYPE], rdx
    mov [rbp - SL_NAME], rcx
    mov [rbp - SL_ABSENT], r8
    mov qword [r8], 0

    mov rdi, rdx                ; __self_class__
    mov rsi, [rbp - SL_TYPE]
    mov rdx, rcx                ; the name
    call super_find
    test edx, edx
    jz .sl_absent
    mov [rbp - SL_ATTR], rax
    mov [rbp - SL_TAG], rdx
    INCREF_VAL rax, rdx         ; ours from here on

    cmp qword [rbp - SL_TAG], TAG_PTR
    jne .sl_plain               ; an immediate is never a descriptor

    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel staticmethod_type]
    cmp rcx, rdx
    je .sl_staticmethod
    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    je .sl_classmethod
    lea rdx, [rel property_type]
    cmp rcx, rdx
    je .sl_property
    lea rdx, [rel func_type]
    cmp rcx, rdx
    je .sl_bind_obj
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    je .sl_bind_obj

.sl_plain:
    mov rax, [rbp - SL_ATTR]
    mov rdx, [rbp - SL_TAG]
    leave
    ret

;; --- a function, bound to __self__ unless __self__ IS the class -------------
.sl_bind_obj:
    mov rsi, [rbp - SL_OBJ]
    test rsi, rsi
    jz .sl_plain                ; super(B): no instance to bind to
    cmp rsi, [rbp - SL_OBJTYPE]
    je .sl_plain                ; super(B, B).m is A.m, not a bound method
    mov rdi, [rbp - SL_ATTR]
    call method_new             ; INCREFs both
    mov [rbp - SL_BIND], rax
    mov rdi, [rbp - SL_ATTR]
    call obj_decref             ; drop the reference the walk took
    mov rax, [rbp - SL_BIND]
    mov edx, TAG_PTR
    leave
    ret

;; --- a staticmethod, unwrapped ---------------------------------------------
.sl_staticmethod:
    mov rcx, [rax + PyStaticMethodObject.sm_callable]
    mov [rbp - SL_BIND], rcx
    mov rdi, rcx
    call obj_incref
    mov rdi, [rbp - SL_ATTR]
    call obj_decref
    mov rax, [rbp - SL_BIND]
    mov edx, TAG_PTR
    leave
    ret

;; --- a classmethod, bound to the class -------------------------------------
.sl_classmethod:
    mov rdi, [rax + PyClassMethodObject.cm_callable]
    mov rsi, [rbp - SL_OBJTYPE]
    call method_new             ; INCREFs both
    mov [rbp - SL_BIND], rax
    mov rdi, [rbp - SL_ATTR]
    call obj_decref
    mov rax, [rbp - SL_BIND]
    mov edx, TAG_PTR
    leave
    ret

;; --- a property, run --------------------------------------------------------
.sl_property:
    mov rdi, rax
    mov rsi, [rbp - SL_OBJ]
    call property_descr_get     ; -> (rax, rdx), owned
    mov [rbp - SL_BIND], rax
    mov [rbp - SL_TYPE], rdx    ; SL_TYPE is spent; reuse it for the tag
    mov rdi, [rbp - SL_ATTR]
    call obj_decref
    mov rax, [rbp - SL_BIND]
    mov rdx, [rbp - SL_TYPE]
    leave
    ret

.sl_absent:
    mov rcx, [rbp - SL_ABSENT]
    mov qword [rcx], 1
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC super_lookup

;; ============================================================================
;; super_frame_class() -> rax = the __class__ cell's contents, borrowed, or 0
;;                        with a RuntimeError pending;
;;                        rdx = the first positional argument, borrowed
;;
;; What the zero-argument form reads out of the calling frame.  A builtin has
;; no Python frame of its own, so eval_saved_r12 is the method that wrote
;; `super()` -- the same frame sys._getframe() hands back at depth 0.
;;
;; The compiler guarantees both halves: sym_note_super records a use of
;; __class__ in any function inside a class body that mentions `super`, so the
;; free variable exists even when nothing reads it, and type_from_parts fills
;; the cell through __classcell__ once the class is built.  The checks are
;; still made, because a function lifted out of its class reaches here too.
;; ============================================================================
SFC_FRAME_P equ 8
SFC_CODE    equ 16
SFC_FRAME   equ 32              ; 16 used + 16 pad = 32, 16-aligned
DEF_FUNC_LOCAL super_frame_class, SFC_FRAME
    push rbx
    push r12
    mov rax, [rel eval_saved_r12]
    test rax, rax
    jz .sfc_no_args
    mov [rbp - SFC_FRAME_P], rax
    mov rcx, [rax + PyFrame.code]
    test rcx, rcx
    jz .sfc_no_args
    mov [rbp - SFC_CODE], rcx
    cmp dword [rcx + PyCodeObject.co_argcount], 0
    je .sfc_no_args

    ; Find __class__ among the free variables.
    mov rsi, [rcx + PyCodeObject.co_localsplusnames]
    test rsi, rsi
    jz .sfc_no_cell
    mov r8, [rcx + PyCodeObject.co_localspluskinds]
    test r8, r8
    jz .sfc_no_cell
    mov r9, [rsi + PyTupleObject.ob_item]
    mov r10, [rsi + PyTupleObject.ob_size]
    xor rbx, rbx                ; the slot index, across the strcmp calls
.sfc_scan:
    cmp rbx, r10
    jge .sfc_no_cell
    cmp rbx, [r8 + PyBytesObject.ob_size]
    jae .sfc_no_cell
    movzx eax, byte [r8 + PyBytesObject.data + rbx]
    test eax, CO_FAST_FREE
    jz .sfc_scan_next
    mov rax, [r9 + rbx*8]
    test rax, rax
    jz .sfc_scan_next
    mov r12, r9                 ; the names array, across the call
    push r8
    push r10
    lea rdi, [rax + PyStrObject.data]
    lea rsi, [rel sup_class_name]
    call sup_streq
    pop r10
    pop r8
    mov r9, r12
    test eax, eax
    jz .sfc_found
.sfc_scan_next:
    inc rbx
    jmp .sfc_scan

.sfc_found:
    mov rax, [rbp - SFC_FRAME_P]
    mov rcx, [rax + PyFrame.localsplus + rbx*8]
    test rcx, rcx
    jz .sfc_empty_cell
    mov rcx, [rcx + PyCellObject.ob_ref]
    test rcx, rcx
    jz .sfc_empty_cell
    mov rdx, [rax + PyFrame.localsplus]     ; the first positional argument
    test rdx, rdx
    jz .sfc_arg_deleted
    mov rax, rcx
    pop r12
    pop rbx
    leave
    ret

.sfc_no_args:
    SET_EXC exc_RuntimeError_type, "super(): no arguments"
    jmp .sfc_fail
.sfc_no_cell:
    SET_EXC exc_RuntimeError_type, "super(): __class__ cell not found"
    jmp .sfc_fail
.sfc_empty_cell:
    SET_EXC exc_RuntimeError_type, "super(): empty __class__ cell"
    jmp .sfc_fail
.sfc_arg_deleted:
    SET_EXC exc_RuntimeError_type, "super(): arg[0] deleted"
.sfc_fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC super_frame_class

;; ============================================================================
;; super_construct(rdi = super_type, rsi = args, rdx = nargs) -> Value
;;
;; tp_new, which is where a type's constructor goes: tp_call on a type is what
;; makes that type's INSTANCES callable, and a super object is not callable.
;; ============================================================================
SN_TYPE   equ 8
SN_OBJ    equ 16
SN_OBJTY  equ 24
SN_SELF   equ 32
SN_FRAME  equ 48                ; 32 used + 16 pad = 48, 16-aligned
global super_construct
DEF_FUNC super_construct, SN_FRAME
    mov qword [rbp - SN_OBJ], 0
    mov qword [rbp - SN_OBJTY], 0
    cmp rdx, 0
    je .sn_zero_arg
    cmp rdx, 1
    je .sn_one_arg
    cmp rdx, 2
    je .sn_two_arg
    ; "...got 3" -- render the count, which is what CPython reports.
    lea rcx, [rel sup_msg_buf + SUP_MSG_MAX]
    mov byte [rcx], 0
    mov rax, rdx
    mov r8, 10
.sn_digits:
    xor edx, edx
    div r8
    add dl, '0'
    dec rcx
    mov [rcx], dl
    test rax, rax
    jnz .sn_digits
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel sup_msg_nargs]
    mov rdx, rcx
    call sup_error
    jmp .sn_fail

.sn_one_arg:
    ; super(B) -- unbound.  Useless until it is bound, but CPython builds it.
    mov rax, [rsi]
    mov [rbp - SN_TYPE], rax
    jmp .sn_check_type

.sn_two_arg:
    mov rax, [rsi]
    mov [rbp - SN_TYPE], rax
    mov rax, [rsi + 8]
    mov [rbp - SN_OBJ], rax
    jmp .sn_check_type

.sn_zero_arg:
    call super_frame_class
    test rax, rax
    jz .sn_fail
    mov [rbp - SN_TYPE], rax
    mov [rbp - SN_OBJ], rdx

.sn_check_type:
    ; The first argument has to be a class; the object is checked against it.
    mov rax, [rbp - SN_TYPE]
    V_TEST_PTR rax, rcx
    ja .sn_bad_type             ; NULL or an immediate is not a class
    mov rcx, [rax + PyObject.ob_type]
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .sn_bad_type

    mov rax, [rbp - SN_OBJ]
    test rax, rax
    jz .sn_alloc                ; super(B): no object, no __self_class__
    V_TEST_PTR rax, rcx
    ja .sn_bad_obj
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .sn_none_obj
    mov rdi, [rbp - SN_TYPE]
    mov rsi, rax
    call super_check
    test rax, rax
    jz .sn_fail                 ; super_check set the TypeError
    mov [rbp - SN_OBJTY], rax
    jmp .sn_alloc

.sn_none_obj:
    ; CPython treats super(B, None) as the unbound form.
    mov qword [rbp - SN_OBJ], 0

.sn_alloc:
    mov edi, PySuperObject_size
    lea rsi, [rel super_type]
    call gc_alloc               ; ob_refcnt = 1, ob_type set
    mov [rbp - SN_SELF], rax
    mov rcx, [rbp - SN_TYPE]
    mov [rax + PySuperObject.su_type], rcx
    mov rcx, [rbp - SN_OBJ]
    mov [rax + PySuperObject.su_obj], rcx
    mov rcx, [rbp - SN_OBJTY]
    mov [rax + PySuperObject.su_obj_type], rcx

    mov rdi, [rbp - SN_TYPE]
    call obj_incref
    mov rdi, [rbp - SN_OBJ]
    test rdi, rdi
    jz .sn_no_obj_ref
    call obj_incref
.sn_no_obj_ref:
    mov rdi, [rbp - SN_OBJTY]
    test rdi, rdi
    jz .sn_no_objty_ref
    call obj_incref
.sn_no_objty_ref:
    ; gc_track only now, for the reason code_new gives: tracking can trigger a
    ; collection, and a half-filled object would be traversed by it.
    mov rdi, [rbp - SN_SELF]
    call gc_track

    mov rax, [rbp - SN_SELF]
    mov edx, TAG_PTR
    leave
    ret

.sn_bad_type:
    mov rdi, [rbp - SN_TYPE]
    call value_type             ; NULL for an immediate, which prints nothing
    test rax, rax
    jz .sn_bad_type_bare
    mov rdx, [rax + PyTypeObject.tp_name]
    jmp .sn_bad_type_msg
.sn_bad_type_bare:
    xor edx, edx
.sn_bad_type_msg:
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel sup_msg_argtype]
    call sup_error
    jmp .sn_fail
.sn_bad_obj:
    SET_EXC exc_TypeError_type, "super(type, obj): obj must be an instance or subtype of type"
.sn_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC super_construct

;; ============================================================================
;; super_getattr(rdi = the super object, rsi = the name)
;;   -> rax = payload, rdx = tag; (0, 0) for "not here, try the generic path"
;;
;; tp_getattr.  The three attributes of its own answer first -- CPython keeps
;; them as members, and `s.__self__` must not be looked for in the MRO -- and
;; everything else is super_lookup.  An unbound super has no __self_class__
;; and so nothing to search, which is what makes `super(B).__class__` answer
;; `super` rather than something out of B's bases.
;; ============================================================================
SG_SELF  equ 8
SG_NAME  equ 16
SG_ABS   equ 24
SG_FRAME equ 32                 ; 24 used + 8 pad = 32, 16-aligned
global super_getattr
DEF_FUNC super_getattr, SG_FRAME
    mov [rbp - SG_SELF], rdi
    mov [rbp - SG_NAME], rsi

    lea rdi, [rsi + PyStrObject.data]
    lea rsi, [rel sup_a_self]
    call sup_streq
    test eax, eax
    jz .sg_self

    mov rdi, [rbp - SG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    lea rsi, [rel sup_a_thisclass]
    call sup_streq
    test eax, eax
    jz .sg_thisclass

    mov rdi, [rbp - SG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    lea rsi, [rel sup_a_selfclass]
    call sup_streq
    test eax, eax
    jz .sg_selfclass

    ; An ordinary attribute: search the MRO past __thisclass__.
    mov rax, [rbp - SG_SELF]
    mov rdx, [rax + PySuperObject.su_obj_type]
    test rdx, rdx
    jz .sg_absent               ; unbound: nothing to search
    mov rdi, [rax + PySuperObject.su_type]
    mov rsi, [rax + PySuperObject.su_obj]
    mov rcx, [rbp - SG_NAME]
    lea r8, [rbp - SG_ABS]
    call super_lookup
    leave
    ret

.sg_self:
    mov rax, [rbp - SG_SELF]
    mov rax, [rax + PySuperObject.su_obj]
    test rax, rax
    jz .sg_none
    jmp .sg_one
.sg_thisclass:
    mov rax, [rbp - SG_SELF]
    mov rax, [rax + PySuperObject.su_type]
    jmp .sg_one
.sg_selfclass:
    mov rax, [rbp - SG_SELF]
    mov rax, [rax + PySuperObject.su_obj_type]
    test rax, rax
    jz .sg_none
    jmp .sg_one
.sg_none:
    lea rax, [rel none_singleton]
.sg_one:
    INCREF rax
    mov edx, TAG_PTR
    leave
    ret

.sg_absent:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC super_getattr

;; ============================================================================
;; super_repr(rdi = the super object) -> rax = PyStrObject*, rdx = TAG_PTR
;;
;; CPython's two forms: "<super: <class 'B'>, <B object>>" when it is bound,
;; and "<super: <class 'B'>, NULL>" when it is not.  Both name types, and a
;; type's name is a C string in the table, so this never runs Python.
;; ============================================================================
SR_SELF  equ 8
SR_LEN   equ 16
SR_BUF   equ 528
SR_FRAME equ 544                ; 528 used + 16 pad = 544, 16-aligned
DEF_FUNC_LOCAL super_repr, SR_FRAME
    push rbx
    push r12
    mov [rbp - SR_SELF], rdi
    lea rbx, [rbp - SR_BUF]
    xor r12d, r12d

    CSTRING rsi, "<super: <class '"
    call .sr_append

    mov rax, [rbp - SR_SELF]
    mov rax, [rax + PySuperObject.su_type]
    test rax, rax
    jz .sr_type_null
    mov rsi, [rax + PyTypeObject.tp_name]
    jmp .sr_type_name
.sr_type_null:
    CSTRING rsi, "NULL"
.sr_type_name:
    call .sr_append

    CSTRING rsi, "'>, "
    call .sr_append

    mov rax, [rbp - SR_SELF]
    mov rax, [rax + PySuperObject.su_obj_type]
    test rax, rax
    jz .sr_obj_null
    CSTRING rsi, "<"
    call .sr_append
    mov rax, [rbp - SR_SELF]
    mov rax, [rax + PySuperObject.su_obj_type]
    mov rsi, [rax + PyTypeObject.tp_name]
    call .sr_append
    CSTRING rsi, " object>>"
    call .sr_append
    jmp .sr_build
.sr_obj_null:
    CSTRING rsi, "NULL>"
    call .sr_append

.sr_build:
    mov byte [rbx + r12], 0
    mov rdi, rbx
    call str_from_cstr_heap
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

;; .sr_append(rsi = a C string) -- copies it into the buffer at r12, stopping
;; short of the end.  A tp_name is a literal in an assembled table, so the cap
;; is a guard rather than a case that happens.
.sr_append:
    movzx eax, byte [rsi]
    test al, al
    jz .sr_append_done
    cmp r12, SR_BUF - 16
    jae .sr_append_done
    mov [rbx + r12], al
    inc r12
    inc rsi
    jmp .sr_append
.sr_append_done:
    ret
END_FUNC super_repr

;; ============================================================================
;; super_dealloc / super_traverse / super_clear
;;
;; All three fields are strong: an unbound super has only the first, and a
;; bound one holds its instance, which is how a super stored on that instance
;; makes a cycle the collector has to be able to see.
;; ============================================================================
DEF_FUNC_LOCAL super_dealloc
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PySuperObject.su_type]
    test rdi, rdi
    jz .sd_no_type
    call obj_decref
.sd_no_type:
    mov rdi, [rbx + PySuperObject.su_obj]
    test rdi, rdi
    jz .sd_no_obj
    call obj_decref
.sd_no_obj:
    mov rdi, [rbx + PySuperObject.su_obj_type]
    test rdi, rdi
    jz .sd_no_objty
    call obj_decref
.sd_no_objty:
    mov rdi, rbx
    call gc_dealloc
    pop rbx
    leave
    ret
END_FUNC super_dealloc

global super_traverse
DEF_FUNC super_traverse
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    mov rdi, [rbx + PySuperObject.su_type]
    VISIT_PTR rdi
    mov rdi, [rbx + PySuperObject.su_obj]
    VISIT_PTR rdi
    mov rdi, [rbx + PySuperObject.su_obj_type]
    VISIT_PTR rdi
    pop r12
    pop rbx
    leave
    ret
END_FUNC super_traverse

global super_clear
DEF_FUNC super_clear
    push rbx
    push r12
    mov rbx, rdi
    xor r12d, r12d
.scl_loop:
    cmp r12d, 3
    jge .scl_done
    mov rax, r12
    shl rax, 3
    mov rdi, [rbx + PySuperObject.su_type + rax]
    mov qword [rbx + PySuperObject.su_type + rax], 0
    test rdi, rdi
    jz .scl_next
    call obj_decref
.scl_next:
    inc r12d
    jmp .scl_loop
.scl_done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC super_clear

section .data
align 8
global super_type
super_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq sup_name_str             ; tp_name
    dq PySuperObject_size       ; tp_basicsize
    dq super_dealloc            ; tp_dealloc
    dq super_repr               ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call  (a super is not callable)
    dq super_getattr            ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq super_construct          ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC        ; tp_flags
    dq 0                        ; tp_bases
    dq super_traverse           ; tp_traverse
    dq super_clear              ; tp_clear
    dq 0                        ; tp_dictoffset
    dq 0                        ; tp_tailslots

sup_msg_nargs:     db "super() expected at most 2 arguments, got ", 1, 0
sup_msg_noattr:    db "'super' object has no attribute '", 1, "'", 0
sup_msg_argtype:   db "super() argument 1 must be a type, not ", 1, 0

sup_name_str:      db "super", 0
sup_class_name:    db "__class__", 0
sup_a_self:        db "__self__", 0
sup_a_thisclass:   db "__thisclass__", 0
sup_a_selfclass:   db "__self_class__", 0

section .bss
sup_msg_buf: resb SUP_MSG_MAX + 24      ; the message, then room for the digits
