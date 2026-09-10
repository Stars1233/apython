; pyo/method.asm - the bound method
;
; A bound method is the pair (function, self) that an attribute lookup makes
; when it finds a function on the class: `c.m` is not the function, it is a
; two-word object that remembers the instance and prepends it to every call.
; instance_getattr and type_getattr_meta build them; this file is what they
; build.
;
; It came out of class.asm, which held the metatype, the instance and this,
; and was over the size a hand-written file in this tree is allowed.  The seam
; is clean: nothing here touches a type's layout, and the only thing class.asm
; still asks of it is method_new.

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern ap_free
extern gc_alloc
extern gc_track
extern gc_dealloc
extern obj_incref
extern obj_hash
extern object_hash
extern obj_decref
extern obj_dealloc
extern obj_repr
extern obj_repr_address
extern rbt_append_cstr
extern str_from_cstr
extern str_new_heap
extern func_getattr
extern func_type
extern builtin_func_type
extern type_type
extern kw_names_pending
extern none_singleton
extern exc_TypeError_type
extern raise_exception
extern raise_type_error_counted

global method_new
global method_construct
global method_type
global method_traverse
global method_clear

section .text

;; ============================================================================
;; method_new(func, self) -> PyMethodObject*
;; Create a bound method wrapping func+self.
;; rdi = func (callable), rsi = self (instance)
;; ============================================================================
DEF_FUNC method_new
    push rbx
    push r12

    mov rbx, rdi                ; func
    mov r12, rsi                ; self

    mov edi, PyMethodObject_size
    lea rsi, [rel method_type]
    call gc_alloc
    ; ob_refcnt=1, ob_type set by gc_alloc
    mov [rax + PyMethodObject.im_func], rbx
    mov [rax + PyMethodObject.im_self], r12

    ; INCREF func and self.  im_self is a Value, not necessarily a pointer:
    ; binding a builtin method to an immediate int is what `getattr(5,
    ; "bit_length")` asks for, and an unguarded incref would treat the encoded
    ; number as an address.
    push rax
    mov rdi, rbx
    call obj_incref
    INCREF_V r12, rax

    ; Track in GC
    mov rdi, [rsp]
    call gc_track
    pop rax

    pop r12
    pop rbx
    leave
    ret
END_FUNC method_new

;; ============================================================================
;; method_construct(rdi = the class, rsi = args, rdx = nargs)
;;   -> rax = the bound method, edx = TAG_PTR
;;
;; tp_new for method_type -- `types.MethodType(func, obj)`.  There was none,
;; so type_call fell through to .normal_type_call and TC_REFUSE_EXTRA_ARGS
;; answered "method() takes no arguments" for every call; contextlib's
;; `return MethodType(cm_exit, cm)` is the line that put twenty-four of
;; CPython's test modules on the floor.
;;
;; The class is ignored rather than dispatched on: method_type carries
;; TYPE_FLAG_FINAL, as CPython's does, so it can only ever be method_type.
;; ============================================================================
MTN_NARGS equ 8
MTN_FRAME equ 24            ; + 1 push, 16-aligned
DEF_FUNC method_construct, MTN_FRAME
    push rbx
    mov rbx, rsi                ; args
    mov [rbp - MTN_NARGS], rdx

    ; Keyword values arrive in the same array with their names in
    ; kw_names_pending, so a bare arity check would count them as positional.
    ; CPython's method_new refuses them outright, and so does this.
    mov rax, [rel kw_names_pending]
    test rax, rax
    jnz .mtn_kwargs

    cmp rdx, 2
    jne .mtn_arity

    mov rdi, [rbx]              ; args[0], the function
    V_TEST_PTR rdi, rax
    ja .mtn_not_callable        ; an immediate has no tp_call to read
    test rdi, rdi
    jz .mtn_not_callable
    mov rax, [rdi + PyObject.ob_type]
    cmp qword [rax + PyTypeObject.tp_call], 0
    je .mtn_not_callable

    ; self is a Value: binding to an immediate int is legitimate, and
    ; method_new's INCREF_V is already written for it.  None is not -- CPython
    ; names it, because `MethodType(f, None)` is how an unbound method used to
    ; be spelled and silence there would be a trap.
    mov rsi, [rbx + 8]
    test rsi, rsi
    jz .mtn_none
    lea rax, [rel none_singleton]
    cmp rsi, rax
    je .mtn_none

    call method_new
    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.mtn_kwargs:
    mov qword [rel kw_names_pending], 0   ; consumed, however this ends
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "method() takes no keyword arguments"
    call raise_exception

.mtn_arity:
    mov rsi, [rbp - MTN_NARGS]
    CSTRING rdi, "method expected 2 arguments, got "
    xor edx, edx
    jmp raise_type_error_counted

.mtn_not_callable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "first argument must be callable"
    call raise_exception

.mtn_none:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "instance must not be None"
    call raise_exception
END_FUNC method_construct

;; ============================================================================
;; method_call(self_method, args, nargs) -> rax = Value
;; Call a bound method: prepend im_self to args, dispatch to im_func's tp_call.
;; rdi = PyMethodObject*, rsi = args, rdx = nargs
;; ============================================================================
;; The argument array method_call has to build -- im_self and then the
;; caller's arguments -- lives in this frame for the sizes real code uses, and
;; in the allocator above them.  A malloc and a free per call is what a bound
;; method held in a variable used to cost, and `sorted(key=obj.method)` and a
;; stored callback are both written that way.
;;
;; The cap is not caution about frame size: `m(*[0] * 10**7)` must stay a
;; MemoryError rather than become a stack overflow, and a dynamic `sub rsp`
;; would also be invisible to lint's alignment check.
MC_FREE  equ 8              ; the heap array to release, or 0
MC_BUF   equ 160            ; 19 slots at [rbp-160, rbp-8)
MC_FRAME equ 160            ; + 4 pushes = 192, 16-aligned
MC_MAX_STACK equ 17         ; self plus this many still fits
DEF_FUNC_LOCAL method_call, MC_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; method obj
    mov r12, rsi                ; original args
    mov r13, rdx                ; original nargs
    mov qword [rbp - MC_FREE], 0

    cmp rdx, MC_MAX_STACK
    ja .mc_big
    lea r14, [rbp - MC_BUF]
    jmp .mc_have_args
.mc_big:
    ; (nargs+1) Values, one word each.  This said `shl rdi, 4` -- two words per
    ; argument -- left over from the fat (payload, tag) representation, so it
    ; asked for twice the memory it went on to use.  The copy loop below
    ; already strides by 8.
    lea rdi, [rdx + 1]
    shl rdi, 3
    call ap_malloc
    mov r14, rax                ; new args array
    mov [rbp - MC_FREE], rax
.mc_have_args:

    ; new_args[0] = im_self (a pointer is its own Value)
    mov rcx, [rbx + PyMethodObject.im_self]
    mov [r14], rcx

    ; Copy original args to new_args[1..]
    xor ecx, ecx
.mc_copy:
    cmp rcx, r13
    jge .mc_copy_done
    mov rax, rcx
    shl rax, 3                  ; one Value per slot
    mov rdx, [r12 + rax]
    lea r9, [rcx + 1]
    shl r9, 3                   ; dest slot (offset by one for self)
    mov [r14 + r9], rdx
    inc rcx
    jmp .mc_copy
.mc_copy_done:

    ; Call im_func's tp_call(im_func, new_args, nargs+1)
    mov rdi, [rbx + PyMethodObject.im_func]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    mov rsi, r14
    lea rdx, [r13 + 1]
    call rax
    V_UNPACK rax, rdx           ; tp_call returns a Value
    push rax                    ; save result payload
    push rdx                    ; save result tag

    ; Free the temp args array, if it was not this frame's.
    mov rdi, [rbp - MC_FREE]
    test rdi, rdi
    jz .mc_no_free
    call ap_free
.mc_no_free:

    pop rdx                     ; restore result tag
    pop rax                     ; restore result payload
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
END_FUNC method_call

;; ============================================================================
;; method_dealloc(PyObject *self)
;; Free a bound method, DECREF func and self.
;; ============================================================================
DEF_FUNC_LOCAL method_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx

    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_func]
    call obj_decref
    mov rdi, [rbx + PyMethodObject.im_self]
    XDECREF_V rdi, rsi
    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC method_dealloc

;; ============================================================================
;; method_getattr(rdi = bound method, rsi = name str) -> rax = Value, or NULL
;;
;; `__self__` and `__func__` are the bound method's own two attributes and
;; are answered here; everything else is the underlying function's.  Both
;; used to be delegated, and `"x".upper.__self__` reached func_getattr with a
;; builtin in hand and came back with something that was not an object --
;; a SIGSEGV on printing it.
;; ============================================================================
MG_SELF  equ 8
MG_NAME  equ 16
MG_FRAME equ 32             ; + 0 pushes = 32, 16-aligned
DEF_FUNC method_getattr, MG_FRAME
    mov [rbp - MG_SELF], rdi
    mov [rbp - MG_NAME], rsi

    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__self__"
    extern ap_strcmp
    call ap_strcmp
    test eax, eax
    jz .mg_self

    ; `__func__` is the method's own too, and delegating it asked the function
    ; for an attribute of its own name -- which a plain function does not
    ; have.  inspect.signature() of a bound method is written on it.
    mov rsi, [rbp - MG_NAME]
    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__func__"
    call ap_strcmp
    test eax, eax
    jz .mg_func

    ; The underlying callable decides which getattr answers: func_getattr
    ; reads a PyFunctionObject's own dict, and a builtin does not have one.
    mov rdi, [rbp - MG_SELF]
    mov rdi, [rdi + PyMethodObject.im_func]
    mov rsi, [rbp - MG_NAME]    ; the comparison above clobbered it
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel builtin_func_type]
    cmp rax, rcx
    je .mg_builtin
    extern func_getattr
    call func_getattr           ; already returns a Value
    leave
    ret
.mg_builtin:
    extern builtin_func_getattr
    call builtin_func_getattr
    leave
    ret

.mg_self:
    ; im_self is a Value, so an immediate receiver comes back as itself.
    mov rax, [rbp - MG_SELF]
    mov rax, [rax + PyMethodObject.im_self]
    INCREF_V rax, rcx
    leave
    ret

.mg_func:
    mov rax, [rbp - MG_SELF]
    mov rax, [rax + PyMethodObject.im_func]
    test rax, rax
    jz .mg_no_func
    mov rdi, rax
    push rax
    extern obj_incref
    call obj_incref
    pop rax
.mg_no_func:
    leave
    ret
END_FUNC method_getattr


;; ============================================================================
;; method_repr(PyMethodObject *self) -> str
;; "<bound method Qual of <self repr>>".  Bound methods had no tp_repr at all,
;; so printing one produced nothing printable.
;; ============================================================================
MR_SELF  equ 8
MR_LEN   equ 16
MR_BUF   equ 1048
MR_FRAME equ 1056           ; + 2 pushes = 1072
DEF_FUNC method_repr, MR_FRAME
    push rbx
    push r12
    mov [rbp - MR_SELF], rdi
    lea rbx, [rbp - MR_BUF]
    xor r12d, r12d

    ; A builtin bound to a CLASS is CPython's fourth descriptor form:
    ; "<built-in method from_bytes of type object at 0x...>".  int.from_bytes
    ; and its three siblings are builtins wrapped in a classmethod object,
    ; and binding one produces a bound method here where CPython produces a
    ; builtin_function_or_method -- which is a divergence of its own, and one
    ; DIVERGENCES.md records; the repr is not.
    mov rax, [rdi + PyMethodObject.im_func]
    test rax, rax
    jz .mr_ordinary
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    jne .mr_ordinary
    cmp qword [rax + PyBuiltinObject.func_kind], BUILTIN_KIND_ON_TYPE
    je mr_builtin_classmethod
    ; A builtin bound to an INSTANCE is CPython's third form:
    ; "<built-in method upper of str object at 0x...>".  It read as
    ; "<bound method upper of 'x'>" -- which is the form for a Python
    ; function, and the wrong one for a method the interpreter supplies.
    jmp mr_builtin_instance

.mr_ordinary:
    CSTRING rsi, "<bound method "
.mr_pre:
    movzx eax, byte [rsi]
    test al, al
    jz .mr_qual
    inc rsi
    mov [rbx + r12], al
    inc r12
    jmp .mr_pre

.mr_qual:
    ; the function's __qualname__, or its __name__ if it has none
    mov rax, [rbp - MR_SELF]
    mov rax, [rax + PyMethodObject.im_func]
    test rax, rax
    jz .mr_of
    ; A qualified name is what CPython shows; the code object carries one,
    ; and a builtin has only its own name field.
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel func_type]
    cmp rcx, rdx
    jne .mr_builtin_name
    mov rdi, [rax + PyFuncObject.func_code]
    test rdi, rdi
    jz .mr_of
    mov rdi, [rdi + PyCodeObject.co_qualname]
    test rdi, rdi
    jnz .mr_copy_name
    mov rax, [rbp - MR_SELF]
    mov rax, [rax + PyMethodObject.im_func]
    mov rdi, [rax + PyFuncObject.func_name]
    test rdi, rdi
    jz .mr_of
    jmp .mr_copy_name
.mr_builtin_name:
    mov rdi, [rax + PyBuiltinObject.func_name]
    test rdi, rdi
    jz .mr_of
.mr_copy_name:
    mov rcx, [rdi + PyStrObject.ob_size]
    lea rsi, [rdi + PyStrObject.data]
    xor edx, edx
.mr_name_loop:
    cmp rdx, rcx
    jge .mr_of
    cmp r12, MR_BUF - 64
    jae .mr_of
    movzx eax, byte [rsi + rdx]
    mov [rbx + r12], al
    inc r12
    inc rdx
    jmp .mr_name_loop

.mr_of:
    CSTRING rsi, " of "
.mr_of_loop:
    movzx eax, byte [rsi]
    test al, al
    jz .mr_self_repr
    inc rsi
    mov [rbx + r12], al
    inc r12
    jmp .mr_of_loop

.mr_self_repr:
    mov rax, [rbp - MR_SELF]
    mov rdi, [rax + PyMethodObject.im_self]
    test rdi, rdi
    jz .mr_close
    ; obj_repr takes a Value, which is what im_self holds.
    mov [rbp - MR_LEN], r12
    extern obj_repr
    call obj_repr
    V_UNPACK rax, rdx
    test rax, rax
    jz .mr_close
    mov r12, [rbp - MR_LEN]
    mov rcx, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor edx, edx
.mr_self_loop:
    cmp rdx, rcx
    jge .mr_self_done
    cmp r12, MR_BUF - 8
    jae .mr_self_done
    push rax
    movzx eax, byte [rsi + rdx]
    mov [rbx + r12], al
    pop rax
    inc r12
    inc rdx
    jmp .mr_self_loop
.mr_self_done:
    mov rdi, rax
    call obj_decref

.mr_close:
    mov byte [rbx + r12], '>'
    inc r12
    mov rdi, rbx
    mov rsi, r12
    extern str_new_heap
    call str_new_heap
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
;; The classmethod form, out of line: it shares nothing with the loop above
;; but the frame it is written into.  rbp is method_repr's; rbx and r12 are
;; still pushed, and the epilogue below is method_repr's own.
;; ============================================================================
;; mr_builtin_instance -> rax = the str, an out-of-line arm of method_repr
;;
;; "<built-in method NAME of TYPE object at 0xADDR>", where TYPE is the
;; receiver's type -- not the descriptor's owner, so a subclass's instance
;; names the subclass, as CPython's does.
;; ============================================================================
mr_builtin_instance:
    lea rdi, [rbp - MR_BUF]
    CSTRING rsi, "<built-in method "
    call rbt_append_cstr
    mov rdi, rax
    mov rcx, [rbp - MR_SELF]
    mov rcx, [rcx + PyMethodObject.im_func]
    mov rsi, [rcx + PyBuiltinObject.func_name]
    test rsi, rsi
    jz .mrbi_no_name
    add rsi, PyStrObject.data
    call rbt_append_cstr
.mrbi_no_name:
    mov rdi, rax
    CSTRING rsi, " of "
    call rbt_append_cstr
    mov rdi, rax
    mov rcx, [rbp - MR_SELF]
    mov rcx, [rcx + PyMethodObject.im_self]
    push rdi
    mov rdi, rcx
    extern value_type
    call value_type
    pop rdi
    test rax, rax
    jz .mrbi_no_type
    mov rsi, [rax + PyTypeObject.tp_name]
    call rbt_append_cstr
.mrbi_no_type:
    mov rdi, rax
    CSTRING rsi, " object"
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - MR_SELF]
    mov rsi, [rsi + PyMethodObject.im_self]
    ; The address CPython prints is what id() answers, and for an immediate
    ; id() answers the NUMBER -- so a bound method of 7 must say 0x7 and not
    ; the encoded word, whose high bits are the tag.
    V_TEST_PTR rsi, rax
    jbe .mrbi_addr
    V_TO_I64 rsi
.mrbi_addr:
    call obj_repr_address       ; writes " at 0xADDR>"
    lea rdi, [rbp - MR_BUF]
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret

;; ============================================================================
;; mr_builtin_classmethod -> rax = the str, an out-of-line arm of method_repr
;;
;; "<built-in method NAME of type object at 0xADDR>", for a builtin bound to
;; a CLASS rather than to an instance.
;; ============================================================================
mr_builtin_classmethod:
    lea rdi, [rbp - MR_BUF]
    CSTRING rsi, "<built-in method "
    call rbt_append_cstr
    mov rdi, rax
    mov rcx, [rbp - MR_SELF]
    mov rcx, [rcx + PyMethodObject.im_func]
    mov rsi, [rcx + PyBuiltinObject.func_name]
    test rsi, rsi
    jz .mrbc_no_name
    add rsi, PyStrObject.data
    call rbt_append_cstr
.mrbc_no_name:
    mov rdi, rax
    CSTRING rsi, " of type object"
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - MR_SELF]
    mov rsi, [rsi + PyMethodObject.im_self]
    call obj_repr_address       ; writes " at 0xADDR>"
    lea rdi, [rbp - MR_BUF]
    call str_from_cstr
    pop r12
    pop rbx
    leave
    ret

END_FUNC method_repr

;; ============================================================================
;; method_richcompare(left, right, op, left_tag, right_tag) -> (rax, edx)
;;
;; CPython compares im_self by IDENTITY and im_func by EQUALITY, and answers
;; only EQ and NE; every ordering is NotImplemented, which is what makes
;; `sorted(list_of_methods)` a TypeError there and here.
;;
;; im_func by equality and not by identity because two wrappers over the same
;; underlying callable need not be the same object: a builtin method reached
;; twice through the type can hand back a fresh PyBuiltinObject each time.
;;
;; Without this, `method` fell back to identity and `c.m == c.m` was False --
;; each attribute load builds a new wrapper.  Any code that keeps a callback
;; and later asks whether it already has one saw every method as new.
;; ============================================================================
MRC_LEFT  equ 8
MRC_RIGHT equ 16
MRC_OP    equ 24
MRC_FRAME equ 32            ; + 0 pushes = 32, 16-aligned
DEF_FUNC method_richcompare, MRC_FRAME
    V_UNPACK rdi, rcx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, r8            ; right Value -> (payload, tag)
    mov [rbp - MRC_LEFT], rdi
    mov [rbp - MRC_RIGHT], rsi
    mov [rbp - MRC_OP], rdx

    ; Only EQ and NE; anything else is NotImplemented, as CPython's is.
    cmp edx, PY_EQ
    je .mrc_kinds
    cmp edx, PY_NE
    jne .mrc_not_impl

.mrc_kinds:
    ; The other side has to be a method too.  A non-pointer never is, and
    ; answering False here rather than NotImplemented would stop the other
    ; operand's own __eq__ from ever being asked.
    test r8d, TAG_RC_BIT
    jz .mrc_not_impl
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel method_type]
    cmp rax, rcx
    jne .mrc_not_impl

    ; im_self by identity.
    mov rax, [rdi + PyMethodObject.im_self]
    cmp rax, [rsi + PyMethodObject.im_self]
    jne .mrc_false

    ; im_func by equality.
    mov rdi, [rdi + PyMethodObject.im_func]
    mov rsi, [rsi + PyMethodObject.im_func]
    cmp rdi, rsi
    je .mrc_true                ; the common case, and it cannot raise
    extern obj_richcompare_bool
    mov edx, PY_EQ
    call obj_richcompare_bool
    test eax, eax
    js .mrc_error               ; the comparison raised
    jz .mrc_false
    jmp .mrc_true

.mrc_true:
    cmp qword [rbp - MRC_OP], PY_EQ
    je .mrc_ret_true
    jmp .mrc_ret_false
.mrc_false:
    cmp qword [rbp - MRC_OP], PY_EQ
    je .mrc_ret_false
.mrc_ret_true:
    extern bool_true
    lea rax, [rel bool_true]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.mrc_ret_false:
    extern bool_false
    lea rax, [rel bool_false]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.mrc_not_impl:
    ; A NULL Value is NotImplemented to the caller, not failure -- the
    ; dispatcher turns it into the reflected call and then into identity.
    RET_NULL
    leave
    ret
.mrc_error:
    RET_NULL
    leave
    ret
END_FUNC method_richcompare

;; ============================================================================
;; method_hash(PyObject *self) -> int64
;;
;; CPython's: _Py_HashPointer(im_self) ^ hash(im_func).  It has to agree with
;; method_richcompare or the dict invariant breaks -- two methods that compare
;; equal must hash equal, and they do because both halves are the same
;; question.
;;
;; im_self is hashed by ADDRESS and not through its __hash__, which is the
;; half that is easy to get wrong.  It follows from richcompare comparing the
;; receiver by identity: a class that sets `__hash__ = None` is still a
;; perfectly good receiver, and `hash(u.m)` answers in CPython where hashing
;; the receiver properly would raise.  object_hash is that address hash.
;; ============================================================================
DEF_FUNC method_hash, 16    ; + 2 pushes = 32, 16-aligned
    push rbx
    push r12
    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_self]
    xor edx, edx
    call object_hash
    mov r12, rax

    mov rdi, [rbx + PyMethodObject.im_func]
    call obj_hash
    cmp rax, -1
    je .mh_out
    xor rax, r12
    ; -1 is the error signal, so a real hash never takes that value.
    cmp rax, -1
    jne .mh_out
    mov rax, -2
.mh_out:
    pop r12
    pop rbx
    leave
    ret
END_FUNC method_hash

section .data

method_name_str:    db "method", 0

; method_type - type descriptor for bound methods
align 8
global method_type
method_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq method_name_str          ; tp_name
    dq PyMethodObject_size      ; tp_basicsize
    dq method_dealloc           ; tp_dealloc
    dq method_repr              ; tp_repr
    dq method_repr              ; tp_str
    dq method_hash              ; tp_hash
    dq method_call              ; tp_call
    dq method_getattr           ; tp_getattr
    dq 0                        ; tp_setattr
    dq method_richcompare       ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq method_construct         ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC | TYPE_FLAG_FINAL      ; tp_flags
    dq 0                        ; tp_bases
    dq method_traverse                        ; tp_traverse
    dq method_clear                        ; tp_clear
    dq 0         ; tp_dictoffset
    dq 0                        ; tp_tailslots

section .text

;; ============================================================================
;; ---- method_traverse / method_clear ----
;; ============================================================================
DEF_FUNC method_traverse, 8        ; rsp 16-aligned at the call the macros below expand to
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_func]
    VISIT_PTR rdi
    mov rdi, [rbx + PyMethodObject.im_self]
    VISIT_V rdi, rsi            ; a Value: an immediate self is not an address

    pop rbx
    leave
    ret
END_FUNC method_traverse

DEF_FUNC method_clear, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyMethodObject.im_func]
    mov qword [rbx + PyMethodObject.im_func], 0
    test rdi, rdi
    jz .no_func
    call obj_decref
.no_func:
    mov rdi, [rbx + PyMethodObject.im_self]
    mov qword [rbx + PyMethodObject.im_self], 0
    XDECREF_V rdi, rsi
.no_self:

    pop rbx
    leave
    ret
END_FUNC method_clear
