; abcmod.asm - the _abc accelerator module
;
; abc.py imports eight names from here and builds ABCMeta on them.  The pure
; Python fallback it reaches for when the import fails, _py_abc, needs
; _weakrefset and so _weakref, which does not exist here -- so this module is
; what makes `import abc` work, and with it collections.abc, os, inspect,
; typing and most of what stands on them.
;
; The algorithm is CPython's, in Modules/_abc.c.  Two deliberate departures,
; both recorded in bugs.md:
;
;   * The registry and the caches hold strong references.  CPython uses weak
;     ones so a registered class can be collected; here a class that is
;     registered lives as long as the ABC does.
;
;   * Step 6 of the subclass check -- recursing into cls.__subclasses__() to
;     find a registration made on a subclass of the ABC -- is missing, because
;     types do not keep a subclass list.  Direct registrations and real
;     inheritance are both handled.
;
; State lives in the class's own tp_dict, under the names _py_abc uses, and is
; read straight out of it rather than through type_getattr: an ABC's subclass
; must not inherit its base's registry.

%include "macros.inc"
%include "object.inc"
%include "value.inc"

extern dict_new
extern dict_get
extern dict_set
extern set_new
extern set_add
extern set_contains
extern str_from_cstr_heap
extern module_new
extern builtin_func_new
extern obj_incref
extern obj_decref
extern obj_dealloc
extern obj_is_true
extern tuple_new
extern type_getattr
extern type_is_subtype
extern type_check_is_class
extern value_type
extern raise_exception
extern exc_TypeError_type
extern exc_RuntimeError_type
extern bool_true
extern bool_false
extern none_singleton
extern notimpl_singleton
extern type_type

section .text

; ----------------------------------------------------------------------------
; abc_state_get(rdi = cls, rsi = name cstr) -> rax = Value, or 0 if absent
; Reads the class's own tp_dict.  Borrowed; the caller does not own it.
; ----------------------------------------------------------------------------
AG_CLS   equ 8
AG_FRAME equ 16
DEF_FUNC_LOCAL abc_state_get, AG_FRAME
    push rbx
    mov [rbp - AG_CLS], rdi
    mov rdi, rsi
    call str_from_cstr_heap
    mov rbx, rax
    mov rax, [rbp - AG_CLS]
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .absent
    mov rsi, rbx
    call dict_get
    mov rdi, rbx
    mov rbx, rax
    call obj_decref
    mov rax, rbx
    pop rbx
    leave
    ret
.absent:
    mov rdi, rbx
    call obj_decref
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC abc_state_get

; ----------------------------------------------------------------------------
; abc_state_set(rdi = cls, rsi = name cstr, rdx = value Value)
; Stores into the class's own tp_dict, creating it if the type has none.
; ----------------------------------------------------------------------------
AS_CLS   equ 8
AS_VAL   equ 16
AS_FRAME equ 16
DEF_FUNC_LOCAL abc_state_set, AS_FRAME
    push rbx
    mov [rbp - AS_CLS], rdi
    mov [rbp - AS_VAL], rdx
    mov rdi, rsi
    call str_from_cstr_heap
    mov rbx, rax
    mov rax, [rbp - AS_CLS]
    mov rdi, [rax + PyTypeObject.tp_dict]
    test rdi, rdi
    jnz .have_dict
    call dict_new
    mov rcx, [rbp - AS_CLS]
    mov [rcx + PyTypeObject.tp_dict], rax
    mov rdi, rax
.have_dict:
    mov rsi, rbx
    mov rdx, [rbp - AS_VAL]
    call dict_set
    mov rdi, rbx
    call obj_decref
    pop rbx
    leave
    ret
END_FUNC abc_state_set

; ----------------------------------------------------------------------------
; abc_fresh_set(rdi = cls, rsi = name cstr) -> rax = the new set (borrowed)
; ----------------------------------------------------------------------------
AF_CLS   equ 8
AF_NAME  equ 16
AF_FRAME equ 16
DEF_FUNC_LOCAL abc_fresh_set, AF_FRAME
    push rbx
    mov [rbp - AF_CLS], rdi
    mov [rbp - AF_NAME], rsi
    call set_new
    mov rbx, rax
    mov rdi, [rbp - AF_CLS]
    mov rsi, [rbp - AF_NAME]
    mov rdx, rbx
    call abc_state_set          ; the dict takes its own reference
    mov rdi, rbx
    call obj_decref
    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC abc_fresh_set

; ----------------------------------------------------------------------------
; get_cache_token() -> int
; ----------------------------------------------------------------------------
DEF_FUNC abc_get_cache_token
    mov rax, [rel abc_invalidation_counter]
    V_PACK_I64 rax, rcx
    leave
    ret
END_FUNC abc_get_cache_token


; ----------------------------------------------------------------------------
; abc_getattr(rdi = object Value, rsi = name str) -> rax = Value, or 0
; Whatever tp_getattr the object's type offers, else its type's tp_dict.
; ----------------------------------------------------------------------------
AGA_OBJ  equ 8
AGA_NAME equ 16
AGA_FRAME equ 32
DEF_FUNC_LOCAL abc_getattr, AGA_FRAME
    V_TEST_PTR rdi, rax
    ja .aga_none
    test rdi, rdi
    jz .aga_none
    mov [rbp - AGA_OBJ], rdi
    mov [rbp - AGA_NAME], rsi
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_getattr]
    test rax, rax
    jz .aga_dict
    call rax
    V_UNPACK rax, rdx
    test edx, edx
    jz .aga_dict
    leave
    V_PACK rax, rdx
    ret
.aga_dict:
    mov rdi, [rbp - AGA_OBJ]
    mov rdi, [rdi + PyObject.ob_type]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .aga_none
    mov rsi, [rbp - AGA_NAME]
    call dict_get
    test rax, rax
    jz .aga_none
    INCREF_V rax, rcx
    leave
    ret
.aga_none:
    xor eax, eax
    leave
    ret
END_FUNC abc_getattr

; ----------------------------------------------------------------------------
; abc_is_abstract(rdi = value Value) -> eax 0/1
; ----------------------------------------------------------------------------
DEF_FUNC_LOCAL abc_is_abstract, 16
    push rbx
    CSTRING rsi, "__isabstractmethod__"
    push rdi
    mov rdi, rsi
    call str_from_cstr_heap
    mov rbx, rax
    pop rdi
    mov rsi, rbx
    call abc_getattr
    push rax
    mov rdi, rbx
    call obj_decref
    pop rdi
    test rdi, rdi
    jz .aia_no
    mov rbx, rdi
    call obj_is_true
    push rax
    mov rdi, rbx
    DECREF_V rdi, rcx
    pop rax
    pop rbx
    leave
    ret
.aia_no:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC abc_is_abstract

; ----------------------------------------------------------------------------
; abc_compute_abstracts(rdi = cls)
;
; __abstractmethods__ is what makes an abstract class refuse to be
; instantiated, and CPython computes it here rather than in abc.py: the names
; in the class body that are marked abstract, plus any the bases left abstract
; and this class did not override.
; ----------------------------------------------------------------------------
CA_CLS   equ 8
CA_SET   equ 16
CA_IDX   equ 24
CA_DICT  equ 32
CA_BASE  equ 40
CA_KEY   equ 48
CA_FRAME equ 64
DEF_FUNC_LOCAL abc_compute_abstracts, CA_FRAME
    push rbx
    push r12
    mov [rbp - CA_CLS], rdi
    call set_new
    mov [rbp - CA_SET], rax

    ; The class's own namespace.
    mov rax, [rbp - CA_CLS]
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jz .ca_bases
    mov [rbp - CA_DICT], rax
    mov qword [rbp - CA_IDX], 0
.ca_own_loop:
    mov rax, [rbp - CA_DICT]
    mov rcx, [rbp - CA_IDX]
    cmp rcx, [rax + PyDictObject.dk_nentries]
    jge .ca_bases
    mov rdx, [rax + PyDictObject.entries]
    imul rcx, rcx, DICT_ENTRY_SIZE
    mov rbx, [rdx + rcx + DictEntry.key]
    mov r12, [rdx + rcx + DictEntry.value]
    inc qword [rbp - CA_IDX]
    test rbx, rbx
    jz .ca_own_loop
    mov rdi, r12
    call abc_is_abstract
    test eax, eax
    jz .ca_own_loop
    mov rdi, [rbp - CA_SET]
    mov rsi, rbx
    call set_add
    jmp .ca_own_loop

.ca_bases:
    ; Anything a base left abstract and this class did not define concretely.
    ; The MRO rather than __bases__: a static type keeps no tp_bases tuple,
    ; and every entry carries its own __abstractmethods__ anyway.
    mov rbx, [rbp - CA_CLS]
.ca_base_loop:
    MRO_NEXT rbx, [rbp - CA_CLS]
    test rbx, rbx
    jz .ca_store
    mov rdi, rbx
    CSTRING rsi, "__abstractmethods__"
    call abc_state_get
    mov r12, rax
    test r12, r12
    jz .ca_base_loop
    V_TEST_PTR r12, rax
    ja .ca_base_loop
    mov qword [rbp - CA_IDX], 0
.ca_names_loop:
    mov rcx, [rbp - CA_IDX]
    cmp rcx, [r12 + PyDictObject.capacity]
    jge .ca_base_loop
    mov rdx, [r12 + PyDictObject.entries]
    shl rcx, 4                  ; SET_ENTRY_SIZE
    mov rax, [rdx + rcx + 8]    ; the key (the hash sits at +0)
    inc qword [rbp - CA_IDX]
    test rax, rax
    jz .ca_names_loop
    V_TEST_PTR rax, rcx
    ja .ca_names_loop
    mov [rbp - CA_KEY], rax
    mov rdi, [rbp - CA_CLS]
    mov rsi, rax
    call type_getattr
    V_UNPACK rax, rdx
    test edx, edx
    jz .ca_names_loop
    push rax
    push rdx
    mov rdi, rax
    V_PACK rdi, rdx
    call abc_is_abstract
    mov ecx, eax
    pop rdx
    pop rdi
    push rcx
    DECREF_VAL rdi, rdx
    pop rcx
    test ecx, ecx
    jz .ca_names_loop
    mov rdi, [rbp - CA_SET]
    mov rsi, [rbp - CA_KEY]
    call set_add
    jmp .ca_names_loop

.ca_store:
    mov rax, [rbp - CA_SET]
    extern frozenset_type
    lea rcx, [rel frozenset_type]
    mov [rax + PyObject.ob_type], rcx   ; the name set is immutable
    mov rdi, [rbp - CA_CLS]
    CSTRING rsi, "__abstractmethods__"
    mov rdx, rax
    call abc_state_set
    mov rdi, [rbp - CA_SET]
    call obj_decref
    pop r12
    pop rbx
    leave
    ret
END_FUNC abc_compute_abstracts

; ----------------------------------------------------------------------------
; _abc_init(cls) -> None
; ----------------------------------------------------------------------------
AI_CLS   equ 8
AI_FRAME equ 16
DEF_FUNC abc_init_func, AI_FRAME
    cmp rsi, 1
    jl .bad
    mov rax, [rdi]
    mov [rbp - AI_CLS], rax
    mov rdi, rax
    call type_check_is_class
    test eax, eax
    jz .bad

    mov rdi, [rbp - AI_CLS]
    CSTRING rsi, "_abc_registry"
    call abc_fresh_set
    mov rdi, [rbp - AI_CLS]
    CSTRING rsi, "_abc_cache"
    call abc_fresh_set
    mov rdi, [rbp - AI_CLS]
    CSTRING rsi, "_abc_negative_cache"
    call abc_fresh_set
    mov rdi, [rbp - AI_CLS]
    CSTRING rsi, "_abc_negative_cache_version"
    mov rdx, [rel abc_invalidation_counter]
    V_PACK_I64 rdx, rcx
    call abc_state_set

    mov rdi, [rbp - AI_CLS]
    call abc_compute_abstracts

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "_abc_init() requires a class"
    call raise_exception
END_FUNC abc_init_func

; ----------------------------------------------------------------------------
; abc_call_issubclass(rdi = sub, rsi = parent) -> eax 0/1, -1 on error
; The builtin, so a registered class that is itself an ABC gets its own
; __subclasscheck__ consulted.
; ----------------------------------------------------------------------------
DEF_FUNC_LOCAL abc_call_issubclass, 16
    push rbx
    sub rsp, 16
    mov [rsp], rdi
    mov [rsp + 8], rsi
    extern builtin_issubclass
    mov rdi, rsp
    mov esi, 2
    call builtin_issubclass
    add rsp, 16
    test rax, rax
    jz .err
    mov rbx, rax
    mov rdi, rax
    call obj_is_true
    mov rdi, rbx
    mov ebx, eax
    call obj_decref
    mov eax, ebx
    pop rbx
    leave
    ret
.err:
    mov eax, -1
    pop rbx
    leave
    ret
END_FUNC abc_call_issubclass

; ----------------------------------------------------------------------------
; abc_subclasscheck(rdi = cls, rsi = subclass) -> eax 1 yes / 0 no / -1 error
; Modules/_abc.c, _abc__abc_subclasscheck_impl.
; ----------------------------------------------------------------------------
SC_CLS    equ 8
SC_SUB    equ 16
SC_CACHE  equ 24
SC_NEG    equ 32
SC_HOOK   equ 40
SC_IDX    equ 48
SC_REG    equ 56
SC_FRAME  equ 64
DEF_FUNC abc_subclasscheck, SC_FRAME
    push rbx
    push r12
    mov [rbp - SC_CLS], rdi
    mov [rbp - SC_SUB], rsi

    ; 1. the positive cache
    CSTRING rsi, "_abc_cache"
    call abc_state_get
    mov [rbp - SC_CACHE], rax
    test rax, rax
    jz .no_state
    mov rdi, rax
    mov rsi, [rbp - SC_SUB]
    call set_contains
    test eax, eax
    jnz .yes

    ; 2. the negative cache, invalidated when the token has moved on
    mov rdi, [rbp - SC_CLS]
    CSTRING rsi, "_abc_negative_cache_version"
    call abc_state_get
    V_TO_I64 rax
    cmp rax, [rel abc_invalidation_counter]
    jge .neg_current
    mov rdi, [rbp - SC_CLS]
    CSTRING rsi, "_abc_negative_cache"
    call abc_fresh_set
    mov [rbp - SC_NEG], rax
    mov rdi, [rbp - SC_CLS]
    CSTRING rsi, "_abc_negative_cache_version"
    mov rdx, [rel abc_invalidation_counter]
    V_PACK_I64 rdx, rcx
    call abc_state_set
    jmp .hook

.neg_current:
    mov rdi, [rbp - SC_CLS]
    CSTRING rsi, "_abc_negative_cache"
    call abc_state_get
    mov [rbp - SC_NEG], rax
    test rax, rax
    jz .hook
    mov rdi, rax
    mov rsi, [rbp - SC_SUB]
    call set_contains
    test eax, eax
    jnz .no

.hook:
    ; 3. __subclasshook__, which is where the structural ABCs answer
    CSTRING rdi, "__subclasshook__"
    call str_from_cstr_heap
    mov rbx, rax
    mov rdi, [rbp - SC_CLS]
    mov rsi, rbx
    call type_getattr
    mov r12, rax
    mov rdi, rbx
    call obj_decref
    mov rax, r12
    test rax, rax
    jz .mro
    mov [rbp - SC_HOOK], rax

    V_TEST_PTR rax, rcx
    ja .mro                     ; not callable; fall through to the MRO check
    ; type_getattr hands back what is in the dict, wrappers included, and
    ; __subclasshook__ is always a classmethod -- unwrap it and pass the
    ; class explicitly, which is what binding would have done.
    mov rcx, [rax + PyObject.ob_type]
    extern classmethod_type
    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    jne .hook_plain
    mov rdx, [rax + PyClassMethodObject.cm_callable]
    test rdx, rdx
    jz .hook_release
    mov rcx, [rdx + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .hook_release
    sub rsp, 16
    mov rax, [rbp - SC_CLS]
    mov [rsp], rax
    mov rax, [rbp - SC_SUB]
    mov [rsp + 8], rax
    mov rdi, rdx
    mov rsi, rsp
    mov edx, 2
    call rcx
    add rsp, 16
    jmp .hook_have_result
.hook_plain:
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .hook_release
    sub rsp, 16
    mov rax, [rbp - SC_SUB]
    mov [rsp], rax
    mov rdi, [rbp - SC_HOOK]
    mov rsi, rsp
    mov edx, 1
    call rcx
    add rsp, 16
.hook_have_result:
    mov rbx, rax                ; rbx = the hook's result
    mov rdi, [rbp - SC_HOOK]
    call obj_decref
    mov qword [rbp - SC_HOOK], 0
    test rbx, rbx
    jz .error

    lea rax, [rel notimpl_singleton]
    cmp rbx, rax
    je .hook_undecided
    mov rdi, rbx
    call obj_is_true
    mov r12d, eax
    mov rdi, rbx
    call obj_decref
    test r12d, r12d
    jz .cache_no
    jmp .cache_yes

.hook_undecided:
    mov rdi, rbx
    call obj_decref
    jmp .mro

.hook_release:
    mov rdi, [rbp - SC_HOOK]
    call obj_decref

.mro:
    ; 4. an ordinary subclass, found by walking the MRO
    mov rdi, [rbp - SC_SUB]
    mov rsi, [rbp - SC_CLS]
    call type_is_subtype
    test eax, eax
    jnz .cache_yes

    ; 5. a subclass of something registered
    mov rdi, [rbp - SC_CLS]
    CSTRING rsi, "_abc_registry"
    call abc_state_get
    test rax, rax
    jz .cache_no
    mov [rbp - SC_REG], rax
    mov qword [rbp - SC_IDX], 0
.reg_loop:
    mov rax, [rbp - SC_REG]
    mov rcx, [rbp - SC_IDX]
    cmp rcx, [rax + PyDictObject.capacity]
    jge .cache_no
    mov rdx, [rax + PyDictObject.entries]
    shl rcx, 4                  ; SET_ENTRY_SIZE
    mov rdi, [rdx + rcx + 8]    ; the entry's key Value (hash sits at +0)
    inc qword [rbp - SC_IDX]
    V_TEST_PTR rdi, rax
    ja .reg_loop
    test rdi, rdi
    jz .reg_loop
    mov rsi, rdi                ; the registered class
    mov rdi, [rbp - SC_SUB]
    call abc_call_issubclass
    cmp eax, -1
    je .error
    test eax, eax
    jz .reg_loop
    jmp .cache_yes

.cache_yes:
    mov rdi, [rbp - SC_CACHE]
    test rdi, rdi
    jz .yes
    mov rsi, [rbp - SC_SUB]
    call set_add
.yes:
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret

.cache_no:
    mov rdi, [rbp - SC_NEG]
    test rdi, rdi
    jz .no
    mov rsi, [rbp - SC_SUB]
    call set_add
.no:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.no_state:
    ; Not initialised as an ABC: fall back to plain inheritance.
    mov rdi, [rbp - SC_SUB]
    mov rsi, [rbp - SC_CLS]
    call type_is_subtype
    pop r12
    pop rbx
    leave
    ret

.error:
    mov eax, -1
    pop r12
    pop rbx
    leave
    ret
END_FUNC abc_subclasscheck

; ----------------------------------------------------------------------------
; _abc_subclasscheck(cls, subclass) -> bool
; ----------------------------------------------------------------------------
DEF_FUNC abc_subclasscheck_func
    cmp rsi, 2
    jl .bad
    push rbx
    mov rbx, [rdi + 8]
    mov rdi, [rdi]
    mov rsi, rbx
    call abc_subclasscheck
    cmp eax, -1
    je .propagate
    test eax, eax
    jz .false
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop rbx
    leave
    ret
.false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop rbx
    leave
    ret
.propagate:
    xor eax, eax
    pop rbx
    leave
    ret
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "_abc_subclasscheck() takes 2 arguments"
    call raise_exception
END_FUNC abc_subclasscheck_func

; ----------------------------------------------------------------------------
; _abc_instancecheck(cls, instance) -> bool
;
; CPython consults instance.__class__ as well as type(instance), so that an
; object which lies about its class is believed.  Nothing here can lie yet,
; so the two are the same object and one check does.
; ----------------------------------------------------------------------------
IC_CLS   equ 8
IC_SUB   equ 16
IC_FRAME equ 16
DEF_FUNC abc_instancecheck_func, IC_FRAME
    cmp rsi, 2
    jl .bad
    mov rax, [rdi]
    mov [rbp - IC_CLS], rax
    mov rdi, [rdi + 8]
    call value_type
    mov [rbp - IC_SUB], rax

    mov rdi, [rbp - IC_CLS]
    CSTRING rsi, "_abc_cache"
    call abc_state_get
    test rax, rax
    jz .full
    mov rdi, rax
    mov rsi, [rbp - IC_SUB]
    call set_contains
    test eax, eax
    jnz .true

.full:
    mov rdi, [rbp - IC_CLS]
    mov rsi, [rbp - IC_SUB]
    call abc_subclasscheck
    cmp eax, -1
    je .propagate
    test eax, eax
    jz .false
.true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.propagate:
    xor eax, eax
    leave
    ret
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "_abc_instancecheck() takes 2 arguments"
    call raise_exception
END_FUNC abc_instancecheck_func

; ----------------------------------------------------------------------------
; _abc_register(cls, subclass) -> subclass
; ----------------------------------------------------------------------------
RG_CLS   equ 8
RG_SUB   equ 16
RG_FRAME equ 16
DEF_FUNC abc_register_func, RG_FRAME
    cmp rsi, 2
    jl .bad
    mov rax, [rdi]
    mov [rbp - RG_CLS], rax
    mov rax, [rdi + 8]
    mov [rbp - RG_SUB], rax
    mov rdi, rax
    call type_check_is_class
    test eax, eax
    jz .not_a_class

    ; Already a subclass?  Registering is then a no-op.
    mov rdi, [rbp - RG_SUB]
    mov rsi, [rbp - RG_CLS]
    call type_is_subtype
    test eax, eax
    jnz .done

    ; The other way round would make an inheritance cycle.
    mov rdi, [rbp - RG_CLS]
    mov rsi, [rbp - RG_SUB]
    call type_is_subtype
    test eax, eax
    jnz .cycle

    mov rdi, [rbp - RG_CLS]
    CSTRING rsi, "_abc_registry"
    call abc_state_get
    test rax, rax
    jz .done
    mov rdi, rax
    mov rsi, [rbp - RG_SUB]
    call set_add
    inc qword [rel abc_invalidation_counter]

.done:
    mov rax, [rbp - RG_SUB]
    mov rdi, rax
    call obj_incref
    mov rax, [rbp - RG_SUB]
    leave
    ret
.not_a_class:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "Can only register classes"
    call raise_exception
.cycle:
    lea rdi, [rel exc_RuntimeError_type]
    CSTRING rsi, "Refusing to create an inheritance cycle"
    call raise_exception
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "_abc_register() takes 2 arguments"
    call raise_exception
END_FUNC abc_register_func

; ----------------------------------------------------------------------------
; _get_dump(cls) -> (registry, cache, negative_cache, negative_cache_version)
; ----------------------------------------------------------------------------
GD_CLS   equ 8
GD_TUP   equ 16
GD_FRAME equ 16
DEF_FUNC abc_get_dump_func, GD_FRAME
    cmp rsi, 1
    jl .bad
    mov rax, [rdi]
    mov [rbp - GD_CLS], rax
    mov edi, 4
    call tuple_new
    mov [rbp - GD_TUP], rax

    %macro GD_SLOT 2
    mov rdi, [rbp - GD_CLS]
    CSTRING rsi, %2
    call abc_state_get
    test rax, rax
    jnz %%have
    lea rax, [rel none_singleton]
    %%have:
    INCREF_V rax, rcx
    mov rcx, [rbp - GD_TUP]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx + %1 * 8], rax
    %endmacro

    GD_SLOT 0, "_abc_registry"
    GD_SLOT 1, "_abc_cache"
    GD_SLOT 2, "_abc_negative_cache"
    GD_SLOT 3, "_abc_negative_cache_version"

    mov rax, [rbp - GD_TUP]
    leave
    ret
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "_get_dump() requires a class"
    call raise_exception
END_FUNC abc_get_dump_func

; ----------------------------------------------------------------------------
; _reset_registry(cls) -> None
; ----------------------------------------------------------------------------
DEF_FUNC abc_reset_registry_func
    cmp rsi, 1
    jl .bad
    mov rdi, [rdi]
    CSTRING rsi, "_abc_registry"
    call abc_fresh_set
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "_reset_registry() requires a class"
    call raise_exception
END_FUNC abc_reset_registry_func

; ----------------------------------------------------------------------------
; _reset_caches(cls) -> None
; ----------------------------------------------------------------------------
RC_CLS   equ 8
RC_FRAME equ 16
DEF_FUNC abc_reset_caches_func, RC_FRAME
    cmp rsi, 1
    jl .bad
    mov rax, [rdi]
    mov [rbp - RC_CLS], rax
    mov rdi, rax
    CSTRING rsi, "_abc_cache"
    call abc_fresh_set
    mov rdi, [rbp - RC_CLS]
    CSTRING rsi, "_abc_negative_cache"
    call abc_fresh_set
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.bad:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "_reset_caches() requires a class"
    call raise_exception
END_FUNC abc_reset_caches_func

; ============================================================================
; Module construction
; ============================================================================
%macro ABC_ADD_FUNC 2
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

ABC_FRAME equ 8
global abc_module_create
DEF_FUNC abc_module_create, ABC_FRAME
    push rbx
    push r12

    call dict_new
    mov r12, rax

    ABC_ADD_FUNC abc_get_cache_token,      abcm_get_cache_token
    ABC_ADD_FUNC abc_init_func,            abcm_abc_init
    ABC_ADD_FUNC abc_register_func,        abcm_abc_register
    ABC_ADD_FUNC abc_instancecheck_func,   abcm_abc_instancecheck
    ABC_ADD_FUNC abc_subclasscheck_func,   abcm_abc_subclasscheck
    ABC_ADD_FUNC abc_get_dump_func,        abcm_get_dump
    ABC_ADD_FUNC abc_reset_registry_func,  abcm_reset_registry
    ABC_ADD_FUNC abc_reset_caches_func,    abcm_reset_caches

    lea rdi, [rel abcm_name]
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
END_FUNC abc_module_create

section .data
align 8
abc_invalidation_counter: dq 0

section .rodata
abcm_name:                 db "_abc", 0
abcm_get_cache_token:      db "get_cache_token", 0
abcm_abc_init:             db "_abc_init", 0
abcm_abc_register:         db "_abc_register", 0
abcm_abc_instancecheck:    db "_abc_instancecheck", 0
abcm_abc_subclasscheck:    db "_abc_subclasscheck", 0
abcm_get_dump:             db "_get_dump", 0
abcm_reset_registry:       db "_reset_registry", 0
abcm_reset_caches:         db "_reset_caches", 0
