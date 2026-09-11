; pyo/genericalias.asm - types.GenericAlias: `list[int]` as an object (PEP 585)
;
; Split out of pyo/descriptors.asm, which was at the 100k cap that
; src/compiler/lint.py holds hand-written files to -- four bytes of headroom,
; so the next line written into it failed the build.  This is the same seam
; uniontype.asm was cut along and the file's own docblock already named: an
; alias is a type of its own, reached through __class_getitem__ and nb_or,
; and it shares nothing with the descriptors around it but the tables that
; name its slots.
;
; union_type stays behind, because it is spelt in the same table block as the
; descriptor types; it borrows generic_alias_dealloc and the two are the same
; two-field record, which is why that one is exported.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

; --- what descriptors.asm keeps ---
extern ga_name_str
extern generic_alias_as_number
extern union_type

; --- the rest of the tree ---
extern ap_free
extern ap_malloc
extern ap_strcmp
extern bool_false
extern bool_true
extern dict_get
extern ellipsis_singleton
extern exc_TypeError_type
extern get_iterator
extern none_type
extern obj_decref
extern obj_dealloc
extern obj_hash
extern obj_incref
extern obj_repr
extern obj_richcompare_bool
extern raise_exception
extern str_from_cstr
extern str_new_heap
extern str_type
extern tuple_new
extern tuple_type
extern type_type

section .text

;; ============================================================================
;; types.GenericAlias -- what `list[int]` evaluates to (PEP 585).
;;
;; _collections_abc does `GenericAlias = type(list[int])` at import time, and
;; every stdlib module carrying annotations reaches it eventually.  It is a
;; two-field record: the origin type and the argument.  It is callable, so
;; `list[int]()` still builds a list.
;;
;; generic_alias_new(rdi = origin type, rsi = the argument) -> rax = the alias,
;;   a new reference, or 0 with the exception pending
;; ============================================================================
DEF_FUNC generic_alias_new
    push rbx
    push r12
    mov rbx, rdi                ; origin
    mov r12, rsi                ; args
    mov edi, PyGenericAliasObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel generic_alias_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyGenericAliasObject.ga_origin], rbx
    mov [rax + PyGenericAliasObject.ga_args], r12
    mov qword [rax + PyGenericAliasObject.ga_starred], 0
    push rax
    mov rdi, rbx
    call obj_incref
    ; ga_args is a VALUE, not a pointer: `list[0]` puts an int immediate here
    ; and obj_incref on one writes through the number.
    mov rax, r12
    INCREF_V rax, rcx
.gan_done:
    pop rax
    pop r12
    pop rbx
    leave
    ret
END_FUNC generic_alias_new

;; ============================================================================
;; generic_alias_dealloc(rdi = the alias) -> nothing; the object is freed
;;
;; union_type borrows this: a union is the same two-field record, so the two
;; release the same two fields.  It is why this one is exported.
;; ============================================================================
global generic_alias_dealloc
DEF_FUNC generic_alias_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyGenericAliasObject.ga_origin]
    test rdi, rdi
    jz .gad_args
    call obj_decref
.gad_args:
    mov rax, [rbx + PyGenericAliasObject.ga_args]
    test rax, rax
    jz .gad_free
    DECREF_V rax, rcx
.gad_free:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC generic_alias_dealloc

;; ============================================================================
;; generic_alias_construct(rdi = type, rsi = args, rdx = nargs)
;;   -> rax = a new GenericAlias, rdx = TAG_PTR
;;
;; tp_new for generic_alias_type: types.GenericAlias(origin, args).
;;
;; The type had no constructor, so `GenericAlias(list, str)` fell through to
;; the ordinary class-construction path: it allocated a GC-headed block, left
;; ga_origin and ga_args holding whatever was there, and then freed it with
;; this type's tp_dealloc -- a plain free at the object address rather than at
;; the GC head, which glibc reports as a double free.  os.PathLike is written
;; `__class_getitem__ = classmethod(GenericAlias)`, so `os.PathLike[str]` was
;; the crash; importlib.resources is one line of it.
;; ============================================================================
;; ============================================================================
;; generic_alias_iter(rdi = the alias) -> rax = an iterator over one item, the
;;   same alias with its star set
;;
;; PEP 646 compiles `*tuple[int, str]` in an annotation as the expression
;; followed by UNPACK_SEQUENCE 1, so the value that reaches __annotations__ is
;; whatever iterating the alias yields once.  CPython's is the alias again
;; with __unpacked__ True; without a tp_iter at all the unpack said
;; "'types.GenericAlias' object is not iterable".
;;
;; A one-item tuple and its own iterator, rather than a new iterator type:
;; there is exactly one item and it is built here.
;; ============================================================================
GAI_TUP   equ 8
GAI_FRAME equ 16            ; + 0 pushes = 16-aligned
DEF_FUNC generic_alias_iter, GAI_FRAME
    mov rsi, [rdi + PyGenericAliasObject.ga_args]
    mov rdi, [rdi + PyGenericAliasObject.ga_origin]
    call generic_alias_new
    test rax, rax
    jz .gai_fail
    mov qword [rax + PyGenericAliasObject.ga_starred], 1
    mov [rbp - GAI_TUP], rax
    mov edi, 1
    extern tuple_new
    call tuple_new
    test rax, rax
    jz .gai_fail
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rdx, [rbp - GAI_TUP]
    mov [rcx], rdx              ; the tuple takes the reference
    mov [rbp - GAI_TUP], rax
    mov rdi, rax
    mov esi, TAG_PTR
    extern get_iterator
    call get_iterator
    push rax
    mov rdi, [rbp - GAI_TUP]
    call obj_decref             ; the iterator holds it now
    pop rax
    leave
    ret
.gai_fail:
    xor eax, eax
    leave
    ret
END_FUNC generic_alias_iter

;; ============================================================================
;; generic_alias_construct(rdi = type, rsi = args, rdx = nargs) -> Value
;;
;; tp_new: `types.GenericAlias(list, int)` spelt out, which is what
;; _collections_abc and typing both do rather than subscripting.
;; ============================================================================
DEF_FUNC generic_alias_construct
    cmp rdx, 2
    jne .gac_error
    mov rdi, [rsi]              ; the origin
    mov rsi, [rsi + 8]          ; the argument, whatever it is
    V_TEST_PTR rdi, rax
    ja .gac_error
    test rdi, rdi
    jz .gac_error
    call generic_alias_new
    mov edx, TAG_PTR            ; a constructor returns the (payload, tag) pair
    leave
    ret
.gac_error:
    RAISE exc_TypeError_type, "GenericAlias expected 2 arguments"
END_FUNC generic_alias_construct

;; ============================================================================
;; generic_alias_class_getitem(rdi = args, rsi = nargs) -> Value: the alias
;;
;; The builtin registered as __class_getitem__ on each container type.
;; args[0] = cls, args[1] = the subscript.
;; ============================================================================
DEF_FUNC generic_alias_class_getitem
    cmp rsi, 2
    jl .gacg_bad
    mov rax, [rdi + 8]
    mov rdi, [rdi]
    mov rsi, rax
    call generic_alias_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gacg_bad:
    RAISE exc_TypeError_type, "__class_getitem__() takes exactly one argument"
END_FUNC generic_alias_class_getitem

;; ============================================================================
;; generic_alias_repr(rdi = the alias) -> rax = a PyStrObject*, or 0
;;
;; "list[int]" -- origin name, then the argument's repr.
;; ============================================================================
GAR_BUF   equ 264
GAR_SELF  equ 272
GAR_FRAME equ 296            ; + 5 pushes = 336, 16-aligned
DEF_FUNC generic_alias_repr, GAR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov [rbp - GAR_SELF], rdi
    lea rbx, [rbp - GAR_BUF]
    xor r13d, r13d

    ; An UNPACKED alias reprs with a leading star: `*tuple[int, str]` is what
    ; PEP 646 writes and what CPython prints back.
    cmp qword [rdi + PyGenericAliasObject.ga_starred], 0
    je .gar_no_star
    mov byte [rbx], '*'
    mov r13d, 1
.gar_no_star:

    mov rax, [rdi + PyGenericAliasObject.ga_origin]
    test rax, rax
    jz .gar_open
    mov rsi, [rax + PyObject.ob_type]
    test rsi, rsi
    jz .gar_open
    test qword [rsi + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .gar_open
    mov rdi, rax
    call .gar_qualified

.gar_open:
    mov byte [rbx + r13], '['
    inc r13
    mov rdi, [rbp - GAR_SELF]
    mov rdi, [rdi + PyGenericAliasObject.ga_args]
    test rdi, rdi
    jz .gar_close
    ; A tuple argument prints comma-joined without its parentheses, and a
    ; type prints as its name: list[int], not list[<class 'int'>].
    extern tuple_type
    V_TEST_PTR rdi, rax         ; classify before reading ob_type: the
    ja .gar_one                 ; argument may be a number
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    jne .gar_one
    mov r14, [rdi + PyTupleObject.ob_size]
    mov r15, [rdi + PyTupleObject.ob_item]
    xor r12d, r12d
.gar_tuple_loop:
    cmp r12, r14
    jge .gar_close
    test r12, r12
    jz .gar_no_comma
    mov byte [rbx + r13], ','
    mov byte [rbx + r13 + 1], ' '
    add r13, 2
.gar_no_comma:
    mov rdi, [r15 + r12*8]
    call .gar_emit_one
    inc r12
    jmp .gar_tuple_loop

.gar_one:
    call .gar_emit_one
    jmp .gar_close

;; .gar_emit_one(rdi = a Value) -- append its display form to the buffer
.gar_emit_one:
    push r12
    push r14
    push r15
    ; ... shows as "...", which is what CPython's alias repr does even though
    ; repr(Ellipsis) is "Ellipsis"
    extern ellipsis_singleton
    lea rax, [rel ellipsis_singleton]
    cmp rdi, rax
    jne .geo_not_ellipsis
    cmp r13, GAR_BUF - 8
    jae .geo_done
    mov byte [rbx + r13], '.'
    mov byte [rbx + r13 + 1], '.'
    mov byte [rbx + r13 + 2], '.'
    add r13, 3
    jmp .geo_done
.geo_not_ellipsis:
    ; a type shows as its (unqualified) name
    V_TEST_PTR rdi, rax
    ja .geo_repr
    test rdi, rdi
    jz .geo_repr
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .geo_repr
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .geo_repr
    call .gar_qualified
    jmp .geo_done

.geo_repr:
    call obj_repr
    V_UNPACK rax, rdx
    test rax, rax
    jz .geo_done
    push rax
    mov r8, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor ecx, ecx
.geo_repr_copy:
    cmp rcx, r8
    jge .geo_repr_done
    cmp r13, GAR_BUF - 8
    jae .geo_repr_done
    movzx eax, byte [rsi + rcx]
    mov [rbx + r13], al
    inc r13
    inc rcx
    jmp .geo_repr_copy
.geo_repr_done:
    pop rdi
    call obj_decref
.geo_done:
    pop r15
    pop r14
    pop r12
    ret


;; .gar_qualified(rdi = a class) -- append "module.QualName" to rbx at r13.
;;
;; CPython writes an alias's origin and its type arguments the way it writes a
;; class in an annotation: qualified by module, with "builtins" left off, and
;; using __qualname__ so a nested class keeps its "Outer.Inner".  Only the
;; bare tp_name was written, which for a class built by a metaclass of its own
;; -- every ABC, and os.PathLike is one -- was not reached at all, because the
;; test for "is this a class?" was a comparison against the two metatypes this
;; tree ships rather than TYPE_FLAG_METATYPE.
.gar_qualified:
    push r12
    push r14
    push r15
    mov r12, rdi                    ; the class
    mov r14, [rdi + PyTypeObject.tp_dict]

    ; --- the module, unless it is "builtins" ---
    test r14, r14
    jz .gq_module_from_name
    CSTRING rdi, "__module__"
    call str_from_cstr
    mov rsi, rax
    mov rdi, r14
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .gq_module_from_name
    cmp edx, TAG_PTR
    jne .gq_module_from_name
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .gq_module_from_name
    lea rsi, [rax + PyStrObject.data]
    mov rdi, rsi
    CSTRING rsi, "builtins"
    push rdi
    call ap_strcmp
    pop rsi
    test eax, eax
    jz .gq_qualname                 ; "builtins" is left off
    call .gq_copy_cstr
    mov byte [rbx + r13], '.'
    inc r13
    jmp .gq_qualname

.gq_module_from_name:
    ; A static type records no __module__; its tp_name carries the dotted
    ; prefix instead -- "types.GenericAlias" -- and everything else is a
    ; builtin, which prints unqualified either way.
    mov rsi, [r12 + PyTypeObject.tp_name]
    xor ecx, ecx
    xor r15, r15                    ; length of the prefix, 0 for none
.gq_scan:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .gq_scanned
    cmp al, '.'
    jne .gq_scan_next
    lea r15, [rcx + 1]
.gq_scan_next:
    inc rcx
    jmp .gq_scan
.gq_scanned:
    test r15, r15
    jz .gq_qualname
    xor ecx, ecx
.gq_prefix:
    cmp rcx, r15
    jge .gq_qualname
    cmp r13, GAR_BUF - 8
    jae .gq_qualname
    movzx eax, byte [rsi + rcx]
    mov [rbx + r13], al
    inc r13
    inc rcx
    jmp .gq_prefix

.gq_qualname:
    test r14, r14
    jz .gq_from_tp_name
    CSTRING rdi, "__qualname__"
    call str_from_cstr
    mov rsi, rax
    mov rdi, r14
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .gq_from_tp_name
    cmp edx, TAG_PTR
    jne .gq_from_tp_name
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .gq_from_tp_name
    lea rsi, [rax + PyStrObject.data]
    call .gq_copy_cstr
    jmp .gq_done

.gq_from_tp_name:
    ; The last dotted component: "types.GenericAlias" prints "GenericAlias",
    ; the prefix having gone in as the module above.
    mov rsi, [r12 + PyTypeObject.tp_name]
    mov rdi, rsi
    xor ecx, ecx
.gq_last_dot:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .gq_tail
    cmp al, '.'
    jne .gq_dot_next
    lea rdi, [rsi + rcx + 1]
.gq_dot_next:
    inc rcx
    jmp .gq_last_dot
.gq_tail:
    mov rsi, rdi
    call .gq_copy_cstr
.gq_done:
    pop r15
    pop r14
    pop r12
    ret

;; .gq_copy_cstr(rsi = a NUL-terminated string) -- append it, bounded.
.gq_copy_cstr:
    movzx eax, byte [rsi]
    test al, al
    jz .gq_copied
    inc rsi
    cmp r13, GAR_BUF - 8
    jae .gq_copied
    mov [rbx + r13], al
    inc r13
    jmp .gq_copy_cstr
.gq_copied:
    ret

.gar_close:
    mov byte [rbx + r13], ']'
    inc r13
    mov rdi, rbx
    mov rsi, r13
    call str_new_heap
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC generic_alias_repr

;; ============================================================================
;; generic_alias_call(rdi = the alias, rsi = args, rdx = nargs) -> Value
;;
;; Calling an alias constructs the origin: list[int]() is a list.  Tail-jumps
;; into the origin's own tp_call, so the alias costs nothing at the call.
;; ============================================================================
DEF_FUNC_BARE generic_alias_call
    mov rax, [rdi + PyGenericAliasObject.ga_origin]
    mov rdi, rax
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .gac_bad
    jmp rcx
.gac_bad:
    RAISE exc_TypeError_type, "generic alias is not callable"
END_FUNC generic_alias_call

;; ============================================================================
;; generic_alias_getattr(rdi = the alias, rsi = name str) -> Value, or 0
;;
;; __origin__ and __args__, the two fields typing reads off an alias.
;; ============================================================================
GAG_NAME  equ 8
GAG_FRAME equ 24            ; + 1 push = 32, 16-aligned

DEF_FUNC generic_alias_getattr, GAG_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - GAG_NAME], rsi
    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__origin__"
    call ap_strcmp
    test eax, eax
    jz .gag_origin

    ; __args__ is always a tuple, even when the subscript was one thing:
    ; CPython wraps it, and typing.get_args() and every annotation reader
    ; expect that.  This used to answer NULL, which reads as "no attribute".
    mov rdi, [rbp - GAG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "__args__"
    call ap_strcmp
    test eax, eax
    jz .gag_args

    ; PEP 646 asks whether an alias is the unpacked form, and typing reads it
    ; by name.
    mov rdi, [rbp - GAG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "__unpacked__"
    call ap_strcmp
    test eax, eax
    jnz .gag_missing
    cmp qword [rbx + PyGenericAliasObject.ga_starred], 0
    je .gag_not_unpacked
    lea rax, [rel bool_true]
    jmp .gag_bool
.gag_not_unpacked:
    lea rax, [rel bool_false]
.gag_bool:
    INCREF rax
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.gag_args:

    mov rax, [rbx + PyGenericAliasObject.ga_args]
    test rax, rax
    jz .gag_empty_args
    V_TEST_PTR rax, rcx
    ja .gag_wrap_args
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    jne .gag_wrap_args
    mov rdi, rax
    call obj_incref
    mov rax, [rbx + PyGenericAliasObject.ga_args]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.gag_wrap_args:
    mov edi, 1
    call tuple_new
    test rax, rax
    jz .gag_missing
    mov rcx, [rbx + PyGenericAliasObject.ga_args]
    mov rdx, [rax + PyTupleObject.ob_item]
    mov [rdx], rcx
    push rax
    mov rax, rcx
    INCREF_V rax, rcx
    pop rax
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.gag_empty_args:
    xor edi, edi
    call tuple_new
    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.gag_missing:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
.gag_origin:
    mov rax, [rbx + PyGenericAliasObject.ga_origin]
    push rax
    mov rdi, rax
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC generic_alias_getattr

;; ga_emit_name(rdi = Value, rsi = buffer, rdx = length, r8 = capacity)
;;   -> rax = new length
;; The display form used inside a generic alias or a union: a type shows as
;; its unqualified name, Ellipsis as "...", anything else as its repr.
DEF_FUNC ga_emit_name, 8            ; 3 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    mov rbx, rsi
    mov r13, rdx
    mov r12, rdi

    lea rax, [rel ellipsis_singleton]
    cmp r12, rax
    jne .gen_not_ellipsis
    cmp r13, 240
    jae .gen_out
    mov byte [rbx + r13], '.'
    mov byte [rbx + r13 + 1], '.'
    mov byte [rbx + r13 + 2], '.'
    add r13, 3
    jmp .gen_out

.gen_not_ellipsis:
    ; NoneType prints as None, in a union and in a subscript alike:
    ; `int | None` and `list[None]` are what CPython spells these.
    lea rax, [rel none_type]
    cmp r12, rax
    jne .gen_not_none
    cmp r13, 240
    jae .gen_out
    mov byte [rbx + r13], 'N'
    mov byte [rbx + r13 + 1], 'o'
    mov byte [rbx + r13 + 2], 'n'
    mov byte [rbx + r13 + 3], 'e'
    add r13, 4
    jmp .gen_out

.gen_not_none:
    V_TEST_PTR r12, rax
    ja .gen_repr
    test r12, r12
    jz .gen_repr
    ; Any class, whichever metatype made it -- a class built by a metaclass of
    ; its own is still a class, and comparing ob_type against the two
    ; metatypes this tree ships answered no for it.
    mov rax, [r12 + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .gen_repr

.gen_typename:
    ; CPython qualifies a class with its module here as it does in a repr:
    ; `__main__.C | None`, and `int | None` for anything in builtins.  The
    ; module comes from the type's dict, and the name is what follows the
    ; last dot of tp_name -- a builtin whose tp_name already carries its
    ; module would otherwise be printed with it twice.
    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .gen_no_module
    push rdi
    CSTRING rdi, "__module__"
    extern str_from_cstr
    call str_from_cstr
    pop rdi
    test rax, rax
    jz .gen_no_module
    mov rsi, rax
    extern dict_get
    call dict_get
    test rax, rax
    jz .gen_no_module
    V_TEST_PTR rax, rcx
    ja .gen_no_module
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .gen_no_module
    mov rcx, [rax + PyStrObject.ob_size]
    test rcx, rcx
    jz .gen_no_module
    lea rdi, [rax + PyStrObject.data]
    cmp rcx, 8
    jne .gen_copy_module
    push rax
    push rcx
    CSTRING rsi, "builtins"
    call ap_strcmp
    pop rcx
    pop rax
    test eax, eax
    jz .gen_no_module           ; builtins is left off, as CPython leaves it
    lea rdi, [rax + PyStrObject.data]
.gen_copy_module:
    xor edx, edx
.gen_mod_loop:
    cmp rdx, rcx
    jge .gen_mod_done
    cmp r13, 240
    jae .gen_mod_done
    mov al, [rdi + rdx]
    mov [rbx + r13], al
    inc r13
    inc rdx
    jmp .gen_mod_loop
.gen_mod_done:
    cmp r13, 240
    jae .gen_no_module
    mov byte [rbx + r13], '.'
    inc r13

.gen_no_module:
    mov rsi, [r12 + PyTypeObject.tp_name]
    mov rdi, rsi
    xor ecx, ecx
.gen_last_dot:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .gen_name_start
    cmp al, '.'
    jne .gen_dot_next
    lea rdi, [rsi + rcx + 1]
.gen_dot_next:
    inc rcx
    jmp .gen_last_dot
.gen_name_start:
    mov rsi, rdi
.gen_name_copy:
    movzx eax, byte [rsi]
    test al, al
    jz .gen_out
    inc rsi
    cmp r13, 240
    jae .gen_out
    mov [rbx + r13], al
    inc r13
    jmp .gen_name_copy

.gen_repr:
    mov rdi, r12
    call obj_repr
    V_UNPACK rax, rdx
    test rax, rax
    jz .gen_out
    push rax
    mov r8, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor ecx, ecx
.gen_repr_copy:
    cmp rcx, r8
    jge .gen_repr_done
    cmp r13, 240
    jae .gen_repr_done
    movzx eax, byte [rsi + rcx]
    mov [rbx + r13], al
    inc r13
    inc rcx
    jmp .gen_repr_copy
.gen_repr_done:
    pop rdi
    call obj_decref

.gen_out:
    mov rax, r13
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC ga_emit_name


;; ============================================================================
;; generic_alias_hash(rdi = self) -> rax = hash
;; generic_alias_richcompare(rdi = left, rsi = right, edx = op) -> Value
;;
;; union_type got both of these and generic_alias_type did not, so
;; `{list[int]: 1}[list[int]]` was a KeyError: two aliases spelt the same way
;; hashed by identity and compared by it.  Unlike a union, an alias is ordered
;; -- list[int, str] is not list[str, int] -- so this is a plain combine over
;; (origin, args) rather than union's set equality.
;; ============================================================================
GAH_FRAME equ 16            ; + 2 pushes = 32

DEF_FUNC generic_alias_hash, GAH_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov rdi, [rbx + PyGenericAliasObject.ga_origin]
    test rdi, rdi
    jz .gah_no_origin
    call obj_hash
    jmp .gah_have_origin
.gah_no_origin:
    xor eax, eax
.gah_have_origin:
    mov r12, rax
    mov rdi, [rbx + PyGenericAliasObject.ga_args]
    test rdi, rdi
    jz .gah_done
    call obj_hash
    imul r12, r12, 1000003
    xor r12, rax
.gah_done:
    mov rax, r12
    cmp rax, -1
    jne .gah_ret
    mov rax, -2
.gah_ret:
    pop r12
    pop rbx
    leave
    ret
END_FUNC generic_alias_hash

;; ============================================================================
;; generic_alias_richcompare(rdi = left, rsi = right, edx = op) -> Value
;;
;; The other half of the pair documented above hash: an alias is ORDERED, so
;; equality is origin-then-args in order, and every other op declines.
;; ============================================================================
GRC_LEFT  equ 8
GRC_RIGHT equ 16
GRC_OP    equ 24
GRC_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC generic_alias_richcompare, GRC_FRAME
    cmp edx, PY_EQ
    je .grc_ok
    cmp edx, PY_NE
    jne .grc_decline
.grc_ok:
    mov [rbp - GRC_OP], edx
    ; Both sides must be aliases; anything else declines so the protocol can
    ; try the other operand.
    V_TEST_PTR rdi, rax
    ja .grc_decline
    V_TEST_PTR rsi, rax
    ja .grc_decline
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel generic_alias_type]
    cmp rax, rcx
    jne .grc_decline
    mov rax, [rsi + PyObject.ob_type]
    cmp rax, rcx
    jne .grc_decline
    mov [rbp - GRC_LEFT], rdi
    mov [rbp - GRC_RIGHT], rsi

    mov rdi, [rdi + PyGenericAliasObject.ga_origin]
    mov rsi, [rsi + PyGenericAliasObject.ga_origin]
    mov edx, PY_EQ
    extern obj_richcompare_bool
    call obj_richcompare_bool
    test eax, eax
    jl .grc_raised
    test eax, eax
    jz .grc_false

    mov rdi, [rbp - GRC_LEFT]
    mov rdi, [rdi + PyGenericAliasObject.ga_args]
    mov rsi, [rbp - GRC_RIGHT]
    mov rsi, [rsi + PyGenericAliasObject.ga_args]
    mov edx, PY_EQ
    call obj_richcompare_bool
    test eax, eax
    jl .grc_raised
    test eax, eax
    jz .grc_false

    mov eax, 1
    jmp .grc_answer
.grc_false:
    xor eax, eax
.grc_answer:
    cmp dword [rbp - GRC_OP], PY_NE
    jne .grc_emit
    xor eax, 1
.grc_emit:
    test eax, eax
    jz .grc_emit_false
    lea rax, [rel bool_true]
    jmp .grc_emit_done
.grc_emit_false:
    lea rax, [rel bool_false]
.grc_emit_done:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.grc_decline:
    xor eax, eax                ; a NULL Value: NotImplemented
    xor edx, edx
    leave
    ret
.grc_raised:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC generic_alias_richcompare

section .data

align 8
global generic_alias_type
generic_alias_type:
    dq 1                            ; ob_refcnt (immortal)
    dq type_type                    ; ob_type
    dq ga_name_str                  ; tp_name
    dq PyGenericAliasObject_size    ; tp_basicsize
    dq generic_alias_dealloc        ; tp_dealloc
    dq generic_alias_repr           ; tp_repr
    dq generic_alias_repr           ; tp_str
    dq generic_alias_hash           ; tp_hash
    dq generic_alias_call           ; tp_call
    dq generic_alias_getattr        ; tp_getattr
    dq 0                            ; tp_setattr
    dq generic_alias_richcompare    ; tp_richcompare
    dq generic_alias_iter           ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq generic_alias_construct      ; tp_new
    dq generic_alias_as_number      ; tp_as_number
    dq 0                            ; tp_as_sequence
    dq 0                            ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq 0                            ; tp_flags
    dq 0                            ; tp_bases
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset
    dq 0                        ; tp_tailslots
    dq 0                        ; tp_as_buffer
