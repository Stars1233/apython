; exception.asm - Exception type objects and exception object creation
;
; Provides:
;   - PyTypeObject singletons for all standard Python exception types
;   - exc_new(type, msg_str) -> PyExceptionObject*
;   - exc_from_cstr(type, msg_cstr) -> PyExceptionObject*
;   - exc_isinstance(exc, type) -> bool (walks tp_base chain)
;   - exception_type_table[] for EXC_* ID -> PyTypeObject* lookup
;
; Exception hierarchy (simplified):
;   BaseException
;     Exception
;       TypeError, ValueError, RuntimeError, NotImplementedError,
;       LookupError (KeyError, IndexError),
;       ArithmeticError (ZeroDivisionError, OverflowError),
;       AttributeError, NameError, StopIteration,
;       AssertionError, OSError, RecursionError, UnicodeError

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "errcodes.inc"

extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern str_from_cstr
extern str_from_cstr_heap
extern obj_decref
extern obj_dealloc
extern obj_incref
extern str_type
extern type_getattr
extern type_repr
extern type_type
extern raise_exception_obj
extern str_new_heap
extern obj_repr
extern obj_str
extern raise_exception
extern exc_traverse
extern exc_clear_gc
extern tuple_new
extern tuple_type
extern ap_strcmp
extern dict_get
extern dict_new
extern dict_set
extern eg_dealloc
extern exc_BaseExceptionGroup_type
extern exc_ExceptionGroup_type

; exc_new(PyTypeObject *type, PyObject *msg_str, int msg_tag) -> PyExceptionObject*
; Creates a new exception with given type and message string.
; msg_str is INCREFed. type is stored but not INCREFed (types are immortal).
; rdx = msg_tag (TAG_PTR for heap objs, TAG_SMALLINT for ints, 0 for NULL).
EN_EXC equ 8
EN_MSG equ 16
EN_FRAME equ 16
DEF_FUNC exc_new, EN_FRAME
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; type
    mov r12, rsi            ; msg Value (0 = no message)

    ; Allocate exception object (GC-tracked)
    mov edi, PyExceptionObject_size
    mov rsi, rbx               ; type
    call gc_alloc
    ; ob_refcnt=1, ob_type set by gc_alloc
    mov [rax + PyExceptionObject.exc_type], rbx
    mov [rax + PyExceptionObject.exc_value], r12
    mov qword [rax + PyExceptionObject.exc_tb], 0
    mov qword [rax + PyExceptionObject.exc_context], 0
    mov qword [rax + PyExceptionObject.exc_cause], 0
    mov qword [rax + PyExceptionObject.exc_args], 0
    mov qword [rax + PyExceptionObject.exc_dict], 0
    mov qword [rax + PyExceptionObject.exc_suppress], 0

    ; INCREF the message
    INCREF_V r12, r13

    ; Create args tuple: (msg,) if msg present, else ()
    mov [rbp - EN_EXC], rax   ; save exc
    test r12, r12             ; a NULL Value is 0 and no real Value is
    jz .empty_args
    mov edi, 1
    call tuple_new
    INCREF_V r12, r13
    mov r8, [rax + PyTupleObject.ob_item]
    mov [r8], r12
    jmp .set_args
.empty_args:
    xor edi, edi
    call tuple_new
.set_args:
    mov rcx, [rbp - EN_EXC]
    mov [rcx + PyExceptionObject.exc_args], rax

    ; Track in GC
    mov rdi, rcx
    call gc_track

    mov rax, [rbp - EN_EXC]

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC exc_new

; exc_is_exception(rdi = object) -> eax 0/1
; True when the object is an instance of BaseException, i.e. when reading
; PyExceptionObject.exc_context off it is defined.
DEF_FUNC_BARE exc_is_exception
    V_TEST_PTR rdi, rax
    ja .nope
    test rdi, rdi
    jz .nope
    mov rdi, [rdi + PyObject.ob_type]
    lea rsi, [rel exc_BaseException_type]
    extern type_is_subtype
    jmp type_is_subtype
.nope:
    xor eax, eax
    ret
END_FUNC exc_is_exception

; exc_set_context(rdi = new exception, rsi = exception being handled)
; Implements CPython's implicit chaining: the exception raised while another
; is being handled gets that one as its __context__.  The chain is first
; scanned for `new` so a re-raise cannot make it point at itself.
ESC_NEW equ 8
ESC_OLD equ 16
ESC_FRAME equ 16
DEF_FUNC exc_set_context, ESC_FRAME
    cmp rdi, rsi
    je .esc_done
    mov [rbp - ESC_NEW], rdi
    mov [rbp - ESC_OLD], rsi
    call exc_is_exception
    test eax, eax
    jz .esc_done
    mov rdi, [rbp - ESC_OLD]
    call exc_is_exception
    test eax, eax
    jz .esc_done

    mov rdi, [rbp - ESC_NEW]
    mov rsi, [rbp - ESC_OLD]
    ; Break an existing link back to `new` so the chain stays acyclic.
    mov rax, rsi
.esc_scan:
    mov rcx, [rax + PyExceptionObject.exc_context]
    test rcx, rcx
    jz .esc_link
    cmp rcx, rdi
    jne .esc_next
    mov qword [rax + PyExceptionObject.exc_context], 0
    push rdi
    push rsi
    mov rdi, rcx
    call obj_decref
    pop rsi
    pop rdi
    jmp .esc_link
.esc_next:
    mov rax, rcx
    jmp .esc_scan

.esc_link:
    ; Drop whatever context `new` already had, then take a reference to `old`.
    mov rax, [rdi + PyExceptionObject.exc_context]
    test rax, rax
    jz .esc_store
    push rdi
    push rsi
    mov rdi, rax
    call obj_decref
    pop rsi
    pop rdi
.esc_store:
    INCREF rsi
    mov [rdi + PyExceptionObject.exc_context], rsi
.esc_done:
    leave
    ret
END_FUNC exc_set_context

; raise_key_error(rdi = key Value) -- does not return.
; CPython reports the missing key itself as the exception's single argument,
; so that KeyError('k') and str(e) == "'k'" carry which key was absent.
DEF_FUNC raise_key_error
    mov rsi, rdi                ; exc_new takes the message as a Value
    lea rdi, [rel exc_KeyError_type]
    xor edx, edx
    call exc_new
    mov rdi, rax
    call raise_exception_obj
    ud2
END_FUNC raise_key_error

; exc_from_cstr(PyTypeObject *type, const char *msg) -> PyExceptionObject*
; Creates exception with a C string message (converted to PyStrObject).
DEF_FUNC exc_from_cstr
    push rbx

    mov rbx, rdi            ; save type

    ; Convert C string to PyStrObject (heap — stored in exception struct)
    mov rdi, rsi
    call str_from_cstr_heap
    ; rax = str obj (refcnt=1)

    ; Now create exception: exc_new(type, str, TAG_PTR)
    mov rdi, rbx
    mov rsi, rax
    mov edx, TAG_PTR
    call exc_new
    ; rax = exception obj
    ; exc_new INCREFs the str, so we need to DECREF our copy
    push rax
    mov rdi, [rax + PyExceptionObject.exc_value]
    DECREF_V rdi, rsi
    pop rax

    pop rbx
    leave
    ret
END_FUNC exc_from_cstr

; exc_dealloc(PyExceptionObject *exc)
; Free exception and DECREF its fields.
DEF_FUNC exc_dealloc
    push rbx

    mov rbx, rdi

    ; XDECREF exc_value (tag-aware: may be SmallInt)
    mov rdi, [rbx + PyExceptionObject.exc_value]
    XDECREF_V rdi, rsi
.no_value:

    ; XDECREF exc_tb
    mov rdi, [rbx + PyExceptionObject.exc_tb]
    test rdi, rdi
    jz .no_tb
    call obj_decref
.no_tb:

    ; XDECREF exc_context
    mov rdi, [rbx + PyExceptionObject.exc_context]
    test rdi, rdi
    jz .no_context
    call obj_decref
.no_context:

    ; XDECREF exc_cause
    mov rdi, [rbx + PyExceptionObject.exc_cause]
    test rdi, rdi
    jz .no_cause
    call obj_decref
.no_cause:

    ; XDECREF exc_args
    mov rdi, [rbx + PyExceptionObject.exc_args]
    test rdi, rdi
    jz .no_args
    call obj_decref
.no_args:

    ; XDECREF exc_dict
    mov rdi, [rbx + PyExceptionObject.exc_dict]
    test rdi, rdi
    jz .no_dict
    call obj_decref
.no_dict:

    ; Free the object (GC-aware)
    mov rdi, rbx
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC exc_dealloc

; exc_repr(PyExceptionObject *exc) -> PyObject* (string)
; Returns "TypeName(msg)" or just "TypeName()" if no message.
ER_EXC   equ 8
ER_POS   equ 16
ER_BUF   equ 528         ; 512 bytes, [rbp-528, rbp-16)
ER_FRAME equ 544
DEF_FUNC exc_repr, ER_FRAME
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov [rbp - ER_EXC], rdi

    ; repr(exc) is TypeName(arg_reprs...).  This printed the stored value
    ; unquoted and only ever one of them, so repr(ValueError('a','b')) was
    ; "ValueError(a)".
    lea rdi, [rbp - ER_BUF]
    xor r13d, r13d                  ; output length
    mov rax, [rbx + PyExceptionObject.ob_type]
    mov rsi, [rax + PyTypeObject.tp_name]
.er_copy_name:
    movzx eax, byte [rsi]
    test al, al
    jz .er_name_done
    cmp r13, 480
    jge .er_name_done
    mov [rdi + r13], al
    inc r13
    inc rsi
    jmp .er_copy_name
.er_name_done:
    mov byte [rdi + r13], '('
    inc r13

    mov rax, [rbx + PyExceptionObject.exc_args]
    test rax, rax
    jz .er_close
    mov r12, [rax + PyTupleObject.ob_size]
    xor ecx, ecx
    mov [rbp - ER_POS], rcx
.er_arg_loop:
    mov rcx, [rbp - ER_POS]
    cmp rcx, r12
    jge .er_close
    test rcx, rcx
    jz .er_no_comma
    lea rdi, [rbp - ER_BUF]
    mov byte [rdi + r13], ','
    mov byte [rdi + r13 + 1], ' '
    add r13, 2
.er_no_comma:
    mov rax, [rbx + PyExceptionObject.exc_args]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rdi, [rax + rcx * 8]
    call obj_repr
    V_UNPACK rax, rdx
    test rax, rax
    jz .er_next
    push rax
    mov r8, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    lea rdi, [rbp - ER_BUF]
    xor ecx, ecx
.er_copy_arg:
    cmp rcx, r8
    jge .er_arg_copied
    cmp r13, 500
    jge .er_arg_copied
    movzx eax, byte [rsi + rcx]
    mov [rdi + r13], al
    inc r13
    inc rcx
    jmp .er_copy_arg
.er_arg_copied:
    pop rdi
    call obj_decref
.er_next:
    inc qword [rbp - ER_POS]
    jmp .er_arg_loop

.er_close:
    lea rdi, [rbp - ER_BUF]
    mov byte [rdi + r13], ')'
    inc r13
    mov rsi, r13
    call str_new_heap
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC exc_repr

; exc_str(PyExceptionObject *exc) -> PyObject* (string)
; Returns the message string, or type name if no message.
ES_EXC   equ 8
ES_FRAME equ 16
DEF_FUNC exc_str, ES_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - ES_EXC], rdi

    ; str(exc) is defined by args, not by a single stored value: '' for none,
    ; str(args[0]) for one, and the tuple's repr for more.  This returned the
    ; stored value when it happened to be a string and the *type name*
    ; otherwise, so str(ValueError()) was "ValueError" and
    ; str(ValueError("a","b")) was "a".
    mov rax, [rbx + PyExceptionObject.exc_args]
    test rax, rax
    jz .es_empty
    mov rcx, [rax + PyTupleObject.ob_size]
    test rcx, rcx
    jz .es_empty
    cmp rcx, 1
    jne .es_tuple

    ; KeyError is the one that shows its single argument's repr, so that a
    ; missing key prints with its quotes.
    mov rcx, [rbx + PyExceptionObject.ob_type]
    lea rdx, [rel exc_KeyError_type]
    cmp rcx, rdx
    je .es_one_repr

    mov rcx, [rax + PyTupleObject.ob_item]
    mov rdi, [rcx]
    call obj_str
    V_UNPACK rax, rdx
    pop rbx
    leave
    ret

.es_one_repr:
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rdi, [rcx]
    call obj_repr
    V_UNPACK rax, rdx
    pop rbx
    leave
    ret

.es_tuple:
    mov rdi, rax
    call obj_repr
    V_UNPACK rax, rdx
    pop rbx
    leave
    ret

.es_empty:
    CSTRING rdi, ""
    call str_from_cstr
    pop rbx
    leave
    ret
END_FUNC exc_str

; exc_getattr(PyExceptionObject *exc, PyStrObject *name) -> PyObject* or NULL
; Handle attribute access on exception objects: args, __context__, __cause__, etc.
global exc_getattr
DEF_FUNC exc_getattr
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; exc
    mov r12, rsi            ; name str

    ; Compare attribute name
    lea rdi, [r12 + PyStrObject.data]

    ; Check "args"
    CSTRING rsi, "args"
    call ap_strcmp
    test eax, eax
    jz .get_args

    ; Check "__context__"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__context__"
    call ap_strcmp
    test eax, eax
    jz .get_context

    ; Check "__cause__"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__cause__"
    call ap_strcmp
    test eax, eax
    jz .get_cause

    ; Check "__suppress_context__"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__suppress_context__"
    call ap_strcmp
    test eax, eax
    jz .get_suppress

    ; Check "__traceback__"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__traceback__"
    call ap_strcmp
    test eax, eax
    jz .get_tb

    ; Check "code" (for SystemExit.code).  Only SystemExit has it; on any
    ; other exception `code` is an ordinary instance attribute.
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "code"
    call ap_strcmp
    test eax, eax
    jnz .not_code
    mov rdi, rbx
    lea rsi, [rel exc_SystemExit_type]
    call exc_isinstance
    test eax, eax
    jnz .get_code
.not_code:

    ; Check "value" (for StopIteration.value)
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "value"
    call ap_strcmp
    test eax, eax
    jz .get_value

    ; Not found — walk the type's MRO (for user-defined subclass attrs).
    ; Only the exact type's dict used to be consulted, so a method defined on
    ; an exception's *base* was invisible.
    mov r13, [rbx + PyObject.ob_type]   ; origin
    mov r14, r13                        ; walker
.eg_type_walk:
    test r14, r14
    jz .check_exc_dict
    mov rdi, [r14 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .eg_type_next
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .found_in_type
.eg_type_next:
    MRO_NEXT r14, r13
    jmp .eg_type_walk

.check_exc_dict:
    ; Check exc_dict for custom instance attributes
    mov rdi, [rbx + PyExceptionObject.exc_dict]
    test rdi, rdi
    jz .not_found
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .found_in_dict

.not_found:
    RET_NULL
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_in_dict:
    INCREF_VAL rax, rdx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.found_in_type:
    ; A plain function found on the class is a method and has to be bound;
    ; returning it raw made exc.method() call it with no self.  Descriptors
    ; are returned as they are, for LOAD_ATTR to unwrap.
    cmp edx, TAG_PTR
    jne .fit_raw
    mov rcx, [rax + PyObject.ob_type]
    extern func_type
    lea rdx, [rel func_type]
    cmp rcx, rdx
    je .fit_bind
    extern builtin_func_type
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    je .fit_bind
    mov edx, TAG_PTR
.fit_raw:
    INCREF_VAL rax, rdx     ; tag-aware INCREF (rdx = tag from dict_get)
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.fit_bind:
    mov rdi, rax
    mov rsi, rbx
    extern method_new
    call method_new
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.get_args:
    mov rax, [rbx + PyExceptionObject.exc_args]
    test rax, rax
    jz .return_empty_tuple
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.return_empty_tuple:
    xor edi, edi
    call tuple_new
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.get_code:
    ; An explicit `e.code = x` wins; otherwise code is args[0] when there is
    ; exactly one argument, the whole args tuple when there are more, and
    ; None when there are none.
    mov rdi, [rbx + PyExceptionObject.exc_dict]
    test rdi, rdi
    jz .code_from_args
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .found_in_dict
.code_from_args:
    mov rax, [rbx + PyExceptionObject.exc_args]
    test rax, rax
    jz .return_none
    mov rcx, [rax + PyTupleObject.ob_size]
    test rcx, rcx
    jz .return_none
    cmp rcx, 1
    jne .code_tuple
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rax, [rcx]
    INCREF_V rax, rdx
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.code_tuple:
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.get_context:
    mov rax, [rbx + PyExceptionObject.exc_context]
    test rax, rax
    jz .return_none
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.get_suppress:
    mov rax, [rbx + PyExceptionObject.exc_suppress]
    test rax, rax
    jz .suppress_false
    extern bool_true
    lea rax, [rel bool_true]
    jmp .suppress_ret
.suppress_false:
    extern bool_false
    lea rax, [rel bool_false]
.suppress_ret:
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.get_cause:
    mov rax, [rbx + PyExceptionObject.exc_cause]
    test rax, rax
    jz .return_none
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.get_tb:
    mov rax, [rbx + PyExceptionObject.exc_tb]
    test rax, rax
    jz .return_none
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.get_value:
    ; Return exc_args[0] if args is non-empty, else None
    mov rax, [rbx + PyExceptionObject.exc_args]
    test rax, rax
    jz .return_none
    ; Check if tuple has at least 1 element
    cmp qword [rax + PyTupleObject.ob_size], 0
    je .return_none
    ; Return args[0]
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rax, [rcx]
    INCREF_V rax, rdx
    V_UNPACK rax, rdx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.return_none:
    extern none_singleton
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC exc_getattr

; exc_setattr(PyExceptionObject *exc, PyStrObject *name, PyObject *value, int value_tag)
; Store a custom attribute on an exception object using exc_dict.
; rdi = exc, rsi = name, rdx = value, ecx = value_tag
global exc_setattr
DEF_FUNC exc_setattr
    push rbx
    mov rbx, rdi            ; exc

    ; Create exc_dict if needed
    mov rax, [rbx + PyExceptionObject.exc_dict]
    test rax, rax
    jnz .esa_have_dict
    push rsi
    push rdx
    push rcx
    call dict_new
    mov [rbx + PyExceptionObject.exc_dict], rax
    pop rcx
    pop rdx
    pop rsi
.esa_have_dict:
    mov rdi, [rbx + PyExceptionObject.exc_dict]
    ; rsi = name and rdx = value are both already Values
    call dict_set

    xor eax, eax            ; return 0 (success)
    xor edx, edx

    pop rbx
    leave
    ret
END_FUNC exc_setattr

; exc_isinstance(PyExceptionObject *exc, PyTypeObject *type) -> int (0/1)
; Check if exception is an instance of type, walking tp_base chain.
; If type is a tuple, checks each element.
extern tuple_type
DEF_FUNC_BARE exc_isinstance
    ; rdi = exc, rsi = target type (or tuple of types)
    ; The target is an arbitrary expression -- `except 5:` is legal syntax --
    ; so classify it before dereferencing.  An immediate's payload is not an
    ; address; this walked ob_type off it.  Putting the check here covers all
    ; five callers and the recursion through nested tuples below.
    V_TEST_PTR rsi, rax
    ja .not_a_class
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .tuple_match

    ; A class is anything whose type is one of the three metatypes: the
    ; builtin exception types carry exc_metatype, a `class E(Exception)`
    ; heaptype carries user_type_metatype, and everything else type_type.
    lea rcx, [rel exc_metatype]
    cmp rax, rcx
    je .is_class
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .is_class
    extern user_type_metatype
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    jne .not_a_class

.is_class:
    ; Single type: the exception's MRO, so a class with several bases is
    ; caught by an `except` naming any of them.
    mov rdi, [rdi + PyExceptionObject.ob_type]
    jmp type_is_subtype
.not_a_class:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "catching classes that do not inherit from BaseException is not allowed"
    call raise_exception
.not_match:
    xor eax, eax
    ret

.tuple_match:
    ; rsi = tuple of types. Check each element.
    push rbx
    push r12
    push r13
    mov rbx, rdi               ; save exc
    mov r12, [rsi + PyTupleObject.ob_item]       ; type payloads
    mov r13, [rsi + PyTupleObject.ob_size]        ; count
    xor ecx, ecx
.tuple_loop:
    cmp rcx, r13
    jge .tuple_no_match
    push rcx
    mov rdi, rbx               ; exc
    mov rsi, [r12 + rcx*8]    ; type element
    ; Recursive call for nested tuples
    call exc_isinstance
    pop rcx
    test eax, eax
    jnz .tuple_found
    inc rcx
    jmp .tuple_loop
.tuple_found:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    ret
.tuple_no_match:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    ret
END_FUNC exc_isinstance

; type_is_exc_subclass(PyTypeObject *type) -> int (0/1)
; Walk tp_base chain checking for a type with tp_dealloc == exc_dealloc.
; Detects user-defined exception classes (e.g., class MyError(Exception): pass)
global type_is_exc_subclass
DEF_FUNC_BARE type_is_exc_subclass
    lea rdx, [rel exc_dealloc]
    lea rcx, [rel eg_dealloc]
    mov r10, rdi                    ; origin of the walk
.tie_walk:
    test rdi, rdi
    jz .tie_no
    mov rax, [rdi + PyTypeObject.tp_dealloc]
    cmp rax, rdx
    je .tie_yes
    cmp rax, rcx
    je .tie_yes
    push r10
    mov rsi, rdi
    mov rdi, r10
    extern type_mro_next
    call type_mro_next
    pop r10
    mov rdi, rax
    jmp .tie_walk
.tie_yes:
    mov eax, 1
    ret
.tie_no:
    xor eax, eax
    ret
END_FUNC type_is_exc_subclass

; exc_type_from_id(int exc_id) -> PyTypeObject*
; Look up exception type from EXC_* constant.
DEF_FUNC_BARE exc_type_from_id
    lea rax, [rel exception_type_table]
    mov rax, [rax + rdi*8]
    ret
END_FUNC exc_type_from_id

; exc_type_call(PyTypeObject *type, PyObject **args, int64_t nargs) -> PyObject*
; tp_call for exception metatype. Creates an exception instance.
; rdi = exception type (the class being called, e.g. ValueError)
; rsi = args array
; rdx = nargs
ETC_EXC   equ 8
ETC_ARGS  equ 16
ETC_NARGS equ 24
ETC_FRAME equ 24
DEF_FUNC exc_type_call, ETC_FRAME
    push rbx
    push r12

    mov rbx, rdi            ; rbx = type
    mov [rbp - ETC_ARGS], rsi
    mov [rbp - ETC_NARGS], rdx

    ; Check if the type has its own constructor (e.g., ExceptionGroup).
    ; It lives in tp_new; tp_call would make instances callable.
    mov rax, [rbx + PyTypeObject.tp_new]
    test rax, rax
    jz .default_exc_create
    ; Delegate to the type's own constructor, which still returns a fat pair
    mov rdi, rbx
    mov rsi, [rbp - ETC_ARGS]
    mov rdx, [rbp - ETC_NARGS]
    pop r12
    pop rbx
    leave
    sub rsp, 8                  ; keep the callee's rsp 16-byte aligned
    call rax
    add rsp, 8
    V_PACK rax, rdx
    ret

.default_exc_create:
    ; Get message from args[0] if nargs >= 1
    test edx, edx
    jz .no_args
    mov rsi, [rsi]           ; args[0] is already the message Value
    jmp .create
.no_args:
    xor esi, esi             ; msg = NULL (no message)
    xor edx, edx             ; no tag
.create:
    ; Create exception: exc_new(type, msg, msg_tag)
    mov rdi, rbx
    call exc_new
    mov [rbp - ETC_EXC], rax

    ; Build args tuple from all arguments (not just the first one)
    ; exc_new already created a 0-or-1 element args tuple, replace if nargs > 1
    mov rcx, [rbp - ETC_NARGS]
    cmp rcx, 2
    jl .done

    ; Need to build a proper args tuple with all nargs items
    mov rdi, rcx
    call tuple_new
    mov r12, rax             ; r12 = new args tuple
    mov rcx, [rbp - ETC_NARGS]
    mov rsi, [rbp - ETC_ARGS]
    xor edx, edx
.copy_args:
    mov rcx, [rbp - ETC_NARGS]   ; reload loop limit (clobbered below)
    cmp rdx, rcx
    jge .replace_args
    mov rcx, rdx
    shl rcx, 3                    ; one Value per arg slot
    mov rdi, [rsi + rcx]          ; the argument Value
    INCREF_V rdi, r8
    mov r9, [r12 + PyTupleObject.ob_item]
    mov [r9 + rdx * 8], rdi
    inc rdx
    jmp .copy_args
.replace_args:
    ; DECREF old args tuple
    mov rdi, [rbp - ETC_EXC]
    mov rax, [rdi + PyExceptionObject.exc_args]
    test rax, rax
    jz .set_new_args
    push r12
    mov rdi, rax
    call obj_decref
    pop r12
.set_new_args:
    mov rdi, [rbp - ETC_EXC]
    mov [rdi + PyExceptionObject.exc_args], r12

.done:
    mov rax, [rbp - ETC_EXC]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
END_FUNC exc_type_call

; ============================================================================
; Traceback support
; ============================================================================

; traceback_new() -> PyTracebackObject*
; Allocates a new traceback with tb_next=NULL, tb_lineno=0.
global traceback_new
DEF_FUNC traceback_new
    mov edi, PyTracebackObject_size
    call ap_malloc
    mov qword [rax + PyTracebackObject.ob_refcnt], 1
    lea rcx, [rel traceback_type]
    mov [rax + PyTracebackObject.ob_type], rcx
    mov qword [rax + PyTracebackObject.tb_next], 0
    mov qword [rax + PyTracebackObject.tb_lineno], 0
    mov qword [rax + PyTracebackObject.tb_code], 0
    mov qword [rax + PyTracebackObject.tb_lasti], 0
    leave
    ret
END_FUNC traceback_new

; traceback_dealloc(PyTracebackObject *tb)
; XDECREF tb_next, free self.
global traceback_dealloc
DEF_FUNC traceback_dealloc
    push rbx
    push r12
    mov rbx, rdi
.td_node:
    ; Iterative, not recursive: a traceback chain is as deep as the call
    ; stack was, and freeing it recursively would overflow on exactly the
    ; deep-recursion case that produced it.
    mov rdi, [rbx + PyTracebackObject.tb_code]
    test rdi, rdi
    jz .td_no_code
    mov qword [rbx + PyTracebackObject.tb_code], 0
    call obj_decref
.td_no_code:
    mov r12, [rbx + PyTracebackObject.tb_next]
    mov rdi, rbx
    call ap_free
    test r12, r12
    jz .td_done
    dec qword [r12 + PyTracebackObject.ob_refcnt]
    jnz .td_done                   ; still referenced elsewhere
    mov rbx, r12
    jmp .td_node
.td_done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC traceback_dealloc

; traceback_getattr(PyTracebackObject *tb, PyStrObject *name) -> (rax, edx)
; Handles tb_lineno, tb_next, tb_frame attributes.
global traceback_getattr
DEF_FUNC traceback_getattr
    push rbx
    push r12

    mov rbx, rdi            ; tb
    mov r12, rsi            ; name str

    ; Check "tb_lineno"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "tb_lineno"
    call ap_strcmp
    test eax, eax
    jz .tb_get_lineno

    ; Check "tb_next"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "tb_next"
    call ap_strcmp
    test eax, eax
    jz .tb_get_next

    ; Check "tb_frame"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "tb_frame"
    call ap_strcmp
    test eax, eax
    jz .tb_return_none

    ; Not found
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.tb_get_lineno:
    mov rax, [rbx + PyTracebackObject.tb_lineno]
    mov edx, TAG_SMALLINT
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.tb_get_next:
    mov rax, [rbx + PyTracebackObject.tb_next]
    test rax, rax
    jz .tb_return_none
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.tb_return_none:
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC traceback_getattr

; ============================================================================
; Data section - Exception type objects and name strings
; ============================================================================
section .data

; Exception type name strings
exc_name_BaseException:     db "BaseException", 0
exc_name_Exception:         db "Exception", 0
exc_name_TypeError:         db "TypeError", 0
exc_name_ValueError:        db "ValueError", 0
exc_name_KeyError:          db "KeyError", 0
exc_name_IndexError:        db "IndexError", 0
exc_name_AttributeError:    db "AttributeError", 0
exc_name_NameError:         db "NameError", 0
exc_name_UnboundLocalError: db "UnboundLocalError", 0
exc_name_RuntimeError:      db "RuntimeError", 0
exc_name_StopIteration:     db "StopIteration", 0
exc_name_ZeroDivisionError: db "ZeroDivisionError", 0
exc_name_ImportError:       db "ImportError", 0
exc_name_NotImplementedError: db "NotImplementedError", 0
exc_name_FileNotFoundError: db "FileNotFoundError", 0
exc_name_OverflowError:     db "OverflowError", 0
exc_name_AssertionError:    db "AssertionError", 0
exc_name_KeyboardInterrupt: db "KeyboardInterrupt", 0
exc_name_MemoryError:       db "MemoryError", 0
exc_name_RecursionError:    db "RecursionError", 0
exc_name_SystemExit:        db "SystemExit", 0
exc_name_OSError:           db "OSError", 0
exc_name_LookupError:       db "LookupError", 0
exc_name_ArithmeticError:   db "ArithmeticError", 0
exc_name_UnicodeError:      db "UnicodeError", 0
exc_name_Warning:           db "Warning", 0
exc_name_DeprecationWarning: db "DeprecationWarning", 0
exc_name_UserWarning:       db "UserWarning", 0
exc_name_CancelledError:    db "CancelledError", 0
exc_name_StopAsyncIteration: db "StopAsyncIteration", 0
exc_name_TimeoutError:      db "TimeoutError", 0
exc_name_GeneratorExit:     db "GeneratorExit", 0
exc_name_ModuleNotFoundError: db "ModuleNotFoundError", 0
exc_name_SyntaxError:       db "SyntaxError", 0
exc_name_EOFError:          db "EOFError", 0
exc_name_UnicodeDecodeError: db "UnicodeDecodeError", 0
exc_name_UnicodeEncodeError: db "UnicodeEncodeError", 0
exc_name_ConnectionError:   db "ConnectionError", 0
exc_name_ConnectionResetError: db "ConnectionResetError", 0
exc_name_ConnectionRefusedError: db "ConnectionRefusedError", 0
exc_name_ConnectionAbortedError: db "ConnectionAbortedError", 0
exc_name_BrokenPipeError:   db "BrokenPipeError", 0
exc_name_PermissionError:   db "PermissionError", 0
exc_name_IsADirectoryError: db "IsADirectoryError", 0
exc_name_NotADirectoryError: db "NotADirectoryError", 0
exc_name_ProcessLookupError: db "ProcessLookupError", 0
exc_name_ChildProcessError: db "ChildProcessError", 0
exc_name_BlockingIOError:   db "BlockingIOError", 0
exc_name_InterruptedError:  db "InterruptedError", 0
exc_name_FloatingPointError: db "FloatingPointError", 0
exc_name_BufferError:       db "BufferError", 0
exc_name_ReferenceError:    db "ReferenceError", 0
exc_name_SystemError:       db "SystemError", 0
exc_name_RuntimeWarning:    db "RuntimeWarning", 0
exc_name_FutureWarning:     db "FutureWarning", 0
exc_name_ImportWarning:     db "ImportWarning", 0
exc_name_UnicodeWarning:    db "UnicodeWarning", 0
exc_name_ResourceWarning:   db "ResourceWarning", 0
exc_name_BytesWarning:      db "BytesWarning", 0
exc_name_PendingDeprecationWarning: db "PendingDeprecationWarning", 0
exc_name_SyntaxWarning:     db "SyntaxWarning", 0
exc_name_EncodingWarning:   db "EncodingWarning", 0

; Exception metatype - provides tp_call so exception types can be called
; e.g., ValueError("msg") works via CALL opcode
align 8
global exc_metatype
exc_metatype:
    dq 1                    ; ob_refcnt (immortal)
    dq type_type            ; ob_type
    dq exc_meta_name        ; tp_name
    dq TYPE_OBJECT_SIZE     ; tp_basicsize (PyTypeObject size)
    dq 0                    ; tp_dealloc (types are immortal)
    dq type_repr            ; tp_repr — <class 'ExcName'>
    dq type_repr            ; tp_str — same as repr
    dq 0                    ; tp_hash
    dq exc_type_call        ; tp_call  <-- enables CALL on exception types
    dq type_getattr         ; tp_getattr — enables __name__ etc.
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq 0                    ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq 0                    ; tp_flags (no HAVE_GC — exc types are static, not gc_alloc'd)
    dq 0                    ; tp_bases
    dq 0                    ; tp_traverse
    dq 0                    ; tp_clear
    dq 0 ; tp_dictoffset

exc_meta_name: db "exception_metatype", 0

; Traceback type object (immortal)
align 8
global traceback_type
traceback_type:
    dq 1                    ; ob_refcnt (immortal)
    dq type_type            ; ob_type
    dq tb_type_name         ; tp_name
    dq PyTracebackObject_size ; tp_basicsize
    dq traceback_dealloc    ; tp_dealloc
    dq 0                    ; tp_repr
    dq 0                    ; tp_str
    dq 0                    ; tp_hash
    dq 0                    ; tp_call
    dq traceback_getattr    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq 0                    ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
tb_type_name: db "traceback", 0

; Macro to define an exception type singleton
; %1 = label, %2 = name string, %3 = tp_base (or 0)
%macro DEF_EXC_TYPE 3
align 8
global %1
%1:
    dq 1                    ; ob_refcnt (immortal)
    dq exc_metatype         ; ob_type (metatype with tp_call)
    dq %2                   ; tp_name
    dq PyExceptionObject_size ; tp_basicsize
    dq exc_dealloc          ; tp_dealloc
    dq exc_repr             ; tp_repr
    dq exc_str              ; tp_str
    dq 0                    ; tp_hash
    dq 0                    ; tp_call
    dq exc_getattr          ; tp_getattr
    dq exc_setattr          ; tp_setattr
    dq 0                    ; tp_richcompare
    dq 0                    ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq %3                   ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq TYPE_FLAG_HAVE_GC    ; tp_flags
    dq 0                    ; tp_bases
    dq exc_traverse         ; tp_traverse
    dq exc_clear_gc         ; tp_clear
    dq 0         ; tp_dictoffset
%endmacro

; Define all exception types
extern object_type
DEF_EXC_TYPE exc_BaseException_type, exc_name_BaseException, object_type
DEF_EXC_TYPE exc_Exception_type, exc_name_Exception, exc_BaseException_type
DEF_EXC_TYPE exc_TypeError_type, exc_name_TypeError, exc_Exception_type
DEF_EXC_TYPE exc_ValueError_type, exc_name_ValueError, exc_Exception_type
DEF_EXC_TYPE exc_KeyError_type, exc_name_KeyError, exc_LookupError_type
DEF_EXC_TYPE exc_IndexError_type, exc_name_IndexError, exc_LookupError_type
DEF_EXC_TYPE exc_AttributeError_type, exc_name_AttributeError, exc_Exception_type
DEF_EXC_TYPE exc_NameError_type, exc_name_NameError, exc_Exception_type
DEF_EXC_TYPE exc_UnboundLocalError_type, exc_name_UnboundLocalError, exc_NameError_type
DEF_EXC_TYPE exc_RuntimeError_type, exc_name_RuntimeError, exc_Exception_type
DEF_EXC_TYPE exc_StopIteration_type, exc_name_StopIteration, exc_Exception_type
DEF_EXC_TYPE exc_ZeroDivisionError_type, exc_name_ZeroDivisionError, exc_ArithmeticError_type
DEF_EXC_TYPE exc_ImportError_type, exc_name_ImportError, exc_Exception_type
DEF_EXC_TYPE exc_NotImplementedError_type, exc_name_NotImplementedError, exc_RuntimeError_type
DEF_EXC_TYPE exc_FileNotFoundError_type, exc_name_FileNotFoundError, exc_OSError_type
DEF_EXC_TYPE exc_OverflowError_type, exc_name_OverflowError, exc_ArithmeticError_type
DEF_EXC_TYPE exc_AssertionError_type, exc_name_AssertionError, exc_Exception_type
DEF_EXC_TYPE exc_KeyboardInterrupt_type, exc_name_KeyboardInterrupt, exc_BaseException_type
DEF_EXC_TYPE exc_MemoryError_type, exc_name_MemoryError, exc_Exception_type
DEF_EXC_TYPE exc_RecursionError_type, exc_name_RecursionError, exc_RuntimeError_type
DEF_EXC_TYPE exc_SystemExit_type, exc_name_SystemExit, exc_BaseException_type
DEF_EXC_TYPE exc_OSError_type, exc_name_OSError, exc_Exception_type
DEF_EXC_TYPE exc_LookupError_type, exc_name_LookupError, exc_Exception_type
DEF_EXC_TYPE exc_ArithmeticError_type, exc_name_ArithmeticError, exc_Exception_type
DEF_EXC_TYPE exc_UnicodeError_type, exc_name_UnicodeError, exc_ValueError_type
DEF_EXC_TYPE exc_Warning_type, exc_name_Warning, exc_Exception_type
DEF_EXC_TYPE exc_DeprecationWarning_type, exc_name_DeprecationWarning, exc_Warning_type
DEF_EXC_TYPE exc_UserWarning_type, exc_name_UserWarning, exc_Warning_type
DEF_EXC_TYPE exc_CancelledError_type, exc_name_CancelledError, exc_BaseException_type
DEF_EXC_TYPE exc_StopAsyncIteration_type, exc_name_StopAsyncIteration, exc_Exception_type
DEF_EXC_TYPE exc_TimeoutError_type, exc_name_TimeoutError, exc_Exception_type
DEF_EXC_TYPE exc_GeneratorExit_type, exc_name_GeneratorExit, exc_BaseException_type
DEF_EXC_TYPE exc_ModuleNotFoundError_type, exc_name_ModuleNotFoundError, exc_ImportError_type
DEF_EXC_TYPE exc_SyntaxError_type, exc_name_SyntaxError, exc_Exception_type
DEF_EXC_TYPE exc_EOFError_type, exc_name_EOFError, exc_Exception_type
DEF_EXC_TYPE exc_UnicodeDecodeError_type, exc_name_UnicodeDecodeError, exc_UnicodeError_type
DEF_EXC_TYPE exc_UnicodeEncodeError_type, exc_name_UnicodeEncodeError, exc_UnicodeError_type
DEF_EXC_TYPE exc_ConnectionError_type, exc_name_ConnectionError, exc_OSError_type
DEF_EXC_TYPE exc_ConnectionResetError_type, exc_name_ConnectionResetError, exc_ConnectionError_type
DEF_EXC_TYPE exc_ConnectionRefusedError_type, exc_name_ConnectionRefusedError, exc_ConnectionError_type
DEF_EXC_TYPE exc_ConnectionAbortedError_type, exc_name_ConnectionAbortedError, exc_ConnectionError_type
DEF_EXC_TYPE exc_BrokenPipeError_type, exc_name_BrokenPipeError, exc_ConnectionError_type
DEF_EXC_TYPE exc_PermissionError_type, exc_name_PermissionError, exc_OSError_type
DEF_EXC_TYPE exc_IsADirectoryError_type, exc_name_IsADirectoryError, exc_OSError_type
DEF_EXC_TYPE exc_NotADirectoryError_type, exc_name_NotADirectoryError, exc_OSError_type
DEF_EXC_TYPE exc_ProcessLookupError_type, exc_name_ProcessLookupError, exc_OSError_type
DEF_EXC_TYPE exc_ChildProcessError_type, exc_name_ChildProcessError, exc_OSError_type
DEF_EXC_TYPE exc_BlockingIOError_type, exc_name_BlockingIOError, exc_OSError_type
DEF_EXC_TYPE exc_InterruptedError_type, exc_name_InterruptedError, exc_OSError_type
DEF_EXC_TYPE exc_FloatingPointError_type, exc_name_FloatingPointError, exc_ArithmeticError_type
DEF_EXC_TYPE exc_BufferError_type, exc_name_BufferError, exc_Exception_type
DEF_EXC_TYPE exc_ReferenceError_type, exc_name_ReferenceError, exc_Exception_type
DEF_EXC_TYPE exc_SystemError_type, exc_name_SystemError, exc_Exception_type
DEF_EXC_TYPE exc_RuntimeWarning_type, exc_name_RuntimeWarning, exc_Warning_type
DEF_EXC_TYPE exc_FutureWarning_type, exc_name_FutureWarning, exc_Warning_type
DEF_EXC_TYPE exc_ImportWarning_type, exc_name_ImportWarning, exc_Warning_type
DEF_EXC_TYPE exc_UnicodeWarning_type, exc_name_UnicodeWarning, exc_Warning_type
DEF_EXC_TYPE exc_ResourceWarning_type, exc_name_ResourceWarning, exc_Warning_type
DEF_EXC_TYPE exc_BytesWarning_type, exc_name_BytesWarning, exc_Warning_type
DEF_EXC_TYPE exc_PendingDeprecationWarning_type, exc_name_PendingDeprecationWarning, exc_Warning_type
DEF_EXC_TYPE exc_SyntaxWarning_type, exc_name_SyntaxWarning, exc_Warning_type
DEF_EXC_TYPE exc_EncodingWarning_type, exc_name_EncodingWarning, exc_Warning_type

; Exception type lookup table indexed by EXC_* constants
align 8
global exception_type_table
exception_type_table:
    dq exc_BaseException_type        ; EXC_BASE_EXCEPTION = 0
    dq exc_Exception_type            ; EXC_EXCEPTION = 1
    dq exc_TypeError_type            ; EXC_TYPE_ERROR = 2
    dq exc_ValueError_type           ; EXC_VALUE_ERROR = 3
    dq exc_KeyError_type             ; EXC_KEY_ERROR = 4
    dq exc_IndexError_type           ; EXC_INDEX_ERROR = 5
    dq exc_AttributeError_type       ; EXC_ATTRIBUTE_ERROR = 6
    dq exc_NameError_type            ; EXC_NAME_ERROR = 7
    dq exc_RuntimeError_type         ; EXC_RUNTIME_ERROR = 8
    dq exc_StopIteration_type        ; EXC_STOP_ITERATION = 9
    dq exc_ZeroDivisionError_type    ; EXC_ZERO_DIVISION = 10
    dq exc_ImportError_type          ; EXC_IMPORT_ERROR = 11
    dq exc_NotImplementedError_type  ; EXC_NOT_IMPLEMENTED = 12
    dq exc_FileNotFoundError_type    ; EXC_FILE_NOT_FOUND = 13
    dq exc_OverflowError_type       ; EXC_OVERFLOW_ERROR = 14
    dq exc_AssertionError_type       ; EXC_ASSERTION_ERROR = 15
    dq exc_KeyboardInterrupt_type    ; EXC_KEYBOARD_INTERRUPT = 16
    dq exc_MemoryError_type          ; EXC_MEMORY_ERROR = 17
    dq exc_RecursionError_type       ; EXC_RECURSION_ERROR = 18
    dq exc_SystemExit_type           ; EXC_SYSTEM_EXIT = 19
    dq exc_OSError_type              ; EXC_OS_ERROR = 20
    dq exc_LookupError_type          ; EXC_LOOKUP_ERROR = 21
    dq exc_ArithmeticError_type      ; EXC_ARITHMETIC_ERROR = 22
    dq exc_UnicodeError_type         ; EXC_UNICODE_ERROR = 23
    dq exc_BaseExceptionGroup_type   ; EXC_BASE_EXCEPTION_GROUP = 24
    dq exc_ExceptionGroup_type       ; EXC_EXCEPTION_GROUP = 25
    dq exc_CancelledError_type       ; EXC_CANCELLED_ERROR = 26
    dq exc_StopAsyncIteration_type   ; EXC_STOP_ASYNC_ITERATION = 27
    dq exc_TimeoutError_type         ; EXC_TIMEOUT_ERROR = 28
