; buildclass.asm - Building a class object
;
; type.__new__, type_from_parts, __build_class__, and the keyword handling
; between them.  This is what `class C(B, metaclass=M): ...` compiles down to:
; resolve the metaclass, run __prepare__, execute the body into the namespace
; it returns, build the type, then apply __set_name__ to every descriptor in it.

%include "macros.inc"
%include "object.inc"

extern dict_new
extern dunder_call_3
extern dunder_lookup
extern dict_get
extern dict_set
extern str_from_cstr
extern str_from_cstr_heap
extern obj_getattr_opt
extern obj_call_n
extern tuple_new
extern obj_str
extern obj_incref
extern obj_decref
extern obj_dealloc
extern none_singleton
extern int_from_i64
extern str_type
extern bool_type
extern float_type
extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern fatal_error
extern raise_exception
extern build_class_pending
extern sys_write
extern range_new
extern int_to_i64
extern current_exception
extern obj_as_index
extern init_iter_types
extern obj_repr
extern eval_frame
extern frame_new
extern frame_free
extern ap_memcpy
extern instance_dealloc
extern instance_repr
extern instance_getattr
extern instance_setattr
extern type_call
extern user_type_metatype
extern super_type
extern staticmethod_type
extern classmethod_type
extern property_type
extern func_type
extern type_type
extern list_type
extern dict_type
extern tuple_type
extern set_type
extern bytes_type
extern ap_strcmp
extern kw_names_pending
extern object_type

; New builtin function implementations (in builtins_extra.asm)
extern builtin_abs
extern builtin_divmod
extern builtin_int_fn
extern int_type_call
extern str_type_call
extern bool_type_call
extern float_type_call
extern bytearray_type_call
extern memoryview_type_call
extern bytearray_type
extern memoryview_type
extern builtin_str_fn
extern builtin_ord
extern builtin_chr
extern builtin_hex
extern builtin_id
extern builtin_hash_fn
extern builtin_callable
extern builtin_iter_fn
extern builtin_next_fn
extern builtin_any
extern builtin_all
extern builtin_sum
extern builtin_min
extern builtin_max
extern builtin_getattr
extern builtin_hasattr
extern builtin_setattr

; Iterator builtins (in itertools.asm)
extern builtin_enumerate
extern builtin_zip
extern builtin_map
extern builtin_filter
extern builtin_reversed
extern builtin_sorted
extern builtin_chain
extern builtin_globals
extern builtin_locals
extern builtin_dir
extern builtin_breakpoint

; Exception types
extern exc_BaseException_type
extern exc_Exception_type
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_KeyError_type
extern exc_IndexError_type
extern exc_AttributeError_type
extern exc_NameError_type
extern exc_UnboundLocalError_type
extern exc_RuntimeError_type
extern exc_StopIteration_type
extern exc_ZeroDivisionError_type
extern exc_ImportError_type
extern exc_NotImplementedError_type
extern exc_OverflowError_type
extern exc_AssertionError_type
extern exc_OSError_type
extern exc_LookupError_type
extern exc_ArithmeticError_type
extern exc_RecursionError_type
extern exc_MemoryError_type
extern exc_KeyboardInterrupt_type
extern exc_SystemExit_type
extern exc_UnicodeError_type
extern exc_Warning_type
extern exc_DeprecationWarning_type
extern exc_UserWarning_type
extern exc_BaseExceptionGroup_type
extern exc_ExceptionGroup_type
extern exc_CancelledError_type
extern exc_StopAsyncIteration_type
extern exc_TimeoutError_type

; --- moved to a sibling file by the split ---
extern add_builtin
extern add_builtin_str
extern add_builtin_type
extern add_exc_type_builtin
extern bi_builtins_name
extern bi_dunder_name
extern bi_name_ArithmeticError
extern bi_name_AssertionError
extern bi_name_AttributeError
extern bi_name_BaseException
extern bi_name_BaseExceptionGroup
extern bi_name_BlockingIOError
extern bi_name_BrokenPipeError
extern bi_name_BufferError
extern bi_name_BytesWarning
extern bi_name_CancelledError
extern bi_name_ChildProcessError
extern bi_name_ConnectionAbortedError
extern bi_name_ConnectionError
extern bi_name_ConnectionRefusedError
extern bi_name_ConnectionResetError
extern bi_name_DeprecationWarning
extern bi_name_EOFError
extern bi_name_Ellipsis
extern bi_name_EncodingWarning
extern bi_name_Exception
extern bi_name_ExceptionGroup
extern bi_name_FloatingPointError
extern bi_name_FutureWarning
extern bi_name_GeneratorExit
extern bi_name_ImportError
extern bi_name_ImportWarning
extern bi_name_IndentationError
extern bi_name_IndexError
extern bi_name_InterruptedError
extern bi_name_IsADirectoryError
extern bi_name_KeyError
extern bi_name_KeyboardInterrupt
extern bi_name_LookupError
extern bi_name_MemoryError
extern bi_name_ModuleNotFoundError
extern bi_name_NameError
extern bi_name_NotADirectoryError
extern bi_name_NotImplemented
extern bi_name_NotImplementedError
extern bi_name_OSError
extern bi_name_OverflowError
extern bi_name_PendingDeprecationWarning
extern bi_name_PermissionError
extern bi_name_ProcessLookupError
extern bi_name_RecursionError
extern bi_name_ReferenceError
extern bi_name_ResourceWarning
extern bi_name_RuntimeError
extern bi_name_RuntimeWarning
extern bi_name_StopAsyncIteration
extern bi_name_StopIteration
extern bi_name_SyntaxError
extern bi_name_SyntaxWarning
extern bi_name_SystemError
extern bi_name_SystemExit
extern bi_name_TabError
extern bi_name_TimeoutError
extern bi_name_TypeError
extern bi_name_UnboundLocalError
extern bi_name_UnicodeDecodeError
extern bi_name_UnicodeEncodeError
extern bi_name_UnicodeError
extern bi_name_UnicodeWarning
extern bi_name_UserWarning
extern bi_name_ValueError
extern bi_name_Warning
extern bi_name_ZeroDivisionError
extern bi_name___import__
extern bi_name_abs
extern bi_name_aiter
extern bi_name_all
extern bi_name_anext
extern bi_name_any
extern bi_name_ascii
extern bi_name_bin
extern bi_name_bool
extern bi_name_breakpoint
extern bi_name_build_class
extern bi_name_bytearray
extern bi_name_bytes
extern bi_name_callable
extern bi_name_chain
extern bi_name_chr
extern bi_name_classmethod
extern bi_name_compile
extern bi_name_delattr
extern bi_name_dict
extern bi_name_dir
extern bi_name_divmod
extern bi_name_enumerate
extern bi_name_eval
extern bi_name_exec
extern bi_name_filter
extern bi_name_float
extern bi_name_format
extern bi_name_frozenset
extern bi_name_getattr
extern bi_name_globals
extern bi_name_hasattr
extern bi_name_hash
extern bi_name_hex
extern bi_name_id
extern bi_name_input
extern bi_name_int
extern bi_name_isinstance
extern bi_name_issubclass
extern bi_name_iter
extern bi_name_len
extern bi_name_list
extern bi_name_locals
extern bi_name_map
extern bi_name_max
extern bi_name_memoryview
extern bi_name_min
extern bi_name_next
extern bi_name_object
extern bi_name_oct
extern bi_name_open
extern bi_name_ord
extern bi_name_pow
extern bi_name_print
extern bi_name_property
extern bi_name_range
extern bi_name_repr
extern bi_name_reversed
extern bi_name_round
extern bi_name_set
extern bi_name_setattr
extern bi_name_slice
extern bi_name_sorted
extern bi_name_staticmethod
extern bi_name_str
extern bi_name_sum
extern bi_name_super
extern bi_name_tuple
extern bi_name_type
extern bi_name_vars
extern bi_name_zip
extern build_class_obj
extern builtin_bool
extern builtin_float
extern builtin_func_call
extern builtin_func_dealloc
extern builtin_func_name_str
extern builtin_func_new
extern builtin_func_new_checked
extern builtin_func_repr
extern builtin_func_repr_unknown_str
extern builtin_func_type
extern builtin_isinstance
extern builtin_issubclass
extern builtin_len
extern builtin_print
extern builtin_range
extern builtin_repr
extern builtin_type
extern builtins_dict_global
extern builtins_init

section .text

;; ============================================================================
;; type.__new__(mcls, name, bases, ns) -> a new class whose metatype is mcls
;;
;; A metaclass __new__ almost always ends in
;; `super().__new__(mcls, name, bases, ns)`, and without this that resolved to
;; object.__new__ and produced an *instance* of the metaclass rather than a
;; class.  ABCMeta is written exactly that way, so abc.py depends on it.
;; ============================================================================
global type_method_new
DEF_FUNC type_method_new
    push rbx
    push r12
    ; type.__new__(mcls, name, bases, ns, **kwds): the keywords are for
    ; __init_subclass__ and are not ours to reject.
    cmp rsi, 4
    jl .tmn_error
    mov rbx, rdi                    ; args
    mov r12, [rdi]                  ; mcls

    mov rdi, [rbx + 8]              ; name
    mov rsi, [rbx + 16]             ; bases
    mov rdx, [rbx + 24]             ; namespace
    ; type_from_parts adopts a reference to each
    push rdi
    call obj_incref
    pop rdi
    push rdi
    mov rdi, rdx
    call obj_incref
    pop rdi
    mov rsi, [rbx + 16]
    mov rdx, [rbx + 24]
    call type_from_parts
    test rax, rax
    jz .tmn_failed                  ; a __set_name__ raised, and it is pending

    ; The metatype is whatever __new__ was handed, not the default.
    mov [rax + PyObject.ob_type], r12
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tmn_failed:
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret

.tmn_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "type.__new__() takes at least 3 arguments"
    call raise_exception
END_FUNC type_method_new


;; ============================================================================
;; type_apply_set_name(PyTypeObject *cls, PyDictObject *ns)
;;
;; Call __set_name__(owner, name) on every value in the class body that defines
;; one, once the class exists.  It is the hook a descriptor uses to learn what
;; it was assigned to, and enum is built on it: each member starts as a
;; _proto_member and __set_name__ is what replaces it with the real member.
;;
;; The names are snapshotted first, because a __set_name__ is entitled to
;; setattr on the owner -- which is this very dict, and rehashing it under the
;; walk would lose entries.
;;
;; Returns 0 when one of them raised.  Returning the class anyway would leave
;; the exception pending with nothing to attach it to, and the next opcode to
;; look at a NULL would be the one that crashed.
;; ============================================================================
TSN_CLS   equ 8
TSN_NS    equ 16
TSN_KEYS  equ 24
TSN_I     equ 32
TSN_N     equ 40
TSN_NAME  equ 48
TSN_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC type_apply_set_name, TSN_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - TSN_CLS], rdi
    mov [rbp - TSN_NS], rsi
    test rsi, rsi
    jz .done

    ; --- snapshot the keys ---
    mov r12, [rsi + PyDictObject.capacity]
    mov r13, [rsi + PyDictObject.entries]
    xor ebx, ebx
    xor ecx, ecx
.count:
    cmp rcx, r12
    jae .counted
    imul rax, rcx, DICT_ENTRY_SIZE
    cmp qword [r13 + rax + DictEntry.key], 0
    je .count_next
    inc rbx
.count_next:
    inc rcx
    jmp .count
.counted:
    test rbx, rbx
    jz .done
    mov [rbp - TSN_N], rbx
    mov rdi, rbx
    call tuple_new
    test rax, rax
    jz .done
    mov [rbp - TSN_KEYS], rax
    mov rdx, [rax + PyTupleObject.ob_item]
    mov rsi, [rbp - TSN_NS]
    mov r12, [rsi + PyDictObject.capacity]
    mov r13, [rsi + PyDictObject.entries]
    xor ebx, ebx
    xor ecx, ecx
.fill:
    cmp rcx, r12
    jae .filled
    imul rax, rcx, DICT_ENTRY_SIZE
    mov rdi, [r13 + rax + DictEntry.key]
    test rdi, rdi
    jz .fill_next
    INCREF_V rdi, r8
    mov [rdx + rbx*8], rdi
    inc rbx
.fill_next:
    inc rcx
    jmp .fill
.filled:

    ; --- call each value's __set_name__, if its TYPE defines one ---
    mov qword [rbp - TSN_I], 0
.loop:
    mov rax, [rbp - TSN_I]
    cmp rax, [rbp - TSN_N]
    jae .release
    mov rcx, [rbp - TSN_KEYS]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov rcx, [rcx + rax*8]
    mov [rbp - TSN_NAME], rcx

    mov rdi, [rbp - TSN_NS]
    mov rsi, rcx
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .next                            ; deleted while we walked
    ; The classification is the TAG's to make: after V_UNPACK rax holds a
    ; payload, and testing a payload as if it were a Value calls a small int a
    ; pointer.
    cmp edx, TAG_PTR
    jne .next
    test rax, rax
    jz .next
    mov rbx, rax

    ; Looked up on the type, not the instance: an instance attribute called
    ; __set_name__ is not the hook.
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel tsn_name]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .next

    mov rdi, rbx                        ; self = the value
    mov rsi, [rbp - TSN_CLS]            ; owner
    mov rdx, [rbp - TSN_NAME]           ; name
    lea rcx, [rel tsn_name]
    mov r8d, TAG_PTR
    call dunder_call_3
    V_UNPACK rax, rdx
    test edx, edx
    jz .raised
    mov rdi, rax
    DECREF_V rdi, rsi
.next:
    inc qword [rbp - TSN_I]
    jmp .loop

.release:
    mov rdi, [rbp - TSN_KEYS]
    call obj_decref
.done:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
.raised:
    mov rdi, [rbp - TSN_KEYS]
    call obj_decref
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_apply_set_name

;; ============================================================================
;; type_from_parts(rdi = name str, rsi = bases tuple or NULL, rdx = namespace dict)
;;   -> rax = the new type object, one strong reference
;;
;; The heaptype construction shared by __build_class__ and the three-argument
;; type().  Extracted rather than duplicated: type() used to fall through to
;; type_call's .normal_type_call, which treats type_type as an ordinary class
;; -- it allocated an instance-sized block and let type fields be written into
;; it, printing <class ''> and then aborting with a double free.
;;
;; The frame is built by hand, not by DEF_FUNC's size argument, so that the
;; body's [rbp - TFP_BASE] slot keeps meaning what it meant inside __build_class__.
;; ============================================================================
; The class keywords `class C(B, tag="t")` carries.  __init_subclass__ is
; called from inside type_from_parts, which never saw them -- so a base that
; declares `def __init_subclass__(cls, **kw)` was handed an empty kw.  Set
; around the call and cleared by it, the same convention kw_names_pending uses.
section .data
align 8
global class_kwnames_pending
class_kwnames_pending: dq 0
global class_kwvalues_pending
class_kwvalues_pending: dq 0

section .text
global type_from_parts
DEF_FUNC type_from_parts
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40             ; the epilogue's `add rsp` must match this

TFP_BASE  equ 48            ; the layout base: the widest of the bases
TFP_BASES equ 56            ; the bases tuple, or NULL
TFP_EXC   equ 64            ; current_exception, to tell a raise from a miss
    mov r14, rdi                ; class name str
    mov r15, rdx                ; namespace dict, becomes tp_dict
    mov [rbp - TFP_BASES], rsi
    DUNDER_EXC_SAVE [rbp - TFP_EXC]

    ; The layout base is the widest base, not simply the first: `class
    ; C(Mixin, list)` has to be laid out as a list.  Ties go to the earlier
    ; base, which is what CPython's solid-base rule gives for the ordinary
    ; single-inheritance case.
    xor eax, eax                ; best base
    test rsi, rsi
    jz .tfp_base_done
    mov rcx, [rsi + PyTupleObject.ob_size]
    mov r8, [rsi + PyTupleObject.ob_item]
    xor r9, r9
    xor r10, r10                ; best basicsize
.tfp_base_scan:
    cmp r9, rcx
    jge .tfp_base_done
    mov r11, [r8 + r9*8]
    test r11, r11
    jz .tfp_base_next
    mov rdx, [r11 + PyTypeObject.tp_basicsize]
    cmp rdx, r10
    jbe .tfp_base_next
    mov r10, rdx
    mov rax, r11
.tfp_base_next:
    inc r9
    jmp .tfp_base_scan
.tfp_base_done:
    mov [rbp - TFP_BASE], rax   ; layout base, or NULL
    mov rdx, r15                ; restore namespace (scan clobbered rdx)

    ; Allocate the type object (GC-tracked)
    mov edi, TYPE_OBJECT_SIZE
    lea rsi, [rel user_type_metatype]
    call gc_alloc
    mov r12, rax            ; r12 = new type object (ob_refcnt=1, ob_type set)
    mov [rel build_class_pending], rax  ; register for exception cleanup

    ; Zero-fill the type object (skip ob_refcnt and ob_type, already set by gc_alloc)
    lea rdi, [r12 + 16]
    xor eax, eax
    mov ecx, (TYPE_OBJECT_SIZE - 16) / 8
    rep stosq

    ; tp_name: point to class_name string's data area
    lea rax, [r14 + PyStrObject.data]
    mov [r12 + PyTypeObject.tp_name], rax

    ; Instance layout.  A heaptype embeds its base's layout and puts its own
    ; __dict__ immediately after it, so both numbers come from the base:
    ; tp_dictoffset is the base's basicsize, and tp_basicsize is that plus
    ; the dict word.  With no base that yields 16 and 24 -- exactly
    ; PyInstanceObject, which is where those constants came from.
    ;
    ; A variable-size base such as str keeps its data inline, so there is no
    ; fixed offset past the header for a dict.  It gets one at the tail
    ; instead, which is what TP_DICT_AT_TAIL means; bytes and __slots__ classes
    ; still get none.
    mov qword [r12 + PyTypeObject.tp_basicsize], PyInstanceObject_size
    mov qword [r12 + PyTypeObject.tp_dictoffset], PyInstanceObject.inst_dict
    mov rax, [rbp - TFP_BASE]               ; base class
    test rax, rax
    jz .bc_layout_done
    ; If the base already has a dict slot -- another heaptype, or an int
    ; subclass -- share it rather than adding a second one, which would
    ; collide with whatever the base put there.
    mov rcx, [rax + PyTypeObject.tp_dictoffset]
    test rcx, rcx
    jnz .bc_layout_inherit

    mov rcx, [rax + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_STR_SUBCLASS
    jnz .bc_layout_no_dict
    test rcx, TYPE_FLAG_INT_SUBCLASS
    jnz .bc_layout_done             ; int subclasses wrap rather than embed

    ; bytes keeps its data inline exactly as str does, so its subclasses get
    ; the same tail dict.  Putting one at the base's basicsize instead landed
    ; it *inside* the data: `B(bytes)` with an attribute corrupted itself.
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .bc_layout_no_dict
    ; bytearray and memoryview are resizable or borrow their storage, so a
    ; tail would move or not be theirs; they get no dict rather than a
    ; corrupting one.
    extern bytearray_type
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .bc_layout_none
    extern memoryview_type
    lea rcx, [rel memoryview_type]
    cmp rax, rcx
    je .bc_layout_none

    ; A builtin base with a fixed-size header: the dict goes just past it.
    mov rcx, [rax + PyTypeObject.tp_basicsize]
    test rcx, rcx
    jz .bc_layout_done
    mov [r12 + PyTypeObject.tp_dictoffset], rcx
    add rcx, 8
    mov [r12 + PyTypeObject.tp_basicsize], rcx
    jmp .bc_layout_done

.bc_layout_inherit:
    mov [r12 + PyTypeObject.tp_dictoffset], rcx
    mov rcx, [rax + PyTypeObject.tp_basicsize]
    mov [r12 + PyTypeObject.tp_basicsize], rcx
    jmp .bc_layout_done

.bc_layout_none:
    ; The base's own header, and no dict at all: tp_basicsize still has to be
    ; the base's, or the dealloc slot walk reads a negative count.
    mov qword [r12 + PyTypeObject.tp_dictoffset], 0
    mov rcx, [rax + PyTypeObject.tp_basicsize]
    mov [r12 + PyTypeObject.tp_basicsize], rcx
    jmp .bc_layout_done

.bc_layout_no_dict:
    mov qword [r12 + PyTypeObject.tp_dictoffset], TP_DICT_AT_TAIL
    mov rcx, [rax + PyTypeObject.tp_basicsize]
    mov [r12 + PyTypeObject.tp_basicsize], rcx

.bc_layout_done:

    ; Wire instance methods
    lea rax, [rel instance_dealloc]
    mov [r12 + PyTypeObject.tp_dealloc], rax

    lea rax, [rel instance_repr]
    mov [r12 + PyTypeObject.tp_repr], rax

    extern instance_str
    lea rax, [rel instance_str]
    mov [r12 + PyTypeObject.tp_str], rax

    ; tp_call left NULL: calling the type goes through metatype.tp_call (type_call).
    ; Calling instances falls through to __call__ dunder dispatch.

    lea rax, [rel instance_getattr]
    mov [r12 + PyTypeObject.tp_getattr], rax

    lea rax, [rel instance_setattr]
    mov [r12 + PyTypeObject.tp_setattr], rax

    ; tp_flags = HEAPTYPE | HAVE_GC (enables dunder dispatch fallbacks + GC tracking)
    mov qword [r12 + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE | TYPE_FLAG_HAVE_GC

    ; Set tp_traverse and tp_clear for GC cycle detection
    extern instance_traverse
    extern instance_clear
    lea rax, [rel instance_traverse]
    mov [r12 + PyTypeObject.tp_traverse], rax
    lea rax, [rel instance_clear]
    mov [r12 + PyTypeObject.tp_clear], rax

    ; tp_dict = class_dict (ownership transferred from r15, no INCREF needed)
    mov [r12 + PyTypeObject.tp_dict], r15

    ; __new__ is an implicit staticmethod.  Without the wrapper, looking it up
    ; through the class or through super() binds it like an ordinary method
    ; and prepends the instance, so `super().__new__(cls, *args)` arrived one
    ; argument too long -- which is exactly how every metaclass in the stdlib
    ; calls it.
    lea rdi, [rel bc_new_name]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    call dict_get
    mov rbx, rax                ; the current __new__, as a Value
    V_TEST_PTR rbx, rax
    ja .tfp_new_done
    test rbx, rbx
    jz .tfp_new_done
    mov rax, [rbx + PyObject.ob_type]
    extern func_type
    lea rcx, [rel func_type]
    cmp rax, rcx
    jne .tfp_new_done
    sub rsp, 16
    mov [rsp], rbx
    extern staticmethod_type
    extern staticmethod_construct
    lea rdi, [rel staticmethod_type]
    mov rsi, rsp
    mov edx, 1
    call staticmethod_construct
    V_UNPACK rax, rdx
    add rsp, 16
    test rax, rax
    jz .tfp_new_done
    mov rbx, rax
    mov rdi, r15
    mov rsi, [rsp]              ; the "__new__" key
    mov rdx, rbx
    call dict_set
    mov rdi, rbx
    call obj_decref             ; the dict holds it now
.tfp_new_done:
    pop rdi
    call obj_decref             ; the key

    ; A class statement's body sets __module__ itself; three-argument type()
    ; hands over a bare namespace, and without __module__ the repr comes out
    ; unqualified.  Fill it from the running frame's __name__, as CPython does.
    lea rdi, [rel bc_module_name]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .bc_have_module
    extern eval_saved_r12
    mov rcx, [rel eval_saved_r12]
    test rcx, rcx
    jz .bc_have_module
    mov rcx, [rcx + PyFrame.globals]
    test rcx, rcx
    jz .bc_have_module
    lea rdi, [rel bc_dunder_name_name]
    call str_from_cstr_heap
    push rax
    mov rdi, [rel eval_saved_r12]
    mov rdi, [rdi + PyFrame.globals]
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx
    pop rdi
    push rax
    push rdx
    call obj_decref                 ; the "__name__" key
    pop rdx
    pop rax
    test edx, edx
    jz .bc_have_module
    mov rdi, r15
    mov rsi, [rsp]                  ; the "__module__" key
    mov rdx, rax
    call dict_set
.bc_have_module:
    pop rdi
    call obj_decref

    ; INCREF class_name (type object refers to it via tp_name)
    mov rdi, r14
    call obj_incref

    ; === Parse __slots__ from class_dict ===
    ; r12=type, r15=class_dict, [rbp - TFP_BASE]=base_class
    lea rdi, [rel bc_slots_name]
    call str_from_cstr_heap
    push rax                        ; save __slots__ str
    mov rdi, r15                    ; class_dict
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rdi                         ; __slots__ str
    push rdx                        ; save dict_get tag
    push rax                        ; save dict_get value
    call obj_decref                 ; DECREF __slots__ str
    pop rax                         ; value
    pop rdx                         ; tag
    test edx, edx
    jz .bc_no_slots

    ; Must be TAG_PTR and a tuple or list
    cmp edx, TAG_PTR
    jne .bc_no_slots
    extern tuple_type
    extern list_type
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    je .bc_slots_tuple
    lea rdx, [rel list_type]
    cmp rcx, rdx
    jne .bc_no_slots

    ; rax = slots list — get size and item pointers (same layout as tuple for ob_size/ob_item)
.bc_slots_tuple:
    ; rax = slots sequence (tuple or list, both have ob_size at same offset)
    mov rbx, rax                    ; rbx = slots sequence
    mov r13, [rbx + PyTupleObject.ob_size]  ; r13 = nslots (works for both)
    test r13, r13
    jz .bc_no_slots

    ; Determine base_basicsize
    ; Slots are laid out after the whole instance header, which is what
    ; tp_basicsize was just set to -- the base's layout plus the dict word.
    ; Using the *base's* basicsize instead puts the first slot on top of the
    ; dict pointer.
    mov rdi, [r12 + PyTypeObject.tp_basicsize]
.bc_have_basic:
    ; rdi = base_basicsize
    ; Set tp_basicsize = base_basicsize + nslots * 8 (one Value per slot)
    mov rax, r13
    shl rax, 3                      ; nslots * 8
    add rax, rdi                    ; + base_basicsize
    mov [r12 + PyTypeObject.tp_basicsize], rax

    ; Set TYPE_FLAG_HAS_SLOTS
    or qword [r12 + PyTypeObject.tp_flags], TYPE_FLAG_HAS_SLOTS

    ; Create member descriptors for each slot
    ; rbx = slots tuple, r13 = nslots, rdi = base_basicsize
    push rdi                        ; save base_basicsize
    xor edx, edx                    ; i = 0

.bc_slot_loop:
    cmp rdx, r13                    ; i < nslots?
    jge .bc_slots_done

    push rdx                        ; save i

    ; Get slot name: slots_tuple[i]
    mov rax, [rbx + PyTupleObject.ob_item]       ; payloads
    mov rcx, [rax + rdx*8]                        ; name payload
    V_UNPACK rcx, r8
    cmp r8d, TAG_PTR
    jne .bc_slot_skip               ; skip non-string slots

    ; Compute offset = base_basicsize + i * 8
    mov rdi, [rsp + 8]             ; base_basicsize
    mov rax, [rsp]                 ; i
    shl rax, 3
    add rdi, rax                   ; offset

    ; Create descriptor: member_descr_new(offset, name_str)
    mov rsi, rcx                   ; name string
    push rcx                       ; save name for dict_set
    INCREF rsi                     ; descriptor takes ownership
    extern member_descr_new
    call member_descr_new          ; rax = new descriptor

    ; Add to class_dict: dict_set(dict, name, descriptor, TAG_PTR, TAG_PTR)
    mov rdi, r15                   ; class_dict
    pop rsi                        ; name (key)
    mov rdx, rax                   ; descriptor (value)
    push rax                       ; save descriptor for DECREF
    call dict_set

    ; DECREF our ref on descriptor (dict now owns one via INCREF in dict_set)
    pop rdi
    call obj_decref

.bc_slot_skip:
    pop rdx                        ; restore i
    inc rdx
    jmp .bc_slot_loop

.bc_slots_done:
    pop rdi                        ; clean base_basicsize

.bc_no_slots:

    ; Look up "__init__" in class_dict for tp_init
    lea rdi, [rel bc_init_name]
    call str_from_cstr_heap
    push rax                ; save __init__ str obj

    mov rdi, r15            ; class_dict
    mov rsi, rax            ; "__init__" str
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    mov rbx, rax            ; rbx = __init__ func or NULL

    ; DECREF the "__init__" string
    pop rdi
    call obj_decref

    ; Store tp_init (func ptr or 0)
    mov [r12 + PyTypeObject.tp_init], rbx

    ; Set tp_base: use explicit base class, or default to object_type
    mov rax, [rbp - TFP_BASE]
    test rax, rax
    jnz .bc_have_base
    lea rax, [rel object_type]
    mov [rbp - TFP_BASE], rax           ; update saved base for later use
.bc_have_base:
    mov [r12 + PyTypeObject.tp_base], rax
    mov rdi, rax
    call obj_incref

    ; tp_bases and tp_mro.  With these in place a lookup or a subclass test
    ; can see every base, not just the first.
    mov rax, [rbp - TFP_BASES]
    test rax, rax
    jz .bc_no_bases
    cmp qword [rax + PyTupleObject.ob_size], 0
    jne .bc_have_bases_tuple
.bc_no_bases:
    ; No explicit bases: the linearization is (C, object).  An *empty* tuple
    ; means the same thing as none at all and reaches here through
    ; `class C(metaclass=M)` and `type.__new__(M, n, (), d)`, which the NULL
    ; test alone missed -- those classes got an MRO of just [C], so they were
    ; not even instances of object.  It stays invisible until a merge needs
    ; the object at the end: enum's `StrEnum(str, ReprEnum)` linearised to
    ; [StrEnum, str, object, ReprEnum, Enum].
    mov edi, 1
    extern tuple_new
    call tuple_new
    mov rcx, [rax + PyTupleObject.ob_item]
    lea rdx, [rel object_type]
    mov [rcx], rdx
    mov rdi, rdx
    mov [rbp - TFP_BASES], rax
    call obj_incref
    jmp .bc_bases_ready
.bc_have_bases_tuple:
    mov rdi, rax
    call obj_incref
.bc_bases_ready:
    mov rax, [rbp - TFP_BASES]
    mov [r12 + PyTypeObject.tp_bases], rax
    mov rdi, r12
    mov rsi, rax
    extern mro_compute
    call mro_compute
    mov [r12 + PyTypeObject.tp_mro], rax

    ; Inherit the family bits from every base, not only the layout one: a
    ; `class C(Mixin, list)` is still a list subclass.  The container bits
    ; were defined and set on the base types but never inherited at all, so
    ; nothing downstream could tell a list subclass from any other class.
    mov rax, [rbp - TFP_BASES]
    mov rcx, [rax + PyTupleObject.ob_size]
    mov r8, [rax + PyTupleObject.ob_item]
    xor r9, r9
    xor r10, r10
.bc_flag_scan:
    cmp r9, rcx
    jge .bc_flags_done
    mov r11, [r8 + r9*8]
    test r11, r11
    jz .bc_flag_next
    or r10, [r11 + PyTypeObject.tp_flags]
.bc_flag_next:
    inc r9
    jmp .bc_flag_scan
.bc_flags_done:
    and r10, TYPE_FLAG_INT_SUBCLASS | TYPE_FLAG_STR_SUBCLASS | \
             TYPE_FLAG_LIST_SUBCLASS | TYPE_FLAG_TUPLE_SUBCLASS | \
             TYPE_FLAG_DICT_SUBCLASS | TYPE_FLAG_SET_SUBCLASS
    or [r12 + PyTypeObject.tp_flags], r10

    ; A class deriving from `type` is a metatype: its instances are classes,
    ; so it uses type's attribute slots.  Leaving instance_getattr/setattr
    ; wired made `cls.x = 1` inside a metaclass __new__ walk tp_dictoffset on
    ; a PyTypeObject and write through a bogus offset.
    mov rdi, [rbp - TFP_BASE]
    test rdi, rdi
    jz .bc_not_metatype
    lea rsi, [rel type_type]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jz .bc_not_metatype
    extern type_getattr
    extern type_setattr
    lea rax, [rel type_getattr]
    mov [r12 + PyTypeObject.tp_getattr], rax
    lea rax, [rel type_setattr]
    mov [r12 + PyTypeObject.tp_setattr], rax
    mov qword [r12 + PyTypeObject.tp_dictoffset], 0
    mov qword [r12 + PyTypeObject.tp_basicsize], TYPE_OBJECT_SIZE
    ; Calling a metatype builds a class, so it needs type's tp_call, not the
    ; instance-constructing one a heaptype gets by default.
    extern type_call
    lea rax, [rel type_call]
    mov [r12 + PyTypeObject.tp_call], rax
    ; And say so in a bit, so that "is this object a class?" is one test
    ; rather than a comparison against the two metatypes we happen to ship.
    or qword [r12 + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
.bc_not_metatype:

    ; If base is an exception type, inherit exception-compatible methods
    extern type_is_exc_subclass
    mov rdi, [rbp - TFP_BASE]
    call type_is_exc_subclass
    test eax, eax
    jz .bc_check_int_sub

    ; Exception subclass: override instance_* with exc_* methods
    extern exc_dealloc
    extern exc_repr
    extern exc_str
    lea rax, [rel exc_dealloc]
    mov [r12 + PyTypeObject.tp_dealloc], rax
    lea rax, [rel exc_repr]
    mov [r12 + PyTypeObject.tp_repr], rax
    lea rax, [rel exc_str]
    mov [r12 + PyTypeObject.tp_str], rax
    ; Exception getattr/setattr for custom attributes via exc_dict
    extern exc_getattr
    extern exc_setattr
    lea rax, [rel exc_getattr]
    mov [r12 + PyTypeObject.tp_getattr], rax
    lea rax, [rel exc_setattr]
    mov [r12 + PyTypeObject.tp_setattr], rax
    ; Wire exc traverse/clear for exception subclasses
    extern exc_traverse
    extern exc_clear_gc
    lea rax, [rel exc_traverse]
    mov [r12 + PyTypeObject.tp_traverse], rax
    lea rax, [rel exc_clear_gc]
    mov [r12 + PyTypeObject.tp_clear], rax
    jmp .bc_no_set_base

.bc_check_int_sub:
    ; Int subclass: inherit int-compatible repr/str and number methods
    mov rax, [r12 + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jz .bc_check_builtin_sub
    ; tp_repr and tp_str stay instance_repr/instance_str.  They already find
    ; the class's own __repr__ first and fall back to the builtin base's slot
    ; when there is none -- which is how a list subclass prints as a list.
    ; Overwriting them with int's here meant an int subclass that defined
    ; __repr__ never had it called.
    extern int_type
    mov rdi, [rel int_type + PyTypeObject.tp_as_number]
    mov [r12 + PyTypeObject.tp_as_number], rdi
    mov rdi, [rel int_type + PyTypeObject.tp_richcompare]
    mov [r12 + PyTypeObject.tp_richcompare], rdi
    mov rdi, [rel int_type + PyTypeObject.tp_hash]
    mov [r12 + PyTypeObject.tp_hash], rdi
    jmp .bc_no_set_base

.bc_check_builtin_sub:
    ; Inherit the base's constructor (tp_new) where that is the whole story.
    mov rax, [rbp - TFP_BASE]              ; base class
    test rax, rax
    jz .bc_no_set_base

    ; ...but not for bytes, bytearray or memoryview: inheriting tp_new sends
    ; type_call straight to the base constructor and returns, so a subclass
    ; __init__ never ran.  They go through .normal_type_call, which asks the
    ; base to build the instance and then runs __init__ on it.
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .bc_container_sub
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .bc_container_sub
    lea rcx, [rel memoryview_type]
    cmp rax, rcx
    je .bc_container_sub

    ; Not for the container families.  Inheriting tp_new sends type_call
    ; straight to the base constructor, which returns a plain list (or
    ; tuple, dict, set) and never reaches __init__ -- so the subclass name
    ; was lost and its __init__ never ran.  Leaving tp_new NULL routes them
    ; through .normal_type_call, the same path an ordinary class takes.
    mov rcx, [r12 + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_LIST_SUBCLASS | TYPE_FLAG_TUPLE_SUBCLASS | \
              TYPE_FLAG_DICT_SUBCLASS | TYPE_FLAG_SET_SUBCLASS | \
              TYPE_FLAG_STR_SUBCLASS
    jnz .bc_container_sub
    mov rdi, [rax + PyTypeObject.tp_new]
    test rdi, rdi
    jz .bc_no_set_base
    ; Don't inherit object_type_call or type_call
    extern object_type_call
    lea rcx, [rel object_type_call]
    cmp rdi, rcx
    je .bc_no_set_base
    lea rcx, [rel type_call]
    cmp rdi, rcx
    je .bc_no_set_base
    ; Inherit the constructor from the base (for bytearray, etc.)
    mov [r12 + PyTypeObject.tp_new], rdi
    ; Use builtin_sub_dealloc instead of instance_dealloc
    ; (builtin subclasses don't have inst_dict at +16)
    extern builtin_sub_dealloc
    lea rax, [rel builtin_sub_dealloc]
    mov [r12 + PyTypeObject.tp_dealloc], rax

.bc_container_sub:
    ; Inherit the base's protocol slots.  These have no Python-level dunder
    ; that instance_getattr could route to, so a subclass with none of them
    ; is not a container at all: d["k"] = 1 raised, because a heaptype's
    ; tp_as_mapping is NULL.  type_install_slots runs after this and
    ; overrides whichever ones the class defines for itself.
    mov rax, [rbp - TFP_BASE]              ; base class
    mov rcx, [rax + PyTypeObject.tp_as_number]
    mov [r12 + PyTypeObject.tp_as_number], rcx
    mov rcx, [rax + PyTypeObject.tp_as_sequence]
    mov [r12 + PyTypeObject.tp_as_sequence], rcx
    mov rcx, [rax + PyTypeObject.tp_as_mapping]
    mov [r12 + PyTypeObject.tp_as_mapping], rcx
    mov rcx, [rax + PyTypeObject.tp_hash]
    mov [r12 + PyTypeObject.tp_hash], rcx
    mov rcx, [rax + PyTypeObject.tp_richcompare]
    mov [r12 + PyTypeObject.tp_richcompare], rcx
    mov rcx, [rax + PyTypeObject.tp_iter]
    mov [r12 + PyTypeObject.tp_iter], rcx
    mov rcx, [rax + PyTypeObject.tp_iternext]
    mov [r12 + PyTypeObject.tp_iternext], rcx

.bc_no_set_base:

    ; Fill the type's slots from the dunders it defines.  Until now a
    ; heaptype's tp_iter, tp_iternext, tp_hash, tp_call, tp_richcompare and
    ; tp_as_* were all left at zero, and every operation that wanted one had
    ; to grow its own dunder fallback -- or, more often, not.
    extern type_install_slots
    mov rdi, r12
    call type_install_slots

    ; Call parent's __init_subclass__ if present
    mov rax, [rbp - TFP_BASE]          ; base class
    test rax, rax
    jz .bc_no_init_subclass

    ; Look up __init_subclass__ on the base class (walk MRO)
    extern dunder_lookup
    mov rdi, rax               ; base class (as type)
    CSTRING rsi, "__init_subclass__"
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .bc_no_init_subclass

    ; object's own is a classmethod wrapper; unwrap it, since the class it
    ; would bind is already going in as args[0].
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel classmethod_type]
    cmp rcx, rdx
    jne .bc_is_have_callable
    mov rax, [rax + PyClassMethodObject.cm_callable]
    test rax, rax
    jz .bc_no_init_subclass
.bc_is_have_callable:

    ; Call __init_subclass__(new_class, **class_keywords).  The keywords are
    ; the whole point of the hook -- `class C(B, tag="t")` -- and they arrive
    ; through the pending pair, because the class statement knows them and
    ; this function does not.
    SPUSH_PTR r12              ; args[0] = new class
    mov rdi, rax               ; callable
    mov rsi, rsp               ; args
    mov edx, 1                 ; nargs
    mov rcx, [rel class_kwnames_pending]
    mov r8, [rel class_kwvalues_pending]
    call bc_call_kw
    add rsp, 16                ; pop fat args
    test rax, rax
    jz .bc_init_subclass_failed
    mov rdi, rax
    call obj_decref
    jmp .bc_no_init_subclass

.bc_init_subclass_failed:
    ; It ran and raised; the exception is pending and the class is not built.
    DUNDER_RAISED [rbp - TFP_EXC], .tfp_set_name_failed

.bc_no_init_subclass:

    ; Handle __classcell__: look in class_dict for the cell, set its ob_ref to the new type
    lea rdi, [rel bc_classcell_name]
    call str_from_cstr_heap
    push rax                ; save key str
    mov rdi, r15            ; class_dict
    mov rsi, rax
    call dict_get           ; returns cell or NULL
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rdi                 ; key str
    push rdx                ; save dict_get tag
    push rax                ; save cell payload
    call obj_decref         ; DECREF key str
    pop rax                 ; restore cell payload
    pop rdx                 ; restore dict_get tag
    test edx, edx
    jz .bc_no_classcell
    ; cell.ob_ref = new type (r12), with tag
    mov [rax + PyCellObject.ob_ref], r12        ; a type pointer is its own Value
    mov rdi, r12
    call obj_incref         ; cell holds a ref to the type
.bc_no_classcell:

    ; Track the type object in GC
    extern gc_track
    mov rdi, r12
    call gc_track

    ; Now that the class exists, tell every descriptor in it what it is called.
    mov rdi, r12
    mov rsi, r15
    call type_apply_set_name
    test eax, eax
    jz .tfp_set_name_failed

    ; Return the new type object - clear pending flag first
    mov qword [rel build_class_pending], 0
    mov rax, r12

    add rsp, 40                 ; must match the sub in the prologue
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.tfp_set_name_failed:
    ; A __set_name__ raised.  The class is discarded and the exception carried
    ; out as a NULL return, which is what every builtin does.
    mov qword [rel build_class_pending], 0
    mov rdi, r12
    call obj_decref
    xor eax, eax
    add rsp, 40                 ; must match the sub in the prologue
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_from_parts




;; ============================================================================
;; bc_call_kw(callable, Value *pos, uint64_t npos, names, values) -> Value
;;
;; One call with the class keywords attached.  The convention the interpreter
;; already uses is kw_names_pending plus the values sitting after the
;; positional ones, so both __prepare__ and the metaclass are reached exactly
;; the way an ordinary keyword call is.
;; ============================================================================
BCK_MAX   equ 20

BCK_FN    equ 8
BCK_NPOS  equ 16
BCK_NAMES equ 24
BCK_VALS  equ 32
BCK_NKW   equ 40
BCK_ARGS  equ 48 + BCK_MAX * 8
BCK_FRAME equ ((BCK_ARGS + 15) / 16) * 16 + 8    ; + 3 pushes = 16-aligned
DEF_FUNC_LOCAL bc_call_kw, BCK_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - BCK_FN], rdi
    mov rbx, rsi
    mov [rbp - BCK_NPOS], rdx
    mov [rbp - BCK_NAMES], rcx
    mov [rbp - BCK_VALS], r8

    xor eax, eax
    test r8, r8
    jz .have_nkw
    mov rax, [r8 + PyTupleObject.ob_size]
.have_nkw:
    mov [rbp - BCK_NKW], rax
    add rax, rdx
    cmp rax, BCK_MAX
    ja .too_many

    ; the positional arguments, then the keyword values
    xor r12d, r12d                      ; the write index
.copy_pos:
    cmp r12, [rbp - BCK_NPOS]
    jae .copy_kw
    mov rax, [rbx + r12*8]
    lea rcx, [rbp - BCK_ARGS]
    mov [rcx + r12*8], rax
    inc r12
    jmp .copy_pos
.copy_kw:
    cmp qword [rbp - BCK_NKW], 0
    je .no_kw
    mov rsi, [rbp - BCK_VALS]
    mov rsi, [rsi + PyTupleObject.ob_item]
    xor r13d, r13d
.copy_kw_loop:
    cmp r13, [rbp - BCK_NKW]
    jae .kw_copied
    mov rax, [rsi + r13*8]
    lea rcx, [rbp - BCK_ARGS]
    mov [rcx + r12*8], rax
    inc r12
    inc r13
    jmp .copy_kw_loop
.kw_copied:
    mov rax, [rbp - BCK_NAMES]
    mov [rel kw_names_pending], rax
.no_kw:
    mov rdi, [rbp - BCK_FN]
    lea rsi, [rbp - BCK_ARGS]
    mov rdx, r12
    call obj_call_n
    mov qword [rel kw_names_pending], 0
    pop r13
    pop r12
    pop rbx
    leave
    ret
.too_many:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "too many class keyword arguments"
    call raise_exception
END_FUNC bc_call_kw

;; ============================================================================
;; bc_split_kwargs(names, Value *kwvals, PyObject **out) -> rax = 1 ok, 0 error
;;
;; Split a class statement's keywords into the metaclass and the rest:
;;   out[0] = the metaclass= value, borrowed, or 0
;;   out[1] = a tuple of the other names, owned
;;   out[2] = a tuple of their values, owned
;;
;; The two tuples are sized by a first pass, because they must be exactly as
;; long as what goes into them: one sized for the keywords INCLUDING metaclass
;; would end with a NULL slot, and whoever read it next would walk into the
;; hole.  Doing this as a function rather than inline keeps it away from the
;; four callee-saved registers __build_class__ has already spoken for.
;; ============================================================================
BSK_NAMES equ 8
BSK_VALS  equ 16
BSK_OUT   equ 24
BSK_N     equ 32
BSK_KEPT  equ 40
BSK_I     equ 48
BSK_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC_LOCAL bc_split_kwargs, BSK_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - BSK_NAMES], rdi
    mov [rbp - BSK_VALS], rsi
    mov [rbp - BSK_OUT], rdx
    mov qword [rdx], 0
    mov qword [rdx + 8], 0
    mov qword [rdx + 16], 0
    mov rax, [rdi + PyTupleObject.ob_size]
    mov [rbp - BSK_N], rax

    ; --- pass one: how many are not metaclass= ---
    mov qword [rbp - BSK_KEPT], 0
    mov qword [rbp - BSK_I], 0
.count:
    mov rax, [rbp - BSK_I]
    cmp rax, [rbp - BSK_N]
    jae .counted
    mov rdi, [rbp - BSK_NAMES]
    mov rdi, [rdi + PyTupleObject.ob_item]
    mov rdi, [rdi + rax*8]
    add rdi, PyStrObject.data
    CSTRING rsi, "metaclass"
    call ap_strcmp
    test eax, eax
    je .count_next
    inc qword [rbp - BSK_KEPT]
.count_next:
    inc qword [rbp - BSK_I]
    jmp .count
.counted:

    mov rdi, [rbp - BSK_KEPT]
    call tuple_new
    test rax, rax
    jz .fail
    mov r12, rax
    mov rdi, [rbp - BSK_KEPT]
    call tuple_new
    test rax, rax
    jz .fail_names
    mov r13, rax

    ; --- pass two: fill them, and pick out the metaclass ---
    xor ebx, ebx                        ; the write index
    mov qword [rbp - BSK_I], 0
.fill:
    mov rax, [rbp - BSK_I]
    cmp rax, [rbp - BSK_N]
    jae .filled
    mov rdi, [rbp - BSK_NAMES]
    mov rdi, [rdi + PyTupleObject.ob_item]
    mov rdi, [rdi + rax*8]
    add rdi, PyStrObject.data
    CSTRING rsi, "metaclass"
    call ap_strcmp
    mov rcx, [rbp - BSK_I]
    mov rdx, [rbp - BSK_VALS]
    mov rdx, [rdx + rcx*8]              ; the value Value
    test eax, eax
    jne .keep
    mov rcx, [rbp - BSK_OUT]
    mov [rcx], rdx                      ; the metaclass, borrowed
    jmp .fill_next
.keep:
    mov rcx, [rbp - BSK_I]
    mov rax, [rbp - BSK_NAMES]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rax, [rax + rcx*8]              ; the name
    INCREF rax
    mov rcx, [r12 + PyTupleObject.ob_item]
    mov [rcx + rbx*8], rax
    INCREF_V rdx, rax
    mov rcx, [r13 + PyTupleObject.ob_item]
    mov [rcx + rbx*8], rdx
    inc rbx
.fill_next:
    inc qword [rbp - BSK_I]
    jmp .fill
.filled:
    mov rcx, [rbp - BSK_OUT]
    mov [rcx + 8], r12
    mov [rcx + 16], r13
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret

.fail_names:
    mov rdi, r12
    call obj_decref
.fail:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC bc_split_kwargs

;; ============================================================================
;; bc_prepare_namespace(meta, name, bases, fallback) -> rax = mapping, or 0
;;
;; The namespace a class body executes in.  A metaclass that overrides
;; __prepare__ -- enum's EnumType, returning an _EnumDict that records every
;; member as it is stored -- gets that mapping used as the body's locals.
;; Returns 0 to mean "keep the fallback".
;; ============================================================================
BPN_META  equ 8
BPN_NAME  equ 16
BPN_BASES equ 24
BPN_FALL  equ 32
BPN_FN    equ 40
BPN_ARGS  equ 64
BPN_KWN   equ 80
BPN_KWV   equ 88
BPN_EXC   equ 96          ; current_exception before the call
BPN_FRAME equ 104         ; + 1 push = 112
DEF_FUNC_LOCAL bc_prepare_namespace, BPN_FRAME
    push rbx
    mov [rbp - BPN_META], rdi
    mov [rbp - BPN_NAME], rsi
    mov [rbp - BPN_BASES], rdx
    mov [rbp - BPN_FALL], rcx
    mov [rbp - BPN_KWN], r8
    mov [rbp - BPN_KWV], r9

    ; __prepare__ is looked up on the metaclass as an attribute, so a
    ; classmethod arrives already bound.
    lea rdi, [rel bc_prepare_name]
    call str_from_cstr_heap
    test rax, rax
    jz .none
    mov rbx, rax
    mov rdi, [rbp - BPN_META]
    mov rsi, rbx
    call obj_getattr_opt
    mov [rbp - BPN_FN], rax
    mov rdi, rbx
    call obj_decref
    cmp qword [rbp - BPN_FN], 0
    je .none

    mov rax, [rbp - BPN_NAME]
    mov [rbp - BPN_ARGS], rax
    mov rax, [rbp - BPN_BASES]
    test rax, rax
    jnz .have_bases
    xor edi, edi
    call tuple_new
.have_bases:
    mov [rbp - BPN_ARGS + 8], rax
    DUNDER_EXC_SAVE [rbp - BPN_EXC]
    mov rdi, [rbp - BPN_FN]
    lea rsi, [rbp - BPN_ARGS]
    mov edx, 2
    mov rcx, [rbp - BPN_KWN]
    mov r8, [rbp - BPN_KWV]
    call bc_call_kw
    mov rbx, rax
    mov rdi, [rbp - BPN_FN]
    call obj_decref
    test rbx, rbx
    jnz .have_ns
    ; A NULL means either "there is no usable __prepare__" or "it ran and
    ; raised", and they are not the same: treating the second as the first
    ; built the class anyway and left the exception to surface somewhere
    ; unrelated.  -1 says the caller must propagate.
    DUNDER_RAISED [rbp - BPN_EXC], .failed
    jmp .none
.have_ns:

    ; Only a real object can be a namespace; anything else keeps the fallback.
    V_TEST_PTR rbx, rcx
    ja .none
    mov rdi, [rbp - BPN_FALL]
    call obj_decref
    mov rax, rbx
    pop rbx
    leave
    ret
.none:
    xor eax, eax
    pop rbx
    leave
    ret
.failed:
    mov rax, -1
    pop rbx
    leave
    ret
END_FUNC bc_prepare_namespace

;; ============================================================================
;; builtin___build_class__(PyObject **args, int64_t nargs) -> PyObject*
;; __build_class__(body_func, class_name, *bases)
;;
;; 1. body_func = args[0], class_name = args[1]
;; 2. Create a class dict
;; 3. Execute body_func with class_dict as locals
;; 4. Create a new type object with class_dict as tp_dict
;; 5. Return the new type
;; ============================================================================
DEF_FUNC builtin___build_class__
    push rbx
    push r12
    push r13
    push r14
    push r15
BCL_BASES equ 48        ; the bases tuple built from args[2:]
BCL_META  equ 56        ; the metaclass= keyword, or 0
BCL_NPOS  equ 64        ; positional arg count (nargs minus the keywords)
; bc_split_kwargs fills a three-slot scratch: the metaclass, then the class's
; own keyword names and values as two tuples.  Frame offsets count DOWN from
; rbp while the slots count up in address, so the lowest offset is out[2].
BCL_OMETA equ 88
BCL_OKWN  equ 80
BCL_OKWV  equ 72
    sub rsp, 64

    ; Check nargs >= 2
    cmp rsi, 2
    jl .build_class_error

    mov rbx, rdi            ; rbx = args
    mov qword [rbp - BCL_META], 0
    mov [rbp - BCL_NPOS], rsi

    ; `class C(metaclass=M)` passes M as a keyword, and it arrives in the
    ; positional array with its name in kw_names_pending.  Without splitting
    ; them off, M was treated as a *base* -- which is why metaclass= appeared
    ; to be ignored: the metatype never got a chance to run.
    mov qword [rbp - BCL_OKWN], 0
    mov qword [rbp - BCL_OKWV], 0
    mov rax, [rel kw_names_pending]
    test rax, rax
    jz .bc_no_kwargs
    mov rcx, [rax + PyTupleObject.ob_size]
    sub [rbp - BCL_NPOS], rcx

    ; Every keyword but metaclass= belongs to the class, and is handed on to
    ; __prepare__, to the metaclass, and through it to __init_subclass__.
    mov rdi, rax
    mov rsi, [rbp - BCL_NPOS]
    lea rsi, [rbx + rsi*8]              ; where the keyword values start
    lea rdx, [rbp - BCL_OMETA]
    call bc_split_kwargs
    test eax, eax
    jz .build_class_error
    mov rax, [rbp - BCL_OMETA]
    mov [rbp - BCL_META], rax
.bc_no_kwargs:
    ; Consumed: anything we call from here on must not see them again.
    mov qword [rel kw_names_pending], 0
    mov rsi, [rbp - BCL_NPOS]
    ; r12 will be used later for the type object

    ; Collect every base into a tuple.  Only args[2] used to be read, so
    ; `class C(A, B)` silently produced a class that had never heard of B.
    xor eax, eax
    mov [rbp - BCL_BASES], rax
    cmp rsi, 3
    jl .bc_no_base
    push rsi
    lea rdi, [rsi - 2]      ; nbases
    extern tuple_new
    call tuple_new
    pop rsi
    mov [rbp - BCL_BASES], rax
    mov r8, [rax + PyTupleObject.ob_item]
    xor r9, r9
.bc_base_copy:
    lea rcx, [r9 + 2]
    cmp rcx, rsi
    jge .bc_no_base
    mov rdx, [rbx + rcx*8]
    ; A base must be a class.  `class C(1)` used to store and INCREF the
    ; integer, and mro_compute then walked tp_base off it.
    push rsi
    push r8
    push r9
    push rdx
    mov rdi, rdx
    extern type_check_is_class
    call type_check_is_class
    pop rdx
    pop r9
    pop r8
    pop rsi
    test eax, eax
    jz .build_class_base_error
    ; Prevent subclassing bool
    extern bool_type
    lea rcx, [rel bool_type]
    cmp rdx, rcx
    je .build_class_bool_error
    mov [r8 + r9*8], rdx
    push rsi
    push r8
    push r9
    mov rdi, rdx
    call obj_incref
    pop r9
    pop r8
    pop rsi
    inc r9
    jmp .bc_base_copy

.bc_no_base:

    mov r13, [rbx]          ; r13 = body_func (args[0])
    mov r14, [rbx + 8]     ; r14 = class_name (args[1])

    ; A metaclass is inherited: `class D(C)` where type(C) is M gives D the
    ; metatype M as well.  CPython picks the most derived metatype among the
    ; bases; the winner is the one that is a subtype of every other, and
    ; starting from `type` makes an ordinary base contribute nothing.
    cmp qword [rbp - BCL_META], 0
    jne .bc_metaclass_settled
    mov rcx, [rbp - BCL_BASES]
    test rcx, rcx
    jz .bc_metaclass_settled
    lea r8, [rel type_type]                 ; r8 = winner so far
    mov r9, [rcx + PyTupleObject.ob_size]
    mov r10, [rcx + PyTupleObject.ob_item]
    xor r11d, r11d
.bc_meta_scan:
    cmp r11, r9
    jge .bc_meta_scan_done
    mov rdi, [r10 + r11*8]
    V_TEST_PTR rdi, rax
    ja .bc_meta_scan_next
    test rdi, rdi
    jz .bc_meta_scan_next
    mov rdi, [rdi + PyObject.ob_type]       ; the base's metatype
    cmp rdi, r8
    je .bc_meta_scan_next
    push r8
    push r9
    push r10
    push r11
    mov rsi, r8
    call type_is_subtype                    ; is it more derived than the winner?
    pop r11
    pop r10
    pop r9
    pop r8
    test eax, eax
    jz .bc_meta_scan_next
    mov rdi, [r10 + r11*8]
    mov r8, [rdi + PyObject.ob_type]
.bc_meta_scan_next:
    inc r11
    jmp .bc_meta_scan
.bc_meta_scan_done:
    ; The three built-in metatypes go through type_from_parts as before --
    ; they have no __new__ of their own to run.
    lea rax, [rel type_type]
    cmp r8, rax
    je .bc_metaclass_settled
    extern user_type_metatype
    lea rax, [rel user_type_metatype]
    cmp r8, rax
    je .bc_metaclass_settled
    extern exc_metatype
    lea rax, [rel exc_metatype]
    cmp r8, rax
    je .bc_metaclass_settled
    mov [rbp - BCL_META], r8

.bc_metaclass_settled:

    ; The namespace the body executes in.  A metaclass may supply its own
    ; through __prepare__, and that has to happen BEFORE the body runs --
    ; enum's EnumType returns an _EnumDict whose __setitem__ records each
    ; member, so with a plain dict every enum class fails on _member_names.
    ; Which metaclass it is has to be settled first, which is why the scan
    ; above moved ahead of the body.
    call dict_new
    mov r15, rax            ; r15 = class_dict
    cmp qword [rbp - BCL_META], 0
    je .bc_ns_ready
    mov rdi, [rbp - BCL_META]
    mov rsi, r14            ; the class name
    mov rdx, [rbp - BCL_BASES]
    mov rcx, r15            ; the plain dict, freed if __prepare__ supplies one
    mov r8, [rbp - BCL_OKWN]
    mov r9, [rbp - BCL_OKWV]
    call bc_prepare_namespace
    cmp rax, -1
    je .bc_prepare_failed       ; __prepare__ raised; it is already pending
    test rax, rax
    jz .bc_ns_ready
    mov r15, rax
.bc_ns_ready:

    ; Execute body function with class_dict as locals
    ; frame_new(code, globals, builtins, locals)
    mov rdi, [r13 + PyFuncObject.func_code]     ; code from body func
    mov rsi, [r13 + PyFuncObject.func_globals]  ; globals from body func
    mov rdx, [rel builtins_dict_global]         ; builtins dict
    mov rcx, r15                                ; class_dict as locals
    call frame_new
    mov r12, rax            ; r12 = new frame

    ; Store body function in frame for COPY_FREE_VARS (closure support)
    mov [r12 + PyFrame.func_obj], r13

    ; eval_frame(frame)
    mov rdi, r12
    call eval_frame
    V_UNPACK rax, rdx           ; eval_frame returns a Value
    ; A class body that raised returns NULL with current_exception set.  The
    ; same omission the module-body path had: the exception was left pending
    ; and the class built anyway, so `class C: raise X` inside a try/except
    ; produced a class *and* an error reported somewhere else entirely.
    test edx, edx
    jnz .bc_body_ok
    extern current_exception
    cmp qword [rel current_exception], 0
    jne .bc_body_raised
.bc_body_ok:
    ; DECREF return value (should be None — TAG_NONE, not a pointer)
    mov rsi, rdx
    DECREF_VAL rax, rsi

    ; Free the frame
    mov rdi, r12
    call frame_free

    ; With a metaclass, CPython calls meta(name, bases, ns) rather than
    ; building the type itself -- that is what runs M.__new__ and
    ; M.__init__, and what makes type(C) be M.
    cmp qword [rbp - BCL_META], 0
    je .bc_no_metaclass
    mov rdi, [rbp - BCL_META]
    extern type_check_is_class
    push rdi
    call type_check_is_class
    pop rdi
    test eax, eax
    jz .bc_no_metaclass

    ; meta(name, bases, ns, **kwds)
    mov rcx, [rbp - BCL_BASES]
    test rcx, rcx
    jnz .bc_meta_have_bases
    push rdi
    xor edi, edi
    call tuple_new
    mov [rbp - BCL_BASES], rax
    pop rdi
.bc_meta_have_bases:
    sub rsp, 32
    mov [rsp], r14                      ; name
    mov rcx, [rbp - BCL_BASES]
    mov [rsp + 8], rcx                  ; bases
    mov [rsp + 16], r15                 ; namespace
    mov rsi, rsp
    mov edx, 3
    mov rcx, [rbp - BCL_OKWN]
    mov r8, [rbp - BCL_OKWV]
    call bc_call_kw
    V_UNPACK rax, rdx
    add rsp, 32
    push rax
    mov rdi, [rbp - BCL_BASES]
    test rdi, rdi
    jz .bc_meta_bases_done
    call obj_decref
.bc_meta_bases_done:
    ; The metaclass took its own reference to the namespace -- type.__new__
    ; increfs before type_from_parts adopts it -- so ours is not the class's.
    ; Only the no-metaclass path below transfers it; here it was dropped on
    ; the floor, one dict per class built through a metaclass, which is every
    ; enum and every ABC.
    mov rdi, r15
    test rdi, rdi
    jz .bc_meta_done
    xor r15d, r15d
    call obj_decref
.bc_meta_done:
    pop rax
    jmp .bc_have_class

.bc_no_metaclass:
    ; Build the heaptype from (name, bases, namespace); the three-argument
    ; type() reaches the same code.  The class keywords go through the pending
    ; pair: type_from_parts calls __init_subclass__ and has no other way to
    ; know them.
    mov rax, [rbp - BCL_OKWN]
    mov [rel class_kwnames_pending], rax
    mov rax, [rbp - BCL_OKWV]
    mov [rel class_kwvalues_pending], rax
    mov rdi, r14
    mov rsi, [rbp - BCL_BASES]
    mov rdx, r15
    call type_from_parts
    mov qword [rel class_kwnames_pending], 0
    mov qword [rel class_kwvalues_pending], 0
    push rax
    mov rdi, [rbp - BCL_BASES]
    test rdi, rdi
    jz .bc_bases_released
    call obj_decref         ; type_from_parts took its own reference
.bc_bases_released:
    pop rax
    test rax, rax
    jz .bc_have_class       ; NULL, with the exception already pending

.bc_have_class:
    add rsp, 64        ; must match the sub above: the epilogue unwinds
                       ; the locals by hand before popping the registers
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret


.build_class_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__build_class__ requires 2+ arguments"
    call raise_exception

.bc_prepare_failed:
    ; __prepare__ raised.  Release the fallback namespace and the bases and
    ; let its exception keep unwinding, rather than building the class with
    ; an exception already pending.
    mov rdi, r15
    call obj_decref
    mov rdi, [rbp - BCL_BASES]
    test rdi, rdi
    jz .bc_body_raised_go
    call obj_decref
    jmp .bc_body_raised_go

.bc_body_raised:
    ; Release the frame and the namespace, then let the body's exception
    ; keep unwinding in the caller's frame.
    mov rdi, r12
    call frame_free
    mov rdi, r15
    call obj_decref
    mov rdi, [rbp - BCL_BASES]
    test rdi, rdi
    jz .bc_body_raised_go
    call obj_decref
.bc_body_raised_go:
    extern eval_exception_unwind
    jmp eval_exception_unwind

.build_class_base_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "bases must be types"
    call raise_exception

.build_class_bool_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "type 'bool' is not an acceptable base type"
    call raise_exception
END_FUNC builtin___build_class__
section .rodata
bc_prepare_name: db "__prepare__", 0
tsn_name: db "__set_name__", 0
bc_init_name: db "__init__", 0
bc_module_name: db "__module__", 0
bc_dunder_name_name: db "__name__", 0
bc_classcell_name: db "__classcell__", 0
bc_slots_name: db "__slots__", 0
bc_new_name:          db "__new__", 0
section .text
