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
extern traceback_type
extern ap_memcpy
extern oserror_str
extern oserror_new
extern raise_oserror
extern raise_oserror_owned
extern raise_oserror_owned2
extern raise_oserror_build
extern type_number_methods

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
extern none_singleton
extern int_from_i64
extern int_is_integer
extern int_to_i64
extern int_type
extern obj_repr
extern obj_str
extern raise_exception
extern tuple_new
extern tuple_type
extern ap_strcmp
extern kw_names_pending
extern dict_get
extern dict_new
extern dict_set
extern eg_dealloc
extern exc_BaseExceptionGroup_type
extern exc_ExceptionGroup_type

;; ============================================================================
;; exc_new(PyTypeObject *type, PyObject *msg_str, int msg_tag) -> PyExceptionObject*
;; Creates a new exception with given type and message string.
;; msg_str and type are both INCREFed.
;; rdx = msg_tag (TAG_PTR for heap objs, TAG_SMALLINT for ints, 0 for NULL).
;; ============================================================================
EN_EXC equ 8
EN_FRAME equ 24            ; + 3 pushes = 48, 16-aligned
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
    ; ob_refcnt=1, ob_type set by gc_alloc -- but gc_alloc does not count the
    ; type it stamps, and "exception types are immortal" is true of the
    ; sixty-nine builtin ones and false of every class a program writes.  An
    ; uncounted one is freed by the collector out from under live instances,
    ; because a class's only cycle is with its own MRO tuple.
    inc qword [rbx + PyObject.ob_refcnt]
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

;; ============================================================================
;; exc_is_exception(rdi = object) -> eax 0/1
;; True when the object is an instance of BaseException, i.e. when reading
;; PyExceptionObject.exc_context off it is defined.
;; ============================================================================
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

;; ============================================================================
;; exc_set_context(rdi = new exception, rsi = exception being handled)
;; Implements CPython's implicit chaining: the exception raised while another
;; is being handled gets that one as its __context__.  The chain is first
;; scanned for `new` so a re-raise cannot make it point at itself.
;; ============================================================================
ESC_NEW equ 8
ESC_OLD equ 16
ESC_FRAME equ 16            ; + 0 pushes = 16
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

;; ============================================================================
;; raise_key_error(rdi = key Value) -- does not return.
;; CPython reports the missing key itself as the exception's single argument,
;; so that KeyError('k') and str(e) == "'k'" carry which key was absent.
;; ============================================================================
DEF_FUNC raise_key_error
    extern current_exception
    call set_key_error
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    call raise_exception_obj
    ud2
END_FUNC raise_key_error

;; ============================================================================
;; set_key_error(rdi = key Value) -- the same KeyError, RECORDED rather than
;; raised, for a caller that has a C stack it still has to unwind by hand.
;; ============================================================================
global set_key_error
DEF_FUNC set_key_error
    mov rsi, rdi                ; exc_new takes the message as a Value
    lea rdi, [rel exc_KeyError_type]
    xor edx, edx
    call exc_new
    mov rdi, rax
    extern exc_install
    call exc_install
    leave
    ret
END_FUNC set_key_error

;; ============================================================================
;; exc_from_cstr(PyTypeObject *type, const char *msg) -> PyExceptionObject*
;; Creates exception with a C string message (converted to PyStrObject).
;; ============================================================================
DEF_FUNC exc_from_cstr, 8            ; 1 pushes, so rsp is 16-aligned
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

;; ============================================================================
;; exc_dealloc(PyExceptionObject *exc)
;; Free exception and DECREF its fields.
;; ============================================================================
ED_TYPE  equ 8
ED_FRAME equ 24                    ; + 1 push = 32, so rsp is 16-aligned
DEF_FUNC exc_dealloc, ED_FRAME
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

    ; Save ob_type before freeing: gc_dealloc reads it, then frees.  A frame
    ; slot rather than a push, so both calls below stay 16-aligned.
    mov rax, [rbx + PyObject.ob_type]
    mov [rbp - ED_TYPE], rax

    ; Free the object (GC-aware)
    mov rdi, rbx
    call gc_dealloc

    ; Release the class AFTER the instance, the order instance_dealloc uses.
    mov rdi, [rbp - ED_TYPE]
    call obj_decref

    pop rbx
    leave
    ret
END_FUNC exc_dealloc

;; ============================================================================
;; exc_repr(PyExceptionObject *exc) -> PyObject* (string)
;; Returns "TypeName(msg)" or just "TypeName()" if no message.
;; ============================================================================
ER_EXC   equ 8
ER_POS   equ 16
ER_BUF   equ 528         ; 512 bytes, [rbp-528, rbp-16)
ER_FRAME equ 552            ; + 3 pushes = 576, 16-aligned
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

;; ============================================================================
;; exc_is_syntax(PyObject *exc) -> eax = 1 when it is a SyntaxError carrying a
;; location: args == (msg, (filename, lineno, offset, text)).
;; ============================================================================
DEF_FUNC exc_is_syntax, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    lea rsi, [rel exc_SyntaxError_type]
    call exc_isinstance
    test eax, eax
    jz .no
    mov rax, [rbx + PyExceptionObject.exc_args]
    test rax, rax
    jz .no
    cmp qword [rax + PyTupleObject.ob_size], 2
    jne .no
    mov rax, [rax + PyTupleObject.ob_item]
    mov rax, [rax + 8]
    V_TEST_PTR rax, rcx
    ja .no
    test rax, rax
    jz .no
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    jne .no
    ; Four fields, or CPython's six with end_lineno and end_offset.
    cmp qword [rax + PyTupleObject.ob_size], 4
    jl .no
    mov eax, 1
    pop rbx
    leave
    ret
.no:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC exc_is_syntax

;; ============================================================================
;; exc_syntax_str(PyObject *exc) -> PyStrObject*
;;
;; "invalid syntax (f.py, line 1)" -- CPython's SyntaxError_str, which appends
;; the basename of the filename and the line number to the message.  Each half
;; is dropped when its field is not there, so `SyntaxError('m', (None, 1, 1,
;; 't'))` renders as "m (line 1)" and one with neither is the bare message.
;;
;; This was the bare message, which is the form every tool that prints a caught
;; SyntaxError itself shows.  The traceback block carries the location
;; separately, so the difference was invisible in tracebacks and present
;; everywhere else.
;; ============================================================================
SS_MSG   equ 8              ; the str() of args[0], owned
SS_LOC   equ 16             ; the location tuple, borrowed
SS_DIG   equ 56             ; 32 bytes: the line number's digits, backwards
SS_BUF   equ 584            ; 512 bytes of assembly space, at the bottom
SS_FRAME equ 584            ; + 3 pushes = 608
DEF_FUNC exc_syntax_str, SS_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi

    mov rax, [rdi + PyExceptionObject.exc_args]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rdi, [rax]
    call obj_str
    V_UNPACK rax, rdx
    test rax, rax
    jz .ss_out
    mov [rbp - SS_MSG], rax

    ; Only a located SyntaxError gets a suffix.
    mov rdi, rbx
    call exc_is_syntax
    test eax, eax
    jz .ss_bare
    mov rax, [rbx + PyExceptionObject.exc_args]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rax, [rax + 8]
    mov [rbp - SS_LOC], rax

    ; The message first, truncated to leave room for the suffix.
    mov r13, [rbp - SS_MSG]
    mov rdx, [r13 + PyStrObject.ob_size]
    cmp rdx, 400
    jle .ss_msg_len
    mov edx, 400
.ss_msg_len:
    lea rdi, [rbp - SS_BUF]
    lea rsi, [r13 + PyStrObject.data]
    mov r12, rdi
    add r12, rdx                        ; where the suffix will start
    call ap_memcpy

    ; " (basename", if there is a filename.  CPython prints the basename only.
    mov rax, [rbp - SS_LOC]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rax, [rax]
    V_TEST_PTR rax, rcx
    ja .ss_no_file
    test rax, rax
    jz .ss_no_file
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .ss_no_file

    mov word [r12], ' ('                ; ' ' then '(' -- NASM is little-endian
    add r12, 2
    lea rsi, [rax + PyStrObject.data]
    mov rcx, [rax + PyStrObject.ob_size]
    add rcx, rsi                        ; one past the end
    mov r8, rsi
.ss_base:
    cmp r8, rcx
    jae .ss_base_done
    cmp byte [r8], '/'
    jne .ss_base_next
    lea rsi, [r8 + 1]
.ss_base_next:
    inc r8
    jmp .ss_base
.ss_base_done:
    sub rcx, rsi                        ; what is left of it is the basename
    cmp rcx, 100
    jle .ss_base_len
    mov ecx, 100
.ss_base_len:
    mov rdx, rcx
    mov rdi, r12
    add r12, rdx
    call ap_memcpy
    mov r13d, 1                         ; the parenthesis is open
    jmp .ss_line

.ss_no_file:
    xor r13d, r13d
.ss_line:
    ; ", line N", or " (line N" when there was no filename to open with.
    mov rax, [rbp - SS_LOC]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rax, [rax + 8]
    ; The line may be an immediate or a heap int: the exception's args are
    ; whatever the caller put there, and past +-2^50 -- or under INT_STRESS,
    ; past 8 -- an ordinary line number is boxed.
    V_IS_INT rax, rcx
    jae .ss_line_imm
    V_TEST_PTR rax, rcx
    ja .ss_close
    test rax, rax
    jz .ss_close
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel int_type]
    cmp rcx, rdx
    jne .ss_close
    mov rdi, rax
    mov edx, TAG_PTR
    call int_to_i64
    jmp .ss_line_have
.ss_line_imm:
    V_TO_I64 rax
.ss_line_have:
    test r13d, r13d
    jnz .ss_line_sep
    mov word [r12], ' ('
    add r12, 2
    mov r13d, 1
    jmp .ss_line_word
.ss_line_sep:
    mov word [r12], ', '                ; ',' then ' '
    add r12, 2
.ss_line_word:
    mov dword [r12], 'line'
    mov byte [r12 + 4], ' '
    add r12, 5

    ; The digits, least significant first into a scratch and then reversed.
    xor r8d, r8d
.ss_digit:
    xor edx, edx
    mov ecx, 10
    div rcx                             ; rax = rax/10, rdx = the digit
    add dl, '0'
    mov [rbp - SS_DIG + r8], dl
    inc r8
    test rax, rax
    jnz .ss_digit
.ss_emit:
    dec r8
    mov dl, [rbp - SS_DIG + r8]
    mov [r12], dl
    inc r12
    test r8, r8
    jnz .ss_emit

.ss_close:
    test r13d, r13d
    jz .ss_finish
    mov byte [r12], ')'
    inc r12
.ss_finish:
    lea rdi, [rbp - SS_BUF]
    mov rsi, r12
    sub rsi, rdi                        ; the length written
    call str_new_heap
    test rax, rax
    jz .ss_bare
    mov rdi, [rbp - SS_MSG]
    mov [rbp - SS_MSG], rax
    call obj_decref
.ss_bare:
    mov rax, [rbp - SS_MSG]
.ss_out:
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC exc_syntax_str


;; ============================================================================
;; exc_str(PyExceptionObject *exc) -> PyObject* (string)
;; Returns the message string, or type name if no message.
;; ============================================================================
ES_EXC   equ 8
ES_FRAME equ 24            ; + 1 push = 32, 16-aligned
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
    ; An OSError renders as "[Errno N] strerror: 'file' -> 'file2'".  The test
    ; is exc_isinstance, not the exact-pointer compare the KeyError arm below
    ; uses, or every subclass -- which is what os actually raises -- would miss
    ; it.  Fixing this fixes the uncaught-OSError traceback line too, since
    ; traceback.asm renders tp_name + ": " + obj_str(exc).
    mov rdi, rbx
    lea rsi, [rel exc_OSError_type]
    call exc_isinstance
    test eax, eax
    jz .es_check_syntax
    mov rdi, rbx
    call oserror_str
    cmp rax, -1
    je .es_raised
    test rax, rax
    jz .es_check_syntax
    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.es_raised:
    xor eax, eax                    ; a NULL Value, with the exception pending
    xor edx, edx
    pop rbx
    leave
    ret

.es_check_syntax:
    mov rax, [rbx + PyExceptionObject.exc_args]
    mov rcx, [rax + PyTupleObject.ob_size]

    ; A syntax error's args are (msg, (filename, lineno, offset, text)), and
    ; str() renders the pair the way CPython does rather than showing the
    ; tuple: "msg (filename, line N)".
    cmp rcx, 2
    jne .es_not_syntax
    mov rdi, rbx
    call exc_is_syntax
    test eax, eax
    jz .es_not_syntax
    mov rdi, rbx
    call exc_syntax_str
    test rax, rax
    jz .es_not_syntax
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.es_not_syntax:
    ; exc_is_syntax is a call, so the args pointer and the count it left in rax
    ; and rcx are gone; both have to come back before the ordinary paths use
    ; them.
    mov rax, [rbx + PyExceptionObject.exc_args]
    mov rcx, [rax + PyTupleObject.ob_size]

    ; A Unicode{Decode,Encode}Error carries five arguments -- encoding,
    ; object, start, end, reason -- and CPython renders them into a sentence.
    ; Falling through to .es_tuple printed the tuple instead, so str() of one
    ; raised from lib/_codecs.py was "('ascii', b'abc', 1, 2, 'ordinal not in
    ; range(128)')".  The asm sites that raise these build the sentence
    ; themselves for exactly that reason; now they need not.
    cmp rcx, 5
    jne .es_not_unicode
    mov rdi, rbx
    call unicode_error_str
    test rax, rax
    jz .es_not_unicode
    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.es_not_unicode:
    mov rax, [rbx + PyExceptionObject.exc_args]
    mov rcx, [rax + PyTupleObject.ob_size]

    cmp rcx, 1
    jne .es_tuple

    ; KeyError is the one that shows its single argument's repr, so that a
    ; missing key prints with its quotes -- and so does a SUBCLASS of it.
    ; CPython gives KeyError its own tp_str and subclasses inherit it; this
    ; was an exact-pointer compare, so `class K(KeyError)` lost the quotes.
    push rax
    push rax                        ; exc_args, and a pad for the alignment
    mov rdi, [rbx + PyExceptionObject.ob_type]
    lea rsi, [rel exc_KeyError_type]
    extern type_is_subtype
    call type_is_subtype
    pop rcx
    pop rcx                         ; exc_args back
    test eax, eax
    mov rax, rcx
    jnz .es_one_repr

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
    ; A SyntaxError renders its msg, and with no args at all the msg is None:
    ; CPython's str(SyntaxError()) is "None", not the empty string every other
    ; argument-less exception gives.
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    call exc_isinstance
    test eax, eax
    jz .es_empty_str
    CSTRING rdi, "None"
    call str_from_cstr
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.es_empty_str:
    CSTRING rdi, ""
    call str_from_cstr
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC exc_str

;; ============================================================================
;; unicode_error_str(rdi = the exception) -> rax = PyStrObject*, or 0
;;
;; CPython's wording for a UnicodeDecodeError or UnicodeEncodeError, out of the
;; five arguments the exception carries:
;;
;;   'ascii' codec can't decode byte 0xc3 in position 1: ordinal not in range(128)
;;   'ascii' codec can't encode character '\u1234' in position 1: <reason>
;;
;; and the plural forms when the span is wider than one.  Answers 0 for
;; anything that is not one of the two types, or whose arguments are not the
;; shapes below, so exc_str falls back to the tuple repr.
;; ============================================================================
UES_EXC   equ 8
UES_ARGS  equ 16
UES_START equ 24
UES_END   equ 32
UES_BUF   equ 288           ; the sentence, built in place
UES_FRAME equ 296            ; + 1 push = 304, 16-aligned

extern rbt_append_cstr
extern msg_append_i64
extern msg_append_hex2
extern msg_append_escaped_cp
extern str_cp_at
DEF_FUNC unicode_error_str, UES_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - UES_EXC], rdi

    ; Which of the two, and therefore which verb?
    mov rdi, rbx
    lea rsi, [rel exc_UnicodeDecodeError_type]
    call exc_isinstance
    test eax, eax
    jnz .ues_decode
    mov rdi, rbx
    lea rsi, [rel exc_UnicodeEncodeError_type]
    call exc_isinstance
    test eax, eax
    jz .ues_no
    xor r9d, r9d                ; encode
    jmp .ues_have_kind
.ues_decode:
    mov r9d, 1                  ; decode
.ues_have_kind:
    mov [rbp - UES_START], r9   ; borrow the slot until the args are read

    mov rax, [rbx + PyExceptionObject.exc_args]
    mov rax, [rax + PyTupleObject.ob_item]
    mov [rbp - UES_ARGS], rax

    ; args[0] must be a str, args[2] and args[3] ints, args[4] a str.
    mov rdi, [rax]
    V_TEST_PTR rdi, rcx
    ja .ues_no
    mov rcx, [rdi + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .ues_no
    mov rdi, [rax + 32]
    V_TEST_PTR rdi, rcx
    ja .ues_no
    mov rcx, [rdi + PyObject.ob_type]
    cmp rcx, rdx
    jne .ues_no

    mov r9, [rbp - UES_START]   ; the kind, before the slot is reused
    mov rdi, [rax + 16]
    V_IS_INT rdi, rcx
    jb .ues_no
    V_TO_I64 rdi
    mov [rbp - UES_START], rdi
    mov rax, [rbp - UES_ARGS]
    mov rdi, [rax + 24]
    V_IS_INT rdi, rcx
    jb .ues_no
    V_TO_I64 rdi
    mov [rbp - UES_END], rdi

    ; "'<encoding>' codec can't "
    lea rdi, [rbp - UES_BUF]
    lea rsi, [rel ues_quote]
    call rbt_append_cstr
    mov rdi, rax
    mov rcx, [rbp - UES_ARGS]
    mov rsi, [rcx]
    lea rsi, [rsi + PyStrObject.data]
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel ues_codec]
    call rbt_append_cstr
    mov rdi, rax
    test r9d, r9d
    jz .ues_verb_encode
    lea rsi, [rel ues_decode_w]
    jmp .ues_verb_done
.ues_verb_encode:
    lea rsi, [rel ues_encode_w]
.ues_verb_done:
    push r9
    call rbt_append_cstr
    pop r9

    ; One position, or a span?
    mov rcx, [rbp - UES_START]
    inc rcx
    cmp rcx, [rbp - UES_END]
    jne .ues_span

    ; "byte 0xNN " or "character 'X' "
    mov rdi, rax
    test r9d, r9d
    jz .ues_one_char
    lea rsi, [rel ues_byte]
    call rbt_append_cstr
    mov rdi, rax
    mov rcx, [rbp - UES_ARGS]
    mov rsi, [rcx + 8]          ; the bytes
    V_TEST_PTR rsi, rcx
    ja .ues_no
    mov rcx, [rbp - UES_START]
    cmp rcx, [rsi + PyBytesObject.ob_size]
    jae .ues_no
    movzx esi, byte [rsi + PyBytesObject.data + rcx]
    call msg_append_hex2
    jmp .ues_position
.ues_one_char:
    lea rsi, [rel ues_char]
    call rbt_append_cstr
    mov rdi, rax
    mov rcx, [rbp - UES_ARGS]
    mov rsi, [rcx + 8]          ; the str
    mov rdx, [rbp - UES_START]
    call msg_append_escaped_cp
    jmp .ues_position

.ues_span:
    mov rdi, rax
    test r9d, r9d
    jz .ues_span_chars
    lea rsi, [rel ues_bytes_pl]
    jmp .ues_span_emit
.ues_span_chars:
    lea rsi, [rel ues_chars_pl]
.ues_span_emit:
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel ues_in_pos]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - UES_START]
    call msg_append_i64
    mov rdi, rax
    lea rsi, [rel ues_dash]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - UES_END]
    dec rsi
    call msg_append_i64
    jmp .ues_reason

.ues_position:
    mov rdi, rax
    lea rsi, [rel ues_in_pos]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - UES_START]
    call msg_append_i64

.ues_reason:
    mov rdi, rax
    lea rsi, [rel ues_colon]
    call rbt_append_cstr
    mov rdi, rax
    mov rcx, [rbp - UES_ARGS]
    mov rsi, [rcx + 32]
    lea rsi, [rsi + PyStrObject.data]
    call rbt_append_cstr

    lea rdi, [rbp - UES_BUF]
    call str_from_cstr
    pop rbx
    leave
    ret

.ues_no:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC unicode_error_str

;; ============================================================================
;; exc_getattr(PyExceptionObject *exc, PyStrObject *name) -> PyObject* or NULL
;; Handle attribute access on exception objects: args, __context__, __cause__, etc.
;; ============================================================================
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

    ; UnicodeEncodeError's and UnicodeDecodeError's five, which are their
    ; whole point: an error handler is given the exception and reads the span
    ; it has to replace out of it.  They are not fields either -- args is
    ; (encoding, object, start, end, reason), which is the shape both the
    ; message builder and lib/_codecs already assume -- so reading them back
    ; out keeps one source of truth and needs no five-argument constructor.
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "encoding"
    xor r14d, r14d
    call ap_strcmp
    test eax, eax
    jz .uni_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "object"
    mov r14d, 1
    call ap_strcmp
    test eax, eax
    jz .uni_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "start"
    mov r14d, 2
    call ap_strcmp
    test eax, eax
    jz .uni_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "end"
    mov r14d, 3
    call ap_strcmp
    test eax, eax
    jz .uni_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "reason"
    mov r14d, 4
    call ap_strcmp
    test eax, eax
    jz .uni_attr
    jmp .not_unicode_attr

.uni_attr:
    mov rdi, rbx

    lea rsi, [rel exc_UnicodeError_type]
    call exc_isinstance
    test eax, eax
    jz .not_unicode_attr
    ; An explicit assignment wins, as it does for SyntaxError's.
    mov rdi, [rbx + PyExceptionObject.exc_dict]
    test rdi, rdi
    jz .uni_from_args
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .found_in_dict
.uni_from_args:
    mov rax, [rbx + PyExceptionObject.exc_args]
    test rax, rax
    jz .return_none
    movsxd rcx, r14d
    cmp [rax + PyTupleObject.ob_size], rcx
    jle .return_none
    mov rdx, [rax + PyTupleObject.ob_item]
    mov rax, [rdx + rcx*8]
    INCREF_V rax, rdx
    V_UNPACK rax, rdx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.not_unicode_attr:

    ; SyntaxError's seven attributes.  They are not fields: they live in the
    ; args tuple CPython puts them in -- args = (msg, (filename, lineno,
    ; offset, text, end_lineno, end_offset)) -- so reading them back out of it
    ; keeps one source of truth, and an exception built by hand with the same
    ; args answers the same way.  r14d carries which one: 0 is the message and
    ; 1..6 index the location tuple.
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "msg"
    xor r14d, r14d
    call ap_strcmp
    test eax, eax
    jz .syn_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "filename"
    mov r14d, 1
    call ap_strcmp
    test eax, eax
    jz .syn_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "lineno"
    mov r14d, 2
    call ap_strcmp
    test eax, eax
    jz .syn_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "offset"
    mov r14d, 3
    call ap_strcmp
    test eax, eax
    jz .syn_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "text"
    mov r14d, 4
    call ap_strcmp
    test eax, eax
    jz .syn_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "end_lineno"
    mov r14d, 5
    call ap_strcmp
    test eax, eax
    jz .syn_attr
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "end_offset"
    mov r14d, 6
    call ap_strcmp
    test eax, eax
    jz .syn_attr
    jmp .not_syntax_attr

.syn_attr:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    call exc_isinstance
    test eax, eax
    jz .not_syntax_attr
    ; An explicit `e.lineno = 3` wins, the way .get_code lets one win.
    mov rdi, [rbx + PyExceptionObject.exc_dict]
    test rdi, rdi
    jz .syn_from_args
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .found_in_dict
.syn_from_args:
    mov rax, [rbx + PyExceptionObject.exc_args]
    test rax, rax
    jz .return_none
    test r14d, r14d
    jnz .syn_loc
    cmp qword [rax + PyTupleObject.ob_size], 1
    jl .return_none
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rax, [rcx]
    jmp .syn_have
.syn_loc:
    ; Anything short of a real tuple in args[1] means the attribute is absent,
    ; which is None -- `SyntaxError('boom')` has a msg and no location.
    cmp qword [rax + PyTupleObject.ob_size], 2
    jl .return_none
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rax, [rcx + 8]
    V_TEST_PTR rax, rcx
    ja .return_none
    test rax, rax
    jz .return_none
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    jne .return_none
    movsxd rcx, r14d
    cmp [rax + PyTupleObject.ob_size], rcx
    jl .return_none
    mov rdx, [rax + PyTupleObject.ob_item]
    mov rax, [rdx + rcx*8 - 8]
.syn_have:
    INCREF_V rax, rdx
    V_UNPACK rax, rdx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
.not_syntax_attr:

    ; The instance dict comes first: a class attribute used to shadow the
    ; instance attribute of the same name, so `e.x = 2` then `e.x` read the
    ; class default.
    mov rdi, [rbx + PyExceptionObject.exc_dict]
    test rdi, rdi
    jz .eg_type_start
    mov rsi, r12
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .found_in_dict
.eg_type_start:

    ; Then the type's MRO (for user-defined subclass attrs).  Only the exact
    ; type's dict used to be consulted, so a method defined on an exception's
    ; *base* was invisible.
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

;; ============================================================================
;; exc_setattr(PyExceptionObject *exc, PyStrObject *name, PyObject *value, int value_tag)
;; Store a custom attribute on an exception object using exc_dict.
;; rdi = exc, rsi = name, rdx = value, ecx = value_tag
;; ============================================================================
ESA_VAL   equ 8
ESA_TAG   equ 16
ESA_ISTB  equ 24            ; the field takes a traceback, not an exception
ESA_FRAME equ 32            ; + 2 pushes = 48
DEF_FUNC exc_setattr, ESA_FRAME
    push rbx
    push r12
    mov rbx, rdi            ; exc
    mov r12, rsi            ; the name
    mov [rbp - ESA_VAL], rdx
    mov [rbp - ESA_TAG], rcx

    ; Four names are fields of the object, not entries in its dict, and
    ; exc_getattr reads them from the fields.  Writing them to the dict left
    ; the assignment invisible: `e.__cause__ = other` read back as None, and
    ; the report the traceback printer produced had no cause chain in it.
    ; `raise x from y` goes through the fields directly, which is why only the
    ; hand-written form was affected.
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__cause__"
    call ap_strcmp
    test eax, eax
    jz .esa_cause
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__context__"
    call ap_strcmp
    test eax, eax
    jz .esa_context
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__traceback__"
    call ap_strcmp
    test eax, eax
    jz .esa_tb
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "__suppress_context__"
    call ap_strcmp
    test eax, eax
    jz .esa_suppress
    ; ap_strcmp clobbers the argument registers, so the value and its tag come
    ; back from the frame.
    mov rsi, r12
    mov rdx, [rbp - ESA_VAL]
    mov rcx, [rbp - ESA_TAG]

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

    pop r12
    pop rbx
    leave
    ret

    ; Each field holds a strong reference, and None means "none of it": the
    ; report walks a NULL field, not a None one.
.esa_cause:
    mov esi, PyExceptionObject.exc_cause
    ; `raise x from y` also sets __suppress_context__, and so does assigning
    ; to __cause__ -- CPython's cause setter does both.
    mov qword [rbx + PyExceptionObject.exc_suppress], 1
    jmp .esa_field
.esa_context:
    mov esi, PyExceptionObject.exc_context
    jmp .esa_field
.esa_tb:
    mov esi, PyExceptionObject.exc_tb
    mov qword [rbp - ESA_ISTB], 1
    jmp .esa_field_typed
.esa_field:
    mov qword [rbp - ESA_ISTB], 0
.esa_field_typed:
    mov rdx, [rbp - ESA_VAL]
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    jne .esa_field_check
    xor edx, edx            ; None clears it
    jmp .esa_field_store
.esa_field_check:
    ; All three take None or one particular kind of object, and nothing else.
    ; Only None was being tested for, so the INCREF below dereferenced an
    ; int -- `e.__cause__ = 5` was a segfault where CPython raises TypeError.
    mov rdi, rdx
    V_TEST_PTR rdi, rax
    ja .esa_field_bad
    test rdi, rdi
    jz .esa_field_bad
    mov rax, [rdi + PyObject.ob_type]
    cmp qword [rbp - ESA_ISTB], 0
    jne .esa_field_tb_check
    push rsi
    push rdx
    mov rdi, rax
    lea rsi, [rel exc_BaseException_type]
    extern type_is_subtype
    call type_is_subtype
    pop rdx
    pop rsi
    test eax, eax
    jz .esa_field_bad
    jmp .esa_field_store
.esa_field_tb_check:
    lea rcx, [rel traceback_type]
    cmp rax, rcx
    jne .esa_field_bad
.esa_field_store:
    mov r12, rdx
    test r12, r12
    jz .esa_field_swap
    mov rdi, r12
    push rsi
    call obj_incref
    pop rsi
.esa_field_swap:
    mov rdi, [rbx + rsi]
    mov [rbx + rsi], r12
    test rdi, rdi
    jz .esa_field_done
    call obj_decref
.esa_field_done:
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret

.esa_field_bad:
    cmp qword [rbp - ESA_ISTB], 0
    jne .esa_field_bad_tb
    cmp esi, PyExceptionObject.exc_cause
    jne .esa_field_bad_context
    RAISE exc_TypeError_type, \
        "exception cause must be None or derive from BaseException"
.esa_field_bad_context:
    RAISE exc_TypeError_type, \
        "exception context must be None or derive from BaseException"
.esa_field_bad_tb:
    RAISE exc_TypeError_type, "__traceback__ must be a traceback or None"

.esa_suppress:
    ; CPython's setter takes a bool and nothing else, down to the wording.
    mov rdi, [rbp - ESA_VAL]
    lea rcx, [rel bool_true]
    cmp rdi, rcx
    je .esa_suppress_true
    lea rcx, [rel bool_false]
    cmp rdi, rcx
    jne .esa_suppress_bad
    xor eax, eax
    jmp .esa_suppress_store
.esa_suppress_true:
    mov eax, 1
.esa_suppress_store:
    mov [rbx + PyExceptionObject.exc_suppress], rax
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret
.esa_suppress_bad:
    RAISE exc_TypeError_type, "attribute value type must be bool"
END_FUNC exc_setattr

;; ============================================================================
;; exc_isinstance(PyExceptionObject *exc, PyTypeObject *type) -> int (0/1)
;; Check if exception is an instance of type, walking tp_base chain.
;; If type is a tuple, checks each element.
;; ============================================================================
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
    RAISE exc_TypeError_type, "catching classes that do not inherit from BaseException is not allowed"
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

;; ============================================================================
;; type_is_exc_subclass(PyTypeObject *type) -> int (0/1)
;; Walk tp_base chain checking for a type with tp_dealloc == exc_dealloc.
;; Detects user-defined exception classes (e.g., class MyError(Exception): pass)
;; ============================================================================
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


; exc_type_call(PyTypeObject *type, PyObject **args, int64_t nargs) -> rax = Value
; tp_call for exception metatype. Creates an exception instance.
; rdi = exception type (the class being called, e.g. ValueError)
; rsi = args array
; rdx = nargs
ETC_EXC   equ 8
ETC_ARGS  equ 16
ETC_NARGS equ 24
ETC_KWFAM equ 32            ; 0 none, 1 AttributeError, 2 ImportError
ETC_KW1   equ 40            ; the 'name' keyword's value, or 0
ETC_KW2   equ 48            ; 'obj' for AttributeError, 'path' for ImportError
ETC_FRAME equ 48            ; + 2 pushes = 64, 16-byte aligned
;; ============================================================================
;; exc_method_init(args, nargs) -> None -- BaseException.__init__
;;
;; `class E(Exception)` whose __init__ calls super().__init__(msg) has to end
;; up with args == (msg,), which is what str(e) and repr(e) are built from.
;; BaseException had no __init__ of its own, so the super() call walked past
;; every exception type in the MRO and reached object.__init__, which takes
;; its arguments and ignores them: re.error's message came out as the whole
;; three-item constructor tuple, and so did every other exception that
;; composes its own message.
;; ============================================================================
EMI_SELF  equ 8
EMI_TUP   equ 16
EMI_FRAME equ 40            ; + 1 push = 48, 16-aligned

DEF_FUNC exc_method_init, EMI_FRAME
    push rbx
    test rsi, rsi
    jz .emi_none
    mov rax, [rdi]
    V_TEST_PTR rax, rcx
    ja .emi_none
    test rax, rax
    jz .emi_none
    mov [rbp - EMI_SELF], rax

    ; args = the tuple of everything after self
    lea rbx, [rdi + 8]
    dec rsi
    mov [rbp - EMI_TUP], rsi
    mov rdi, rsi
    call tuple_new
    test rax, rax
    jz .emi_none
    mov rcx, rax
    mov r8, [rax + PyTupleObject.ob_item]
    xor edx, edx
.emi_copy:
    cmp rdx, [rbp - EMI_TUP]
    jge .emi_copied
    mov r9, [rbx + rdx*8]
    INCREF_V r9, r10
    mov [r8 + rdx*8], r9
    inc rdx
    jmp .emi_copy
.emi_copied:
    mov rdi, [rbp - EMI_SELF]
    mov rsi, [rdi + PyExceptionObject.exc_args]
    mov [rdi + PyExceptionObject.exc_args], rcx
    test rsi, rsi
    jz .emi_none
    mov rdi, rsi
    call obj_decref

.emi_none:
    LOAD_NONE rax
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC exc_method_init

;; ============================================================================
;; exc_method_with_traceback(rdi = args, rsi = nargs) -> rax = Value, self
;;
;; `exc.with_traceback(tb)` sets __traceback__ and hands the exception back, so
;; that `raise e.with_traceback(tb)` is one expression.  unittest's
;; assertRaises does `self.exception = exc_value.with_traceback(None)` in its
;; __exit__, so without this every assertRaises raised AttributeError from
;; inside a __exit__ that was already handling an exception.
;;
;; args[0] is the exception: builtin_func_call has already checked the receiver
;; against the owner type_stamp_methods stamped on this dict.
;; ============================================================================
EWT_SELF  equ 8
EWT_FRAME equ 16            ; + 0 pushes = 16, 16-aligned
DEF_FUNC exc_method_with_traceback, EWT_FRAME
    cmp rsi, 2
    jne .ewt_arity
    mov rax, [rdi]
    mov [rbp - EWT_SELF], rax
    mov rdx, [rdi + 8]              ; the traceback, as a Value

    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .ewt_clear
    ; Only a traceback or None.  An immediate is neither, and reading ob_type
    ; off one is a dereference of the number.
    V_TEST_PTR rdx, rcx
    ja .ewt_bad
    mov rcx, [rdx + PyObject.ob_type]
    lea r8, [rel traceback_type]
    cmp rcx, r8
    jne .ewt_bad
    INCREF rdx
    jmp .ewt_store
.ewt_clear:
    xor edx, edx                    ; None clears it, as the setter does
.ewt_store:
    mov rax, [rbp - EWT_SELF]
    mov rcx, [rax + PyExceptionObject.exc_tb]
    mov [rax + PyExceptionObject.exc_tb], rdx
    test rcx, rcx
    jz .ewt_ret
    push rax
    push rax                        ; pad: the call below needs an even list
    mov rdi, rcx
    call obj_decref
    pop rax
    pop rax
.ewt_ret:
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx                 ; builtins return one Value
    ret
.ewt_bad:
    RAISE exc_TypeError_type, "__traceback__ must be a traceback or None"
.ewt_arity:
    dec rsi                         ; the count CPython reports excludes self
    CSTRING rdi, "BaseException.with_traceback() takes exactly one argument ("
    CSTRING rdx, " given)"
    extern raise_type_error_counted
    jmp raise_type_error_counted
END_FUNC exc_method_with_traceback

;; ============================================================================
;; exc_method_new(args, nargs) -> a bare exception of args[0]'s type
;;
;; BaseException.__new__.  CPython has one and this tree did not, so
;; `ValueError.__new__(ValueError)` resolved up the MRO to object.__new__ --
;; which is a different function with a different rule.  It happened to work
;; while object.__new__ accepted anything; once it started refusing types
;; whose allocation it does not own, every `cls.__new__(cls)` on an exception
;; became a TypeError.  copyreg and unittest both write that idiom.
;;
;; The remaining arguments become `.args`, as CPython's does -- `__init__` is
;; what interprets them, and `__new__` only has to record them.
;; ============================================================================
EMN_TYPE  equ 8
EMN_EXC   equ 16
EMN_ARGS  equ 24
EMN_NARGS equ 32
EMN_TUP   equ 40
EMN_I     equ 48
EMN_FRAME equ 72            ; + 1 push = 80, 16-aligned
DEF_FUNC exc_method_new, EMN_FRAME
    push rbx
    test rsi, rsi
    jz .emn_no_type
    mov [rbp - EMN_NARGS], rsi
    mov [rbp - EMN_ARGS], rdi
    mov rbx, [rdi]              ; args[0] = the class
    ; ...if it IS a class.  Nothing checked, so `ValueError("x").__new__(V)`
    ; -- which reaches here with the INSTANCE in args[0], because the wrapper
    ; below binds -- built an exception whose exc_type was an instance and
    ; aborted with "double free or corruption" when it was freed.
    V_TEST_PTR rbx, rax
    ja .emn_not_a_type
    mov rax, [rbx + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .emn_not_a_type
    mov rdi, rbx
    lea rsi, [rel exc_BaseException_type]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jz .emn_not_an_exc
    mov [rbp - EMN_TYPE], rbx

    ; A bare instance: no message, so exc_new builds an empty args tuple.
    mov rdi, rbx
    xor esi, esi
    xor edx, edx
    call exc_new
    test rax, rax
    jz .emn_fail
    mov [rbp - EMN_EXC], rax

    ; Anything after the class becomes .args.
    mov rcx, [rbp - EMN_NARGS]
    dec rcx
    jz .emn_done
    mov rdi, rcx
    call tuple_new
    test rax, rax
    jz .emn_fail
    mov [rbp - EMN_TUP], rax
    mov qword [rbp - EMN_I], 0
.emn_copy:
    mov rdx, [rbp - EMN_I]
    mov rcx, [rbp - EMN_NARGS]
    dec rcx
    cmp rdx, rcx
    jge .emn_install
    mov rsi, [rbp - EMN_ARGS]
    lea rcx, [rdx + 1]          ; skip the class
    mov rdi, [rsi + rcx * 8]
    INCREF_V rdi, r8
    mov rax, [rbp - EMN_TUP]
    mov r9, [rax + PyTupleObject.ob_item]
    mov [r9 + rdx * 8], rdi
    inc qword [rbp - EMN_I]
    jmp .emn_copy
.emn_install:
    mov rdi, [rbp - EMN_EXC]
    mov rax, [rdi + PyExceptionObject.exc_args]
    test rax, rax
    jz .emn_set
    mov rdi, rax
    call obj_decref
.emn_set:
    mov rdi, [rbp - EMN_EXC]
    mov rax, [rbp - EMN_TUP]
    mov [rdi + PyExceptionObject.exc_args], rax
.emn_done:
    mov rax, [rbp - EMN_EXC]
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.emn_fail:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.emn_no_type:
    pop rbx
    RAISE exc_TypeError_type, \
          "BaseException.__new__(): not enough arguments"
.emn_not_a_type:
    mov rsi, rbx
    pop rbx
    CSTRING rdi, `BaseException.__new__(X): X is not a type object (\x01)`
    extern raise_type_error_with_name
    jmp raise_type_error_with_name
.emn_not_an_exc:
    ; raise_type_error_with_TYPENAME: the argument here IS a class, so the
    ; name wanted is its own and not its type's, which is always "type".
    mov rsi, rbx
    pop rbx
    CSTRING rdi, \
        `BaseException.__new__(\x01): \x01 is not a subtype of BaseException`
    extern raise_type_error_with_typename
    jmp raise_type_error_with_typename
END_FUNC exc_method_new

;; ============================================================================
;; exc_install_methods() -- give BaseException a tp_dict with __init__ in it
;;
;; One dict on the root of the exception hierarchy is enough: every other
;; exception type reaches it through the MRO, which is what a super() call
;; walks.
;; ============================================================================
EIM_KEY   equ 8
EIM_FN    equ 16
EIM_FRAME equ 40            ; + 1 push = 48, 16-aligned

;; EXC_ADD_METHOD impl, "name" -- one entry in BaseException's tp_dict.
;; rbx holds the dict.  This was open-coded for the single method that used to
;; be here; a second one is what turned fifteen lines into a macro.
;; EXC_ADD_STATIC impl, "name" -- the same, wrapped in a staticmethod so that
;; reading it off an INSTANCE does not bind the instance as its first argument.
%macro EXC_ADD_STATIC 2
    CSTRING rdi, %2
    call str_from_cstr_heap
    mov [rbp - EIM_KEY], rax
    lea rdi, [rel %1]
    CSTRING rsi, %2
    call builtin_func_new
    mov [rbp - EIM_FN], rax
    sub rsp, 16
    mov [rsp], rax
    ; The class to build, which staticmethod_construct no longer ignores.
    extern staticmethod_type
    lea rdi, [rel staticmethod_type]
    mov rsi, rsp
    mov edx, 1
    extern staticmethod_construct
    call staticmethod_construct
    add rsp, 16
    push rax
    mov rdi, [rbp - EIM_FN]
    call obj_decref             ; the staticmethod holds it now
    pop rax
    mov [rbp - EIM_FN], rax
    mov rdi, rbx
    mov rsi, [rbp - EIM_KEY]
    mov rdx, rax
    call dict_set
    mov rdi, [rbp - EIM_KEY]
    call obj_decref
    mov rdi, [rbp - EIM_FN]
    call obj_decref
%endmacro

%macro EXC_ADD_METHOD 2
    CSTRING rdi, %2
    call str_from_cstr_heap
    mov [rbp - EIM_KEY], rax
    lea rdi, [rel %1]
    CSTRING rsi, %2
    call builtin_func_new
    mov [rbp - EIM_FN], rax
    mov rdi, rbx
    mov rsi, [rbp - EIM_KEY]
    mov rdx, rax
    call dict_set
    mov rdi, [rbp - EIM_KEY]
    call obj_decref
    mov rdi, [rbp - EIM_FN]
    call obj_decref
%endmacro

global exc_install_methods
DEF_FUNC exc_install_methods, EIM_FRAME
    push rbx
    call dict_new
    test rax, rax
    jz .eim_out
    mov rbx, rax
    mov [rel exc_BaseException_type + PyTypeObject.tp_dict], rbx

    extern builtin_func_new
    EXC_ADD_METHOD exc_method_init, "__init__"
    EXC_ADD_METHOD exc_method_with_traceback, "with_traceback"
    ; Stamp the owner on it, which is what makes builtin_func_call check the
    ; receiver.  Without it `BaseException.__init__([], 'a')` wrote a tuple
    ; into a list's 57th byte -- exc_args' offset -- and released whatever
    ; word was there.
    lea rdi, [rel exc_BaseException_type]
    extern type_stamp_methods
    call type_stamp_methods

    ; __new__ goes in after the stamp AND inside a staticmethod.  Its first
    ; argument is the class being built, not a BaseException, so the receiver
    ; check the stamp installs would refuse every correct call -- and a bare
    ; builtin function BINDS on instance access, which put the instance in
    ; args[0] for `ValueError("x").__new__(ValueError)`.  A staticmethod is
    ; what CPython makes __new__, and it is what stops both.
    EXC_ADD_STATIC exc_method_new, "__new__"
.eim_out:
    pop rbx
    leave
    ret
END_FUNC exc_install_methods

;; ============================================================================
;; exc_user_init(rdi = an exception type) -> rax = its own __init__, or 0
;;
;; The Python `__init__` a class in the exception hierarchy defines, found
;; along the MRO because it is inherited: `class F(E): pass` runs E's.  A
;; builtin's is the default and does not count -- BaseException's is what
;; stores .args, and it is what refuses keywords.
;; ============================================================================
global exc_user_init
DEF_FUNC exc_user_init, 8            ; 1 push, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    test rdi, rdi
    jz .eui_none
    lea rsi, [rel exc_init_name]
    extern dunder_lookup
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .eui_none
    cmp edx, TAG_PTR
    jne .eui_none
    test rax, rax
    jz .eui_none
    mov rcx, [rax + PyObject.ob_type]
    extern builtin_func_type
    lea rdx, [rel builtin_func_type]
    cmp rcx, rdx
    je .eui_none                ; a builtin: the default, not a definition
    pop rbx
    leave
    ret
.eui_none:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC exc_user_init

DEF_FUNC exc_type_call, ETC_FRAME
    push rbx
    push r12

    mov rbx, rdi            ; rbx = type
    mov [rbp - ETC_ARGS], rsi
    mov [rbp - ETC_NARGS], rdx
    mov qword [rbp - ETC_KW1], 0
    mov qword [rbp - ETC_KW2], 0
    ; The family is read again at .done, and the user-__init__ path below
    ; reaches it without going through exc_kw_family.
    mov qword [rbp - ETC_KWFAM], 0

    ; ------------------------------------------------------------------
    ; Keyword arguments.  AttributeError and ImportError each carry two
    ; named attributes -- `AttributeError("x", name=n, obj=o)` and
    ; `ImportError("x", name=n, path=p)` -- and the stdlib reads both back:
    ; importlib sets ModuleNotFoundError.name, and the "did you mean"
    ; machinery reads AttributeError.name and .obj.  Neither existed here.
    ; The keywords were folded into .args, so `.args` came out one item too
    ; long and `.name` was an AttributeError of its own.
    ;
    ; They are the only builtin exceptions that take keywords; every other
    ; one answers "takes no keyword arguments", as CPython's does.
    ; ------------------------------------------------------------------
    ; A class that defines its own __init__ takes whatever keywords that
    ; __init__ takes, and type_call runs it after this returns.  Consuming
    ; kw_names_pending here, and refusing the keywords before that runs, made
    ; every user exception with a keyword parameter unconstructible.  Only
    ; the count comes off, so .args holds the positionals; the names stay
    ; pending for the __init__ call.
    mov rdi, rbx
    call exc_user_init
    test rax, rax
    jz .etc_no_user_init
    mov r12, [rel kw_names_pending]
    test r12, r12
    jz .etc_kw_done
    mov rcx, [r12 + PyTupleObject.ob_size]
    sub [rbp - ETC_NARGS], rcx
    jmp .etc_kw_done
.etc_no_user_init:

    mov rdi, rbx
    call exc_kw_family
    mov [rbp - ETC_KWFAM], rax

    mov r12, [rel kw_names_pending]
    test r12, r12
    jz .etc_kw_done
    mov qword [rel kw_names_pending], 0     ; consumed, however this ends
    mov rcx, [r12 + PyTupleObject.ob_size]
    sub [rbp - ETC_NARGS], rcx              ; .args gets the positionals only
    cmp qword [rbp - ETC_KWFAM], 0
    je .etc_no_keywords

    xor edx, edx                            ; the keyword index
.etc_kw_loop:
    cmp rdx, [r12 + PyTupleObject.ob_size]
    jge .etc_kw_done
    mov rax, [r12 + PyTupleObject.ob_item]
    mov r8, [rax + rdx*8]                   ; the keyword's name
    mov rax, [rbp - ETC_NARGS]
    add rax, rdx
    mov rcx, [rbp - ETC_ARGS]
    mov r9, [rcx + rax*8]                   ; the value that goes with it

    push rdx
    push r8
    push r9
    sub rsp, 8
    lea rdi, [r8 + PyStrObject.data]
    CSTRING rsi, "name"
    call ap_strcmp
    test eax, eax
    jz .etc_kw_name
    mov r8, [rsp + 16]
    lea rdi, [r8 + PyStrObject.data]
    cmp qword [rbp - ETC_KWFAM], 1
    je .etc_kw_cmp_obj
    CSTRING rsi, "path"
    jmp .etc_kw_cmp2
.etc_kw_cmp_obj:
    CSTRING rsi, "obj"
.etc_kw_cmp2:
    call ap_strcmp
    test eax, eax
    jz .etc_kw_second
    add rsp, 8
    pop r9
    pop r8
    pop rdx
    lea rdi, [r8 + PyStrObject.data]
    mov rsi, [rbx + PyTypeObject.tp_name]
    call exc_raise_bad_keyword
.etc_kw_name:
    add rsp, 8
    pop r9
    pop r8
    pop rdx
    mov [rbp - ETC_KW1], r9
    jmp .etc_kw_next
.etc_kw_second:
    add rsp, 8
    pop r9
    pop r8
    pop rdx
    mov [rbp - ETC_KW2], r9
.etc_kw_next:
    inc rdx
    jmp .etc_kw_loop
.etc_no_keywords:
    xor edi, edi                ; no keyword name: the whole class is refused
    mov rsi, [rbx + PyTypeObject.tp_name]
    call exc_raise_bad_keyword
.etc_kw_done:
    mov rdx, [rbp - ETC_NARGS]
    mov rsi, [rbp - ETC_ARGS]

    ; Check for a constructor (ExceptionGroup's, or OSError's).  It lives in
    ; tp_new; tp_call would make instances callable.
    ;
    ; The search follows the base chain, because a constructor is inherited:
    ; CPython's FileNotFoundError(2, "x", "/f") runs OSError.__new__ and comes
    ; out with .errno and .filename set.  DEF_EXC_TYPE leaves tp_new 0 on every
    ; subclass, so looking only at the exact type found nothing for them.  The
    ; remapping OSError's constructor does is separately gated on the type
    ; being exactly OSError, which is what CPython gates it on too.
    ; The walk stops at object: BaseException's tp_base IS object_type, and
    ; add_builtin_type puts object_type_call in object's tp_new -- so running
    ; past it made every exception build a bare object instead, and `raise
    ; ValueError("x")` became "exceptions must derive from BaseException".
    mov rax, rbx
.etc_find_new:
    lea rcx, [rel object_type]
    cmp rax, rcx
    je .default_exc_create
    mov rcx, [rax + PyTypeObject.tp_new]
    test rcx, rcx
    jnz .etc_have_new
    mov rax, [rax + PyTypeObject.tp_base]
    test rax, rax
    jnz .etc_find_new
    jmp .default_exc_create
.etc_have_new:
    mov rax, rcx
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
    ; The two named attributes, defaulting to None -- CPython reports None
    ; there, not AttributeError, when they were not given.
    cmp qword [rbp - ETC_KWFAM], 0
    je .etc_finish
    mov rdi, [rbp - ETC_EXC]
    mov rsi, [rbp - ETC_KWFAM]
    mov rdx, [rbp - ETC_KW1]
    mov rcx, [rbp - ETC_KW2]
    call exc_store_named
.etc_finish:
    mov rax, [rbp - ETC_EXC]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; tp_call returns one Value
    ret
END_FUNC exc_type_call

;; exc_kw_family(rdi = the type) -> rax = 0 none, 1 AttributeError,
;;                                       2 ImportError
DEF_FUNC_LOCAL exc_kw_family, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    lea rsi, [rel exc_AttributeError_type]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jnz .ekf_attr
    mov rdi, rbx
    lea rsi, [rel exc_ImportError_type]
    call type_is_subtype
    test eax, eax
    jnz .ekf_import
    xor eax, eax
    pop rbx
    leave
    ret
.ekf_attr:
    mov eax, 1
    pop rbx
    leave
    ret
.ekf_import:
    mov eax, 2
    pop rbx
    leave
    ret
END_FUNC exc_kw_family

;; exc_raise_bad_keyword(rdi = the keyword's name as a C string, or 0 when the
;;                        type takes none at all; rsi = the type's name)
;; CPython's two wordings:
;;   'foo' is an invalid keyword argument for AttributeError()
;;   ValueError() takes no keyword arguments
ERB_NAME  equ 8
ERB_TYPE  equ 16
ERB_BUF   equ 192
ERB_FRAME equ 192           ; + 0 pushes = 192, 16-aligned
DEF_FUNC_LOCAL exc_raise_bad_keyword, ERB_FRAME
    mov [rbp - ERB_NAME], rdi
    mov [rbp - ERB_TYPE], rsi
    lea rdi, [rbp - ERB_BUF]
    cmp qword [rbp - ERB_NAME], 0
    je .erb_none_taken
    CSTRING rsi, "'"
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - ERB_NAME]
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, "' is an invalid keyword argument for "
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - ERB_TYPE]
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, "()"
    call rbt_append_cstr
    jmp .erb_raise
.erb_none_taken:
    mov rsi, [rbp - ERB_TYPE]
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, "() takes no keyword arguments"
    call rbt_append_cstr
.erb_raise:
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rbp - ERB_BUF]
    call raise_exception
END_FUNC exc_raise_bad_keyword

;; exc_store_named(rdi = the exception, rsi = the family,
;;                 rdx = the 'name' Value or 0, rcx = the second one or 0)
ESN_EXC   equ 8
ESN_FAM   equ 16
ESN_V1    equ 24
ESN_V2    equ 32
ESN_FRAME equ 48            ; + 0 pushes = 48
DEF_FUNC_LOCAL exc_store_named, ESN_FRAME
    mov [rbp - ESN_EXC], rdi
    mov [rbp - ESN_FAM], rsi
    mov [rbp - ESN_V1], rdx
    mov [rbp - ESN_V2], rcx

    CSTRING rdi, "name"
    call str_from_cstr_heap
    push rax
    sub rsp, 8
    mov rdi, [rbp - ESN_EXC]
    mov rsi, [rsp + 8]
    mov rdx, [rbp - ESN_V1]
    test rdx, rdx
    jnz .esn_have1
    lea rdx, [rel none_singleton]
.esn_have1:
    INCREF_V rdx, rcx
    xor ecx, ecx
    call exc_setattr
    add rsp, 8
    pop rdi
    call obj_decref

    cmp qword [rbp - ESN_FAM], 1
    je .esn_second_obj
    CSTRING rdi, "path"
    jmp .esn_second
.esn_second_obj:
    CSTRING rdi, "obj"
.esn_second:
    call str_from_cstr_heap
    push rax
    sub rsp, 8
    mov rdi, [rbp - ESN_EXC]
    mov rsi, [rsp + 8]
    mov rdx, [rbp - ESN_V2]
    test rdx, rdx
    jnz .esn_have2
    lea rdx, [rel none_singleton]
.esn_have2:
    INCREF_V rdx, rcx
    xor ecx, ecx
    call exc_setattr
    add rsp, 8
    pop rdi
    call obj_decref
    leave
    ret
END_FUNC exc_store_named





;; ============================================================================
;; Data section - Exception type objects and name strings
;; ============================================================================
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
exc_name_FileExistsError:   db "FileExistsError", 0
exc_name_UnicodeTranslateError: db "UnicodeTranslateError", 0
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
exc_name_IndentationError:  db "IndentationError", 0
exc_name_TabError:          db "TabError", 0
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
    dq type_number_methods      ; tp_as_number -- `C | None` builds a
                                ; union, and a class's `|` is its
                                ; METATYPE's slot, not its own
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    ; tp_base — `type`, because this IS a metatype and a metatype is a kind of
    ; type.  With 0 here, type_is_subtype's walk (which for a static type is
    ; the tp_base chain) stopped at exc_metatype itself, so every builtin
    ; exception class answered False to isinstance(cls, type) -- and CPython's
    ; warnings.py opens by asserting exactly that, which is why importing it,
    ; and pathlib, tempfile, shutil, random, argparse and tarfile behind it,
    ; failed.  user_type_metatype has said type_type here all along.
    dq type_type            ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq TYPE_FLAG_METATYPE   ; tp_flags (no HAVE_GC — exc types are static, not gc_alloc'd)
    dq 0                    ; tp_bases
    dq 0                    ; tp_traverse
    dq 0                    ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots

; The metatype is implementation detail -- it exists so an exception class can
; carry a tp_call of its own -- and it says `type`, as user_type_metatype
; already did.  Naming itself made `type(ValueError)` print
; <class 'exception_metatype'> where CPython prints <class 'type'>.
exc_meta_name: db "type", 0
exc_init_name: db "__init__", 0


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
    dq 0                        ; tp_tailslots
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
DEF_EXC_TYPE exc_FileExistsError_type, exc_name_FileExistsError, exc_OSError_type
DEF_EXC_TYPE exc_UnicodeTranslateError_type, exc_name_UnicodeTranslateError, exc_UnicodeError_type
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
; CPython 3.12: TimeoutError is an OSError, and the errno map sends ETIMEDOUT
; here.  It was parented on Exception, so `except OSError` missed it.
DEF_EXC_TYPE exc_TimeoutError_type, exc_name_TimeoutError, exc_OSError_type
DEF_EXC_TYPE exc_GeneratorExit_type, exc_name_GeneratorExit, exc_BaseException_type
DEF_EXC_TYPE exc_ModuleNotFoundError_type, exc_name_ModuleNotFoundError, exc_ImportError_type
DEF_EXC_TYPE exc_SyntaxError_type, exc_name_SyntaxError, exc_Exception_type
DEF_EXC_TYPE exc_IndentationError_type, exc_name_IndentationError, exc_SyntaxError_type
DEF_EXC_TYPE exc_TabError_type, exc_name_TabError, exc_IndentationError_type
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

section .text

;; ============================================================================
;; GC traverse and clear.  These lived in gc.asm, which left the collector
;; holding the reference graph of every type in the system; a type's own
;; file is the only place that knows which of its fields are owned.
;; ============================================================================

;; ============================================================================
;; ---- exc_traverse / exc_clear ----
;; ============================================================================
DEF_FUNC exc_traverse, 8        ; rsp 16-aligned at the call the macros below expand to
    push rbx
    mov rbx, rdi

    ; Visit exc_value (fat)
    mov rdi, [rbx + PyExceptionObject.exc_value]
    VISIT_V rdi, rsi

    ; Visit heap ptrs
    mov rdi, [rbx + PyExceptionObject.exc_tb]
    VISIT_PTR rdi
    mov rdi, [rbx + PyExceptionObject.exc_context]
    VISIT_PTR rdi
    mov rdi, [rbx + PyExceptionObject.exc_cause]
    VISIT_PTR rdi
    mov rdi, [rbx + PyExceptionObject.exc_args]
    VISIT_PTR rdi

    pop rbx
    leave
    ret
END_FUNC exc_traverse

DEF_FUNC exc_clear_gc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    ; DECREF_VAL exc_value
    mov rdi, [rbx + PyExceptionObject.exc_value]
    mov qword [rbx + PyExceptionObject.exc_value], 0
    DECREF_V rdi, rsi

    ; XDECREF + NULL heap ptrs
    mov rdi, [rbx + PyExceptionObject.exc_tb]
    mov qword [rbx + PyExceptionObject.exc_tb], 0
    test rdi, rdi
    jz .no_tb
    call obj_decref
.no_tb:
    mov rdi, [rbx + PyExceptionObject.exc_context]
    mov qword [rbx + PyExceptionObject.exc_context], 0
    test rdi, rdi
    jz .no_ctx
    call obj_decref
.no_ctx:
    mov rdi, [rbx + PyExceptionObject.exc_cause]
    mov qword [rbx + PyExceptionObject.exc_cause], 0
    test rdi, rdi
    jz .no_cause
    call obj_decref
.no_cause:
    mov rdi, [rbx + PyExceptionObject.exc_args]
    mov qword [rbx + PyExceptionObject.exc_args], 0
    test rdi, rdi
    jz .no_args
    call obj_decref
.no_args:

    pop rbx
    leave
    ret
END_FUNC exc_clear_gc


section .rodata
ues_quote:    db "'", 0
ues_codec:    db "' codec can't ", 0
ues_decode_w: db "decode ", 0
ues_encode_w: db "encode ", 0
ues_byte:     db "byte 0x", 0
ues_char:     db "character ", 0
ues_bytes_pl: db "bytes", 0
ues_chars_pl: db "characters", 0
ues_in_pos:   db " in position ", 0
ues_dash:     db "-", 0
ues_colon:    db ": ", 0
