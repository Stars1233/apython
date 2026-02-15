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
extern ap_free
extern str_from_cstr
extern obj_decref
extern obj_incref
extern str_type
extern type_getattr
extern type_repr
extern type_type

; exc_new(PyTypeObject *type, PyObject *msg_str) -> PyExceptionObject*
; Creates a new exception with given type and message string.
; msg_str is INCREFed. type is stored but not INCREFed (types are immortal).
DEF_FUNC exc_new
    push rbx
    push r12

    mov rbx, rdi            ; type
    mov r12, rsi            ; msg_str

    ; Allocate exception object
    mov edi, PyExceptionObject_size
    call ap_malloc

    ; Initialize fields
    mov qword [rax + PyExceptionObject.ob_refcnt], 1
    mov [rax + PyExceptionObject.ob_type], rbx
    mov [rax + PyExceptionObject.exc_type], rbx
    mov [rax + PyExceptionObject.exc_value], r12
    mov qword [rax + PyExceptionObject.exc_tb], 0
    mov qword [rax + PyExceptionObject.exc_context], 0
    mov qword [rax + PyExceptionObject.exc_cause], 0

    ; INCREF the message string
    test r12, r12
    jz .done
    INCREF r12

.done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC exc_new

; exc_from_cstr(PyTypeObject *type, const char *msg) -> PyExceptionObject*
; Creates exception with a C string message (converted to PyStrObject).
DEF_FUNC exc_from_cstr
    push rbx

    mov rbx, rdi            ; save type

    ; Convert C string to PyStrObject
    mov rdi, rsi
    call str_from_cstr
    ; rax = str obj (refcnt=1)

    ; Now create exception: exc_new(type, str)
    mov rdi, rbx
    mov rsi, rax
    call exc_new
    ; rax = exception obj
    ; exc_new INCREFs the str, so we need to DECREF our copy
    push rax
    mov rdi, [rax + PyExceptionObject.exc_value]
    call obj_decref
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

    ; XDECREF exc_value
    mov rdi, [rbx + PyExceptionObject.exc_value]
    test rdi, rdi
    jz .no_value
    call obj_decref
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

    ; Free the object
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC exc_dealloc

; exc_repr(PyExceptionObject *exc) -> PyObject* (string)
; Returns "TypeName(msg)" or just "TypeName()" if no message.
DEF_FUNC exc_repr
    push rbx
    push r12
    sub rsp, 256            ; buffer for formatting

    mov rbx, rdi            ; exc

    ; Get type name
    mov rax, [rbx + PyExceptionObject.ob_type]
    mov r12, [rax + PyTypeObject.tp_name]  ; C string ptr

    ; Build string: "TypeName(msg)" into stack buffer
    lea rdi, [rbp - 256 - 16]   ; buffer start (well within stack)
    ; Copy type name
    mov rsi, r12
.copy_name:
    lodsb
    test al, al
    jz .name_done
    stosb
    jmp .copy_name
.name_done:
    mov byte [rdi], '('
    inc rdi

    ; Copy message if present
    mov rax, [rbx + PyExceptionObject.exc_value]
    test rax, rax
    jz .no_msg

    ; Check if message is a string
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .no_msg

    ; Copy string data
    mov rsi, rax
    add rsi, PyStrObject.data
    mov rcx, [rax + PyStrObject.ob_size]
.copy_msg:
    test rcx, rcx
    jz .msg_done
    lodsb
    stosb
    dec rcx
    jmp .copy_msg

.no_msg:
.msg_done:
    mov byte [rdi], ')'
    inc rdi
    mov byte [rdi], 0

    ; Create string from buffer
    lea rdi, [rbp - 256 - 16]
    call str_from_cstr

    add rsp, 256
    pop r12
    pop rbx
    leave
    ret
END_FUNC exc_repr

; exc_str(PyExceptionObject *exc) -> PyObject* (string)
; Returns the message string, or type name if no message.
DEF_FUNC exc_str

    ; Return exc_value if it's a string
    mov rax, [rdi + PyExceptionObject.exc_value]
    test rax, rax
    jz .use_type_name

    ; Check if it's a string
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .use_type_name

    ; INCREF and return the message
    INCREF rax
    leave
    ret

.use_type_name:
    ; Return type name as string
    mov rax, [rdi + PyExceptionObject.ob_type]
    mov rdi, [rax + PyTypeObject.tp_name]
    call str_from_cstr
    leave
    ret
END_FUNC exc_str

; exc_isinstance(PyExceptionObject *exc, PyTypeObject *type) -> int (0/1)
; Check if exception is an instance of type, walking tp_base chain.
DEF_FUNC_BARE exc_isinstance
    ; rdi = exc, rsi = target type
    mov rax, [rdi + PyExceptionObject.ob_type]
.walk:
    test rax, rax
    jz .not_match
    cmp rax, rsi
    je .match
    mov rax, [rax + PyTypeObject.tp_base]
    jmp .walk
.match:
    mov eax, 1
    ret
.not_match:
    xor eax, eax
    ret
END_FUNC exc_isinstance

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
DEF_FUNC exc_type_call
    push rbx

    mov rbx, rdi            ; rbx = type

    ; Get message from args[0] if nargs >= 1
    test edx, edx
    jz .no_args
    mov rsi, [rsi]          ; rsi = args[0] (message string)
    jmp .create
.no_args:
    xor esi, esi            ; msg = NULL (no message)
.create:
    ; Create exception: exc_new(type, msg)
    mov rdi, rbx
    call exc_new
    ; rax = new exception object

    pop rbx
    leave
    ret
END_FUNC exc_type_call

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

; Exception metatype - provides tp_call so exception types can be called
; e.g., ValueError("msg") works via CALL opcode
align 8
global exc_metatype
exc_metatype:
    dq 1                    ; ob_refcnt (immortal)
    dq type_type            ; ob_type
    dq exc_meta_name        ; tp_name
    dq 192                  ; tp_basicsize (PyTypeObject size)
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
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases

exc_meta_name: db "exception_metatype", 0

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
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
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
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases
%endmacro

; Define all exception types
DEF_EXC_TYPE exc_BaseException_type, exc_name_BaseException, 0
DEF_EXC_TYPE exc_Exception_type, exc_name_Exception, exc_BaseException_type
DEF_EXC_TYPE exc_TypeError_type, exc_name_TypeError, exc_Exception_type
DEF_EXC_TYPE exc_ValueError_type, exc_name_ValueError, exc_Exception_type
DEF_EXC_TYPE exc_KeyError_type, exc_name_KeyError, exc_LookupError_type
DEF_EXC_TYPE exc_IndexError_type, exc_name_IndexError, exc_LookupError_type
DEF_EXC_TYPE exc_AttributeError_type, exc_name_AttributeError, exc_Exception_type
DEF_EXC_TYPE exc_NameError_type, exc_name_NameError, exc_Exception_type
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
