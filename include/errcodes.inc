; errcodes.inc - Error and exception codes

%ifndef ERRCODES_INC
%define ERRCODES_INC

; Exception type IDs
EXC_BASE_EXCEPTION     equ 0
EXC_EXCEPTION          equ 1
EXC_TYPE_ERROR         equ 2
EXC_VALUE_ERROR        equ 3
EXC_KEY_ERROR          equ 4
EXC_INDEX_ERROR        equ 5
EXC_ATTRIBUTE_ERROR    equ 6
EXC_NAME_ERROR         equ 7
EXC_RUNTIME_ERROR      equ 8
EXC_STOP_ITERATION     equ 9
EXC_ZERO_DIVISION      equ 10
EXC_IMPORT_ERROR       equ 11
EXC_NOT_IMPLEMENTED    equ 12
EXC_FILE_NOT_FOUND     equ 13
EXC_OVERFLOW_ERROR     equ 14
EXC_ASSERTION_ERROR    equ 15
EXC_KEYBOARD_INTERRUPT equ 16
EXC_MEMORY_ERROR       equ 17
EXC_RECURSION_ERROR    equ 18
EXC_SYSTEM_EXIT        equ 19
EXC_OS_ERROR           equ 20
EXC_LOOKUP_ERROR       equ 21
EXC_ARITHMETIC_ERROR   equ 22
EXC_UNICODE_ERROR      equ 23
EXC_COUNT              equ 24  ; total

; Exception object
struc PyExceptionObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .exc_type:    resq 1  ; +16: ptr to exception type object
    .exc_value:   resq 1  ; +24: ptr to value (usually a string)
    .exc_tb:      resq 1  ; +32: ptr to traceback or NULL
    .exc_context: resq 1  ; +40: ptr to __context__ or NULL
    .exc_cause:   resq 1  ; +48: ptr to __cause__ or NULL
    .exc_args:    resq 1  ; +56: ptr to args tuple or NULL
endstruc

; Fatal error codes (non-Python, internal)
ERR_OUT_OF_MEMORY     equ 1
ERR_INVALID_PYC       equ 2
ERR_MARSHAL_ERROR     equ 3
ERR_INTERNAL          equ 4
ERR_UNIMPLEMENTED_OP  equ 5
ERR_STACK_OVERFLOW    equ 6
ERR_STACK_UNDERFLOW   equ 7

%endif ; ERRCODES_INC
