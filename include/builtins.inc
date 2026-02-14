; builtins.inc - Builtin function IDs

%ifndef BUILTINS_INC
%define BUILTINS_INC

; Builtin function identifiers
BUILTIN_PRINT       equ 0
BUILTIN_LEN         equ 1
BUILTIN_RANGE       equ 2
BUILTIN_TYPE        equ 3
BUILTIN_ISINSTANCE  equ 4
BUILTIN_ID          equ 5
BUILTIN_INT         equ 6
BUILTIN_STR         equ 7
BUILTIN_BOOL        equ 8
BUILTIN_LIST        equ 9
BUILTIN_DICT        equ 10
BUILTIN_TUPLE       equ 11
BUILTIN_SET         equ 12
BUILTIN_FLOAT       equ 13
BUILTIN_INPUT       equ 14
BUILTIN_ABS         equ 15
BUILTIN_MIN         equ 16
BUILTIN_MAX         equ 17
BUILTIN_SUM         equ 18
BUILTIN_SORTED      equ 19
BUILTIN_REVERSED    equ 20
BUILTIN_ENUMERATE   equ 21
BUILTIN_ZIP         equ 22
BUILTIN_MAP         equ 23
BUILTIN_FILTER      equ 24
BUILTIN_HASATTR     equ 25
BUILTIN_GETATTR     equ 26
BUILTIN_SETATTR     equ 27
BUILTIN_REPR        equ 28
BUILTIN_HASH        equ 29
BUILTIN_CALLABLE    equ 30
BUILTIN_ITER        equ 31
BUILTIN_NEXT        equ 32
BUILTIN_SUPER       equ 33
BUILTIN_ORD         equ 34
BUILTIN_CHR         equ 35
BUILTIN_HEX         equ 36
BUILTIN_OCT         equ 37
BUILTIN_BIN         equ 38
BUILTIN_ROUND       equ 39
BUILTIN_DIVMOD      equ 40
BUILTIN_POW         equ 41
BUILTIN_FORMAT      equ 42
BUILTIN_VARS        equ 43
BUILTIN_DIR         equ 44
BUILTIN_OBJECT      equ 45
BUILTIN_ISSUBCLASS  equ 46
BUILTIN_COUNT       equ 47  ; total number of builtins

; Builtin C function object
struc PyBuiltinObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .func_id:     resq 1  ; +16: builtin function ID
    .func_name:   resq 1  ; +24: ptr to name string
    .func_ptr:    resq 1  ; +32: ptr to C implementation
endstruc

%endif ; BUILTINS_INC
