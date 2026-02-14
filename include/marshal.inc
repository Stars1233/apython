; marshal.inc - Marshal format type codes

%ifndef MARSHAL_INC
%define MARSHAL_INC

; Marshal type codes (from CPython Python/marshal.c)
MARSHAL_TYPE_NULL             equ 0x30  ; '0'
MARSHAL_TYPE_NONE             equ 0x4E  ; 'N'
MARSHAL_TYPE_FALSE            equ 0x46  ; 'F'
MARSHAL_TYPE_TRUE             equ 0x54  ; 'T'
MARSHAL_TYPE_STOPITER         equ 0x53  ; 'S'
MARSHAL_TYPE_ELLIPSIS         equ 0x2E  ; '.'
MARSHAL_TYPE_INT              equ 0x69  ; 'i'
MARSHAL_TYPE_INT64            equ 0x49  ; 'I'
MARSHAL_TYPE_FLOAT            equ 0x66  ; 'f'
MARSHAL_TYPE_BINARY_FLOAT     equ 0x67  ; 'g'
MARSHAL_TYPE_COMPLEX          equ 0x78  ; 'x'
MARSHAL_TYPE_BINARY_COMPLEX   equ 0x79  ; 'y'
MARSHAL_TYPE_LONG             equ 0x6C  ; 'l'
MARSHAL_TYPE_STRING           equ 0x73  ; 's'
MARSHAL_TYPE_INTERNED         equ 0x74  ; 't'
MARSHAL_TYPE_REF              equ 0x72  ; 'r'
MARSHAL_TYPE_TUPLE            equ 0x28  ; '('
MARSHAL_TYPE_LIST             equ 0x5B  ; '['
MARSHAL_TYPE_DICT             equ 0x7B  ; '{'
MARSHAL_TYPE_CODE             equ 0x63  ; 'c'
MARSHAL_TYPE_UNICODE          equ 0x75  ; 'u'
MARSHAL_TYPE_SET              equ 0x3C  ; '<'
MARSHAL_TYPE_FROZENSET        equ 0x3E  ; '>'
MARSHAL_TYPE_ASCII            equ 0x61  ; 'a'
MARSHAL_TYPE_ASCII_INTERNED   equ 0x41  ; 'A'
MARSHAL_TYPE_SMALL_TUPLE      equ 0x29  ; ')'
MARSHAL_TYPE_SHORT_ASCII      equ 0x7A  ; 'z'
MARSHAL_TYPE_SHORT_ASCII_INTERNED equ 0x5A ; 'Z'

; Flag to indicate object should be added to reference list
MARSHAL_FLAG_REF              equ 0x80

; .pyc file header
PYC_MAGIC_3_12    equ 0x0A0D0DCB  ; 3531 in little-endian with \r\n
PYC_HEADER_SIZE   equ 16          ; magic(4) + flags(4) + timestamp/size(8)

%endif ; MARSHAL_INC
