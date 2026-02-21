; types.inc - Type object layout for apython

%ifndef TYPES_INC
%define TYPES_INC

; Type object layout
struc PyTypeObject
    .ob_refcnt:      resq 1  ; +0
    .ob_type:        resq 1  ; +8  (metatype, usually &type_type)
    .tp_name:        resq 1  ; +16  ptr to C string
    .tp_basicsize:   resq 1  ; +24
    .tp_dealloc:     resq 1  ; +32  function ptr
    .tp_repr:        resq 1  ; +40  function ptr
    .tp_str:         resq 1  ; +48  function ptr (defaults to tp_repr if NULL)
    .tp_hash:        resq 1  ; +56  function ptr
    .tp_call:        resq 1  ; +64  function ptr
    .tp_getattr:     resq 1  ; +72  function ptr
    .tp_setattr:     resq 1  ; +80  function ptr
    .tp_richcompare: resq 1  ; +88  function ptr
    .tp_iter:        resq 1  ; +96  function ptr
    .tp_iternext:    resq 1  ; +104 function ptr
    .tp_init:        resq 1  ; +112 function ptr
    .tp_new:         resq 1  ; +120 function ptr
    .tp_as_number:   resq 1  ; +128 ptr to number methods or NULL
    .tp_as_sequence: resq 1  ; +136 ptr to sequence methods or NULL
    .tp_as_mapping:  resq 1  ; +144 ptr to mapping methods or NULL
    .tp_base:        resq 1  ; +152 ptr to base type
    .tp_dict:        resq 1  ; +160 ptr to type's __dict__
    .tp_mro:         resq 1  ; +168 ptr to MRO tuple
    .tp_flags:       resq 1  ; +176
    .tp_bases:       resq 1  ; +184 ptr to bases tuple
    .tp_traverse:    resq 1  ; +192 GC traverse function ptr
    .tp_clear:       resq 1  ; +200 GC clear function ptr
endstruc

TYPE_OBJECT_SIZE equ PyTypeObject_size

; Number methods structure
struc PyNumberMethods
    .nb_add:           resq 1  ; +0
    .nb_subtract:      resq 1  ; +8
    .nb_multiply:      resq 1  ; +16
    .nb_remainder:     resq 1  ; +24
    .nb_divmod:        resq 1  ; +32
    .nb_power:         resq 1  ; +40
    .nb_negative:      resq 1  ; +48
    .nb_positive:      resq 1  ; +56
    .nb_absolute:      resq 1  ; +64
    .nb_bool:          resq 1  ; +72
    .nb_invert:        resq 1  ; +80
    .nb_lshift:        resq 1  ; +88
    .nb_rshift:        resq 1  ; +96
    .nb_and:           resq 1  ; +104
    .nb_xor:           resq 1  ; +112
    .nb_or:            resq 1  ; +120
    .nb_int:           resq 1  ; +128
    .nb_float:         resq 1  ; +136
    .nb_floor_divide:  resq 1  ; +144
    .nb_true_divide:   resq 1  ; +152
    .nb_index:         resq 1  ; +160
    ; Inplace slots (used by NB_INPLACE_* opcodes)
    .nb_iadd:          resq 1  ; +168
    .nb_isub:          resq 1  ; +176
    .nb_imul:          resq 1  ; +184
    .nb_irem:          resq 1  ; +192
    .nb_ipow:          resq 1  ; +200
    .nb_ilshift:       resq 1  ; +208
    .nb_irshift:       resq 1  ; +216
    .nb_iand:          resq 1  ; +224
    .nb_ixor:          resq 1  ; +232
    .nb_ior:           resq 1  ; +240
    .nb_ifloor_divide: resq 1  ; +248
    .nb_itrue_divide:  resq 1  ; +256
endstruc

; Sequence methods structure
struc PySequenceMethods
    .sq_length:        resq 1  ; +0
    .sq_concat:        resq 1  ; +8
    .sq_repeat:        resq 1  ; +16
    .sq_item:          resq 1  ; +24
    .sq_ass_item:      resq 1  ; +32
    .sq_contains:      resq 1  ; +40
    .sq_inplace_concat: resq 1 ; +48
    .sq_inplace_repeat: resq 1 ; +56
endstruc

; Mapping methods structure
struc PyMappingMethods
    .mp_length:        resq 1  ; +0
    .mp_subscript:     resq 1  ; +8
    .mp_ass_subscript: resq 1  ; +16
endstruc

; Type flags
TYPE_FLAG_HEAPTYPE     equ 0x0001
TYPE_FLAG_BASETYPE     equ 0x0002
TYPE_FLAG_HAVE_GC      equ 0x0004
TYPE_FLAG_INT_SUBCLASS equ 0x0100
TYPE_FLAG_STR_SUBCLASS equ 0x0200
TYPE_FLAG_LIST_SUBCLASS equ 0x0400
TYPE_FLAG_TUPLE_SUBCLASS equ 0x0800
TYPE_FLAG_DICT_SUBCLASS equ 0x1000
TYPE_FLAG_HAS_SLOTS     equ 0x2000

; Comparison opcodes (used by tp_richcompare)
PY_LT equ 0
PY_LE equ 1
PY_EQ equ 2
PY_NE equ 3
PY_GT equ 4
PY_GE equ 5

%endif ; TYPES_INC
