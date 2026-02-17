; object.inc - Object layout definitions for apython

%ifndef OBJECT_INC
%define OBJECT_INC

; PyObject base structure
struc PyObject
    .ob_refcnt: resq 1    ; +0: reference count (int64)
    .ob_type:   resq 1    ; +8: pointer to type object
endstruc

OBJ_HEADER_SIZE equ PyObject_size  ; 16

; PyVarObject (variable-size objects)
struc PyVarObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: number of items
endstruc

VAR_HEADER_SIZE equ PyVarObject_size  ; 24

; Integer object (GMP-backed)
; mpz_t is a struct: { int _mp_alloc, int _mp_size, mp_limb_t *_mp_d }
; On x86-64 GMP: sizeof(mpz_t) = 16 bytes (4+4+8)
struc PyIntObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .mpz:       resb 16   ; +16: inline mpz_t (alloc:4, size:4, *d:8)
endstruc

; Float object
struc PyFloatObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .value:     resq 1    ; +16: double value
endstruc

; String object
struc PyStrObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: length in bytes
    .ob_hash:   resq 1    ; +24: cached hash (-1 = not computed)
    .data:                 ; +32: null-terminated UTF-8 bytes (variable)
endstruc

; Bytes object
struc PyBytesObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: length in bytes
    .data:                 ; +24: raw bytes (variable)
endstruc

; ByteArray object — same layout as bytes (variable-size inline data)
struc PyByteArrayObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: length in bytes
    .data:                 ; +24: raw bytes (variable)
endstruc

; MemoryView object — reference to a slice of a bytes-like object
struc PyMemoryViewObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .mv_source: resq 1    ; +16: ptr to source bytes-like object (strong ref)
    .mv_buf:    resq 1    ; +24: ptr to start of buffer data
    .mv_len:    resq 1    ; +32: length of view in bytes
endstruc

; List object
struc PyListObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: item count
    .allocated: resq 1    ; +24: capacity
    .ob_item:   resq 1    ; +32: ptr to PyObject*[] array
endstruc

; Tuple object
struc PyTupleObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: item count
    .ob_hash:   resq 1    ; +24: cached hash (-1 = not computed)
    .ob_item:              ; +32: inline PyObject* array (variable)
endstruc

; Dict object
struc PyDictObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: item count
    .capacity:  resq 1    ; +24: hash table capacity
    .entries:   resq 1    ; +32: ptr to entry array
    .dk_version: resq 1   ; +40: version counter (bumped on mutation)
endstruc

; Dict entry
struc DictEntry
    .hash:      resq 1     ; +0: cached hash
    .key:       resq 1     ; +8: key PyObject* (64-bit)
    .value:     resq 1     ; +16: value payload
    .value_tag: resq 1     ; +24: value tag
endstruc

DICT_ENTRY_SIZE equ DictEntry_size  ; 32

; Function object
struc PyFuncObject
    .ob_refcnt:     resq 1  ; +0
    .ob_type:       resq 1  ; +8
    .func_code:     resq 1  ; +16: ptr to code object
    .func_globals:  resq 1  ; +24: ptr to globals dict
    .func_name:     resq 1  ; +32: ptr to str object
    .func_defaults: resq 1  ; +40: ptr to defaults tuple or NULL
    .func_closure:  resq 1  ; +48: ptr to closure tuple or NULL
    .func_kwdefaults: resq 1 ; +56: ptr to kwdefaults dict or NULL
    .func_dict:     resq 1  ; +64: ptr to __dict__ or NULL (arbitrary attrs)
endstruc

; Code object
struc PyCodeObject
    .ob_refcnt:         resq 1  ; +0
    .ob_type:           resq 1  ; +8
    .co_argcount:       resd 1  ; +16
    .co_kwonlyargcount: resd 1  ; +20
    .co_nlocals:        resd 1  ; +24
    .co_stacksize:      resd 1  ; +28
    .co_flags:          resd 1  ; +32
    .co_nlocalsplus:    resd 1  ; +36
    .co_consts:         resq 1  ; +40: ptr to tuple
    .co_names:          resq 1  ; +48: ptr to tuple
    .co_localsplusnames: resq 1 ; +56: ptr to tuple
    .co_localspluskinds: resq 1 ; +64: ptr to bytes
    .co_filename:       resq 1  ; +72: ptr to str
    .co_name:           resq 1  ; +80: ptr to str
    .co_qualname:       resq 1  ; +88: ptr to str
    .co_exceptiontable: resq 1  ; +96: ptr to bytes
    .co_code_len:       resd 1  ; +104: bytecode length in bytes
    .co_posonlyargcount: resd 1 ; +108: positional-only arg count
    .co_code:                   ; +112: bytecode (variable)
endstruc

; Module object
struc PyModuleObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .mod_name:    resq 1  ; +16: ptr to str
    .mod_dict:    resq 1  ; +24: ptr to module dict
endstruc

; Class instance object
struc PyInstanceObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8  (type is the class)
    .inst_dict:   resq 1  ; +16: ptr to instance __dict__
endstruc

; Int subclass instance (class MyInt(int): ...)
struc PyIntSubclassObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8  (type is the subclass)
    .inst_dict:   resq 1  ; +16: ptr to instance __dict__
    .int_value:   resq 1  ; +24: SmallInt tagged value or GMP pointer
endstruc

; Iterator objects
struc PyListIterObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .it_seq:    resq 1    ; +16: ptr to list
    .it_index:  resq 1    ; +24: current index
endstruc

struc PyTupleIterObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .it_seq:    resq 1
    .it_index:  resq 1
endstruc

struc PyStrIterObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .it_seq:    resq 1    ; +16: ptr to str
    .it_index:  resq 1    ; +24: current byte index
endstruc

struc PyDictIterObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .it_dict:   resq 1    ; +16: ptr to dict
    .it_index:  resq 1    ; +24: current entry index
    .it_kind:   resq 1    ; +32: 0=keys, 1=values, 2=items
endstruc

struc PyRangeIterObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .it_current: resq 1   ; +16: current value
    .it_stop:    resq 1   ; +24: stop value
    .it_step:    resq 1   ; +32: step value
endstruc

struc PyStrIterObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .it_seq:    resq 1
    .it_index:  resq 1
endstruc

struc PyBytesIterObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .it_seq:    resq 1    ; +16: ptr to bytes obj
    .it_index:  resq 1    ; +24: current index
endstruc

; Slice object
struc PySliceObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .start:     resq 1    ; +16
    .stop:      resq 1    ; +24
    .step:      resq 1    ; +32
endstruc

; Cell object (for closures)
struc PyCellObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .ob_ref:    resq 1    ; +16: contained value payload (or 0 if empty)
    .ob_ref_tag: resq 1   ; +24: contained value tag (TAG_NULL if empty)
endstruc

; Bound method
struc PyMethodObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .im_func:   resq 1    ; +16: function
    .im_self:   resq 1    ; +24: instance
endstruc

; StaticMethod wrapper (descriptor)
struc PyStaticMethodObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .sm_callable: resq 1  ; +16: wrapped function
endstruc

; ClassMethod wrapper (descriptor)
struc PyClassMethodObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .cm_callable: resq 1  ; +16: wrapped function
endstruc

; Property descriptor
struc PyPropertyObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .prop_get:    resq 1  ; +16: fget function or NULL
    .prop_set:    resq 1  ; +24: fset function or NULL
    .prop_del:    resq 1  ; +32: fdel function or NULL
endstruc

; Generator object
struc PyGenObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .gi_frame:  resq 1    ; +16: suspended frame
    .gi_running: resq 1   ; +24: 1 if currently executing
    .gi_code:   resq 1    ; +32: code object
    .gi_name:   resq 1    ; +40: name string
endstruc

; File object (for sys.stdout, sys.stderr, sys.stdin)
struc PyFileObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .file_fd:     resq 1  ; +16: OS file descriptor
    .file_name:   resq 1  ; +24: name string ('<stdout>' etc)
    .file_mode:   resq 1  ; +32: mode string ('w', 'r')
endstruc

; Sentinel values
NULL_OBJ equ 0

%endif ; OBJECT_INC
