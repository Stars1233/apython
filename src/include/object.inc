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

; Integer object (GMP-backed)
; CPython's integer hash modulus (PyHASH_MODULUS = 2^61 - 1 on 64-bit).
; sign(v) * (|v| mod PYHASH_MODULUS); see int_hash_i64 in src/pyo/int.asm.
PYHASH_MODULUS equ 0x1FFFFFFFFFFFFFFF

; mpz_t is a struct: { int _mp_alloc, int _mp_size, mp_limb_t *_mp_d }
; On x86-64 GMP: sizeof(mpz_t) = 16 bytes (4+4+8)
; An integer is "compact" when its value fits in int64: .ival holds it and
; .mpz is UNINITIALIZED, so creating one costs no GMP init and no limb
; allocation.  Anything wider is GMP-backed (.compact == 0, .mpz valid).
; Reading .mpz on a possibly-compact object requires INT_NEED_MPZ first,
; which promotes in place.  .mpz stays at +16 so existing GMP code is
; unaffected.
struc PyIntObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .mpz:       resb 16   ; +16: inline mpz_t (alloc:4, size:4, *d:8)
    .ival:      resq 1    ; +32: int64 value      (valid iff .compact != 0)
    .compact:   resq 1    ; +40: 1 = compact, 0 = GMP-backed
endstruc

; Promote a possibly-compact PyIntObject to GMP form in place.  Two
; instructions when it is already GMP-backed; int_promote_mpz preserves every
; register, so this is safe to drop in front of any .mpz access.
%macro INT_NEED_MPZ 1           ; %1 = register holding PyIntObject*
    cmp qword [%1 + PyIntObject.compact], 0
    je %%done
%ifidni %1, rdi
    call int_promote_mpz
%else
    push rdi
    mov rdi, %1
    call int_promote_mpz
    pop rdi
%endif
%%done:
%endmacro

; Float object
struc PyFloatObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .value:     resq 1    ; +16: double value
endstruc

; Complex object.
;
; Unlike float -- which is a NaN-boxed immediate, leaving PyFloatObject above
; all but vestigial -- a complex is a real heap object.  A pointer is its own
; Value, so the box is untouched and --selftest-value has nothing to say about
; it.  It holds two raw doubles and no references, so it is invisible to the
; collector: no tp_traverse, no tp_clear, no TYPE_FLAG_HAVE_GC, and ap_malloc
; rather than gc_alloc (gc_alloc hands back raw + GC_HEAD_SIZE, which
; obj_dealloc's plain-free path would hand to ap_free unshifted).
struc PyComplexObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .cval_real: resq 1    ; +16: double
    .cval_imag: resq 1    ; +24: double
endstruc

; String object
; A str stores UTF-8 and carries both lengths: ob_size is what the bytes
; occupy, ob_length is how many code points that is.  Python counts in code
; points, C counts in bytes, and almost every string here is ASCII -- where
; the two are equal, which is the fast path every code-point-aware operation
; checks first.
struc PyStrObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: length in bytes
    .ob_hash:   resq 1    ; +24: cached hash (-1 = not computed)
    .ob_length: resq 1    ; +32: length in code points
    .data:                 ; +40: null-terminated UTF-8 bytes (variable)
endstruc

; Bytes object
struc PyBytesObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: length in bytes
    .data:                 ; +24: raw bytes (variable)
endstruc

; ByteArray object.  Unlike bytes, the data is OUT OF LINE: a bytearray is
; mutable and grows, and inline data at a fixed offset in the object body
; cannot -- which is why it used to have no append, no extend and no slice
; assignment at all.  ob_bytes is its own allocation, doubling as it fills, so
; appending n bytes costs O(n) copies in total.  The compiler's Buf is the
; same shape.
;
; The buffer is always NUL-terminated one byte past ob_size, so the data
; pointer can be handed to anything that wants a C string; ob_cap counts the
; usable bytes and excludes that terminator.
struc PyByteArrayObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: length in bytes
    .ob_cap:    resq 1    ; +24: usable bytes at ob_bytes, excluding the NUL
    .ob_bytes:  resq 1    ; +32: the buffer
endstruc

; MemoryView object — reference to a slice of a bytes-like object
struc PyMemoryViewObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .mv_source: resq 1    ; +16: the source object, strong; 0 once released
    .mv_buf:    resq 1    ; +24: the start of the data; 0 once released
    .mv_len:    resq 1    ; +32: the view's length in BYTES, not items
    .mv_itemsize: resq 1  ; +40: bytes per item, 1 unless cast()
    .mv_format:   resq 1  ; +48: the one-character format, as a C string
    .mv_readonly: resq 1  ; +56: 1 for a view over bytes
endstruc

; A released memoryview: mv_buf is 0 and every operation raises.  The source
; reference is dropped at that moment rather than at dealloc, which is the
; point of release() -- it lets the buffer's owner resize again.
MV_RELEASED equ 0

; List object
struc PyListObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: item count
    .allocated: resq 1    ; +24: capacity
    .ob_item:   resq 1    ; +32: ptr to payload (u64[]) array
endstruc

; Weak reference (src/pyo/weakrefmod.asm).  The referent is borrowed and is
; zeroed when it dies; the side table in that file owns the reference objects.
struc PyWeakRefObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .wr_object:   resq 1  ; +16: the referent, 0 once it is gone
    .wr_callback: resq 1  ; +24: owned callable, or 0
    .wr_hash:     resq 1  ; +32: cached hash, -1 until computed
endstruc

; Tuple object
struc PyTupleObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: item count
    .ob_hash:   resq 1    ; +24: cached hash (-1 = not computed)
    .ob_item:   resq 1     ; +32: ptr to payload (u64[]) array
endstruc

; Dict object
struc PyDictObject
    .ob_refcnt: resq 1    ; +0
    .ob_type:   resq 1    ; +8
    .ob_size:   resq 1    ; +16: item count
    .capacity:  resq 1    ; +24: hash table capacity
    .entries:   resq 1    ; +32: ptr to entry array
    .dk_version: resq 1   ; +40: version counter (bumped on mutation)
    .dk_tombstones: resq 1 ; +48: count of deleted (holed) entries
    ; Compact layout, appended so `set` -- which shares this header but has
    ; its own entry code -- is unaffected by their presence.
    .dk_indices: resq 1   ; +56: ptr to int64[capacity], -1 empty, -2 dummy,
                          ;      else an index into .entries
    .dk_nentries: resq 1  ; +64: high-water mark of .entries in use
endstruc

; The dict is CPython's compact layout: a sparse index array over a *dense*
; entry array appended in insertion order.  Iteration walks the dense array,
; which is why dict order is insertion order.  The unused tail of .entries is
; kept zeroed, so a walker that scans 0..capacity and skips key == 0 reads
; the same items in the same order without knowing about .dk_nentries.
DICT_IX_EMPTY equ -1
DICT_IX_DUMMY equ -2

; Dict entry
; An entry's state lives in .key and .hash rather than in a tag byte:
;   occupied  -> .key != 0
;   empty     -> .key == 0 and .hash == 0
;   tombstone -> .key == 0 and .hash == -1
; A key Value is never 0, and CPython's hash never returns -1 (it maps -1 to
; -2), so the three states are distinct and each test is an imm32 compare.
struc DictEntry
    .hash:      resq 1     ; +0:  cached hash of key (-1 marks a tombstone)
    .key:       resq 1     ; +8:  key Value (0 when not occupied)
    .value:     resq 1     ; +16: value Value
endstruc

DICT_ENTRY_SIZE equ DictEntry_size  ; 24
DICT_INIT_CAP   equ 8               ; initial hash-table slots for dict and set
ENTRY_TOMBSTONE_HASH equ -1

; A set entry mirrors DictEntry minus the value: hash at +0, key Value at +8.
SET_ENTRY_HASH equ 0
SET_ENTRY_KEY  equ 8
SET_ENTRY_SIZE equ 16           ; the two fields above, and nothing else

; Set-entry counterpart of ENTRY_CLASSIFY.
%macro SET_ENTRY_CLASSIFY 3     ; %1 = entry ptr, %2 = empty label, %3 = tombstone label
    cmp qword [%1 + SET_ENTRY_KEY], 0
    jne %%occupied
    cmp qword [%1 + SET_ENTRY_HASH], ENTRY_TOMBSTONE_HASH
    je %3
    jmp %2
%%occupied:
%endmacro

; Classify a dict or set entry during a probe.  Jumps to %2 when the slot was
; never used (probing can stop) and to %3 when it holds a tombstone (probing
; must continue); falls through when the slot is occupied.
%macro ENTRY_CLASSIFY 3         ; %1 = entry ptr, %2 = empty label, %3 = tombstone label
    cmp qword [%1 + DictEntry.key], 0
    jne %%occupied
    cmp qword [%1 + DictEntry.hash], ENTRY_TOMBSTONE_HASH
    je %3
    jmp %2
%%occupied:
%endmacro

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
    .co_firstlineno:    resd 1  ; +112: line of the `def`/module start
    .co_pad0:           resd 1  ; +116: (alignment)
    .co_linetable:      resq 1  ; +120: ptr to bytes (PEP 626 table) or NULL
    .co_code:                   ; +128: bytecode (variable)
endstruc

; The compiler self-modifies nothing, but the *interpreter* does: op_load_global
; writes eight bytes of inline cache at [rbx+2..rbx+7] and op_compare_op reads
; byte [rbx+2].  Either can run on the last instruction of a code object, so the
; bytecode allocation carries a zeroed tail so those reads and writes stay
; inside it.  co_code_len still records the true length.
CODE_TAIL_PAD equ 8

; Arguments to code_new().  There are too many fields for the SysV register set,
; so they arrive in one struct.  code_new STEALS every object reference in here;
; build into a zeroed CodeSpec on the stack and call code_spec_clear() to
; release the lot on any error path.
struc CodeSpec
    .code_bytes:      resq 1  ; +0:  const uint8_t* (borrowed; copied inline)
    .code_len:        resq 1  ; +8:  length in bytes
    .consts:          resq 1  ; +16: PyTupleObject*        (stolen)
    .names:           resq 1  ; +24: PyTupleObject*        (stolen)
    .localsplusnames: resq 1  ; +32: PyTupleObject*        (stolen)
    .localspluskinds: resq 1  ; +40: PyBytesObject* or 0   (stolen)
    .filename:        resq 1  ; +48: PyStrObject*          (stolen)
    .name:            resq 1  ; +56: PyStrObject*          (stolen)
    .qualname:        resq 1  ; +64: PyStrObject*          (stolen)
    .exceptiontable:  resq 1  ; +72: PyBytesObject* or 0   (stolen)
    .linetable:       resq 1  ; +80: PyBytesObject* or 0   (stolen)
    .argcount:        resd 1  ; +88
    .posonlyargcount: resd 1  ; +92
    .kwonlyargcount:  resd 1  ; +96
    .nlocals:         resd 1  ; +100: len(varnames) -- the true count, not nlocalsplus
    .stacksize:       resd 1  ; +104
    .flags:           resd 1  ; +108
    .firstlineno:     resd 1  ; +112
    .pad0:            resd 1  ; +116
endstruc

; Module object
struc PyModuleObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .mod_name:    resq 1  ; +16: ptr to str
    .mod_dict:    resq 1  ; +24: ptr to module dict
endstruc

; Member descriptor object (for __slots__)
; PEP 585 generic alias: list[int].  _collections_abc takes GenericAlias from
; `type(list[int])`, and every annotation-bearing stdlib module needs it.
struc PyGenericAliasObject
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .ga_origin: resq 1  ; +16: the type being subscripted (owned)
    .ga_args:   resq 1  ; +24: the argument, a tuple if it was one (owned)
endstruc

; A getset descriptor: a named pair of C accessors living in a type's dict.
; CPython exposes func.__code__, code.co_name and friends this way, and
; types.py takes GetSetDescriptorType from one of them.
struc PyGetSetDescrObject
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .gs_get:    resq 1  ; +16: C getter(self) -> Value, or 0
    .gs_set:    resq 1  ; +24: C setter(self, value) -> int, or 0
    .gs_name:   resq 1  ; +32: ptr to the name (PyStrObject*, owned)
endstruc

; types.SimpleNamespace: attributes are the entries of a dict.
struc PySimpleNamespaceObject
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .ns_dict:   resq 1  ; +16: the attribute dict (owned)
endstruc

; Read-only view of a mapping.  type.__dict__ is one of these in CPython,
; and types.py takes MappingProxyType from it.
struc PyMappingProxyObject
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .mp_mapping: resq 1 ; +16: the wrapped dict (owned)
endstruc

struc PyMemberDescrObject
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .md_offset: resq 1  ; +16: byte offset in instance for slot payload
    .md_name:   resq 1  ; +24: ptr to slot name (PyStrObject*, owned)
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
    .int_value:   resq 1  ; +24: the wrapped integer, as a Value
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
    .ob_refcnt:  resq 1
    .ob_type:    resq 1
    .it_dict:    resq 1    ; +16: ptr to dict
    .it_index:   resq 1    ; +24: current entry index
    .it_kind:    resq 1    ; +32: 0=keys, 1=values, 2=items
    .it_version: resq 1    ; +40: dk_version snapshot for mutation detection
endstruc

struc PyDictViewObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .dv_dict:   resq 1    ; +16: ptr to dict (INCREF'd)
    .dv_kind:   resq 1    ; +24: 0=keys, 1=values, 2=items
endstruc

struc PyRangeObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .start:     resq 1    ; +16: start value (raw i64)
    .stop:      resq 1    ; +24: stop value (raw i64)
    .step:      resq 1    ; +32: step value (raw i64)
endstruc

struc PyRangeIterObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .it_current: resq 1   ; +16: current value
    .it_stop:    resq 1   ; +24: stop value
    .it_step:    resq 1   ; +32: step value
endstruc

struc PyBytesIterObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .it_seq:    resq 1    ; +16: ptr to bytes obj
    .it_index:  resq 1    ; +24: current index
endstruc

; Slice object
struc PySliceObject
    .ob_refcnt:   resq 1
    .ob_type:     resq 1
    .start:       resq 1    ; +16: Value
    .stop:        resq 1    ; +24: Value
    .step:        resq 1    ; +32: Value
endstruc

; Cell object (for closures)
struc PyCellObject
    .ob_refcnt: resq 1
    .ob_type:   resq 1
    .ob_ref:    resq 1    ; +16: contained Value (0 if empty)
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
    .gi_return_value: resq 1 ; +48: return Value (for StopIteration.value)
endstruc

; File object (for sys.stdout, sys.stderr, sys.stdin)
struc PyFileObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .file_fd:     resq 1  ; +16: OS file descriptor
    .file_name:   resq 1  ; +24: name string ('<stdout>' etc)
    .file_mode:   resq 1  ; +32: mode string ('w', 'r')
endstruc

; _io.FileIO.  The first three fields are PyInstanceObject's, because FileIO
; is a heaptype built with type_from_parts over _RawIOBase: instance_new
; allocates tp_basicsize and zero-fills, so the fields below start where the
; base's layout ends and cost nothing but a patched tp_basicsize.
struc PyFileIOObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .inst_dict:   resq 1  ; +16: the tp_dictoffset _RawIOBase hands down
    .fio_fd:      resq 1  ; +24: the descriptor, meaningless unless FIO_OPEN
    .fio_flags:   resq 1  ; +32
    .fio_name:    resq 1  ; +40: the name as given, a Value -- an adopted
                          ;      descriptor makes it an int, not a str
    .fio_blksize: resq 1  ; +48: st_blksize, what readall() grows by
    .fio_mode:    resq 1  ; +56: the mode string, strong
endstruc

FIO_OPEN      equ 0x01
FIO_READABLE  equ 0x02
FIO_WRITABLE  equ 0x04
FIO_APPENDING equ 0x08
FIO_CREATED   equ 0x10
FIO_CLOSEFD   equ 0x20
; Whether seekability has been asked yet, and the answer.  One lseek settles
; it, and a pipe must answer False rather than raising every time.
FIO_SEEK_KNOWN equ 0x40
FIO_SEEKABLE   equ 0x80

; Async task object (for event loop)
struc AsyncTask
    .ob_refcnt:    resq 1  ; +0
    .ob_type:      resq 1  ; +8
    .coro:         resq 1  ; +16: PyGenObject* (coroutine)
    .result:       resq 1  ; +24: result Value
    .pad_result:   resq 1  ; +32: (was result_tag)
    .exception:    resq 1  ; +40: exception or NULL
    ; +48 must line up with PyGenObject.gi_return_value: op_send reads that
    ; offset on whatever object a coroutine yielded, a task included.
    .send_value:   resq 1  ; +48: Value to send on next resume
    .pad_send:     resq 1  ; +56: (was send_tag)
    .done:         resd 1  ; +64: 0=pending, 1=done
    .cancelling:   resd 1  ; +68: 1=cancel requested
    .n_waiters:    resd 1  ; +72: number of waiters
    .pad:          resd 1  ; +76
    .waiters:      resq 1  ; +80: ptr to AsyncTask*[] array
    .waiters_cap:  resd 1  ; +88: waiters array capacity
    .pad2:         resd 1  ; +92
    .next:         resq 1  ; +96: ready queue linkage
    .ts_sec:       resq 1  ; +104: timespec seconds (for uring timeout SQE)
    .ts_nsec:      resq 1  ; +112: timespec nanoseconds
endstruc

; Sleep awaitable object (for asyncio.sleep)
struc SleepAwaitable
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .delay_ns:  resq 1  ; +16
    .yielded:   resd 1  ; +24: 0=not yet, 1=done
    .pad:       resd 1  ; +28
endstruc

; WaitFor awaitable object (for asyncio.wait_for)
struc WaitForAwaitable
    .ob_refcnt:       resq 1  ; +0
    .ob_type:         resq 1  ; +8
    .inner_task:      resq 1  ; +16: AsyncTask* being waited on
    .timeout_ns:      resq 1  ; +24: timeout in nanoseconds
    .state:           resd 1  ; +32: 0=init, 1=yielded, 2=done
    .pad:             resd 1  ; +36
    .outer_task:      resq 1  ; +40: AsyncTask* (the waiter, set by task_step)
    .gi_return_value: resq 1  ; +48: result Value (matches PyGenObject offset)
endstruc

; Async stream reader
struc AsyncStreamReader
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .fd:        resd 1  ; +16: socket file descriptor
    .eof:       resd 1  ; +20: 1 = EOF reached
endstruc

; Async stream writer
struc AsyncStreamWriter
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .fd:        resd 1  ; +16: socket file descriptor
    .closed:    resd 1  ; +20: 1 = closed
endstruc

; ReadAwaitable — awaitable for stream.read(n)
struc ReadAwaitable
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .fd:        resd 1  ; +16: socket fd
    .nbytes:    resd 1  ; +20: bytes to read
    .yielded:   resd 1  ; +24: 0=not yet, 1=done
    .pad:       resd 1  ; +28
endstruc

; WriteAwaitable — awaitable for stream.write(data)
struc WriteAwaitable
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .fd:        resd 1  ; +16: socket fd
    .pad1:      resd 1  ; +20
    .data:      resq 1  ; +24: string Value (INCREF'd)
    .yielded:   resd 1  ; +32: 0=not yet, 1=done
    .pad2:      resd 1  ; +36
endstruc

; DrainAwaitable — awaitable for stream.drain()
struc DrainAwaitable
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .fd:        resd 1  ; +16: socket fd
    .yielded:   resd 1  ; +20: 0=not yet, 1=done
endstruc

; ConnectAwaitable — awaitable for open_connection
struc ConnectAwaitable
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .fd:        resd 1  ; +16: socket fd (already connected)
    .yielded:   resd 1  ; +20: 0=not yet, 1=done
endstruc

; AcceptAwaitable — awaitable for start_server (non-blocking accept)
struc AcceptAwaitable
    .ob_refcnt:  resq 1  ; +0
    .ob_type:    resq 1  ; +8
    .listen_fd:  resd 1  ; +16: listening socket fd
    .yielded:    resd 1  ; +20: 0=not yet, 1=done
endstruc

; AsyncGenASend — wrapper returned by async_gen.__anext__()
; SEND/tp_iternext on this object resumes the async gen.
; Layout must keep gi_return_value at +48, gi_return_tag at +56
; because op_send reads these offsets from the receiver on exhaustion.
;; The box CALL_INTRINSIC_1 INTRINSIC_ASYNC_GEN_WRAP puts around a value an
;; async generator yields.  An async generator's frame yields for two different
;; reasons -- an item for its consumer, and an `await` passing through to the
;; event loop -- and this box is the only thing that tells them apart.
struc AsyncGenWrapped
    .ob_refcnt:     resq 1  ; +0
    .ob_type:       resq 1  ; +8
    .agw_value:     resq 1  ; +16: the wrapped Value, owned
endstruc

struc AsyncGenASend
    .ob_refcnt:     resq 1  ; +0
    .ob_type:       resq 1  ; +8
    .ags_gen:       resq 1  ; +16: pointer to async generator (INCREF'd)
    .ags_state:     resd 1  ; +24: 0=initial, 1=yielded, 2=closed
    .pad0:          resd 1  ; +28
    .pad1:          resq 1  ; +32
    .pad2:          resq 1  ; +40
    .gi_return_value: resq 1 ; +48: yielded Value (for SEND exhaustion path)
endstruc

; Sentinel values

;; ============================================================================
;; Type objects: PyTypeObject and the three method tables
;; (was include/types.inc)
;; ============================================================================

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
    ; Byte offset of the instance __dict__ within an instance, or 0 for a
    ; type whose instances have none.  Only ever read for a heaptype, which
    ; is gc_alloc'd and zero-filled, so a static table that predates this
    ; field cannot be misread.
    .tp_dictoffset:  resq 1  ; +208
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
    ; The @ operator.  These were missing entirely, and binary_op_offsets
    ; mapped NB_MATRIX_MULTIPLY to offset 0 -- which is nb_add -- so 3 @ 4
    ; silently returned 7.
    .nb_matmul:        resq 1  ; +264
    .nb_imatmul:       resq 1  ; +272
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

; Linux x86-64 `struct stat`, as the stat/lstat/fstat syscalls fill it.
; STAT_SIZE and a couple of the offsets were copied privately into
; src/marshal.asm, src/compiler/compile.asm and inline into
; src/builtins_obj.asm; posix reads nearly all of them.
;
; st_uid and st_gid are 32-bit and st_mode is too -- a 64-bit read of any of
; the three silently ORs in its neighbour, which is the trap CLAUDE.md names.
struc StatBuf
    .st_dev:        resq 1  ; +0
    .st_ino:        resq 1  ; +8
    .st_nlink:      resq 1  ; +16
    .st_mode:       resd 1  ; +24  (32-bit)
    .st_uid:        resd 1  ; +28  (32-bit)
    .st_gid:        resd 1  ; +32  (32-bit)
    .st_pad0:       resd 1  ; +36
    .st_rdev:       resq 1  ; +40
    .st_size:       resq 1  ; +48
    .st_blksize:    resq 1  ; +56
    .st_blocks:     resq 1  ; +64
    .st_atime:      resq 1  ; +72
    .st_atime_ns:   resq 1  ; +80
    .st_mtime:      resq 1  ; +88
    .st_mtime_ns:   resq 1  ; +96
    .st_ctime:      resq 1  ; +104
    .st_ctime_ns:   resq 1  ; +112
    .st_unused:     resq 3  ; +120
endstruc                    ; 144 bytes

; open() flags, as <fcntl.h> has them on Linux.  builtin_open_fn hardcoded
; 0x241, 0x441 and 0xc1 inline.
O_RDONLY    equ 0o0
O_WRONLY    equ 0o1
O_RDWR      equ 0o2
O_ACCMODE   equ 0o3
O_CREAT     equ 0o100
O_EXCL      equ 0o200
O_NOCTTY    equ 0o400
O_TRUNC     equ 0o1000
O_APPEND    equ 0o2000
O_NONBLOCK  equ 0o4000
O_DIRECTORY equ 0o200000
O_NOFOLLOW  equ 0o400000
O_CLOEXEC   equ 0o2000000

; access() modes
F_OK equ 0
X_OK equ 1
W_OK equ 2
R_OK equ 4

; lseek() whence
SEEK_SET equ 0
SEEK_CUR equ 1
SEEK_END equ 2

; st_mode's file-type field, and the mask that selects it.
S_IFMT   equ 0o170000
S_IFSOCK equ 0o140000
S_IFLNK  equ 0o120000
S_IFREG  equ 0o100000
S_IFBLK  equ 0o060000
S_IFDIR  equ 0o040000
S_IFCHR  equ 0o020000
S_IFIFO  equ 0o010000

; linux_dirent64, the record getdents64 writes.  d_reclen is the stride to the
; next record and is TWO bytes: a 64-bit read of it ORs in d_type and five
; bytes of the name, which is exactly the multi-gigabyte-memcpy bug CLAUDE.md
; records.  lint.py derives widths from `resd` and so does not catch a `resw`.
struc LinuxDirent64
    .d_ino:    resq 1   ; +0
    .d_off:    resq 1   ; +8
    .d_reclen: resw 1   ; +16  (16-bit -- the stride)
    .d_type:   resb 1   ; +18
    .d_name:            ; +19, NUL-terminated
endstruc

; Linux's `struct utsname`: six fixed 65-byte NUL-terminated fields.  os.uname()
; exposes the first five; the sixth, domainname, is a GNU extension CPython
; does not report.
UTSNAME_FIELD equ 65
UTSNAME_SIZE  equ 65 * 6

; ioctl request numbers
TCGETS equ 0x5401

; fcntl commands, for the close-on-exec flag os.set_inheritable moves
F_GETFD    equ 1
F_SETFD    equ 2
FD_CLOEXEC equ 1

; d_type values
DT_UNKNOWN equ 0
DT_FIFO    equ 1
DT_CHR     equ 2
DT_DIR     equ 4
DT_BLK     equ 6
DT_REG     equ 8
DT_LNK     equ 10
DT_SOCK    equ 12

; waitpid() options, and the encoding of the status word it fills in.
WNOHANG   equ 1
WUNTRACED equ 2
WCONTINUED equ 8

; A struct sequence: a tuple whose entries are also named (os.stat_result,
; sys.version_info).  The descriptor hangs off the end of the type -- one
; qword just past PyTypeObject -- so tp_getattr, tp_repr and tp_dealloc can
; each find their own from the instance's type instead of needing a thunk.
struc StructSeqField
    .name:  resq 1          ; +0: NUL-terminated C string
    .index: resq 1          ; +8: below n_in_sequence => ob_item[index];
endstruc                    ;     at or above it => the (index - n) tail slot

struc StructSeqDesc
    .n_in_sequence: resq 1  ; +0: the tuple's length, NOT the field count
    .n_fields:      resq 1  ; +8: including the named-only tail
    .fields:        resq 1  ; +16: -> StructSeqField[n_fields]
endstruc

; The descriptor's offset within a struct-sequence type object.
STRUCTSEQ_DESC equ PyTypeObject_size

; One row of builtin_module_table (src/modtable.asm): a module compiled into
; the interpreter.  Both sys.modules and sys.builtin_module_names are built
; from it, so the two cannot drift apart again.
struc BuiltinModule
    .name:      resq 1      ; +0: NUL-terminated C string
    .create_fn: resq 1      ; +8: PyObject *(*)(void), or 0 when import_init
endstruc                    ;     wires that module some other way

BUILTIN_MODULE_COUNT equ 10

; Type flags
TYPE_FLAG_HEAPTYPE     equ 0x0001
TYPE_FLAG_BASETYPE     equ 0x0002
TYPE_FLAG_HAVE_GC      equ 0x0004
TYPE_FLAG_INT_SUBCLASS equ 0x0100
TYPE_FLAG_STR_SUBCLASS equ 0x0200
TYPE_FLAG_LIST_SUBCLASS equ 0x0400
TYPE_FLAG_TUPLE_SUBCLASS equ 0x0800
TYPE_FLAG_DICT_SUBCLASS equ 0x1000
TYPE_FLAG_SET_SUBCLASS equ 0x4000   ; 0x2000 is TYPE_FLAG_HAS_SLOTS
TYPE_FLAG_HAS_SLOTS     equ 0x2000
; Instances of this type are classes.  "Is this object a class?" used to be
; asked as `ob_type is user_type_metatype`, which is false for a class built by
; a metaclass of its own -- so a classmethod reached through such a class bound
; the metaclass instead of the class.
TYPE_FLAG_METATYPE      equ 0x8000
; float and complex keep their value inline, exactly as int and str do, so a
; subclass cannot come from instance_new either -- it has to be built by the
; base's own constructor, and every conversion helper has to recognise it.
; The low nibble range is full; these two start the next one.
TYPE_FLAG_FLOAT_SUBCLASS   equ 0x10000
TYPE_FLAG_COMPLEX_SUBCLASS equ 0x20000

; Comparison opcodes (used by tp_richcompare)
PY_LT equ 0
PY_LE equ 1
PY_EQ equ 2
PY_NE equ 3
PY_GT equ 4
PY_GE equ 5

;; ============================================================================
;; PyFrame
;; (was include/frame.inc)
;; ============================================================================

; Execution frame layout
struc PyFrame
    .prev_frame:    resq 1  ; +0:  ptr to previous frame
    .code:          resq 1  ; +8:  ptr to code object
    .globals:       resq 1  ; +16: ptr to globals dict
    .builtins:      resq 1  ; +24: ptr to builtins dict
    .locals:        resq 1  ; +32: ptr to locals dict (or NULL for fast locals)
    .instr_ptr:     resq 1  ; +40: saved bytecode pointer
    .stack_ptr:     resq 1  ; +48: saved value stack pointer
    .stack_base:    resq 1  ; +56: ptr to bottom of the value stack
    .return_offset: resd 1  ; +64: return offset
    .nlocalsplus:   resd 1  ; +68: number of locals + cells + frees
    .func_obj:      resq 1  ; +72: ptr to function object (for closures)
    .localsplus:            ; +80: Value array (variable size)
endstruc

FRAME_HEADER_SIZE equ PyFrame.localsplus

;; ============================================================================
;; Exceptions, tracebacks, and the fatal error codes
;; (was include/errcodes.inc)
;; ============================================================================

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
EXC_UNICODE_ERROR          equ 23
EXC_BASE_EXCEPTION_GROUP   equ 24
EXC_EXCEPTION_GROUP        equ 25
EXC_CANCELLED_ERROR        equ 26
EXC_STOP_ASYNC_ITERATION   equ 27
EXC_TIMEOUT_ERROR          equ 28

; Exception object
struc PyExceptionObject
    .ob_refcnt:     resq 1  ; +0
    .ob_type:       resq 1  ; +8
    .exc_type:      resq 1  ; +16: ptr to exception type object
    .exc_value:     resq 1  ; +24: value, as a Value
    .exc_tb:        resq 1  ; +32: ptr to traceback or NULL
    .exc_context:   resq 1  ; +40: ptr to __context__ or NULL
    .exc_cause:     resq 1  ; +48: ptr to __cause__ or NULL
    .exc_args:      resq 1  ; +56: ptr to args tuple or NULL
    .exc_dict:      resq 1  ; +64: ptr to instance dict or NULL (for custom attrs)
    .exc_suppress:  resq 1  ; +72: __suppress_context__ (set by `raise .. from`)
endstruc

; Exception group object (extends PyExceptionObject with eg_exceptions)
struc PyExceptionGroupObject
    .ob_refcnt:     resq 1  ; +0
    .ob_type:       resq 1  ; +8
    .exc_type:      resq 1  ; +16
    .exc_value:     resq 1  ; +24 (message, as a Value)
    .exc_tb:        resq 1  ; +32
    .exc_context:   resq 1  ; +40
    .exc_cause:     resq 1  ; +48
    .exc_args:      resq 1  ; +56
    .exc_dict:      resq 1  ; +64: ptr to instance dict or NULL
    .exc_suppress:  resq 1  ; +72: __suppress_context__
    .eg_exceptions: resq 1  ; +72: tuple of sub-exceptions
endstruc

; Traceback object
struc PyTracebackObject
    .ob_refcnt: resq 1  ; +0
    .ob_type:   resq 1  ; +8
    .tb_next:   resq 1  ; +16: ptr to next traceback or NULL
    .tb_lineno: resq 1  ; +24: line number (i64), 0 when unknown
    .tb_code:   resq 1  ; +32: owned ref to the PyCodeObject, or NULL
    .tb_lasti:  resq 1  ; +40: instruction offset in code units
endstruc

; Fatal error codes (non-Python, internal)

;; ============================================================================
;; Builtin function ids and PyBuiltinObject
;; (was include/builtins.inc)
;; ============================================================================

; Builtin function identifiers

; Builtin C function object
struc PyBuiltinObject
    .ob_refcnt:   resq 1  ; +0
    .ob_type:     resq 1  ; +8
    .func_id:     resq 1  ; +16: builtin function ID
    .func_name:   resq 1  ; +24: ptr to name string
    .func_ptr:    resq 1  ; +32: ptr to C implementation
    .min_args:    resq 1  ; +40: minimum nargs (including self), 0 = no check
    .max_args:    resq 1  ; +48: maximum nargs, -1 = no max check
endstruc

;; ============================================================================
;; The GC header, and the traverse helpers
;; (was include/gc.inc)
;; ============================================================================

; Cycle-collecting GC, CPython 3.12 compatible

; PyGC_Head — prepended BEFORE PyObject in GC-tracked allocations
; User code gets pointer at alloc_ptr + GC_HEAD_SIZE
struc PyGC_Head
    .gc_next: resq 1       ; +0: doubly-linked list next (ptr to PyGC_Head)
    .gc_prev: resq 1       ; +8: prev ptr; low 2 bits = state flags during collection
endstruc

GC_HEAD_SIZE equ PyGC_Head_size   ; 16

; GC state bits stored in low 2 bits of gc_prev
GC_PREV_MASK_COLLECTING equ 1     ; object is in unreachable set
GC_PREV_SHIFT           equ 2     ; gc_refs stored in bits 2+
GC_PREV_MASK            equ ~3    ; mask to extract prev pointer (clear low 2 bits)

; VISIT_V — call visit callback on a NaN-boxed Value if it holds a pointer
; r14 must be loaded with the visit callback function pointer before use
%macro VISIT_V 2                  ; %1 = Value reg, %2 = scratch (clobbered)
    lea %2, [%1 - 1]
    cmp %2, [rel v_ptr_max_m1]
    ja %%skip                     ; immediate or NULL — not a reference
    mov rdi, %1
    call r14                      ; visit callback(obj)
%%skip:
%endmacro

; VISIT_PTR — call visit callback on a plain heap pointer (NULL-safe)
%macro VISIT_PTR 1                ; %1 = ptr_reg
    test %1, %1
    jz %%skip
    mov rdi, %1
    call r14
%%skip:
%endmacro

%endif ; OBJECT_INC
