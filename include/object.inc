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
NULL_OBJ equ 0

%endif ; OBJECT_INC
