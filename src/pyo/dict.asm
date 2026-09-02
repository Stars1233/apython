; pyo/dict.asm - Dict type implementation
; Open-addressing hash table with linear probing

%include "macros.inc"
%include "object.inc"

extern obj_richcompare_bool
extern eval_exception_unwind
extern bool_true
extern bool_false
extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_hash
extern obj_decref
extern obj_dealloc
extern str_type
extern fatal_error
extern raise_exception
extern obj_incref
extern type_type
extern tuple_type

; Initial capacity (must be power of 2)
; DICT_INIT_CAP now lives in object.inc, shared with the subclass path

; Tombstone marker for deleted dict entries.
; When an entry is deleted, key_tag is set to this value so that
; linear probing continues past it (instead of stopping as at empty slots).
; Must never match a valid tag value.

;; ============================================================================
;; dict_new() -> PyDictObject*
;; Allocate a new empty dict with initial capacity 8
;; ============================================================================
DEF_FUNC dict_new
    push rbx
    ; Header
    mov edi, PyDictObject_size
    lea rsi, [rel dict_type]
    call gc_alloc
    mov rbx, rax
    mov qword [rbx + PyDictObject.ob_size], 0
    mov qword [rbx + PyDictObject.capacity], DICT_INIT_CAP
    mov qword [rbx + PyDictObject.dk_version], 1
    mov qword [rbx + PyDictObject.dk_tombstones], 0
    mov qword [rbx + PyDictObject.dk_nentries], 0

    mov rdi, rbx
    mov rsi, DICT_INIT_CAP
    call dict_alloc_tables

    mov rdi, rbx
    call gc_track
    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC dict_new

;; ============================================================================
;; dict_alloc_tables(rdi = dict, rsi = capacity)
;; Allocates the dense entry array (zeroed, so the unused tail reads as empty)
;; and the sparse index array (all DICT_IX_EMPTY).  Sets .capacity.
;; ============================================================================
DEF_FUNC dict_alloc_tables
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov [rbx + PyDictObject.capacity], r12

    ; dense entries, zero-filled
    mov rdi, r12
    imul rdi, rdi, DICT_ENTRY_SIZE
    call ap_malloc
    mov [rbx + PyDictObject.entries], rax
    mov rdi, rax
    mov rcx, r12
    imul rcx, rcx, DICT_ENTRY_SIZE / 8
    xor eax, eax
    rep stosq

    ; sparse indices, all empty
    lea rdi, [r12 * 8]
    call ap_malloc
    mov [rbx + PyDictObject.dk_indices], rax
    mov rdi, rax
    mov rcx, r12
    mov rax, DICT_IX_EMPTY
    rep stosq

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_alloc_tables

;; ============================================================================
;; dict_type_call(PyTypeObject *type, PyObject **args, int64_t nargs) -> PyDictObject*
;; Constructor: dict() or dict(mapping)
;; ============================================================================
extern kw_names_pending
extern dict_method_update

DEF_FUNC dict_type_call
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov rbx, rsi               ; args
    mov r12, rdx               ; nargs

    ; Check for keyword arguments
    mov r14, [rel kw_names_pending]
    mov qword [rel kw_names_pending], 0  ; clear immediately

    ; Determine positional arg count
    xor r13d, r13d             ; r13 = n_pos = nargs
    mov r13, r12
    test r14, r14
    jz .dtc_no_kw
    mov rax, [r14 + PyTupleObject.ob_size]
    sub r13, rax               ; r13 = n_pos = nargs - n_kw

.dtc_no_kw:
    ; dict() with no pos args (may have kwargs)
    test r13, r13
    jz .dtc_no_pos

    ; dict(arg) - one positional arg (may also have kwargs)
    cmp r13, 1
    jne .dtc_error

    ; Check if arg is a dict
    mov rdi, [rbx]             ; args[0]
    V_TEST_PTR rdi, rax
    ja .dtc_try_iterable
    mov rax, [rdi + PyObject.ob_type]
    REQUIRE_DICT_TYPE rax, rcx, .dtc_try_iterable

    ; dict(other_dict) → create new dict and copy entries
    push rdi                   ; save source dict
    call dict_new
    mov r15, rax               ; r15 = new dict
    pop rdi                    ; rdi = source dict

    ; Copy all entries from source
    mov r8, [rdi + PyDictObject.capacity]
    xor ecx, ecx
.dtc_copy_loop:
    cmp rcx, r8
    jge .dtc_copy_done
    imul rax, rcx, DICT_ENTRY_SIZE
    add rax, [rdi + PyDictObject.entries]
    cmp qword [rax + DictEntry.key], 0   ; occupied?
    je .dtc_copy_next
    push rcx
    push r8
    push rdi
    mov rdi, r15               ; new dict
    mov rsi, [rax + DictEntry.key]
    mov rdx, [rax + DictEntry.value]
    call dict_set
    pop rdi
    pop r8
    pop rcx
.dtc_copy_next:
    inc rcx
    jmp .dtc_copy_loop
.dtc_copy_done:
    ; Fall through to add kwargs if present
    jmp .dtc_add_kwargs

.dtc_try_iterable:
    ; Not a dict.  "A mapping, or an iterable of pairs" is exactly what
    ; dict.update means, so it is decided in one place: build an empty dict and
    ; update it.  Doing it again here is how the constructor came to accept
    ; pairs and reject a mappingproxy while update accepted both.
    call dict_new
    mov r15, rax
    sub rsp, 24
    mov [rsp], r15
    mov rax, [rbx]                      ; args[0]
    mov [rsp + 8], rax
    mov rdi, rsp
    mov esi, 2
    call dict_method_update             ; kw_names_pending is already cleared
    add rsp, 24
    test rax, rax
    jz .dtc_error                       ; update left its exception pending
    mov rdi, rax
    DECREF_V rdi, rsi                   ; the None it returns
    jmp .dtc_add_kwargs

.dtc_iter_type_error:
    ; DECREF iterator and raise TypeError
    mov rdi, r13
    call obj_decref
    jmp .dtc_error

.dtc_error_pop:
    add rsp, 8
    jmp .dtc_error

.dtc_no_pos:
    ; No positional args — create empty dict (kwargs will be added below)
    call dict_new
    mov r15, rax

.dtc_add_kwargs:
    ; Add keyword arguments if present
    test r14, r14
    jz .dtc_return_dict

    ; r14 = kw_names tuple, rbx = args, r13 was n_pos (now reuse)
    ; kwargs start at args[n_pos] — reload n_pos
    mov rax, r12               ; total nargs
    mov rcx, [r14 + PyTupleObject.ob_size]
    sub rax, rcx               ; rax = n_pos
    mov r13, rcx               ; r13 = n_kw
    mov rcx, rax               ; rcx = n_pos (index into args)

    ; kw_names.ob_item has the key strings, args[n_pos + i] has values
    mov rax, [r14 + PyTupleObject.ob_item]      ; keys payload array
    xor r8d, r8d              ; kw index
.dtc_kw_loop:
    cmp r8, r13
    jge .dtc_return_dict

    ; Calculate arg position: args[(n_pos + r8)]
    push r8
    push rcx
    push rax
    push rdx

    ; Get key from kw_names
    mov rsi, [rax + r8*8]         ; key payload (string)
    V_UNPACK rsi, r8

    ; Get value from args
    lea r9, [rcx + r8]            ; wait, need original r8 (kw index)
    ; Recalculate: value is at args[n_pos + kw_index]
    pop rdx
    pop rax
    pop rcx
    pop r8

    push r8
    push rcx
    push rax
    push rdx

    ; key from kw_names tuple items
    mov r9, [r14 + PyTupleObject.ob_item]
    mov rsi, [r9 + r8*8]         ; key Value

    ; value from args: index = n_pos + kw_index
    add rcx, r8                   ; rcx = n_pos + kw_index
    shl rcx, 3                    ; one Value per arg slot
    mov rdx, [rbx + rcx]         ; value Value

    ; dict_set(dict, key Value, value Value)
    mov rdi, r15
    call dict_set

    pop rdx
    pop rax
    pop rcx
    pop r8
    inc r8
    jmp .dtc_kw_loop

.dtc_return_dict:
    mov rax, r15
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.dtc_error:
    extern exc_TypeError_type
    RAISE exc_TypeError_type, "dict() argument must be a mapping or iterable"
END_FUNC dict_type_call

;; ============================================================================
;; dict_keys_equal(rdi=a_key, rsi=b_key, edx=a_tag, ecx=b_tag) -> int (1=equal, 0=not)
;;
;; Was identity, then a hand-rolled cross-type numeric compare, then a
;; strcmp, then tp_richcompare -- most of PyObject_RichCompareBool, with the
;; reflected call missing.  So a key whose __eq__ lives on the *lookup* side
;; rather than the stored side was never found.
;; ============================================================================
DEF_FUNC_LOCAL dict_keys_equal
    ; Both arguments are Values.
    mov edx, PY_EQ
    call obj_richcompare_bool
    cmp eax, -1
    je .dke_error
    leave
    ret

.dke_error:
    ; The probe loop has no error channel; the exception is already pending.
    leave
    jmp eval_exception_unwind
END_FUNC dict_keys_equal

;; ============================================================================
;; dict_get(rdi=dict, rsi=key Value) -> rax = value Value, or 0 when absent
;; Linear probing lookup
;; ============================================================================
DEF_FUNC dict_get, 8
    push rbx
    push r12
    mov rbx, rdi                ; the dict; rdi does not survive the call
    call dict_lookup            ; rax = entries index or -1
    test rax, rax
    js .dg_miss
    mov rcx, [rbx + PyDictObject.entries]
    imul rax, rax, DICT_ENTRY_SIZE
    mov rax, [rcx + rax + DictEntry.value]
    pop r12
    pop rbx
    leave
    ret
.dg_miss:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_get

;; ============================================================================
;; dict_lookup(rdi = dict, rsi = key Value) -> rax = entries index, or -1
;;   rdx = the indices slot the key hashes to (where an insert would go, or
;;         the first dummy on the probe path), r8 = hash
;; The one probe loop; every read path goes through it.
;; ============================================================================
DL_DICT  equ 8
DL_KEY   equ 16
DL_HASH  equ 24
DL_MASK  equ 32
DL_SLOT  equ 40
DL_FREE  equ 48
DL_FRAME equ 64             ; + 3 pushes = 88, not 16-aligned
DEF_FUNC dict_lookup, DL_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - DL_DICT], rdi
    mov [rbp - DL_KEY], rsi

    mov rdi, rsi
    call obj_hash
    mov [rbp - DL_HASH], rax

    mov rbx, [rbp - DL_DICT]
    mov rcx, [rbx + PyDictObject.capacity]
    dec rcx
    mov [rbp - DL_MASK], rcx
    and rax, rcx
    mov [rbp - DL_SLOT], rax
    mov qword [rbp - DL_FREE], -1
    xor r13d, r13d              ; probes

.dl_probe:
    cmp r13, [rbx + PyDictObject.capacity]
    jge .dl_miss
    mov rax, [rbx + PyDictObject.dk_indices]
    mov rcx, [rbp - DL_SLOT]
    mov r12, [rax + rcx*8]      ; the index stored here
    cmp r12, DICT_IX_EMPTY
    je .dl_miss
    cmp r12, DICT_IX_DUMMY
    jne .dl_occupied
    ; remember the first reusable slot for an insert
    cmp qword [rbp - DL_FREE], -1
    jne .dl_next
    mov [rbp - DL_FREE], rcx
    jmp .dl_next

.dl_occupied:
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r12, DICT_ENTRY_SIZE
    add rax, rcx
    mov rcx, [rbp - DL_HASH]
    cmp rcx, [rax + DictEntry.hash]
    jne .dl_next
    mov rdi, [rax + DictEntry.key]
    mov rsi, [rbp - DL_KEY]
    call dict_keys_equal
    test eax, eax
    jz .dl_next
    mov rax, r12                ; found: the entries index
    jmp .dl_out

.dl_next:
    mov rcx, [rbp - DL_SLOT]
    inc rcx
    and rcx, [rbp - DL_MASK]
    mov [rbp - DL_SLOT], rcx
    inc r13
    jmp .dl_probe

.dl_miss:
    ; An insert goes into the first dummy seen, else this empty slot.
    mov rcx, [rbp - DL_FREE]
    cmp rcx, -1
    jne .dl_have_free
    mov rcx, [rbp - DL_SLOT]
.dl_have_free:
    mov [rbp - DL_SLOT], rcx
    mov rax, -1

.dl_out:
    mov rdx, [rbp - DL_SLOT]
    mov r8, [rbp - DL_HASH]
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_lookup

;; ============================================================================
;; dict_get_index(rdi=dict, rsi=key, edx=key_tag) -> int64
;; Like dict_get but returns the slot index (for IC caching), -1 if not found.
;; ============================================================================
DEF_FUNC dict_get_index, 8
    ; The index into the *dense* array, which the LOAD_GLOBAL inline cache
    ; caches.  A dense index never moves except on a resize, and the cache is
    ; already guarded by dk_version, so it is strictly more stable than the
    ; hash slot this used to return.
    push rbx
    call dict_lookup
    pop rbx
    leave
    ret
END_FUNC dict_get_index

;; ============================================================================
;; dict_find_slot(rdi=dict, rsi=key, rdx=hash, rcx=key_tag)
;;   -> rax = entry ptr, rdx = 1 if existing key found, 0 if empty/tombstone slot
;; Internal helper used by dict_set.
;; Tombstone reuse: if no match found but a tombstone was seen, returns it
;; instead of the empty slot, so inserts reclaim deleted entries.
;; ============================================================================

;; ============================================================================
;; dict_resize(PyDictObject *dict)
;; Double capacity and rehash all entries
;; ============================================================================
DR_DICT  equ 8
DR_OLDE  equ 16
DR_OLDN  equ 24
DR_FRAME equ 32             ; + 3 pushes = 56, not 16-aligned
DEF_FUNC dict_resize, DR_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - DR_DICT], rbx
    mov rax, [rbx + PyDictObject.entries]
    mov [rbp - DR_OLDE], rax
    mov rax, [rbx + PyDictObject.dk_nentries]
    mov [rbp - DR_OLDN], rax

    ; Grow only when the live count warrants it; a table full of holes is
    ; compacted at the same capacity instead.
    mov r12, [rbx + PyDictObject.capacity]
    mov rdx, r12
    shr rdx, 1
    cmp [rbx + PyDictObject.ob_size], rdx
    jl .dr_same_cap
    shl r12, 1
.dr_same_cap:
    ; r12, not rcx: ap_free below is a call and rcx is caller-saved.
    mov rdi, [rbx + PyDictObject.dk_indices]
    call ap_free
    mov rdi, rbx
    mov rsi, r12
    call dict_alloc_tables

    ; Re-append the live entries in their existing order, dropping holes.
    mov qword [rbx + PyDictObject.dk_nentries], 0
    mov qword [rbx + PyDictObject.dk_tombstones], 0
    xor r12d, r12d              ; index into the old dense array
.dr_loop:
    cmp r12, [rbp - DR_OLDN]
    jge .dr_done
    mov rax, [rbp - DR_OLDE]
    imul rcx, r12, DICT_ENTRY_SIZE
    add rax, rcx
    cmp qword [rax + DictEntry.key], 0
    je .dr_next

    ; place it: hash is already known, so probe the fresh index array
    mov r13, [rax + DictEntry.hash]
    mov rcx, [rbx + PyDictObject.capacity]
    dec rcx
    mov rdx, r13
    and rdx, rcx                ; slot
.dr_probe:
    mov rsi, [rbx + PyDictObject.dk_indices]
    cmp qword [rsi + rdx*8], DICT_IX_EMPTY
    je .dr_place
    inc rdx
    and rdx, rcx
    jmp .dr_probe
.dr_place:
    mov rdi, [rbx + PyDictObject.dk_nentries]
    mov [rsi + rdx*8], rdi
    mov rsi, [rbx + PyDictObject.entries]
    imul rcx, rdi, DICT_ENTRY_SIZE
    add rsi, rcx
    mov rcx, [rax + DictEntry.hash]
    mov [rsi + DictEntry.hash], rcx
    mov rcx, [rax + DictEntry.key]
    mov [rsi + DictEntry.key], rcx
    mov rcx, [rax + DictEntry.value]
    mov [rsi + DictEntry.value], rcx
    inc qword [rbx + PyDictObject.dk_nentries]

.dr_next:
    inc r12
    jmp .dr_loop

.dr_done:
    mov rdi, [rbp - DR_OLDE]
    call ap_free
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_resize

;; ============================================================================
;; dict_set(rdi=dict, rsi=key Value, rdx=value Value)
;; Insert or update a key-value pair.
;; ============================================================================
DS_DICT  equ 8
DS_KEY   equ 16
DS_VAL   equ 24
DS_FRAME equ 32             ; + 3 pushes = 56, not 16-aligned
DEF_FUNC dict_set, DS_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - DS_DICT], rdi
    mov [rbp - DS_KEY], rsi
    mov [rbp - DS_VAL], rdx

    call dict_lookup            ; rax = index or -1, rdx = slot, r8 = hash
    mov rbx, [rbp - DS_DICT]
    test rax, rax
    js .ds_insert

    ; Update in place: the key keeps its position, as in CPython.
    mov rcx, [rbx + PyDictObject.entries]
    imul rax, rax, DICT_ENTRY_SIZE
    add rcx, rax
    mov rdi, [rcx + DictEntry.value]
    mov rsi, [rbp - DS_VAL]
    mov [rcx + DictEntry.value], rsi
    INCREF_V rsi, rax
    DECREF_V rdi, rax
    jmp .ds_bump

.ds_insert:
    mov r12, rdx                ; the indices slot to claim
    mov r13, r8                 ; hash
    ; Room for one more dense entry?
    mov rax, [rbx + PyDictObject.dk_nentries]
    inc rax
    mov rcx, [rbx + PyDictObject.capacity]
    mov rdx, rcx
    shr rdx, 2
    lea rdx, [rdx + rdx*2]      ; capacity * 3/4
    cmp rax, rdx
    jle .ds_have_room
    mov rdi, rbx
    call dict_resize
    ; the slot is stale after a rebuild; find it again
    mov rdi, rbx
    mov rsi, [rbp - DS_KEY]
    call dict_lookup
    mov r12, rdx
    mov r13, r8

.ds_have_room:
    mov rax, [rbx + PyDictObject.dk_nentries]
    mov rcx, [rbx + PyDictObject.entries]
    imul rdx, rax, DICT_ENTRY_SIZE
    add rcx, rdx
    mov [rcx + DictEntry.hash], r13
    mov rdx, [rbp - DS_KEY]
    mov [rcx + DictEntry.key], rdx
    INCREF_V rdx, rsi
    mov rdx, [rbp - DS_VAL]
    mov [rcx + DictEntry.value], rdx
    INCREF_V rdx, rsi

    ; point the sparse slot at it
    mov rcx, [rbx + PyDictObject.dk_indices]
    mov [rcx + r12*8], rax
    inc qword [rbx + PyDictObject.dk_nentries]
    inc qword [rbx + PyDictObject.ob_size]

.ds_bump:
    inc qword [rbx + PyDictObject.dk_version]
    jnz .ds_done
    mov qword [rbx + PyDictObject.dk_version], 1
.ds_done:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_set

;; ============================================================================
;; dict_dealloc(PyObject *self)
;; Free all entries, then free dict
;; ============================================================================
DEF_FUNC dict_dealloc
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r13, [rbx + PyDictObject.dk_nentries]
    mov r12, 0
.dde_loop:
    cmp r12, r13
    jge .dde_done
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r12, DICT_ENTRY_SIZE
    add rax, rcx
    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .dde_next
    push rax
    DECREF_V rdi, rsi
    pop rax
    mov rdi, [rax + DictEntry.value]
    DECREF_V rdi, rsi
.dde_next:
    inc r12
    jmp .dde_loop
.dde_done:
    mov rdi, [rbx + PyDictObject.entries]
    test rdi, rdi
    jz .dde_no_entries
    call ap_free
.dde_no_entries:
    mov rdi, [rbx + PyDictObject.dk_indices]
    test rdi, rdi
    jz .dde_no_idx
    call ap_free
.dde_no_idx:
    mov rdi, rbx
    call gc_dealloc
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_dealloc

;; ============================================================================
;; dict_len(PyObject *self) -> int64_t
;; Returns ob_size (number of items)
;; ============================================================================
global dict_len
dict_len:
    mov rax, [rdi + PyDictObject.ob_size]
    ret

;; ============================================================================
;; dict_subscript(rdi=dict, rsi=key, edx=key_tag) -> (rax=value, edx=value_tag)
;; mp_subscript: look up key, raise KeyError if not found
;; ============================================================================
DEF_FUNC dict_subscript
    push rbx

    mov rbx, rsi               ; save the key Value for the error message
    call dict_get              ; both take a key Value
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jz .key_error

    ; INCREF the returned value (dict_get returns borrowed fat ref)
    INCREF_VAL rax, rdx                ; value may be SmallInt
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.key_error:
    ; The key itself is the argument, as in CPython: d["k"] reports
    ; KeyError('k'), not a fixed "key not found".  rbx already holds it.
    mov rdi, rbx               ; the key Value, saved on entry
    extern raise_key_error
    call raise_key_error
END_FUNC dict_subscript

;; ============================================================================
;; dict_ass_subscript(rdi=dict, rsi=key Value, rdx=value Value)
;; A value Value of 0 (NULL) means "delete this key".
;; mp_ass_subscript: set key=value or delete key from dict
;; ============================================================================
DEF_FUNC_BARE dict_ass_subscript
    ; A NULL Value is 0 and no real Value is, so this test needs no tag
    test rdx, rdx
    jz .das_delete
    jmp dict_set
.das_delete:
    jmp dict_del
END_FUNC dict_ass_subscript

;; ============================================================================
;; dict_del(rdi=dict, rsi=key Value) -> int (0=ok, -1=not found)
;; Delete key from dict. DECREFs both key and value.
;; ============================================================================
DD_DICT  equ 8
DD_KEYV  equ 16
DD_FRAME equ 32             ; + 2 pushes = 48
DEF_FUNC dict_del, DD_FRAME
    push rbx
    push r12
    mov [rbp - DD_DICT], rdi
    mov [rbp - DD_KEYV], rsi

    call dict_lookup            ; rax = index or -1, rdx = slot
    mov rbx, [rbp - DD_DICT]
    test rax, rax
    js .dd_missing
    mov r12, rdx                ; the slot to mark dummy

    ; Hole the dense entry.  It keeps its position so the surrounding order
    ; is preserved; the index array forgets it.
    mov rcx, [rbx + PyDictObject.entries]
    imul rax, rax, DICT_ENTRY_SIZE
    add rcx, rax
    mov rdi, [rcx + DictEntry.key]
    mov rsi, [rcx + DictEntry.value]
    mov qword [rcx + DictEntry.key], 0
    mov qword [rcx + DictEntry.value], 0
    mov qword [rcx + DictEntry.hash], ENTRY_TOMBSTONE_HASH
    push rsi
    DECREF_V rdi, rax
    pop rdi
    DECREF_V rdi, rax

    mov rcx, [rbx + PyDictObject.dk_indices]
    mov qword [rcx + r12*8], DICT_IX_DUMMY
    dec qword [rbx + PyDictObject.ob_size]
    inc qword [rbx + PyDictObject.dk_tombstones]
    inc qword [rbx + PyDictObject.dk_version]
    jnz .dd_done
    mov qword [rbx + PyDictObject.dk_version], 1
.dd_done:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.dd_missing:
    mov rdi, [rbp - DD_KEYV]
    call raise_key_error
END_FUNC dict_del

; dict_repr is in src/repr.asm
extern dict_repr

;; ============================================================================
;; dict_tp_iter(PyDictObject *dict) -> PyDictIterObject*
;; Create a new dict key iterator.
;; rdi = dict
;; ============================================================================
DEF_FUNC dict_tp_iter
    push rbx

    mov rbx, rdi               ; save dict

    mov edi, PyDictIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel dict_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyDictIterObject.it_dict], rbx
    mov qword [rax + PyDictIterObject.it_index], 0
    mov qword [rax + PyDictIterObject.it_kind], 0  ; 0 = keys
    ; Snapshot dk_version for mutation detection
    mov rcx, [rbx + PyDictObject.dk_version]
    mov [rax + PyDictIterObject.it_version], rcx

    ; INCREF the dict
    mov rdi, rbx
    call obj_incref

    pop rbx
    leave
    ret
END_FUNC dict_tp_iter

;; ============================================================================
;; dict_iter_next(PyDictIterObject *self) -> (rax=key, edx=key_tag) or (0, TAG_NULL)
;; Return next key, or (0, TAG_NULL) if exhausted.
;; Scans entries for next non-empty slot.
;; rdi = iterator
;; ============================================================================
extern exc_RuntimeError_type

DEF_FUNC_BARE dict_iter_next
    ; Mutation detection: compare saved version with current
    mov rax, [rdi + PyDictIterObject.it_dict]         ; dict
    mov rcx, [rax + PyDictObject.dk_version]
    cmp rcx, [rdi + PyDictIterObject.it_version]
    jne .di_mutation_error

    mov r10, [rdi + PyDictIterObject.it_kind]         ; 0=keys, 1=values, 2=items
    mov rcx, [rdi + PyDictIterObject.it_index]        ; current index
    mov rdx, [rax + PyDictObject.capacity]            ; capacity
    mov rsi, [rax + PyDictObject.entries]              ; entries ptr

.di_scan:
    cmp rcx, rdx
    jge .di_exhausted

    ; Check if entry at index has a key (key_tag != TAG_NULL)
    imul rax, rcx, DictEntry_size
    add rax, rsi
    ENTRY_CLASSIFY rax, .di_skip, .di_skip

    ; Found a valid entry — advance index
    inc rcx
    mov [rdi + PyDictIterObject.it_index], rcx

    ; Branch on kind
    cmp r10, 1
    je .di_return_value
    ja .di_return_item

    ; kind=0: return key
    mov rax, [rax + DictEntry.key]
    INCREF_V rax, rdx
    ret

.di_return_value:
    ; kind=1: return value
    mov rax, [rax + DictEntry.value]
    INCREF_V rax, rdx
    ret

.di_return_item:
    ; kind=2: return (key, value) 2-tuple
    ; rax = entry ptr — need to allocate tuple, so must save entry
    push rbx
    push r12
    mov rbx, rax                ; save entry ptr

    ; Allocate 2-tuple
    mov edi, 2
    extern tuple_new
    call tuple_new
    mov r12, rax                ; r12 = new tuple

    mov r9, [r12 + PyTupleObject.ob_item]

    ; tuple[0] = key
    mov rax, [rbx + DictEntry.key]
    INCREF_V rax, rdx
    mov [r9], rax

    ; tuple[1] = value
    mov rax, [rbx + DictEntry.value]
    INCREF_V rax, rdx
    mov [r9 + 8], rax

    mov rax, r12

    pop r12
    pop rbx
    ret

.di_skip:
    inc rcx
    jmp .di_scan

.di_exhausted:
    mov [rdi + PyDictIterObject.it_index], rcx
    RET_NULL
    ret

.di_mutation_error:
    RAISE exc_RuntimeError_type, "dictionary changed size during iteration"
END_FUNC dict_iter_next

;; ============================================================================
;; dict_iter_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL dict_iter_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF the dict
    mov rdi, [rbx + PyDictIterObject.it_dict]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC dict_iter_dealloc

;; ============================================================================
;; dict_iter_self(PyObject *self) -> self with INCREF
;; ============================================================================
dict_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret

;; ============================================================================
;; dict_contains(rdi=dict, rsi=key, edx=key_tag) -> int (0 or 1)
;; For the 'in' operator: checks if key exists in dict.
;; ============================================================================
DEF_FUNC_BARE dict_contains
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jz .dc_no
    mov eax, 1
    ret
.dc_no:
    xor eax, eax
    ret
END_FUNC dict_contains

;; ============================================================================
;; Dict View Objects
;; dict.keys(), dict.values(), dict.items() return view objects.
;; Views hold a reference to the dict and support iteration + len().
;; ============================================================================

;; ============================================================================
;; dict_view_new(rdi=dict, rsi=kind, rdx=type_ptr) -> PyDictViewObject*
;; Create a new dict view. kind: 0=keys, 1=values, 2=items
;; ============================================================================
DEF_FUNC dict_view_new
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; dict
    mov r12, rsi               ; kind
    mov r13, rdx               ; view type

    mov edi, PyDictViewObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    mov [rax + PyObject.ob_type], r13
    mov [rax + PyDictViewObject.dv_dict], rbx
    mov [rax + PyDictViewObject.dv_kind], r12

    ; INCREF dict
    mov rdi, rbx
    call obj_incref

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_view_new

;; ============================================================================
;; dict_view_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL dict_view_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF dict
    mov rdi, [rbx + PyDictViewObject.dv_dict]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC dict_view_dealloc

;; ============================================================================
;; dict_view_len(rdi=view) -> i64
;; Returns the number of items in the underlying dict.
;; ============================================================================
DEF_FUNC_BARE dict_view_len
    mov rax, [rdi + PyDictViewObject.dv_dict]
    mov rax, [rax + PyDictObject.ob_size]
    ret
END_FUNC dict_view_len

;; ============================================================================
;; dict_view_iter(rdi=view) -> PyDictIterObject*
;; Create an iterator for this view, using the view's kind.
;; ============================================================================
DEF_FUNC dict_view_iter
    push rbx
    push r12

    mov rbx, rdi               ; view

    mov edi, PyDictIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel dict_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov rdi, [rbx + PyDictViewObject.dv_dict]
    mov [rax + PyDictIterObject.it_dict], rdi
    mov qword [rax + PyDictIterObject.it_index], 0
    mov rcx, [rbx + PyDictViewObject.dv_kind]
    mov [rax + PyDictIterObject.it_kind], rcx
    ; Snapshot dk_version for mutation detection
    mov rcx, [rdi + PyDictObject.dk_version]
    mov [rax + PyDictIterObject.it_version], rcx

    ; INCREF dict
    push rax                    ; save iterator
    call obj_incref
    pop rax                     ; restore iterator

    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_view_iter

;; ============================================================================
;; dict_keys_view_contains(rdi=view, rsi=key, rdx=key_tag) -> int (0 or 1)
;; sq_contains for dict_keys view: delegates to dict_contains on underlying dict.
;; ============================================================================
DEF_FUNC_BARE dict_keys_view_contains
    mov rdi, [rdi + PyDictViewObject.dv_dict]
    jmp dict_contains           ; (rdi=dict, rsi=key Value)
END_FUNC dict_keys_view_contains

;; ============================================================================
;; dict_nb_or(left, right, ltag, rtag) -> new dict (merge)
;; Implements dict | dict -> new dict containing all items from both.
;; Right dict values override left on key collision.
;; ============================================================================
DNO_LEFT  equ 8
DNO_RIGHT equ 16
DNO_NEW   equ 24
DNO_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC dict_nb_or, DNO_FRAME
    ; Both operands of `|` must be dicts.  These slots used to read whatever
    ; they were handed as a PyDictObject: `{1:2} | 5` dereferenced address
    ; 5 + capacity_offset.  A NULL Value declines, so the protocol can still
    ; reach a user class's __ror__.
    V_TEST_PTR rdi, rax         ; ja == not a pointer, so not a dict either
    ja .nb_or_decline
    V_TEST_PTR rsi, rax
    ja .nb_or_decline
    mov rax, [rdi + PyObject.ob_type]
    REQUIRE_DICT_TYPE rax, rcx, .nb_or_decline
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_DICT_TYPE rax, rcx, .nb_or_decline
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    mov [rbp - DNO_LEFT], rdi       ; left dict
    mov [rbp - DNO_RIGHT], rsi      ; right dict

    ; Create new dict
    call dict_new
    mov [rbp - DNO_NEW], rax

    ; Copy all entries from left dict
    mov rdi, [rbp - DNO_LEFT]
    mov r8, [rdi + PyDictObject.capacity]
    xor ecx, ecx                    ; index = 0
.dno_copy_left:
    cmp rcx, r8
    jge .dno_copy_right_start

    imul rax, rcx, DICT_ENTRY_SIZE
    add rax, [rdi + PyDictObject.entries]
    ; Check if entry is occupied (value_tag != 0)
    cmp qword [rax + DictEntry.key], 0   ; occupied?
    je .dno_left_next

    ; dict_set(dict, key, value, value_tag, key_tag)
    push rcx
    push r8
    push rdi
    mov rdi, [rbp - DNO_NEW]
    mov rsi, [rax + DictEntry.key]
    mov rdx, [rax + DictEntry.value]
    call dict_set
    pop rdi
    pop r8
    pop rcx

.dno_left_next:
    inc rcx
    jmp .dno_copy_left

.dno_copy_right_start:
    ; Copy all entries from right dict (overrides left)
    mov rdi, [rbp - DNO_RIGHT]
    mov r8, [rdi + PyDictObject.capacity]
    xor ecx, ecx
.dno_copy_right:
    cmp rcx, r8
    jge .dno_done

    imul rax, rcx, DICT_ENTRY_SIZE
    add rax, [rdi + PyDictObject.entries]
    cmp qword [rax + DictEntry.key], 0   ; occupied?
    je .dno_right_next

    push rcx
    push r8
    push rdi
    mov rdi, [rbp - DNO_NEW]
    mov rsi, [rax + DictEntry.key]
    mov rdx, [rax + DictEntry.value]
    call dict_set
    pop rdi
    pop r8
    pop rcx

.dno_right_next:
    inc rcx
    jmp .dno_copy_right

.dno_done:
    mov rax, [rbp - DNO_NEW]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; return one Value
    ret
.nb_or_decline:
    xor eax, eax                ; NULL Value = NotImplemented
    leave
    ret
END_FUNC dict_nb_or

;; ============================================================================
;; dict_nb_ior(left, right, ltag, rtag) -> left dict (inplace merge |=)
;; Iterates right dict entries and dict_set each into left.
;; Returns (left, TAG_PTR) with INCREF on left.
;; ============================================================================
DIO_LEFT  equ 8
DIO_RIGHT equ 16
DIO_FRAME equ 24            ; + 0 pushes = 24, not 16-aligned

DEF_FUNC dict_nb_ior, DIO_FRAME
    ; Both operands must be dicts.  CPython's `|=` also accepts any iterable of
    ; key/value pairs; this slot read whatever it was given as a PyDictObject,
    ; so `d |= 5` was an arbitrary dereference.  Declining is safe and gives the
    ; right exception type; the iterable-of-pairs form is a gap, in bugs.md.
    V_TEST_PTR rdi, rax         ; ja == not a pointer, so not a dict either
    ja .nb_ior_decline
    V_TEST_PTR rsi, rax
    ja .nb_ior_decline
    mov rax, [rdi + PyObject.ob_type]
    REQUIRE_DICT_TYPE rax, rcx, .nb_ior_decline
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_DICT_TYPE rax, rcx, .nb_ior_decline
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    mov [rbp - DIO_LEFT], rdi       ; left dict
    mov [rbp - DIO_RIGHT], rsi      ; right dict

    ; Iterate right dict entries, set each into left
    mov rdi, [rbp - DIO_RIGHT]
    mov r8, [rdi + PyDictObject.capacity]
    xor ecx, ecx
.dio_loop:
    cmp rcx, r8
    jge .dio_done

    imul rax, rcx, DICT_ENTRY_SIZE
    add rax, [rdi + PyDictObject.entries]
    cmp qword [rax + DictEntry.key], 0   ; occupied?
    je .dio_next

    push rcx
    push r8
    push rdi
    mov rdi, [rbp - DIO_LEFT]
    mov rsi, [rax + DictEntry.key]
    mov rdx, [rax + DictEntry.value]
    call dict_set
    pop rdi
    pop r8
    pop rcx

.dio_next:
    inc rcx
    jmp .dio_loop

.dio_done:
    ; Return left dict with INCREF (caller will DECREF both operands)
    mov rax, [rbp - DIO_LEFT]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; return one Value
    ret
.nb_ior_decline:
    xor eax, eax                ; NULL Value = NotImplemented
    leave
    ret
END_FUNC dict_nb_ior

;; ============================================================================
;; dict_richcompare(left, right, op, left_tag, right_tag) -> (payload, tag)
;; rdi=left, rsi=right, edx=op, rcx=left_tag, r8=right_tag
;; Only supports Py_EQ (2) and Py_NE (3).
;; Two dicts are equal if they have the same size and all key-value pairs match.
;; ============================================================================

DRC_LEFT  equ 8
DRC_RIGHT equ 16
DRC_OP    equ 24
DRC_LVAL  equ 32
DRC_LTAG  equ 40
DRC_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC dict_richcompare, DRC_FRAME
    V_UNPACK rdi, rcx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, r8            ; right Value -> (payload, tag)
    ; edx = op (PY_EQ=2, PY_NE=3)
    mov [rbp - DRC_LEFT], rdi
    mov [rbp - DRC_RIGHT], rsi
    mov [rbp - DRC_OP], edx

    ; The right operand is dereferenced as a dict below, so it has to be
    ; one: an immediate's payload is not an address, and any other object's
    ; fields are not ob_size/capacity/entries.
    cmp r8d, TAG_PTR
    jne .drc_not_impl
    lea rax, [rel dict_type]
    cmp [rsi + PyObject.ob_type], rax
    jne .drc_not_impl

    ; Only handle EQ (2) and NE (3)
    cmp edx, 2
    je .drc_do_eq
    cmp edx, 3
    je .drc_do_eq

.drc_not_impl:
    ; Unsupported op — return NotImplemented (NULL)
    RET_NULL
    leave
    ret

.drc_do_eq:
    ; Compare sizes
    mov rdi, [rbp - DRC_LEFT]
    mov rsi, [rbp - DRC_RIGHT]
    mov rax, [rdi + PyDictObject.ob_size]
    mov rcx, [rsi + PyDictObject.ob_size]
    cmp rax, rcx
    jne .drc_not_equal

    ; Same size — check all key-value pairs from left exist in right with same value
    mov r9, [rdi + PyDictObject.capacity]
    xor r10d, r10d                  ; index = 0

.drc_loop:
    cmp r10, r9
    jge .drc_equal

    mov rdi, [rbp - DRC_LEFT]
    imul rax, r10, DICT_ENTRY_SIZE
    add rax, [rdi + PyDictObject.entries]

    ; Skip empty entries
    cmp qword [rax + DictEntry.key], 0   ; occupied?
    je .drc_next

    ; Save entry data to stack slots (safe across function calls)
    push r9
    push r10
    mov r11, [rax + DictEntry.value]        ; left value
    V_UNPACK r11, r9
    mov [rbp - DRC_LVAL], r11               ; save to stack slot
    mov [rbp - DRC_LTAG], r9                ; save to stack slot

    ; Lookup key in right dict
    mov rdi, [rbp - DRC_RIGHT]
    mov rsi, [rax + DictEntry.key]
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    ; rax = right value, edx = tag (0 = not found)
    ; NOTE: r11 and r9 are caller-saved and may be clobbered by dict_get
    test edx, edx
    jz .drc_not_equal_pop           ; key not in right

    ; Reload left value and tag from stack slots
    mov r11, [rbp - DRC_LVAL]
    mov r9d, [rbp - DRC_LTAG]

    ; Quick compare: same payload and same tag → equal
    cmp rax, r11
    jne .drc_values_differ
    cmp edx, r9d
    je .drc_values_match

.drc_values_differ:
    ; For SmallInt: both TAG_SMALLINT, compare payloads directly
    cmp r9d, TAG_SMALLINT
    jne .drc_ptr_compare
    cmp edx, TAG_SMALLINT
    jne .drc_not_equal_pop
    ; Both SmallInt, payloads differ → not equal
    jmp .drc_not_equal_pop

.drc_ptr_compare:
    ; Both TAG_PTR: use tp_richcompare
    cmp r9d, TAG_PTR
    jne .drc_not_equal_pop
    cmp edx, TAG_PTR
    jne .drc_not_equal_pop
    ; Call tp_richcompare(left_val, right_val, PY_EQ, TAG_PTR, TAG_PTR)
    mov rdi, r11                    ; left value
    mov rsi, rax                    ; right value
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .drc_not_equal_pop           ; no tp_richcompare
    mov edx, 2                      ; PY_EQ
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    V_PACK rdi, rcx             ; left  -> Value
    V_PACK rsi, r8              ; right -> Value
    call rax
    V_UNPACK rax, rdx           ; tp_richcompare returns a Value
    ; Result: (rax=payload, edx=tag).  True and False are heap singletons
    ; now, so test truthiness instead of looking for an inline bool payload.
    extern obj_is_true
    test edx, edx
    jz .drc_not_equal_pop           ; NULL result: treat as not equal
    mov rdi, rax
    mov rsi, rdx
    push rax
    push rdx
    V_PACK rdi, rsi
    call obj_is_true
    pop rdx
    pop rdi
    mov r11d, eax                   ; truthiness
    push r11
    DECREF_VAL rdi, rdx
    pop r11
    test r11d, r11d
    jz .drc_not_equal_pop
    jmp .drc_values_match

.drc_not_equal_pop:
    pop r10
    pop r9
.drc_not_equal:
    ; Return based on op: EQ→False, NE→True
    cmp dword [rbp - DRC_OP], 3     ; NE?
    je .drc_ret_true
    xor eax, eax                    ; False
    RET_BOOL_RAX
    leave
    ret

.drc_values_match:
    pop r10
    pop r9

.drc_next:
    inc r10
    jmp .drc_loop

.drc_equal:
    ; Return based on op: EQ→True, NE→False
    cmp dword [rbp - DRC_OP], 3     ; NE?
    je .drc_ret_false
.drc_ret_true:
    mov eax, 1                      ; True
    RET_BOOL_RAX
    leave
    ret

.drc_ret_false:
    xor eax, eax                    ; False
    RET_BOOL_RAX
    leave
    ret
END_FUNC dict_richcompare

;; ============================================================================
;; dict_reversed(args, nargs) -> PyDictIterObject* (reverse key iterator)
;; Called as dict.__reversed__(self).
;; args[0] = dict (self), nargs = 1
;; ============================================================================
DEF_FUNC dict_reversed
    ; args[0] = self (dict)
    mov rax, [rdi]             ; dict payload
    push rbx

    mov rbx, rax               ; rbx = dict

    mov edi, PyDictIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel dict_rev_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyDictIterObject.it_dict], rbx
    ; Set it_index to capacity - 1 (start from end)
    mov rcx, [rbx + PyDictObject.capacity]
    dec rcx
    mov [rax + PyDictIterObject.it_index], rcx
    mov qword [rax + PyDictIterObject.it_kind], 0  ; 0 = keys
    ; Snapshot dk_version for mutation detection
    mov rcx, [rbx + PyDictObject.dk_version]
    mov [rax + PyDictIterObject.it_version], rcx

    ; INCREF the dict
    push rax
    mov rdi, rbx
    call obj_incref
    pop rax

    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC dict_reversed

;; ============================================================================
;; dict_rev_iter_next(PyDictIterObject *self) -> (rax=key, edx=key_tag) or NULL
;; Like dict_iter_next but scans backwards (decrements index).
;; ============================================================================
DEF_FUNC_BARE dict_rev_iter_next
    ; Mutation detection
    mov rax, [rdi + PyDictIterObject.it_dict]
    mov rcx, [rax + PyDictObject.dk_version]
    cmp rcx, [rdi + PyDictIterObject.it_version]
    jne .dri_mutation_error

    mov rcx, [rdi + PyDictIterObject.it_index]        ; current index
    mov rsi, [rax + PyDictObject.entries]              ; entries ptr

.dri_scan:
    test rcx, rcx
    js .dri_exhausted           ; index < 0 → done

    ; Check if entry at index has a valid key
    imul rax, rcx, DictEntry_size
    add rax, rsi
    ENTRY_CLASSIFY rax, .dri_skip, .dri_skip

    ; Found a valid entry — save decremented index
    dec rcx
    mov [rdi + PyDictIterObject.it_index], rcx

    ; Return key
    mov rax, [rax + DictEntry.key]
    INCREF_V rax, rdx
    ret

.dri_skip:
    dec rcx
    jmp .dri_scan

.dri_exhausted:
    mov [rdi + PyDictIterObject.it_index], rcx
    RET_NULL
    ret

.dri_mutation_error:
    RAISE exc_RuntimeError_type, "dictionary changed size during iteration"
END_FUNC dict_rev_iter_next

;; ============================================================================
;; Data section
;; ============================================================================
section .data

; dict_repr_str removed - repr now in src/repr.asm
dict_iter_name: db "dict_keyiterator", 0
dict_rev_iter_name: db "dict_reversekeyiterator", 0
dict_keys_view_name: db "dict_keys", 0
dict_values_view_name: db "dict_values", 0
dict_items_view_name: db "dict_items", 0

dict_name_str: db "dict", 0

; Dict mapping methods
align 8
global dict_mapping_methods
dict_mapping_methods:
    dq dict_len                 ; mp_length
    dq dict_subscript           ; mp_subscript
    dq dict_ass_subscript       ; mp_ass_subscript

; Dict number methods (for | operator)
align 8
dict_number_methods:
    dq 0                        ; nb_add          +0
    dq 0                        ; nb_subtract     +8
    dq 0                        ; nb_multiply     +16
    dq 0                        ; nb_remainder    +24
    dq 0                        ; nb_divmod       +32
    dq 0                        ; nb_power        +40
    dq 0                        ; nb_negative     +48
    dq 0                        ; nb_positive     +56
    dq 0                        ; nb_absolute     +64
    dq 0                        ; nb_bool         +72
    dq 0                        ; nb_invert       +80
    dq 0                        ; nb_lshift       +88
    dq 0                        ; nb_rshift       +96
    dq 0                        ; nb_and          +104
    dq 0                        ; nb_xor          +112
    dq dict_nb_or               ; nb_or           +120 (dict merge |)
    dq 0                        ; nb_int          +128
    dq 0                        ; nb_float        +136
    dq 0                        ; nb_floor_divide +144
    dq 0                        ; nb_true_divide  +152
    dq 0                        ; nb_index        +160
    ; Inplace slots
    dq 0                        ; nb_iadd         +168
    dq 0                        ; nb_isub         +176
    dq 0                        ; nb_imul         +184
    dq 0                        ; nb_irem         +192
    dq 0                        ; nb_ipow         +200
    dq 0                        ; nb_ilshift      +208
    dq 0                        ; nb_irshift      +216
    dq 0                        ; nb_iand         +224
    dq 0                        ; nb_ixor         +232
    dq dict_nb_ior              ; nb_ior          +240 (dict inplace merge |=)
    dq 0                        ; nb_ifloor_divide +248
    dq 0                        ; nb_itrue_divide +256
    dq 0 ; nb_matmul
    dq 0 ; nb_imatmul

; Dict sequence methods (for 'in' operator)
align 8
dict_sequence_methods:
    dq dict_len                 ; sq_length
    dq 0                        ; sq_concat
    dq 0                        ; sq_repeat
    dq 0                        ; sq_item
    dq 0                        ; sq_ass_item
    dq dict_contains            ; sq_contains
    dq 0                        ; sq_inplace_concat
    dq 0                        ; sq_inplace_repeat

; Dict type object
align 8
global dict_type
dict_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq dict_name_str            ; tp_name
    dq PyDictObject_size        ; tp_basicsize
    dq dict_dealloc             ; tp_dealloc
    dq dict_repr                ; tp_repr
    dq dict_repr                ; tp_str
    extern hash_not_implemented
    dq hash_not_implemented     ; tp_hash (raises TypeError)
    dq 0                ; tp_call  (instances are not callable)
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq dict_richcompare         ; tp_richcompare
    dq dict_tp_iter             ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq dict_type_call       ; tp_new  (constructor)
    dq dict_number_methods      ; tp_as_number
    dq dict_sequence_methods    ; tp_as_sequence (for 'in' operator)
    dq dict_mapping_methods     ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC | TYPE_FLAG_DICT_SUBCLASS  ; tp_flags
    dq 0                        ; tp_bases
    dq dict_traverse                        ; tp_traverse
    dq dict_clear_gc                        ; tp_clear
    dq 0          ; tp_dictoffset

; Dict key iterator type
align 8
global dict_iter_type
dict_iter_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq dict_iter_name           ; tp_name
    dq PyDictIterObject_size    ; tp_basicsize
    dq dict_iter_dealloc        ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq dict_iter_self           ; tp_iter (return self)
    dq dict_iter_next           ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; Dict reverse key iterator type
align 8
global dict_rev_iter_type
dict_rev_iter_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq dict_rev_iter_name       ; tp_name
    dq PyDictIterObject_size    ; tp_basicsize
    dq dict_iter_dealloc        ; tp_dealloc (reuse forward iter dealloc)
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq dict_iter_self           ; tp_iter (return self)
    dq dict_rev_iter_next       ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; Dict keys view sequence methods (len + contains)
align 8
dict_keys_view_seq_methods:
    dq dict_view_len            ; sq_length
    dq 0                        ; sq_concat
    dq 0                        ; sq_repeat
    dq 0                        ; sq_item
    dq 0                        ; sq_ass_item
    dq dict_keys_view_contains  ; sq_contains
    dq 0                        ; sq_inplace_concat
    dq 0                        ; sq_inplace_repeat

; Dict view sequence methods (for len(), values/items views)
align 8
dict_view_sequence_methods:
    dq dict_view_len            ; sq_length
    dq 0                        ; sq_concat
    dq 0                        ; sq_repeat
    dq 0                        ; sq_item
    dq 0                        ; sq_ass_item
    dq 0                        ; sq_contains
    dq 0                        ; sq_inplace_concat
    dq 0                        ; sq_inplace_repeat

; Dict keys view type
align 8
global dict_keys_view_type
dict_keys_view_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq dict_keys_view_name      ; tp_name
    dq PyDictViewObject_size    ; tp_basicsize
    dq dict_view_dealloc        ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq dict_view_iter           ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq dict_keys_view_seq_methods ; tp_as_sequence (with sq_contains)
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; Dict values view type
align 8
global dict_values_view_type
dict_values_view_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq dict_values_view_name    ; tp_name
    dq PyDictViewObject_size    ; tp_basicsize
    dq dict_view_dealloc        ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq dict_view_iter           ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq dict_view_sequence_methods ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; Dict items view type
align 8
global dict_items_view_type
dict_items_view_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq dict_items_view_name     ; tp_name
    dq PyDictViewObject_size    ; tp_basicsize
    dq dict_view_dealloc        ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq dict_view_iter           ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq dict_view_sequence_methods ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

section .text

;; ============================================================================
;; GC traverse and clear.  These lived in gc.asm, which left the collector
;; holding the reference graph of every type in the system; a type's own
;; file is the only place that knows which of its fields are owned.
;; ============================================================================

; ---- dict_traverse / dict_clear ----

DEF_FUNC dict_traverse
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    test r13, r13
    jz .done
.loop:
    dec r13
    ; Check for empty/tombstone
    ENTRY_CLASSIFY r12, .next, .next

    ; Visit key
    mov rdi, [r12 + DictEntry.key]

    VISIT_V rdi, rsi
    ; Visit value
    mov rdi, [r12 + DictEntry.value]

    VISIT_V rdi, rsi

.next:
    add r12, DICT_ENTRY_SIZE
    test r13, r13
    jnz .loop
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_traverse

DEF_FUNC dict_clear_gc
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]

    test r13, r13
    jz .done
.loop:
    dec r13
    ENTRY_CLASSIFY r12, .next, .next

    ; DECREF key
    push r12
    push r13
    mov rdi, [r12 + DictEntry.key]
    V_UNPACK rdi, rsi
    DECREF_VAL rdi, rsi
    pop r13
    pop r12

    ; DECREF value
    push r12
    push r13
    mov rdi, [r12 + DictEntry.value]
    V_UNPACK rdi, rsi
    DECREF_VAL rdi, rsi
    pop r13
    pop r12

    ; Clear entry.  It has to become a *tombstone*, not just a zeroed key:
    ; ENTRY_CLASSIFY reads key==0 with any hash other than -1 as "empty",
    ; which ends a probe early, so a surviving key further along the chain
    ; becomes unreachable.
    mov qword [r12 + DictEntry.key], 0
    mov qword [r12 + DictEntry.value], 0
    mov qword [r12 + DictEntry.hash], ENTRY_TOMBSTONE_HASH

.next:
    add r12, DICT_ENTRY_SIZE
    test r13, r13
    jnz .loop
.done:
    ; Keep the header coherent with the table we just emptied: the sparse
    ; index array has to forget the entries too.
    mov rdi, [rbx + PyDictObject.dk_indices]
    test rdi, rdi
    jz .no_indices
    mov rcx, [rbx + PyDictObject.capacity]
    mov rax, DICT_IX_EMPTY
    rep stosq
.no_indices:
    mov qword [rbx + PyDictObject.ob_size], 0
    mov qword [rbx + PyDictObject.dk_nentries], 0
    mov qword [rbx + PyDictObject.dk_tombstones], 0
    inc qword [rbx + PyDictObject.dk_version]

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_clear_gc
