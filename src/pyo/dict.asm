; dict_obj.asm - Dict type implementation
; Phase 4: open-addressing hash table with linear probing

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern obj_hash
extern obj_decref
extern obj_dealloc
extern str_type
extern ap_strcmp
extern ap_memset
extern fatal_error
extern raise_exception
extern exc_KeyError_type
extern obj_incref
extern str_from_cstr
extern type_type

; Initial capacity (must be power of 2)
DICT_INIT_CAP equ 8

;; ============================================================================
;; dict_new() -> PyDictObject*
;; Allocate a new empty dict with initial capacity 8
;; ============================================================================
DEF_FUNC dict_new
    push rbx

    ; Allocate PyDictObject header
    mov edi, PyDictObject_size
    call ap_malloc
    mov rbx, rax                ; rbx = dict

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel dict_type]
    mov [rbx + PyObject.ob_type], rax
    mov qword [rbx + PyDictObject.ob_size], 0
    mov qword [rbx + PyDictObject.capacity], DICT_INIT_CAP
    mov qword [rbx + PyDictObject.dk_version], 1

    ; Allocate entries array: capacity * DICT_ENTRY_SIZE
    mov edi, DICT_INIT_CAP * DICT_ENTRY_SIZE
    call ap_malloc
    mov [rbx + PyDictObject.entries], rax

    ; Zero out entries (NULL key = empty slot)
    mov rdi, rax
    xor esi, esi
    mov edx, DICT_INIT_CAP * DICT_ENTRY_SIZE
    call ap_memset

    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC dict_new

;; ============================================================================
;; dict_keys_equal(rdi=a_key, rsi=b_key, edx=a_tag, ecx=b_tag) -> int (1=equal, 0=not)
;; Internal helper: value equality for SmallInts, string comparison for heap ptrs
;; ============================================================================
DEF_FUNC_LOCAL dict_keys_equal
    ; Value/pointer equality — handles SmallInts and same-object ptrs
    cmp rdi, rsi
    jne .dke_diff_payload
    ; Same payload — equal (same SmallInt value or same heap ptr)
    mov eax, 1
    leave
    ret

.dke_diff_payload:
    ; Different payloads — if either is not TAG_PTR, can't be equal
    ; (Two SmallInts with same value would have matched above)
    cmp edx, TAG_PTR
    jne .dke_not_equal
    cmp ecx, TAG_PTR
    jne .dke_not_equal

    ; Both heap ptrs with different addresses — check string equality
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi

    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .dke_ne_pop

    mov rax, [r12 + PyObject.ob_type]
    cmp rax, rcx
    jne .dke_ne_pop

    ; Both strings — compare data
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jnz .dke_ne_pop

    ; Equal strings
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret

.dke_ne_pop:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.dke_not_equal:
    xor eax, eax
    leave
    ret
END_FUNC dict_keys_equal

;; ============================================================================
;; dict_get(rdi=dict, rsi=key, edx=key_tag) -> (rax=value, rdx=value_tag) or (0, TAG_NULL)
;; Linear probing lookup
;; ============================================================================
DG_KTAG equ 8
DEF_FUNC dict_get, 8
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; rbx = dict
    mov r12, rsi                ; r12 = key
    mov [rbp - DG_KTAG], rdx    ; save key_tag

    ; Hash the key
    mov rdi, r12
    mov esi, edx                ; key tag
    call obj_hash
    mov r13, rax                ; r13 = hash

    ; capacity mask = capacity - 1 (capacity is power of 2)
    mov r14, [rbx + PyDictObject.capacity]
    lea r15, [r14 - 1]          ; r15 = mask

    ; Starting slot = hash & mask
    mov rcx, r13
    and rcx, r15                ; rcx = slot index

    ; r14 reused as probe counter
    xor r14d, r14d              ; probes done

align 16
.probe_loop:
    ; Check if we've probed all slots
    cmp r14, [rbx + PyDictObject.capacity]
    jge .not_found

    ; Compute entry address: entries + slot * DICT_ENTRY_SIZE
    mov rax, [rbx + PyDictObject.entries]
    imul rdx, rcx, DICT_ENTRY_SIZE
    add rax, rdx                ; rax = entry ptr

    ; Check if slot is empty (key_tag == TAG_NULL)
    mov rdi, [rax + DictEntry.key]
    cmp qword [rax + DictEntry.key_tag], 0
    je .not_found

    ; Check hash first (fast reject)
    cmp r13, [rax + DictEntry.hash]
    jne .next_slot

    ; Hash matches - check key equality
    ; rdi already has entry.key
    mov rsi, r12                ; our key
    mov edx, [rax + DictEntry.key_tag]  ; entry's key tag
    push rcx                    ; save slot
    push rax                    ; save entry ptr
    mov ecx, [rbp - DG_KTAG]   ; our key tag
    call dict_keys_equal
    pop rdx                     ; restore entry ptr into rdx
    pop rcx                     ; restore slot
    test eax, eax
    jz .next_slot

    ; Found - return entry.value + tag
    mov rax, [rdx + DictEntry.value]
    mov rdx, [rdx + DictEntry.value_tag]
    jmp .done

.next_slot:
    ; Linear probe: slot = (slot + 1) & mask
    inc rcx
    and rcx, r15
    inc r14
    jmp .probe_loop

.not_found:
    RET_NULL

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_get

;; ============================================================================
;; dict_get_index(rdi=dict, rsi=key, edx=key_tag) -> int64
;; Like dict_get but returns the slot index (for IC caching), -1 if not found.
;; ============================================================================
GI_KTAG equ 8
DEF_FUNC dict_get_index, 8
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; rbx = dict
    mov r12, rsi                ; r12 = key
    mov [rbp - GI_KTAG], rdx    ; save key_tag

    mov rdi, r12
    mov esi, edx                ; key tag
    call obj_hash
    mov r13, rax                ; r13 = hash

    mov r14, [rbx + PyDictObject.capacity]
    lea r15, [r14 - 1]          ; r15 = mask

    mov rcx, r13
    and rcx, r15                ; rcx = slot index

    xor r14d, r14d              ; probes done

.gi_probe:
    cmp r14, [rbx + PyDictObject.capacity]
    jge .gi_not_found

    mov rax, [rbx + PyDictObject.entries]
    imul rdx, rcx, DICT_ENTRY_SIZE
    add rax, rdx

    mov rdi, [rax + DictEntry.key]
    cmp qword [rax + DictEntry.key_tag], 0
    je .gi_not_found

    cmp r13, [rax + DictEntry.hash]
    jne .gi_next

    mov rsi, r12
    mov edx, [rax + DictEntry.key_tag]
    push rcx
    mov ecx, [rbp - GI_KTAG]
    call dict_keys_equal
    pop rcx
    test eax, eax
    jz .gi_next

    ; Found: return slot index
    mov rax, rcx
    jmp .gi_done

.gi_next:
    inc rcx
    and rcx, r15
    inc r14
    jmp .gi_probe

.gi_not_found:
    mov rax, -1

.gi_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_get_index

;; ============================================================================
;; dict_find_slot(rdi=dict, rsi=key, rdx=hash, ecx=key_tag)
;;   -> rax = entry ptr, rdx = 1 if existing key found, 0 if empty slot
;; Internal helper used by dict_set
;; ============================================================================
FS_KTAG equ 8
DEF_FUNC_LOCAL dict_find_slot, 8
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; dict
    mov r12, rsi                ; key
    mov r13, rdx                ; hash
    mov [rbp - FS_KTAG], ecx    ; save key_tag

    ; mask = capacity - 1
    mov r14, [rbx + PyDictObject.capacity]
    lea r15, [r14 - 1]          ; mask

    ; slot = hash & mask
    mov rcx, r13
    and rcx, r15

    xor r14d, r14d              ; probe counter

.find_loop:
    cmp r14, [rbx + PyDictObject.capacity]
    jge .table_full

    ; entry = entries + slot * DICT_ENTRY_SIZE
    mov rax, [rbx + PyDictObject.entries]
    imul rdx, rcx, DICT_ENTRY_SIZE
    add rax, rdx                ; rax = entry ptr

    ; Empty slot? (check key_tag for TAG_NULL=0)
    mov rdi, [rax + DictEntry.key]
    cmp qword [rax + DictEntry.key_tag], 0
    je .found_empty

    ; Hash match?
    cmp r13, [rax + DictEntry.hash]
    jne .find_next

    ; Key equality check
    ; rdi = entry.key
    mov rsi, r12
    mov edx, [rax + DictEntry.key_tag]  ; entry's key tag
    push rcx
    push rax
    mov ecx, [rbp - FS_KTAG]   ; our key tag
    call dict_keys_equal
    pop rax                     ; entry ptr
    pop rcx                     ; slot
    test eax, eax
    jnz .found_existing

.find_next:
    inc rcx
    and rcx, r15
    inc r14
    jmp .find_loop

.found_empty:
    ; rax = entry ptr, rdx = 0 (empty)
    xor edx, edx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.found_existing:
    ; rax = entry ptr, rdx = 1 (existing)
    mov edx, 1
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.table_full:
    ; Should never happen if load factor is maintained
    lea rdi, [rel .err_full]
    call fatal_error

section .rodata
.err_full: db "dict: hash table full", 0
section .text
END_FUNC dict_find_slot

;; ============================================================================
;; dict_resize(PyDictObject *dict)
;; Double capacity and rehash all entries
;; ============================================================================
DEF_FUNC_LOCAL dict_resize
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; dict

    ; Save old entries and capacity
    mov r12, [rbx + PyDictObject.entries]    ; old entries
    mov r13, [rbx + PyDictObject.capacity]   ; old capacity

    ; New capacity = old * 2
    lea r14, [r13 * 2]          ; r14 = new capacity
    mov [rbx + PyDictObject.capacity], r14

    ; Allocate new entries array
    imul rdi, r14, DICT_ENTRY_SIZE
    call ap_malloc
    mov r15, rax                ; r15 = new entries

    ; Zero new entries
    mov rdi, r15
    xor esi, esi
    imul rdx, r14, DICT_ENTRY_SIZE
    call ap_memset

    ; Store new entries pointer
    mov [rbx + PyDictObject.entries], r15

    ; Rehash: iterate old entries, re-insert non-empty ones
    xor ecx, ecx               ; ecx = index into old entries

.rehash_loop:
    cmp rcx, r13                ; compared against old capacity
    jge .rehash_done

    ; old_entry = old_entries + i * DICT_ENTRY_SIZE
    imul rax, rcx, DICT_ENTRY_SIZE
    add rax, r12                ; rax = old entry ptr

    ; Skip empty slots (check key_tag for TAG_NULL=0)
    cmp qword [rax + DictEntry.key_tag], 0
    je .rehash_next

    ; Compute new slot: hash & (new_capacity - 1)
    push rcx                    ; save outer index
    mov rcx, [rax + DictEntry.hash]
    mov rdx, r14
    dec rdx                     ; new mask
    and rcx, rdx                ; starting slot

    ; Save entry data (including key_tag)
    push qword [rax + DictEntry.hash]
    push qword [rax + DictEntry.key]
    push qword [rax + DictEntry.value]
    push qword [rax + DictEntry.value_tag]
    push qword [rax + DictEntry.key_tag]

    ; Linear probe in new table to find empty slot
.rehash_probe:
    imul rax, rcx, DICT_ENTRY_SIZE
    add rax, r15                ; new entry ptr
    cmp qword [rax + DictEntry.key_tag], 0
    je .rehash_insert

    inc rcx
    mov rax, r14
    dec rax
    and rcx, rax                ; slot = (slot+1) & new_mask
    jmp .rehash_probe

.rehash_insert:
    ; rax = target entry ptr in new table
    pop qword [rax + DictEntry.key_tag]
    pop qword [rax + DictEntry.value_tag]
    pop qword [rax + DictEntry.value]
    pop qword [rax + DictEntry.key]
    pop qword [rax + DictEntry.hash]

    pop rcx                     ; restore outer index

.rehash_next:
    inc ecx
    jmp .rehash_loop

.rehash_done:
    ; Free old entries array
    mov rdi, r12
    call ap_free

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_resize

;; ============================================================================
;; dict_set(rdi=dict, rsi=key, rdx=value, rcx=value_tag, r8=key_tag)
;; Insert or update a key-value pair.
;; ============================================================================
DS_VTAG equ 8
DS_KTAG equ 16
DEF_FUNC dict_set, 16
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; dict
    mov r12, rsi                ; key
    mov r13, rdx                ; value
    mov [rbp - DS_VTAG], rcx    ; save value_tag
    mov [rbp - DS_KTAG], r8     ; save key_tag

    ; Hash the key
    mov rdi, r12
    mov esi, r8d                ; key tag
    call obj_hash
    mov r14, rax                ; r14 = hash

    ; Find slot
    mov rdi, rbx                ; dict
    mov rsi, r12                ; key
    mov rdx, r14                ; hash
    mov ecx, [rbp - DS_KTAG]   ; key_tag
    call dict_find_slot
    ; rax = entry ptr, edx = 1 if existing, 0 if empty

    test edx, edx
    jnz .update_existing

    ; --- Insert new entry ---
    ; Store hash, key, key_tag, value, value_tag
    mov [rax + DictEntry.hash], r14
    mov [rax + DictEntry.key], r12
    mov rcx, [rbp - DS_KTAG]
    mov [rax + DictEntry.key_tag], rcx
    mov [rax + DictEntry.value], r13

    ; Store value tag from caller
    mov rcx, [rbp - DS_VTAG]
    mov [rax + DictEntry.value_tag], rcx

    ; INCREF key (tag-aware)
    mov rsi, [rax + DictEntry.key_tag]
    INCREF_VAL r12, rsi
    ; INCREF value (tag-aware)
    mov rsi, [rax + DictEntry.value_tag]
    INCREF_VAL r13, rsi

    ; Increment ob_size
    inc qword [rbx + PyDictObject.ob_size]

    ; Check load factor: ob_size > capacity * 3/4
    mov rax, [rbx + PyDictObject.capacity]
    mov rcx, rax
    shr rcx, 2                  ; capacity / 4
    imul rcx, rcx, 3            ; capacity * 3/4
    cmp [rbx + PyDictObject.ob_size], rcx
    jle .done

    ; Resize needed
    mov rdi, rbx
    call dict_resize
    jmp .done

.update_existing:
    ; rax = entry ptr with matching key
    ; DECREF old value (fat)
    push rax                    ; save entry ptr
    mov rdi, [rax + DictEntry.value]
    mov rsi, [rax + DictEntry.value_tag]
    DECREF_VAL rdi, rsi
    pop rax                     ; restore entry ptr

    ; Store new value and INCREF it
    mov [rax + DictEntry.value], r13
    ; Store new value tag from caller
    mov rcx, [rbp - DS_VTAG]
    mov [rax + DictEntry.value_tag], rcx
    mov rsi, [rax + DictEntry.value_tag]
    INCREF_VAL r13, rsi                ; value may be SmallInt

.done:
    ; Bump version counter (skip 0 on wrap)
    inc qword [rbx + PyDictObject.dk_version]
    cmp qword [rbx + PyDictObject.dk_version], 0
    jne .ver_ok
    mov qword [rbx + PyDictObject.dk_version], 1
.ver_ok:
    pop r14
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
    push r14

    mov rbx, rdi                ; self (dict)
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor r14d, r14d              ; index

.dealloc_loop:
    cmp r14, r13
    jge .dealloc_entries_done

    ; entry = entries + index * DICT_ENTRY_SIZE
    imul rax, r14, DICT_ENTRY_SIZE
    add rax, r12

    ; Skip empty slots (check key_tag for TAG_NULL=0)
    mov rdi, [rax + DictEntry.key]
    cmp qword [rax + DictEntry.key_tag], 0
    je .dealloc_next

    ; DECREF key (tag-aware)
    push rax
    mov rsi, [rax + DictEntry.key_tag]
    DECREF_VAL rdi, rsi

    ; DECREF value (tag-aware)
    pop rax
    mov rdi, [rax + DictEntry.value]
    mov rsi, [rax + DictEntry.value_tag]
    DECREF_VAL rdi, rsi

.dealloc_next:
    inc r14
    jmp .dealloc_loop

.dealloc_entries_done:
    ; Free entries array
    mov rdi, r12
    call ap_free

    ; Free dict object itself
    mov rdi, rbx
    call ap_free

    pop r14
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
dict_len:
    mov rax, [rdi + PyDictObject.ob_size]
    ret

;; ============================================================================
;; dict_subscript(rdi=dict, rsi=key, edx=key_tag) -> (rax=value, edx=value_tag)
;; mp_subscript: look up key, raise KeyError if not found
;; ============================================================================
DEF_FUNC dict_subscript
    push rbx

    mov rbx, rsi               ; save key for error msg

    ; dict_get(dict, key, key_tag) — edx already has key_tag
    call dict_get
    test edx, edx
    jz .key_error

    ; INCREF the returned value (dict_get returns borrowed fat ref)
    INCREF_VAL rax, rdx                ; value may be SmallInt
    pop rbx
    leave
    ret

.key_error:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "key not found"
    call raise_exception
END_FUNC dict_subscript

;; ============================================================================
;; dict_ass_subscript(rdi=dict, rsi=key, rdx=value, ecx=key_tag, r8d=value_tag)
;; mp_ass_subscript: set key=value or delete key from dict
;; ============================================================================
DEF_FUNC_BARE dict_ass_subscript
    ; If value is NULL, this is a delete operation
    test rdx, rdx
    jz .das_delete
    ; dict_set wants (rdi=dict, rsi=key, rdx=value, rcx=value_tag, r8=key_tag)
    ; Caller passes ecx=key_tag, r8d=value_tag — swap them
    xchg ecx, r8d
    jmp dict_set
.das_delete:
    ; dict_del wants (rdi=dict, rsi=key, edx=key_tag)
    mov edx, ecx               ; key_tag from caller's ecx
    jmp dict_del
END_FUNC dict_ass_subscript

;; ============================================================================
;; dict_del(rdi=dict, rsi=key, edx=key_tag) -> int (0=ok, -1=not found)
;; Delete key from dict. DECREFs both key and value.
;; ============================================================================
DD_KTAG equ 8
DEF_FUNC dict_del, 8
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; dict
    mov r12, rsi                ; key
    mov [rbp - DD_KTAG], rdx    ; save key_tag

    ; Hash the key
    mov rdi, r12
    mov esi, edx                ; key tag
    call obj_hash
    mov r13, rax                ; hash

    ; capacity mask
    mov r14, [rbx + PyDictObject.capacity]
    lea r15, [r14 - 1]          ; mask

    ; Starting slot
    mov rcx, r13
    and rcx, r15
    xor r14d, r14d              ; probe counter

.dd_probe:
    cmp r14, [rbx + PyDictObject.capacity]
    jge .dd_not_found

    mov rax, [rbx + PyDictObject.entries]
    imul rdx, rcx, DICT_ENTRY_SIZE
    add rax, rdx

    mov rdi, [rax + DictEntry.key]
    cmp qword [rax + DictEntry.key_tag], 0
    je .dd_not_found

    cmp r13, [rax + DictEntry.hash]
    jne .dd_next

    mov rsi, r12
    mov edx, [rax + DictEntry.key_tag]  ; entry's key tag
    push rcx
    push rax
    mov ecx, [rbp - DD_KTAG]   ; our key tag
    call dict_keys_equal
    pop rdx                     ; entry ptr
    pop rcx
    test eax, eax
    jz .dd_next

    ; Found: null out entry, DECREF key and value, decrement size
    push qword [rdx + DictEntry.key_tag]
    mov rdi, [rdx + DictEntry.key]
    mov qword [rdx + DictEntry.key], 0
    mov qword [rdx + DictEntry.key_tag], 0
    push qword [rdx + DictEntry.value]
    push qword [rdx + DictEntry.value_tag]
    mov qword [rdx + DictEntry.value], 0
    mov qword [rdx + DictEntry.value_tag], 0

    ; DECREF key (tag-aware)
    mov rsi, [rsp + 16]         ; key_tag (3 pushes deep)
    DECREF_VAL rdi, rsi
    pop rsi                     ; value_tag
    pop rdi                     ; value payload
    DECREF_VAL rdi, rsi         ; DECREF value (fat)
    add rsp, 8                  ; pop key_tag
    dec qword [rbx + PyDictObject.ob_size]
    ; Bump version counter
    inc qword [rbx + PyDictObject.dk_version]
    cmp qword [rbx + PyDictObject.dk_version], 0
    jne .dd_ver_ok
    mov qword [rbx + PyDictObject.dk_version], 1
.dd_ver_ok:
    xor eax, eax               ; return 0 = success
    jmp .dd_done

.dd_next:
    inc rcx
    and rcx, r15
    inc r14
    jmp .dd_probe

.dd_not_found:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "key not found"
    call raise_exception
    ; raise_exception does not return

.dd_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
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
DEF_FUNC_BARE dict_iter_next
    mov rax, [rdi + PyDictIterObject.it_dict]      ; dict
    mov rcx, [rdi + PyDictIterObject.it_index]      ; current index
    mov rdx, [rax + PyDictObject.capacity]          ; capacity
    mov rsi, [rax + PyDictObject.entries]            ; entries ptr

.di_scan:
    cmp rcx, rdx
    jge .di_exhausted

    ; Check if entry at index has a key (key_tag != TAG_NULL)
    imul rax, rcx, DictEntry_size
    add rax, rsi
    mov r8, [rax + DictEntry.key]
    cmp qword [rax + DictEntry.key_tag], 0
    je .di_skip
    ; Also check value_tag (deleted entries have TAG_NULL value_tag)
    cmp qword [rax + DictEntry.value_tag], 0
    je .di_skip

    ; Found a valid entry — return the key with its tag
    inc rcx
    mov [rdi + PyDictIterObject.it_index], rcx
    mov edx, [rax + DictEntry.key_tag]  ; key tag from entry
    mov rax, r8
    ; INCREF key (tag-aware)
    INCREF_VAL rax, rdx
    ret

.di_skip:
    inc rcx
    jmp .di_scan

.di_exhausted:
    mov [rdi + PyDictIterObject.it_index], rcx
    RET_NULL
    ret
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
    ; edx already has key_tag, pass through to dict_get
    call dict_get
    test edx, edx
    jz .dc_no
    mov eax, 1
    ret
.dc_no:
    xor eax, eax
    ret
END_FUNC dict_contains

;; ============================================================================
;; Data section
;; ============================================================================
section .data

; dict_repr_str removed - repr now in src/repr.asm
dict_iter_name: db "dict_keyiterator", 0

dict_name_str: db "dict", 0

; Dict mapping methods
align 8
global dict_mapping_methods
dict_mapping_methods:
    dq dict_len                 ; mp_length
    dq dict_subscript           ; mp_subscript
    dq dict_ass_subscript       ; mp_ass_subscript

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
    dq 0                        ; tp_hash (unhashable)
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq dict_tp_iter             ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq dict_sequence_methods    ; tp_as_sequence (for 'in' operator)
    dq dict_mapping_methods     ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_DICT_SUBCLASS  ; tp_flags
    dq 0                        ; tp_bases

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
