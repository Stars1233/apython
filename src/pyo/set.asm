; set.asm - Set type implementation
; Hash table with open-addressing and linear probing (no values, keys only)

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern obj_hash
extern obj_decref
extern obj_dealloc
extern obj_incref
extern str_type
extern ap_strcmp
extern ap_memset
extern fatal_error
extern str_from_cstr
extern type_type

; Set entry layout constants
SET_ENTRY_HASH    equ 0
SET_ENTRY_KEY     equ 8
SET_ENTRY_KEY_TAG equ 16
SET_ENTRY_SIZE    equ 24

; Initial capacity (must be power of 2)
SET_INIT_CAP equ 8

;; ============================================================================
;; set_new() -> PySetObject* (uses PyDictObject layout)
;; Allocate a new empty set with initial capacity 8
;; ============================================================================
DEF_FUNC set_new
    push rbx

    ; Allocate set header (reuses PyDictObject layout)
    mov edi, PyDictObject_size
    call ap_malloc
    mov rbx, rax                ; rbx = set

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel set_type]
    mov [rbx + PyObject.ob_type], rax
    mov qword [rbx + PyDictObject.ob_size], 0
    mov qword [rbx + PyDictObject.capacity], SET_INIT_CAP

    ; Allocate entries array: capacity * SET_ENTRY_SIZE
    mov edi, SET_INIT_CAP * SET_ENTRY_SIZE
    call ap_malloc
    mov [rbx + PyDictObject.entries], rax

    ; Zero out entries (NULL key = empty slot)
    mov rdi, rax
    xor esi, esi
    mov edx, SET_INIT_CAP * SET_ENTRY_SIZE
    call ap_memset

    mov rax, rbx
    pop rbx
    leave
    ret
END_FUNC set_new

;; ============================================================================
;; set_keys_equal(PyObject *a, PyObject *b, a_tag, b_tag) -> int (1=equal, 0=not)
;; Internal helper: pointer equality or string comparison
;; rdi=a, rsi=b, edx=a_tag, ecx=b_tag
;; ============================================================================
DEF_FUNC_LOCAL set_keys_equal
    push rbx
    push r12
    push r13
    push r14

    ; Pointer equality check
    cmp rdi, rsi
    je .equal

    mov rbx, rdi                ; save a
    mov r12, rsi                ; save b
    mov r13d, edx               ; save a_tag
    mov r14d, ecx               ; save b_tag

    ; Check SmallInt: both tagged?
    cmp r13d, TAG_SMALLINT
    jne .check_b_si
    cmp r14d, TAG_SMALLINT
    jne .not_equal              ; a is SmallInt, b is not

    ; Both SmallInts - compare directly
    cmp rbx, r12
    je .equal
    jmp .not_equal

.check_b_si:
    ; a is not SmallInt; if b is SmallInt, not equal
    cmp r14d, TAG_SMALLINT
    je .not_equal

    ; Neither is SmallInt - check if both are strings: a->ob_type == &str_type
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .not_equal

    ; Check b->ob_type == &str_type
    mov rax, [r12 + PyObject.ob_type]
    cmp rax, rcx
    jne .not_equal

    ; Both are strings - compare .data with strcmp
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .equal

.not_equal:
    xor eax, eax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.equal:
    mov eax, 1
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_keys_equal

;; ============================================================================
;; set_find_slot(set, key, hash, key_tag)
;;   rdi=set, rsi=key, rdx=hash, ecx=key_tag
;;   -> rax = entry ptr, rdx = 1 if existing key found, 0 if empty slot
;; Internal helper used by set_add and set_contains
;; ============================================================================
SFS_KEY_TAG equ 8
DEF_FUNC_LOCAL set_find_slot
    sub rsp, SFS_KEY_TAG
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; set
    mov r12, rsi                ; key
    mov r13, rdx                ; hash
    mov [rbp - SFS_KEY_TAG], ecx ; save key_tag

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

    ; entry = entries + slot * SET_ENTRY_SIZE
    mov rax, [rbx + PyDictObject.entries]
    imul rdx, rcx, SET_ENTRY_SIZE
    add rax, rdx                ; rax = entry ptr

    ; Empty slot? Check key_tag (TAG_NULL=0 means empty)
    mov rdi, [rax + SET_ENTRY_KEY]
    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .found_empty

    ; Hash match?
    cmp r13, [rax + SET_ENTRY_HASH]
    jne .find_next

    ; Key equality check
    ; rdi = entry.key (already loaded above)
    push rcx                    ; save slot
    push rax                    ; save entry ptr
    mov edx, [rax + SET_ENTRY_KEY_TAG]  ; a_tag (entry key)
    mov rsi, r12                        ; b = lookup key
    mov ecx, [rbp - SFS_KEY_TAG]        ; b_tag (lookup key tag)
    call set_keys_equal
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
    CSTRING rdi, "set: hash table full"
    call fatal_error
END_FUNC set_find_slot

;; ============================================================================
;; set_resize(set)
;; Double capacity and rehash all entries
;; ============================================================================
DEF_FUNC_LOCAL set_resize
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; set

    ; Save old entries and capacity
    mov r12, [rbx + PyDictObject.entries]    ; old entries
    mov r13, [rbx + PyDictObject.capacity]   ; old capacity

    ; New capacity = old * 2
    lea r14, [r13 * 2]          ; r14 = new capacity
    mov [rbx + PyDictObject.capacity], r14

    ; Allocate new entries array
    imul rdi, r14, SET_ENTRY_SIZE
    call ap_malloc
    mov r15, rax                ; r15 = new entries

    ; Zero new entries
    mov rdi, r15
    xor esi, esi
    imul rdx, r14, SET_ENTRY_SIZE
    call ap_memset

    ; Store new entries pointer
    mov [rbx + PyDictObject.entries], r15

    ; Rehash: iterate old entries, re-insert non-empty ones
    xor ecx, ecx               ; ecx = index into old entries

.rehash_loop:
    cmp rcx, r13                ; compared against old capacity
    jge .rehash_done

    ; old_entry = old_entries + i * SET_ENTRY_SIZE
    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12                ; rax = old entry ptr

    ; Skip empty slots (check key_tag for TAG_NULL=0)
    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .rehash_next

    ; Compute new slot: hash & (new_capacity - 1)
    push rcx                    ; save outer index
    mov rcx, [rax + SET_ENTRY_HASH]
    mov rdx, r14
    dec rdx                     ; new mask
    and rcx, rdx                ; starting slot

    ; Save entry data
    push qword [rax + SET_ENTRY_HASH]
    push qword [rax + SET_ENTRY_KEY]
    push qword [rax + SET_ENTRY_KEY_TAG]

    ; Linear probe in new table to find empty slot
.rehash_probe:
    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r15                ; new entry ptr
    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .rehash_insert

    inc rcx
    mov rax, r14
    dec rax
    and rcx, rax                ; slot = (slot+1) & new_mask
    jmp .rehash_probe

.rehash_insert:
    ; rax = target entry ptr in new table
    pop qword [rax + SET_ENTRY_KEY_TAG]
    pop qword [rax + SET_ENTRY_KEY]
    pop qword [rax + SET_ENTRY_HASH]

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
END_FUNC set_resize

;; ============================================================================
;; set_add(set, key, key_tag) -> void
;; Add a key to the set. rdx = key_tag.
;; ============================================================================
DEF_FUNC set_add
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; set
    mov r12, rsi                ; key
    mov r14, rdx                ; key_tag

    ; Hash the key
    mov rdi, r12
    mov rsi, r14                ; key_tag (saved from rdx on entry)
    call obj_hash
    mov r13, rax                ; r13 = hash

    ; Find slot
    mov rdi, rbx                ; set
    mov rsi, r12                ; key
    mov rdx, r13                ; hash
    mov ecx, r14d               ; key_tag
    call set_find_slot
    ; rax = entry ptr, edx = 1 if existing, 0 if empty

    test edx, edx
    jnz .done                   ; key already exists, do nothing

    ; --- Insert new entry ---
    ; Store hash and key
    mov [rax + SET_ENTRY_HASH], r13
    mov [rax + SET_ENTRY_KEY], r12

    ; Store key tag from caller
    mov [rax + SET_ENTRY_KEY_TAG], r14

    ; INCREF key (tag-aware)
    INCREF_VAL r12, r14

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
    call set_resize

.done:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_add

;; ============================================================================
;; set_contains(set, key) -> int (0/1)
;; Check if key is in the set
;; ============================================================================
DEF_FUNC set_contains
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; set
    mov r12, rsi                ; key
    mov r14d, edx               ; key_tag

    ; Hash the key
    mov rdi, r12
    mov esi, r14d               ; key_tag
    call obj_hash
    mov r13, rax                ; r13 = hash

    ; Find slot
    mov rdi, rbx                ; set
    mov rsi, r12                ; key
    mov rdx, r13                ; hash
    mov ecx, r14d               ; key_tag
    call set_find_slot
    ; rax = entry ptr, edx = 1 if found, 0 if empty slot

    mov eax, edx                ; return 1 if found, 0 if not

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_contains

;; ============================================================================
;; set_contains_sq(self, key) -> int (0/1)
;; sq_contains wrapper for the sequence methods (for "in" operator)
;; ============================================================================
DEF_FUNC_BARE set_contains_sq
    jmp set_contains
END_FUNC set_contains_sq

;; ============================================================================
;; set_remove(set, key) -> int (0=ok, -1=not found)
;; Remove a key from the set
;; ============================================================================
SR_KEY_TAG equ 8
DEF_FUNC set_remove, SR_KEY_TAG
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; set
    mov r12, rsi                ; key
    mov [rbp - SR_KEY_TAG], edx ; save key_tag

    ; Hash the key
    mov rdi, r12
    mov esi, edx                ; key_tag (passed by caller in rdx)
    call obj_hash
    mov r13, rax                ; hash

    ; capacity mask
    mov r14, [rbx + PyDictObject.capacity]
    lea r15, [r14 - 1]          ; mask

    ; Starting slot
    mov rcx, r13
    and rcx, r15
    xor r14d, r14d              ; probe counter

.sr_probe:
    cmp r14, [rbx + PyDictObject.capacity]
    jge .sr_not_found

    mov rax, [rbx + PyDictObject.entries]
    imul rdx, rcx, SET_ENTRY_SIZE
    add rax, rdx

    mov rdi, [rax + SET_ENTRY_KEY]
    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .sr_not_found

    cmp r13, [rax + SET_ENTRY_HASH]
    jne .sr_next

    ; rdi = entry.key (already loaded)
    push rcx                    ; save slot
    push rax                    ; save entry ptr
    mov edx, [rax + SET_ENTRY_KEY_TAG]  ; a_tag (entry key)
    mov rsi, r12                        ; b = lookup key
    mov ecx, [rbp - SR_KEY_TAG]         ; b_tag (lookup key tag)
    call set_keys_equal
    pop rdx                     ; entry ptr
    pop rcx
    test eax, eax
    jz .sr_next

    ; Found: null out entry, DECREF key, decrement size
    mov rdi, [rdx + SET_ENTRY_KEY]
    mov rsi, [rdx + SET_ENTRY_KEY_TAG]
    mov qword [rdx + SET_ENTRY_KEY], 0
    mov qword [rdx + SET_ENTRY_KEY_TAG], 0
    DECREF_VAL rdi, rsi
    dec qword [rbx + PyDictObject.ob_size]
    xor eax, eax               ; return 0 = success
    jmp .sr_done

.sr_next:
    inc rcx
    and rcx, r15
    inc r14
    jmp .sr_probe

.sr_not_found:
    mov eax, -1

.sr_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_remove

;; ============================================================================
;; set_dealloc(PyObject *self)
;; Free all entries, then free set
;; ============================================================================
DEF_FUNC set_dealloc
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; self (set)
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor r14d, r14d              ; index

.dealloc_loop:
    cmp r14, r13
    jge .dealloc_entries_done

    ; entry = entries + index * SET_ENTRY_SIZE
    imul rax, r14, SET_ENTRY_SIZE
    add rax, r12

    ; Skip empty slots (check key_tag for TAG_NULL=0)
    mov rdi, [rax + SET_ENTRY_KEY]
    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .dealloc_next

    ; DECREF key (fat value)
    mov rsi, [rax + SET_ENTRY_KEY_TAG]
    DECREF_VAL rdi, rsi

.dealloc_next:
    inc r14
    jmp .dealloc_loop

.dealloc_entries_done:
    ; Free entries array
    mov rdi, r12
    call ap_free

    ; Free set object itself
    mov rdi, rbx
    call ap_free

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_dealloc

;; ============================================================================
;; set_type_call(self, args, nargs) -> new set
;; Constructor: set() or set(iterable)
;; self = set_type, args = arg array, nargs = count
;; ============================================================================
extern raise_exception
extern exc_TypeError_type

STC_FRAME equ 8
DEF_FUNC set_type_call, STC_FRAME
    push rbx
    push r12

    ; nargs can be 0 or 1
    cmp rdx, 0
    je .stc_empty
    cmp rdx, 1
    jne .stc_error

    ; set(iterable): create set, iterate and add
    mov r12, [rsi]          ; iterable payload
    mov ecx, [rsi + 8]     ; iterable tag

    call set_new
    mov rbx, rax            ; rbx = new set

    ; Get iterator: tp_iter(iterable)
    mov rdi, r12
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .stc_not_iterable
    call rax
    mov r12, rax            ; r12 = iterator

.stc_iter_loop:
    ; Get next: tp_iternext(iterator)
    mov rdi, r12
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    test edx, edx           ; check tag (NULL = exhausted)
    jz .stc_iter_done

    ; Add to set
    mov rdi, rbx            ; set
    mov rsi, rax            ; key payload
    ; edx = key tag (from tp_iternext)
    push rdx
    call set_add
    pop rdx                 ; balance (set_add may clobber)
    jmp .stc_iter_loop

.stc_iter_done:
    ; DECREF iterator
    mov rdi, r12
    call obj_decref

    mov rax, rbx            ; return new set
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.stc_empty:
    call set_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.stc_not_iterable:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "set() argument is not iterable"
    call raise_exception

.stc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "set() takes at most 1 argument"
    call raise_exception
END_FUNC set_type_call

; set_repr is in src/repr.asm
extern set_repr

;; ============================================================================
;; set_len(PyObject *self) -> int64_t
;; Returns ob_size (number of items)
;; ============================================================================
DEF_FUNC_BARE set_len
    mov rax, [rdi + PyDictObject.ob_size]
    ret
END_FUNC set_len

;; ============================================================================
;; set_tp_iter(set) -> SetIterObject*
;; Create a new set iterator.
;; rdi = set
;; ============================================================================
DEF_FUNC set_tp_iter
    push rbx

    mov rbx, rdi               ; save set

    ; Reuse PyDictIterObject layout (same structure: refcnt, type, source, index)
    mov edi, PyDictIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel set_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyDictIterObject.it_dict], rbx     ; store set ptr
    mov qword [rax + PyDictIterObject.it_index], 0
    mov qword [rax + PyDictIterObject.it_kind], 0

    ; INCREF the set
    push rax
    mov rdi, rbx
    call obj_incref
    pop rax

    pop rbx
    leave
    ret
END_FUNC set_tp_iter

;; ============================================================================
;; set_iter_next(iter) -> PyObject* or NULL
;; Return next key, or NULL if exhausted.
;; Scans entries for next non-empty slot.
;; rdi = iterator
;; ============================================================================
DEF_FUNC_BARE set_iter_next
    mov rax, [rdi + PyDictIterObject.it_dict]      ; set
    mov rcx, [rdi + PyDictIterObject.it_index]      ; current index
    mov rdx, [rax + PyDictObject.capacity]          ; capacity
    mov rsi, [rax + PyDictObject.entries]            ; entries ptr

.si_scan:
    cmp rcx, rdx
    jge .si_exhausted

    ; Check if entry at index has a key
    imul rax, rcx, SET_ENTRY_SIZE
    add rax, rsi
    mov r8, [rax + SET_ENTRY_KEY]
    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .si_skip

    ; Found a valid entry -- return the key with tag
    inc rcx
    mov [rdi + PyDictIterObject.it_index], rcx
    mov edx, [rax + SET_ENTRY_KEY_TAG]  ; key tag
    mov rax, r8
    INCREF_VAL rax, edx
    ret

.si_skip:
    inc rcx
    jmp .si_scan

.si_exhausted:
    mov [rdi + PyDictIterObject.it_index], rcx
    RET_NULL
    ret
END_FUNC set_iter_next

;; ============================================================================
;; set_iter_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL set_iter_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF the set
    mov rdi, [rbx + PyDictIterObject.it_dict]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC set_iter_dealloc

;; ============================================================================
;; set_iter_self(PyObject *self) -> self with INCREF
;; ============================================================================
set_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC set_iter_self

;; ============================================================================
;; Data section
;; ============================================================================
section .data

; set_repr_str removed - repr now in src/repr.asm
set_iter_name: db "set_iterator", 0

set_name_str: db "set", 0

; Set sequence methods (for sq_contains -> "in" operator, and sq_length -> len())
align 8
global set_seq_methods
set_seq_methods:
    dq set_len                  ; sq_length       +0
    dq 0                        ; sq_concat       +8
    dq 0                        ; sq_repeat       +16
    dq 0                        ; sq_item          +24
    dq 0                        ; sq_ass_item      +32
    dq set_contains_sq          ; sq_contains      +40
    dq 0                        ; sq_inplace_concat +48
    dq 0                        ; sq_inplace_repeat +56

; Set type object
align 8
global set_type
set_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq set_name_str             ; tp_name
    dq PyDictObject_size        ; tp_basicsize (reuse dict layout)
    dq set_dealloc              ; tp_dealloc
    dq set_repr                 ; tp_repr
    dq set_repr                 ; tp_str
    dq 0                        ; tp_hash (unhashable)
    dq set_type_call            ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq set_tp_iter              ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq set_seq_methods          ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

; Set iterator type
align 8
global set_iter_type
set_iter_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq set_iter_name            ; tp_name
    dq PyDictIterObject_size    ; tp_basicsize
    dq set_iter_dealloc         ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq set_iter_self            ; tp_iter (return self)
    dq set_iter_next            ; tp_iternext
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
