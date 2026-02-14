; dict_obj.asm - Dict type implementation
; Phase 4: open-addressing hash table with linear probing

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

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

; Initial capacity (must be power of 2)
DICT_INIT_CAP equ 8

;; ============================================================================
;; dict_new() -> PyDictObject*
;; Allocate a new empty dict with initial capacity 8
;; ============================================================================
global dict_new
dict_new:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

;; ============================================================================
;; dict_keys_equal(PyObject *a, PyObject *b) -> int (1=equal, 0=not)
;; Internal helper: pointer equality or string comparison
;; ============================================================================
dict_keys_equal:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    ; Pointer equality check
    cmp rdi, rsi
    je .equal

    mov rbx, rdi                ; save a
    mov r12, rsi                ; save b

    ; Check if both are strings: a->ob_type == &str_type
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
    pop r12
    pop rbx
    pop rbp
    ret

.equal:
    mov eax, 1
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; dict_get(PyDictObject *dict, PyObject *key) -> PyObject* or NULL
;; Linear probing lookup
;; ============================================================================
global dict_get
dict_get:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; rbx = dict
    mov r12, rsi                ; r12 = key

    ; Hash the key
    mov rdi, r12
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

    ; Check if slot is empty (key == NULL)
    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .not_found

    ; Check hash first (fast reject)
    cmp r13, [rax + DictEntry.hash]
    jne .next_slot

    ; Hash matches - check key equality
    ; rdi already has entry.key
    mov rsi, r12                ; our key
    push rcx                    ; save slot
    push rax                    ; save entry ptr
    call dict_keys_equal
    pop rdx                     ; restore entry ptr into rdx
    pop rcx                     ; restore slot
    test eax, eax
    jz .next_slot

    ; Found - return entry.value
    mov rax, [rdx + DictEntry.value]
    jmp .done

.next_slot:
    ; Linear probe: slot = (slot + 1) & mask
    inc rcx
    and rcx, r15
    inc r14
    jmp .probe_loop

.not_found:
    xor eax, eax               ; return NULL

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; dict_find_slot(PyDictObject *dict, PyObject *key, int64_t hash)
;;   -> rax = entry ptr, rdx = 1 if existing key found, 0 if empty slot
;; Internal helper used by dict_set
;; ============================================================================
dict_find_slot:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; dict
    mov r12, rsi                ; key
    mov r13, rdx                ; hash

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

    ; Empty slot?
    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .found_empty

    ; Hash match?
    cmp r13, [rax + DictEntry.hash]
    jne .find_next

    ; Key equality check
    ; rdi = entry.key
    mov rsi, r12
    push rcx
    push rax
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
    pop rbp
    ret

.found_existing:
    ; rax = entry ptr, rdx = 1 (existing)
    mov edx, 1
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.table_full:
    ; Should never happen if load factor is maintained
    lea rdi, [rel .err_full]
    call fatal_error

section .rodata
.err_full: db "dict: hash table full", 0
section .text

;; ============================================================================
;; dict_resize(PyDictObject *dict)
;; Double capacity and rehash all entries
;; ============================================================================
dict_resize:
    push rbp
    mov rbp, rsp
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

    ; Skip empty slots
    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .rehash_next

    ; Compute new slot: hash & (new_capacity - 1)
    push rcx                    ; save outer index
    mov rcx, [rax + DictEntry.hash]
    mov rdx, r14
    dec rdx                     ; new mask
    and rcx, rdx                ; starting slot

    ; Save entry data
    push qword [rax + DictEntry.hash]
    push qword [rax + DictEntry.key]
    push qword [rax + DictEntry.value]

    ; Linear probe in new table to find empty slot
.rehash_probe:
    imul rax, rcx, DICT_ENTRY_SIZE
    add rax, r15                ; new entry ptr
    cmp qword [rax + DictEntry.key], 0
    je .rehash_insert

    inc rcx
    mov rax, r14
    dec rax
    and rcx, rax                ; slot = (slot+1) & new_mask
    jmp .rehash_probe

.rehash_insert:
    ; rax = target entry ptr in new table
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
    pop rbp
    ret

;; ============================================================================
;; dict_set(PyDictObject *dict, PyObject *key, PyObject *value)
;; Insert or update a key-value pair
;; ============================================================================
global dict_set
dict_set:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; dict
    mov r12, rsi                ; key
    mov r13, rdx                ; value

    ; Hash the key
    mov rdi, r12
    call obj_hash
    mov r14, rax                ; r14 = hash

    ; Find slot
    mov rdi, rbx                ; dict
    mov rsi, r12                ; key
    mov rdx, r14                ; hash
    call dict_find_slot
    ; rax = entry ptr, edx = 1 if existing, 0 if empty

    test edx, edx
    jnz .update_existing

    ; --- Insert new entry ---
    ; Store hash, key, value
    mov [rax + DictEntry.hash], r14
    mov [rax + DictEntry.key], r12
    mov [rax + DictEntry.value], r13

    ; INCREF key and value
    INCREF r12
    INCREF r13

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
    ; DECREF old value
    push rax                    ; save entry ptr
    mov rdi, [rax + DictEntry.value]
    call obj_decref
    pop rax                     ; restore entry ptr

    ; Store new value and INCREF it
    mov [rax + DictEntry.value], r13
    INCREF r13

.done:
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; dict_dealloc(PyObject *self)
;; Free all entries, then free dict
;; ============================================================================
global dict_dealloc
dict_dealloc:
    push rbp
    mov rbp, rsp
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

    ; Skip empty slots
    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .dealloc_next

    ; DECREF key
    push rax
    call obj_decref

    ; DECREF value
    pop rax
    mov rdi, [rax + DictEntry.value]
    call obj_decref

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
    pop rbp
    ret

;; ============================================================================
;; dict_len(PyObject *self) -> int64_t
;; Returns ob_size (number of items)
;; ============================================================================
dict_len:
    mov rax, [rdi + PyDictObject.ob_size]
    ret

;; ============================================================================
;; dict_subscript(PyDictObject *dict, PyObject *key) -> PyObject*
;; mp_subscript: look up key, fatal KeyError if not found
;; ============================================================================
global dict_subscript
dict_subscript:
    push rbp
    mov rbp, rsp
    push rbx

    mov rbx, rsi               ; save key for error msg

    ; dict_get(dict, key)
    call dict_get
    test rax, rax
    jz .key_error

    ; INCREF the returned value (dict_get returns borrowed ref)
    INCREF rax
    pop rbx
    pop rbp
    ret

.key_error:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "key not found"
    call raise_exception

;; ============================================================================
;; dict_ass_subscript(PyDictObject *dict, PyObject *key, PyObject *value)
;; mp_ass_subscript: set key=value in dict
;; ============================================================================
global dict_ass_subscript
dict_ass_subscript:
    ; Simply delegate to dict_set
    jmp dict_set

;; ============================================================================
;; dict_repr(PyObject *self) -> PyStrObject*
;; Returns "{...}" placeholder
;; ============================================================================
global dict_repr
dict_repr:
    extern str_from_cstr
    lea rdi, [rel dict_repr_str]
    jmp str_from_cstr

;; ============================================================================
;; Data section
;; ============================================================================
section .data

dict_repr_str: db "{...}", 0

dict_name_str: db "dict", 0

; Dict mapping methods
align 8
global dict_mapping_methods
dict_mapping_methods:
    dq dict_len                 ; mp_length
    dq dict_subscript           ; mp_subscript
    dq dict_ass_subscript       ; mp_ass_subscript

; Dict type object
align 8
global dict_type
dict_type:
    dq 1                        ; ob_refcnt (immortal)
    dq 0                        ; ob_type
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
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq dict_mapping_methods     ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_DICT_SUBCLASS  ; tp_flags
    dq 0                        ; tp_bases
