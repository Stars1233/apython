; set.asm - Set type implementation
; Hash table with open-addressing and linear probing (no values, keys only)

%include "macros.inc"
%include "object.inc"

extern get_iterator_opt
extern list_type
extern tuple_type
extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_hash
extern obj_decref
extern obj_dealloc
extern obj_incref
extern str_type
extern eval_exception_unwind
extern obj_richcompare_bool
extern ap_memcpy
extern ap_memset
extern fatal_error
extern type_type

; Set entry layout constants
SET_ENTRY_HASH    equ 0
SET_ENTRY_KEY     equ 8

; Initial capacity (must be power of 2)
SET_INIT_CAP equ 8

; SET_HASH_VALUE key, out -- the hash of a key Value, with the int case inline.
;
; Every one of the three sites that needed a hash used to V_UNPACK the Value,
; V_PACK the identical word straight back and call obj_hash, whose first
; instruction is another V_UNPACK -- and whose whole answer for an int
; immediate is the value itself, because |n| < 2^50 is below PYHASH_MODULUS
; and int_hash_i64's `.ihi_small` arm returns it unchanged.  Only -1 is
; special, and only because hash(-1) is reserved for "error".  Nothing in set
; can observe that fixup today -- every insert and every probe hashes through
; here, and set_resize reuses the stored hash -- but it is what keeps this and
; obj_hash from ever disagreeing about the same key, which is the invariant
; the entries rely on.
;
; This is set's parallel of dict_lookup's cached string hash: n-queens hashes
; nothing but ints, and the call was most of what `c in cols` cost.
%macro SET_HASH_VALUE 2         ; %1 = key Value (preserved), %2 = the hash out
    V_IS_INT %1, %2
    jb %%slow
    mov %2, %1
    V_TO_I64 %2
    cmp %2, -1
    jne %%done
    mov %2, -2
    jmp %%done
%%slow:
    mov rdi, %1
    call obj_hash
    mov %2, rax
%%done:
%endmacro

; Tombstone marker for deleted entries (must not collide with any tag)

;; ============================================================================
;; set_new() -> PySetObject* (uses PyDictObject layout)
;; Allocate a new empty set with initial capacity 8
;; ============================================================================
DEF_FUNC_BARE set_new
    lea rdi, [rel set_type]
    jmp set_new_of_type
END_FUNC set_new

;; ============================================================================
;; set_new_of_type(rdi = set_type or frozenset_type) -> PySetObject*
;;
;; The two share every field of the layout and differ only in ob_type, so the
;; operators can hand the result the type the left operand had.  They used to
;; call set_new unconditionally, which is why frozenset({1}) | frozenset({2})
;; was a set.
;; ============================================================================
DEF_FUNC set_new_of_type
    push rbx
    push r12
    mov r12, rdi                ; the type to build

    ; Allocate set header (GC-tracked, reuses PyDictObject layout)
    mov edi, PyDictObject_size
    mov rsi, r12
    call gc_alloc
    mov rbx, rax                ; rbx = set (ob_refcnt=1, ob_type set)

    mov qword [rbx + PyDictObject.ob_size], 0
    mov qword [rbx + PyDictObject.capacity], SET_INIT_CAP
    mov qword [rbx + PyDictObject.dk_version], 0
    mov qword [rbx + PyDictObject.dk_tombstones], 0
    mov qword [rbx + SET_FINGER], 0

    ; Allocate entries array: capacity * SET_ENTRY_SIZE
    mov edi, SET_INIT_CAP * SET_ENTRY_SIZE
    call ap_malloc
    mov [rbx + PyDictObject.entries], rax

    ; Zero out entries (NULL key = empty slot)
    mov rdi, rax
    xor esi, esi
    mov edx, SET_INIT_CAP * SET_ENTRY_SIZE
    call ap_memset

    mov rdi, rbx
    call gc_track

    mov rax, rbx
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_new_of_type

;; ============================================================================
;; set_result_type(rdi = a set or frozenset) -> rax = the type a derived set
;; should be built with: set_type or frozenset_type.
;;
;; CPython's rule for the operators and for copy(): the result takes the LEFT
;; operand's kind, and a subclass of either yields the plain base rather than
;; the subclass -- frozenset({1}).copy() is a frozenset, and F({1}).copy() for
;; a frozenset subclass F is a frozenset too.
;; ============================================================================
DEF_FUNC_BARE set_result_type
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    je .srt_frozen
    lea rcx, [rel set_type]
    cmp rax, rcx
    je .srt_plain
    ; A subclass: which base does it descend from?  frozenset_type carries
    ; TYPE_FLAG_SET_SUBCLASS itself, so the flag cannot answer this; the MRO
    ; can.
    push rdi
    mov rdi, rax
    lea rsi, [rel frozenset_type]
    extern type_is_subtype
    call type_is_subtype
    pop rdi
    test eax, eax
    jnz .srt_frozen
.srt_plain:
    lea rax, [rel set_type]
    ret
.srt_frozen:
    lea rax, [rel frozenset_type]
    ret
END_FUNC set_result_type

;; ============================================================================
;; set_coerce_operand(rdi = the other operand, a Value)
;;   -> rax = an OWNED set-like pointer, or 0 with the exception pending
;;
;; set.union, .intersection, .difference and .symmetric_difference take any
;; iterable, where the operators take only a set.  All four read the argument
;; as a PyDictObject regardless, so `{1,2}.difference([1])` walked a list's
;; header as a hash table, found nothing occupied, and answered {1,2} -- a
;; silently wrong answer rather than a crash.  A set argument is handed back
;; INCREF'd so the caller can release its operand unconditionally.
;; ============================================================================
SCO_ARG   equ 8         ; a one-Value args array for set_type_call
SCO_FRAME equ 16            ; + 0 pushes = 16, 16-aligned

DEF_FUNC set_coerce_operand, SCO_FRAME
    V_TEST_PTR rdi, rax
    ja .sco_build
    test rdi, rdi
    jz .sco_build
    mov rax, [rdi + PyObject.ob_type]
    REQUIRE_SET_TYPE rax, rcx, .sco_build
    mov rax, rdi
    INCREF rax
    leave
    ret

.sco_build:
    mov [rbp - SCO_ARG], rdi
    lea rsi, [rbp - SCO_ARG]
    lea rdi, [rel set_type]
    mov edx, 1
    call set_type_call          ; raises for a non-iterable, NULL if it threw
    leave
    ret
END_FUNC set_coerce_operand

;; ============================================================================
;; frozenset_hash(rdi = self, edx = tag) -> rax = the hash
;;
;; frozenset_type.tp_hash was 0, so a frozenset could not be a dict key or a
;; set member -- the very things it exists for.  This is CPython's own
;; frozenset_hash, over the per-entry hashes the table already stores: XOR
;; makes it order-insensitive, which is what set equality requires of it.
;; ============================================================================
global frozenset_hash
DEF_FUNC frozenset_hash
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor r8d, r8d                ; the accumulator; nothing here calls out
    xor ecx, ecx

.fsh_loop:
    cmp rcx, r13
    jge .fsh_length
    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    cmp qword [rax + SET_ENTRY_KEY], 0      ; occupied?
    je .fsh_next

    ; h ^= ((h ^ 89869747) ^ (h << 16)) * 3644798167
    mov rdx, [rax + SET_ENTRY_HASH]
    mov rsi, rdx
    mov edi, 89869747
    xor rsi, rdi
    mov rdi, rdx
    shl rdi, 16
    xor rsi, rdi
    mov rdi, 3644798167
    imul rsi, rdi
    xor r8, rsi

.fsh_next:
    inc rcx
    jmp .fsh_loop

.fsh_length:
    ; Fold the size in, so frozensets of different sizes do not collide as
    ; readily, then scramble once more.
    mov rax, [rbx + PyDictObject.ob_size]
    inc rax
    mov edi, 1927868237
    imul rax, rdi
    xor r8, rax

    mov rax, r8
    mov edi, 69069
    imul rax, rdi
    mov edi, 907133923
    add rax, rdi

    cmp rax, -1                 ; -1 is the error sentinel everywhere else
    jne .fsh_ret
    mov eax, 590923713

.fsh_ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC frozenset_hash

;; ============================================================================
;; set_keys_equal(rdi = a Value, rsi = b Value) -> eax = 1 if equal, 0 if not
;;
;; Was an identity check plus a string compare, and nothing else: no
;; cross-type numeric equality, so 1.0 in {1} was False where the same
;; lookup in a dict succeeded, and no __eq__, so a user class could never
;; find its own key.  Set membership is PyObject_RichCompareBool, the same
;; as everywhere else.
;;
;; It took a (payload, tag) pair and packed both back, which was the only
;; reason set_find_slot unpacked them -- the entries hold Values and so does
;; the probe key.  Now it is only reached when the two Values DIFFER, which
;; rules out every int and every identity hit before the call is made.
;; ============================================================================
DEF_FUNC_LOCAL set_keys_equal
    mov edx, PY_EQ
    call obj_richcompare_bool
    cmp eax, -1
    je .ske_error
    leave
    ret

.ske_error:
    ; The caller has no error channel; the exception is already pending.
    leave
    jmp eval_exception_unwind
END_FUNC set_keys_equal

;; ============================================================================
;; set_find_slot(rdi = the set, rsi = the key Value, rdx = its hash)
;;   -> rax = entry ptr, rdx = 1 if the key is there, 0 if the slot is free
;;
;; Internal helper used by set_add and set_contains.
;;
;; A slot freed by set_remove holds a TOMBSTONE: key 0, hash -1.  The probe
;; cannot stop there -- the key it is looking for may sit further along the
;; run that the tombstone is part of -- but the slot is reusable, and this
;; remembers the first one and hands it back if the probe reaches a slot that
;; was never used at all.
;;
;; Skipping tombstones without remembering them meant every insert consumed a
;; fresh EMPTY slot, so a set that is added to and removed from in equal
;; measure only ever filled up.  With few distinct keys -- which all land in
;; one narrow band of slots -- the dead entries piled into a single
;; linear-probe run and every later add walked the whole of it: n-queens with
;; three such sets ran 500x slower than CPython and grew the tables without
;; bound.  dict never had this; its probe has always remembered the first
;; reusable slot (dl_probe's DL_FREE).
;; ============================================================================
SFS_FREE    equ 8               ; first tombstone seen on this probe, or 0
SFS_ENTRIES equ 16              ; the entry array the probe is walking
SFS_FRAME   equ 24              ; 24 + 5 pushes keeps rsp 16-aligned
DEF_FUNC_LOCAL set_find_slot
    sub rsp, SFS_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; set
    mov r12, rsi                ; the key, a Value
    mov r13, rdx                ; hash

.sfs_restart:
    mov qword [rbp - SFS_FREE], 0   ; no reusable slot seen yet

    ; r14 = probes REMAINING, counting down.  It was a count UP compared
    ; against a capacity reloaded from the set header on every iteration, for
    ; a bound the load factor already makes unreachable -- the same thing
    ; dict_lookup's probe was fixed for.
    mov r14, [rbx + PyDictObject.capacity]
    mov r15, r14
    dec r15                     ; mask

    ; slot = hash & mask
    mov rcx, r13
    and rcx, r15

.find_loop:
    dec r14
    js .table_full

    ; entry = entries + slot * SET_ENTRY_SIZE.  SET_ENTRY_SIZE is 16, which no
    ; index scale reaches, but two lea do -- and without imul's latency in the
    ; middle of the recurrence.
    mov rax, [rbx + PyDictObject.entries]
    lea rdx, [rcx + rcx]
    lea rax, [rax + rdx*8]

    SET_ENTRY_CLASSIFY rax, .found_empty, .find_tombstone

    ; Hash match?
    cmp r13, [rax + SET_ENTRY_HASH]
    jne .find_next

    ; The stored key is a Value and so is the probe key, and the encoding is a
    ; bijection: equal Values are the same object, and two equal small ints
    ; have bit-identical Values.  So `cmp` answers for every int, every
    ; interned str and every identity hit -- where this used to V_UNPACK the
    ; entry, call set_keys_equal, V_PACK both operands back and call
    ; obj_richcompare_bool, two call/ret pairs and about seventy instructions
    ; to conclude what one compare does.  dict_lookup has had its inline
    ; compare since a52b70d; set was left out of it.
    mov rdi, [rax + SET_ENTRY_KEY]
    cmp rdi, r12
    je .found_existing

    ; Different Values still need the real question asked: 1.0 == 1, and a
    ; user class decides for itself.
    mov rdx, [rbx + PyDictObject.entries]
    mov [rbp - SFS_ENTRIES], rdx
    push rcx                    ; save slot
    push rax                    ; save entry ptr
    mov rsi, r12                ; b = the lookup key
    call set_keys_equal
    mov edi, eax                ; save equality result (survives pops)
    pop rax                     ; entry ptr
    pop rcx                     ; slot

    ; __eq__ is arbitrary Python and may have added to THIS set: a resize
    ; frees the entry array and rehashes into a new one, which leaves the
    ; entry pointer just restored dangling and the mask, the probe budget and
    ; the remembered free slot all describing a table that no longer exists.
    ; The probe starts again rather than trusting any of it -- a set whose
    ; keys collide and whose __eq__ grows it used to walk the freed array and
    ; end at fatal_error("set: hash table full").
    mov rdx, [rbx + PyDictObject.entries]
    cmp rdx, [rbp - SFS_ENTRIES]
    jne .sfs_restart
    test edi, edi
    jnz .found_existing
    ; An occupied slot holding a DIFFERENT key.  Keep probing -- and jump,
    ; rather than falling through: the arm below records a reusable slot, and
    ; a live entry recorded there is handed back as free by .found_empty and
    ; then overwritten.  That reads as a set whose len() counts a key its
    ; iteration no longer contains.
    jmp .find_next

.find_tombstone:
    ; Remember the FIRST one and keep probing.  Stopping here would insert a
    ; duplicate of a key that is still live further along the run.
    cmp qword [rbp - SFS_FREE], 0
    jne .find_next
    mov [rbp - SFS_FREE], rax

.find_next:
    inc rcx
    and rcx, r15
    jmp .find_loop

.found_empty:
    ; A never-used slot ends the probe: the key is not in the table.  If a
    ; tombstone was passed on the way, hand that one back instead, so the
    ; insert reuses a dead slot rather than consuming a live one.
    mov rdx, [rbp - SFS_FREE]
    test rdx, rdx
    cmovnz rax, rdx
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
    ; No never-used slot anywhere.  That is only fatal if there was no
    ; reusable one either -- a table made entirely of live entries.  The load
    ; factor is meant to prevent it; reusing a tombstone here is what makes
    ; the claim true rather than merely intended.
    mov rax, [rbp - SFS_FREE]
    test rax, rax
    jz .really_full
    xor edx, edx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.really_full:
    CSTRING rdi, "set: hash table full"
    call fatal_error
END_FUNC set_find_slot

;; ============================================================================
;; set_resize_to(rdi = set, rsi = the new capacity) -> void
;; Rebuild the table at the capacity asked for, rehashing nothing: every
;; entry carries the hash it was stored with.
;; ============================================================================
DEF_FUNC_LOCAL set_resize_to, 8         ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; set
    mov r14, rsi                ; the capacity asked for

    ; Save old entries and capacity
    mov r12, [rbx + PyDictObject.entries]    ; old entries
    mov r13, [rbx + PyDictObject.capacity]   ; old capacity

    mov [rbx + PyDictObject.capacity], r14
    mov qword [rbx + PyDictObject.dk_tombstones], 0  ; rehash clears tombstones
    mov qword [rbx + SET_FINGER], 0     ; and the table it indexed is gone

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

    ; Skip slots that are not occupied
    SET_ENTRY_CLASSIFY rax, .rehash_next, .rehash_next

    ; Compute new slot: hash & (new_capacity - 1)
    push rcx                    ; save outer index
    mov rcx, [rax + SET_ENTRY_HASH]
    mov rdx, r14
    dec rdx                     ; new mask
    and rcx, rdx                ; starting slot

    ; Save entry data
    push qword [rax + SET_ENTRY_HASH]
    push qword [rax + SET_ENTRY_KEY]

    ; Linear probe in new table to find empty slot
.rehash_probe:
    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r15                ; new entry ptr
    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .rehash_insert

    inc rcx
    mov rax, r14
    dec rax
    and rcx, rax                ; slot = (slot+1) & new_mask
    jmp .rehash_probe

.rehash_insert:
    ; rax = target entry ptr in new table
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
END_FUNC set_resize_to

;; ============================================================================
;; set_resize(rdi = set) -> void
;; Rebuild at twice the capacity, which is what an insert that has run out of
;; room wants.
;; ============================================================================
DEF_FUNC_BARE set_resize
    mov rsi, [rdi + PyDictObject.capacity]
    add rsi, rsi
    jmp set_resize_to
END_FUNC set_resize

;; ============================================================================
;; set_reserve(rdi = set, rsi = how many more elements are coming) -> void
;;
;; Grow ONCE, so that a bulk build of a known size does not rebuild the table
;; on the way.  Every bulk path started at eight slots and rehashed at 7, 14,
;; 28, 56...: BUILD_SET (which is handed the element count), both
;; constructors, update, copy and all four binary operators.  dict_reserve
;; does the same job for dicts and for the same reason.
;;
;; The table holds ob_size + tombstones at three quarters of capacity, so the
;; room needed is that many slots rounded up to a power of two.  A set that
;; already has the room is left alone.
;; ============================================================================
global set_reserve
DEF_FUNC_BARE set_reserve
    mov rax, [rdi + PyDictObject.ob_size]
    add rax, [rdi + PyDictObject.dk_tombstones]
    add rax, rsi
    mov rcx, [rdi + PyDictObject.capacity]
    mov rdx, rcx
    shr rdx, 2
    lea rdx, [rdx + rdx*2]      ; capacity * 3/4
    cmp rax, rdx
    jle .srv_done               ; the room is already there
    cmp rcx, SET_INIT_CAP
    jae .srv_grow
    mov ecx, SET_INIT_CAP
.srv_grow:
    mov rdx, rcx
    shr rdx, 2
    lea rdx, [rdx + rdx*2]
    cmp rax, rdx
    jle .srv_resize
    add rcx, rcx
    jmp .srv_grow
.srv_resize:
    mov rsi, rcx
    jmp set_resize_to
.srv_done:
    ret
END_FUNC set_reserve

;; ============================================================================
;; set_clone_into(rdi = a fresh empty set, rsi = the source set) -> void
;;
;; Copy the source's table wholesale instead of re-inserting its elements.
;; The destination takes the source's CAPACITY, so the entry array transfers
;; verbatim -- tombstones included, because in a flat table an entry's slot IS
;; its probe position and a tombstone is what keeps a chain alive.  Not one
;; key is hashed and not one slot is probed.
;;
;; copy() and the self-half of `a | b` used to walk every slot and call
;; set_add per element, which is a hash, a probe, a load-factor test and a
;; possible resize each.  dict_copy_shallow was changed the same way and for
;; the same reason.
;;
;; The caller owns the destination's type; this touches only the table.
;; ============================================================================
global set_clone_into
DEF_FUNC set_clone_into, 16             ; + 4 pushes = 48, 16-aligned
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; dst
    mov r12, rsi                ; src

    cmp qword [r12 + PyDictObject.ob_size], 0
    je .sci_done                ; nothing to carry; the default table is right

    mov rdi, rbx
    mov rsi, [r12 + PyDictObject.capacity]
    call set_resize_to          ; the same shape, so the slots line up

    mov rdi, [rbx + PyDictObject.entries]
    mov rsi, [r12 + PyDictObject.entries]
    mov rdx, [r12 + PyDictObject.capacity]
    shl rdx, 4                  ; * SET_ENTRY_SIZE
    call ap_memcpy

    mov rax, [r12 + PyDictObject.ob_size]
    mov [rbx + PyDictObject.ob_size], rax
    mov rax, [r12 + PyDictObject.dk_tombstones]
    mov [rbx + PyDictObject.dk_tombstones], rax

    ; One reference for each key the copy now holds.  A tombstone carries a
    ; zero key and owns nothing.
    mov r13, [rbx + PyDictObject.entries]
    mov r14, [rbx + PyDictObject.capacity]
    shl r14, 4
    add r14, r13
.sci_loop:
    cmp r13, r14
    jae .sci_done
    mov rax, [r13 + SET_ENTRY_KEY]
    test rax, rax
    jz .sci_next
    INCREF_V rax, rcx
.sci_next:
    add r13, SET_ENTRY_SIZE
    jmp .sci_loop

.sci_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_clone_into

;; ============================================================================
;; set_add(set, key, key_tag) -> void
;; set_add(rdi=set, rsi=key Value) -> int
;; ============================================================================
DEF_FUNC set_add
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; set
    mov r12, rsi                ; the key, a Value

    SET_HASH_VALUE r12, r13     ; r13 = hash

    ; Find slot
    mov rdi, rbx                ; set
    mov rsi, r12                ; the key
    mov rdx, r13                ; hash
    call set_find_slot
    ; rax = entry ptr, edx = 1 if existing, 0 if empty

    test edx, edx
    jnz .done                   ; key already exists, do nothing

    ; --- Insert new entry ---
    ; The slot may be a reused tombstone rather than a never-used one; both
    ; arrive here with key 0, and only the tombstone carries hash -1.  Taking
    ; one back has to be counted, because dk_tombstones feeds the load factor
    ; below -- left standing it would resize a table with room to spare, and
    ; the capacity would climb on every add.
    cmp qword [rax + SET_ENTRY_HASH], ENTRY_TOMBSTONE_HASH
    jne .fresh_slot
    dec qword [rbx + PyDictObject.dk_tombstones]
.fresh_slot:
    ; Store the hash and the key; the entries hold Values, which is what the
    ; key already is.
    mov [rax + SET_ENTRY_HASH], r13
    INCREF_V r12, rcx
    mov [rax + SET_ENTRY_KEY], r12

    ; Increment ob_size
    inc qword [rbx + PyDictObject.ob_size]

    ; Check load factor: (ob_size + tombstones) > capacity * 3/4
    mov rax, [rbx + PyDictObject.capacity]
    mov rcx, rax
    shr rcx, 2                  ; capacity / 4
    imul rcx, rcx, 3            ; capacity * 3/4
    mov rax, [rbx + PyDictObject.ob_size]
    add rax, [rbx + PyDictObject.dk_tombstones]
    cmp rax, rcx
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
;; set_contains(rdi = the set, rsi = the key Value) -> eax = 0 or 1
;;
;; A SET is unhashable, but `set() in {1, 2}` is False in CPython rather
;; than a TypeError: its set_contains retries with a frozenset built from
;; the key, because that is the only thing the set could be holding.  A
;; frozenset built here is the same test done up front.
;; ============================================================================
SCT_TMP   equ 8             ; the frozenset standing in for a set key, or 0
SCT_KEY   equ 16            ; the key Value being looked up
SCT_FRAME equ 32            ; + 4 pushes = 64, 16-aligned
DEF_FUNC set_contains, SCT_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov qword [rbp - SCT_TMP], 0
    V_TEST_PTR rsi, rax
    ja .sct_not_set
    test rsi, rsi
    jz .sct_not_set
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel set_type]
    cmp rax, rcx
    jne .sct_not_set
    mov [rbp - SCT_KEY], rsi
    push rdi
    lea rsi, [rbp - SCT_KEY]
    lea rdi, [rel frozenset_type]
    mov edx, 1
    call frozenset_type_call
    pop rdi
    V_UNPACK rax, rdx
    test rax, rax
    jz .sct_failed
    mov [rbp - SCT_TMP], rax
    mov rsi, rax
.sct_not_set:

    mov rbx, rdi                ; set
    mov r12, rsi                ; the key, a Value

    SET_HASH_VALUE r12, r13     ; r13 = hash

    ; Find slot
    mov rdi, rbx                ; set
    mov rsi, r12                ; the key
    mov rdx, r13                ; hash
    call set_find_slot
    ; rax = entry ptr, edx = 1 if found, 0 if empty slot

    mov eax, edx                ; return 1 if found, 0 if not

    ; The frozenset built for a set key, if there was one.
    mov rdi, [rbp - SCT_TMP]
    test rdi, rdi
    jz .sct_out
    push rax
    call obj_decref
    pop rax
.sct_out:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.sct_failed:
    xor eax, eax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

END_FUNC set_contains

;; ============================================================================
;; set_richcompare(self, other, op, self_tag, other_tag) -> (rax, edx) fat value
;; Compares two sets. Only PY_EQ and PY_NE implemented.
;; ============================================================================
SRC_SELF  equ 8
SRC_OTHER equ 16
SRC_OP    equ 24
; The key each of the three subset walks below probes with is BORROWED from
; the table it is walking, and set_contains runs __eq__ -- arbitrary Python,
; which may clear that set and take the key's last reference with it.  Held
; here for the turn, the way CPython holds it.
SRC_KEY   equ 32
SRC_IDX   equ 40
SRC_FRAME equ 48            ; + 3 pushes = 72... see the pad below
DEF_FUNC set_richcompare, SRC_FRAME
    V_UNPACK rdi, rcx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, r8            ; right Value -> (payload, tag)
    push rbx
    push r12
    push r13

    mov [rbp - SRC_SELF], rdi
    mov [rbp - SRC_OTHER], rsi
    mov [rbp - SRC_OP], rdx

    ; Check other is a set or frozenset
    test r8d, TAG_RC_BIT
    jz .src_not_impl
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel set_type]
    cmp rax, rcx
    je .src_is_set
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    je .src_is_set
    jmp .src_not_impl

.src_is_set:
    cmp edx, PY_EQ
    je .src_eq
    cmp edx, PY_NE
    je .src_ne
    cmp edx, PY_LE
    je .src_le
    cmp edx, PY_GE
    je .src_ge
    cmp edx, PY_LT
    je .src_lt
    cmp edx, PY_GT
    je .src_gt
    jmp .src_not_impl

.src_eq:
    ; Check lengths
    mov rax, [rdi + PyDictObject.ob_size]
    cmp rax, [rsi + PyDictObject.ob_size]
    jne .src_false

    ; Every element of self must be in other
    mov rbx, rdi               ; self (set)
    mov r12, rsi               ; other (set)
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx               ; index
.src_eq_loop:
    cmp rcx, [rbx + PyDictObject.capacity]
    jge .src_true
    mov [rbp - SRC_IDX], rcx

    ; Get entry at index
    imul rax, rcx, SET_ENTRY_SIZE
    add rax, [rbx + PyDictObject.entries]
    ; Occupied entries have a non-zero key Value
    mov rsi, [rax + SET_ENTRY_KEY]
    test rsi, rsi
    jz .src_eq_next

    ; Entry is occupied — check if key is in other set
    INCREF_V rsi, rax                    ; ours across __eq__
    mov [rbp - SRC_KEY], rsi
    mov rdi, r12               ; other set
    call set_contains
    mov rdi, [rbp - SRC_KEY]
    push rax
    push rax                             ; pad
    DECREF_V rdi, rcx
    pop rax
    pop rcx
    test eax, eax
    jz .src_false              ; not found → not equal

.src_eq_next:
    mov rcx, [rbp - SRC_IDX]
    inc rcx
    jmp .src_eq_loop

.src_le:
    ; self <= other: self is subset of other (every elem of self in other)
    mov rbx, rdi               ; self
    mov r12, rsi               ; other
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx
.src_le_loop:
    cmp rcx, [rbx + PyDictObject.capacity]
    jge .src_true
    mov [rbp - SRC_IDX], rcx
    imul rax, rcx, SET_ENTRY_SIZE
    add rax, [rbx + PyDictObject.entries]
    mov rsi, [rax + SET_ENTRY_KEY]
    test rsi, rsi                        ; occupied?
    jz .src_le_next
    INCREF_V rsi, rax                    ; ours across __eq__
    mov [rbp - SRC_KEY], rsi
    mov rdi, r12
    call set_contains
    mov rdi, [rbp - SRC_KEY]
    push rax
    push rax                             ; pad
    DECREF_V rdi, rcx
    pop rax
    pop rcx
    test eax, eax
    jz .src_false
.src_le_next:
    mov rcx, [rbp - SRC_IDX]
    inc rcx
    jmp .src_le_loop

.src_ge:
    ; self >= other: other is subset of self → swap and do <=
    mov rbx, rsi               ; other (check all of other in self)
    mov r12, rdi               ; self
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx
.src_ge_loop:
    cmp rcx, [rbx + PyDictObject.capacity]
    jge .src_true
    mov [rbp - SRC_IDX], rcx
    imul rax, rcx, SET_ENTRY_SIZE
    add rax, [rbx + PyDictObject.entries]
    mov rsi, [rax + SET_ENTRY_KEY]
    test rsi, rsi                        ; occupied?
    jz .src_ge_next
    INCREF_V rsi, rax                    ; ours across __eq__
    mov [rbp - SRC_KEY], rsi
    mov rdi, r12
    call set_contains
    mov rdi, [rbp - SRC_KEY]
    push rax
    push rax                             ; pad
    DECREF_V rdi, rcx
    pop rax
    pop rcx
    test eax, eax
    jz .src_false
.src_ge_next:
    mov rcx, [rbp - SRC_IDX]
    inc rcx
    jmp .src_ge_loop

.src_lt:
    ; self < other: proper subset (self <= other AND len(self) < len(other))
    mov rax, [rdi + PyDictObject.ob_size]
    cmp rax, [rsi + PyDictObject.ob_size]
    jge .src_false             ; not strictly smaller → false
    jmp .src_le                ; then check subset

.src_gt:
    ; self > other: proper superset (self >= other AND len(self) > len(other))
    mov rax, [rdi + PyDictObject.ob_size]
    cmp rax, [rsi + PyDictObject.ob_size]
    jle .src_false             ; not strictly larger → false
    jmp .src_ge                ; then check superset

.src_ne:
    ; PY_NE = not PY_EQ
    push rdi
    push rsi
    mov edx, PY_EQ
    call set_richcompare
    pop rsi
    pop rdi
    ; Negate: if bool_true → return bool_false, vice versa
    test edx, edx
    jz .src_not_impl           ; NULL result → propagate
    lea rcx, [rel bool_true]
    cmp rax, rcx
    je .src_false              ; EQ was True → NE is False
    jmp .src_true              ; EQ was False → NE is True

.src_true:
    extern bool_true
    lea rax, [rel bool_true]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.src_false:
    extern bool_false
    lea rax, [rel bool_false]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.src_not_impl:
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_richcompare

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
SR_ENTRIES equ 8                ; the entry array the probe is walking
SR_FRAME equ 24                 ; 24 + 5 pushes keeps rsp 16-aligned
DEF_FUNC set_remove, SR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; set
    mov r12, rsi                ; the key, a Value

    SET_HASH_VALUE r12, r13     ; r13 = hash

    ; An independent second copy of set_find_slot's probe, because a removal
    ; has to tombstone the slot it lands on rather than be handed one; it gets
    ; the same treatment, restart included.
.sr_restart:
    mov r14, [rbx + PyDictObject.capacity]  ; probes remaining, counting down
    mov r15, r14
    dec r15                     ; mask

    ; Starting slot
    mov rcx, r13
    and rcx, r15

.sr_probe:
    dec r14
    js .sr_not_found

    mov rax, [rbx + PyDictObject.entries]
    lea rdx, [rcx + rcx]
    lea rax, [rax + rdx*8]      ; entries + slot * SET_ENTRY_SIZE

    SET_ENTRY_CLASSIFY rax, .sr_not_found, .sr_next

    cmp r13, [rax + SET_ENTRY_HASH]
    jne .sr_next

    ; Equal Values are the same key; see set_find_slot.
    mov rdi, [rax + SET_ENTRY_KEY]
    cmp rdi, r12
    mov rdx, rax
    je .sr_found

    mov rdx, [rbx + PyDictObject.entries]
    mov [rbp - SR_ENTRIES], rdx
    push rcx                    ; save slot
    push rax                    ; save entry ptr
    mov rsi, r12                ; b = the lookup key
    call set_keys_equal
    pop rdx                     ; entry ptr
    pop rcx
    ; As in set_find_slot: an __eq__ that grew this set has moved the entries
    ; out from under the pointer just restored.
    mov rsi, [rbx + PyDictObject.entries]
    cmp rsi, [rbp - SR_ENTRIES]
    jne .sr_restart
    test eax, eax
    jz .sr_next

.sr_found:
    ; Found: tombstone the entry, release the key, decrement the size
    mov rdi, [rdx + SET_ENTRY_KEY]
    mov qword [rdx + SET_ENTRY_KEY], 0
    mov qword [rdx + SET_ENTRY_HASH], ENTRY_TOMBSTONE_HASH   ; tombstone
    DECREF_V rdi, rsi
    dec qword [rbx + PyDictObject.ob_size]
    inc qword [rbx + PyDictObject.dk_tombstones]
    xor eax, eax               ; return 0 = success
    jmp .sr_done

.sr_next:
    inc rcx
    and rcx, r15
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

    ; Skip slots that are not occupied
    SET_ENTRY_CLASSIFY rax, .dealloc_next, .dealloc_next

    ; DECREF key (fat value)
    mov rdi, [rax + SET_ENTRY_KEY]
    V_UNPACK rdi, rsi
    DECREF_VAL rdi, rsi

.dealloc_next:
    inc r14
    jmp .dealloc_loop

.dealloc_entries_done:
    ; Free entries array
    mov rdi, r12
    call ap_free

    ; Free set object itself (GC-aware)
    mov rdi, rbx
    call gc_dealloc

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
extern current_exception
extern raise_exception
extern exc_TypeError_type

STC_EXC   equ 16            ; current_exception before the iteration started
STC_FRAME equ 16            ; + 2 pushes = 32, 16-aligned
DEF_FUNC set_type_call, STC_FRAME
    NO_KEYWORDS "set() takes no keyword arguments"
    push rbx
    push r12

    ; nargs can be 0 or 1
    test rdx, rdx
    je .stc_empty
    cmp rdx, 1
    jne .stc_error

    ; set(iterable): create set, iterate and add
    mov r12, [rsi]          ; args[0]

    ; Only a heap pointer can be iterable
    V_TEST_PTR r12, rcx
    ja .stc_not_iterable

    call set_new
    mov rbx, rax            ; rbx = new set

    ; A set source is CLONED -- its table copies wholesale, so no key is
    ; hashed and no slot probed.  A list or a tuple knows how many elements
    ; are coming, so the room is taken once instead of rehashing at 7, 14,
    ; 28, 56 on the way.
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel set_type]
    cmp rax, rcx
    je .stc_clone
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    je .stc_clone
    lea rcx, [rel list_type]
    cmp rax, rcx
    je .stc_reserve
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    jne .stc_sized_done
.stc_reserve:
    mov rdi, rbx
    mov rsi, [r12 + PyListObject.ob_size]
    call set_reserve
    jmp .stc_sized_done
.stc_clone:
    mov rdi, rbx
    mov rsi, r12
    call set_clone_into
    jmp .stc_cloned
.stc_sized_done:

    ; Get iterator: tp_iter(iterable)
    ; get_iterator_opt, not tp_iter: an object with __getitem__ and no
    ; __iter__ is iterable, and reading the slot rejects it.
    mov rdi, r12
    mov esi, TAG_PTR
    call get_iterator_opt
    test rax, rax
    jz .stc_not_iterable_decref_set
    mov r12, rax            ; r12 = iterator

    DUNDER_EXC_SAVE [rbp - STC_EXC]
.stc_iter_loop:
    ; Get next: tp_iternext(iterator)
    mov rdi, r12
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx           ; check tag (NULL = exhausted)
    jz .stc_iter_done

    ; Add to set (set_add INCREFs, so DECREF the iternext ref after)
    mov rdi, rbx            ; set
    mov rsi, rax            ; key payload
    ; edx = key tag (from tp_iternext)
    push rax                ; save key payload
    push rdx                ; save key tag
    push rdx                ; alignment padding (3 pushes = odd, matches ABI)
    V_PACK rsi, rdx         ; set_add takes a key Value
    call set_add
    add rsp, 8              ; drop alignment padding
    pop rsi                 ; key tag
    pop rdi                 ; key payload
    DECREF_VAL rdi, rsi     ; release iternext's reference
    jmp .stc_iter_loop

.stc_iter_done:
    ; DECREF iterator
    mov rdi, r12
    call obj_decref
.stc_cloned:

    ; NULL is exhaustion and a raise alike.  Read as exhaustion, a raising
    ; __getitem__ or __next__ produced a short set and a stranded exception.
    EXC_RAISED_SINCE [rbp - STC_EXC], rcx, .stc_iter_raised

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

.stc_iter_raised:
    mov rdi, rbx                ; the partly built set
    call obj_decref
    xor eax, eax                ; a NULL Value, with the exception pending
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret

.stc_not_iterable_decref_set:
    ; set was already allocated but iterable has no tp_iter — free set
    mov rdi, rbx
    call obj_decref

.stc_not_iterable:
    mov rsi, r12                ; args[0]: the iterator never replaced it
    CSTRING rdi, `'\x01' object is not iterable`
    extern raise_type_error_with_name
    jmp raise_type_error_with_name

.stc_error:
    mov rsi, rdx
    CSTRING rdi, "set expected at most 1 argument, got "
    xor edx, edx
    extern raise_type_error_counted
    jmp raise_type_error_counted
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
DEF_FUNC set_tp_iter, 8            ; 1 push, so rsp is 16-aligned
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
    ; The size when iteration began.  A set that grows or shrinks under a
    ; `for` walks a rebuilt entry array with a stale index; nothing checked,
    ; so `for x in s: s.add(2)` quietly finished with whatever it happened to
    ; visit where CPython raises.
    mov rcx, [rbx + PyDictObject.ob_size]
    mov [rax + PyDictIterObject.it_version], rcx

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
    test rax, rax
    jz .si_done                 ; already dropped, or cleared by the collector
    mov rcx, [rax + PyDictObject.ob_size]
    cmp rcx, [rdi + PyDictIterObject.it_version]
    jne .si_mutation_error
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
    SET_ENTRY_CLASSIFY rax, .si_skip, .si_skip

    ; Found a valid entry -- return the key Value
    inc rcx
    mov [rdi + PyDictIterObject.it_index], rcx
    mov rax, r8
    INCREF_V rax, rdx
    ret

.si_skip:
    inc rcx
    jmp .si_scan

.si_exhausted:
    ; Drop the set at exhaustion, as CPython's setiter does, so a __del__ on
    ; the last reference runs while the iterator can still say "exhausted".
    mov [rdi + PyDictIterObject.it_index], rcx
    mov rax, [rdi + PyDictIterObject.it_dict]
    test rax, rax
    jz .si_done
    mov qword [rdi + PyDictIterObject.it_dict], 0
    push rdi
    mov rdi, rax
    call obj_decref
    pop rdi
.si_done:
    RET_NULL
    ret

.si_mutation_error:
    extern exc_RuntimeError_type
    RAISE exc_RuntimeError_type, "Set changed size during iteration"
END_FUNC set_iter_next

;; ============================================================================
;; set_iter_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL set_iter_dealloc, 8            ; 1 push, so rsp is 16-aligned
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
DEF_FUNC_BARE set_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC set_iter_self

;; ============================================================================
;; frozenset_type_call(self, args, nargs) -> frozenset
;; Same as set_type_call but creates frozenset (reuses set_new, sets ob_type)
;; rdi = self (frozenset_type), rsi = args (16-byte fat slots), rdx = nargs
;; ============================================================================
global frozenset_type_call
FTC_EXC   equ 16            ; current_exception before the iteration started
FTC_FRAME equ 16            ; + 2 pushes = 32, 16-aligned
DEF_FUNC frozenset_type_call, FTC_FRAME
    NO_KEYWORDS "frozenset() takes no keyword arguments"
    push rbx
    push r12

    ; nargs can be 0 or 1
    test rdx, rdx
    je .ftc_empty
    cmp rdx, 1
    jne .ftc_error

    ; frozenset(iterable): create set, iterate and add, then set type
    mov r12, [rsi]          ; args[0]
    V_TEST_PTR r12, rcx
    ja .ftc_not_iterable

    ; frozenset(f) IS f.  A frozenset cannot change, so there is nothing a
    ; copy of one could be for -- CPython's make_new_set says "frozenset(f)
    ; is idempotent" and hands the argument straight back.  This built a
    ; whole second table and answered an object that compared equal but was
    ; not the same one.  Exact type only: a subclass may carry state that a
    ; plain frozenset does not.
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    jne .ftc_build
    INCREF r12
    mov rax, r12
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ftc_build:
    call set_new
    mov rbx, rax

    ; A set source is cloned and a sized one is presized, exactly as in
    ; set_type_call above.
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel set_type]
    cmp rax, rcx
    je .ftc_clone
    lea rcx, [rel list_type]
    cmp rax, rcx
    je .ftc_reserve
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    jne .ftc_sized_done
.ftc_reserve:
    mov rdi, rbx
    mov rsi, [r12 + PyListObject.ob_size]
    call set_reserve
    jmp .ftc_sized_done
.ftc_clone:
    mov rdi, rbx
    mov rsi, r12
    call set_clone_into
    jmp .ftc_cloned
.ftc_sized_done:

    ; Get iterator
    ; get_iterator_opt, not tp_iter: an object with __getitem__ and no
    ; __iter__ is iterable, and reading the slot rejects it.
    mov rdi, r12
    mov esi, TAG_PTR
    call get_iterator_opt
    test rax, rax
    jz .ftc_not_iterable_decref
    mov r12, rax

    DUNDER_EXC_SAVE [rbp - FTC_EXC]
.ftc_iter_loop:
    mov rdi, r12
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    call rax
    V_UNPACK rax, rdx           ; tp_iternext returns a Value
    test edx, edx
    jz .ftc_iter_done

    mov rdi, rbx
    mov rsi, rax
    push rax
    push rdx
    push rdx
    V_PACK rsi, rdx         ; set_add takes a key Value
    call set_add
    add rsp, 8
    pop rsi
    pop rdi
    DECREF_VAL rdi, rsi
    jmp .ftc_iter_loop

.ftc_iter_done:
    mov rdi, r12
    call obj_decref
.ftc_cloned:

    ; NULL is exhaustion and a raise alike.  Read as exhaustion, a raising
    ; __getitem__ or __next__ produced a short set and a stranded exception.
    EXC_RAISED_SINCE [rbp - FTC_EXC], rcx, .ftc_iter_raised

    ; Set type to frozenset_type
    lea rax, [rel frozenset_type]
    mov [rbx + PyObject.ob_type], rax
    mov rax, rbx
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ftc_empty:
    call set_new
    lea rcx, [rel frozenset_type]
    mov [rax + PyObject.ob_type], rcx
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ftc_iter_raised:
    mov rdi, rbx                ; the partly built set
    call obj_decref
    xor eax, eax                ; a NULL Value, with the exception pending
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret

.ftc_not_iterable_decref:
    mov rdi, rbx
    call obj_decref
.ftc_not_iterable:
    mov rsi, r12                ; args[0]: the iterator never replaced it
    CSTRING rdi, `'\x01' object is not iterable`
    jmp raise_type_error_with_name
.ftc_error:
    mov rsi, rdx
    CSTRING rdi, "frozenset expected at most 1 argument, got "
    xor edx, edx
    extern raise_type_error_counted
    jmp raise_type_error_counted
END_FUNC frozenset_type_call


;; ============================================================================
;; Set number method wrappers (nb_* convention -> set_method_* convention)
;; nb_* calling convention: rdi=left, rsi=right, rdx=ltag, rcx=rtag
;; set_method calling convention: rdi=args_array, rsi=nargs
;; ============================================================================

extern set_method_union
extern set_method_intersection
extern set_method_difference
extern set_method_symmetric_difference

SNB_FRAME equ 32            ; + 0 pushes = 32

;; set_nb_or(left, right, ltag, rtag) -> new set (union)
; The two operands, laid out as the args array the method form expects.
SNB_LEFT  equ 32
SNB_RIGHT equ 24

; Both operands of a set operator must be sets.  The method forms are laxer on
; purpose -- set.union(iterable) takes any iterable -- but the OPERATORS are
; not: CPython raises TypeError for `{1,2} | [1]`, and these slots used to hand
; the right operand to set_method_union, which reads it as a PyDictObject.
; `{1,2} | 5` was an arbitrary read through address 5.
;
; Declining with a NULL Value rather than raising is what lets the protocol try
; the other operand and then a user class's __ror__.
%macro SET_NB_REQUIRE_BOTH 0
    V_TEST_PTR rdi, rax         ; ja == not a pointer, so not a set either
    ja %%bad
    V_TEST_PTR rsi, rax
    ja %%bad
    mov rax, [rdi + PyObject.ob_type]
    REQUIRE_SET_TYPE rax, rcx, %%bad
    mov rax, [rsi + PyObject.ob_type]
    REQUIRE_SET_TYPE rax, rcx, %%bad
    jmp %%ok
%%bad:
    xor eax, eax                ; NULL Value = NotImplemented
    leave
    ret
%%ok:
%endmacro
DEF_FUNC set_nb_or, SNB_FRAME
    SET_NB_REQUIRE_BOTH
    mov [rbp - SNB_LEFT], rdi         ; args[0] = left
    mov [rbp - SNB_RIGHT], rsi         ; args[1] = right
    lea rdi, [rbp - SNB_LEFT]
    mov esi, 2
    call set_method_union
    leave
    ret
END_FUNC set_nb_or

;; set_nb_and(left, right, ltag, rtag) -> new set (intersection)
DEF_FUNC set_nb_and, SNB_FRAME
    SET_NB_REQUIRE_BOTH
    mov [rbp - SNB_LEFT], rdi         ; args[0] = left
    mov [rbp - SNB_RIGHT], rsi         ; args[1] = right
    lea rdi, [rbp - SNB_LEFT]
    mov esi, 2
    call set_method_intersection
    leave
    ret
END_FUNC set_nb_and

;; ============================================================================
;; The in-place set operators.
;;
;; `s &= t` used to fall through to the binary form and REBIND, so every other
;; name for the same set went on seeing the old value.  bugs.md filed that as a
;; deliberate omission, on the grounds that a by-name `__iand__` that did not
;; really mutate would be worse than none at all.  It would be; the answer is
;; to mutate.
;;
;; Each computes the binary result into a fresh set and then swaps the two
;; sets' storage, so the object the caller is holding is the one that changes.
;; The temporary leaves holding the old table and releases it on the way out.
;; Doing it that way means the four share every line of the actual set algebra
;; with the binary forms, rather than growing a second implementation of it.
;;
;; frozenset does NOT get these -- it shares set_number_methods, so each one
;; asks set_result_type what kind the receiver is and declines for a frozen
;; one.  CPython's frozenset has no `__iand__` either, and `fs &= t` rebinds
;; there too.
;; ============================================================================
%macro DEF_SET_INPLACE 2        ; %1 = name suffix, %2 = the binary form
DEF_FUNC set_nb_i%1, SNB_FRAME
    SET_NB_REQUIRE_BOTH         ; both are sets, or NotImplemented
    mov [rbp - SNB_LEFT], rdi
    mov [rbp - SNB_RIGHT], rsi

    ; An immutable receiver has nothing to mutate: hand back NotImplemented
    ; and let the protocol fall back to the binary form and rebind.
    call set_result_type
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    je %%decline

    mov rdi, [rbp - SNB_LEFT]
    mov rsi, [rbp - SNB_RIGHT]
    call %2                     ; the binary form: an owned set, as a Value
    test rax, rax
    jz %%decline                ; NULL: it raised, or it declined

    mov rdi, [rbp - SNB_LEFT]
    mov rsi, rax                ; takes over the temporary's reference
    call set_swap_storage       ; -> rax = the receiver, one new reference
    leave
    ret
%%decline:
    xor eax, eax                ; a NULL Value is NotImplemented here
    leave
    ret
END_FUNC set_nb_i%1
%endmacro

DEF_SET_INPLACE and, set_nb_and
DEF_SET_INPLACE or,  set_nb_or
DEF_SET_INPLACE sub, set_nb_sub
DEF_SET_INPLACE xor, set_nb_xor

;; ============================================================================
;; set_swap_storage(rdi = the set to mutate,
;;                  rsi = a fresh set holding the answer, whose reference this
;;                        takes over)
;;   -> rax = the mutated set, with a reference of its own
;;
;; set and frozenset reuse the dict header, of which a set uses four words.
;; Swapping them moves the answer into the caller's object and the old table
;; into the temporary, which then releases it -- so the old keys are dropped
;; by the ordinary set dealloc rather than by a second copy of it here.
;; ============================================================================
DEF_FUNC set_swap_storage, 8        ; rsp 16-aligned at the call the macros below expand to
    push rbx
    push r12
    sub rsp, 8                  ; 8 + 16 = 24; + the pushed rbp = aligned
    mov rbx, rdi
    mov r12, rsi

    mov rax, [rbx + PyDictObject.ob_size]
    mov rcx, [r12 + PyDictObject.ob_size]
    mov [rbx + PyDictObject.ob_size], rcx
    mov [r12 + PyDictObject.ob_size], rax

    mov rax, [rbx + PyDictObject.capacity]
    mov rcx, [r12 + PyDictObject.capacity]
    mov [rbx + PyDictObject.capacity], rcx
    mov [r12 + PyDictObject.capacity], rax

    mov rax, [rbx + PyDictObject.entries]
    mov rcx, [r12 + PyDictObject.entries]
    mov [rbx + PyDictObject.entries], rcx
    mov [r12 + PyDictObject.entries], rax

    mov rax, [rbx + PyDictObject.dk_tombstones]
    mov rcx, [r12 + PyDictObject.dk_tombstones]
    mov [rbx + PyDictObject.dk_tombstones], rcx
    mov [r12 + PyDictObject.dk_tombstones], rax

    ; The pop cursor indexes the table, so it travels with it.
    mov rax, [rbx + SET_FINGER]
    mov rcx, [r12 + SET_FINGER]
    mov [rbx + SET_FINGER], rcx
    mov [r12 + SET_FINGER], rax

    ; The version counter belongs to the object, not to the table, so it does
    ; not travel -- but it does have to move, or an iterator that is mid-walk
    ; will not notice that the set changed under it.
    inc qword [rbx + PyDictObject.dk_version]

    DECREF_REG r12              ; releases the old table and its keys
    mov rax, rbx
    INCREF rax
    add rsp, 8
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_swap_storage

;; set_nb_sub(left, right, ltag, rtag) -> new set (difference)
DEF_FUNC set_nb_sub, SNB_FRAME
    SET_NB_REQUIRE_BOTH
    mov [rbp - SNB_LEFT], rdi         ; args[0] = left
    mov [rbp - SNB_RIGHT], rsi         ; args[1] = right
    lea rdi, [rbp - SNB_LEFT]
    mov esi, 2
    call set_method_difference
    leave
    ret
END_FUNC set_nb_sub

;; set_nb_xor(left, right, ltag, rtag) -> new set (symmetric_difference)
DEF_FUNC set_nb_xor, SNB_FRAME
    SET_NB_REQUIRE_BOTH
    mov [rbp - SNB_LEFT], rdi         ; args[0] = left
    mov [rbp - SNB_RIGHT], rsi         ; args[1] = right
    lea rdi, [rbp - SNB_LEFT]
    mov esi, 2
    call set_method_symmetric_difference
    leave
    ret
END_FUNC set_nb_xor


;; ============================================================================
;; Data section
;; ============================================================================
section .data

; set_repr_str removed - repr now in src/repr.asm
set_iter_name: db "set_iterator", 0

set_name_str: db "set", 0
frozenset_name_str: db "frozenset", 0

; Set number methods (for |, &, -, ^ operators)
align 8
set_number_methods:
    dq 0                        ; nb_add          +0
    dq set_nb_sub               ; nb_subtract     +8  (set difference -)
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
    dq set_nb_and               ; nb_and          +104 (set intersection &)
    dq set_nb_xor               ; nb_xor          +112 (set symmetric_difference ^)
    dq set_nb_or                ; nb_or           +120 (set union |)
    dq 0                        ; nb_int          +128
    dq 0                        ; nb_float        +136
    dq 0                        ; nb_floor_divide +144
    dq 0                        ; nb_true_divide  +152
    dq 0                        ; nb_index        +160
    dq 0                        ; nb_iadd         +168
    dq set_nb_isub            ; nb_isub         +176
    dq 0                        ; nb_imul         +184
    dq 0                        ; nb_irem         +192
    dq 0                        ; nb_ipow         +200
    dq 0                        ; nb_ilshift      +208
    dq 0                        ; nb_irshift      +216
    dq set_nb_iand            ; nb_iand         +224
    dq set_nb_ixor            ; nb_ixor         +232
    dq set_nb_ior            ; nb_ior          +240
    dq 0                        ; nb_ifloor_divide +248
    dq 0                        ; nb_itrue_divide +256
    dq 0 ; nb_matmul
    dq 0 ; nb_imatmul

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
    extern hash_not_implemented
    dq hash_not_implemented     ; tp_hash (raises TypeError)
    dq 0                ; tp_call  (instances are not callable)
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq set_richcompare          ; tp_richcompare
    dq set_tp_iter              ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq set_type_call        ; tp_new  (constructor)
    dq set_number_methods       ; tp_as_number
    dq set_seq_methods          ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC | TYPE_FLAG_SET_SUBCLASS ; tp_flags
    dq 0                        ; tp_bases
    dq set_traverse                        ; tp_traverse
    dq set_clear_gc                        ; tp_clear
    dq 0         ; tp_dictoffset
    dq 0                        ; tp_tailslots

; Frozenset type object
align 8
global frozenset_type
frozenset_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq frozenset_name_str       ; tp_name
    dq PyDictObject_size        ; tp_basicsize (reuse dict layout)
    dq set_dealloc              ; tp_dealloc (same as set)
    dq set_repr                 ; tp_repr (TODO: frozenset({...}) format)
    dq set_repr                 ; tp_str
    dq frozenset_hash           ; tp_hash
    dq 0                ; tp_call  (instances are not callable)
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq set_richcompare          ; tp_richcompare
    dq set_tp_iter              ; tp_iter (reuse set iter)
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq frozenset_type_call  ; tp_new  (constructor)
    dq set_number_methods       ; tp_as_number
    dq set_seq_methods          ; tp_as_sequence (reuse set methods)
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC | TYPE_FLAG_SET_SUBCLASS ; tp_flags
    dq 0                        ; tp_bases
    dq set_traverse                        ; tp_traverse
    dq set_clear_gc                        ; tp_clear
    dq 0         ; tp_dictoffset
    dq 0                        ; tp_tailslots

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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots

section .text

;; ============================================================================
;; GC traverse and clear.  These lived in gc.asm, which left the collector
;; holding the reference graph of every type in the system; a type's own
;; file is the only place that knows which of its fields are owned.
;; ============================================================================

;; ============================================================================
;; ---- set_traverse / set_clear ----
;; Set entries are 24 bytes (hash+key+key_tag_qword), distinct from DictEntry (32 bytes).
;; ============================================================================
SET_ENTRY_SIZE_GC    equ 16
SET_ENTRY_KEY_GC     equ 8

DEF_FUNC set_traverse, 8        ; rsp 16-aligned at the call the macros below expand to
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]   ; set reuses PyDictObject layout for header
    mov r13, [rbx + PyDictObject.capacity]
    test r13, r13
    jz .st_done
.st_loop:
    dec r13
    ; Check for empty (key_tag == 0) or tombstone (key_tag == 0xdead)
    SET_ENTRY_CLASSIFY r12, .st_next, .st_next

    ; Visit key
    mov rdi, [r12 + SET_ENTRY_KEY_GC]
    VISIT_V rdi, rsi

.st_next:
    add r12, SET_ENTRY_SIZE_GC
    test r13, r13
    jnz .st_loop
.st_done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_traverse

DEF_FUNC set_clear_gc, 8        ; rsp 16-aligned at the call the macros below expand to
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]

    test r13, r13
    jz .sc_done
.sc_loop:
    dec r13
    SET_ENTRY_CLASSIFY r12, .sc_next, .sc_next

    ; DECREF key
    push r12
    push r13
    mov rdi, [r12 + SET_ENTRY_KEY_GC]
    DECREF_V rdi, rsi
    pop r13
    pop r12

    ; Clear entry
    mov qword [r12 + SET_ENTRY_KEY_GC], 0

.sc_next:
    add r12, SET_ENTRY_SIZE_GC
    test r13, r13
    jnz .sc_loop
.sc_done:
    mov qword [rbx + PyDictObject.ob_size], 0

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_clear_gc
