; methods/set.asm - set and frozenset methods
;
; Methods are registered into each type's tp_dict by methods_init, in
; methods_init.asm.  A method is name(PyObject *self, PyObject **args,
; int64_t nargs); args are borrowed, the result is a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"


; External functions
extern frozenset_type
extern obj_decref
extern tuple_type
extern none_singleton
extern bool_true
extern bool_false
extern raise_exception
extern exc_TypeError_type
extern exc_KeyError_type
extern set_type
extern frozenset_hash
extern int_from_i64
extern tuple_type_call
extern obj_dealloc

; Set entry layout constants (must match set.asm)
SET_ENTRY_KEY     equ 8
SET_ENTRY_SIZE    equ 16
extern set_add
extern set_contains
extern set_remove
extern set_new
extern set_new_of_type
extern set_result_type
extern set_coerce_operand

; --- moved to a sibling file by the split ---

section .text

;; ============================================================================
;; set_method_add(args, nargs) -> None
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_add
    cmp rsi, 2
    jne .sma_error

    mov rax, rdi            ; args ptr
    mov rdi, [rax]          ; self (set)
    mov rsi, [rax + 8]     ; elem payload
    call set_add

    RET_NONE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.sma_error:
    RAISE exc_TypeError_type, "add() takes exactly one argument"
END_FUNC set_method_add

;; ============================================================================
;; set_method_remove(args, nargs) -> None (raises KeyError if missing)
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_remove
    cmp rsi, 2
    jne .smr_error

    mov rax, rdi
    mov rdi, [rax]          ; self
    mov rsi, [rax + 8]     ; elem payload
    call set_remove
    test eax, eax
    jnz .smr_keyerr

    RET_NONE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smr_keyerr:
    RAISE exc_KeyError_type, "element not in set"

.smr_error:
    RAISE exc_TypeError_type, "remove() takes exactly one argument"
END_FUNC set_method_remove

;; ============================================================================
;; set_method_discard(args, nargs) -> None (no error if missing)
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_discard
    cmp rsi, 2
    jne .smd_error

    mov rax, rdi
    mov rdi, [rax]          ; self
    mov rsi, [rax + 8]     ; elem payload
    call set_remove
    ; Ignore return value (don't care if not found)

    RET_NONE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smd_error:
    RAISE exc_TypeError_type, "discard() takes exactly one argument"
END_FUNC set_method_discard

;; ============================================================================
;; set_method_pop(args, nargs) -> removed element
;; args[0]=self
;; Scans for first occupied entry, removes and returns it.
;; ============================================================================
SMP_FRAME equ 16    ; save self + entry ptr
DEF_FUNC set_method_pop, SMP_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 1
    jne .smpop_error

    mov rbx, [rdi]          ; self (set)

    ; Check empty
    cmp qword [rbx + PyDictObject.ob_size], 0
    je .smpop_empty

    ; Scan for first non-empty entry
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx            ; index

.smpop_scan:
    cmp rcx, r13
    jge .smpop_empty         ; shouldn't happen

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12             ; entry ptr

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    jne .smpop_found
    inc ecx
    jmp .smpop_scan

.smpop_found:
    ; rax = entry ptr with valid key
    ; Get key (return value) — DON'T incref, we're removing it
    mov rcx, [rax + SET_ENTRY_KEY]        ; key payload
    V_UNPACK rcx, r12

    ; Clear the entry (mark as empty)
    mov qword [rax + SET_ENTRY_KEY], 0
    dec qword [rbx + PyDictObject.ob_size]

    ; Return the key (ownership transfers, no INCREF/DECREF needed)
    mov rax, rcx
    mov edx, r12d
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smpop_empty:
    RAISE exc_KeyError_type, "pop from an empty set"

.smpop_error:
    RAISE exc_TypeError_type, "pop() takes no arguments"
END_FUNC set_method_pop

;; ============================================================================
;; set_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
DEF_FUNC set_method_clear
    push rbx
    push r12
    push r13

    cmp rsi, 1
    jne .smc_error

    mov rbx, [rdi]          ; self (set)
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx

.smc_loop:
    cmp rcx, r13
    jge .smc_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx                ; save index

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smc_next

    ; DECREF key
    mov rdi, [rax + SET_ENTRY_KEY]
    V_UNPACK rdi, rsi
    mov qword [rax + SET_ENTRY_KEY], 0
    DECREF_VAL rdi, rsi

.smc_next:
    pop rcx
    inc ecx
    jmp .smc_loop

.smc_done:
    mov qword [rbx + PyDictObject.ob_size], 0

    RET_NONE
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smc_error:
    RAISE exc_TypeError_type, "clear() takes no arguments"
END_FUNC set_method_clear

;; ============================================================================
;; set_method_copy(args, nargs) -> new set (shallow copy)
;; args[0]=self
;; ============================================================================
DEF_FUNC set_method_copy
    push rbx
    push r12
    push r13
    push r14

    cmp rsi, 1
    jne .smcp_error

    mov r14, [rdi]          ; self (source set)

    ; An exact frozenset is immutable, so CPython hands the argument straight
    ; back rather than copying it -- f.copy() is f.
    mov rax, [r14 + PyObject.ob_type]
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    jne .smcp_build
    INCREF r14
    mov rax, r14
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.smcp_build:
    ; Create new empty set, of the left operand's kind: a frozenset operator
    ; or method answers a frozenset, not a set.
    mov rdi, r14
    call set_result_type
    mov rdi, rax
    call set_new_of_type
    mov rbx, rax            ; rbx = new set

    ; Iterate source entries
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smcp_loop:
    cmp rcx, r13
    jge .smcp_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smcp_next

    ; Add key to new set
    mov rdi, rbx            ; new set
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_add

.smcp_next:
    pop rcx
    inc ecx
    jmp .smcp_loop

.smcp_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smcp_error:
    ; Unreachable now that copy is registered with its arity: the shared
    ; path names the type, "set.copy() takes no arguments (1 given)".
    RAISE exc_TypeError_type, "copy() takes no arguments"
END_FUNC set_method_copy

;; ============================================================================
;; set_method_union(args, nargs) -> new set = self | other
;; args[0]=self, args[1]=other (iterable)
;; ============================================================================
DEF_FUNC_LOCAL set_binop_union, 8            ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smu_error

    mov r14, [rdi]          ; self
    ; The method forms take any iterable where the operators take only a set.
    ; This read the argument as a PyDictObject whatever it was, so
    ; s.difference([1]) walked a list's header as a hash table, found nothing
    ; occupied, and answered s unchanged.
    mov rdi, [rdi + 8]
    call set_coerce_operand
    test rax, rax
    jz .smu_fail
    mov r15, rax            ; owned: the set itself, or one built from it

    ; Copy self → new set
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    ; The result takes the left operand's kind: a frozenset operator or method
    ; answers a frozenset, not a set.
    mov rdi, r14
    call set_result_type
    mov rdi, rax
    call set_new_of_type
    mov rbx, rax            ; new set
    xor ecx, ecx

.smu_copy_self:
    cmp rcx, r13
    jge .smu_add_other

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smu_cs_next

    mov rdi, rbx
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_add

.smu_cs_next:
    pop rcx
    inc ecx
    jmp .smu_copy_self

.smu_add_other:
    ; Now add all elements from other
    mov r12, [r15 + PyDictObject.entries]
    mov r13, [r15 + PyDictObject.capacity]
    xor ecx, ecx

.smu_add_loop:
    cmp rcx, r13
    jge .smu_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smu_al_next

    mov rdi, rbx
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_add

.smu_al_next:
    pop rcx
    inc ecx
    jmp .smu_add_loop

.smu_done:
    mov rdi, r15
    call obj_decref     ; the coerced operand
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smu_fail:
    ; not iterable, or iterating it raised
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.smu_error:
    RAISE exc_TypeError_type, "union() takes exactly one argument"
END_FUNC set_binop_union

;; ============================================================================
;; set_method_update(args, nargs) -> None
;; args[0]=self, args[1]=any iterable.  Also serves as set.__init__.
;;
;; This read args[1] as a PyDictObject unconditionally, so s.update([2,3])
;; walked a list's fields as a hash table -- and a set subclass, which fills
;; itself through __init__, crashed on construction from any list.
;; ============================================================================
SU_SELF   equ 8
SU_TMP    equ 16        ; materialised sequence, owned, or 0
SU_FRAME  equ 40            ; + 3 pushes = 64, 16-aligned

DEF_FUNC_LOCAL set_update_one, SU_FRAME
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov [rbp - SU_SELF], rbx
    mov qword [rbp - SU_TMP], 0
    cmp rsi, 2
    jl .supd_done

    mov r12, [rdi + 8]      ; the source Value
    V_TEST_PTR r12, rax
    ja .supd_materialise
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel set_type]
    cmp rax, rcx
    je .supd_from_set
    extern frozenset_type
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    je .supd_from_set

.supd_materialise:
    ; Any other iterable: materialise it and add the elements one by one.
    lea rsi, [rdi + 8]
    lea rdi, [rel tuple_type]
    mov edx, 1
    call tuple_type_call        ; raises for a non-iterable
    ; It also answers NULL when the iteration itself raised -- a __getitem__
    ; or __next__ that threw partway through -- and reading ob_size off that
    ; NULL made `s.update(G())` a segfault rather than G's own exception.
    test rax, rax
    jz .supd_fail
    mov [rbp - SU_TMP], rax
    mov r12, rax
    mov r13, [r12 + PyTupleObject.ob_size]
    xor ecx, ecx
.supd_seq_loop:
    cmp rcx, r13
    jge .supd_release
    push rcx
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rsi, [rax + rcx * 8]
    mov rdi, [rbp - SU_SELF]
    call set_add
    pop rcx
    inc rcx
    jmp .supd_seq_loop

.supd_release:
    mov rdi, [rbp - SU_TMP]
    mov qword [rbp - SU_TMP], 0
    call obj_decref
    jmp .supd_done

.supd_from_set:
    mov r13, [r12 + PyDictObject.capacity]
    xor ecx, ecx

.supd_loop:
    cmp rcx, r13
    jge .supd_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, [r12 + PyDictObject.entries]
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .supd_next

    mov rdi, [rbp - SU_SELF]
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_add

.supd_next:
    pop rcx
    inc ecx
    jmp .supd_loop

.supd_done:
    extern none_singleton
    RET_NONE
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.supd_fail:
    xor eax, eax                ; a NULL Value, with the exception pending
    xor edx, edx
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC set_update_one

;; ============================================================================
;; set_method_intersection(args, nargs) -> new set = self & other
;; args[0]=self, args[1]=other
;; ============================================================================
DEF_FUNC_LOCAL set_binop_intersection, 8            ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smi_error

    mov r14, [rdi]          ; self
    ; The method forms take any iterable where the operators take only a set.
    ; This read the argument as a PyDictObject whatever it was, so
    ; s.difference([1]) walked a list's header as a hash table, found nothing
    ; occupied, and answered s unchanged.
    mov rdi, [rdi + 8]
    call set_coerce_operand
    test rax, rax
    jz .smi_fail
    mov r15, rax            ; owned: the set itself, or one built from it

    ; The result takes the left operand's kind: a frozenset operator or method
    ; answers a frozenset, not a set.
    mov rdi, r14
    call set_result_type
    mov rdi, rax
    call set_new_of_type
    mov rbx, rax            ; new set

    ; Iterate self, add if in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smi_loop:
    cmp rcx, r13
    jge .smi_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smi_next

    ; Check if key is in other
    push rax                ; save entry ptr
    mov rdi, r15            ; other set
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    pop rcx                 ; restore entry ptr (was rax)
    test eax, eax
    jz .smi_next

    ; In both — add to result
    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    call set_add

.smi_next:
    pop rcx
    inc ecx
    jmp .smi_loop

.smi_done:
    mov rdi, r15
    call obj_decref     ; the coerced operand
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smi_fail:
    ; not iterable, or iterating it raised
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.smi_error:
    RAISE exc_TypeError_type, "intersection() takes exactly one argument"
END_FUNC set_binop_intersection

;; ============================================================================
;; set_method_difference(args, nargs) -> new set = self - other
;; args[0]=self, args[1]=other
;; ============================================================================
DEF_FUNC_LOCAL set_binop_difference, 8            ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smdf_error

    mov r14, [rdi]          ; self
    ; The method forms take any iterable where the operators take only a set.
    ; This read the argument as a PyDictObject whatever it was, so
    ; s.difference([1]) walked a list's header as a hash table, found nothing
    ; occupied, and answered s unchanged.
    mov rdi, [rdi + 8]
    call set_coerce_operand
    test rax, rax
    jz .smdf_fail
    mov r15, rax            ; owned: the set itself, or one built from it

    ; The result takes the left operand's kind: a frozenset operator or method
    ; answers a frozenset, not a set.
    mov rdi, r14
    call set_result_type
    mov rdi, rax
    call set_new_of_type
    mov rbx, rax            ; new set

    ; Iterate self, add if NOT in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smdf_loop:
    cmp rcx, r13
    jge .smdf_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smdf_next

    ; Check if key is in other
    push rax
    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    pop rcx                 ; entry ptr
    test eax, eax
    jnz .smdf_next          ; in other — skip

    ; NOT in other — add to result
    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    call set_add

.smdf_next:
    pop rcx
    inc ecx
    jmp .smdf_loop

.smdf_done:
    mov rdi, r15
    call obj_decref     ; the coerced operand
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smdf_fail:
    ; not iterable, or iterating it raised
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.smdf_error:
    RAISE exc_TypeError_type, "difference() takes exactly one argument"
END_FUNC set_binop_difference

;; ============================================================================
;; set_fold(rdi = args, rsi = nargs, rdx = the two-operand body)
;;   -> a Value: a new set, or NULL with the exception pending
;;
;; union, intersection and difference take ANY number of iterables in CPython
;; -- `s.difference(a, b)` is `(s - a) - b`, and the no-argument forms are a
;; copy.  All three here took exactly one and raised
;; "union() takes exactly one argument" for everything else, so
;; `set().union(*parts)` -- the ordinary way to flatten a list of sets -- was
;; a TypeError.
;;
;; Folding is what makes the three variadic without a second implementation of
;; any of them: the bodies stay two-operand, and this walks the arguments.  A
;; single operand is the overwhelmingly common case and still reaches the body
;; directly, with no intermediate copy.
;; ============================================================================
SF_ARGS  equ 8
SF_N     equ 16
SF_BODY  equ 24
SF_ACC   equ 32
SF_IDX   equ 40
SF_TMP   equ 56             ; the two-slot argument array: [-56]=acc, [-48]=other
SF_FRAME equ 64             ; + 0 pushes = 64

DEF_FUNC_LOCAL set_fold, SF_FRAME
    cmp rsi, 2
    je .sf_direct           ; the common case: no accumulator needed
    cmp rsi, 1
    jl .sf_arity

    mov [rbp - SF_ARGS], rdi
    mov [rbp - SF_N], rsi
    mov [rbp - SF_BODY], rdx

    ; Seed the fold with a copy of self, which is also the whole answer for
    ; the no-argument form.  A set is a pointer, and a pointer is its own
    ; Value, so nothing here needs unpacking.
    mov esi, 1
    call set_method_copy
    test rax, rax
    jz .sf_fail
    mov [rbp - SF_ACC], rax
    mov qword [rbp - SF_IDX], 1

.sf_loop:
    mov rcx, [rbp - SF_IDX]
    cmp rcx, [rbp - SF_N]
    jge .sf_done

    mov rax, [rbp - SF_ACC]
    mov [rbp - SF_TMP], rax
    mov rax, [rbp - SF_ARGS]
    mov rcx, [rbp - SF_IDX]
    mov rax, [rax + rcx * 8]
    mov [rbp - SF_TMP + 8], rax
    lea rdi, [rbp - SF_TMP]
    mov esi, 2
    call [rbp - SF_BODY]
    test rax, rax
    jz .sf_drop_acc

    mov rdi, [rbp - SF_ACC]
    mov [rbp - SF_ACC], rax     ; the new accumulator, before the old one dies
    call obj_decref
    inc qword [rbp - SF_IDX]
    jmp .sf_loop

.sf_done:
    mov rax, [rbp - SF_ACC]
    leave
    ret

.sf_drop_acc:
    mov rdi, [rbp - SF_ACC]
    call obj_decref
.sf_fail:
    xor eax, eax                ; a NULL Value, with the exception pending
    leave
    ret

.sf_direct:
    mov rax, rdx
    leave
    jmp rax                     ; a tail call: rdi and rsi are already right

.sf_arity:
    RAISE exc_TypeError_type, "descriptor requires a set argument"
END_FUNC set_fold

;; The three variadic entry points.  The operators reach the bodies through
;; these too, always with exactly two operands, so they cost one jump.
DEF_FUNC_BARE set_method_union
    lea rdx, [rel set_binop_union]
    jmp set_fold
END_FUNC set_method_union

DEF_FUNC_BARE set_method_intersection
    lea rdx, [rel set_binop_intersection]
    jmp set_fold
END_FUNC set_method_intersection

DEF_FUNC_BARE set_method_difference
    lea rdx, [rel set_binop_difference]
    jmp set_fold
END_FUNC set_method_difference

;; ============================================================================
;; set_method_update(args, nargs) -> None
;;
;; update is variadic in CPython as well, and this took one source and
;; silently IGNORED the rest -- `s.update(a, b)` dropped b without a word,
;; which is worse than the TypeError the other three raised.  It is also
;; set.__init__, so it has to tolerate nargs == 1.
;; ============================================================================
SUV_ARGS  equ 8
SUV_N     equ 16
SUV_IDX   equ 24
SUV_TMP   equ 40            ; [-40] = self, [-32] = one source
SUV_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC set_method_update, SUV_FRAME
    mov [rbp - SUV_ARGS], rdi
    mov [rbp - SUV_N], rsi
    mov qword [rbp - SUV_IDX], 1
    mov rax, [rdi]
    mov [rbp - SUV_TMP], rax    ; self, once; only the source slot moves
.suv_loop:
    mov rcx, [rbp - SUV_IDX]
    cmp rcx, [rbp - SUV_N]
    jge .suv_done
    mov rax, [rbp - SUV_ARGS]
    mov rax, [rax + rcx * 8]
    mov [rbp - SUV_TMP + 8], rax
    lea rdi, [rbp - SUV_TMP]
    mov esi, 2
    call set_update_one
    test rax, rax
    jz .suv_fail
    inc qword [rbp - SUV_IDX]
    jmp .suv_loop
.suv_done:
    RET_NONE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.suv_fail:
    xor eax, eax                ; a NULL Value, with the exception pending
    leave
    ret
END_FUNC set_method_update

;; ============================================================================
;; The three in-place method forms: intersection_update, difference_update and
;; symmetric_difference_update.  All three were simply absent, so the only way
;; to narrow a set in place was `s &= t` -- which, until the commit before
;; this one, did not narrow it in place either.
;;
;; Each computes the ordinary answer and then swaps the two sets' storage, the
;; same trick the nb_inplace_* slots use, so the object the caller holds is
;; the one that changes.  frozenset gets none of them, and calling one through
;; the unbound descriptor with a frozen receiver is a TypeError rather than a
;; mutation of something immutable.
;; ============================================================================
SUI_FRAME equ 16            ; + 1 push = 24, not 16-aligned
%macro DEF_SET_UPDATE 2-3    ; %1 = method name, %2 = the value-producing form,
                             ; %3 = a name symbol when the form takes exactly one
DEF_FUNC set_method_%1, SUI_FRAME
    push rbx
    cmp rsi, 1
    jl %%bad
%if %0 >= 3
    ; The delegate would raise under ITS own name; CPython names the method
    ; that was actually called.
    cmp rsi, 2
    je %%arity_ok
    lea rdi, [rel %3]
    call set_raise_arity
%%arity_ok:
%endif
    mov rbx, [rdi]              ; self, kept across the call

    push rdi
    push rsi
    mov rdi, rbx
    call set_result_type
    lea rcx, [rel frozenset_type]
    cmp rax, rcx
    pop rsi
    pop rdi
    je %%frozen

    call %2
    test rax, rax
    jz %%fail

    mov rdi, rbx
    mov rsi, rax                ; takes over the answer's reference
    extern set_swap_storage
    call set_swap_storage
    mov rdi, rax
    call obj_decref             ; set_swap_storage hands back a reference
    RET_NONE
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
%%fail:
    xor eax, eax                ; a NULL Value, with the exception pending
    pop rbx
    leave
    ret
%%frozen:
    pop rbx
    RAISE exc_TypeError_type, "descriptor requires a 'set' object"
%%bad:
    pop rbx
    RAISE exc_TypeError_type, "descriptor requires a set argument"
END_FUNC set_method_%1
%endmacro

DEF_SET_UPDATE intersection_update,         set_method_intersection
DEF_SET_UPDATE difference_update,           set_method_difference
DEF_SET_UPDATE symmetric_difference_update, set_method_symmetric_difference, \
               sn_symmetric_difference_update

;; ============================================================================
;; set_method_symmetric_difference(args, nargs) -> new set = self ^ other
;; args[0]=self, args[1]=other
;; ============================================================================
;; ============================================================================
;; set_raise_arity(rdi = the method's name, rsi = nargs) -- does not return
;;
;; CPython's wording for a set method given the wrong number of arguments:
;;   set.symmetric_difference() takes exactly one argument (2 given)
;; nargs counts self, so the number reported is one less.
;; ============================================================================
SRA_NAME  equ 8
SRA_N     equ 16
SRA_BUF   equ 160
SRA_FRAME equ 160           ; + 0 pushes = 160, 16-aligned
DEF_FUNC_LOCAL set_raise_arity, SRA_FRAME
    mov [rbp - SRA_NAME], rdi
    dec rsi                     ; self does not count
    mov [rbp - SRA_N], rsi
    lea rdi, [rbp - SRA_BUF]
    lea rsi, [rel sn_set_dot]
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - SRA_NAME]
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel sn_takes_one]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - SRA_N]
    extern msg_append_i64
    call msg_append_i64
    mov rdi, rax
    lea rsi, [rel sn_given]
    call rbt_append_cstr
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rbp - SRA_BUF]
    extern raise_exception
    call raise_exception
END_FUNC set_raise_arity

section .rodata
sn_set_dot:   db "set.", 0
sn_takes_one: db "() takes exactly one argument (", 0
sn_given:     db " given)", 0
sn_symmetric_difference:        db "symmetric_difference", 0
sn_symmetric_difference_update: db "symmetric_difference_update", 0
section .text

DEF_FUNC set_method_symmetric_difference, 8            ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smsd_error

    mov r14, [rdi]          ; self
    ; The method forms take any iterable where the operators take only a set.
    ; This read the argument as a PyDictObject whatever it was, so
    ; s.difference([1]) walked a list's header as a hash table, found nothing
    ; occupied, and answered s unchanged.
    mov rdi, [rdi + 8]
    call set_coerce_operand
    test rax, rax
    jz .smsd_fail
    mov r15, rax            ; owned: the set itself, or one built from it

    ; The result takes the left operand's kind: a frozenset operator or method
    ; answers a frozenset, not a set.
    mov rdi, r14
    call set_result_type
    mov rdi, rax
    call set_new_of_type
    mov rbx, rax            ; new set

    ; Add elements in self but NOT in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smsd_self_loop:
    cmp rcx, r13
    jge .smsd_other

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smsd_s_next

    push rax
    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    pop rcx
    test eax, eax
    jnz .smsd_s_next        ; in other, skip

    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    call set_add

.smsd_s_next:
    pop rcx
    inc ecx
    jmp .smsd_self_loop

.smsd_other:
    ; Add elements in other but NOT in self
    mov r12, [r15 + PyDictObject.entries]
    mov r13, [r15 + PyDictObject.capacity]
    xor ecx, ecx

.smsd_other_loop:
    cmp rcx, r13
    jge .smsd_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smsd_o_next

    push rax
    mov rdi, r14
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    pop rcx
    test eax, eax
    jnz .smsd_o_next        ; in self, skip

    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    call set_add

.smsd_o_next:
    pop rcx
    inc ecx
    jmp .smsd_other_loop

.smsd_done:
    mov rdi, r15
    call obj_decref     ; the coerced operand
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smsd_fail:
    ; not iterable, or iterating it raised
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.smsd_error:
    ; CPython names the method in full and reports the count:
    ;   set.symmetric_difference() takes exactly one argument (2 given)
    lea rdi, [rel sn_symmetric_difference]
    call set_raise_arity
END_FUNC set_method_symmetric_difference

;; ============================================================================
;; set_method_issubset(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if every element of self is in other.
;; ============================================================================
DEF_FUNC set_method_issubset, 8            ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smss_error

    mov r14, [rdi]          ; self
    ; The three predicates take any iterable, as the four builders beside them
    ; do: `{1}.issubset([1])` read a list's header as a hash table.  It found
    ; nothing occupied -- so issubset answered True for anything and isdisjoint
    ; answered True for everything -- and a two-element list made the probe run
    ; off the end of a table it had measured wrong, into "set: hash table
    ; full", which is a fatal_error and not an exception.
    mov rdi, [rdi + 8]
    call set_coerce_operand
    test rax, rax
    jz .smss_fail
    mov r15, rax            ; owned: the set itself, or one built from it

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smss_loop:
    cmp rcx, r13
    jge .smss_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smss_next

    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    test eax, eax
    jz .smss_false          ; not in other

.smss_next:
    pop rcx
    inc ecx
    jmp .smss_loop

.smss_true:
    mov rdi, r15
    call obj_decref
    RET_TRUE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smss_false:
    pop rcx                 ; balance the push in loop
    mov rdi, r15
    call obj_decref
    RET_FALSE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smss_fail:
    xor eax, eax                ; a NULL Value, with the exception pending
    xor edx, edx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smss_error:
    RAISE exc_TypeError_type, "issubset() takes exactly one argument"
END_FUNC set_method_issubset

;; ============================================================================
;; set_method_issuperset(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if every element of other is in self.
;; ============================================================================
DEF_FUNC set_method_issuperset, 8            ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smis_error

    mov r15, [rdi]          ; self (check contains)
    mov rdi, [rdi + 8]     ; other (iterate this); see issubset above
    call set_coerce_operand
    test rax, rax
    jz .smis_fail
    mov r14, rax            ; owned

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smis_loop:
    cmp rcx, r13
    jge .smis_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smis_next

    mov rdi, r15            ; check in self
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    test eax, eax
    jz .smis_false

.smis_next:
    pop rcx
    inc ecx
    jmp .smis_loop

.smis_true:
    mov rdi, r14
    call obj_decref
    RET_TRUE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smis_false:
    pop rcx
    mov rdi, r14
    call obj_decref
    RET_FALSE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smis_fail:
    xor eax, eax
    xor edx, edx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smis_error:
    RAISE exc_TypeError_type, "issuperset() takes exactly one argument"
END_FUNC set_method_issuperset

;; ============================================================================
;; set_method_isdisjoint(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if self and other have no common elements.
;; ============================================================================
DEF_FUNC set_method_isdisjoint, 8            ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smdj_error

    mov r14, [rdi]          ; self
    mov rdi, [rdi + 8]     ; other; see issubset above
    call set_coerce_operand
    test rax, rax
    jz .smdj_fail
    mov r15, rax            ; owned

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smdj_loop:
    cmp rcx, r13
    jge .smdj_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY], 0   ; occupied?
    je .smdj_next

    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    call set_contains
    test eax, eax
    jnz .smdj_false         ; found in other — not disjoint

.smdj_next:
    pop rcx
    inc ecx
    jmp .smdj_loop

.smdj_true:
    mov rdi, r15
    call obj_decref
    RET_TRUE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smdj_false:
    pop rcx
    mov rdi, r15
    call obj_decref
    RET_FALSE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.smdj_fail:
    xor eax, eax
    xor edx, edx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smdj_error:
    RAISE exc_TypeError_type, "isdisjoint() takes exactly one argument"
END_FUNC set_method_isdisjoint

;; ============================================================================
;; frozenset.__hash__(self) -> int
;;
;; frozenset's OWN hash, not the argument's.  With no __hash__ in
;; frozenset's tp_dict the lookup walked the MRO to object's, which answers
;; the address -- so frozenset.__hash__(f) and hash(f) disagreed on the one
;; type that exists to be a dict key.
;;
;; It cannot go through obj_hash the way generic_method_hash does: that reads
;; tp_hash, and a frozenset subclass defining __hash__ of its own has
;; slot_tp_hash there, so the unbound frozenset.__hash__ would re-enter the
;; subclass's method instead of naming the type it was reached through.
;; ============================================================================
global frozenset_dunder_hash
DEF_FUNC frozenset_dunder_hash
    REQUIRE_SELF frozenset_type, "__hash__"
    cmp rsi, 1
    jne .fdh_error
    mov rdi, [rdi]
    mov edx, TAG_PTR            ; tp_hash forwards edx to int_unwrap
    call frozenset_hash
    ; A hash does not fit an int immediate -- it is a full 64 bits, and
    ; adding v_int_bias to one lands in the float range.  int_from_i64 boxes
    ; it when it has to, which is what builtin_hash_fn does.
    mov rdi, rax
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret
.fdh_error:
    RAISE exc_TypeError_type, "__hash__() takes no arguments"
END_FUNC frozenset_dunder_hash

;; ============================================================================
;; set_sub_fill(rdi = the new instance, rsi = the args after cls, rdx = count)
;;
;; Fills a frozenset subclass from its constructor argument.  frozenset is
;; immutable, so -- exactly like tuple, which container_dunder_new already
;; special-cases for this reason -- its contents have to arrive through
;; __new__.  It has no __init__ of its own, as CPython's has none; a SET
;; subclass does, and fills there, which is why only the frozen side needs
;; this.
;; ============================================================================
SSF_ARGS  equ 16            ; a two-slot argument array for set_method_update
SSF_FRAME equ 32            ; + 0 pushes = 32, 16-aligned
global set_sub_fill
DEF_FUNC set_sub_fill, SSF_FRAME
    test rdx, rdx
    jz .ssf_done                ; frozenset() -- nothing to fill from

    mov [rbp - SSF_ARGS], rdi
    mov rax, [rsi]
    mov [rbp - SSF_ARGS + 8], rax
    lea rdi, [rbp - SSF_ARGS]
    mov esi, 2
    call set_method_update      ; returns None, which is ours to release
    V_UNPACK rax, rdx
    test edx, edx
    jz .ssf_done
    cmp edx, TAG_PTR
    jne .ssf_done
    mov rdi, rax
    call obj_decref

.ssf_done:
    leave
    ret
END_FUNC set_sub_fill
