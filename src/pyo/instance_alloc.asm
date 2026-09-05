; pyo/instance_alloc.asm - allocating an instance, and the builtin-subclass forms
;
; instance_new is where every user-defined class's instance comes from.  A
; class deriving from a builtin needs more than a zeroed body: the base's own
; fields are embedded in the instance, and list and dict want a real backing
; array before any method touches them -- a NULL ob_item is how list marks
; "currently being sorted", so the first append on a fresh subclass instance
; reported "list modified during sort".  int, str and tuple are immutable and
; carry their value inline, so each gets a constructor of its own that fills
; the base portion at allocation time and can be called as `int.__new__(cls,
; v)` without going back through cls.__new__ -- which is how enum builds its
; members, and would otherwise recurse forever.
;
; It came out of class.asm, which held the metatype, the instance, the bound
; method and this, and was over the size a hand-written file in this tree is
; allowed.

%include "macros.inc"
%include "object.inc"

extern object_type
extern exc_TypeError_type
extern raise_exception
extern str_from_cstr_heap
extern type_call
extern dict_get
extern raise_type_error_with_typename
extern new_dunder_cstr
extern init_dunder_cstr
extern ap_malloc
extern ap_memcpy
extern gc_alloc
extern gc_track
extern gc_dealloc
extern obj_incref
extern obj_decref
extern obj_dealloc
extern dict_new
extern dict_alloc_tables
extern str_set_length
extern builtin_int_fn
extern builtin_str_fn
extern tuple_type_call
extern int_type
extern tuple_type

global instance_new
global int_sub_new
global str_sub_new
global tuple_sub_fill
global builtin_sub_init_base
global builtin_sub_alloc
global builtin_sub_dealloc

section .text

;; ============================================================================
;; instance_new(PyTypeObject *type) -> PyInstanceObject*
;; Allocate a new instance of the given class type.
;; rdi = type (the class)
;; Returns: new instance with refcnt=1, ob_type=type, inst_dict=new dict
;; ============================================================================
;; ============================================================================
;; builtin_sub_init_base(rdi = instance)
;;
;; Give the embedded base portion of a builtin-container subclass a valid
;; empty state.  instance_new zeroes the body, which is already a correct
;; empty tuple, but list and dict want a real backing array -- a NULL
;; ob_item is how list marks "currently being sorted", so the first
;; l.append() on a fresh subclass instance reported "list modified during
;; sort".
;; ============================================================================

;; ============================================================================
;; int_sub_new(rdi = type, rsi = args, rdx = nargs) -> (rax, rdx) value pair
;;
;; An int, or an instance of an int subclass carrying one.  It is what
;; `int(...)` does for such a type, reachable as a function so that
;; `int.__new__(cls, v)` can build the instance WITHOUT going back through
;; cls.__new__ -- which is how enum makes its members, and would otherwise
;; recurse forever.
;; ============================================================================
ISN_TYPE  equ 8
ISN_VAL   equ 16
ISN_TAG   equ 24
ISN_FRAME equ 32          ; + 2 pushes = 48
DEF_FUNC int_sub_new, ISN_FRAME
    push rbx
    push r12
    mov [rbp - ISN_TYPE], rdi
    mov rdi, rsi
    mov rsi, rdx
    extern builtin_int_fn
    call builtin_int_fn
    test edx, edx
    jz .isn_fail
    mov [rbp - ISN_VAL], rax
    mov [rbp - ISN_TAG], rdx

    ; int itself takes the bare value; a subclass wraps it.
    mov rbx, [rbp - ISN_TYPE]
    lea rcx, [rel int_type]
    cmp rbx, rcx
    je .isn_bare

    mov edi, PyIntSubclassObject_size
    mov rsi, rbx
    call gc_alloc
    mov r12, rax
    mov qword [r12 + PyIntSubclassObject.inst_dict], 0
    mov rax, [rbp - ISN_VAL]
    mov rdx, [rbp - ISN_TAG]
    V_PACK rax, rdx
    mov [r12 + PyIntSubclassObject.int_value], rax   ; the reference transfers
    mov rdi, rbx
    INCREF rdi
    mov rdi, r12
    call gc_track
    mov rax, r12
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.isn_bare:
    mov rax, [rbp - ISN_VAL]
    mov rdx, [rbp - ISN_TAG]
    pop r12
    pop rbx
    leave
    ret
.isn_fail:
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret
END_FUNC int_sub_new

;; ============================================================================
;; str_sub_new(rdi = subclass type, rsi = args, rdx = nargs) -> instance
;;
;; A str keeps its characters inline, so its instances are variable-size and
;; instance_new -- which allocates exactly tp_basicsize -- cannot make one.
;; A str subclass therefore has to be built here, from the argument, the way
;; str's own constructor would.  Without this the instance was an empty
;; string of the right type, so CustomStr("100") was "".
;;
;; The instance carries a __dict__ at its tail, past the data and its padding,
;; because there is no fixed offset past inline data to put one at.  The extra
;; word is allocated here and tp_dictoffset says TP_DICT_AT_TAIL.
;; ============================================================================
SSN_TYPE  equ 8
SSN_SRC   equ 16
SSN_FRAME equ 32            ; + 2 pushes = 48

DEF_FUNC str_sub_new, SSN_FRAME
    push rbx
    push r12

    mov [rbp - SSN_TYPE], rdi
    mov qword [rbp - SSN_SRC], 0
    test rdx, rdx
    jz .ssn_empty

    ; str(x) of the arguments gives a plain str to copy from.  This called
    ; obj_str on args[0] and ignored the rest, so a str subclass could not be
    ; built from the DECODING form: S(b"abc", "utf-8") came out as the repr
    ; "b'abc'".  builtin_str_fn is the whole of str(), keyword arguments
    ; included, and its one-argument case is the same obj_str.
    mov rdi, rsi
    mov rsi, rdx
    extern builtin_str_fn
    call builtin_str_fn
    V_UNPACK rax, rdx
    test edx, edx
    jz .ssn_failed
    mov [rbp - SSN_SRC], rax
    mov rbx, rax
    mov r12, [rbx + PyStrObject.ob_size]
    jmp .ssn_have_src

.ssn_empty:
    xor ebx, ebx
    xor r12d, r12d

.ssn_have_src:
    ; header + length + 8, matching str_new_heap's padding for the 8-byte
    ; comparisons ap_strcmp does, + 8 more for the tail __dict__ pointer, and
    ; one word per __slots__ entry after that.  A str subclass has nowhere
    ; else to put a slot: its characters are inline, so a fixed offset past
    ; the header lands on them.
    mov rdi, [rbp - SSN_TYPE]
    mov rcx, [rdi + PyTypeObject.tp_tailslots]
    shl rcx, 3
    lea rdi, [r12 + PyStrObject.data + 16]
    add rdi, rcx
    mov rsi, [rbp - SSN_TYPE]
    extern gc_alloc
    call gc_alloc                   ; sets ob_refcnt and ob_type
    mov [rax + PyStrObject.ob_size], r12
    mov qword [rax + PyStrObject.ob_hash], -1
    mov [rax + PyStrObject.ob_length], r12   ; corrected after the copy
    mov qword [rax + PyStrObject.data + r12], 0
    ; The tail __dict__ and every tail slot start empty.  gc_alloc does not
    ; zero, and a slot read before it is written is a Value either way.
    mov rcx, [rbp - SSN_TYPE]
    mov rcx, [rcx + PyTypeObject.tp_tailslots]
    inc rcx                         ; the dict word, then the slots
    lea rdx, [rax + PyStrObject.data + r12 + 8]
.ssn_zero_tail:
    mov qword [rdx], 0
    add rdx, 8
    dec rcx
    jnz .ssn_zero_tail

    test rbx, rbx
    jz .ssn_no_copy
    push rax
    lea rdi, [rax + PyStrObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    call ap_memcpy
    mov rdi, [rsp]
    extern str_set_length
    call str_set_length
    mov rdi, [rbp - SSN_SRC]
    call obj_decref
    pop rax

.ssn_no_copy:
    ; The tail __dict__, unless __slots__ suppresses it.  It is created here
    ; rather than lazily so that every consumer of LOAD_INST_DICT can keep
    ; reading a NULL as "this family has no dict at all".  SSN_SRC is dead by
    ; now -- the copy path decref'd it.
    mov [rbp - SSN_SRC], rax
    mov rdi, [rbp - SSN_TYPE]
    mov rcx, [rdi + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_HAS_SLOTS
    jnz .ssn_no_tail_dict
    cmp qword [rdi + PyTypeObject.tp_dictoffset], TP_DICT_AT_TAIL
    jne .ssn_no_tail_dict
    extern dict_new
    call dict_new
    mov rdx, [rbp - SSN_SRC]
    INST_DICT_TAIL rcx, rdx
    mov [rcx], rax
.ssn_no_tail_dict:
    mov rax, [rbp - SSN_SRC]

    ; gc_alloc does not INCREF the type it stamps into ob_type.
    push rax
    mov rdi, [rbp - SSN_TYPE]
    call obj_incref
    pop rax
    mov rdi, rax
    push rax
    extern gc_track
    call gc_track
    pop rax
    pop r12
    pop rbx
    leave
    ret

.ssn_failed:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_sub_new

;; ============================================================================
;; tuple_sub_fill(rdi = instance, rsi = args, rdx = nargs)
;;
;; A tuple is immutable and has no __init__, so a subclass cannot be filled
;; after the fact the way list, dict and set are -- the contents have to be
;; put in at construction, which is what tuple.__new__ does.  Without this a
;; tuple subclass was always empty.
;; ============================================================================
TSF_INST  equ 8
TSF_TMP   equ 16
TSF_FRAME equ 40            ; + 3 pushes = 64, 16-aligned

DEF_FUNC tuple_sub_fill, TSF_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - TSF_INST], rdi
    mov qword [rbp - TSF_TMP], 0
    mov qword [rdi + PyTupleObject.ob_hash], -1
    test rdx, rdx
    jz .tsf_done                ; Sub() is the empty tuple

    ; Materialise the argument, so any iterable works.
    push rsi
    lea rdi, [rel tuple_type]
    mov edx, 1
    pop rsi
    call tuple_type_call
    mov [rbp - TSF_TMP], rax
    mov rbx, rax
    mov r12, [rbx + PyTupleObject.ob_size]
    test r12, r12
    jz .tsf_release

    ; Own copy of the item array: the temporary is about to be released.
    mov rdi, r12
    shl rdi, 3
    call ap_malloc
    mov r13, rax
    mov rcx, [rbp - TSF_INST]
    mov [rcx + PyTupleObject.ob_item], r13
    mov [rcx + PyTupleObject.ob_size], r12

    xor ecx, ecx
.tsf_copy:
    cmp rcx, r12
    jge .tsf_release
    mov rax, [rbx + PyTupleObject.ob_item]
    mov rdi, [rax + rcx * 8]
    mov [r13 + rcx * 8], rdi
    push rcx
    INCREF_V rdi, rax
    pop rcx
    inc rcx
    jmp .tsf_copy

.tsf_release:
    mov rdi, [rbp - TSF_TMP]
    mov qword [rbp - TSF_TMP], 0
    call obj_decref

.tsf_done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_sub_fill

DEF_FUNC builtin_sub_init_base, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_flags]

    test rax, TYPE_FLAG_LIST_SUBCLASS
    jnz .bsib_list
    test rax, TYPE_FLAG_DICT_SUBCLASS | TYPE_FLAG_SET_SUBCLASS
    jnz .bsib_dict
    jmp .bsib_done              ; tuple: zeroed is already an empty tuple

.bsib_list:
    mov edi, 4 * 8
    call ap_malloc
    mov [rbx + PyListObject.ob_item], rax
    mov qword [rbx + PyListObject.allocated], 4
    jmp .bsib_done

.bsib_dict:
    ; A dict now owns two arrays, and a set only one -- so let the dict's own
    ; allocator build them rather than hand-rolling a header that would be
    ; missing dk_indices.
    mov rdi, rbx
    mov rax, [rbx + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_SET_SUBCLASS
    jnz .bsib_set_table
    mov rsi, DICT_INIT_CAP
    extern dict_alloc_tables
    call dict_alloc_tables
    mov qword [rbx + PyDictObject.dk_nentries], 0
    jmp .bsib_done

.bsib_set_table:
    ; A set keeps the old single-array layout.
    mov edi, DICT_INIT_CAP * DICT_ENTRY_SIZE
    call ap_malloc
    mov [rbx + PyDictObject.entries], rax
    mov rdi, rax
    mov ecx, DICT_INIT_CAP * DICT_ENTRY_SIZE / 8
    xor eax, eax
    rep stosq
    mov qword [rbx + PyDictObject.capacity], DICT_INIT_CAP

.bsib_done:
    pop rbx
    leave
    ret
END_FUNC builtin_sub_init_base

;; ============================================================================
;; builtin_sub_alloc(rdi = type) -> rax = a zeroed instance
;;
;; The allocation half of a builtin constructor that has to honour the type it
;; was handed.  float and complex keep their value inline, exactly as int and
;; str do, so a subclass of either cannot come from instance_new -- the base's
;; own constructor builds it, and this is the only part that differs between
;; the base and a subclass.
;;
;; A heaptype always carries TYPE_FLAG_HAVE_GC, so it has to come from
;; gc_alloc and be tracked.  complex itself does not: it owns nothing, and
;; gc_alloc hands back raw + GC_HEAD_SIZE, which obj_dealloc's plain-free path
;; would give ap_free unshifted.  Both branches are here so a caller cannot
;; pick the wrong one.
;;
;; Everything past the header is zeroed, the tail __dict__ slot included: a
;; subclass instance is reachable before its __init__ has run, and the
;; collector reads tp_dictoffset on the way past.
;; ============================================================================
BSA_TYPE  equ 8
BSA_SAVE  equ 16
BSA_FRAME equ 16            ; + 0 pushes = 16

DEF_FUNC builtin_sub_alloc, BSA_FRAME
    mov [rbp - BSA_TYPE], rdi
    mov rsi, [rdi + PyTypeObject.tp_basicsize]
    test qword [rdi + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .bsa_plain
    mov rdi, rsi
    mov rsi, [rbp - BSA_TYPE]
    call gc_alloc               ; sets ob_refcnt and ob_type itself
    jmp .bsa_zero
.bsa_plain:
    mov rdi, rsi
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    mov rcx, [rbp - BSA_TYPE]
    mov [rax + PyObject.ob_type], rcx

.bsa_zero:
    mov rcx, [rbp - BSA_TYPE]
    mov rcx, [rcx + PyTypeObject.tp_basicsize]
    lea rdx, [rax + PyObject_size]
    sub rcx, PyObject_size
.bsa_zero_loop:
    cmp rcx, 8
    jb .bsa_zeroed
    mov qword [rdx], 0
    add rdx, 8
    sub rcx, 8
    jmp .bsa_zero_loop

.bsa_zeroed:
    ; The instance holds a reference to its type, as every instance does.
    mov rcx, [rbp - BSA_TYPE]
    inc qword [rcx + PyObject.ob_refcnt]
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .bsa_done
    mov [rbp - BSA_SAVE], rax
    mov rdi, rax
    call gc_track               ; may collect, which is why the body is zeroed
    mov rax, [rbp - BSA_SAVE]
.bsa_done:
    leave
    ret
END_FUNC builtin_sub_alloc

DEF_FUNC instance_new
    push rbx
    push r12

    mov rbx, rdi                ; rbx = type

    ; Allocate using tp_basicsize (GC-tracked, supports __slots__)
    mov rdi, [rbx + PyTypeObject.tp_basicsize]
    push rdi                    ; save size for zero-fill
    mov rsi, rbx                ; type
    call gc_alloc
    mov r12, rax                ; r12 = instance (ob_refcnt=1, ob_type set)

    ; Zero-fill body past header (handles slot init to TAG_NULL)
    pop rcx                     ; size in bytes
    sub rcx, OBJ_HEADER_SIZE
    jle .skip_zero
    lea rdi, [r12 + OBJ_HEADER_SIZE]
    shr rcx, 3
    xor eax, eax
    rep stosq
.skip_zero:

    ; INCREF type (stored in ob_type)
    mov rdi, rbx
    call obj_incref

    ; Create inst_dict only if class doesn't have __slots__ (or has __dict__ in __slots__)
    mov rax, [rbx + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HAS_SLOTS
    jnz .in_no_dict              ; __slots__ suppresses inst_dict

    cmp qword [rbx + PyTypeObject.tp_dictoffset], 0
    je .in_no_dict              ; this family's instances carry no dict
    cmp qword [rbx + PyTypeObject.tp_dictoffset], TP_DICT_AT_TAIL
    je .in_no_dict              ; a tail dict belongs to str_sub_new, not here
    call dict_new
    STORE_INST_DICT r12, rax, rcx, .in_no_dict

.in_no_dict:
    mov rdi, r12
    call gc_track

    mov rax, r12                ; return instance
    pop r12
    pop rbx
    leave
    ret
END_FUNC instance_new

;; ============================================================================
;; builtin_sub_dealloc(PyObject *self)
;; Dealloc for heap-type subclasses of builtin types (bytes, bytearray, etc.)
;; These don't have inst_dict — just DECREF the type and free.
;; ============================================================================
DEF_FUNC builtin_sub_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    ; Save ob_type before freeing (gc_dealloc reads ob_type)
    push qword [rbx + PyObject.ob_type]

    ; Free the object (may be GC-tracked) — must happen before type DECREF
    mov rdi, rbx
    call gc_dealloc

    ; DECREF ob_type (the class) AFTER freeing the object
    pop rdi
    call obj_decref

    pop rbx
    leave
    ret
END_FUNC builtin_sub_dealloc


;; ============================================================================
;; object_type_call(args, nargs) -> PyObject*
;; object() returns a bare instance of object_type
;; ============================================================================
DEF_FUNC_BARE object_type_call
    ; Create a bare instance with object_type (gc_alloc since HAVE_GC)
    push rbp
    mov rbp, rsp
    ; object() takes nothing.  It accepted anything and dropped it, so
    ; `object(1)` was an object rather than the TypeError CPython raises.
    ; This sits in object's tp_new, so the count is in edx -- rsi is the
    ; argument array.
    test edx, edx
    jnz .otc_no_args
    mov edi, OBJ_HEADER_SIZE
    lea rsi, [rel object_type]
    call gc_alloc

    ; gc_alloc does not INCREF the type it stamps into ob_type, and
    ; instance_dealloc DECREFs it -- so without this the reference count of
    ; object_type itself went down by one for every object() that died.  It
    ; starts at 1, so the FIRST such instance took it to zero and handed
    ; &object_type, a .data address, to ap_free: the heap was corrupted from
    ; then on, and the crash landed in whatever allocated next.
    ; instance_new and slots_new both INCREF here for the same reason.
    push rax
    lea rdi, [rel object_type]
    call obj_incref
    pop rax

    ; Track in GC
    push rax
    mov rdi, rax
    call gc_track
    pop rax
    mov edx, TAG_PTR
    pop rbp
    ret

.otc_no_args:
    RAISE exc_TypeError_type, "object() takes no arguments"
END_FUNC object_type_call

;; ============================================================================
;; type_defines_dunder(rdi = a type, rsi = the name as a C string,
;;                     edx = the PyTypeObject slot that stands for it, or 0)
;;   -> eax = 1 if some class BEFORE object on the MRO defines it, else 0
;;
;; What CPython asks as `type->tp_new != object_new`.  object's own entry is
;; not a definition: it is the default the question is trying to distinguish
;; from, so the walk stops there.
;;
;; A builtin defines its constructor in a SLOT and not in its dict -- bytes
;; has a tp_new and no `__new__` key -- so the slot is asked as well, and a
;; subclass inherits it by pointer.  Passing 0 for it asks the dict alone.
;; ============================================================================
TDD_TYPE  equ 8
TDD_NAME  equ 16
TDD_SLOT  equ 24
TDD_FRAME equ 40            ; + 1 push = 48, 16-aligned
global type_defines_dunder
DEF_FUNC type_defines_dunder, TDD_FRAME
    push rbx
    mov [rbp - TDD_TYPE], rdi
    movsxd rdx, edx
    mov [rbp - TDD_SLOT], rdx
    mov rdi, rsi
    call str_from_cstr_heap
    mov [rbp - TDD_NAME], rax
    mov rbx, [rbp - TDD_TYPE]
.tdd_walk:
    test rbx, rbx
    jz .tdd_no
    lea rax, [rel object_type]
    cmp rbx, rax
    je .tdd_no

    ; The slot first: a builtin defines its constructor there and not in its
    ; dict, and a subclass of one inherits it by pointer.
    mov rdx, [rbp - TDD_SLOT]
    test rdx, rdx
    jz .tdd_dict
    mov rax, [rbx + rdx]
    test rax, rax
    jz .tdd_dict
    lea rcx, [rel object_type_call]
    cmp rax, rcx
    je .tdd_dict
    lea rcx, [rel type_call]
    cmp rax, rcx
    je .tdd_dict
    extern object_method_init
    lea rcx, [rel object_method_init]
    cmp rax, rcx
    je .tdd_dict
    jmp .tdd_yes

.tdd_dict:
    mov rdi, [rbx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .tdd_next
    mov rsi, [rbp - TDD_NAME]
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx               ; the tag: a hit may be a false-looking payload
    jnz .tdd_yes
.tdd_next:
    MRO_NEXT rbx, [rbp - TDD_TYPE]
    jmp .tdd_walk
.tdd_yes:
    mov rdi, [rbp - TDD_NAME]
    call obj_decref
    mov eax, 1
    pop rbx
    leave
    ret
.tdd_no:
    mov rdi, [rbp - TDD_NAME]
    call obj_decref
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC type_defines_dunder

;; ============================================================================
;; object_new_fn(args, nargs) -> instance
;; Implements object.__new__(cls) — creates a bare instance of cls.
;; args[0] = cls (the type to instantiate)
;; ============================================================================
ONF_TYPE  equ 8
ONF_FRAME equ 16            ; + 0 pushes = 16, 16-aligned
DEF_FUNC object_new_fn, ONF_FRAME
    ; args[0] = cls
    mov rdi, [rdi]              ; cls payload (PyTypeObject*)
    mov [rbp - ONF_TYPE], rdi
    ; Excess arguments are CPython's object_new error, not something to
    ; drop: a class that overrides neither half has nowhere to put them.
    cmp rsi, 1
    jbe .onf_build
    lea rsi, [rel new_dunder_cstr]
    mov edx, PyTypeObject.tp_new
    call type_defines_dunder
    test eax, eax
    jnz .onf_own_new
    mov rdi, [rbp - ONF_TYPE]
    lea rsi, [rel init_dunder_cstr]
    mov edx, PyTypeObject.tp_init
    call type_defines_dunder
    test eax, eax
    jz .onf_no_args
.onf_build:
    mov rdi, [rbp - ONF_TYPE]
    call instance_new
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.onf_own_new:
    RAISE exc_TypeError_type, \
          "object.__new__() takes exactly one argument (the type to instantiate)"
.onf_no_args:
    mov rsi, [rbp - ONF_TYPE]
    CSTRING rdi, `\x01() takes no arguments`
    jmp raise_type_error_with_typename
END_FUNC object_new_fn
