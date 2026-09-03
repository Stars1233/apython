; pyo/code.asm - Code object type

%include "macros.inc"
%include "object.inc"

extern exc_TypeError_type
extern raise_exception
extern none_singleton
extern ap_free
extern ap_malloc
extern ap_memcpy
extern ap_memset
extern ap_strcmp
extern obj_decref
extern obj_dealloc
extern obj_incref
extern str_from_cstr
extern type_type
; code objects are not GC-tracked (allocated by marshal via ap_malloc)

; --- code_new frame layout ---
CN_SPEC  equ 8
CN_CODE  equ 16
CN_FRAME equ 16             ; + 2 pushes = 32

;; ============================================================================
;; code_new(CodeSpec *spec) -> PyCodeObject*
;;
;; The runtime code-object constructor, used by the source compiler.  Mirrors
;; mdo_code (src/marshal.asm:850) field for field; the two are the only places
;; that build a PyCodeObject, and they must stay in step.
;;
;; Every object reference in the spec is STOLEN.  On the error path the caller
;; calls code_spec_clear(spec) instead, which releases exactly the same set.
;;
;; Two things the layout forces, both of which are silent if got wrong:
;;   - The bytecode lives INLINE at +128.  eval_frame does
;;     `lea rbx, [rax + PyCodeObject.co_code]`, so it cannot be a separate
;;     bytes object.  It must also be writable heap: the interpreter rewrites
;;     opcodes in place to specialize them (opcodes_build.asm:970 turns
;;     FOR_ITER into FOR_ITER_RANGE), so no two code objects may share it.
;;   - CODE_TAIL_PAD zero bytes follow it.  op_load_global writes eight bytes
;;     of inline cache at [rbx+2..rbx+7] and op_compare_op reads byte [rbx+2];
;;     either can sit on the last instruction of the object.
;; ============================================================================

;; ============================================================================
;; code_traverse / code_clear
;;
;; A code object holds nine owned objects, and one of them -- co_consts -- can
;; hold another code object.  With no tp_traverse the collector could not see
;; through any of them, so a module's code holding a function's code holding
;; a reference back through func_globals was a cycle it could not break.
;;
;; The nine are not contiguous: co_code_len, co_posonlyargcount and
;; co_firstlineno sit between co_exceptiontable and co_linetable, so this
;; cannot be a strided loop the way code_spec_clear is.
;; ============================================================================
global code_traverse
DEF_FUNC code_traverse
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyCodeObject.co_consts]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_names]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_localsplusnames]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_localspluskinds]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_filename]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_name]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_qualname]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_exceptiontable]
    VISIT_PTR rdi
    mov rdi, [rbx + PyCodeObject.co_linetable]
    VISIT_PTR rdi
    pop rbx
    leave
    ret
END_FUNC code_traverse

;; Only co_consts, which is the field that can close a cycle.  The rest are
;; strings and bytes that hold nothing, and the eval loop reads co_names and
;; the two localsplus tables through r14 and the frame -- clearing those out
;; from under a frame that is still unwinding would be worse than the cycle.
global code_clear
DEF_FUNC code_clear
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyCodeObject.co_consts]
    test rdi, rdi
    jz .cc_done
    mov qword [rbx + PyCodeObject.co_consts], 0
    call obj_decref
.cc_done:
    pop rbx
    leave
    ret
END_FUNC code_clear

DEF_FUNC code_new, CN_FRAME
    push rbx
    push r12

    mov [rbp - CN_SPEC], rdi
    mov rbx, rdi                            ; rbx = spec

    ; ap_malloc(sizeof(header) + code_len + CODE_TAIL_PAD)
    mov rdi, [rbx + CodeSpec.code_len]
    add rdi, PyCodeObject.co_code + CODE_TAIL_PAD
    lea rsi, [rel code_type]
    extern gc_alloc
    call gc_alloc                           ; fatal on OOM, never returns NULL
    mov r12, rax
    mov [rbp - CN_CODE], rax

    ; --- object header ---
    mov qword [r12 + PyObject.ob_refcnt], 1
    lea rax, [rel code_type]
    mov [r12 + PyObject.ob_type], rax

    ; --- scalar fields ---
    mov eax, [rbx + CodeSpec.argcount]
    mov [r12 + PyCodeObject.co_argcount], eax
    mov eax, [rbx + CodeSpec.kwonlyargcount]
    mov [r12 + PyCodeObject.co_kwonlyargcount], eax
    mov eax, [rbx + CodeSpec.posonlyargcount]
    mov [r12 + PyCodeObject.co_posonlyargcount], eax
    mov eax, [rbx + CodeSpec.stacksize]
    mov [r12 + PyCodeObject.co_stacksize], eax
    mov eax, [rbx + CodeSpec.flags]
    mov [r12 + PyCodeObject.co_flags], eax
    mov eax, [rbx + CodeSpec.firstlineno]
    mov [r12 + PyCodeObject.co_firstlineno], eax
    mov dword [r12 + PyCodeObject.co_pad0], 0

    ; co_nlocals is the true len(varnames) from the spec.  (marshal stores
    ; nlocalsplus there instead; nothing but code_getattr reads the field, so
    ; the two disagree harmlessly and this one matches CPython.)
    mov eax, [rbx + CodeSpec.nlocals]
    mov [r12 + PyCodeObject.co_nlocals], eax

    ; co_nlocalsplus is derived from the tuple, exactly as marshal derives it
    xor eax, eax
    mov rdx, [rbx + CodeSpec.localsplusnames]
    test rdx, rdx
    jz .no_lpnames
    mov eax, [rdx + PyVarObject.ob_size]
.no_lpnames:
    mov [r12 + PyCodeObject.co_nlocalsplus], eax

    ; --- object fields: references are stolen, so no INCREF ---
    mov rax, [rbx + CodeSpec.consts]
    mov [r12 + PyCodeObject.co_consts], rax
    mov rax, [rbx + CodeSpec.names]
    mov [r12 + PyCodeObject.co_names], rax
    mov rax, [rbx + CodeSpec.localsplusnames]
    mov [r12 + PyCodeObject.co_localsplusnames], rax
    mov rax, [rbx + CodeSpec.localspluskinds]
    mov [r12 + PyCodeObject.co_localspluskinds], rax
    mov rax, [rbx + CodeSpec.filename]
    mov [r12 + PyCodeObject.co_filename], rax
    mov rax, [rbx + CodeSpec.name]
    mov [r12 + PyCodeObject.co_name], rax
    mov rax, [rbx + CodeSpec.qualname]
    mov [r12 + PyCodeObject.co_qualname], rax
    mov rax, [rbx + CodeSpec.exceptiontable]
    mov [r12 + PyCodeObject.co_exceptiontable], rax
    mov rax, [rbx + CodeSpec.linetable]
    mov [r12 + PyCodeObject.co_linetable], rax

    ; --- bytecode, copied inline, then the zeroed tail ---
    mov rax, [rbx + CodeSpec.code_len]
    mov [r12 + PyCodeObject.co_code_len], eax

    lea rdi, [r12 + PyCodeObject.co_code]
    mov rsi, [rbx + CodeSpec.code_bytes]
    mov rdx, [rbx + CodeSpec.code_len]
    test rdx, rdx
    jz .no_code
    call ap_memcpy
.no_code:
    lea rdi, [r12 + PyCodeObject.co_code]
    add rdi, [rbx + CodeSpec.code_len]
    xor esi, esi
    mov edx, CODE_TAIL_PAD
    call ap_memset

    ; gc_track only now: it can trigger a collection, and the traverse would
    ; walk fields that were not yet written.
    mov rdi, r12
    extern gc_track
    call gc_track

    mov rax, r12
    pop r12
    pop rbx
    leave
    ret
END_FUNC code_new

;; ============================================================================
;; code_spec_clear(CodeSpec *spec)
;; Release every object reference the spec holds and zero the slots.  This is
;; the single error path for a half-built spec: because code_new steals exactly
;; this set, "call code_new" and "call code_spec_clear" are the only two ways a
;; spec can be disposed of, and neither can double-free the other's references.
;; ============================================================================
DEF_FUNC code_spec_clear
    push rbx
    push r12
    mov rbx, rdi
    lea r12, [rbx + CodeSpec.consts]        ; first object slot
.loop:
    lea rax, [rbx + CodeSpec.linetable]
    cmp r12, rax
    ja .done
    mov rdi, [r12]
    test rdi, rdi
    jz .next
    mov qword [r12], 0
    call obj_decref
.next:
    add r12, 8
    jmp .loop
.done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC code_spec_clear

; code_dealloc(PyObject *self)
; Free code object and decref contained objects
DEF_FUNC code_dealloc
    push rbx
    push r12
    push r13
    mov rbx, rdi

    ; co_consts is a fat tuple — just DECREF it (tuple_dealloc handles elements)
    mov rdi, [rbx + PyCodeObject.co_consts]
    test rdi, rdi
    jz .skip_consts
    call obj_decref
.skip_consts:

    ; DECREF co_names
    mov rdi, [rbx + PyCodeObject.co_names]
    test rdi, rdi
    jz .skip_names
    call obj_decref
.skip_names:

    ; DECREF co_localsplusnames
    mov rdi, [rbx + PyCodeObject.co_localsplusnames]
    test rdi, rdi
    jz .skip_locals
    call obj_decref
.skip_locals:

    ; DECREF co_localspluskinds
    mov rdi, [rbx + PyCodeObject.co_localspluskinds]
    test rdi, rdi
    jz .skip_kinds
    call obj_decref
.skip_kinds:

    ; DECREF co_linetable
    mov rdi, [rbx + PyCodeObject.co_linetable]
    test rdi, rdi
    jz .skip_linetable
    call obj_decref
.skip_linetable:

    ; DECREF co_filename
    mov rdi, [rbx + PyCodeObject.co_filename]
    test rdi, rdi
    jz .skip_filename
    call obj_decref
.skip_filename:

    ; DECREF co_name
    mov rdi, [rbx + PyCodeObject.co_name]
    test rdi, rdi
    jz .skip_name
    call obj_decref
.skip_name:

    ; DECREF co_qualname
    mov rdi, [rbx + PyCodeObject.co_qualname]
    test rdi, rdi
    jz .skip_qualname
    call obj_decref
.skip_qualname:

    ; DECREF co_exceptiontable
    mov rdi, [rbx + PyCodeObject.co_exceptiontable]
    test rdi, rdi
    jz .skip_exc
    call obj_decref
.skip_exc:

    ; Free the code object itself
    mov rdi, rbx
    extern gc_dealloc
    call gc_dealloc

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC code_dealloc

; code_repr(PyObject *self) -> PyStrObject*
DEF_FUNC_BARE code_repr
    lea rdi, [rel code_repr_str]
    jmp str_from_cstr
END_FUNC code_repr

; code_getattr(PyCodeObject *self, PyObject *name) -> (rax, edx) or NULL
; rdi = code object, rsi = name string
DEF_FUNC code_getattr
    push rbx
    push r12

    mov rbx, rdi            ; rbx = code
    mov r12, rsi            ; r12 = name

    ; Check for co_kwonlyargcount
    lea rdi, [rel co_attr_kwonlyargcount]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .return_kwonlyargcount

    ; Check for co_argcount
    lea rdi, [rel co_attr_argcount]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .return_argcount

    ; Check for co_varnames (return co_localsplusnames)
    lea rdi, [rel co_attr_varnames]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .return_varnames

    ; co_positions() is a method, not a field.
    lea rdi, [rel co_attr_positions]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .return_positions

    ; Everything else is a straight field read.  Only three of the seventeen
    ; co_* were reachable from Python, which is most of what inspect,
    ; dataclasses and traceback formatting want from a code object.
    lea r8, [rel code_attr_table]
.cg_scan:
    mov rdi, [r8]                   ; name cstr, 0 terminates the table
    test rdi, rdi
    jz .cg_not_found
    push r8
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    pop r8
    test eax, eax
    jz .cg_found
    add r8, 24
    jmp .cg_scan

.cg_found:
    mov rcx, [r8 + 8]               ; byte offset into PyCodeObject
    mov rdx, [r8 + 16]              ; 0 = qword ptr, 1 = dword int
    test rdx, rdx
    jnz .cg_int_field
    mov rax, [rbx + rcx]
    test rax, rax
    jz .cg_none
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.cg_none:
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.cg_int_field:
    movsxd rax, dword [rbx + rcx]
    mov edx, TAG_SMALLINT
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.cg_not_found:
    ; Not found
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.return_positions:
    ; A *bound* method: `code.co_positions()` and `f = code.co_positions`
    ; must both know which code object they belong to.
    call _get_co_positions_builtin
    mov rdi, rax
    mov rsi, rbx
    extern method_new
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.return_kwonlyargcount:
    movsxd rax, dword [rbx + PyCodeObject.co_kwonlyargcount]
    mov edx, TAG_SMALLINT
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.return_argcount:
    movsxd rax, dword [rbx + PyCodeObject.co_argcount]
    mov edx, TAG_SMALLINT
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.return_varnames:
    mov rax, [rbx + PyCodeObject.co_localsplusnames]
    test rax, rax
    jz .return_none
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.return_none:
    xor eax, eax
    RET_NONE
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC code_getattr

;; ============================================================================
;; code.co_positions() -> an iterable of (lineno, end_lineno, col, end_col)
;;
;; CPython's traceback.py calls this on every traceback entry, to underline
;; the failing expression.  This tree records lines and not columns yet (the
;; caret work is its own job), and CPython's own format allows None for a
;; missing column -- so what comes back is one (line, line, None, None) per
;; code unit, which is enough for traceback.py to run and gives it exactly
;; the information there is.
;;
;; A list rather than a generator: the caller indexes into it with
;; islice, and a list is the shortest thing that supports that.
;; ============================================================================
CPO_CODE  equ 8
CPO_LIST  equ 16
CPO_I     equ 24
CPO_N     equ 32
CPO_FRAME equ 48            ; + 0 pushes = 48
DEF_FUNC code_method_co_positions, CPO_FRAME
    test rsi, rsi
    jz .cpo_args
    mov rdi, [rdi]
    mov [rbp - CPO_CODE], rdi
    mov eax, [rdi + PyCodeObject.co_code_len]
    shr eax, 1                  ; code units, two bytes each
    mov [rbp - CPO_N], rax

    mov rdi, rax
    extern list_new
    call list_new
    test rax, rax
    jz .cpo_failed
    mov [rbp - CPO_LIST], rax
    mov qword [rbp - CPO_I], 0

.cpo_loop:
    mov rax, [rbp - CPO_I]
    cmp rax, [rbp - CPO_N]
    jge .cpo_done
    mov rdi, [rbp - CPO_CODE]
    mov rsi, rax
    extern code_addr2line
    call code_addr2line
    push rax
    sub rsp, 8
    mov edi, 4
    extern tuple_new
    call tuple_new
    add rsp, 8
    pop rcx
    test rax, rax
    jz .cpo_drop
    mov rdx, [rax + PyTupleObject.ob_item]
    mov rsi, rcx
    V_PACK_I64 rsi, r8
    mov [rdx], rsi
    mov [rdx + 8], rsi
    LOAD_NONE rsi
    INCREF rsi
    mov [rdx + 16], rsi
    INCREF rsi
    mov [rdx + 24], rsi

    push rax
    sub rsp, 8
    mov rdi, [rbp - CPO_LIST]
    mov rsi, rax
    extern list_append
    call list_append
    add rsp, 8
    pop rdi
    extern obj_decref
    call obj_decref             ; list_append took its own reference
    inc qword [rbp - CPO_I]
    jmp .cpo_loop

.cpo_done:
    mov rax, [rbp - CPO_LIST]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.cpo_drop:
    mov rdi, [rbp - CPO_LIST]
    call obj_decref
.cpo_failed:
    xor eax, eax
    leave
    ret
.cpo_args:
    RAISE exc_TypeError_type, "co_positions() takes no arguments"
END_FUNC code_method_co_positions

section .rodata
align 8
; {name, offset, is_int} -- 0 name terminates
co_n_name:        db "co_name", 0
co_n_qualname:    db "co_qualname", 0
co_n_filename:    db "co_filename", 0
co_n_consts:      db "co_consts", 0
co_n_names:       db "co_names", 0
co_n_localsplusnames: db "co_localsplusnames", 0
co_n_firstlineno: db "co_firstlineno", 0
co_n_flags:       db "co_flags", 0
co_n_nlocals:     db "co_nlocals", 0
co_n_stacksize:   db "co_stacksize", 0
co_n_posonly:     db "co_posonlyargcount", 0
align 8
code_attr_table:
    dq co_n_name,        PyCodeObject.co_name,        0
    dq co_n_qualname,    PyCodeObject.co_qualname,    0
    dq co_n_filename,    PyCodeObject.co_filename,    0
    dq co_n_consts,      PyCodeObject.co_consts,      0
    dq co_n_names,       PyCodeObject.co_names,       0
    dq co_n_localsplusnames, PyCodeObject.co_localsplusnames, 0
    dq co_n_firstlineno, PyCodeObject.co_firstlineno, 1
    dq co_n_flags,       PyCodeObject.co_flags,       1
    dq co_n_nlocals,     PyCodeObject.co_nlocals,     1
    dq co_n_stacksize,   PyCodeObject.co_stacksize,   1
    dq co_n_posonly,     PyCodeObject.co_posonlyargcount, 1
    dq 0, 0, 0
section .text


section .data

co_attr_kwonlyargcount: db "co_kwonlyargcount", 0
co_attr_argcount:       db "co_argcount", 0
co_attr_varnames:       db "co_varnames", 0
code_repr_str: db "<code object>", 0
code_type_name: db "code", 0

; code type object
align 8
global code_type
code_type:
    dq 1                ; ob_refcnt
    dq type_type        ; ob_type
    dq code_type_name   ; tp_name
    dq PyCodeObject_size ; tp_basicsize
    dq code_dealloc     ; tp_dealloc
    dq code_repr        ; tp_repr
    dq code_repr        ; tp_str
    dq 0                ; tp_hash
    dq 0                ; tp_call
    dq code_getattr     ; tp_getattr
    dq 0                ; tp_setattr
    dq 0                ; tp_richcompare
    dq 0                ; tp_iter
    dq 0                ; tp_iternext
    dq 0                ; tp_init
    dq 0                ; tp_new
    dq 0                ; tp_as_number
    dq 0                ; tp_as_sequence
    dq 0                ; tp_as_mapping
    dq 0                ; tp_base
    dq 0                ; tp_dict
    dq 0                ; tp_mro
    dq TYPE_FLAG_HAVE_GC ; tp_flags
    dq 0                ; tp_bases
    dq code_traverse    ; tp_traverse
    dq code_clear       ; tp_clear
    dq 0 ; tp_dictoffset


section .bss
_co_positions_cache: resq 1

section .text
DEF_FUNC_LOCAL _get_co_positions_builtin
    mov rax, [rel _co_positions_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel code_method_co_positions]
    lea rsi, [rel co_attr_positions]
    extern builtin_func_new
    call builtin_func_new
    mov [rel _co_positions_cache], rax
.ret:
    leave
    ret
END_FUNC _get_co_positions_builtin

section .rodata
co_attr_positions: db "co_positions", 0
