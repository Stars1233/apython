; gcmod.asm - the `gc` module
;
; There was no gc module at all, and gc_collect_gen had three callers, every
; one of them inside gc.asm.  Nothing else in the tree even declared it, so
; there was no way to ask for a collection -- and gc.collect() is what a test
; for a reference cycle is written around.
;
; What is here is the part of CPython's gc that answers about this collector:
; collect, enable/disable/isenabled, the counts and the thresholds, the two
; lists, get_objects, is_tracked, get_referents and the debug flags.
;
; get_objects is a walk of the three generation lists, which are circular and
; doubly linked with static sentinels -- a header here once claimed the
; collector kept no list it could hand back, which was never true.  What is
; still absent is get_referrers, which needs a reverse edge nothing records,
; and freeze/unfreeze and get_stats, which are about machinery this collector
; does not have.

%include "macros.inc"
%include "object.inc"

extern dict_new
extern dict_set
extern obj_decref
extern str_from_cstr_heap
extern builtin_func_new
extern list_new
extern tuple_new
extern int_from_i64
extern obj_as_index
extern none_singleton
extern bool_true
extern bool_false
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type

extern gc_collect_gen
extern gc_walk_generation
extern gc_enabled
extern gc_debug
extern gc_garbage_list
extern list_append
extern obj_incref
extern obj_dealloc
extern gc_gen0_count
extern gc_gen1_collections
extern gc_gen2_collections
extern gc_gen0_threshold
extern gc_gen1_threshold
extern gc_gen2_threshold

section .text

;; ============================================================================
;; gc_mod_collect(args, nargs) -> the number of unreachable objects found
;;   collect(generation=2)
;; ============================================================================
GMC_GEN   equ 8
GMCF_FRAME equ 16           ; + 0 pushes = 16

DEF_FUNC gc_mod_collect, GMCF_FRAME
    cmp rsi, 1
    ja .gmc_args
    mov qword [rbp - GMC_GEN], 2    ; the whole of it, as CPython's default is
    test rsi, rsi
    jz .gmc_have_gen
    mov rdi, [rdi]
    V_UNPACK rdi, rdx
    call obj_as_index
    cmp rax, 0
    jl .gmc_range
    cmp rax, 2
    jg .gmc_range
    mov [rbp - GMC_GEN], rax
.gmc_have_gen:
    mov edi, [rbp - GMC_GEN]
    call gc_collect_gen
    mov rdi, rax
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret

.gmc_args:
    RAISE exc_TypeError_type, "collect() takes at most 1 argument"
.gmc_range:
    RAISE exc_ValueError_type, "invalid generation"
END_FUNC gc_mod_collect

;; ============================================================================
;; enable / disable / isenabled
;; ============================================================================
DEF_FUNC gc_mod_enable
    mov qword [rel gc_enabled], 1
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC gc_mod_enable

DEF_FUNC gc_mod_disable
    mov qword [rel gc_enabled], 0
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC gc_mod_disable

DEF_FUNC gc_mod_isenabled
    cmp qword [rel gc_enabled], 0
    je .gmi_false
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret
.gmi_false:
    RET_FALSE
    leave
    V_PACK rax, rdx
    ret
END_FUNC gc_mod_isenabled

;; ============================================================================
;; get_count() / get_threshold() -> a 3-tuple each
;;
;; gm_three(rdi = the first, rsi = the second, rdx = the third) -> a tuple
;; ============================================================================
GM3_A     equ 8
GM3_B     equ 16
GM3_C     equ 24
GM3_TUP   equ 32
GM3_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC_LOCAL gm_three, GM3_FRAME
    mov [rbp - GM3_A], rdi
    mov [rbp - GM3_B], rsi
    mov [rbp - GM3_C], rdx
    mov edi, 3
    call tuple_new
    test rax, rax
    jz .gm3_out
    mov [rbp - GM3_TUP], rax

    mov rdi, [rbp - GM3_A]
    call int_from_i64
    V_PACK rax, rdx
    mov rcx, [rbp - GM3_TUP]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx], rax

    mov rdi, [rbp - GM3_B]
    call int_from_i64
    V_PACK rax, rdx
    mov rcx, [rbp - GM3_TUP]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx + 8], rax

    mov rdi, [rbp - GM3_C]
    call int_from_i64
    V_PACK rax, rdx
    mov rcx, [rbp - GM3_TUP]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx + 16], rax

    mov rax, [rbp - GM3_TUP]
.gm3_out:
    leave
    ret
END_FUNC gm_three

; CPython's get_count() is (allocations since the last gen0 pass, gen0
; collections since the last gen1 pass, gen1 collections since the last gen2
; pass) -- only the first is an object count.  The last two were reported
; from a pair of counters nothing ever incremented, so they were always 0;
; the collector's real ones are gc_gen1_collections and gc_gen2_collections,
; which are also what its own thresholds are compared against.
DEF_FUNC gc_mod_get_count
    test rsi, rsi
    jnz .gmgc_args
    mov rdi, [rel gc_gen0_count]
    mov rsi, [rel gc_gen1_collections]
    mov rdx, [rel gc_gen2_collections]
    call gm_three
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gmgc_args:
    RAISE exc_TypeError_type, "get_count() takes no arguments"
END_FUNC gc_mod_get_count

DEF_FUNC gc_mod_get_threshold
    test rsi, rsi
    jnz .gmgt_args
    mov rdi, [rel gc_gen0_threshold]
    mov rsi, [rel gc_gen1_threshold]
    mov rdx, [rel gc_gen2_threshold]
    call gm_three
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gmgt_args:
    RAISE exc_TypeError_type, "get_threshold() takes no arguments"
END_FUNC gc_mod_get_threshold

;; ============================================================================
;; set_threshold(gen0[, gen1[, gen2]])
;; ============================================================================
GST_ARGS  equ 8
GST_NARGS equ 16
GST_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC gc_mod_set_threshold, GST_FRAME
    test rsi, rsi
    jz .gst_args
    cmp rsi, 3
    ja .gst_args
    mov [rbp - GST_ARGS], rdi
    mov [rbp - GST_NARGS], rsi

    mov rdi, [rdi]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov [rel gc_gen0_threshold], rax

    cmp qword [rbp - GST_NARGS], 2
    jl .gst_done
    mov rdi, [rbp - GST_ARGS]
    mov rdi, [rdi + 8]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov [rel gc_gen1_threshold], rax

    cmp qword [rbp - GST_NARGS], 3
    jl .gst_done
    mov rdi, [rbp - GST_ARGS]
    mov rdi, [rdi + 16]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov [rel gc_gen2_threshold], rax

.gst_done:
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gst_args:
    RAISE exc_TypeError_type, "set_threshold() takes 1 to 3 arguments"
END_FUNC gc_mod_set_threshold

;; ============================================================================
;; get_objects([generation]) -> a list of every tracked object
;;
;; The three generations are circular doubly-linked lists with static
;; sentinels, so this is a walk; gc_walk_generation does it with the
;; collector's own reentrancy flag raised.  With no argument, or with None,
;; every generation, oldest first -- which is the order CPython's own
;; get_objects builds its list in.
;; ============================================================================
GGO_LIST  equ 8
GGO_GEN   equ 16
GGO_FRAME equ 32            ; + 1 push = 40

DEF_FUNC gc_mod_get_objects, GGO_FRAME
    push rbx
    cmp rsi, 1
    ja .ggo_args
    mov qword [rbp - GGO_GEN], -1       ; all of them
    test rsi, rsi
    jz .ggo_have_gen
    mov rdi, [rdi]
    LOAD_NONE rax
    cmp rdi, rax
    je .ggo_have_gen
    V_UNPACK rdi, rdx
    call obj_as_index
    cmp rax, 2
    jg .ggo_range
    mov [rbp - GGO_GEN], rax    ; negative means all of them, as CPython has it
.ggo_have_gen:
    xor edi, edi
    call list_new
    mov [rbp - GGO_LIST], rax

    mov rbx, [rbp - GGO_GEN]
    cmp rbx, 0
    jge .ggo_one
    xor ebx, ebx
.ggo_all_loop:
    cmp rbx, 3
    jge .ggo_out
    mov rdi, rbx
    mov rsi, [rbp - GGO_LIST]
    call gc_walk_generation
    inc rbx
    jmp .ggo_all_loop

.ggo_one:
    mov rdi, rbx
    mov rsi, [rbp - GGO_LIST]
    call gc_walk_generation

.ggo_out:
    mov rax, [rbp - GGO_LIST]
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.ggo_args:
    RAISE exc_TypeError_type, "get_objects() takes at most 1 argument"
.ggo_range:
    RAISE exc_ValueError_type, "generation parameter must be less than the number of available generations (3)"
END_FUNC gc_mod_get_objects

;; ============================================================================
;; is_tracked(obj)
;;
;; An immediate -- an int, a float, None -- is not an object at all here, so
;; it is never tracked.  A heap object is tracked when its type asked to be
;; and it is currently in a generation list.
;; ============================================================================
DEF_FUNC gc_mod_is_tracked
    cmp rsi, 1
    jne .git_args
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .git_false                       ; an immediate is not an object at all
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .git_false
    cmp qword [rdi - GC_HEAD_SIZE + PyGC_Head.gc_next], 0
    je .git_false
    RET_TRUE
    leave
    V_PACK rax, rdx
    ret
.git_false:
    RET_FALSE
    leave
    V_PACK rax, rdx
    ret
.git_args:
    RAISE exc_TypeError_type, "is_tracked() takes exactly one argument"
END_FUNC gc_mod_is_tracked

;; ============================================================================
;; get_referents(*objs) -> everything the arguments refer to
;;
;; tp_traverse is what the collector itself walks with, so this is the same
;; walk with a different visit callback.  The callback reaches its
;; destination through a file-local global, the way gc_visit_reachable
;; reaches the sentinel it rescues into: the visit ABI is one argument.
;; ============================================================================
section .data
gm_referents_out: dq 0
section .text

DEF_FUNC_LOCAL gm_visit_append
    push rbx
    mov rbx, rdi
    mov rdi, [rel gm_referents_out]
    mov rsi, rbx
    call list_append
    pop rbx
    leave
    ret
END_FUNC gm_visit_append

GGR_LIST  equ 8
GGR_ARGS  equ 16
GGR_NARGS equ 24
GGR_SAVED equ 32
GGR_FRAME equ 48            ; + 2 pushes = 64

DEF_FUNC gc_mod_get_referents, GGR_FRAME
    push rbx
    push r12
    mov [rbp - GGR_ARGS], rdi
    mov [rbp - GGR_NARGS], rsi
    xor edi, edi
    call list_new
    mov [rbp - GGR_LIST], rax
    mov rax, [rel gm_referents_out]
    mov [rbp - GGR_SAVED], rax
    mov rax, [rbp - GGR_LIST]
    mov [rel gm_referents_out], rax

    xor ebx, ebx
.ggr_loop:
    cmp rbx, [rbp - GGR_NARGS]
    jge .ggr_done
    mov rax, [rbp - GGR_ARGS]
    mov r12, [rax + rbx*8]
    V_TEST_PTR r12, rax
    ja .ggr_next
    mov rax, [r12 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_traverse]
    test rax, rax
    jz .ggr_next
    mov rdi, r12
    lea r14, [rel gm_visit_append]
    call rax
.ggr_next:
    inc rbx
    jmp .ggr_loop
.ggr_done:
    mov rax, [rbp - GGR_SAVED]
    mov [rel gm_referents_out], rax
    mov rax, [rbp - GGR_LIST]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC gc_mod_get_referents

;; ============================================================================
;; set_debug(flags) / get_debug()
;; ============================================================================
DEF_FUNC gc_mod_set_debug
    cmp rsi, 1
    jne .gsd_args
    mov rdi, [rdi]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov [rel gc_debug], rax
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.gsd_args:
    RAISE exc_TypeError_type, "set_debug() takes exactly one argument"
END_FUNC gc_mod_set_debug

DEF_FUNC gc_mod_get_debug
    test rsi, rsi
    jnz .ggd_args
    mov rdi, [rel gc_debug]
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret
.ggd_args:
    RAISE exc_TypeError_type, "get_debug() takes no arguments"
END_FUNC gc_mod_get_debug

;; ============================================================================
;; gc_module_create() -> the module dict
;; ============================================================================
GMC_DICT  equ 8
GMC_NAME  equ 16
GMC_MOD   equ 24
GMC_FRAME equ 32            ; + 0 pushes = 32

;; gm_add(rdi = dict, rsi = name cstr, rdx = function)
GMA_FUNC  equ 8
GMA_KEY   equ 16
GMA_FRAME equ 24            ; + 3 pushes = 48

DEF_FUNC_LOCAL gm_add, GMA_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov rdi, r13
    mov rsi, r12
    call builtin_func_new
    mov [rbp - GMA_FUNC], rax
    mov rdi, r12
    call str_from_cstr_heap
    mov [rbp - GMA_KEY], rax
    mov rdi, rbx
    mov rsi, rax
    mov rdx, [rbp - GMA_FUNC]
    call dict_set
    mov rdi, [rbp - GMA_KEY]
    call obj_decref
    mov rdi, [rbp - GMA_FUNC]
    call obj_decref
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC gm_add

global gc_module_create
DEF_FUNC gc_module_create, GMC_FRAME
    call dict_new
    mov [rbp - GMC_DICT], rax

    mov rdi, rax
    lea rsi, [rel gm_n_collect]
    lea rdx, [rel gc_mod_collect]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_enable]
    lea rdx, [rel gc_mod_enable]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_disable]
    lea rdx, [rel gc_mod_disable]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_isenabled]
    lea rdx, [rel gc_mod_isenabled]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_get_count]
    lea rdx, [rel gc_mod_get_count]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_get_threshold]
    lea rdx, [rel gc_mod_get_threshold]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_set_threshold]
    lea rdx, [rel gc_mod_set_threshold]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_get_objects]
    lea rdx, [rel gc_mod_get_objects]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_is_tracked]
    lea rdx, [rel gc_mod_is_tracked]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_get_referents]
    lea rdx, [rel gc_mod_get_referents]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_set_debug]
    lea rdx, [rel gc_mod_set_debug]
    call gm_add
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_get_debug]
    lea rdx, [rel gc_mod_get_debug]
    call gm_add

    ; The five DEBUG_* flags, with CPython's values.
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_d_stats]
    mov edx, GC_DEBUG_STATS
    call gm_add_int
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_d_coll]
    mov edx, GC_DEBUG_COLLECTABLE
    call gm_add_int
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_d_uncoll]
    mov edx, GC_DEBUG_UNCOLLECTABLE
    call gm_add_int
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_d_saveall]
    mov edx, GC_DEBUG_SAVEALL
    call gm_add_int
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_d_leak]
    mov edx, GC_DEBUG_LEAK
    call gm_add_int

    ; gc.garbage and gc.callbacks: both always empty here.  Nothing is ever
    ; put in garbage, because nothing is declared uncollectable -- a __del__
    ; no longer keeps a cycle alive in CPython either -- and there is no
    ; callback machinery to run.  They exist because code reads them.
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_garbage]
    call gm_add_empty_list
    ; The collector needs this one by name: DEBUG_SAVEALL puts the unreachable
    ; set in it instead of clearing it.  The module dict owns the reference.
    mov [rel gc_garbage_list], rax
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_callbacks]
    call gm_add_empty_list

    ; And the module object around it, as every builtin module does.
    lea rdi, [rel gm_n_gc]
    call str_from_cstr_heap
    mov [rbp - GMC_NAME], rax
    mov rdi, rax
    mov rsi, [rbp - GMC_DICT]
    extern module_new
    call module_new
    mov [rbp - GMC_MOD], rax
    mov rdi, [rbp - GMC_NAME]
    call obj_decref             ; module_new took its own
    mov rdi, [rbp - GMC_DICT]
    call obj_decref
    mov rax, [rbp - GMC_MOD]
    leave
    ret
END_FUNC gc_module_create

;; gm_add_empty_list(rdi = dict, rsi = name cstr)
GME_LIST  equ 8
GME_KEY   equ 16
GME_FRAME equ 24            ; + 2 pushes = 40

DEF_FUNC_LOCAL gm_add_empty_list, GME_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    xor edi, edi
    call list_new
    mov [rbp - GME_LIST], rax
    mov rdi, r12
    call str_from_cstr_heap
    mov [rbp - GME_KEY], rax
    mov rdi, rbx
    mov rsi, rax
    mov rdx, [rbp - GME_LIST]
    call dict_set
    mov rdi, [rbp - GME_KEY]
    call obj_decref
    mov rdi, [rbp - GME_LIST]
    call obj_decref
    mov rax, [rbp - GME_LIST]           ; borrowed; the module dict owns it
    pop r12
    pop rbx
    leave
    ret
END_FUNC gm_add_empty_list

;; gm_add_int(rdi = dict, rsi = name cstr, rdx = value)
GMI_KEY   equ 8
GMI_VAL   equ 16
GMI_FRAME equ 32            ; + 2 pushes = 48

DEF_FUNC_LOCAL gm_add_int, GMI_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    mov rdi, rdx
    call int_from_i64
    V_PACK rax, rdx
    mov [rbp - GMI_VAL], rax
    mov rdi, r12
    call str_from_cstr_heap
    mov [rbp - GMI_KEY], rax
    mov rdi, rbx
    mov rsi, rax
    mov rdx, [rbp - GMI_VAL]
    call dict_set
    mov rdi, [rbp - GMI_KEY]
    call obj_decref
    mov rdi, [rbp - GMI_VAL]
    DECREF_V rdi, rax
    pop r12
    pop rbx
    leave
    ret
END_FUNC gm_add_int

section .rodata
gm_n_collect:       db "collect", 0
gm_n_enable:        db "enable", 0
gm_n_disable:       db "disable", 0
gm_n_isenabled:     db "isenabled", 0
gm_n_get_count:     db "get_count", 0
gm_n_get_threshold: db "get_threshold", 0
gm_n_set_threshold: db "set_threshold", 0
gm_n_garbage:       db "garbage", 0
gm_n_callbacks:     db "callbacks", 0
gm_n_get_objects:   db "get_objects", 0
gm_n_is_tracked:    db "is_tracked", 0
gm_n_get_referents: db "get_referents", 0
gm_n_set_debug:     db "set_debug", 0
gm_n_get_debug:     db "get_debug", 0
gm_n_d_stats:       db "DEBUG_STATS", 0
gm_n_d_coll:        db "DEBUG_COLLECTABLE", 0
gm_n_d_uncoll:      db "DEBUG_UNCOLLECTABLE", 0
gm_n_d_saveall:     db "DEBUG_SAVEALL", 0
gm_n_d_leak:        db "DEBUG_LEAK", 0
gm_n_gc:            db "gc", 0
