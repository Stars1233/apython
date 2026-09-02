; gcmod.asm - the `gc` module
;
; There was no gc module at all, and gc_collect_gen had three callers, every
; one of them inside gc.asm.  Nothing else in the tree even declared it, so
; there was no way to ask for a collection -- and gc.collect() is what a test
; for a reference cycle is written around.
;
; What is here is the part of CPython's gc that answers about this collector:
; collect, enable/disable/isenabled, the counts and the thresholds, and the
; two lists.  get_objects and the debug flags are not: this collector keeps no
; list it could hand back, and has no debug output to switch on.

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
extern gc_enabled
extern gc_gen0_count
extern gc_gen1_count
extern gc_gen2_count
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

DEF_FUNC gc_mod_get_count
    test rsi, rsi
    jnz .gmgc_args
    mov rdi, [rel gc_gen0_count]
    mov rsi, [rel gc_gen1_count]
    mov rdx, [rel gc_gen2_count]
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

    ; gc.garbage and gc.callbacks: both always empty here.  Nothing is ever
    ; put in garbage, because nothing is declared uncollectable -- a __del__
    ; no longer keeps a cycle alive in CPython either -- and there is no
    ; callback machinery to run.  They exist because code reads them.
    mov rdi, [rbp - GMC_DICT]
    lea rsi, [rel gm_n_garbage]
    call gm_add_empty_list
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
    pop r12
    pop rbx
    leave
    ret
END_FUNC gm_add_empty_list

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
gm_n_gc:            db "gc", 0
