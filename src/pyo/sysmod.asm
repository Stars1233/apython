; sysmod.asm - sys module initialization
; Creates the sys module with standard attributes

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern ap_strlen
extern obj_decref
extern obj_incref
extern obj_dealloc
extern ap_strcmp
extern str_from_cstr_heap
extern str_new
extern str_type
extern int_type
extern bool_type
extern int_from_i64
extern int_to_i64
extern none_singleton
extern bool_true
extern bool_false
extern dict_new
extern dict_set
extern dict_get
extern list_new
extern list_append
extern tuple_new
extern type_type
extern module_new
extern fileobj_new
extern builtin_func_new
extern fatal_error
extern raise_exception

;; SYS_ADD_FUNC impl, name -- the module dict is in r15 here, not r12, so
;; MODULE_ADD_FUNC does not fit; this is the same four steps.
%macro SYS_ADD_FUNC 2
    lea rdi, [rel %1]
    lea rsi, [rel %2]
    call builtin_func_new
    push rax
    lea rdi, [rel %2]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref
%endmacro

;; SYS_ADD_FUNC_ALIAS impl, name, alias -- the same object under two names.
;; sys.excepthook and sys.__excepthook__ ARE the same object in CPython, and
;; code checks it: `sys.excepthook is sys.__excepthook__` is how a program
;; asks whether anything has replaced the hook.  Two SYS_ADD_FUNC calls made
;; two objects and that test was always False.
%macro SYS_ADD_FUNC_ALIAS 3
    lea rdi, [rel %1]
    lea rsi, [rel %2]
    call builtin_func_new
    push rax
    lea rdi, [rel %2]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    lea rdi, [rel %3]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref
%endmacro


;; ============================================================================
;; sm_sort_names(rdi = a tuple of str) -> nothing; sorts it in place, by bytes
;;
;; An insertion sort over thirty-odd names, which is what it takes to keep
;; sys.builtin_module_names in CPython's order once two tables feed it.
;; ============================================================================
SSN_TUP   equ 8
SSN_N     equ 16
SSN_I     equ 24
SSN_J     equ 32
SSN_FRAME equ 48            ; + 1 push = 56... one word more to land right
DEF_FUNC_LOCAL sm_sort_names, 56        ; + 1 push = 64, 16-aligned
    push rbx
    mov rbx, rdi
    mov [rbp - SSN_TUP], rdi
    mov rax, [rdi + PyTupleObject.ob_size]
    mov [rbp - SSN_N], rax
    mov qword [rbp - SSN_I], 1
.ssn_outer:
    mov rax, [rbp - SSN_I]
    cmp rax, [rbp - SSN_N]
    jge .ssn_done
    mov [rbp - SSN_J], rax
.ssn_inner:
    cmp qword [rbp - SSN_J], 0
    jle .ssn_next
    mov rcx, [rbx + PyTupleObject.ob_item]
    mov rdx, [rbp - SSN_J]
    mov rdi, [rcx + rdx*8 - 8]
    mov rsi, [rcx + rdx*8]
    add rdi, PyStrObject.data
    add rsi, PyStrObject.data
    call ap_strcmp
    test eax, eax
    jle .ssn_next
    mov rcx, [rbx + PyTupleObject.ob_item]
    mov rdx, [rbp - SSN_J]
    mov rdi, [rcx + rdx*8 - 8]
    mov rsi, [rcx + rdx*8]
    mov [rcx + rdx*8 - 8], rsi
    mov [rcx + rdx*8], rdi
    dec qword [rbp - SSN_J]
    jmp .ssn_inner
.ssn_next:
    inc qword [rbp - SSN_I]
    jmp .ssn_outer
.ssn_done:
    pop rbx
    leave
    ret
END_FUNC sm_sort_names

;; ============================================================================
;; sys_module_init(int argc, char **argv) -> void
;; Initialize the sys module and register it in sys.modules
;; ============================================================================
SMI_ARGC  equ 8
SMI_ARGV  equ 16
SMI_TMP   equ 24            ; whichever object is being installed just now
;; ============================================================================
;; sm_add_str(rdi = name cstr, rsi = value cstr, rdx = the module dict)
;;
;; One string attribute, interned from two C strings.  Open-coded it is
;; fifteen lines, and this file does it fifty-odd times; the prefix family is
;; where that started to hide which names were actually being set.
;; ============================================================================
SAS_DICT  equ 8
SAS_KEY   equ 16
SAS_FRAME equ 24            ; + 1 push = 32, 16-aligned
DEF_FUNC_LOCAL sm_add_str, SAS_FRAME
    push rbx
    mov [rbp - SAS_DICT], rdx
    mov rbx, rsi
    call str_from_cstr_heap             ; the key
    mov [rbp - SAS_KEY], rax
    mov rdi, rbx
    call str_from_cstr_heap             ; the value
    mov rbx, rax
    mov rdi, [rbp - SAS_DICT]
    mov rsi, [rbp - SAS_KEY]
    mov rdx, rax
    call dict_set                       ; takes its own reference to both
    mov rdi, rbx
    call obj_decref
    mov rdi, [rbp - SAS_KEY]
    call obj_decref
    pop rbx
    leave
    ret
END_FUNC sm_add_str

DEF_FUNC sys_module_init, 40
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - SMI_ARGC], rdi          ; argc
    mov [rbp - SMI_ARGV], rsi         ; argv

    ; Initialize int_max_str_digits to 4300 (CPython default)
    mov qword [rel sys_int_max_str_digits], 4300

    ; Create sys.modules dict
    call dict_new
    mov [rel sys_modules_dict], rax
    mov r14, rax                ; r14 = modules dict

    ; Create sys module dict
    call dict_new
    mov r15, rax                ; r15 = sys dict

    ; --- sys.modules ---
    lea rdi, [rel sm_modules]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, r14
    call dict_set
    pop rdi
    call obj_decref

    ; --- sys.path ---
    xor edi, edi                ; empty list
    call list_new
    mov [rel sys_path_list], rax
    mov r12, rax                ; r12 = sys.path list

    ; Add script directory to sys.path (computed from argv[1] later)
    ; For now, add empty string (current dir) as fallback
    lea rdi, [rel sm_empty]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    call list_append
    pop rdi
    call obj_decref

    ; Store sys.path
    lea rdi, [rel sm_path]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, r12
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, r12            ; DECREF sys.path (dict_set INCREF'd)
    call obj_decref

    ; --- sys.argv ---
    xor edi, edi
    call list_new
    mov r13, rax                ; r13 = sys.argv list

    ; Populate sys.argv from process argv, starting at the *script*.  CPython's
    ; sys.argv[0] is the script, not the interpreter, so a script reading
    ; sys.argv[1] for its first argument was getting its own path.  main has
    ; already shifted argv past a -t if there was one, so index 1 is the script
    ; either way.
    mov rcx, [rbp - SMI_ARGC]         ; argc
    mov rdx, [rbp - SMI_ARGV]        ; argv
    mov ebx, 1                 ; i = 1: skip the interpreter
.argv_loop:
    cmp rbx, rcx
    jge .argv_done
    push rcx
    push rdx
    push rbx

    mov rdi, [rdx + rbx * 8]  ; argv[i]
    call str_from_cstr_heap
    push rax
    mov rdi, r13
    mov rsi, rax
    call list_append
    pop rdi
    call obj_decref

    pop rbx
    pop rdx
    pop rcx
    inc rbx
    jmp .argv_loop
.argv_done:

    lea rdi, [rel sm_argv]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, r13
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, r13            ; DECREF sys.argv (dict_set INCREF'd)
    call obj_decref

    ; --- sys.maxsize ---
    mov rdi, 0x7fffffffffffffff
    call int_from_i64
    push rdx                   ; save value tag
    push rax                   ; save value payload
    lea rdi, [rel sm_maxsize]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax               ; key = "maxsize" (str)
    mov rdx, [rsp + 8]        ; value payload
    mov ecx, [rsp + 16]       ; value tag (from int_from_i64)
    V_PACK rdx, rcx           ; dict_set takes Values
    call dict_set
    pop rdi
    call obj_decref            ; DECREF key string
    pop rdi                    ; value payload (SmallInt, no DECREF)
    pop rcx                    ; value tag (discard)

    ; --- sys.platform ---
    lea rdi, [rel sm_linux]
    call str_from_cstr_heap
    push rax
    lea rdi, [rel sm_platform]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.version ---
    lea rdi, [rel sm_version_val]
    call str_from_cstr_heap
    push rax
    lea rdi, [rel sm_version]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.version_info (a struct sequence) ---
    ; It was a bare tuple, so type(sys.version_info).__name__ was 'tuple' and
    ; sys.version_info.major was an AttributeError.  platform.py and several
    ; stdlib modules read the names.
    extern version_info_type
    extern float_info_type
    extern int_info_type
    extern hash_info_type
    extern float_info_v0
    extern float_info_v3
    extern float_info_v8
    extern hash_info_v5
    extern structseq_new
    extern structseq_set
    extern structseq_init_type
    lea rdi, [rel version_info_type]
    call structseq_init_type
    ; sys.UnraisableHookArgs, which sys.unraisablehook is handed.  Only the
    ; type needs registering; the instances are built one at a time by the
    ; unraisable printer.
    extern unraisable_args_type
    lea rdi, [rel unraisable_args_type]
    call structseq_init_type
    lea rdi, [rel version_info_type]
    call structseq_new
    mov rbx, rax                ; rbx = the version_info object

    ; (3, 12, 0, 'final', 0)
    mov rdi, rbx
    xor esi, esi
    mov rdx, 3
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 1
    mov rdx, 12
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 2
    xor edx, edx
    V_PACK_I64 rdx, rcx
    call structseq_set
    lea rdi, [rel sm_final]
    call str_from_cstr_heap
    mov rdx, rax                ; structseq_set takes over the reference
    mov rdi, rbx
    mov esi, 3
    call structseq_set
    mov rdi, rbx
    mov esi, 4
    xor edx, edx
    V_PACK_I64 rdx, rcx
    call structseq_set

    lea rdi, [rel sm_version_info]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, rbx
    call obj_decref

    ; --- sys.executable ---
    lea rdi, [rel sm_empty]
    call str_from_cstr_heap
    push rax
    lea rdi, [rel sm_executable]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.prefix / exec_prefix, and the base_ pair beside them ---
    ; base_prefix and base_exec_prefix are what a virtualenv leaves pointing
    ; at the installation it was made from; with no virtualenv all four name
    ; the same place.  CPython's getopt, gettext and optparse read the base_
    ; pair unconditionally, so their absence was an AttributeError at import
    ; rather than anything the program could ask about.
    lea rdi, [rel sm_prefix]
    lea rsi, [rel sm_empty]
    mov rdx, r15
    call sm_add_str
    lea rdi, [rel sm_exec_prefix]
    lea rsi, [rel sm_empty]
    mov rdx, r15
    call sm_add_str
    lea rdi, [rel sm_base_prefix]
    lea rsi, [rel sm_empty]
    mov rdx, r15
    call sm_add_str
    lea rdi, [rel sm_base_exec_prefix]
    lea rsi, [rel sm_empty]
    mov rdx, r15
    call sm_add_str

    ; --- sys.platlibdir ---
    ; The directory name a platform puts its libraries in: "lib" everywhere
    ; but the 64-bit RPM layouts, which use "lib64".  sysconfig reads it
    ; unconditionally while building _CONFIG_VARS, so without it pydoc, cgitb
    ; and everything downstream of them stopped at an AttributeError.
    lea rdi, [rel sm_platlibdir]
    lea rsi, [rel sm_lib]
    mov rdx, r15
    call sm_add_str

    ; --- sys.abiflags ---
    ; The suffix a CPython build puts on its library names -- "d" for a debug
    ; build, empty for an ordinary one.  sysconfig interpolates it into the
    ; name of the _sysconfigdata module it looks for, so its absence stopped
    ; the same imports platlibdir did, one line further on.
    lea rdi, [rel sm_abiflags]
    lea rsi, [rel sm_empty]
    mov rdx, r15
    call sm_add_str

    ; --- sys.copyright ---
    ; site.py reads it to build the `copyright` banner, unconditionally, so
    ; without it `import site` was an AttributeError.  Ours carries both
    ; notices: the interpreter is MIT, and the standard library it runs is
    ; the PSF's.
    lea rdi, [rel sm_copyright]
    lea rsi, [rel sm_copyright_text]
    mov rdx, r15
    call sm_add_str

    ; --- sys.stdout (fd=1) ---
    mov rdi, 1
    lea rsi, [rel sm_stdout_name]
    lea rdx, [rel sm_mode_w]
    call fileobj_new
    mov [rel sys_stdout_obj], rax
    push rax
    lea rdi, [rel sm_stdout]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.stderr (fd=2) ---
    mov rdi, 2
    lea rsi, [rel sm_stderr_name]
    lea rdx, [rel sm_mode_w]
    call fileobj_new
    push rax
    lea rdi, [rel sm_stderr]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.stdin (fd=0) ---
    xor edi, edi
    lea rsi, [rel sm_stdin_name]
    lea rdx, [rel sm_mode_r]
    call fileobj_new
    push rax
    lea rdi, [rel sm_stdin]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.exit function ---
    lea rdi, [rel sys_exit_func]
    lea rsi, [rel sm_exit]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_exit]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.getrecursionlimit / sys.setrecursionlimit ---
    lea rdi, [rel sys_getrecursionlimit_func]
    lea rsi, [rel sm_getrecursionlimit]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_getrecursionlimit]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    lea rdi, [rel sys_setrecursionlimit_func]
    lea rsi, [rel sm_setrecursionlimit]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_setrecursionlimit]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.implementation ---
    ; types.py takes SimpleNamespace from `type(sys.implementation)`, so this
    ; has to be a namespace rather than a tuple or a dict.  cache_tag is what
    ; a loader uses to find .pyc files, and apython reads CPython 3.12 ones.
    ; r14/r15 belong to the module dicts here, so the namespace lives in a
    ; frame slot.
    extern namespace_new
    extern namespace_set
    call namespace_new
    mov [rbp - SMI_TMP], rax

    lea rdi, [rel sm_apython_name]
    call str_from_cstr_heap
    push rax
    mov rdi, [rbp - SMI_TMP]
    lea rsi, [rel sm_name]
    mov rdx, rax
    call namespace_set
    pop rdi
    call obj_decref

    lea rdi, [rel sm_cache_tag_val]
    call str_from_cstr_heap
    push rax
    mov rdi, [rbp - SMI_TMP]
    lea rsi, [rel sm_cache_tag]
    mov rdx, rax
    call namespace_set
    pop rdi
    call obj_decref

    ; version comes back out of the sys dict rather than a stale register
    lea rdi, [rel sm_version_info]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx
    pop rdi
    push rax
    push rdx
    call obj_decref
    pop rdx
    pop rax
    test edx, edx
    jz .no_impl_version
    mov rdi, [rbp - SMI_TMP]
    lea rsi, [rel sm_version]
    mov rdx, rax
    call namespace_set
.no_impl_version:

    lea rdi, [rel sm_implementation]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rbp - SMI_TMP]
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, [rbp - SMI_TMP]
    call obj_decref

    ; --- sys.warnoptions (empty; nothing parses -W yet) ---
    xor edi, edi
    call list_new
    mov [rbp - SMI_TMP], rax
    lea rdi, [rel sm_warnoptions]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rbp - SMI_TMP]
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, [rbp - SMI_TMP]
    call obj_decref

    ; --- sys.builtin_module_names ---
    ; The modules that are compiled in rather than found on sys.path.  os.py
    ; reads it to decide which platform module to import, and several others
    ; use it to tell a built-in apart from a shadowing file.
    ; Built from builtin_module_table, the same rows import_init registers
    ; from.  It used to be a hand-written array here, and it had never grown
    ; `asyncio` or `errno` -- so a module sitting in sys.modules was absent
    ; from the list os.py gates its platform import on.
    ;
    ; The list also carries the modules this tree supplies from lib/ in place
    ; of a CPython builtin.  From a program's point of view they ARE built in
    ; -- the interpreter finds them with no path entry, and nothing on
    ; sys.path shadows one -- and CPython's own importlib._bootstrap tests
    ; this list by name before it will load `_thread` or `_warnings`, which
    ; is what twenty-one modules of its Lib/ stop at.
    extern builtin_module_table
    extern builtin_module_count
    mov rdi, [rel builtin_module_count]
    add rdi, SM_SUPPLIED_COUNT
    call tuple_new
    mov [rbp - SMI_TMP], rax
    xor r13d, r13d
.sm_bmn_loop:
    cmp r13, [rel builtin_module_count]
    jge .sm_bmn_supplied
    lea rax, [rel builtin_module_table]
    mov rcx, r13
    shl rcx, 4                              ; BuiltinModule_size
    mov rdi, [rax + rcx + BuiltinModule.name]
    call str_from_cstr_heap
    mov rcx, [rbp - SMI_TMP]
    mov rcx, [rcx + PyTupleObject.ob_item]  ; reload: the allocation moves it
    mov [rcx + r13*8], rax
    inc r13
    jmp .sm_bmn_loop
.sm_bmn_supplied:
    xor r12d, r12d
.sm_bmn_sup_loop:
    cmp r12, SM_SUPPLIED_COUNT
    jge .sm_bmn_done
    lea rax, [rel sm_supplied_names]
    mov rdi, [rax + r12*8]
    call str_from_cstr_heap
    mov rcx, [rbp - SMI_TMP]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx + r13*8], rax
    inc r13
    inc r12
    jmp .sm_bmn_sup_loop
.sm_bmn_done:
    ; CPython's is sorted, and a program may rely on that -- so the two
    ; tables are concatenated and then sorted, rather than each being kept in
    ; order against the other.
    mov rdi, [rbp - SMI_TMP]
    call sm_sort_names

    lea rdi, [rel sm_builtin_module_names]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rbp - SMI_TMP]
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, [rbp - SMI_TMP]
    call obj_decref

    ; --- sys.byteorder ---
    lea rdi, [rel sm_little]
    call str_from_cstr_heap
    push rax
    lea rdi, [rel sm_byteorder]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.flags ---
    extern flags_type
    lea rdi, [rel flags_type]
    call structseq_init_type
    lea rdi, [rel flags_type]
    call structseq_new
    mov rbx, rax
    mov rdi, rbx
    mov esi, 0
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 1
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 2
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 3
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 4
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 5
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 6
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 7
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 8
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 9
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 10
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 11
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 12
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 13
    lea rdx, [rel bool_false]
    inc qword [rdx + PyObject.ob_refcnt]
    call structseq_set
    mov rdi, rbx
    mov esi, 14
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 15
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 16
    lea rdx, [rel bool_false]
    inc qword [rdx + PyObject.ob_refcnt]
    call structseq_set
    mov rdi, rbx
    mov esi, 17
    mov rdx, 4300
    V_PACK_I64 rdx, rcx
    call structseq_set
    lea rdi, [rel sm_flags]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, rbx
    call obj_decref
    ; --- sys.float_info ---
    lea rdi, [rel float_info_type]
    call structseq_init_type
    lea rdi, [rel float_info_type]
    call structseq_new
    mov rbx, rax
    mov rdi, rbx
    mov esi, 0
    mov rdx, [rel float_info_v0]
    V_FROM_F64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 1
    mov rdx, 1024
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 2
    mov rdx, 308
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 3
    mov rdx, [rel float_info_v3]
    V_FROM_F64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 4
    mov rdx, -1021
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 5
    mov rdx, -307
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 6
    mov rdx, 15
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 7
    mov rdx, 53
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 8
    mov rdx, [rel float_info_v8]
    V_FROM_F64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 9
    mov rdx, 2
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 10
    mov rdx, 1
    V_PACK_I64 rdx, rcx
    call structseq_set
    lea rdi, [rel sm_float_info]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, rbx
    call obj_decref

    ; --- sys.int_info ---
    ; GMP-backed ints have no fixed digit size; these are CPython's
    ; numbers, which is what the modules reading them expect.
    lea rdi, [rel int_info_type]
    call structseq_init_type
    lea rdi, [rel int_info_type]
    call structseq_new
    mov rbx, rax
    mov rdi, rbx
    mov esi, 0
    mov rdx, 30
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 1
    mov rdx, 4
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 2
    mov rdx, 4300
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 3
    mov rdx, 640
    V_PACK_I64 rdx, rcx
    call structseq_set
    lea rdi, [rel sm_int_info]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, rbx
    call obj_decref

    ; --- sys.hash_info ---
    ; modulus and imag are ours for real -- int_hash_i64 uses 2^61-1
    ; and complex_hash 1000003.  algorithm says 'fnv' because
    ; str_hash is FNV-1a, not siphash; seed_bits and cutoff are 0
    ; because there is no hash randomisation to describe.
    lea rdi, [rel hash_info_type]
    call structseq_init_type
    lea rdi, [rel hash_info_type]
    call structseq_new
    mov rbx, rax
    mov rdi, rbx
    mov esi, 0
    mov rdx, 64
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 1
    mov rdx, 2305843009213693951
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 2
    mov rdx, 314159
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 3
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 4
    mov rdx, 1000003
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 5
    lea rdi, [rel hash_info_v5]
    call str_from_cstr_heap
    mov rdx, rax
    mov rdi, rbx
    mov esi, 5
    call structseq_set
    mov rdi, rbx
    mov esi, 6
    mov rdx, 64
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 7
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    mov rdi, rbx
    mov esi, 8
    mov rdx, 0
    V_PACK_I64 rdx, rcx
    call structseq_set
    lea rdi, [rel sm_hash_info]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, rbx
    call obj_decref

    ; --- sys.getdefaultencoding function ---
    lea rdi, [rel sys_getdefaultencoding_func]
    lea rsi, [rel sm_getdefaultencoding]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_getdefaultencoding]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.getfilesystemencoding function ---
    ; os._createenviron decodes posix.environ with it.  It answers 'utf-8'
    ; unconditionally: PEP 540's locale handling does not exist here, and
    ; neither does the surrogateescape error handler that would make a
    ; non-UTF-8 filename survive the round trip -- bugs.md records the gap.
    lea rdi, [rel sys_getfsencoding_func]
    lea rsi, [rel sm_getfsencoding]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_getfsencoding]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.getfilesystemencodeerrors function ---
    ; os._fscodec reads it next to getfilesystemencoding.  It answers
    ; 'surrogateescape', which is the name CPython uses -- and which
    ; str.encode here accepts and ignores; bugs.md records that.
    lea rdi, [rel sys_getfsencodeerrors_func]
    lea rsi, [rel sm_getfsencodeerrors]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_getfsencodeerrors]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.excepthook / sys.__excepthook__ / sys.unraisablehook ---
    ;
    ; threading reads sys.excepthook at import time, to save and restore it
    ; around a thread's run().  It is the same report the interpreter prints
    ; for an uncaught exception, which traceback_print already produces.
    SYS_ADD_FUNC_ALIAS sys_excepthook_func, sm_excepthook, sm_dunder_excepthook
    SYS_ADD_FUNC_ALIAS sys_unraisablehook_func, sm_unraisablehook, \
                       sm_dunder_unraisablehook
    SYS_ADD_FUNC sys_exc_info_func, sm_exc_info
    ; audit() and addaudithook() do nothing: there are no audit hooks here,
    ; and with none installed CPython's audit() is a no-op too.  os.walk,
    ; os.listdir and half of shutil call audit() unconditionally, and an
    ; AttributeError from it stopped them.
    SYS_ADD_FUNC sys_audit_func, sm_audit
    SYS_ADD_FUNC sys_audit_func, sm_addaudithook

    ; --- sys._getframe / sys._getframemodulename ---
    ;
    ; warnings._deprecated reaches for both, and nine stdlib modules come in
    ; behind that one call.  What comes back is a SNAPSHOT: see
    ; src/pyo/frameobj.asm for why it cannot be the frame itself.
    extern sys_getframe_func
    extern sys_getframemodulename_func
    SYS_ADD_FUNC sys_getframe_func, sm_getframe
    SYS_ADD_FUNC sys_getframemodulename_func, sm_getframemodulename

    ; --- sys.intern function ---
    lea rdi, [rel sys_intern_func]
    lea rsi, [rel sm_intern]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_intern]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.get_int_max_str_digits function ---
    lea rdi, [rel sys_get_int_max_str_digits_func]
    lea rsi, [rel sm_get_int_max_str_digits]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_get_int_max_str_digits]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.set_int_max_str_digits function ---
    lea rdi, [rel sys_set_int_max_str_digits_func]
    lea rsi, [rel sm_set_int_max_str_digits]
    call builtin_func_new
    push rax
    lea rdi, [rel sm_set_int_max_str_digits]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- Create the sys module object ---
    lea rdi, [rel sm_sys]
    call str_from_cstr_heap
    mov rdi, rax
    mov rsi, r15                ; dict
    call module_new
    mov [rel sys_module_obj], rax

    ; Register sys in sys.modules
    lea rdi, [rel sm_sys]
    call str_from_cstr_heap
    push rax
    mov rdi, r14                ; sys.modules dict
    mov rsi, rax
    mov rdx, [rel sys_module_obj]
    call dict_set
    pop rdi
    call obj_decref

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sys_module_init

;; ============================================================================
;; sys.getrecursionlimit() / sys.setrecursionlimit(n)
;; The interpreter has had a recursion counter since the bare SIGSEGV was
;; replaced with a RecursionError; these expose the limit it checks.
;; ============================================================================
DEF_FUNC sys_getrecursionlimit_func
    mov rax, [rel recursion_limit]
    extern recursion_limit
    mov edx, TAG_SMALLINT
    leave
    V_PACK rax, rdx
    ret
END_FUNC sys_getrecursionlimit_func

DEF_FUNC sys_setrecursionlimit_func
    cmp rsi, 1
    jne .srl_error
    mov rdi, [rdi]
    V_UNPACK rdi, rdx
    extern int_is_integer
    push rdi
    push rdx
    call int_is_integer
    pop rdx
    pop rdi
    test eax, eax
    jz .srl_error
    extern int_to_i64
    call int_to_i64
    cmp rax, 1
    jl .srl_value_error
    mov [rel recursion_limit], rax
    RET_NONE
    leave
    V_PACK rax, rdx
    ret
.srl_error:
    RAISE exc_TypeError_type, "setrecursionlimit() argument must be an int"
.srl_value_error:
    extern exc_ValueError_type
    RAISE exc_ValueError_type, "recursion limit must be greater or equal than 1"
END_FUNC sys_setrecursionlimit_func

;; ============================================================================
;; sys_exit_func(PyObject **args, int64_t nargs) -> PyObject*
;; sys.exit([code]) — exit the process
;; ============================================================================
DEF_FUNC sys_exit_func
    ; sys.exit raises SystemExit; it does not call the exit syscall.  Calling
    ; it directly skipped every `finally` block and context-manager __exit__
    ; between here and the top, and made `except SystemExit` unwritable.
    ; main.asm turns an uncaught SystemExit into the process status.
    xor edx, edx                ; no argument -> SystemExit()
    cmp rsi, 1
    jne .se_raise
    mov rdx, [rdi]              ; args[0], already a Value
    lea rax, [rel none_singleton]
    cmp rdx, rax
    jne .se_raise
    xor edx, edx                ; sys.exit(None) is SystemExit(None) -> args ()
.se_raise:
    mov rsi, rdx
    lea rdi, [rel exc_SystemExit_type]
    xor edx, edx
    extern exc_SystemExit_type
    extern exc_new
    call exc_new
    mov rdi, rax
    extern raise_exception_obj
    call raise_exception_obj
    ud2
END_FUNC sys_exit_func

;; ============================================================================
;; sys_getdefaultencoding_func(PyObject **args, int64_t nargs) -> rax = Value
;; Returns "utf-8"
;; ============================================================================
DEF_FUNC sys_getdefaultencoding_func
    lea rdi, [rel sm_utf8]
    call str_from_cstr_heap
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC sys_getdefaultencoding_func

;; ============================================================================
;; sys_get_int_max_str_digits_func(PyObject **args, int64_t nargs) -> rax = Value
;; Returns the current int max str digits limit
;; ============================================================================
DEF_FUNC sys_get_int_max_str_digits_func
    cmp rsi, 0
    jne .get_imsd_error
    mov rdi, [rel sys_int_max_str_digits]
    call int_from_i64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.get_imsd_error:
    extern exc_TypeError_type
    RAISE exc_TypeError_type, "get_int_max_str_digits() takes no arguments"
END_FUNC sys_get_int_max_str_digits_func

;; ============================================================================
;; sys_set_int_max_str_digits_func(PyObject **args, int64_t nargs) -> rax = Value
;; Sets the int max str digits limit. 0 = unlimited, otherwise >= 640
;; ============================================================================
DEF_FUNC sys_set_int_max_str_digits_func
    cmp rsi, 1
    jne .set_imsd_error

    mov rdi, [rdi]            ; args[0]

    V_UNPACK rdi, rdx
    call int_to_i64
    ; rax = new limit
    test rax, rax
    jz .set_imsd_ok         ; 0 = unlimited
    cmp rax, 640
    jl .set_imsd_value_error

.set_imsd_ok:
    mov [rel sys_int_max_str_digits], rax
    RET_NONE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.set_imsd_value_error:
    extern exc_ValueError_type
    RAISE exc_ValueError_type, "set_int_max_str_digits: value must be 0 or >= 640"

.set_imsd_error:
    RAISE exc_TypeError_type, "set_int_max_str_digits() takes exactly 1 argument"
END_FUNC sys_set_int_max_str_digits_func

;; ============================================================================
;; sys_path_add_script_dir(const char *pyc_path)
;; Extract directory from the .pyc path and prepend to sys.path[0]
;; ============================================================================
DEF_FUNC sys_path_add_script_dir
    push rbx
    push r12

    mov rbx, rdi                ; pyc_path

    ; Find last '/' in path
    mov rdi, rbx
    call ap_strlen
    mov r12, rax                ; length
    mov rcx, rax

.find_slash:
    dec rcx
    js .use_dot                 ; no slash found, use "."
    cmp byte [rbx + rcx], '/'
    jne .find_slash

    ; Found slash at position rcx
    ; Create string from path[0..rcx] (the directory part)
    test rcx, rcx
    jz .use_root                ; path starts with /, use "/"

    ; Create substring
    lea rdi, [rbx]
    mov rsi, rcx                ; length (excludes the slash)
    call str_new
    jmp .set_path

.use_root:
    lea rdi, [rel sm_slash]
    call str_from_cstr_heap
    jmp .set_path

.use_dot:
    lea rdi, [rel sm_dot]
    call str_from_cstr_heap

.set_path:
    ; Replace sys.path[0] with this directory
    ; rax = payload, rdx = tag (TAG_PTR)
    push rdx                    ; save new path tag
    push rax                    ; save new path payload
    mov rdi, [rel sys_path_list]
    ; Set list item 0
    mov rcx, [rdi + PyListObject.ob_item]       ; payloads
    ; DECREF old item[0]
    mov rdi, [rcx]
    DECREF_V rdi, rsi
    pop rax                     ; restore new path payload
    pop rdx                     ; restore new path tag
    V_PACK rax, rdx
    mov rdi, [rel sys_path_list]
    mov rcx, [rdi + PyListObject.ob_item]
    mov [rcx], rax

    pop r12
    pop rbx
    leave
    ret
END_FUNC sys_path_add_script_dir

;; ============================================================================
;; Data
;; ============================================================================

;; ============================================================================
;; sys.intern(string) -> string
;;
;; A real intern table, so `sys.intern(a) is sys.intern(b)` for equal strings.
;; functools and enum both intern names and then compare them with `is`.
;; ============================================================================
;; sys.getfilesystemencodeerrors() -> 'surrogateescape'
DEF_FUNC sys_getfsencodeerrors_func
    lea rdi, [rel sm_surrogateescape]
    call str_from_cstr_heap
    leave
    mov edx, TAG_PTR
    V_PACK rax, rdx
    ret
END_FUNC sys_getfsencodeerrors_func

;; sys.audit(event, *args) / sys.addaudithook(hook) -> None
;;
;; No hooks, so nothing to run.  Both are here because the stdlib calls
;; audit() on the way into any number of ordinary operations and does not
;; guard it.
;; ============================================================================
DEF_FUNC sys_audit_func
    LOAD_NONE rax
    leave
    ret
END_FUNC sys_audit_func

;; ============================================================================
;; sys.getfilesystemencoding() -> 'utf-8'
DEF_FUNC sys_getfsencoding_func
    lea rdi, [rel sm_utf8]
    call str_from_cstr_heap
    leave
    mov edx, TAG_PTR
    V_PACK rax, rdx
    ret
END_FUNC sys_getfsencoding_func

DEF_FUNC sys_intern_func
    cmp rsi, 1
    jne .si_error               ; exactly one argument, as CPython requires
    push rbx
    mov rbx, [rdi]
    V_TEST_PTR rbx, rax
    ja .si_not_str
    test rbx, rbx
    jz .si_not_str
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .si_not_str

    mov rax, [rel sys_intern_table]
    test rax, rax
    jnz .si_have_table
    call dict_new
    mov [rel sys_intern_table], rax
.si_have_table:
    mov rdi, [rel sys_intern_table]
    mov rsi, rbx
    call dict_get
    test rax, rax
    jz .si_insert
    mov rbx, rax
    jmp .si_return
.si_insert:
    mov rdi, [rel sys_intern_table]
    mov rsi, rbx
    mov rdx, rbx
    call dict_set
.si_return:
    mov rdi, rbx
    call obj_incref
    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.si_not_str:
    mov rsi, rbx
    pop rbx
    extern raise_type_error_with_name
    CSTRING rdi, `intern() argument must be str, not \x01`
    call raise_type_error_with_name
.si_error:
    RAISE exc_TypeError_type, "sys.intern() takes exactly one argument"
END_FUNC sys_intern_func

section .rodata

seh_not_exc_msg:
    db "TypeError: print_exception(): Exception expected for value, ", 0
seh_not_exc_end: db " found", 10, 0
sm_sys:          db "sys", 0
sm_modules:      db "modules", 0
sm_path:         db "path", 0
sm_argv:         db "argv", 0
sm_maxsize:      db "maxsize", 0
sm_platform:     db "platform", 0
sm_linux:        db "linux", 0
sm_version:      db "version", 0
sm_version_val:  db "3.12.0 (apython ", VERSION_STR, ")", 0
sm_version_info: db "version_info", 0
sm_float_info:   db "float_info", 0
sm_flags:        db "flags", 0
sm_int_info:     db "int_info", 0
sm_hash_info:    db "hash_info", 0
sm_final:        db "final", 0
sm_executable:   db "executable", 0
sm_prefix:       db "prefix", 0
sm_exec_prefix:  db "exec_prefix", 0
sm_base_prefix:      db "base_prefix", 0
sm_base_exec_prefix: db "base_exec_prefix", 0
sm_platlibdir:   db "platlibdir", 0
sm_abiflags:     db "abiflags", 0
sm_lib:          db "lib", 0
sm_copyright:        db "copyright", 0
sm_copyright_text:
    db "Copyright (c) 2026 Jeff Garzik.", 10
    db "All Rights Reserved.", 10
    db 10
    db "Portions of the standard library are", 10
    db "Copyright (c) 2001-2026 Python Software Foundation.", 10
    db "All Rights Reserved.", 0
sm_stdout:       db "stdout", 0
sm_stderr:       db "stderr", 0
sm_stdin:        db "stdin", 0
sm_stdout_name:  db "<stdout>", 0
sm_stderr_name:  db "<stderr>", 0
sm_stdin_name:   db "<stdin>", 0
sm_mode_w:       db "w", 0
sm_mode_r:       db "r", 0
sm_exit:         db "exit", 0
sm_getrecursionlimit: db "getrecursionlimit", 0
sm_setrecursionlimit: db "setrecursionlimit", 0
sm_implementation: db "implementation", 0
sm_name:         db "name", 0
sm_apython_name: db "apython", 0
sm_cache_tag:    db "cache_tag", 0
sm_cache_tag_val: db "cpython-312", 0
sm_warnoptions:  db "warnoptions", 0
sm_builtin_module_names: db "builtin_module_names", 0

; The CPython builtins this tree supplies from lib/ instead.  Each is found
; with no path entry, so saying it is built in is what a program observes.
sm_n_thread:      db "_thread", 0
sm_n_warnings:    db "_warnings", 0
sm_n_imp:         db "_imp", 0
sm_n_codecs:      db "_codecs", 0
sm_n_collections: db "_collections", 0
sm_n_operator:    db "_operator", 0
sm_n_string:      db "_string", 0
sm_n_random:      db "_random", 0
sm_n_contextvars: db "_contextvars", 0
sm_n_typing:      db "_typing", 0
sm_n_atexit:      db "atexit", 0
sm_n_binascii:    db "binascii", 0
sm_n_itertools:   db "itertools", 0
sm_n_tokenize:    db "_tokenize", 0
sm_n_ast:         db "_ast", 0
sm_n_struct:      db "_struct", 0
align 8
sm_supplied_names:
    dq sm_n_thread, sm_n_warnings, sm_n_imp, sm_n_codecs, sm_n_collections
    dq sm_n_operator, sm_n_string, sm_n_random, sm_n_contextvars, sm_n_typing
    dq sm_n_atexit, sm_n_binascii, sm_n_itertools, sm_n_tokenize, sm_n_ast
    dq sm_n_struct
SM_SUPPLIED_COUNT equ 16
sm_audit:         db "audit", 0
sm_addaudithook:  db "addaudithook", 0
sm_getfsencoding: db "getfilesystemencoding", 0
sm_getfsencodeerrors: db "getfilesystemencodeerrors", 0
sm_surrogateescape: db "surrogateescape", 0
sm_intern:       db "intern", 0
sm_excepthook:   db "excepthook", 0
sm_dunder_excepthook: db "__excepthook__", 0
sm_dunder_unraisablehook: db "__unraisablehook__", 0
sm_unraisablehook: db "unraisablehook", 0
sm_exc_info:     db "exc_info", 0
sm_getframe:     db "_getframe", 0
sm_getframemodulename: db "_getframemodulename", 0
sm_byteorder:    db "byteorder", 0
sm_little:       db "little", 0
sm_getdefaultencoding: db "getdefaultencoding", 0
sm_get_int_max_str_digits: db "get_int_max_str_digits", 0
sm_set_int_max_str_digits: db "set_int_max_str_digits", 0
sm_utf8:         db "utf-8", 0
sm_empty:        db "", 0
sm_slash:        db "/", 0
sm_dot:          db ".", 0

section .bss
global sys_modules_dict
sys_modules_dict: resq 1

global sys_path_list
sys_path_list: resq 1

global sys_module_obj
sys_module_obj: resq 1

global sys_stdout_obj
sys_stdout_obj: resq 1

section .data
align 8
; Not in .bss: the source compiler consults this to reject an over-long
; decimal literal, and ./apython foo.py compiles before sysmodule_init runs.
global sys_int_max_str_digits
sys_int_max_str_digits: dq 4300

sys_intern_table: dq 0

section .text

;; ============================================================================
;; sys.excepthook(type, value, traceback)
;;
;; The report the interpreter prints for an uncaught exception, on stderr.
;; threading saves and restores this around a thread's run(), and reads it at
;; import time -- which is where CPython's threading.py stopped.
;;
;; Only the VALUE is used: the exception object carries its own type and
;; traceback, and CPython's C hook falls back on the same when the three
;; arguments disagree.
;; ============================================================================
DEF_FUNC sys_excepthook_func
    cmp rsi, 3
    jl .seh_args
    mov rdi, [rdi + 8]          ; args[1], the exception
    ; "is a pointer" is not "is an exception": None is a pointer too, and
    ; `sys.excepthook(*sys.exc_info())` outside an except block passes three
    ; of them.  traceback_print read the exception's own fields off it.
    push rdi
    extern value_type
    call value_type             ; a Value, which may be an immediate
    pop rdi
    test rax, rax
    jz .seh_bad
    push rdi
    push rax
    mov rdi, rax
    extern exc_BaseException_type
    lea rsi, [rel exc_BaseException_type]
    extern type_is_subtype
    call type_is_subtype
    pop rsi                     ; the type
    pop rdi                     ; the value
    test eax, eax
    jz .seh_not_exc
    extern traceback_print
    call traceback_print
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.seh_not_exc:
    ; CPython REPORTS this rather than raising it -- the hook is what runs
    ; when there is nothing left to catch anything, so it prints the TypeError
    ; on stderr and returns.  Same text, same place.  rsi = the type.
    push rsi
    extern tb_write_cstr
    lea rdi, [rel seh_not_exc_msg]
    call tb_write_cstr
    pop rdi
    mov rdi, [rdi + PyTypeObject.tp_name]
    call tb_write_cstr
    lea rdi, [rel seh_not_exc_end]
    call tb_write_cstr
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.seh_bad:
    RAISE exc_TypeError_type, "excepthook(): Exception expected for value"
.seh_args:
    RAISE exc_TypeError_type, "excepthook() takes exactly 3 arguments"
END_FUNC sys_excepthook_func

;; ============================================================================
;; sys.unraisablehook(unraisable)
;;
;; What CPython calls for an exception that cannot be propagated -- in a
;; __del__, or in a generator being finalised.  The argument is a structseq;
;; the only field this can use is exc_value, and a missing one is not an
;; error, because the whole point of the hook is that nothing escapes it.
;; ============================================================================
UNH_EXC   equ 8
UNH_KEY   equ 16
UNH_ARG   equ 24
UNH_OBJ   equ 32
UNH_FRAME equ 48            ; + 0 pushes = 48
DEF_FUNC sys_unraisablehook_func, UNH_FRAME
    mov qword [rbp - UNH_OBJ], 0
    test rsi, rsi
    jz .suh_done
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .suh_done
    test rdi, rdi
    jz .suh_done
    mov [rbp - UNH_EXC], rdi
    mov [rbp - UNH_ARG], rdi

    ; The object the exception came out of, for the first line of the report.
    CSTRING rdi, "object"
    call str_from_cstr_heap
    mov [rbp - UNH_KEY], rax
    mov rdi, [rbp - UNH_ARG]
    mov rsi, rax
    call obj_getattr_opt
    push rax
    mov rdi, [rbp - UNH_KEY]
    call obj_decref
    pop rax
    test rax, rax
    jz .suh_no_object
    V_TEST_PTR rax, rcx
    ja .suh_drop_object
    mov [rbp - UNH_OBJ], rax
    jmp .suh_no_object
.suh_drop_object:
    mov rdi, rax
    XDECREF_V rdi, rcx
.suh_no_object:

    CSTRING rdi, "exc_value"
    extern str_from_cstr_heap
    call str_from_cstr_heap
    mov [rbp - UNH_KEY], rax
    mov rdi, [rbp - UNH_EXC]
    mov rsi, rax
    extern obj_getattr_opt
    call obj_getattr_opt
    mov [rbp - UNH_EXC], rax
    mov rdi, [rbp - UNH_KEY]
    call obj_decref
    mov rax, [rbp - UNH_EXC]
    test rax, rax
    jz .suh_done
    V_TEST_PTR rax, rcx
    ja .suh_drop
    ; The whole report, object line included -- the same one the interpreter
    ; prints when nothing has replaced this hook.
    mov rdi, rax
    mov rsi, [rbp - UNH_OBJ]
    extern traceback_unraisable_default
    call traceback_unraisable_default
.suh_drop:
    mov rdi, [rbp - UNH_EXC]
    XDECREF_V rdi, rcx
    mov rdi, [rbp - UNH_OBJ]
    test rdi, rdi
    jz .suh_done
    call obj_decref
.suh_done:
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC sys_unraisablehook_func


;; ============================================================================
;; sys.exc_info() -> (type, value, traceback), or (None, None, None)
;;
;; The exception being handled -- handled_exception, which an except block
;; installs and POP_EXCEPT takes down, and which a generator carries across a
;; suspension in PyFrame.exc_state.  Reading current_exception here answered
;; None from an `await` inside a handler onwards, and answered an exception
;; that was merely in flight in places where nothing was being handled at all.
;; threading reads it to report a thread that died, and CPython's contextlib
;; and unittest both use it.
;; ============================================================================
DEF_FUNC sys_exc_info_func
    extern handled_exception
    mov rax, [rel handled_exception]
    test rax, rax
    jz .sei_none
    push rax
    sub rsp, 8
    mov edi, 3
    extern tuple_new
    call tuple_new
    add rsp, 8
    pop rcx
    test rax, rax
    jz .sei_failed
    mov rdx, [rax + PyTupleObject.ob_item]
    mov rsi, [rcx + PyObject.ob_type]
    INCREF rsi
    mov [rdx], rsi
    INCREF rcx
    mov [rdx + 8], rcx
    mov rsi, [rcx + PyExceptionObject.exc_tb]
    test rsi, rsi
    jnz .sei_have_tb
    LOAD_NONE rsi
.sei_have_tb:
    INCREF rsi
    mov [rdx + 16], rsi
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.sei_failed:
    xor eax, eax
    leave
    ret

.sei_none:
    mov edi, 3
    call tuple_new
    test rax, rax
    jz .sei_failed
    mov rdx, [rax + PyTupleObject.ob_item]
    LOAD_NONE rcx
    INCREF rcx
    mov [rdx], rcx
    INCREF rcx
    mov [rdx + 8], rcx
    INCREF rcx
    mov [rdx + 16], rcx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
END_FUNC sys_exc_info_func
