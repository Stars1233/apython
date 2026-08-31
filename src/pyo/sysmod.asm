; sysmod.asm - sys module initialization
; Creates the sys module with standard attributes

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern ap_strlen
extern obj_decref
extern obj_incref
extern obj_dealloc
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

; ============================================================================
; sys_module_init(int argc, char **argv) -> void
; Initialize the sys module and register it in sys.modules
; ============================================================================
DEF_FUNC sys_module_init, 32
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - 8], rdi          ; argc
    mov [rbp - 16], rsi         ; argv

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

    ; Populate sys.argv from process argv
    mov rcx, [rbp - 8]         ; argc
    mov rdx, [rbp - 16]        ; argv
    xor ebx, ebx               ; i = 0
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
    mov rdi, 0x7FFFFFFFFFFFFFFF
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

    ; --- sys.version_info (tuple) ---
    mov rdi, 5
    call tuple_new
    mov rbx, rax                ; rbx = version_info tuple
    mov r8, [rbx + PyTupleObject.ob_item]       ; payloads
    ; (3, 12, 0, 'final', 0)
    ; slot 0: 3
    mov rdi, 3
    V_PACK_I64 rdi, rcx
    mov [r8], rdi
    ; slot 1: 12
    mov rdi, 12
    V_PACK_I64 rdi, rcx
    mov [r8 + 8], rdi
    ; slot 2: 0
    xor edi, edi
    V_PACK_I64 rdi, rcx
    mov [r8 + 16], rdi
    ; slot 3: 'final' (string, TAG_PTR)
    lea rdi, [rel sm_final]
    call str_from_cstr_heap
    mov r8, [rbx + PyTupleObject.ob_item]       ; reload payloads (clobbered)
    mov [r8 + 24], rax
    ; slot 4: 0
    xor edi, edi
    V_PACK_I64 rdi, rcx
    mov [r8 + 32], rdi

    lea rdi, [rel sm_version_info]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx
    call dict_set
    pop rdi
    call obj_decref
    ; DECREF tuple
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

    ; --- sys.prefix / sys.exec_prefix ---
    lea rdi, [rel sm_empty]
    call str_from_cstr_heap
    push rax
    lea rdi, [rel sm_prefix]
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

    lea rdi, [rel sm_empty]
    call str_from_cstr_heap
    push rax
    lea rdi, [rel sm_exec_prefix]
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
    mov [rbp - 24], rax

    lea rdi, [rel sm_apython_name]
    call str_from_cstr_heap
    push rax
    mov rdi, [rbp - 24]
    lea rsi, [rel sm_name]
    mov rdx, rax
    call namespace_set
    pop rdi
    call obj_decref

    lea rdi, [rel sm_cache_tag_val]
    call str_from_cstr_heap
    push rax
    mov rdi, [rbp - 24]
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
    mov rdi, [rbp - 24]
    lea rsi, [rel sm_version]
    mov rdx, rax
    call namespace_set
.no_impl_version:

    lea rdi, [rel sm_implementation]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rbp - 24]
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, [rbp - 24]
    call obj_decref

    ; --- sys.warnoptions (empty; nothing parses -W yet) ---
    xor edi, edi
    call list_new
    mov [rbp - 24], rax
    lea rdi, [rel sm_warnoptions]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rbp - 24]
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, [rbp - 24]
    call obj_decref

    ; --- sys.builtin_module_names ---
    ; The modules that are compiled in rather than found on sys.path.  os.py
    ; reads it to decide which platform module to import, and several others
    ; use it to tell a built-in apart from a shadowing file.
    mov edi, SM_BUILTIN_COUNT
    call tuple_new
    mov [rbp - 24], rax
    xor r13d, r13d
.sm_bmn_loop:
    cmp r13, SM_BUILTIN_COUNT
    jge .sm_bmn_done
    lea rax, [rel sm_builtin_names]
    mov rdi, [rax + r13*8]
    call str_from_cstr_heap
    mov rcx, [rbp - 24]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx + r13*8], rax
    inc r13
    jmp .sm_bmn_loop
.sm_bmn_done:
    lea rdi, [rel sm_builtin_module_names]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rbp - 24]
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, [rbp - 24]
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

    ; --- sys.int_info (simple stub: bits_per_digit=30, sizeof_digit=4) ---
    ; Skip for now — not critical for basic imports

    ; --- sys.float_info (stub) ---
    ; Skip for now

    ; --- sys.hash_info (stub) ---
    ; Skip for now

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

; ============================================================================
; sys.getrecursionlimit() / sys.setrecursionlimit(n)
; The interpreter has had a recursion counter since the bare SIGSEGV was
; replaced with a RecursionError; these expose the limit it checks.
; ============================================================================
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
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.srl_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "setrecursionlimit() argument must be an int"
    call raise_exception
.srl_value_error:
    extern exc_ValueError_type
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "recursion limit must be greater or equal than 1"
    call raise_exception
END_FUNC sys_setrecursionlimit_func

; ============================================================================
; sys_exit_func(PyObject **args, int64_t nargs) -> PyObject*
; sys.exit([code]) — exit the process
; ============================================================================
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

; ============================================================================
; sys_getdefaultencoding_func(PyObject **args, int64_t nargs) -> PyObject*
; Returns "utf-8"
; ============================================================================
DEF_FUNC sys_getdefaultencoding_func
    lea rdi, [rel sm_utf8]
    call str_from_cstr_heap
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC sys_getdefaultencoding_func

; ============================================================================
; sys_get_int_max_str_digits_func(PyObject **args, int64_t nargs) -> PyObject*
; Returns the current int max str digits limit
; ============================================================================
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "get_int_max_str_digits() takes no arguments"
    call raise_exception
END_FUNC sys_get_int_max_str_digits_func

; ============================================================================
; sys_set_int_max_str_digits_func(PyObject **args, int64_t nargs) -> PyObject*
; Sets the int max str digits limit. 0 = unlimited, otherwise >= 640
; ============================================================================
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
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.set_imsd_value_error:
    extern exc_ValueError_type
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "set_int_max_str_digits: value must be 0 or >= 640"
    call raise_exception

.set_imsd_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "set_int_max_str_digits() takes exactly 1 argument"
    call raise_exception
END_FUNC sys_set_int_max_str_digits_func

; ============================================================================
; sys_path_add_script_dir(const char *pyc_path)
; Extract directory from the .pyc path and prepend to sys.path[0]
; ============================================================================
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

; ============================================================================
; Data
; ============================================================================

;; ============================================================================
;; sys.intern(string) -> string
;;
;; A real intern table, so `sys.intern(a) is sys.intern(b)` for equal strings.
;; functools and enum both intern names and then compare them with `is`.
;; ============================================================================
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "sys.intern() takes exactly one argument"
    call raise_exception
END_FUNC sys_intern_func

section .rodata

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
sm_final:        db "final", 0
sm_executable:   db "executable", 0
sm_prefix:       db "prefix", 0
sm_exec_prefix:  db "exec_prefix", 0
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
sm_bmn_abc:      db "_abc", 0
sm_bmn_weakref:  db "_weakref", 0
sm_bmn_sre:      db "_sre", 0
sm_bmn_builtins: db "builtins", 0
sm_bmn_sys:      db "sys", 0
sm_bmn_time:     db "time", 0
align 8
sm_builtin_names:
    dq sm_bmn_abc, sm_bmn_weakref, sm_bmn_sre, sm_bmn_builtins, sm_bmn_sys
    dq sm_bmn_time
SM_BUILTIN_COUNT equ 6
sm_intern:       db "intern", 0
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
