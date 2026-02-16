; sysmod.asm - sys module initialization
; Creates the sys module with standard attributes

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"

extern ap_malloc
extern ap_free
extern ap_strlen
extern ap_memcpy
extern obj_decref
extern obj_incref
extern obj_dealloc
extern str_from_cstr
extern str_new
extern str_type
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
extern module_type
extern fileobj_new
extern builtin_func_new
extern sys_exit
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
    call str_from_cstr
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
    call str_from_cstr
    push rax
    mov rdi, r12
    mov rsi, rax
    call list_append
    pop rdi
    call obj_decref

    ; Store sys.path
    lea rdi, [rel sm_path]
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, r12
    call dict_set
    pop rdi
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
    call str_from_cstr
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
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, r13
    call dict_set
    pop rdi
    call obj_decref

    ; --- sys.maxsize ---
    mov rdi, 0x7FFFFFFFFFFFFFFF
    call int_from_i64
    push rax
    lea rdi, [rel sm_maxsize]
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.platform ---
    lea rdi, [rel sm_linux]
    call str_from_cstr
    push rax
    lea rdi, [rel sm_platform]
    call str_from_cstr
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
    call str_from_cstr
    push rax
    lea rdi, [rel sm_version]
    call str_from_cstr
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
    ; (3, 12, 0, 'final', 0)
    mov rdi, 3
    bts rdi, 63
    mov [rbx + PyTupleObject.ob_item], rdi
    mov rdi, 12
    bts rdi, 63
    mov [rbx + PyTupleObject.ob_item + 8], rdi
    xor edi, edi
    bts rdi, 63
    mov [rbx + PyTupleObject.ob_item + 16], rdi
    push rbx
    lea rdi, [rel sm_final]
    call str_from_cstr
    pop rbx
    mov [rbx + PyTupleObject.ob_item + 24], rax
    xor edi, edi
    bts rdi, 63
    mov [rbx + PyTupleObject.ob_item + 32], rdi

    lea rdi, [rel sm_version_info]
    call str_from_cstr
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
    call str_from_cstr
    push rax
    lea rdi, [rel sm_executable]
    call str_from_cstr
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
    call str_from_cstr
    push rax
    lea rdi, [rel sm_prefix]
    call str_from_cstr
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
    call str_from_cstr
    push rax
    lea rdi, [rel sm_exec_prefix]
    call str_from_cstr
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
    call str_from_cstr
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
    call str_from_cstr
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
    call str_from_cstr
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
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; --- sys.byteorder ---
    lea rdi, [rel sm_little]
    call str_from_cstr
    push rax
    lea rdi, [rel sm_byteorder]
    call str_from_cstr
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
    call str_from_cstr
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
    call str_from_cstr
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
    call str_from_cstr
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
    call str_from_cstr
    mov rdi, rax
    mov rsi, r15                ; dict
    call module_new
    mov [rel sys_module_obj], rax

    ; Register sys in sys.modules
    lea rdi, [rel sm_sys]
    call str_from_cstr
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
; sys_exit_func(PyObject **args, int64_t nargs) -> PyObject*
; sys.exit([code]) — exit the process
; ============================================================================
DEF_FUNC sys_exit_func
    cmp rsi, 0
    je .exit_0
    cmp rsi, 1
    jne .exit_0

    ; Get exit code from args[0]
    mov rax, [rdi]
    ; Check if SmallInt
    test rax, rax
    js .exit_smallint
    ; Not SmallInt — exit 1
    mov edi, 1
    call sys_exit

.exit_smallint:
    ; Decode SmallInt
    shl rax, 1
    sar rax, 1
    mov edi, eax
    call sys_exit

.exit_0:
    xor edi, edi
    call sys_exit
END_FUNC sys_exit_func

; ============================================================================
; sys_getdefaultencoding_func(PyObject **args, int64_t nargs) -> PyObject*
; Returns "utf-8"
; ============================================================================
DEF_FUNC sys_getdefaultencoding_func
    lea rdi, [rel sm_utf8]
    call str_from_cstr
    leave
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

    mov rdi, [rdi]
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
    leave
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
    call str_from_cstr
    jmp .set_path

.use_dot:
    lea rdi, [rel sm_dot]
    call str_from_cstr

.set_path:
    ; Replace sys.path[0] with this directory
    push rax
    mov rdi, [rel sys_path_list]
    ; Set list item 0
    mov rcx, [rdi + PyListObject.ob_item]
    ; DECREF old item[0]
    push rax
    mov rdi, [rcx]
    test rdi, rdi
    js .skip_decref
    jz .skip_decref
    call obj_decref
.skip_decref:
    pop rax
    mov rdi, [rel sys_path_list]
    mov rcx, [rdi + PyListObject.ob_item]
    mov [rcx], rax              ; store new path (already has refcount 1)

    add rsp, 8                  ; discard saved path from push at .set_path
    pop r12
    pop rbx
    leave
    ret
END_FUNC sys_path_add_script_dir

; ============================================================================
; Data
; ============================================================================
section .rodata

sm_sys:          db "sys", 0
sm_modules:      db "modules", 0
sm_path:         db "path", 0
sm_argv:         db "argv", 0
sm_maxsize:      db "maxsize", 0
sm_platform:     db "platform", 0
sm_linux:        db "linux", 0
sm_version:      db "version", 0
sm_version_val:  db "3.12.0 (apython)", 0
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

global sys_int_max_str_digits
sys_int_max_str_digits: resq 1
