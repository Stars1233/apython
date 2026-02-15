; import.asm - Import machinery for apython
; Handles IMPORT_NAME / IMPORT_FROM opcodes and module loading

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"
%include "marshal.inc"

extern ap_malloc
extern ap_free
extern ap_memcpy
extern ap_strlen
extern ap_strcmp
extern ap_memcmp
extern obj_decref
extern obj_incref
extern obj_dealloc
extern str_from_cstr
extern str_new
extern str_type
extern int_from_i64
extern none_singleton
extern dict_new
extern dict_get
extern dict_set
extern list_new
extern list_append
extern tuple_new
extern type_type
extern module_new
extern module_type
extern fatal_error
extern raise_exception
extern eval_frame
extern frame_new
extern frame_free
extern pyc_read_file
extern sys_open
extern sys_close

; Marshal globals (save/restore across nested loads)
extern marshal_buf
extern marshal_pos
extern marshal_len
extern marshal_refs
extern marshal_ref_count
extern marshal_ref_cap

; sys module globals
extern sys_modules_dict
extern sys_path_list
extern sys_module_obj
extern sys_module_init
extern sys_path_add_script_dir

; builtins
extern builtins_dict_global
extern exc_ImportError_type

; Named frame-layout constants
IM_FRAME    equ 80

IM_NAME     equ 8           ; import name str
IM_FROMLIST equ 16          ; fromlist
IM_LEVEL    equ 24          ; level (int)
IM_DOTPOS   equ 32          ; dot position for dotted names
IM_TOPMOD   equ 40          ; top-level module
IM_MBUF     equ 48          ; saved marshal_buf
IM_MPOS     equ 56          ; saved marshal_pos
IM_MLEN     equ 64          ; saved marshal_len
IM_MREFS    equ 72          ; saved marshal_refs
IM_MRCNT    equ 80          ; saved marshal_ref_count (shares slot with frame... move to separate)

; Re-layout to avoid collisions
IF_NAME     equ 8
IF_FROMLIST equ 16
IF_LEVEL    equ 24
IF_TOPMOD   equ 32
IF_MBUF     equ 40
IF_MPOS     equ 48
IF_MLEN     equ 56
IF_MREFS    equ 64
IF_MRCNT    equ 72
IF_MRCAP    equ 80

IF_FRAME    equ 88

; ============================================================================
; import_init(int argc, char **argv)
; Initialize the import system: sys module + builtins in sys.modules
; ============================================================================
DEF_FUNC import_init
    push rbx
    push r12
    mov rbx, rdi                ; argc
    mov r12, rsi                ; argv

    ; Initialize sys module
    mov rdi, rbx
    mov rsi, r12
    call sys_module_init

    ; Register builtins module in sys.modules
    lea rdi, [rel im_builtins]
    call str_from_cstr
    push rax                    ; key

    ; Create builtins module wrapping existing builtins dict
    lea rdi, [rel im_builtins]
    call str_from_cstr
    mov rdi, rax
    mov rsi, [rel builtins_dict_global]
    call module_new
    mov rbx, rax                ; builtins module

    ; dict_set(sys_modules, "builtins", builtins_module)
    mov rdi, [rel sys_modules_dict]
    pop rsi                     ; key = "builtins"
    mov rdx, rbx
    call dict_set

    ; DECREF key and module (dict_set INCREFs both)
    ; key was already consumed, but dict_set INCREF'd it; we need to decref our copy
    ; Actually str_from_cstr returned with refcount 1, dict_set INCREF'd, so decref ours
    ; The key was popped and passed to dict_set as rsi. dict_set INCREFs key and value.
    ; We need to DECREF our references.
    mov rdi, rbx                ; builtins module
    call obj_decref

    pop r12
    pop rbx
    leave
    ret
END_FUNC import_init

; ============================================================================
; import_module(PyObject *name_str, PyObject *fromlist, int64_t level) -> PyObject*
; Main import entry point
; name_str = module name from co_names
; fromlist = tuple of names or None
; level = 0 for absolute, >0 for relative
; Returns: module object (new reference)
; ============================================================================
DEF_FUNC import_module, IF_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - IF_NAME], rdi        ; name_str
    mov [rbp - IF_FROMLIST], rsi    ; fromlist
    mov [rbp - IF_LEVEL], rdx       ; level

    ; For now, skip relative import handling (level > 0)
    ; TODO: resolve relative imports

    ; Get name as C string for comparisons
    mov rdi, [rbp - IF_NAME]
    lea rbx, [rdi + PyStrObject.data]  ; rbx = name cstr
    mov r14, [rdi + PyStrObject.ob_size] ; r14 = name length

    ; Check sys.modules first
    mov rdi, [rel sys_modules_dict]
    mov rsi, [rbp - IF_NAME]
    call dict_get
    test rax, rax
    jnz .found_cached

    ; For dotted names (a.b.c), import each component
    ; First, check if name contains a dot
    xor ecx, ecx
.find_dot:
    cmp rcx, r14
    jge .no_dot
    cmp byte [rbx + rcx], '.'
    je .has_dots
    inc rcx
    jmp .find_dot

.no_dot:
    ; Simple name, no dots
    mov rdi, [rbp - IF_NAME]
    call import_find_and_load
    test rax, rax
    jz .import_error
    mov r12, rax                ; r12 = module

    ; If fromlist is empty/None, return this module
    ; If fromlist is non-empty, return this module (it's already the leaf)
    mov rax, r12
    jmp .done

.has_dots:
    ; Dotted name: import each component
    ; e.g. "os.path" -> import "os", then import "os.path"
    ; Return: if fromlist empty -> top-level; if fromlist non-empty -> leaf

    ; Import first component (up to first dot)
    ; Find first dot position
    xor ecx, ecx
.find_first_dot:
    cmp byte [rbx + rcx], '.'
    je .got_first_dot
    cmp rcx, r14
    jge .got_first_dot
    inc rcx
    jmp .find_first_dot
.got_first_dot:

    ; Create substring for first component
    mov rdi, rbx
    mov rsi, rcx
    call str_new
    push rax                    ; save first component name

    ; Check sys.modules for first component
    mov rdi, [rel sys_modules_dict]
    mov rsi, rax
    call dict_get
    test rax, rax
    jnz .have_first_component

    ; Load first component
    mov rdi, [rsp]              ; first component name
    call import_find_and_load
    test rax, rax
    jz .dotted_error

.have_first_component:
    mov r12, rax                ; r12 = top-level module
    inc qword [r12 + PyObject.ob_refcnt]
    mov [rbp - IF_TOPMOD], r12

    ; DECREF first component name
    pop rdi
    call obj_decref

    ; Now try to import the full dotted name
    mov rdi, [rel sys_modules_dict]
    mov rsi, [rbp - IF_NAME]
    call dict_get
    test rax, rax
    jnz .have_full_dotted

    ; Need to load intermediate + full path
    mov rdi, [rbp - IF_NAME]
    call import_find_and_load
    test rax, rax
    jz .dotted_load_error

.have_full_dotted:
    mov r13, rax                ; r13 = leaf module

    ; Decide what to return based on fromlist
    mov rax, [rbp - IF_FROMLIST]
    ; Check if fromlist is None
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .return_top
    ; Check if fromlist is empty tuple
    test rax, rax
    js .return_top              ; SmallInt = level, not fromlist
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel none_singleton]
    mov rdx, [rdx + PyObject.ob_type]
    cmp rcx, rdx
    je .return_top
    ; Check tuple size
    mov rcx, [rax + PyTupleObject.ob_size]
    test rcx, rcx
    jz .return_top

    ; fromlist is non-empty -> return leaf module
    mov rax, r13
    inc qword [rax + PyObject.ob_refcnt]
    ; DECREF top module
    mov rdi, r12
    call obj_decref
    jmp .done

.return_top:
    ; fromlist empty -> return top-level module
    mov rax, r12
    ; r12 already INCREF'd above
    jmp .done

.dotted_error:
    pop rdi                     ; clean up first component name
    call obj_decref
    jmp .import_error

.dotted_load_error:
    mov rdi, r12
    call obj_decref
    jmp .import_error

.found_cached:
    ; Found in sys.modules, INCREF and return
    inc qword [rax + PyObject.ob_refcnt]
    jmp .done

.import_error:
    ; Raise ImportError
    lea rdi, [rel exc_ImportError_type]
    lea rsi, [rbx]              ; module name cstr
    call raise_exception
    ; does not return

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_module

; ============================================================================
; import_find_and_load(PyObject *name_str) -> PyObject*
; Find module on disk and load it
; Returns module object or NULL
; ============================================================================
DEF_FUNC import_find_and_load, IF_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; name_str
    mov [rbp - IF_NAME], rdi

    ; Convert dots to slashes for path lookup
    ; name_str data -> "os.path" -> need to search for "os/path"
    lea r14, [rbx + PyStrObject.data]  ; name cstr
    mov r15, [rbx + PyStrObject.ob_size] ; name length

    ; Build a path buffer with dots replaced by slashes
    ; Max path = 4096
    sub rsp, 4096
    mov r12, rsp                ; r12 = path_component buffer

    ; Copy name, replacing '.' with '/'
    xor ecx, ecx
.copy_name:
    cmp rcx, r15
    jge .copy_done
    movzx eax, byte [r14 + rcx]
    cmp al, '.'
    jne .no_replace
    mov al, '/'
.no_replace:
    mov [r12 + rcx], al
    inc rcx
    jmp .copy_name
.copy_done:
    mov byte [r12 + rcx], 0    ; null-terminate

    ; Search sys.path for the module
    mov rdi, [rel sys_path_list]
    mov r13, [rdi + PyListObject.ob_size]  ; r13 = number of paths
    mov rdi, [rdi + PyListObject.ob_item]  ; rdi = paths array
    mov [rbp - IF_TOPMOD], rdi  ; save paths array ptr
    xor r15d, r15d              ; r15 = path index

.search_loop:
    cmp r15, r13
    jge .not_found

    mov rdi, [rbp - IF_TOPMOD]
    mov rdi, [rdi + r15 * 8]   ; path entry (str object)
    ; Skip SmallInt entries
    test rdi, rdi
    js .next_path

    ; Build full path: <dir>/<name>/__pycache__/__init__.cpython-312.pyc
    ; Use a second stack buffer for full path
    lea rsi, [rdi + PyStrObject.data]  ; dir cstr
    mov rdx, [rdi + PyStrObject.ob_size] ; dir length
    lea rdi, [rbp - IF_FRAME - 4096]   ; oops, can't reference below our alloc
    ; Use a fixed BSS buffer instead
    mov rdi, [rel import_path_buf_ptr]

    ; Copy dir
    push rdx
    push rsi
    mov rdi, [rel import_path_buf_ptr]
    test rdi, rdi
    jnz .have_buf
    ; Allocate path buffer
    mov edi, 8192
    call ap_malloc
    mov [rel import_path_buf_ptr], rax
    mov rdi, rax
.have_buf:
    pop rsi
    pop rdx

    ; Copy dir to buffer
    push rdx                    ; save dir length
    mov rcx, rdx
    test rcx, rcx
    jz .no_dir_copy
    push rdi
    call ap_memcpy
    pop rdi
.no_dir_copy:
    pop rdx                     ; restore dir length

    ; Append '/' if dir is non-empty
    test rdx, rdx
    jz .no_slash
    mov byte [rdi + rdx], '/'
    inc rdx
.no_slash:

    ; Try 1: <dir>/<name>/__pycache__/__init__.cpython-312.pyc (package)
    push rdx                    ; save offset
    push rdi                    ; save buf

    ; Append name_component
    lea rsi, [r12]              ; name with slashes
    mov rdi, [rsp]              ; buf
    add rdi, [rsp + 8]         ; buf + offset
    call ap_strlen
    push rax                    ; save name len
    lea rsi, [r12]
    mov rdi, [rsp + 8]
    add rdi, [rsp + 16]
    mov rdx, rax
    call ap_memcpy
    pop rcx                     ; name len
    pop rdi                     ; buf
    pop rdx                     ; offset
    add rdx, rcx

    ; Append "/__pycache__/__init__.cpython-312.pyc"
    push rdx
    push rdi
    lea rsi, [rel im_pkg_pyc_suffix]
    add rdi, rdx
    mov rdx, im_pkg_pyc_suffix_len
    call ap_memcpy
    pop rdi
    pop rdx
    add rdx, im_pkg_pyc_suffix_len
    mov byte [rdi + rdx], 0

    ; Try to open this path
    push rdi                    ; save buf
    mov esi, 0                  ; O_RDONLY
    xor edx, edx
    call sys_open
    pop rdi
    test rax, rax
    jns .found_package

    ; Try 2: <dir>/__pycache__/<name>.cpython-312.pyc (module)
    mov rdi, [rel import_path_buf_ptr]

    ; Reload dir info from path entry
    mov rax, [rbp - IF_TOPMOD]
    mov rax, [rax + r15 * 8]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]

    ; Copy dir
    push rdx
    test rdx, rdx
    jz .no_dir_copy2
    push rdi
    call ap_memcpy
    pop rdi
.no_dir_copy2:
    pop rdx

    ; Append '/' if needed
    test rdx, rdx
    jz .no_slash2
    mov byte [rdi + rdx], '/'
    inc rdx
.no_slash2:

    ; Append "__pycache__/"
    push rdx
    push rdi
    lea rsi, [rel im_pycache_prefix]
    add rdi, rdx
    mov rdx, im_pycache_prefix_len
    call ap_memcpy
    pop rdi
    pop rdx
    add rdx, im_pycache_prefix_len

    ; For dotted names, we need the last component only
    ; e.g., "os.path" -> look for "path.cpython-312.pyc" in os/__pycache__/
    ; But we need the parent's __pycache__. Actually for submodules,
    ; the parent must already be imported and have __path__ set.
    ; For simplicity, handle the leaf component name
    ; Find last '/' in r12 (name component path)
    lea rsi, [r12]
    call ap_strlen
    mov rcx, rax
    mov r8, rsi                 ; start of name component
.find_last_slash:
    dec rcx
    js .no_subdir
    cmp byte [rsi + rcx], '/'
    jne .find_last_slash
    ; Found slash - leaf is after it
    lea r8, [rsi + rcx + 1]
    jmp .have_leaf
.no_subdir:
    mov r8, rsi                 ; whole name is the leaf
.have_leaf:

    ; Append leaf name
    push rdx                    ; save offset
    push rdi                    ; save buf
    push r8                     ; save leaf ptr across strlen
    mov rdi, r8
    call ap_strlen
    pop r8                      ; restore leaf ptr
    pop rdi                     ; restore buf
    pop rdx                     ; restore offset
    ; rax = leaf name length
    mov rcx, rax
    push rdx                    ; save offset
    push rdi                    ; save buf
    push rcx                    ; save leaf len
    lea rsi, [r8]               ; src = leaf name
    add rdi, rdx                ; dst = buf + offset
    mov rdx, rcx                ; count = leaf len
    call ap_memcpy
    pop rcx                     ; leaf len
    pop rdi                     ; buf
    pop rdx                     ; offset
    add rdx, rcx

    ; Append ".cpython-312.pyc"
    push rdx
    push rdi
    lea rsi, [rel im_pyc_suffix]
    mov rdi, [rsp]
    add rdi, [rsp + 8]
    mov rdx, im_pyc_suffix_len
    call ap_memcpy
    pop rdi
    pop rdx
    add rdx, im_pyc_suffix_len
    mov byte [rdi + rdx], 0

    ; Try to open
    push rdi
    mov esi, 0                  ; O_RDONLY
    xor edx, edx
    call sys_open
    pop rdi
    test rax, rax
    jns .found_module

    ; Try 3: <dir>/<name>.cpython-312.pyc (module, no __pycache__)
    mov rdi, [rel import_path_buf_ptr]
    mov rax, [rbp - IF_TOPMOD]
    mov rax, [rax + r15 * 8]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    push rdx
    test rdx, rdx
    jz .no_dir_copy3
    push rdi
    call ap_memcpy
    pop rdi
.no_dir_copy3:
    pop rdx
    test rdx, rdx
    jz .no_slash3
    mov byte [rdi + rdx], '/'
    inc rdx
.no_slash3:
    ; Append leaf name + ".cpython-312.pyc"
    push rdx                    ; save offset
    push rdi                    ; save buf
    push r8                     ; save leaf ptr across strlen
    mov rdi, r8
    call ap_strlen
    pop r8                      ; restore leaf ptr
    pop rdi                     ; restore buf
    pop rdx                     ; restore offset
    mov rcx, rax                ; leaf name length
    push rdx                    ; save offset
    push rdi                    ; save buf
    push rcx                    ; save leaf len
    lea rsi, [r8]               ; src = leaf name
    add rdi, rdx                ; dst = buf + offset
    mov rdx, rcx                ; count = leaf len
    call ap_memcpy
    pop rcx                     ; leaf len
    pop rdi                     ; buf
    pop rdx                     ; offset
    add rdx, rcx
    push rdx
    push rdi
    lea rsi, [rel im_pyc_suffix]
    mov rdi, [rsp]
    add rdi, [rsp + 8]
    mov rdx, im_pyc_suffix_len
    call ap_memcpy
    pop rdi
    pop rdx
    add rdx, im_pyc_suffix_len
    mov byte [rdi + rdx], 0

    push rdi
    mov esi, 0
    xor edx, edx
    call sys_open
    pop rdi
    test rax, rax
    jns .found_module

.next_path:
    inc r15
    jmp .search_loop

.found_package:
    ; rax = fd, close it (we'll re-open via pyc_read_file)
    mov rdi, rax
    call sys_close

    ; Load as package (is_package = 1)
    mov rdi, [rbp - IF_NAME]
    mov rsi, [rel import_path_buf_ptr]
    mov edx, 1                  ; is_package
    call import_load_module
    jmp .load_done

.found_module:
    ; rax = fd, close it
    mov rdi, rax
    call sys_close

    ; Load as module (is_package = 0)
    mov rdi, [rbp - IF_NAME]
    mov rsi, [rel import_path_buf_ptr]
    xor edx, edx               ; is_package = 0
    call import_load_module

.load_done:
    add rsp, 4096
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    xor eax, eax
    add rsp, 4096
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_find_and_load

; ============================================================================
; import_load_module(PyObject *name_str, const char *path_cstr, int is_package) -> PyObject*
; Load a .pyc file and execute it as a module
; ============================================================================
DEF_FUNC import_load_module, IF_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; name_str
    mov r12, rsi                ; path_cstr
    mov r13d, edx               ; is_package

    ; Save marshal globals
    mov rax, [rel marshal_buf]
    mov [rbp - IF_MBUF], rax
    mov rax, [rel marshal_pos]
    mov [rbp - IF_MPOS], rax
    mov rax, [rel marshal_len]
    mov [rbp - IF_MLEN], rax
    mov rax, [rel marshal_refs]
    mov [rbp - IF_MREFS], rax
    mov rax, [rel marshal_ref_count]
    mov [rbp - IF_MRCNT], rax
    mov rax, [rel marshal_ref_cap]
    mov [rbp - IF_MRCAP], rax

    ; Read .pyc file -> code object
    mov rdi, r12
    call pyc_read_file
    test rax, rax
    jz .load_failed
    mov r14, rax                ; r14 = code object

    ; Restore marshal globals
    mov rax, [rbp - IF_MBUF]
    mov [rel marshal_buf], rax
    mov rax, [rbp - IF_MPOS]
    mov [rel marshal_pos], rax
    mov rax, [rbp - IF_MLEN]
    mov [rel marshal_len], rax
    mov rax, [rbp - IF_MREFS]
    mov [rel marshal_refs], rax
    mov rax, [rbp - IF_MRCNT]
    mov [rel marshal_ref_count], rax
    mov rax, [rbp - IF_MRCAP]
    mov [rel marshal_ref_cap], rax

    ; Create module dict
    call dict_new
    mov r15, rax                ; r15 = module dict

    ; Set __name__
    lea rdi, [rel im_dunder_name]
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx                ; name_str
    call dict_set
    pop rdi
    call obj_decref

    ; Set __file__
    mov rdi, r12                ; path cstr
    call str_from_cstr
    push rax                    ; file str
    lea rdi, [rel im_dunder_file]
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

    ; Set __loader__ = None
    lea rdi, [rel im_dunder_loader]
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    pop rdi
    call obj_decref

    ; Set __spec__ = None
    lea rdi, [rel im_dunder_spec]
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    pop rdi
    call obj_decref

    ; Set __package__
    test r13d, r13d
    jz .set_parent_package

    ; Package: __package__ = name, __path__ = [dir]
    lea rdi, [rel im_dunder_package]
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx                ; name_str
    call dict_set
    pop rdi
    call obj_decref

    ; Compute package directory from path
    ; path = ".../pkg/__pycache__/__init__.cpython-312.pyc"
    ; We want ".../pkg"
    mov rdi, r12
    call ap_strlen
    mov rcx, rax
    ; Walk backwards past two slashes to get to pkg dir
    dec rcx
.find_slash1:
    dec rcx
    js .use_dot_path
    cmp byte [r12 + rcx], '/'
    jne .find_slash1
    ; Found first slash (before __init__.cpython...)
    dec rcx
.find_slash2:
    dec rcx
    js .use_dot_path
    cmp byte [r12 + rcx], '/'
    jne .find_slash2
    ; Found second slash (before __pycache__)
    ; pkg dir = path[0..rcx]
    mov rdi, r12
    mov rsi, rcx
    call str_new
    jmp .set_path

.use_dot_path:
    lea rdi, [rel im_dot]
    call str_from_cstr

.set_path:
    push rax                    ; pkg dir str

    ; __path__ = [pkg_dir]
    xor edi, edi
    call list_new
    mov r8, rax                 ; list
    push r8
    mov rdi, r8
    mov rsi, [rsp + 8]         ; pkg dir str
    call list_append
    ; DECREF pkg dir str
    mov rdi, [rsp + 8]
    call obj_decref

    ; Set __path__ in dict
    lea rdi, [rel im_dunder_path]
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]         ; list
    call dict_set
    pop rdi                     ; key
    call obj_decref
    pop rdi                     ; list
    call obj_decref
    add rsp, 8                  ; pop saved pkg dir str (already decref'd)

    jmp .create_module

.set_parent_package:
    ; Non-package: __package__ = parent name (before last '.')
    lea rdi, [rbx + PyStrObject.data]
    mov rcx, [rbx + PyStrObject.ob_size]
    ; Find last dot
    mov rdx, rcx
.find_last_dot:
    dec rdx
    js .no_parent_pkg
    cmp byte [rdi + rdx], '.'
    jne .find_last_dot

    ; Parent package = name[0..rdx]
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, rdx
    call str_new
    jmp .store_package

.no_parent_pkg:
    ; Top-level module: __package__ = ""
    lea rdi, [rel im_empty]
    call str_from_cstr

.store_package:
    push rax
    lea rdi, [rel im_dunder_package]
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

.create_module:
    ; Create module object
    mov rdi, rbx                ; name
    mov rsi, r15                ; dict
    call module_new
    mov r13, rax                ; r13 = module object (reuse r13, is_package no longer needed)

    ; Register in sys.modules BEFORE execution (circular import safety)
    mov rdi, [rel sys_modules_dict]
    mov rsi, rbx                ; name_str
    mov rdx, r13
    call dict_set

    ; Set __builtins__ in module dict
    lea rdi, [rel im_dunder_builtins]
    call str_from_cstr
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rel builtins_dict_global]
    call dict_set
    pop rdi
    call obj_decref

    ; Execute module code
    ; frame_new(code, globals=mod_dict, builtins, locals=mod_dict)
    mov rdi, r14                ; code
    mov rsi, r15                ; globals = module dict
    mov rdx, [rel builtins_dict_global]  ; builtins
    mov rcx, r15                ; locals = module dict
    call frame_new
    mov r12, rax                ; r12 = frame

    mov rdi, r12
    call eval_frame
    ; rax = return value (ignore)
    ; XDECREF return value
    test rax, rax
    jz .no_retval
    js .no_retval               ; SmallInt, skip
    mov rdi, rax
    call obj_decref
.no_retval:

    ; Free frame
    mov rdi, r12
    call frame_free

    ; DECREF code object
    mov rdi, r14
    call obj_decref

    ; Return module (already in sys.modules with INCREF from dict_set)
    mov rax, r13

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.load_failed:
    ; Restore marshal globals even on failure
    mov rax, [rbp - IF_MBUF]
    mov [rel marshal_buf], rax
    mov rax, [rbp - IF_MPOS]
    mov [rel marshal_pos], rax
    mov rax, [rbp - IF_MLEN]
    mov [rel marshal_len], rax
    mov rax, [rbp - IF_MREFS]
    mov [rel marshal_refs], rax
    mov rax, [rbp - IF_MRCNT]
    mov [rel marshal_ref_count], rax
    mov rax, [rbp - IF_MRCAP]
    mov [rel marshal_ref_cap], rax

    xor eax, eax
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_load_module

; ============================================================================
; Data
; ============================================================================
section .rodata

im_builtins:        db "builtins", 0
im_dunder_name:     db "__name__", 0
im_dunder_file:     db "__file__", 0
im_dunder_loader:   db "__loader__", 0
im_dunder_spec:     db "__spec__", 0
im_dunder_package:  db "__package__", 0
im_dunder_path:     db "__path__", 0
im_dunder_builtins: db "__builtins__", 0
im_empty:           db "", 0
im_dot:             db ".", 0

; Path suffixes for module search
im_pkg_pyc_suffix:     db "/__pycache__/__init__.cpython-312.pyc", 0
im_pkg_pyc_suffix_len  equ $ - im_pkg_pyc_suffix - 1

im_pycache_prefix:     db "__pycache__/", 0
im_pycache_prefix_len  equ $ - im_pycache_prefix - 1

im_pyc_suffix:         db ".cpython-312.pyc", 0
im_pyc_suffix_len      equ $ - im_pyc_suffix - 1

section .bss
import_path_buf_ptr: resq 1    ; malloc'd path buffer (lazy-allocated)
