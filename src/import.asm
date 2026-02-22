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
extern str_from_cstr_heap
extern str_new_heap
extern smallstr_to_obj
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

; Builtin modules
extern time_module_create
extern asyncio_module_create
extern sre_module_create

; --- import_module frame layout ---
IF_NAME     equ 8            ; import name str
IF_FROMLIST equ 16           ; fromlist
IF_LEVEL    equ 24           ; level (int)
IF_TOPMOD   equ 32           ; top-level module
IF_MBUF     equ 40           ; saved marshal_buf
IF_MPOS     equ 48           ; saved marshal_pos
IF_MLEN     equ 56           ; saved marshal_len
IF_MREFS    equ 64           ; saved marshal_refs
IF_MRCNT    equ 72           ; saved marshal_ref_count
IF_MRCAP    equ 80           ; saved marshal_ref_cap
IF_FRAME    equ 88

; --- import_find_and_load frame layout ---
; path_component buffer lives on stack below frame locals
FL_NAME     equ 8            ; name_str (PyObject*)
FL_LEAF     equ 16           ; leaf name cstr ptr
FL_LEAFLEN  equ 24           ; leaf name length
FL_PATHBUF  equ 32           ; path component buffer ptr (on stack)
FL_PATHLEN  equ 40           ; path component length
FL_FRAME    equ 48
FL_STKSZ    equ 4096         ; stack buffer for path component

; Path buffer size
PATHBUF_SIZE equ 8192

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

    ; Add "lib" to sys.path for stdlib modules
    lea rdi, [rel im_lib_path]
    call str_from_cstr_heap
    push rax
    mov rdi, [rel sys_path_list]
    mov rsi, rax
    mov edx, TAG_PTR
    call list_append
    pop rdi
    call obj_decref

    ; Add "tests/cpython" to sys.path for test support
    lea rdi, [rel im_tests_cpython_path]
    call str_from_cstr_heap
    push rax
    mov rdi, [rel sys_path_list]
    mov rsi, rax
    mov edx, TAG_PTR
    call list_append
    pop rdi
    call obj_decref

    ; Register builtins module in sys.modules
    lea rdi, [rel im_builtins]
    call str_from_cstr_heap
    push rax                    ; key

    ; Create builtins module wrapping existing builtins dict
    lea rdi, [rel im_builtins]
    call str_from_cstr_heap
    push rax                    ; save name for DECREF
    mov rdi, rax
    mov rsi, [rel builtins_dict_global]
    call module_new
    mov rbx, rax                ; builtins module
    pop rdi                     ; DECREF name (module_new INCREF'd)
    call obj_decref

    ; dict_set(sys_modules, "builtins", builtins_module)
    mov rdi, [rel sys_modules_dict]
    pop rsi                     ; key = "builtins"
    push rsi                    ; re-save key for DECREF
    mov rdx, rbx
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi                     ; DECREF key (dict_set INCREF'd)
    call obj_decref
    mov rdi, rbx                ; DECREF module (dict_set INCREF'd)
    call obj_decref

    ; Register time module in sys.modules
    call time_module_create
    mov rbx, rax                ; time module
    lea rdi, [rel im_time_name]
    call str_from_cstr_heap
    push rax                    ; save key for DECREF
    mov rdi, [rel sys_modules_dict]
    mov rsi, rax                ; key = "time"
    mov rdx, rbx                ; value = time module
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi                     ; DECREF key
    call obj_decref
    mov rdi, rbx                ; DECREF module (dict_set INCREF'd)
    call obj_decref

    ; Register asyncio module in sys.modules
    call asyncio_module_create
    mov rbx, rax                ; asyncio module
    lea rdi, [rel im_asyncio_name]
    call str_from_cstr_heap
    push rax                    ; save key for DECREF
    mov rdi, [rel sys_modules_dict]
    mov rsi, rax                ; key = "asyncio"
    mov rdx, rbx                ; value = asyncio module
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi                     ; DECREF key
    call obj_decref
    mov rdi, rbx                ; DECREF module (dict_set INCREF'd)
    call obj_decref

    ; Register _sre module in sys.modules
    call sre_module_create
    mov rbx, rax                ; _sre module
    lea rdi, [rel im_sre_name]
    call str_from_cstr_heap
    push rax                    ; save key for DECREF
    mov rdi, [rel sys_modules_dict]
    mov rsi, rax                ; key = "_sre"
    mov rdx, rbx                ; value = _sre module
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi                     ; DECREF key
    call obj_decref
    mov rdi, rbx                ; DECREF module (dict_set INCREF'd)
    call obj_decref

    ; Add system python3 lib path to sys.path for re module access
    lea rdi, [rel im_python3_lib_path]
    call str_from_cstr_heap
    push rax
    mov rdi, [rel sys_path_list]
    mov rsi, rax
    mov edx, TAG_PTR
    call list_append
    pop rdi
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
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
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
    call str_new_heap
    push rax                    ; save first component name

    ; Check sys.modules for first component
    mov rdi, [rel sys_modules_dict]
    mov rsi, rax
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
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
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jnz .have_full_dotted

    ; Need to load intermediate + full path
    mov rdi, [rbp - IF_NAME]
    call import_find_and_load
    test rax, rax
    jz .dotted_load_error

.have_full_dotted:
    mov r13, rax                ; r13 = leaf module

    ; Set submodule as attr on parent (CPython behavior)
    ; Extract leaf name from full dotted name (after last dot)
    mov rcx, r14                ; name length
.find_leaf_dot:
    dec rcx
    js .skip_set_parent
    cmp byte [rbx + rcx], '.'
    jne .find_leaf_dot
    ; leaf name starts at rbx + rcx + 1
    lea rdi, [rbx + rcx + 1]
    mov rsi, r14
    sub rsi, rcx
    dec rsi                     ; leaf name length
    call str_new_heap                ; rax = leaf name str
    push rax                    ; save leaf name str
    ; Set on parent's mod_dict
    mov rdi, [r12 + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .no_parent_dict
    mov rsi, rax                ; key = leaf name
    mov rdx, r13                ; value = leaf module
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
.no_parent_dict:
    pop rdi
    call obj_decref             ; DECREF leaf name str

.skip_set_parent:
    ; Decide what to return based on fromlist
    mov rax, [rbp - IF_FROMLIST]
    ; Check if fromlist is None (inline TAG_NONE has payload=0, pointer form has none_singleton)
    test rax, rax
    jz .return_top
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .return_top
    ; Check tuple size (fromlist is always None or tuple)
    mov rcx, [rax + PyTupleObject.ob_size]
    test rcx, rcx
    jz .return_top
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
; Find module on disk and load it.
; For dotted names (e.g. "unittest.case"), searches parent package's __path__.
; Returns module object or NULL.
; ============================================================================
DEF_FUNC import_find_and_load, FL_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - FL_NAME], rdi    ; save name_str

    ; Check sys.modules first — avoid re-loading already-imported modules
    mov rdi, [rel sys_modules_dict]
    mov rsi, [rbp - FL_NAME]
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jnz .found_in_sysmod

    ; Ensure path buffer is allocated
    mov rdi, [rel import_path_buf_ptr]
    test rdi, rdi
    jnz .have_buf
    mov edi, PATHBUF_SIZE
    call ap_malloc
    mov [rel import_path_buf_ptr], rax
.have_buf:

    ; Compute leaf name: last component after final '.'
    ; e.g. "unittest.case" -> leaf="case", "unittest" -> leaf="unittest"
    mov rbx, [rbp - FL_NAME]
    lea r14, [rbx + PyStrObject.data]  ; full name cstr
    mov r15, [rbx + PyStrObject.ob_size] ; full name length

    ; Find last '.' to get leaf name
    mov rcx, r15
.find_last_dot:
    dec rcx
    js .no_dot_found
    cmp byte [r14 + rcx], '.'
    jne .find_last_dot
    ; Found dot at position rcx; leaf starts at rcx+1
    lea rax, [r14 + rcx + 1]
    mov [rbp - FL_LEAF], rax
    mov rax, r15
    sub rax, rcx
    dec rax                     ; leaf length
    mov [rbp - FL_LEAFLEN], rax
    jmp .have_leaf

.no_dot_found:
    ; No dot: leaf = full name
    mov [rbp - FL_LEAF], r14
    mov [rbp - FL_LEAFLEN], r15

.have_leaf:
    ; For dotted names, try parent package's __path__ first
    ; Find last '.' position again to extract parent name
    mov rcx, r15
.find_parent_dot:
    dec rcx
    js .search_sys_path         ; no dot -> top-level, search sys.path
    cmp byte [r14 + rcx], '.'
    jne .find_parent_dot

    ; Dotted name: parent = name[0..rcx]
    ; Create parent name string and look up in sys.modules
    lea rdi, [r14]              ; parent name cstr
    mov rsi, rcx                ; parent name length
    call str_new_heap
    mov r12, rax                ; r12 = parent name str

    ; Look up parent in sys.modules
    mov rdi, [rel sys_modules_dict]
    mov rsi, r12
    mov edx, TAG_PTR
    call dict_get
    mov r13, rax                ; r13 = parent module payload (or 0)
    push rdx                    ; save dict_get tag for found check

    ; DECREF parent name str
    mov rdi, r12
    call obj_decref

    ; If parent not found, fall through to sys.path search
    pop rdx                     ; restore dict_get tag
    test edx, edx
    jz .search_sys_path

    ; Get parent's __path__ attribute (from module dict)
    ; Module dict is at module.ob_dict (PyModuleObject.ob_dict)
    mov rax, [r13 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_getattr]
    test rax, rax
    jz .search_sys_path

    ; getattr(parent_module, "__path__")
    ; Save tp_getattr (caller-saved regs clobbered by str_from_cstr)
    push rax                    ; save tp_getattr
    push r13                    ; save parent module
    lea rdi, [rel im_dunder_path]
    call str_from_cstr_heap
    pop r13                     ; restore parent module
    pop rcx                     ; restore tp_getattr
    push rax                    ; save "__path__" str
    mov rdi, r13                ; parent module
    mov rsi, rax                ; "__path__" key
    call rcx                    ; tp_getattr
    mov r12, rax                ; r12 = __path__ list (or NULL)
    pop rdi                     ; DECREF "__path__" str
    call obj_decref

    test r12, r12
    jz .search_sys_path

    ; r12 = parent's __path__ (a list). Search it for the leaf module.
    mov rdi, r12                ; search_list = __path__
    mov rsi, [rbp - FL_LEAF]    ; leaf cstr
    mov rdx, [rbp - FL_LEAFLEN] ; leaf len
    call import_search_dirs
    ; DECREF __path__ (getattr returned new ref)
    push rax                    ; save result
    mov rdi, r12
    call obj_decref
    pop rax

    test rax, rax
    jnz .found_result           ; found it in parent's __path__
    ; Fall through to sys.path

.search_sys_path:
    ; Build full path component: name with dots replaced by slashes
    ; Stack buffer for path component
    sub rsp, FL_STKSZ
    mov r12, rsp                ; r12 = path_component buffer

    ; Copy name, replacing '.' with '/'
    mov rbx, [rbp - FL_NAME]
    lea r14, [rbx + PyStrObject.data]
    mov r15, [rbx + PyStrObject.ob_size]
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

    ; Search sys.path
    mov rdi, [rel sys_path_list]
    mov rsi, [rbp - FL_LEAF]
    mov rdx, [rbp - FL_LEAFLEN]
    mov rcx, r12                ; full path component (with slashes)
    call import_search_syspath
    add rsp, FL_STKSZ

    test rax, rax
    jnz .found_result

    ; Not found anywhere
    xor eax, eax
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.found_in_sysmod:
    ; Already imported — return INCREF'd reference
    inc qword [rax + PyObject.ob_refcnt]
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.found_result:
    ; rax = 1 (package) or 2 (module); path is in import_path_buf_ptr
    mov r12d, eax               ; save type
    mov rdi, [rbp - FL_NAME]
    mov rsi, [rel import_path_buf_ptr]
    xor edx, edx
    cmp r12d, 1
    jne .load_as_module
    mov edx, 1                  ; is_package = 1
.load_as_module:
    call import_load_module

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_find_and_load

; ============================================================================
; import_search_dirs(PyListObject *dirs, const char *leaf, int64_t leaf_len) -> int
; Search a list of directory strings for a module named 'leaf'.
; Tries: <dir>/<leaf>/__pycache__/__init__.cpython-312.pyc (package)
;        <dir>/__pycache__/<leaf>.cpython-312.pyc (module)
;        <dir>/<leaf>.cpython-312.pyc (module, no __pycache__)
; On success, sets import_path_buf_ptr contents and returns 1 (package) or 2 (module).
; On failure returns 0.
; ============================================================================

; Frame layout for import_search_dirs
SD_DIRS     equ 8             ; dirs list
SD_LEAF     equ 16            ; leaf cstr
SD_LEAFLEN  equ 24            ; leaf length
SD_FULLPATH equ 32            ; optional full path component (with slashes)
SD_IDX      equ 40            ; current search index
SD_COUNT    equ 48            ; number of dirs
SD_SPILLED  equ 56            ; spilled SmallStr heap ptr (0 if none)
SD_FRAME    equ 64

DEF_FUNC import_search_dirs, SD_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - SD_DIRS], rdi
    mov [rbp - SD_LEAF], rsi
    mov [rbp - SD_LEAFLEN], rdx
    mov qword [rbp - SD_FULLPATH], 0

    ; Get list size and item array
    mov r14, [rdi + PyListObject.ob_size]   ; count
    mov [rbp - SD_COUNT], r14
    mov qword [rbp - SD_IDX], 0

.sd_loop:
    mov rax, [rbp - SD_IDX]
    cmp rax, [rbp - SD_COUNT]
    jge .sd_not_found

    ; Get dir string (fat list: 16-byte stride)
    mov rdi, [rbp - SD_DIRS]
    mov rcx, [rdi + PyListObject.ob_item]
    mov rdx, [rdi + PyListObject.ob_item_tags]
    mov rax, [rbp - SD_IDX]
    mov rbx, [rcx + rax * 8]      ; rbx = dir str obj payload
    movzx r8d, byte [rdx + rax]   ; dir tag
    mov qword [rbp - SD_SPILLED], 0
    cmp r8d, TAG_SMALLINT
    je .sd_next                 ; skip SmallInts
    test rbx, rbx
    jz .sd_next
    test r8, r8
    jns .sd_have_str            ; not SmallStr, use directly
    ; SmallStr: spill to heap
    mov rdi, rbx               ; payload
    mov rsi, r8                 ; tag
    call smallstr_to_obj
    mov rbx, rax               ; rbx = heap PyStrObject*
    mov [rbp - SD_SPILLED], rbx ; save for DECREF later
.sd_have_str:

    ; Build path in import_path_buf_ptr
    mov r12, [rel import_path_buf_ptr]  ; r12 = dest buf

    ; Copy dir to buffer
    lea rsi, [rbx + PyStrObject.data]
    mov r13, [rbx + PyStrObject.ob_size] ; r13 = offset (dir length)
    test r13, r13
    jz .sd_no_dir
    mov rdi, r12
    mov rdx, r13
    call ap_memcpy
.sd_no_dir:

    ; Append '/' if dir non-empty
    test r13, r13
    jz .sd_no_slash
    mov byte [r12 + r13], '/'
    inc r13
.sd_no_slash:
    ; r13 = current write offset

    ; --- Pattern 1: <dir>/<leaf>/__pycache__/__init__.cpython-312.pyc ---
    ; Append leaf name
    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SD_LEAF]
    mov rdx, [rbp - SD_LEAFLEN]
    mov r15, rdx                ; save leaf len
    call ap_memcpy
    lea r14, [r13 + r15]       ; r14 = offset after leaf

    ; Append suffix
    mov rdi, r12
    add rdi, r14
    lea rsi, [rel im_pkg_pyc_suffix]
    mov rdx, im_pkg_pyc_suffix_len
    call ap_memcpy
    add r14, im_pkg_pyc_suffix_len
    mov byte [r12 + r14], 0

    ; Try to open
    mov rdi, r12
    xor esi, esi                ; O_RDONLY
    xor edx, edx
    call sys_open
    test rax, rax
    jns .sd_found_package

    ; --- Pattern 2: <dir>/__pycache__/<leaf>.cpython-312.pyc ---
    ; Rebuild from dir offset (r13 already has dir+slash offset)
    ; Re-read r13 from dir
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .sd_p2_no_slash
    inc r13                     ; account for '/'
.sd_p2_no_slash:

    ; Append "__pycache__/"
    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_pycache_prefix]
    mov rdx, im_pycache_prefix_len
    call ap_memcpy
    add r13, im_pycache_prefix_len

    ; Append leaf name
    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SD_LEAF]
    mov rdx, [rbp - SD_LEAFLEN]
    call ap_memcpy
    add r13, [rbp - SD_LEAFLEN]

    ; Append ".cpython-312.pyc"
    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_pyc_suffix]
    mov rdx, im_pyc_suffix_len
    call ap_memcpy
    add r13, im_pyc_suffix_len
    mov byte [r12 + r13], 0

    ; Try to open
    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .sd_found_module

    ; --- Pattern 3: <dir>/<leaf>.cpython-312.pyc ---
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .sd_p3_no_slash
    inc r13
.sd_p3_no_slash:

    ; Append leaf name
    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SD_LEAF]
    mov rdx, [rbp - SD_LEAFLEN]
    call ap_memcpy
    add r13, [rbp - SD_LEAFLEN]

    ; Append ".cpython-312.pyc"
    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_pyc_suffix]
    mov rdx, im_pyc_suffix_len
    call ap_memcpy
    add r13, im_pyc_suffix_len
    mov byte [r12 + r13], 0

    ; Try to open
    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .sd_found_module

.sd_next:
    ; DECREF any spilled SmallStr from this iteration
    mov rdi, [rbp - SD_SPILLED]
    test rdi, rdi
    jz .sd_next_no_decref
    call obj_decref
.sd_next_no_decref:
    inc qword [rbp - SD_IDX]
    jmp .sd_loop

.sd_found_package:
    ; Close the test fd
    mov rdi, rax
    call sys_close
    ; DECREF any spilled SmallStr
    mov rdi, [rbp - SD_SPILLED]
    test rdi, rdi
    jz .sd_pkg_no_decref
    call obj_decref
.sd_pkg_no_decref:
    mov eax, 1                  ; return 1 = package
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sd_found_module:
    ; Close the test fd
    mov rdi, rax
    call sys_close
    ; DECREF any spilled SmallStr
    mov rdi, [rbp - SD_SPILLED]
    test rdi, rdi
    jz .sd_mod_no_decref
    call obj_decref
.sd_mod_no_decref:
    mov eax, 2                  ; return 2 = module
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sd_not_found:
    xor eax, eax               ; return 0 = not found
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_search_dirs

; ============================================================================
; import_search_syspath(PyListObject *sys_path, const char *leaf, int64_t leaf_len,
;                       const char *full_component) -> int
; Searches sys.path for a module. For dotted names, full_component has slashes.
; Tries patterns:
;   <dir>/<full_component>/__pycache__/__init__.cpython-312.pyc (package)
;   <dir>/__pycache__/<leaf>.cpython-312.pyc (module)
;   <dir>/<leaf>.cpython-312.pyc (module, no __pycache__)
; Returns 1 (package), 2 (module), or 0 (not found).
; ============================================================================

SS_DIRS     equ 8
SS_LEAF     equ 16
SS_LEAFLEN  equ 24
SS_FULL     equ 32            ; full path component (dots->slashes)
SS_IDX      equ 40
SS_COUNT    equ 48
SS_SPILLED  equ 56            ; spilled SmallStr heap ptr (0 if none)
SS_FRAME    equ 64

DEF_FUNC import_search_syspath, SS_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - SS_DIRS], rdi
    mov [rbp - SS_LEAF], rsi
    mov [rbp - SS_LEAFLEN], rdx
    mov [rbp - SS_FULL], rcx

    mov r14, [rdi + PyListObject.ob_size]
    mov [rbp - SS_COUNT], r14
    mov qword [rbp - SS_IDX], 0

.ss_loop:
    mov rax, [rbp - SS_IDX]
    cmp rax, [rbp - SS_COUNT]
    jge .ss_not_found

    ; Get dir string (fat list: 16-byte stride)
    mov rdi, [rbp - SS_DIRS]
    mov rcx, [rdi + PyListObject.ob_item]
    mov rdx, [rdi + PyListObject.ob_item_tags]
    mov rax, [rbp - SS_IDX]
    mov rbx, [rcx + rax * 8]      ; rbx = dir str obj payload
    movzx r8d, byte [rdx + rax]   ; dir tag
    mov qword [rbp - SS_SPILLED], 0
    cmp r8d, TAG_SMALLINT
    je .ss_next                 ; skip SmallInts
    test rbx, rbx
    jz .ss_next
    test r8, r8
    jns .ss_have_str            ; not SmallStr, use directly
    ; SmallStr: spill to heap
    mov rdi, rbx               ; payload
    mov rsi, r8                 ; tag
    call smallstr_to_obj
    mov rbx, rax               ; rbx = heap PyStrObject*
    mov [rbp - SS_SPILLED], rbx ; save for DECREF later
.ss_have_str:

    mov r12, [rel import_path_buf_ptr]  ; dest buf

    ; Copy dir to buffer
    lea rsi, [rbx + PyStrObject.data]
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .ss_no_dir
    mov rdi, r12
    mov rdx, r13
    call ap_memcpy
.ss_no_dir:
    test r13, r13
    jz .ss_no_slash
    mov byte [r12 + r13], '/'
    inc r13
.ss_no_slash:

    ; --- Pattern 1: <dir>/<full>/__pycache__/__init__.cpython-312.pyc ---
    ; Append full component (dots->slashes)
    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SS_FULL]
    push r13                    ; save dir offset
    mov rdi, rsi                ; strlen(full)
    call ap_strlen
    mov r15, rax                ; r15 = full component length
    pop r13

    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SS_FULL]
    mov rdx, r15
    call ap_memcpy
    lea r14, [r13 + r15]       ; offset after full component

    ; Append package suffix
    mov rdi, r12
    add rdi, r14
    lea rsi, [rel im_pkg_pyc_suffix]
    mov rdx, im_pkg_pyc_suffix_len
    call ap_memcpy
    add r14, im_pkg_pyc_suffix_len
    mov byte [r12 + r14], 0

    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .ss_found_package

    ; --- Pattern 2: <dir>/__pycache__/<leaf>.cpython-312.pyc ---
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .ss_p2_no_slash
    inc r13
.ss_p2_no_slash:

    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_pycache_prefix]
    mov rdx, im_pycache_prefix_len
    call ap_memcpy
    add r13, im_pycache_prefix_len

    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SS_LEAF]
    mov rdx, [rbp - SS_LEAFLEN]
    call ap_memcpy
    add r13, [rbp - SS_LEAFLEN]

    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_pyc_suffix]
    mov rdx, im_pyc_suffix_len
    call ap_memcpy
    add r13, im_pyc_suffix_len
    mov byte [r12 + r13], 0

    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .ss_found_module

    ; --- Pattern 3: <dir>/<leaf>.cpython-312.pyc ---
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .ss_p3_no_slash
    inc r13
.ss_p3_no_slash:

    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SS_LEAF]
    mov rdx, [rbp - SS_LEAFLEN]
    call ap_memcpy
    add r13, [rbp - SS_LEAFLEN]

    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_pyc_suffix]
    mov rdx, im_pyc_suffix_len
    call ap_memcpy
    add r13, im_pyc_suffix_len
    mov byte [r12 + r13], 0

    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .ss_found_module

.ss_next:
    ; DECREF any spilled SmallStr from this iteration
    mov rdi, [rbp - SS_SPILLED]
    test rdi, rdi
    jz .ss_next_no_decref
    call obj_decref
.ss_next_no_decref:
    inc qword [rbp - SS_IDX]
    jmp .ss_loop

.ss_found_package:
    mov rdi, rax
    call sys_close
    ; DECREF any spilled SmallStr
    mov rdi, [rbp - SS_SPILLED]
    test rdi, rdi
    jz .ss_pkg_no_decref
    call obj_decref
.ss_pkg_no_decref:
    mov eax, 1
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.ss_found_module:
    mov rdi, rax
    call sys_close
    ; DECREF any spilled SmallStr
    mov rdi, [rbp - SS_SPILLED]
    test rdi, rdi
    jz .ss_mod_no_decref
    call obj_decref
.ss_mod_no_decref:
    mov eax, 2
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.ss_not_found:
    xor eax, eax
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_search_syspath

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

    ; Reset marshal refs so inner import allocates fresh arrays
    mov qword [rel marshal_refs], 0
    mov qword [rel marshal_ref_count], 0
    mov qword [rel marshal_ref_cap], 0

    ; Read .pyc file -> code object
    mov rdi, r12
    call pyc_read_file
    test rax, rax
    jz .load_failed
    mov r14, rax                ; r14 = code object

    ; Free inner import's marshal refs array (if allocated)
    mov rdi, [rel marshal_refs]
    test rdi, rdi
    jz .skip_inner_refs_free
    call ap_free
.skip_inner_refs_free:

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
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx                ; name_str
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; Set __file__
    mov rdi, r12                ; path cstr
    call str_from_cstr_heap
    push rax                    ; file str
    lea rdi, [rel im_dunder_file]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; Set __loader__ = None
    lea rdi, [rel im_dunder_loader]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    lea rdx, [rel none_singleton]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; Set __spec__ = None
    lea rdi, [rel im_dunder_spec]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    lea rdx, [rel none_singleton]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; Set __package__
    test r13d, r13d
    jz .set_parent_package

    ; Package: __package__ = name, __path__ = [dir]
    lea rdi, [rel im_dunder_package]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, rbx                ; name_str
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
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
    call str_new_heap
    jmp .set_path

.use_dot_path:
    lea rdi, [rel im_dot]
    call str_from_cstr_heap

.set_path:
    push rax                    ; pkg dir str

    ; __path__ = [pkg_dir]
    xor edi, edi
    call list_new
    mov r8, rax                 ; list
    push r8
    mov rdi, r8
    mov rsi, [rsp + 8]         ; pkg dir str
    mov edx, TAG_PTR
    call list_append
    ; DECREF pkg dir str
    mov rdi, [rsp + 8]
    call obj_decref

    ; Set __path__ in dict
    lea rdi, [rel im_dunder_path]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]         ; list
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
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
    call str_new_heap
    jmp .store_package

.no_parent_pkg:
    ; Top-level module: __package__ = ""
    lea rdi, [rel im_empty]
    call str_from_cstr_heap

.store_package:
    push rax
    lea rdi, [rel im_dunder_package]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
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
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set

    ; Set __builtins__ in module dict
    lea rdi, [rel im_dunder_builtins]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    mov rdx, [rel builtins_dict_global]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
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
    ; rax = return value (ignore), edx = tag
    ; XDECREF return value (tag-aware)
    mov rdi, rax
    mov rsi, rdx
    DECREF_VAL rdi, rsi
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
    ; Free inner import's marshal refs array (if allocated)
    mov rdi, [rel marshal_refs]
    test rdi, rdi
    jz .skip_fail_refs_free
    call ap_free
.skip_fail_refs_free:

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

im_lib_path:        db "lib", 0
im_tests_cpython_path: db "tests/cpython", 0
im_time_name:       db "time", 0
im_asyncio_name:    db "asyncio", 0
im_sre_name:        db "_sre", 0
im_python3_lib_path: db "/usr/lib/python3.12", 0
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
