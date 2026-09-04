; import.asm - Import machinery for apython
; Handles IMPORT_NAME / IMPORT_FROM opcodes and module loading

%include "macros.inc"
%include "object.inc"

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
extern str_type
extern none_singleton
extern dict_new
extern dict_get
extern dict_set
extern dict_del
extern current_exception
extern eval_exception_unwind
extern list_new
extern list_append
extern type_type
extern module_new
extern fatal_error
extern raise_exception
extern eval_frame
extern frame_new
extern frame_free
extern code_from_path
extern path_is_source
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
extern sys_module_init

; builtins
extern builtins_dict_global
extern exc_ImportError_type
extern exc_ModuleNotFoundError_type

; Builtin modules

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
IF_EXC      equ 88           ; current_exception on entry (see .import_error)
IF_POS      equ 96           ; byte position while walking a dotted name
IF_PARENT   equ 104          ; the module the next component hangs off
IF_LEAF     equ 112          ; the module the walk has reached
IF_FRAME    equ 128         ; + 5 pushes = 168, not 16-aligned

; --- import_find_and_load frame layout ---
; path_component buffer lives on stack below frame locals
FL_NAME     equ 8            ; name_str (PyObject*)
FL_LEAF     equ 16           ; leaf name cstr ptr
FL_LEAFLEN  equ 24           ; leaf name length
FL_FRAME    equ 48          ; + 5 pushes = 88, not 16-aligned
FL_STKSZ    equ 4096         ; stack buffer for path component

; Path buffer size
PATHBUF_SIZE equ 8192
; Room for the longest suffix this file appends after the directory and the
; module component: "/__pycache__/__init__.cpython-312.pyc" is 37 bytes.
IM_PATH_MARGIN equ 64

;; ============================================================================
;; import_init(int argc, char **argv)
;; Initialize the import system: sys module + builtins in sys.modules

;; ============================================================================
;; import_add_exe_relative_path(rdi = suffix cstr)
;; Appends <directory of the running binary>/<suffix> to sys.path.  Falls back
;; to the plain relative entry when /proc/self/exe cannot be read.

;; ============================================================================
;; import_resolve_relative(rdi = name str, rsi = globals dict, rdx = level)
;;   -> rax = the absolute name, a new reference
;;
;; `from . import x` inside a package: level counts how far up from the
;; importing module's package to start.  Level was read off the stack and then
;; ignored, so every relative import in the stdlib looked like an import of the
;; empty name.
;; ============================================================================
IRR_NAME  equ 8
IRR_LEVEL equ 16
IRR_PKG   equ 24
IRR_LEN   equ 32
IRR_BUF   equ 1064          ; 1024 bytes, [rbp-1064, rbp-40)
global import_resolve_relative
IRR_FRAME equ 1072          ; + 2 pushes = 1088
DEF_FUNC import_resolve_relative, IRR_FRAME
    push rbx
    push r12
    mov [rbp - IRR_NAME], rdi
    mov [rbp - IRR_LEVEL], rdx

    ; The importing module's package.  __package__ is set at import time; for
    ; a package itself it is the package's own name.
    test rsi, rsi
    jz .irr_no_package
    push rsi
    lea rdi, [rel irr_package_key]
    call str_from_cstr_heap
    mov rbx, rax
    pop rdi
    mov rsi, rbx
    call dict_get
    push rax
    mov rdi, rbx
    call obj_decref
    pop rax
    test rax, rax
    jz .irr_no_package
    V_TEST_PTR rax, rcx
    ja .irr_no_package
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .irr_no_package
    mov [rbp - IRR_PKG], rax
    mov rcx, [rax + PyStrObject.ob_size]
    test rcx, rcx
    jz .irr_no_package

    ; Copy the package name, then drop one trailing component per extra level.
    lea rbx, [rbp - IRR_BUF]
    cmp rcx, IRR_BUF - 64
    jae .irr_no_package
    mov [rbp - IRR_LEN], rcx
    mov rdi, rbx
    mov rax, [rbp - IRR_PKG]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, rcx
    call ap_memcpy

    mov r12, [rbp - IRR_LEVEL]
    dec r12
.irr_strip:
    test r12, r12
    jle .irr_have_base
    mov rcx, [rbp - IRR_LEN]
.irr_find_dot:
    test rcx, rcx
    jz .irr_beyond_top
    dec rcx
    cmp byte [rbx + rcx], '.'
    jne .irr_find_dot
    mov [rbp - IRR_LEN], rcx
    dec r12
    jmp .irr_strip

.irr_have_base:
    ; Append ".name" when there is a name; `from . import x` has none.
    mov rax, [rbp - IRR_NAME]
    mov rdx, [rax + PyStrObject.ob_size]
    test rdx, rdx
    jz .irr_build
    mov rcx, [rbp - IRR_LEN]
    mov byte [rbx + rcx], '.'
    inc rcx
    mov [rbp - IRR_LEN], rcx
    lea rdi, [rbx + rcx]
    lea rsi, [rax + PyStrObject.data]
    push rdx
    call ap_memcpy
    pop rdx
    add [rbp - IRR_LEN], rdx

.irr_build:
    mov rdi, rbx
    mov rsi, [rbp - IRR_LEN]
    call str_new_heap
    pop r12
    pop rbx
    leave
    ret

.irr_beyond_top:
    RAISE exc_ImportError_type, "attempted relative import beyond top-level package"
.irr_no_package:
    RAISE exc_ImportError_type, "attempted relative import with no known parent package"
END_FUNC import_resolve_relative

;; ============================================================================
IAR_SUFFIX equ 8
IAR_BUF    equ 4128            ; 4096 bytes of path, [rbp-4128, rbp-32)
IAR_FRAME  equ 4144         ; + 2 pushes = 4160
DEF_FUNC_LOCAL import_add_exe_relative_path, IAR_FRAME
    push rbx
    push r12
    mov [rbp - IAR_SUFFIX], rdi

    lea rbx, [rbp - IAR_BUF]
    CSTRING rdi, "/proc/self/exe"
    mov rsi, rbx
    mov edx, 4000
    extern readlink
    call readlink
    test rax, rax
    jle .iar_relative
    mov r12, rax                ; length of the resolved path

    ; Cut back to the last '/', keeping it, so "…/apython" becomes "…/".
.iar_trim:
    test r12, r12
    jz .iar_relative
    dec r12
    cmp byte [rbx + r12], '/'
    jne .iar_trim
    inc r12

    ; Append the suffix.
    mov rsi, [rbp - IAR_SUFFIX]
.iar_copy:
    movzx eax, byte [rsi]
    test al, al
    jz .iar_done
    cmp r12, 4090
    jae .iar_relative
    mov [rbx + r12], al
    inc r12
    inc rsi
    jmp .iar_copy
.iar_done:
    mov rdi, rbx
    mov rsi, r12
    call str_new_heap
    jmp .iar_append

.iar_relative:
    mov rdi, [rbp - IAR_SUFFIX]
    call str_from_cstr_heap

.iar_append:
    push rax
    mov rdi, [rel sys_path_list]
    mov rsi, rax
    call list_append
    pop rdi
    call obj_decref
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_add_exe_relative_path

;; ============================================================================
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
    call dict_set
    pop rdi                     ; DECREF key (dict_set INCREF'd)
    call obj_decref
    mov rdi, rbx                ; DECREF module (dict_set INCREF'd)
    call obj_decref

    ; Every remaining builtin module, from the one table
    ; sys.builtin_module_names is also built from.
    extern builtin_module_table
    xor r12d, r12d                          ; r12 = the row index
.ii_mod_loop:
    extern builtin_module_count
    cmp r12, [rel builtin_module_count]
    jge .ii_mods_done
    lea rax, [rel builtin_module_table]
    mov rcx, r12
    shl rcx, 4                              ; BuiltinModule_size
    mov rbx, [rax + rcx + BuiltinModule.create_fn]
    test rbx, rbx
    jz .ii_mod_next                         ; wired above, or by sys_module_init
    mov rdi, [rax + rcx + BuiltinModule.name]
    push r12                                ; r12 is the loop index and also
    push rdi                                ; the frame register; save it here
    call rbx
    mov rbx, rax                            ; the module, owned
    pop rdi
    call str_from_cstr_heap
    push rax                                ; the key
    push rax                                ; pushed twice to keep rsp aligned
    mov rdi, [rel sys_modules_dict]
    mov rsi, rax
    mov rdx, rbx
    call dict_set
    pop rdi
    pop rdi
    call obj_decref                         ; the key; dict_set took its own
    mov rdi, rbx
    call obj_decref                         ; the module, likewise
    pop r12
.ii_mod_next:
    inc r12
    jmp .ii_mod_loop
.ii_mods_done:

    ; PYTHONPATH, colon-separated, appended in order.
    ;
    ; CPython's own /usr/lib/python3.12 used to be appended here
    ; unconditionally, nominally "for re module access" -- which never
    ; worked, because apython cannot run that stdlib.  What it did do, once
    ; a failing module body stopped being swallowed, was turn "module not
    ; available" into "module half-imports and then raises from inside
    ; types.py".  A stdlib on the path has to be the caller's decision.
    CSTRING rdi, "PYTHONPATH"
    extern getenv
    call getenv
    test rax, rax
    jz .no_pythonpath
    mov r12, rax                ; cursor over the value
.pp_entry:
    cmp byte [r12], 0
    je .no_pythonpath
    ; measure up to ':' or NUL
    xor ecx, ecx
.pp_scan:
    movzx eax, byte [r12 + rcx]
    test al, al
    jz .pp_have_len
    cmp al, ':'
    je .pp_have_len
    inc rcx
    jmp .pp_scan
.pp_have_len:
    test rcx, rcx
    jz .pp_skip_sep             ; empty entry
    mov rdi, r12
    mov rsi, rcx
    push rcx
    call str_new_heap
    pop rcx
    push rax
    push rcx
    mov rdi, [rel sys_path_list]
    mov rsi, rax
    call list_append
    pop rcx
    pop rdi
    call obj_decref
.pp_skip_sep:
    add r12, rcx
    cmp byte [r12], 0
    je .no_pythonpath
    inc r12                     ; step over the ':'
    jmp .pp_entry
.no_pythonpath:

    ; The modules apython ships itself, last.  They are found relative to the
    ; interpreter binary rather than the working directory -- a relative "lib"
    ; entry only resolved when apython happened to be run from its own source
    ; tree, so `import itertools` worked there and nowhere else.  Last, so a
    ; real stdlib named by PYTHONPATH wins: these stand in for CPython's C
    ; modules, not for its Python ones.
    lea rdi, [rel im_lib_path]
    call import_add_exe_relative_path
    lea rdi, [rel im_tests_cpython_path]
    call import_add_exe_relative_path

    pop r12
    pop rbx
    leave
    ret
END_FUNC import_init

;; ============================================================================
;; import_raise_not_found(rdi = module name C string) -- does not return
;; Raises ModuleNotFoundError("No module named 'x'"), matching CPython.
;; ============================================================================
IRNF_BUF equ 256
DEF_FUNC_LOCAL import_raise_not_found, IRNF_BUF
    mov rsi, rdi                ; the name
    lea rdi, [rbp - IRNF_BUF]
    xor ecx, ecx
    lea rdx, [rel im_no_module_prefix]
.irnf_prefix:
    movzx eax, byte [rdx]
    test al, al
    jz .irnf_name
    inc rdx
    mov [rdi + rcx], al
    inc rcx
    jmp .irnf_prefix
.irnf_name:
    movzx eax, byte [rsi]
    test al, al
    jz .irnf_close
    inc rsi
    cmp rcx, IRNF_BUF - 4
    jae .irnf_close
    mov [rdi + rcx], al
    inc rcx
    jmp .irnf_name
.irnf_close:
    mov byte [rdi + rcx], 0x27      ; closing quote
    inc rcx
    mov byte [rdi + rcx], 0
    mov rsi, rdi
    lea rdi, [rel exc_ModuleNotFoundError_type]
    call raise_exception
    ud2
END_FUNC import_raise_not_found

;; ============================================================================
;; import_module(PyObject *name_str, PyObject *fromlist, int64_t level) -> PyObject*
;; Main import entry point
;; name_str = module name from co_names
;; fromlist = tuple of names or None
;; level = 0 for absolute, >0 for relative
;; Returns: module object (new reference)
;; ============================================================================
DEF_FUNC import_module, IF_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - IF_NAME], rdi        ; name_str
    mov [rbp - IF_FROMLIST], rsi    ; fromlist
    mov [rbp - IF_LEVEL], rdx       ; level
    DUNDER_EXC_SAVE [rbp - IF_EXC]  ; see .import_error

    ; Get name as C string for comparisons
    mov rdi, [rbp - IF_NAME]
    lea rbx, [rdi + PyStrObject.data]  ; rbx = name cstr
    mov r14, [rdi + PyStrObject.ob_size] ; r14 = name length

    ; Check sys.modules first
    mov rdi, [rel sys_modules_dict]
    mov rsi, [rbp - IF_NAME]
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
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
    ; Dotted name: import each prefix in turn, `a`, then `a.b`, then `a.b.c`,
    ; binding each as an attribute of the one before.  Importing only the
    ; first component and then the whole name skipped the intermediate
    ; packages entirely, so anything three deep -- `import a.b.c` -- reported
    ; ModuleNotFoundError, and the submodule was hung off the *top* package
    ; rather than its own parent.
    mov qword [rbp - IF_POS], 0
    mov qword [rbp - IF_PARENT], 0
    mov qword [rbp - IF_LEAF], 0
    mov qword [rbp - IF_TOPMOD], 0

.walk_component:
    ; The prefix ends at the next dot, or at the end of the name.
    mov rcx, [rbp - IF_POS]
.walk_scan:
    cmp rcx, r14
    jae .walk_have_end
    cmp byte [rbx + rcx], '.'
    je .walk_have_end
    inc rcx
    jmp .walk_scan
.walk_have_end:
    push rcx                    ; where this component ends
    mov rdi, rbx
    mov rsi, rcx
    call str_new_heap           ; the prefix, "a" then "a.b" then "a.b.c"
    test rax, rax
    jz .walk_oom
    push rax

    mov rdi, [rel sys_modules_dict]
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .walk_have_module
    mov rdi, [rsp]
    call import_find_and_load
    test rax, rax
    jz .walk_load_failed

.walk_have_module:
    mov r13, rax                ; this prefix's module, borrowed
    pop rdi                     ; the prefix string
    call obj_decref
    pop rcx                     ; where the component ended

    ; Bind it on its parent under the bare component name.
    mov rax, [rbp - IF_PARENT]
    test rax, rax
    jz .walk_no_parent
    push rcx
    push r13
    mov rdi, [rbp - IF_POS]
    lea rdi, [rbx + rdi]        ; the bare component
    mov rsi, rcx
    sub rsi, [rbp - IF_POS]
    call str_new_heap
    test rax, rax
    jz .walk_no_parent_pop
    push rax
    mov rax, [rbp - IF_PARENT]
    mov rdi, [rax + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .walk_drop_key
    mov rsi, [rsp]
    mov rdx, [rsp + 8]          ; this prefix's module
    call dict_set
.walk_drop_key:
    pop rdi
    call obj_decref
.walk_no_parent_pop:
    pop r13
    pop rcx
.walk_no_parent:

    mov [rbp - IF_PARENT], r13
    mov [rbp - IF_LEAF], r13
    cmp qword [rbp - IF_TOPMOD], 0
    jne .walk_have_top
    mov [rbp - IF_TOPMOD], r13
.walk_have_top:

    cmp rcx, r14
    jae .walk_done
    lea rcx, [rcx + 1]          ; step past the dot
    mov [rbp - IF_POS], rcx
    jmp .walk_component

.walk_done:
    mov r12, [rbp - IF_TOPMOD]
    inc qword [r12 + PyObject.ob_refcnt]
    mov r13, [rbp - IF_LEAF]

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

.walk_load_failed:
    pop rdi                     ; the prefix string
    call obj_decref
    pop rcx                     ; where the component ended
    jmp .import_error

.walk_oom:
    pop rcx
    jmp .import_error

.found_cached:
    ; Found in sys.modules.  With an empty fromlist a dotted import still
    ; evaluates to the *top* package -- `import a.b` binds `a` -- so a cache
    ; hit on the full name has to hand back the first component instead, or
    ; the IMPORT_FROM walk that follows is applied to the wrong module.
    mov rdx, [rbp - IF_FROMLIST]
    test rdx, rdx
    jz .cached_want_top
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .cached_want_top
    cmp qword [rdx + PyTupleObject.ob_size], 0
    jne .cached_as_is
.cached_want_top:
    ; Only when the name is dotted; otherwise it already is the top.
    xor ecx, ecx
.cached_find_dot:
    cmp rcx, r14
    jae .cached_as_is
    cmp byte [rbx + rcx], '.'
    je .cached_dotted
    inc rcx
    jmp .cached_find_dot
.cached_dotted:
    push rax
    mov rdi, rbx
    mov rsi, rcx
    call str_new_heap
    test rax, rax
    jz .cached_pop_as_is
    push rax
    mov rdi, [rel sys_modules_dict]
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx
    mov rcx, rax
    pop rdi
    push rcx
    call obj_decref             ; the prefix string
    pop rcx
    pop rax
    test rcx, rcx
    jz .cached_as_is            ; not cached after all; the full one will do
    mov rax, rcx
    jmp .cached_as_is
.cached_pop_as_is:
    pop rax
.cached_as_is:
    inc qword [rax + PyObject.ob_refcnt]
    jmp .done

.import_error:
    ; If the module was found and its body raised, that exception is the
    ; error; raising ImportError over it would replace the real cause with
    ; a generic "no module named X".
    ;
    ; Compared against the value saved on entry, not against 0:
    ; current_exception is also the exception *being handled*, so inside
    ; `except ImportError:` -- which is how the whole stdlib probes for its
    ; optional C accelerators -- a bare test sees the handled exception and
    ; re-propagates it in place of the real one.
    DUNDER_RAISED [rbp - IF_EXC], .propagate_pending

    ; ModuleNotFoundError, not a bare ImportError: it is an ImportError
    ; subclass and stdlib code catches it specifically.  CPython's wording
    ; is "No module named 'x'".
    mov rdi, rbx                ; module name cstr
    call import_raise_not_found
    ; does not return

.propagate_pending:
    jmp eval_exception_unwind

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_module

;; ============================================================================
;; import_find_and_load(PyObject *name_str) -> PyObject*
;; Find module on disk and load it.
;; For dotted names (e.g. "unittest.case"), searches parent package's __path__.
;; Returns module object or NULL.
;; ============================================================================
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
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
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
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
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

;; ============================================================================
;; import_source_path(rdi = the path a search matched) -> rax = the path to
;;   record as __file__, which is rdi itself unless it is a __pycache__ entry
;;
;; "<dir>/__pycache__/<name>.cpython-312.pyc" becomes "<dir>/<name>.py".  The
;; other two cache shapes the search tries -- a bare "<name>.cpython-312.pyc"
;; beside the source, and a package's __init__ -- are handled by the same
;; rewrite, since both keep the name in the last component.  A .pyc that is
;; NOT in a __pycache__ directory is CPython's sourceless form, whose
;; __file__ is the .pyc, so it is left alone.
;;
;; The answer is written into a buffer of its own: the caller's path is the
;; one the code object was loaded from and is still needed.
;; ============================================================================
global import_source_path
DEF_FUNC import_source_path
    push rbx
    push r12
    push r13
    push r14                    ; four pushes keep rsp 16-aligned at the call
    mov rbx, rdi

    ; It has to end in ".cpython-312.pyc".
    mov rdi, rbx
    call ap_strlen
    mov r12, rax                ; the length
    cmp r12, 16 + 13            ; the suffix, plus the shortest marker
    jb .isp_keep
    ; ap_memcmp with an explicit length, not ap_strcmp: that one compares
    ; eight bytes at a time and reads past the terminator, and the path here
    ; sits in a heap buffer whose tail was never written.  Valgrind says so.
    lea rdi, [rbx + r12 - 16]
    lea rsi, [rel isp_suffix]
    mov edx, 16
    call ap_memcmp
    test eax, eax
    jnz .isp_keep

    ; ...and contain "/__pycache__/" somewhere before the last component.
    xor r13, r13                ; the index of the marker, once found
    mov r14, -1
.isp_scan:
    lea rax, [r13 + 13]
    cmp rax, r12
    ja .isp_scanned
    push r13
    lea rdi, [rbx + r13]
    lea rsi, [rel isp_marker]
    mov edx, 13
    call ap_memcmp
    pop r13
    test eax, eax
    jnz .isp_scan_next
    mov r14, r13
.isp_scan_next:
    inc r13
    jmp .isp_scan
.isp_scanned:
    cmp r14, -1
    je .isp_keep

    ; <dir> is everything before the marker; <name> is between the marker's
    ; trailing slash and the suffix.
    lea rax, [r14 + 13]         ; the first byte of <name>
    lea rcx, [r12 - 16]         ; one past its last
    cmp rcx, rax
    jbe .isp_keep
    sub rcx, rax                ; the name's length
    lea rdx, [r14 + rcx + 4]    ; <dir> + '/' + <name> + ".py" + NUL
    cmp rdx, ISP_BUFSZ
    jae .isp_keep

    lea rdi, [rel isp_buf]
    mov rsi, rbx
    mov rdx, r14
    push rcx
    push rax
    call ap_memcpy              ; <dir>, without the trailing slash
    pop rax
    pop rcx
    lea rdi, [rel isp_buf]
    add rdi, r14
    mov byte [rdi], '/'
    inc rdi
    lea rsi, [rbx + rax]
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    lea rdi, [rel isp_buf]
    add rdi, r14
    add rdi, rcx
    inc rdi
    mov dword [rdi], `.py\0`
    lea rax, [rel isp_buf]
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.isp_keep:
    mov rax, rbx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_source_path

;; ============================================================================
;; import_search_dirs(PyListObject *dirs, const char *leaf, int64_t leaf_len) -> int
;; Search a list of directory strings for a module named 'leaf'.
;; Tries: <dir>/<leaf>/__pycache__/__init__.cpython-312.pyc (package)
;;        <dir>/__pycache__/<leaf>.cpython-312.pyc (module)
;;        <dir>/<leaf>.cpython-312.pyc (module, no __pycache__)
;; On success, sets import_path_buf_ptr contents and returns 1 (package) or 2 (module).
;; On failure returns 0.
;; ============================================================================

; Frame layout for import_search_dirs
SD_DIRS     equ 8             ; dirs list
SD_LEAF     equ 16            ; leaf cstr
SD_LEAFLEN  equ 24            ; leaf length
SD_FULLPATH equ 32            ; optional full path component (with slashes)
SD_IDX      equ 40            ; current search index
SD_COUNT    equ 48            ; number of dirs
SD_FRAME    equ 56          ; + 5 pushes = 96

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
    mov rax, [rbp - SD_IDX]
    mov rbx, [rcx + rax * 8]      ; rbx = dir str obj payload
    V_UNPACK rbx, r8
    cmp r8d, TAG_SMALLINT
    je .sd_next                 ; skip SmallInts
    test rbx, rbx
    jz .sd_next

    ; Build path in import_path_buf_ptr
    mov r12, [rel import_path_buf_ptr]  ; r12 = dest buf

    ; Bound the assembled path before writing any of it.  Nothing checked
    ; it: a sys.path entry longer than the buffer memcpy'd straight past the
    ; end of the 8192-byte heap block, so sys.path.insert(0, "A"*9000)
    ; followed by any import corrupted the heap.  Too long simply means the
    ; module is not findable here.
    lea rsi, [rbx + PyStrObject.data]
    mov r13, [rbx + PyStrObject.ob_size] ; r13 = offset (dir length)
    mov rax, r13
    add rax, [rbp - SD_LEAFLEN]
    add rax, IM_PATH_MARGIN
    cmp rax, PATHBUF_SIZE
    jae .sd_next

    ; Copy dir to buffer
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

    ; --- Pattern 4: <dir>/<leaf>/__init__.py (a package, from source) ---
    ; The source patterns come last, so a .pyc that is already there still
    ; wins and nothing an existing user has changes behaviour.
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .sd_p4_no_slash
    inc r13
.sd_p4_no_slash:
    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SD_LEAF]
    mov rdx, [rbp - SD_LEAFLEN]
    call ap_memcpy
    add r13, [rbp - SD_LEAFLEN]

    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_pkg_py_suffix]
    mov rdx, im_pkg_py_suffix_len
    call ap_memcpy
    add r13, im_pkg_py_suffix_len
    mov byte [r12 + r13], 0

    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .sd_found_package

    ; --- Pattern 5: <dir>/<leaf>.py (a module, from source) ---
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .sd_p5_no_slash
    inc r13
.sd_p5_no_slash:
    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SD_LEAF]
    mov rdx, [rbp - SD_LEAFLEN]
    call ap_memcpy
    add r13, [rbp - SD_LEAFLEN]

    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_py_suffix]
    mov rdx, im_py_suffix_len
    call ap_memcpy
    add r13, im_py_suffix_len
    mov byte [r12 + r13], 0

    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .sd_found_module

.sd_next:
    inc qword [rbp - SD_IDX]
    jmp .sd_loop

.sd_found_package:
    ; Close the test fd
    mov rdi, rax
    call sys_close
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

;; ============================================================================
;; import_search_syspath(PyListObject *sys_path, const char *leaf, int64_t leaf_len,
;;                       const char *full_component) -> int
;; Searches sys.path for a module. For dotted names, full_component has slashes.
;; Tries patterns:
;;   <dir>/<full_component>/__pycache__/__init__.cpython-312.pyc (package)
;;   <dir>/__pycache__/<leaf>.cpython-312.pyc (module)
;;   <dir>/<leaf>.cpython-312.pyc (module, no __pycache__)
;; Returns 1 (package), 2 (module), or 0 (not found).
;; ============================================================================

SS_DIRS     equ 8
SS_LEAF     equ 16
SS_LEAFLEN  equ 24
SS_FULL     equ 32            ; full path component (dots->slashes)
SS_IDX      equ 40
SS_COUNT    equ 48
SS_FRAME    equ 56          ; + 5 pushes = 96

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
    mov rax, [rbp - SS_IDX]
    mov rbx, [rcx + rax * 8]      ; rbx = dir str obj payload
    V_UNPACK rbx, r8
    cmp r8d, TAG_SMALLINT
    je .ss_next                 ; skip SmallInts
    test rbx, rbx
    jz .ss_next

    mov r12, [rel import_path_buf_ptr]  ; dest buf

    ; Same bound as the search loop above.
    lea rsi, [rbx + PyStrObject.data]
    mov r13, [rbx + PyStrObject.ob_size]
    mov rax, r13
    add rax, [rbp - SS_LEAFLEN]
    add rax, IM_PATH_MARGIN
    cmp rax, PATHBUF_SIZE
    jae .ss_next

    ; Copy dir to buffer
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
    ; The dotted component can be longer than the leaf checked above.
    mov rax, r13
    add rax, r15
    add rax, IM_PATH_MARGIN
    cmp rax, PATHBUF_SIZE
    jae .ss_next

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

    ; --- Pattern 4: <dir>/<full>/__init__.py (a package, from source) ---
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .ss_p4_no_slash
    inc r13
.ss_p4_no_slash:
    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SS_FULL]
    mov rdx, r15                ; the dotted component's length, from pattern 1
    call ap_memcpy
    add r13, r15

    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_pkg_py_suffix]
    mov rdx, im_pkg_py_suffix_len
    call ap_memcpy
    add r13, im_pkg_py_suffix_len
    mov byte [r12 + r13], 0

    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .ss_found_package

    ; --- Pattern 5: <dir>/<leaf>.py (a module, from source) ---
    mov r13, [rbx + PyStrObject.ob_size]
    test r13, r13
    jz .ss_p5_no_slash
    inc r13
.ss_p5_no_slash:
    mov rdi, r12
    add rdi, r13
    mov rsi, [rbp - SS_LEAF]
    mov rdx, [rbp - SS_LEAFLEN]
    call ap_memcpy
    add r13, [rbp - SS_LEAFLEN]

    mov rdi, r12
    add rdi, r13
    lea rsi, [rel im_py_suffix]
    mov rdx, im_py_suffix_len
    call ap_memcpy
    add r13, im_py_suffix_len
    mov byte [r12 + r13], 0

    mov rdi, r12
    xor esi, esi
    xor edx, edx
    call sys_open
    test rax, rax
    jns .ss_found_module

.ss_next:
    inc qword [rbp - SS_IDX]
    jmp .ss_loop

.ss_found_package:
    mov rdi, rax
    call sys_close
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

;; ============================================================================
;; import_load_module(PyObject *name_str, const char *path_cstr, int is_package) -> PyObject*
;; Load a .pyc file and execute it as a module
;; ============================================================================
DEF_FUNC import_load_module, IF_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; name_str
    mov r12, rsi                ; path_cstr
    mov r13d, edx               ; is_package
    DUNDER_EXC_SAVE [rbp - IF_EXC]

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

    ; Read the file -> code object; a .py is compiled on the spot.
    mov rdi, r12
    call code_from_path
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
    call dict_set
    pop rdi
    call obj_decref

    ; Set __file__
    ;
    ; The SOURCE path, not the cache file the code actually came out of.
    ; CPython records the .py even when it executes a cached .pyc -- it is
    ; what a module's repr prints, what inspect.getsource opens, and what a
    ; traceback through the module names -- and this recorded whichever file
    ; the search matched, so every import through __pycache__ answered
    ; ".../__pycache__/m.cpython-312.pyc".
    mov rdi, r12                ; path cstr
    call import_source_path     ; -> the .py, or r12 unchanged
    mov rdi, rax
    call str_from_cstr_heap
    push rax                    ; file str
    lea rdi, [rel im_dunder_file]
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

    ; Set __loader__ = None
    lea rdi, [rel im_dunder_loader]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    lea rdx, [rel none_singleton]
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
    call dict_set
    pop rdi
    call obj_decref

    ; Compute package directory from path.  A cached package is
    ; ".../pkg/__pycache__/__init__.cpython-312.pyc" -- two components to
    ; strip -- while a source package is ".../pkg/__init__.py", which is one.
    ; Either way we want ".../pkg".
    mov rdi, r12
    call path_is_source
    push rax
    mov rdi, r12
    call ap_strlen
    pop r8
    mov rcx, rax
    dec rcx
.find_slash1:
    dec rcx
    js .use_dot_path
    cmp byte [r12 + rcx], '/'
    jne .find_slash1
    ; Found the slash before __init__.*; for a source package that is already
    ; the package directory.
    test r8, r8
    jnz .have_pkg_dir
    dec rcx
.find_slash2:
    dec rcx
    js .use_dot_path
    cmp byte [r12 + rcx], '/'
    jne .find_slash2
    ; Found the second slash, the one before __pycache__.
.have_pkg_dir:
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
    call str_from_cstr_heap
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
    V_UNPACK rax, rdx           ; eval_frame returns a Value
    ; A module body that raised returns a NULL Value with current_exception
    ; set.  This used to be discarded -- "rax = return value (ignore)" -- so
    ; the module was returned as if it had loaded, the exception stayed
    ; pending, and the importer's own try/except never saw it.  Every other
    ; eval_frame caller propagates; see opcodes_call.asm .propagate_exc.
    test edx, edx
    jnz .body_returned
    DUNDER_RAISED [rbp - IF_EXC], .body_raised
.body_returned:
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

    ; The module owns the dict now; release the reference dict_new gave us.
    mov rdi, r15
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

.body_raised:
    ; Undo the whole load and hand the caller a NULL with the body's
    ; exception still pending.
    mov rdi, r12
    call frame_free
    mov rdi, r14
    call obj_decref

    ; Drop the half-built module from sys.modules, as CPython's
    ; remove_module() does, so a retry re-executes the body instead of
    ; handing back an empty module.  The body may have deleted the entry
    ; itself, so look before deleting -- dict_del raises on a missing key.
    mov rdi, [rel sys_modules_dict]
    mov rsi, rbx
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jz .br_not_cached
    mov rdi, [rel sys_modules_dict]
    mov rsi, rbx
    call dict_del
.br_not_cached:

    mov rdi, r13                ; our own reference to the module
    call obj_decref
    mov rdi, r15                ; and to its dict
    call obj_decref

    xor eax, eax
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC import_load_module

;; ============================================================================
;; Data
;; ============================================================================
section .rodata

im_no_module_prefix: db "No module named '", 0
irr_package_key:    db "__package__", 0
im_lib_path:        db "lib", 0
im_tests_cpython_path: db "tests/cpython", 0
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

im_pkg_py_suffix:      db "/__init__.py", 0
im_pkg_py_suffix_len   equ $ - im_pkg_py_suffix - 1

im_py_suffix:          db ".py", 0
im_py_suffix_len       equ $ - im_py_suffix - 1

section .rodata
isp_suffix: db ".cpython-312.pyc", 0
isp_marker: db "/__pycache__/", 0

section .bss
ISP_BUFSZ equ 4096
isp_buf: resb ISP_BUFSZ

section .bss
import_path_buf_ptr: resq 1    ; malloc'd path buffer (lazy-allocated)
