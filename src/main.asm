; main.asm - Entry point for apython
; Parses argv, loads .pyc, creates frame, runs bytecode

%include "macros.inc"
%include "object.inc"
%include "compiler.inc"

extern bool_init
extern ap_strcmp
extern value_selftest_main
extern compile_selftest_main
extern dis_main
extern dis_mode
extern builtins_init
extern methods_init
extern import_init
extern sys_path_add_script_dir
extern dict_new
extern dict_set
extern frame_new
extern frame_free
extern frame_pool_drain
extern eval_frame
extern code_from_path
extern fatal_error
extern obj_decref
extern str_from_cstr_heap
extern none_singleton
extern module_new
extern sys_modules_dict
extern sys_module_obj
extern sys_write

;; ============================================================================
;; main(int argc, char **argv) -> int
;; The 8 pads the five pushes to a 16-aligned rsp.  main holds argc and argv in
;; r14/r15 across the whole compile, so a source file with a float literal
;; reaches glibc's strtod from here -- and that is one of the few call paths out
;; of this codebase that actually uses aligned SSE.
;; ============================================================================
DEF_FUNC main, 8
    push rbx
    push r12
    push r13
    push r14
    push r15

    ; Check argc >= 2
    cmp edi, 2
    jl .usage

    ; Save argc/argv early
    mov r14d, edi               ; r14 = argc
    mov r15, rsi                ; r15 = argv

    ; Check for --version flag
    mov rax, [r15 + 8]          ; rax = argv[1]
    cmp word [rax], 0x2d2d      ; "--" little-endian
    jne .not_version
    mov rcx, [rax + 2]          ; load 8 bytes: "version\0"
    mov rdx, 0x006e6f6973726576 ; "version\0" little-endian
    cmp rcx, rdx
    jne .not_version

    ; Print version and exit 0
    mov edi, 1                  ; stdout
    lea rsi, [rel version_msg]
    mov edx, version_msg_len
    call sys_write
    xor eax, eax
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.not_version:

    ; Check for -h / --help / -?  CPython accepts all three, writes the help to
    ; stdout and exits 0, so we do the same.  The options listed are ours: a
    ; help screen naming flags this interpreter does not implement would be
    ; worse than no help screen at all.
    mov rdi, [r15 + 8]          ; rdi = argv[1]
    lea rsi, [rel help_flag]
    call ap_strcmp
    test eax, eax
    je .do_help
    mov rdi, [r15 + 8]
    lea rsi, [rel help_short_flag]
    call ap_strcmp
    test eax, eax
    je .do_help
    mov rdi, [r15 + 8]
    lea rsi, [rel help_q_flag]
    call ap_strcmp
    test eax, eax
    jne .not_help
.do_help:
    mov edi, 1                  ; stdout
    lea rsi, [rel help_msg]
    mov edx, help_msg_len
    call sys_write
    xor eax, eax
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.not_help:

    ; Check for --selftest-value flag (NaN-box encoding self-test)
    mov rdi, [r15 + 8]          ; rdi = argv[1]
    lea rsi, [rel selftest_flag]
    call ap_strcmp
    test eax, eax
    jne .not_selftest
    call bool_init
    call value_selftest_main
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.not_selftest:

    ; Check for --selftest-compile flag (source compiler self-test)
    mov rdi, [r15 + 8]          ; rdi = argv[1]
    lea rsi, [rel selftest_compile_flag]
    call ap_strcmp
    test eax, eax
    jne .not_selftest_compile
    call bool_init
    call compile_selftest_main
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.not_selftest_compile:

    ; Check for --dis: compile argv[2] as an expression and print its bytecode.
    ; Fidelity is semantic rather than byte-for-byte, so putting this beside
    ; `python3 -m dis` is the quickest way to localise a codegen bug.
    mov rdi, [r15 + 8]
    lea rsi, [rel dis_flag]
    call ap_strcmp
    test eax, eax
    jne .not_dis
    cmp r14, 3                  ; argc
    jl .usage
    call bool_init
    ; `--dis -x <source>` disassembles in exec mode rather than eval mode.
    mov rdi, [r15 + 16]         ; argv[2]
    cmp byte [rdi], '-'
    jne .dis_go
    cmp byte [rdi + 1], 'x'
    jne .dis_go
    cmp r14, 4
    jl .usage
    mov qword [rel dis_mode], CMODE_EXEC
    mov rdi, [r15 + 24]
.dis_go:
    call dis_main
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.not_dis:

    ; Check for -t flag (opcode tracing)
    mov rax, [r15 + 8]         ; rax = argv[1]
    cmp byte [rax], '-'
    jne .no_trace_flag
    cmp byte [rax + 1], 't'
    jne .no_trace_flag
    cmp byte [rax + 2], 0
    jne .no_trace_flag
    ; Point dispatch at the tracing table.  Setting a flag was not enough:
    ; every handler dispatches inline and only eval_dispatch tested it.
    extern opcode_dispatch_table
    extern opcode_trace_table
    lea rax, [rel opcode_trace_table]
    mov [rel opcode_dispatch_table], rax
    add r15, 8                  ; skip -t in argv
    dec r14d                    ; adjust argc
    cmp r14d, 2
    jl .usage
.no_trace_flag:

    ; Save argv[1] (the .pyc filename, after -t shift if any)
    mov rbx, [r15 + 8]         ; rbx = argv[1]

    ; Initialize subsystems
    call bool_init

    ; Load the file -> code object.  A .py is compiled here; anything else is
    ; read as marshalled bytecode.
    mov rdi, rbx
    call code_from_path
    test rax, rax
    jz .load_failed
    mov r12, rax                ; r12 = code object

    ; Create builtins dict
    call builtins_init
    mov r13, rax                ; r13 = builtins dict

    ; Initialize type methods (str, list, dict tp_dict)
    call methods_init

    ; Initialize import system (sys module, sys.modules, etc.)
    mov edi, r14d               ; argc
    mov rsi, r15                ; argv
    call import_init

    ; SIGINT raises KeyboardInterrupt rather than killing the process where it
    ; stands, which is what every `except KeyboardInterrupt` in the stdlib is
    ; written against.  After import_init, because it builds a builtin
    ; function object and the type machinery has to be up.
    extern signal_default_init
    call signal_default_init

    ; The SyntaxWarnings the compile recorded, now that there is an
    ; interpreter to emit them from: code_from_path above runs before
    ; builtins_init and import_init, so nothing could be imported then.
    extern comp_flush_warnings
    call comp_flush_warnings

    ; Set sys.path[0] to script directory
    mov rdi, rbx                ; pyc filename
    call sys_path_add_script_dir

    ; Create globals dict
    call dict_new
    push rax                    ; save globals dict on stack

    ; Set __name__ = "__main__" in globals
    lea rdi, [rel __name__cstr]
    call str_from_cstr_heap
    push rax                    ; save key str
    lea rdi, [rel __main__cstr]
    call str_from_cstr_heap
    push rax                    ; save value str
    mov rdx, rax                ; value = "__main__" str
    mov rsi, [rsp + 8]          ; key = "__name__" str
    mov rdi, [rsp + 16]         ; dict = globals (from stack)
    call dict_set
    pop rdi                     ; value str
    call obj_decref
    pop rdi                     ; key str
    call obj_decref

    ; Set __doc__ = None: a module without a docstring still HAS the name,
    ; and reading it was a NameError.  The body's own STORE_NAME overwrites
    ; this when there is a docstring.
    lea rdi, [rel __doc__cstr]
    call str_from_cstr_heap
    push rax
    mov rdi, [rsp + 8]         ; globals dict
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    pop rdi
    call obj_decref

    ; Set __package__ = None in globals (top-level module has no package)
    lea rdi, [rel __package__cstr]
    call str_from_cstr_heap
    push rax
    mov rdi, [rsp + 8]         ; globals dict
    mov rsi, rax
    lea rdx, [rel none_singleton]
    call dict_set
    pop rdi
    call obj_decref

    ; Set __builtins__ in globals
    extern builtins_dict_global
    lea rdi, [rel __builtins__cstr]
    call str_from_cstr_heap
    push rax
    mov rdi, [rsp + 8]         ; globals dict
    mov rsi, rax
    mov rdx, [rel builtins_dict_global]
    call dict_set
    pop rdi
    call obj_decref

    pop rax                     ; rax = globals dict
    mov r14, rax                ; r14 = globals dict

    ; Create __main__ module and register in sys.modules
    lea rdi, [rel __main__cstr]
    call str_from_cstr_heap
    push rax                    ; save "__main__" name str
    mov rdi, rax
    mov rsi, r14                ; dict = globals
    call module_new
    push rax                    ; save module object
    ; Register in sys.modules
    mov rdi, [rel sys_modules_dict]
    mov rsi, [rsp + 8]         ; key = "__main__" str
    mov rdx, rax               ; value = module object
    call dict_set
    pop rdi                     ; module object (owned by sys.modules now)
    call obj_decref
    pop rdi                     ; "__main__" str
    call obj_decref

    ; Create execution frame
    ; frame_new(code, globals, builtins, locals)
    ; For module-level code, locals == globals
    mov rdi, r12                ; code
    mov rsi, r14                ; globals
    mov rdx, r13                ; builtins
    mov rcx, r14                ; locals = globals
    call frame_new
    mov rbx, rax                ; rbx = frame

    ; Execute the bytecode
    mov rdi, rbx
    call eval_frame
    V_UNPACK rax, rdx           ; eval_frame returns a Value
    ; rax = return value (ignore for module-level code)

    ; Clean up
    mov rdi, rbx
    call frame_free

    ; Check for unhandled exception
    extern current_exception
    mov rdi, [rel current_exception]
    test rdi, rdi
    jz .exit_ok

    ; An uncaught SystemExit is the process status, not an error report.
    push rdi
    lea rsi, [rel exc_SystemExit_type]
    extern exc_SystemExit_type
    extern exc_isinstance
    call exc_isinstance
    pop rdi
    test eax, eax
    jnz .system_exit

    ; Print the traceback and the exception, CPython's shape.
    extern traceback_print
    ; Whatever is waiting on stdout goes out before the report does.  CPython
    ; flushes both streams in flush_io() on its way into the display routine,
    ; and it is what puts a program's output ahead of the traceback that ended
    ; it rather than after: the two streams are buffered differently, so
    ; without this the order through a pipe came out inverted.  The unraisable
    ; hook does NOT do it -- CPython's does not either.
    push rdi
    extern fileobj_flush_std
    call fileobj_flush_std
    pop rdi
    call traceback_print

    ; DECREF the exception object before exiting
    mov rdi, [rel current_exception]
    call obj_decref
    mov qword [rel current_exception], 0

    ; Exit 1
    mov ebx, 1
    jmp .exit_cleanup

.system_exit:
    ; SystemExit.code: absent or None -> 0, an int -> that status, anything
    ; else -> print it on stderr and exit 1.
    mov rax, [rdi + PyExceptionObject.exc_args]
    test rax, rax
    jz .se_zero
    mov rcx, [rax + PyTupleObject.ob_size]
    test rcx, rcx
    jz .se_zero
    mov rcx, [rax + PyTupleObject.ob_item]
    mov rax, [rcx]                 ; args[0] as a Value
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .se_zero
    V_IS_INT rax, rcx
    jae .se_int
    V_TEST_PTR rax, rcx
    ja .se_one                     ; a float status is not a status
    mov rcx, [rax + PyObject.ob_type]
    extern int_type
    lea rdx, [rel int_type]
    cmp rcx, rdx
    je .se_bigint
    ; True and False are ints: sys.exit(False) exits 0, not 1.
    extern bool_type
    lea rdx, [rel bool_type]
    cmp rcx, rdx
    je .se_bigint
    extern str_type
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .se_one
    ; A message: print it and exit 1.
    mov rdx, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    mov edi, 2
    call sys_write
    mov edi, 2
    lea rsi, [rel newline_char]
    mov edx, 1
    call sys_write
    jmp .se_one

.se_bigint:
    ; A heap int still has to fit a process status; take it modulo 256 the
    ; way the kernel does with the low byte of the syscall argument.
    mov rdi, rax
    mov edx, TAG_PTR
    extern int_to_i64
    call int_to_i64
    mov ebx, eax
    and ebx, 0xff
    jmp .se_finish

.se_int:
    V_TO_I64 rax
    mov ebx, eax
    and ebx, 0xff
    jmp .se_finish

.se_one:
    mov ebx, 1
    jmp .se_finish

.se_zero:
    xor ebx, ebx
.se_finish:
    mov rdi, [rel current_exception]
    call obj_decref
    mov qword [rel current_exception], 0
    jmp .exit_cleanup

.exit_ok:
    xor ebx, ebx

.exit_cleanup:
    ; Whatever is still waiting in stdout's buffer goes out first, and before
    ; the collection below: a __del__ that prints has to reach the same
    ; stream, in order, and a buffer abandoned at exit is output that was
    ; produced and never seen.
    extern fileobj_flush_std
    call fileobj_flush_std

    ; Break sys.modules cycle: sys_modules_dict -> sys module -> sys_dict
    ;   -> "modules" entry -> sys_modules_dict
    ; NULL out sys_module.mod_dict and DECREF the old dict twice:
    ;   once for creation ref, once for module_new INCREF
    ;   (module_dealloc won't DECREF since we NULLed mod_dict)
    mov rax, [rel sys_module_obj]
    test rax, rax
    jz .no_sys_module
    mov rdi, [rax + PyModuleObject.mod_dict]
    mov qword [rax + PyModuleObject.mod_dict], 0
    test rdi, rdi
    jz .no_sys_module
    push rdi
    call obj_decref
    pop rdi
    call obj_decref
.no_sys_module:

    ; Finalize cyclic garbage while the interpreter is still whole.  CPython
    ; runs a full collection at shutdown, which is what makes a __del__ on an
    ; object in a cycle run at all; without one, every such cycle was simply
    ; abandoned.  It goes before the sys.modules cascade rather than after:
    ; module dicts are what most cycles hang off, and a collection run after
    ; they are torn down finds nothing to do.
    extern gc_collect_gen
    mov edi, 2
    call gc_collect_gen

    ; DECREF owned objects.
    ;
    ; sys.modules goes first, and before the code object.  Freeing it cascades
    ; into every module dict -- __main__'s among them, which is the globals
    ; dict -- and those dicts hold strings that came out of the running code
    ; object's co_consts and co_names.  Releasing the code object first drove
    ; some of those to zero, and the module teardown then read freed memory:
    ; valgrind saw the invalid read in dict_dealloc, and libc's allocator
    ; noticed it as "unaligned fastbin chunk" once an unrelated change moved
    ; the heap around.  The missing INCREF behind it was in
    ; sre_match_groupdict_method and is fixed; this order is kept because
    ; dropping the users before the object they borrow from is right anyway,
    ; and it stops the next such bug from being a crash at teardown.
    mov rdi, [rel sys_modules_dict]
    call obj_decref

    mov rdi, r14            ; globals dict
    call obj_decref
    mov rdi, r12            ; code object
    call obj_decref

    ; DECREF builtins dict (after sys.modules cascade reduced its refcount)
    mov rdi, [rel builtins_dict_global]
    call obj_decref

    ; Drain frame pools
    call frame_pool_drain

    mov eax, ebx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.usage:
    CSTRING rdi, "usage: apython [option] ... <file>; try `apython --help'"
    call fatal_error

.load_failed:
    ; A source file that failed to compile has a real SyntaxError pending, with
    ; a line number in it; saying "failed to load" instead would throw that
    ; away.  Nothing has run yet, so there is no traceback to print.
    mov rdi, [rel current_exception]
    test rdi, rdi
    jz .load_failed_plain
    ; Whatever is waiting on stdout goes out before the report does.  CPython
    ; flushes both streams in flush_io() on its way into the display routine,
    ; and it is what puts a program's output ahead of the traceback that ended
    ; it rather than after: the two streams are buffered differently, so
    ; without this the order through a pipe came out inverted.  The unraisable
    ; hook does NOT do it -- CPython's does not either.
    push rdi
    extern fileobj_flush_std
    call fileobj_flush_std
    pop rdi
    call traceback_print
    mov rdi, [rel current_exception]
    call obj_decref
    mov qword [rel current_exception], 0
    ; .exit_cleanup DECREFs the globals dict and the code object, and at this
    ; point neither exists -- nothing has been loaded yet.  Return straight out
    ; with the status instead.
    mov eax, 1
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
.load_failed_plain:
    CSTRING rdi, "Error: failed to load file"
    call fatal_error
END_FUNC main

section .rodata
help_flag: db "--help", 0
help_short_flag: db "-h", 0
help_q_flag: db "-?", 0
help_msg:
    db "usage: apython [option] ... [file]", 10
    db "Options:", 10
    db "-h     : print this help message and exit (also -? or --help)", 10
    db "-t     : trace opcodes (partial: DISPATCH-terminated handlers are", 10
    db "         not traced -- see bugs.md)", 10
    db "--version : print the apython version number and exit", 10
    db "--dis [-x] <source> : print the bytecode compiled from <source>;", 10
    db "         -x compiles in exec mode rather than eval mode", 10
    db "--selftest-value : run the Value encoding self-test and exit", 10
    db "--selftest-compile : run the source compiler self-test and exit", 10
    db 10
    db "Arguments:", 10
    db "file   : a .py file to compile and run, or a .pyc file to run directly", 10
help_msg_len equ $ - help_msg

selftest_flag: db "--selftest-value", 0
selftest_compile_flag: db "--selftest-compile", 0
dis_flag: db "--dis", 0
version_msg: db "apython ", VERSION_STR, 10
version_msg_len equ $ - version_msg
__name__cstr: db "__name__", 0
__doc__cstr: db "__doc__", 0
__main__cstr: db "__main__", 0
__package__cstr: db "__package__", 0
__builtins__cstr: db "__builtins__", 0
newline_char: db 10
