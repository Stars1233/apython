; main.asm - Entry point for apython
; Parses argv, loads .pyc, creates frame, runs bytecode

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "errcodes.inc"
%include "frame.inc"

extern bool_init
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
extern pyc_read_file
extern fatal_error
extern obj_decref
extern str_from_cstr_heap
extern none_singleton
extern module_new
extern sys_modules_dict
extern sys_module_obj

; main(int argc, char **argv) -> int
DEF_FUNC main
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

    ; Check for -t flag (opcode tracing)
    mov rax, [r15 + 8]         ; rax = argv[1]
    cmp byte [rax], '-'
    jne .no_trace_flag
    cmp byte [rax + 1], 't'
    jne .no_trace_flag
    cmp byte [rax + 2], 0
    jne .no_trace_flag
    extern trace_opcodes
    mov byte [rel trace_opcodes], 1
    add r15, 8                  ; skip -t in argv
    dec r14d                    ; adjust argc
    cmp r14d, 2
    jl .usage
.no_trace_flag:

    ; Save argv[1] (the .pyc filename, after -t shift if any)
    mov rbx, [r15 + 8]         ; rbx = argv[1]

    ; Initialize subsystems
    call bool_init

    ; Load .pyc file -> code object
    mov rdi, rbx
    call pyc_read_file
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
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi                     ; value str
    call obj_decref
    pop rdi                     ; key str
    call obj_decref

    ; Set __package__ = None in globals (top-level module has no package)
    lea rdi, [rel __package__cstr]
    call str_from_cstr_heap
    push rax
    mov rdi, [rsp + 8]         ; globals dict
    mov rsi, rax
    lea rdx, [rel none_singleton]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
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
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
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
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
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
    ; rax = return value (ignore for module-level code)

    ; Clean up
    mov rdi, rbx
    call frame_free

    ; Check for unhandled exception
    extern current_exception
    mov rdi, [rel current_exception]
    test rdi, rdi
    jz .exit_ok

    ; Print exception to stderr: "ExceptionType: message\n"
    ; Get type name
    mov rax, [rdi + PyObject.ob_type]
    mov rsi, [rax + PyTypeObject.tp_name]
    ; strlen of type name
    push rdi
    mov rdi, rsi
    xor ecx, ecx
.strlen_type:
    cmp byte [rdi + rcx], 0
    je .strlen_type_done
    inc ecx
    jmp .strlen_type
.strlen_type_done:
    mov edx, ecx
    push rsi
    push rdx
    mov edi, 2                  ; stderr
    extern sys_write
    call sys_write
    pop rdx
    pop rsi
    pop rdi

    ; Check for message (must be TAG_PTR)
    extern PyExceptionObject
    cmp qword [rdi + PyExceptionObject.exc_value_tag], TAG_PTR
    jne .print_newline
    mov rax, [rdi + PyExceptionObject.exc_value]
    test rax, rax
    jz .print_newline

    ; Print ": "
    push rax
    mov edi, 2
    lea rsi, [rel colon_space]
    mov edx, 2
    call sys_write
    pop rax

    ; Print message (must be a string)
    mov rcx, [rax + PyObject.ob_type]
    extern str_type
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .print_newline

    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    mov edi, 2
    call sys_write

.print_newline:
    mov edi, 2
    lea rsi, [rel newline_char]
    mov edx, 1
    call sys_write

    ; DECREF the exception object before exiting
    mov rdi, [rel current_exception]
    call obj_decref
    mov qword [rel current_exception], 0

    ; Exit 1
    mov ebx, 1
    jmp .exit_cleanup

.exit_ok:
    xor ebx, ebx

.exit_cleanup:
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

    ; DECREF owned objects
    mov rdi, r14            ; globals dict
    call obj_decref
    mov rdi, r12            ; code object
    call obj_decref

    ; DECREF sys.modules (cascades to free all modules and their dicts)
    mov rdi, [rel sys_modules_dict]
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
    CSTRING rdi, "Usage: apython [-t] <file.pyc>"
    call fatal_error

.load_failed:
    CSTRING rdi, "Error: failed to load .pyc file"
    call fatal_error
END_FUNC main

section .rodata
__name__cstr: db "__name__", 0
__main__cstr: db "__main__", 0
__package__cstr: db "__package__", 0
__builtins__cstr: db "__builtins__", 0
colon_space: db ": "
newline_char: db 10
