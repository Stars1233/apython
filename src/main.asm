; main.asm - Entry point for apython
; Parses argv, loads .pyc, creates frame, runs bytecode

%include "macros.inc"
%include "object.inc"
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
extern eval_frame
extern pyc_read_file
extern fatal_error
extern obj_decref
extern str_from_cstr
extern none_singleton
extern module_new
extern sys_modules_dict

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
    call str_from_cstr
    push rax                    ; save key str
    lea rdi, [rel __main__cstr]
    call str_from_cstr
    mov rdx, rax                ; value = "__main__" str
    pop rsi                     ; key = "__name__" str
    mov rdi, [rsp]              ; dict = globals (from stack)
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set

    ; Set __package__ = None in globals (top-level module has no package)
    lea rdi, [rel __package__cstr]
    call str_from_cstr
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
    call str_from_cstr
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
    call str_from_cstr
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

    ; Exit 0
    xor eax, eax
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
