; builtins.asm - Builtin functions and builtins dict initialization
; Phase 4: print, len, and builtin function wrapper type

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"
%include "builtins.inc"

extern dict_new
extern dict_get
extern dict_set
extern str_from_cstr
extern str_from_cstr_heap
extern obj_str
extern obj_incref
extern obj_decref
extern obj_dealloc
extern none_singleton
extern int_from_i64
extern str_type
extern bool_type
extern float_type
extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern fatal_error
extern raise_exception
extern build_class_pending
extern sys_write
extern range_new
extern int_to_i64
extern current_exception
extern obj_as_index
extern init_iter_types
extern obj_repr
extern eval_frame
extern frame_new
extern frame_free
extern ap_memcpy
extern instance_dealloc
extern instance_repr
extern instance_getattr
extern instance_setattr
extern type_call
extern user_type_metatype
extern super_type
extern staticmethod_type
extern classmethod_type
extern property_type
extern func_type
extern type_type
extern list_type
extern dict_type
extern tuple_type
extern set_type
extern bytes_type

; New builtin function implementations (in builtins_extra.asm)
extern builtin_abs
extern builtin_divmod
extern builtin_int_fn
extern int_type_call
extern str_type_call
extern bool_type_call
extern float_type_call
extern bytearray_type_call
extern memoryview_type_call
extern bytearray_type
extern memoryview_type
extern builtin_str_fn
extern builtin_ord
extern builtin_chr
extern builtin_hex
extern builtin_id
extern builtin_hash_fn
extern builtin_callable
extern builtin_iter_fn
extern builtin_next_fn
extern builtin_any
extern builtin_all
extern builtin_sum
extern builtin_min
extern builtin_max
extern builtin_getattr
extern builtin_hasattr
extern builtin_setattr

; Iterator builtins (in itertools.asm)
extern builtin_enumerate
extern builtin_zip
extern builtin_map
extern builtin_filter
extern builtin_reversed
extern builtin_sorted
extern builtin_chain
extern builtin_globals
extern builtin_locals
extern builtin_dir
extern builtin_breakpoint

; Exception types
extern exc_BaseException_type
extern exc_Exception_type
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_KeyError_type
extern exc_IndexError_type
extern exc_AttributeError_type
extern exc_NameError_type
extern exc_UnboundLocalError_type
extern exc_RuntimeError_type
extern exc_StopIteration_type
extern exc_ZeroDivisionError_type
extern exc_ImportError_type
extern exc_NotImplementedError_type
extern exc_OverflowError_type
extern exc_AssertionError_type
extern exc_OSError_type
extern exc_LookupError_type
extern exc_ArithmeticError_type
extern exc_RecursionError_type
extern exc_MemoryError_type
extern exc_KeyboardInterrupt_type
extern exc_SystemExit_type
extern exc_UnicodeError_type
extern exc_Warning_type
extern exc_DeprecationWarning_type
extern exc_UserWarning_type
extern exc_BaseExceptionGroup_type
extern exc_ExceptionGroup_type
extern exc_CancelledError_type
extern exc_StopAsyncIteration_type
extern exc_TimeoutError_type

;; ============================================================================
;; builtin_func_new(void *func_ptr, const char *name_cstr) -> PyBuiltinObject*
;; Create a new builtin function wrapper object
;; ============================================================================
DEF_FUNC builtin_func_new
    push rbx
    push r12
    push r13

    mov rbx, rdi                ; func_ptr
    mov r12, rsi                ; name_cstr

    ; Create a string object for the name (heap — stored in struct field)
    mov rdi, r12
    call str_from_cstr_heap
    mov r13, rax                ; r13 = name string object

    ; Allocate PyBuiltinObject
    mov edi, PyBuiltinObject_size
    call ap_malloc
    ; rax = new object

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel builtin_func_type]
    mov [rax + PyObject.ob_type], rcx

    ; Fill builtin-specific fields
    mov qword [rax + PyBuiltinObject.func_id], 0   ; not used for func_ptr dispatch
    mov [rax + PyBuiltinObject.func_name], r13
    mov [rax + PyBuiltinObject.func_ptr], rbx
    mov qword [rax + PyBuiltinObject.min_args], 0  ; 0 = no check
    mov qword [rax + PyBuiltinObject.max_args], -1 ; -1 = no max check

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC builtin_func_new

;; ============================================================================
;; builtin_func_new_checked(void *func_ptr, const char *name_cstr,
;;                          int64_t min_args, int64_t max_args)
;; Like builtin_func_new but sets arg count bounds for validation.
;; rdx = min_args (including self), rcx = max_args (-1 = no max)
;; ============================================================================
global builtin_func_new_checked
DEF_FUNC builtin_func_new_checked
    push r14
    push r15
    mov r14, rdx                ; min_args
    mov r15, rcx                ; max_args
    call builtin_func_new
    mov [rax + PyBuiltinObject.min_args], r14
    mov [rax + PyBuiltinObject.max_args], r15
    pop r15
    pop r14
    leave
    ret
END_FUNC builtin_func_new_checked

;; ============================================================================
;; builtin_func_call(PyObject *self, PyObject **args, int64_t nargs) -> PyObject*
;; Dispatch to the underlying C function: func_ptr(args, nargs)
;; Validates nargs against min_args/max_args if set.
;; ============================================================================
DEF_FUNC_BARE builtin_func_call
    ; self = rdi, args = rsi, nargs = rdx
    ; Check min_args (0 = no check)
    mov rcx, [rdi + PyBuiltinObject.min_args]
    test rcx, rcx
    jz .bfc_no_min_check
    cmp rdx, rcx
    jl .bfc_too_few
.bfc_no_min_check:
    ; Check max_args (-1 = no check)
    mov rcx, [rdi + PyBuiltinObject.max_args]
    cmp rcx, -1
    je .bfc_no_max_check
    cmp rdx, rcx
    jg .bfc_too_many
.bfc_no_max_check:
    ; Extract func_ptr from self
    mov rax, [rdi + PyBuiltinObject.func_ptr]
    ; Call func_ptr(args, nargs) — builtins return a Value, so this stays a
    ; tail call.
    mov rdi, rsi                ; args
    mov rsi, rdx                ; nargs
    jmp rax

.bfc_too_few:
    extern exc_TypeError_type
    extern raise_exception
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "function takes at least 1 argument"
    call raise_exception
.bfc_too_many:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "function takes at most N arguments"
    call raise_exception
END_FUNC builtin_func_call

;; ============================================================================
;; builtin_func_dealloc(PyObject *self)
;; Free the builtin function wrapper
;; ============================================================================
DEF_FUNC_LOCAL builtin_func_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF the name string
    mov rdi, [rbx + PyBuiltinObject.func_name]
    test rdi, rdi
    jz .no_name
    call obj_decref
.no_name:

    ; Free the object
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC builtin_func_dealloc

;; ============================================================================
;; builtin_func_repr(PyObject *self) -> PyObject*
;; Returns "<built-in function NAME>"
;; ============================================================================
DEF_FUNC_LOCAL builtin_func_repr

    ; For simplicity, just return the name string with INCREF
    mov rax, [rdi + PyBuiltinObject.func_name]
    test rax, rax
    jz .fallback
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

.fallback:
    lea rdi, [rel builtin_func_repr_unknown_str]
    call str_from_cstr
    leave
    ret
END_FUNC builtin_func_repr

section .rodata
builtin_func_repr_unknown_str: db "<built-in function>", 0
section .text

;; ============================================================================
;; builtin_print(PyObject **args, int64_t nargs) -> PyObject*
;; Print each arg separated by spaces, followed by newline
;; Buffered: builds output in stack buffer, single fwrite() at end
;; ============================================================================
; Print frame layout
PR_SEP       equ 8     ; sep string ptr (0 = default " ")
PR_SEP_TAG   equ 16    ; sep tag
PR_END       equ 24    ; end string ptr (0 = default "\n")
PR_END_TAG   equ 32    ; end tag
PR_FILE_FD   equ 40    ; file descriptor (1 = stdout)
PR_BUF       equ 48    ; start of 4096 byte buffer
PR_FRAME     equ 4144  ; total frame size (48 + 4096)

extern kw_names_pending
extern ap_strcmp

DEF_FUNC builtin_print, PR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi                ; args array
    mov r12, rsi                ; nargs
    xor r13d, r13d              ; r13 = current arg index
    xor r15d, r15d              ; r15 = buffer write offset

    ; Initialize defaults
    mov qword [rbp - PR_SEP], 0       ; NULL = default " "
    mov qword [rbp - PR_END], 0       ; NULL = default "\n"
    mov qword [rbp - PR_FILE_FD], 1   ; stdout

    ; Check for keyword arguments
    mov rax, [rel kw_names_pending]
    test rax, rax
    jz .print_no_kw

    ; Parse kwargs
    mov rcx, [rax + PyTupleObject.ob_size]  ; n_kw
    sub r12, rcx                             ; r12 = n_pos (positional count)
    ; Process each kwarg
    xor r9d, r9d                             ; kw index
.print_kw_loop:
    cmp r9, rcx
    jge .print_kw_done
    push rcx
    push rax
    push r9

    ; Get kwarg name
    mov r10, [rax + PyTupleObject.ob_item]        ; kw names payloads
    mov r10, [r10 + r9*8]                          ; kw name str

    ; Get kwarg value position: n_pos + kw_index
    mov r11, r12                   ; n_pos
    add r11, r9                    ; n_pos + kw_index
    shl r11, 3                     ; one Value per slot
    ; value at [rbx + r11]

    ; Check "sep"
    push r10
    push r11
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "sep"
    call ap_strcmp
    test eax, eax
    pop r11
    pop r10
    jz .print_kw_sep

    ; Check "end"
    push r10
    push r11
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "end"
    call ap_strcmp
    test eax, eax
    pop r11
    pop r10
    jz .print_kw_end

    ; Check "file"
    push r10
    push r11
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "file"
    call ap_strcmp
    test eax, eax
    pop r11
    pop r10
    jz .print_kw_file

    ; Check "flush" — accept but ignore
    push r10
    push r11
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "flush"
    call ap_strcmp
    test eax, eax
    pop r11
    pop r10
    jz .print_kw_next

    ; Unknown keyword — skip (be lenient)
    jmp .print_kw_next

.print_kw_sep:
    mov rax, [rbx + r11]
    V_UNPACK rax, rdx
    mov [rbp - PR_SEP], rax
    mov [rbp - PR_SEP_TAG], rdx
    jmp .print_kw_next

.print_kw_end:
    mov rax, [rbx + r11]
    V_UNPACK rax, rdx
    mov [rbp - PR_END], rax
    mov [rbp - PR_END_TAG], rdx
    jmp .print_kw_next

.print_kw_file:
    ; file kwarg: get file descriptor from file object
    mov rax, [rbx + r11]           ; file object Value
    V_TEST_PTR rax, rdx
    ja .print_kw_next               ; non-pointer file= → ignore
    mov rax, [rax + PyFileObject.file_fd]
    mov [rbp - PR_FILE_FD], rax
    jmp .print_kw_next

.print_kw_next:
    pop r9
    pop rax
    pop rcx
    inc r9
    jmp .print_kw_loop

.print_kw_done:
    mov qword [rel kw_names_pending], 0

.print_no_kw:

align 16
.print_loop:
    cmp r13, r12
    jge .print_flush

    ; Get string representation: obj_str(args[i]) with tag
    mov rax, r13
    shl rax, 3                  ; one Value per slot
    mov rdi, [rbx + rax]       ; arg Value
    call obj_str
    ; obj_str returns (rax=payload, edx=tag)
    mov r14, rax                ; r14 = result payload
    mov r9, rdx                 ; r9 = result tag

    test r9d, r9d
    jz .skip_arg                ; TAG_NULL → skip

    ; Heap string: get length from ob_size
    mov rcx, [r14 + PyStrObject.ob_size]

    ; Check if it fits in buffer (need room for data + possible space)
    lea rax, [r15 + rcx + 2]   ; +2 for space and newline
    cmp rax, 4096
    jae .flush_and_write_direct

    ; Copy string data into buffer
    lea rdi, [rbp - PR_FRAME + r15] ; dest = buf + offset
    lea rsi, [r14 + PyStrObject.data]  ; src = str data
    mov rdx, rcx                ; len
    ; Inline small copy (most strings are short)
    test rcx, rcx
    jz .copy_done
    call ap_memcpy
.copy_done:
    add r15, [r14 + PyStrObject.ob_size]

    ; DECREF the string representation (known TAG_PTR heap string;
    ; r9 tag may have been clobbered by ap_memcpy call above)
    mov rdi, r14
    call obj_decref

.skip_arg:
    ; Append separator if not the last arg
    inc r13
    cmp r13, r12
    jge .print_flush

    ; Check if custom sep was provided
    cmp qword [rbp - PR_SEP], 0
    jne .print_custom_sep

    ; Default: single space
    mov byte [rbp - PR_FRAME + r15], ' '
    inc r15
    jmp .print_loop

.print_custom_sep:
    ; Custom sep — check if None (means default " ")
    mov rax, [rbp - PR_SEP_TAG]
    cmp eax, TAG_PTR
    jne .print_default_sep_fallback

    ; Heap string sep
    mov rax, [rbp - PR_SEP]
    ; Check if None singleton
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .print_default_sep_fallback

    mov rcx, [rax + PyStrObject.ob_size]
    ; The per-argument copy checks the 4096-byte buffer; this one did not, so
    ; print(..., sep="X"*5000) wrote past the frame over the return address.
    lea rdx, [r15 + rcx + 2]
    cmp rdx, 4096
    jae .print_sep_direct
    ; Copy sep bytes into buffer
    lea rdi, [rbp - PR_FRAME + r15]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, rcx
    test rcx, rcx
    jz .print_sep_done
    push rcx
    call ap_memcpy
    pop rcx
.print_sep_done:
    add r15, rcx
    jmp .print_loop

.print_sep_direct:
    ; Flush what is buffered, then write the separator straight out.
    test r15, r15
    jz .print_sep_write
    mov rdi, [rbp - PR_FILE_FD]
    lea rsi, [rbp - PR_FRAME]
    mov rdx, r15
    call sys_write
    xor r15d, r15d
.print_sep_write:
    mov rax, [rbp - PR_SEP]
    mov rdi, [rbp - PR_FILE_FD]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    call sys_write
    jmp .print_loop

.print_default_sep_fallback:
    mov byte [rbp - PR_FRAME + r15], ' '
    inc r15
    jmp .print_loop

.flush_and_write_direct:
    ; Buffer full - flush what we have, then write this string directly
    ; First flush buffer
    test r15, r15
    jz .write_direct
    mov edi, 1                  ; fd = stdout
    lea rsi, [rbp - PR_FRAME]      ; buf
    mov rdx, r15                ; len
    call sys_write
    xor r15d, r15d              ; reset offset

.write_direct:
    ; Write this string directly
    mov edi, 1                  ; fd = stdout
    lea rsi, [r14 + PyStrObject.data]
    mov rdx, [r14 + PyStrObject.ob_size]  ; len
    call sys_write

    ; DECREF the string representation (known TAG_PTR heap string;
    ; r9 tag was clobbered by sys_write calls above)
    mov rdi, r14
    call obj_decref
    jmp .skip_arg

.print_flush:
    ; Append end string (default: "\n")
    cmp qword [rbp - PR_END], 0
    jne .print_custom_end

    ; Default: newline
    mov byte [rbp - PR_FRAME + r15], 10
    inc r15
    jmp .print_do_flush

.print_custom_end:
    ; Check if None (means default "\n")
    mov rax, [rbp - PR_END_TAG]
    cmp eax, TAG_PTR
    jne .print_default_end
    mov rax, [rbp - PR_END]
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .print_default_end

    ; Custom end string, bounded the same way
    mov rcx, [rax + PyStrObject.ob_size]
    lea rdx, [r15 + rcx + 2]
    cmp rdx, 4096
    jae .print_end_direct
    lea rdi, [rbp - PR_FRAME + r15]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, rcx
    test rcx, rcx
    jz .print_end_copy_done
    push rcx
    call ap_memcpy
    pop rcx
.print_end_copy_done:
    add r15, rcx
    jmp .print_do_flush

.print_end_direct:
    test r15, r15
    jz .print_end_write
    mov rdi, [rbp - PR_FILE_FD]
    lea rsi, [rbp - PR_FRAME]
    mov rdx, r15
    call sys_write
    xor r15d, r15d
.print_end_write:
    mov rax, [rbp - PR_END]
    mov rdi, [rbp - PR_FILE_FD]
    lea rsi, [rax + PyStrObject.data]
    mov rdx, [rax + PyStrObject.ob_size]
    call sys_write
    jmp .print_do_flush

.print_default_end:
    mov byte [rbp - PR_FRAME + r15], 10
    inc r15

.print_do_flush:
    ; Single sys_write for entire output
    mov rdi, [rbp - PR_FILE_FD]  ; fd (1 = stdout)
    lea rsi, [rbp - PR_FRAME]      ; buf
    mov rdx, r15                ; len
    call sys_write

    ; Return None (with INCREF)
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC builtin_print

;; ============================================================================
;; builtin_len(PyObject **args, int64_t nargs) -> PyObject*
;; Returns len() of the first argument
;; Phase 4 stub: checks ob_size for variable-size objects
;; ============================================================================
LEN_EXC   equ 8
LEN_FRAME equ 16
DEF_FUNC builtin_len, LEN_FRAME
    push rbx

    ; Check nargs == 1
    cmp rsi, 1
    jne .len_error

    mov rbx, [rdi]              ; rbx = args[0]
    V_TEST_PTR rbx, rax
    ja .len_type_error

    ; Check if the object has a mapping mp_length
    mov rax, [rbx + PyObject.ob_type]
    test rax, rax
    jz .len_error

    ; Try tp_as_mapping->mp_length first
    mov rcx, [rax + PyTypeObject.tp_as_mapping]
    test rcx, rcx
    jz .try_sequence
    mov rcx, [rcx + PyMappingMethods.mp_length]
    test rcx, rcx
    jz .try_sequence
    mov rdi, rbx
    call rcx
    jmp .make_int

.try_sequence:
    ; Try tp_as_sequence->sq_length
    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_as_sequence]
    test rcx, rcx
    jz .try_dunder_len
    mov rcx, [rcx + PySequenceMethods.sq_length]
    test rcx, rcx
    jz .try_dunder_len
    mov rdi, rbx
    call rcx
    jmp .make_int

.try_dunder_len:
    ; Try __len__ dunder on heaptype
    mov rax, [rbx + PyObject.ob_type]
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_HEAPTYPE
    jz .len_type_error

    extern dunder_len
    extern dunder_call_1
    mov rdi, rbx
    DUNDER_EXC_SAVE [rbp - LEN_EXC]
    lea rsi, [rel dunder_len]
    call dunder_call_1
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .no_dunder_len

    ; __len__ returned a result — extract integer value
    push rdx                ; save tag for SmallInt check
    push rax                ; save result for DECREF
    ; Check if SmallInt (tag == TAG_SMALLINT)
    cmp qword [rsp + 8], TAG_SMALLINT
    je .len_smallint
    ; Heap int — read value (assume fits in 64 bits)
    extern int_to_i64
    mov rdi, rax
    call int_to_i64
    pop rdi                 ; DECREF the int result
    add rsp, 8              ; discard saved tag
    push rax                ; save extracted value
    call obj_decref
    pop rax
    jmp .make_int

.len_smallint:
    ; SmallInt: payload IS the int64 value, no DECREF needed
    pop rax                 ; restore payload
    add rsp, 8              ; discard saved tag
    jmp .make_int

.no_dunder_len:
    ; A NULL from dunder_call_1 means either "no __len__ on this type" or
    ; "__len__ raised"; only the first is a fallback.
    DUNDER_RAISED [rbp - LEN_EXC], .len_failed

    ; There is no last-resort ob_size read here any more.  Every real
    ; container reaches len() through sq_length, mp_length or __len__; the
    ; fallback only caught things that have no length at all, and read +16 --
    ; which for an iterator is it_seq, so len(reversed([1,2,3])) returned a
    ; heap address.
    jmp .len_type_error

.make_int:
    ; rax = length; create an int object
    mov rdi, rax
    call int_from_i64
    ; int_from_i64 returns (rax=payload, edx=tag) — preserve edx

    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.len_failed:
    RET_NULL
    pop rbx
    leave
    ret

.len_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "len() takes exactly one argument"
    call raise_exception

.len_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object has no len()"
    call raise_exception
END_FUNC builtin_len

;; ============================================================================
;; builtin_range(PyObject **args, int64_t nargs) -> PyObject*
;; range(stop) or range(start, stop) or range(start, stop, step)
;; ============================================================================
DEF_FUNC builtin_range
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; args
    mov r12, rsi               ; nargs

    cmp r12, 1
    je .range_1
    cmp r12, 2
    je .range_2
    cmp r12, 3
    je .range_3

    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "range expected 1 to 3 arguments"
    call raise_exception

.range_1:
    ; range(stop): start=0, stop=args[0], step=1
    mov rdi, [rbx]             ; args[0]
    V_UNPACK rdi, rdx
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    mov rsi, rax               ; stop
    xor edi, edi               ; start = 0
    mov edx, 1                 ; step = 1
    call range_new
    jmp .range_done

.range_2:
    ; range(start, stop): step=1
    mov rdi, [rbx]             ; args[0]
    V_UNPACK rdi, rdx
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    mov r13, rax               ; start
    mov rdi, [rbx + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    mov rsi, rax               ; stop
    mov rdi, r13               ; start
    mov edx, 1                 ; step = 1
    call range_new
    jmp .range_done

.range_3:
    ; range(start, stop, step)
    mov rdi, [rbx]             ; args[0]
    V_UNPACK rdi, rdx
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    push rax                   ; start
    mov rdi, [rbx + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    push rax                   ; stop
    mov rdi, [rbx + 16]
    V_UNPACK rdi, rdx       ; args[2]
    call obj_as_index      ; raises for a non-integer, rather than
                           ; decoding its payload as one
    mov rdx, rax               ; step
    ; A zero step makes range_obj_sq_length divide by zero and makes
    ; range_iter_next advance by nothing, so len(range(0,5,0)) was SIGFPE
    ; and `for i in range(0,5,0)` hung.
    test rdx, rdx
    jz .range_zero_step
    pop rsi                    ; stop
    pop rdi                    ; start
    call range_new
    jmp .range_done

.range_zero_step:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "range() arg 3 must not be zero"
    call raise_exception

.range_done:
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC builtin_range

;; ============================================================================
;; builtin_type(PyObject **args, int64_t nargs) -> PyObject*
;; type(obj) -> returns obj's type
;; ============================================================================
DEF_FUNC builtin_type

    cmp rsi, 1
    jne .type_error

    mov rsi, rdi               ; save args ptr
    mov rdi, [rsi]             ; obj = args[0] payload

    ; SmallInt check (tag at args[0]+8)
    V_TEST_INT_M [rsi], r11      ; args[0] an int immediate?
    jae .type_smallint

    ; Float check
    V_TEST_F64_M [rsi], r11      ; args[0] a float?
    jbe .type_float

    ; Bool check

    ; None check

    mov rax, [rdi + PyObject.ob_type]
    ; See value_type: the heaptype metatype is not a language-visible type.
    extern user_type_metatype
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    jne .type_have_type
    lea rax, [rel type_type]
.type_have_type:
    INCREF rax

    mov edx, TAG_PTR
    leave
    ret

.type_smallint:
    extern int_type
    lea rax, [rel int_type]
    INCREF rax
    mov edx, TAG_PTR
    leave
    ret

.type_float:
    lea rax, [rel float_type]
    INCREF rax
    mov edx, TAG_PTR
    leave
    ret

.type_bool:
    extern bool_type
    lea rax, [rel bool_type]
    INCREF rax
    mov edx, TAG_PTR
    leave
    ret

.type_none:
    extern none_type
    lea rax, [rel none_type]
    INCREF rax
    mov edx, TAG_PTR
    leave
    ret

.type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "type() takes 1 argument"
    call raise_exception
END_FUNC builtin_type

;; ============================================================================
;; builtin_isinstance(PyObject **args, int64_t nargs) -> PyObject*
;; isinstance(obj, type) -> True/False
;; Walks the full tp_base chain for inheritance.
;; ============================================================================
ISI_OBJ   equ 8         ; the object as a Value, for __instancecheck__
ISI_FRAME equ 16
DEF_FUNC builtin_isinstance, ISI_FRAME
    push rbx
    push r12

    cmp rsi, 2
    jne .isinstance_error

    extern bool_true
    extern bool_false

    mov rax, [rdi]
    mov [rbp - ISI_OBJ], rax   ; the object as a Value, for __instancecheck__
    mov rax, [rdi]             ; rax = args[0] = obj
    V_UNPACK rax, r8
    mov rcx, [rdi + 8]         ; rcx = args[1] = type_to_check
    V_UNPACK rcx, r9

    ; Get obj's type (tag-aware for all inline types)
    cmp r8d, TAG_SMALLINT
    je .isinstance_smallint
    cmp r8d, TAG_FLOAT
    je .isinstance_float
    cmp r8d, TAG_PTR
    jne .isinstance_false      ; unknown non-pointer tag → False
    mov rdx, [rax + PyObject.ob_type]
    jmp .isinstance_got_type

.isinstance_none:
    lea rdx, [rel none_type]
    jmp .isinstance_got_type

.isinstance_smallint:
    lea rdx, [rel int_type]
    jmp .isinstance_got_type

.isinstance_float:
    lea rdx, [rel float_type]
    jmp .isinstance_got_type

.isinstance_bool:
    lea rdx, [rel bool_type]

.isinstance_got_type:
    ; rdx = obj's type, rcx = type_to_check (may be tuple)
    ; Second arg must be TAG_PTR (type or tuple)
    cmp r9d, TAG_PTR
    jne .isinstance_type_error
    mov rax, [rcx + PyObject.ob_type]
    extern tuple_type
    lea r8, [rel tuple_type]
    cmp rax, r8
    je .isinstance_tuple
    ; Any class, including one built by a user metaclass.
    push rcx
    push rdx
    mov rdi, rcx
    extern type_check_is_class
    call type_check_is_class
    pop rdx
    pop rcx
    test eax, eax
    jz .isinstance_type_error

    ; A metaclass may define __instancecheck__ -- that is how ABCMeta makes
    ; isinstance() consult a registry rather than the MRO.
    push rcx
    push rdx
    mov rdi, rcx                ; the class
    mov rsi, [rbp - ISI_OBJ]    ; the object
    CSTRING rdx, "__instancecheck__"
    extern type_custom_check
    call type_custom_check
    pop rdx
    pop rcx
    cmp eax, -1
    je .isinstance_check
    test eax, eax
    jz .isinstance_false
    jmp .isinstance_true

.isinstance_check:
    ; The MRO, not the tp_base chain: a class with several bases is an
    ; instance of all of them.
    extern type_is_subtype
    mov rdi, rdx
    mov rsi, rcx
    call type_is_subtype
    test eax, eax
    jnz .isinstance_true
    jmp .isinstance_false

.isinstance_tuple:
    ; rcx = tuple of types. Check obj against each.
    mov rbx, rcx               ; rbx = tuple
    mov r12, rdx               ; r12 = obj's type (saved)
    mov rsi, [rbx + PyTupleObject.ob_item]  ; payloads
    mov rcx, [rbx + PyTupleObject.ob_size]
    xor r8d, r8d               ; index
.isinstance_tuple_loop:
    cmp r8, rcx
    jge .isinstance_false
    push rcx
    push r8
    push rsi
    push rsi                   ; keep the stack 16-byte aligned
    mov rdi, [rsi + r8*8]      ; the class from the tuple
    mov rsi, [rbp - ISI_OBJ]   ; the object
    CSTRING rdx, "__instancecheck__"
    call type_custom_check
    cmp eax, -1
    jne .isinstance_tuple_verdict
    mov rsi, [rsp]             ; the saved payload array
    mov r8, [rsp + 16]
    mov rdi, r12               ; obj's type
    mov rsi, [rsi + r8*8]      ; type from tuple
    call type_is_subtype
.isinstance_tuple_verdict:
    pop rsi
    pop rsi
    test eax, eax
    jnz .isinstance_tuple_match
    pop r8
    pop rcx
    inc r8
    jmp .isinstance_tuple_loop

.isinstance_tuple_match:
    add rsp, 16                ; pop saved r8, rcx
    jmp .isinstance_true

.isinstance_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isinstance_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.isinstance_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "isinstance() arg 2 must be a type, a tuple of types, or a union"
    call raise_exception

.isinstance_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "isinstance() takes 2 arguments"
    call raise_exception
END_FUNC builtin_isinstance

;; ============================================================================
;; builtin_issubclass(PyObject **args, int64_t nargs) -> PyObject*
;; issubclass(cls, parent) -> True/False
;; Walks the full tp_base chain for inheritance.
;; Supports tuple second arg: issubclass(cls, (type1, type2, ...))
;; ============================================================================
DEF_FUNC builtin_issubclass
    push rbx
    push r12
    push r13

    cmp rsi, 2
    jne .issubclass_error

    mov rdx, [rdi]             ; rdx = args[0] = cls
    V_UNPACK rdx, r8
    mov rcx, [rdi + 8]         ; rcx = args[1] = parent
    V_UNPACK rcx, r9

    ; Validate first arg is a type.  A user metaclass makes its instances
    ; classes too, so this is a subtype test, not three pointer compares.
    cmp r8d, TAG_PTR
    jne .issubclass_arg1_error
    push rcx
    push rdx
    push r9
    mov rdi, rdx
    call type_check_is_class
    pop r9
    pop rdx
    pop rcx
    test eax, eax
    jz .issubclass_arg1_error

    ; Check if second arg is a tuple
    cmp r9d, TAG_PTR
    jne .issubclass_arg2_error
    mov rax, [rcx + PyObject.ob_type]
    lea r10, [rel tuple_type]
    cmp rax, r10
    je .issubclass_tuple
    ; Validate second arg is a type
    push rcx
    push rdx
    mov rdi, rcx
    call type_check_is_class
    pop rdx
    pop rcx
    test eax, eax
    jz .issubclass_arg2_error

    ; Single type check.  A metaclass __subclasscheck__ -- ABCMeta's, above
    ; all -- decides before the MRO is walked, since a virtual subclass is
    ; not in anyone's MRO.
.issubclass_walk:
    push rcx
    push rdx
    mov rdi, rcx                ; the parent class
    mov rsi, rdx                ; the candidate subclass
    CSTRING rdx, "__subclasscheck__"
    call type_custom_check
    pop rdx
    pop rcx
    cmp eax, -1
    jne .issubclass_from_hook
    mov rdi, rdx
    mov rsi, rcx
    call type_is_subtype
    test eax, eax
    jnz .issubclass_true
    jmp .issubclass_false
.issubclass_from_hook:
    test eax, eax
    jnz .issubclass_true
    jmp .issubclass_false

.issubclass_tuple:
    ; rcx = tuple of types. Check cls against each.
    mov rbx, rcx               ; rbx = tuple
    mov r12, rdx               ; r12 = cls (saved)
    mov rsi, [rbx + PyTupleObject.ob_item]  ; payloads array
    mov r13, [rbx + PyTupleObject.ob_size]  ; count
    xor r8d, r8d               ; index
.issubclass_tuple_loop:
    cmp r8, r13
    jge .issubclass_false
    push rsi
    push r8
    mov rdi, [rsi + r8*8]      ; the parent from the tuple
    mov rsi, r12               ; cls
    CSTRING rdx, "__subclasscheck__"
    call type_custom_check
    cmp eax, -1
    jne .issubclass_tuple_verdict
    mov rax, [rsp + 8]         ; the saved payload array
    mov r8, [rsp]
    mov rdi, r12               ; cls
    mov rsi, [rax + r8*8]      ; type from tuple
    call type_is_subtype
.issubclass_tuple_verdict:
    test eax, eax
    jnz .issubclass_tuple_match
    pop r8
    pop rsi
    inc r8
    jmp .issubclass_tuple_loop

.issubclass_tuple_match:
    add rsp, 16               ; pop saved r8, rsi
    jmp .issubclass_true

.issubclass_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.issubclass_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.issubclass_arg1_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "issubclass() arg 1 must be a class"
    call raise_exception

.issubclass_arg2_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "issubclass() arg 2 must be a class, a tuple of classes, or a union"
    call raise_exception

.issubclass_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "issubclass() takes 2 arguments"
    call raise_exception
END_FUNC builtin_issubclass

;; ============================================================================
;; builtin_repr(PyObject **args, int64_t nargs) -> PyObject*
;; repr(obj)
;; ============================================================================
DEF_FUNC builtin_repr

    cmp rsi, 1
    jne .repr_error

    mov rdi, [rdi]             ; args[0]
    call obj_repr
    ; rdx = tag from obj_repr
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.repr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "repr() takes 1 argument"
    call raise_exception
END_FUNC builtin_repr

;; ============================================================================
;; builtin_bool(PyObject **args, int64_t nargs) -> PyObject*
;; bool()    -> False
;; bool(x)   -> True if x is truthy, False otherwise
;; ============================================================================
global builtin_bool
DEF_FUNC builtin_bool

    cmp rsi, 0
    je .bool_no_args
    cmp rsi, 1
    jne .bool_error

    ; bool(x) - test truthiness
    mov rdi, [rdi]             ; args[0]
    extern obj_is_true
    call obj_is_true           ; eax = 0 or 1
    test eax, eax
    jz .bool_ret_false
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.bool_ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.bool_no_args:
    ; bool() -> False
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.bool_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "bool() takes at most 1 argument"
    call raise_exception
END_FUNC builtin_bool

;; ============================================================================
;; builtin_float(PyObject **args, int64_t nargs) -> PyObject*
;; float()    -> 0.0
;; float(x)   -> convert x to float (int, float, or string)
;; ============================================================================
global builtin_float
BF_FRAME equ 32
DEF_FUNC builtin_float, BF_FRAME

    cmp rsi, 0
    je .float_no_args
    cmp rsi, 1
    jne .float_error

    ; float(x) - convert x
    mov rdi, [rdi]             ; args[0]
    V_UNPACK rdi, rsi

    ; TAG_FLOAT fast-path: already a float, return as-is
    cmp esi, TAG_FLOAT
    je .float_passthrough

    ; TAG_PTR: check for string
    cmp esi, TAG_PTR
    jne .float_numeric          ; non-pointer tag → numeric conversion

    ; Check if it's a string
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .float_from_str

    ; A class defining __float__ now carries nb_float; float_to_f64 below
    ; knows nothing about it and returned 0.0 for such an object.
    mov rcx, [rax + PyTypeObject.tp_as_number]
    test rcx, rcx
    jz .float_numeric
    mov rcx, [rcx + PyNumberMethods.nb_float]
    test rcx, rcx
    jz .float_numeric
    call rcx                    ; nb_float returns a Value
    mov rdi, rax
    V_UNPACK rdi, rdx
    cmp edx, TAG_FLOAT
    jne .float_numeric          ; not a float: let the generic path complain
    mov rax, rdi
    mov edx, TAG_FLOAT
    leave
    ret

.float_numeric:
    ; float_to_f64 answers 0.0 for anything it does not recognise, so
    ; float(None) and float([1]) quietly produced 0.0.
    push rdi
    push rsi
    extern binop_is_number
    call binop_is_number
    pop rsi
    pop rdi
    test eax, eax
    jz .float_bad_type

    extern float_to_f64
    call float_to_f64          ; xmm0 = double
    extern float_from_f64
    call float_from_f64        ; rax = double bits, edx = TAG_FLOAT

    leave
    ret

.float_bad_type:
    V_PACK rdi, rsi
    mov rsi, rdi
    CSTRING rdi, `float() argument must be a string or a real number, not '\x01'`
    extern raise_type_error_with_name
    call raise_type_error_with_name

.float_passthrough:
    mov rax, rdi
    mov edx, TAG_FLOAT
    leave
    ret

.float_from_str:
    ; rdi = PyStrObject*. Parse string → double via strtod.
    lea rdi, [rdi + PyStrObject.data]   ; rdi = null-terminated string data
    mov [rbp - 8], rdi                  ; save start ptr

    ; Call strtod(str, &endptr)
    extern strtod
    lea rsi, [rbp - 16]                ; &endptr at [rbp-16]
    call strtod wrt ..plt
    ; xmm0 = parsed value, [rbp-16] = endptr

    ; Check endptr > start (parsed something)
    mov rax, [rbp - 16]                ; endptr
    cmp rax, [rbp - 8]                 ; compare with start
    je .float_str_error                ; nothing parsed → error

    ; Skip trailing whitespace after parsed portion
.float_skip_ws:
    movzx ecx, byte [rax]
    cmp cl, ' '
    je .float_ws_next
    cmp cl, 9                          ; tab
    je .float_ws_next
    cmp cl, 10                         ; newline
    je .float_ws_next
    cmp cl, 13                         ; carriage return
    je .float_ws_next
    jmp .float_ws_done
.float_ws_next:
    inc rax
    jmp .float_skip_ws
.float_ws_done:
    cmp byte [rax], 0
    jne .float_str_error               ; trailing garbage → ValueError

    ; xmm0 still holds the strtod result
    call float_from_f64                ; rax = double bits, edx = TAG_FLOAT
    leave
    ret

.float_str_error:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "could not convert string to float"
    call raise_exception

.float_no_args:
    ; float() -> 0.0
    xorpd xmm0, xmm0
    call float_from_f64
    leave
    ret

.float_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "float() takes at most 1 argument"
    call raise_exception
END_FUNC builtin_float

;; ============================================================================
;; type.__new__(mcls, name, bases, ns) -> a new class whose metatype is mcls
;;
;; A metaclass __new__ almost always ends in
;; `super().__new__(mcls, name, bases, ns)`, and without this that resolved to
;; object.__new__ and produced an *instance* of the metaclass rather than a
;; class.  ABCMeta is written exactly that way, so abc.py depends on it.
;; ============================================================================
global type_method_new
DEF_FUNC type_method_new
    push rbx
    push r12
    cmp rsi, 4
    jne .tmn_error
    mov rbx, rdi                    ; args
    mov r12, [rdi]                  ; mcls

    mov rdi, [rbx + 8]              ; name
    mov rsi, [rbx + 16]             ; bases
    mov rdx, [rbx + 24]             ; namespace
    ; type_from_parts adopts a reference to each
    push rdi
    call obj_incref
    pop rdi
    push rdi
    mov rdi, rdx
    call obj_incref
    pop rdi
    mov rsi, [rbx + 16]
    mov rdx, [rbx + 24]
    call type_from_parts

    ; The metatype is whatever __new__ was handed, not the default.
    mov [rax + PyObject.ob_type], r12
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.tmn_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "type.__new__() takes exactly 3 arguments"
    call raise_exception
END_FUNC type_method_new

;; ============================================================================
;; type_from_parts(rdi = name str, rsi = bases tuple or NULL, rdx = namespace dict)
;;   -> rax = the new type object, one strong reference
;;
;; The heaptype construction shared by __build_class__ and the three-argument
;; type().  Extracted rather than duplicated: type() used to fall through to
;; type_call's .normal_type_call, which treats type_type as an ordinary class
;; -- it allocated an instance-sized block and let type fields be written into
;; it, printing <class ''> and then aborting with a double free.
;;
;; The frame is built by hand, not by DEF_FUNC's size argument, so that the
;; body's [rbp - TFP_BASE] slot keeps meaning what it meant inside __build_class__.
;; ============================================================================
global type_from_parts
DEF_FUNC type_from_parts
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24

TFP_BASE  equ 48            ; the layout base: the widest of the bases
TFP_BASES equ 56            ; the bases tuple, or NULL
    mov r14, rdi                ; class name str
    mov r15, rdx                ; namespace dict, becomes tp_dict
    mov [rbp - TFP_BASES], rsi

    ; The layout base is the widest base, not simply the first: `class
    ; C(Mixin, list)` has to be laid out as a list.  Ties go to the earlier
    ; base, which is what CPython's solid-base rule gives for the ordinary
    ; single-inheritance case.
    xor eax, eax                ; best base
    test rsi, rsi
    jz .tfp_base_done
    mov rcx, [rsi + PyTupleObject.ob_size]
    mov r8, [rsi + PyTupleObject.ob_item]
    xor r9, r9
    xor r10, r10                ; best basicsize
.tfp_base_scan:
    cmp r9, rcx
    jge .tfp_base_done
    mov r11, [r8 + r9*8]
    test r11, r11
    jz .tfp_base_next
    mov rdx, [r11 + PyTypeObject.tp_basicsize]
    cmp rdx, r10
    jbe .tfp_base_next
    mov r10, rdx
    mov rax, r11
.tfp_base_next:
    inc r9
    jmp .tfp_base_scan
.tfp_base_done:
    mov [rbp - TFP_BASE], rax   ; layout base, or NULL
    mov rdx, r15                ; restore namespace (scan clobbered rdx)

    ; Allocate the type object (GC-tracked)
    mov edi, TYPE_OBJECT_SIZE
    lea rsi, [rel user_type_metatype]
    call gc_alloc
    mov r12, rax            ; r12 = new type object (ob_refcnt=1, ob_type set)
    mov [rel build_class_pending], rax  ; register for exception cleanup

    ; Zero-fill the type object (skip ob_refcnt and ob_type, already set by gc_alloc)
    lea rdi, [r12 + 16]
    xor eax, eax
    mov ecx, (TYPE_OBJECT_SIZE - 16) / 8
    rep stosq

    ; tp_name: point to class_name string's data area
    lea rax, [r14 + PyStrObject.data]
    mov [r12 + PyTypeObject.tp_name], rax

    ; Instance layout.  A heaptype embeds its base's layout and puts its own
    ; __dict__ immediately after it, so both numbers come from the base:
    ; tp_dictoffset is the base's basicsize, and tp_basicsize is that plus
    ; the dict word.  With no base that yields 16 and 24 -- exactly
    ; PyInstanceObject, which is where those constants came from.
    ;
    ; A variable-size base such as str keeps its data inline, so there is no
    ; fixed offset past the header for a dict; those get none, as bytes and
    ; __slots__ classes already do.
    mov qword [r12 + PyTypeObject.tp_basicsize], PyInstanceObject_size
    mov qword [r12 + PyTypeObject.tp_dictoffset], PyInstanceObject.inst_dict
    mov rax, [rbp - TFP_BASE]               ; base class
    test rax, rax
    jz .bc_layout_done
    ; If the base already has a dict slot -- another heaptype, or an int
    ; subclass -- share it rather than adding a second one, which would
    ; collide with whatever the base put there.
    mov rcx, [rax + PyTypeObject.tp_dictoffset]
    test rcx, rcx
    jnz .bc_layout_inherit

    mov rcx, [rax + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_STR_SUBCLASS
    jnz .bc_layout_no_dict
    test rcx, TYPE_FLAG_INT_SUBCLASS
    jnz .bc_layout_done             ; int subclasses wrap rather than embed

    ; A builtin base with a fixed-size header: the dict goes just past it.
    mov rcx, [rax + PyTypeObject.tp_basicsize]
    test rcx, rcx
    jz .bc_layout_done
    mov [r12 + PyTypeObject.tp_dictoffset], rcx
    add rcx, 8
    mov [r12 + PyTypeObject.tp_basicsize], rcx
    jmp .bc_layout_done

.bc_layout_inherit:
    mov [r12 + PyTypeObject.tp_dictoffset], rcx
    mov rcx, [rax + PyTypeObject.tp_basicsize]
    mov [r12 + PyTypeObject.tp_basicsize], rcx
    jmp .bc_layout_done

.bc_layout_no_dict:
    mov qword [r12 + PyTypeObject.tp_dictoffset], 0
    mov rcx, [rax + PyTypeObject.tp_basicsize]
    mov [r12 + PyTypeObject.tp_basicsize], rcx

.bc_layout_done:

    ; Wire instance methods
    lea rax, [rel instance_dealloc]
    mov [r12 + PyTypeObject.tp_dealloc], rax

    lea rax, [rel instance_repr]
    mov [r12 + PyTypeObject.tp_repr], rax

    extern instance_str
    lea rax, [rel instance_str]
    mov [r12 + PyTypeObject.tp_str], rax

    ; tp_call left NULL: calling the type goes through metatype.tp_call (type_call).
    ; Calling instances falls through to __call__ dunder dispatch.

    lea rax, [rel instance_getattr]
    mov [r12 + PyTypeObject.tp_getattr], rax

    lea rax, [rel instance_setattr]
    mov [r12 + PyTypeObject.tp_setattr], rax

    ; tp_flags = HEAPTYPE | HAVE_GC (enables dunder dispatch fallbacks + GC tracking)
    mov qword [r12 + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE | TYPE_FLAG_HAVE_GC

    ; Set tp_traverse and tp_clear for GC cycle detection
    extern instance_traverse
    extern instance_clear
    lea rax, [rel instance_traverse]
    mov [r12 + PyTypeObject.tp_traverse], rax
    lea rax, [rel instance_clear]
    mov [r12 + PyTypeObject.tp_clear], rax

    ; tp_dict = class_dict (ownership transferred from r15, no INCREF needed)
    mov [r12 + PyTypeObject.tp_dict], r15

    ; __new__ is an implicit staticmethod.  Without the wrapper, looking it up
    ; through the class or through super() binds it like an ordinary method
    ; and prepends the instance, so `super().__new__(cls, *args)` arrived one
    ; argument too long -- which is exactly how every metaclass in the stdlib
    ; calls it.
    lea rdi, [rel bc_new_name]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    call dict_get
    mov rbx, rax                ; the current __new__, as a Value
    V_TEST_PTR rbx, rax
    ja .tfp_new_done
    test rbx, rbx
    jz .tfp_new_done
    mov rax, [rbx + PyObject.ob_type]
    extern func_type
    lea rcx, [rel func_type]
    cmp rax, rcx
    jne .tfp_new_done
    sub rsp, 16
    mov [rsp], rbx
    extern staticmethod_type
    extern staticmethod_construct
    lea rdi, [rel staticmethod_type]
    mov rsi, rsp
    mov edx, 1
    call staticmethod_construct
    V_UNPACK rax, rdx
    add rsp, 16
    test rax, rax
    jz .tfp_new_done
    mov rbx, rax
    mov rdi, r15
    mov rsi, [rsp]              ; the "__new__" key
    mov rdx, rbx
    call dict_set
    mov rdi, rbx
    call obj_decref             ; the dict holds it now
.tfp_new_done:
    pop rdi
    call obj_decref             ; the key

    ; A class statement's body sets __module__ itself; three-argument type()
    ; hands over a bare namespace, and without __module__ the repr comes out
    ; unqualified.  Fill it from the running frame's __name__, as CPython does.
    lea rdi, [rel bc_module_name]
    call str_from_cstr_heap
    push rax
    mov rdi, r15
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx
    test edx, edx
    jnz .bc_have_module
    extern eval_saved_r12
    mov rcx, [rel eval_saved_r12]
    test rcx, rcx
    jz .bc_have_module
    mov rcx, [rcx + PyFrame.globals]
    test rcx, rcx
    jz .bc_have_module
    lea rdi, [rel bc_dunder_name_name]
    call str_from_cstr_heap
    push rax
    mov rdi, [rel eval_saved_r12]
    mov rdi, [rdi + PyFrame.globals]
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx
    pop rdi
    push rax
    push rdx
    call obj_decref                 ; the "__name__" key
    pop rdx
    pop rax
    test edx, edx
    jz .bc_have_module
    mov rdi, r15
    mov rsi, [rsp]                  ; the "__module__" key
    mov rdx, rax
    call dict_set
.bc_have_module:
    pop rdi
    call obj_decref

    ; INCREF class_name (type object refers to it via tp_name)
    mov rdi, r14
    call obj_incref

    ; === Parse __slots__ from class_dict ===
    ; r12=type, r15=class_dict, [rbp - TFP_BASE]=base_class
    lea rdi, [rel bc_slots_name]
    call str_from_cstr_heap
    push rax                        ; save __slots__ str
    mov rdi, r15                    ; class_dict
    mov rsi, rax
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rdi                         ; __slots__ str
    push rdx                        ; save dict_get tag
    push rax                        ; save dict_get value
    call obj_decref                 ; DECREF __slots__ str
    pop rax                         ; value
    pop rdx                         ; tag
    test edx, edx
    jz .bc_no_slots

    ; Must be TAG_PTR and a tuple or list
    cmp edx, TAG_PTR
    jne .bc_no_slots
    extern tuple_type
    extern list_type
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel tuple_type]
    cmp rcx, rdx
    je .bc_slots_tuple
    lea rdx, [rel list_type]
    cmp rcx, rdx
    jne .bc_no_slots

    ; rax = slots list — get size and item pointers (same layout as tuple for ob_size/ob_item)
.bc_slots_tuple:
    ; rax = slots sequence (tuple or list, both have ob_size at same offset)
    mov rbx, rax                    ; rbx = slots sequence
    mov r13, [rbx + PyTupleObject.ob_size]  ; r13 = nslots (works for both)
    test r13, r13
    jz .bc_no_slots

    ; Determine base_basicsize
    ; Slots are laid out after the whole instance header, which is what
    ; tp_basicsize was just set to -- the base's layout plus the dict word.
    ; Using the *base's* basicsize instead puts the first slot on top of the
    ; dict pointer.
    mov rdi, [r12 + PyTypeObject.tp_basicsize]
.bc_have_basic:
    ; rdi = base_basicsize
    ; Set tp_basicsize = base_basicsize + nslots * 8 (one Value per slot)
    mov rax, r13
    shl rax, 3                      ; nslots * 8
    add rax, rdi                    ; + base_basicsize
    mov [r12 + PyTypeObject.tp_basicsize], rax

    ; Set TYPE_FLAG_HAS_SLOTS
    or qword [r12 + PyTypeObject.tp_flags], TYPE_FLAG_HAS_SLOTS

    ; Create member descriptors for each slot
    ; rbx = slots tuple, r13 = nslots, rdi = base_basicsize
    push rdi                        ; save base_basicsize
    xor edx, edx                    ; i = 0

.bc_slot_loop:
    cmp rdx, r13                    ; i < nslots?
    jge .bc_slots_done

    push rdx                        ; save i

    ; Get slot name: slots_tuple[i]
    mov rax, [rbx + PyTupleObject.ob_item]       ; payloads
    mov rcx, [rax + rdx*8]                        ; name payload
    V_UNPACK rcx, r8
    cmp r8d, TAG_PTR
    jne .bc_slot_skip               ; skip non-string slots

    ; Compute offset = base_basicsize + i * 8
    mov rdi, [rsp + 8]             ; base_basicsize
    mov rax, [rsp]                 ; i
    shl rax, 3
    add rdi, rax                   ; offset

    ; Create descriptor: member_descr_new(offset, name_str)
    mov rsi, rcx                   ; name string
    push rcx                       ; save name for dict_set
    INCREF rsi                     ; descriptor takes ownership
    extern member_descr_new
    call member_descr_new          ; rax = new descriptor

    ; Add to class_dict: dict_set(dict, name, descriptor, TAG_PTR, TAG_PTR)
    mov rdi, r15                   ; class_dict
    pop rsi                        ; name (key)
    mov rdx, rax                   ; descriptor (value)
    push rax                       ; save descriptor for DECREF
    call dict_set

    ; DECREF our ref on descriptor (dict now owns one via INCREF in dict_set)
    pop rdi
    call obj_decref

.bc_slot_skip:
    pop rdx                        ; restore i
    inc rdx
    jmp .bc_slot_loop

.bc_slots_done:
    pop rdi                        ; clean base_basicsize

.bc_no_slots:

    ; Look up "__init__" in class_dict for tp_init
    lea rdi, [rel bc_init_name]
    call str_from_cstr_heap
    push rax                ; save __init__ str obj

    mov rdi, r15            ; class_dict
    mov rsi, rax            ; "__init__" str
    call dict_get
    V_UNPACK rax, rdx           ; dict_get returns a Value
    mov rbx, rax            ; rbx = __init__ func or NULL

    ; DECREF the "__init__" string
    pop rdi
    call obj_decref

    ; Store tp_init (func ptr or 0)
    mov [r12 + PyTypeObject.tp_init], rbx

    ; Set tp_base: use explicit base class, or default to object_type
    mov rax, [rbp - TFP_BASE]
    test rax, rax
    jnz .bc_have_base
    lea rax, [rel object_type]
    mov [rbp - TFP_BASE], rax           ; update saved base for later use
.bc_have_base:
    mov [r12 + PyTypeObject.tp_base], rax
    mov rdi, rax
    call obj_incref

    ; tp_bases and tp_mro.  With these in place a lookup or a subclass test
    ; can see every base, not just the first.
    mov rax, [rbp - TFP_BASES]
    test rax, rax
    jnz .bc_have_bases_tuple
    ; No explicit bases: the linearization is (C, object).
    mov edi, 1
    extern tuple_new
    call tuple_new
    mov rcx, [rax + PyTupleObject.ob_item]
    lea rdx, [rel object_type]
    mov [rcx], rdx
    mov rdi, rdx
    mov [rbp - TFP_BASES], rax
    call obj_incref
    jmp .bc_bases_ready
.bc_have_bases_tuple:
    mov rdi, rax
    call obj_incref
.bc_bases_ready:
    mov rax, [rbp - TFP_BASES]
    mov [r12 + PyTypeObject.tp_bases], rax
    mov rdi, r12
    mov rsi, rax
    extern mro_compute
    call mro_compute
    mov [r12 + PyTypeObject.tp_mro], rax

    ; Inherit the family bits from every base, not only the layout one: a
    ; `class C(Mixin, list)` is still a list subclass.  The container bits
    ; were defined and set on the base types but never inherited at all, so
    ; nothing downstream could tell a list subclass from any other class.
    mov rax, [rbp - TFP_BASES]
    mov rcx, [rax + PyTupleObject.ob_size]
    mov r8, [rax + PyTupleObject.ob_item]
    xor r9, r9
    xor r10, r10
.bc_flag_scan:
    cmp r9, rcx
    jge .bc_flags_done
    mov r11, [r8 + r9*8]
    test r11, r11
    jz .bc_flag_next
    or r10, [r11 + PyTypeObject.tp_flags]
.bc_flag_next:
    inc r9
    jmp .bc_flag_scan
.bc_flags_done:
    and r10, TYPE_FLAG_INT_SUBCLASS | TYPE_FLAG_STR_SUBCLASS | \
             TYPE_FLAG_LIST_SUBCLASS | TYPE_FLAG_TUPLE_SUBCLASS | \
             TYPE_FLAG_DICT_SUBCLASS | TYPE_FLAG_SET_SUBCLASS
    or [r12 + PyTypeObject.tp_flags], r10

    ; A class deriving from `type` is a metatype: its instances are classes,
    ; so it uses type's attribute slots.  Leaving instance_getattr/setattr
    ; wired made `cls.x = 1` inside a metaclass __new__ walk tp_dictoffset on
    ; a PyTypeObject and write through a bogus offset.
    mov rdi, [rbp - TFP_BASE]
    test rdi, rdi
    jz .bc_not_metatype
    lea rsi, [rel type_type]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jz .bc_not_metatype
    extern type_getattr
    extern type_setattr
    lea rax, [rel type_getattr]
    mov [r12 + PyTypeObject.tp_getattr], rax
    lea rax, [rel type_setattr]
    mov [r12 + PyTypeObject.tp_setattr], rax
    mov qword [r12 + PyTypeObject.tp_dictoffset], 0
    mov qword [r12 + PyTypeObject.tp_basicsize], TYPE_OBJECT_SIZE
    ; Calling a metatype builds a class, so it needs type's tp_call, not the
    ; instance-constructing one a heaptype gets by default.
    extern type_call
    lea rax, [rel type_call]
    mov [r12 + PyTypeObject.tp_call], rax
.bc_not_metatype:

    ; If base is an exception type, inherit exception-compatible methods
    extern type_is_exc_subclass
    mov rdi, [rbp - TFP_BASE]
    call type_is_exc_subclass
    test eax, eax
    jz .bc_check_int_sub

    ; Exception subclass: override instance_* with exc_* methods
    extern exc_dealloc
    extern exc_repr
    extern exc_str
    lea rax, [rel exc_dealloc]
    mov [r12 + PyTypeObject.tp_dealloc], rax
    lea rax, [rel exc_repr]
    mov [r12 + PyTypeObject.tp_repr], rax
    lea rax, [rel exc_str]
    mov [r12 + PyTypeObject.tp_str], rax
    ; Exception getattr/setattr for custom attributes via exc_dict
    extern exc_getattr
    extern exc_setattr
    lea rax, [rel exc_getattr]
    mov [r12 + PyTypeObject.tp_getattr], rax
    lea rax, [rel exc_setattr]
    mov [r12 + PyTypeObject.tp_setattr], rax
    ; Wire exc traverse/clear for exception subclasses
    extern exc_traverse
    extern exc_clear_gc
    lea rax, [rel exc_traverse]
    mov [r12 + PyTypeObject.tp_traverse], rax
    lea rax, [rel exc_clear_gc]
    mov [r12 + PyTypeObject.tp_clear], rax
    jmp .bc_no_set_base

.bc_check_int_sub:
    ; Int subclass: inherit int-compatible repr/str and number methods
    mov rax, [r12 + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jz .bc_check_builtin_sub
    extern int_type
    mov rdi, [rel int_type + PyTypeObject.tp_repr]
    mov [r12 + PyTypeObject.tp_repr], rdi
    mov rdi, [rel int_type + PyTypeObject.tp_str]
    mov [r12 + PyTypeObject.tp_str], rdi
    mov rdi, [rel int_type + PyTypeObject.tp_as_number]
    mov [r12 + PyTypeObject.tp_as_number], rdi
    mov rdi, [rel int_type + PyTypeObject.tp_richcompare]
    mov [r12 + PyTypeObject.tp_richcompare], rdi
    mov rdi, [rel int_type + PyTypeObject.tp_hash]
    mov [r12 + PyTypeObject.tp_hash], rdi
    jmp .bc_no_set_base

.bc_check_builtin_sub:
    ; Inherit the base's constructor (tp_new) for bytearray, memoryview and
    ; bytes subclasses.
    mov rax, [rbp - TFP_BASE]              ; base class
    test rax, rax
    jz .bc_no_set_base

    ; Not for the container families.  Inheriting tp_new sends type_call
    ; straight to the base constructor, which returns a plain list (or
    ; tuple, dict, set) and never reaches __init__ -- so the subclass name
    ; was lost and its __init__ never ran.  Leaving tp_new NULL routes them
    ; through .normal_type_call, the same path an ordinary class takes.
    mov rcx, [r12 + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_LIST_SUBCLASS | TYPE_FLAG_TUPLE_SUBCLASS | \
              TYPE_FLAG_DICT_SUBCLASS | TYPE_FLAG_SET_SUBCLASS | \
              TYPE_FLAG_STR_SUBCLASS
    jnz .bc_container_sub
    mov rdi, [rax + PyTypeObject.tp_new]
    test rdi, rdi
    jz .bc_no_set_base
    ; Don't inherit object_type_call or type_call
    extern object_type_call
    lea rcx, [rel object_type_call]
    cmp rdi, rcx
    je .bc_no_set_base
    lea rcx, [rel type_call]
    cmp rdi, rcx
    je .bc_no_set_base
    ; Inherit the constructor from the base (for bytearray, etc.)
    mov [r12 + PyTypeObject.tp_new], rdi
    ; Use builtin_sub_dealloc instead of instance_dealloc
    ; (builtin subclasses don't have inst_dict at +16)
    extern builtin_sub_dealloc
    lea rax, [rel builtin_sub_dealloc]
    mov [r12 + PyTypeObject.tp_dealloc], rax

.bc_container_sub:
    ; Inherit the base's protocol slots.  These have no Python-level dunder
    ; that instance_getattr could route to, so a subclass with none of them
    ; is not a container at all: d["k"] = 1 raised, because a heaptype's
    ; tp_as_mapping is NULL.  type_install_slots runs after this and
    ; overrides whichever ones the class defines for itself.
    mov rax, [rbp - TFP_BASE]              ; base class
    mov rcx, [rax + PyTypeObject.tp_as_number]
    mov [r12 + PyTypeObject.tp_as_number], rcx
    mov rcx, [rax + PyTypeObject.tp_as_sequence]
    mov [r12 + PyTypeObject.tp_as_sequence], rcx
    mov rcx, [rax + PyTypeObject.tp_as_mapping]
    mov [r12 + PyTypeObject.tp_as_mapping], rcx
    mov rcx, [rax + PyTypeObject.tp_hash]
    mov [r12 + PyTypeObject.tp_hash], rcx
    mov rcx, [rax + PyTypeObject.tp_richcompare]
    mov [r12 + PyTypeObject.tp_richcompare], rcx
    mov rcx, [rax + PyTypeObject.tp_iter]
    mov [r12 + PyTypeObject.tp_iter], rcx
    mov rcx, [rax + PyTypeObject.tp_iternext]
    mov [r12 + PyTypeObject.tp_iternext], rcx

.bc_no_set_base:

    ; Fill the type's slots from the dunders it defines.  Until now a
    ; heaptype's tp_iter, tp_iternext, tp_hash, tp_call, tp_richcompare and
    ; tp_as_* were all left at zero, and every operation that wanted one had
    ; to grow its own dunder fallback -- or, more often, not.
    extern type_install_slots
    mov rdi, r12
    call type_install_slots

    ; Call parent's __init_subclass__ if present
    mov rax, [rbp - TFP_BASE]          ; base class
    test rax, rax
    jz .bc_no_init_subclass

    ; Look up __init_subclass__ on the base class (walk MRO)
    extern dunder_lookup
    mov rdi, rax               ; base class (as type)
    CSTRING rsi, "__init_subclass__"
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .bc_no_init_subclass

    ; Call __init_subclass__(new_class)
    ; rax = the dunder function (borrowed ref)
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .bc_no_init_subclass

    SPUSH_PTR r12              ; args[0] = new class
    mov rdi, rax               ; callable = __init_subclass__ func
    mov rsi, rsp               ; args
    mov edx, 1                 ; nargs = 1
    call rcx
    V_UNPACK rax, rdx           ; tp_call returns a Value
    add rsp, 16                ; pop fat args
    ; DECREF result if non-NULL
    test edx, edx               ; the tag, not the payload: a hit may be int 0
    jz .bc_no_init_subclass
    mov rdi, rax
    call obj_decref

.bc_no_init_subclass:

    ; Handle __classcell__: look in class_dict for the cell, set its ob_ref to the new type
    lea rdi, [rel bc_classcell_name]
    call str_from_cstr_heap
    push rax                ; save key str
    mov rdi, r15            ; class_dict
    mov rsi, rax
    call dict_get           ; returns cell or NULL
    V_UNPACK rax, rdx           ; dict_get returns a Value
    pop rdi                 ; key str
    push rdx                ; save dict_get tag
    push rax                ; save cell payload
    call obj_decref         ; DECREF key str
    pop rax                 ; restore cell payload
    pop rdx                 ; restore dict_get tag
    test edx, edx
    jz .bc_no_classcell
    ; cell.ob_ref = new type (r12), with tag
    mov [rax + PyCellObject.ob_ref], r12        ; a type pointer is its own Value
    mov rdi, r12
    call obj_incref         ; cell holds a ref to the type
.bc_no_classcell:

    ; Track the type object in GC
    extern gc_track
    mov rdi, r12
    call gc_track

    ; Return the new type object - clear pending flag first
    mov qword [rel build_class_pending], 0
    mov rax, r12

    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_from_parts

;; ============================================================================
;; builtin___build_class__(PyObject **args, int64_t nargs) -> PyObject*
;; __build_class__(body_func, class_name, *bases)
;;
;; 1. body_func = args[0], class_name = args[1]
;; 2. Create a class dict
;; 3. Execute body_func with class_dict as locals
;; 4. Create a new type object with class_dict as tp_dict
;; 5. Return the new type
;; ============================================================================
DEF_FUNC builtin___build_class__
    push rbx
    push r12
    push r13
    push r14
    push r15
BCL_BASES equ 48        ; the bases tuple built from args[2:]
BCL_META  equ 56        ; the metaclass= keyword, or 0
BCL_NPOS  equ 64        ; positional arg count (nargs minus the keywords)
    sub rsp, 48

    ; Check nargs >= 2
    cmp rsi, 2
    jl .build_class_error

    mov rbx, rdi            ; rbx = args
    mov qword [rbp - BCL_META], 0
    mov [rbp - BCL_NPOS], rsi

    ; `class C(metaclass=M)` passes M as a keyword, and it arrives in the
    ; positional array with its name in kw_names_pending.  Without splitting
    ; them off, M was treated as a *base* -- which is why metaclass= appeared
    ; to be ignored: the metatype never got a chance to run.
    mov rax, [rel kw_names_pending]
    test rax, rax
    jz .bc_no_kwargs
    mov rcx, [rax + PyTupleObject.ob_size]
    sub [rbp - BCL_NPOS], rcx
    mov r8, [rax + PyTupleObject.ob_item]
    xor r9d, r9d
.bc_kw_loop:
    cmp r9, rcx
    jge .bc_no_kwargs
    mov rdx, [r8 + r9*8]            ; the keyword name
    push rcx
    push r8
    push r9
    lea rdi, [rdx + PyStrObject.data]
    CSTRING rsi, "metaclass"
    call ap_strcmp
    pop r9
    pop r8
    pop rcx
    test eax, eax
    jne .bc_kw_next
    mov rdx, [rbp - BCL_NPOS]
    add rdx, r9
    mov rax, [rbx + rdx*8]          ; its value
    mov [rbp - BCL_META], rax
.bc_kw_next:
    inc r9
    jmp .bc_kw_loop
.bc_no_kwargs:
    ; Consumed: anything we call from here on must not see them again.
    mov qword [rel kw_names_pending], 0
    mov rsi, [rbp - BCL_NPOS]
    ; r12 will be used later for the type object

    ; Collect every base into a tuple.  Only args[2] used to be read, so
    ; `class C(A, B)` silently produced a class that had never heard of B.
    xor eax, eax
    mov [rbp - BCL_BASES], rax
    cmp rsi, 3
    jl .bc_no_base
    push rsi
    lea rdi, [rsi - 2]      ; nbases
    extern tuple_new
    call tuple_new
    pop rsi
    mov [rbp - BCL_BASES], rax
    mov r8, [rax + PyTupleObject.ob_item]
    xor r9, r9
.bc_base_copy:
    lea rcx, [r9 + 2]
    cmp rcx, rsi
    jge .bc_no_base
    mov rdx, [rbx + rcx*8]
    ; A base must be a class.  `class C(1)` used to store and INCREF the
    ; integer, and mro_compute then walked tp_base off it.
    push rsi
    push r8
    push r9
    push rdx
    mov rdi, rdx
    extern type_check_is_class
    call type_check_is_class
    pop rdx
    pop r9
    pop r8
    pop rsi
    test eax, eax
    jz .build_class_base_error
    ; Prevent subclassing bool
    extern bool_type
    lea rcx, [rel bool_type]
    cmp rdx, rcx
    je .build_class_bool_error
    mov [r8 + r9*8], rdx
    push rsi
    push r8
    push r9
    mov rdi, rdx
    call obj_incref
    pop r9
    pop r8
    pop rsi
    inc r9
    jmp .bc_base_copy

.bc_no_base:

    mov r13, [rbx]          ; r13 = body_func (args[0])
    mov r14, [rbx + 8]     ; r14 = class_name (args[1])

    ; Create class dict (will become tp_dict)
    call dict_new
    mov r15, rax            ; r15 = class_dict

    ; Execute body function with class_dict as locals
    ; frame_new(code, globals, builtins, locals)
    mov rdi, [r13 + PyFuncObject.func_code]     ; code from body func
    mov rsi, [r13 + PyFuncObject.func_globals]  ; globals from body func
    mov rdx, [rel builtins_dict_global]         ; builtins dict
    mov rcx, r15                                ; class_dict as locals
    call frame_new
    mov r12, rax            ; r12 = new frame

    ; Store body function in frame for COPY_FREE_VARS (closure support)
    mov [r12 + PyFrame.func_obj], r13

    ; eval_frame(frame)
    mov rdi, r12
    call eval_frame
    V_UNPACK rax, rdx           ; eval_frame returns a Value
    ; A class body that raised returns NULL with current_exception set.  The
    ; same omission the module-body path had: the exception was left pending
    ; and the class built anyway, so `class C: raise X` inside a try/except
    ; produced a class *and* an error reported somewhere else entirely.
    test edx, edx
    jnz .bc_body_ok
    extern current_exception
    cmp qword [rel current_exception], 0
    jne .bc_body_raised
.bc_body_ok:
    ; DECREF return value (should be None — TAG_NONE, not a pointer)
    mov rsi, rdx
    DECREF_VAL rax, rsi

    ; Free the frame
    mov rdi, r12
    call frame_free

    ; A metaclass is inherited: `class D(C)` where type(C) is M gives D the
    ; metatype M as well.  CPython picks the most derived metatype among the
    ; bases; the winner is the one that is a subtype of every other, and
    ; starting from `type` makes an ordinary base contribute nothing.
    cmp qword [rbp - BCL_META], 0
    jne .bc_metaclass_settled
    mov rcx, [rbp - BCL_BASES]
    test rcx, rcx
    jz .bc_metaclass_settled
    lea r8, [rel type_type]                 ; r8 = winner so far
    mov r9, [rcx + PyTupleObject.ob_size]
    mov r10, [rcx + PyTupleObject.ob_item]
    xor r11d, r11d
.bc_meta_scan:
    cmp r11, r9
    jge .bc_meta_scan_done
    mov rdi, [r10 + r11*8]
    V_TEST_PTR rdi, rax
    ja .bc_meta_scan_next
    test rdi, rdi
    jz .bc_meta_scan_next
    mov rdi, [rdi + PyObject.ob_type]       ; the base's metatype
    cmp rdi, r8
    je .bc_meta_scan_next
    push r8
    push r9
    push r10
    push r11
    mov rsi, r8
    call type_is_subtype                    ; is it more derived than the winner?
    pop r11
    pop r10
    pop r9
    pop r8
    test eax, eax
    jz .bc_meta_scan_next
    mov rdi, [r10 + r11*8]
    mov r8, [rdi + PyObject.ob_type]
.bc_meta_scan_next:
    inc r11
    jmp .bc_meta_scan
.bc_meta_scan_done:
    ; The three built-in metatypes go through type_from_parts as before --
    ; they have no __new__ of their own to run.
    lea rax, [rel type_type]
    cmp r8, rax
    je .bc_metaclass_settled
    extern user_type_metatype
    lea rax, [rel user_type_metatype]
    cmp r8, rax
    je .bc_metaclass_settled
    extern exc_metatype
    lea rax, [rel exc_metatype]
    cmp r8, rax
    je .bc_metaclass_settled
    mov [rbp - BCL_META], r8

.bc_metaclass_settled:
    ; With a metaclass, CPython calls meta(name, bases, ns) rather than
    ; building the type itself -- that is what runs M.__new__ and
    ; M.__init__, and what makes type(C) be M.
    cmp qword [rbp - BCL_META], 0
    je .bc_no_metaclass
    mov rdi, [rbp - BCL_META]
    extern type_check_is_class
    push rdi
    call type_check_is_class
    pop rdi
    test eax, eax
    jz .bc_no_metaclass

    ; meta(name, bases, ns) through its type's tp_call
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .bc_no_metaclass
    sub rsp, 32
    mov [rsp], r14                      ; name
    mov rcx, [rbp - BCL_BASES]
    test rcx, rcx
    jnz .bc_meta_have_bases
    push rax
    push rdi
    xor edi, edi
    call tuple_new
    mov rcx, rax
    mov [rbp - BCL_BASES], rax
    pop rdi
    pop rax
.bc_meta_have_bases:
    mov [rsp + 8], rcx                  ; bases
    mov [rsp + 16], r15                 ; namespace
    mov rsi, rsp
    mov edx, 3
    call rax
    V_UNPACK rax, rdx
    add rsp, 32
    push rax
    mov rdi, [rbp - BCL_BASES]
    test rdi, rdi
    jz .bc_meta_done
    call obj_decref
.bc_meta_done:
    pop rax
    jmp .bc_have_class

.bc_no_metaclass:
    ; Build the heaptype from (name, bases, namespace); the three-argument
    ; type() reaches the same code.
    mov rdi, r14
    mov rsi, [rbp - BCL_BASES]
    mov rdx, r15
    call type_from_parts
    push rax
    mov rdi, [rbp - BCL_BASES]
    test rdi, rdi
    jz .bc_bases_released
    call obj_decref         ; type_from_parts took its own reference
.bc_bases_released:
    pop rax

.bc_have_class:
    add rsp, 48
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret


.build_class_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__build_class__ requires 2+ arguments"
    call raise_exception

.bc_body_raised:
    ; Release the frame and the namespace, then let the body's exception
    ; keep unwinding in the caller's frame.
    mov rdi, r12
    call frame_free
    mov rdi, r15
    call obj_decref
    mov rdi, [rbp - BCL_BASES]
    test rdi, rdi
    jz .bc_body_raised_go
    call obj_decref
.bc_body_raised_go:
    extern eval_exception_unwind
    jmp eval_exception_unwind

.build_class_base_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "bases must be types"
    call raise_exception

.build_class_bool_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "type 'bool' is not an acceptable base type"
    call raise_exception
END_FUNC builtin___build_class__

section .rodata
bc_init_name: db "__init__", 0
bc_module_name: db "__module__", 0
bc_dunder_name_name: db "__name__", 0
bc_classcell_name: db "__classcell__", 0
bc_slots_name: db "__slots__", 0
section .text

;; ============================================================================
;; Helper: add_builtin(dict, name_cstr, func_ptr)
;; Adds a builtin to the dict. Used by builtins_init.
;; rdi=dict, rsi=name_cstr, rdx=func_ptr
;; ============================================================================
DEF_FUNC_LOCAL add_builtin
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; dict
    mov r12, rsi               ; name_cstr
    mov r13, rdx               ; func_ptr

    ; Create function wrapper
    mov rdi, r13
    mov rsi, r12
    call builtin_func_new
    push rax                   ; save func obj

    ; Create key string (heap — used as dict key, then DECREFed)
    mov rdi, r12
    call str_from_cstr_heap
    push rax                   ; save key

    ; dict_set
    mov rdi, rbx
    mov rsi, rax               ; key
    mov rdx, [rsp + 8]        ; func obj
    call dict_set

    ; DECREF key and value
    pop rdi                    ; key
    call obj_decref
    pop rdi                    ; func obj
    call obj_decref

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC add_builtin

;; Helper: add_builtin_type(dict, name_cstr, type_obj, tp_call_fn)
;; Registers a type object directly in builtins (for isinstance to work).
;; Sets type_obj.tp_call = tp_call_fn so the type is callable.
;; rdi=dict, rsi=name_cstr, rdx=type_obj, rcx=tp_call_fn
DEF_FUNC_LOCAL add_builtin_type
    push rbx
    push r12

    mov rbx, rdi               ; dict
    mov r12, rdx               ; type_obj

    ; Install the constructor in tp_new.  It must NOT go in tp_call: tp_call
    ; on a type governs whether that type's INSTANCES are callable.
    mov [r12 + PyTypeObject.tp_new], rcx

    ; Create key string (heap — used as dict key, then DECREFed)
    push r12
    mov rdi, rsi
    call str_from_cstr_heap
    mov rcx, rax               ; key str
    V_PACK rdx, rcx

    ; dict_set(dict, key, type_obj)
    mov rdi, rbx
    mov rsi, rcx
    pop rdx                    ; type_obj
    push rcx                   ; save key for DECREF
    call dict_set

    ; DECREF key
    pop rdi
    call obj_decref

    pop r12
    pop rbx
    leave
    ret
END_FUNC add_builtin_type

;; ============================================================================
;; builtins_init() -> PyDictObject*
;; Create and populate the builtins dictionary
;; ============================================================================
DEF_FUNC builtins_init
    push rbx

    ; Initialize iterator types (patches list/tuple tp_iter)
    call init_iter_types

    ; Create the builtins dict
    call dict_new
    mov rbx, rax                ; rbx = builtins dict

    ; Store globally for __build_class__ to access
    mov [rel builtins_dict_global], rbx

    ; Create __build_class__ wrapper and store globally
    lea rdi, [rel builtin___build_class__]
    lea rsi, [rel bi_name_build_class]
    call builtin_func_new
    mov [rel build_class_obj], rax

    ; Register __build_class__ in builtins dict
    mov rdi, rbx
    lea rsi, [rel bi_name_build_class]
    lea rdx, [rel builtin___build_class__]
    call add_builtin

    ; Add builtins using helper
    mov rdi, rbx
    lea rsi, [rel bi_name_print]
    lea rdx, [rel builtin_print]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_len]
    lea rdx, [rel builtin_len]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_range]
    extern range_obj_type
    extern range_type_call
    lea rdx, [rel range_obj_type]
    lea rcx, [rel range_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_type]
    lea rdx, [rel type_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_isinstance]
    lea rdx, [rel builtin_isinstance]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_issubclass]
    lea rdx, [rel builtin_issubclass]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_repr]
    lea rdx, [rel builtin_repr]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_float]
    lea rdx, [rel float_type]
    lea rcx, [rel float_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_bool]
    lea rdx, [rel bool_type]
    lea rcx, [rel bool_type_call]
    call add_builtin_type

    extern object_type
    extern object_type_call
    mov rdi, rbx
    lea rsi, [rel bi_name_object]
    lea rdx, [rel object_type]
    lea rcx, [rel object_type_call]
    call add_builtin_type

    ; Register new builtins (from builtins_extra.asm)
    mov rdi, rbx
    lea rsi, [rel bi_name_abs]
    lea rdx, [rel builtin_abs]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_divmod]
    lea rdx, [rel builtin_divmod]
    call add_builtin

    ; Register int as the int_type object (not a function wrapper)
    ; so isinstance(42, int) works correctly
    mov rdi, rbx
    lea rsi, [rel bi_name_int]
    lea rdx, [rel int_type]
    lea rcx, [rel int_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_str]
    lea rdx, [rel str_type]
    lea rcx, [rel str_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_ord]
    lea rdx, [rel builtin_ord]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_chr]
    lea rdx, [rel builtin_chr]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_hex]
    lea rdx, [rel builtin_hex]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_id]
    lea rdx, [rel builtin_id]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_hash]
    lea rdx, [rel builtin_hash_fn]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_callable]
    lea rdx, [rel builtin_callable]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_iter]
    lea rdx, [rel builtin_iter_fn]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_next]
    lea rdx, [rel builtin_next_fn]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_any]
    lea rdx, [rel builtin_any]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_all]
    lea rdx, [rel builtin_all]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_sum]
    lea rdx, [rel builtin_sum]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_min]
    lea rdx, [rel builtin_min]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_max]
    lea rdx, [rel builtin_max]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_getattr]
    lea rdx, [rel builtin_getattr]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_hasattr]
    lea rdx, [rel builtin_hasattr]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_setattr]
    lea rdx, [rel builtin_setattr]
    call add_builtin

    ; Register iterator builtins (from itertools.asm)
    extern enumerate_iter_type
    extern enumerate_type_call
    mov rdi, rbx
    lea rsi, [rel bi_name_enumerate]
    lea rdx, [rel enumerate_iter_type]
    lea rcx, [rel enumerate_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_zip]
    extern zip_iter_type
    extern zip_type_call
    lea rdx, [rel zip_iter_type]
    lea rcx, [rel zip_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_map]
    extern map_iter_type
    extern map_type_call
    lea rdx, [rel map_iter_type]
    lea rcx, [rel map_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_filter]
    extern filter_iter_type
    extern filter_type_call
    lea rdx, [rel filter_iter_type]
    lea rcx, [rel filter_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_reversed]
    extern reversed_iter_type
    extern reversed_type_call
    lea rdx, [rel reversed_iter_type]
    lea rcx, [rel reversed_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_sorted]
    lea rdx, [rel builtin_sorted]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_chain]
    lea rdx, [rel builtin_chain]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_globals]
    lea rdx, [rel builtin_globals]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_locals]
    lea rdx, [rel builtin_locals]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_dir]
    lea rdx, [rel builtin_dir]
    call add_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_breakpoint]
    lea rdx, [rel builtin_breakpoint]
    call add_builtin

    ; Register super type as builtin (LOAD_SUPER_ATTR needs it loadable)
    mov rdi, rbx
    lea rsi, [rel bi_name_super]
    lea rdx, [rel super_type]
    call add_exc_type_builtin

    ; Register descriptor types as builtins
    mov rdi, rbx
    lea rsi, [rel bi_name_staticmethod]
    lea rdx, [rel staticmethod_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_classmethod]
    lea rdx, [rel classmethod_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_property]
    lea rdx, [rel property_type]
    call add_exc_type_builtin

    ; Register NotImplemented singleton as builtin constant
    extern notimpl_singleton
    mov rdi, rbx
    lea rsi, [rel bi_name_NotImplemented]
    lea rdx, [rel notimpl_singleton]
    call add_exc_type_builtin

    ; Register Ellipsis singleton as builtin constant
    extern ellipsis_singleton
    mov rdi, rbx
    lea rsi, [rel bi_name_Ellipsis]
    lea rdx, [rel ellipsis_singleton]
    call add_exc_type_builtin

    ; Register exception types as builtins
    mov rdi, rbx
    lea rsi, [rel bi_name_BaseException]
    lea rdx, [rel exc_BaseException_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_Exception]
    lea rdx, [rel exc_Exception_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_TypeError]
    lea rdx, [rel exc_TypeError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ValueError]
    lea rdx, [rel exc_ValueError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_KeyError]
    lea rdx, [rel exc_KeyError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_IndexError]
    lea rdx, [rel exc_IndexError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_AttributeError]
    lea rdx, [rel exc_AttributeError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_NameError]
    lea rdx, [rel exc_NameError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_UnboundLocalError]
    lea rdx, [rel exc_UnboundLocalError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_RuntimeError]
    lea rdx, [rel exc_RuntimeError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_StopIteration]
    lea rdx, [rel exc_StopIteration_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ZeroDivisionError]
    lea rdx, [rel exc_ZeroDivisionError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_NotImplementedError]
    lea rdx, [rel exc_NotImplementedError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_OverflowError]
    lea rdx, [rel exc_OverflowError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_AssertionError]
    lea rdx, [rel exc_AssertionError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_OSError]
    lea rdx, [rel exc_OSError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_LookupError]
    lea rdx, [rel exc_LookupError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ArithmeticError]
    lea rdx, [rel exc_ArithmeticError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_RecursionError]
    lea rdx, [rel exc_RecursionError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ImportError]
    lea rdx, [rel exc_ImportError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_MemoryError]
    lea rdx, [rel exc_MemoryError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_KeyboardInterrupt]
    lea rdx, [rel exc_KeyboardInterrupt_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_SystemExit]
    lea rdx, [rel exc_SystemExit_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeError]
    lea rdx, [rel exc_UnicodeError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_Warning]
    lea rdx, [rel exc_Warning_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_DeprecationWarning]
    lea rdx, [rel exc_DeprecationWarning_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_UserWarning]
    lea rdx, [rel exc_UserWarning_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_BaseExceptionGroup]
    lea rdx, [rel exc_BaseExceptionGroup_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_ExceptionGroup]
    lea rdx, [rel exc_ExceptionGroup_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_CancelledError]
    lea rdx, [rel exc_CancelledError_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_StopAsyncIteration]
    lea rdx, [rel exc_StopAsyncIteration_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_TimeoutError]
    lea rdx, [rel exc_TimeoutError_type]
    call add_exc_type_builtin

    extern exc_GeneratorExit_type
    mov rdi, rbx
    lea rsi, [rel bi_name_GeneratorExit]
    lea rdx, [rel exc_GeneratorExit_type]
    call add_exc_type_builtin

    extern exc_ModuleNotFoundError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ModuleNotFoundError]
    lea rdx, [rel exc_ModuleNotFoundError_type]
    call add_exc_type_builtin

    extern exc_SyntaxError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_SyntaxError]
    lea rdx, [rel exc_SyntaxError_type]
    call add_exc_type_builtin

    extern exc_IndentationError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_IndentationError]
    lea rdx, [rel exc_IndentationError_type]
    call add_exc_type_builtin

    extern exc_TabError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_TabError]
    lea rdx, [rel exc_TabError_type]
    call add_exc_type_builtin

    extern exc_EOFError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_EOFError]
    lea rdx, [rel exc_EOFError_type]
    call add_exc_type_builtin

    extern exc_UnicodeDecodeError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeDecodeError]
    lea rdx, [rel exc_UnicodeDecodeError_type]
    call add_exc_type_builtin

    extern exc_UnicodeEncodeError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeEncodeError]
    lea rdx, [rel exc_UnicodeEncodeError_type]
    call add_exc_type_builtin

    extern exc_ConnectionError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ConnectionError]
    lea rdx, [rel exc_ConnectionError_type]
    call add_exc_type_builtin

    extern exc_ConnectionResetError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ConnectionResetError]
    lea rdx, [rel exc_ConnectionResetError_type]
    call add_exc_type_builtin

    extern exc_ConnectionRefusedError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ConnectionRefusedError]
    lea rdx, [rel exc_ConnectionRefusedError_type]
    call add_exc_type_builtin

    extern exc_ConnectionAbortedError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ConnectionAbortedError]
    lea rdx, [rel exc_ConnectionAbortedError_type]
    call add_exc_type_builtin

    extern exc_BrokenPipeError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_BrokenPipeError]
    lea rdx, [rel exc_BrokenPipeError_type]
    call add_exc_type_builtin

    extern exc_PermissionError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_PermissionError]
    lea rdx, [rel exc_PermissionError_type]
    call add_exc_type_builtin

    extern exc_IsADirectoryError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_IsADirectoryError]
    lea rdx, [rel exc_IsADirectoryError_type]
    call add_exc_type_builtin

    extern exc_NotADirectoryError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_NotADirectoryError]
    lea rdx, [rel exc_NotADirectoryError_type]
    call add_exc_type_builtin

    extern exc_ProcessLookupError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ProcessLookupError]
    lea rdx, [rel exc_ProcessLookupError_type]
    call add_exc_type_builtin

    extern exc_ChildProcessError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ChildProcessError]
    lea rdx, [rel exc_ChildProcessError_type]
    call add_exc_type_builtin

    extern exc_BlockingIOError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_BlockingIOError]
    lea rdx, [rel exc_BlockingIOError_type]
    call add_exc_type_builtin

    extern exc_InterruptedError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_InterruptedError]
    lea rdx, [rel exc_InterruptedError_type]
    call add_exc_type_builtin

    extern exc_FloatingPointError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_FloatingPointError]
    lea rdx, [rel exc_FloatingPointError_type]
    call add_exc_type_builtin

    extern exc_BufferError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_BufferError]
    lea rdx, [rel exc_BufferError_type]
    call add_exc_type_builtin

    extern exc_ReferenceError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ReferenceError]
    lea rdx, [rel exc_ReferenceError_type]
    call add_exc_type_builtin

    extern exc_SystemError_type
    mov rdi, rbx
    lea rsi, [rel bi_name_SystemError]
    lea rdx, [rel exc_SystemError_type]
    call add_exc_type_builtin

    extern exc_RuntimeWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_RuntimeWarning]
    lea rdx, [rel exc_RuntimeWarning_type]
    call add_exc_type_builtin

    extern exc_FutureWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_FutureWarning]
    lea rdx, [rel exc_FutureWarning_type]
    call add_exc_type_builtin

    extern exc_ImportWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ImportWarning]
    lea rdx, [rel exc_ImportWarning_type]
    call add_exc_type_builtin

    extern exc_UnicodeWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_UnicodeWarning]
    lea rdx, [rel exc_UnicodeWarning_type]
    call add_exc_type_builtin

    extern exc_ResourceWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_ResourceWarning]
    lea rdx, [rel exc_ResourceWarning_type]
    call add_exc_type_builtin

    extern exc_BytesWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_BytesWarning]
    lea rdx, [rel exc_BytesWarning_type]
    call add_exc_type_builtin

    extern exc_PendingDeprecationWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_PendingDeprecationWarning]
    lea rdx, [rel exc_PendingDeprecationWarning_type]
    call add_exc_type_builtin

    extern exc_SyntaxWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_SyntaxWarning]
    lea rdx, [rel exc_SyntaxWarning_type]
    call add_exc_type_builtin

    extern exc_EncodingWarning_type
    mov rdi, rbx
    lea rsi, [rel bi_name_EncodingWarning]
    lea rdx, [rel exc_EncodingWarning_type]
    call add_exc_type_builtin

    ; Register data types as builtins
    mov rdi, rbx
    lea rsi, [rel bi_name_list]
    lea rdx, [rel list_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_dict]
    lea rdx, [rel dict_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_tuple]
    lea rdx, [rel tuple_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_set]
    lea rdx, [rel set_type]
    call add_exc_type_builtin

    mov rdi, rbx
    lea rsi, [rel bi_name_bytes]
    lea rdx, [rel bytes_type]
    extern bytes_type_call
    lea rcx, [rel bytes_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_bytearray]
    lea rdx, [rel bytearray_type]
    lea rcx, [rel bytearray_type_call]
    call add_builtin_type

    mov rdi, rbx
    lea rsi, [rel bi_name_memoryview]
    lea rdx, [rel memoryview_type]
    lea rcx, [rel memoryview_type_call]
    call add_builtin_type

    ; eval
    mov rdi, rbx
    lea rsi, [rel bi_name_eval]
    extern builtin_eval_fn
    lea rdx, [rel builtin_eval_fn]
    call add_builtin

    ; compile
    mov rdi, rbx
    lea rsi, [rel bi_name_compile]
    extern builtin_compile_fn
    lea rdx, [rel builtin_compile_fn]
    call add_builtin

    ; round
    mov rdi, rbx
    lea rsi, [rel bi_name_round]
    extern builtin_round_fn
    lea rdx, [rel builtin_round_fn]
    call add_builtin

    ; pow
    mov rdi, rbx
    lea rsi, [rel bi_name_pow]
    extern builtin_pow_fn
    lea rdx, [rel builtin_pow_fn]
    call add_builtin

    ; input
    mov rdi, rbx
    lea rsi, [rel bi_name_input]
    extern builtin_input_fn
    lea rdx, [rel builtin_input_fn]
    call add_builtin

    ; open
    mov rdi, rbx
    lea rsi, [rel bi_name_open]
    extern builtin_open_fn
    lea rdx, [rel builtin_open_fn]
    call add_builtin

    ; bin
    mov rdi, rbx
    lea rsi, [rel bi_name_bin]
    extern builtin_bin
    lea rdx, [rel builtin_bin]
    call add_builtin

    ; oct
    mov rdi, rbx
    lea rsi, [rel bi_name_oct]
    extern builtin_oct
    lea rdx, [rel builtin_oct]
    call add_builtin

    ; ascii
    mov rdi, rbx
    lea rsi, [rel bi_name_ascii]
    extern builtin_ascii_fn
    lea rdx, [rel builtin_ascii_fn]
    call add_builtin

    ; format
    mov rdi, rbx
    lea rsi, [rel bi_name_format]
    extern builtin_format_fn
    lea rdx, [rel builtin_format_fn]
    call add_builtin

    ; vars
    mov rdi, rbx
    lea rsi, [rel bi_name_vars]
    extern builtin_vars_fn
    lea rdx, [rel builtin_vars_fn]
    call add_builtin

    ; delattr
    mov rdi, rbx
    lea rsi, [rel bi_name_delattr]
    extern builtin_delattr_fn
    lea rdx, [rel builtin_delattr_fn]
    call add_builtin

    ; aiter
    mov rdi, rbx
    lea rsi, [rel bi_name_aiter]
    extern builtin_aiter_fn
    lea rdx, [rel builtin_aiter_fn]
    call add_builtin

    ; anext
    mov rdi, rbx
    lea rsi, [rel bi_name_anext]
    extern builtin_anext_fn
    lea rdx, [rel builtin_anext_fn]
    call add_builtin

    ; __import__
    mov rdi, rbx
    lea rsi, [rel bi_name___import__]
    extern builtin_import_fn
    lea rdx, [rel builtin_import_fn]
    call add_builtin

    ; slice
    mov rdi, rbx
    lea rsi, [rel bi_name_slice]
    extern slice_type
    extern slice_type_call
    lea rdx, [rel slice_type]
    lea rcx, [rel slice_type_call]
    call add_builtin_type

    ; frozenset
    mov rdi, rbx
    lea rsi, [rel bi_name_frozenset]
    extern frozenset_type
    extern frozenset_type_call
    lea rdx, [rel frozenset_type]
    lea rcx, [rel frozenset_type_call]
    call add_builtin_type

    ; Return builtins dict
    mov rax, rbx

    pop rbx
    leave
    ret
END_FUNC builtins_init

;; ============================================================================
;; add_exc_type_builtin(dict, name_cstr, type_ptr)
;; Register an exception type object in the builtins dict.
;; Types are immortal, so no DECREF needed on the value.
;; rdi=dict, rsi=name_cstr, rdx=type_ptr
;; ============================================================================
DEF_FUNC_LOCAL add_exc_type_builtin
    push rbx
    push r12

    mov rbx, rdi               ; dict
    mov r12, rdx               ; type_ptr

    ; Create key string (heap — used as dict key, then DECREFed)
    mov rdi, rsi
    call str_from_cstr_heap
    push rax                   ; save key

    ; dict_set(dict, key, type_ptr)
    ; INCREF the type (dict_set will INCREF it, types are immortal anyway)
    mov rdi, rbx
    mov rsi, rax               ; key
    mov rdx, r12               ; type object
    call dict_set

    ; DECREF key
    pop rdi
    call obj_decref

    pop r12
    pop rbx
    leave
    ret
END_FUNC add_exc_type_builtin

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata

bi_name_breakpoint:   db "breakpoint", 0
bi_name_print:        db "print", 0
bi_name_len:          db "len", 0
bc_new_name:          db "__new__", 0
bi_name_range:        db "range", 0
bi_name_type:         db "type", 0
bi_name_isinstance:   db "isinstance", 0
bi_name_issubclass:   db "issubclass", 0
bi_name_repr:         db "repr", 0
bi_name_float:        db "float", 0
bi_name_bool:         db "bool", 0
bi_name_object:       db "object", 0
bi_name_build_class:  db "__build_class__", 0

; New builtin names
bi_name_abs:          db "abs", 0
bi_name_int:          db "int", 0
bi_name_str:          db "str", 0
bi_name_ord:          db "ord", 0
bi_name_chr:          db "chr", 0
bi_name_hex:          db "hex", 0
bi_name_id:           db "id", 0
bi_name_hash:         db "hash", 0
bi_name_callable:     db "callable", 0
bi_name_iter:         db "iter", 0
bi_name_next:         db "next", 0
bi_name_any:          db "any", 0
bi_name_all:          db "all", 0
bi_name_sum:          db "sum", 0
bi_name_min:          db "min", 0
bi_name_max:          db "max", 0
bi_name_getattr:      db "getattr", 0
bi_name_hasattr:      db "hasattr", 0
bi_name_setattr:      db "setattr", 0

; Iterator builtin names
bi_name_enumerate:    db "enumerate", 0
bi_name_zip:          db "zip", 0
bi_name_map:          db "map", 0
bi_name_filter:       db "filter", 0
bi_name_reversed:     db "reversed", 0
bi_name_sorted:       db "sorted", 0
bi_name_chain:        db "chain", 0
bi_name_divmod:       db "divmod", 0
bi_name_globals:      db "globals", 0
bi_name_locals:       db "locals", 0
bi_name_dir:          db "dir", 0
bi_name_eval:         db "eval", 0
bi_name_compile:      db "compile", 0
bi_name_super:        db "super", 0
bi_name_staticmethod: db "staticmethod", 0
bi_name_classmethod:  db "classmethod", 0
bi_name_property:     db "property", 0
bi_name_NotImplemented: db "NotImplemented", 0
bi_name_Ellipsis:      db "Ellipsis", 0

; Exception type names
bi_name_BaseException:     db "BaseException", 0
bi_name_Exception:         db "Exception", 0
bi_name_TypeError:         db "TypeError", 0
bi_name_ValueError:        db "ValueError", 0
bi_name_KeyError:          db "KeyError", 0
bi_name_IndexError:        db "IndexError", 0
bi_name_AttributeError:    db "AttributeError", 0
bi_name_NameError:         db "NameError", 0
bi_name_UnboundLocalError: db "UnboundLocalError", 0
bi_name_RuntimeError:      db "RuntimeError", 0
bi_name_StopIteration:     db "StopIteration", 0
bi_name_ZeroDivisionError: db "ZeroDivisionError", 0
bi_name_NotImplementedError: db "NotImplementedError", 0
bi_name_OverflowError:     db "OverflowError", 0
bi_name_AssertionError:    db "AssertionError", 0
bi_name_OSError:           db "OSError", 0
bi_name_LookupError:       db "LookupError", 0
bi_name_ArithmeticError:   db "ArithmeticError", 0
bi_name_RecursionError:    db "RecursionError", 0
bi_name_ImportError:       db "ImportError", 0
bi_name_MemoryError:       db "MemoryError", 0
bi_name_KeyboardInterrupt: db "KeyboardInterrupt", 0
bi_name_SystemExit:        db "SystemExit", 0
bi_name_UnicodeError:      db "UnicodeError", 0
bi_name_Warning:           db "Warning", 0
bi_name_DeprecationWarning: db "DeprecationWarning", 0
bi_name_UserWarning:       db "UserWarning", 0
bi_name_BaseExceptionGroup: db "BaseExceptionGroup", 0
bi_name_ExceptionGroup:    db "ExceptionGroup", 0
bi_name_CancelledError:    db "CancelledError", 0
bi_name_StopAsyncIteration: db "StopAsyncIteration", 0
bi_name_TimeoutError:      db "TimeoutError", 0
bi_name_GeneratorExit:     db "GeneratorExit", 0
bi_name_ModuleNotFoundError: db "ModuleNotFoundError", 0
bi_name_SyntaxError:       db "SyntaxError", 0
bi_name_IndentationError:  db "IndentationError", 0
bi_name_TabError:          db "TabError", 0
bi_name_EOFError:          db "EOFError", 0
bi_name_UnicodeDecodeError: db "UnicodeDecodeError", 0
bi_name_UnicodeEncodeError: db "UnicodeEncodeError", 0
bi_name_ConnectionError:   db "ConnectionError", 0
bi_name_ConnectionResetError: db "ConnectionResetError", 0
bi_name_ConnectionRefusedError: db "ConnectionRefusedError", 0
bi_name_ConnectionAbortedError: db "ConnectionAbortedError", 0
bi_name_BrokenPipeError:   db "BrokenPipeError", 0
bi_name_PermissionError:   db "PermissionError", 0
bi_name_IsADirectoryError: db "IsADirectoryError", 0
bi_name_NotADirectoryError: db "NotADirectoryError", 0
bi_name_ProcessLookupError: db "ProcessLookupError", 0
bi_name_ChildProcessError: db "ChildProcessError", 0
bi_name_BlockingIOError:   db "BlockingIOError", 0
bi_name_InterruptedError:  db "InterruptedError", 0
bi_name_FloatingPointError: db "FloatingPointError", 0
bi_name_BufferError:       db "BufferError", 0
bi_name_ReferenceError:    db "ReferenceError", 0
bi_name_SystemError:       db "SystemError", 0
bi_name_RuntimeWarning:    db "RuntimeWarning", 0
bi_name_FutureWarning:     db "FutureWarning", 0
bi_name_ImportWarning:     db "ImportWarning", 0
bi_name_UnicodeWarning:    db "UnicodeWarning", 0
bi_name_ResourceWarning:   db "ResourceWarning", 0
bi_name_BytesWarning:      db "BytesWarning", 0
bi_name_PendingDeprecationWarning: db "PendingDeprecationWarning", 0
bi_name_SyntaxWarning:     db "SyntaxWarning", 0
bi_name_EncodingWarning:   db "EncodingWarning", 0
bi_name_list:              db "list", 0
bi_name_dict:              db "dict", 0
bi_name_tuple:             db "tuple", 0
bi_name_set:               db "set", 0
bi_name_bytes:             db "bytes", 0
bi_name_bytearray:         db "bytearray", 0
bi_name_memoryview:        db "memoryview", 0
bi_name_round:             db "round", 0
bi_name_pow:               db "pow", 0
bi_name_input:             db "input", 0
bi_name_open:              db "open", 0
bi_name_bin:               db "bin", 0
bi_name_oct:               db "oct", 0
bi_name_ascii:             db "ascii", 0
bi_name_format:            db "format", 0
bi_name_vars:              db "vars", 0
bi_name_delattr:           db "delattr", 0
bi_name_aiter:             db "aiter", 0
bi_name_anext:             db "anext", 0
bi_name___import__:        db "__import__", 0
bi_name_slice:             db "slice", 0
bi_name_frozenset:         db "frozenset", 0

section .data

global builtins_dict_global
builtins_dict_global: dq 0

global build_class_obj
build_class_obj: dq 0

builtin_func_name_str: db "builtin_function_or_method", 0

; Builtin function type object
align 8
global builtin_func_type
builtin_func_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq builtin_func_name_str    ; tp_name
    dq PyBuiltinObject_size     ; tp_basicsize
    dq builtin_func_dealloc     ; tp_dealloc
    dq builtin_func_repr        ; tp_repr
    dq builtin_func_repr        ; tp_str
    dq 0                        ; tp_hash
    dq builtin_func_call        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
