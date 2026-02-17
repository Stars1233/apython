; methods.asm - Type methods for str, list, and dict
; Implements method functions and methods_init to populate tp_dict

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"


; External functions
extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_memcpy
extern ap_memset
extern ap_strcmp
extern ap_strlen
extern ap_strstr
extern ap_memcmp
extern obj_incref
extern obj_decref
extern obj_dealloc
extern obj_repr
extern str_from_cstr
extern str_new
extern str_type
extern list_new
extern list_append
extern list_type
extern tuple_new
extern tuple_type
extern dict_new
extern dict_get
extern dict_set
extern dict_del
extern dict_type
extern none_singleton
extern bool_true
extern bool_false
extern int_from_i64
extern int_to_i64
extern builtin_func_new
extern raise_exception
extern fatal_error
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_IndexError_type
extern exc_KeyError_type
extern int_type
extern set_type
extern object_type
extern object_new_fn
extern staticmethod_type

; Set entry layout constants (must match set.asm)
SET_ENTRY_HASH    equ 0
SET_ENTRY_KEY     equ 8
SET_ENTRY_KEY_TAG equ 16
SET_ENTRY_SIZE    equ 24
extern set_add
extern set_contains
extern set_remove
extern set_new
extern set_tp_iter

;; ============================================================================
;; HELPER: add_method_to_dict(dict, name_cstr, func_ptr)
;; rdi=dict, rsi=name_cstr, rdx=func_ptr
;; Creates a builtin func wrapper and stores it in the dict.
;; ============================================================================
DEF_FUNC_LOCAL add_method_to_dict
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; dict
    mov r12, rsi            ; name_cstr
    mov r13, rdx            ; func_ptr

    ; Create builtin func wrapper: builtin_func_new(func_ptr, name_cstr)
    mov rdi, r13
    mov rsi, r12
    call builtin_func_new
    push rax                ; save func obj

    ; Create key string from name
    mov rdi, r12
    call str_from_cstr
    push rax                ; save key str

    ; dict_set(dict, key, func_obj)
    mov rdi, rbx
    mov rsi, rax            ; key
    mov rdx, [rsp + 8]     ; func obj
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set

    ; DECREF key (dict_set did INCREF)
    pop rdi
    call obj_decref

    ; DECREF func obj (dict_set did INCREF)
    pop rdi
    call obj_decref

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC add_method_to_dict

;; ############################################################################
;;                         STRING METHODS
;; ############################################################################

;; ============================================================================
;; str_method_upper(args, nargs) -> new uppercase string
;; args[0] = self (PyStrObject*)
;; ============================================================================
DEF_FUNC str_method_upper
    push rbx
    push r12
    push r13

    mov rax, [rdi]          ; self = args[0]
    mov rbx, rax            ; rbx = self
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = length

    ; Create new string: str_new(data, len)
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new
    mov r13, rax            ; r13 = new string

    ; Convert each byte to uppercase in-place
    xor ecx, ecx
.upper_loop:
    cmp rcx, r12
    jge .upper_done
    movzx eax, byte [r13 + PyStrObject.data + rcx]
    cmp al, 'a'
    jb .upper_next
    cmp al, 'z'
    ja .upper_next
    sub al, 32             ; 'a'-'A' = 32
    mov [r13 + PyStrObject.data + rcx], al
.upper_next:
    inc rcx
    jmp .upper_loop
.upper_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_upper

;; ============================================================================
;; str_method_lower(args, nargs) -> new lowercase string
;; ============================================================================
DEF_FUNC str_method_lower
    push rbx
    push r12
    push r13

    mov rax, [rdi]          ; self
    mov rbx, rax
    mov r12, [rbx + PyStrObject.ob_size]

    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r12
    call str_new
    mov r13, rax

    xor ecx, ecx
.lower_loop:
    cmp rcx, r12
    jge .lower_done
    movzx eax, byte [r13 + PyStrObject.data + rcx]
    cmp al, 'A'
    jb .lower_next
    cmp al, 'Z'
    ja .lower_next
    add al, 32
    mov [r13 + PyStrObject.data + rcx], al
.lower_next:
    inc rcx
    jmp .lower_loop
.lower_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_lower

;; ============================================================================
;; str_method_strip(args, nargs) -> new stripped string
;; Strip whitespace (space, tab, newline, cr) from both ends
;; ============================================================================
DEF_FUNC str_method_strip
    push rbx
    push r12
    push r13
    push r14

    mov rax, [rdi]          ; self
    mov rbx, rax
    mov r12, [rbx + PyStrObject.ob_size]  ; length

    ; Find start (skip leading whitespace)
    xor r13d, r13d          ; r13 = start index
.strip_left:
    cmp r13, r12
    jge .strip_empty
    movzx eax, byte [rbx + PyStrObject.data + r13]
    cmp al, ' '
    je .strip_left_next
    cmp al, 9              ; tab
    je .strip_left_next
    cmp al, 10             ; newline
    je .strip_left_next
    cmp al, 13             ; carriage return
    je .strip_left_next
    jmp .strip_right_start
.strip_left_next:
    inc r13
    jmp .strip_left

.strip_empty:
    ; All whitespace - return empty string
    lea rdi, [rel empty_str_cstr]
    call str_from_cstr
    jmp .strip_ret

.strip_right_start:
    ; Find end (skip trailing whitespace)
    mov r14, r12            ; r14 = end (exclusive)
.strip_right:
    cmp r14, r13
    jle .strip_empty
    movzx eax, byte [rbx + PyStrObject.data + r14 - 1]
    cmp al, ' '
    je .strip_right_next
    cmp al, 9
    je .strip_right_next
    cmp al, 10
    je .strip_right_next
    cmp al, 13
    je .strip_right_next
    jmp .strip_make
.strip_right_next:
    dec r14
    jmp .strip_right

.strip_make:
    ; Create new string from [start, end)
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r13
    mov rsi, r14
    sub rsi, r13            ; length = end - start
    call str_new

.strip_ret:
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_strip

;; ============================================================================
;; str_method_startswith(args, nargs) -> bool_true/bool_false
;; args[0]=self, args[1]=prefix
;; ============================================================================
DEF_FUNC str_method_startswith
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; prefix (args[1])

    mov r13, [r12 + PyStrObject.ob_size]  ; prefix length

    ; If prefix is longer than self, return False
    cmp r13, [rbx + PyStrObject.ob_size]
    jg .sw_false

    ; Compare first prefix_len bytes
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, r13
    ; Manual byte comparison since ap_strcmp needs null-terminated
    xor ecx, ecx
.sw_cmp:
    cmp rcx, r13
    jge .sw_true
    movzx eax, byte [rdi + rcx]
    cmp al, [rsi + rcx]
    jne .sw_false
    inc rcx
    jmp .sw_cmp

.sw_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sw_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_startswith

;; ============================================================================
;; str_method_endswith(args, nargs) -> bool_true/bool_false
;; args[0]=self, args[1]=suffix
;; ============================================================================
DEF_FUNC str_method_endswith
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; suffix
    mov r13, [r12 + PyStrObject.ob_size]  ; suffix length
    mov r14, [rbx + PyStrObject.ob_size]  ; self length

    ; If suffix longer than self, False
    cmp r13, r14
    jg .ew_false

    ; Compare last suffix_len bytes of self with suffix
    mov rcx, r14
    sub rcx, r13            ; offset = self_len - suffix_len
    lea rdi, [rbx + PyStrObject.data]
    add rdi, rcx
    lea rsi, [r12 + PyStrObject.data]
    xor ecx, ecx
.ew_cmp:
    cmp rcx, r13
    jge .ew_true
    movzx eax, byte [rdi + rcx]
    cmp al, [rsi + rcx]
    jne .ew_false
    inc rcx
    jmp .ew_cmp

.ew_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.ew_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_endswith

;; ============================================================================
;; str_method_find(args, nargs) -> SmallInt index or -1
;; args[0]=self, args[1]=substr
;; ============================================================================
DEF_FUNC str_method_find
    push rbx
    push r12

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; substr

    ; Use ap_strstr to find substring
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strstr

    test rax, rax
    jz .find_not_found

    ; Compute index: result_ptr - self.data
    lea rcx, [rbx + PyStrObject.data]
    sub rax, rcx
    ; rax = index
    mov rdi, rax
    call int_from_i64

    pop r12
    pop rbx
    leave
    ret

.find_not_found:
    mov rdi, -1
    call int_from_i64

    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_find

;; ============================================================================
;; str_method_replace(args, nargs) -> new string with replacements
;; args[0]=self, args[1]=old, args[2]=new
;; Uses callee-saved regs for key state, stack locals for buffer management.
;; ============================================================================
DEF_FUNC str_method_replace
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40             ; [rbp-48]=buf_ptr, [rbp-56]=buf_alloc, [rbp-64]=write_pos, [rbp-72]=self_len, [rbp-80]=pad

    ; rbx = self, r12 = old_str, r13 = new_str, r14 = self_len, r15 = scan_pos
    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; old
    mov r13, [rdi + 32]     ; new
    mov r14, [rbx + PyStrObject.ob_size]
    mov [rbp-72], r14

    ; If old_str is empty, return a copy of self
    cmp qword [r12 + PyStrObject.ob_size], 0
    je .replace_copy_self

    ; Allocate initial buffer: self_len * 2 + 64
    lea rdi, [r14 * 2 + 64]
    mov [rbp-56], rdi       ; buf_alloc
    call ap_malloc
    mov [rbp-48], rax       ; buf_ptr
    mov qword [rbp-64], 0   ; write_pos = 0

    xor r15d, r15d          ; r15 = scan position

.replace_scan:
    ; Check if remaining text is long enough for old_str
    mov rax, r14
    sub rax, r15
    cmp rax, [r12 + PyStrObject.ob_size]
    jl .replace_copy_tail

    ; Search for old_str from scan pos
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r15
    lea rsi, [r12 + PyStrObject.data]
    call ap_strstr
    test rax, rax
    jz .replace_copy_tail

    ; Found at rax; compute found_pos relative to self.data start
    lea rcx, [rbx + PyStrObject.data]
    sub rax, rcx            ; rax = found_pos (absolute index in self)
    push rax                ; save found_pos on stack

    ; --- ensure buffer space ---
    mov rcx, rax
    sub rcx, r15            ; prefix_len = found_pos - scan_pos
    mov rdx, [rbp-64]       ; write_pos
    add rdx, rcx
    add rdx, [r13 + PyStrObject.ob_size]
    add rdx, r14            ; generous upper bound for rest
    cmp rdx, [rbp-56]
    jl .replace_space_ok
    shl rdx, 1
    mov [rbp-56], rdx
    mov rdi, [rbp-48]
    mov rsi, rdx
    call ap_realloc
    mov [rbp-48], rax
.replace_space_ok:

    ; --- copy prefix: bytes from scan_pos to found_pos ---
    pop rax                 ; found_pos
    push rax                ; keep on stack
    mov rcx, rax
    sub rcx, r15            ; prefix_len
    test rcx, rcx
    jz .replace_no_prefix

    mov rdi, [rbp-48]
    add rdi, [rbp-64]
    lea rsi, [rbx + PyStrObject.data]
    add rsi, r15
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp-64], rcx

.replace_no_prefix:
    ; --- copy new_str ---
    mov rcx, [r13 + PyStrObject.ob_size]
    test rcx, rcx
    jz .replace_adv

    mov rdi, [rbp-48]
    add rdi, [rbp-64]
    lea rsi, [r13 + PyStrObject.data]
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp-64], rcx

.replace_adv:
    pop rax                 ; found_pos
    add rax, [r12 + PyStrObject.ob_size]
    mov r15, rax            ; advance scan past old_str
    jmp .replace_scan

.replace_copy_tail:
    ; Copy remaining bytes from scan_pos to end
    mov rcx, r14
    sub rcx, r15
    test rcx, rcx
    jz .replace_make_str

    mov rdi, [rbp-48]
    add rdi, [rbp-64]
    lea rsi, [rbx + PyStrObject.data]
    add rsi, r15
    mov rdx, rcx
    push rcx
    call ap_memcpy
    pop rcx
    add [rbp-64], rcx

.replace_make_str:
    mov rdi, [rbp-48]
    mov rsi, [rbp-64]       ; result length
    call str_new
    push rax

    mov rdi, [rbp-48]
    call ap_free

    pop rax
    mov edx, TAG_PTR
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.replace_copy_self:
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r14
    call str_new
    mov edx, TAG_PTR
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_replace

;; ============================================================================
;; str_method_join(args, nargs) -> new string
;; args[0]=self (separator), args[1]=list
;; self.join(iterable)
;; Regs: rbx=self(sep), r12=list, r13=count, r14=sep_len
;; Stack: [rbp-48]=total_len, [rbp-56]=buf_ptr, [rbp-64]=write_pos
;; ============================================================================
DEF_FUNC str_method_join
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 32             ; 3 locals + alignment pad = 32

    mov rbx, [rdi]          ; self (separator)
    mov r12, [rdi + 16]     ; list

    mov r13, [r12 + PyListObject.ob_size]  ; item count
    mov r14, [rbx + PyStrObject.ob_size]   ; sep length

    ; If list is empty, return empty string
    test r13, r13
    jz .join_empty

    ; First pass: compute total length
    xor r15d, r15d          ; r15 = total data length
    xor ecx, ecx
.join_len_loop:
    cmp rcx, r13
    jge .join_len_done
    push rcx
    mov rax, [r12 + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4              ; index * 16
    mov rax, [rax + rdx]    ; payload
    add r15, [rax + PyStrObject.ob_size]
    pop rcx
    inc rcx
    jmp .join_len_loop

.join_len_done:
    ; Add separator lengths: sep_len * (count - 1)
    mov rax, r13
    dec rax
    imul rax, r14
    add r15, rax
    mov [rbp-48], r15       ; total_len

    ; Allocate buffer
    lea rdi, [r15 + 1]
    call ap_malloc
    mov [rbp-56], rax       ; buf_ptr
    mov qword [rbp-64], 0   ; write_pos = 0

    ; Second pass: copy data
    xor ecx, ecx
.join_copy_loop:
    cmp rcx, r13
    jge .join_make_str
    push rcx

    ; If not the first item, copy separator
    test rcx, rcx
    jz .join_no_sep

    mov rdi, [rbp-56]
    add rdi, [rbp-64]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r14
    call ap_memcpy
    add [rbp-64], r14

.join_no_sep:
    mov rcx, [rsp]          ; reload index
    mov rax, [r12 + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4              ; index * 16
    mov rax, [rax + rdx]    ; item payload
    mov rdx, [rax + PyStrObject.ob_size]
    push rdx                ; save item_len

    mov rdi, [rbp-56]
    add rdi, [rbp-64]
    lea rsi, [rax + PyStrObject.data]
    call ap_memcpy

    pop rdx                 ; item_len
    add [rbp-64], rdx

    pop rcx
    inc rcx
    jmp .join_copy_loop

.join_make_str:
    mov rdi, [rbp-56]
    mov rsi, [rbp-48]       ; total_len
    call str_new
    push rax

    mov rdi, [rbp-56]
    call ap_free

    pop rax
    mov edx, TAG_PTR
    add rsp, 32
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.join_empty:
    lea rdi, [rel empty_str_cstr]
    call str_from_cstr
    mov edx, TAG_PTR
    add rsp, 32
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_join

;; ============================================================================
;; str_method_split(args, nargs) -> list of strings
;; If nargs==1: split by whitespace
;; If nargs==2: split by args[1]
;; ============================================================================
DEF_FUNC str_method_split
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8              ; align

    mov rbx, [rdi]          ; self
    mov r14, rsi            ; nargs
    ; Save args[1] if present
    cmp r14, 2
    jl .split_no_sep
    mov r15, [rdi + 16]     ; separator string
    jmp .split_by_sep

.split_no_sep:
    ; Split by whitespace
    mov r12, [rbx + PyStrObject.ob_size]  ; self length

    ; Create result list
    mov rdi, 8
    call list_new
    mov r13, rax            ; r13 = result list

    ; Scan through self
    xor ecx, ecx            ; ecx = position
.ws_scan:
    ; Skip leading whitespace
    cmp rcx, r12
    jge .ws_done
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, ' '
    je .ws_skip
    cmp al, 9
    je .ws_skip
    cmp al, 10
    je .ws_skip
    cmp al, 13
    je .ws_skip
    jmp .ws_word_start
.ws_skip:
    inc rcx
    jmp .ws_scan

.ws_word_start:
    ; Found start of word at rcx
    mov r15, rcx            ; word start
.ws_word_scan:
    inc rcx
    cmp rcx, r12
    jge .ws_word_end
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, ' '
    je .ws_word_end
    cmp al, 9
    je .ws_word_end
    cmp al, 10
    je .ws_word_end
    cmp al, 13
    je .ws_word_end
    jmp .ws_word_scan

.ws_word_end:
    ; Word from r15 to rcx (exclusive)
    push rcx
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r15
    mov rsi, rcx
    sub rsi, r15            ; length
    call str_new
    ; Append to list
    mov rdi, r13
    mov rsi, rax
    push rax
    mov edx, TAG_PTR
    call list_append
    pop rdi
    call obj_decref         ; list_append did INCREF
    pop rcx
    jmp .ws_scan

.ws_done:
    mov rax, r13
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    ret

.split_by_sep:
    ; Split by separator string r15
    mov r12, [rbx + PyStrObject.ob_size]  ; self length
    mov r14, [r15 + PyStrObject.ob_size]  ; sep length

    ; Create result list
    mov rdi, 8
    call list_new
    mov r13, rax            ; r13 = result list

    ; If sep is empty, raise ValueError
    test r14, r14
    jz .split_empty_sep

    xor ecx, ecx            ; scan position
.sep_scan:
    push rcx                ; save scan pos

    ; Search for separator starting at current position
    lea rdi, [rbx + PyStrObject.data]
    add rdi, rcx
    lea rsi, [r15 + PyStrObject.data]
    call ap_strstr
    pop rcx

    test rax, rax
    jz .sep_tail

    ; Found separator at rax
    lea rdx, [rbx + PyStrObject.data]
    sub rax, rdx            ; found_pos in self
    push rax                ; save found_pos

    ; Create substring from rcx to found_pos
    lea rdi, [rbx + PyStrObject.data]
    add rdi, rcx
    mov rsi, rax
    sub rsi, rcx            ; length = found_pos - scan_pos
    call str_new
    mov rdi, r13
    mov rsi, rax
    push rax
    mov edx, TAG_PTR
    call list_append
    pop rdi
    call obj_decref

    pop rcx                 ; found_pos
    add rcx, r14            ; advance past separator
    jmp .sep_scan

.sep_tail:
    ; Copy remaining string from rcx to end
    lea rdi, [rbx + PyStrObject.data]
    add rdi, rcx
    mov rsi, r12
    sub rsi, rcx            ; remaining length
    call str_new
    mov rdi, r13
    mov rsi, rax
    push rax
    mov edx, TAG_PTR
    call list_append
    pop rdi
    call obj_decref

    mov rax, r13
    mov edx, TAG_PTR
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.split_empty_sep:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "empty separator"
    call raise_exception
END_FUNC str_method_split


;; ============================================================================
;; str_method_format(args, nargs) -> new formatted string
;; args[0]=self (format string), args[1..]=positional arguments
;; Handles {} (auto-index) and {N} (explicit index).
;; ============================================================================
DEF_FUNC str_method_format
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24             ; [rbp-48]=buf, [rbp-56]=buf_used, [rbp-64]=buf_cap

    mov rbx, rdi            ; args array
    mov r14, rsi            ; nargs

    ; Get format string data
    mov rax, [rbx]          ; self = format string
    lea r12, [rax + PyStrObject.data]  ; r12 = fmt data ptr
    mov r13d, [rax + PyStrObject.ob_size] ; r13 = fmt length

    ; Allocate initial output buffer
    lea rdi, [r13 + 64]    ; generous initial size
    call ap_malloc
    mov [rbp-48], rax       ; buf
    mov qword [rbp-56], 0   ; buf_used = 0
    lea rax, [r13 + 64]
    mov [rbp-64], rax       ; buf_cap

    xor ecx, ecx            ; ecx = source index
    xor r15d, r15d          ; r15d = auto-index counter

.fmt_loop:
    cmp ecx, r13d
    jge .fmt_done
    movzx eax, byte [r12 + rcx]
    cmp al, '{'
    je .fmt_brace
    cmp al, '}'
    je .fmt_close_brace
    ; Regular char — append to buffer
    push rcx
    ; Ensure space
    mov rdi, [rbp-56]       ; used
    inc rdi                 ; need 1 more
    cmp rdi, [rbp-64]
    jbe .fmt_char_ok
    ; Grow buffer
    mov rdi, [rbp-64]
    shl rdi, 1
    mov [rbp-64], rdi
    mov rsi, rdi
    mov rdi, [rbp-48]
    call ap_realloc
    mov [rbp-48], rax
.fmt_char_ok:
    pop rcx
    mov rdi, [rbp-48]
    mov rax, [rbp-56]
    movzx edx, byte [r12 + rcx]
    mov [rdi + rax], dl
    inc qword [rbp-56]
    inc ecx
    jmp .fmt_loop

.fmt_brace:
    inc ecx                 ; skip '{'
    cmp ecx, r13d
    jge .fmt_done
    movzx eax, byte [r12 + rcx]
    ; Check for {{ (literal brace)
    cmp al, '{'
    je .fmt_literal_brace
    ; Check for } (empty placeholder = auto-index)
    cmp al, '}'
    je .fmt_auto_index
    ; Check for digit (explicit index)
    cmp al, '0'
    jb .fmt_done            ; unexpected char, bail
    cmp al, '9'
    ja .fmt_done
    ; Parse number
    xor edx, edx            ; edx = arg_index
.fmt_parse_num:
    movzx eax, byte [r12 + rcx]
    cmp al, '}'
    je .fmt_have_index
    sub al, '0'
    imul edx, 10
    movzx eax, al
    add edx, eax
    inc ecx
    cmp ecx, r13d
    jl .fmt_parse_num
    jmp .fmt_done
.fmt_have_index:
    inc ecx                 ; skip '}'
    jmp .fmt_insert_arg

.fmt_auto_index:
    inc ecx                 ; skip '}'
    mov edx, r15d           ; edx = auto-index
    inc r15d
    ; fall through to .fmt_insert_arg

.fmt_insert_arg:
    ; edx = arg index (0-based among format args, which are args[1..])
    lea eax, [edx + 1]     ; args index (skip self)
    cmp rax, r14
    jge .fmt_loop           ; out of range, skip
    push rcx
    push rdx
    ; Get the arg object and convert to string
    shl rax, 4              ; offset = index * 16
    mov rdi, [rbx + rax]    ; arg object payload
    mov r8d, [rbx + rax + 8] ; arg tag
    ; Call obj_repr or tp_str
    push rdi
    cmp r8d, TAG_SMALLINT
    je .fmt_smallint_str
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_str]
    test rax, rax
    jz .fmt_use_repr
    pop rdi
    call rax
    jmp .fmt_have_str
.fmt_use_repr:
    pop rdi
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_repr]
    test rax, rax
    jz .fmt_skip_arg
    call rax
    jmp .fmt_have_str
.fmt_smallint_str:
    pop rdi
    mov esi, TAG_SMALLINT
    call obj_repr          ; SmallInt → repr is fine
.fmt_have_str:
    ; rax = string object; append its data to buffer
    push rax                ; save str obj for DECREF
    mov edx, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    ; Ensure buffer has space
    mov rdi, [rbp-56]
    add rdi, rdx
    cmp rdi, [rbp-64]
    jbe .fmt_copy_ok
    mov rdi, [rbp-64]
.fmt_grow_copy:
    shl rdi, 1
    mov rax, [rbp-56]
    add rax, rdx
    cmp rdi, rax
    jb .fmt_grow_copy
    mov [rbp-64], rdi
    mov rsi, rdi
    mov rdi, [rbp-48]
    call ap_realloc
    mov [rbp-48], rax
    ; Re-read str data (rax was clobbered)
    mov rax, [rsp]          ; str obj
    mov edx, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
.fmt_copy_ok:
    ; Copy string data
    mov rdi, [rbp-48]
    add rdi, [rbp-56]
    xor ecx, ecx
.fmt_copy_str:
    cmp ecx, edx
    jge .fmt_copy_done
    mov al, [rsi + rcx]
    mov [rdi + rcx], al
    inc ecx
    jmp .fmt_copy_str
.fmt_copy_done:
    movzx eax, dx
    add [rbp-56], rax

    ; DECREF the temporary string
    pop rdi                 ; str obj
    call obj_decref
.fmt_skip_arg:
    pop rdx
    pop rcx
    jmp .fmt_loop

.fmt_literal_brace:
    ; {{ → output single {
    push rcx
    mov rdi, [rbp-48]
    mov rax, [rbp-56]
    mov byte [rdi + rax], '{'
    inc qword [rbp-56]
    pop rcx
    inc ecx                 ; skip second {
    jmp .fmt_loop

.fmt_close_brace:
    ; }} → output single }
    inc ecx                 ; skip first }
    cmp ecx, r13d
    jge .fmt_done
    movzx eax, byte [r12 + rcx]
    cmp al, '}'
    jne .fmt_loop           ; lone } — ignore (CPython raises error, we skip)
    push rcx
    mov rdi, [rbp-48]
    mov rax, [rbp-56]
    mov byte [rdi + rax], '}'
    inc qword [rbp-56]
    pop rcx
    inc ecx                 ; skip second }
    jmp .fmt_loop

.fmt_done:
    ; NUL-terminate and create string
    mov rdi, [rbp-48]
    mov rax, [rbp-56]
    mov byte [rdi + rax], 0
    call str_from_cstr
    push rax

    ; Free buffer
    mov rdi, [rbp-48]
    call ap_free

    pop rax
    mov edx, TAG_PTR
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_format

;; ============================================================================
;; str_method_lstrip(args, nargs) -> new string with left whitespace removed
;; args[0] = self (PyStrObject*)
;; ============================================================================
DEF_FUNC str_method_lstrip
    push rbx
    push r12
    push r13

    mov rax, [rdi]          ; self = args[0]
    mov rbx, rax            ; rbx = self
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = length

    ; Find start (skip leading whitespace)
    xor r13d, r13d          ; r13 = start index
.lstrip_left:
    cmp r13, r12
    jge .lstrip_empty
    movzx eax, byte [rbx + PyStrObject.data + r13]
    cmp al, ' '
    je .lstrip_left_next
    cmp al, 9              ; tab
    je .lstrip_left_next
    cmp al, 10             ; newline
    je .lstrip_left_next
    cmp al, 13             ; carriage return
    je .lstrip_left_next
    jmp .lstrip_make
.lstrip_left_next:
    inc r13
    jmp .lstrip_left

.lstrip_empty:
    ; All whitespace - return empty string
    lea rdi, [rel empty_str_cstr]
    call str_from_cstr
    jmp .lstrip_ret

.lstrip_make:
    ; Create new string from [start, end)
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r13
    mov rsi, r12
    sub rsi, r13            ; length = len - start
    call str_new

.lstrip_ret:
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_lstrip

;; ============================================================================
;; str_method_rstrip(args, nargs) -> new string with right whitespace removed
;; args[0] = self (PyStrObject*)
;; ============================================================================
DEF_FUNC str_method_rstrip
    push rbx
    push r12
    push r13

    mov rax, [rdi]          ; self = args[0]
    mov rbx, rax            ; rbx = self
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = length

    ; Find end (skip trailing whitespace)
    mov r13, r12            ; r13 = end (exclusive)
.rstrip_right:
    cmp r13, 0
    jle .rstrip_empty
    movzx eax, byte [rbx + PyStrObject.data + r13 - 1]
    cmp al, ' '
    je .rstrip_right_next
    cmp al, 9              ; tab
    je .rstrip_right_next
    cmp al, 10             ; newline
    je .rstrip_right_next
    cmp al, 13             ; carriage return
    je .rstrip_right_next
    jmp .rstrip_make
.rstrip_right_next:
    dec r13
    jmp .rstrip_right

.rstrip_empty:
    ; All whitespace - return empty string
    lea rdi, [rel empty_str_cstr]
    call str_from_cstr
    jmp .rstrip_ret

.rstrip_make:
    ; Create new string from [0, end)
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r13            ; length = end
    call str_new

.rstrip_ret:
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_rstrip

;; ============================================================================
;; str_method_count(args, nargs) -> SmallInt count of occurrences
;; args[0]=self, args[1]=sub
;; ============================================================================
DEF_FUNC str_method_count
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; substr
    xor r13d, r13d          ; r13 = count
    mov r14, [r12 + PyStrObject.ob_size]  ; sub length

    ; If sub is empty, return len+1
    test r14, r14
    jz .count_empty_sub

    ; Start scanning from self.data
    lea rdi, [rbx + PyStrObject.data]

.count_scan:
    lea rsi, [r12 + PyStrObject.data]
    push rdi
    call ap_strstr
    pop rdi                 ; restore (not needed, but stack balance)
    test rax, rax
    jz .count_done

    ; Found one occurrence
    inc r13
    ; Advance past this match
    lea rdi, [rax + r14]    ; move past the match
    jmp .count_scan

.count_empty_sub:
    ; Empty substring: count = len(self) + 1
    mov r13, [rbx + PyStrObject.ob_size]
    inc r13

.count_done:
    mov rdi, r13
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_count

;; ============================================================================
;; str_method_index(args, nargs) -> SmallInt index (raises ValueError if not found)
;; args[0]=self, args[1]=substr
;; ============================================================================
DEF_FUNC str_method_index
    push rbx
    push r12

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; substr

    ; Use ap_strstr to find substring
    lea rdi, [rbx + PyStrObject.data]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strstr

    test rax, rax
    jz .str_index_not_found

    ; Compute index: result_ptr - self.data
    lea rcx, [rbx + PyStrObject.data]
    sub rax, rcx
    ; rax = index
    mov rdi, rax
    call int_from_i64

    pop r12
    pop rbx
    leave
    ret

.str_index_not_found:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "substring not found"
    call raise_exception
END_FUNC str_method_index

;; ============================================================================
;; str_method_rfind(args, nargs) -> SmallInt index or -1
;; args[0]=self, args[1]=substr
;; Find rightmost occurrence of substr in self.
;; ============================================================================
DEF_FUNC str_method_rfind
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; substr
    mov r13, [rbx + PyStrObject.ob_size]   ; self length
    mov r14, [r12 + PyStrObject.ob_size]   ; sub length

    ; If sub_len > self_len, return -1
    cmp r14, r13
    jg .rfind_not_found

    ; If sub_len == 0, return self_len
    test r14, r14
    jz .rfind_empty_sub

    ; Walk backward from (self_len - sub_len) down to 0
    mov rcx, r13
    sub rcx, r14            ; rcx = last possible start position

.rfind_loop:
    cmp rcx, 0
    jl .rfind_not_found

    ; Compare sub with self[rcx..rcx+sub_len]
    push rcx
    lea rdi, [rbx + PyStrObject.data]
    add rdi, rcx
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, r14
    call ap_memcmp
    pop rcx

    test eax, eax
    jz .rfind_found

    dec rcx
    jmp .rfind_loop

.rfind_found:
    mov rdi, rcx
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.rfind_empty_sub:
    mov rdi, r13
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.rfind_not_found:
    mov rdi, -1
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_rfind

;; ============================================================================
;; str_method_isdigit(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all chars are digits and len>0, else False
;; ============================================================================
DEF_FUNC str_method_isdigit
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isdigit_false

    xor edx, edx            ; index
.isdigit_loop:
    cmp rdx, rcx
    jge .isdigit_true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, '0'
    jb .isdigit_false
    cmp sil, '9'
    ja .isdigit_false
    inc rdx
    jmp .isdigit_loop

.isdigit_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.isdigit_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC str_method_isdigit

;; ============================================================================
;; str_method_isalpha(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all chars are alphabetic and len>0, else False
;; ============================================================================
DEF_FUNC str_method_isalpha
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isalpha_false

    xor edx, edx            ; index
.isalpha_loop:
    cmp rdx, rcx
    jge .isalpha_true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 'A'
    jb .isalpha_false
    cmp sil, 'Z'
    jbe .isalpha_next        ; A-Z is alpha
    cmp sil, 'a'
    jb .isalpha_false
    cmp sil, 'z'
    ja .isalpha_false
.isalpha_next:
    inc rdx
    jmp .isalpha_loop

.isalpha_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.isalpha_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC str_method_isalpha

;; ============================================================================
;; str_method_isalnum(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all chars are alphanumeric (0-9, A-Z, a-z) and len>0
;; ============================================================================
DEF_FUNC str_method_isalnum
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isalnum_false

    xor edx, edx            ; index
.isalnum_loop:
    cmp rdx, rcx
    jge .isalnum_true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, '0'
    jb .isalnum_false
    cmp sil, '9'
    jbe .isalnum_next        ; 0-9
    cmp sil, 'A'
    jb .isalnum_false
    cmp sil, 'Z'
    jbe .isalnum_next        ; A-Z
    cmp sil, 'a'
    jb .isalnum_false
    cmp sil, 'z'
    ja .isalnum_false
.isalnum_next:
    inc rdx
    jmp .isalnum_loop

.isalnum_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.isalnum_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC str_method_isalnum

;; ============================================================================
;; str_method_isspace(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all chars are whitespace (space/tab/newline/CR/VT/FF) and len>0
;; ============================================================================
DEF_FUNC str_method_isspace
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isspace_false

    xor edx, edx            ; index
.isspace_loop:
    cmp rdx, rcx
    jge .isspace_true
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 0x20           ; space
    je .isspace_next
    cmp sil, 0x09           ; tab
    jb .isspace_false
    cmp sil, 0x0D           ; tab(09), newline(0A), VT(0B), FF(0C), CR(0D)
    ja .isspace_false
.isspace_next:
    inc rdx
    jmp .isspace_loop

.isspace_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.isspace_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC str_method_isspace

;; ============================================================================
;; str_method_isupper(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all cased chars are uppercase, and there is at least one cased char
;; ============================================================================
DEF_FUNC str_method_isupper
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .isupper_false

    xor edx, edx            ; index
    xor r8d, r8d            ; has_cased flag
.isupper_loop:
    cmp rdx, rcx
    jge .isupper_check_cased
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 'A'
    jb .isupper_next         ; non-alpha, skip
    cmp sil, 'Z'
    jbe .isupper_found_upper ; A-Z: uppercase, good
    cmp sil, 'a'
    jb .isupper_next         ; non-alpha, skip
    cmp sil, 'z'
    jbe .isupper_false       ; a-z: lowercase, fail
.isupper_next:
    inc rdx
    jmp .isupper_loop
.isupper_found_upper:
    mov r8d, 1               ; found at least one cased char
    inc rdx
    jmp .isupper_loop
.isupper_check_cased:
    test r8d, r8d
    jz .isupper_false        ; no cased chars found

    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.isupper_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC str_method_isupper

;; ============================================================================
;; str_method_islower(args, nargs) -> bool_true/bool_false
;; args[0] = self
;; Returns True if all cased chars are lowercase, and there is at least one cased char
;; ============================================================================
DEF_FUNC str_method_islower
    mov rax, [rdi]          ; self
    mov rcx, [rax + PyStrObject.ob_size]

    ; Empty string -> False
    test rcx, rcx
    jz .islower_false

    xor edx, edx            ; index
    xor r8d, r8d            ; has_cased flag
.islower_loop:
    cmp rdx, rcx
    jge .islower_check_cased
    movzx esi, byte [rax + PyStrObject.data + rdx]
    cmp sil, 'a'
    jb .islower_check_upper
    cmp sil, 'z'
    jbe .islower_found_lower ; a-z: lowercase, good
    jmp .islower_next        ; > 'z', non-alpha, skip
.islower_check_upper:
    cmp sil, 'A'
    jb .islower_next         ; non-alpha, skip
    cmp sil, 'Z'
    jbe .islower_false       ; A-Z: uppercase, fail
.islower_next:
    inc rdx
    jmp .islower_loop
.islower_found_lower:
    mov r8d, 1               ; found at least one cased char
    inc rdx
    jmp .islower_loop
.islower_check_cased:
    test r8d, r8d
    jz .islower_false        ; no cased chars found

    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.islower_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC str_method_islower

;; ============================================================================
;; str_method_removeprefix(args, nargs) -> new string
;; args[0]=self, args[1]=prefix
;; If self starts with prefix, return self[len(prefix):], else return self.
;; ============================================================================
DEF_FUNC str_method_removeprefix
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; prefix
    mov r13, [rbx + PyStrObject.ob_size]   ; self len
    mov r14, [r12 + PyStrObject.ob_size]   ; prefix len

    ; If prefix longer than self, return self (INCREF)
    cmp r14, r13
    jg .rmpfx_return_self

    ; Compare first prefix_len bytes
    xor ecx, ecx
.rmpfx_cmp:
    cmp rcx, r14
    jge .rmpfx_match
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, [r12 + PyStrObject.data + rcx]
    jne .rmpfx_return_self
    inc rcx
    jmp .rmpfx_cmp

.rmpfx_match:
    ; Prefix matches - return str_new(data+preflen, len-preflen)
    lea rdi, [rbx + PyStrObject.data]
    add rdi, r14
    mov rsi, r13
    sub rsi, r14
    call str_new
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.rmpfx_return_self:
    mov rax, rbx
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_removeprefix

;; ============================================================================
;; str_method_removesuffix(args, nargs) -> new string
;; args[0]=self, args[1]=suffix
;; If self ends with suffix, return self[:len(self)-len(suffix)], else return self.
;; ============================================================================
DEF_FUNC str_method_removesuffix
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; suffix
    mov r13, [rbx + PyStrObject.ob_size]   ; self len
    mov r14, [r12 + PyStrObject.ob_size]   ; suffix len

    ; If suffix longer than self, return self (INCREF)
    cmp r14, r13
    jg .rmsfx_return_self

    ; If suffix is empty, return self (INCREF)
    test r14, r14
    jz .rmsfx_return_self

    ; Compare last suffix_len bytes of self with suffix
    mov rcx, r13
    sub rcx, r14            ; offset = self_len - suffix_len
    xor edx, edx
.rmsfx_cmp:
    cmp rdx, r14
    jge .rmsfx_match
    movzx eax, byte [rbx + PyStrObject.data + rcx]
    cmp al, [r12 + PyStrObject.data + rdx]
    jne .rmsfx_return_self
    inc rcx
    inc rdx
    jmp .rmsfx_cmp

.rmsfx_match:
    ; Suffix matches - return str_new(data, len-suffixlen)
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r13
    sub rsi, r14
    call str_new
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.rmsfx_return_self:
    mov rax, rbx
    INCREF rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_removesuffix

;; ============================================================================
;; str_method_encode(args, nargs) -> bytes
;; args[0]=self, args[1]=encoding (optional, default 'utf-8')
;; For now, supports 'utf-8' and 'ascii' — both just copy raw bytes.
;; ============================================================================
DEF_FUNC str_method_encode
    push rbx
    push r12
    ; args[0] = self (str)
    mov rbx, [rdi]             ; rbx = self str obj
    mov r12, [rbx + PyStrObject.ob_size]  ; r12 = length
    ; Allocate bytes object
    mov rdi, r12
    extern bytes_new
    call bytes_new
    ; Copy string data into bytes object
    lea rdi, [rax + PyBytesObject.data]
    lea rsi, [rbx + PyStrObject.data]
    mov rdx, r12
    push rax                   ; save bytes obj
    extern ap_memcpy
    call ap_memcpy
    pop rax                    ; return bytes obj
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC str_method_encode


;; ############################################################################
;;                         LIST METHODS
;; ############################################################################

;; ============================================================================
;; list_method_append(args, nargs) -> None
;; args[0]=self, args[1]=item
;; ============================================================================
DEF_FUNC list_method_append

    mov rax, [rdi]          ; self (list)
    mov rsi, [rdi + 16]     ; item payload
    mov edx, [rdi + 24]     ; item tag (16-byte stride)
    mov rdi, rax
    call list_append

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC list_method_append

;; ============================================================================
;; list_method_pop(args, nargs) -> removed item
;; args[0]=self, optionally args[1]=index (default: last)
;; ============================================================================
DEF_FUNC list_method_pop
    push rbx
    push r12
    push r13

    mov rax, rdi            ; rax = args ptr
    mov rbx, [rax]          ; self (list)
    mov r12, rsi            ; nargs

    ; Get index
    cmp r12, 2
    jge .pop_idx
    ; Default: pop last element
    mov r13, [rbx + PyListObject.ob_size]
    dec r13                 ; index = size - 1
    jmp .pop_do

.pop_idx:
    mov rdi, [rax + 16]    ; args[1]
    mov edx, [rax + 24]    ; args[1] tag
    call int_to_i64
    mov r13, rax

    ; Handle negative index
    test r13, r13
    jns .pop_do
    add r13, [rbx + PyListObject.ob_size]

.pop_do:
    ; Bounds check
    cmp r13, 0
    jl .pop_error
    cmp r13, [rbx + PyListObject.ob_size]
    jge .pop_error

    ; Get the item (it already has refs from being in the list)
    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r13
    shl rcx, 4              ; index * 16
    mov r12, [rax + rcx]    ; r12 = item payload to return
    push qword [rax + rcx + 8]  ; save item tag on stack
    ; Don't DECREF since we're transferring ownership to caller

    ; Shift items down: for i = index .. size-2, items[i] = items[i+1]
    mov rcx, r13            ; i = index
    mov rdx, [rbx + PyListObject.ob_size]
    dec rdx                 ; size - 1
.pop_shift:
    cmp rcx, rdx
    jge .pop_shrink
    mov rax, [rbx + PyListObject.ob_item]
    mov r8, rcx
    shl r8, 4               ; i * 16
    mov r9, [rax + r8 + 16] ; items[i+1] payload
    mov r10, [rax + r8 + 24] ; items[i+1] tag
    mov [rax + r8], r9       ; items[i] payload
    mov [rax + r8 + 8], r10  ; items[i] tag
    inc rcx
    jmp .pop_shift

.pop_shrink:
    dec qword [rbx + PyListObject.ob_size]

    ; Return item (ownership transferred, no extra INCREF needed)
    mov rax, r12
    pop rdx                  ; item tag
    pop r13
    pop r12
    pop rbx
    leave
    ret

.pop_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "pop index out of range"
    call raise_exception
END_FUNC list_method_pop

;; ============================================================================
;; list_method_insert(args, nargs) -> None
;; args[0]=self, args[1]=index, args[2]=item
;; ============================================================================
DEF_FUNC list_method_insert
    push rbx
    push r12
    push r13
    push r14

    mov rax, rdi            ; args (16-byte stride)
    mov rbx, [rax]          ; self = args[0]
    push rax

    ; Get index
    mov rdi, [rax + 16]     ; args[1] payload (16B stride)
    mov edx, [rax + 24]     ; args[1] tag
    call int_to_i64
    mov r12, rax            ; index

    pop rax
    mov r13, [rax + 32]     ; item = args[2] payload (16B stride)
    mov r14, [rax + 40]     ; item tag = args[2] tag

    ; Clamp index to [0, size]
    test r12, r12
    jns .ins_pos
    add r12, [rbx + PyListObject.ob_size]
    test r12, r12
    jns .ins_pos
    xor r12d, r12d
.ins_pos:
    cmp r12, [rbx + PyListObject.ob_size]
    jle .ins_ok
    mov r12, [rbx + PyListObject.ob_size]
.ins_ok:

    ; First append a dummy to grow the list if needed
    ; (reuse list_append logic for growth, then shift)
    ; Actually, let's just handle growth manually:
    ; Check if size == allocated
    mov rax, [rbx + PyListObject.ob_size]
    cmp rax, [rbx + PyListObject.allocated]
    jl .ins_no_grow
    ; Double capacity
    mov rdi, [rbx + PyListObject.allocated]
    shl rdi, 1
    mov [rbx + PyListObject.allocated], rdi
    mov rdi, [rbx + PyListObject.ob_item]
    mov rsi, [rbx + PyListObject.allocated]
    shl rsi, 4              ; new_cap * 16
    call ap_realloc
    mov [rbx + PyListObject.ob_item], rax
.ins_no_grow:

    ; Shift items up: for i = size-1 down to index, items[i+1] = items[i]
    mov rcx, [rbx + PyListObject.ob_size]
    dec rcx                 ; i = size - 1
.ins_shift:
    cmp rcx, r12
    jl .ins_place
    mov rax, [rbx + PyListObject.ob_item]
    mov r8, rcx
    shl r8, 4               ; i * 16
    mov r9, [rax + r8]      ; payload
    mov r10, [rax + r8 + 8] ; tag
    mov [rax + r8 + 16], r9  ; items[i+1] payload
    mov [rax + r8 + 24], r10 ; items[i+1] tag
    dec rcx
    jmp .ins_shift

.ins_place:
    ; Place item at index (16-byte fat slot)
    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r12
    shl rcx, 4              ; index * 16
    mov [rax + rcx], r13    ; payload
    ; Store item tag from args
    mov [rax + rcx + 8], r14
    INCREF_VAL r13, r14
    inc qword [rbx + PyListObject.ob_size]

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_method_insert

;; ============================================================================
;; list_method_reverse(args, nargs) -> None
;; args[0]=self, reverse in place
;; ============================================================================
DEF_FUNC list_method_reverse

    mov rax, [rdi]          ; self
    mov rcx, [rax + PyListObject.ob_size]
    test rcx, rcx
    jz .rev_done

    mov rdi, [rax + PyListObject.ob_item]
    xor esi, esi            ; lo = 0
    dec rcx                 ; hi = size - 1
.rev_loop:
    cmp rsi, rcx
    jge .rev_done
    ; Compute byte offsets for lo and hi
    mov rax, rsi
    shl rax, 4              ; lo * 16
    mov rdx, rcx
    shl rdx, 4              ; hi * 16
    ; Swap 16-byte fat elements (payload + tag)
    mov r8, [rdi + rax]      ; lo payload
    mov r9, [rdi + rax + 8]  ; lo tag
    mov r10, [rdi + rdx]     ; hi payload
    mov r11, [rdi + rdx + 8] ; hi tag
    mov [rdi + rax], r10
    mov [rdi + rax + 8], r11
    mov [rdi + rdx], r8
    mov [rdi + rdx + 8], r9
    inc rsi
    dec rcx
    jmp .rev_loop

.rev_done:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC list_method_reverse

;; ============================================================================
;; list_method_sort(args, nargs) -> None
;; Bubble sort using generic tp_richcompare (PY_GT) for comparison
;; args[0]=self
;; ============================================================================
LS_I     equ 8
LS_SWAP  equ 16
LS_LTAG  equ 24
LS_RTAG  equ 28
LS_FRAME equ 32
DEF_FUNC list_method_sort, LS_FRAME
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rbx + PyListObject.ob_size]

    ; Bubble sort
    cmp r12, 2
    jl .sort_done

.sort_outer:
    mov qword [rbp - LS_SWAP], 0    ; swapped = false
    mov r13, 1                       ; i = 1
.sort_inner:
    cmp r13, r12
    jge .sort_check
    ; Compare items[i-1] and items[i] using tp_richcompare(left, right, PY_GT)
    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r13
    shl rcx, 4                       ; i * 16
    mov rdi, [rax + rcx - 16]        ; left = items[i-1] payload
    mov rsi, [rax + rcx]             ; right = items[i] payload
    mov r8d, [rax + rcx - 8]         ; left tag
    mov [rbp - LS_LTAG], r8d
    mov r8d, [rax + rcx + 8]         ; right tag
    mov [rbp - LS_RTAG], r8d
    mov [rbp - LS_I], r13            ; save i

    ; Get left's type for tp_richcompare (tag at [rax + rcx - 8])
    cmp dword [rax + rcx - 8], TAG_SMALLINT
    je .sort_smallint_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .sort_have_type
.sort_smallint_type:
    lea rax, [rel int_type]
.sort_have_type:
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .sort_no_swap                 ; no richcompare → don't swap

    ; Call tp_richcompare(left, right, PY_GT, left_tag, right_tag)
    mov ecx, [rbp - LS_LTAG]
    mov r8d, [rbp - LS_RTAG]
    mov edx, PY_GT
    call rax
    ; rax = bool result (bool_true or bool_false)
    mov r13, [rbp - LS_I]           ; restore i

    ; Check if result is bool_true (meaning left > right → swap)
    lea rcx, [rel bool_true]
    cmp rax, rcx
    sete cl                          ; cl = 1 if swap needed
    ; DECREF the bool result
    push rcx
    mov rdi, rax
    call obj_decref
    pop rcx
    test cl, cl
    jz .sort_no_swap

    ; Swap items[i-1] and items[i] (16-byte fat elements)
    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r13
    shl rcx, 4                       ; i * 16
    mov r8, [rax + rcx - 16]         ; items[i-1] payload
    mov r9, [rax + rcx - 8]          ; items[i-1] tag
    mov r10, [rax + rcx]             ; items[i] payload
    mov r11, [rax + rcx + 8]         ; items[i] tag
    mov [rax + rcx - 16], r10
    mov [rax + rcx - 8], r11
    mov [rax + rcx], r8
    mov [rax + rcx + 8], r9
    mov qword [rbp - LS_SWAP], 1    ; swapped = true

.sort_no_swap:
    inc r13
    jmp .sort_inner

.sort_check:
    cmp qword [rbp - LS_SWAP], 0
    jnz .sort_outer

.sort_done:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_method_sort

;; ============================================================================
;; list_method_index(args, nargs) -> SmallInt index
;; args[0]=self, args[1]=value
;; Linear scan with pointer equality
;; ============================================================================
DEF_FUNC list_method_index
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; value to find
    mov r13, [rbx + PyListObject.ob_size]

    xor ecx, ecx
.index_loop:
    cmp rcx, r13
    jge .index_not_found
    mov rax, [rbx + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4              ; index * 16
    cmp r12, [rax + rdx]    ; compare payload
    je .index_found

    ; Also check SmallInt equality: if both are SmallInts, compare values
    mov rdi, r12
    test rdi, rdi
    jns .index_check_str    ; not SmallInt, try string
    mov rax, [rbx + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4
    mov rsi, [rax + rdx]    ; list item payload
    test rsi, rsi
    jns .index_next         ; list item not SmallInt
    ; Both SmallInts - already compared by pointer above (SmallInts with same
    ; value have the same tagged representation), so no match
    jmp .index_next

.index_check_str:
    ; Try string comparison: if both are str_type, compare data
    mov rax, [rbx + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4
    mov rsi, [rax + rdx]    ; list item payload
    cmp dword [rax + rdx + 8], TAG_SMALLINT
    je .index_next          ; list item is SmallInt
    mov rax, [rdi + PyObject.ob_type]
    lea r8, [rel str_type]
    cmp rax, r8
    jne .index_next
    mov rax, [rsi + PyObject.ob_type]
    cmp rax, r8
    jne .index_next
    ; Both strings - compare
    push rcx
    push rdi
    push rsi
    lea rdi, [rdi + PyStrObject.data]
    lea rsi, [rsi + PyStrObject.data]
    call ap_strcmp
    pop rsi
    pop rdi
    pop rcx
    test eax, eax
    jz .index_found

.index_next:
    inc rcx
    jmp .index_loop

.index_found:
    mov rdi, rcx
    call int_from_i64
    pop r13
    pop r12
    pop rbx
    leave
    ret

.index_not_found:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "x not in list"
    call raise_exception
END_FUNC list_method_index

;; ============================================================================
;; list_method_count(args, nargs) -> SmallInt
;; args[0]=self, args[1]=value
;; ============================================================================
LC_IDX    equ 8
LC_FRAME  equ 8

DEF_FUNC list_method_count, LC_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; value payload
    mov r15d, [rdi + 24]    ; value tag
    mov r13, [rbx + PyListObject.ob_size]
    xor r14d, r14d          ; count = 0

    mov qword [rbp - LC_IDX], 0
.count_loop:
    mov rcx, [rbp - LC_IDX]
    cmp rcx, r13
    jge .count_done
    mov rax, [rbx + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4              ; index * 16
    mov rdi, [rax + rdx]    ; item payload
    mov r8d, [rax + rdx + 8] ; item tag

    ; Fast path: payload equality
    cmp rdi, r12
    je .count_hit

    ; __eq__ dispatch
    cmp r8d, TAG_SMALLINT
    je .count_eq_int
    mov rax, [rdi + PyObject.ob_type]
    jmp .count_eq_call
.count_eq_int:
    lea rax, [rel int_type]
.count_eq_call:
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .count_next           ; no richcompare → not equal
    ; tp_richcompare(item, value, PY_EQ, item_tag, value_tag)
    mov rsi, r12
    mov edx, PY_EQ
    mov ecx, r8d
    mov r8d, r15d
    call rax
    lea rcx, [rel bool_true]
    cmp rax, rcx
    je .count_hit
    ; DECREF result if non-NULL heap ptr
    test rax, rax
    jz .count_next
    mov rdi, rax
    call obj_decref
    jmp .count_next

.count_hit:
    inc r14
.count_next:
    inc qword [rbp - LC_IDX]
    jmp .count_loop

.count_done:
    mov rdi, r14
    call int_from_i64
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_method_count

;; ============================================================================
;; list_method_copy(args, nargs) -> new list (shallow copy)
;; args[0]=self
;; ============================================================================
DEF_FUNC list_method_copy
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rbx + PyListObject.ob_size]

    ; Create new list with same capacity
    mov rdi, r12
    test rdi, rdi
    jnz .copy_alloc
    mov rdi, 4
.copy_alloc:
    call list_new
    mov r13, rax            ; new list

    ; Append each item (list_append does INCREF)
    xor ecx, ecx
.copy_loop:
    cmp rcx, r12
    jge .copy_done
    push rcx
    mov rax, [rbx + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4              ; index * 16
    mov rsi, [rax + rdx]    ; payload
    mov edx, [rax + rdx + 8] ; tag from fat slot
    mov rdi, r13
    call list_append
    pop rcx
    inc rcx
    jmp .copy_loop

.copy_done:
    mov rax, r13
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_method_copy

;; ============================================================================
;; list_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
DEF_FUNC list_method_clear
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rbx + PyListObject.ob_size]

    ; DECREF all items (fat 16-byte slots)
    xor r13d, r13d
.clear_loop:
    cmp r13, r12
    jge .clear_done
    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r13
    shl rcx, 4              ; index * 16
    mov rdi, [rax + rcx]    ; payload
    mov rsi, [rax + rcx + 8] ; tag
    push r13
    DECREF_VAL rdi, rsi
    pop r13
    inc r13
    jmp .clear_loop

.clear_done:
    mov qword [rbx + PyListObject.ob_size], 0

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_method_clear

;; ============================================================================
;; list_method_extend(args, nargs) -> None
;; args[0]=self, args[1]=iterable (must be a list for now)
;; ============================================================================
DEF_FUNC list_method_extend
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; iterable (list)
    mov r13, [r12 + PyListObject.ob_size]

    xor r14d, r14d
.extend_loop:
    cmp r14, r13
    jge .extend_done
    push r14
    mov rax, [r12 + PyListObject.ob_item]
    mov rcx, r14
    shl rcx, 4              ; index * 16
    mov rsi, [rax + rcx]    ; payload
    mov edx, [rax + rcx + 8] ; tag from fat slot
    mov rdi, rbx
    call list_append
    pop r14
    inc r14
    jmp .extend_loop

.extend_done:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_method_extend


;; ############################################################################
;;                         DICT METHODS
;; ############################################################################

;; ============================================================================
;; dict_method_get(args, nargs) -> value or None
;; args[0]=self, args[1]=key, optionally args[2]=default
;; ============================================================================
DEF_FUNC dict_method_get
    push rbx
    push r12

    mov rax, rdi            ; args
    mov rbx, [rax]          ; self (dict)
    mov r12, rsi            ; nargs
    push rax

    ; dict_get(self, key)
    mov rdi, rbx
    mov rsi, [rax + 16]     ; key payload
    mov edx, [rax + 24]     ; key tag
    call dict_get

    test edx, edx
    jnz .dg_found

    ; Not found - return default or None
    pop rcx                 ; args
    cmp r12, 3
    jl .dg_ret_none
    ; Return args[2] (default)
    mov rax, [rcx + 32]     ; default payload
    mov edx, [rcx + 40]     ; default tag
    INCREF_VAL rax, rdx
    pop r12
    pop rbx
    leave
    ret

.dg_ret_none:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.dg_found:
    add rsp, 8              ; discard saved args
    ; INCREF the value (dict_get returns borrowed ref, rdx=tag)
    INCREF_VAL rax, rdx
    ; rdx already has correct tag from dict_get
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_method_get

;; ============================================================================
;; dict_method_keys(args, nargs) -> list of keys
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_keys
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    ; Create result list
    mov rdi, [rbx + PyDictObject.ob_size]
    test rdi, rdi
    jnz .dk_alloc
    mov rdi, 4
.dk_alloc:
    call list_new
    mov r12, rax            ; result list

    ; Iterate entries
    mov r13, [rbx + PyDictObject.capacity]
    xor r14d, r14d          ; index

.dk_loop:
    cmp r14, r13
    jge .dk_done
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r14, DICT_ENTRY_SIZE
    add rax, rcx

    ; Check if slot is occupied (key != NULL and value_tag != TAG_NULL)
    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .dk_next
    mov rcx, [rax + DictEntry.value_tag]
    test rcx, rcx
    jz .dk_next

    ; Append key to list (read key_tag from entry)
    push r14
    mov edx, [rax + DictEntry.key_tag]
    mov rsi, rdi            ; key
    mov rdi, r12            ; list
    call list_append
    pop r14

.dk_next:
    inc r14
    jmp .dk_loop

.dk_done:
    mov rax, r12
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_method_keys

;; ============================================================================
;; dict_method_values(args, nargs) -> list of values
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_values
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    mov rdi, [rbx + PyDictObject.ob_size]
    test rdi, rdi
    jnz .dv_alloc
    mov rdi, 4
.dv_alloc:
    call list_new
    mov r12, rax

    mov r13, [rbx + PyDictObject.capacity]
    xor r14d, r14d

.dv_loop:
    cmp r14, r13
    jge .dv_done
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r14, DICT_ENTRY_SIZE
    add rax, rcx

    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .dv_next
    mov rcx, [rax + DictEntry.value_tag]
    test rcx, rcx
    jz .dv_next                 ; TAG_NULL = empty slot

    ; Append value to list (read value_tag from entry)
    push r14
    mov edx, [rax + DictEntry.value_tag]
    mov rsi, [rax + DictEntry.value]  ; value payload
    mov rdi, r12
    call list_append
    pop r14

.dv_next:
    inc r14
    jmp .dv_loop

.dv_done:
    mov rax, r12
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_method_values

;; ============================================================================
;; dict_method_items(args, nargs) -> list of (key, value) tuples
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_items
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    mov rdi, [rbx + PyDictObject.ob_size]
    test rdi, rdi
    jnz .di_alloc
    mov rdi, 4
.di_alloc:
    call list_new
    mov r12, rax             ; result list

    mov r13, [rbx + PyDictObject.capacity]
    xor r14d, r14d

.di_loop:
    cmp r14, r13
    jge .di_done
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r14, DICT_ENTRY_SIZE
    add rax, rcx

    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .di_next
    mov r8, [rax + DictEntry.value_tag]
    test r8, r8
    jz .di_next                 ; TAG_NULL = empty slot
    mov rcx, [rax + DictEntry.value]

    ; Create (key, value) tuple
    push r14
    push rdi                ; save key
    push rcx                ; save value
    push r8                 ; save value_tag

    mov rdi, 2
    call tuple_new
    mov r14, rax            ; tuple

    pop r8                  ; value_tag
    pop rcx                 ; value
    pop rdi                 ; key

    ; Store key in tuple[0] (fat 16-byte slot)
    mov [r14 + PyTupleObject.ob_item], rdi
    mov qword [r14 + PyTupleObject.ob_item + 8], TAG_PTR
    INCREF rdi

    ; Store value in tuple[1] (fat 16-byte slot, use stored tag)
    mov [r14 + PyTupleObject.ob_item + 16], rcx
    mov [r14 + PyTupleObject.ob_item + 24], r8
    INCREF_VAL rcx, r8

    ; Append tuple to list
    mov rdi, r12
    mov rsi, r14
    mov edx, TAG_PTR
    call list_append

    ; DECREF tuple (list_append did INCREF)
    mov rdi, r14
    call obj_decref

    pop r14

.di_next:
    inc r14
    jmp .di_loop

.di_done:
    mov rax, r12
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_method_items

;; ============================================================================
;; dict_method_pop(args, nargs) -> value
;; args[0]=self, args[1]=key, optionally args[2]=default
;; ============================================================================
DEF_FUNC dict_method_pop
dict_method_pop_v2 equ dict_method_pop
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r14, rdi            ; r14 = args
    mov rbx, [r14]          ; self
    mov r12, rsi            ; nargs
    mov r13, [r14 + 16]     ; key payload (16-byte stride)
    mov r15d, [r14 + 24]    ; key tag

    ; Try dict_get
    mov rdi, rbx
    mov rsi, r13
    mov edx, r15d           ; key tag
    call dict_get
    test edx, edx
    jz .dpop2_not_found

    ; dict_get returns fat (rax=payload, rdx=tag)
    INCREF_VAL rax, rdx
    push rdx                ; save tag across dict_del
    push rax                ; save payload

    mov rdi, rbx
    mov rsi, r13
    mov edx, r15d           ; key tag
    call dict_del

    pop rax                 ; restore payload
    pop rdx                 ; restore tag
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.dpop2_not_found:
    cmp r12, 3
    jl .dpop2_error
    mov rax, [r14 + 32]     ; default = args[2] payload (16-byte stride)
    mov edx, [r14 + 40]     ; default tag
    INCREF_VAL rax, rdx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.dpop2_error:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "key not found"
    call raise_exception
END_FUNC dict_method_pop

;; ============================================================================
;; dict_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_clear
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    ; DECREF all keys and values
    mov r12, [rbx + PyDictObject.capacity]
    xor r13d, r13d

.dc_loop:
    cmp r13, r12
    jge .dc_clear_entries

    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r13, DICT_ENTRY_SIZE
    lea r14, [rax + rcx]    ; r14 = entry ptr

    mov rdi, [r14 + DictEntry.key]
    test rdi, rdi
    jz .dc_next

    ; DECREF key (tag-aware)
    mov rsi, [r14 + DictEntry.key_tag]
    DECREF_VAL rdi, rsi

    ; DECREF value (tag-aware)
    mov rdi, [r14 + DictEntry.value]
    mov rsi, [r14 + DictEntry.value_tag]
    DECREF_VAL rdi, rsi

.dc_next:
    inc r13
    jmp .dc_loop

.dc_clear_entries:
    ; Zero out all entries
    mov rdi, [rbx + PyDictObject.entries]
    xor esi, esi
    imul rdx, r12, DICT_ENTRY_SIZE
    call ap_memset

    ; Reset size to 0
    mov qword [rbx + PyDictObject.ob_size], 0

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_method_clear

;; ============================================================================
;; dict_method_update(args, nargs) -> None
;; args[0]=self, args[1]=other_dict
;; Merge other_dict into self
;; ============================================================================
DEF_FUNC dict_method_update
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 16]     ; other dict

    mov r13, [r12 + PyDictObject.capacity]
    xor r14d, r14d

.du_loop:
    cmp r14, r13
    jge .du_done

    mov rax, [r12 + PyDictObject.entries]
    imul rcx, r14, DICT_ENTRY_SIZE
    add rax, rcx

    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .du_next
    mov rcx, [rax + DictEntry.value_tag]
    test rcx, rcx
    jz .du_next                 ; TAG_NULL = empty slot

    ; dict_set(self, key, value, value_tag, key_tag)
    push r14
    mov r8d, [rax + DictEntry.key_tag]    ; key tag from entry
    mov ecx, [rax + DictEntry.value_tag]  ; value tag from entry
    mov rdx, [rax + DictEntry.value]      ; value payload
    mov rsi, rdi            ; key
    mov rdi, rbx            ; self
    call dict_set
    pop r14

.du_next:
    inc r14
    jmp .du_loop

.du_done:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_method_update

;; ============================================================================
;; dict_method_setdefault(args, nargs) -> value
;; args[0]=self, args[1]=key, args[2]=default (optional, default=None)
;; ============================================================================
DEF_FUNC dict_method_setdefault
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, [rdi]          ; self (dict)
    mov r12, [rdi + 16]     ; key payload
    mov r14d, [rdi + 24]    ; key tag
    mov r13, rsi            ; nargs

    ; Save args ptr for default value access
    push rdi

    ; dict_get(self, key)
    mov rdi, rbx
    mov rsi, r12
    mov edx, r14d           ; key tag
    call dict_get

    test edx, edx
    jnz .sd_found

    ; Not found - determine default value
    pop rdi                 ; restore args ptr
    cmp r13, 3
    jl .sd_use_none
    mov r13, [rdi + 32]     ; default = args[2] payload
    mov r15d, [rdi + 40]    ; default = args[2] tag
    jmp .sd_set_default

.sd_use_none:
    lea r13, [rel none_singleton]
    mov r15d, TAG_PTR

.sd_set_default:
    ; dict_set(self, key, default_val)
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov ecx, r15d           ; default val tag
    mov r8d, r14d           ; key tag
    call dict_set

    ; INCREF and return default_val
    INCREF_VAL r13, r15
    mov rax, r13
    mov edx, r15d           ; return tag
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sd_found:
    add rsp, 8              ; discard saved args ptr
    ; INCREF the found value (dict_get returns borrowed ref, rdx=tag)
    INCREF_VAL rax, rdx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_method_setdefault

;; ============================================================================
;; dict_method_copy(args, nargs) -> new dict (shallow copy)
;; args[0]=self
;; ============================================================================
DEF_FUNC dict_method_copy
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    ; Create new dict
    call dict_new
    mov r12, rax            ; r12 = new dict

    ; Iterate over self's entries
    mov r13, [rbx + PyDictObject.capacity]
    xor r14d, r14d          ; index

.dcopy_loop:
    cmp r14, r13
    jge .dcopy_done

    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r14, DICT_ENTRY_SIZE
    add rax, rcx

    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .dcopy_next
    mov rcx, [rax + DictEntry.value_tag]
    test rcx, rcx
    jz .dcopy_next              ; TAG_NULL = empty slot

    ; dict_set(new_dict, key, value, value_tag, key_tag)
    push r14
    mov r8d, [rax + DictEntry.key_tag]    ; key tag from entry
    mov ecx, [rax + DictEntry.value_tag]  ; value tag from entry
    mov rdx, [rax + DictEntry.value]      ; value payload
    mov rsi, rdi            ; key
    mov rdi, r12            ; new dict
    call dict_set
    pop r14

.dcopy_next:
    inc r14
    jmp .dcopy_loop

.dcopy_done:
    mov rax, r12
    mov edx, TAG_PTR         ; dict is heap ptr
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dict_method_copy

;; ============================================================================
;; dict_method_popitem(args, nargs) -> (key, value) tuple
;; args[0]=self. Removes and returns last inserted item.
;; ============================================================================
DEF_FUNC dict_method_popitem
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (dict)

    ; Check if dict is empty
    cmp qword [rbx + PyDictObject.ob_size], 0
    je .dpopitem_empty

    ; Find last non-NULL entry by scanning backward
    mov r12, [rbx + PyDictObject.capacity]
    dec r12                  ; start from capacity-1

.dpopitem_scan:
    cmp r12, 0
    jl .dpopitem_empty       ; shouldn't happen, but safety
    mov rax, [rbx + PyDictObject.entries]
    imul rcx, r12, DICT_ENTRY_SIZE
    add rax, rcx

    mov r13, [rax + DictEntry.key]
    test r13, r13
    jz .dpopitem_prev
    mov rcx, [rax + DictEntry.value_tag]
    test rcx, rcx
    jz .dpopitem_prev           ; TAG_NULL = empty slot
    mov r14, [rax + DictEntry.value]
    jmp .dpopitem_found

.dpopitem_prev:
    dec r12
    jmp .dpopitem_scan

.dpopitem_found:
    ; r13 = key, r14 = value, rcx = value_tag
    push rcx                 ; save value_tag across tuple_new
    ; Create 2-tuple
    mov rdi, 2
    call tuple_new
    pop rcx                  ; restore value_tag
    mov r12, rax             ; r12 = tuple

    ; Set tuple[0] = key, tuple[1] = value (fat 16-byte slots)
    mov [r12 + PyTupleObject.ob_item], r13
    ; Key from dict entries — always heap ptr (strings)
    mov qword [r12 + PyTupleObject.ob_item + 8], TAG_PTR
    INCREF r13
    mov [r12 + PyTupleObject.ob_item + 16], r14
    ; Use stored value_tag from dict entry
    mov [r12 + PyTupleObject.ob_item + 24], rcx
    INCREF_VAL r14, rcx

    ; Delete key from dict
    mov rdi, rbx
    mov rsi, r13
    mov edx, TAG_PTR
    call dict_del

    mov rax, r12
    mov edx, TAG_PTR         ; tuple is heap ptr
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.dpopitem_empty:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "dictionary is empty"
    call raise_exception
END_FUNC dict_method_popitem

;; ============================================================================
;; list_method_remove(args, nargs) -> None
;; args[0]=self, args[1]=value
;; Removes first occurrence of value. Raises ValueError if not found.
;; ============================================================================
DEF_FUNC list_method_remove
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, [rdi]          ; self (list)
    mov r12, [rdi + 16]     ; value payload
    mov r15d, [rdi + 24]    ; value tag
    mov r13, [rbx + PyListObject.ob_size]

    xor r14d, r14d          ; index = 0

.lremove_loop:
    cmp r14, r13
    jge .lremove_not_found

    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r14
    shl rcx, 4              ; index * 16
    mov rdi, [rax + rcx]    ; item payload
    mov r8d, [rax + rcx + 8] ; item tag

    ; Fast path: payload equality (same SmallInt or same object)
    cmp rdi, r12
    je .lremove_found

    ; __eq__ dispatch: get item type's tp_richcompare
    cmp r8d, TAG_SMALLINT
    je .lremove_eq_int
    mov rax, [rdi + PyObject.ob_type]
    jmp .lremove_eq_call
.lremove_eq_int:
    lea rax, [rel int_type]
.lremove_eq_call:
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .lremove_next         ; no richcompare → not equal
    ; tp_richcompare(item, value, PY_EQ, item_tag, right_tag)
    ; rdi = item payload (already set)
    mov rsi, r12             ; value payload
    mov edx, PY_EQ
    mov ecx, r8d             ; item tag
    push r8                  ; save item tag across call
    mov r8d, r15d            ; value tag
    call rax
    pop r8
    ; rax = result (bool_true or bool_false)
    lea rcx, [rel bool_true]
    cmp rax, rcx
    je .lremove_found
    ; DECREF result if heap ptr
    test rax, rax
    jz .lremove_next
    mov rdi, rax
    call obj_decref

.lremove_next:
    inc r14
    jmp .lremove_loop

.lremove_found:
    ; r14 = index of found item
    ; Get the item for DECREF (read payload + tag)
    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r14
    shl rcx, 4              ; index * 16
    mov r12, [rax + rcx]    ; item payload (save for DECREF)
    mov r13, [rax + rcx + 8] ; item tag (save for DECREF)

    ; Shift remaining items left (16-byte fat elements)
    mov rcx, r14
    mov rdx, [rbx + PyListObject.ob_size]
    dec rdx                  ; size - 1
.lremove_shift:
    cmp rcx, rdx
    jge .lremove_shrink
    mov rax, [rbx + PyListObject.ob_item]
    mov r8, rcx
    shl r8, 4                ; i * 16
    mov r9, [rax + r8 + 16]  ; items[i+1] payload
    mov r10, [rax + r8 + 24] ; items[i+1] tag
    mov [rax + r8], r9        ; items[i] payload
    mov [rax + r8 + 8], r10   ; items[i] tag
    inc rcx
    jmp .lremove_shift

.lremove_shrink:
    dec qword [rbx + PyListObject.ob_size]

    ; DECREF the removed item (fat value)
    mov rdi, r12
    mov rsi, r13
    DECREF_VAL rdi, rsi

    ; Return None
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.lremove_not_found:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "list.remove(x): x not in list"
    call raise_exception
END_FUNC list_method_remove

;; ============================================================================
;; tuple_method_index(args, nargs) -> SmallInt index
;; args[0]=self (tuple), args[1]=value
;; ============================================================================
DEF_FUNC tuple_method_index
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (tuple)
    mov r12, [rdi + 16]     ; value to find (payload)
    mov r14d, [rdi + 24]    ; value tag
    mov r13, [rbx + PyTupleObject.ob_size]

    xor ecx, ecx
.tindex_loop:
    cmp rcx, r13
    jge .tindex_not_found

    ; Tuple items are inline at [self + PyTupleObject.ob_item + i*16]
    mov rax, rcx
    shl rax, 4
    mov rdx, rax             ; save offset for tag access
    mov rax, [rbx + PyTupleObject.ob_item + rax]

    ; Check pointer equality
    cmp rax, r12
    je .tindex_found

    ; Check SmallInt equality (item tag at [rbx + ob_item + offset + 8])
    cmp dword [rbx + PyTupleObject.ob_item + rdx + 8], TAG_SMALLINT
    jne .tindex_check_str
    cmp r14d, TAG_SMALLINT
    jne .tindex_next
    ; Both SmallInts - already compared by pointer above
    jmp .tindex_next

.tindex_check_str:
    ; Try string comparison: if both are str_type, compare data
    mov rsi, rax             ; tuple item
    cmp r14d, TAG_SMALLINT
    je .tindex_next          ; value is SmallInt, item is not
    mov rax, [r12 + PyObject.ob_type]
    lea r8, [rel str_type]
    cmp rax, r8
    jne .tindex_next
    mov rax, [rsi + PyObject.ob_type]
    cmp rax, r8
    jne .tindex_next
    ; Both strings - compare
    push rcx
    push rsi
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rsi + PyStrObject.data]
    call ap_strcmp
    pop rsi
    pop rcx
    test eax, eax
    jz .tindex_found

.tindex_next:
    inc rcx
    jmp .tindex_loop

.tindex_found:
    mov rdi, rcx
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.tindex_not_found:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "tuple.index(x): x not in tuple"
    call raise_exception
END_FUNC tuple_method_index

;; ============================================================================
;; tuple_method_count(args, nargs) -> SmallInt
;; args[0]=self (tuple), args[1]=value
;; ============================================================================
DEF_FUNC tuple_method_count
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self (tuple)
    mov r12, [rdi + 16]     ; value
    mov r13, [rbx + PyTupleObject.ob_size]
    xor r14d, r14d          ; count = 0

    xor ecx, ecx
.tcount_loop:
    cmp rcx, r13
    jge .tcount_done

    mov rax, rcx
    shl rax, 4
    mov rax, [rbx + PyTupleObject.ob_item + rax]

    ; Check pointer equality
    cmp rax, r12
    jne .tcount_next
    inc r14

.tcount_next:
    inc rcx
    jmp .tcount_loop

.tcount_done:
    mov rdi, r14
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_method_count


;; ############################################################################
;;                         SET METHODS
;; ############################################################################

;; ============================================================================
;; set_method_add(args, nargs) -> None
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_add
    cmp rsi, 2
    jne .sma_error

    mov rax, rdi            ; args ptr
    mov rdi, [rax]          ; self (set)
    mov rsi, [rax + 16]     ; elem payload
    mov edx, [rax + 24]     ; elem tag
    call set_add

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.sma_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "add() takes exactly one argument"
    call raise_exception
END_FUNC set_method_add

;; ============================================================================
;; set_method_remove(args, nargs) -> None (raises KeyError if missing)
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_remove
    cmp rsi, 2
    jne .smr_error

    mov rax, rdi
    mov rdi, [rax]          ; self
    mov rsi, [rax + 16]     ; elem payload
    mov edx, [rax + 24]     ; elem tag
    call set_remove
    test eax, eax
    jnz .smr_keyerr

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.smr_keyerr:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "element not in set"
    call raise_exception

.smr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "remove() takes exactly one argument"
    call raise_exception
END_FUNC set_method_remove

;; ============================================================================
;; set_method_discard(args, nargs) -> None (no error if missing)
;; args[0]=self, args[1]=elem
;; ============================================================================
DEF_FUNC set_method_discard
    cmp rsi, 2
    jne .smd_error

    mov rax, rdi
    mov rdi, [rax]          ; self
    mov rsi, [rax + 16]     ; elem payload
    mov edx, [rax + 24]     ; elem tag
    call set_remove
    ; Ignore return value (don't care if not found)

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.smd_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "discard() takes exactly one argument"
    call raise_exception
END_FUNC set_method_discard

;; ============================================================================
;; set_method_pop(args, nargs) -> removed element
;; args[0]=self
;; Scans for first occupied entry, removes and returns it.
;; ============================================================================
SMP_FRAME equ 16    ; save self + entry ptr
DEF_FUNC set_method_pop, SMP_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 1
    jne .smpop_error

    mov rbx, [rdi]          ; self (set)

    ; Check empty
    cmp qword [rbx + PyDictObject.ob_size], 0
    je .smpop_empty

    ; Scan for first non-empty entry
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx            ; index

.smpop_scan:
    cmp rcx, r13
    jge .smpop_empty         ; shouldn't happen

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12             ; entry ptr

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    jne .smpop_found
    inc ecx
    jmp .smpop_scan

.smpop_found:
    ; rax = entry ptr with valid key
    ; Get key (return value) — DON'T incref, we're removing it
    mov rcx, [rax + SET_ENTRY_KEY]        ; key payload
    mov r12d, [rax + SET_ENTRY_KEY_TAG]   ; key tag

    ; Clear the entry (mark as empty)
    mov qword [rax + SET_ENTRY_KEY], 0
    mov qword [rax + SET_ENTRY_KEY_TAG], 0
    dec qword [rbx + PyDictObject.ob_size]

    ; Return the key (ownership transfers, no INCREF/DECREF needed)
    mov rax, rcx
    mov edx, r12d
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smpop_empty:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "pop from an empty set"
    call raise_exception

.smpop_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "pop() takes no arguments"
    call raise_exception
END_FUNC set_method_pop

;; ============================================================================
;; set_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
DEF_FUNC set_method_clear
    push rbx
    push r12
    push r13

    cmp rsi, 1
    jne .smc_error

    mov rbx, [rdi]          ; self (set)
    mov r12, [rbx + PyDictObject.entries]
    mov r13, [rbx + PyDictObject.capacity]
    xor ecx, ecx

.smc_loop:
    cmp rcx, r13
    jge .smc_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx                ; save index

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smc_next

    ; DECREF key
    mov rdi, [rax + SET_ENTRY_KEY]
    mov rsi, [rax + SET_ENTRY_KEY_TAG]
    mov qword [rax + SET_ENTRY_KEY], 0
    mov qword [rax + SET_ENTRY_KEY_TAG], 0
    DECREF_VAL rdi, rsi

.smc_next:
    pop rcx
    inc ecx
    jmp .smc_loop

.smc_done:
    mov qword [rbx + PyDictObject.ob_size], 0

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "clear() takes no arguments"
    call raise_exception
END_FUNC set_method_clear

;; ============================================================================
;; set_method_copy(args, nargs) -> new set (shallow copy)
;; args[0]=self
;; ============================================================================
DEF_FUNC set_method_copy
    push rbx
    push r12
    push r13
    push r14

    cmp rsi, 1
    jne .smcp_error

    mov r14, [rdi]          ; self (source set)

    ; Create new empty set
    call set_new
    mov rbx, rax            ; rbx = new set

    ; Iterate source entries
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smcp_loop:
    cmp rcx, r13
    jge .smcp_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smcp_next

    ; Add key to new set
    mov rdi, rbx            ; new set
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_add

.smcp_next:
    pop rcx
    inc ecx
    jmp .smcp_loop

.smcp_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smcp_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "copy() takes no arguments"
    call raise_exception
END_FUNC set_method_copy

;; ============================================================================
;; set_method_union(args, nargs) -> new set = self | other
;; args[0]=self, args[1]=other (iterable)
;; ============================================================================
DEF_FUNC set_method_union
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smu_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 16]     ; other set

    ; Copy self → new set
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    call set_new
    mov rbx, rax            ; new set
    xor ecx, ecx

.smu_copy_self:
    cmp rcx, r13
    jge .smu_add_other

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smu_cs_next

    mov rdi, rbx
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_add

.smu_cs_next:
    pop rcx
    inc ecx
    jmp .smu_copy_self

.smu_add_other:
    ; Now add all elements from other
    mov r12, [r15 + PyDictObject.entries]
    mov r13, [r15 + PyDictObject.capacity]
    xor ecx, ecx

.smu_add_loop:
    cmp rcx, r13
    jge .smu_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smu_al_next

    mov rdi, rbx
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_add

.smu_al_next:
    pop rcx
    inc ecx
    jmp .smu_add_loop

.smu_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smu_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "union() takes exactly one argument"
    call raise_exception
END_FUNC set_method_union

;; ============================================================================
;; set_method_intersection(args, nargs) -> new set = self & other
;; args[0]=self, args[1]=other
;; ============================================================================
DEF_FUNC set_method_intersection
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smi_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 16]     ; other

    call set_new
    mov rbx, rax            ; new set

    ; Iterate self, add if in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smi_loop:
    cmp rcx, r13
    jge .smi_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smi_next

    ; Check if key is in other
    push rax                ; save entry ptr
    mov rdi, r15            ; other set
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_contains
    pop rcx                 ; restore entry ptr (was rax)
    test eax, eax
    jz .smi_next

    ; In both — add to result
    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    mov edx, [rcx + SET_ENTRY_KEY_TAG]
    call set_add

.smi_next:
    pop rcx
    inc ecx
    jmp .smi_loop

.smi_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smi_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "intersection() takes exactly one argument"
    call raise_exception
END_FUNC set_method_intersection

;; ============================================================================
;; set_method_difference(args, nargs) -> new set = self - other
;; args[0]=self, args[1]=other
;; ============================================================================
DEF_FUNC set_method_difference
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smdf_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 16]     ; other

    call set_new
    mov rbx, rax            ; new set

    ; Iterate self, add if NOT in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smdf_loop:
    cmp rcx, r13
    jge .smdf_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smdf_next

    ; Check if key is in other
    push rax
    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_contains
    pop rcx                 ; entry ptr
    test eax, eax
    jnz .smdf_next          ; in other — skip

    ; NOT in other — add to result
    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    mov edx, [rcx + SET_ENTRY_KEY_TAG]
    call set_add

.smdf_next:
    pop rcx
    inc ecx
    jmp .smdf_loop

.smdf_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smdf_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "difference() takes exactly one argument"
    call raise_exception
END_FUNC set_method_difference

;; ============================================================================
;; set_method_symmetric_difference(args, nargs) -> new set = self ^ other
;; args[0]=self, args[1]=other
;; ============================================================================
DEF_FUNC set_method_symmetric_difference
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smsd_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 16]     ; other

    call set_new
    mov rbx, rax            ; new set

    ; Add elements in self but NOT in other
    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smsd_self_loop:
    cmp rcx, r13
    jge .smsd_other

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smsd_s_next

    push rax
    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_contains
    pop rcx
    test eax, eax
    jnz .smsd_s_next        ; in other, skip

    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    mov edx, [rcx + SET_ENTRY_KEY_TAG]
    call set_add

.smsd_s_next:
    pop rcx
    inc ecx
    jmp .smsd_self_loop

.smsd_other:
    ; Add elements in other but NOT in self
    mov r12, [r15 + PyDictObject.entries]
    mov r13, [r15 + PyDictObject.capacity]
    xor ecx, ecx

.smsd_other_loop:
    cmp rcx, r13
    jge .smsd_done

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smsd_o_next

    push rax
    mov rdi, r14
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_contains
    pop rcx
    test eax, eax
    jnz .smsd_o_next        ; in self, skip

    mov rdi, rbx
    mov rsi, [rcx + SET_ENTRY_KEY]
    mov edx, [rcx + SET_ENTRY_KEY_TAG]
    call set_add

.smsd_o_next:
    pop rcx
    inc ecx
    jmp .smsd_other_loop

.smsd_done:
    mov rax, rbx
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smsd_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "symmetric_difference() takes exactly one argument"
    call raise_exception
END_FUNC set_method_symmetric_difference

;; ============================================================================
;; set_method_issubset(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if every element of self is in other.
;; ============================================================================
DEF_FUNC set_method_issubset
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smss_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 16]     ; other

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smss_loop:
    cmp rcx, r13
    jge .smss_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smss_next

    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_contains
    test eax, eax
    jz .smss_false          ; not in other

.smss_next:
    pop rcx
    inc ecx
    jmp .smss_loop

.smss_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smss_false:
    pop rcx                 ; balance the push in loop
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smss_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "issubset() takes exactly one argument"
    call raise_exception
END_FUNC set_method_issubset

;; ============================================================================
;; set_method_issuperset(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if every element of other is in self.
;; ============================================================================
DEF_FUNC set_method_issuperset
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smis_error

    mov r14, [rdi + 16]     ; other (iterate this)
    mov r15, [rdi]          ; self (check contains)

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smis_loop:
    cmp rcx, r13
    jge .smis_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smis_next

    mov rdi, r15            ; check in self
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_contains
    test eax, eax
    jz .smis_false

.smis_next:
    pop rcx
    inc ecx
    jmp .smis_loop

.smis_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smis_false:
    pop rcx
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smis_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "issuperset() takes exactly one argument"
    call raise_exception
END_FUNC set_method_issuperset

;; ============================================================================
;; set_method_isdisjoint(args, nargs) -> bool
;; args[0]=self, args[1]=other
;; True if self and other have no common elements.
;; ============================================================================
DEF_FUNC set_method_isdisjoint
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .smdj_error

    mov r14, [rdi]          ; self
    mov r15, [rdi + 16]     ; other

    mov r12, [r14 + PyDictObject.entries]
    mov r13, [r14 + PyDictObject.capacity]
    xor ecx, ecx

.smdj_loop:
    cmp rcx, r13
    jge .smdj_true

    imul rax, rcx, SET_ENTRY_SIZE
    add rax, r12
    push rcx

    cmp qword [rax + SET_ENTRY_KEY_TAG], 0
    je .smdj_next

    mov rdi, r15
    mov rsi, [rax + SET_ENTRY_KEY]
    mov edx, [rax + SET_ENTRY_KEY_TAG]
    call set_contains
    test eax, eax
    jnz .smdj_false         ; found in other — not disjoint

.smdj_next:
    pop rcx
    inc ecx
    jmp .smdj_loop

.smdj_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smdj_false:
    pop rcx
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.smdj_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "isdisjoint() takes exactly one argument"
    call raise_exception
END_FUNC set_method_isdisjoint


;; ############################################################################
;;                       METHODS_INIT
;; ############################################################################

;; ============================================================================
;; methods_init()
;; Populate tp_dict for str_type, list_type, dict_type
;; ============================================================================
DEF_FUNC methods_init
    push rbx
    push r12

    ;; --- str methods ---
    call dict_new
    mov rbx, rax            ; rbx = str method dict

    mov rdi, rbx
    lea rsi, [rel mn_upper]
    lea rdx, [rel str_method_upper]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_lower]
    lea rdx, [rel str_method_lower]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_strip]
    lea rdx, [rel str_method_strip]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_startswith]
    lea rdx, [rel str_method_startswith]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_endswith]
    lea rdx, [rel str_method_endswith]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_find]
    lea rdx, [rel str_method_find]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_replace]
    lea rdx, [rel str_method_replace]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_join]
    lea rdx, [rel str_method_join]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_split]
    lea rdx, [rel str_method_split]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_format]
    lea rdx, [rel str_method_format]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_lstrip]
    lea rdx, [rel str_method_lstrip]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_rstrip]
    lea rdx, [rel str_method_rstrip]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel str_method_count]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_index]
    lea rdx, [rel str_method_index]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_rfind]
    lea rdx, [rel str_method_rfind]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isdigit]
    lea rdx, [rel str_method_isdigit]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isalpha]
    lea rdx, [rel str_method_isalpha]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_removeprefix]
    lea rdx, [rel str_method_removeprefix]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_removesuffix]
    lea rdx, [rel str_method_removesuffix]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_encode]
    lea rdx, [rel str_method_encode]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isalnum]
    lea rdx, [rel str_method_isalnum]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isspace]
    lea rdx, [rel str_method_isspace]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isupper]
    lea rdx, [rel str_method_isupper]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_islower]
    lea rdx, [rel str_method_islower]
    call add_method_to_dict

    ; Store dict in str_type.tp_dict
    lea rax, [rel str_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    ; INCREF the dict (type holds ref; dict_new gave us refcnt=1, which we keep)

    ;; --- list methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_append]
    lea rdx, [rel list_method_append]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel list_method_pop]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_insert]
    lea rdx, [rel list_method_insert]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_reverse]
    lea rdx, [rel list_method_reverse]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_sort]
    lea rdx, [rel list_method_sort]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_index]
    lea rdx, [rel list_method_index]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel list_method_count]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_copy]
    lea rdx, [rel list_method_copy]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel list_method_clear]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_extend]
    lea rdx, [rel list_method_extend]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_remove]
    lea rdx, [rel list_method_remove]
    call add_method_to_dict

    ; Store in list_type.tp_dict
    lea rax, [rel list_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- dict methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_get]
    lea rdx, [rel dict_method_get]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_keys]
    lea rdx, [rel dict_method_keys]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_values]
    lea rdx, [rel dict_method_values]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_items]
    lea rdx, [rel dict_method_items]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel dict_method_pop_v2]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel dict_method_clear]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_update]
    lea rdx, [rel dict_method_update]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_setdefault]
    lea rdx, [rel dict_method_setdefault]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_copy]
    lea rdx, [rel dict_method_copy]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_popitem]
    lea rdx, [rel dict_method_popitem]
    call add_method_to_dict

    ; Store in dict_type.tp_dict
    lea rax, [rel dict_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- tuple methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_index]
    lea rdx, [rel tuple_method_index]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_count]
    lea rdx, [rel tuple_method_count]
    call add_method_to_dict

    ; Store in tuple_type.tp_dict
    lea rax, [rel tuple_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- set methods ---
    call dict_new
    mov rbx, rax

    mov rdi, rbx
    lea rsi, [rel mn_add]
    lea rdx, [rel set_method_add]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_remove]
    lea rdx, [rel set_method_remove]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_discard]
    lea rdx, [rel set_method_discard]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_pop]
    lea rdx, [rel set_method_pop]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_clear]
    lea rdx, [rel set_method_clear]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_copy]
    lea rdx, [rel set_method_copy]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_union]
    lea rdx, [rel set_method_union]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_intersection]
    lea rdx, [rel set_method_intersection]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_difference]
    lea rdx, [rel set_method_difference]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_symmetric_difference]
    lea rdx, [rel set_method_symmetric_difference]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_issubset]
    lea rdx, [rel set_method_issubset]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_issuperset]
    lea rdx, [rel set_method_issuperset]
    call add_method_to_dict

    mov rdi, rbx
    lea rsi, [rel mn_isdisjoint]
    lea rdx, [rel set_method_isdisjoint]
    call add_method_to_dict

    ; Store in set_type.tp_dict
    lea rax, [rel set_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    ;; --- object_type methods (just __new__) ---
    call dict_new
    mov rbx, rax

    ; Create builtin_func for object_new_fn
    lea rdi, [rel object_new_fn]
    lea rsi, [rel mn___new__]
    call builtin_func_new
    push rax                    ; save builtin_func

    ; Wrap in PyStaticMethodObject
    mov edi, PyStaticMethodObject_size
    call ap_malloc
    mov qword [rax + PyStaticMethodObject.ob_refcnt], 1
    lea rcx, [rel staticmethod_type]
    mov [rax + PyStaticMethodObject.ob_type], rcx
    pop rcx                     ; builtin_func
    mov [rax + PyStaticMethodObject.sm_callable], rcx
    push rax                    ; save staticmethod wrapper

    ; Create key string
    lea rdi, [rel mn___new__]
    call str_from_cstr
    push rax                    ; save key

    ; dict_set(dict, key, staticmethod_wrapper, TAG_PTR, TAG_PTR)
    mov rdi, rbx
    mov rsi, rax                ; key
    mov rdx, [rsp + 8]         ; staticmethod wrapper
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set

    ; DECREF key
    pop rdi
    call obj_decref

    ; DECREF staticmethod wrapper (dict_set did INCREF)
    pop rdi
    call obj_decref

    ; Store in object_type.tp_dict
    lea rax, [rel object_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    pop r12
    pop rbx
    leave
    ret
END_FUNC methods_init

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata

empty_str_cstr: db 0

; Method name strings
mn_upper:       db "upper", 0
mn_lower:       db "lower", 0
mn_strip:       db "strip", 0
mn_startswith:  db "startswith", 0
mn_endswith:    db "endswith", 0
mn_find:        db "find", 0
mn_replace:     db "replace", 0
mn_join:        db "join", 0
mn_split:       db "split", 0
mn_format:      db "format", 0
mn_append:      db "append", 0
mn_pop:         db "pop", 0
mn_insert:      db "insert", 0
mn_reverse:     db "reverse", 0
mn_sort:        db "sort", 0
mn_index:       db "index", 0
mn_count:       db "count", 0
mn_copy:        db "copy", 0
mn_clear:       db "clear", 0
mn_extend:      db "extend", 0
mn_get:         db "get", 0
mn_keys:        db "keys", 0
mn_values:      db "values", 0
mn_items:       db "items", 0
mn_update:      db "update", 0
mn_lstrip:      db "lstrip", 0
mn_rstrip:      db "rstrip", 0
mn_rfind:       db "rfind", 0
mn_isdigit:     db "isdigit", 0
mn_isalpha:     db "isalpha", 0
mn_removeprefix: db "removeprefix", 0
mn_removesuffix: db "removesuffix", 0
mn_encode:      db "encode", 0
mn_setdefault:  db "setdefault", 0
mn_popitem:     db "popitem", 0
mn_remove:      db "remove", 0
mn_add:         db "add", 0
mn_discard:     db "discard", 0
mn_union:       db "union", 0
mn_intersection: db "intersection", 0
mn_difference:  db "difference", 0
mn_symmetric_difference: db "symmetric_difference", 0
mn_issubset:    db "issubset", 0
mn_issuperset:  db "issuperset", 0
mn_isdisjoint:  db "isdisjoint", 0
mn_isalnum:     db "isalnum", 0
mn_isspace:     db "isspace", 0
mn_isupper:     db "isupper", 0
mn_islower:     db "islower", 0
mn___new__:     db "__new__", 0
