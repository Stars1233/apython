; methods.asm - Type methods for str, list, and dict
; Implements method functions and methods_init to populate tp_dict

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

; External functions
extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_memcpy
extern ap_memset
extern ap_strcmp
extern ap_strlen
extern ap_strstr
extern obj_incref
extern obj_decref
extern obj_dealloc
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

;; ============================================================================
;; HELPER: add_method_to_dict(dict, name_cstr, func_ptr)
;; rdi=dict, rsi=name_cstr, rdx=func_ptr
;; Creates a builtin func wrapper and stores it in the dict.
;; ============================================================================
add_method_to_dict:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

;; ############################################################################
;;                         STRING METHODS
;; ############################################################################

;; ============================================================================
;; str_method_upper(args, nargs) -> new uppercase string
;; args[0] = self (PyStrObject*)
;; ============================================================================
global str_method_upper
str_method_upper:
    push rbp
    mov rbp, rsp
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
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; str_method_lower(args, nargs) -> new lowercase string
;; ============================================================================
global str_method_lower
str_method_lower:
    push rbp
    mov rbp, rsp
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
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; str_method_strip(args, nargs) -> new stripped string
;; Strip whitespace (space, tab, newline, cr) from both ends
;; ============================================================================
global str_method_strip
str_method_strip:
    push rbp
    mov rbp, rsp
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
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; str_method_startswith(args, nargs) -> bool_true/bool_false
;; args[0]=self, args[1]=prefix
;; ============================================================================
global str_method_startswith
str_method_startswith:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; prefix (args[1])

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
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.sw_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; str_method_endswith(args, nargs) -> bool_true/bool_false
;; args[0]=self, args[1]=suffix
;; ============================================================================
global str_method_endswith
str_method_endswith:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; suffix
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
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.ew_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; str_method_find(args, nargs) -> SmallInt index or -1
;; args[0]=self, args[1]=substr
;; ============================================================================
global str_method_find
str_method_find:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; substr

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
    pop rbp
    ret

.find_not_found:
    mov rdi, -1
    call int_from_i64

    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; str_method_replace(args, nargs) -> new string with replacements
;; args[0]=self, args[1]=old, args[2]=new
;; Uses callee-saved regs for key state, stack locals for buffer management.
;; ============================================================================
global str_method_replace
str_method_replace:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40             ; [rbp-48]=buf_ptr, [rbp-56]=buf_alloc, [rbp-64]=write_pos, [rbp-72]=self_len, [rbp-80]=pad

    ; rbx = self, r12 = old_str, r13 = new_str, r14 = self_len, r15 = scan_pos
    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; old
    mov r13, [rdi + 16]     ; new
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
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.replace_copy_self:
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, r14
    call str_new
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; str_method_join(args, nargs) -> new string
;; args[0]=self (separator), args[1]=list
;; self.join(iterable)
;; Regs: rbx=self(sep), r12=list, r13=count, r14=sep_len
;; Stack: [rbp-48]=total_len, [rbp-56]=buf_ptr, [rbp-64]=write_pos
;; ============================================================================
global str_method_join
str_method_join:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 32             ; 3 locals + alignment pad = 32

    mov rbx, [rdi]          ; self (separator)
    mov r12, [rdi + 8]      ; list

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
    mov rax, [rax + rcx*8]
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
    mov rax, [rax + rcx*8]  ; item
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
    add rsp, 32
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.join_empty:
    lea rdi, [rel empty_str_cstr]
    call str_from_cstr
    add rsp, 32
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; str_method_split(args, nargs) -> list of strings
;; If nargs==1: split by whitespace
;; If nargs==2: split by args[1]
;; ============================================================================
global str_method_split
str_method_split:
    push rbp
    mov rbp, rsp
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
    mov r15, [rdi + 8]      ; separator string
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
    pop rbp
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
    call list_append
    pop rdi
    call obj_decref

    mov rax, r13
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.split_empty_sep:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "empty separator"
    call raise_exception


;; ############################################################################
;;                         LIST METHODS
;; ############################################################################

;; ============================================================================
;; list_method_append(args, nargs) -> None
;; args[0]=self, args[1]=item
;; ============================================================================
global list_method_append
list_method_append:
    push rbp
    mov rbp, rsp

    mov rax, [rdi]          ; self (list)
    mov rsi, [rdi + 8]      ; item
    mov rdi, rax
    call list_append

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    pop rbp
    ret

;; ============================================================================
;; list_method_pop(args, nargs) -> removed item
;; args[0]=self, optionally args[1]=index (default: last)
;; ============================================================================
global list_method_pop
list_method_pop:
    push rbp
    mov rbp, rsp
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
    mov rdi, [rax + 8]     ; args[1]
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
    mov r12, [rax + r13*8]  ; r12 = item to return
    ; Don't DECREF since we're transferring ownership to caller

    ; Shift items down: for i = index .. size-2, items[i] = items[i+1]
    mov rcx, r13            ; i = index
    mov rdx, [rbx + PyListObject.ob_size]
    dec rdx                 ; size - 1
.pop_shift:
    cmp rcx, rdx
    jge .pop_shrink
    mov rax, [rbx + PyListObject.ob_item]
    mov r8, [rax + rcx*8 + 8]  ; items[i+1]
    mov [rax + rcx*8], r8
    inc rcx
    jmp .pop_shift

.pop_shrink:
    dec qword [rbx + PyListObject.ob_size]

    ; Return item (ownership transferred, no extra INCREF needed)
    mov rax, r12
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.pop_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "pop index out of range"
    call raise_exception

;; ============================================================================
;; list_method_insert(args, nargs) -> None
;; args[0]=self, args[1]=index, args[2]=item
;; ============================================================================
global list_method_insert
list_method_insert:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov rax, rdi            ; args
    mov rbx, [rax]          ; self
    push rax

    ; Get index
    mov rdi, [rax + 8]      ; args[1]
    call int_to_i64
    mov r12, rax            ; index

    pop rax
    mov r13, [rax + 16]     ; item = args[2]

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
    shl rsi, 3
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
    mov r8, [rax + rcx*8]
    mov [rax + rcx*8 + 8], r8
    dec rcx
    jmp .ins_shift

.ins_place:
    ; Place item at index
    mov rax, [rbx + PyListObject.ob_item]
    mov [rax + r12*8], r13
    INCREF r13
    inc qword [rbx + PyListObject.ob_size]

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_method_reverse(args, nargs) -> None
;; args[0]=self, reverse in place
;; ============================================================================
global list_method_reverse
list_method_reverse:
    push rbp
    mov rbp, rsp

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
    mov r8, [rdi + rsi*8]
    mov r9, [rdi + rcx*8]
    mov [rdi + rsi*8], r9
    mov [rdi + rcx*8], r8
    inc rsi
    dec rcx
    jmp .rev_loop

.rev_done:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    pop rbp
    ret

;; ============================================================================
;; list_method_sort(args, nargs) -> None
;; Simple bubble sort (ints only via int_to_i64 comparison)
;; args[0]=self
;; ============================================================================
global list_method_sort
list_method_sort:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, [rdi]          ; self
    mov r12, [rbx + PyListObject.ob_size]

    ; Bubble sort
    cmp r12, 2
    jl .sort_done

.sort_outer:
    xor r13d, r13d          ; swapped = false
    mov r14, 1              ; i = 1
.sort_inner:
    cmp r14, r12
    jge .sort_check
    ; Compare items[i-1] and items[i]
    mov rax, [rbx + PyListObject.ob_item]
    mov rdi, [rax + r14*8 - 8]   ; items[i-1]
    push r14
    push r13
    call int_to_i64
    mov r15, rax                  ; val_a

    mov rax, [rbx + PyListObject.ob_item]
    mov r14, [rsp + 8]           ; reload i
    mov rdi, [rax + r14*8]       ; items[i]
    call int_to_i64
    ; rax = val_b, r15 = val_a
    pop r13
    pop r14

    cmp r15, rax
    jle .sort_no_swap

    ; Swap items[i-1] and items[i]
    mov rax, [rbx + PyListObject.ob_item]
    mov r8, [rax + r14*8 - 8]
    mov r9, [rax + r14*8]
    mov [rax + r14*8 - 8], r9
    mov [rax + r14*8], r8
    mov r13d, 1             ; swapped = true

.sort_no_swap:
    inc r14
    jmp .sort_inner

.sort_check:
    test r13d, r13d
    jnz .sort_outer

.sort_done:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_method_index(args, nargs) -> SmallInt index
;; args[0]=self, args[1]=value
;; Linear scan with pointer equality
;; ============================================================================
global list_method_index
list_method_index:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; value to find
    mov r13, [rbx + PyListObject.ob_size]

    xor ecx, ecx
.index_loop:
    cmp rcx, r13
    jge .index_not_found
    mov rax, [rbx + PyListObject.ob_item]
    cmp r12, [rax + rcx*8]
    je .index_found

    ; Also check SmallInt equality: if both are SmallInts, compare values
    mov rdi, r12
    test rdi, rdi
    jns .index_check_str    ; not SmallInt, try string
    mov rax, [rbx + PyListObject.ob_item]
    mov rsi, [rax + rcx*8]
    test rsi, rsi
    jns .index_next         ; list item not SmallInt
    ; Both SmallInts - already compared by pointer above (SmallInts with same
    ; value have the same tagged representation), so no match
    jmp .index_next

.index_check_str:
    ; Try string comparison: if both are str_type, compare data
    mov rax, [rbx + PyListObject.ob_item]
    mov rsi, [rax + rcx*8]
    test rsi, rsi
    js .index_next          ; list item is SmallInt
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
    pop rbp
    ret

.index_not_found:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "x not in list"
    call raise_exception

;; ============================================================================
;; list_method_count(args, nargs) -> SmallInt
;; args[0]=self, args[1]=value
;; ============================================================================
global list_method_count
list_method_count:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; value
    mov r13, [rbx + PyListObject.ob_size]
    xor r14d, r14d          ; count = 0

    xor ecx, ecx
.count_loop:
    cmp rcx, r13
    jge .count_done
    mov rax, [rbx + PyListObject.ob_item]
    cmp r12, [rax + rcx*8]
    jne .count_next
    inc r14
.count_next:
    inc rcx
    jmp .count_loop

.count_done:
    mov rdi, r14
    call int_from_i64
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_method_copy(args, nargs) -> new list (shallow copy)
;; args[0]=self
;; ============================================================================
global list_method_copy
list_method_copy:
    push rbp
    mov rbp, rsp
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
    mov rsi, [rax + rcx*8]
    mov rdi, r13
    call list_append
    pop rcx
    inc rcx
    jmp .copy_loop

.copy_done:
    mov rax, r13
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
global list_method_clear
list_method_clear:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, [rdi]          ; self
    mov r12, [rbx + PyListObject.ob_size]

    ; DECREF all items
    xor r13d, r13d
.clear_loop:
    cmp r13, r12
    jge .clear_done
    mov rax, [rbx + PyListObject.ob_item]
    mov rdi, [rax + r13*8]
    push r13
    call obj_decref
    pop r13
    inc r13
    jmp .clear_loop

.clear_done:
    mov qword [rbx + PyListObject.ob_size], 0

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_method_extend(args, nargs) -> None
;; args[0]=self, args[1]=iterable (must be a list for now)
;; ============================================================================
global list_method_extend
list_method_extend:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; iterable (list)
    mov r13, [r12 + PyListObject.ob_size]

    xor r14d, r14d
.extend_loop:
    cmp r14, r13
    jge .extend_done
    push r14
    mov rax, [r12 + PyListObject.ob_item]
    mov rsi, [rax + r14*8]
    mov rdi, rbx
    call list_append
    pop r14
    inc r14
    jmp .extend_loop

.extend_done:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret


;; ############################################################################
;;                         DICT METHODS
;; ############################################################################

;; ============================================================================
;; dict_method_get(args, nargs) -> value or None
;; args[0]=self, args[1]=key, optionally args[2]=default
;; ============================================================================
global dict_method_get
dict_method_get:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    mov rax, rdi            ; args
    mov rbx, [rax]          ; self (dict)
    mov r12, rsi            ; nargs
    push rax

    ; dict_get(self, key)
    mov rdi, rbx
    mov rsi, [rax + 8]      ; key
    call dict_get

    test rax, rax
    jnz .dg_found

    ; Not found - return default or None
    pop rcx                 ; args
    cmp r12, 3
    jl .dg_ret_none
    ; Return args[2] (default)
    mov rax, [rcx + 16]
    INCREF rax
    pop r12
    pop rbx
    pop rbp
    ret

.dg_ret_none:
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    pop rbp
    ret

.dg_found:
    add rsp, 8              ; discard saved args
    ; INCREF the value (dict_get returns borrowed ref)
    INCREF rax
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; dict_method_keys(args, nargs) -> list of keys
;; args[0]=self
;; ============================================================================
global dict_method_keys
dict_method_keys:
    push rbp
    mov rbp, rsp
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

    ; Check if slot is occupied (key != NULL and value != NULL)
    mov rdi, [rax + DictEntry.key]
    test rdi, rdi
    jz .dk_next
    mov rcx, [rax + DictEntry.value]
    test rcx, rcx
    jz .dk_next

    ; Append key to list
    push r14
    mov rsi, rdi            ; key
    mov rdi, r12            ; list
    call list_append
    pop r14

.dk_next:
    inc r14
    jmp .dk_loop

.dk_done:
    mov rax, r12
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; dict_method_values(args, nargs) -> list of values
;; args[0]=self
;; ============================================================================
global dict_method_values
dict_method_values:
    push rbp
    mov rbp, rsp
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
    mov rcx, [rax + DictEntry.value]
    test rcx, rcx
    jz .dv_next

    ; Append value to list
    push r14
    mov rsi, rcx            ; value
    mov rdi, r12
    call list_append
    pop r14

.dv_next:
    inc r14
    jmp .dv_loop

.dv_done:
    mov rax, r12
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; dict_method_items(args, nargs) -> list of (key, value) tuples
;; args[0]=self
;; ============================================================================
global dict_method_items
dict_method_items:
    push rbp
    mov rbp, rsp
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
    mov rcx, [rax + DictEntry.value]
    test rcx, rcx
    jz .di_next

    ; Create (key, value) tuple
    push r14
    push rdi                ; save key
    push rcx                ; save value

    mov rdi, 2
    call tuple_new
    mov r14, rax            ; tuple

    pop rcx                 ; value
    pop rdi                 ; key

    ; Store key in tuple[0]
    mov [r14 + PyTupleObject.ob_item], rdi
    INCREF rdi

    ; Store value in tuple[1]
    mov [r14 + PyTupleObject.ob_item + 8], rcx
    INCREF rcx

    ; Append tuple to list
    mov rdi, r12
    mov rsi, r14
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
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; dict_method_pop(args, nargs) -> value
;; args[0]=self, args[1]=key, optionally args[2]=default
;; ============================================================================
global dict_method_pop
dict_method_pop:
dict_method_pop_v2:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov r14, rdi            ; r14 = args
    mov rbx, [r14]          ; self
    mov r12, rsi            ; nargs
    mov r13, [r14 + 8]      ; key

    ; Try dict_get
    mov rdi, rbx
    mov rsi, r13
    call dict_get
    test rax, rax
    jz .dpop2_not_found

    INCREF rax
    push rax

    mov rdi, rbx
    mov rsi, r13
    call dict_del

    pop rax
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.dpop2_not_found:
    cmp r12, 3
    jl .dpop2_error
    mov rax, [r14 + 16]     ; default = args[2]
    INCREF rax
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.dpop2_error:
    lea rdi, [rel exc_KeyError_type]
    CSTRING rsi, "key not found"
    call raise_exception

;; ============================================================================
;; dict_method_clear(args, nargs) -> None
;; args[0]=self
;; ============================================================================
global dict_method_clear
dict_method_clear:
    push rbp
    mov rbp, rsp
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

    ; DECREF key
    call obj_decref

    ; DECREF value
    mov rdi, [r14 + DictEntry.value]
    test rdi, rdi
    jz .dc_next
    call obj_decref

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
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; dict_method_update(args, nargs) -> None
;; args[0]=self, args[1]=other_dict
;; Merge other_dict into self
;; ============================================================================
global dict_method_update
dict_method_update:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov rbx, [rdi]          ; self
    mov r12, [rdi + 8]      ; other dict

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
    mov rsi, [rax + DictEntry.value]
    test rsi, rsi
    jz .du_next

    ; dict_set(self, key, value)
    push r14
    mov rdx, rsi            ; value
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
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret


;; ############################################################################
;;                       METHODS_INIT
;; ############################################################################

;; ============================================================================
;; methods_init()
;; Populate tp_dict for str_type, list_type, dict_type
;; ============================================================================
global methods_init
methods_init:
    push rbp
    mov rbp, rsp
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

    ; Store in dict_type.tp_dict
    lea rax, [rel dict_type]
    mov [rax + PyTypeObject.tp_dict], rbx

    pop r12
    pop rbx
    pop rbp
    ret

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
