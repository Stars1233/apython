; marshal.asm - Read Python marshal format from buffer
; Implements the marshal deserialization protocol for .pyc files

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "marshal.inc"

section .text

extern none_singleton
extern bool_true
extern bool_false
extern int_from_i64
extern str_new
extern str_from_cstr
extern tuple_new
extern bytes_from_data
extern ap_malloc
extern ap_free
extern ap_realloc
extern fatal_error
extern memcpy
extern code_type

; Initial capacity for the reference list
MARSHAL_REFS_INIT_CAP equ 64

;--------------------------------------------------------------------------
; marshal_read_byte() -> byte in al
; Read one byte from marshal_buf[marshal_pos], increment marshal_pos.
;--------------------------------------------------------------------------
global marshal_read_byte
marshal_read_byte:
    push rbp
    mov rbp, rsp

    mov rax, [rel marshal_pos]
    cmp rax, [rel marshal_len]
    jge mread_byte_eof

    mov rcx, [rel marshal_buf]
    movzx eax, byte [rcx + rax]
    inc qword [rel marshal_pos]

    pop rbp
    ret

mread_byte_eof:
    lea rdi, [rel marshal_err_eof]
    call fatal_error

;--------------------------------------------------------------------------
; marshal_read_long() -> int32 in eax
; Read 4 bytes little-endian from buffer.
;--------------------------------------------------------------------------
global marshal_read_long
marshal_read_long:
    push rbp
    mov rbp, rsp

    mov rax, [rel marshal_pos]
    lea rcx, [rax + 4]
    cmp rcx, [rel marshal_len]
    jg mread_long_eof

    mov rcx, [rel marshal_buf]
    mov eax, [rcx + rax]       ; little-endian read (x86 native)
    add qword [rel marshal_pos], 4

    pop rbp
    ret

mread_long_eof:
    lea rdi, [rel marshal_err_eof]
    call fatal_error

;--------------------------------------------------------------------------
; marshal_read_long64() -> int64 in rax
; Read 8 bytes little-endian from buffer.
;--------------------------------------------------------------------------
global marshal_read_long64
marshal_read_long64:
    push rbp
    mov rbp, rsp

    mov rax, [rel marshal_pos]
    lea rcx, [rax + 8]
    cmp rcx, [rel marshal_len]
    jg mread_long64_eof

    mov rcx, [rel marshal_buf]
    mov rax, [rcx + rax]       ; little-endian read (x86 native)
    add qword [rel marshal_pos], 8

    pop rbp
    ret

mread_long64_eof:
    lea rdi, [rel marshal_err_eof]
    call fatal_error

;--------------------------------------------------------------------------
; marshal_read_bytes(int64_t n) -> pointer to bytes in buffer (rax)
; Returns pointer to current position in buffer, advances pos by n.
;--------------------------------------------------------------------------
global marshal_read_bytes
marshal_read_bytes:
    push rbp
    mov rbp, rsp

    mov rsi, rdi               ; rsi = n
    mov rax, [rel marshal_pos]
    lea rcx, [rax + rsi]
    cmp rcx, [rel marshal_len]
    jg mread_bytes_eof

    mov rcx, [rel marshal_buf]
    lea rax, [rcx + rax]       ; rax = &buf[pos]
    add [rel marshal_pos], rsi

    pop rbp
    ret

mread_bytes_eof:
    lea rdi, [rel marshal_err_eof]
    call fatal_error

;--------------------------------------------------------------------------
; marshal_init_refs() - Initialize the reference list
;--------------------------------------------------------------------------
global marshal_init_refs
marshal_init_refs:
    push rbp
    mov rbp, rsp

    mov qword [rel marshal_ref_count], 0

    ; Check if already allocated with sufficient capacity
    cmp qword [rel marshal_ref_cap], 0
    jne .already_allocated

    ; Allocate initial ref array
    mov rdi, MARSHAL_REFS_INIT_CAP * 8
    call ap_malloc
    mov [rel marshal_refs], rax
    mov qword [rel marshal_ref_cap], MARSHAL_REFS_INIT_CAP

.already_allocated:
    pop rbp
    ret

;--------------------------------------------------------------------------
; marshal_add_ref(PyObject *obj) - Add object to reference list
;--------------------------------------------------------------------------
global marshal_add_ref
marshal_add_ref:
    push rbp
    mov rbp, rsp
    push rbx

    mov rbx, rdi               ; rbx = obj

    ; Check if we need to grow
    mov rax, [rel marshal_ref_count]
    cmp rax, [rel marshal_ref_cap]
    jl .store

    ; Grow: double the capacity
    mov rdi, [rel marshal_refs]
    mov rax, [rel marshal_ref_cap]
    shl rax, 1                 ; new_cap = old_cap * 2
    mov [rel marshal_ref_cap], rax
    lea rsi, [rax * 8]         ; new_cap * sizeof(ptr)
    call ap_realloc
    mov [rel marshal_refs], rax

.store:
    mov rax, [rel marshal_ref_count]
    mov rcx, [rel marshal_refs]
    mov [rcx + rax * 8], rbx   ; refs[count] = obj
    inc qword [rel marshal_ref_count]

    pop rbx
    pop rbp
    ret

;--------------------------------------------------------------------------
; marshal_read_object() -> PyObject*
; Main marshal deserialization dispatcher.
;
; Register convention within this function and its handlers:
;   rbx = type code (without FLAG_REF)
;   r12 = FLAG_REF indicator (0 or 1)
; Both are callee-saved and pushed in the prologue.
;--------------------------------------------------------------------------
global marshal_read_object
marshal_read_object:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    ; Read type byte
    call marshal_read_byte
    movzx eax, al
    mov ebx, eax               ; ebx = full type byte

    ; Check FLAG_REF
    xor r12d, r12d             ; r12 = 0 means no FLAG_REF
    test ebx, MARSHAL_FLAG_REF
    jz .no_flag_ref
    mov r12d, 1                ; r12 = 1 means FLAG_REF set
    and ebx, ~MARSHAL_FLAG_REF ; strip the flag bit
.no_flag_ref:

    ; Dispatch on type code (ebx = type without FLAG_REF)
    cmp ebx, MARSHAL_TYPE_NONE
    je mdo_none
    cmp ebx, MARSHAL_TYPE_TRUE
    je mdo_true
    cmp ebx, MARSHAL_TYPE_FALSE
    je mdo_false
    cmp ebx, MARSHAL_TYPE_INT
    je mdo_int
    cmp ebx, MARSHAL_TYPE_INT64
    je mdo_int64
    cmp ebx, MARSHAL_TYPE_LONG
    je mdo_long
    cmp ebx, MARSHAL_TYPE_BINARY_FLOAT
    je mdo_binary_float
    cmp ebx, MARSHAL_TYPE_SHORT_ASCII
    je mdo_short_ascii
    cmp ebx, MARSHAL_TYPE_SHORT_ASCII_INTERNED
    je mdo_short_ascii
    cmp ebx, MARSHAL_TYPE_ASCII
    je mdo_ascii
    cmp ebx, MARSHAL_TYPE_ASCII_INTERNED
    je mdo_ascii
    cmp ebx, MARSHAL_TYPE_UNICODE
    je mdo_unicode
    cmp ebx, MARSHAL_TYPE_STRING
    je mdo_bytes
    cmp ebx, MARSHAL_TYPE_INTERNED
    je mdo_bytes
    cmp ebx, MARSHAL_TYPE_SMALL_TUPLE
    je mdo_small_tuple
    cmp ebx, MARSHAL_TYPE_TUPLE
    je mdo_tuple
    cmp ebx, MARSHAL_TYPE_REF
    je mdo_ref
    cmp ebx, MARSHAL_TYPE_CODE
    je mdo_code
    cmp ebx, MARSHAL_TYPE_STOPITER
    je mdo_none                 ; stub: return None
    cmp ebx, MARSHAL_TYPE_ELLIPSIS
    je mdo_none                 ; stub: return None
    cmp ebx, MARSHAL_TYPE_NULL
    je mdo_null

    ; Unknown type
    lea rdi, [rel marshal_err_unknown]
    call fatal_error

;--------------------------------------------------------------------------
; mfinish: common epilogue for marshal_read_object
; rax = the result object, r12 = FLAG_REF flag
;--------------------------------------------------------------------------
mfinish:
    ; If FLAG_REF was set, add to reference list
    test r12d, r12d
    jz .no_add_ref
    push rax
    mov rdi, rax
    call marshal_add_ref
    pop rax
.no_add_ref:
    pop r12
    pop rbx
    pop rbp
    ret

;--------------------------------------------------------------------------
; TYPE_NONE handler
;--------------------------------------------------------------------------
mdo_none:
    lea rax, [rel none_singleton]
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_TRUE handler
;--------------------------------------------------------------------------
mdo_true:
    lea rax, [rel bool_true]
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_FALSE handler
;--------------------------------------------------------------------------
mdo_false:
    lea rax, [rel bool_false]
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_NULL handler - return NULL pointer
;--------------------------------------------------------------------------
mdo_null:
    xor eax, eax
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_INT handler: read 4-byte signed int, create int object
;--------------------------------------------------------------------------
mdo_int:
    call marshal_read_long
    movsx rdi, eax             ; sign-extend to 64-bit
    call int_from_i64
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_INT64 handler: read 8-byte signed int, create int object
;--------------------------------------------------------------------------
mdo_int64:
    call marshal_read_long64
    mov rdi, rax
    call int_from_i64
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_LONG handler: read marshal multi-precision integer
; Format: ndigits (signed int32), then |ndigits| 16-bit "digits"
; Each digit is a base-2^15 digit. Sign from ndigits sign.
; For simplicity, we only handle values that fit in int64.
;--------------------------------------------------------------------------
mdo_long:
    push r13
    push r14
    push r15
    sub rsp, 16                ; [rsp+0] = digit index, [rsp+8] = shift amount

    call marshal_read_long     ; eax = ndigits (signed)
    movsx r13, eax             ; r13 = ndigits (sign-extended)

    ; Compute absolute digit count
    mov r14, r13
    test r14, r14
    jns .long_pos
    neg r14                    ; r14 = |ndigits|
.long_pos:

    ; Reconstruct value from base-2^15 digits (little-endian)
    xor r15d, r15d             ; r15 = accumulated value
    mov qword [rsp + 0], 0    ; digit index = 0
    mov qword [rsp + 8], 0    ; shift amount = 0

.long_digit_loop:
    mov rax, [rsp + 0]
    cmp rax, r14
    jge .long_digits_done

    ; Read one 16-bit digit (2 bytes, little-endian)
    call marshal_read_byte     ; low byte
    movzx r8d, al
    call marshal_read_byte     ; high byte
    movzx eax, al
    shl eax, 8
    or r8d, eax               ; r8d = 16-bit digit value

    ; Accumulate: value |= (uint64_t)digit << shift
    mov rax, r8
    mov rcx, [rsp + 8]        ; shift amount in rcx (cl used by shl)
    shl rax, cl
    or r15, rax

    ; Advance
    add qword [rsp + 8], 15   ; shift += 15
    inc qword [rsp + 0]       ; index++
    jmp .long_digit_loop

.long_digits_done:
    ; Apply sign
    test r13, r13
    jns .long_not_neg
    neg r15
.long_not_neg:

    ; Create int object
    mov rdi, r15
    call int_from_i64

    add rsp, 16
    pop r15
    pop r14
    pop r13
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_BINARY_FLOAT handler: read 8 bytes as double
; Stub: skip 8 bytes and return None
;--------------------------------------------------------------------------
mdo_binary_float:
    mov rdi, 8
    call marshal_read_bytes    ; skip 8 bytes
    lea rax, [rel none_singleton]  ; stub: return None
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_SHORT_ASCII / TYPE_SHORT_ASCII_INTERNED handler
; Read 1-byte length, then bytes -> str_new
; r12 holds FLAG_REF; we save it on the stack while using r12 as temp.
;--------------------------------------------------------------------------
mdo_short_ascii:
    push r12                   ; save FLAG_REF on stack
    push r13

    call marshal_read_byte     ; al = length
    movzx r13d, al             ; r13 = length

    mov rdi, r13
    call marshal_read_bytes    ; rax = pointer to string data in buffer
    mov rdi, rax               ; data ptr
    mov rsi, r13               ; length
    call str_new

    pop r13
    pop r12                    ; restore FLAG_REF
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_ASCII / TYPE_ASCII_INTERNED handler
; Read 4-byte length, then bytes -> str_new
;--------------------------------------------------------------------------
mdo_ascii:
    push r12                   ; save FLAG_REF
    push r13

    call marshal_read_long     ; eax = length
    mov r13d, eax              ; r13 = length (unsigned)

    mov rdi, r13
    call marshal_read_bytes    ; rax = pointer to string data
    mov rdi, rax               ; data ptr
    mov rsi, r13               ; length
    call str_new

    pop r13
    pop r12                    ; restore FLAG_REF
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_UNICODE handler
; Read 4-byte length, then bytes -> str_new (treat as UTF-8)
;--------------------------------------------------------------------------
mdo_unicode:
    push r12                   ; save FLAG_REF
    push r13

    call marshal_read_long     ; eax = length
    mov r13d, eax              ; r13 = length (unsigned)

    mov rdi, r13
    call marshal_read_bytes    ; rax = pointer to data
    mov rdi, rax               ; data ptr
    mov rsi, r13               ; length
    call str_new

    pop r13
    pop r12                    ; restore FLAG_REF
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_STRING / TYPE_INTERNED handler
; Read 4-byte length, then bytes -> bytes_from_data
;--------------------------------------------------------------------------
mdo_bytes:
    push r12                   ; save FLAG_REF
    push r13

    call marshal_read_long     ; eax = length
    mov r13d, eax              ; r13 = length (unsigned)

    mov rdi, r13
    call marshal_read_bytes    ; rax = pointer to data
    mov rdi, rax               ; data ptr
    mov rsi, r13               ; length
    call bytes_from_data

    pop r13
    pop r12                    ; restore FLAG_REF
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_SMALL_TUPLE handler: 1-byte count, then recursive reads
;--------------------------------------------------------------------------
mdo_small_tuple:
    push r12                   ; save FLAG_REF
    push r13
    push r14
    push r15
    sub rsp, 16                ; [rsp+0]=saved FLAG_REF, [rsp+8]=ref index

    ; Reserve ref slot BEFORE reading children (CPython does the same).
    ; Children will get ref indices after this slot, matching CPython order.
    mov [rsp + 0], r12         ; save FLAG_REF
    test r12d, r12d
    jz .stuple_no_reserve
    xor edi, edi               ; NULL placeholder
    call marshal_add_ref
    mov rax, [rel marshal_ref_count]
    dec rax
    mov [rsp + 8], rax         ; save ref index for fixup
.stuple_no_reserve:

    call marshal_read_byte     ; al = count
    movzx r13d, al             ; r13 = count

    ; Allocate tuple
    mov rdi, r13
    call tuple_new
    mov r14, rax               ; r14 = tuple

    ; Read elements
    xor r15d, r15d             ; r15 = index
.stuple_loop:
    cmp r15, r13
    jge .stuple_done
    push r13
    push r14
    push r15
    call marshal_read_object
    pop r15
    pop r14
    pop r13
    ; Store element in tuple
    mov [r14 + PyTupleObject.ob_item + r15 * 8], rax
    inc r15
    jmp .stuple_loop

.stuple_done:
    ; Fix up reserved ref slot with the actual tuple
    mov rax, [rsp + 0]        ; saved FLAG_REF
    test eax, eax
    jz .stuple_no_fixup
    mov rax, [rsp + 8]        ; ref index
    mov rcx, [rel marshal_refs]
    mov [rcx + rax * 8], r14  ; fix up placeholder with tuple
.stuple_no_fixup:
    mov rax, r14               ; return the tuple
    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12                    ; restore original r12
    xor r12d, r12d             ; clear FLAG_REF — we handled it ourselves
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_TUPLE handler: 4-byte count, then recursive reads
;--------------------------------------------------------------------------
mdo_tuple:
    push r12                   ; save FLAG_REF
    push r13
    push r14
    push r15
    sub rsp, 16                ; [rsp+0]=saved FLAG_REF, [rsp+8]=ref index

    ; Reserve ref slot BEFORE reading children (same as CPython)
    mov [rsp + 0], r12
    test r12d, r12d
    jz .tuple_no_reserve
    xor edi, edi               ; NULL placeholder
    call marshal_add_ref
    mov rax, [rel marshal_ref_count]
    dec rax
    mov [rsp + 8], rax         ; save ref index for fixup
.tuple_no_reserve:

    call marshal_read_long     ; eax = count
    mov r13d, eax              ; r13 = count (unsigned)

    ; Allocate tuple
    mov rdi, r13
    call tuple_new
    mov r14, rax               ; r14 = tuple

    ; Read elements
    xor r15d, r15d             ; r15 = index
.tuple_loop:
    cmp r15, r13
    jge .tuple_done
    push r13
    push r14
    push r15
    call marshal_read_object
    pop r15
    pop r14
    pop r13
    mov [r14 + PyTupleObject.ob_item + r15 * 8], rax
    inc r15
    jmp .tuple_loop

.tuple_done:
    ; Fix up reserved ref slot with the actual tuple
    mov rax, [rsp + 0]        ; saved FLAG_REF
    test eax, eax
    jz .tuple_no_fixup
    mov rax, [rsp + 8]        ; ref index
    mov rcx, [rel marshal_refs]
    mov [rcx + rax * 8], r14  ; fix up placeholder with tuple
.tuple_no_fixup:
    mov rax, r14
    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12                    ; restore original r12
    xor r12d, r12d             ; clear FLAG_REF — we handled it ourselves
    jmp mfinish

;--------------------------------------------------------------------------
; TYPE_REF handler: read 4-byte index, return refs[index]
;--------------------------------------------------------------------------
mdo_ref:
    call marshal_read_long     ; eax = index
    mov edi, eax               ; zero-extend to rdi (index is unsigned)
    ; Bounds check
    cmp rdi, [rel marshal_ref_count]
    jge mdo_ref_oob
    mov rcx, [rel marshal_refs]
    mov rax, [rcx + rdi * 8]
    jmp mfinish

mdo_ref_oob:
    lea rdi, [rel marshal_err_ref_oob]
    call fatal_error

;--------------------------------------------------------------------------
; TYPE_CODE handler: read all code object fields
;
; This handler manages its own FLAG_REF handling: if FLAG_REF was set,
; it adds a NULL placeholder to the ref list before reading sub-objects
; (to handle self-referential structures), then fixes it up at the end.
; It returns directly rather than going through mfinish.
;
; Python 3.12 marshal order for code objects:
;   5 x long: argcount, posonlyargcount, kwonlyargcount, stacksize, flags
;   10 x object: co_code, co_consts, co_names, co_localsplusnames,
;                co_localspluskinds, co_filename, co_name, co_qualname,
;                co_linetable, co_exceptiontable
;   1 x long: co_firstlineno (between co_qualname and co_linetable)
;
; Stack frame layout (relative to rsp after sub rsp, 128):
;   [rsp +  0] co_argcount (4 bytes)
;   [rsp +  4] co_kwonlyargcount (4 bytes)
;   [rsp +  8] co_stacksize (4 bytes)
;   [rsp + 12] co_flags (4 bytes)
;   [rsp + 16] co_code_obj ptr (8 bytes)
;   [rsp + 24] co_consts ptr (8 bytes)
;   [rsp + 32] co_names ptr (8 bytes)
;   [rsp + 40] co_localsplusnames ptr (8 bytes)
;   [rsp + 48] co_localspluskinds ptr (8 bytes)
;   [rsp + 56] co_filename ptr (8 bytes)
;   [rsp + 64] co_name ptr (8 bytes)
;   [rsp + 72] co_qualname ptr (8 bytes)
;   [rsp + 80] co_lnotab ptr (8 bytes)
;   [rsp + 88] co_exceptiontable ptr (8 bytes)
;   [rsp + 96] saved FLAG_REF (8 bytes)
;   [rsp +104] ref index placeholder (8 bytes, used only if FLAG_REF)
; Total: 112 bytes needed, using 128 for alignment.
;--------------------------------------------------------------------------
mdo_code:
    push r13                   ; r13 = code object pointer (after alloc)
    push r14                   ; r14 = bytecode length
    push r15                   ; r15 = scratch
    sub rsp, 128               ; local storage

    ; Save FLAG_REF (r12) in our local frame
    mov [rsp + 96], r12

    ; If FLAG_REF was set, add a NULL placeholder to the ref list
    test r12d, r12d
    jz .code_no_placeholder
    xor edi, edi               ; NULL placeholder
    call marshal_add_ref
    mov rax, [rel marshal_ref_count]
    dec rax
    mov [rsp + 104], rax       ; save ref index
.code_no_placeholder:

    ; Read fields in marshal order
    call marshal_read_long     ; co_argcount
    mov [rsp + 0], eax

    call marshal_read_long     ; co_posonlyargcount (discard)

    call marshal_read_long     ; co_kwonlyargcount
    mov [rsp + 4], eax

    call marshal_read_long     ; co_stacksize
    mov [rsp + 8], eax

    call marshal_read_long     ; co_flags
    mov [rsp + 12], eax

    call marshal_read_object   ; co_code (bytes object)
    mov [rsp + 16], rax

    call marshal_read_object   ; co_consts (tuple)
    mov [rsp + 24], rax

    call marshal_read_object   ; co_names (tuple)
    mov [rsp + 32], rax

    call marshal_read_object   ; co_localsplusnames (tuple)
    mov [rsp + 40], rax

    call marshal_read_object   ; co_localspluskinds (bytes)
    mov [rsp + 48], rax

    call marshal_read_object   ; co_filename (str)
    mov [rsp + 56], rax

    call marshal_read_object   ; co_name (str)
    mov [rsp + 64], rax

    call marshal_read_object   ; co_qualname (str)
    mov [rsp + 72], rax

    call marshal_read_long     ; co_firstlineno (discard)

    call marshal_read_object   ; co_linetable (bytes)
    mov [rsp + 80], rax

    call marshal_read_object   ; co_exceptiontable (bytes)
    mov [rsp + 88], rax

    ; Compute bytecode length from the co_code bytes object
    mov rax, [rsp + 16]        ; co_code bytes object
    test rax, rax
    jz .code_zero_len
    mov r14, [rax + PyBytesObject.ob_size]  ; r14 = bytecode length
    jmp .code_have_len
.code_zero_len:
    xor r14d, r14d
.code_have_len:

    ; Allocate PyCodeObject: fixed header + bytecode
    lea rdi, [r14 + PyCodeObject.co_code]
    call ap_malloc
    mov r13, rax               ; r13 = code object

    ; Fill base header
    mov qword [r13 + PyObject.ob_refcnt], 1
    lea rax, [rel code_type]
    mov [r13 + PyObject.ob_type], rax

    ; Fill integer fields
    mov eax, [rsp + 0]
    mov [r13 + PyCodeObject.co_argcount], eax

    mov eax, [rsp + 4]
    mov [r13 + PyCodeObject.co_kwonlyargcount], eax

    ; co_nlocals = len(co_localsplusnames)
    mov rax, [rsp + 40]        ; co_localsplusnames tuple
    test rax, rax
    jz .code_nlocals_zero
    mov rax, [rax + PyTupleObject.ob_size]  ; ob_size is qword
    mov r15d, eax              ; truncate to 32-bit count
    jmp .code_nlocals_set
.code_nlocals_zero:
    xor r15d, r15d
.code_nlocals_set:
    mov [r13 + PyCodeObject.co_nlocals], r15d

    mov eax, [rsp + 8]
    mov [r13 + PyCodeObject.co_stacksize], eax

    mov eax, [rsp + 12]
    mov [r13 + PyCodeObject.co_flags], eax

    ; co_nlocalsplus = len(co_localsplusnames)
    ; r15d already has this from above
    mov [r13 + PyCodeObject.co_nlocalsplus], r15d

    ; Fill pointer fields
    mov rax, [rsp + 24]        ; co_consts
    mov [r13 + PyCodeObject.co_consts], rax

    mov rax, [rsp + 32]        ; co_names
    mov [r13 + PyCodeObject.co_names], rax

    mov rax, [rsp + 40]        ; co_localsplusnames
    mov [r13 + PyCodeObject.co_localsplusnames], rax

    mov rax, [rsp + 48]        ; co_localspluskinds
    mov [r13 + PyCodeObject.co_localspluskinds], rax

    mov rax, [rsp + 56]        ; co_filename
    mov [r13 + PyCodeObject.co_filename], rax

    mov rax, [rsp + 64]        ; co_name
    mov [r13 + PyCodeObject.co_name], rax

    mov rax, [rsp + 72]        ; co_qualname
    mov [r13 + PyCodeObject.co_qualname], rax

    mov rax, [rsp + 88]        ; co_exceptiontable
    mov [r13 + PyCodeObject.co_exceptiontable], rax

    ; Bytecode length and padding
    mov dword [r13 + PyCodeObject.co_code_len], r14d
    mov dword [r13 + PyCodeObject.co_padding], 0

    ; Copy bytecode from co_code bytes object into inline area
    test r14, r14
    jz .code_no_bytecode
    lea rdi, [r13 + PyCodeObject.co_code]
    mov rax, [rsp + 16]        ; co_code bytes object
    lea rsi, [rax + PyBytesObject.data]
    mov rdx, r14
    call memcpy wrt ..plt
.code_no_bytecode:

    ; Update ref placeholder if FLAG_REF was set
    mov r12, [rsp + 96]        ; restore FLAG_REF into r12
    test r12d, r12d
    jz .code_no_fixup
    mov rax, [rsp + 104]       ; ref index
    mov rcx, [rel marshal_refs]
    mov [rcx + rax * 8], r13   ; fix up the placeholder with actual code obj
.code_no_fixup:

    mov rax, r13               ; return code object

    add rsp, 128
    pop r15
    pop r14
    pop r13

    ; We handled FLAG_REF ourselves (placeholder + fixup), so skip mfinish.
    ; But r12 is restored and the marshal_read_object prologue pushed rbx, r12.
    ; We still need to pop those and return.
    pop r12
    pop rbx
    pop rbp
    ret

;--------------------------------------------------------------------------
; BSS section: marshal global state
;--------------------------------------------------------------------------
section .bss
global marshal_buf
global marshal_pos
global marshal_len
global marshal_refs
global marshal_ref_count
global marshal_ref_cap

marshal_buf:       resq 1     ; pointer to file data
marshal_pos:       resq 1     ; current read position
marshal_len:       resq 1     ; total data length
marshal_refs:      resq 1     ; pointer to PyObject* array
marshal_ref_count: resq 1     ; number of refs stored
marshal_ref_cap:   resq 1     ; capacity of ref array

;--------------------------------------------------------------------------
; Read-only data: error messages
;--------------------------------------------------------------------------
section .rodata
marshal_err_eof:     db "marshal: unexpected end of data", 0
marshal_err_unknown: db "marshal: unknown type code", 0
marshal_err_ref_oob: db "marshal: reference index out of bounds", 0
