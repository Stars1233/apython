; except.asm - Exception table parser for Python 3.12
;
; Parses co_exceptiontable to find exception handlers.
; Python 3.12 exception table format:
;   Variable-length entries, each consisting of:
;     start: unsigned varint (bytecode offset in halfwords / instruction units)
;     length: unsigned varint (range length in instruction units)
;     target: unsigned varint (handler target in instruction units)
;     depth_lasti: unsigned varint (depth << 1 | push_lasti)
;
;   Varint encoding: each byte contributes 6 bits of data.
;     bits 0-5: data bits
;     bit 6: continuation flag (1 = more bytes follow)
;     bit 7: unused (0 for start/length/target, but first byte of start has
;             bit 7 unused)
;
; Actually, Python 3.12 uses a simpler encoding:
;   Each byte: bits 0-5 = value, bit 6 = extend (more bytes), bit 7 = start marker
;   But the real format is: unsigned LEB128-like with 6-bit chunks
;   Read sequence: for each entry component, accumulate bytes while bit 6 is set

%include "macros.inc"
%include "object.inc"
%include "errcodes.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

; exc_table_find_handler(PyCodeObject *code, int bytecode_offset_halfwords)
;   -> rax = handler target (in halfwords), rdx = stack depth, rcx = push_lasti
;   -> rax = -1 if no handler found
;
; bytecode_offset_halfwords = (rbx - &code.co_code) / 2
;
; rdi = code object
; esi = bytecode offset in instruction units (halfwords, i.e., 2-byte units)
global exc_table_find_handler
exc_table_find_handler:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12d, esi           ; r12d = target offset (in instruction units)

    ; Get co_exceptiontable (PyBytesObject*)
    mov rax, [rdi + PyCodeObject.co_exceptiontable]
    test rax, rax
    jz .not_found

    ; Get table data pointer and size
    mov r13, [rax + PyBytesObject.ob_size]  ; r13 = table length
    test r13, r13
    jz .not_found
    lea r14, [rax + PyBytesObject.data]     ; r14 = table data start
    xor r15d, r15d                          ; r15 = current position in table

.scan_entry:
    cmp r15, r13
    jge .not_found

    ; Read start (unsigned varint)
    call .read_varint
    mov ebx, eax            ; ebx = start

    ; Read length (unsigned varint)
    ; NOTE: .read_varint clobbers ecx/esi, so use r8/r9 for length/target
    call .read_varint
    mov r8d, eax            ; r8d = length (safe from .read_varint)

    ; Read target (unsigned varint)
    call .read_varint
    mov r9d, eax            ; r9d = target (safe from .read_varint)

    ; Read depth_lasti (unsigned varint)
    call .read_varint
    ; eax = depth_lasti: depth = eax >> 1, push_lasti = eax & 1
    mov edi, eax            ; edi = depth_lasti

    ; Check if bytecode_offset is in range [start, start+length)
    cmp r12d, ebx
    jb .scan_entry           ; offset < start, try next
    lea edx, [ebx + r8d]    ; edx = start + length
    cmp r12d, edx
    jge .scan_entry          ; offset >= start + length, try next

    ; Found a matching handler!
    ; Return: rax = target, edx = depth, ecx = push_lasti
    mov eax, r9d            ; handler target in instruction units
    mov ecx, edi
    and ecx, 1              ; push_lasti flag
    shr edi, 1
    mov edx, edi            ; stack depth

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.not_found:
    mov rax, -1
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; Internal: read an unsigned varint from table at r14+r15
; Returns value in eax, advances r15
; Big-endian 6-bit chunks: first byte = high bits, bit 6 = continue
; Algorithm: val = b & 63; while (b & 64) { val <<= 6; b = next; val |= b & 63; }
.read_varint:
    cmp r15, r13
    jge .varint_zero         ; safety: don't read past end
    movzx edx, byte [r14 + r15]
    inc r15
    mov eax, edx
    and eax, 0x3F           ; initial value = bits 0-5 of first byte

.varint_loop:
    test edx, 0x40          ; check continue bit
    jz .varint_done
    cmp r15, r13
    jge .varint_done         ; safety
    shl eax, 6              ; shift accumulated value LEFT
    movzx edx, byte [r14 + r15]
    inc r15
    mov ecx, edx
    and ecx, 0x3F
    or eax, ecx             ; OR in new 6 bits at bottom
    jmp .varint_loop

.varint_zero:
    xor eax, eax
.varint_done:
    ret
