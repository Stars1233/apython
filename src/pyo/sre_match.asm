; sre_match.asm - SRE_Match type implementation
; Provides group/groups/groupdict/start/end/span methods and
; re/string/pos/endpos/lastindex/lastgroup/regs attributes.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"
%include "sre.inc"

extern ap_malloc
extern ap_free
extern ap_memcpy
extern ap_strcmp
extern obj_decref
extern obj_incref
extern obj_dealloc
extern str_from_cstr_heap
extern str_new_heap
extern int_from_i64
extern dict_new
extern dict_set
extern dict_get
extern tuple_new
extern none_singleton
extern raise_exception
extern exc_TypeError_type
extern exc_IndexError_type
extern type_type
extern builtin_func_new
extern method_new
extern sre_pattern_type
extern sre_strcmp

; ============================================================================
; sre_match_new(SRE_State* state, SRE_PatternObject* pattern,
;               PyStrObject* string) -> SRE_MatchObject*
; Create a match object from a successful match state.
; ============================================================================
MN_STATE    equ 8
MN_PAT      equ 16
MN_STR      equ 24
MN_FRAME    equ 24

DEF_FUNC sre_match_new, MN_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - MN_STATE], rdi
    mov [rbp - MN_PAT], rsi
    mov [rbp - MN_STR], rdx

    ; Calculate marks count (groups * 2 + 2 for group 0)
    mov eax, [rsi + SRE_PatternObject.groups]
    lea ecx, [eax + 1]
    shl ecx, 1                 ; marks_count = (groups + 1) * 2
    mov r12d, ecx              ; r12 = marks count

    ; Allocate match object: base size + marks array
    lea edi, [r12*8]
    add edi, SRE_MatchObject_size
    call ap_malloc
    mov rbx, rax               ; rbx = match object

    ; Initialize header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel sre_match_type]
    mov [rbx + PyObject.ob_type], rax

    ; Set fields
    mov rax, [rbp - MN_PAT]
    mov [rbx + SRE_MatchObject.pattern], rax
    inc qword [rax + PyObject.ob_refcnt]

    mov rax, [rbp - MN_STR]
    mov [rbx + SRE_MatchObject.string], rax
    inc qword [rax + PyObject.ob_refcnt]

    mov rdi, [rbp - MN_STATE]
    mov rax, [rdi + SRE_State.str_begin]
    mov [rbx + SRE_MatchObject.str_begin], rax

    ; pos = original_pos (the pos argument passed to match/search)
    mov rax, [rdi + SRE_State.original_pos]
    mov [rbx + SRE_MatchObject.pos], rax

    ; endpos
    mov rdi, [rbp - MN_STATE]
    mov rax, [rdi + SRE_State.codepoint_buf]
    test rax, rax
    jnz .mn_unicode_endpos
    mov rax, [rdi + SRE_State.str_end]
    sub rax, [rdi + SRE_State.str_begin]
    jmp .mn_set_endpos
.mn_unicode_endpos:
    mov rax, [rdi + SRE_State.codepoint_len]
.mn_set_endpos:
    mov [rbx + SRE_MatchObject.endpos], rax

    ; lastindex
    mov rdi, [rbp - MN_STATE]
    mov rax, [rdi + SRE_State.lastindex]
    mov [rbx + SRE_MatchObject.lastindex], rax

    ; lastgroup: look up indexgroup[lastindex] if available
    mov qword [rbx + SRE_MatchObject.lastgroup], 0
    mov rax, [rbp - MN_STATE]
    mov rcx, [rax + SRE_State.lastindex]
    cmp rcx, 0
    jle .mn_no_lastgroup
    ; Check if pattern has indexgroup tuple
    mov rax, [rbp - MN_PAT]
    mov rax, [rax + SRE_PatternObject.indexgroup]
    test rax, rax
    jz .mn_no_lastgroup
    ; Bounds check: lastindex < tuple.ob_size
    cmp rcx, [rax + PyVarObject.ob_size]
    jge .mn_no_lastgroup
    ; indexgroup is a fat tuple: 16-byte stride at ob_item
    shl rcx, 4
    add rcx, PyTupleObject.ob_item
    mov rdx, [rax + rcx + 8]   ; tag
    cmp edx, TAG_PTR
    jne .mn_no_lastgroup        ; None or non-string → NULL
    mov rax, [rax + rcx]       ; payload (string ptr)
    inc qword [rax + PyObject.ob_refcnt]
    mov [rbx + SRE_MatchObject.lastgroup], rax
.mn_no_lastgroup:

    ; marks_count
    mov [rbx + SRE_MatchObject.marks_count], r12

    ; Copy marks: group 0 = (str_start, str_pos), then from state marks
    mov rdi, [rbp - MN_STATE]

    ; Mark 0 = match start position
    mov rax, [rdi + SRE_State.str_start]
    lea rcx, [rbx + SRE_MatchObject.marks]
    mov [rcx], rax

    ; Mark 1 = match end position
    mov rax, [rdi + SRE_State.str_pos]
    mov [rcx + 8], rax

    ; Copy state marks to match.marks[2..] (group 1+ data)
    ; State marks[0] = MARK 0 from bytecode = group 1 start
    ; State marks[1] = MARK 1 from bytecode = group 1 end
    ; etc. These go into match.marks[2..] (after group 0 pair)
    mov r13, [rdi + SRE_State.marks_size]
    cmp r13, 0
    je .mn_no_marks

    ; Source: state marks from index 0
    mov rsi, [rdi + SRE_State.marks]
    lea rdi, [rcx + 16]       ; destination: after group 0 marks

    ; Count: min(state.marks_size, r12 - 2)
    mov rcx, r12
    sub rcx, 2
    cmp r13, rcx
    jle .mn_use_state_count
    mov r13, rcx
.mn_use_state_count:
    test r13, r13
    jle .mn_no_marks
    shl r13, 3                 ; bytes
    mov rdx, r13
    call ap_memcpy

    ; Initialize any remaining marks to -1
    jmp .mn_init_remaining

.mn_no_marks:
    ; Initialize all marks after group 0 to -1
    lea rdi, [rbx + SRE_MatchObject.marks + 16]
    mov rcx, r12
    sub rcx, 2
    test rcx, rcx
    jle .mn_done
.mn_fill_neg1:
    mov qword [rdi], -1
    add rdi, 8
    dec rcx
    jnz .mn_fill_neg1
    jmp .mn_done

.mn_init_remaining:
    ; Fill remaining marks (beyond what was copied) with -1
    ; r13 = bytes copied, r12 = total marks_count
    lea rdi, [rbx + SRE_MatchObject.marks + 16]
    add rdi, r13               ; past copied marks
    mov rcx, r12
    sub rcx, 2
    shl rcx, 3                ; total needed bytes
    sub rcx, r13              ; remaining bytes
    shr rcx, 3                ; remaining entries
    test rcx, rcx
    jle .mn_done
.mn_fill_remaining:
    mov qword [rdi], -1
    add rdi, 8
    dec rcx
    jnz .mn_fill_remaining
.mn_done:
    mov rax, rbx
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_match_new

; ============================================================================
; sre_match_dealloc(PyObject* self)
; ============================================================================
DEF_FUNC sre_match_dealloc
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + SRE_MatchObject.pattern]
    test rdi, rdi
    jz .no_pat
    call obj_decref
.no_pat:

    mov rdi, [rbx + SRE_MatchObject.string]
    test rdi, rdi
    jz .no_str
    call obj_decref
.no_str:

    mov rdi, [rbx + SRE_MatchObject.lastgroup]
    test rdi, rdi
    jz .no_lg
    call obj_decref
.no_lg:

    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC sre_match_dealloc

; ============================================================================
; sre_match_repr(PyObject* self) -> PyObject*
; ============================================================================
DEF_FUNC sre_match_repr
    CSTRING rdi, "<re.Match object>"
    call str_from_cstr_heap
    leave
    ret
END_FUNC sre_match_repr

; ============================================================================
; sre_match_bool(PyObject* self) -> i64 (0 or 1)
; Match objects are always truthy.
; ============================================================================
DEF_FUNC_BARE sre_match_bool
    mov rax, 1
    ret
END_FUNC sre_match_bool

; ============================================================================
; Helper: get substring for a group index
; sre_match_get_group_str(SRE_MatchObject* self, i64 group_idx) -> (rax, edx)
; Returns string for the group, or (0, TAG_NONE) if unmatched.
; ============================================================================
DEF_FUNC sre_match_get_group_str
    push rbx
    push r12
    push r13
    mov rbx, rdi               ; self

    ; Validate group index
    mov rax, [rbx + SRE_MatchObject.pattern]
    mov ecx, [rax + SRE_PatternObject.groups]
    inc ecx                     ; groups + 1 (includes group 0)
    cmp rsi, rcx
    jae .group_out_of_range

    ; Get marks for this group
    shl rsi, 1                  ; mark_start = group * 2
    lea rax, [rbx + SRE_MatchObject.marks]
    mov r12, [rax + rsi*8]     ; start pos (codepoint index)
    mov r13, [rax + rsi*8 + 8] ; end pos (codepoint index)

    cmp r12, -1
    je .group_none
    cmp r13, -1
    je .group_none

    ; Check if string has non-ASCII (scan for bytes >= 0x80)
    mov rdi, [rbx + SRE_MatchObject.string]
    mov rcx, [rdi + PyStrObject.ob_size]
    lea rdi, [rdi + PyStrObject.data]
    xor r8d, r8d
.group_scan_ascii:
    cmp r8, rcx
    jge .group_ascii
    cmp byte [rdi + r8], 0x80
    jae .group_unicode
    inc r8
    jmp .group_scan_ascii

.group_ascii:
    ; ASCII mode: byte index = codepoint index
    mov rdi, [rbx + SRE_MatchObject.str_begin]
    add rdi, r12
    mov rsi, r13
    sub rsi, r12
    call str_new_heap
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.group_unicode:
    ; Unicode mode: walk UTF-8 to find byte offsets for codepoint indices
    mov rdi, [rbx + SRE_MatchObject.string]
    mov rcx, [rdi + PyStrObject.ob_size]   ; total byte length
    lea rdi, [rdi + PyStrObject.data]

    ; Find byte offset for start codepoint index (r12)
    xor r8d, r8d               ; byte offset
    xor r9d, r9d               ; codepoint count
.group_find_start:
    cmp r9, r12
    jge .group_found_start
    cmp r8, rcx
    jge .group_found_start
    movzx eax, byte [rdi + r8]
    cmp al, 0x80
    jb .group_gs1
    cmp al, 0xE0
    jb .group_gs2
    cmp al, 0xF0
    jb .group_gs3
    add r8, 4
    jmp .group_gsinc
.group_gs1:
    inc r8
    jmp .group_gsinc
.group_gs2:
    add r8, 2
    jmp .group_gsinc
.group_gs3:
    add r8, 3
.group_gsinc:
    inc r9
    jmp .group_find_start
.group_found_start:
    push r8                    ; save start byte offset
    ; Find byte offset for end codepoint index (r13)
.group_find_end:
    cmp r9, r13
    jge .group_found_end
    cmp r8, rcx
    jge .group_found_end
    movzx eax, byte [rdi + r8]
    cmp al, 0x80
    jb .group_ge1
    cmp al, 0xE0
    jb .group_ge2
    cmp al, 0xF0
    jb .group_ge3
    add r8, 4
    jmp .group_geinc
.group_ge1:
    inc r8
    jmp .group_geinc
.group_ge2:
    add r8, 2
    jmp .group_geinc
.group_ge3:
    add r8, 3
.group_geinc:
    inc r9
    jmp .group_find_end
.group_found_end:
    pop rax                    ; start byte offset
    ; Create substring from byte offsets
    mov rdi, [rbx + SRE_MatchObject.str_begin]
    add rdi, rax
    mov rsi, r8
    sub rsi, rax
    call str_new_heap
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.group_none:
    xor eax, eax
    mov edx, TAG_NONE
    pop r13
    pop r12
    pop rbx
    leave
    ret

.group_out_of_range:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "no such group"
    call raise_exception
END_FUNC sre_match_get_group_str

; ============================================================================
; sre_match_resolve_group_idx(rdi=self, rsi=payload, edx=tag) -> rsi=int_idx
; Resolve a group argument: SmallInt passes through, string does dict lookup.
; On error (no such group), raises IndexError.
; ============================================================================
DEF_FUNC sre_match_resolve_group_idx
    ; SmallInt → return as-is
    cmp edx, TAG_SMALLINT
    je .rgi_done

    ; TAG_PTR (string) → look up in pattern->groupindex
    test edx, TAG_RC_BIT
    jz .rgi_type_error

    push rdi                    ; save self
    mov rdi, [rdi + SRE_MatchObject.pattern]
    mov rdi, [rdi + SRE_PatternObject.groupindex]
    test rdi, rdi
    jz .rgi_no_group_pop

    ; dict_get(groupindex, key, key_tag)
    ; rsi = key (already set), edx = TAG_PTR
    mov edx, TAG_PTR
    call dict_get
    ; rax = val payload, edx = val tag
    test edx, edx
    jz .rgi_no_group_pop2
    ; group_idx = val payload (SmallInt)
    mov rsi, rax
    pop rdi                     ; restore self (discard)
.rgi_done:
    leave
    ret

.rgi_no_group_pop:
    pop rdi
.rgi_no_group_pop2:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "no such group"
    call raise_exception

.rgi_type_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "no such group"
    call raise_exception
END_FUNC sre_match_resolve_group_idx

; ============================================================================
; sre_match_group_method(self, args, nargs)
; group(*args) — return one or more subgroups of the match.
; group() or group(0) returns entire match.
; group(n) returns nth group.
; group(a, b, ...) returns tuple.
; ============================================================================
DEF_FUNC sre_match_group_method
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13

    mov rbx, [rdi]             ; self = args[0] payload
    lea r12, [rdi + 16]        ; user args start at args[1]
    lea r13, [rsi - 1]         ; user nargs

    ; No args: return group(0)
    test r13, r13
    jz .group_zero

    ; One arg: return single group
    cmp r13, 1
    je .group_single

    ; Multiple args: return tuple
    mov edi, r13d
    call tuple_new
    push rax                   ; save tuple

    xor ecx, ecx               ; index
.group_multi_loop:
    cmp rcx, r13
    jge .group_multi_done
    push rcx
    push r13
    ; Get group index from args (may be string or int)
    mov rax, rcx
    shl rax, 4                 ; 16-byte stride
    mov rsi, [r12 + rax]       ; group arg payload
    mov edx, [r12 + rax + 8]  ; group arg tag
    mov rdi, rbx
    call sre_match_resolve_group_idx
    ; rsi = resolved int index
    mov rdi, rbx
    call sre_match_get_group_str
    ; rax = payload, edx = tag
    pop r13
    pop rcx
    ; Set in tuple: tuple.ob_item[ecx] = (rax, edx)
    mov rdi, [rsp]             ; tuple (on stack from push before loop)
    ; INCREF if needed
    INCREF_VAL rax, rdx
    ; Write payload and tag
    mov rsi, rcx
    shl rsi, 4                 ; index * 16
    add rsi, PyTupleObject.ob_item
    mov [rdi + rsi], rax       ; payload
    mov [rdi + rsi + 8], rdx   ; tag
    inc rcx
    jmp .group_multi_loop

.group_multi_done:
    pop rax                    ; tuple
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.group_zero:
    mov rdi, rbx
    xor esi, esi               ; group 0
    call sre_match_get_group_str
    pop r13
    pop r12
    pop rbx
    leave
    ret

.group_single:
    mov rsi, [r12]             ; group arg payload
    mov edx, [r12 + 8]        ; group arg tag
    mov rdi, rbx
    call sre_match_resolve_group_idx
    ; rsi = resolved int index
    mov rdi, rbx
    call sre_match_get_group_str
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_match_group_method

; ============================================================================
; sre_match_groups_method(self, args, nargs)
; groups(default=None) — return all groups as a tuple.
; ============================================================================
GS_DEFAULT     equ 8
GS_DEFAULT_TAG equ 16
GS_FRAME       equ 16

DEF_FUNC sre_match_groups_method, GS_FRAME
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, [rdi]             ; self = args[0] payload
    lea r12, [rdi + 16]        ; user args
    lea r13, [rsi - 1]         ; user nargs

    ; default arg (default = None)
    xor eax, eax
    mov edx, TAG_NONE
    cmp r13, 1
    jb .groups_no_default
    mov rax, [r12]             ; default payload
    mov edx, [r12 + 8]        ; default tag
.groups_no_default:
    mov [rbp - GS_DEFAULT], rax
    mov [rbp - GS_DEFAULT_TAG], rdx

    ; Get number of groups
    mov rax, [rbx + SRE_MatchObject.pattern]
    mov ecx, [rax + SRE_PatternObject.groups]
    test ecx, ecx
    jz .groups_empty

    ; Create tuple of size = groups
    mov edi, ecx
    push rcx
    call tuple_new
    pop rcx
    push rax                   ; save tuple

    ; Fill groups 1..groups
    mov r14d, ecx              ; total groups
    mov ecx, 1                 ; start from group 1
.groups_loop:
    cmp ecx, r14d
    ja .groups_done
    push rcx
    mov rdi, rbx
    mov rsi, rcx
    call sre_match_get_group_str
    ; If None and default was given, substitute default
    cmp edx, TAG_NONE
    jne .groups_use_val
    ; Use default instead of None
    mov rax, [rbp - GS_DEFAULT]
    mov edx, [rbp - GS_DEFAULT_TAG]
.groups_use_val:
    pop rcx
    push rcx
    ; Inline tuple_set: tuple.ob_item[group-1] = (rax, edx)
    mov rdi, [rsp + 8]        ; tuple
    INCREF_VAL rax, rdx
    lea esi, [ecx - 1]        ; tuple index = group - 1
    movsx rsi, esi
    shl rsi, 4                 ; * 16
    add rsi, PyTupleObject.ob_item
    mov [rdi + rsi], rax       ; payload
    mov [rdi + rsi + 8], rdx   ; tag
    pop rcx
    inc ecx
    jmp .groups_loop

.groups_done:
    pop rax                    ; tuple
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.groups_empty:
    ; Return empty tuple
    xor edi, edi
    call tuple_new
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_match_groups_method

; ============================================================================
; sre_match_start_method(self, args, nargs)
; start(group=0)
; ============================================================================
DEF_FUNC sre_match_start_method
    ; rdi = args (fat array), rsi = nargs
    push rbx
    mov rbx, [rdi]             ; self = args[0] payload
    lea rcx, [rdi + 16]        ; user args
    lea rdx, [rsi - 1]         ; user nargs

    ; Get group index (default 0)
    xor esi, esi               ; default group = 0
    test rdx, rdx
    jz .start_default
    mov rsi, [rcx]             ; group arg payload
    mov edx, [rcx + 8]        ; group arg tag
    mov rdi, rbx
    call sre_match_resolve_group_idx
    ; rsi = resolved int index
    jmp .start_resolved
.start_default:
.start_resolved:

    ; Get mark for group start
    shl rsi, 1                 ; mark_index = group * 2
    lea rax, [rbx + SRE_MatchObject.marks]

    ; Bounds check
    cmp rsi, [rbx + SRE_MatchObject.marks_count]
    jae .start_error

    mov rax, [rax + rsi*8]
    RET_TAG_SMALLINT

    pop rbx
    leave
    ret

.start_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "no such group"
    call raise_exception
END_FUNC sre_match_start_method

; ============================================================================
; sre_match_end_method(self, args, nargs)
; end(group=0)
; ============================================================================
DEF_FUNC sre_match_end_method
    ; rdi = args (fat array), rsi = nargs
    push rbx
    mov rbx, [rdi]             ; self = args[0] payload
    lea rcx, [rdi + 16]        ; user args
    lea rdx, [rsi - 1]         ; user nargs

    ; Get group index (default 0)
    xor esi, esi               ; default group = 0
    test rdx, rdx
    jz .end_default
    mov rsi, [rcx]             ; group arg payload
    mov edx, [rcx + 8]        ; group arg tag
    mov rdi, rbx
    call sre_match_resolve_group_idx
    ; rsi = resolved int index
    jmp .end_resolved
.end_default:
.end_resolved:

    shl rsi, 1
    inc rsi                     ; mark_index = group * 2 + 1
    lea rax, [rbx + SRE_MatchObject.marks]

    cmp rsi, [rbx + SRE_MatchObject.marks_count]
    jae .end_error

    mov rax, [rax + rsi*8]
    RET_TAG_SMALLINT

    pop rbx
    leave
    ret

.end_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "no such group"
    call raise_exception
END_FUNC sre_match_end_method

; ============================================================================
; sre_match_span_method(self, args, nargs)
; span(group=0) -> (start, end) tuple
; ============================================================================
DEF_FUNC sre_match_span_method
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    mov rbx, [rdi]             ; self = args[0] payload
    lea rcx, [rdi + 16]        ; user args
    lea rdx, [rsi - 1]         ; user nargs

    xor r12d, r12d             ; group = 0
    test rdx, rdx
    jz .span_default
    mov rsi, [rcx]             ; group arg payload
    mov edx, [rcx + 8]        ; group arg tag
    mov rdi, rbx
    call sre_match_resolve_group_idx
    mov r12, rsi               ; resolved int index
.span_default:

    ; Get start and end
    mov rcx, r12
    shl rcx, 1
    lea rax, [rbx + SRE_MatchObject.marks]

    ; Bounds check
    lea rdx, [rcx + 1]
    cmp rdx, [rbx + SRE_MatchObject.marks_count]
    jae .span_error

    mov r8, [rax + rcx*8]      ; start
    mov r9, [rax + rcx*8 + 8]  ; end

    ; Save mark values across tuple_new call (clobbers caller-saved regs)
    push r8
    push r9

    ; Create tuple
    mov edi, 2
    call tuple_new

    pop r9
    pop r8

    ; Set start: tuple.ob_item[0] = (r8, TAG_SMALLINT)
    mov [rax + PyTupleObject.ob_item], r8
    mov qword [rax + PyTupleObject.ob_item + 8], TAG_SMALLINT

    ; Set end: tuple.ob_item[1] = (r9, TAG_SMALLINT)
    mov [rax + PyTupleObject.ob_item + 16], r9
    mov qword [rax + PyTupleObject.ob_item + 24], TAG_SMALLINT
    mov edx, TAG_PTR

    pop r12
    pop rbx
    leave
    ret

.span_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "no such group"
    call raise_exception
END_FUNC sre_match_span_method

; ============================================================================
; sre_match_groupdict_method(self, args, nargs)
; groupdict(default=None) -> dict of named groups
; ============================================================================
GD_SELF        equ 8
GD_RESULT      equ 16
GD_ENTRIES     equ 24
GD_IDX         equ 32
GD_CAP         equ 40
GD_DEFAULT     equ 48
GD_DEFAULT_TAG equ 56
GD_FRAME       equ 56

DEF_FUNC sre_match_groupdict_method, GD_FRAME
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13
    push r14

    mov rax, [rdi]             ; self = args[0] payload
    mov [rbp - GD_SELF], rax

    ; default arg (default = None)
    xor ecx, ecx
    mov r8d, TAG_NONE
    lea rdx, [rsi - 1]        ; user nargs
    cmp rdx, 1
    jb .gd_no_default
    mov rcx, [rdi + 16]       ; default payload
    mov r8d, [rdi + 24]       ; default tag
.gd_no_default:
    mov [rbp - GD_DEFAULT], rcx
    mov [rbp - GD_DEFAULT_TAG], r8

    ; Create result dict
    call dict_new
    mov [rbp - GD_RESULT], rax

    ; Get pattern's groupindex dict
    mov rax, [rbp - GD_SELF]
    mov rax, [rax + SRE_MatchObject.pattern]
    mov rcx, [rax + SRE_PatternObject.groupindex]
    test rcx, rcx
    jz .gd_done

    ; Iterate groupindex entries
    mov rax, [rcx + PyDictObject.entries]
    mov [rbp - GD_ENTRIES], rax
    mov rax, [rcx + PyDictObject.capacity]
    mov [rbp - GD_CAP], rax
    mov qword [rbp - GD_IDX], 0

.gd_loop:
    mov rax, [rbp - GD_IDX]
    cmp rax, [rbp - GD_CAP]
    jge .gd_done

    ; entry = entries + idx * DICT_ENTRY_SIZE
    mov rcx, [rbp - GD_ENTRIES]
    imul rax, DICT_ENTRY_SIZE
    lea r12, [rcx + rax]       ; r12 = entry ptr

    ; Skip empty/tombstone entries
    mov rax, [r12 + DictEntry.key_tag]
    test rax, rax
    jz .gd_next                ; empty slot (key_tag == 0)
    cmp qword [r12 + DictEntry.hash], -1
    je .gd_next                ; tombstone

    ; name = entry->key, name_tag = entry->key_tag
    mov r13, [r12 + DictEntry.key]       ; name payload
    mov r14, [r12 + DictEntry.key_tag]   ; name tag

    ; group_idx = entry->value (SmallInt payload)
    mov rsi, [r12 + DictEntry.value]     ; group index

    ; sre_match_get_group_str(self, group_idx)
    mov rdi, [rbp - GD_SELF]
    call sre_match_get_group_str
    ; rax = val payload, edx = val tag
    ; If None, substitute default
    cmp edx, TAG_NONE
    jne .gd_use_val
    mov rax, [rbp - GD_DEFAULT]
    mov edx, [rbp - GD_DEFAULT_TAG]
.gd_use_val:

    ; dict_set(result, name, val, val_tag, name_tag)
    push rax
    push rdx
    mov rdi, [rbp - GD_RESULT]
    mov rsi, r13               ; key
    ; rdx = val (still in rax from call), edx = val_tag
    mov rdx, [rsp + 8]        ; val payload
    mov ecx, [rsp]            ; val tag
    mov r8, r14                ; key tag
    call dict_set
    pop rdx
    pop rax
    DECREF_VAL rax, rdx

.gd_next:
    inc qword [rbp - GD_IDX]
    jmp .gd_loop

.gd_done:
    mov rax, [rbp - GD_RESULT]
    mov edx, TAG_PTR

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_match_groupdict_method

; ============================================================================
; sre_match_expand_method(self, args, nargs)
; expand(template) — stub: returns template as-is.
; Real expansion handled by re._expand() in pure Python.
; ============================================================================
DEF_FUNC sre_match_expand_method
    ; rdi = args (fat array), rsi = nargs
    ; args[1] = template
    mov rax, [rdi + 16]       ; template payload
    mov rdx, [rdi + 24]       ; template tag
    INCREF_VAL rax, rdx
    leave
    ret
END_FUNC sre_match_expand_method

; ============================================================================
; sre_match_copy_method(self, args, nargs)
; __copy__ / __deepcopy__ — match objects are effectively immutable.
; Returns self (INCREF'd).
; ============================================================================
DEF_FUNC sre_match_copy_method
    ; rdi = args (fat array), rsi = nargs
    mov rax, [rdi]             ; self payload
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC sre_match_copy_method

; ============================================================================
; sre_match_subscript(self, key, key_tag) -> (rax, edx)
; __getitem__ for match[group] indexing.
; ============================================================================
DEF_FUNC sre_match_subscript
    ; rdi = self, rsi = key payload, rdx = key tag
    push rbx
    push r12

    mov rbx, rdi               ; self
    mov r12, rsi               ; key payload

    ; Check tag: SmallInt = direct group index
    cmp edx, TAG_SMALLINT
    je .ms_int_key

    ; Check tag: string = named group lookup
    test edx, TAG_RC_BIT
    jz .ms_type_error

    ; String key: look up in pattern->groupindex
    mov rdi, [rbx + SRE_MatchObject.pattern]
    mov rdi, [rdi + SRE_PatternObject.groupindex]
    test rdi, rdi
    jz .ms_no_such_group

    ; dict_get(groupindex, key, key_tag)
    mov rsi, r12               ; key
    mov edx, TAG_PTR           ; key_tag (string is TAG_PTR)
    call dict_get
    ; rax = val payload, edx = val tag
    test edx, edx
    jz .ms_no_such_group

    ; group_idx = val payload (SmallInt)
    mov r12, rax               ; group index

.ms_int_key:
    mov rdi, rbx               ; self
    mov rsi, r12               ; group index
    call sre_match_get_group_str

    pop r12
    pop rbx
    leave
    ret

.ms_no_such_group:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "no such group"
    call raise_exception

.ms_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "group index must be int or string"
    call raise_exception
END_FUNC sre_match_subscript

; ============================================================================
; sre_match_getattr(PyObject* self, PyObject* name) -> PyObject*
; ============================================================================
DEF_FUNC sre_match_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self
    mov r12, rsi               ; name

    lea rdi, [r12 + PyStrObject.data]

    ; Check methods
    lea rsi, [rel sm_group_str]
    call sre_strcmp
    test eax, eax
    jz .mga_group

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_groups_str]
    call sre_strcmp
    test eax, eax
    jz .mga_groups

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_groupdict_str]
    call sre_strcmp
    test eax, eax
    jz .mga_groupdict

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_start_str]
    call sre_strcmp
    test eax, eax
    jz .mga_start

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_end_str]
    call sre_strcmp
    test eax, eax
    jz .mga_end

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_span_str]
    call sre_strcmp
    test eax, eax
    jz .mga_span

    ; Check attributes
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_re_str]
    call sre_strcmp
    test eax, eax
    jz .mga_re

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_string_str]
    call sre_strcmp
    test eax, eax
    jz .mga_string

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_pos_str]
    call sre_strcmp
    test eax, eax
    jz .mga_pos

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_endpos_str]
    call sre_strcmp
    test eax, eax
    jz .mga_endpos

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_lastindex_str]
    call sre_strcmp
    test eax, eax
    jz .mga_lastindex

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_lastgroup_str]
    call sre_strcmp
    test eax, eax
    jz .mga_lastgroup

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_regs_str]
    call sre_strcmp
    test eax, eax
    jz .mga_regs

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_expand_str]
    call sre_strcmp
    test eax, eax
    jz .mga_expand

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_copy_str]
    call sre_strcmp
    test eax, eax
    jz .mga_copy

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sm_deepcopy_str]
    call sre_strcmp
    test eax, eax
    jz .mga_copy

    ; Not found
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

    ; --- Methods (create bound methods) ---
.mga_group:
    lea rdi, [rel sre_match_group_method]
    lea rsi, [rel sm_group_str]
    jmp .mga_bind
.mga_groups:
    lea rdi, [rel sre_match_groups_method]
    lea rsi, [rel sm_groups_str]
    jmp .mga_bind
.mga_groupdict:
    lea rdi, [rel sre_match_groupdict_method]
    lea rsi, [rel sm_groupdict_str]
    jmp .mga_bind
.mga_start:
    lea rdi, [rel sre_match_start_method]
    lea rsi, [rel sm_start_str]
    jmp .mga_bind
.mga_end:
    lea rdi, [rel sre_match_end_method]
    lea rsi, [rel sm_end_str]
    jmp .mga_bind
.mga_span:
    lea rdi, [rel sre_match_span_method]
    lea rsi, [rel sm_span_str]
    jmp .mga_bind

.mga_bind:
    push rbx                   ; save self for method_new
    call builtin_func_new      ; rax = builtin func wrapper
    pop rsi                    ; rsi = self (match object)
    mov rdi, rax               ; rdi = func
    push rax                   ; save for DECREF
    call method_new            ; rax = bound method
    push rax
    mov rdi, [rsp + 8]         ; DECREF builtin func (method_new INCREF'd)
    call obj_decref
    pop rax
    add rsp, 8
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

    ; --- Attributes ---
.mga_re:
    mov rax, [rbx + SRE_MatchObject.pattern]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.mga_string:
    mov rax, [rbx + SRE_MatchObject.string]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.mga_pos:
    mov rax, [rbx + SRE_MatchObject.pos]
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret

.mga_endpos:
    mov rax, [rbx + SRE_MatchObject.endpos]
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret

.mga_lastindex:
    mov rax, [rbx + SRE_MatchObject.lastindex]
    cmp rax, -1
    je .mga_lastindex_none
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret
.mga_lastindex_none:
    xor eax, eax
    mov edx, TAG_NONE
    pop r12
    pop rbx
    leave
    ret

.mga_lastgroup:
    mov rax, [rbx + SRE_MatchObject.lastgroup]
    test rax, rax
    jz .mga_lastgroup_none
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
.mga_lastgroup_none:
    xor eax, eax
    mov edx, TAG_NONE
    pop r12
    pop rbx
    leave
    ret

.mga_regs:
    ; Build tuple of (start, end) tuples for all groups
    push r13
    push r14
    push r15

    mov rax, [rbx + SRE_MatchObject.pattern]
    mov r14d, [rax + SRE_PatternObject.groups]
    inc r14d                   ; ngroups = groups + 1
    movsx r14, r14d

    ; Create outer tuple
    mov edi, r14d
    call tuple_new
    mov r15, rax               ; r15 = outer tuple

    xor r13d, r13d             ; i = 0
.mga_regs_loop:
    cmp r13, r14
    jge .mga_regs_done

    ; Get start/end marks for group i
    mov rcx, r13
    shl rcx, 1                 ; mark_index = i * 2
    lea rax, [rbx + SRE_MatchObject.marks]
    push qword [rax + rcx*8]      ; start
    push qword [rax + rcx*8 + 8]  ; end

    ; Create inner tuple (2 elements)
    mov edi, 2
    call tuple_new
    ; rax = inner tuple

    pop rcx                    ; end
    pop rdx                    ; start

    ; Set inner[0] = start (SmallInt)
    mov [rax + PyTupleObject.ob_item], rdx
    mov qword [rax + PyTupleObject.ob_item + 8], TAG_SMALLINT

    ; Set inner[1] = end (SmallInt)
    mov [rax + PyTupleObject.ob_item + 16], rcx
    mov qword [rax + PyTupleObject.ob_item + 24], TAG_SMALLINT

    ; Set outer[i] = inner (TAG_PTR)
    mov rcx, r13
    shl rcx, 4                 ; i * 16
    mov [r15 + PyTupleObject.ob_item + rcx], rax
    mov qword [r15 + PyTupleObject.ob_item + rcx + 8], TAG_PTR

    inc r13
    jmp .mga_regs_loop

.mga_regs_done:
    mov rax, r15
    mov edx, TAG_PTR
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.mga_expand:
    lea rdi, [rel sre_match_expand_method]
    lea rsi, [rel sm_expand_str]
    jmp .mga_bind

.mga_copy:
    lea rdi, [rel sre_match_copy_method]
    lea rsi, [rel sm_copy_str]
    jmp .mga_bind
END_FUNC sre_match_getattr

; sre_strcmp is now shared from sre_pattern.asm (extern sre_strcmp)

; ============================================================================
; Type definition
; ============================================================================
section .data
align 8

sre_match_mapping_methods:
    dq 0                       ; mp_length
    dq sre_match_subscript     ; mp_subscript
    dq 0                       ; mp_ass_subscript

align 8
global sre_match_type
sre_match_type:
    dq 1                       ; ob_refcnt (immortal)
    dq type_type               ; ob_type
    dq sm_type_name            ; tp_name
    dq SRE_MatchObject_size    ; tp_basicsize
    dq sre_match_dealloc       ; tp_dealloc
    dq sre_match_repr          ; tp_repr
    dq sre_match_repr          ; tp_str
    dq 0                       ; tp_hash
    dq 0                       ; tp_call
    dq sre_match_getattr       ; tp_getattr
    dq 0                       ; tp_setattr
    dq 0                       ; tp_richcompare
    dq 0                       ; tp_iter
    dq 0                       ; tp_iternext
    dq 0                       ; tp_init
    dq 0                       ; tp_new
    dq 0                       ; tp_as_number
    dq 0                       ; tp_as_sequence
    dq sre_match_mapping_methods ; tp_as_mapping
    dq 0                       ; tp_base
    dq 0                       ; tp_dict
    dq 0                       ; tp_mro
    dq 0                       ; tp_flags
    dq 0                       ; tp_bases

section .rodata
sm_type_name:      db "re.Match", 0
sm_group_str:      db "group", 0
sm_groups_str:     db "groups", 0
sm_groupdict_str:  db "groupdict", 0
sm_start_str:      db "start", 0
sm_end_str:        db "end", 0
sm_span_str:       db "span", 0
sm_re_str:         db "re", 0
sm_string_str:     db "string", 0
sm_pos_str:        db "pos", 0
sm_endpos_str:     db "endpos", 0
sm_lastindex_str:  db "lastindex", 0
sm_lastgroup_str:  db "lastgroup", 0
sm_regs_str:       db "regs", 0
sm_expand_str:     db "expand", 0
sm_copy_str:       db "__copy__", 0
sm_deepcopy_str:   db "__deepcopy__", 0
