; sre_pattern.asm - SRE_Pattern type implementation
; Provides match/search/fullmatch/findall/finditer/sub/subn/split methods.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"
%include "sre.inc"

extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_memcpy
extern obj_decref
extern obj_incref
extern obj_dealloc
extern str_from_cstr_heap
extern str_new_heap
extern str_concat
extern int_from_i64
extern dict_new
extern dict_set
extern dict_get
extern list_new
extern list_append
extern tuple_new
extern none_singleton
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
extern type_type
extern str_type
extern builtin_func_new
extern method_new
extern func_type
extern method_type
extern builtin_func_type
extern bool_true
extern bool_false
extern smallstr_to_obj

; SRE engine functions
extern sre_match
extern sre_search
extern sre_state_init
extern sre_state_fini
extern sre_string_len
extern sre_utf8_codepoint_to_byte

; SRE match object
extern sre_match_new
extern sre_match_type

; ============================================================================
; Forward declarations
; ============================================================================

; ============================================================================
; sre_pattern_dealloc(PyObject* self)
; ============================================================================
DEF_FUNC sre_pattern_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF pattern string
    mov rdi, [rbx + SRE_PatternObject.pattern]
    test rdi, rdi
    jz .no_pattern
    call obj_decref
.no_pattern:

    ; DECREF groupindex
    mov rdi, [rbx + SRE_PatternObject.groupindex]
    test rdi, rdi
    jz .no_gi
    call obj_decref
.no_gi:

    ; DECREF indexgroup
    mov rdi, [rbx + SRE_PatternObject.indexgroup]
    test rdi, rdi
    jz .no_ig
    call obj_decref
.no_ig:

    ; Free code array
    mov rdi, [rbx + SRE_PatternObject.code]
    test rdi, rdi
    jz .no_code
    call ap_free
.no_code:

    ; Free object
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC sre_pattern_dealloc

; ============================================================================
; sre_pattern_repr(PyObject* self) -> PyObject*
; ============================================================================
DEF_FUNC sre_pattern_repr
    CSTRING rdi, "re.compile(...)"
    call str_from_cstr_heap
    leave
    ret
END_FUNC sre_pattern_repr

; ============================================================================
; Helper: run match/search/fullmatch and return MatchObject or None
; rdi = pattern obj, rsi = string, rdx = pos, rcx = endpos,
; r8 = mode (0=match, 1=search, 2=fullmatch)
; ============================================================================
PM_PAT      equ 8
PM_STR      equ 16
PM_POS      equ 24
PM_ENDPOS   equ 32
PM_MODE     equ 40
PM_STATE    equ 40 + SRE_State_size  ; state base at BOTTOM of frame
PM_FRAME    equ 40 + SRE_State_size

DEF_FUNC sre_pattern_do_match, PM_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov [rbp - PM_PAT], rdi
    mov [rbp - PM_STR], rsi
    mov [rbp - PM_POS], rdx
    mov [rbp - PM_ENDPOS], rcx
    mov [rbp - PM_MODE], r8

    ; Initialize state
    lea rdi, [rbp - PM_STATE]
    mov rsi, [rbp - PM_PAT]
    mov rdx, [rbp - PM_STR]
    mov rcx, [rbp - PM_POS]
    mov r8, [rbp - PM_ENDPOS]
    call sre_state_init

    ; Set fullmatch mode if applicable
    mov rax, [rbp - PM_MODE]
    cmp rax, 2
    jne .no_fullmatch
    lea rdi, [rbp - PM_STATE]
    mov dword [rdi + SRE_State.match_all], 1
.no_fullmatch:

    ; Run match or search
    mov rax, [rbp - PM_MODE]
    cmp rax, 1
    je .do_search

    ; match/fullmatch: try at pos only
    lea rdi, [rbp - PM_STATE]
    mov rsi, [rbp - PM_PAT]
    mov rsi, [rsi + SRE_PatternObject.code]
    call sre_match
    jmp .check_result

.do_search:
    lea rdi, [rbp - PM_STATE]
    call sre_search

.check_result:
    test eax, eax
    jz .no_match

    ; Create SRE_MatchObject
    lea rdi, [rbp - PM_STATE]
    mov rsi, [rbp - PM_PAT]
    mov rdx, [rbp - PM_STR]
    call sre_match_new
    mov r12, rax               ; match object

    ; Clean up state
    lea rdi, [rbp - PM_STATE]
    call sre_state_fini

    mov rax, r12
    mov edx, TAG_PTR

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.no_match:
    lea rdi, [rbp - PM_STATE]
    call sre_state_fini

    ; Return None
    xor eax, eax
    mov edx, TAG_NONE

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_pattern_do_match

; ============================================================================
; sre_pattern_match_method(self, args, nargs)
; Pattern.match(string, pos=0, endpos=sys.maxsize)
; ============================================================================
DEF_FUNC sre_pattern_match_method
    ; rdi = args (fat array), rsi = nargs
    ; args[0] = self (pattern), args[1..] = user args
    push rbx
    push r12
    mov rbx, [rdi]             ; pattern = args[0] payload
    lea r12, [rdi + 16]        ; user args start at args[1]
    lea rdx, [rsi - 1]         ; user nargs

    ; Get string arg
    cmp rdx, 0
    je .match_error
    mov rsi, [r12]             ; string payload

    ; Save user nargs for endpos check
    mov r8, rdx                ; r8 = user nargs

    ; pos (default 0)
    xor ecx, ecx
    cmp r8, 2
    jb .match_no_pos
    mov rcx, [r12 + 16]       ; pos
.match_no_pos:
    mov rdx, rcx               ; rdx = pos

    ; endpos (default large)
    mov rcx, 0x7FFFFFFFFFFFFFFF
    cmp r8, 3
    jb .match_no_endpos
    mov rcx, [r12 + 32]       ; endpos
.match_no_endpos:

    mov rdi, rbx               ; pattern
    ; rsi already = string
    ; rdx = pos
    ; rcx = endpos
    xor r8d, r8d               ; mode = 0 (match)
    call sre_pattern_do_match

    pop r12
    pop rbx
    leave
    ret

.match_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "match() requires a string argument"
    call raise_exception
END_FUNC sre_pattern_match_method

; ============================================================================
; sre_pattern_search_method(self, args, nargs)
; ============================================================================
DEF_FUNC sre_pattern_search_method
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13
    mov rbx, [rdi]             ; pattern = args[0] payload
    lea r12, [rdi + 16]        ; user args
    lea r13, [rsi - 1]         ; user nargs

    cmp r13, 0
    je .search_error
    mov rsi, [r12]             ; string payload

    ; pos (default 0)
    xor edx, edx
    cmp r13, 2
    jb .search_no_pos
    mov rdx, [r12 + 16]       ; pos
.search_no_pos:

    ; endpos (default max)
    mov rcx, 0x7FFFFFFFFFFFFFFF
    cmp r13, 3
    jb .search_no_endpos
    mov rcx, [r12 + 32]       ; endpos
.search_no_endpos:

    mov rdi, rbx
    mov r8d, 1                 ; mode = search
    call sre_pattern_do_match

    pop r13
    pop r12
    pop rbx
    leave
    ret

.search_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "search() requires a string argument"
    call raise_exception
END_FUNC sre_pattern_search_method

; ============================================================================
; sre_pattern_fullmatch_method(self, args, nargs)
; ============================================================================
DEF_FUNC sre_pattern_fullmatch_method
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13
    mov rbx, [rdi]             ; pattern = args[0] payload
    lea r12, [rdi + 16]        ; user args
    lea r13, [rsi - 1]         ; user nargs

    cmp r13, 0
    je .fm_error
    mov rsi, [r12]             ; string payload

    ; pos (default 0)
    xor edx, edx
    cmp r13, 2
    jb .fm_no_pos
    mov rdx, [r12 + 16]       ; pos
.fm_no_pos:

    ; endpos (default max)
    mov rcx, 0x7FFFFFFFFFFFFFFF
    cmp r13, 3
    jb .fm_no_endpos
    mov rcx, [r12 + 32]       ; endpos
.fm_no_endpos:

    mov rdi, rbx
    mov r8d, 2                 ; mode = fullmatch
    call sre_pattern_do_match

    pop r13
    pop r12
    pop rbx
    leave
    ret

.fm_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "fullmatch() requires a string argument"
    call raise_exception
END_FUNC sre_pattern_fullmatch_method

; ============================================================================
; sre_substr_from_state(rdi=state, rsi=start_idx, rdx=end_idx)
; -> (rax=payload, edx=tag)
; Extracts substring handling both ASCII and codepoint modes.
; Returns (0, TAG_NONE) if start or end is -1.
; ============================================================================
global sre_substr_from_state
DEF_FUNC sre_substr_from_state
    ; Check for unmatched group
    cmp rsi, -1
    je .ss_none
    cmp rdx, -1
    je .ss_none

    push rbx
    push r12
    push r13

    mov rbx, rdi               ; state
    mov r12, rsi               ; start codepoint index
    mov r13, rdx               ; end codepoint index

    ; Check ASCII or Unicode mode
    mov rax, [rbx + SRE_State.codepoint_buf]
    test rax, rax
    jnz .ss_unicode

    ; ASCII mode: byte index = codepoint index
    mov rdi, [rbx + SRE_State.str_begin]
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

.ss_unicode:
    ; Unicode mode: use shared UTF-8 walker to find byte offsets
    mov rdi, [rbx + SRE_State.str_begin]
    mov rsi, [rbx + SRE_State.str_end]
    sub rsi, rdi               ; total byte length

    ; Find byte offset for start codepoint index (r12)
    mov rdx, r12               ; target = start codepoint idx
    xor ecx, ecx               ; start_byte = 0
    xor r8d, r8d               ; start_cp = 0
    push rdi                   ; save str_begin
    push rsi                   ; save byte_len
    call sre_utf8_codepoint_to_byte
    mov r14, rax               ; r14 = start byte offset

    ; Find byte offset for end codepoint index (r13)
    pop rsi                    ; restore byte_len
    pop rdi                    ; restore str_begin
    mov rdx, r13               ; target = end codepoint idx
    mov rcx, r14               ; start_byte = start offset
    mov r8, r12                ; start_cp = start idx
    call sre_utf8_codepoint_to_byte
    ; rax = end byte offset, r14 = start byte offset

    ; Create substring from byte offsets
    mov rdi, [rbx + SRE_State.str_begin]
    add rdi, r14
    mov rsi, rax
    sub rsi, r14
    call str_new_heap
    mov edx, TAG_PTR

    pop r13
    pop r12
    pop rbx
    leave
    ret

.ss_none:
    xor eax, eax
    mov edx, TAG_NONE
    leave
    ret
END_FUNC sre_substr_from_state

; ============================================================================
; sre_pattern_findall_method(self, args, nargs)
; Returns list of all non-overlapping matches.
; If pattern has no groups: list of matched strings.
; If 1 group: list of group(1) strings.
; If >1 groups: list of tuples.
; ============================================================================
FA_PAT      equ 8
FA_STR      equ 16
FA_LIST     equ 24
FA_STATE    equ 24 + SRE_State_size   ; state base at BOTTOM of frame
FA_FRAME    equ 24 + SRE_State_size

DEF_FUNC sre_pattern_findall_method, FA_FRAME
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rax, [rdi]             ; pattern = args[0] payload
    mov [rbp - FA_PAT], rax
    lea rdx, [rsi - 1]         ; user nargs
    cmp rdx, 0
    je .fa_error

    mov rax, [rdi + 16]       ; string = args[1] payload
    mov [rbp - FA_STR], rax

    ; pos (default 0)
    xor ecx, ecx
    cmp rdx, 2
    jb .fa_no_pos
    mov rcx, [rdi + 32]       ; args[2] = pos
.fa_no_pos:
    push rcx                   ; save pos

    ; endpos (default max)
    mov r8, 0x7FFFFFFFFFFFFFFF
    cmp rdx, 3
    jb .fa_no_endpos
    mov r8, [rdi + 48]        ; args[3] = endpos
.fa_no_endpos:
    push r8                    ; save endpos

    ; Create result list
    xor edi, edi
    call list_new
    mov [rbp - FA_LIST], rax
    mov r14, rax               ; r14 = result list

    ; Init state
    pop r8                     ; endpos
    pop rcx                    ; pos
    lea rdi, [rbp - FA_STATE]
    mov rsi, [rbp - FA_PAT]
    mov rdx, [rbp - FA_STR]
    call sre_state_init

    mov r12, [rbp - FA_PAT]   ; pattern
    mov r15d, [r12 + SRE_PatternObject.groups]  ; number of groups

.fa_loop:
    ; Reset marks
    lea rdi, [rbp - FA_STATE]
    mov qword [rdi + SRE_State.marks_size], 0
    mov qword [rdi + SRE_State.lastmark], -1
    mov qword [rdi + SRE_State.lastindex], -1
    mov qword [rdi + SRE_State.repeat_ctx], 0

    ; Search
    lea rdi, [rbp - FA_STATE]
    call sre_search
    test eax, eax
    jz .fa_done

    ; Found a match. What to append depends on groups.
    cmp r15d, 0
    je .fa_no_groups
    cmp r15d, 1
    je .fa_one_group

    ; Multiple groups: create tuple of group strings
    jmp .fa_multi_groups

.fa_no_groups:
    ; Append matched substring (group 0)
    lea rdi, [rbp - FA_STATE]
    mov rsi, [rdi + SRE_State.str_start]  ; match start
    mov rdx, [rdi + SRE_State.str_pos]    ; match end
    call sre_substr_from_state
    ; rax = str payload, edx = tag
    mov rdi, r14
    mov rsi, rax
    ; edx already set
    push rax
    push rdx
    call list_append
    pop rdx
    pop rax
    DECREF_VAL rax, rdx
    lea rdi, [rbp - FA_STATE]
    mov rcx, [rdi + SRE_State.str_pos]
    jmp .fa_advance

.fa_one_group:
    ; Append group(1) string
    ; State marks[0:1] = group 1 in CPython convention
    lea rdi, [rbp - FA_STATE]
    mov rax, [rdi + SRE_State.marks]
    cmp qword [rdi + SRE_State.marks_size], 2
    jb .fa_no_groups           ; fallback to whole match
    mov rsi, [rax]            ; mark[0] = group 1 start
    mov rdx, [rax + 8]       ; mark[1] = group 1 end
    lea rdi, [rbp - FA_STATE]
    call sre_substr_from_state
    ; rax = payload, edx = tag (str or None)
    mov rdi, r14
    mov rsi, rax
    ; edx already set
    push rax
    push rdx
    call list_append
    pop rdx
    pop rax
    DECREF_VAL rax, rdx
    lea rdi, [rbp - FA_STATE]
    mov rcx, [rdi + SRE_State.str_pos]
    jmp .fa_advance

.fa_multi_groups:
    ; Create tuple of group(1)..group(N) strings
    mov edi, r15d              ; N groups
    push r15
    call tuple_new
    pop r15
    mov rbx, rax               ; rbx = tuple

    mov ecx, 1                 ; group index (1-based)
.fa_mg_loop:
    cmp ecx, r15d
    ja .fa_mg_done
    push rcx

    ; Get marks for group ecx: state marks[2*(ecx-1)] and marks[2*(ecx-1)+1]
    ; State marks are 0-based: MARK 0/1 = group 1, MARK 2/3 = group 2, etc.
    lea rdi, [rbp - FA_STATE]
    mov rax, [rdi + SRE_State.marks]
    movsx r8, ecx
    dec r8                     ; r8 = group - 1
    shl r8, 1                  ; r8 = 2 * (group - 1)
    ; Check marks_size > 2*(group-1)+1
    lea r9, [r8 + 2]
    cmp r9, [rdi + SRE_State.marks_size]
    ja .fa_mg_none

    mov rsi, [rax + r8*8]     ; start
    mov rdx, [rax + r8*8 + 8] ; end
    lea rdi, [rbp - FA_STATE]
    call sre_substr_from_state
    jmp .fa_mg_set

.fa_mg_none:
    xor eax, eax
    mov edx, TAG_NONE

.fa_mg_set:
    ; Set tuple[group-1] = (rax, edx)
    ; Fresh strings from sre_substr_from_state have refcount=1;
    ; tuple takes ownership without INCREF. TAG_NONE has no RC_BIT.
    pop rcx
    push rcx
    lea esi, [ecx - 1]
    movsx rsi, esi
    mov r8, [rbx + PyTupleObject.ob_item]       ; payloads
    mov r9, [rbx + PyTupleObject.ob_item_tags]  ; tags
    mov [r8 + rsi*8], rax
    mov byte [r9 + rsi], dl

    pop rcx
    inc ecx
    jmp .fa_mg_loop

.fa_mg_done:
    ; Append tuple to list
    mov rdi, r14
    mov rsi, rbx
    mov edx, TAG_PTR
    call list_append
    mov rdi, rbx
    call obj_decref
    lea rdi, [rbp - FA_STATE]
    mov rcx, [rdi + SRE_State.str_pos]
    jmp .fa_advance

.fa_advance:
    ; Advance position past the match
    lea rdi, [rbp - FA_STATE]
    mov rax, [rdi + SRE_State.str_start]
    cmp rcx, rax
    jne .fa_advance_ok
    ; Zero-width match: advance by 1 to avoid infinite loop
    inc rcx
.fa_advance_ok:
    lea rdi, [rbp - FA_STATE]
    mov [rdi + SRE_State.str_pos], rcx
    mov [rdi + SRE_State.str_start], rcx

    ; Check if past end
    push rcx
    lea rdi, [rbp - FA_STATE]
    call sre_string_len
    pop rcx
    cmp rcx, rax
    ja .fa_done

    jmp .fa_loop

.fa_done:
    lea rdi, [rbp - FA_STATE]
    call sre_state_fini

    mov rax, r14
    mov edx, TAG_PTR

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.fa_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "findall() requires a string argument"
    call raise_exception
END_FUNC sre_pattern_findall_method

; ============================================================================
; sre_pattern_sub_method(self, args, nargs)
; Pattern.sub(repl, string, count=0)
; ============================================================================
SUB_PAT      equ 8
SUB_REPL     equ 16
SUB_STR      equ 24
SUB_COUNT    equ 32
SUB_RESULT   equ 40
SUB_NSUBS    equ 48
SUB_LASTEND  equ 56
SUB_CALLABLE equ 64
SUB_REPL_TAG equ 72
SUB_STATE    equ 72 + SRE_State_size
SUB_FRAME    equ 72 + SRE_State_size

DEF_FUNC sre_pattern_sub_method, SUB_FRAME
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rax, [rdi]             ; pattern = args[0] payload
    mov [rbp - SUB_PAT], rax
    lea rdx, [rsi - 1]         ; user nargs
    cmp rdx, 2
    jb .sub_error

    ; repl = user_args[0], string = user_args[1]
    mov rax, [rdi + 16]       ; args[1] = repl
    mov [rbp - SUB_REPL], rax
    mov rax, [rdi + 32]       ; args[2] = string
    mov [rbp - SUB_STR], rax

    ; Save repl tag for str_concat usage
    mov rcx, [rdi + 24]       ; repl tag = args[1] tag
    mov [rbp - SUB_REPL_TAG], rcx

    ; Check if repl is callable by type whitelist
    ; (tp_call check alone fails because add_builtin_type patches tp_call
    ;  on str_type/int_type/etc. for constructor use, making all instances
    ;  appear callable)
    mov qword [rbp - SUB_CALLABLE], 0
    test rcx, rcx
    js .sub_repl_not_callable  ; SmallStr (bit 63 set)
    cmp ecx, TAG_PTR
    jne .sub_repl_not_callable
    mov rax, [rbp - SUB_REPL]
    mov rax, [rax + PyObject.ob_type]  ; type(repl)
    lea rcx, [rel func_type]
    cmp rax, rcx
    je .sub_is_callable
    lea rcx, [rel method_type]
    cmp rax, rcx
    je .sub_is_callable
    lea rcx, [rel builtin_func_type]
    cmp rax, rcx
    je .sub_is_callable
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .sub_is_callable
    ; Heaptype with tp_call (user class with __call__): check tp_call
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .sub_repl_not_callable
.sub_is_callable:
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .sub_repl_not_callable
    mov [rbp - SUB_CALLABLE], rax
.sub_repl_not_callable:

    ; count = user_args[2] if user_nargs >= 3, else 0
    xor eax, eax
    cmp rdx, 3
    jb .sub_no_count
    mov rax, [rdi + 48]       ; args[3] = count
.sub_no_count:
    mov [rbp - SUB_COUNT], rax
    mov qword [rbp - SUB_NSUBS], 0
    mov qword [rbp - SUB_LASTEND], 0

    ; Create empty result string
    CSTRING rdi, ""
    call str_from_cstr_heap
    mov [rbp - SUB_RESULT], rax

    ; Init state
    lea rdi, [rbp - SUB_STATE]
    mov rsi, [rbp - SUB_PAT]
    mov rdx, [rbp - SUB_STR]
    xor ecx, ecx
    mov r8, 0x7FFFFFFFFFFFFFFF
    call sre_state_init

.sub_loop:
    ; Check count limit
    mov rax, [rbp - SUB_COUNT]
    test rax, rax
    jz .sub_search              ; 0 means unlimited
    cmp [rbp - SUB_NSUBS], rax
    jge .sub_finish

.sub_search:
    lea rdi, [rbp - SUB_STATE]
    mov qword [rdi + SRE_State.marks_size], 0
    mov qword [rdi + SRE_State.lastmark], -1
    mov qword [rdi + SRE_State.lastindex], -1
    mov qword [rdi + SRE_State.repeat_ctx], 0
    call sre_search
    test eax, eax
    jz .sub_finish

    ; Get match start and end
    lea rdi, [rbp - SUB_STATE]
    mov r12, [rdi + SRE_State.str_start]  ; match start (char index)
    mov r13, [rdi + SRE_State.str_pos]    ; match end (char index)

    ; Append unmatched text before this match
    mov rax, [rbp - SUB_LASTEND]
    cmp rax, r12
    jge .sub_no_prefix
    ; Substring from lastend to match_start (Unicode-safe)
    lea rdi, [rbp - SUB_STATE]
    mov rsi, [rbp - SUB_LASTEND]
    mov rdx, r12
    call sre_substr_from_state
    ; Concat with result
    mov rdi, [rbp - SUB_RESULT]
    mov rsi, rax
    mov ecx, TAG_PTR
    push rax                   ; save for DECREF
    call str_concat
    push rax                   ; save new result
    mov rdi, [rbp - SUB_RESULT]
    call obj_decref
    pop rax
    mov [rbp - SUB_RESULT], rax
    pop rdi
    call obj_decref
.sub_no_prefix:

    ; Get replacement: callable or string
    cmp qword [rbp - SUB_CALLABLE], 0
    jnz .sub_callable_repl

    ; String replacement: concat repl directly
    ; Check if repl is SmallStr — must widen to heap for str_concat
    mov rcx, [rbp - SUB_REPL_TAG]
    test rcx, rcx
    js .sub_widen_repl          ; SmallStr (bit 63 set)
    mov rdi, [rbp - SUB_RESULT]
    mov rsi, [rbp - SUB_REPL]
    mov ecx, TAG_PTR
    call str_concat
    jmp .sub_repl_concated

.sub_widen_repl:
    mov rdi, [rbp - SUB_REPL]
    mov rsi, [rbp - SUB_REPL_TAG]
    call smallstr_to_obj
    ; rax = heap string
    push rax                   ; save for DECREF
    mov rdi, [rbp - SUB_RESULT]
    mov rsi, rax
    mov ecx, TAG_PTR
    call str_concat
    push rax                   ; save new result
    mov rdi, [rsp + 8]        ; DECREF widened string
    call obj_decref
    pop rax                    ; restore new result
    add rsp, 8                 ; pop saved widened string

.sub_repl_concated:
    push rax
    mov rdi, [rbp - SUB_RESULT]
    call obj_decref
    pop rax
    mov [rbp - SUB_RESULT], rax
    jmp .sub_after_repl

.sub_callable_repl:
    ; Create match object from current state
    push r12
    push r13
    lea rdi, [rbp - SUB_STATE]
    mov rsi, [rbp - SUB_PAT]
    mov rdx, [rbp - SUB_STR]
    call sre_match_new
    ; rax = match object
    mov r14, rax               ; r14 = match

    ; Build 1-arg fat array on stack: [match_payload, match_tag]
    sub rsp, 16
    mov [rsp], r14             ; payload = match ptr
    mov qword [rsp + 8], TAG_PTR  ; tag = TAG_PTR

    ; Call tp_call(repl, args, 1)
    mov rdi, [rbp - SUB_REPL]
    mov rsi, rsp               ; args ptr
    mov edx, 1                 ; nargs
    mov rax, [rbp - SUB_CALLABLE]
    call rax
    ; rax = replacement string payload, edx = replacement tag
    add rsp, 16               ; pop fat array

    ; Save replacement string
    push rax
    push rdx

    ; DECREF match object
    mov rdi, r14
    call obj_decref

    ; Concat replacement with result
    pop rcx                    ; replacement tag → ecx for str_concat
    pop rsi                    ; replacement string payload
    push rsi                   ; save repl payload for DECREF
    push rcx                   ; save repl tag for DECREF
    mov rdi, [rbp - SUB_RESULT]
    call str_concat
    push rax
    mov rdi, [rbp - SUB_RESULT]
    call obj_decref
    pop rax
    mov [rbp - SUB_RESULT], rax

    ; DECREF replacement string from tp_call
    pop rdx                    ; repl tag
    pop rax                    ; repl payload
    DECREF_VAL rax, rdx

    pop r13
    pop r12

.sub_after_repl:
    ; Update state
    mov [rbp - SUB_LASTEND], r13
    inc qword [rbp - SUB_NSUBS]

    ; Advance past match
    cmp r13, r12
    jne .sub_advance_ok
    inc r13                    ; zero-width match
.sub_advance_ok:
    lea rdi, [rbp - SUB_STATE]
    mov [rdi + SRE_State.str_pos], r13
    mov [rdi + SRE_State.str_start], r13

    ; Check if past end
    push r13
    lea rdi, [rbp - SUB_STATE]
    call sre_string_len
    pop r13
    cmp r13, rax
    ja .sub_finish

    jmp .sub_loop

.sub_finish:
    ; Append remaining text after last match (Unicode-safe)
    lea rdi, [rbp - SUB_STATE]
    call sre_string_len
    mov rcx, rax               ; string length
    mov rax, [rbp - SUB_LASTEND]
    cmp rax, rcx
    jge .sub_done

    lea rdi, [rbp - SUB_STATE]
    mov rsi, [rbp - SUB_LASTEND]
    mov rdx, rcx
    call sre_substr_from_state
    mov rdi, [rbp - SUB_RESULT]
    mov rsi, rax
    mov ecx, TAG_PTR
    push rax
    call str_concat
    push rax
    mov rdi, [rbp - SUB_RESULT]
    call obj_decref
    pop rax
    mov [rbp - SUB_RESULT], rax
    pop rdi
    call obj_decref

.sub_done:
    lea rdi, [rbp - SUB_STATE]
    call sre_state_fini

    mov rax, [rbp - SUB_RESULT]
    mov edx, TAG_PTR

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sub_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "sub() requires repl and string arguments"
    call raise_exception
END_FUNC sre_pattern_sub_method

; ============================================================================
; sre_pattern_subn_method(self, args, nargs)
; Like sub but returns (result, count) tuple.
; Duplicated sub loop with count tracking.
; ============================================================================
SN_PAT      equ 8
SN_REPL     equ 16
SN_STR      equ 24
SN_COUNT    equ 32
SN_RESULT   equ 40
SN_NSUBS    equ 48
SN_LASTEND  equ 56
SN_CALLABLE equ 64
SN_REPL_TAG equ 72
SN_STATE    equ 72 + SRE_State_size
SN_FRAME    equ 72 + SRE_State_size

DEF_FUNC sre_pattern_subn_method, SN_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rax, [rdi]
    mov [rbp - SN_PAT], rax
    lea rdx, [rsi - 1]
    cmp rdx, 2
    jb .subn_error

    mov rax, [rdi + 16]
    mov [rbp - SN_REPL], rax
    mov rax, [rdi + 32]
    mov [rbp - SN_STR], rax

    ; Save repl tag for str_concat usage
    mov rcx, [rdi + 24]        ; repl tag = args[1] tag
    mov [rbp - SN_REPL_TAG], rcx

    ; Check if repl is callable by type whitelist
    mov qword [rbp - SN_CALLABLE], 0
    test rcx, rcx
    js .subn_repl_not_callable  ; SmallStr (bit 63 set)
    cmp ecx, TAG_PTR
    jne .subn_repl_not_callable
    mov rax, [rbp - SN_REPL]
    mov rax, [rax + PyObject.ob_type]  ; type(repl)
    lea rcx, [rel func_type]
    cmp rax, rcx
    je .subn_is_callable
    lea rcx, [rel method_type]
    cmp rax, rcx
    je .subn_is_callable
    lea rcx, [rel builtin_func_type]
    cmp rax, rcx
    je .subn_is_callable
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .subn_is_callable
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .subn_repl_not_callable
.subn_is_callable:
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .subn_repl_not_callable
    mov [rbp - SN_CALLABLE], rax
.subn_repl_not_callable:

    xor eax, eax
    cmp rdx, 3
    jb .subn_no_count
    mov rax, [rdi + 48]
.subn_no_count:
    mov [rbp - SN_COUNT], rax
    mov qword [rbp - SN_NSUBS], 0
    mov qword [rbp - SN_LASTEND], 0

    CSTRING rdi, ""
    call str_from_cstr_heap
    mov [rbp - SN_RESULT], rax

    lea rdi, [rbp - SN_STATE]
    mov rsi, [rbp - SN_PAT]
    mov rdx, [rbp - SN_STR]
    xor ecx, ecx
    mov r8, 0x7FFFFFFFFFFFFFFF
    call sre_state_init

.subn_loop:
    mov rax, [rbp - SN_COUNT]
    test rax, rax
    jz .subn_search
    cmp [rbp - SN_NSUBS], rax
    jge .subn_finish

.subn_search:
    lea rdi, [rbp - SN_STATE]
    mov qword [rdi + SRE_State.marks_size], 0
    mov qword [rdi + SRE_State.lastmark], -1
    mov qword [rdi + SRE_State.lastindex], -1
    mov qword [rdi + SRE_State.repeat_ctx], 0
    call sre_search
    test eax, eax
    jz .subn_finish

    lea rdi, [rbp - SN_STATE]
    mov r12, [rdi + SRE_State.str_start]
    mov r13, [rdi + SRE_State.str_pos]

    mov rax, [rbp - SN_LASTEND]
    cmp rax, r12
    jge .subn_no_prefix
    lea rdi, [rbp - SN_STATE]
    mov rsi, [rbp - SN_LASTEND]
    mov rdx, r12
    call sre_substr_from_state
    mov rdi, [rbp - SN_RESULT]
    mov rsi, rax
    mov ecx, TAG_PTR
    push rax
    call str_concat
    push rax
    mov rdi, [rbp - SN_RESULT]
    call obj_decref
    pop rax
    mov [rbp - SN_RESULT], rax
    pop rdi
    call obj_decref
.subn_no_prefix:

    ; Get replacement: callable or string
    cmp qword [rbp - SN_CALLABLE], 0
    jnz .subn_callable_repl

    ; String replacement: check SmallStr and widen if needed
    mov rcx, [rbp - SN_REPL_TAG]
    test rcx, rcx
    js .subn_widen_repl         ; SmallStr (bit 63 set)
    mov rdi, [rbp - SN_RESULT]
    mov rsi, [rbp - SN_REPL]
    mov ecx, TAG_PTR
    call str_concat
    jmp .subn_repl_concated

.subn_widen_repl:
    mov rdi, [rbp - SN_REPL]
    mov rsi, [rbp - SN_REPL_TAG]
    call smallstr_to_obj
    push rax                   ; save for DECREF
    mov rdi, [rbp - SN_RESULT]
    mov rsi, rax
    mov ecx, TAG_PTR
    call str_concat
    push rax                   ; save new result
    mov rdi, [rsp + 8]        ; DECREF widened string
    call obj_decref
    pop rax                    ; restore new result
    add rsp, 8                 ; pop saved widened string

.subn_repl_concated:
    push rax
    mov rdi, [rbp - SN_RESULT]
    call obj_decref
    pop rax
    mov [rbp - SN_RESULT], rax
    jmp .subn_after_repl

.subn_callable_repl:
    ; Create match object
    push r12
    push r13
    lea rdi, [rbp - SN_STATE]
    mov rsi, [rbp - SN_PAT]
    mov rdx, [rbp - SN_STR]
    call sre_match_new
    mov r14, rax

    ; Build 1-arg fat array on stack
    sub rsp, 16
    mov [rsp], r14
    mov qword [rsp + 8], TAG_PTR

    ; Call tp_call(repl, args, 1)
    mov rdi, [rbp - SN_REPL]
    mov rsi, rsp
    mov edx, 1
    mov rax, [rbp - SN_CALLABLE]
    call rax
    add rsp, 16

    push rax
    push rdx

    ; DECREF match
    mov rdi, r14
    call obj_decref

    ; Concat replacement
    pop rcx                    ; replacement tag
    pop rsi                    ; replacement payload
    push rsi                   ; save repl payload for DECREF
    push rcx                   ; save repl tag for DECREF
    mov rdi, [rbp - SN_RESULT]
    call str_concat
    push rax
    mov rdi, [rbp - SN_RESULT]
    call obj_decref
    pop rax
    mov [rbp - SN_RESULT], rax

    ; DECREF replacement string from tp_call
    pop rdx                    ; repl tag
    pop rax                    ; repl payload
    DECREF_VAL rax, rdx

    pop r13
    pop r12

.subn_after_repl:
    mov [rbp - SN_LASTEND], r13
    inc qword [rbp - SN_NSUBS]

    cmp r13, r12
    jne .subn_advance_ok
    inc r13
.subn_advance_ok:
    lea rdi, [rbp - SN_STATE]
    mov [rdi + SRE_State.str_pos], r13
    mov [rdi + SRE_State.str_start], r13

    push r13
    lea rdi, [rbp - SN_STATE]
    call sre_string_len
    pop r13
    cmp r13, rax
    ja .subn_finish

    jmp .subn_loop

.subn_finish:
    lea rdi, [rbp - SN_STATE]
    call sre_string_len
    mov rcx, rax
    mov rax, [rbp - SN_LASTEND]
    cmp rax, rcx
    jge .subn_build_tuple

    lea rdi, [rbp - SN_STATE]
    mov rsi, [rbp - SN_LASTEND]
    mov rdx, rcx
    call sre_substr_from_state
    mov rdi, [rbp - SN_RESULT]
    mov rsi, rax
    mov ecx, TAG_PTR
    push rax
    call str_concat
    push rax
    mov rdi, [rbp - SN_RESULT]
    call obj_decref
    pop rax
    mov [rbp - SN_RESULT], rax
    pop rdi
    call obj_decref

.subn_build_tuple:
    lea rdi, [rbp - SN_STATE]
    call sre_state_fini

    ; Create (result, nsubs) tuple
    mov edi, 2
    call tuple_new
    mov rbx, rax

    mov r8, [rbx + PyTupleObject.ob_item]       ; payloads
    mov r9, [rbx + PyTupleObject.ob_item_tags]  ; tags

    mov r12, [rbp - SN_RESULT]
    inc qword [r12 + PyObject.ob_refcnt]
    mov [r8], r12
    mov byte [r9], TAG_PTR

    mov rax, [rbp - SN_NSUBS]
    mov [r8 + 8], rax
    mov byte [r9 + 1], TAG_SMALLINT

    ; DECREF result string (tuple holds a ref)
    mov rdi, r12
    call obj_decref

    mov rax, rbx
    mov edx, TAG_PTR

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.subn_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "subn() requires repl and string arguments"
    call raise_exception
END_FUNC sre_pattern_subn_method

; ============================================================================
; sre_pattern_split_method(self, args, nargs)
; Pattern.split(string, maxsplit=0)
; ============================================================================
SP_PAT      equ 8
SP_STR      equ 16
SP_MAX      equ 24
SP_LIST     equ 32
SP_NSPLIT   equ 40
SP_LASTEND  equ 48
SP_STATE    equ 48 + SRE_State_size  ; state base at BOTTOM of frame
SP_FRAME    equ 48 + SRE_State_size

DEF_FUNC sre_pattern_split_method, SP_FRAME
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rax, [rdi]             ; pattern = args[0] payload
    mov [rbp - SP_PAT], rax
    lea rdx, [rsi - 1]         ; user nargs
    cmp rdx, 0
    je .split_error

    mov rax, [rdi + 16]       ; string = args[1] payload
    mov [rbp - SP_STR], rax

    ; maxsplit
    xor eax, eax
    cmp rdx, 2
    jb .split_no_max
    mov rax, [rdi + 32]       ; args[2] = maxsplit
.split_no_max:
    mov [rbp - SP_MAX], rax
    mov qword [rbp - SP_NSPLIT], 0
    mov qword [rbp - SP_LASTEND], 0

    ; Create result list
    xor edi, edi
    call list_new
    mov r14, rax

    ; Get group count from pattern
    mov rax, [rbp - SP_PAT]
    mov r15d, [rax + SRE_PatternObject.groups]

    ; Init state
    lea rdi, [rbp - SP_STATE]
    mov rsi, [rbp - SP_PAT]
    mov rdx, [rbp - SP_STR]
    xor ecx, ecx
    mov r8, 0x7FFFFFFFFFFFFFFF
    call sre_state_init

.split_loop:
    mov rax, [rbp - SP_MAX]
    test rax, rax
    jz .split_search
    cmp [rbp - SP_NSPLIT], rax
    jge .split_finish

.split_search:
    lea rdi, [rbp - SP_STATE]
    mov qword [rdi + SRE_State.marks_size], 0
    mov qword [rdi + SRE_State.lastmark], -1
    mov qword [rdi + SRE_State.lastindex], -1
    mov qword [rdi + SRE_State.repeat_ctx], 0
    call sre_search
    test eax, eax
    jz .split_finish

    lea rdi, [rbp - SP_STATE]
    mov r12, [rdi + SRE_State.str_start]
    mov r13, [rdi + SRE_State.str_pos]

    ; Skip zero-width matches at start
    cmp r13, r12
    jne .split_nonzero
    cmp r12, [rbp - SP_LASTEND]
    je .split_advance_skip
.split_nonzero:

    ; Append text before match using sre_substr_from_state
    lea rdi, [rbp - SP_STATE]
    mov rsi, [rbp - SP_LASTEND]
    mov rdx, r12
    call sre_substr_from_state
    mov rdi, r14
    mov rsi, rax
    ; edx already set
    push rax
    push rdx
    call list_append
    pop rdx
    pop rax
    DECREF_VAL rax, rdx

    ; Append capturing groups 1..N
    test r15d, r15d
    jz .split_no_captures
    mov ebx, 1                 ; group index
.split_cap_loop:
    cmp ebx, r15d
    ja .split_no_captures
    push rbx
    ; Get marks for group ebx: state marks[2*(ebx-1)] and marks[2*(ebx-1)+1]
    lea rdi, [rbp - SP_STATE]
    mov rax, [rdi + SRE_State.marks]
    movsx r8, ebx
    dec r8                     ; group - 1
    shl r8, 1                  ; 2 * (group - 1)
    lea r9, [r8 + 2]
    cmp r9, [rdi + SRE_State.marks_size]
    ja .split_cap_none

    mov rsi, [rax + r8*8]     ; start
    mov rdx, [rax + r8*8 + 8] ; end
    lea rdi, [rbp - SP_STATE]
    call sre_substr_from_state
    jmp .split_cap_append

.split_cap_none:
    xor eax, eax
    mov edx, TAG_NONE

.split_cap_append:
    mov rdi, r14
    mov rsi, rax
    ; edx already set
    push rax
    push rdx
    call list_append
    pop rdx
    pop rax
    DECREF_VAL rax, rdx

    pop rbx
    inc ebx
    jmp .split_cap_loop

.split_no_captures:
    mov [rbp - SP_LASTEND], r13
    inc qword [rbp - SP_NSPLIT]

.split_advance_skip:
    ; Advance
    cmp r13, r12
    jne .split_adv_ok
    inc r13
.split_adv_ok:
    lea rdi, [rbp - SP_STATE]
    mov [rdi + SRE_State.str_pos], r13
    mov [rdi + SRE_State.str_start], r13

    push r13
    lea rdi, [rbp - SP_STATE]
    call sre_string_len
    pop r13
    cmp r13, rax
    ja .split_finish
    jmp .split_loop

.split_finish:
    ; Append remaining text using sre_substr_from_state
    lea rdi, [rbp - SP_STATE]
    call sre_string_len
    mov rcx, rax               ; string length
    mov rsi, [rbp - SP_LASTEND]
    lea rdi, [rbp - SP_STATE]
    mov rdx, rcx
    call sre_substr_from_state
    mov rdi, r14
    mov rsi, rax
    ; edx already set
    push rax
    push rdx
    call list_append
    pop rdx
    pop rax
    DECREF_VAL rax, rdx

    lea rdi, [rbp - SP_STATE]
    call sre_state_fini

    mov rax, r14
    mov edx, TAG_PTR

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.split_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "split() requires a string argument"
    call raise_exception
END_FUNC sre_pattern_split_method

; ============================================================================
; sre_pattern_getattr(PyObject* self, PyObject* name) -> PyObject*
; ============================================================================
DEF_FUNC sre_pattern_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self (pattern)
    mov r12, rsi               ; name string

    ; Get name as C string
    lea rdi, [r12 + PyStrObject.data]

    ; Check known attributes
    lea rsi, [rel sp_match_str]
    call sre_strcmp
    test eax, eax
    jz .ga_match

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_search_str]
    call sre_strcmp
    test eax, eax
    jz .ga_search

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_fullmatch_str]
    call sre_strcmp
    test eax, eax
    jz .ga_fullmatch

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_findall_str]
    call sre_strcmp
    test eax, eax
    jz .ga_findall

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_sub_str]
    call sre_strcmp
    test eax, eax
    jz .ga_sub

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_subn_str]
    call sre_strcmp
    test eax, eax
    jz .ga_subn

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_split_str]
    call sre_strcmp
    test eax, eax
    jz .ga_split

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_pattern_str]
    call sre_strcmp
    test eax, eax
    jz .ga_pattern_attr

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_flags_str]
    call sre_strcmp
    test eax, eax
    jz .ga_flags

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_groups_str]
    call sre_strcmp
    test eax, eax
    jz .ga_groups

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_groupindex_str]
    call sre_strcmp
    test eax, eax
    jz .ga_groupindex

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_finditer_str]
    call sre_strcmp
    test eax, eax
    jz .ga_finditer

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_scanner_str]
    call sre_strcmp
    test eax, eax
    jz .ga_scanner

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_copy_str]
    call sre_strcmp
    test eax, eax
    jz .ga_copy

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_deepcopy_str]
    call sre_strcmp
    test eax, eax
    jz .ga_copy

    ; Not found
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.ga_match:
    lea rdi, [rel sre_pattern_match_method]
    lea rsi, [rel sp_match_str]
    call builtin_func_new
    jmp .ga_bind
.ga_search:
    lea rdi, [rel sre_pattern_search_method]
    lea rsi, [rel sp_search_str]
    call builtin_func_new
    jmp .ga_bind
.ga_fullmatch:
    lea rdi, [rel sre_pattern_fullmatch_method]
    lea rsi, [rel sp_fullmatch_str]
    call builtin_func_new
    jmp .ga_bind
.ga_findall:
    lea rdi, [rel sre_pattern_findall_method]
    lea rsi, [rel sp_findall_str]
    call builtin_func_new
    jmp .ga_bind
.ga_sub:
    lea rdi, [rel sre_pattern_sub_method]
    lea rsi, [rel sp_sub_str]
    call builtin_func_new
    jmp .ga_bind
.ga_subn:
    lea rdi, [rel sre_pattern_subn_method]
    lea rsi, [rel sp_subn_str]
    call builtin_func_new
    jmp .ga_bind
.ga_split:
    lea rdi, [rel sre_pattern_split_method]
    lea rsi, [rel sp_split_str]
    call builtin_func_new
    jmp .ga_bind
.ga_finditer:
    lea rdi, [rel sre_pattern_finditer_method]
    lea rsi, [rel sp_finditer_str]
    call builtin_func_new
    jmp .ga_bind
.ga_scanner:
    lea rdi, [rel sre_pattern_finditer_method]
    lea rsi, [rel sp_scanner_str]
    call builtin_func_new
    jmp .ga_bind
.ga_copy:
    lea rdi, [rel sre_pattern_copy_method]
    lea rsi, [rel sp_copy_str]
    call builtin_func_new
    jmp .ga_bind

.ga_bind:
    ; rax = builtin func from builtin_func_new
    push rax                    ; save func
    mov rdi, rax                ; func
    mov rsi, rbx                ; self (pattern)
    call method_new
    push rax
    mov rdi, [rsp + 8]         ; DECREF unbound func
    call obj_decref
    pop rax
    add rsp, 8
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ga_pattern_attr:
    ; Return pattern.pattern (the regex string)
    mov rax, [rbx + SRE_PatternObject.pattern]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ga_flags:
    mov rax, [rbx + SRE_PatternObject.flags]
    movsx rax, eax
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret

.ga_groups:
    mov eax, [rbx + SRE_PatternObject.groups]
    movsx rax, eax
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret

.ga_groupindex:
    mov rax, [rbx + SRE_PatternObject.groupindex]
    test rax, rax
    jz .ga_empty_dict
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
.ga_empty_dict:
    call dict_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_pattern_getattr

; ============================================================================
; Helper: strcmp for attribute lookup
; ============================================================================
global sre_strcmp
DEF_FUNC sre_strcmp
    ; rdi = str1, rsi = str2
.cmp_loop:
    movzx eax, byte [rdi]
    movzx ecx, byte [rsi]
    cmp al, cl
    jne .cmp_ne
    test al, al
    jz .cmp_eq
    inc rdi
    inc rsi
    jmp .cmp_loop
.cmp_eq:
    xor eax, eax
    leave
    ret
.cmp_ne:
    mov eax, 1
    leave
    ret
END_FUNC sre_strcmp

; ============================================================================
; sre_pattern_copy_method(self, args, nargs)
; __copy__ / __deepcopy__ — patterns are effectively immutable.
; Returns self (INCREF'd).
; ============================================================================
DEF_FUNC sre_pattern_copy_method
    mov rax, [rdi]             ; self payload
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC sre_pattern_copy_method

; ============================================================================
; sre_pattern_hash(PyObject* self) -> i64
; hash(pattern_str) XOR flags XOR (groups << 16)
; ============================================================================
extern str_hash

DEF_FUNC sre_pattern_hash
    push rbx
    mov rbx, rdi               ; self

    ; hash(pattern_str)
    mov rdi, [rbx + SRE_PatternObject.pattern]
    call str_hash

    ; XOR flags
    mov ecx, [rbx + SRE_PatternObject.flags]
    movsx rcx, ecx
    xor rax, rcx

    ; XOR (groups << 16)
    mov ecx, [rbx + SRE_PatternObject.groups]
    movsx rcx, ecx
    shl rcx, 16
    xor rax, rcx

    pop rbx
    leave
    ret
END_FUNC sre_pattern_hash

; ============================================================================
; sre_pattern_richcompare(rdi=left, rsi=right, edx=op, rcx=left_tag, r8=right_tag)
;   -> (rax=payload, edx=tag)
; Supports PY_EQ and PY_NE only. Compares type, flags, groups, pattern string.
; ============================================================================
extern str_compare

DEF_FUNC sre_pattern_richcompare
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; left
    mov r12, rsi               ; right
    mov r13d, edx              ; op

    ; Only support PY_EQ (2) and PY_NE (3)
    cmp r13d, PY_EQ
    je .prc_compare
    cmp r13d, PY_NE
    je .prc_compare

    ; Return NotImplemented → for now return False
    xor eax, eax
    mov edx, TAG_NONE
    jmp .prc_ret

.prc_compare:
    ; Check right is also a Pattern
    cmp r8d, TAG_PTR
    jne .prc_ne_result
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel sre_pattern_type]
    cmp rax, rcx
    jne .prc_ne_result

    ; Compare flags
    mov eax, [rbx + SRE_PatternObject.flags]
    cmp eax, [r12 + SRE_PatternObject.flags]
    jne .prc_ne_result

    ; Compare groups
    mov eax, [rbx + SRE_PatternObject.groups]
    cmp eax, [r12 + SRE_PatternObject.groups]
    jne .prc_ne_result

    ; Compare pattern strings via str_compare
    mov rdi, [rbx + SRE_PatternObject.pattern]
    mov rsi, [r12 + SRE_PatternObject.pattern]
    mov edx, PY_EQ
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call str_compare
    ; rax = payload (bool_true/bool_false ptr), edx = tag (TAG_PTR)
    ; Check if str_compare returned bool_true
    lea rcx, [rel bool_true]
    cmp rax, rcx
    jne .prc_ne_result

    ; Patterns are equal
    cmp r13d, PY_EQ
    je .prc_true
    jmp .prc_false

.prc_ne_result:
    ; Patterns are not equal
    cmp r13d, PY_NE
    je .prc_true
    jmp .prc_false

.prc_true:
    mov eax, 1
    mov edx, TAG_BOOL
    jmp .prc_ret
.prc_false:
    xor eax, eax
    mov edx, TAG_BOOL
.prc_ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_pattern_richcompare

; ============================================================================
; sre_pattern_finditer_method(self, args, nargs)
; Pattern.finditer(string, pos=0, endpos=sys.maxsize)
; Returns an SRE_Scanner iterator.
; ============================================================================
DEF_FUNC sre_pattern_finditer_method
    ; rdi = args (fat array), rsi = nargs
    push rbx
    push r12
    push r13

    mov rbx, [rdi]             ; self = pattern
    lea r12, [rdi + 16]        ; user args
    lea r13, [rsi - 1]         ; user nargs

    cmp r13, 0
    je .fi_error

    ; string = user_args[0]
    mov rsi, [r12]             ; string payload

    ; pos (default 0)
    xor edx, edx
    cmp r13, 2
    jb .fi_no_pos
    mov rdx, [r12 + 16]       ; pos
.fi_no_pos:

    ; endpos (default large)
    mov rcx, 0x7FFFFFFFFFFFFFFF
    cmp r13, 3
    jb .fi_no_endpos
    mov rcx, [r12 + 32]       ; endpos
.fi_no_endpos:

    ; scanner_new(pattern, string, pos, endpos)
    mov rdi, rbx               ; pattern
    ; rsi = string (already set)
    ; rdx = pos
    ; rcx = endpos
    call sre_scanner_new

    mov edx, TAG_PTR

    pop r13
    pop r12
    pop rbx
    leave
    ret

.fi_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "finditer() requires a string argument"
    call raise_exception
END_FUNC sre_pattern_finditer_method

; ============================================================================
; SRE_Scanner implementation
; ============================================================================

; ============================================================================
; sre_scanner_new(rdi=pattern, rsi=string, rdx=pos, rcx=endpos) -> rax
; Allocates and initializes an SRE_ScannerObject.
; ============================================================================
DEF_FUNC sre_scanner_new
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi               ; pattern
    mov r13, rsi               ; string
    mov r14, rdx               ; pos
    mov r15, rcx               ; endpos

    mov edi, SRE_ScannerObject_size
    call ap_malloc
    mov rbx, rax

    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel sre_scanner_type]
    mov [rbx + PyObject.ob_type], rax

    mov [rbx + SRE_ScannerObject.pattern], r12
    inc qword [r12 + PyObject.ob_refcnt]

    mov [rbx + SRE_ScannerObject.string], r13
    inc qword [r13 + PyObject.ob_refcnt]

    mov [rbx + SRE_ScannerObject.pos], r14
    mov [rbx + SRE_ScannerObject.endpos], r15

    mov rax, rbx

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_scanner_new

; ============================================================================
; sre_scanner_dealloc(rdi=self)
; ============================================================================
DEF_FUNC sre_scanner_dealloc
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + SRE_ScannerObject.pattern]
    call obj_decref

    mov rdi, [rbx + SRE_ScannerObject.string]
    call obj_decref

    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC sre_scanner_dealloc

; ============================================================================
; sre_scanner_iter_self(rdi=self) -> rax (no edx, tp_iter convention)
; ============================================================================
DEF_FUNC_BARE sre_scanner_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC sre_scanner_iter_self

; ============================================================================
; sre_scanner_iternext(rdi=self) -> (rax, edx) fat value
; Returns next match via sre_search, or NULL on exhaustion.
; ============================================================================
SI_SELF    equ 8
SI_STATE   equ 8 + SRE_State_size
SI_FRAME   equ 8 + SRE_State_size

DEF_FUNC sre_scanner_iternext, SI_FRAME
    push rbx
    push r12
    push r13

    mov [rbp - SI_SELF], rdi
    mov rbx, rdi

    ; Check if past end
    mov rax, [rbx + SRE_ScannerObject.pos]
    cmp rax, [rbx + SRE_ScannerObject.endpos]
    ja .si_exhausted

    ; Init state
    lea rdi, [rbp - SI_STATE]
    mov rsi, [rbx + SRE_ScannerObject.pattern]
    mov rdx, [rbx + SRE_ScannerObject.string]
    mov rcx, [rbx + SRE_ScannerObject.pos]
    mov r8, [rbx + SRE_ScannerObject.endpos]
    call sre_state_init

    ; Reset state marks
    lea rdi, [rbp - SI_STATE]
    mov qword [rdi + SRE_State.marks_size], 0
    mov qword [rdi + SRE_State.lastmark], -1
    mov qword [rdi + SRE_State.lastindex], -1
    mov qword [rdi + SRE_State.repeat_ctx], 0

    ; Search
    lea rdi, [rbp - SI_STATE]
    call sre_search
    test eax, eax
    jz .si_no_match

    ; Get match boundaries
    lea rdi, [rbp - SI_STATE]
    mov r12, [rdi + SRE_State.str_start]   ; match_start
    mov r13, [rdi + SRE_State.str_pos]     ; match_end

    ; Create match object
    lea rdi, [rbp - SI_STATE]
    mov rbx, [rbp - SI_SELF]
    mov rsi, [rbx + SRE_ScannerObject.pattern]
    mov rdx, [rbx + SRE_ScannerObject.string]
    call sre_match_new
    push rax                   ; save match object

    ; Clean up state
    lea rdi, [rbp - SI_STATE]
    call sre_state_fini

    ; Advance scanner position: max(match_end, match_start + 1)
    mov rbx, [rbp - SI_SELF]
    mov rax, r13               ; match_end
    cmp rax, r12
    jne .si_advance_ok
    lea rax, [r12 + 1]        ; zero-width guard
.si_advance_ok:
    mov [rbx + SRE_ScannerObject.pos], rax

    pop rax                    ; match object
    mov edx, TAG_PTR

    pop r13
    pop r12
    pop rbx
    leave
    ret

.si_no_match:
    lea rdi, [rbp - SI_STATE]
    call sre_state_fini

.si_exhausted:
    RET_NULL

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_scanner_iternext

; ============================================================================
; sre_scanner_search_method(self, args, nargs)
; scanner.search() — same as iternext but returns None instead of NULL
; ============================================================================
DEF_FUNC sre_scanner_search_method
    mov rdi, [rdi]             ; self = args[0] payload
    call sre_scanner_iternext
    ; If NULL (tag=0), return None
    test edx, edx
    jnz .ssm_done
    xor eax, eax
    mov edx, TAG_NONE
.ssm_done:
    leave
    ret
END_FUNC sre_scanner_search_method

; ============================================================================
; sre_scanner_match_method(self, args, nargs)
; scanner.match() — anchored match at current pos, returns match or None
; ============================================================================
SM2_SELF   equ 8
SM2_STATE  equ 8 + SRE_State_size
SM2_FRAME  equ 8 + SRE_State_size

DEF_FUNC sre_scanner_match_method, SM2_FRAME
    push rbx
    push r12
    push r13

    mov rbx, [rdi]             ; self = args[0] payload
    mov [rbp - SM2_SELF], rbx

    ; Check if past end
    mov rax, [rbx + SRE_ScannerObject.pos]
    cmp rax, [rbx + SRE_ScannerObject.endpos]
    ja .sm2_none

    ; Init state
    lea rdi, [rbp - SM2_STATE]
    mov rsi, [rbx + SRE_ScannerObject.pattern]
    mov rdx, [rbx + SRE_ScannerObject.string]
    mov rcx, [rbx + SRE_ScannerObject.pos]
    mov r8, [rbx + SRE_ScannerObject.endpos]
    call sre_state_init

    ; Reset marks
    lea rdi, [rbp - SM2_STATE]
    mov qword [rdi + SRE_State.marks_size], 0
    mov qword [rdi + SRE_State.lastmark], -1
    mov qword [rdi + SRE_State.lastindex], -1
    mov qword [rdi + SRE_State.repeat_ctx], 0

    ; Anchored match (not search)
    lea rdi, [rbp - SM2_STATE]
    mov rbx, [rbp - SM2_SELF]
    mov rsi, [rbx + SRE_ScannerObject.pattern]
    mov rsi, [rsi + SRE_PatternObject.code]
    call sre_match
    test eax, eax
    jz .sm2_no_match

    ; Get match boundaries
    lea rdi, [rbp - SM2_STATE]
    mov r12, [rdi + SRE_State.str_start]
    mov r13, [rdi + SRE_State.str_pos]

    ; Create match object
    lea rdi, [rbp - SM2_STATE]
    mov rbx, [rbp - SM2_SELF]
    mov rsi, [rbx + SRE_ScannerObject.pattern]
    mov rdx, [rbx + SRE_ScannerObject.string]
    call sre_match_new
    push rax

    lea rdi, [rbp - SM2_STATE]
    call sre_state_fini

    ; Advance scanner pos
    mov rbx, [rbp - SM2_SELF]
    mov rax, r13
    cmp rax, r12
    jne .sm2_adv_ok
    lea rax, [r12 + 1]
.sm2_adv_ok:
    mov [rbx + SRE_ScannerObject.pos], rax

    pop rax
    mov edx, TAG_PTR

    pop r13
    pop r12
    pop rbx
    leave
    ret

.sm2_no_match:
    lea rdi, [rbp - SM2_STATE]
    call sre_state_fini

.sm2_none:
    xor eax, eax
    mov edx, TAG_NONE

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_scanner_match_method

; ============================================================================
; sre_scanner_getattr(rdi=self, rsi=name) -> (rax, edx)
; ============================================================================
DEF_FUNC sre_scanner_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self
    mov r12, rsi               ; name

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sc_match_str]
    call sre_strcmp
    test eax, eax
    jz .sga_match

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sc_search_str]
    call sre_strcmp
    test eax, eax
    jz .sga_search

    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel sp_pattern_str]
    call sre_strcmp
    test eax, eax
    jz .sga_pattern

    ; Not found
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.sga_match:
    lea rdi, [rel sre_scanner_match_method]
    lea rsi, [rel sc_match_str]
    call builtin_func_new
    jmp .sga_bind
.sga_search:
    lea rdi, [rel sre_scanner_search_method]
    lea rsi, [rel sc_search_str]
    call builtin_func_new
    jmp .sga_bind

.sga_bind:
    push rax
    mov rdi, rax
    mov rsi, rbx
    call method_new
    push rax
    mov rdi, [rsp + 8]
    call obj_decref
    pop rax
    add rsp, 8
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.sga_pattern:
    mov rax, [rbx + SRE_ScannerObject.pattern]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_scanner_getattr

; ============================================================================
; Type definitions
; ============================================================================
section .data
align 8

; SRE_Scanner type
global sre_scanner_type
sre_scanner_type:
    dq 1                       ; ob_refcnt (immortal)
    dq type_type               ; ob_type
    dq sc_type_name            ; tp_name
    dq SRE_ScannerObject_size  ; tp_basicsize
    dq sre_scanner_dealloc     ; tp_dealloc
    dq 0                       ; tp_repr
    dq 0                       ; tp_str
    dq 0                       ; tp_hash
    dq 0                       ; tp_call
    dq sre_scanner_getattr     ; tp_getattr
    dq 0                       ; tp_setattr
    dq 0                       ; tp_richcompare
    dq sre_scanner_iter_self   ; tp_iter
    dq sre_scanner_iternext    ; tp_iternext
    dq 0                       ; tp_init
    dq 0                       ; tp_new
    dq 0                       ; tp_as_number
    dq 0                       ; tp_as_sequence
    dq 0                       ; tp_as_mapping
    dq 0                       ; tp_base
    dq 0                       ; tp_dict
    dq 0                       ; tp_mro
    dq 0                       ; tp_flags
    dq 0                       ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear

align 8

; SRE_Pattern type
global sre_pattern_type
sre_pattern_type:
    dq 1                       ; ob_refcnt (immortal)
    dq type_type               ; ob_type
    dq sp_type_name            ; tp_name
    dq SRE_PatternObject_size  ; tp_basicsize
    dq sre_pattern_dealloc     ; tp_dealloc
    dq sre_pattern_repr        ; tp_repr
    dq sre_pattern_repr        ; tp_str
    dq sre_pattern_hash        ; tp_hash
    dq 0                       ; tp_call (patterns aren't directly callable)
    dq sre_pattern_getattr     ; tp_getattr
    dq 0                       ; tp_setattr
    dq sre_pattern_richcompare ; tp_richcompare
    dq 0                       ; tp_iter
    dq 0                       ; tp_iternext
    dq 0                       ; tp_init
    dq 0                       ; tp_new
    dq 0                       ; tp_as_number
    dq 0                       ; tp_as_sequence
    dq 0                       ; tp_as_mapping
    dq 0                       ; tp_base
    dq 0                       ; tp_dict
    dq 0                       ; tp_mro
    dq 0                       ; tp_flags
    dq 0                       ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear

section .rodata
sp_type_name:     db "re.Pattern", 0
sp_match_str:     db "match", 0
sp_search_str:    db "search", 0
sp_fullmatch_str: db "fullmatch", 0
sp_findall_str:   db "findall", 0
sp_finditer_str:  db "finditer", 0
sp_sub_str:       db "sub", 0
sp_subn_str:      db "subn", 0
sp_split_str:     db "split", 0
sp_pattern_str:   db "pattern", 0
sp_flags_str:     db "flags", 0
sp_groups_str:    db "groups", 0
sp_groupindex_str: db "groupindex", 0
sp_scanner_str:    db "scanner", 0
sp_copy_str:       db "__copy__", 0
sp_deepcopy_str:   db "__deepcopy__", 0

; Scanner type rodata
sc_type_name:      db "re.Scanner", 0
sc_match_str:      db "match", 0
sc_search_str:     db "search", 0
