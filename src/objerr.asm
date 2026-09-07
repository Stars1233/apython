; objerr.asm - the arity and receiver error messages.
;
; Split out of object.asm, which reached the 100k cap that src/compiler/lint.py
; enforces.  The seam is the one object.asm already had: these six build a
; message into a stack buffer and hand it to raise_exception, and none of them
; returns.  They share nothing with the object protocol above them but the two
; append helpers, and every one of them exists for the same reason -- CPython
; reports the count and the receiver it actually got, and this tree used to
; report the rule and leave the caller to guess what it had passed.
;
; The remaining raise_* helpers stay in object.asm: they are interleaved with
; value_type, dunder_require_self and the msg_append_* primitives they are
; built on, and cutting between them would mean moving those too.

%include "macros.inc"
%include "object.inc"

; --- the message primitives, which stay in object.asm ---
extern msg_append_i64
extern rbt_append_cstr

; --- what the messages describe ---
extern value_type
extern ap_strcmp

; --- raising ---
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type

section .text


;; ============================================================================
;; raise_type_error_counted(rdi = the text before the number, rsi = the count,
;;                          rdx = the text after it, or 0)
;;   -> does not return: the composed message is raised as a TypeError
;;
;; "str() takes at most 3 arguments (4 given)".  CPython reports the count in
;; every arity message and this tree reported it in almost none, so a caller
;; was told the rule but not what it had actually passed.
;; ============================================================================
RTC_N     equ 8
RTC_TAIL  equ 16
RTC_BUF   equ 192
RTC_FRAME equ 192           ; + 0 pushes = 192, 16-aligned
global raise_type_error_counted
DEF_FUNC raise_type_error_counted, RTC_FRAME
    mov [rbp - RTC_N], rsi
    mov [rbp - RTC_TAIL], rdx
    mov rsi, rdi
    lea rdi, [rbp - RTC_BUF]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RTC_N]
    call msg_append_i64
    cmp qword [rbp - RTC_TAIL], 0
    je .rtc_raise
    mov rdi, rax
    mov rsi, [rbp - RTC_TAIL]
    call rbt_append_cstr
.rtc_raise:
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rbp - RTC_BUF]
    call raise_exception
END_FUNC raise_type_error_counted

;; ============================================================================
;; raise_value_error_counted(rdi = the text before the number, rsi = the
;;                           count, rdx = the text after it, or 0)
;;   -> does not return: the composed message is raised as a ValueError
;;
;; The same composition as raise_type_error_counted, for the messages that
;; are ValueErrors: "Item 0 of second argument (exceptions) is not an
;; exception".
;; ============================================================================
global raise_value_error_counted
DEF_FUNC raise_value_error_counted, RTC_FRAME
    mov [rbp - RTC_N], rsi
    mov [rbp - RTC_TAIL], rdx
    mov rsi, rdi
    lea rdi, [rbp - RTC_BUF]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RTC_N]
    call msg_append_i64
    cmp qword [rbp - RTC_TAIL], 0
    je .rvc_raise
    mov rdi, rax
    mov rsi, [rbp - RTC_TAIL]
    call rbt_append_cstr
.rvc_raise:
    extern exc_ValueError_type
    lea rdi, [rel exc_ValueError_type]
    lea rsi, [rbp - RTC_BUF]
    call raise_exception
END_FUNC raise_value_error_counted

;; ============================================================================
;; raise_final_base(rdi = the type's name, as a C string)
;;   -> does not return: the message is raised as a TypeError
;;
;; "type 'bool' is not an acceptable base type", for a type CPython gives no
;; Py_TPFLAGS_BASETYPE.
;; ============================================================================
RFB_BUF   equ 176
RFB_FRAME equ 176           ; + 0 pushes = 176, 16-aligned
global raise_final_base
DEF_FUNC raise_final_base, RFB_FRAME
    mov rdx, rdi
    lea rdi, [rbp - RFB_BUF]
    CSTRING rsi, "type '"
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, rdx
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, "' is not an acceptable base type"
    call rbt_append_cstr
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rbp - RFB_BUF]
    call raise_exception
END_FUNC raise_final_base

;; ============================================================================
;; raise_descriptor_receiver(rdi = the PyBuiltinObject, rsi = the receiver
;;                           Value)
;;   -> does not return: the message is raised as a TypeError
;;
;; CPython has two wordings here, and which one you get says what kind of
;; descriptor you reached:
;;   descriptor 'append' for 'list' objects doesn't apply to a 'tuple' object
;;   descriptor '__neg__' requires a 'int' object but received a 'float'
;; The first is a method descriptor, the second a slot wrapper.  func_kind
;; already records which, for the repr; this is the second reader of it.
;; ============================================================================
RDR_DESC  equ 8
RDR_RECV  equ 16
RDR_BUF   equ 240
RDR_FRAME equ 240           ; + 0 pushes = 240, 16-aligned
global raise_descriptor_receiver
DEF_FUNC raise_descriptor_receiver, RDR_FRAME
    mov [rbp - RDR_DESC], rdi
    mov [rbp - RDR_RECV], rsi
    lea rdi, [rbp - RDR_BUF]
    CSTRING rsi, "descriptor '"
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RDR_DESC]
    mov rsi, [rsi + PyBuiltinObject.func_name]
    lea rsi, [rsi + PyStrObject.data]
    call rbt_append_cstr
    mov rdi, rax

    mov rcx, [rbp - RDR_DESC]
    cmp qword [rcx + PyBuiltinObject.func_kind], BUILTIN_KIND_WRAPPER
    je .rdr_wrapper

    CSTRING rsi, "' for '"
    call rbt_append_cstr
    mov rdi, rax
    call .rdr_owner_name
    mov rdi, rax
    CSTRING rsi, "' objects doesn't apply to a '"
    call rbt_append_cstr
    mov rdi, rax
    call .rdr_recv_name
    mov rdi, rax
    CSTRING rsi, "' object"
    call rbt_append_cstr
    jmp .rdr_raise

.rdr_wrapper:
    CSTRING rsi, "' requires a '"
    call rbt_append_cstr
    mov rdi, rax
    call .rdr_owner_name
    mov rdi, rax
    CSTRING rsi, "' object but received a '"
    call rbt_append_cstr
    mov rdi, rax
    call .rdr_recv_name
    mov rdi, rax
    CSTRING rsi, "'"
    call rbt_append_cstr

.rdr_raise:
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rbp - RDR_BUF]
    call raise_exception

;; The two names, appended at the cursor in rdi.
.rdr_owner_name:
    mov rcx, [rbp - RDR_DESC]
    mov rcx, [rcx + PyBuiltinObject.func_owner]
    mov rsi, [rcx + PyTypeObject.tp_name]
    jmp rbt_append_cstr
.rdr_recv_name:
    push rdi
    mov rdi, [rbp - RDR_RECV]
    call value_type
    test rax, rax
    jz .rdr_recv_unknown
    mov rsi, [rax + PyTypeObject.tp_name]
    jmp .rdr_recv_go
.rdr_recv_unknown:
    CSTRING rsi, "object"
.rdr_recv_go:
    pop rdi
    jmp rbt_append_cstr
END_FUNC raise_descriptor_receiver

;; ============================================================================
;; raise_wrapper_arity(rdi = the number of arguments wanted, not counting
;;                     self; rsi = the number given, likewise)
;;   -> does not return: the message is raised as a TypeError
;;
;; "expected 0 arguments, got 1" -- CPython's wording for a slot wrapper.
;; Every one of these said "expected exactly one argument", which is neither
;; the count nor, for the nullary ones, even the right number.
;; ============================================================================
RWA_WANT  equ 8
RWA_GOT   equ 16
RWA_BUF   equ 192
RWA_FRAME equ 192           ; + 0 pushes = 192, 16-aligned
global raise_wrapper_arity
DEF_FUNC raise_wrapper_arity, RWA_FRAME
    mov [rbp - RWA_WANT], rdi
    mov [rbp - RWA_GOT], rsi
    lea rdi, [rbp - RWA_BUF]
    ; CPython has two helpers here and they differ by one space.  Most
    ; wrappers use check_num_args -- "expected 1 argument, got 0" -- while
    ; the __setitem__/__delitem__ shape goes through PyArg_UnpackTuple with
    ; an EMPTY function name, and that format leaves the gap where the name
    ; would have been.
    test edx, edx
    jz .rwa_no_gap
    CSTRING rsi, " expected "
    jmp .rwa_opened
.rwa_no_gap:
    CSTRING rsi, "expected "
.rwa_opened:
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RWA_WANT]
    call msg_append_i64
    mov rdi, rax
    cmp qword [rbp - RWA_WANT], 1
    je .rwa_singular
    CSTRING rsi, " arguments, got "
    jmp .rwa_join
.rwa_singular:
    CSTRING rsi, " argument, got "
.rwa_join:
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RWA_GOT]
    call msg_append_i64
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rbp - RWA_BUF]
    call raise_exception
END_FUNC raise_wrapper_arity

;; ============================================================================
;; raise_builtin_arity(rdi = the PyBuiltinObject, rsi = the count given,
;;                     rdx = the count wanted, ecx = 0 too few / 1 too many)
;;   -- neither count includes self
;;   -> does not return: the message is raised as a TypeError
;;
;; CPython's shapes, which differ by whether the method takes a fixed number:
;;   list.append() takes exactly one argument (2 given)
;;   str.upper() takes no arguments (1 given)
;;   hex() takes at most 2 arguments (3 given)
;;   endswith() takes at least 1 argument (0 given)
;;   expected 0 arguments, got 1            <- a slot wrapper
;;
;; A method with a range says "at most" when it was given too many and "at
;; least" when too few; reading the direction off min != max alone reported
;; "at most 1 arguments (0 given)", which is both wrong and self-contradictory.
;; ============================================================================
RBA_DESC  equ 8
RBA_GOT   equ 16
RBA_WANT  equ 24
RBA_OVER  equ 32            ; 1 = too many, 0 = too few
RBA_BUF   equ 240
RBA_FRAME equ 240           ; + 0 pushes = 240, 16-aligned
global raise_builtin_arity
DEF_FUNC raise_builtin_arity, RBA_FRAME
    mov [rbp - RBA_DESC], rdi
    mov [rbp - RBA_GOT], rsi
    mov [rbp - RBA_WANT], rdx
    mov [rbp - RBA_OVER], rcx

    ; A slot wrapper has its own wording, and never names itself.
    cmp qword [rdi + PyBuiltinObject.func_kind], BUILTIN_KIND_WRAPPER
    jne .rba_method
    ; ...except that the two-argument item wrappers go through CPython's
    ; OTHER helper, whose format leaves a gap for the name it was given
    ; empty.  __setitem__ and __delitem__ are the pair.
    mov rsi, [rdi + PyBuiltinObject.func_name]
    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__setitem__"
    push rdx
    call ap_strcmp
    pop rdx
    test eax, eax
    jz .rba_wrapper_gap
    mov rcx, [rbp - RBA_DESC]
    mov rsi, [rcx + PyBuiltinObject.func_name]
    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "__delitem__"
    push rdx
    call ap_strcmp
    pop rdx
    test eax, eax
    jz .rba_wrapper_gap
    xor edx, edx
    jmp .rba_wrapper
.rba_wrapper_gap:
    mov edx, 1
.rba_wrapper:
    mov rdi, [rbp - RBA_WANT]
    mov rsi, [rbp - RBA_GOT]
    jmp raise_wrapper_arity

.rba_method:
    lea rdi, [rbp - RBA_BUF]
    mov rcx, [rbp - RBA_DESC]
    cmp qword [rcx + PyBuiltinObject.func_owner], 0
    je .rba_bare_name
    mov rsi, [rcx + PyBuiltinObject.func_owner]
    mov rsi, [rsi + PyTypeObject.tp_name]
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, "."
    call rbt_append_cstr
    mov rdi, rax
.rba_bare_name:
    mov rcx, [rbp - RBA_DESC]
    mov rsi, [rcx + PyBuiltinObject.func_name]
    lea rsi, [rsi + PyStrObject.data]
    call rbt_append_cstr
    mov rdi, rax

    ; A fixed arity names the number; a range says "at most".
    mov rcx, [rbp - RBA_DESC]
    mov rax, [rcx + PyBuiltinObject.min_args]
    cmp rax, [rcx + PyBuiltinObject.max_args]
    jne .rba_range
    cmp qword [rbp - RBA_WANT], 0
    je .rba_none
    cmp qword [rbp - RBA_WANT], 1
    je .rba_one
    CSTRING rsi, "() takes exactly "
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RBA_WANT]
    call msg_append_i64
    mov rdi, rax
    CSTRING rsi, " arguments ("
    jmp .rba_tail
.rba_one:
    CSTRING rsi, "() takes exactly one argument ("
    jmp .rba_tail
.rba_none:
    CSTRING rsi, "() takes no arguments ("
    jmp .rba_tail
.rba_range:
    cmp qword [rbp - RBA_OVER], 0
    jne .rba_range_over
    CSTRING rsi, "() takes at least "
    jmp .rba_range_count
.rba_range_over:
    CSTRING rsi, "() takes at most "
.rba_range_count:
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RBA_WANT]
    call msg_append_i64
    mov rdi, rax
    CSTRING rsi, " arguments ("
    cmp qword [rbp - RBA_WANT], 1
    jne .rba_tail
    CSTRING rsi, " argument ("
.rba_tail:
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RBA_GOT]
    call msg_append_i64
    mov rdi, rax
    CSTRING rsi, " given)"
    call rbt_append_cstr
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rbp - RBA_BUF]
    call raise_exception
END_FUNC raise_builtin_arity
