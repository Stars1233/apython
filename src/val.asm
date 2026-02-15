; val.asm - Fat value widen/narrow helpers for 128-bit stack values
;
; val_widen:  64-bit heap value → (payload, tag) in (rax, rdx)
; val_narrow: (payload, tag) → 64-bit heap value in rax
;
; Conservative mode (Phase 3): payloads stay in heap format.
;   SmallInt payloads keep bit 63 encoding.
;   None/Bool payloads keep singleton pointers.
;   val_narrow returns payload unchanged.
;
; Future phases will decode SmallInt to raw int64, extract float
; double bits, and build SmallStr inline.

%include "macros.inc"
%include "object.inc"

;; ============================================================================
;; val_widen(rdi: heap_value) -> (rax: payload, rdx: tag)
;;
;; Classify a 64-bit heap-format value and return as fat (payload, tag) pair.
;; Conservative: payload = heap format unchanged.
;; ============================================================================
DEF_FUNC val_widen
    mov rax, rdi
    test rdi, rdi
    js .smallint
    jz .null
    ; Default: heap pointer
    mov edx, TAG_PTR
    leave
    ret
.smallint:
    mov edx, TAG_SMALLINT
    leave
    ret
.null:
    xor edx, edx               ; TAG_NULL = 0
    leave
    ret
END_FUNC val_widen

;; ============================================================================
;; val_narrow(rdi: payload, rsi: tag) -> rax: heap_value
;;
;; Convert a fat (payload, tag) pair back to 64-bit heap format.
;; Conservative: returns payload unchanged (all types stay in heap format).
;; ============================================================================
DEF_FUNC val_narrow
    mov rax, rdi
    leave
    ret
END_FUNC val_narrow
