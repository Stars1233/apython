; frame.inc - Frame layout definitions

%ifndef FRAME_INC
%define FRAME_INC

; Execution frame layout
struc PyFrame
    .prev_frame:    resq 1  ; +0:  ptr to previous frame
    .code:          resq 1  ; +8:  ptr to code object
    .globals:       resq 1  ; +16: ptr to globals dict
    .builtins:      resq 1  ; +24: ptr to builtins dict
    .locals:        resq 1  ; +32: ptr to locals dict (or NULL for fast locals)
    .instr_ptr:     resq 1  ; +40: saved bytecode pointer
    .stack_ptr:     resq 1  ; +48: saved payload stack pointer
    .stack_base:    resq 1  ; +56: ptr to bottom of payload stack
    .stack_tag_ptr: resq 1  ; +64: saved tag stack pointer
    .stack_tag_base: resq 1 ; +72: ptr to bottom of tag stack
    .return_offset: resd 1  ; +80: return offset
    .nlocalsplus:   resd 1  ; +84: number of locals + cells + frees
    .func_obj:      resq 1  ; +88: ptr to function object (for closures)
    .locals_tag_base: resq 1 ; +96: ptr to locals tag array
    .localsplus:             ; +104: Value64 payload array (variable size)
endstruc

FRAME_HEADER_SIZE equ PyFrame.localsplus

%endif ; FRAME_INC
