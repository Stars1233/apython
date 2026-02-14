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
    .stack_ptr:     resq 1  ; +48: saved value stack pointer
    .stack_base:    resq 1  ; +56: ptr to bottom of value stack
    .return_offset: resd 1  ; +64: return offset
    .nlocalsplus:   resd 1  ; +68: number of locals + cells + frees
    .func_obj:      resq 1  ; +72: ptr to function object (for closures)
    .localsplus:             ; +80: PyObject*[] array (variable size)
endstruc

FRAME_HEADER_SIZE equ PyFrame.localsplus

%endif ; FRAME_INC
