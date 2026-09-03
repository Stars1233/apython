; pyo/memoryview.asm - the memoryview type
;
; Split out of bytes.asm along with bytearray; see the note there.  A
; memoryview owns nothing: it borrows a buffer from a bytes or a bytearray and
; tells the owner when to let go again, which is why release() and the export
; count are as much of this file as the reading is.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
extern ap_strcmp
extern bool_false
extern bool_true
extern bytearray_export_acquired
extern bytearray_export_released
extern bytearray_type
extern bytes_compare
extern bytes_from_data
extern bytes_like_ptr_len
extern bytes_method_hex
extern bytes_type
extern exc_NotImplementedError_type
extern exc_ValueError_type
extern hash_not_implemented
extern int_is_integer
extern io_buffer_acquired
extern io_buffer_released
extern list_append
extern list_new
extern memoryview_iter_type
extern mv_format_H
extern mv_format_I
extern mv_format_L
extern mv_format_Q
extern none_singleton
extern obj_as_index
extern obj_dealloc
extern str_from_cstr_heap
extern str_type
extern tuple_new
section .text

extern ap_malloc
extern ap_free
extern ap_memcpy
extern type_type
extern obj_incref
extern obj_decref
extern raise_exception
extern exc_TypeError_type
extern int_type
extern bool_type
extern exc_IndexError_type
extern int_to_i64
extern slice_type
extern slice_indices

section .text

;; ============================================================================
;; memoryview_type_call(type, args, nargs) -> PyMemoryViewObject*
;; Constructor: memoryview(bytes_obj)
;; ============================================================================
global memoryview_type_call
MV_FRAME equ 8              ; + 0 pushes = 8, not 16-aligned
DEF_FUNC memoryview_type_call, MV_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    cmp rdx, 1
    jne .mv_error
    mov rdi, [rsi]                     ; arg0 payload
    ; Must be a bytes-like object (reject all non-pointer tags)
    V_TEST_PTR_M [rsi], r11      ; args[0] a pointer?
    ja .mv_error
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .mv_check_bytearray

.mv_from_bytes:
    ; rdi = the source, bytes or bytearray
    push rdi
    mov edi, PyMemoryViewObject_size
    call ap_malloc
    pop rdi                            ; source

    ; Init header
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx
    mov [rax + PyMemoryViewObject.mv_source], rdi
    push rax                           ; save result
    push rdi                           ; save for INCREF
    INCREF rdi
    ; A bytearray source counts this view, so a later resize can refuse.  The
    ; BytesIO sites below already do the equivalent through io_buffer_*; this
    ; arm called neither, which is the hole.
    call bytearray_export_acquired
    pop rdi
    pop rax

    ; A bytes keeps its data inline and a bytearray does not, so the two
    ; cannot share one read -- which is what this did while the layouts
    ; happened to match.
    push rax
    call bytes_like_ptr_len            ; rax = data, r10 = length
    mov rcx, rax
    pop rax
    mov [rax + PyMemoryViewObject.mv_buf], rcx
    mov [rax + PyMemoryViewObject.mv_len], r10

    ; A view starts out over single bytes, and is read-only exactly when its
    ; source is.
    mov qword [rax + PyMemoryViewObject.mv_itemsize], 1
    lea rcx, [rel mv_format_B]
    mov [rax + PyMemoryViewObject.mv_format], rcx
    mov rcx, [rdi + PyObject.ob_type]
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    sete cl
    movzx ecx, cl
    mov [rax + PyMemoryViewObject.mv_readonly], rcx

    mov edx, TAG_PTR
    leave
    ret

.mv_check_bytearray:
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .mv_from_bytes
    lea rcx, [rel memoryview_type]
    cmp rax, rcx
    je .mv_from_view
    jmp .mv_error

.mv_from_view:
    ; memoryview(memoryview) shares the same window, as CPython's does.
    push rdi
    mov edi, PyMemoryViewObject_size
    call ap_malloc
    pop rdi
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_source]
    mov [rax + PyMemoryViewObject.mv_source], rcx
    test rcx, rcx
    jz .mv_view_no_src
    inc qword [rcx + PyObject.ob_refcnt]
    push rax
    push rdi
    mov rdi, rcx
    call bytearray_export_acquired
    call io_buffer_acquired     ; a second view over a BytesIO is a second
    pop rdi                     ; export, and its release will decrement
    pop rax
.mv_view_no_src:
    mov rcx, [rdi + PyMemoryViewObject.mv_buf]
    mov [rax + PyMemoryViewObject.mv_buf], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_len]
    mov [rax + PyMemoryViewObject.mv_len], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_itemsize]
    mov [rax + PyMemoryViewObject.mv_itemsize], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_format]
    mov [rax + PyMemoryViewObject.mv_format], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_readonly]
    mov [rax + PyMemoryViewObject.mv_readonly], rcx
    mov edx, TAG_PTR
    leave
    ret

.mv_error:
    RAISE exc_TypeError_type, "memoryview: a bytes-like object is required"
END_FUNC memoryview_type_call


;; Proper dealloc:
DEF_FUNC memoryview_dealloc_proper
    push rdi                           ; save self
    mov rdi, [rdi + PyMemoryViewObject.mv_source]
    test rdi, rdi
    jz .mvd_no_source                  ; already released
    push rdi
    call bytearray_export_released
    call io_buffer_released
    pop rdi
    call obj_decref
.mvd_no_source:
    pop rdi                            ; restore self
    call ap_free
    leave
    ret
END_FUNC memoryview_dealloc_proper

;; ============================================================================
;; memoryview_check(rdi = self) -> returns, or raises
;;
;; Every operation on a released view raises, which is the whole point of
;; release(): it lets the buffer's owner resize again, so anything still
;; pointing into the old buffer has to be refused rather than read.
;; ============================================================================
DEF_FUNC_BARE memoryview_check
    cmp qword [rdi + PyMemoryViewObject.mv_buf], MV_RELEASED
    je memoryview_released_error
    ret
END_FUNC memoryview_check

DEF_FUNC memoryview_released_error
    RAISE exc_ValueError_type, "operation forbidden on released memoryview object"
END_FUNC memoryview_released_error

;; ============================================================================
;; memoryview_item_value(rdi = self, rsi = item index) -> rax = the item Value
;;
;; itemsize bytes, read little-endian and unsigned -- which is all the
;; formats below need, since cast() accepts only the unsigned ones.
;; ============================================================================
DEF_FUNC_BARE memoryview_item_value
    mov rcx, [rdi + PyMemoryViewObject.mv_itemsize]
    mov rax, [rdi + PyMemoryViewObject.mv_buf]
    imul rsi, rcx
    add rax, rsi
    cmp rcx, 1
    je .miv_1
    cmp rcx, 2
    je .miv_2
    cmp rcx, 4
    je .miv_4
    mov rax, [rax]
    ret
.miv_1:
    movzx eax, byte [rax]
    ret
.miv_2:
    movzx eax, word [rax]
    ret
.miv_4:
    mov eax, [rax]
    ret
END_FUNC memoryview_item_value

;; ============================================================================
;; memoryview_getattr(rdi = self, rsi = name str) -> rax = Value, or NULL
;;
;; tp_getattr was 0, so a memoryview had no attributes and no methods at all.
;; _pyio reads nbytes and readonly, calls tobytes and cast, and uses `with
;; memoryview(b) as view:` around every readinto.
;;
;; NULL for an unknown name rather than a raise, so op_load_attr falls through
;; to the MRO's tp_dicts -- the contract every other tp_getattr keeps.
;; ============================================================================
MVG_SELF  equ 8
MVG_NAME  equ 16
MVG_FRAME equ 32            ; + 0 pushes = 32

%macro MVG_NAME_IS 2            ; %1 = the C string, %2 = the label
    mov rdi, [rbp - MVG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, %1
    call ap_strcmp
    test eax, eax
    jz %2
%endmacro

DEF_FUNC memoryview_getattr, MVG_FRAME
    mov [rbp - MVG_SELF], rdi
    mov [rbp - MVG_NAME], rsi

    MVG_NAME_IS "nbytes",       .mvg_nbytes
    MVG_NAME_IS "itemsize",     .mvg_itemsize
    MVG_NAME_IS "format",       .mvg_format
    MVG_NAME_IS "readonly",     .mvg_readonly
    MVG_NAME_IS "obj",          .mvg_obj
    MVG_NAME_IS "ndim",         .mvg_ndim
    MVG_NAME_IS "shape",        .mvg_shape
    MVG_NAME_IS "strides",      .mvg_strides
    MVG_NAME_IS "suboffsets",   .mvg_suboffsets
    MVG_NAME_IS "c_contiguous", .mvg_true
    MVG_NAME_IS "f_contiguous", .mvg_true
    MVG_NAME_IS "contiguous",   .mvg_true

    ; Not an attribute of ours: the methods live in tp_dict.
    RET_NULL
    leave
    V_PACK rax, rdx
    ret

.mvg_nbytes:
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    jmp .mvg_int

.mvg_itemsize:
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_itemsize]
    jmp .mvg_int

.mvg_ndim:
    mov eax, 1
.mvg_int:
    V_PACK_I64 rax, rcx
    mov edx, TAG_PTR
    leave
    ret

.mvg_format:
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov rdi, [rbp - MVG_SELF]
    mov rdi, [rdi + PyMemoryViewObject.mv_format]
    call str_from_cstr_heap
    leave
    ret

.mvg_readonly:
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov rdi, [rbp - MVG_SELF]
    cmp qword [rdi + PyMemoryViewObject.mv_readonly], 0
    jne .mvg_true
    lea rax, [rel bool_false]
    jmp .mvg_bool_out
.mvg_true:
    lea rax, [rel bool_true]
.mvg_bool_out:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.mvg_obj:
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_source]
    test rax, rax
    jz .mvg_none
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.mvg_none:
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret

.mvg_shape:
    ; One dimension, so a 1-tuple of the item count.
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov edi, 1
    call tuple_new
    test rax, rax
    jz .mvg_none
    mov [rbp - MVG_NAME], rax          ; the name is finished with
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    xor edx, edx
    div qword [rdi + PyMemoryViewObject.mv_itemsize]
    V_PACK_I64 rax, rcx
    mov rcx, [rbp - MVG_NAME]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx], rax
    mov rax, [rbp - MVG_NAME]
    mov edx, TAG_PTR
    leave
    ret

.mvg_strides:
    ; Contiguous, so the stride is the item size.
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov edi, 1
    call tuple_new
    test rax, rax
    jz .mvg_none
    mov [rbp - MVG_NAME], rax
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_itemsize]
    V_PACK_I64 rax, rcx
    mov rcx, [rbp - MVG_NAME]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx], rax
    mov rax, [rbp - MVG_NAME]
    mov edx, TAG_PTR
    leave
    ret

.mvg_suboffsets:
    xor edi, edi
    call tuple_new
    mov edx, TAG_PTR
    leave
    ret
END_FUNC memoryview_getattr

;; ============================================================================
;; memoryview_repr(rdi = self, edx = tag) -> a str
;;
;; tp_repr was 0, so printing one reached obj_repr's fallback and raised
;; "build_string expects str".
;; ============================================================================
DEF_FUNC memoryview_repr
    cmp qword [rdi + PyMemoryViewObject.mv_buf], MV_RELEASED
    je .mvr_released
    CSTRING rdi, "<memory>"
    call str_from_cstr_heap
    leave
    ret
.mvr_released:
    CSTRING rdi, "<released memory>"
    call str_from_cstr_heap
    leave
    ret
END_FUNC memoryview_repr

;; ============================================================================
;; The methods.  Each takes (rdi = args Value[], rsi = nargs), args[0] = self.
;; ============================================================================
MVM_SELF  equ 8
MVM_ARG   equ 16
MVM_TMP   equ 24
MVM_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC memoryview_method_tobytes, MVM_FRAME
    test rsi, rsi
    jz .mvt_argerr
    mov rdi, [rdi]
    mov [rbp - MVM_SELF], rdi
    call memoryview_check
    mov rdi, [rbp - MVM_SELF]
    mov rsi, [rdi + PyMemoryViewObject.mv_len]
    mov rdi, [rdi + PyMemoryViewObject.mv_buf]
    call bytes_from_data
    mov edx, TAG_PTR
    leave
    ret
.mvt_argerr:
    RAISE exc_TypeError_type, "tobytes() takes no arguments"
END_FUNC memoryview_method_tobytes

DEF_FUNC memoryview_method_release, MVM_FRAME
    test rsi, rsi
    jz .mvrl_argerr
    mov rdi, [rdi]
    ; Releasing twice is not an error, as CPython has it.
    cmp qword [rdi + PyMemoryViewObject.mv_buf], MV_RELEASED
    je .mvrl_done
    mov qword [rdi + PyMemoryViewObject.mv_buf], MV_RELEASED
    mov rax, [rdi + PyMemoryViewObject.mv_source]
    mov qword [rdi + PyMemoryViewObject.mv_source], 0
    test rax, rax
    jz .mvrl_done
    push rax                    ; io_buffer_released returns in rax, so the
    mov rdi, rax                ; source has to survive the call in a slot
    call bytearray_export_released
    call io_buffer_released     ; a BytesIO counts its live views
    pop rdi
    call obj_decref
.mvrl_done:
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.mvrl_argerr:
    RAISE exc_TypeError_type, "release() takes no arguments"
END_FUNC memoryview_method_release

;; __enter__ hands the view back; __exit__ releases it.  `with memoryview(b)
;; as view:` is how _pyio wraps every readinto.
DEF_FUNC memoryview_method_enter, MVM_FRAME
    test rsi, rsi
    jz .mve_argerr
    mov rax, [rdi]
    push rax
    mov rdi, rax
    call memoryview_check
    pop rax
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.mve_argerr:
    RAISE exc_TypeError_type, "__enter__() takes no arguments"
END_FUNC memoryview_method_enter

DEF_FUNC memoryview_method_exit, MVM_FRAME
    test rsi, rsi
    jz .mvx_argerr
    mov esi, 1
    call memoryview_method_release
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.mvx_argerr:
    RAISE exc_TypeError_type, "__exit__() takes three arguments"
END_FUNC memoryview_method_exit

;; memoryview.cast(fmt) -- what re._compiler._bytes_to_codes calls with 'I'.
MVC_SELF  equ 8
MVC_FMT   equ 16
MVC_SIZE  equ 24
MVC_STR   equ 32
MVC_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC memoryview_method_cast, MVC_FRAME
    cmp rsi, 2
    jl .mvc_argerr
    mov rax, [rdi]
    mov [rbp - MVC_SELF], rax
    mov rcx, [rdi + 8]
    mov [rbp - MVC_STR], rcx
    mov rdi, rax
    call memoryview_check

    ; One character, and only the unsigned formats: those are what
    ; memoryview_item_value reads, and what CPython's own callers use here.
    mov rcx, [rbp - MVC_STR]
    V_TEST_PTR rcx, rax
    ja .mvc_badfmt
    mov rax, [rcx + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rax, rdx
    jne .mvc_badfmt
    cmp qword [rcx + PyStrObject.ob_size], 1
    jne .mvc_badfmt
    movzx eax, byte [rcx + PyStrObject.data]

    lea rdx, [rel mv_format_B]
    mov esi, 1
    cmp al, 'B'
    je .mvc_have_fmt
    cmp al, 'b'
    je .mvc_have_fmt
    lea rdx, [rel mv_format_H]
    mov esi, 2
    cmp al, 'H'
    je .mvc_have_fmt
    lea rdx, [rel mv_format_I]
    mov esi, 4
    cmp al, 'I'
    je .mvc_have_fmt
    lea rdx, [rel mv_format_L]
    mov esi, 8
    cmp al, 'L'
    je .mvc_have_fmt
    lea rdx, [rel mv_format_Q]
    mov esi, 8
    cmp al, 'Q'
    jne .mvc_badfmt
.mvc_have_fmt:
    mov [rbp - MVC_SIZE], rsi
    mov [rbp - MVC_FMT], rdx

    ; The byte length must divide evenly, as CPython requires.
    mov rdi, [rbp - MVC_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    xor edx, edx
    div qword [rbp - MVC_SIZE]
    test rdx, rdx
    jnz .mvc_badlen

    mov edi, PyMemoryViewObject_size
    call ap_malloc
    test rax, rax
    jz .mvc_badlen
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx
    mov rdi, [rbp - MVC_SELF]
    mov rcx, [rdi + PyMemoryViewObject.mv_source]
    mov [rax + PyMemoryViewObject.mv_source], rcx
    test rcx, rcx
    jz .mvc_no_src
    inc qword [rcx + PyObject.ob_refcnt]
    ; cast() is the fourth place a view takes a share of another's source, and
    ; release and dealloc decrement for every view that has one -- so without
    ; the matching acquire this view's release drove a BytesIO's export count
    ; below what is outstanding, and the next write reallocated the storage
    ; underneath it.  lib/_io.py's readinto does `b = b.cast("B")`, so it is
    ; on the ordinary path, not a corner.
    push rax
    push rdi
    mov rdi, rcx
    call bytearray_export_acquired
    call io_buffer_acquired
    pop rdi
    pop rax
.mvc_no_src:
    mov rcx, [rdi + PyMemoryViewObject.mv_buf]
    mov [rax + PyMemoryViewObject.mv_buf], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_len]
    mov [rax + PyMemoryViewObject.mv_len], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_readonly]
    mov [rax + PyMemoryViewObject.mv_readonly], rcx
    mov rcx, [rbp - MVC_SIZE]
    mov [rax + PyMemoryViewObject.mv_itemsize], rcx
    mov rcx, [rbp - MVC_FMT]
    mov [rax + PyMemoryViewObject.mv_format], rcx
    mov edx, TAG_PTR
    leave
    ret

.mvc_badfmt:
    RAISE exc_ValueError_type, "memoryview: destination format must be a native single character format prefixed with an optional '@'"
.mvc_badlen:
    RAISE exc_TypeError_type, "memoryview: length is not a multiple of itemsize"
.mvc_argerr:
    RAISE exc_TypeError_type, "cast() takes at least 1 argument"
END_FUNC memoryview_method_cast

;; memoryview.tolist() and .hex()
MVL_SELF  equ 8
MVL_OUT   equ 16
MVL_N     equ 24
MVL_FRAME equ 32            ; + 1 push = 40

DEF_FUNC memoryview_method_tolist, 40
    push rbx
    test rsi, rsi
    jz .mvtl_argerr
    mov rdi, [rdi]
    mov [rbp - MVL_SELF], rdi
    call memoryview_check
    mov rdi, [rbp - MVL_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    xor edx, edx
    div qword [rdi + PyMemoryViewObject.mv_itemsize]
    mov [rbp - MVL_N], rax
    xor edi, edi
    call list_new
    test rax, rax
    jz .mvtl_fail
    mov [rbp - MVL_OUT], rax
    xor ebx, ebx
.mvtl_loop:
    cmp rbx, [rbp - MVL_N]
    jge .mvtl_done
    mov rdi, [rbp - MVL_SELF]
    mov rsi, rbx
    call memoryview_item_value
    V_PACK_I64 rax, rcx
    mov rsi, rax
    mov rdi, [rbp - MVL_OUT]
    push rsi
    push rsi
    call list_append
    pop rsi
    pop rsi
    DECREF_V rsi, rcx           ; V_PACK may have boxed it
    inc rbx
    jmp .mvtl_loop
.mvtl_done:
    mov rax, [rbp - MVL_OUT]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.mvtl_fail:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
.mvtl_argerr:
    RAISE exc_TypeError_type, "tolist() takes no arguments"
END_FUNC memoryview_method_tolist

;; ============================================================================
;; memoryview iteration.  tp_iter was 0, so `for b in mv` and list(mv) both
;; failed -- and _pyio iterates a view in more than one place.  The index is
;; checked against the current length each time, as the bytes iterator does.
;; ============================================================================
DEF_FUNC memoryview_tp_iter
    push rbx
    mov rbx, rdi
    call memoryview_check
    mov edi, PyBytesIterObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel memoryview_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyBytesIterObject.it_seq], rbx
    mov qword [rax + PyBytesIterObject.it_index], 0
    inc qword [rbx + PyObject.ob_refcnt]
    pop rbx
    leave
    ret
END_FUNC memoryview_tp_iter

;; memoryview.hex() -- through a temporary bytes, as bytearray's read-only
;; methods do, for the same reason: bytes_method_hex reads a bytes layout.
MVH_TMP   equ 8
MVH_FRAME equ 16            ; + 0 pushes = 16

DEF_FUNC memoryview_method_hex, MVH_FRAME
    test rsi, rsi
    jz .mvh_argerr
    mov rdi, [rdi]
    call memoryview_check
    call memoryview_method_hex_self
    leave
    ret
.mvh_argerr:
    RAISE exc_TypeError_type, "hex() takes no arguments"
END_FUNC memoryview_method_hex

DEF_FUNC memoryview_method_hex_self, MVH_FRAME
    mov rsi, [rdi + PyMemoryViewObject.mv_len]
    mov rdi, [rdi + PyMemoryViewObject.mv_buf]
    call bytes_from_data
    test rax, rax
    jz .mvhs_fail
    mov [rbp - MVH_TMP], rax
    sub rsp, 16
    mov [rsp], rax
    mov rdi, rsp
    mov esi, 1
    call bytes_method_hex
    add rsp, 16
    push rax
    push rax
    mov rdi, [rbp - MVH_TMP]
    call obj_decref
    pop rax
    pop rax
    mov edx, TAG_PTR
    leave
    ret
.mvhs_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC memoryview_method_hex_self

DEF_FUNC_BARE memoryview_iter_next
    mov rax, [rdi + PyBytesIterObject.it_seq]
    cmp qword [rax + PyMemoryViewObject.mv_buf], MV_RELEASED
    je .mvin_done
    mov rcx, [rdi + PyBytesIterObject.it_index]
    mov rdx, [rax + PyMemoryViewObject.mv_len]
    push rdi
    push rax
    mov r8, [rax + PyMemoryViewObject.mv_itemsize]
    mov rax, rdx
    xor edx, edx
    div r8                      ; the item count
    pop rdi                     ; the view
    cmp rcx, rax
    jge .mvin_pop_done
    mov rsi, rcx
    call memoryview_item_value
    pop rdi                     ; the iterator
    inc qword [rdi + PyBytesIterObject.it_index]
    V_PACK_I64 rax, rcx
    ret
.mvin_pop_done:
    pop rdi
.mvin_done:
    xor eax, eax
    ret
END_FUNC memoryview_iter_next




;; ============================================================================
;; memoryview_subscript(obj, key) -> PyMemoryViewObject* (slice)
;; ============================================================================
MS_OBJ   equ 8
MS_KEY   equ 16
MS_START equ 24
MS_COUNT equ 32
MS_FRAME equ 32             ; + 0 pushes = 32
DEF_FUNC memoryview_subscript, MS_FRAME
    ; A view indexes in ITEMS, not bytes.  They are the same thing until
    ; cast() has been called, which is why the byte length stood in for the
    ; item count here and the count was one divide away from being wrong.
    mov [rbp - MS_OBJ], rdi
    call memoryview_check
    V_UNPACK rsi, rdx           ; key Value -> (payload, tag)
    mov [rbp - MS_KEY], rsi

    cmp edx, TAG_SMALLINT
    je .ms_int_index
    cmp edx, TAG_PTR            ; a float key is neither: classify
    jne .ms_type_error          ; fully before dereferencing, or raw
                                ; f64 bits get used as an address
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    jne .ms_int_index_heap

    ; --- slice ---
    mov rdi, [rbp - MS_OBJ]            ; the length slice_indices clamps
    call memoryview_nitems             ; against is the VIEW's, in items
    mov rsi, rax
    mov rdi, [rbp - MS_KEY]            ; slice obj
    call slice_indices                 ; rax=start, rdx=stop, rcx=step

    ; A step other than 1 needs a stride, which this view does not carry;
    ; CPython answers with a non-contiguous view.  Recorded in bugs.md.
    cmp rcx, 1
    jne .ms_step_error

    sub rdx, rax                       ; item count
    jns .ms_have_count
    xor edx, edx                       ; an empty slice, not a negative one
.ms_have_count:
    mov [rbp - MS_START], rax
    mov [rbp - MS_COUNT], rdx

    mov edi, PyMemoryViewObject_size
    call ap_malloc
    test rax, rax
    jz .ms_fail

    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx

    ; Every field the source carries has to come across.  A missed itemsize
    ; left the new view with 0, and memoryview_len divides by it.
    mov rdi, [rbp - MS_OBJ]
    mov rcx, [rdi + PyMemoryViewObject.mv_itemsize]
    mov [rax + PyMemoryViewObject.mv_itemsize], rcx
    mov rdx, [rdi + PyMemoryViewObject.mv_format]
    mov [rax + PyMemoryViewObject.mv_format], rdx
    mov rdx, [rdi + PyMemoryViewObject.mv_readonly]
    mov [rax + PyMemoryViewObject.mv_readonly], rdx

    mov rdx, [rbp - MS_START]
    imul rdx, rcx
    add rdx, [rdi + PyMemoryViewObject.mv_buf]
    mov [rax + PyMemoryViewObject.mv_buf], rdx
    mov rdx, [rbp - MS_COUNT]
    imul rdx, rcx
    mov [rax + PyMemoryViewObject.mv_len], rdx

    ; The slice shares the ORIGINAL owner, not the view it came from: a
    ; chain of slices would otherwise keep every intermediate alive.
    mov rcx, [rdi + PyMemoryViewObject.mv_source]
    mov [rax + PyMemoryViewObject.mv_source], rcx
    test rcx, rcx
    jz .ms_no_source
    inc qword [rcx + PyObject.ob_refcnt]
    push rax
    mov rdi, rcx
    call bytearray_export_acquired
    call io_buffer_acquired
    pop rax
.ms_no_source:
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.ms_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret

.ms_int_index:
    ; rsi = the index, as an i64
    mov rdi, [rbp - MS_OBJ]
    push rsi
    call memoryview_nitems             ; rax = item count
    pop rsi
    mov rcx, rax
    test rsi, rsi
    jns .ms_check_bounds
    add rsi, rcx
.ms_check_bounds:
    cmp rsi, 0
    jl .ms_index_error
    cmp rsi, rcx
    jge .ms_index_error
    mov rdi, [rbp - MS_OBJ]
    call memoryview_item_value
    mov edx, TAG_SMALLINT
    leave
    V_PACK rax, rdx
    ret

.ms_int_index_heap:
    mov rax, [rsi + PyObject.ob_type]   ; int_to_i64 reads PyIntObject.compact
    REQUIRE_INT_TYPE rax, rcx, .ms_type_error   ; unconditionally
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax
    jmp .ms_int_index

.ms_index_error:
    RAISE exc_IndexError_type, "index out of range"

.ms_step_error:
    RAISE exc_NotImplementedError_type, "memoryview: only step 1 is supported"

.ms_type_error:
    RAISE exc_TypeError_type, "memoryview: invalid slice key"
END_FUNC memoryview_subscript

;; ============================================================================
;; memoryview_nitems(rdi = self) -> rax = the length in ITEMS
;; ============================================================================
DEF_FUNC_BARE memoryview_nitems
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    mov r8, [rdi + PyMemoryViewObject.mv_itemsize]
    cmp r8, 1
    je .mvn_done
    push rdx
    xor edx, edx
    div r8
    pop rdx
.mvn_done:
    ret
END_FUNC memoryview_nitems

;; ============================================================================
;; memoryview_ass_subscript(rdi = self, rsi = key Value, rdx = value Value)
;;
;; mp_ass_subscript was 0, so a view over a bytearray was read-only in
;; practice -- and readinto(), which is the whole reason _pyio takes a view,
;; is nothing but writes through one.
;;
;; A slice assignment must be the same size: a view cannot resize its owner.
;; ============================================================================
MA_OBJ    equ 8
MA_KEY    equ 16
MA_VAL    equ 24
MA_START  equ 32
MA_COUNT  equ 40
MA_FRAME  equ 48            ; + 0 pushes = 48

DEF_FUNC memoryview_ass_subscript, MA_FRAME
    mov [rbp - MA_OBJ], rdi
    mov [rbp - MA_KEY], rsi
    mov [rbp - MA_VAL], rdx
    call memoryview_check
    test rdx, rdx
    jz .ma_del_error
    cmp qword [rdi + PyMemoryViewObject.mv_readonly], 0
    jne .ma_readonly

    mov rsi, [rbp - MA_KEY]
    V_TEST_PTR rsi, rax
    jbe .ma_maybe_slice
.ma_int_key:
    ; An item assignment: one integer, range-checked like bytearray's.
    mov rdi, [rbp - MA_KEY]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rsi, rax
    mov rdi, [rbp - MA_OBJ]
    push rsi
    call memoryview_nitems
    pop rsi
    mov rcx, rax
    test rsi, rsi
    jns .ma_bounds
    add rsi, rcx
.ma_bounds:
    cmp rsi, 0
    jl .ma_index_error
    cmp rsi, rcx
    jge .ma_index_error
    mov [rbp - MA_START], rsi

    mov rdi, [rbp - MA_VAL]
    V_UNPACK rdi, rdx
    call int_is_integer         ; not a tag test: a heap int, a bool and an
    test eax, eax               ; int subclass are all integers here
    jz .ma_value_type
    mov rdi, [rbp - MA_VAL]
    V_UNPACK rdi, rdx
    call obj_as_index
    cmp rax, 0
    jl .ma_value_range
    cmp rax, 255
    jg .ma_value_range

    mov rdi, [rbp - MA_OBJ]
    mov rsi, [rbp - MA_START]
    mov rcx, [rdi + PyMemoryViewObject.mv_itemsize]
    imul rsi, rcx
    add rsi, [rdi + PyMemoryViewObject.mv_buf]
    cmp rcx, 1
    je .ma_store1
    cmp rcx, 2
    je .ma_store2
    cmp rcx, 4
    je .ma_store4
    mov [rsi], rax
    jmp .ma_ok
.ma_store1:
    mov [rsi], al
    jmp .ma_ok
.ma_store2:
    mov [rsi], ax
    jmp .ma_ok
.ma_store4:
    mov [rsi], eax
.ma_ok:
    xor eax, eax
    leave
    ret

.ma_maybe_slice:
    test rsi, rsi
    jz .ma_key_type
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    jne .ma_int_key

    mov rdi, [rbp - MA_OBJ]
    call memoryview_nitems
    mov rsi, rax
    mov rdi, [rbp - MA_KEY]
    call slice_indices                 ; rax=start, rdx=stop, rcx=step
    cmp rcx, 1
    jne .ma_step_error
    sub rdx, rax
    jns .ma_have_count
    xor edx, edx
.ma_have_count:
    mov [rbp - MA_START], rax
    mov [rbp - MA_COUNT], rdx

    mov rdi, [rbp - MA_VAL]
    call bytes_like_ptr_len            ; rax = data, r10 = length, ecx = ok
    test ecx, ecx
    jz .ma_value_type
    mov rdi, [rbp - MA_OBJ]
    mov rdx, [rbp - MA_COUNT]
    imul rdx, [rdi + PyMemoryViewObject.mv_itemsize]
    cmp r10, rdx
    jne .ma_size_error

    mov rsi, rax                       ; source
    mov rdi, [rbp - MA_START]
    mov rax, [rbp - MA_OBJ]
    imul rdi, [rax + PyMemoryViewObject.mv_itemsize]
    add rdi, [rax + PyMemoryViewObject.mv_buf]
    call ap_memcpy
    jmp .ma_ok

.ma_del_error:
    RAISE exc_TypeError_type, "cannot delete memory"
.ma_readonly:
    RAISE exc_TypeError_type, "cannot modify read-only memory"
.ma_index_error:
    RAISE exc_IndexError_type, "index out of range"
.ma_value_range:
    RAISE exc_ValueError_type, "memoryview: invalid value for format 'B'"
.ma_value_type:
    RAISE exc_TypeError_type, "memoryview: invalid type for assignment"
.ma_size_error:
    RAISE exc_ValueError_type, "memoryview assignment: lvalue and rvalue have different structures"
.ma_step_error:
    RAISE exc_NotImplementedError_type, "memoryview: only step 1 is supported"
.ma_key_type:
    RAISE exc_TypeError_type, "memoryview: invalid slice key"
END_FUNC memoryview_ass_subscript

;; ============================================================================
;; memoryview_len(obj) -> int64
;; ============================================================================
;; len() counts ITEMS, so a view cast to 'I' is a quarter as long as its
;; bytes; and a released view answers no questions at all.
DEF_FUNC memoryview_len
    call memoryview_check
    call memoryview_nitems
    leave
    ret
END_FUNC memoryview_len

;; The by-name half of the two slots above.  A slot with no matching entry in
;; tp_dict answers hasattr() and getattr() wrong, and _pyio reaches
;; __setitem__ through the abstract base classes rather than the syntax.
DEF_FUNC memoryview_dunder_getitem
    REQUIRE_SELF memoryview_type, "__getitem__"
    cmp rsi, 2
    jne .mdg_bad
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call memoryview_subscript
    leave
    ret
.mdg_bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC memoryview_dunder_getitem

DEF_FUNC memoryview_dunder_setitem
    REQUIRE_SELF memoryview_type, "__setitem__"
    cmp rsi, 3
    jne .mds_bad
    mov rdx, [rdi + 16]
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call memoryview_ass_subscript
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.mds_bad:
    RAISE exc_TypeError_type, "expected exactly two arguments"
END_FUNC memoryview_dunder_setitem

DEF_FUNC memoryview_dunder_len
    REQUIRE_SELF memoryview_type, "__len__"
    test rsi, rsi
    jz .mdl_bad
    mov rdi, [rdi]
    call memoryview_len
    mov rdx, rax
    V_PACK_I64 rdx, rcx
    mov rax, rdx
    leave
    ret
.mdl_bad:
    RAISE exc_TypeError_type, "__len__() takes no arguments"
END_FUNC memoryview_dunder_len

;; ============================================================================
;; Type object
;; ============================================================================
section .data

align 8
mv_name_str:  db "memoryview", 0

align 8
memoryview_seq_methods:
    dq memoryview_len       ; +0: sq_length
    dq 0                    ; +8: sq_concat
    dq 0                    ; +16: sq_repeat
    dq 0                    ; +24: sq_item
    dq 0                    ; +32: sq_ass_item
    dq 0                    ; +40: sq_contains
    dq 0                    ; +48: sq_inplace_concat
    dq 0                    ; +56: sq_inplace_repeat

align 8
memoryview_mapping_methods:
    dq memoryview_len       ; +0: mp_length
    dq memoryview_subscript ; +8: mp_subscript
    dq memoryview_ass_subscript ; +16: mp_ass_subscript

align 8
global memoryview_type
memoryview_type:
    dq 1                             ; ob_refcnt
    dq type_type                     ; ob_type
    dq mv_name_str                   ; tp_name
    dq PyMemoryViewObject_size       ; tp_basicsize
    dq memoryview_dealloc_proper     ; tp_dealloc
    dq memoryview_repr               ; tp_repr
    dq memoryview_repr               ; tp_str
    ; Unhashable while the buffer is writable, which is the only kind that
    ; reaches here in practice; a 0 would fall through to the object address.
    dq hash_not_implemented          ; tp_hash
    dq 0                             ; tp_call (set by add_builtin_type)
    dq memoryview_getattr            ; tp_getattr
    dq 0                             ; tp_setattr
    dq bytes_compare                 ; tp_richcompare (over the shared reader)
    dq memoryview_tp_iter            ; tp_iter
    dq 0                             ; tp_iternext
    dq 0                             ; tp_init
    dq 0                             ; tp_new
    dq 0                             ; tp_as_number
    dq memoryview_seq_methods        ; tp_as_sequence
    dq memoryview_mapping_methods    ; tp_as_mapping
    dq 0                             ; tp_base
    dq 0                             ; tp_dict
    dq 0                             ; tp_mro
    dq TYPE_FLAG_FINAL          ; tp_flags -- CPython gives this type no
                                ; Py_TPFLAGS_BASETYPE
    dq 0                             ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

section .rodata
; The one-character format codes a view can carry.  cast() accepts only the
; unsigned ones, which is what memoryview_item_value reads.
global mv_format_B
mv_format_B: db "B", 0
mv_format_H: db "H", 0
mv_format_I: db "I", 0
mv_format_L: db "L", 0
mv_format_Q: db "Q", 0


