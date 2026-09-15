; include/posixpath.inc - the private ABI between posix.asm, posixpath.asm and
; the other callers of posix_path_arg
;
; posixpath.asm turns one argument into a NUL-terminated C string; posix.asm,
; posixdir.asm and modules/io.asm consume what it hands back.  The release
; macro and the accepted-kind constants are what the two halves have to agree
; on, so they live here rather than in either of them -- the same arrangement
; sre.inc, eventloop.inc and listsort.inc already use.

%ifndef POSIXPATH_INC
%define POSIXPATH_INC

; Which kinds a caller takes, for the refusal's wording.  CPython lists the
; kinds THAT function accepts, and they differ.
POSIX_PATH_KIND_PLAIN   equ 0
POSIX_PATH_KIND_FD      equ 1
POSIX_PATH_KIND_FD_NONE equ 2
; _io words the same refusal its own way -- "expected str, bytes or
; os.PathLike object, not NoneType" -- and open() has to say what CPython's
; open() says, so the whole sentence comes from there rather than the
; "<who> should be ..." shape the posix entry points share.
POSIX_PATH_KIND_IO      equ 3

;; ============================================================================
;; posix_path_arg(rdi = Value) -> rax = const char *, rdx = an object the
;;   caller must release, or 0
;;
;; str, bytes, and anything with __fspath__.  Every PyStrObject and
;; PyBytesObject here is NUL-terminated, so for those two the pointer is the
;; caller's own path with no copy and rdx comes back 0.
;;
;; __fspath__ is the case that allocates: its result is a new str or bytes,
;; the returned pointer is into it, and it stays alive exactly as long as the
;; caller holds it.  rdx is that object.  Every caller used to ignore it --
;; the contract said "owned" in a flag nobody read -- so every PathLike
;; argument leaked its resolved path.
;;
;; POSIX_PATH_DONE is the release, and it belongs immediately after the
;; syscall and BEFORE the POSIX_CHECK: by then the kernel has copied the path
;; out, and the check reports the caller's own argument rather than this.
;; Putting it after the check would not run at all, since a raise abandons the
;; C stack.
;;
;; An embedded NUL is refused: the syscall would silently see a shorter path,
;; and CPython raises ValueError for exactly this.
;;
;; Returns rax = 0 with an exception pending on a bad type.
;; ============================================================================
%macro POSIX_PATH_DONE 1        ; %1 = a frame slot holding what rdx returned
    push rax
    push rdx
    mov rdi, %1
    test rdi, rdi
    jz %%none
    mov qword %1, 0
    call obj_decref
%%none:
    pop rdx
    pop rax
%endmacro

;; The pair of them, in the order they have to happen: the errno check names
;; the RESOLVED path, so the release cannot come first -- CPython reports
;; "No such file or directory: '/tmp/x'", not the PathLike object that
;; produced it.  raise_oserror_owned builds the exception, releases the
;; resolved path, and only then raises.
%macro POSIX_PATH_CHECK 3       ; %1 = result, %2 = the original Value,
                                ; %3 = the slot holding the resolved path
    cmp %1, -4095
    jb %%ok
    mov rdi, %1
    neg rdi
    mov rsi, %2
    mov rdx, %3
    call raise_oserror_owned    ; does not return
%%ok:
    POSIX_PATH_DONE %3
%endmacro

%endif ; POSIXPATH_INC
