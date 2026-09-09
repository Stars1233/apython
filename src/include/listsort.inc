; listsort.inc - the private ABI between list.asm's sort method and the
; timsort engine in methods/list_sort.asm.
;
; The MergeState is nearly two kilobytes and lives in the CALLER's frame, not
; in a global: a comparator can run Python that sorts another list, and each
; sort needs its own.  That is the only reason this layout is shared, and it
; is why it is here rather than private to the engine.

%ifndef LISTSORT_INC
%define LISTSORT_INC

; When a run wins this many times in a row, galloping is likely to pay.
ST_MIN_GALLOP     equ 7

; A natural run shorter than minrun is extended to it by binary insertion
; sort, and minrun itself is capped here so the insertion sort stays cheap.
ST_MAX_MINRUN     equ 64

; The run stack.  The merge invariant makes the lengths grow at least as fast
; as the Fibonacci numbers, so the depth is bounded by log_phi(n) -- about
; 1.44 * log2(n), which is 92 for a list that fills the address space.  128 is
; that with room to spare.  (CPython 3.12 gets away with one entry per bit of
; a size_t because it uses powersort, whose bound really is log2(n) + 1.)
ST_MAX_PENDING    equ 128

MS_MINGALLOP equ 0      ; the adaptive threshold, shared across merges
MS_KEYS      equ 8      ; the array being sorted
MS_VALUES    equ 16     ; the array moved in lockstep, or 0
MS_TKEYS     equ 24     ; scratch for keys
MS_TVALS     equ 32     ; scratch for values, or 0
MS_ALLOCED   equ 40     ; scratch capacity, in elements
MS_NRUNS     equ 48
MS_CMP       equ 56     ; the comparator chosen by the pre-sort scan
MS_ELEMCMP   equ 64     ; and, for a list of tuples, the one for element 0
                        ; 72 is padding: the pending pairs are 16 bytes each
                        ; and start on a 16-byte boundary
MS_PENDING   equ 80     ; (base index, length) pairs
MS_SIZE      equ MS_PENDING + ST_MAX_PENDING * 16

%endif ; LISTSORT_INC
