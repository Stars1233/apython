# x86-64 Peephole Optimization Guide

Target: modern x86-64 (Haswell+/Zen2+). 100% Python 3.12 correctness required.

## 5. SSE for Bulk Stack Operations

Value stack slots are 16-byte aligned. Use `movdqa` (aligned 128-bit load/store)
for COPY and SWAP:

```nasm
; COPY: one 16-byte load + one 16-byte store (vs two 8-byte pairs)
op_copy:
    ; compute source slot address
    movdqa xmm0, [rsi]         ; load 128-bit slot (aligned)
    movdqa [r13], xmm0          ; store to TOS
    add r13, 16
    DISPATCH

; SWAP: two loads, two stores, zero GPR register pressure
    movdqa xmm0, [r13 - 16]    ; load TOS
    movdqa xmm1, [rsi]         ; load other
    movdqa [r13 - 16], xmm1    ; store other -> TOS
    movdqa [rsi], xmm0         ; store TOS -> other
```

SSE state is saved/restored by the kernel across context switches. No extra cost.

## 6. Superinstructions

Fuse common opcode pairs into single handlers. Reserve unused opcode slots
(200-255). Patch bytecode at load time in marshal.asm.

| Pair | Frequency | Savings |
|------|-----------|---------|
| LOAD_FAST + LOAD_FAST | Very high | 1 dispatch |
| LOAD_FAST + LOAD_ATTR | Very high | 1 dispatch |
| LOAD_CONST + RETURN_VALUE | High | 1 dispatch |
| STORE_FAST + LOAD_FAST | High | 1 dispatch |
| COMPARE_OP + POP_JUMP_IF_FALSE | Very high | 1 dispatch + stack round-trip |

The COMPARE_OP + POP_JUMP_IF_FALSE fusion is the highest-value superinstruction
because it eliminates the intermediate push/pop of the boolean result:

```nasm
op_compare_pop_jif:
    ; pop two values, compare, branch directly
    VPOP_VAL rsi, r9            ; right
    VPOP_VAL rdi, r8            ; left
    ; ... compare ...
    ; Branch directly on result, never push True/False
    test eax, eax
    jz .take_branch
    add rbx, 6                  ; skip CACHE + POP_JUMP_IF_FALSE
    DISPATCH
.take_branch:
    movzx ecx, byte [rbx+3]    ; POP_JUMP_IF_FALSE's arg
    lea rbx, [rbx + rcx*2]
    DISPATCH
```

Requires bytecode patching infrastructure: scan code objects after marshal,
detect fusible pairs, replace first opcode with super-opcode and NOP the second.

## 7. Data Structure Layout

### Dict entries: consider padding to 32 bytes

Current DictEntry = 24 bytes (hash + key + value). Requires `imul rdx, rcx, 24`
in every probe (3-cycle latency).

Padded to 32 bytes: indexing becomes `shl rcx, 5` (1 cycle). Two entries per
cache line, no straddling. 33% more memory but eliminates imul from hot probe
loop. Extra 8 bytes could store probe distance (Robin Hood hashing) or cached
key length for fast rejection.

### PyTypeObject: move tp_call to a hotter cache line

`tp_call` is at offset +64 — second cache line from the type pointer. When a
type is loaded via `ob_type`, the first cache line (bytes 0-63) is fetched.
`tp_call` requires a second cache line read.

Moving `tp_call` to offset +24 or +32 (after tp_name) would put it in the same
cache line as the type pointer. Types are called far more often than repr'd.

## 8. Encoding Micro-Optimizations

### Use `xor eax, eax` not `xor rax, rax`

2 bytes vs 3 bytes. Both zero the full 64-bit register (Intel implicitly
zero-extends 32-bit ops to 64-bit). Audit for stragglers.

### Use `test eax, eax` not `test rax, rax` (where safe)

2 bytes vs 3 bytes. Safe when value is known to fit in 32 bits.

### `mov rax, 0` → `xor eax, eax`

`mov rax, 0` is 7 bytes and doesn't break dependency chains. `xor eax, eax` is
2 bytes and is recognized by the CPU as a zeroing idiom (breaks false
dependencies, doesn't consume a physical register on recent microarchitectures).

### `inc`/`dec` vs `add/sub 1`

On Haswell+ and Zen+, `inc`/`dec` no longer cause partial-flags stalls. Use
freely — 1 byte shorter than `add reg, 1`.

### `movzx eax, byte [mem]` not `movzx rax, byte [mem]`

Same result (zero-extends to 64-bit), shorter encoding.

## 9. What NOT To Do

### Don't add AVX2 for general memcpy

`rep movsb` is already ERMS-optimized on Ivy Bridge+ (2012). The microcode
unfolds it to cache-line-width transfers. Explicit AVX2 loops are harder to
maintain and only win on very specific AMD microarchitectures for medium sizes
(256-4096 bytes). Not worth the complexity.

### Don't use SIMD for refcounting

The branch in INCREF/DECREF is necessary to guard the memory access. Can't
blindly dereference a SmallInt payload as a pointer. Branch predictor handles
this well because TAG_PTR is the dominant case.

### Don't pad DictEntry without profiling

The 33% memory increase matters for large dicts. Measure first.

### Don't use segment registers for the opcode table base

`lea rdx, [rel opcode_table]` is 7 bytes and 1 cycle. Segment override
prefixes add decoder complexity and save nothing.

### Don't try to keep TOS in a register

All callee-saved registers are occupied (rbx=IP, r12=frame, r13=stack,
r14=consts, r15=names). Using a caller-saved register for TOS fails because
any `call` or DECREF clobbers it. Reclaiming a callee-saved register (e.g.,
r14/r15) would mean reloading consts/names from the frame on every access —
net loss.

## Priority Order

| # | Optimization | Effort | Expected Gain |
|---|-------------|--------|---------------|
| 1 | movdqa for COPY/SWAP | Trivial | 2 instructions -> 1 |
| 2 | COMPARE+POP_JUMP superinstruction | Medium | eliminates stack round-trip |
