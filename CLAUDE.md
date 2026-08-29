# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Python 3.12 bytecode interpreter in x86-64 NASM assembly. Reads `.pyc` files and executes bytecode directly.

## Build & Test

```bash
make              # build ./apython
make clean        # remove build/ and apython
make check        # full test suite: compile .py→.pyc, diff python3 vs ./apython output
make check-cpython # CPython stdlib unit tests (harder, more thorough)
```

**Always run BOTH `make check` AND `make check-cpython` to verify changes.**

Two more gates worth running when touching the value representation:

```bash
./apython --selftest-value      # encode/decode boundaries: +-2^50, +-0.0, inf, NaN, subnormals
make INT_STRESS=1 && bash tests/run_tests.sh
```

`INT_STRESS=1` boxes every integer with `|n| >= 8` onto the heap, so the
ordinary suite exercises the heap-int paths that ±2^50 immediates normally
hide.  It is not expected to pass `check-cpython` (CPython's own test_int
asserts things like `10 is 10`).

**Single test:**
```bash
python3 -m py_compile tests/test_foo.py
python3 tests/test_foo.py > /tmp/expected.txt
./apython tests/__pycache__/test_foo.cpython-312.pyc > /tmp/actual.txt
diff /tmp/expected.txt /tmp/actual.txt
```

**Dependencies:** nasm, gcc (linker), libgmp-dev, python3.12

## Register Convention (eval loop)

Callee-saved registers hold global interpreter state:

| Register | Role |
|----------|------|
| `rbx` | Bytecode IP (into co_code[]) |
| `r12` | Current frame (PyFrame*) |
| `r13` | Value stack top (Value[], one 64-bit word per slot) |
| `r14` | co_consts data ptr (&tuple.ob_item[0]) |
| `r15` | Free — scratch for handlers |
| `ecx` | Opcode arg on handler entry |

co_names is accessed via the `LOAD_CO_NAMES reg` macro (reads the `eval_co_names` global), not a dedicated register.

**Critical rule:** Never hold live values in caller-saved regs (rax, rcx, rdx, rsi, rdi, r8-r11) across `call` or `DECREF`/`DECREF_REG`. Use push/pop or callee-saved regs instead. `DECREF_REG` calls `obj_dealloc` which clobbers all caller-saved regs.

## Value Representation (NaN boxing)

**One 64-bit word per Python value.** The encoding lives in `include/value.inc`;
`src/val.asm` holds the rip-relative constant pool it compares against, and
`./apython --selftest-value` exercises the boundaries.

Let `high16 = v >> 48`:

| `high16` | Meaning | Decode |
|---|---|---|
| `0x0000` | `PyObject*`, stored **raw** (`v == 0` is NULL) | use as-is |
| `0x0001`–`0xFFF1` | float64 | `bits = v - V_F64_OFF` (2^48) |
| `0xFFF2` | async SLEEP sentinel | `delay_ns = v & V_MASK48` |
| `0xFFF3` | async IO_WAIT sentinel | `fd \| dir<<32 = v & V_MASK48` |
| `0xFFF4`–`0xFFF7` | reserved | — |
| `0xFFF8`–`0xFFFF` | int immediate, range ±2^50 | `i = v - V_INT_BIAS` |

Linux x86-64 user addresses are below 2^47, so a pointer needs no masking and
`NULL == 0` still works with a plain `test`.  Doubles pay one add on the way in
and one sub on the way out; negative NaNs at or above `V_NAN_LIM` (x86's default
QNaN among them) are canonicalised to `0x7FF8000000000000` first, which CPython
cannot observe.  Integers outside ±2^50 are boxed into a `PyIntObject`, which
carries a compact `ival` and only initialises its `mpz_t` on overflow.

None, True and False are the ordinary heap singletons — a pointer is its own
Value, so they need no encoding of their own.

Everything stores one word: the value stack (`r13`), `localsplus`, `ob_item` for
list and tuple, `DictEntry.key`/`.value`, the per-object fields, and the
`tp_call` argument array.

Classification macros (all in `include/value.inc`): `V_TEST_PTR`, `V_IS_INT`,
`V_IS_FLOAT`, and the `_M` variants that take a memory operand.  Conversion:
`V_FROM_F64` / `V_TO_F64`, `V_PACK_I64` / `V_TO_I64`.  Refcounting: `INCREF_V`,
`DECREF_V`, `XDECREF_V` — one compare and one branch, NULL-safe.

`V_PACK` / `V_UNPACK` convert to and from the old `(payload, tag)` pair.  They
are migration scaffolding: the tags (`TAG_NULL`, `TAG_SMALLINT`, `TAG_FLOAT`,
`TAG_PTR`, …) survive only inside functions that have not been converted yet,
and at the boundaries between converted and unconverted code.

## Source Layout

- `src/eval.asm` — Bytecode dispatch loop (256-entry jump table)
- `src/opcodes_*.asm` — Opcode handlers by category (load, store, stack, call, build, misc)
- `src/pyo/*.asm` — Type implementations (int, str, list, dict, tuple, func, class, iter, bool, none, bytes, code)
- `src/marshal.asm` — .pyc marshal format deserializer
- `src/pyc.asm` — .pyc file reader (magic validation, header parsing)
- `src/builtins.asm` — Built-in functions (print, len, range, type, isinstance, etc.)
- `src/frame.asm` — Frame alloc/dealloc
- `src/object.asm` — Base PyObject ops (alloc, refcount, dealloc)
- `src/lib/` — Syscall wrappers, string/memory ops (replace libc)
- `include/` — Struct definitions (.inc): object, types, frame, opcodes, macros, marshal, builtins, errcodes

## Key Structs

Defined in `include/*.inc`. All objects start with `PyObject` (ob_refcnt +0, ob_type +8).

- **PyTypeObject** (types.inc, 192 bytes): tp_call +64, tp_getattr +72, tp_setattr +80, tp_as_number +128, tp_as_sequence +136, tp_as_mapping +144
- **PyFrame** (frame.inc): code +8, globals +16, locals +32, stack_ptr +48, stack_base +56, localsplus +80 (variable-size Value[])
- **PyIntObject** (object.inc): mpz +16 (only initialised on overflow), ival +32, compact +40 (1 = the ival is live)
- **DictEntry** (object.inc, 24 bytes): hash +0, key +8, value +16 — occupied ⇔ `key != 0`; empty ⇔ `key == 0 && hash == 0`; tombstone ⇔ `key == 0 && hash == -1`
- **PyCodeObject** (object.inc): co_consts, co_names, co_code starts at +112

## Opcode Handler Pattern

```nasm
op_example:
    ; ecx = arg (already set by eval_dispatch)
    ; rbx already advanced past 2-byte instruction word
    ; ... implementation ...
    DISPATCH          ; jmp eval_dispatch
```

Stack macros: `VPUSH reg` (an encoded Value), `VPUSH_PTR reg`, `VPUSH_INT reg, scratch`, `VPUSH_FLOAT reg, scratch`, `VPUSH_NONE`, `VPUSH_BOOL reg`, `VPUSH_NULL`, `VPOP reg`, `VPEEK reg`.  `VPUSH_VAL` / `VPOP_VAL` are the (payload, tag) shims.

## Named Frame-Layout Constants

**Never use raw numeric offsets** like `[rbp-8]`, `[rbp-16]`, `[rsp+32]` in handler code. Instead, define named `equ` constants at the top of the file and reference them as `[rbp - SA_OBJ]`, `[rsp + BO_LEFT]`, etc.

```nasm
; At top of file, after externs:
SA_OBJ    equ 8
SA_VAL    equ 16
SA_NAME   equ 24
SA_FRAME  equ 24

; In handler:
DEF_FUNC op_store_attr, SA_FRAME
    mov [rbp - SA_OBJ], rdi
    mov rsi, [rbp - SA_NAME]
```

Convention: 2-3 letter handler prefix + field name (e.g., `SA_OBJ`, `CL_NARGS`, `LA_ATTR`). Use `XX_FRAME equ N` for the `DEF_FUNC` frame size argument. For push-based layouts, use offsets relative to `rsp`.

## Python 3.12 CACHE Entries

Opcodes have trailing CACHE words that must be skipped. Key counts (each = 2 bytes):

| Opcode | CACHE entries | Skip bytes |
|--------|--------------|------------|
| LOAD_ATTR | 9 | 18 |
| STORE_ATTR | 4 | 8 |
| CALL | 3 | 6 |
| BINARY_OP | 1 | 2 |
| COMPARE_OP | 1 | 2 |

## Known Bug Patterns

- **Marshal FLAG_REF ordering:** Container types must reserve ref slot BEFORE reading children (r_ref_reserve/r_ref_insert pattern). See marshal.asm.
- **func_call r12 assumption:** func_call assumes r12 = caller's frame. When called from type_call (which overwrites r12), must restore r12 from stack.
- **DECREF clobber:** DECREF_REG contains `call obj_dealloc`. Any value in caller-saved regs is destroyed if refcount hits zero.
- **Double encode/decode:** a function that packs at its exit must not be reached by a tail `jmp` from another that also packs, and a call site must not decode a result its callee already handed over as a Value. Both show up as a value off by exactly 2^48 (floats) or by V_INT_BIAS (ints), not as a crash.
- **Raw payload use after conversion:** once a slot holds a Value, reading it and using it as an int or as raw double bits needs `V_TO_I64` / `V_TO_F64` first. Pointers are the exception — a pointer is its own Value — which is why pointer-only code survived the conversion untouched and non-pointer code did not.
- **Boxing in V_PACK:** `V_PACK` on a TAG_SMALLINT outside ±2^50 allocates a heap int. That is correct but it is an allocation, and the returned reference is owned — do not pack a borrowed integer payload and drop it.

## Adding a New Test

Create `tests/test_feature.py` using only implemented Python features. `make check` auto-discovers `test_*.py` files.

## Debug Strategy

Build includes DWARF symbols (`-g -F dwarf`) and ELF function metadata (STT_FUNC type + size via `DEF_FUNC`/`END_FUNC` macros). All functions use RBP frame pointers, enabling GDB frame-pointer-based unwinding. Zero runtime overhead.

**What works in GDB:**
- `bt` — full backtraces via RBP chain
- `break func_name` — breakpoints on any global function
- `info functions` — lists all functions with correct boundaries
- `disassemble func_name` — disassembly with proper function bounds
- `step`/`next`/`finish` — source-level stepping (maps to .asm lines)
- `info registers` — inspect VM state (rbx=bytecode IP, r12=frame, r13=stack top, r14=consts, r15=names)

**GDB quick start:**
```
gdb ./apython
break eval_frame
run tests/__pycache__/test_foo.cpython-312.pyc
bt                    # backtrace
info registers        # VM state: rbx, r12-r15
print (char*)[r12+8]  # inspect frame->code
break str_from_cstr   # break on runtime function
continue
```

**VM register inspection in GDB:**

| Expression | Meaning |
|------------|---------|
| `$rbx` | Current bytecode IP |
| `$r12` | Current PyFrame* |
| `$r13` | Value stack top |
| `$r14` | co_consts data ptr |
| `$r15` | co_names data ptr |

**Function definition macros** (include/macros.inc):
- `DEF_FUNC name` — global function with RBP frame (push rbp + mov rbp,rsp)
- `DEF_FUNC name, N` — same + allocate N bytes of local space
- `DEF_FUNC_BARE name` — global function, no prologue (opcode handlers, leaf functions)
- `DEF_FUNC_LOCAL name` — file-local function with RBP frame
- `END_FUNC name` — marks function end (required, emits .end label for ELF size)

Write debug scripts to `/tmp/` and run with `bash /tmp/script.sh`.
