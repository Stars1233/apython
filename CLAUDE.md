# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Python 3.12 bytecode interpreter in x86-64 NASM assembly. Reads `.pyc` files and executes bytecode directly.

## Build & Test

```bash
make              # build ./apython
make clean        # remove build/ and apython
make check        # full test suite: compile .py→.pyc, diff python3 vs ./apython output
```

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
| `r13` | Value stack top |
| `r14` | co_consts data ptr (&tuple.ob_item[0]) |
| `r15` | co_names data ptr (&tuple.ob_item[0]) |
| `ecx` | Opcode arg on handler entry |

**Critical rule:** Never hold live values in caller-saved regs (rax, rcx, rdx, rsi, rdi, r8-r11) across `call` or `DECREF`/`DECREF_REG`. Use push/pop or callee-saved regs instead. `DECREF_REG` calls `obj_dealloc` which clobbers all caller-saved regs.

## SmallInt Tagged Pointers

Bit 63 set = inline integer (range -2^62 to 2^62-1). All `INCREF`/`DECREF` macros test bit 63 and skip refcounting for SmallInts. Encode: `bts rax, 63`. Decode: `shl rax, 1` / `sar rax, 1`.

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
- **PyFrame** (frame.inc): code +8, globals +16, locals +32, localsplus +72 (variable-size)
- **PyCodeObject** (object.inc): co_consts, co_names, co_code starts at +112

## Opcode Handler Pattern

```nasm
op_example:
    ; ecx = arg (already set by eval_dispatch)
    ; rbx already advanced past 2-byte instruction word
    ; ... implementation ...
    DISPATCH          ; jmp eval_dispatch
```

Stack macros: `VPUSH reg`, `VPOP reg`, `VPEEK reg`, `VPEEK_AT reg, offset`

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

## Adding a New Test

Create `tests/test_feature.py` using only implemented Python features. `make check` auto-discovers `test_*.py` files.

## Debug

Build includes DWARF symbols (`-g -F dwarf`). GDB: `gdb ./apython` then `run tests/__pycache__/test_foo.cpython-312.pyc`. Write debug scripts to `/tmp/` and run with `bash /tmp/script.sh`.
