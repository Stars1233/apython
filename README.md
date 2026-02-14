# apython

A Python 3.12 bytecode interpreter in x86-64 NASM assembly, exploring the fastest single-core Python execution on x86-64.

## What is this?

apython reads `.pyc` files and executes Python 3.12 bytecode directly — no CPython, no JIT, no interpreter overhead layers. The entire interpreter is hand-written x86-64 assembly, from the eval loop to the type system to I/O.

## Key design choices

- **~10K lines of hand-written x86-64 NASM assembly** — no C runtime, no generated code
- **SmallInt tagged pointers** — bit 63 flags inline integers, skipping heap allocation and refcounting entirely
- **Raw Linux syscalls** — no libc dependency for I/O; buffered writes via direct `syscall`
- **256-entry jump table dispatch** — single indirect jump per opcode
- **GMP for arbitrary precision** — big integers via libgmp when values exceed tagged pointer range
- **Reference counting** — deterministic memory management with tagged pointer awareness

## Quick start

**Dependencies:** nasm, gcc (linker), libgmp-dev, python3.12

```bash
make                # build ./apython

# run a Python script
python3 -m py_compile script.py
./apython __pycache__/script.cpython-312.pyc
```

## Implemented features

**Types (12):** int, str, list, dict, tuple, bool, None, bytes, function, class, code, iterator

**Opcodes (46):**

| Category | Opcodes |
|----------|---------|
| Load/Store | LOAD_CONST, LOAD_FAST, LOAD_GLOBAL, LOAD_NAME, LOAD_ATTR, STORE_FAST, STORE_GLOBAL, STORE_NAME, STORE_ATTR |
| Stack | POP_TOP, PUSH_NULL, COPY, SWAP, NOP, CACHE |
| Arithmetic | BINARY_OP, UNARY_NEGATIVE, UNARY_NOT |
| Comparison | COMPARE_OP, IS_OP, CONTAINS_OP |
| Control flow | JUMP_FORWARD, JUMP_BACKWARD, POP_JUMP_IF_TRUE, POP_JUMP_IF_FALSE, POP_JUMP_IF_NONE, POP_JUMP_IF_NOT_NONE |
| Functions | MAKE_FUNCTION, CALL, RETURN_VALUE, RETURN_CONST, RESUME, INTERPRETER_EXIT |
| Iteration | GET_ITER, FOR_ITER, END_FOR |
| Containers | BUILD_TUPLE, BUILD_LIST, BUILD_MAP, BUILD_CONST_KEY_MAP, LIST_APPEND, LIST_EXTEND, UNPACK_SEQUENCE, BINARY_SUBSCR, STORE_SUBSCR |
| Classes | LOAD_BUILD_CLASS |

**Builtins (7):** print, len, range, type, isinstance, repr, \_\_build\_class\_\_

## Project structure

```
src/
  eval.asm              Bytecode dispatch loop (256-entry jump table)
  opcodes_*.asm         Opcode handlers by category
  builtins.asm          Built-in functions
  marshal.asm           .pyc marshal deserializer
  pyc.asm               .pyc file reader
  frame.asm             Frame allocation/deallocation
  object.asm            Base PyObject operations
  memory.asm            Memory management
  error.asm             Error handling
  main.asm              Entry point
  pyo/                  Type implementations (int, str, list, dict, tuple, ...)
  lib/                  Syscall wrappers, string/memory ops
include/                Struct definitions, macros, constants
tests/                  17 test files (Python scripts)
```

## Testing

```bash
make check    # run full test suite
```

Each test compiles a `.py` file to `.pyc`, runs it through both `python3` and `./apython`, and diffs the output. 17 test files cover arithmetic, strings, lists, dicts, tuples, booleans, None, comparisons, control flow, functions, recursion, for-loops, range, classes, membership, and unary ops.

## Building

**Dependencies:**
- `nasm` — assembler
- `gcc` — linker
- `libgmp-dev` — arbitrary precision integers
- `python3.12` — compiling test `.py` files to `.pyc`

**Make targets:**
| Target | Description |
|--------|-------------|
| `make` | Build `./apython` |
| `make check` | Run full test suite |
| `make clean` | Remove build artifacts |

## License

MIT — see [LICENSE](LICENSE) for details.
