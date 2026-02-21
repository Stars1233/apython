# apython

A Python 3.12 bytecode interpreter in x86-64 NASM assembly (v0.6.0), exploring the fastest single-core Python execution on x86-64.

## What is this?

apython reads `.pyc` files and executes Python 3.12 bytecode directly — no CPython, no JIT, no interpreter overhead layers. The entire interpreter is **~74,000 lines of hand-written x86-64 assembly**, from the eval loop to the type system to the garbage collector to async I/O. It implements 27+ types, 106 opcodes, generators, async/await, pattern matching, a regex engine, cycle-collecting GC, and a pure-assembly asyncio event loop.

## Key design choices

- **~74K lines of focused x86-64 NASM assembly** — no C runtime
- **128-bit fat values** — inline integers, floats, bools and small strings in 16-byte (payload, tag) pairs, skipping heap allocation and refcounting entirely
- **SmallStr optimization** — strings up to 15 bytes stored inline in the 128-bit value slot, zero allocation
- **Raw Linux syscalls** — no libc dependency for I/O; buffered writes via direct `syscall`
- **256-entry jump table dispatch** — x86-BTB-friendly single indirect jump per opcode
- **GMP for arbitrary precision** — big integers via libgmp when values exceed SmallInt range
- **Reference counting + cycle-collecting GC** — deterministic memory management with a 3-generation collector for cycles
- **Full async/await with io_uring** — high-speed async I/O via Linux io_uring (with epoll fallback), zero-copy TCP streams
- **DWARF debug symbols** — full GDB support with frame-pointer unwinding, function boundaries, and source-level stepping

## Quick start

**Dependencies:** nasm, gcc (linker), libgmp-dev, python3.12

```bash
make                # build ./apython
./apython --version # show version

# run a Python script
python3 -m py_compile script.py
./apython __pycache__/script.cpython-312.pyc
```

## Implemented features

### Types (27+)

| Category | Types |
|----------|-------|
| Numeric | int, float, bool, None |
| Sequences | str, bytes, bytearray, memoryview, list, tuple |
| Collections | dict, set, frozenset |
| Iterators | range, slice, iterator, generator, coroutine, async_generator |
| Callables | function, method, builtin_function, code, staticmethod, classmethod, property |
| Runtime | type, object, module, cell, exception, traceback, file |

### Opcodes (106)

| Category | Opcodes |
|----------|---------|
| Load | LOAD_CONST, LOAD_FAST, LOAD_FAST_CHECK, LOAD_FAST_AND_CLEAR, LOAD_GLOBAL, LOAD_NAME, LOAD_ATTR, LOAD_DEREF, LOAD_CLOSURE, LOAD_LOCALS, LOAD_BUILD_CLASS, LOAD_SUPER_ATTR, LOAD_FROM_DICT_OR_DEREF, LOAD_FROM_DICT_OR_GLOBALS |
| Store | STORE_FAST, STORE_GLOBAL, STORE_NAME, STORE_ATTR, STORE_DEREF, STORE_SUBSCR, STORE_SLICE |
| Delete | DELETE_FAST, DELETE_GLOBAL, DELETE_NAME, DELETE_ATTR, DELETE_DEREF, DELETE_SUBSCR |
| Stack | POP_TOP, PUSH_NULL, COPY, SWAP, NOP, CACHE |
| Arithmetic | BINARY_OP (+specialized int add/sub), UNARY_NEGATIVE, UNARY_NOT, UNARY_INVERT, BINARY_SUBSCR, BINARY_SLICE |
| Comparison | COMPARE_OP (+specialized int), IS_OP, CONTAINS_OP |
| Control flow | JUMP_FORWARD, JUMP_BACKWARD, JUMP_BACKWARD_NO_INTERRUPT, POP_JUMP_IF_TRUE, POP_JUMP_IF_FALSE, POP_JUMP_IF_NONE, POP_JUMP_IF_NOT_NONE |
| Functions | MAKE_FUNCTION, CALL, CALL_FUNCTION_EX, CALL_INTRINSIC_1, CALL_INTRINSIC_2, KW_NAMES, RETURN_VALUE, RETURN_CONST, RETURN_GENERATOR, RESUME, COPY_FREE_VARS, MAKE_CELL |
| Iteration | GET_ITER, FOR_ITER (+specialized list/range), END_FOR, GET_LEN |
| Containers | BUILD_TUPLE, BUILD_LIST, BUILD_MAP, BUILD_SET, BUILD_SLICE, BUILD_STRING, BUILD_CONST_KEY_MAP, LIST_APPEND, LIST_EXTEND, SET_ADD, SET_UPDATE, MAP_ADD, DICT_MERGE, DICT_UPDATE, UNPACK_SEQUENCE, UNPACK_EX |
| Formatting | FORMAT_VALUE |
| Pattern matching | MATCH_MAPPING, MATCH_SEQUENCE, MATCH_KEYS, MATCH_CLASS |
| Import | IMPORT_NAME, IMPORT_FROM |
| Async | GET_AWAITABLE, GET_AITER, GET_ANEXT, GET_YIELD_FROM_ITER, SEND, END_SEND, YIELD_VALUE, CLEANUP_THROW, END_ASYNC_FOR, BEFORE_ASYNC_WITH |
| With/Annotations | BEFORE_WITH, WITH_EXCEPT_START, SETUP_ANNOTATIONS |

### Builtins (49 functions + 17 types + 31 exceptions)

**Functions:**
print, len, repr, abs, round, pow, divmod, sum, min, max, any, all,
hash, id, ord, chr, hex, bin, oct, ascii, format, input, eval, open,
range, enumerate, zip, map, filter, reversed, sorted,
type, isinstance, issubclass, callable, super,
iter, next, aiter, anext,
getattr, hasattr, setattr, delattr, vars, dir,
globals, locals, \_\_build\_class\_\_, \_\_import\_\_

**Types:**
int, float, str, bool, object, list, dict, tuple, set, frozenset,
bytes, bytearray, memoryview, slice, staticmethod, classmethod, property

**Exceptions (31):**
BaseException, Exception, TypeError, ValueError, KeyError, IndexError,
AttributeError, NameError, RuntimeError, StopIteration, StopAsyncIteration,
ZeroDivisionError, NotImplementedError, OverflowError, AssertionError,
OSError, LookupError, ArithmeticError, RecursionError, ImportError,
MemoryError, UnicodeError, TimeoutError, KeyboardInterrupt, SystemExit,
Warning, DeprecationWarning, UserWarning,
BaseExceptionGroup, ExceptionGroup, CancelledError

### Language features

- Classes with inheritance, `__init__`, `__repr__`, `__str__`, `__slots__`, MRO
- Generators and `yield` / `yield from`
- `async def`, `await`, `async for`, `async with`
- Closures and nested scopes (`LOAD_DEREF` / `STORE_DEREF`)
- Decorators (`@staticmethod`, `@classmethod`, `@property`, user-defined)
- List/dict/set/generator comprehensions
- f-strings and `format()`
- Pattern matching (`match`/`case` with mapping, sequence, class patterns)
- Exception groups and `except*`
- `with` statements (context managers)
- `*args`, `**kwargs`, keyword-only arguments
- Extended slicing (`a[1:10:2]`, `a[::-1]`)
- `from module import *`
- Multiple inheritance, `super()`

### Modules

| Module | Description |
|--------|-------------|
| sys | argv, exit, version, path, modules, stdin/stdout/stderr, exc_info, maxsize |
| asyncio | Event loop with io_uring backend, coroutine runner, TCP streams (open_connection, start_server), sleep, gather |
| re | SRE regex engine — compile, match, search, findall, finditer, sub, split |
| time | time, sleep, monotonic |
| itertools | chain, islice, count, repeat, zip_longest, product, permutations, combinations, starmap, takewhile, dropwhile, filterfalse, accumulate, groupby, tee, pairwise |
| unittest | Pure Python test framework (TestCase, assertions, test runner) |
| warnings | warn, simplefilter |

### Garbage collection

3-generation cycle-collecting GC with traverse/clear protocols for all container types. Generational thresholds match CPython defaults. Handles reference cycles in dicts, lists, tuples, sets, classes, generators, frames, and closures.

## Test suite

**114 tests** covering arithmetic, strings, lists, dicts, tuples, sets, booleans, None, bytes, floats, comparisons, control flow, functions, recursion, for-loops, while-loops, range, classes, inheritance, generators, async/await, closures, decorators, comprehensions, f-strings, exceptions, pattern matching, slicing, `*args`/`**kwargs`, `with` statements, imports, itertools, and more.

**9 CPython compatibility tests** from the CPython standard library: augmented assignment, booleans, enumerate, floats, integers, keyword-only args, sorting, string methods, and string operators.

```bash
make check          # run all 114 tests (diff python3 vs ./apython output)
make check-cpython  # run 9 CPython stdlib tests
```

All tests are Valgrind-clean.

## Project structure

```
src/
  main.asm              Entry point, --version
  eval.asm              Bytecode dispatch loop (256-entry jump table)
  opcodes_load.asm      Load opcodes
  opcodes_store.asm     Store opcodes
  opcodes_stack.asm     Stack manipulation opcodes
  opcodes_call.asm      Call/function opcodes
  opcodes_build.asm     Container build opcodes
  opcodes_misc.asm      Comparison, control flow, format, pattern matching
  opcodes_async.asm     Async/await opcodes
  opcodes_import.asm    Import opcodes
  builtins.asm          Built-in functions and type registry
  builtins_extra.asm    Additional builtins (itertools constructors, etc.)
  marshal.asm           .pyc marshal deserializer
  pyc.asm               .pyc file reader
  frame.asm             Frame allocation/deallocation
  object.asm            Base PyObject operations, type_type
  memory.asm            Memory management
  error.asm             Error handling and tracebacks
  except.asm            Exception machinery
  gc.asm                3-generation cycle-collecting garbage collector
  import.asm            Module import system
  dunder.asm            Dunder method dispatch (__add__, __eq__, etc.)
  repr.asm              repr/str formatting
  val.asm               128-bit fat value operations
  methods.asm           Method resolution helpers
  sre.asm               SRE regex bytecode engine
  sre_module.asm        re module interface
  itertools.asm         itertools module
  pyo/                  34 type implementation files
    int.asm float.asm str.asm bytes.asm bytearray.asm memview.asm
    list.asm dict.asm tuple.asm set.asm bool.asm none.asm slice.asm
    func.asm class.asm code.asm module.asm cell.asm smallstr.asm
    iter.asm generator.asm exception.asm exc_group.asm fileobj.asm
    descriptors.asm sre_match.asm sre_pattern.asm
    sysmod.asm asyncmod.asm timemod.asm
    eventloop.asm eventloop_poll.asm eventloop_iouring.asm
    asyncio_streams.asm
  lib/                  Syscall wrappers, string/memory ops
    syscall.asm memops.asm string.asm
include/                Struct definitions, macros, constants (.inc files)
lib/                    Pure Python support modules
  unittest/             Test framework (case.py, runner.py, mock.py)
  warnings.py           Warnings module
  test/                 CPython test support infrastructure
tests/                  114 test files + 9 CPython compatibility tests
```

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
| `make check` | Run 114-test suite |
| `make check-cpython` | Run 9 CPython compatibility tests |
| `make clean` | Remove build artifacts |

## License

MIT — see [LICENSE](LICENSE) for details.
