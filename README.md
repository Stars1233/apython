# apython

A Python 3.12 bytecode interpreter in x86-64 NASM assembly, exploring the fastest x86 single-core Python execution, with a focus on floating point and integer performance.

## What is this?

apython reads `.pyc` files and executes Python 3.12 bytecode directly — no CPython, no JIT, no interpreter overhead layers. The entire interpreter is **~86,000 lines of x86-64 assembly**, from the eval loop to the type system to the garbage collector to async I/O. It implements 27+ types, 126 opcode handlers, generators, async/await, multiple inheritance with a C3 MRO, metaclasses, abstract base classes, weak references, pattern matching, real tracebacks, a regex engine, cycle-collecting GC, and a pure-assembly asyncio event loop.  Strings hold UTF-8 and count themselves in code points.

## Key design choices

- **~86K lines of focused x86-64 NASM assembly** — no C runtime
- **NaN-boxed 64-bit values** — one machine word per Python value. Pointers are stored raw (so a dereference costs nothing), floats are offset-encoded into the NaN space, and integers in ±2^50 are immediates — no heap allocation and no refcounting for any of them
- **Raw Linux syscalls** — no libc dependency for I/O; buffered writes via direct `syscall`
- **256-entry jump table dispatch** — x86-BTB-friendly single indirect jump per opcode
- **GMP for arbitrary precision** — big integers via libgmp when values exceed int64_t range
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

### Opcodes (126 handlers)

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
| Exceptions | RAISE_VARARGS, RERAISE, PUSH_EXC_INFO, POP_EXCEPT, CHECK_EXC_MATCH, CHECK_EG_MATCH |
| Formatting | FORMAT_VALUE |
| Pattern matching | MATCH_MAPPING, MATCH_SEQUENCE, MATCH_KEYS, MATCH_CLASS |
| Import | IMPORT_NAME, IMPORT_FROM |
| Async | GET_AWAITABLE, GET_AITER, GET_ANEXT, GET_YIELD_FROM_ITER, SEND, END_SEND, YIELD_VALUE, CLEANUP_THROW, END_ASYNC_FOR, BEFORE_ASYNC_WITH |
| With/Annotations | BEFORE_WITH, WITH_EXCEPT_START, SETUP_ANNOTATIONS |
| Wide operands | EXTENDED_ARG |

Sixteen of these are specialized forms the interpreter rewrites into the
bytecode on first execution — `BINARY_OP_ADD_INT`, `COMPARE_OP_INT_JUMP_TRUE`,
`FOR_ITER_LIST`, `LOAD_ATTR_METHOD`, `LOAD_GLOBAL_MODULE` and their siblings —
each guarded so an operand of the wrong shape falls back to the general
handler.

### Builtins (51 functions + 18 types + 64 exceptions)

**Functions:**
print, len, repr, abs, round, pow, divmod, sum, min, max, any, all,
hash, id, ord, chr, hex, bin, oct, ascii, format, input, eval, open,
range, enumerate, zip, map, filter, reversed, sorted, chain,
isinstance, issubclass, callable, super,
iter, next, aiter, anext,
getattr, hasattr, setattr, delattr, vars, dir,
globals, locals, breakpoint, \_\_build\_class\_\_, \_\_import\_\_

**Types:**
type, int, float, str, bool, object, list, dict, tuple, set, frozenset,
bytes, bytearray, memoryview, slice, staticmethod, classmethod, property

**Exceptions (64):** 63 of CPython 3.12's 69 builtin exceptions, plus
`CancelledError`.  BaseException, Exception, the ArithmeticError, LookupError
and OSError trees (FileNotFoundError, PermissionError, ConnectionResetError,
…), StopIteration, StopAsyncIteration, GeneratorExit, KeyboardInterrupt,
SystemExit, RecursionError, UnicodeDecodeError / UnicodeEncodeError, the
Warning family, and BaseExceptionGroup / ExceptionGroup — which derives from
both BaseExceptionGroup and Exception, so `except Exception` catches it.
Missing: `IOError` / `EnvironmentError` (the OSError aliases),
`FileExistsError`, `IndentationError`, `TabError`, `UnicodeTranslateError`.

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
- Multiple inheritance with a C3 MRO (`__mro__`, `__bases__`), cooperative `super()`
- Metaclasses: `metaclass=`, an inherited metatype, `type.__new__`,
  `__instancecheck__` / `__subclasscheck__`
- Abstract base classes, on a native `_abc`: `abstractmethod`, virtual
  subclass registration, `__subclasshook__`
- Weak references and proxies, with callbacks
- Unicode strings: UTF-8 storage, code-point indexing, slicing, iteration and
  widths; utf-8 / ascii / latin-1 codecs
- Relative imports (`from . import x`, `from ..pkg import y`)
- Real tracebacks: per-frame line numbers decoded from the PEP 626 location
  table, source lines, repeated-frame elision, and the `__cause__` /
  `__context__` chain

### Modules

| Module | Description |
|--------|-------------|
| sys | argv, exit (raises `SystemExit`), version, version_info, path, modules, stdin/stdout/stderr, exc_info, maxsize, platform, byteorder, executable, prefix, implementation, builtin_module_names, warnoptions, intern, getrecursionlimit/setrecursionlimit, get/set_int_max_str_digits |
| _abc | The ABC accelerator abc.py is built on: get_cache_token, _abc_init, _abc_register, _abc_instancecheck, _abc_subclasscheck, _get_dump, _reset_registry, _reset_caches |
| _weakref | Real weak references: ref (subclassable, with callbacks), proxy, getweakrefcount, getweakrefs, _remove_dead_weakref |
| asyncio | Event loop with io_uring backend, coroutine runner, TCP streams (open_connection, start_server), sleep, gather |
| _sre | SRE regex engine — compile, and the pattern methods match, fullmatch, search, findall, finditer, sub, subn, split.  The `re` wrapper module is not shipped, so CPython's own `re` is what an `import re` finds, and that needs a real `eval()` by way of `collections.namedtuple` |
| time | monotonic, process_time.  `time.time` and `time.sleep` are not implemented; `asyncio.sleep` is |
| itertools | chain, cycle, islice, count, repeat, product, starmap, accumulate.  zip_longest, permutations, combinations, takewhile, dropwhile, filterfalse, groupby, tee and pairwise are not implemented |
| unittest | Pure Python test framework (TestCase, assertions, test runner) |
| warnings | warn, simplefilter |

Pure-Python modules shipped in `lib/` and importable as they are: `abc` (CPython's
own, on the native `_abc`), `_codecs`, `_thread`, `collections`, `contextlib`,
`copy`, `functools`, `io`, `itertools`, `operator`, `pickle`, `string`,
`unittest`, `warnings`.  They are found relative to the interpreter binary and
sit at the end of `sys.path`, so a real stdlib named by `PYTHONPATH` wins:
these stand in for CPython's C modules, not for its Python ones.

### How much of CPython's standard library imports

`make check-stdlib` imports all 196 modules of a CPython 3.12 `Lib/` in a
fresh process each and compares the result against `tests/stdlib_floor.txt`,
which records the set that works.  It is a ratchet: a module that imported and
no longer does fails the target.  Point `$CPYTHON_LIB` at a source checkout;
the target skips cleanly when there is not one.

`bugs.md` records what the rest fail on, with counts.

### Garbage collection

3-generation cycle-collecting GC with traverse/clear protocols for all container types. Generational thresholds match CPython defaults. Handles reference cycles in dicts, lists, tuples, sets, classes, generators, frames, and closures.

## Test suite

**161 test files** covering arithmetic, strings, lists, dicts, tuples, sets,
booleans, None, bytes, floats, comparisons, control flow, functions,
recursion, for-loops, while-loops, range, classes, inheritance, multiple
inheritance, generators, async/await, closures, decorators, comprehensions,
f-strings, exceptions, tracebacks, pattern matching, slicing,
`*args`/`**kwargs`, `with` statements, imports, relative imports, itertools,
metaclasses, abstract base classes, weak references, Unicode, the codecs, the
cycle collector across generations, the NaN-boxed value encoding, and more.
Each is run against CPython 3.12 and the outputs diffed, so CPython is the
oracle; the async tests run three times, once per I/O backend, for 180
results in all.

**64 CPython standard-library test files** under `tests/cpython/`, all
enforced — a failure in any of them fails the target.

```bash
make check                  # test files, diffed against python3
make check-cpython          # 64 CPython stdlib test files
make check-stdlib           # how much of a CPython Lib/ imports (a ratchet)
./apython --selftest-value  # Value encode/decode boundaries
make INT_STRESS=1 && bash tests/run_tests.sh   # every |n| >= 8 boxed on the heap
```

`INT_STRESS=1` forces every integer of magnitude 8 or more onto the heap, so
the ordinary suite exercises the heap-int paths that immediates normally
hide.  It is not expected to pass `check-cpython`, whose `test_int.py`
asserts things like `10 is 10`.

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
  builtins.asm          Built-in functions, type registry, type_from_parts
  builtins_extra.asm    Additional builtins (itertools constructors, etc.)
  slots.asm             Slot wrappers installed from a heaptype's dunders
  mro.asm               C3 linearization and MRO walking
  format.asm            The format-spec mini-language
  traceback.asm         PEP 626 line table and traceback rendering
  marshal.asm           .pyc marshal deserializer
  pyc.asm               .pyc file reader
  frame.asm             Frame allocation/deallocation
  object.asm            Base PyObject operations, type_type, rich comparison
  memory.asm            Memory management
  error.asm             fatal_error: unrecoverable failures, straight to stderr
  except.asm            co_exceptiontable parser (handler lookup)
  gc.asm                3-generation cycle-collecting garbage collector
  import.asm            Module import system
  dunder.asm            Dunder method dispatch (__add__, __eq__, etc.)
  repr.asm              repr/str formatting
  val.asm               NaN-boxed Value encoding and helpers
  valtest.asm           --selftest-value: encode/decode boundary checks
  methods.asm           Built-in type methods (str/list/dict/set/tuple/int)
  sre.asm               SRE regex bytecode engine
  sre_module.asm        _sre module interface
  itertools.asm         itertools module
  pyo/                  33 type implementation files
    int.asm float.asm str.asm bytes.asm bytearray.asm memview.asm
    list.asm dict.asm tuple.asm set.asm bool.asm none.asm slice.asm
    func.asm class.asm code.asm module.asm cell.asm
    iter.asm generator.asm exception.asm exc_group.asm fileobj.asm
    descriptors.asm sre_match.asm sre_pattern.asm
    sysmod.asm asyncmod.asm timemod.asm abcmod.asm weakrefmod.asm
    namespace.asm
    eventloop.asm eventloop_poll.asm eventloop_iouring.asm
    asyncio_streams.asm
  lib/                  Syscall wrappers, string/memory ops
    syscall.asm memops.asm string.asm
include/                Struct definitions, macros, constants (.inc files)
lib/                    Pure Python support modules
  abc.py contextlib.py copy.py functools.py io.py itertools.py
  operator.py pickle.py string.py warnings.py
  collections/          namedtuple, defaultdict, Counter, OrderedDict
  unittest/             Test framework (case.py, runner.py, mock.py)
  test/                 CPython test support infrastructure
tests/                  161 test files
  cpython/              64 CPython standard-library test files
  expected/             recorded transcripts for the two tests CPython cannot serve as an oracle for
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
| `make check` | Run the 149-file test suite |
| `make check-cpython` | Run the 64 CPython stdlib test files |
| `make INT_STRESS=1` | Build with every integer of magnitude ≥ 8 heap-boxed |
| `make clean` | Remove build artifacts |

## License

MIT — see [LICENSE](LICENSE) for details.
