# APython Audit: vs Python 3.12 Reference

Audit date: 2026-02-16. Compares apython (x86-64 NASM Python 3.12 bytecode interpreter)
against reference CPython 3.12.

---

## 1. Opcode Coverage

### 1.1 Implemented Opcodes (95/120 = 79%)

Python 3.12 has ~120 non-specialized, non-instrumented opcodes (0-176).
Specialized (129+) and instrumented (237+) opcodes are NOT needed -- CPython
falls back to base opcodes in .pyc files.

**Fully implemented (94):**

| # | Name | Notes |
|---|------|-------|
| 0 | CACHE | No-op |
| 1 | POP_TOP | |
| 2 | PUSH_NULL | |
| 3 | INTERPRETER_EXIT | |
| 4 | END_FOR | |
| 5 | END_SEND | |
| 9 | NOP | |
| 11 | UNARY_NEGATIVE | |
| 12 | UNARY_NOT | |
| 15 | UNARY_INVERT | |
| 25 | BINARY_SUBSCR | +1 CACHE |
| 26 | BINARY_SLICE | |
| 27 | STORE_SLICE | |
| 30 | GET_LEN | |
| 31 | MATCH_MAPPING | |
| 32 | MATCH_SEQUENCE | |
| 33 | MATCH_KEYS | |
| 35 | PUSH_EXC_INFO | |
| 36 | CHECK_EXC_MATCH | |
| 49 | WITH_EXCEPT_START | |
| 53 | BEFORE_WITH | |
| 60 | STORE_SUBSCR | +1 CACHE |
| 61 | DELETE_SUBSCR | |
| 68 | GET_ITER | |
| 69 | GET_YIELD_FROM_ITER | `yield from` support |
| 71 | LOAD_BUILD_CLASS | |
| 74 | LOAD_ASSERTION_ERROR | |
| 75 | RETURN_GENERATOR | |
| 83 | RETURN_VALUE | |
| 85 | SETUP_ANNOTATIONS | |
| 87 | LOAD_LOCALS | |
| 89 | POP_EXCEPT | |
| 90 | STORE_NAME | |
| 91 | DELETE_NAME | |
| 92 | UNPACK_SEQUENCE | +1 CACHE |
| 93 | FOR_ITER | +1 CACHE |
| 94 | UNPACK_EX | |
| 95 | STORE_ATTR | +4 CACHE |
| 96 | DELETE_ATTR | |
| 97 | STORE_GLOBAL | |
| 98 | DELETE_GLOBAL | |
| 99 | SWAP | |
| 100 | LOAD_CONST | |
| 101 | LOAD_NAME | |
| 102 | BUILD_TUPLE | |
| 103 | BUILD_LIST | |
| 104 | BUILD_SET | |
| 105 | BUILD_MAP | |
| 106 | LOAD_ATTR | +9 CACHE |
| 107 | COMPARE_OP | +1 CACHE |
| 108 | IMPORT_NAME | Full import system with sys.modules |
| 152 | MATCH_CLASS | Structural pattern matching class patterns |
| 109 | IMPORT_FROM | Including submodule fallback |
| 110 | JUMP_FORWARD | |
| 114 | POP_JUMP_IF_FALSE | |
| 115 | POP_JUMP_IF_TRUE | |
| 116 | LOAD_GLOBAL | +4 CACHE |
| 117 | IS_OP | |
| 118 | CONTAINS_OP | |
| 119 | RERAISE | |
| 120 | COPY | |
| 121 | RETURN_CONST | |
| 122 | BINARY_OP | +1 CACHE, all 26 NB_* ops |
| 123 | SEND | Generator/coroutine send |
| 124 | LOAD_FAST | |
| 125 | STORE_FAST | |
| 126 | DELETE_FAST | |
| 127 | LOAD_FAST_CHECK | |
| 128 | POP_JUMP_IF_NOT_NONE | |
| 129 | POP_JUMP_IF_NONE | |
| 130 | RAISE_VARARGS | args 0,1,2 |
| 132 | MAKE_FUNCTION | Closures, defaults, kwdefaults |
| 133 | BUILD_SLICE | 2-arg and 3-arg |
| 134 | JUMP_BACKWARD_NO_INTERRUPT | |
| 135 | MAKE_CELL | |
| 136 | LOAD_CLOSURE | |
| 137 | LOAD_DEREF | |
| 138 | STORE_DEREF | |
| 139 | DELETE_DEREF | |
| 140 | JUMP_BACKWARD | |
| 141 | LOAD_SUPER_ATTR | +1 CACHE |
| 142 | CALL_FUNCTION_EX | kwargs partially supported |
| 143 | LOAD_FAST_AND_CLEAR | |
| 144 | EXTENDED_ARG | |
| 145 | LIST_APPEND | |
| 146 | SET_ADD | |
| 147 | MAP_ADD | |
| 149 | COPY_FREE_VARS | |
| 150 | YIELD_VALUE | |
| 151 | RESUME | no-op (correct) |
| 155 | FORMAT_VALUE | Including format specs |
| 156 | BUILD_CONST_KEY_MAP | |
| 157 | BUILD_STRING | |
| 162 | LIST_EXTEND | |
| 163 | SET_UPDATE | |
| 164 | DICT_MERGE | |
| 165 | DICT_UPDATE | |
| 171 | CALL | +3 CACHE |
| 172 | KW_NAMES | |
| 173 | CALL_INTRINSIC_1 | args 3,5,6 only |
| 174 | CALL_INTRINSIC_2 | arg 1 only |
| 175 | LOAD_FROM_DICT_OR_GLOBALS | |
| 176 | LOAD_FROM_DICT_OR_DEREF | Comprehension scoping |

### 1.2 Missing Opcodes (7)

| # | Name | Difficulty | Impact | Notes |
|---|------|-----------|--------|-------|
| 37 | CHECK_EG_MATCH | Medium | Low | `except*` (ExceptionGroup, PEP 654) |
| 50 | GET_AITER | Hard | Low | `async for` - needs async framework |
| 51 | GET_ANEXT | Hard | Low | `async for` |
| 52 | BEFORE_ASYNC_WITH | Hard | Low | `async with` |
| 54 | END_ASYNC_FOR | Hard | Low | `async for` cleanup |
| 55 | CLEANUP_THROW | Hard | Low | Generator `.throw()` cleanup |
| 131 | GET_AWAITABLE | Hard | Low | `await` expression |

---

## 2. Built-in Functions

### 2.1 Implemented (43)

**Functions (40):**
`print`, `len`, `range`, `type`, `isinstance`, `issubclass`, `repr`, `bool`,
`float`, `abs`, `int`, `str`, `ord`, `chr`, `hex`, `id`, `hash`, `callable`,
`iter`, `next`, `any`, `all`, `sum`, `min`, `max`, `getattr`, `hasattr`,
`setattr`, `enumerate`, `zip`, `map`, `filter`, `reversed`, `sorted`,
`super`, `globals`, `locals`, `dir`, `divmod`, `eval` (restricted literal),
`__build_class__`

**Types as constructors:**
`int`, `str`, `bool`, `float`, `object`, `list`, `dict`, `tuple`, `set`,
`bytes`, `bytearray`, `memoryview`

**Descriptors:**
`staticmethod`, `classmethod`, `property`

### 2.2 Missing Built-ins (by priority)

**High priority (commonly used):**
- `input()` - stdin reading
- `open()` - file I/O (even basic read-only)
- `format()` - delegates to `__format__`
- `round()`
- `pow()` (3-arg modular form)
- `vars()`
- `frozenset()`
- `slice()` as callable

**Medium priority:**
- `bin()` / `oct()`
- `sorted()` with `key=` and `reverse=` kwargs
- `enumerate()` `start=` kwarg may be incomplete
- `map()` / `filter()` multi-iterable forms
- `zip()` with `strict=` kwarg
- `complex()`
- `breakpoint()`
- `print()` with `file=` kwarg

**Low priority:**
- `__import__()`
- `ascii()`
- `delattr()` (already have DELETE_ATTR opcode)
- `aiter()` / `anext()`
- `exec()` / `compile()`

---

## 3. Type Methods and Protocols

### 3.1 str - Implemented Methods (20)
`upper`, `lower`, `strip`, `lstrip`, `rstrip`, `split`, `find`, `rfind`,
`replace`, `startswith`, `endswith`, `join`, `count`, `index`, `isdigit`,
`isalpha`, `removeprefix`, `removesuffix`, `encode`, `format`

Also: `__contains__` (full substring search via `ap_strstr`), `__mul__`
(string repetition)

### 3.1b str - Missing Methods
- `rindex()`
- `isalnum()`, `isspace()`, `isupper()`, `islower()`, `istitle()`
- `title()`, `capitalize()`, `swapcase()`, `casefold()`
- `center()`, `ljust()`, `rjust()`, `zfill()`
- `partition()`, `rpartition()`, `rsplit()`, `splitlines()`
- `expandtabs()`, `maketrans()`, `translate()`
- `format_map()`
- `__getitem__` negative index / full slice support

### 3.2 list - Implemented Methods (11)
`append`, `extend`, `insert`, `remove`, `pop`, `clear`, `index`, `count`,
`reverse`, `sort` (generic via tp_richcompare), `copy`

Also: `__add__` (concatenation), `__mul__` (repetition) via sq_concat/sq_repeat

### 3.2b list - Missing / Limitations
- `sort()` with `key=` and `reverse=` kwargs
- Extended slice assignment (`a[1:3:2] = ...`) raises error (basic `a[1:3] = [4,5]` works via STORE_SLICE)
- `__eq__` / comparison operators

### 3.3 dict - Implemented Methods (10)
`get`, `keys`, `values`, `items`, `pop`, `clear`, `update`, `copy`,
`setdefault`, `popitem`

### 3.3b dict - Missing
- `fromkeys()` (classmethod)
- `__or__` / `__ior__` (dict merge operators, 3.9+)
- `__eq__` comparison
- Dict views (keys/values/items return lists, not view objects)

### 3.4 tuple - Implemented Methods (2)
`index`, `count`

Also: `__add__` (concatenation), `__mul__` (repetition) via sq_concat/sq_repeat

### 3.5 set - Limitations
Internal `set_add` and `set_remove` exist but are not exposed as methods.
- Missing method dispatch: `add()`, `remove()`, `discard()`, `pop()`, `clear()`
- Missing set algebra: `union()`, `intersection()`, `difference()`, `symmetric_difference()`
- Missing predicates: `issubset()`, `issuperset()`, `isdisjoint()`
- Missing operators: `|`, `&`, `-`, `^`
- `frozenset` type entirely missing

### 3.6 bytes - Implemented
`__repr__`, `__len__`, `__getitem__` (indexing returns int), slicing,
`__iter__`, `__contains__`, `decode()`

### 3.6b bytes - Missing
- `hex()`, `count()`, `find()`, `replace()`, `split()`, `join()`
- `startswith()`, `endswith()`

### 3.7 int - Missing Methods
- `bit_length()`, `bit_count()`
- `to_bytes()`, `from_bytes()` (classmethod)
- `__index__` protocol (for use in slicing etc.)
- `conjugate()`, `numerator`, `denominator` (numeric tower)

### 3.8 float - Missing Methods
- `is_integer()`, `as_integer_ratio()`
- `hex()`, `fromhex()` (classmethod)
- `__round__()` for `round()` builtin

### 3.9 User-Defined Dunder Dispatch

Full dunder dispatch system in `src/dunder.asm` with `dunder_lookup()` walking
the type->tp_base chain. Supported dunders (~35):

**Comparison:** `__eq__`, `__ne__`, `__lt__`, `__le__`, `__gt__`, `__ge__`
**Arithmetic:** `__add__`, `__radd__`, `__sub__`, `__rsub__`, `__mul__`, `__rmul__`,
`__truediv__`, `__rtruediv__`, `__floordiv__`, `__rfloordiv__`, `__mod__`, `__rmod__`,
`__pow__`, `__rpow__`, `__matmul__`
**Bitwise:** `__and__`, `__or__`, `__xor__`, `__lshift__`, `__rshift__`
**Inplace:** `__iadd__`, `__isub__`, `__imul__`
**Unary:** `__neg__`
**Iterator:** `__iter__`, `__next__`
**Container:** `__getitem__`, `__setitem__`, `__delitem__`, `__contains__`, `__len__`, `__bool__`
**Callable:** `__call__`, `__hash__`
**Representation:** `__repr__`, `__str__`
**Descriptor:** `__get__`, `__set__`, `__delete__`

### 3.9b Missing Dunder Support
- `__enter__` / `__exit__` (found by name, not general dunder dispatch)
- `__init_subclass__` (basic support exists)
- `__class_getitem__` (basic support exists)
- `__slots__`
- `__new__` user-defined
- `__del__` (destructor, low priority)

---

## 4. Remaining Feature Gaps

### 4.1 Async/Await
- All async opcodes unimplemented (GET_AITER, GET_ANEXT, etc.)
- No event loop, no coroutine support
- Low priority for a bytecode interpreter

### 4.2 Exception Features
- `raise X from Y` - cause chain not fully stored
- `except*` (ExceptionGroup) - CHECK_EG_MATCH missing
- Exception `__traceback__` attribute
- `with` statement works but `__exit__` error handling is simplified

### 4.3 Generators
- Basic `yield` works, `yield from` works (SEND + GET_YIELD_FROM_ITER)
- `.send()` and `.close()` work
- `.throw()` incomplete (CLEANUP_THROW missing)

---

## 5. Proposed Implementation Order

Priority based on: how many real Python programs are blocked, bang-for-buck,
dependency chains.

### Tier 2: Common Library Features

| # | Feature | Status |
|---|---------|--------|
| 9 | `input()` | **TODO** |
| 10 | `open()` + basic file I/O | **TODO** |
| 11 | `round()`, `pow()` | **TODO** (`divmod` done) |

### Tier 3: Completeness

| # | Feature | Status |
|---|---------|--------|
| 22 | Slice assignment | Partial (STORE_SLICE works; extended slices not) |
| 23 | Set operations | **TODO** (internal add/remove exist, no method dispatch) |
| 26 | `str.is*()` family | Partial (`isdigit`, `isalpha` done; rest TODO) |

### Tier 4: Advanced / Niche

| # | Feature | Status |
|---|---------|--------|
| 31 | Exception groups (CHECK_EG_MATCH) | **TODO** |
| 33 | CLEANUP_THROW | **TODO** |
| 34 | Async/await (5 opcodes) | **TODO** |
| 35 | `frozenset` | **TODO** |
| 36 | `complex` type | **TODO** |
| 37 | `memoryview`/`bytearray` | Partial (types registered, limited functionality) |

---

## 6. Known Bugs and Limitations

| Issue | Location | Severity |
|-------|----------|----------|
| CALL_FUNCTION_EX kwargs incomplete | opcodes_call.asm | Medium |
| Extended slice assignment unsupported | pyo/list.asm | Low |
| CALL_INTRINSIC_1 only args 3,5,6 | opcodes_misc.asm | Low |
| CALL_INTRINSIC_2 only arg 1 | opcodes_misc.asm | Low |
| marshal: frozenset stub returns None | marshal.asm | Medium |
| WITH_EXCEPT_START simplified (no traceback) | opcodes_call.asm | Low |

---

## 7. Summary Statistics

| Category | Implemented | Total | % |
|----------|------------|-------|---|
| Opcodes (non-specialized) | 95 | ~102 real | ~93% |
| Built-in functions | 43 | ~70 | ~61% |
| Core types | 13 | ~15 | ~87% |
| str methods | 20 | ~45 | ~44% |
| list methods | 11 | ~15 | ~73% |
| dict methods | 10 | ~13 | ~77% |
| set methods | 0 (internal only) | ~17 | 0% |
| tuple methods | 2 | 2 | 100% |
| bytes methods | 6 | ~30 | ~20% |
| User dunder dispatch | ~35 | ~40 | ~88% |
| Test files | 55 | - | - |

The interpreter handles a substantial subset of Python 3.12 including
multi-file programs with imports: arithmetic, control flow, functions with
full keyword argument binding, closures, classes with inheritance and dunder
dispatch, generators (including `yield from`), exceptions, iterators, string
formatting (f-strings with format specs, `%` formatting), and container
operations. The biggest remaining gaps are set method dispatch, a handful of
builtins (`input`, `open`, `round`, `pow`), async/await, and niche type methods.
