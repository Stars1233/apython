# APython Audit: Gaps & Incomplete Features

Audit date: 2026-02-17. Lists only missing and partially-implemented items
vs CPython 3.12 reference.

Current stats: 96 opcodes implemented (~94%), 51 builtins, 68 test files.

---

## 1. Missing Opcodes (6)

| # | Name | Difficulty | Impact | Notes |
|---|------|-----------|--------|-------|
| 50 | GET_AITER | Hard | Low | `async for` - needs async framework |
| 51 | GET_ANEXT | Hard | Low | `async for` |
| 52 | BEFORE_ASYNC_WITH | Hard | Low | `async with` |
| 54 | END_ASYNC_FOR | Hard | Low | `async for` cleanup |
| 55 | CLEANUP_THROW | Hard | Low | Generator `.throw()` cleanup |
| 131 | GET_AWAITABLE | Hard | Low | `await` expression |

---

## 2. Missing Built-in Functions

**High priority (commonly used):**
- `format()` - delegates to `__format__`
- `vars()`
- `frozenset()` - type not registered as builtin constructor
- `slice()` - type not registered as builtin constructor

**Medium priority:**
- `bin()` / `oct()`
- `sorted()` with `key=` and `reverse=` kwargs (basic sorted works)
- `map()` multi-iterable form (only 2-arg works)
- `zip()` with `strict=` kwarg
- `print()` with `sep=`, `end=`, `file=` kwargs
- `complex()`
- `breakpoint()`

**Low priority:**
- `__import__()`
- `ascii()`
- `delattr()`
- `aiter()` / `anext()`
- `exec()` / `compile()`

---

## 3. Type Method Gaps

### 3.1 str - Missing Methods
- `rindex()`
- `istitle()`
- `title()`, `capitalize()`, `swapcase()`, `casefold()`
- `center()`, `ljust()`, `rjust()`, `zfill()`
- `partition()`, `rpartition()`, `rsplit()`, `splitlines()`
- `expandtabs()`, `maketrans()`, `translate()`
- `format_map()`
- `__getitem__` negative index / full slice support

### 3.2 list - Missing / Limitations
- `sort()` with `key=` and `reverse=` kwargs
- Extended slice assignment (`a[1:3:2] = ...`) - basic `a[1:3] = [4,5]` works

### 3.3 dict - Missing
- `fromkeys()` (classmethod)
- `__or__` / `__ior__` (dict merge operators, 3.9+)
- `__eq__` comparison
- Dict views (keys/values/items return lists, not view objects)

### 3.4 set - Missing
- Operators: `|`, `&`, `-`, `^`
- `frozenset` type not exposed as builtin

### 3.5 bytes - Missing
- `hex()`, `count()`, `find()`, `replace()`, `split()`, `join()`
- `startswith()`, `endswith()`

### 3.6 int - Missing Methods
- `bit_length()`, `bit_count()`
- `to_bytes()`, `from_bytes()` (classmethod)
- `__index__` protocol
- `conjugate()`, `numerator`, `denominator` (numeric tower)

### 3.7 float - Missing Methods
- `is_integer()`, `as_integer_ratio()`
- `hex()`, `fromhex()` (classmethod)

### 3.8 Missing Dunder Support
- `__init_subclass__`
- `__class_getitem__`
- `__slots__`
- `__del__` (destructor)

---

## 4. Feature Gaps

### 4.1 Async/Await
- All async opcodes unimplemented (GET_AITER, GET_ANEXT, etc.)
- No event loop, no coroutine support

### 4.2 Exception Features
- `raise X from Y` - RAISE_VARARGS pops & discards cause (exc_cause field exists but never set)
- Exception `__traceback__` attribute (field exists, accessible, but tb object is minimal)

### 4.3 Generators
- `.throw()` incomplete (CLEANUP_THROW opcode missing)

---

## 5. Known Bugs and Limitations

| Issue | Location | Severity |
|-------|----------|----------|
| Extended slice assignment unsupported | methods.asm | Low |
| CALL_INTRINSIC_1 only args 3,5,6 | opcodes_misc.asm | Low |
| CALL_INTRINSIC_2 args 2-4 are type-param stubs (no-op) | opcodes_misc.asm | Low |
| generator.throw() acts like close() (no exception injection) | pyo/generator.asm | Medium |
