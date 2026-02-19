# APython TODO: Remaining 3.12 Compliance Gaps

## 1. Missing Built-in Functions

| Function | Difficulty | Notes |
|----------|-----------|-------|
| `complex()` | Hard | No complex type exists |
| `breakpoint()` | Low | No-op or print to stderr |
| `exec()` / `compile()` | Hard | Runtime compilation |

## 2. Partial Built-in Functions

| Function | Issue |
|----------|-------|
| `print()` | `file=` kwarg ignored (always stdout) |

## 3. Type Method Gaps

### str
- `maketrans()`, `format_map()`

### dict
- `fromkeys()` classmethod
- `__ior__` (`|=` in-place merge)
- keys/values/items return lists, not lazy view objects

### bytes
- `replace()`, `split()`, `join()`

### int
- `to_bytes()`, `from_bytes()` as callable methods

### float
- `as_integer_ratio()`, `hex()`, `fromhex()`

## 4. Missing Dunder/Feature Support

| Feature | Notes |
|---------|-------|
| `__slots__` | Not implemented |
| `__del__` (destructor) | Not implemented |
| Extended slice assignment | `a[1:3:2] = ...` unsupported; basic `a[1:3] = [4,5]` works |

## 5. Known Limitations

| Issue | Location | Severity |
|-------|----------|----------|
| CALL_INTRINSIC_2 args 2-4 are type-param stubs | opcodes_misc.asm | Low |
| Exception `__traceback__` minimal | eval.asm | Low |
