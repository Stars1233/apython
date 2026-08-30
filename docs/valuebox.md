# The Value: one NaN-boxed 64-bit word

This document describes apython's value representation. It replaces the earlier
briefing that specified a 64-bit payload with a parallel sidecar array of u8
tags; that design shipped, was measured, and was reverted — see
[History](#history) at the end.

## The encoding

A **Value** is one 64-bit word. Let `high16 = v >> 48`:

| `high16` | Meaning | Decode |
|---|---|---|
| `0x0000` | `PyObject*`, stored **raw**; `v == 0` is NULL | use as-is |
| `0x0001`–`0xFFF1` | float64 | `bits = v - V_F64_OFF` |
| `0xFFF2` | async SLEEP sentinel | `delay_ns = v & V_MASK48` |
| `0xFFF3` | async IO_WAIT sentinel | `fd \| dir<<32 = v & V_MASK48` |
| `0xFFF4`–`0xFFF7` | reserved | — |
| `0xFFF8`–`0xFFFF` | int immediate | `i = v - V_INT_BIAS` |

```
V_F64_OFF   = 0x0001_0000_0000_0000   ; 2^48
V_INT_LO    = 0xFFF8_0000_0000_0000
V_INT_BIAS  = 0xFFFC_0000_0000_0000   ; immediate int range [-2^50, 2^50)
V_NAN_LIM   = 0xFFF1_0000_0000_0000   ; raw doubles at or above this are purified
V_CANON_NAN = 0x7FF8_0000_0000_0000
```

The constants live in `include/value.inc`. x86-64 has no `cmp r64, imm64`, so
each one also has a rip-relative home in `src/val.asm`'s `.rodata` and the
macros compare against `[rel v_*]`.

### Why pointers are raw and floats are offset

The obvious NaN box tags the *pointer* (`0xFFFA<<48 | p`) and leaves doubles
alone. Inverting it — raw pointers in low space, doubles shifted up by 2^48 —
is strictly better here:

- Every object dereference is free. Pointer traffic dominates real Python far
  more than float traffic does, and a masked pointer costs two cycles on each
  one.
- `NULL == 0` survives, so every `test rax, rax` error check and every
  pointer-only struct field kept working untouched. That property alone removed
  an enormous amount of churn from the migration.
- Doubles pay one `add` on the way in and one `sub` on the way out.

It works because Linux x86-64 user virtual addresses are below 2^47, so a heap
pointer always has `high16 == 0`; and because non-NaN doubles span
`[0, 0xFFF0000000000000]`, which after the offset lands in
`[0x0001…, 0xFFF1000000000000]` — clear of both pointer space and tag space.

### NaN purification

Raw doubles at or above `V_NAN_LIM` are negative NaNs with high mantissa bits
set, including the default QNaN x86 produces for `inf - inf`
(`0xFFF8000000000000`). Those would collide with the integer range, so
`V_FROM_F64` canonicalises them to `V_CANON_NAN` first: `cmp` + `cmovae` +
`add`, no branch. Positive NaNs and `0xFFF0xxxx` NaNs pass through unchanged.
CPython prints `nan` for every NaN sign and payload, so this is unobservable
from Python.

### Integers

Immediates cover `[-2^50, 2^50)`. Anything wider is boxed into a
`PyIntObject`, which is *not* immediately a GMP number:

```
.ob_refcnt +0 | .ob_type +8 | .mpz +16 (mpz_t) | .ival +32 (int64) | .compact +40
```

`.compact != 0` means `.ival` holds the value and `.mpz` was never initialised.
`INT_NEED_MPZ` promotes in place on overflow. So the cost of leaving the
immediate range is one small allocation, not a GMP init.

Every `int64 -> Value` conversion must go through `V_PACK_I64` or
`val_from_i64`, which range-check and box. This is the single biggest hazard of
the design: `hash()`, `id()`, `int.from_bytes`, `time.time_ns` and every GMP
result can exceed 2^50.

### None, True and False

They have no encoding of their own. They are the existing immortal heap
singletons `none_singleton`, `bool_true` and `bool_false`, and a pointer is its
own Value. Giving them dedicated tags is what produced the old dual
representation bug class, where the same object could arrive as either
`(0, TAG_NONE)` or `(none_singleton, TAG_PTR)` and only one of the two was
handled.

## Where Values live

Everything stores one word per slot:

- the value stack (`r13`) and `PyFrame.localsplus`
- `ob_item` for list and tuple
- `DictEntry.key` / `.value`, and set entries
- every per-object field that holds a Python value (cell contents, slice
  start/stop/step, generator return value, exception value, `__slots__`
  members, the marshal ref table)
- the `tp_call` argument array — which is why `CALL` no longer copies the
  value stack into a temporary array before dispatching: the arguments are
  already a contiguous `Value[]` and `tp_call` gets a pointer into it

Freeing `r15` (the old tag-stack pointer) gave the eval loop a scratch register
back.

## Macros

All in `include/value.inc`.

| Macro | Purpose |
|---|---|
| `V_TEST_PTR v, scratch` | Follow with `ja` to branch away when NULL or an immediate |
| `V_TEST_PTR_M [m], scratch` | Same, for a Value in memory |
| `V_TEST_INT_M [m], scratch` | `jae` when it is an int immediate |
| `V_TEST_F64_M [m], scratch` | `jbe` when it is a float |
| `V_FROM_F64 v, scratch` / `V_TO_F64 v` | double bits ↔ Value |
| `V_PACK_I64 v, scratch` / `V_TO_I64 v` | int64 ↔ Value (packing may box) |
| `INCREF_V v, scratch` | One compare, one branch; no-op unless it is a pointer |
| `DECREF_V v, scratch` | Same shape; NULL-safe, so `XDECREF_V` is only for clarity |
| `V_PACK v, tag` / `V_UNPACK v, tag` | Migration shims to and from `(payload, tag)` |

`DECREF_V` is the reason the encoding keeps NULL at zero and pointers at the
bottom: `lea scratch,[v-1]` / `cmp scratch,[rel v_ptr_max_m1]` / `ja skip`
covers "is a pointer" and "is not NULL" in a single unsigned compare.

## Testing

`./apython --selftest-value` runs before the Python suite and checks four
groups: integer boundaries around ±2^50, float cases (signed zero, inf,
subnormals, DBL_MAX, canonical and x86 NaN, the purification threshold),
NULL/pointer classification and refcount gating, and `V_PACK`/`V_UNPACK`
round-trips. Failure ids are `group*1000 + case + 1`. Encoding bugs are close
to undebuggable from Python-level symptoms, so this pays for itself.

`make INT_STRESS=1` builds with every integer of magnitude ≥ 8 boxed onto the
heap, which makes the ordinary suite exercise the heap-int paths that
immediates normally hide. It found six pre-existing bugs the first time it ran.
It is not expected to pass `make check-cpython`, whose `test_int.py` asserts
things like `10 is 10`.

`tests/test_nanbox_int.py` and `tests/test_nanbox_float.py` cover the same
ground from Python.

## History

The first attempt at removing 128-bit "fat" values kept a 64-bit payload but
moved the tag into a parallel `u8[]` sidecar array — one for the value stack,
one for each container, one for frame locals. It worked, but it did not pay:
two streams still had to be kept in step on every push, pop, store and
collection resize, and the split was a steady source of bugs — a tag read
without its payload, a payload dereferenced without checking its tag, a
container resized on one side only.

NaN boxing removes the second stream entirely. `nofat.md`, which specified the
sidecar design, has been replaced by this document.
