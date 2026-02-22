# apython Major Design Update Briefing
## FatValue (128-bit) → Value64 (64-bit payload) + Sidecar Tags

Audience: AI coding agent implementing the migration.

## 0. Executive Summary
`apython` currently propagates a **128-bit “FAT value”** through the VM stack, locals, and containers to support **native int64/float64** and other immediates without boxing. This doubles memory moves, reduces cache density, and increases bandwidth pressure.

This update replaces FAT values with a **64-bit payload (`Value64`)** plus a **sidecar tag stream** (byte tags in hot paths; optionally bit-packed tags in cold/large arrays). This preserves:
- **Full 64-bit float semantics** (exact IEEE-754 bit patterns, NaN payloads, -0.0)
- **Full signed int64 range**
- **Single u64 payload moves** across the interpreter

We explicitly accept that **SmallStr** may be reduced or removed (pointer-only strings are acceptable) to maximize numeric throughput.

Deliverable: a fully integrated VM/storage refactor, keeping Python 3.12 bytecode compatibility intact.

---

## 1. Goals and Non-Goals
### Goals
1. Replace all 128-bit FAT value plumbing with:
   - `payload: u64`
   - `tag: u8` (or packed bits in selected structures)
2. Preserve numeric fast paths:
   - `int64` stored inline as two’s complement bits
   - `float64` stored inline as raw IEEE-754 bits
3. Keep pointer/object model compatible with existing container and runtime semantics.
4. Reduce memory bandwidth (2× win on common paths), improve cache locality.
5. Keep reference counting / GC behavior correct: **only PTR-tagged values participate in RC/GC**.
6. Maintain correctness for Python 3.12 semantics (including edge cases like NaN payloads, -0.0).

### Non-Goals (v1 of this change)
- JIT compilation.
- Reintroducing large inline SmallStr (15B). Optional limited small-string can be added later.
- Aggressive bytecode specialization beyond a small set of hot ops (can come later).

---

## 2. Key Idea: Split Representation
### Definition
- `Value64` (payload): `u64`
- `Tag` (sidecar): `u8`

A “value” in the VM is the pair `(payload, tag)`.

### Rationale
- **Full-width float64 and int64** cannot coexist with in-word tagging without losing bits or special-casing NaNs.
- Sidecar tagging keeps payload unmolested and permits fast dispatch.

---

## 3. Tag Set (Opinionated v1)
Use a compact `u8` enum. Keep values stable; do not reorder after initial integration.

Suggested tags:
- `TAG_PTR`      : payload is canonical pointer to heap object
- `TAG_I64`      : payload is two’s complement signed int64 bits
- `TAG_F64`      : payload is IEEE-754 float64 bits
- `TAG_BOOL`     : payload 0/1
- `TAG_NONE`     : payload ignored (or 0)
- `TAG_SMALLINT` : optional alias of TAG_I64 (avoid unless helpful)
- `TAG_RESERVED*`: for future (bytes, smallstr, interned ids, etc.)

Note: If you already have other immediates (e.g., small bytes), either convert to PTR or allocate a new tag later.

---

## 4. VM Data Structures
### 4.1 Value Stack (Hot Path)
Replace a single array of 128-bit FAT values with:
- `stack_payload: u64[]`
- `stack_tag: u8[]`
- `sp: usize`

Constraints:
- Arrays must be contiguous and aligned.
- Capacity growth must grow both arrays together.

### 4.2 Locals / Fast Locals
Represent locals as the same pair arrays:
- `locals_payload: u64[]`
- `locals_tag: u8[]`

### 4.3 Evaluation temporaries / instruction operands
All VM internal APIs must accept `(payload, tag)` pairs.

---

## 5. Containers
Different containers may choose different tag storage strategies.

### 5.1 List / Tuple
Default (simple, correct):
- `items_payload: u64[]`
- `items_tag: u8[]`

Optional optimization (later):
- pack tags to 2–4 bits/element for large arrays, but keep `u8` tags for the VM stack.

### 5.2 Dict / Hash table
Dict entries already have metadata; store tag per entry:
- `entry.payload: u64`
- `entry.tag: u8`

### 5.3 Object attributes
Same as dict-like storage: `payload + tag`.

---

## 6. Reference Counting and GC Integration
### Principle
Only values with `TAG_PTR` participate in refcount and cycle-collector marking.

### Operations
- `INCREF(value)` does nothing unless tag==PTR
- `DECREF(value)` does nothing unless tag==PTR

If the project has/needs a cycle collector:
- object graph traversal only follows PTR-tagged payloads.
- immediate tags are ignored.

---

## 7. Arithmetic and Fast Paths
### 7.1 Hot numeric ops
Implement specialized fast paths for top bytecodes:
- `BINARY_ADD`, `BINARY_SUBTRACT`, `BINARY_MULTIPLY`, `BINARY_TRUE_DIVIDE`
- Comparisons for numeric

Dispatch rule:
1. If both tags are `I64` → perform i64 op.
   - On overflow: fallback to big-int path (PTR) or raise per semantics.
2. If both tags are `F64` → perform f64 op, preserve exact result bits (including NaNs).
3. Mixed `I64`/`F64` → convert i64→f64 and perform f64.
4. Otherwise → fallback to generic object protocol (PTR-based).

### 7.2 Overflow handling
Python ints are arbitrary precision. With inline i64:
- If operation overflows i64, allocate a big-int object and return `TAG_PTR`.

### 7.3 Division semantics
- `TRUE_DIVIDE` returns float.
- `FLOOR_DIVIDE` returns int (may require big-int).

---

## 8. Calling Convention and ABI inside the Interpreter
### Register usage (guidance)
Treat `(payload, tag)` as a pair.
- Payload: prefer a GPR (e.g., RAX)
- Tag: prefer an 8-bit register or separate GPR (e.g., RCX low byte)

### Stack push/pop macros
Provide macro-like helpers:
- `PUSH(payload, tag)`
- `POP() -> (payload, tag)`
- `PEEK(k)`

Ensure bounds checks are consistent with current VM behavior.

---

## 9. Memory Layout and Alignment
- `payload[]` aligned to 8 bytes.
- `tag[]` aligned to at least 16 bytes for vectorized copying (optional).

Capacity growth:
- Grow payload array in u64 units.
- Grow tags array in u8 units.
- Ensure both reallocations are coordinated to avoid inconsistent state.

---

## 10. Migration Plan (Implementation Sequence)
### Phase 1 — Introduce new types and adapters
1. Add `Value64` and `Tag` definitions.
2. Add helper functions/macros for:
   - constructing i64/f64/ptr
   - tag checks
   - boxing/unboxing
3. Add temporary shims that convert FAT ↔ (payload, tag) for incremental migration.

### Phase 2 — VM stack + locals conversion
1. Convert the evaluation stack to `payload[] + tag[]`.
2. Convert locals/fast locals.
3. Update opcode handlers to use new stack macros.
4. Ensure all tests pass at this stage using shims for remaining components.

### Phase 3 — Runtime API conversion
1. Update internal functions to accept/return `(payload, tag)`.
2. Update refcount helpers to be PTR-gated.
3. Update exception paths and value printing.

### Phase 4 — Container conversion
1. Convert list/tuple to store payload+tag.
2. Convert dict/attributes to store payload+tag.
3. Remove FAT usage from container code.

### Phase 5 — Remove FAT system
1. Delete FAT value definitions and associated move/copy code.
2. Remove shims.
3. Re-run full test suite.

---

## 11. Correctness Requirements / Edge Cases
1. **Float exactness:** payload bit patterns preserved across moves; NaN payloads preserved.
2. **-0.0:** must round-trip and compare correctly per Python semantics.
3. **Int overflow:** must promote to big-int.
4. **Pointer tagging:** must not leak immediates into RC paths.
5. **Container copying:** must copy both arrays/tags consistently.

---

## 12. Performance Requirements
1. VM hot loops must avoid 128-bit moves.
2. Stack operations should be:
   - 1× u64 store/load for payload
   - 1× u8 store/load for tag
3. Keep tag checks branch-predictable:
   - common-case numeric code should stay in the same tag categories.
4. Consider optional micro-optimizations (after correctness):
   - widen tag loads to 32/64 bits for batch ops
   - inline opcode specializations for numeric cases

---

## 13. Testing Strategy
### 13.1 Unit tests
- Tag encoding/decoding
- Stack push/pop correctness
- Container store/load correctness
- RC/GC gating correctness

### 13.2 Numeric torture tests
- i64 edge: min/max, overflow transitions, mixed ops
- f64 edge: NaN payloads, infinities, subnormals, -0.0
- mixed int/float arithmetic

### 13.3 Regression suite
- Run existing `apython` compatibility tests (3.12 bytecode corpus) before and after.
- Add microbenchmarks:
  - tight int loops
  - tight float loops
  - list of floats arithmetic

---

## 14. Acceptance Criteria
- All existing tests pass.
- Numeric semantics verified for edge cases.
- Measured improvements:
  - lower L1/L2 misses in interpreter loops
  - improved throughput on numeric microbenchmarks

---

## 15. Notes on SmallStr
SmallStr is optional in this design. If retained, two reasonable paths:
1. **Drop it** (strings are PTR) — simplest and likely acceptable.
2. **SmallStr≤8** using a special tag and storing bytes in payload; length can be stored in tag (requires extra tag bits or a second tag byte). This can be deferred.

