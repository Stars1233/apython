# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Python 3.12 implementation in x86-64 NASM assembly: a bytecode interpreter, and
a compiler for the source language written in the same assembly.  Reads `.py`
through `compiler/`, or `.pyc` through the marshal reader.

## Build & Test

```bash
make              # build ./apython
make clean        # remove build/ and apython
make check        # full test suite: compile .py→.pyc, diff python3 vs ./apython output
make check-cpython # CPython stdlib unit tests (harder, more thorough)
make check-stdlib # how much of a CPython 3.12 Lib/ imports; a ratchet
make check-source # the whole corpus compiled by OUR compiler; a ratchet
make check-cpython-source  # the CPython corpus, compiled by OUR compiler
```

```bash
./apython --selftest-compile   # source-compiler invariants and tokenizer
python3 compiler/lint.py       # static checks over compiler/*.asm
```

**Always run BOTH `make check` AND `make check-cpython` to verify changes.**

`make check-stdlib` needs a CPython source checkout; point `$CPYTHON_LIB` at
its `Lib/` (default `~/tmp/repo/cpython/Lib`).  It compares against
`tests/stdlib_floor.txt` and fails when a module that used to import stops, or
when a new one crashes.  Raise the floor with
`bash tests/stdlib_probe.sh --record` in the commit that earns it.
`make check` runs 233 test files (256 results: the async tests run against the
default, poll and io_uring backends); `make check-cpython` runs all 64 files
under `tests/cpython/`, none of them tolerated as failing.

`make check-source` and `make check-cpython-source` hand apython the `.py`
instead of the `.pyc`, so our own compiler produces the bytecode, and diff the
result against `python3`.  They are the only things that exercise the compiler
on a large body of ordinary code -- most of its bugs were found there rather
than by a test written for them, including several that need a whole file
rather than a snippet to appear at all.  They also reach interpreter paths a
`.pyc` cannot, because CPython's constant folder settles `3 * "ab"`,
`True & False` and `-7 // 2` before any of them becomes an opcode.

`check-cpython-source` is the harder of the two: that corpus is CPython's own
and written to be adversarial; all 64 of its files now run identically through
our compiler.  Each ratchets against a floor file
(`tests/compile_floor.txt`, `tests/cpython_source_floor.txt`); raise one with
`bash tests/source_probe.sh --record` or
`bash tests/cpython_source_probe.sh --record` in the commit that earns it.

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
- `src/opcodes_*.asm` — Opcode handlers by category (load, store, stack, call, build, misc, async, import)
- `src/pyo/*.asm` — Type implementations (int, str, list, dict, tuple, func, class, iter, bool, none, bytes, code)
- `src/marshal.asm` — .pyc marshal format deserializer
- `src/pyc.asm` — .pyc file reader (magic validation, header parsing)
- `src/builtins.asm` — Built-in functions (print, len, range, type, isinstance, etc.) and `type_from_parts`
- `src/slots.asm` — Installs slot wrappers on a heaptype from the dunders it defines
- `src/mro.asm` — C3 linearization, `type_mro_next`, `type_is_subtype`
- `src/format.asm` — The format-spec mini-language (`format()`, f-strings, `%`)
- `src/traceback.asm` — PEP 626 line-table decoding and traceback rendering
- `src/frame.asm` — Frame alloc/dealloc
- `src/object.asm` — Base PyObject ops (alloc, refcount, dealloc, `obj_richcompare_bool`)
- `src/lib/` — Syscall wrappers, string/memory ops (replace libc)
- `compiler/` — The Python **source** compiler (see below)
- `include/` — Struct definitions (.inc): object, types, frame, opcodes, macros, marshal, builtins, errcodes

## Source Compiler (`compiler/`)

Turns Python 3.12 source into a `PyCodeObject` this interpreter runs.  Reached
through `compile()`, `exec()`, `eval()`, `./apython foo.py`, and `import` of a
`.py` when no `.pyc` is there.  The whole language: `match`, `except*`,
f-strings, async, comprehensions, PEP 695 type parameters.

| file | role |
|------|------|
| `compiler.inc` | token kinds, AST kinds, binding powers, `Buf`/`Comp`/`CompUnit`/`Instr` |
| `tables.asm` | **generated** — char classes, keywords, operators, opcode metadata |
| `gen_tables.py` | regenerates `tables.asm` from CPython 3.12's `opcode`/`dis` |
| `gen_prule.py` | regenerates the expression grammar table inside `parse.asm` |
| `arena.asm` | growable `Buf` and bump `Arena` (the tree has neither otherwise) |
| `lex.asm` | tokenizer: indentation, operators, names, numbers, strings |
| `ast.asm` | 32-byte nodes in a `Buf`, addressed by u32 index |
| `parse.asm` | Pratt expression parser + `prule_table`, the precedence grammar |
| `parse_stmt.asm` | statements, and the soft keywords `match` and `type` |
| `pattern.asm` | `match` patterns |
| `fstring.asm` | f-string fields, lexed as spans of the same source |
| `symtab.asm` | scopes, local/cell/free classification, name mangling |
| `codegen.asm` | AST kind → emitter jump table; `_stmt`/`_func`/`_try`/`_comp`/`_async`/`_match`/`_egroup` for the rest |
| `assemble.asm` | EXTENDED_ARG fixpoint, stack depth, exception table, line table |
| `compile.asm` | pipeline driver and lifetime |
| `evalexec.asm` | the `compile()`, `exec()` and `eval()` builtins |
| `srcfile.asm` | `code_from_path`: `./apython foo.py` and import from source |
| `comperr.asm` | error recording |
| `unicodename.asm` | **generated** -- the names `\N{...}` resolves |
| `gen_unicodename.py` | regenerates `unicodename.asm` from `unicodedata` |
| `uniname.asm` | the search over it, plus the algorithmic CJK family |
| `dis.asm` | `--dis`, for diffing against `python3 -m dis` |
| `comptest.asm` | `--selftest-compile` |
| `lint.py` | static checks, run by `make check` |

**Never call `raise_exception` from `compiler/`.** It tail-jumps into
`eval_exception_unwind`, which calls `fatal_error` when there is no live
interpreter frame — and `./apython foo.py` compiles before any frame exists.
Record the error with `comp_error()` and return 0/NULL; the driver turns it
into a pending exception after every buffer is freed.

`op_meta` in `tables.asm` is the keystone: one row per opcode drives CACHE
padding, instruction sizing, stack-depth accounting and successor computation.
Because every emission routes through it, a forgotten CACHE is not a mistake an
emitter can make. Its numbers are CPython's, taken from the running
interpreter's own modules rather than transcribed.

Regenerate with `python3 compiler/gen_tables.py > compiler/tables.asm`,
`python3 compiler/gen_prule.py`, and
`python3 compiler/gen_unicodename.py > compiler/unicodename.asm`; all three
outputs are committed, so building never needs Python.

**Gates:** `make check-source` and `make check-cpython-source` (both corpora
compiled by this compiler and diffed against `python3` — where nearly every bug
below was found), `./apython --selftest-compile`, `python3 compiler/lint.py`,
and the `tests/test_compile_*.py` files.  All but the two `-source` targets run
inside `make check`.

### Compiler bug patterns

These cost real time; the shapes recur.

- **A binding power that is equal where it should be one below.** The Pratt
  driver continues while `lbp > min_bp`, so an operand parsed AT an operator's
  own power stops before it.  A ternary's else branch at `BP_TERNARY` nests
  left, a lambda body at `BP_TERNARY` loses its own `if`.  Both produce wrong
  answers, not errors.
- **Two index spaces that collide.** Object indices and node indices come from
  different arenas and overlap freely.  `sym_visit`'s generic walk follows a
  node's `a`/`b`/`c`, and any kind whose fields are *object* indices has to be
  on the exclusion list — `AST_HANDLER` was not, so `except E as e` visited
  whatever node sat at e's object index.  Nothing smaller than a whole file
  brings the two into range.
- **A stack effect read from the interpreter rather than from CPython.**
  `MATCH_KEYS` consumes neither the subject nor the keys tuple; a depth taken
  from what the handler *looks* like it does is silently one out, and the
  damage surfaces somewhere else.
- **A jump to a label that was never bound.** It held -1, which the resolver
  read as an unsigned offset past the end of the stream.  `asm_check_labels`
  now rejects it; before that it was a jump off the end.
- **A return value clobbered by the epilogue.** `cg_class_value` restored the
  enclosing scope through `eax` on its way out, so every failure was reported
  as a success and the caller emitted code for something that was never built.
- **A function emitted while a data section is current.** NASM allows it and it
  links; the fault arrives when the CPU refuses to execute the page, as a
  SIGSEGV on the function's own `push rbp`.  `lint.py` checks for it.
- **A callee-saved register used without saving it.** `main` keeps argc and
  argv in r14 and r15 across the compile, so a scratch r14 in an emitter hands
  back a different argv and the crash lands in `sys.argv` construction.
  `lint.py` checks both directions.

## Key Structs

Defined in `include/*.inc`. All objects start with `PyObject` (ob_refcnt +0, ob_type +8).

- **PyTypeObject** (types.inc): tp_call +64, tp_getattr +72, tp_setattr +80, tp_as_number +128, tp_as_sequence +136, tp_as_mapping +144, tp_base +152, tp_mro +168, tp_bases +184, tp_dictoffset +208
- **PyFrame** (frame.inc): code +8, globals +16, locals +32, stack_ptr +48, stack_base +56, localsplus +80 (variable-size Value[])
- **PyIntObject** (object.inc): mpz +16 (only initialised on overflow), ival +32, compact +40 (1 = the ival is live)
- **DictEntry** (object.inc, 24 bytes): hash +0, key +8, value +16 — occupied ⇔ `key != 0`; empty ⇔ `key == 0 && hash == 0`; tombstone ⇔ `key == 0 && hash == -1`
- **PyCodeObject** (object.inc): co_consts, co_names, co_firstlineno +112, co_linetable +120, co_code starts at +128
- **PyStrObject** (object.inc): ob_size +16 is the length in **bytes**, ob_length +32 the length in **code points**, data +40 is NUL-terminated UTF-8.  They are equal for ASCII, which is the fast path every code-point-aware operation checks first.  A new string must set both: `str_set_length` counts, or compute it directly when the arithmetic is obvious.  `str_cp_offset` and `str_byte_to_cp` convert between the two index spaces
- **PyWeakRefObject** (object.inc): wr_object +16 is a *borrowed* referent, zeroed when it dies.  The links live in a side table in `src/pyo/weakrefmod.asm`, consulted by `obj_dealloc` only when `weakref_live` is non-zero

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
- **Shadowing a builtin base's slot:** a dunder that `object` itself supplies is not a definition.  `type_install_slots` skips them (`slot_is_object_default`), or a tuple subclass would compare by identity instead of by contents.  The same technique keeps `instance_repr`/`instance_str` from picking up `object.__repr__` ahead of a builtin base's
- **Boxing in V_PACK:** `V_PACK` on a TAG_SMALLINT outside ±2^50 allocates a heap int. That is correct but it is an allocation, and the returned reference is owned — do not pack a borrowed integer payload and drop it.
- **`current_exception` is also the exception *being handled*.** It stays set for the length of an `except` block, so `cmp qword [rel current_exception], 0` cannot mean "did that call raise?". Snapshot it before the call and compare (`DUNDER_EXC_SAVE` / `DUNDER_RAISED`), or a loop inside a handler re-raises what the handler caught.
- **Following `tp_base` to resolve an attribute or answer a subclass question.** With multiple inheritance the answer lives on the MRO: use `MRO_NEXT walker, origin` (or `type_is_subtype`), keeping the type the search *started from* as the origin. A static type has no `tp_mro`, and for it `MRO_NEXT` still yields `tp_base`, so single-inheritance code reads the same.
- **Writing through an inherited method table.** `type_from_parts` gives a builtin subclass its base's `tp_as_number` / `tp_as_sequence` / `tp_as_mapping` *pointer*. Writing a slot through it patches the builtin's own static table for the whole process; `slot_ensure_table` copies first. The same shape applies to anything else inherited by pointer.
- **A 64-bit read of a 4-byte struct field.** `mov rdx, [rsi + Token.len]`
  assembles fine and silently ORs in the next field as the high half; here it
  produced a multi-gigabyte `ap_memcpy`. Use the 32-bit form (`mov edx`), which
  zero-extends. `compiler/lint.py` checks this.
- **A call made with `rsp` misaligned.** After `DEF_FUNC`'s `push rbp`, a
  `sub rsp, N` and P register pushes, the SysV ABI wants `(N + 8*P) % 16 == 0`.
  Much of `src/` predates this and violates it harmlessly, but the compiler
  calls `strtod`, and glibc's float paths do use aligned SSE. `compiler/lint.py`
  checks it; pad the frame rather than the push list.
- **A frame slot overlapping a struct in the same frame.** A hand-picked
  `equ` for a large struct silently overlaps the scalar slots above it the
  first time the struct grows, and the symptom is one field reading as garbage.
  Derive the offset instead: `CS_UNIT equ 48 + CompUnit_size`.
- **Following a node's `a`/`b`/`c` without asking what kind it is.** The node and object arenas overlap freely, so a generic walk that visits a field holding an *object* index lands on an unrelated node. `sym_visit` keeps an exclusion list; `cg_has_annotation` recurses only into the compound statements whose fields really are blocks. A `for/else` is the other half of the same trap: it hides its else block in `clist` with `nchild` at 0, where no child-list walk reaches it.
- **Leaving a block early must emit its cleanup outside that block's own region.** The exception table is built from a per-instruction handler stamp, so the `__exit__` a `return` emits carries whatever stamp is current — the with's own, unless the unwinder sets it to the enclosing one first. Each entry on the block stack records that enclosing handler for exactly this. A `return` also leaves every enclosing *loop*, whose iterator is on the stack under the return value.
- **A borrowed pointer in `CompUnit.names` or `.consts`.** Both hold borrowed references; the object arena owns them. A string interned at the call site and released leaves a dangling pointer whose symptom is a wild jump inside `dict_lookup` at run time, and a code object never handed to the arena is simply never freed. `comp_intern_cstr` and `comp_intern_keep` are the way in.
- **A variable-size builtin whose subclass gets a fixed-offset `__dict__`.** str and bytes keep their data inline, so a dict at the base's `tp_basicsize` lands *inside* it. They get `TP_DICT_AT_TAIL`; bytearray and memoryview, which can move or borrow their storage, get none at all — but still need `tp_basicsize` set, or the dealloc slot walk reads a negative count.
- **Asking "is this object a class?" by comparing metatypes.** `ob_type is user_type_metatype` is false for a class built by a metaclass of its own, so a classmethod reached through such a class bound the *metaclass*. Test `TYPE_FLAG_METATYPE` on the object's type instead; it is set on `type`, on the two metatypes we ship, and on any class deriving from `type`.
- **An empty `bases` tuple is not the same as no bases.** `type(n, (), d)` substituted `object`; the metaclass paths did not, and those classes got an MRO of just `[C]` — not even instances of `object`. Invisible until a merge needs the `object` that anchors the end.
- **A builtin's behaviour that lives only in a slot.** The stdlib asks questions by name: `hasattr(f, '__get__')` decides whether something is a descriptor, `member_type.__str__ is object.__str__` decides whether a type defines its own `str()`. A slot with no matching entry in `tp_dict` answers those wrong. When adding one, the thunk must call the *defining* type's slot, not the argument's, or a subclass re-dispatches into itself.
- **A constructor in `tp_call` rather than `tp_new`.** `tp_call` on a type is what makes that type's *instances* callable; the constructor goes in `tp_new`, which `type_call` consults. `mappingproxy` had neither, so calling it fell through to the ordinary class-construction path and left its fields holding whatever was there.
- **Reading a key's tag out of `edx` in an `mp_subscript`.** `BINARY_SUBSCR` builds the key Value with `V_PACK`, which *clobbers* the register the tag was in — the value left behind happens to equal `TAG_SMALLINT` for positive ints and not for negative ones. Classify from the Value itself with `V_TEST_PTR`.
- **A constant that is a Value, not a pointer.** `ast_obj_at` hands back whatever the object arena holds, and `class C: 42` puts an immediate int there. Reading `ob_type` off one dereferences the number.
- **A removed load whose guard stayed.** The `(payload, tag)` conversion deleted many `key_tag` loads; where the `test`/`jz` that used them was left in place it now reads a stale register — `from mod import *` and `dict.popitem()` both failed this way, silently. When deleting a load, delete its test.

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
