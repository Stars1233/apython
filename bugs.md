# Known bugs

Open items only.  A bug that has been fixed belongs in the commit that fixed
it, not here; this file is the list of what is still wrong.

Every entry below was reproduced against the current build.  Each says what
the difference from CPython 3.12 is and, where it is known, why it is not a
one-line fix.

## Correctness

- **Case conversion is ASCII-only.**  `"é".upper()` is `"é"`, not `"É"`;
  `upper`, `lower`, `title`, `capitalize` and `swapcase` all leave a
  non-ASCII byte as it is.  Needs Unicode case tables.

- **`bytes.decode` does not validate UTF-8.**  CPython raises
  UnicodeDecodeError for a stray continuation byte or a truncated sequence;
  here the bytes come through as they are.  The resulting string is at least
  self-consistent -- every walk over it treats such a byte as one code point
  of one byte, so `len`, indexing, slicing and iteration agree.

- **`str.encode` and `bytes.decode` know only utf-8, ascii and latin-1.**
  Any other name is a LookupError, where CPython would find the codec through
  the registry; reaching it from the interpreter would mean calling Python
  from a builtin method.  The `errors` argument is accepted and ignored --
  every failure is strict.

- **`format(x, "")` is not `str(x)` for a float.**  `format(1.0, "")` gives
  `"1"` where CPython gives `"1.0"`: an empty spec falls into the general
  formatter with type `g` and precision 6, rather than short-circuiting to
  `repr`.  `f"{1.0}"` and `str(1.0)` are both right; only the explicit
  `format()` with an empty spec is not.

- **`str.format` does not accept attribute or index access in a field.**
  `"{0.attr}"` and `"{0[key]}"` are not resolved; a field is a position, a
  name, or empty.  A nested spec, `"{:{}}"`, is likewise not substituted.

- **`tuple(t) is t` is False.**  CPython returns the argument unchanged when
  it is already an exact tuple.

- **`func.__name__ = "x"` is silently ignored.**  Functions have no settable
  attributes.

- **Dict views have no repr and no contents.**  `repr(d.keys())` is empty
  where CPython gives `dict_keys(['a'])`; the same for `.values()` and
  `.items()`.

- **`bytearray` is not subscriptable.**  It has `sq_length` but no `sq_item`
  and no `tp_as_mapping`, so `b[0]`, `b[1:]` and `reversed(bytearray(...))`
  raise.  It is iterable, and the constructors take every form CPython's do.

- **The `_abc` registry and caches hold strong references.**  CPython uses
  weak ones, so a class registered against an ABC can be collected and the
  ABC's caches shrink; here a registered class lives as long as the ABC.
  Registries are process-lifetime and small in practice.  Revisit if
  `_weakref` lands.

- **`_abc_subclasscheck` does not recurse into `cls.__subclasses__()`.**
  CPython's step 6 finds a registration made on a *subclass* of the ABC;
  types keep no subclass list here, so `issubclass(X, ABC)` is False when X
  was registered against a subclass of ABC rather than against ABC itself.
  Direct registration and real inheritance both work.

- **No platform module, so `os` cannot import.**  `os.py` looks for `posix`
  and raises "no os specific module found" without it.  That is the single
  largest blocker in the stdlib: 47 of the 196 modules fail on it.

- **Missing C modules, by how many stdlib modules each blocks:** `_io` (10),
  `math` (9), `_codecs` (6), `_struct` (5), `_socket` (5), `binascii` (4),
  `_imp` (3), `_string` (2), `errno` (2), and one each for a long tail.
  `complex` does not exist as a type either, which stops `copyreg` and `copy`.

- **Weak references keep no per-object slot.**  The links live in a side
  table keyed by the referent's address rather than in the object, so
  `tp_weaklistoffset` does not exist and `__weakref__` is not an attribute.
  Everything observable through `_weakref` works; a C extension expecting the
  slot would not.

- **`int` and `float` have no `__abs__`, `__int__`, `__float__`, `__index__` or
  `__trunc__`.**  `abs(-5)` works, `(-5).__abs__()` is an AttributeError, and
  the stdlib asks by name -- `operator.index` goes through `__index__`, and a
  class delegating to `int.__int__` finds nothing.  `src/methods/num.asm`
  carried eight implementations of these, written but never registered and
  never converted to the one-Value return convention; they also truncated a
  big int through `self_to_i64`, so they have been deleted.

  Registering the *builtins* under those names instead does not work, which is
  the reason this is still open: `builtin_abs` and `builtin_int_fn` resolve a
  non-exact operand through the numeric protocol, which for a subclass finds
  the very dunder being registered.  `int(M(0))` where `class M(int)` then
  recurses until the stack goes.  They need implementations that read the
  value out of the `PyIntObject` directly, the way `int()` does for an exact
  int -- the trap CLAUDE.md records as "the thunk must call the *defining*
  type's slot, not the argument's".

- **`object.__lt__`, `__le__`, `__gt__` and `__ge__` are missing.**  They
  exist in CPython and always return NotImplemented.  Adding them here would
  shadow a builtin base's own comparison, because a heaptype's slot is
  installed from whatever the MRO's dunder lookup finds and there are no slot
  wrappers to tell object's default apart at that point.  `__eq__`, `__ne__`
  and `__hash__` are present and are skipped explicitly when slots are
  installed.

- **`_thread` is a single-threaded stand-in.**  `lib/_thread.py` gives
  `get_ident` a constant, makes locks uncontended, and raises from
  `start_new_thread`.  Everything in the stdlib that only takes a lock works;
  anything that expects a second thread does not.

- **`_abc_instancecheck` does not honour a spoofed `__class__`.**  CPython
  checks both `instance.__class__` and `type(instance)`; this checks only the
  type, so an object that lies about its class -- a mock, mostly -- is judged
  by what it really is.

- **A `str` subclass has no instance `__dict__`.**  A str keeps its
  characters inline, so there is no fixed offset past the header to put one
  at; CPython uses a negative `tp_dictoffset` scaled by `tp_itemsize`.  These
  behave like `bytes` and like a `__slots__` class.

- **A heaptype's layout base is the widest of its bases, not CPython's solid
  base.**  `class C(A, B)` where A and B are unrelated builtin subclasses of
  different layouts is accepted and laid out as the wider one, where CPython
  raises "multiple bases have instance lay-out conflict".

- **Cyclic garbage is not finalized at shutdown.**  CPython runs `__del__` on
  the cycles still alive when the interpreter exits; apython does not, so
  those finalizers never run.  `tests/test_del_and_gc_state.py` records this
  divergence in its recorded transcript deliberately -- see the note there.

- **Repetition too large to allocate reports OverflowError where CPython says
  MemoryError.**  `ap_malloc` exits fatally rather than returning NULL, so
  the two cases cannot be told apart.

- **The container repr cycle stack is 64 deep**; CPython's limit is far
  higher, so a legitimately deep nesting reports RecursionError.

- **Traceback rendering has no caret line.**  CPython underlines the failing
  expression (`^^^^^^^^`, and `~~^~~` for binary operators and subscripts)
  using the column fields of the location table and, for the anchor forms, a
  tokenizer over the source segment.  Everything else in the report --
  frames, line numbers, source lines, the repeated-frame elision, the
  `__cause__` / `__context__` preamble -- matches; only the caret line is
  missing.

## Missing pieces

These are absences rather than wrong answers — the interpreter raises rather
than lying — but they are ordinary Python that does not work:

- `time.time` and `time.sleep`.  The `time` module has only `monotonic` and
  `process_time`; `asyncio.sleep` exists.
- `itertools.zip_longest`, `permutations`, `combinations`, `takewhile`,
  `dropwhile`, `filterfalse`, `groupby`, `tee`, `pairwise`.
- The `re` wrapper module.  The `_sre` engine underneath is complete, but
  without a shipped `re.py` an `import re` finds CPython's, which needs
  `enum` and `types`.
- `collections.deque`.
- Four builtin exceptions: `IOError` / `EnvironmentError`, `FileExistsError`,
  `UnicodeTranslateError`.

## Robustness

- **Iterators are not GC-tracked, so a cycle through one leaks.**
  `list_iter_type`, `tuple_iter_type`, `dict_iter_type` and the dict views have
  `tp_flags` 0 with no `tp_traverse`/`tp_clear`, and their objects come from
  `ap_malloc` rather than `gc_alloc`.  An iterator holds a strong reference to
  the container it walks, so `a = []; a.append(iter(a))` is a cycle the
  collector cannot see.  `src/gc.asm` used to carry eight traverse/clear
  callbacks for exactly these types; none was ever installed in a slot, so they
  were deleted rather than left looking like working code.  Wiring them up
  means switching those four types to `gc_alloc` + `gc_track` and setting
  `TYPE_FLAG_HAVE_GC`.

- **Code objects, asyncio `Task`s and `wait_for` wrappers are not GC-tracked
  either**, for the same reason and with the same history: `code_traverse`,
  `task_traverse`/`task_clear` and `wait_for_traverse`/`wait_for_clear` were
  written, never installed in a slot, and have now been deleted alongside the
  iterator eight.  A `Task` holds its coroutine, which holds a frame, whose
  locals can hold the task -- an ordinary cycle that never collects.

  Tracking `Task` needs one thing fixed first: the ready queue links tasks
  through `AsyncTask.next` **without taking a reference**
  (`ready_enqueue`, `src/pyo/eventloop.asm`).  Today nothing can free a queued
  task out from under the queue, because the collector cannot see tasks at all;
  make them visible and a cyclic task sitting in the queue becomes collectable,
  leaving a dangling `next` pointer.  Either the queue takes a reference or the
  collector treats it as a root.  The mechanical part is small -- `gc_alloc` +
  `gc_track` in `task_new`, `gc_dealloc` instead of `ap_free` in
  `task_dealloc`, `TYPE_FLAG_HAVE_GC` and the two slots -- and
  `task_clear` also has to start clearing `exception` and the waiters array,
  which the deleted version did not.

- **There is no full collection and no `gc` module.**  `gc_collect` was a
  four-line wrapper on `gc_collect_gen(2)` whose comment named two callers --
  `gc.collect()` and exit cleanup -- neither of which exists; it has been
  deleted with the rest.  Only the automatic generational collections run, so
  `tests/test_gc_generations.py` has to provoke them with churn rather than ask
  for one.


- **`s += x` in a loop is O(n^2)**: `str_concat` always allocates, and
  `src/opcodes/arith.asm` routes `NB_INPLACE_ADD` to the same `sq_concat`, so
  each step copies the whole accumulated string.  CPython's ceval resizes in
  place when the left operand's refcount is 1.  Measured, though, the two are
  level -- 40k appends of ten bytes take 1.19s here against 1.20s under
  CPython 3.12 -- because that optimization does not fire for the ordinary
  module-level accumulator either.  Doing it would make apython faster than
  CPython on this shape rather than close a gap, and it needs the eval loop to
  give up its stack reference before the concat, so it is recorded rather than
  done.

- **Recursive deallocation overflows the stack**: `a=[]`, then 300k times
  `a=[a]`, then `del a`.  Needs a trashcan mechanism.

- **Crafted `.pyc` and `_sre` bytecode are trusted.**  Marshal validates
  offsets but not types, so a `co_consts` slot holding an int is
  dereferenced by `eval_frame`; `frame_new` adds two `.pyc` fields in 32
  bits; `sre_match` bounds the opcode but not its operands.  This is an
  attacker model rather than breakage, and is a separate track.

## Test infrastructure

Two tests compare against a recorded transcript in `tests/expected/` rather
than against CPython, because CPython cannot serve as an oracle for them:

- `test_sre.py` feeds hand-written SRE bytecode to `_sre.compile()`, a
  private API that does not validate its input; CPython segfaults on the
  group pattern it uses.
- `test_del_and_gc_state.py` compares stderr wording that legitimately
  differs, and its transcript also freezes the shutdown-finalization gap
  above.  The asserts, not the transcript, are what establish correctness.

Both are self-asserting.  Any *new* recorded-oracle test needs the same
justification, or it risks blessing a divergence instead of catching it.

## Style debt

Everything here assembles and runs.  These are places the tree does not follow
STYLE.md, listed so the gap is a known quantity rather than a surprise to
whoever copies a neighbouring file.

- **360 raw `[rbp +- N]` frame offsets across 29 files.**  STYLE.md requires
  named `equ` constants; these survive from before that rule.  Four files hold
  most of them: `src/opcodes/build.asm` (117), `src/repr.asm` (64),
  `src/pyo/float.asm` (57) and `src/pyo/sysmod.asm` (17).  The `[rsp +- N]`
  form is a different thing and is explicitly allowed, which is why earlier
  counts here were roughly twice as large.
  A hand-picked offset silently overlaps the slot above it the first time a
  struct in the same frame grows, which is the failure this rule exists to
  prevent.

- **195 of 402 `XX_FRAME equ` constants carry no alignment arithmetic in a
  trailing comment**, which STYLE.md asks for because it is how a reader checks
  the `(N + 8*pushes) % 16 == 0` rule without recounting the pushes.

- **360 separator lines use a single `;` where STYLE.md asks for `;;`**, and
  395 functions have no separator or docblock at all.  Heaviest:
  `src/pyo/sre_pattern.asm`, `src/builtins_obj.asm`, `src/sre.asm`.

- **418 uppercase hex digits** (`0xC0` for `0xc0`) across 32 hand-written
  files, concentrated in `src/sre.asm` (137) and `src/pyo/int.asm` (48).

- **117 redundant `global X` immediately above `DEF_FUNC X`.**  Harmless --
  `DEF_FUNC` already emits the `global` with a size expression -- but it reads
  like the `global` + bare-label form that lint now rejects, so the two look
  alike and only one is correct.
