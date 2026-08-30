# Known bugs

Open items only.  A bug that has been fixed belongs in the commit that fixed
it, not here; this file is the list of what is still wrong.

Every entry below was reproduced against the current build.  Each says what
the difference from CPython 3.12 is and, where it is known, why it is not a
one-line fix.

## Correctness

- **A `str` is a byte string, not a sequence of code points.**  `chr(233)` is
  two bytes, so `len(chr(233))` is 2 where CPython says 1, and indexing,
  slicing and iteration all work in bytes.  `ord()` decodes UTF-8, so it
  round-trips, but nothing else does.  `chr(0)` produces an empty string.

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
- Six builtin exceptions: `IOError` / `EnvironmentError`, `FileExistsError`,
  `IndentationError`, `TabError`, `UnicodeTranslateError`.

## Robustness

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
