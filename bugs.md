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

- **The source compiler has no classes, comprehensions or generators yet.**
  `compiler/` handles expressions, statements, control flow, functions,
  lambdas and closures, and `eval()`, `exec()` and `compile()` all run through
  it -- including `collections.namedtuple`'s
  `eval("lambda _cls, ...: _tuple_new(...)")`, which used to be the wall.
  What is still missing above that: `class`, `try`/`except`/`finally`, `with`,
  comprehensions, generators, `async`, f-strings and `match`.

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
