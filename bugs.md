# Known bugs

Open items only.  A bug that has been fixed belongs in the commit that fixed
it, not here; this file is the list of what is still wrong.

Every entry below was reproduced against the current build.  Each says what
the difference from CPython 3.12 is and, where it is known, why it is not a
one-line fix.

## Correctness

- **`format(x, "F")` does not capitalise a non-finite float.**  `'F'` now
  formats like `'f'`, but CPython spells the result of `format(float('inf'),
  'F')` as `INF`; here it is `inf`.  The same for `NAN`, and for the halves of
  a complex.

- **PEP 604 unions are not flattened.**  `int | str | float` builds
  `((int | str), float)` rather than a flat three-member union, so
  `__args__` is nested and `(int|str|float) == (float|str|int)` is False where
  CPython says True.  The repr is flat, which hides it.  Two-member unions
  compare correctly.

- **`~x` on an int subclass prefers the subclass's `__invert__` over
  `int.__invert__`.**  For `class I(int, M)` where `M` defines `__invert__`,
  CPython resolves `int.__invert__` first (it comes earlier in the MRO) and
  answers `-4` for `~I(3)`; here `M`'s wins.  The same shape applies to the
  other unary dunders.

- **`dir()` on a module does not report the module's contents.**  `dir(errno)`
  and `dir(sys)` return only object's own dunders; the module's `__dict__` is
  not consulted.  `tests/test_errno.py` lists its names literally because of it.

- **`slice` objects cannot be ordered.**  CPython compares them as the tuple
  `(start, stop, step)`, so `slice(1) < slice(2)` is True; here it is a
  TypeError.  Equality works.

- **`bytearray` has no arithmetic at all.**  `bytearray_type.tp_as_number` is 0
  and `bytearray_seq_methods` has neither `sq_concat` nor `sq_repeat`, so
  `bytearray(b"a") + bytearray(b"b")`, `bytearray(b"a") * 2` and
  `bytearray(b"%d") % 5` are all TypeErrors.  `tests/test_binop_matrix.py`
  skips those cells rather than blessing them.

- **The set operators build their result with `set_new` whatever the left
  operand was**, so `frozenset({1}) | frozenset({2})` is a `set`, not a
  `frozenset`.  The same for `&`, `-`, `^` and their inplace forms.  The
  contents are right; only the type is wrong.

- **`%`-formatting takes only a tuple or a mapping on the right.**  CPython
  also accepts a single arbitrary object, so `"ab" % [1, 2]` is `'ab'` there
  and a TypeError here.

- **`bytes` `%`-formatting converts through `str_mod`**, so its conversions are
  str's rather than bytes': `b"%s" % b"x"` is `b"b'x'"` where CPython gives
  `b'x'`, `b"%d" % "x"` answers `b'x'` where CPython raises, `b"%c" % 65` is a
  TypeError, and a `bytes`-keyed mapping (`b"%(a)d" % {b"a": 1}`) raises
  KeyError.  The segfault this path used to carry is fixed; the conversion
  table is still str's.

- **`dict.__ior__` takes only a dict.**  CPython's takes any iterable of
  key/value pairs.

- **PEP 604 unions are thin.**  `None | int` is a TypeError rather than
  `None | int`, and `int | int` is not collapsed to `int`.

- **The binary-operator TypeError names neither the operator nor the operand
  types.**  Ours is the fixed string `unsupported operand type(s)`; CPython
  says `unsupported operand type(s) for +: 'int' and 'str'`, and has two more
  wordings besides (`can only concatenate str ...`, `can't multiply sequence by
  non-int of type ...`).  Matching them needs a formatted-raise helper, which
  `raise_exception` has no equivalent of.  Until then **no test may compare
  `str(e)` for one of these** -- `type(e).__name__` is the contract, and
  `tests/test_binop_matrix.py` says so where the next person will read it.

- **`binary_op1`'s subclass rule is not implemented.**  CPython tries the right
  operand's slot *first* when its type is a proper subclass of the left's and
  overrides the slot.  It cannot fire here: the only builtin static subclass
  relationship is `bool` ⊂ `int`, and `bool_number_methods` holds the very same
  function pointers as `int_number_methods`, which CPython's own
  `if (slotw == slotv)` collapses to nothing.  Revisit if a builtin static type
  is ever given a slot function that differs from its base's.

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

- **`posix` is a subset, and a deliberate one.**  The file, directory and
  process calls `os.py` and `os.path` reach for are there, along with
  `environ`, `stat_result`, `error` and the O_*/W* constants -- enough that
  CPython's own `os.py` imports and works.  What is not: `scandir` and
  `DirEntry`, `symlink`, `link`, `chdir`, `chown`, `utime`, `truncate`,
  `dup2`, `fork`, `execv`, and the whole `*at` family.  `_have_functions` is
  an empty list, which is the honest answer -- no `dir_fd=` support -- and
  os.py reads it to build `supports_dir_fd`.

  `stat_result`'s three timestamps are whole-second ints where CPython gives
  floats; the `_ns` fields carry the exact value in both.

- **Missing C modules**, in rough order of how many stdlib modules each
  blocks: `_io`, `math`, `_struct`, `_socket`, `_imp`, `_collections`,
  `_ast`, `binascii`, `_string`, then a long tail of one apiece.
  `make check-stdlib` gives the current figure.

- **Weak references keep no per-object slot.**  The links live in a side
  table keyed by the referent's address rather than in the object, so
  `tp_weaklistoffset` does not exist and `__weakref__` is not an attribute.
  Everything observable through `_weakref` works; a C extension expecting the
  slot would not.

- **`int` and `float` have no `__abs__`, `__int__`, `__float__`, `__index__` or
  `__trunc__`.**  `abs(-5)` works, `(-5).__abs__()` is an AttributeError, and
  the stdlib asks by name -- `operator.index` goes through `__index__`, and a
  class delegating to `int.__int__` finds nothing.  `src/methods/num.asm`
  carried implementations of these, written but never registered and never
  converted to the one-Value return convention; they also truncated a big int
  through `self_to_i64`, so they have been deleted.

  Registering the *builtins* under those names instead does not work, which is
  the reason this is still open: `builtin_abs` and `builtin_int_fn` resolve a
  non-exact operand through the numeric protocol, which for a subclass finds
  the very dunder being registered.  `int(M(0))` where `class M(int)` then
  recurses until the stack goes.  They need implementations that read the
  value out of the `PyIntObject` directly, the way `int()` does for an exact
  int -- the trap CLAUDE.md records as "the thunk must call the *defining*
  type's slot, not the argument's".

- **`sys.getfilesystemencoding()` always answers `'utf-8'`.**  PEP 540's
  locale handling does not exist, and neither does the `surrogateescape`
  error handler -- `str.encode` and `bytes.decode` accept `errors=` and
  ignore it.  So a filename or environment value that is not valid UTF-8
  does not survive a decode/encode round trip, where CPython preserves it.

- **`real` and `imag` are readable on an instance but not on the type.**
  `(5).real`, `(1.5).real` and `(1+2j).real` all work, through a `tp_getattr`
  chain; `int.real` and `complex.real` are AttributeErrors where CPython hands
  back a member descriptor.  `getset_descr` is a stub whose accessors are NULL
  and which nothing in the tree ever invokes, so this needs that plumbing
  built first.  `complex.conjugate` has the same shape -- it resolves to the
  bare builtin rather than to a method descriptor.

- **`float()` of a large int rounds differently from CPython.**
  `float(10**30)` is `9.999999999999999e+29` here and `1e+30` there.
  `float_to_f64` converts a GMP-backed int with `__gmpz_get_d`, which
  truncates toward zero; CPython's `PyLong_AsDouble` rounds to nearest even.
  Every `complex()` and comparison of such a value inherits it.

- **`complex()` of a string does not accept Unicode spaces or Unicode digits.**
  CPython runs `_PyUnicode_TransformDecimalAndSpaceToASCII` first, so
  `complex("\u30001+2j")` parses there; here any byte past ASCII is a
  malformed string.  ASCII whitespace, brackets, underscores, `inf` and `nan`
  all behave as CPython's do.

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
  iterator ones.  A `Task` holds its coroutine, which holds a frame, whose
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

- **There is no full collection and no `gc` module.**  `gc_collect` was a thin
  wrapper on `gc_collect_gen` whose comment named two callers --
  `gc.collect()` and exit cleanup -- neither of which exists; it has been
  deleted with the rest.  Only the automatic generational collections run, so
  `tests/test_gc_generations.py` has to provoke them with churn rather than ask
  for one.


- **`s += x` in a loop is O(n^2)**: `str_concat` always allocates, and
  `src/opcodes/arith.asm` routes `NB_INPLACE_ADD` to the same `sq_concat`, so
  each step copies the whole accumulated string.  CPython's ceval resizes in
  place when the left operand's refcount is 1.  Measured, though, the two are
  level: repeated appends cost the same here as under CPython 3.12, because
  that optimization does not fire for the ordinary module-level accumulator
  either.  Doing it would make apython faster than
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
whoever copies a neighbouring file.  Counts are deliberately absent -- grep
gives a current one, and a number written here is wrong by the next commit.

- **Frames whose `rsp` is not 16-byte aligned at a `call`.**  Every
  `XX_FRAME equ` now carries the arithmetic in a trailing comment, and writing
  those out is what made the scale visible: a good many say
  `not 16-aligned`, and `src/compiler/lint.py`'s `check_alignment` flags several
  times that number again in functions whose frame size is a literal rather
  than a named constant.  Mostly harmless -- the SysV requirement bites at a
  `call` into libc, and most of these never reach one -- but `builtin_int_fn`,
  `builtin_round_fn` and `builtin_pow_fn` all reach GMP, whose float paths use
  aligned SSE.  This is the debt that keeps `check_alignment` scoped to
  `src/compiler/` plus `src/main.asm` instead of running tree-wide.

- **Functions with no separator or docblock at all**, and, among those that
  have one, docblocks with no `->` signature line.  The signature is the only
  part of a function's contract that nothing checks, so its absence is a real
  gap rather than a cosmetic one.  This is the one item here a script cannot
  finish: writing a signature means reading what the function actually returns.
