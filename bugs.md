# Known bugs

Open items only.  A bug that has been fixed belongs in the commit that fixed
it, not here; this file is the list of what is still wrong.

Every entry below was reproduced against the current build.  Each says what
the difference from CPython 3.12 is and, where it is known, why it is not a
one-line fix.

## Correctness

- **`bytearray` has no `%`.**  `+`, `*`, `+=` and `*=` all work now, through
  `bytearray_seq_methods`; `bytearray(b"%d") % 5` is still a TypeError, since
  `bytearray_type.tp_as_number` is 0 and nothing supplies `nb_remainder`.
  `tests/test_binop_matrix.py` skips that cell rather than blessing it.

- **`%`-formatting takes only a tuple or a mapping on the right.**  CPython
  also accepts a single arbitrary object, so `"ab" % [1, 2]` is `'ab'` there
  and a TypeError here.

- **`bytes` `%`-formatting converts through `str_mod`**, so its conversions are
  str's rather than bytes'.  `b"%s" % b"x"` is right now, but `b"%d" % "x"`
  answers `b'x'` where CPython raises, `b"%s" % "x"` answers `b'x'` where
  CPython raises, `b"%r" % b"x"` is `b"'x'"` rather than `b"b'x'"`, `%c` and
  `%a` are TypeErrors, and a `bytes`-keyed mapping (`b"%(a)d" % {b"a": 1}`)
  raises KeyError.

- **`dict.__ior__` takes only a dict.**  CPython's takes any iterable of
  key/value pairs.

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

- **`str.encode` and `bytes.decode` know only utf-8, ascii and latin-1.**
  Any other name is a LookupError, where CPython would find the codec through
  the registry; reaching it from the interpreter would mean calling Python
  from a builtin method.  The `errors` argument is accepted and ignored --
  every failure is strict.

- **`str.format` does not accept attribute or index access in a field.**
  `"{0.attr}"` and `"{0[key]}"` are not resolved; a field is a position, a
  name, or empty.  A nested spec, `"{:{}}"`, is likewise not substituted.

- **`func.__name__ = "x"` is silently ignored.**  Functions have no settable
  attributes.

- **Dict views have no repr and no contents.**  `repr(d.keys())` is empty
  where CPython gives `dict_keys(['a'])`; the same for `.values()` and
  `.items()`.

- **`bytearray` is not reversible.**  `b[0]`, `b[1:]` and `b[::-1]` all work
  now; `reversed(bytearray(...))` is still a TypeError, because the type has
  no `tp_iter`-side answer for it.

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

- **The binary operator dunders are still not reachable by name.**  The unary
  ones are: `int` and `float` register `__neg__`, `__pos__`, `__abs__`,
  `__invert__`, `__int__`, `__float__`, `__index__`, `__trunc__` and
  `__bool__`.  `int.__add__` and the rest of the binary family, reflected and
  in-place, are still absent -- `dir(int)` is short by about forty names, and
  a class delegating to `int.__add__` finds nothing.

- **`bytes` and `bytearray` are missing most of the string-like methods.**
  Both have `find`, `count`, `startswith`, `endswith`, `split`, `join`,
  `replace`, `rsplit`, `hex` and `decode`; neither has `rfind`, `index`,
  `rindex`, `splitlines`, `strip`/`lstrip`/`rstrip`, `partition`/
  `rpartition`, `upper`/`lower`/`title`/`swapcase`/`capitalize`,
  `center`/`ljust`/`rjust`, `zfill`, `expandtabs`, `translate`, or the
  `is*` predicates.  `bytearray(str, encoding)` is not accepted either.

- **bytearray's read-only methods copy.**  bytes keeps its data inline and
  bytearray keeps it out of line, so the shared method bodies cannot read a
  bytearray directly; each wrapper builds a temporary bytes, runs the bytes
  body and releases it.  Correct, and cheap for a scratch buffer, but it is
  an allocation per call -- worth threading a (pointer, length) pair through
  the bodies if bytearray ever becomes hot.

- **The regex engine differs from CPython in 41 of 816 checked answers.**
  `make check-re` runs `tests/re_differential.py` under both interpreters
  and ratchets against `tests/re_floor.txt`; it needs `$CPYTHON_LIB`,
  because `re` is a Python module and so comes from a real stdlib.  What is
  left, from that diff:

  - An unmatched group reads as `''` with span `(0, 0)` where CPython gives
    `None` and `(-1, -1)`.  `lastindex` is off by one and `lastgroup` is
    always `None`.
  - `findall` and `split` return the whole match where a pattern has exactly
    one group; CPython returns the group.  Both also mishandle a zero-width
    match when advancing.
  - `Match.expand()` and `re.sub`'s `\1` group references are not
    implemented -- the template comes back unchanged.
  - A nested unbounded repeat (`(a*)*b`) recurses until the limit.
  - `()` fails to compile, and `[a-zA-Z0-9_]+` needs `bytes.translate`,
    which does not exist.
  - `bytes` patterns and subjects are unsupported: `sre_state_init` always
    treats the subject as a `PyStrObject` and hardcodes `is_bytes = 0`.

- **`str.translate` cannot take a miss signalled by an exception.**  A table
  with a length -- dict, list, tuple, str, bytes -- is exact, because the
  bound is checked before the lookup.  A mapping object whose `__getitem__`
  raises `LookupError` to mean "not in the table" propagates instead, where
  CPython carries on.  Catching it is not possible: `raise_exception`
  tail-jumps into `eval_exception_unwind`, which resumes the eval loop from
  saved globals rather than returning through the C stack, so a `call` to a
  slot that raises never comes back.  The same limit applies anywhere C code
  here would want to catch an exception.

- **`str.encode` honours no error handler, and `bytes.decode` only honours
  one for utf-8.**  `bytes.decode` reads `errors=` through `codec_error_id`
  and acts on it in the utf-8 fixup loop, but its `ascii` arm jumps straight
  to `.bd_not_decodable` on the first high byte; `str.encode` parks the
  argument in `SE_ERRS` and never passes it to `codec_error_id` at all.  So
  `b"a\xffb".decode("ascii", "ignore")` and `"a\u1234b".encode("ascii",
  "ignore")` both raise where CPython answers `'ab'` and `b'ab'`, and an
  unknown handler name is not reported as a LookupError on those paths
  because it is never looked up.

- **`sys.getfilesystemencoding()` always answers `'utf-8'`.**  PEP 540's
  locale handling does not exist, and neither does the `surrogateescape`
  error handler, so a filename or environment value that is not valid UTF-8
  does not survive a decode/encode round trip, where CPython preserves it.
  The entry above is the other half of why.

- **`complex.conjugate` resolves to the bare builtin rather than to a method
  descriptor.**  `real` and `imag` are getset descriptors now, on `int`,
  `float`, `complex` and `bool`; the *methods* a builtin type registers are
  still plain `PyBuiltinObject`s, so `int.bit_length` reprs as `bit_length`
  where CPython says `<method 'bit_length' of 'int' objects>`.  They are
  callable unbound either way.

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

- **`posix.symlink` and `posix.readlink` do not exist.**  `os.stat` honours
  `follow_symlinks=False` and `posix.lstat` works, so links can be inspected;
  they just cannot be created or read from inside the interpreter, which is
  also why the regression test for `follow_symlinks` has to make do with a
  regular file.

- **`async for` accepts only an async generator.**  A class implementing the
  asynchronous iterator protocol itself -- `__aiter__` returning self and an
  `async def __anext__` raising `StopAsyncIteration` -- is refused with
  `TypeError: 'async for' requires an object with __aiter__ method`, though it
  defines exactly that.  `asyncio` streams and every hand-written async
  iterator are shaped this way.

- **An extended-slice length mismatch does not name the sizes.**  CPython says
  "attempt to assign sequence of size 3 to extended slice of size 2"; here the
  message is "of wrong size", because `src/pyo/list.asm` has no int-to-string
  helper to build the two numbers with -- the one that exists is file-local to
  `src/opcodes/build.asm`.  The exception type and the condition are right.

- **`bytes % args` leaks its temporary when the format is malformed.**  The
  work is done by handing a decoded copy of the format and the arguments to
  `str_mod`, and `str_mod` RAISES for a wrong argument count -- a raise
  abandons the C stack, so neither the copy nor the converted arguments are
  released.  Putting them somewhere the unwinder frees would be worse: an
  argument's `__str__` can run Python, and a raise caught inside it would
  free a buffer `str_mod` is still reading.

- **A `__del__` that raises and catches internally destroys an exception
  that is already unwinding.**  The finaliser prints "Exception ignored in
  __del__" and execution then continues as though nothing had been raised.

- **Two `posix` messages name fewer paths than CPython's.**  `rename` reports
  only its source where CPython reports `'src' -> 'dst'`, and the
  "path should be string, bytes, or os.PathLike" TypeError carries no
  function-name prefix and no "or integer" variant for the calls that take a
  descriptor.  The resolved path itself does reach both.

- **`str()` of a `UnicodeDecodeError` prints its argument tuple.**  CPython
  renders "'ascii' codec can't decode byte 0xc3 in position 1: ordinal not in
  range(128)" from the exception's fields; here the five arguments come out
  as a tuple repr.  The same applies to `UnicodeEncodeError`.

- **A subclass of `_io.FileIO` cannot declare `__slots__`.**  FileIO stores
  its descriptor and flags past the instance header, in the same words a
  subclass's slots would land in: both are placed relative to the base's
  `tp_dictoffset`, which the subclass inherits.  Nothing detects the
  collision.

- **`@abc.abstractmethod` is not enforced.**  A class with an unimplemented
  abstract method instantiates.  `__abstractmethods__` is consulted by
  `type_call`, so the missing half is abc's own bookkeeping rather than the
  interpreter's check.

- **A memoryview with a step other than 1 is not a view.**  `mv[::2]` and
  `mv[::-1]` raise NotImplementedError.  CPython answers with a
  non-contiguous view, which needs a stride the object does not carry -- and
  a stride would have to be honoured by every reader: `tobytes`, iteration,
  `bytes()`, comparison, `hex`, `tolist`, and the write path.  Nothing in
  `_pyio` asks for one, so the field is not there yet.

- **`raise_oserror` leaks its message string.**  Every `OSError` raised from
  `src/posixmod.asm` -- a `mkdir` on an existing directory, an `rmdir` on a
  missing one -- leaves the string `str_from_cstr_heap` built for it.  A loop
  that probes the filesystem with try/except leaks once per attempt.

- **An abandoned generator never releases its frame.**  Taking one value from
  a generator expression and dropping it leaks the frame and whatever the
  frame's stack holds, including the iterator it was walking.  CPython closes
  a generator when it is collected; there is no equivalent here.

- `time.time` and `time.sleep`.  The `time` module has only `monotonic` and
  `process_time`; `asyncio.sleep` exists.
- `itertools.zip_longest`, `permutations`, `combinations`, `takewhile`,
  `dropwhile`, `filterfalse`, `groupby`, `tee`, `pairwise`.
- The `re` wrapper module.  The `_sre` engine underneath is complete, but
  without a shipped `re.py` an `import re` finds CPython's, which needs
  `enum` and `types`.
- `collections.deque`.

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
