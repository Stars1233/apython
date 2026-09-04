# Deliberate divergences

Places apython answers differently from CPython 3.12 **on purpose**.  Each was
weighed and each was chosen; none is a defect waiting for a patch, and none
belongs in `bugs.md`, which is the list of what is still wrong.

Every entry says what the difference is, why the choice was made, and what
changing it would cost -- so that a later decision to revisit one starts from
the reasoning rather than from scratch.

## Object layout and introspection

- **No managed dicts, so the layout attributes differ for any class that has
  an instance `__dict__`.**  CPython 3.12 keeps a class's instance dict in a
  slot before the object rather than in the object, and reports that as
  `__dictoffset__ == -1` and `__weakrefoffset__ == -32`; the header itself is
  then 16 bytes.  Here the dict is a word inside the instance, so
  `class A: pass` reports a positive `__dictoffset__` and a header one word
  wider.  A `__slots__` class has no dict of its own and matches CPython
  exactly.

  Nothing in Python reads these except code that is measuring the layout, and
  a managed dict is a whole allocation strategy -- the values are honest about
  the layout apython actually has.

- **Weak references live in a side table, not in the object, so
  `__weakrefoffset__` is 0 everywhere.**  The links are kept in one dict keyed
  by the referent's address and `obj_dealloc` consults it, which works and is
  tested; what it cannot do is answer CPython's question, because there is no
  offset to report.

  Putting the head in the object is not the 95-line edit it looks like.  The
  `PyTypeObject` field is the easy part.  The word itself has to go in the
  *instances* of all seventeen static types CPython lets you weak-reference --
  set, frozenset, type and both metatypes, function, builtin, method,
  generator, coroutine, async generator, module, memoryview, code, file,
  BytesIO, Task -- which means a field on each of those structs, a zeroing
  store at each of their forty-odd allocation sites, a release in each
  dealloc, and, because `type` is one of them, a second new `PyTypeObject`
  field and another pass over all 94 tables.  A missed zeroing store is a
  garbage pointer walked as a list.  The whole of it buys one number that
  still would not match CPython's, since the basicsizes it is an offset into
  differ anyway.

- **A frame object is a snapshot, so `f_lineno` is where the frame was when
  it was taken.**  CPython's is a live view onto a frame that is still
  running, and reports where it is when the attribute is READ:
  `f = sys._getframe()` on one line and `f.f_lineno` on the next answers the
  second line there and the first here.  Everything that reads it immediately
  -- which is every use in the stdlib -- agrees.  Making it live means the
  frame object holding the PyFrame rather than copying it, and the PyFrame
  outliving the call.

## The platform surface

- **`posix` is a subset, and a deliberate one.**  The file, directory and
  process calls `os.py` and `os.path` reach for are there, along with
  `environ`, `stat_result`, `error` and the O_*/W* constants -- enough that
  CPython's own `os.py` imports and works.  What is not: `scandir` and
  `DirEntry`, `fork`, `execv`, and the whole `*at` family.
  `_have_functions` is an empty list, which is the honest answer -- no
  `dir_fd=` support -- and os.py reads it to build `supports_dir_fd`.

- **`_thread` is a single-threaded stand-in.**  `lib/_thread.py` gives
  `get_ident` a constant, makes locks uncontended, and raises from
  `start_new_thread`.  Everything in the stdlib that only takes a lock works;
  anything that expects a second thread does not.

- **`sys.getfilesystemencoding()` always answers `'utf-8'`.**  PEP 540's
  locale handling does not exist, and neither does the `surrogateescape`
  error handler, so a filename or environment value that is not valid UTF-8
  does not survive a decode/encode round trip, where CPython preserves it.
  The other half of why is in `bugs.md`: the codecs the interpreter can spell
  itself are utf-8, ascii and latin-1, and `surrogateescape` is not among the
  error handlers any of them accept.

## Performance, where the answer is already right

- **`collections.deque` is list-backed, and two itertools functions
  materialise.**  CPython's deque is a block-linked list, so `appendleft` and
  `popleft` are O(1) there and O(n) here; `itertools.groupby` materialises
  each group rather than sharing the source iterator, and `tee` materialises
  the source.  Every observable answer matches for a finite iterable.

- **bytearray's read-only methods copy.**  bytes keeps its data inline and
  bytearray keeps it out of line, so the shared method bodies cannot read a
  bytearray directly; each wrapper builds a temporary bytes, runs the bytes
  body and releases it.  Correct, and cheap for a scratch buffer, but it is
  an allocation per call -- worth threading a (pointer, length) pair through
  the bodies if bytearray ever becomes hot.

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

## Interpreter structure

- **C code here cannot catch a Python exception.**  `raise_exception`
  tail-jumps into `eval_exception_unwind`, which resumes the eval loop from
  saved globals rather than returning through the C stack, so a `call` to a
  slot that raises never comes back.  `str.translate` gets around it by
  reaching a heaptype table through `dunder_call_2`, which does return; the
  general limit stands, and is why the `bytes %` leak recorded in `bugs.md`
  cannot be fixed by catching.

## Test oracles

Three tests compare against a recorded transcript in `tests/expected/` rather
than against CPython, because CPython cannot serve as an oracle for them:

- `test_sre.py` feeds hand-written SRE bytecode to `_sre.compile()`, a
  private API that does not validate its input; CPython segfaults on the
  group pattern it uses.
- `test_traceback_carets.py` and `test_unraisable.py` both let an exception
  be reported on stderr, and the report names the file: CPython absolutizes
  the path of a script it runs directly and a run from a `.pyc` does not.
  `test_unraisable.py` also prints on both streams, which the two interpreters
  interleave differently -- see the stdout buffering entry in `bugs.md`.
  Every line of both was compared against CPython modulo those two before it
  was recorded.

Any *new* recorded-oracle test needs the same justification, or it risks
blessing a divergence instead of catching it.
