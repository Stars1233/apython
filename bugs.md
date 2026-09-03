# Known bugs

Open items only.  A bug that has been fixed belongs in the commit that fixed
it, not here; this file is the list of what is still wrong.

Every entry below was reproduced against the current build.  Each says what
the difference from CPython 3.12 is and, where it is known, why it is not a
one-line fix.

## Correctness

- **`__context__` is not set across `await SOME_TASK`** where the task
  raised.  `current_exception` is a single global and task switching does not
  follow it, so the exception the awaiting coroutine was handling is not what
  is current when the task's exception is re-raised.  The other two shapes --
  a generator resume, and `await` on a coroutine -- are right.

  The fix is a per-task exception state.  It was tried, and it needs
  `task_step` to tell a raise from a `return` out of an except block, which
  it currently cannot: both leave `current_exception` set.

- **`super` is not an object.**  The zero- and two-argument forms both work
  written as `super(...).attr`, because the compiler emits `LOAD_SUPER_ATTR`
  for exactly that shape and the opcode does the MRO walk itself.  What does
  not exist is a super *object*: `s = super()` and `s = super(B, self)` both
  raise "object is not callable", because `super_type` is a placeholder with
  no `tp_call`, no `tp_new` and no fields -- a stand-in that
  `LOAD_SUPER_ATTR` pops and discards.  Anything that stores one, passes one,
  or reaches it through `getattr` fails.  `super(B, B).m` also answers a
  bound method where CPython answers the plain function.

  The fix is a real three-field type and a `tp_getattr` doing the walk that
  `op_load_super_attr` already does inline, plus the frame introspection the
  zero-argument form needs: CPython reads the `__class__` cell and the first
  positional argument out of the calling frame.

- **`str.encode` and `bytes.decode` know only utf-8, ascii and latin-1.**
  Any other name is a LookupError, where CPython would find the codec through
  the registry; reaching it from the interpreter would mean calling Python
  from a builtin method.

- **The `_abc` registry and caches hold strong references.**  CPython uses
  weak ones, so a class registered against an ABC can be collected and the
  ABC's caches shrink; here a registered class lives as long as the ABC.
  Registries are process-lifetime and small in practice.  Revisit if
  `_weakref` lands.

- **`posix` is a subset, and a deliberate one.**  The file, directory and
  process calls `os.py` and `os.path` reach for are there, along with
  `environ`, `stat_result`, `error` and the O_*/W* constants -- enough that
  CPython's own `os.py` imports and works.  What is not: `scandir` and
  `DirEntry`, `fork`, `execv`, and the whole `*at` family.
  `_have_functions` is an empty list, which is the honest answer -- no
  `dir_fd=` support -- and os.py reads it to build `supports_dir_fd`.

- **Missing C modules**, in rough order of how many stdlib modules each
  blocks: `_ast`, `_socket`, `_imp`, `_hashlib` and the `_sha*`/`_md5`
  family, `_csv`, `pyexpat`, `_typing`, then a long tail of one apiece.
  (`_io` is not among them: `src/iomod.asm` supplies `_iocore` and
  `lib/_io.py` assembles both halves under the name `_io`.  Neither are
  `math`, `_collections`, `_struct`, `_random`, `_contextvars`, `_string`,
  `_tokenize`, `_operator`, `binascii` and `atexit`, which are there now --
  the last nine in `lib/`.)  `make check-stdlib` gives the current figure:
  107 of 196.

  `_ast` is the largest of what is left.  The arena AST cannot be exposed as
  it stands: 32-byte POD addressed by a u32 index, freed wholesale at the end
  of a compile, and its shape does not match CPython's `_fields`.  It needs
  its own object model and `PyCF_ONLY_AST` in `builtin_compile_fn`.

  `hashlib` imports but has no digests, because every one of them is a C
  module here as well.

  `math`'s `gamma`, `lgamma`, the n-ary `hypot` and `sumprod` round
  differently from CPython's, which uses its own Lanczos approximation and
  double-double arithmetic where these use glibc and a Neumaier sum.  `dist`
  shares `hypot`'s routine and so shares the note.  `fsum` is exact: it is
  Shewchuk's algorithm, as CPython's is.  `tests/test_math.py` says which is
  which.

- **Weak references keep no per-object slot.**  The links live in a side
  table keyed by the referent's address rather than in the object, so
  `tp_weaklistoffset` does not exist and `__weakref__` is not an attribute.
  Everything observable through `_weakref` works, including which types
  refuse a reference; a C extension expecting the slot would not.

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

- **C code here cannot catch a Python exception.**  `raise_exception`
  tail-jumps into `eval_exception_unwind`, which resumes the eval loop from
  saved globals rather than returning through the C stack, so a `call` to a
  slot that raises never comes back.  `str.translate` gets around it by
  reaching a heaptype table through `dunder_call_2`, which does return; the
  general limit stands, and is why the `bytes %` leak below cannot be fixed
  by catching.

- **`sys.getfilesystemencoding()` always answers `'utf-8'`.**  PEP 540's
  locale handling does not exist, and neither does the `surrogateescape`
  error handler, so a filename or environment value that is not valid UTF-8
  does not survive a decode/encode round trip, where CPython preserves it.
  The entry above is the other half of why.

- **A classmethod on a builtin type reprs as a bound method.**  Ordinary
  methods, slot wrappers and getsets all name themselves and their owner now;
  `int.from_bytes`, `float.fromhex` and `str.maketrans` are wrapped in a
  classmethod object, which `type_stamp_methods` skips, so they answer
  `<bound method from_bytes of <class 'int'>>` where CPython answers
  `<built-in method from_bytes of type object at 0x...>`.

- **`complex()` of a string does not accept Unicode spaces or Unicode digits.**
  CPython runs `_PyUnicode_TransformDecimalAndSpaceToASCII` first, so
  `complex("\u30001+2j")` parses there; here any byte past ASCII is a
  malformed string.  ASCII whitespace, brackets, underscores, `inf` and `nan`
  all behave as CPython's do.

- **`_thread` is a single-threaded stand-in.**  `lib/_thread.py` gives
  `get_ident` a constant, makes locks uncontended, and raises from
  `start_new_thread`.  Everything in the stdlib that only takes a lock works;
  anything that expects a second thread does not.

- **A module's `__file__` is the `.pyc` it was loaded from, where CPython's is
  the `.py`.**  Visible in the attribute and in `repr(module)`, which prints
  it.  CPython records the source path even when it executes a cached
  bytecode file, and derives the cache path from it when it needs to.

- **Our own compiler records no columns, so a traceback from a `.py` has no
  caret line.**  The renderer draws one whenever the location table has
  columns, and a `.pyc` CPython produced always does -- the reports match
  byte for byte there.  `asm_linetable` emits only form 13, "no columns", so
  running the same file from source loses the caret row.

  Threading columns through would mean an end position on every AST node, two
  more fields on `Instr`, and the start and end columns at all 300-odd
  `cg_emit` call sites -- and then the spans would have to agree with
  CPython's choice of which subexpression each opcode belongs to, which is
  not obvious and is not written down anywhere but its compiler.

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

## Missing pieces

These are absences rather than wrong answers — the interpreter raises rather
than lying — but they are ordinary Python that does not work:

- **The unwinder's one-reference invariant is real; the drop it caused is
  not reproducible.**  `raise_exception_obj` takes over its caller's
  reference rather than adding one, so `current_exception` usually holds
  exactly one -- and `eval_exception_unwind` runs arbitrary finalizers while
  that is true, releasing the value stack before it is done with the global.
  This file used to record a concrete drop: `current_exception` pointing at
  an object whose `ob_refcnt` was already 0, reproducible with an `async for`
  over a hand-written `__anext__` that raises.  It no longer happens.  That
  repro is clean, valgrind is clean over the async suite, and a gdb watch for
  `current_exception != 0 && ob_refcnt <= 0` at `instance_dealloc`'s entry
  gets no hits across the corpus.  `gen_dealloc` held the pending exception
  borrowed in a register across `gen_dealloc_close`, which is the shape that
  could produce it, and takes a reference now.  `instance_dealloc`'s refcount
  check stays: it is two instructions, and the invariant it guards is one a
  future caller can break again.

- **`bytes % args` leaks its temporary when the format is malformed.**  The
  work is done by handing a decoded copy of the format and the arguments to
  `str_mod`, and `str_mod` RAISES for a wrong argument count -- a raise
  abandons the C stack, so neither the copy nor the converted arguments are
  released.  Putting them somewhere the unwinder frees would be worse: an
  argument's `__str__` can run Python, and a raise caught inside it would
  free a buffer `str_mod` is still reading.

- **The asm codec sites pre-render their exceptions.**  A decode error
  carries CPython's five fields (`encoding`, `object`, `start`, `end`,
  `reason`) and its message names the codec, the byte and the position; what
  the asm sites still cannot do is raise a real five-argument
  `UnicodeEncodeError` the way `lib/_codecs.py` does, because `exc_new` has
  no way to call an exception type with five arguments.  The *encode* arms
  therefore build CPython's wording by hand and set no fields.

- **A memoryview with a step other than 1 is not a view.**  `mv[::2]` and
  `mv[::-1]` raise NotImplementedError.  CPython answers with a
  non-contiguous view, which needs a stride the object does not carry -- and
  a stride would have to be honoured by every reader: `tobytes`, iteration,
  `bytes()`, comparison, `hex`, `tolist`, and the write path.  Nothing in
  `_pyio` asks for one, so the field is not there yet.

- **stdout is not block-buffered when it is not a terminal.**  CPython's is,
  so a program that writes to both streams through a pipe sees all its
  `print()` output after everything on stderr; here the two interleave as
  they were written.  Visible in any test that lets an exception be reported
  while it is also printing, which is why two of them compare against a
  recorded transcript.

- **An exception raised while another is being handled by a `finally` gets a
  `__context__` where CPython gives it none.**  `current_exception` is also
  the exception being handled, and a `finally` body runs with it set, so a
  raise there chains it.  CPython's exception stack distinguishes the two,
  and only an `except` block counts.  Visible in the report for a generator
  whose cleanup raises: the GeneratorExit appears as context.


## Robustness

- **A builtin registered with no argument counts accepts extras silently.**
  `str.upper("a", 1)` answers 'A' where CPython raises.  The shared arity
  machinery reports CPython's counted wording wherever `min_args`/`max_args`
  were registered; what is left is per-method, and CPython's own wordings
  there are inconsistent between clinic-generated and hand-written methods.

- **The three async awaitable types are not GC-tracked.**
  `WaitForAwaitable`, `GatherAwaitable` and `SleepAwaitable` are ap_malloc'd
  with no `TYPE_FLAG_HAVE_GC` and no traverse, so a cycle through one of them
  never collects.  `Task` itself is tracked now, and the awaitables keep
  counted references to the tasks they hold, which is conservative in the
  safe direction: an untracked holder's reference is not subtracted, so a
  task it holds looks reachable and is never freed early.

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

- **`gc` has no `get_referrers`, `freeze`/`unfreeze` or `get_stats`.**
  `get_referrers` needs a reverse edge nothing records -- CPython finds it by
  traversing every tracked object and asking whether it points at the
  argument, which this could do too and which is O(heap) per call.  The
  freeze family and `get_stats` are about machinery this collector does not
  have: there is no permanent generation and no per-pass statistics.

- **Two of CPython's tracking optimizations are absent, and both are
  visible.**  CPython untracks a dict whose contents are all untrackable, so
  `gc.is_tracked({})` is False there and True here; and a dict whose keys are
  all strings shares its key table, so `gc.get_referents` on one answers with
  the values only, where this answers with the keys as well.  Both are
  conservative in the safe direction -- more is tracked, more is reported.


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

- **`src/pyo/bytes.asm` is 178k, against CLAUDE.md's 100k limit for a
  hand-written file.**  It holds both bytes and bytearray, and the natural
  seam is exactly that -- bytearray's own methods and its resizable storage
  are the half that grew.  `src/methods/init.asm` is the next one up at 90k,
  and its 320 registration sites are four open-coded instructions apiece;
  one row each would take it to about 45k.

- **Functions with no separator or docblock at all**, and, among those that
  have one, docblocks with no `->` signature line.  The signature is the only
  part of a function's contract that nothing checks, so its absence is a real
  gap rather than a cosmetic one.  This is the one item here a script cannot
  finish: writing a signature means reading what the function actually returns.
