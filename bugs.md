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

- **Missing C modules.**  The ranking here used to be by how often each was
  the FIRST import to fail, which is not the same as how many modules each
  blocks: twelve of the thirteen that stopped at `_ast` import
  `importlib.machinery` a few lines later.  Measured by what actually stands
  in the way, over CPython 3.12's 196: `_imp` (with `marshal` and
  `_warnings`) 27, `_hashlib` and the `_sha*`/`_md5` family, `array`,
  `_typing`, `_posixsubprocess`, `_signal`, `_csv`, `pyexpat`, then a long
  tail of one apiece.
  (`_io` is not among them: `src/iomod.asm` supplies `_iocore` and
  `lib/_io.py` assembles both halves under the name `_io`.  `_socket` and
  `select` are the same split over `_socketcore`.  Neither are `math`,
  `_collections`, `_struct`, `_random`, `_contextvars`, `_string`,
  `_tokenize`, `_operator`, `binascii`, `atexit` and `_ast`, which are
  there.)  `make check-stdlib` gives the current figure: 129 of 196.

  `hashlib` imports but has no digests, because every one of them is a C
  module here as well.

  `math`'s `gamma`, `lgamma`, the n-ary `hypot` and `sumprod` round
  differently from CPython's, which uses its own Lanczos approximation and
  double-double arithmetic where these use glibc and a Neumaier sum.  `dist`
  shares `hypot`'s routine and so shares the note.  `fsum` is exact: it is
  Shewchuk's algorithm, as CPython's is.  `tests/test_math.py` says which is
  which.

- **`sys.exc_info()` is empty after an `await` inside an `except` block.**
  CPython saves a frame's exception state and puts it back when the frame
  resumes; the state here is one global plus a copy on the value stack that
  POP_EXCEPT restores, and a suspended frame has no way to reinstate its own.
  So inside `except E: ... await f(); sys.exc_info()` the handler's exception
  reads as None from the await onwards -- `raise` with no argument in that
  position, and any logging that reads exc_info(), see nothing.  Everything
  either side of the await is right, and the exception is still caught: it is
  only the *ambient* state that is lost.  Fixing it means the exception state
  moving onto the frame.

- **A gather cannot be nested inside another.**  `gather(gather(...))` is a
  TypeError here and `[[3]]` in CPython, where a gather returns a future and
  ensure_future takes it as it stands.  The awaitable this returns is not a
  task and cannot be stepped like one, so wrapping it means wrapping an
  arbitrary awaitable in a coroutine, which is what `ensure_future` does and
  what task_new has no way to do.  Until this commit it was a segfault
  rather than a refusal.

- **asyncio's stream layer is a stub, and now an unnecessary one.**
  `src/pyo/asyncio_streams.asm` predates any socket support: it hard-codes
  127.0.0.1 and ignores the `host` argument it is given, discards what
  `connect` returns, reads into a fixed stack buffer, hands back a `str`
  where CPython hands back `bytes`, and raises OSErrors built from fixed
  strings with no errno -- so `except ConnectionRefusedError` cannot catch
  one.  It has no test.  There is a real socket layer under it now
  (`_socketcore`, `lib/_socket.py`), and the stream types should be rewritten
  on top of it rather than on raw syscalls of their own.

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

- **A `SyntaxError` carries none of its attributes.**  CPython sets `.msg`,
  `.filename`, `.lineno`, `.offset` and `.text` from the exception's own
  args, and `str(e)` appends " (file, line N)" to the message.  Here the
  args are there -- the traceback header renders the file, the line and the
  caret out of them -- but `exc_getattr` does not map any of the five names,
  so `e.msg` is an AttributeError and `str(e)` is the bare message.  Every
  tool that reports a syntax error reads at least `.lineno`.

- **`__slots__` names are not mangled.**  `class C: __slots__ = ('__x',)`
  then `self.__x = 5` is an AttributeError here: CPython's `type_new` mangles
  each slot name as it builds the member descriptor -- leaving `__slots__`
  itself as written -- and `type_from_parts` builds them raw, so the
  descriptor is `__x` where every use of it compiles to `_C__x`.  A legal
  program that CPython runs and this does not.

- **`ast.parse` is missing two of its arguments and one of its modes.**
  `type_comments=True` collects nothing, because the tokenizer discards
  comments and has nowhere to put a `# type:` one; `mode="func_type"` is a
  ValueError, because there is no `(int, str) -> bool` start symbol; and
  `mode="single"` accepts more than CPython's does -- its grammar is
  `NEWLINE | simple_stmt | compound_stmt NEWLINE`, so `def f(): pass` without
  a trailing newline and the empty string are both syntax errors there and
  are accepted here.  The tree `single` produces is right: it is
  `Interactive`, and it compiles as `exec` does.

- **PEP 695 exists in the tree and not at run time.**  `ast.parse` now builds
  `TypeAlias`, `TypeVar`, `ParamSpec` and `TypeVarTuple`, and the brackets are
  a grammar rather than a bracket-depth skip, but the code generator still
  lowers `type X = V` to the assignment `X = V` and still discards
  `type_params`.  So `X` is the value itself rather than a `TypeAliasType`
  with a lazily evaluated `__value__` and the alias's own repr, there are no
  runtime `TypeVar` objects, and the annotation scope PEP 695 opens for the
  parameters is not opened -- a bound or a `type` value that names one is a
  NameError where CPython defers it.  A CPython-produced `.pyc` says the same
  thing from the other side: `CALL_INTRINSIC_1` 7, 10 and 11 and
  `CALL_INTRINSIC_2` 2, 3 and 4 are the intrinsics that build these, and they
  raise `SystemError` naming the selector.  (They used to call `fatal_error`
  and kill the interpreter.)  `type_comment` is permanently None for a
  different reason: there are no type comments.

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

  Half of what that needs is now there: every AST node carries all four of
  CPython's positions, and `tests/test_ast.py` diffs them against CPython's
  own `ast.dump(include_attributes=True)`.  What is left is two more fields
  on `Instr`, the start and end columns at all 300-odd `cg_emit` call sites,
  and form 14 in `asm_linetable` -- and then the spans would have to agree
  with CPython's choice of which subexpression each opcode belongs to, which
  is not obvious and is not written down anywhere but its compiler.

- **A frame object is a snapshot, so `f_lineno` is where the frame was when
  it was taken.**  CPython's is a live view onto a frame that is still
  running, and reports where it is when the attribute is READ:
  `f = sys._getframe()` on one line and `f.f_lineno` on the next answers the
  second line there and the first here.  Everything that reads it immediately
  -- which is every use in the stdlib -- agrees.  Making it live means the
  frame object holding the PyFrame rather than copying it, and the PyFrame
  outliving the call.

- **A `str` subclass cannot declare `__slots__`.**  CPython accepts it; here
  it is a TypeError, worded as CPython words the ones it does refuse
  (`nonempty __slots__ not supported for subtype of 'str'`).  A str keeps its
  characters inline and a subclass keeps its dict at the tail past them, so a
  slot at a fixed offset lands on the characters -- it wrote over its own data
  and then crashed.  int, bytes and tuple are refused for the same reason and
  CPython refuses those too; str is the one that differs.  Making it work
  means slots at the tail, which is a layout change reaching
  `instance_dealloc`, `instance_traverse` and the member descriptors.

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

Three tests compare against a recorded transcript in `tests/expected/` rather
than against CPython, because CPython cannot serve as an oracle for them:

- `test_sre.py` feeds hand-written SRE bytecode to `_sre.compile()`, a
  private API that does not validate its input; CPython segfaults on the
  group pattern it uses.
- `test_traceback_carets.py` and `test_unraisable.py` both let an exception
  be reported on stderr, and the report names the file: CPython absolutizes
  the path of a script it runs directly and a run from a `.pyc` does not.
  `test_unraisable.py` also prints on both streams, which the two interpreters
  interleave differently -- see the buffering entry above.  Every line of both
  was compared against CPython modulo those two before it was recorded.

Any *new* recorded-oracle test needs the same justification, or it risks
blessing a divergence instead of catching it.

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

- **`src/pyo/class.asm` is over CLAUDE.md's 100k limit for a hand-written
  file.**  It holds the metatype, the instance and the bound method, and the
  seam between them is not as clean as the one bytes.asm had.

- **Functions with no separator or docblock at all**, and, among those that
  have one, docblocks with no `->` signature line.  The signature is the only
  part of a function's contract that nothing checks, so its absence is a real
  gap rather than a cosmetic one.  This is the one item here a script cannot
  finish: writing a signature means reading what the function actually returns.
