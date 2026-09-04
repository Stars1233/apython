# Known bugs

Open items only.  A bug that has been fixed belongs in the commit that fixed
it, not here; this file is the list of what is still wrong.

Every entry below was reproduced against the current build.  Each says what
the difference from CPython 3.12 is and, where it is known, why it is not a
one-line fix.

Divergences that are *deliberate* -- the `posix` subset, the absence of managed
dicts, a single-threaded `_thread`, the three recorded-oracle tests, and the
rest -- are not bugs and are not here.  They live in `DIVERGENCES.md`, with the
reasoning that chose them and what changing one would cost.

## Correctness

- **Comprehensions are not inlined, so PEP 709's effects are missing.**
  CPython 3.12 runs a list, dict or set comprehension in the *enclosing*
  frame; here each still gets a code object and a frame of its own.  Three
  things follow.  `sys._getframe().f_code.co_name` inside one answers
  `<listcomp>` rather than the enclosing function's name, and a traceback
  through one has an extra entry.  A name the enclosing scope can see but did
  not make a cell -- `__class__` is the one that matters -- is a NameError
  inside the comprehension, so `[super().m() for _ in r]` in a method works
  under CPython and does not here.  And the comprehension's own iteration
  variable does not leak, which is the one part that already matches, because
  our version has a scope to keep it in.

  Inlining means the comprehension's locals becoming the enclosing function's,
  with the shadowed ones saved and restored around it, which is a symbol-table
  change as much as a codegen one.

- **`str.encode` and `bytes.decode` know only utf-8, ascii and latin-1.**
  Any other name is a LookupError, where CPython would find the codec through
  the registry; reaching it from the interpreter would mean calling Python
  from a builtin method.

- **The `_abc` registry and caches hold strong references.**  CPython uses
  weak ones, so a class registered against an ABC can be collected and the
  ABC's caches shrink; here a registered class lives as long as the ABC.
  Registries are process-lifetime and small in practice.  Revisit if
  `_weakref` lands.

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

- **A classmethod on a builtin type reprs as a bound method.**  Ordinary
  methods, slot wrappers and getsets all name themselves and their owner now;
  `int.from_bytes`, `float.fromhex` and `str.maketrans` are wrapped in a
  classmethod object, which `type_stamp_methods` skips, so they answer
  `<bound method from_bytes of <class 'int'>>` where CPython answers
  `<built-in method from_bytes of type object at 0x...>`.

- **A syntax error's wording, its column and the width of its span are our
  own.**  The attributes and `str()` are CPython's now, and the line is
  right, but the message text is this compiler's ("expected ':'" where
  CPython says something longer), the column is the token the parser stopped
  at rather than the one CPython blames, and `end_lineno`/`end_offset` cover
  the single character at that column -- CPython widens the span to a whole
  token or to the subexpression the message is about.  `CompErr` has the two
  fields for a narrower answer; nothing records one yet.

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

- **A module's `__file__` is the `.pyc` it was loaded from, where CPython's is
  the `.py`.**  Visible in the attribute and in `repr(module)`, which prints
  it.  CPython records the source path even when it executes a cached
  bytecode file, and derives the cache path from it when it needs to.

- **A `str` subclass cannot declare `__slots__`.**  CPython accepts it; here
  it is a TypeError, worded as CPython words the ones it does refuse
  (`nonempty __slots__ not supported for subtype of 'str'`).  A str keeps its
  characters inline and a subclass keeps its dict at the tail past them, so a
  slot at a fixed offset lands on the characters -- it wrote over its own data
  and then crashed.  int, bytes and tuple are refused for the same reason and
  CPython refuses those too; str is the one that differs.  Making it work
  means slots at the tail, which is a layout change reaching
  `instance_dealloc`, `instance_traverse` and the member descriptors.

## Missing pieces

These are absences rather than wrong answers — the interpreter raises rather
than lying — but they are ordinary Python that does not work:

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

- **Crafted `.pyc` and `_sre` bytecode are trusted.**  Marshal validates
  offsets but not types, so a `co_consts` slot holding an int is
  dereferenced by `eval_frame`; `frame_new` adds two `.pyc` fields in 32
  bits; `sre_match` bounds the opcode but not its operands.  This is an
  attacker model rather than breakage, and is a separate track.

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
