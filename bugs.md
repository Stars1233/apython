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

- **Missing C modules.**  The ranking here is by what actually stands in the
  way rather than by which import fails first -- the two are not the same,
  and `_imp` was reached by twelve modules a few lines after some other
  import that looked like the blocker.  `_imp`, `marshal`, `_warnings`,
  `_typing`, the `_sha*`/`_md5` family and `_posixsubprocess` are there now,
  and `importlib`, `hashlib`, `random` and `subprocess` with them.  So is
  `_signal`, with delivery at the top of a loop the way CPython's is, and
  `doctest`, `pdb`, `unittest` and `signal` with it.  What is left blocks one or two modules
  apiece and is genuinely C: `termios` (2), then `zlib`, `unicodedata`,
  `_tracemalloc`, `_symtable`, `_ssl`, `_sqlite3`, `_crypt`, `_lzma`, `_bz2`,
  `_ctypes`, `_curses`, `pyexpat` and `_tkinter`.
  (`_io` is not among them: `src/iomod.asm` supplies `_iocore` and
  `lib/_io.py` assembles both halves under the name `_io`.  `_socket` and
  `select` are the same split over `_socketcore`.  Neither are `math`,
  `_collections`, `_struct`, `_random`, `_contextvars`, `_string`,
  `_tokenize`, `_operator`, `binascii`, `atexit` and `_ast`, which are
  there.)  `make check-stdlib` gives the current figure: 170 of 196.

  `math`'s `gamma`, `lgamma`, the n-ary `hypot` and `sumprod` round
  differently from CPython's, which uses its own Lanczos approximation and
  double-double arithmetic where these use glibc and a Neumaier sum.  `dist`
  shares `hypot`'s routine and so shares the note.  `fsum` is exact: it is
  Shewchuk's algorithm, as CPython's is.  `tests/test_math.py` says which is
  which.

- **`signal.setitimer` and `signal.getitimer` are not there.**  `alarm` is,
  and `sleep` sits out an interrupted syscall the way PEP 475 asks, so what
  is missing is the interval timers themselves -- three syscalls and the
  `struct itimerval` they take.  A test that wants a sub-second alarm reaches
  for these; `signal.alarm` only takes whole seconds.

- **PEP 646's starred annotation is refused.**  `def g[T, *Ts](a: T, *rest:
  *Ts)` is "can't use starred expression here": the parser takes `*Ts` in the
  type-parameter list, which is where the shape was added, and not in an
  annotation, where it also belongs.  The rest of PEP 695 works, decorated
  and undecorated alike.

- **The tokenizer cannot warn.**  CPython emits a SyntaxWarning for a number
  that ends against a keyword -- `1if True else 2` compiles, and says so.
  This compiles it silently: the compiler runs before there is an interpreter
  frame to warn from, which is the same reason it may not raise.  What is
  missing is the deferred channel the error protocol already has, applied to
  warnings.

- **`range` clamps a bound wider than an index.**  Its three bounds are int64
  fields where CPython holds objects, so `range(1 << 1000)` is representable
  there and not here -- `_collections_abc` builds one at import to name the
  type its iterator has, and refusing it takes the standard library with it,
  so the bound is clamped.  `len(range(2**70))` therefore answers 2**63 - 1
  where CPython raises, and `type(iter(range(1 << 1000)))` is
  `range_iterator` where CPython has a second type for the wide case.

- **Two index refusals carry the wrong wording.**  A subscript too wide for
  an index is an OverflowError here and an IndexError in CPython, and
  `chr(2**70)` names a C ssize_t where CPython names a C int.  Both used to
  be silent wrong ANSWERS -- obj_as_index truncated -- so this is what is
  left of that; obj_as_index is one funnel and each caller's exception is its
  own.

- **Functions with no docblock at all**, and, among those that have one,
  docblocks with no `->` signature line.  The signature is the only part of a
  function's contract that nothing checks, so its absence is a real gap rather
  than a cosmetic one.  This is the one item here a script cannot finish:
  writing a signature means reading what the function actually returns.  It is
  measured now rather than estimated -- `tests/docblock_floor.txt` holds the
  count per file and `lint.py`'s `check_docblocks` fails when one goes above
  it, so what is left can only shrink.
