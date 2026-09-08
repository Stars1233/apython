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

- **A weakref's callback fires when the REFERENCE dies, not only the
  referent.**  `r = ref(c, cb); del r, c` runs the callback here and does not
  in CPython: dropping the last reference to the `ref` itself should take the
  callback with it, and the side table keeps it reachable.  Found while making
  a `ref` subclass work; not the same bug and not fixed with it.

- **The three things `super`, the method wrappers and `type` still do not
  publish.**  `super.__self__`, `__self_class__` and `__thisclass__` are
  answered by `super_getattr`, which returns a (payload, tag) pair rather than
  a Value, and `super_type` has no `tp_dict` to hold them; the getset
  machinery that publishes memoryview's and property's wants both.
  `__isabstractmethod__` on `property`, `classmethod` and `staticmethod` is
  not answered at all -- CPython computes it from the wrapped callable.  And
  `type.__prepare__`, `type.__instancecheck__` and `type.__subclasscheck__`
  are methods rather than getsets, and want the PLAIN checks: an
  `__instancecheck__` on `type` that went back through `isinstance()` would
  recurse forever through a metaclass that defines one.

- **`set(sequence=())` and `tuple(sequence=())` are not TypeErrors.**  CPython
  refuses a keyword to either.  `list` does refuse it, and carries CPython's
  carve-out for a subclass that overrides `__new__`; the other two never look
  at `kw_names_pending` at all, so `S([1], extra=2)` on a set subclass reads
  the keyword's VALUE as an iterable and reports "'int' object is not
  iterable" where CPython says "set() takes no keyword arguments".

- **`list.__new__(dict)` answers `{}`.**  `container_dunder_new` and
  `scalar_dunder_new` do not check that the class argument is a subtype of the
  type whose `__new__` was reached.  `new_from_slot` does, and words the
  refusal as CPython does; these two predate it.

- **A walrus inside a comprehension in a CLASS body is accepted.**  CPython
  refuses it -- "assignment expression within a comprehension cannot be used
  in a class body" -- because the comprehension's own scope cannot see the
  class namespace it would have to bind into.

- **`case x + 0j:` says "expected ':'" where CPython says "invalid syntax".**
  A name is a capture pattern and the operator is simply what follows it, so
  the value-pattern parser never sees the shape it would reject.  Both refuse
  it; only the wording differs.

- **`set.__and__` reads a freed entry somewhere in CPython's test_set.**
  Valgrind names it: `set_nb_and` -> `set_contains` -> `obj_richcompare_bool`
  reads eight bytes that are not stack'd, malloc'd or recently freed, and the
  process dies in `gc_list_remove` some way later.  An `__eq__` that clears the
  set during the intersection is the obvious shape and is NOT it -- that one
  behaves.  Reached only once the import fixes let the module load.

- **`op_call_function_ex` segfaults somewhere in CPython's test_extcall.**
  Reached only once the import fixes let that module load, and not yet
  reduced: `f(*x)` and `f(**x)` over ints, floats, None and a plain object all
  refuse correctly.  The wording of those refusals is also wrong --
  "list.extend() argument must be iterable" where CPython names the callable
  and the argument position -- which may be the same code.

- **Missing C modules.**  The ranking here is by what actually stands in the
  way rather than by which import fails first -- the two are not the same,
  and `_imp` was reached by twelve modules a few lines after some other
  import that looked like the blocker.  `_imp`, `marshal`, `_warnings`,
  `_typing`, the `_sha*`/`_md5` family and `_posixsubprocess` are there now,
  and `importlib`, `hashlib`, `random` and `subprocess` with them.  So is
  `_signal`, with delivery at the top of a loop the way CPython's is, and
  `doctest`, `pdb`, `unittest` and `signal` with it.  So is `zlib`, as a shim
  over `-lz` on the precedent `-lgmp` set, and `gzip` with it -- and
  `zipfile`, `tarfile` and `shutil`, which imported before and could not
  compress.  So is `array`, which was the largest of these by reach.  What
  is left is genuinely C: `unicodedata`, `_tracemalloc`, `_symtable`, `_ssl`,
  `_sqlite3`, `_crypt`, `_lzma`, `_bz2`, `_ctypes`, `_curses`, `pyexpat` and
  `_tkinter`.
  (`_io` is not among them: `src/modules/io.asm` supplies `_iocore` and
  `lib/_io.py` assembles both halves under the name `_io`.  `_socket` and
  `select` are the same split over `_socketcore`.  Neither are `math`,
  `_collections`, `_struct`, `_random`, `_contextvars`, `_string`,
  `_tokenize`, `_operator`, `binascii`, `atexit` and `_ast`, which are
  there, and so are `_csv` and `termios` -- the second over one raw
  `posix.ioctl`, the same split `_socket` and `select` use.)
  `make check-stdlib` gives the current figure.

  `array` is done, and it is the reason the old "one or two modules apiece"
  reading of this list was wrong: it was what stood between this tree and
  `multiprocessing`, and CPython's own suite imports it from the test modules
  for `struct`, `memoryview`, `io`, `bytes`, `socket`, `re`, `marshal`,
  `codecs` and the compression family.  `fromfile` and `tofile` are the part
  left out -- they want the file object's own read and write, and every caller
  in the suite reaches for `frombytes` and `tobytes` -- and `L` and `Q` hold
  what an int64 holds rather than a uint64, because `obj_as_index` refuses
  anything wider.

  `math`'s `gamma`, `lgamma`, the n-ary `hypot` and `sumprod` round
  differently from CPython's, which uses its own Lanczos approximation and
  double-double arithmetic where these use glibc and a Neumaier sum.  `dist`
  shares `hypot`'s routine and so shares the note.  `fsum` is exact: it is
  Shewchuk's algorithm, as CPython's is.  `tests/test_math.py` says which is
  which.

- **`array.fromfile` and `array.tofile` are absent.**  They want the file
  object's own read and write, and every caller in CPython's suite reaches
  for `frombytes` and `tobytes` instead.

- **One call inside an opcode handler is made with `rsp` misaligned.**
  Recorded in `tests/align_floor.txt`, which `lint.py` ratchets: a new one
  fails the build and the set can only shrink.

  They are not cosmetic, and they are not local.  A misaligned call
  PROPAGATES: the callee's whole frame is 8 out, so every Python frame the
  interpreter runs beneath it is too.  The fault surfaces far away and only
  when something eventually reaches an aligned SSE store -- which is how this
  was found at all: `import gzip; gzip.open(...)` faulted inside libz's
  `inflate`, at a `movaps %xmm0,-0x70(%rbp)`, several thousand instructions
  from anything zlib had done wrong.

  Forty-two of the forty-three are paid.  Most were a loop index or an item
  saved across a call with a lone push; those became frame slots rather than
  pads, because a handler with calls at both push depths has no single frame
  size that satisfies them all -- and the pushed value always had a name.

  The one left is `op_set_update`'s `call set_add`, which lint reports at a
  depth eight above what its own pushes and frame account for.  I could not
  source the difference, and moving the frame by eight only moves which call
  in that handler is wrong.  Changing the code to satisfy a number I do not
  understand is worse than leaving it recorded.

- **Functions with no docblock at all**, and, among those that have one,
  docblocks with no `->` signature line.  The signature is the only part of a
  function's contract that nothing checks, so its absence is a real gap rather
  than a cosmetic one.  This is the one item here a script cannot finish:
  writing a signature means reading what the function actually returns.  It is
  measured now rather than estimated -- `tests/docblock_floor.txt` holds the
  count per file and `lint.py`'s `check_docblocks` fails when one goes above
  it, so what is left can only shrink.
