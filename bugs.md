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

- **CPython's test_weakref overflows the C stack.**  `WeakMethodTestCase.
  test_hashing` and two of its sibling classes die with an unbounded
  recursion whose top frame is `dict_lookup`; valgrind reports "can't grow
  stack" rather than an invalid access.  The test in isolation passes, and so
  does every reduction of it tried so far -- it needs the rest of the module,
  so the state that arms it comes from an earlier test.  The Python-level
  recursion limit is in place and works (`sys.getrecursionlimit()` is 1000
  and a runaway Python function raises RecursionError), so whatever recurses
  here is doing it below the eval loop, where nothing counts the depth.

  The same file also reports 44 valgrind errors of a second kind, all of them
  an object freed by an explicit `gc.collect()` while a live frame still held
  it -- the collector deciding something is unreachable that is not.  Both
  predate the round that recorded them.

  CPython's test_sys_settrace dies of the same thing, in `gc_visit_decref`
  under `exc_traverse` at shutdown, and it is HEAP-LAYOUT SENSITIVE: the same
  commit built at `/tmp/apy-base` passes and built at
  `/home/jgarzik/repo/apython` crashes, because the DWARF path length changes
  the binary's size and with it every allocation address.  A git worktree is
  the usual way to compare two commits, and a worktree whose path differs in
  LENGTH is not a control -- build the comparison at a path of the same
  length, or the answer is about the path.

- **`f(*5)` does not name the callable.**  CPython says
  "__main__.f() argument after * must be an iterable, not int"; this says
  "Value after * must be an iterable, not int", which is CPython's message
  for the OTHER shape -- `f(*a, *b)` and `[*5]`.  The two differ because
  CPython compiles a lone `*x` to a bare CALL_FUNCTION_EX and this compiles
  it to BUILD_LIST + LIST_EXTEND, so the refusal comes from a different
  opcode.  Matching it means matching the codegen, and then teaching
  CALL_FUNCTION_EX to materialise an arbitrary iterable -- it takes a tuple
  or a list today.  The `**` half is done: DICT_MERGE names the callable and
  accepts any mapping.

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

- **Indexing a non-ASCII string is O(n), so a loop over one is quadratic.**
  `str_cp_offset` and `str_byte_to_cp` walk from byte 0 every time, because
  nothing remembers where the last code point was.  `s[i]` in a loop, a slice
  of a wide string and `str.find`'s conversion of its answer back to a code
  point index all pay it; CPython's strings are fixed-width per object and pay
  nothing.  `tests/run_str_bench.sh` runs its wide indexing and slicing cases
  at 50-100x fewer iterations than their ASCII partners for this reason alone,
  which is why those rows cannot be compared with the rest of the suite.

  The walk itself is as cheap as it can be made -- the per-code-point call was
  inlined and cost 20% of the instructions of a wide indexing loop -- but the
  shape is what is wrong.  What closes it is a cursor on the string object:
  one word holding the last (code point index, byte offset) pair, which makes
  forward sequential indexing O(1) amortised.  A zeroed cursor is valid for
  every string, so it needs no invalidation and strings are immutable in any
  case.  It is not done here because it moves `PyStrObject.data` and every one
  of the twenty-odd places that build a string by hand has to initialise the
  new field -- and a missed one is a wrong CHARACTER out of a wide string, in
  a path the suite barely exercises, rather than a crash.

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
