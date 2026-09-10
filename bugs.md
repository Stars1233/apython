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

- **CPython's test_weakref reports 44 valgrind errors of one kind**: an object
  freed by an explicit `gc.collect()` while a live frame still held it -- the
  collector deciding something is unreachable that is not.  The C-stack
  overflow that used to head this entry is gone: it was `hash()` on a class
  written `__hash__ = ref.__hash__`, where the generic slot wrapper and the
  builtin it found dispatched into each other for ever, and the module no
  longer crashes.

  CPython's test_sys_settrace dies in `gc_visit_decref` under `exc_traverse`
  at shutdown, and it is HEAP-LAYOUT SENSITIVE: the same commit built at
  `/tmp/apy-base` passes and built at `/home/jgarzik/repo/apython` crashes,
  because the DWARF path length changes the binary's size and with it every
  allocation address.  A git worktree is the usual way to compare two commits,
  and a worktree whose path differs in LENGTH is not a control -- build the
  comparison at a path of the same length, or the answer is about the path.

- **Calls made with a misaligned stack, everywhere except the paths into
  GMP.**  The SysV ABI wants `rsp % 16 == 0` at a `call`, and glibc's float
  paths and GMP both use aligned SSE.  Every call into GMP is now made
  aligned, and `tests/gmp_align_probe.sh` is the gate: it breaks on every GMP
  call site in the built binary under gdb and reads rsp at each.  The
  same class is still there outside that reach, and three shapes of it are
  measured rather than guessed:

  `INT_NEED_MPZ` expands to `push rdi` / `call int_promote_mpz` / `pop rdi`,
  so every one of its expansions calls at the wrong parity -- all 1,261 in a
  bignum workload.  It is harmless today only because `int_promote_mpz` saves
  rsp and `and`s it, which is the reason its own GMP call never showed.

  `V_PACK`'s cold path is called AFTER `leave` in every function that packs
  its return value on the way out, so it runs at the caller's parity rather
  than the function's -- eight bytes out.

  And the propagation: `obj_richcompare_bool` is entered misaligned by
  `dict_lookup`, which is entered misaligned by `dict_set` and by a dozen
  module-init callers, and everything under any of them inherits it.

  Neither `lint.py` nor any other source-level check can find these.
  `check_alignment` counts only the pushes before the first non-push
  instruction, exempts `DEF_FUNC_BARE` entirely, and cannot see inside a NASM
  macro -- which hides both pushes and branches.  A source-level detector
  written for exactly this produced twenty-two false positives and was thrown
  away.  What works is a CFG walk over the DISASSEMBLY, tracking rsp's offset
  from entry: objdump sees the macros expanded.  The one thing such a walk
  needs told is entry parity, because an opcode handler is reached by `jmp`
  from the dispatch table and is entered at the opposite parity from a
  function reached by `call` -- assume the wrong one and every handler in the
  tree reports as broken.

- **A set or frozenset SUBCLASS is not treated as a set by `update`.**
  CPython asks `PyAnySet_Check`, which is a subtype test; two places here
  still compare the type pointer against `set_type` and `frozenset_type`
  exactly, and a subclass fails both.  (`set_richcompare` was the third and
  is fixed: it asks TYPE_FLAG_SET_SUBCLASS now.)

  `s.update(sub)` and `{*sub}` fall through to the generic iterator path, so
  a subclass that defines `__iter__` is asked -- CPython ignores it and reads
  the table, which is what makes `{*FS([1,2,3])}` `{1, 2, 3}` there and
  `{99}` here.  `set_contains`'s frozenset-for-a-set-key arm is the second,
  and the milder: `SubSet() in s` raises where CPython answers False.

  The fix is the flag test in both, and what makes it more than a one-liner
  is the site it implies: `set_coerce_operand` already accepts a subclass
  through `REQUIRE_SET_TYPE`, so the method forms and the operator forms
  currently disagree with each other as well as with CPython, and the
  remaining exact-type tests have to move together with a test that fixes the
  whole surface at once.

- **`except*` does not look inside a NESTED group, and a group publishes
  neither `split` nor `subgroup` nor `derive`.**  `except* KeyError` over
  `ExceptionGroup("outer", [ExceptionGroup("inner", [KeyError()]), OSError()])`
  matches the OSError and leaves the outer group unhandled, where CPython
  recurses and matches the KeyError through the nesting.  The split is
  `eg_split`, and it walks one level.

  The three methods are the other half of the same gap: the splitting exists
  only as the thing `except*` calls, so a program cannot do it itself.  And
  where CPython's `split` asks the group to `derive()` a new one -- whose
  default builds a plain `ExceptionGroup` -- `eg_split` constructs one of the
  group's OWN type, so a subclass of `ExceptionGroup` splits into more of
  itself rather than into `ExceptionGroup`.  Publishing the three and routing
  the internal split through `derive` is one change, because the type the
  halves get is decided there.

- **`raise SomeExceptionClass` does not run the class's `__init__`.**  The
  class form of the operand reaches `exc_new`, which builds the object and its
  args tuple directly rather than CALLING the type, so
  `class C(Exception):` with an `__init__` of its own is constructed with none
  of it: `raise C` gives `C()` where CPython gives whatever `C()` gives.  The
  `from` clause instantiates a class cause the same way and inherits the same
  limit.  Fixing it means calling the type -- `exc_type_call` -- where
  `.raise_type` calls `exc_new`, on the path every `raise ValueError` takes,
  so it is a hot path and wants measuring rather than just changing.

- **`super(C, obj)` on a PROXY answers differently depending on what comes
  after it in the file.**  CPython's supercheck asks an object what class it
  says it is when neither its type nor the object itself is a subtype, which
  is what makes super() work through a proxy that forwards attribute access --
  `test_descr.test_proxy_super` is exactly that.  It works on its own; in a
  longer program the same call refuses with "obj must be an instance or
  subtype of type", and DELETING an unrelated statement that comes AFTER it
  makes it work again.

  valgrind is clean over both, so it is not memory corruption: it is
  `obj_declared_class` answering 0, which means the `__class__` lookup did not
  produce the class.  That lookup runs the proxy's own `__getattribute__` --
  Python, from inside an opcode handler, which is the one thing this path does
  that no other form of super() does, and it recurses once more because
  `self.__obj` goes through `__getattribute__` too.  Something about that
  nested eval, and not about the object, decides the answer.

  `tests/test_super_bad_object.py` covers the refusals and leaves the proxy
  out for this reason; the shape that fails is the file that test was cut
  down from, with the proxy call followed by two more statements.

- **`member_descriptor` publishes no `__get__`, `__set__` or `__delete__`.**
  A `__slots__` descriptor works through attribute access, and answers
  `AttributeError: 'member_descriptor' object has no attribute '__get__'` when
  a program reaches for the protocol by name -- which
  `inspect.getattr_static`, the descriptor tests and anything walking
  `type.__dict__` do.  The receiver check they would need is
  `member_check_receiver`, which is already there; what is missing is the
  three entries in the type's dict and the thunks behind them.

- **`scandir()` on a BYTES path yields str entries.**  CPython gives a bytes
  path bytes names and bytes paths back; here the argument goes through
  `posix_path_arg`, which hands over a C string, and the entries are built
  from it as str.  Everything works, and works on the right files -- what
  differs is the type of `.name` and `.path`, which `os.walk(b'.')` and the
  bytes half of `glob` then propagate.  Fixing it means carrying the
  argument's own kind through the getdents64 loop and building bytes objects
  on that side, which is the second half of every string-building step in
  `posix_scandir`.

- **A raise from a C-level slot is a non-local jump, so a C caller cannot
  absorb it.**  `slot_mp_subscript` and its siblings end in `slot_reraise`,
  which tail-jumps into `eval_exception_unwind`; a builtin's own miss --
  `dict_subscript`'s KeyError, say -- goes through `RAISE`, which does the
  same.  Neither returns to its caller, so an opcode that wants to try a
  lookup and recover from the miss cannot go through the slot at all.

  `mapping_getitem_opt` is the way round it for a heaptype (ask
  `__getitem__` through `dunder_call_2`, which does return), and LOAD_NAME
  and SETUP_ANNOTATIONS use it for a locals mapping that is not a dict.  It
  does not help for a builtin `__getitem__`, so a dict SUBCLASS keeps the
  direct table read in LOAD_NAME where CPython's `PyDict_CheckExact` sends it
  through `PyObject_GetItem`: an overridden `__getitem__` on a dict subclass
  used as `exec()` locals is not consulted.  Fixing it properly means the
  builtin subscripts reporting a miss by RETURNING rather than by raising,
  which is every caller of `dict_subscript`.

- **`zip(..., strict=True)` does not say which argument was short.**
  CPython's is "zip() argument 2 is shorter than argument 1" (and
  "...longer..."), with an "argument%s 1-%d" plural once there are more than
  two; this says "zip() has arguments with different lengths" whichever
  happened.  The information is all there at the raise -- `zip_iternext`
  knows the index and which direction it found -- so this is wording rather
  than machinery.

- **A user `__eq__` that reaches itself answers False instead of raising
  RecursionError.**  `class D: def __eq__(s, o): return s.me == o.me` with
  `p.me = p` gives False here and RecursionError in CPython.  The container
  comparisons are guarded (`C_RECURSION_ENTER` in list, tuple and dict) and
  Python-level recursion is guarded by `recursion_depth`, so something on the
  instance-comparison path is deciding the answer before either limit is
  reached rather than recursing; which one has not been traced.

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
