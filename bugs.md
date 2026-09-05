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

- **The collector frees live classes when a metaclass-made class is walked as
  a class.**  A class whose metatype is a metaclass of the user's own is
  traversed by the metatype's `tp_traverse`, and that is `instance_traverse`
  -- the generic heaptype one, which walks the instance's slot region.  It
  should be `type_traverse`, which reports the four references a class
  actually holds: `tp_dict`, `tp_base`, `tp_bases` and `tp_mro`.  Installing
  it makes the collector free classes that are still live, and the next
  collection reads the freed block:

      PYTHONPATH=$CPYTHON_LIB ./apython -  <<'EOF'
      import gc
      gc.set_threshold(1, 1, 1)
      import typing
      EOF

  segfaults in `gc_visit_decref`, and valgrind shows the freed block being
  read by `op_load_fast` -- a class that a frame still had in a local.

  What is measured so far:

  - Every edge `type_traverse` reports is one the class owns.  `mro_compute`
    increfs each MRO entry, `type_from_parts` increfs `tp_base` and the bases
    tuple, and `user_type_dealloc` releases all four.  So it is not a plain
    over-report.
  - The crash goes away if `type_traverse` skips `tp_mro`, or if
    `tuple_traverse` reports nothing, or if the collector is off during
    `type_from_parts`.  No other `tp_traverse` in the tree makes any
    difference.  So it lives in the cycle between a class and its own MRO
    tuple.
  - It is the DEALLOC that does the damage, not the clear: installing
    `type_clear` alone changes nothing, and installing `type_traverse` alone
    reproduces it.
  - Each class the collector takes has an `ob_refcnt` of exactly 2 when it is
    classified, and both referrers are traversed containers, so its gc_refs
    reaches 0 by the collector's own arithmetic.  The classes are named:
    `typing.SupportsInt`, `re.RegexFlag`, `enum.EnumCheck`, `typing._C`.
  - The unreachable set they end up in is tiny and self-contained -- the
    class, its bases tuple and its MRO tuple -- while the collection as a
    whole starts phase 4 with 101 roots against 7293 candidates and rescues
    all but a handful.  So the rescue walk works; these few are never reached
    from a root.

  - The referrer dump says what the search was waiting for.  Traversing every
    tracked object looking for one of these classes finds exactly ONE
    referrer -- its own MRO tuple, itself in the unreachable set -- and none
    at all among the reachable ones or in the generations outside the
    collection.  So by refcount these really are garbage: a class and its MRO
    tuple pointing only at each other, which is what a two-object cycle looks
    like, and collecting it is right.

  Which turns the question around.  The classification is sound, and so is
  `type_traverse`; what is wrong is that something still reaches the class
  after it is freed -- `op_load_fast` out of a frame local in one trace, a
  dict_traverse in another -- through a reference that was never counted.  A
  missing INCREF somewhere on the path that hands a class to a local or a
  dict, invisible for as long as these cycles are never collected, which is
  exactly what the inherited `instance_traverse` guarantees.  Finding it means
  catching the store: break on the class's address being written, or record
  every refcount change to it.

  Until it is found, `src/buildclass.asm` leaves a metatype's traverse and
  clear inherited, which is at least self-consistent; `tp_dealloc` is
  `user_type_dealloc`, which is a separate correctness fix and is unaffected.
  The cost of leaving it is a leak: a class is in a cycle with its own MRO
  tuple, so only the collector can free it, and a collector that does not
  report the edge cannot.  A metaclass-made class that goes out of scope stays
  in memory and in its bases' `__subclasses__()`.
  `tests/test_set_name_metatype.py` covers what IS guaranteed -- the survivors
  are intact and valgrind is quiet -- rather than how many are left.

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
  there.)  `make check-stdlib` gives the current figure: 169 of 196.

  `math`'s `gamma`, `lgamma`, the n-ary `hypot` and `sumprod` round
  differently from CPython's, which uses its own Lanczos approximation and
  double-double arithmetic where these use glibc and a Neumaier sum.  `dist`
  shares `hypot`'s routine and so shares the note.  `fsum` is exact: it is
  Shewchuk's algorithm, as CPython's is.  `tests/test_math.py` says which is
  which.

- **Functions with no docblock at all**, and, among those that have one,
  docblocks with no `->` signature line.  The signature is the only part of a
  function's contract that nothing checks, so its absence is a real gap rather
  than a cosmetic one.  This is the one item here a script cannot finish:
  writing a signature means reading what the function actually returns.  It is
  measured now rather than estimated -- `tests/docblock_floor.txt` holds the
  count per file and `lint.py`'s `check_docblocks` fails when one goes above
  it, so what is left can only shrink.
