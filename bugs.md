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

  segfaults in `gc_visit_decref`, having freed `typing._C`, `enum.EnumCheck`,
  `re.RegexFlag` and `typing.SupportsInt` during a collection inside
  `type_apply_set_name`.  Every edge `type_traverse` reports is one the class
  owns -- `mro_compute` increfs each MRO entry, `type_from_parts` increfs
  `tp_base` and the bases tuple, and `user_type_dealloc` releases all four --
  so the fault is an accounting error elsewhere that the extra edges expose.
  Narrowing it: the crash goes away if `type_traverse` skips `tp_mro`, or if
  `tuple_traverse` reports nothing, or if the collector is off during
  `type_from_parts`, and no other `tp_traverse` in the tree makes any
  difference.  So it lives in the type/MRO-tuple cycle.  Until it is found,
  `src/buildclass.asm` leaves a metatype's traverse and clear inherited, which
  is at least self-consistent; `tp_dealloc` is `user_type_dealloc`, which is a
  separate correctness fix and is unaffected.

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
  and `importlib`, `hashlib`, `random` and `subprocess` with them.  What is
  left blocks one or two modules apiece and is genuinely C: `_signal` and
  `termios` (2 each), then `zlib`, `unicodedata`, `_tracemalloc`,
  `_symtable`, `_ssl`, `_sqlite3`, `_crypt`, `pyexpat` and `_tkinter`.
  (`_io` is not among them: `src/iomod.asm` supplies `_iocore` and
  `lib/_io.py` assembles both halves under the name `_io`.  `_socket` and
  `select` are the same split over `_socketcore`.  Neither are `math`,
  `_collections`, `_struct`, `_random`, `_contextvars`, `_string`,
  `_tokenize`, `_operator`, `binascii`, `atexit` and `_ast`, which are
  there.)  `make check-stdlib` gives the current figure: 148 of 196.

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
