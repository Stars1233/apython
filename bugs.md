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

- **A class loses a reference when `__init_subclass__` drops an unfinished
  generator.**  Reduced from a collector crash, and independent of the
  collector: it happens with `gc.disable()`.

      class Base:
          def __init_subclass__(cls, **kw):
              g = (_ for _ in range(3))
              del g                      # never iterated

      class D(Base):
          pass                           # D's refcount is now one too low

  Measured at the call site in `type_from_parts`: the class has an
  `ob_refcnt` of 2 immediately before `__init_subclass__` is invoked and 1
  immediately after.  Every other class comes back with what it went in with.

  The conditions are exact, and each was measured:

  - The generator must NOT run to exhaustion.  `list(genexpr)`, `sum(...)`,
    `max(...)` are all fine; `any(...)` and `all(...)` that short-circuit,
    `next(g)` and then abandoning it, `g.close()`, and a generator that is
    created and never touched all lose one.  A list comprehension in the same
    place is fine.  So it is the GeneratorExit path -- `gen_dealloc` ->
    `gen_dealloc_close` -> `gen_throw` -> a nested `eval_frame`.
  - Keeping the generator alive past the call (stashing it in a global) loses
    nothing, so the loss is at the generator's destruction.
  - It does not need to be the same frame: a helper called from
    `__init_subclass__` that drops the generator loses the reference just the
    same, and `del cls` first does not prevent it.
  - The same function called ordinarily from Python -- as a `sorted(key=...)`
    callback, through `map`, or plainly -- loses nothing.  So the path that
    matters is `bc_call_kw` -> `obj_call_n` -> `func_call`, which is how
    `type_from_parts` invokes the hook, with args[0] a borrowed slot pushed
    by SPUSH_PTR.

  It stays invisible because a class is in a cycle with its own MRO tuple, so
  a refcount one short still never reaches zero -- and the collector, which
  would notice, cannot break that cycle: a metaclass-made class inherits
  `instance_traverse` rather than `type_traverse`.  Installing the right
  traverse in `src/buildclass.asm` is what makes it fatal.  With it,

      PYTHONPATH=$CPYTHON_LIB ./apython -  <<'EOF'
      import gc
      gc.set_threshold(1, 1, 1)
      import typing
      EOF

  segfaults in `gc_visit_decref`, and valgrind shows the freed class being
  read by `op_load_fast`.  Everything the collector does there is correct: a
  referrer dump finds exactly one referrer for each class it takes -- the
  class's own MRO tuple -- so by refcount they really are garbage.  They are
  garbage only because of the missing reference.

  So the order is: fix the reference, then install the traverse and the
  matching clear beside the `tp_dealloc` that is already there.  Until then a
  metaclass-made class leaks: it stays in memory and in its bases'
  `__subclasses__()`.  `tests/test_set_name_metatype.py` covers what IS
  guaranteed -- the survivors are intact and valgrind is quiet -- rather than
  how many are left.

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
