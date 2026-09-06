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
  `doctest`, `pdb`, `unittest` and `signal` with it.  So is `zlib`, as a shim
  over `-lz` on the precedent `-lgmp` set, and `gzip` with it -- and
  `zipfile`, `tarfile` and `shutil`, which imported before and could not
  compress.  What is left blocks one or two modules apiece and is genuinely
  C: `unicodedata`, `_tracemalloc`, `_symtable`, `_ssl`, `_sqlite3`,
  `_crypt`, `_lzma`, `_bz2`, `_ctypes`, `_curses`, `pyexpat` and
  `_tkinter`.
  (`_io` is not among them: `src/modules/io.asm` supplies `_iocore` and
  `lib/_io.py` assembles both halves under the name `_io`.  `_socket` and
  `select` are the same split over `_socketcore`.  Neither are `math`,
  `_collections`, `_struct`, `_random`, `_contextvars`, `_string`,
  `_tokenize`, `_operator`, `binascii`, `atexit` and `_ast`, which are
  there, and so are `_csv` and `termios` -- the second over one raw
  `posix.ioctl`, the same split `_socket` and `select` use.)
  `make check-stdlib` gives the current figure: 178 of 196.

  `math`'s `gamma`, `lgamma`, the n-ary `hypot` and `sumprod` round
  differently from CPython's, which uses its own Lanczos approximation and
  double-double arithmetic where these use glibc and a Neumaier sum.  `dist`
  shares `hypot`'s routine and so shares the note.  `fsum` is exact: it is
  Shewchuk's algorithm, as CPython's is.  `tests/test_math.py` says which is
  which.

- **`split()`, `strip()` and friends do not see the non-ASCII whitespace.**
  CPython splits on U+0085, U+00A0, U+2028, U+2029, U+3000 and the U+2000
  block as readily as on a space: `"a\xa0b".split()` is `['a', 'b']` there and
  `['a\xa0b']` here, and `"\xa0mid\xa0".strip()` is `'mid'` there and
  unchanged here.  The four ASCII separators `\x1c`-`\x1f` were missing for
  the same reason until 2026-09-06 and are now in `str_ws_class`.

  A byte table cannot close the rest: every one of those characters is two or
  three bytes in UTF-8, so the scan loops in `str_split_impl` and
  `str_strip_impl` would have to decode code points rather than walk bytes.
  The shape that fits is the one `str_case_map` already uses -- an ASCII fast
  path over bytes, chosen by `ob_size == ob_length`, and a decoding loop
  behind it.  `splitlines` has its own, different set (it takes `\x1c` and
  U+2028 but not `\x1f` or U+00A0) and the same gap.

- **A read-only property's AttributeError has CPython 3.10's wording.**
  `Plain().r = 2` says `can't set attribute` where CPython 3.12 says
  `property 'r' of 'Plain' object has no setter`, and the deleter case is the
  same shape.  Found while fixing the data-descriptor flag; unrelated to it,
  and it happens on a plain class too.

  CPython's message needs the property's own name, which it learns through
  `__set_name__` and keeps in a `prop_name` field.  `PyPropertyObject` has no
  such field and property has no `__set_name__`, so this is not a wording
  change -- it is that plus the object's type name at the raise site.
  `lib/types.py` raises the old wording by hand in one place too.

- **A LOAD_ATTR site whose key was built at run time specializes and deopts
  forever.**  `op_load_attr_instance`'s guard 5 compares the instance dict's
  stored key against `co_names[i]` by POINTER, and `dict_set` keeps the FIRST
  writer's key object.  When the attribute was created with a name that is not
  the interned constant -- `setattr(o, "".join([...]), 1)` -- the guard can
  never pass, so every execution writes the generic opcode back, re-runs the
  full slow path, and re-specializes: two instruction-stream writes and a
  `dict_get_index` per access.  Measured at 41ms against 18ms for the same
  loop over an attribute set from a constant.  (Still ahead of CPython's 46ms,
  which is why this is a performance note and not a correctness one.)

  The fix is small -- refuse once at the specialization site when the stored
  key is not the same object, and record the refusal in a spare CACHE byte so
  the attempt is not repeated -- but it does not fit: `src/opcodes/load.asm`
  is 107 bytes under lint's 100k cap for a hand-written file, so ANY addition
  to it fails the build.  The seam is the one `arith.asm` / `arith_spec.asm`
  already uses: move the two inline-cache handlers, opcodes 203 and 204, into
  a `load_ic.asm` of their own.  They deopt by rewriting an opcode byte and
  re-dispatching, so they call nothing file-local.

- **The compiler attributes a loop's back edge to the loop header, and
  CPython attributes it to the body.**  For

      for i in range(n):
          total += i

  CPython gives `JUMP_BACKWARD` the line of `total += i` and `END_FOR` the
  line of the `for`; this gives both the `for`.  Nothing could see it until
  `sys.settrace` arrived -- now a traced loop reports one extra `'line'` event
  per iteration, naming the `for` line twice.  `tests/test_settrace.py` is the
  one file in `tests/` that `make check-source` cannot match, and this is why;
  it matches exactly from a CPython `.pyc`, so the tracing rule itself is
  right.  A `try` block's line attribution differs in the same test for what
  looks like the same reason.

  The fix wants the line of the last instruction actually EMITTED, and
  `CompUnit` tracks only `curline`, the line the emitters are currently
  positioned at -- which `cg_stmt` has already restored to the `for`
  statement's by the time the back edge is emitted.  A `.lastline` field
  written by `cg_emit` is the shape; `.pad5` is there to take it.

- **`int / int` double-rounds when either operand is wider than a double.**
  `(10**30) / 7` answers `1.4285714285714283e+29` where CPython answers
  `1.4285714285714285e+29`, and `1 / 10**30` is out by an ulp the same way.
  `int_true_divide` converts each operand to a double and divides, which
  rounds twice; CPython's `long_true_divide` computes the quotient of the two
  exact integers to 54 bits and rounds once.

  Operands inside +-2^50 are unaffected and take a specialized opcode: each
  converts exactly, so the single division rounding is the only one.  The fix
  is a GMP `mpz_tdiv_qr` at a scale chosen from the two bit lengths, then one
  round-half-even using the remainder as the sticky bit -- along with the
  `OverflowError` CPython raises when the quotient is too large for a double,
  which this does not raise either.

- **Functions with no docblock at all**, and, among those that have one,
  docblocks with no `->` signature line.  The signature is the only part of a
  function's contract that nothing checks, so its absence is a real gap rather
  than a cosmetic one.  This is the one item here a script cannot finish:
  writing a signature means reading what the function actually returns.  It is
  measured now rather than estimated -- `tests/docblock_floor.txt` holds the
  count per file and `lint.py`'s `check_docblocks` fails when one goes above
  it, so what is left can only shrink.
