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
  `doctest`, `pdb`, `unittest` and `signal` with it.  What is left blocks one
  or two modules apiece and is genuinely C: `zlib`, `unicodedata`,
  `_tracemalloc`, `_symtable`, `_ssl`, `_sqlite3`, `_crypt`, `_lzma`, `_bz2`,
  `_ctypes`, `_curses`, `pyexpat` and `_tkinter`.
  (`_io` is not among them: `src/iomod.asm` supplies `_iocore` and
  `lib/_io.py` assembles both halves under the name `_io`.  `_socket` and
  `select` are the same split over `_socketcore`.  Neither are `math`,
  `_collections`, `_struct`, `_random`, `_contextvars`, `_string`,
  `_tokenize`, `_operator`, `binascii`, `atexit` and `_ast`, which are
  there, and so are `_csv` and `termios` -- the second over one raw
  `posix.ioctl`, the same split `_socket` and `select` use.)
  `make check-stdlib` gives the current figure: 173 of 196.

  `math`'s `gamma`, `lgamma`, the n-ary `hypot` and `sumprod` round
  differently from CPython's, which uses its own Lanczos approximation and
  double-double arithmetic where these use glibc and a Neumaier sum.  `dist`
  shares `hypot`'s routine and so shares the note.  `fsum` is exact: it is
  Shewchuk's algorithm, as CPython's is.  `tests/test_math.py` says which is
  which.

- **A reflected dunder on a subclass of a builtin loses to the builtin's own
  slot.**  `1 + m`, for a `class MyFloat(float)` defining only `__radd__`,
  answers `3.0` where CPython answers `MyFloat.__radd__`; `2 * MyFloatM(3.0)`
  is the same shape.  int's `nb_add` declines the float subclass, and the
  right operand's slot is tried next -- but MyFloat INHERITS `float_add`, so
  that succeeds and the user's `__radd__` is never consulted.

  `src/slots.asm` installs a wrapper for `__add__` and not for `__radd__` on
  its own, on the stated ground that `op_binary_op`'s reflected-dunder arm
  serves that direction.  It does, for a class with no inherited numeric slot
  at all -- `1 + Plain()` is right -- and cannot for a builtin's subclass,
  where the inherited slot answers first.  CPython's `slot_nb_add` exists on
  such a type precisely because `__radd__` was defined, and notices that
  `self` is the right operand.

  The rule already exists here in `binop_subclass_first`, which is why
  `1 + MyInt(2)` IS right: MyInt is a subclass of int, so the reflected arm
  runs before either slot.  MyFloat is not a subclass of int, so nothing
  reaches it.

- **`obj_binary_op` does not implement the subclass-first rule at all.**
  `sum([1, 2, MyInt(3)])` for an int subclass defining `__radd__` answers `6`
  where CPython answers `MyInt.__radd__`, and `1 + MyInt(3)` answers correctly
  -- the two go through different functions.  `op_binary_op` calls
  `binop_subclass_first` before either slot; `obj_binary_op`, which every
  builtin that adds two objects uses, goes straight to the left type's slot.

  `binop_subclass_first` is file-local to `src/opcodes/arith.asm` and takes
  `(payload, tag)` pairs rather than Values, and it needs the reflected
  dunder's name, which `obj_binary_op` has no table for.  Sharing it means
  exporting it, converting at the boundary, and giving `binary_op_offsets` a
  parallel column of reflected names.

- **`obj_richcompare_bool` does not implement the subclass-first rule either,
  and it is the one every container asks.**  For a str subclass whose `__eq__`
  answers False, `SK("hello") in ["hello"]` is True here and False in CPython;
  so are `in` on a dict and `list.count`.  The *expression* `"hello" == SK(...)`
  is right, because `COMPARE_OP` gives the subclass its reflected call first --
  the two answers come from different functions, exactly as with
  `op_binary_op` and `obj_binary_op` above.

  This is the same missing rule as the entry above but on a different axis, so
  fixing one does not fix the other: `obj_richcompare_bool` needs to test
  `type(right)` for being a proper subclass of `type(left)` that overrides the
  comparison, and run the reflected slot first when it is.  `tests/test_dict_str_keys.py`
  has the case written out and says why it is not asserted there.

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
