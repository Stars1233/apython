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

- **Forty-two calls inside opcode handlers are made with `rsp` misaligned.**
  Recorded one per site in `tests/align_floor.txt`, which `lint.py` ratchets:
  a new one fails the build and the set can only shrink.  Pay one down by
  padding the odd push and re-recording with
  `python3 src/compiler/lint.py --record-alignment`.

  They are not cosmetic, and they are not local.  A misaligned call
  PROPAGATES: the callee's whole frame is 8 out, so every Python frame the
  interpreter runs beneath it is too.  The fault surfaces far away and only
  when something eventually reaches an aligned SSE store -- which is how this
  was found at all: `import gzip; gzip.open(...)` faulted inside libz's
  `inflate`, at a `movaps %xmm0,-0x70(%rbp)`, several thousand instructions
  from anything zlib had done wrong.

  `lint.py` had a check for this and it saw none of them, for two reasons
  both now fixed: it stopped tracking depth at the first label, so a call six
  instructions past the push that unbalanced it went unexamined, and it read
  only a literal `sub rsp, 40` and ignored `sub rsp, SOME_CONST - 16`.
  Teaching it to resolve a label from the depths control reaches it at, and
  to evaluate the arithmetic, turned up all forty-five.

  Three are already fixed and are the ones that mattered: `op_import_name`
  made `call import_module` at a fifth push (the flag now lives in r15, which
  the register convention leaves free); `op_call_function_ex` carved
  `CFX_FRAME2 - 16`, an even number where a jumped-to handler needs an odd
  one; and `op_get_iter`'s hand-rolled frame for `seq_iter_new` was `push rbp`
  and nothing else.  Between them they accounted for every misalignment on
  the `import` path -- `import io` went from thirty-eight misaligned frames
  to none.

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

- **Five families of format-spec difference, found by fuzzing the whole
  grammar.**  Four thousand randomly assembled specs -- fill, align, sign,
  `#`, `0`, width, separator, precision, type -- over seventeen values, diffed
  against `python3`.  Grouping is not among them; these are what was left
  once it was fixed:

  - **An explicit fill and align, then a `0`.**  `format(-7, "*^-05d")` is
    `'*-7**'` in CPython and `'0-700'` here: the `0` flag overwrites the fill
    character that was already given, where CPython leaves an explicit fill
    alone and lets `0` supply one only when none was written.
  - **A precision on an integer presentation is not refused.**
    `format(255, "#020.7")` answers a number here and is
    `ValueError: Precision not allowed in integer format specifier` there.
  - **`n` accepts a separator.**  `format(10**25, "0>#0,n")` groups here;
    CPython says `Cannot specify ',' with 'n'.` because `n` takes its
    separator from the locale.
  - **`Invalid format specifier` names neither the spec nor the type.**
    CPython's is `Invalid format specifier '*#012' for object of type 'int'`.
  - **`#` with an empty float type.**  `format(0.0, "#12.0")` is `' 0.e+00'`
    in CPython and `' 0.0'` here: an empty type with a precision behaves as
    `e`, and `#` keeps the point.

  `tests/test_format_grouping.py` covers the grouping; none of these has a
  test yet, and the corpus that found them is worth keeping -- a
  `tests/formatfuzz_probe.sh` beside the type one is the shape.

- **Functions with no docblock at all**, and, among those that have one,
  docblocks with no `->` signature line.  The signature is the only part of a
  function's contract that nothing checks, so its absence is a real gap rather
  than a cosmetic one.  This is the one item here a script cannot finish:
  writing a signature means reading what the function actually returns.  It is
  measured now rather than estimated -- `tests/docblock_floor.txt` holds the
  count per file and `lint.py`'s `check_docblocks` fails when one goes above
  it, so what is left can only shrink.
