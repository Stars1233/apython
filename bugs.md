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

- **`from mod import *` does not check that `__all__`'s names exist.**  CPython
  answers `AttributeError: module 'mod' has no attribute 'missing'`; this tree
  binds what it finds and silently skips the rest
  (`.is_all_loop`'s `jz .is_all_next` in `src/opcodes/match.asm`).  That is how
  `lib/copyreg.py` came to promise `add_extension`, `remove_extension` and
  `clear_extension_cache` in `__all__` while defining none of them, and nothing
  noticed until a reviewer read the file.

  The skip is one branch, but the replacement is not: the intrinsic runs on a
  hand-rolled frame with the eval loop's bytecode IP saved in `rbx`, and
  `raise_exception` tail-jumps into the unwinder.  Raising from there without
  restoring `rbx` first is the same shape as the `systrace_exception` bug --
  a corrupted IP in every frame that raised.

- **`object.__new__` does not refuse a builtin subclass.**  `object.__new__(list)`
  answers `[]` where CPython raises
  `TypeError: object.__new__(list) is not safe, use list.__new__()`, and `dict`
  and `int` are the same.  CPython's rule is that `object.__new__` refuses any
  type whose `tp_new` is not `object`'s own, unless `__init__` is overridden
  and `__new__` is not.  `copyreg._reconstructor` is the ordinary caller.

- **An exception raised inside `__exit__`, while another is in flight,
  segfaults.**  Four lines, no stdlib:

  ```python
  class CM:
      def __enter__(self): return self
      def __exit__(self, et, ev, tb): raise ValueError("from exit")
  with CM():
      raise TypeError("inner")
  ```

  CPython raises the ValueError with the TypeError as its `__context__`.  This
  tree dies with SIGSEGV in `obj_is_true` (`src/object.asm:2921`), reached from
  `op_pop_jump_if_true` (`src/opcodes/flow.asm:187`) -- a garbage Value on the
  stack, so the unwinder and the with-block's cleanup disagree about how far
  to pop.  Any exception does it; `ev.nosuch` inside `__exit__` is the same
  crash.

  This is what actually stands between apython and CPython's own test suite.
  `unittest`'s `assertRaises` calls `traceback.clear_frames` in its `__exit__`,
  and `frame.clear()` does not exist here, so the AttributeError that follows
  lands in exactly this shape -- and `assertRaises` is in nearly every test
  CPython ships.  `frame.clear()` is worth adding on its own, but it is the
  smaller half.

- **`\b` and `\B` are ASCII-only.**  `re.search(r"\b\d+\b", "eee42")` with
  non-ASCII letters in place of the e's finds `42`, where CPython finds nothing
  because those letters are word characters.  `\B\d` is wrong the same way and
  `re.search(r"\bX\b", "x X y")` with a non-ASCII X finds nothing.  `\w`
  itself is already right -- `sre_uni_isword` (`src/sre.asm`) exists and
  answers correctly -- so this is the `AT` handlers not using it.

- **A non-ASCII subject is re-decoded on every `pattern.match(s, pos)`.**  The
  engine indexes code points, so a non-ASCII subject is decoded to u32 before
  matching, and that is O(len).  A scanner caches it (`SRE_CpCache`, so
  `finditer` decodes once), but a hand-written `pattern.match(s, pos)` loop has
  no scanner and pays per call -- and `json.decoder` is written exactly that
  way.  Caching it would have to hang off the string itself, since nothing else
  in that loop outlives one call; that is a `PyStrObject` change, which is why
  the scanner got one and this did not.

- **`type_install_slots` never clears a slot it has filled.**  `del C.__iter__`
  leaves `tp_iter` pointing at the wrapper, which then finds no dunder and
  answers `RuntimeError: slot wrapper failed without an exception` where CPython
  says `'A' object is not iterable`.  `del C.__len__` is the same, and so is
  `del C.__call__`, which additionally leaves `callable()` answering True.  The
  fix is not simply "clear on `.skip`": that path is also taken when a *builtin*
  base supplies the dunder, and there the slot must be left exactly as
  `type_from_parts` set it.

- **A dunder set to `None` empties the slot, which is wrong for `__call__`.**
  `type_install_slots` skips any dunder explicitly `None`, so the protocol is
  disabled -- right for `__iter__` and `__hash__`, and what Python documents.
  CPython does not extend it to `__call__`: `class C: __call__ = None` leaves
  `callable(C())` True and fails inside the call with `'NoneType' object is not
  callable`, where this tree answers `callable()` False and `'C' object is not
  callable`.  Ours is the more coherent pair; it is still a difference.

- **No builtin type carries `__call__` in its `tp_dict`.**
  `hasattr(len, '__call__')` is False, and so is `hasattr(int, '__call__')` and
  `hasattr(staticmethod(len), '__call__')`, though all three are callable and
  answer `callable()` True.  This is the recorded "a builtin's behaviour that
  lives only in a slot" pattern: the stdlib asks by name, and `__call__` is one
  of the names it asks about.  Instances of classes written in Python are fine
  -- their `__call__` is a real dict entry.

- **A scanner keeps scanning after a failed `match()`.**  CPython ends the scan
  there: `p.scanner(s).match()` returning None makes every later `match()` and
  `search()` on that scanner return None too.  Here the scanner carries on, so
  `sc.match(); sc.search()` finds the first match where CPython finds nothing.
  Independent of the subject's encoding -- it reproduces on pure ASCII.

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

- **A module's leading RESUME and a body's implicit `return None` carry a
  different location from CPython's.**  Visible now that `co_positions()` and
  `co_lines()` report what the table holds: for `compile("x = 1\ny = 2\n")`
  CPython's first entry is `(0, 1, 0, 0)` and its last run is line 2, where
  this compiler gives `(0, 0, None, None)` and a trailing run with no location
  at all.  Only code this compiler produced is affected -- a CPython `.pyc`
  decodes exactly, which is what `tests/test_code_positions.py` pins.  Same
  neighbourhood as the entry below.

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
