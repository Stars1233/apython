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

- **Comprehensions are not inlined, so PEP 709's effects are missing.**
  CPython 3.12 runs a list, dict or set comprehension in the *enclosing*
  frame; here each still gets a code object and a frame of its own.  Three
  things follow.  `sys._getframe().f_code.co_name` inside one answers
  `<listcomp>` rather than the enclosing function's name, and a traceback
  through one has an extra entry.  A name the enclosing scope can see but did
  not make a cell -- `__class__` is the one that matters -- is a NameError
  inside the comprehension, so `[super().m() for _ in r]` in a method works
  under CPython and does not here.  And the comprehension's own iteration
  variable does not leak, which is the one part that already matches, because
  our version has a scope to keep it in.

  Inlining means the comprehension's locals becoming the enclosing function's,
  with the shadowed ones saved and restored around it, which is a symbol-table
  change as much as a codegen one.

- **No `encodings` package, so the registry finds only what this tree ships.**
  `str.encode` and `bytes.decode` reach the registry now, and the registry has
  what `lib/_codecs.py` can express without a table: utf-8 and its BOM'd form,
  ascii, latin-1, the six UTF-16 and UTF-32 forms, the two escape codecs, and
  their aliases.  CPython's `encodings/` is two hundred modules, most of them
  a 256-entry mapping, and this ships none of them -- so `cp1252`,
  `iso-8859-15`, `koi8-r` and the rest are a LookupError unless a CPython
  `Lib/` is on the path, in which case `encodings.search_function` finds them
  and they work.  `namereplace` is the one error handler that only
  approximates: it needs a code point's Unicode NAME, and the table that
  resolves `\N{...}` at compile time indexes the other way.

- **Missing C modules.**  The ranking here used to be by how often each was
  the FIRST import to fail, which is not the same as how many modules each
  blocks: twelve of the thirteen that stopped at `_ast` import
  `importlib.machinery` a few lines later.  Measured by what actually stands
  in the way, over CPython 3.12's 196: `_imp` (with `marshal` and
  `_warnings`) 27, `_hashlib` and the `_sha*`/`_md5` family, `array`,
  `_typing`, `_posixsubprocess`, `_signal`, `_csv`, `pyexpat`, then a long
  tail of one apiece.
  (`_io` is not among them: `src/iomod.asm` supplies `_iocore` and
  `lib/_io.py` assembles both halves under the name `_io`.  `_socket` and
  `select` are the same split over `_socketcore`.  Neither are `math`,
  `_collections`, `_struct`, `_random`, `_contextvars`, `_string`,
  `_tokenize`, `_operator`, `binascii`, `atexit` and `_ast`, which are
  there.)  `make check-stdlib` gives the current figure: 129 of 196.

  `hashlib` imports but has no digests, because every one of them is a C
  module here as well.

  `math`'s `gamma`, `lgamma`, the n-ary `hypot` and `sumprod` round
  differently from CPython's, which uses its own Lanczos approximation and
  double-double arithmetic where these use glibc and a Neumaier sum.  `dist`
  shares `hypot`'s routine and so shares the note.  `fsum` is exact: it is
  Shewchuk's algorithm, as CPython's is.  `tests/test_math.py` says which is
  which.

- **A syntax error's wording and span still differ in a measured tail.**
  `tests/syntax_probe.sh` compiles a corpus of malformed snippets under both
  interpreters and compares all five fields -- msg, lineno, offset,
  end_lineno, end_offset.  Most match; `bash tests/syntax_probe.sh --show`
  prints the rest, and `tests/syntax_floor.txt` keeps them from growing.

  What is left is four shapes.  A bracket that is never closed is reported
  at the token the parser gave up on rather than at the opening bracket, and
  the wording is used where CPython says "invalid syntax" (`class C(object:`)
  and not used where it does (`x = a[`).  Three errors are recorded with no
  position at all and come out on line 0: a starred expression where one
  cannot go, an operator with no operand, and `nonlocal` at module level.
  CPython has several messages that suggest what was meant -- "Perhaps you
  forgot a comma?", "Missing parentheses in call to 'print'" -- which are
  its parser's second pass over a failed rule and have no counterpart here.
  And an escape or a `\N{...}` that will not decode is reported by the
  compiler, where CPython reports it as the codec's own error wrapped in a
  SyntaxError.

- **`ast.parse` is missing two of its arguments.**  `type_comments=True`
  collects nothing, because the tokenizer discards comments and has nowhere
  to put a `# type:` one; and `mode="func_type"` is a ValueError, because
  there is no `(int, str) -> bool` start symbol.

## Style debt

Everything here assembles and runs.  These are places the tree does not follow
STYLE.md, listed so the gap is a known quantity rather than a surprise to
whoever copies a neighbouring file.  Counts are deliberately absent -- grep
gives a current one, and a number written here is wrong by the next commit.

- **Functions with no docblock at all**, and, among those that have one,
  docblocks with no `->` signature line.  The signature is the only part of a
  function's contract that nothing checks, so its absence is a real gap rather
  than a cosmetic one.  This is the one item here a script cannot finish:
  writing a signature means reading what the function actually returns.  It is
  measured now rather than estimated -- `tests/docblock_floor.txt` holds the
  count per file and `lint.py`'s `check_docblocks` fails when one goes above
  it, so what is left can only shrink.
