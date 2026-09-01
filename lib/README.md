# `lib/` — the modules apython ships

apython reads `.pyc` and never `.py`, so these are byte-compiled as part of
`make` and found relative to the interpreter binary.  They sit at the *end* of
`sys.path`, after `PYTHONPATH`, because they stand in for CPython's C modules
rather than for its Python ones: a real stdlib named by `PYTHONPATH` wins.

## Provenance

Most of this tree comes from CPython and is therefore covered by the Python
Software Foundation License, a copy of which is in `LICENSE.python`.  That is
separate from, and in addition to, the MIT license covering the rest of this
repository (`../LICENSE`).

| Origin | Files |
|---|---|
| CPython, unmodified | `abc.py` |
| CPython, modified for apython | `__future__.py`, `collections/`, `contextlib.py`, `copy.py`, `functools.py`, `io.py`, `operator.py`, `pickle.py`, `string.py`, `unittest/`, `warnings.py`, `test/` |
| Written for apython | `_codecs.py`, `_io.py`, `_thread.py`, `itertools.py` |

The four apython files stand in for CPython C extension modules of the same
name; they are covered by the repository's MIT license.  Each carries a
docstring saying what it does and does not implement.

`_io.py` is the one that is only half here: the raw layer and the type
objects are assembly, in the builtin module `_iocore`, and this is the
buffering and text layer stacked on top of them.  The split is invisible from
outside -- the types say `_io` in their `__module__`, and CPython's own
`Lib/io.py` imports every name it needs from `_io` and works unchanged.  It
must not be called `_pyio`, which is the name CPython gives its own Python
replica of the C module: on a path where CPython's stdlib is visible, theirs
would win, and theirs opens with `from io import ...`.
