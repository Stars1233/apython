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
| Written for apython | `_codecs.py`, `_thread.py`, `itertools.py` |

The three apython files stand in for CPython C extension modules of the same
name; they are covered by the repository's MIT license.  Each carries a
docstring saying what it does and does not implement.
