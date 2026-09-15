"""_pickle - AN ALIAS FOR THE PURE-PYTHON IMPLEMENTATION, NOT A C ACCELERATOR.

CPython ships `_pickle` as a C extension and the module above it --
pickle -- prefers it over the Python code in the same file.  There is
no C accelerator here; the Python half is the whole implementation, and it
works.

This module exists for one reason: `test.support.import_fresh_module`
returns **None** when a module named in its `fresh` list cannot be imported,
and a test file that then does `module.__dict__` dies on
"'NoneType' object has no attribute '__dict__'" while its body is still
executing.  That takes every test in the file with it -- test_datetime is
3,513 of them -- and the code under test is the pure implementation, which is
present and correct.

So importing this succeeds and gives back the same objects the pure module
defines.  Nothing here is faster than what it re-exports, and no number
measured through it should be read as native speed.  DIVERGENCES.md records
the arrangement.
"""

# pickle.py guards its import of this and falls back to its own Pickler and
# Unpickler, which is the whole implementation here.  No names are re-exported
# for the reason _json.py gives: pickle would take them as the fast path, and
# they would be the slow one wearing its name.
