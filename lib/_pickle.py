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
#
# PickleBuffer is the exception, and it is not an accelerator: it is a TYPE,
# and protocol 5 is defined in terms of it.  CPython has nowhere else to put
# it -- `from _pickle import PickleBuffer` at pickle.py's module scope is the
# only way in -- so without it the whole out-of-band buffer protocol is
# missing and pickle.py's own `dispatch[PickleBuffer]` arm is unreachable.


class PickleBuffer:
    """A wrapper around a buffer, for PEP 574's out-of-band pickling.

    CPython's is a C type that supports the buffer protocol itself; this one
    cannot, which is the one thing it does not do.  Nothing in pickle.py
    needs that: it reaches for `raw()` and uses the memoryview, and the
    Unpickler hands the buffers from `buffers=` straight back.

    A released PickleBuffer refuses raw() rather than answering a dangling
    view, and a non-contiguous buffer is refused at raw() rather than at
    construction -- both CPython's choices, and both what test_pickle asks.
    """

    __slots__ = ("_view",)

    def __init__(self, buffer):
        self._view = memoryview(buffer)

    def raw(self):
        """-> a NEW one-dimensional memoryview of unsigned bytes.

        New every time, and that is not an optimisation to skip: pickle.py
        writes `with obj.raw() as m:`, so whatever this returns is RELEASED
        when the block ends.  Handing back the stored view meant the second
        call found it released, and the out-of-band path -- which calls raw()
        once inside the dump and once more on the buffer the callback kept --
        died with "operation forbidden on released memoryview object".
        """
        view = self._view
        if view is None:
            raise ValueError(
                "operation forbidden on released PickleBuffer object")
        if not view.contiguous:
            raise BufferError(
                "cannot extract raw buffer from non-contiguous buffer")
        if view.format == "B" and view.ndim == 1:
            return view[:]
        return view.cast("B")

    def release(self):
        """Drop the view.  Idempotent, as CPython's is."""
        view = self._view
        if view is not None:
            self._view = None
            view.release()
