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


# The nine names CPython's _pickle also publishes, resolved on first use.
#
# The comment above says they were left out so that pickle.py would not take
# them as a fast path.  That was the wrong half of the trade, and it cost 855
# tests: `import _pickle` SUCCEEDS here, so test_pickle sets
# has_c_implementation and then does, at class-body scope,
#
#     from _pickle import dump, dumps, load, loads, Pickler, Unpickler
#
# which raises ImportError and takes the whole module down before a single
# test runs.  A stand-in that answers `import` but not `from ... import` is
# the half-implemented-is-worse shape.
#
# They cannot be plain assignments: pickle.py reaches for PickleBuffer above
# at its line 43, long before it has defined a Pickler, so importing pickle
# from here at module scope is circular.  PEP 562's module __getattr__ defers
# the lookup to the first ACCESS, which is pickle.py's own line 1818 -- by
# then pickle is fully executed, and the objects handed back are its own.
# So `pickle.Pickler is pickle._Pickler` either way, exactly as the
# except-ImportError branch would have left it, and nothing is a slow
# implementation wearing a fast name: there is only one implementation.
_FORWARDED = {
    "PickleError": "PickleError",
    "PicklingError": "PicklingError",
    "UnpicklingError": "UnpicklingError",
    "Pickler": "_Pickler",
    "Unpickler": "_Unpickler",
    "dump": "_dump",
    "dumps": "_dumps",
    "load": "_load",
    "loads": "_loads",
}


def __getattr__(name):
    try:
        source = _FORWARDED[name]
    except KeyError:
        raise AttributeError(
            "module %r has no attribute %r" % (__name__, name)) from None
    import pickle
    value = getattr(pickle, source)
    globals()[name] = value         # resolve once; __getattr__ is the slow path
    return value


def __dir__():
    return sorted(set(globals()) | set(_FORWARDED))
