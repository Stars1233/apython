"""POP_EXCEPT releases the exception it replaces, and that release runs code.

Anything the dying exception still holds is freed with it, so an arbitrary
`__del__` runs inside `exc_dealloc`.  Entering that finalizer's Python frame
swaps `handled_exception` into `PyFrame.exc_state` and back, counting it both
ways -- and the global still pointed at the object being freed, so the count
went 0 -> 1 -> 0 and `obj_dealloc` ran on it a SECOND time.  The symptom was a
SIGSEGV in `gc_list_remove` from the outer dealloc, whose block had been freed
and reused underneath it; nothing on the stack named the exception.

The fix is the order every such handler owes: install the replacement, then
release what it replaced.
"""

import gc
import io


class Finalizer:
    """A __del__ that runs a try/except of its own, which is what makes the
    finalizer's frame swap handled_exception."""

    seen = []

    def __del__(self):
        try:
            raise TypeError("inner")
        except TypeError:
            pass
        Finalizer.seen.append(1)


def test_finalizer_inside_pop_except():
    del Finalizer.seen[:]
    for _ in range(5):
        try:
            raise ValueError(Finalizer())
        except ValueError:
            pass
    assert Finalizer.seen == [1] * 5, Finalizer.seen


def test_finalizer_through_the_exception_dict():
    """The same, reached through the instance dict rather than through args --
    which is the road `AttributeError.obj` takes."""
    del Finalizer.seen[:]
    for _ in range(5):
        try:
            raise ValueError("x")
        except ValueError as e:
            e.held = Finalizer()
    assert Finalizer.seen == [1] * 5, Finalizer.seen


def test_nested_handlers_restore_in_order():
    """The restored exception has to be the one the outer block was handling,
    and it has to survive the release of the inner one."""
    try:
        raise ValueError("outer")
    except ValueError as outer:
        try:
            raise TypeError(Finalizer())
        except TypeError:
            pass
        import sys
        assert sys.exc_info()[1] is outer, sys.exc_info()
        assert str(outer) == "outer"


def test_an_io_object_is_the_shape_this_was_found_on():
    """Every _io object has a __del__ through IOBase, which is why an
    exception holding one is the case that crashed."""
    for make in (io.BytesIO, io.StringIO):
        for _ in range(5):
            try:
                raise ValueError(make())
            except ValueError:
                pass
    gc.collect()


for fn in (test_finalizer_inside_pop_except,
           test_finalizer_through_the_exception_dict,
           test_nested_handlers_restore_in_order,
           test_an_io_object_is_the_shape_this_was_found_on):
    fn()
    print(fn.__name__, 'ok')
print('OK')
