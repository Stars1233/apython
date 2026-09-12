"""object.__reduce_ex__, in the language the rest of it is written in.

CPython splits this between C and Python: `copyreg._reduce_ex` handles
protocols 0 and 1, and `reduce_newobj` in Objects/typeobject.c handles 2 and
later.  The C half is what `copy.copy(x)` and every modern pickle actually
use, and it was missing here -- `object.__reduce_ex__` raised, so copy,
deepcopy and pickle of any ordinary instance were a TypeError.  That is what
stopped tarfile, whose addfile() copies its TarInfo.

It is written here rather than in the assembly because assembling the
five-tuple means calling back into Python for __getnewargs_ex__,
__getstate__ and the list and dict iterators anyway -- and because a
reduction assembled wrongly does not fail: it makes a pickle that unpickles
into the wrong object.

A private module rather than an addition to copyreg, because copyreg is
CPython's own file whenever a CPython Lib/ is on sys.path, and an addition
there would be invisible exactly when the stdlib is in use.
src/methods/object.asm imports this by name.
"""

import copyreg


def _reduce_newobj(obj):
    """CPython's reduce_newobj: the protocol-2-and-later reduction."""
    cls = obj.__class__

    args = None
    kwargs = None
    getnewargs_ex = getattr(obj, "__getnewargs_ex__", None)
    if getnewargs_ex is not None:
        args, kwargs = getnewargs_ex()
        if not isinstance(args, tuple):
            raise TypeError("first item of the tuple returned by "
                            "__getnewargs_ex__ must be a tuple")
        if not isinstance(kwargs, dict):
            raise TypeError("second item of the tuple returned by "
                            "__getnewargs_ex__ must be a dict")
    else:
        getnewargs = getattr(obj, "__getnewargs__", None)
        if getnewargs is not None:
            args = getnewargs()
            if not isinstance(args, tuple):
                raise TypeError("__getnewargs__ should return a tuple")

    if kwargs:
        callable = copyreg.__newobj_ex__
        newargs = (cls, args, kwargs)
    elif args is not None:
        callable = copyreg.__newobj__
        newargs = (cls,) + args
    else:
        callable = copyreg.__newobj__
        newargs = (cls,)

    getstate = getattr(obj, "__getstate__", None)
    state = getstate() if getstate is not None else None

    # A list or a dict subclass is rebuilt by appending and by assigning:
    # its contents cannot be handed to __new__.
    listitems = iter(obj) if isinstance(obj, list) else None
    dictitems = iter(obj.items()) if isinstance(obj, dict) else None

    return callable, newargs, state, listitems, dictitems


def object_reduce_ex(obj, protocol):
    """What object.__reduce_ex__ answers.

    A type that OVERRIDES __reduce__ is asked first, and its answer is the
    whole answer -- that is how every builtin iterator, range, slice, Ellipsis
    and NotImplemented get pickled, and CPython's object___reduce_ex___impl
    makes the same test before anything else.  Without it, a __reduce__ added
    to a builtin type had no effect at all: pickle and copy both come through
    here, and both went straight past it to the generic reduction, which then
    refused the type it could not rebuild from a class and a state dict.

    Otherwise: protocol 0 and 1 take copyreg's _reconstructor route, and 2 and
    later take the __newobj__ one.  object.__reduce__(self) is protocol 0, as
    CPython's is.
    """
    reduce = getattr(obj, "__reduce__", None)
    if reduce is not None:
        cls_reduce = getattr(type(obj), "__reduce__", None)
        if cls_reduce is not None and cls_reduce is not _OBJECT_REDUCE:
            return reduce()
    if protocol >= 2:
        return _reduce_newobj(obj)
    return copyreg._reduce_ex(obj, protocol)


# object's own __reduce__, to compare a type's against.  Read once, at import,
# because the comparison runs on every pickle of every object.
_OBJECT_REDUCE = object.__reduce__
