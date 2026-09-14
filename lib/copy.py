# copy.py - Shallow and deep copy operations (minimal for apython)


class Error(Exception):
    pass


def _copy_immutable(x):
    return x


def copy(x):
    """Create a shallow copy of x."""
    cls = type(x)

    copier = _copy_dispatch.get(cls)
    if copier is not None:
        return copier(x)

    if issubclass(cls, type):
        # A class is treated as atomic, as CPython treats it.
        return _copy_immutable(x)

    copier = getattr(cls, "__copy__", None)
    if copier is not None:
        return copier(x)

    reductor = _dispatch_table().get(cls)
    if reductor is not None:
        rv = reductor(x)
    else:
        reductor = getattr(x, "__reduce_ex__", None)
        if reductor is not None:
            rv = reductor(4)
        else:
            reductor = getattr(x, "__reduce__", None)
            if reductor is not None:
                rv = reductor()
            else:
                raise Error("un(shallow)copyable object of type %s" % cls)

    if isinstance(rv, str):
        return x
    return _reconstruct(x, rv)


def deepcopy(x, memo=None, _nil=[]):
    """Create a deep copy of x."""
    if memo is None:
        memo = {}

    d = id(x)
    y = memo.get(d, _nil)
    if y is not _nil:
        return y

    cls = type(x)

    copier = _deepcopy_dispatch.get(cls)
    if copier is not None:
        y = copier(x, memo)
    elif issubclass(cls, type):
        y = _copy_immutable(x)
    else:
        copier = getattr(cls, "__deepcopy__", None)
        if copier is not None:
            y = copier(x, memo)
        else:
            reductor = _dispatch_table().get(cls)
            if reductor is not None:
                rv = reductor(x)
            else:
                reductor = getattr(x, "__reduce_ex__", None)
                if reductor is not None:
                    rv = reductor(4)
                else:
                    reductor = getattr(x, "__reduce__", None)
                    if reductor is not None:
                        rv = reductor()
                    else:
                        raise Error("un(deep)copyable object of type %s" % cls)
            if isinstance(rv, str):
                y = x
            else:
                y = _reconstruct(x, rv, memo)

    # memo[id(x)] must be kept alive for as long as the memo is, or a later
    # id() may land on freed memory and match.
    if y is not x:
        memo[d] = y
        memo.setdefault(id(memo), []).append(x)
    return y


def _dispatch_table():
    # copyreg is imported lazily: copy is reached during interpreter start-up
    # on paths where copyreg is not up yet, and an empty table is the right
    # answer there.
    try:
        import copyreg
    except ImportError:
        return {}
    return copyreg.dispatch_table


def _deepcopy_atomic(x, memo):
    return x


def _deepcopy_list(x, memo):
    y = []
    memo[id(x)] = y
    for item in x:
        y.append(deepcopy(item, memo))
    return y


def _deepcopy_tuple(x, memo):
    y = [deepcopy(item, memo) for item in x]
    # A tuple of things that each copied to themselves IS itself, which is
    # what keeps `deepcopy((1, 2))` from allocating.
    for a, b in zip(x, y):
        if a is not b:
            return tuple(y)
    return x


def _deepcopy_dict(x, memo):
    y = {}
    memo[id(x)] = y
    for key, value in x.items():
        y[deepcopy(key, memo)] = deepcopy(value, memo)
    return y


def _deepcopy_set(x, memo):
    y = set()
    memo[id(x)] = y
    for item in x:
        y.add(deepcopy(item, memo))
    return y


def _deepcopy_frozenset(x, memo):
    return frozenset(deepcopy(item, memo) for item in x)


def _deepcopy_bytearray(x, memo):
    return bytearray(x)


def _deepcopy_method(x, memo):
    return type(x)(x.__func__, deepcopy(x.__self__, memo))


# The dispatch tables are keyed on the EXACT type, which is the point of them.
# These used to be an isinstance ladder, and a ladder answers for a subclass
# with its base's arm: deepcopy of a defaultdict gave a plain dict, of a tuple
# subclass a plain tuple, and of a str subclass the SAME OBJECT uncopied.  An
# exact-type miss falls through to the reduce protocol, which is where a
# subclass is rebuilt as itself.
_copy_dispatch = {}
_deepcopy_dispatch = {}

for _t in (type(None), type(Ellipsis), type(NotImplemented), int, float, bool,
           complex, bytes, str, tuple, frozenset, type, range, slice,
           type(copy), type(len)):
    _copy_dispatch[_t] = _copy_immutable
    _deepcopy_dispatch[_t] = _deepcopy_atomic

_copy_dispatch[list] = list.copy
_copy_dispatch[dict] = dict.copy
_copy_dispatch[set] = set.copy
_copy_dispatch[bytearray] = bytearray.copy

_deepcopy_dispatch[list] = _deepcopy_list
_deepcopy_dispatch[tuple] = _deepcopy_tuple
_deepcopy_dispatch[dict] = _deepcopy_dict
# set, frozenset and bytearray are in CPython's table by way of their own
# __reduce_ex__, which this tree does not supply for them yet; until it does,
# an exact-type entry is what keeps deepcopy of a set from answering set().
_deepcopy_dispatch[set] = _deepcopy_set
_deepcopy_dispatch[frozenset] = _deepcopy_frozenset
_deepcopy_dispatch[bytearray] = _deepcopy_bytearray
try:
    _deepcopy_dispatch[type(_deepcopy_atomic.__get__(0))] = _deepcopy_method
except (AttributeError, TypeError):
    pass
del _t


def _reconstruct(x, info, memo=None):
    """Rebuild an object from what __reduce_ex__ answered.

    All five fields, not the first two.  This used to call func(*args) and
    stop, which built an object of the right class with none of its
    contents -- an honest reflection of a world where object.__reduce_ex__
    always raised, and wrong the moment it stopped: `copy.copy(a)` for a
    plain instance came back with an empty __dict__.

    state is the instance dict, or the (dict, slots) pair a class with
    __slots__ produces; listiter and dictiter rebuild a list or a dict
    subclass, which cannot be handed its contents through __new__.
    """
    if isinstance(info, str):
        return x
    if not isinstance(info, tuple):
        raise Error("__reduce__ must return a string or tuple")
    n = len(info)
    if n < 2 or n > 5:
        raise Error("tuple returned by __reduce__ must have 2-5 elements")
    info = info + (None,) * (5 - n)
    func, args, state, listiter, dictiter = info

    deep = memo is not None
    if deep and args:
        args = tuple(deepcopy(arg, memo) for arg in args)
    y = func(*args)
    if deep:
        memo[id(x)] = y

    if state is not None:
        if deep:
            state = deepcopy(state, memo)
        setstate = getattr(y, "__setstate__", None)
        if setstate is not None:
            setstate(state)
        else:
            if isinstance(state, tuple) and len(state) == 2:
                state, slotstate = state
            else:
                slotstate = None
            if state is not None:
                y.__dict__.update(state)
            if slotstate is not None:
                for key, value in slotstate.items():
                    setattr(y, key, value)

    if listiter is not None:
        for item in listiter:
            y.append(deepcopy(item, memo) if deep else item)
    if dictiter is not None:
        for key, value in dictiter:
            if deep:
                key = deepcopy(key, memo)
                value = deepcopy(value, memo)
            y[key] = value
    return y
