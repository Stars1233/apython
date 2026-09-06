# copy.py - Shallow and deep copy operations (minimal for apython)


class Error(Exception):
    pass


def copy(x):
    """Create a shallow copy of x."""
    cls = type(x)

    # Try __copy__
    copier = getattr(cls, '__copy__', None)
    if copier is not None:
        return copier(x)

    # Built-in immutable types: return as-is
    if isinstance(x, (int, float, bool, str, bytes, tuple, frozenset)):
        return x
    if x is None:
        return x

    # Lists
    if isinstance(x, list):
        return list(x)

    # Dicts
    if isinstance(x, dict):
        return dict(x)

    # Sets
    if isinstance(x, set):
        return set(x)

    # Bytearrays
    if isinstance(x, bytearray):
        return bytearray(x)

    # Generic: try to reconstruct
    reductor = getattr(x, '__reduce_ex__', None)
    if reductor is not None:
        rv = reductor(4)
    else:
        reductor = getattr(x, '__reduce__', None)
        if reductor is not None:
            rv = reductor()
        else:
            raise Error("un(shallow)copyable object of type %s" % cls)
    return _reconstruct(x, rv)


def deepcopy(x, memo=None):
    """Create a deep copy of x."""
    if memo is None:
        memo = {}

    d = id(x)
    y = memo.get(d)
    if y is not None:
        return y

    cls = type(x)

    # Try __deepcopy__
    copier = getattr(cls, '__deepcopy__', None)
    if copier is not None:
        y = copier(x, memo)
        memo[d] = y
        return y

    # Immutable types
    if isinstance(x, (int, float, bool, str, bytes, type)):
        return x
    if x is None:
        return x

    # Tuples
    if isinstance(x, tuple):
        y = tuple(deepcopy(item, memo) for item in x)
        memo[d] = y
        return y

    # Frozensets
    if isinstance(x, frozenset):
        y = frozenset(deepcopy(item, memo) for item in x)
        memo[d] = y
        return y

    # Lists
    if isinstance(x, list):
        y = []
        memo[d] = y
        for item in x:
            y.append(deepcopy(item, memo))
        return y

    # Dicts
    if isinstance(x, dict):
        y = {}
        memo[d] = y
        for key, value in x.items():
            y[deepcopy(key, memo)] = deepcopy(value, memo)
        return y

    # Sets
    if isinstance(x, set):
        y = set()
        memo[d] = y
        for item in x:
            y.add(deepcopy(item, memo))
        return y

    # Bytearrays
    if isinstance(x, bytearray):
        y = bytearray(x)
        memo[d] = y
        return y

    # Generic: try __reduce_ex__
    reductor = getattr(x, '__reduce_ex__', None)
    if reductor is not None:
        rv = reductor(4)
    else:
        reductor = getattr(x, '__reduce__', None)
        if reductor is not None:
            rv = reductor()
        else:
            raise Error("un(deep)copyable object of type %s" % cls)

    return _reconstruct(x, rv, memo)


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
