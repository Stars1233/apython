# operator.py - Standard operators as functions (minimal for apython)

def add(a, b):
    return a + b

def sub(a, b):
    return a - b

def mul(a, b):
    return a * b

def truediv(a, b):
    return a / b

def floordiv(a, b):
    return a // b

def mod(a, b):
    return a % b

def pow(a, b):
    return a ** b

def neg(a):
    return -a

def pos(a):
    return +a

def abs(a):
    return __builtins__["abs"](a) if isinstance(__builtins__, dict) else __builtins__.abs(a)

def eq(a, b):
    return a == b

def ne(a, b):
    return a != b

def lt(a, b):
    return a < b

def le(a, b):
    return a <= b

def gt(a, b):
    return a > b

def ge(a, b):
    return a >= b

def not_(a):
    return not a

def and_(a, b):
    return a & b

def or_(a, b):
    return a | b

def xor(a, b):
    return a ^ b

def lshift(a, b):
    return a << b

def rshift(a, b):
    return a >> b

def is_(a, b):
    return a is b

def is_not(a, b):
    return a is not b

def contains(a, b):
    return b in a

def getitem(a, b):
    return a[b]

def setitem(a, b, c):
    a[b] = c

def delitem(a, b):
    del a[b]

def index(a):
    return a.__index__()

def length_hint(obj, default=0):
    """How many items obj will produce, if it can say.

    len() first, then the type's __length_hint__, then the default -- which is
    CPython's order, and the reason an iterator can size the list built from
    it.  Only len() was consulted here, so every iterator answered `default`
    and list(it) started from nothing however much it was about to yield.
    """
    if not isinstance(default, int):
        raise TypeError("'%s' object cannot be interpreted as an integer"
                        % type(default).__name__)
    try:
        return len(obj)
    except TypeError:
        pass
    try:
        hint = type(obj).__length_hint__
    except AttributeError:
        return default
    try:
        val = hint(obj)
    except TypeError:
        return default
    if val is NotImplemented:
        return default
    if not isinstance(val, int):
        raise TypeError("__length_hint__ must be an integer, not %s"
                        % type(val).__name__)
    if val < 0:
        raise ValueError("__length_hint__() should return >= 0")
    return val

class itemgetter:
    __slots__ = ('_items', '_call')

    def __init__(self, item, *items):
        if not items:
            self._items = (item,)
            self._call = self._single
        else:
            self._items = (item,) + items
            self._call = self._multi

    def _single(self, obj):
        return obj[self._items[0]]

    def _multi(self, obj):
        return tuple(obj[i] for i in self._items)

    def __call__(self, obj):
        return self._call(obj)


class attrgetter:
    __slots__ = ('_attrs',)

    def __init__(self, attr, *attrs):
        if not attrs:
            self._attrs = (attr,)
        else:
            self._attrs = (attr,) + attrs

    def __call__(self, obj):
        if len(self._attrs) == 1:
            return _resolve_attr(obj, self._attrs[0])
        return tuple(_resolve_attr(obj, a) for a in self._attrs)


def _resolve_attr(obj, attr):
    for name in attr.split('.'):
        obj = getattr(obj, name)
    return obj


class methodcaller:
    __slots__ = ('_name', '_args', '_kwargs')

    def __init__(self, name, *args, **kwargs):
        self._name = name
        self._args = args
        self._kwargs = kwargs

    def __call__(self, obj):
        return getattr(obj, self._name)(*self._args, **self._kwargs)
