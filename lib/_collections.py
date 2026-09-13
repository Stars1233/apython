# _collections - the C accelerator module CPython's collections/__init__.py
# reaches for.
#
# It opens with `from _collections import deque` / `defaultdict` in a
# try/except ImportError and exports both from __all__ regardless -- so with
# no _collections at all, `from collections import deque` was an ImportError
# rather than a slower deque.  That blocked contextlib, typing, shlex, glob,
# webbrowser, bdb and the rest of their dependents from importing at all.
#
# So the implementations live here and collections/__init__.py imports them
# back, which is the arrangement CPython uses.  These are the same
# pure-Python classes as before, moved rather than rewritten: deque is
# list-backed, so appendleft and popleft are O(n) where CPython's block-linked
# list is O(1), and every observable answer matches for a finite iterable.
#
# _tuplegetter and _count_elements are deliberately absent.  CPython's
# collections has a pure-Python fallback for each and takes it when the
# import fails, so supplying them would replace a working fallback with a
# slower copy of it.

__all__ = ["deque", "defaultdict"]

# OrderedDict is deliberately NOT here.  CPython's collections/__init__.py
# defines the complete pure-Python class and only *then* tries to override it
# from _collections -- so exporting an `OrderedDict = dict` alias from here
# shadowed the real one everywhere a real stdlib was on the path.  deque and
# defaultdict have no such fallback there and must stay.


class defaultdict(dict):
    """defaultdict(default_factory=None, /, [...]) --> dict with a default.

    A real dict subclass, which is not a detail: isinstance(d, dict) is what
    json, pprint, copy and a great deal of ordinary code branch on, and every
    dict method this class does not mention -- setdefault, clear, popitem,
    fromkeys, ==, reversed -- is inherited rather than forwarded.  It used to
    be a plain class holding a dict in self._data, and CPython's own
    test_defaultdict reported 988 errors against it.

    __missing__ is the whole of the default behaviour, and the interpreter
    already consults it on any dict subclass: dict_subscript checks the type
    and calls it, so `d[k]` reaches this without a __getitem__ here.  Defining
    one would be worse than redundant -- it would force a mapping slot table
    of this class's own and lose the direct read.
    """

    __slots__ = ("default_factory",)

    def __init__(self, default_factory=None, /, *args, **kwargs):
        if default_factory is not None and not callable(default_factory):
            raise TypeError("first argument must be callable or None")
        super().__init__(*args, **kwargs)
        self.default_factory = default_factory

    def __missing__(self, key):
        if self.default_factory is None:
            raise KeyError(key)
        value = self.default_factory()
        self[key] = value
        return value

    def __repr__(self):
        # dict.__repr__ carries the recursion guard, so a dict that contains
        # itself prints {...} rather than running out of stack.
        return "%s(%r, %s)" % (type(self).__name__, self.default_factory,
                               dict.__repr__(self))

    def copy(self):
        return type(self)(self.default_factory, self)

    __copy__ = copy

    def __reduce__(self):
        args = () if self.default_factory is None else (self.default_factory,)
        return type(self), args, None, None, iter(self.items())

    # The three merge operators answer a defaultdict, keeping the factory, as
    # CPython's do -- dict's own would hand back a plain dict.
    def __or__(self, other):
        if not isinstance(other, dict):
            return NotImplemented
        new = self.copy()
        new.update(other)
        return new

    def __ror__(self, other):
        if not isinstance(other, dict):
            return NotImplemented
        new = type(self)(self.default_factory, other)
        new.update(self)
        return new

    def __ior__(self, other):
        self.update(other)
        return self


class deque:
    """deque([iterable[, maxlen]]) --> deque object

    A list-backed double-ended queue.  CPython's is a block-linked list, so
    appendleft and popleft are O(1) there and O(n) here; every observable
    answer is the same.
    """

    def __init__(self, iterable=(), maxlen=None):
        if maxlen is not None and maxlen < 0:
            raise ValueError("maxlen must be non-negative")
        self._maxlen = maxlen
        self._items = []
        self.extend(iterable)

    @property
    def maxlen(self):
        return self._maxlen

    def _trim_right(self):
        if self._maxlen is not None:
            while len(self._items) > self._maxlen:
                del self._items[0]

    def _trim_left(self):
        if self._maxlen is not None:
            while len(self._items) > self._maxlen:
                del self._items[len(self._items) - 1]

    def append(self, x):
        if self._maxlen == 0:
            return
        self._items.append(x)
        self._trim_right()

    def appendleft(self, x):
        if self._maxlen == 0:
            return
        self._items.insert(0, x)
        self._trim_left()

    def extend(self, iterable):
        for x in iterable:
            self.append(x)

    def extendleft(self, iterable):
        for x in iterable:
            self.appendleft(x)

    def pop(self):
        if not self._items:
            raise IndexError("pop from an empty deque")
        return self._items.pop()

    def popleft(self):
        if not self._items:
            raise IndexError("pop from an empty deque")
        x = self._items[0]
        del self._items[0]
        return x

    def clear(self):
        self._items = []

    def count(self, x):
        return self._items.count(x)

    def remove(self, x):
        try:
            self._items.remove(x)
        except ValueError:
            raise ValueError("deque.remove(x): x not in deque")

    def reverse(self):
        self._items.reverse()

    def rotate(self, n=1):
        size = len(self._items)
        if size == 0 or n == 0:
            return
        n = n % size
        if n:
            self._items = self._items[size - n:] + self._items[:size - n]

    def index(self, x, start=0, stop=None):
        if stop is None:
            stop = len(self._items)
        return self._items.index(x, start, stop)

    def insert(self, i, x):
        if self._maxlen is not None and len(self._items) >= self._maxlen:
            raise IndexError("deque already at its maximum size")
        self._items.insert(i, x)

    def copy(self):
        return deque(self._items, self._maxlen)

    def __len__(self):
        return len(self._items)

    def __iter__(self):
        return iter(list(self._items))

    def __reversed__(self):
        return iter(list(reversed(self._items)))

    def __getitem__(self, i):
        return self._items[i]

    def __setitem__(self, i, v):
        self._items[i] = v

    def __delitem__(self, i):
        del self._items[i]

    def __contains__(self, x):
        return x in self._items

    def __bool__(self):
        return len(self._items) != 0

    def __eq__(self, other):
        if isinstance(other, deque):
            return self._items == other._items
        return NotImplemented

    def __ne__(self, other):
        r = self.__eq__(other)
        if r is NotImplemented:
            return r
        return not r

    def __add__(self, other):
        if not isinstance(other, deque):
            return NotImplemented
        return deque(self._items + other._items, self._maxlen)

    def __iadd__(self, other):
        self.extend(other)
        return self

    def __repr__(self):
        if self._maxlen is None:
            return "deque(" + repr(self._items) + ")"
        return "deque(" + repr(self._items) + ", maxlen=" + repr(self._maxlen) + ")"
