# collections - High-performance container datatypes (minimal for apython)
#
# deque and defaultdict live in _collections, and are imported back from
# there.  That is CPython's own arrangement, and the reason for it is that
# CPython's collections/__init__.py does `from _collections import deque` and
# exports the name from __all__ whether or not the import succeeded -- so a
# missing _collections made `from collections import deque` an ImportError
# under a real stdlib, not merely a slower deque.
#
# OrderedDict goes the other way, for the same reason read in reverse.
# CPython defines the complete pure-Python class HERE and only then tries to
# override it with the C one, so an `OrderedDict = dict` alias exported from
# _collections shadowed the real class wherever a real stdlib was on the
# path.  The class below is the whole of what OrderedDict adds over an
# insertion-ordered dict.

from _collections import deque, defaultdict


class OrderedDict(dict):
    """Dictionary that remembers insertion order, and can reorder itself.

    dict has preserved insertion order since 3.7, so the ordering itself is
    inherited.  What this adds is the part that is not free: move_to_end,
    popitem's `last` argument, and an __eq__ that is order-sensitive against
    another OrderedDict.
    """

    def move_to_end(self, key, last=True):
        """Move an existing key to either end of the ordered dictionary."""
        if key not in self:
            raise KeyError(key)
        value = dict.pop(self, key)
        if last:
            dict.__setitem__(self, key, value)
            return
        # No cheaper way with a plain dict behind us: re-insert everything so
        # the moved key leads.
        rest = list(dict.items(self))
        dict.clear(self)
        dict.__setitem__(self, key, value)
        for k, v in rest:
            dict.__setitem__(self, k, v)

    def popitem(self, last=True):
        """Remove and return a (key, value) pair; LIFO if last is true."""
        if not self:
            raise KeyError("dictionary is empty")
        if last:
            return dict.popitem(self)
        key = next(iter(self))
        return key, dict.pop(self, key)

    def __eq__(self, other):
        # Order-sensitive against another OrderedDict, order-insensitive
        # against a plain dict -- which is CPython's rule, not a shortcut.
        #
        # `dict.__eq__` cannot serve the second half: dict registers no
        # __eq__ of its own, so the name resolves to object.__eq__ and
        # answers NotImplemented (bugs.md carries it).  Two plain dicts built
        # from the operands reach dict_richcompare directly instead.
        if isinstance(other, OrderedDict):
            return list(self.items()) == list(other.items())
        if isinstance(other, dict):
            return dict(self) == dict(other)
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented:
            return result
        return not result

    def __repr__(self):
        # 3.12 reprs the mapping, not the item list: OrderedDict({'a': 1}).
        if not self:
            return self.__class__.__name__ + "()"
        return self.__class__.__name__ + "(" + repr(dict(self)) + ")"

    def copy(self):
        return self.__class__(self)

    @classmethod
    def fromkeys(cls, iterable, value=None):
        d = cls()
        for key in iterable:
            d[key] = value
        return d

    def __reduce__(self):
        inst_dict = vars(self).copy() if hasattr(self, "__dict__") else {}
        return self.__class__, (), inst_dict or None, None, iter(self.items())


class Counter:
    """Dict-like for counting hashable items."""

    def __init__(self, iterable=None, **kwargs):
        self._data = {}
        if iterable is not None:
            if isinstance(iterable, dict):
                for key, count in iterable.items():
                    self._data[key] = count
            else:
                for elem in iterable:
                    self._data[elem] = self._data.get(elem, 0) + 1
        if kwargs:
            for key, count in kwargs.items():
                self._data[key] = count

    def __getitem__(self, key):
        return self._data.get(key, 0)

    def __setitem__(self, key, value):
        self._data[key] = value

    def __delitem__(self, key):
        del self._data[key]

    def __contains__(self, key):
        return key in self._data

    def __len__(self):
        return len(self._data)

    def __iter__(self):
        return iter(self._data)

    def __repr__(self):
        return "Counter(%r)" % self._data

    def get(self, key, default=None):
        return self._data.get(key, default)

    def keys(self):
        return self._data.keys()

    def values(self):
        return self._data.values()

    def items(self):
        return self._data.items()

    def most_common(self, n=None):
        items = sorted(self._data.items(), key=lambda x: x[1], reverse=True)
        if n is not None:
            return items[:n]
        return items

    def elements(self):
        for elem, count in self._data.items():
            for _ in range(count):
                yield elem

    def update(self, iterable=None, **kwargs):
        if iterable is not None:
            if isinstance(iterable, dict):
                for key, count in iterable.items():
                    self._data[key] = self._data.get(key, 0) + count
            else:
                for elem in iterable:
                    self._data[elem] = self._data.get(elem, 0) + 1
        for key, count in kwargs.items():
            self._data[key] = self._data.get(key, 0) + count

    def subtract(self, iterable=None, **kwargs):
        if iterable is not None:
            if isinstance(iterable, dict):
                for key, count in iterable.items():
                    self._data[key] = self._data.get(key, 0) - count
            else:
                for elem in iterable:
                    self._data[elem] = self._data.get(elem, 0) - 1
        for key, count in kwargs.items():
            self._data[key] = self._data.get(key, 0) - count

    def total(self):
        return sum(self._data.values())


class ChainMap:
    """A ChainMap groups multiple dicts to create a single, updateable view."""

    def __init__(self, *maps):
        self.maps = list(maps) or [{}]

    def __getitem__(self, key):
        for mapping in self.maps:
            try:
                return mapping[key]
            except KeyError:
                pass
        raise KeyError(key)

    def __setitem__(self, key, value):
        self.maps[0][key] = value

    def __delitem__(self, key):
        try:
            del self.maps[0][key]
        except KeyError:
            raise KeyError(key)

    def __contains__(self, key):
        for mapping in self.maps:
            if key in mapping:
                return True
        return False

    def __len__(self):
        seen = set()
        for mapping in self.maps:
            for key in mapping:
                seen.add(key)
        return len(seen)

    def get(self, key, default=None):
        try:
            return self[key]
        except KeyError:
            return default

    def keys(self):
        seen = set()
        for mapping in self.maps:
            for key in mapping:
                seen.add(key)
        return seen

    def values(self):
        return [self[key] for key in self.keys()]

    def items(self):
        return [(key, self[key]) for key in self.keys()]

    def new_child(self, m=None):
        if m is None:
            m = {}
        return ChainMap(m, *self.maps)

    @property
    def parents(self):
        return ChainMap(*self.maps[1:])


def namedtuple(typename, field_names, rename=False, defaults=None, module=None):
    """Returns a new subclass of tuple with named fields."""
    if isinstance(field_names, str):
        field_names = field_names.replace(',', ' ').split()
    field_names = tuple(field_names)
    num_fields = len(field_names)

    # Build the class
    class _NT(tuple):
        __slots__ = ()
        _fields = field_names

        def __new__(cls, *args, **kwargs):
            if len(args) + len(kwargs) > num_fields:
                raise TypeError("Expected %d arguments, got %d" % (num_fields, len(args) + len(kwargs)))
            values = list(args)
            for name in field_names[len(args):]:
                if name in kwargs:
                    values.append(kwargs[name])
                elif defaults is not None and name in field_names[num_fields - len(defaults):]:
                    idx = list(field_names[num_fields - len(defaults):]).index(name)
                    values.append(defaults[idx])
                else:
                    raise TypeError("Missing required argument: %r" % name)
            return tuple.__new__(cls, values)

        def __repr__(self):
            parts = []
            for i, name in enumerate(field_names):
                parts.append("%s=%r" % (name, self[i]))
            return "%s(%s)" % (typename, ", ".join(parts))

        def _asdict(self):
            return dict(zip(field_names, self))

        def _replace(self, **kwargs):
            d = self._asdict()
            d.update(kwargs)
            return type(self)(**d)

    _NT.__name__ = typename
    _NT.__qualname__ = typename

    # Add property accessors for each field
    for i, name in enumerate(field_names):
        def _getter(self, i=i):
            return self[i]
        setattr(_NT, name, property(_getter))

    return _NT
