# functools - adapted from CPython for apython

def cmp_to_key(mycmp):
    """Convert a cmp= function into a key= function"""
    class K(object):
        __slots__ = ['obj']
        def __init__(self, obj):
            self.obj = obj
        def __lt__(self, other):
            return mycmp(self.obj, other.obj) < 0
        def __gt__(self, other):
            return mycmp(self.obj, other.obj) > 0
        def __eq__(self, other):
            return mycmp(self.obj, other.obj) == 0
        def __le__(self, other):
            return mycmp(self.obj, other.obj) <= 0
        def __ge__(self, other):
            return mycmp(self.obj, other.obj) >= 0
        __hash__ = None
    return K


def reduce(function, iterable, initial=None):
    """Apply function of two arguments cumulatively to iterable items."""
    it = iter(iterable)
    if initial is None:
        try:
            value = next(it)
        except StopIteration:
            raise TypeError("reduce() of empty iterable with no initial value")
    else:
        value = initial
    for element in it:
        value = function(value, element)
    return value


def partial(func, *args, **kwargs):
    """Create a partial function application."""
    def wrapper(*more_args, **more_kwargs):
        all_kwargs = dict(kwargs)
        all_kwargs.update(more_kwargs)
        return func(*args, *more_args, **all_kwargs)
    return wrapper
