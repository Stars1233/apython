# Every object answers __doc__, because object supplies one.  Without it
# `None.__doc__` raised AttributeError, which is exactly what
# types.DynamicClassAttribute does when constructed with no getter:
# `self.__doc__ = doc or fget.__doc__`.  enum builds one of those for every
# member name it has to shadow, so enum could not be imported.
print(None.__doc__ is None or isinstance(None.__doc__, str))
print(hasattr(None, '__doc__'), hasattr(1, '__doc__'), hasattr("s", '__doc__'))
print(hasattr([], '__doc__'), hasattr({}, '__doc__'), hasattr(object(), '__doc__'))


class Undocumented:
    pass


class Documented:
    "the docstring"


print(Undocumented.__doc__)
print(Documented.__doc__)
print(Undocumented().__doc__)
print(Documented().__doc__)


def undocumented_fn():
    pass


def documented_fn():
    "fn doc"


print(undocumented_fn.__doc__, documented_fn.__doc__)

# A class may set its own, and an instance attribute still wins.
class Own:
    __doc__ = "explicit"


print(Own.__doc__, Own().__doc__)

# The shape that found it.
def make(fget=None, doc=None):
    return doc or fget.__doc__


print(make(documented_fn))
print(make(None, "given"))
print(make() is None or isinstance(make(), str))
