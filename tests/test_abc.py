# Abstract base classes, on the native _abc accelerator.
import abc
from abc import ABC, ABCMeta, abstractmethod

class Drawable(ABC):
    @abstractmethod
    def draw(self): ...

class Circle(Drawable):
    def draw(self):
        return "circle"

print(Circle().draw(), issubclass(Circle, Drawable), isinstance(Circle(), Drawable))
try:
    Drawable()
except TypeError as e:
    print("TypeError")

# Virtual subclasses: registered, in nobody's MRO.
class Duck:
    pass

print(issubclass(Duck, Drawable), isinstance(Duck(), Drawable))
Drawable.register(Duck)
print(issubclass(Duck, Drawable), isinstance(Duck(), Drawable))
print(Drawable in Duck.__mro__ if hasattr(Duck, "__mro__") else "n/a")

# Registration is inherited by subclasses of the registered class.
class Mallard(Duck):
    pass
print(issubclass(Mallard, Drawable), isinstance(Mallard(), Drawable))

# __subclasshook__ decides structurally.
class Sized(metaclass=ABCMeta):
    @classmethod
    def __subclasshook__(cls, C):
        if cls is Sized:
            if any("__len__" in B.__dict__ for B in C.__mro__):
                return True
            return NotImplemented
        return NotImplemented

class HasLen:
    def __len__(self):
        return 3
class NoLen:
    pass
print(issubclass(HasLen, Sized), issubclass(NoLen, Sized))
print(isinstance(HasLen(), Sized), isinstance(NoLen(), Sized))

# The cache token moves when a registration invalidates the caches.
t0 = abc.get_cache_token()
class Another:
    pass
Sized.register(Another)
print(abc.get_cache_token() > t0)

# register() returns its argument, so it works as a decorator.
@Drawable.register
class Decorated:
    pass
print(issubclass(Decorated, Drawable))

# Registering a non-class is an error, as is a cycle.
try:
    Drawable.register(42)
except TypeError:
    print("TypeError")
try:
    Duck.register(Drawable)
except (RuntimeError, AttributeError):
    print("no cycle")

# abstractmethod marks, and a subclass that leaves one abstract stays abstract.
class Partial(Drawable):
    pass
try:
    Partial()
except TypeError:
    print("TypeError")
print(sorted(Drawable.__abstractmethods__))
