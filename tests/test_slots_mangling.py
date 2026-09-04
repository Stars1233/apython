# A private __slots__ name is mangled, as any private name in a class body is.
#
# CPython's type_new mangles each slot as it builds the member descriptor, and
# leaves __slots__ itself as the class wrote it.  type_from_parts built them
# raw, so the descriptor was `__x` where every use of it compiles to `_C__x`,
# and `__slots__ = ('__x',)` with `self.__x = 5` in the same class was an
# AttributeError -- a legal program CPython runs and this did not.
#
# The rules are the compiler's, and have to be, or the two halves name
# different things: two leading underscores, not two trailing ones, and the
# class name with its own leading underscores stripped.


class C:
    __slots__ = ('__x', 'y', '_z', '__dunder__', '_C__already', '_')

    def __init__(self):
        self.__x = 1
        self.y = 2
        self._z = 3
        self.__dunder__ = 4
        self._C__already = 5
        self._ = 6

    def read(self):
        return (self.__x, self.y, self._z, self.__dunder__,
                self._C__already, self._)


c = C()
print("inside the class   ", c.read())
print("__slots__ unchanged", C.__slots__)
print("mangled key        ", '_C__x' in C.__dict__, '__x' in C.__dict__)
print("left alone         ", sorted(k for k in C.__dict__
                                    if k in ('y', '_z', '__dunder__', '_')))
print("already mangled    ", '_C__already' in C.__dict__)
print("reachable by name  ", type(C._C__x).__name__)

try:
    c.__x
except AttributeError as exc:
    print("from outside       ", exc)


# A class name with leading underscores loses them; one that is nothing but
# underscores mangles nothing at all.
class _D:
    __slots__ = ('__p',)

    def __init__(self):
        self.__p = 7

    def read(self):
        return self.__p


class ___:
    __slots__ = ('__q',)


print("_D                 ", _D().read(), '_D__p' in _D.__dict__)
print("all underscores    ", '__q' in ___.__dict__)


# The mangling is per class, which is the point: a base's private slot and a
# subclass's do not collide.
class Base:
    __slots__ = ('__v',)

    def set_base(self, v):
        self.__v = v

    def get_base(self):
        return self.__v


class Derived(Base):
    __slots__ = ('__v',)

    def set_derived(self, v):
        self.__v = v

    def get_derived(self):
        return self.__v


d = Derived()
d.set_base("base")
d.set_derived("derived")
print("no collision       ", d.get_base(), d.get_derived())
print("two descriptors    ", '_Base__v' in Base.__dict__,
      '_Derived__v' in Derived.__dict__)
print("distinct offsets   ", Base.__dict__['_Base__v'] is not
      Derived.__dict__['_Derived__v'])

# A slot list given as a list rather than a tuple, and a bare string, are the
# other two forms __slots__ takes.
class ListForm:
    __slots__ = ['__a', '__b']

    def __init__(self):
        self.__a, self.__b = 1, 2

    def read(self):
        return self.__a + self.__b


print("list form          ", ListForm().read(), '_ListForm__a' in ListForm.__dict__)


class StringForm:
    __slots__ = '__only'

    def __init__(self):
        self.__only = "solo"

    def read(self):
        return self.__only


print("string form        ", StringForm().read(),
      '_StringForm__only' in StringForm.__dict__)

# Deleting one, and reading it back after.
e = C()
del e._C__x
try:
    e.read()
except AttributeError as exc:
    print("after del          ", type(exc).__name__)
e._C__x = 99
print("set through mangled", e.read())
print("done")
