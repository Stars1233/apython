# isinstance() believes what an object says its class is.
#
# _abc_instancecheck consulted type(instance) and nothing else, on the
# recorded grounds that "nothing here can lie yet".  A property named
# __class__ has been able to lie for a while, and that is most of what a mock
# is for -- so an object standing in for a registered class was judged by
# what it really was, and answered False.
#
# CPython asks the declared class first and the real type only if the two
# differ and the first said no.

import abc


class Shape(abc.ABC):
    pass


class Square(Shape):
    pass


class Registered:
    pass


Shape.register(Registered)


class SaysShape:
    @property
    def __class__(self): return Shape


class SaysSquare:
    @property
    def __class__(self): return Square


class SaysRegistered:
    @property
    def __class__(self): return Registered


class SaysNothing:
    pass


print("=== the declared class is believed ===")
for label, obj in (("a real subclass", Square()),
                   ("says Shape", SaysShape()),
                   ("says a subclass", SaysSquare()),
                   ("says a registered class", SaysRegistered()),
                   ("says nothing", SaysNothing()),
                   ("a registered instance", Registered())):
    print("%-24s %s" % (label, isinstance(obj, Shape)))

print("=== the answer is stable, cache or no cache ===")
m = SaysShape()
print(isinstance(m, Shape), isinstance(m, Shape), isinstance(SaysShape(), Shape))
n = SaysNothing()
print(isinstance(n, Shape), isinstance(n, Shape))

print("=== and an ordinary isinstance is unchanged ===")
print(isinstance(1, int), isinstance("a", str), isinstance([], list),
      isinstance(1, str), isinstance(Square(), Square),
      isinstance(Square(), object))
print(isinstance(1, (str, int)), isinstance(1.0, (str, list)))

print("=== a lie also works through a non-ABC ===")
class Plain: pass
class SaysPlain:
    @property
    def __class__(self): return Plain
print(isinstance(SaysPlain(), Plain), isinstance(SaysPlain(), Shape))
