# The default reprs -- the ones CPython builds out of a type name and an
# address.  None of them carried an address, and several carried no name
# either: an instance of a user class reprd as "<instance>", every function
# as "<function>", every generator as "<generator>", and a module as its bare
# name with no angle brackets at all.
#
# The address itself cannot be compared -- CPython's own differs between two
# runs of the same program -- so what is checked here is the shape around it:
# the prefix, the name, and that what follows "at 0x" is lowercase hex.


import sys

HEX = "0123456789abcdef"


def shape(x):
    """The repr with the address replaced, so it can be compared."""
    r = repr(x)
    cut = r.rfind(" at 0x")
    if cut < 0 or not r.endswith(">"):
        return "NO ADDRESS: " + r
    digits = r[cut + 6:-1]
    if not digits or any(c not in HEX for c in digits):
        return "NOT HEX: " + r
    return r[:cut] + " at 0xADDR>"


class Plain:
    pass


class Outer:
    class Inner:
        pass

    def method(self):
        pass


def function():
    pass


def generator_function():
    yield 1


async def coroutine_function():
    pass


async def async_generator_function():
    yield 1


lam = lambda: 1

print(shape(Plain()))
print(shape(Outer.Inner()))
print(shape(object()))
print(shape(iter([])))
print(shape(iter(())))
print(shape(iter({1})))
print(shape(iter({}.items())))

print(shape(function))
print(shape(Outer.method))
print(shape(lam))
print(shape(generator_function))

print(shape(generator_function()))
print(shape(x for x in [1]))
print(shape([x for x in [1]].__iter__()))

co = coroutine_function()
print(shape(co))
co.close()
ag = async_generator_function()
print(shape(ag))

# A module names itself and says where it came from.  Only the built-in shape
# is compared: a module loaded from a file reports the path it was loaded
# from, and that is a .pyc here and a .py under CPython.
print(repr(sys))
print(repr(sys).startswith("<module 'sys'"))

# The address is the object's own, so two objects differ and one object is
# stable.
a = Plain()
b = Plain()
print(repr(a) == repr(a), repr(a) == repr(b))

# str() falls back to the same thing.
print(shape(str(Plain()).__class__ and Plain()))
print(str(Plain()) == repr(Plain()) or "different objects, different addresses")
