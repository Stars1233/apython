# float(), int() and operator.index() over a __dunder__ that misbehaves.
#
#     class C:
#         def __float__(self): return 1     # an int, not a float
#     float(C())                            # SIGSEGV
#
# builtin_float calls the type's nb_float slot and then checks the tag of
# what came back.  When it is not a float it falls through to the generic
# numeric path -- with rdi holding the UNPACKED PAYLOAD of the slot's return
# value rather than the object it was asked to convert.  binop_is_number then
# read ob_type off the integer 1, which is address 1.
#
# Two things reach that: a __float__ that returns the wrong type, and a
# __float__ that RAISES -- a raise arrives as a NULL Value, whose payload is
# 0, so the fall-through dereferenced NULL and the exception was lost on the
# way.  Memory `null-means-two-things` again, one register over.
#
# unittest.mock found it.  MagicMock sets __int__, __float__ and the rest as
# class attributes that are themselves MagicMocks, so they return a MagicMock
# rather than a number, and `float(MagicMock())` is the shape above.
import operator


class Wrong:
    def __float__(self):
        return 1

    def __int__(self):
        return 1.5

    def __index__(self):
        return "x"


class Raises:
    def __float__(self):
        raise ValueError("boom-f")

    def __int__(self):
        raise ValueError("boom-i")

    def __index__(self):
        raise ValueError("boom-x")


wrong, raises = Wrong(), Raises()

# --- a wrong return type is a TypeError, not a crash -------------------
for label, fn, obj in (("float wrong", float, wrong),
                       ("int wrong", int, wrong),
                       ("index wrong", operator.index, wrong),
                       ("float raises", float, raises),
                       ("int raises", int, raises),
                       ("index raises", operator.index, raises)):
    try:
        print("%-14s %s" % (label, fn(obj)))
    except Exception as exc:
        print("%-14s %s: %s" % (label, type(exc).__name__, exc))


# --- a non-function callable as the dunder, which is mock's shape ------
class Callable:
    def __call__(self, *args):
        return 7


for name, fn in (("__int__", int), ("__float__", float),
                 ("__index__", operator.index), ("__len__", len),
                 ("__bool__", bool), ("__str__", str), ("__repr__", repr),
                 ("__hash__", hash)):
    K = type("K", (), {})
    setattr(K, name, Callable())
    try:
        fn(K())
        print("%-12s ok" % name)
    except Exception as exc:
        print("%-12s %s" % (name, type(exc).__name__))


# --- and the whole point: these still work ------------------------------
class Good:
    def __float__(self):
        return 2.5

    def __int__(self):
        return 3

    def __index__(self):
        return 4


good = Good()
print("working dunders:", float(good), int(good), operator.index(good))
print("float on the ordinary types:", float(1), float("1.5"), float(True),
      float(b"2.5"), float(bytearray(b"3.5")))
print("int on the ordinary types:", int(1.9), int("7"), int(True), int(b"8"),
      int("ff", 16))
print("index on the ordinary types:", operator.index(5), operator.index(True))

# A strict subclass is ACCEPTED from all three, deprecated, and converted to
# the exact type -- which is what CPython does and what the message says.
import warnings


class SubFloat(float):
    pass


class SubInt(int):
    pass


class ReturnsSubclass:
    def __float__(self):
        return SubFloat(2.5)

    def __int__(self):
        return SubInt(3)

    def __index__(self):
        return SubInt(4)


rs = ReturnsSubclass()
for label, fn in (("float", float), ("int", int), ("index", operator.index)):
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        value = fn(rs)
    print("%-6s %-4r exact %-5s %s"
          % (label, value, type(value).__name__,
             str(caught[0].message) if caught else "NO WARNING"))

# --- what must still be refused ---------------------------------------
for value, what in ((None, "None"), ([1], "a list"), ({}, "a dict"),
                    (object(), "an object")):
    for fn, fname in ((float, "float"), (int, "int"),
                      (operator.index, "index")):
        try:
            fn(value)
            print("%-6s %-10s NOT REFUSED" % (fname, what))
        except TypeError:
            print("%-6s %-10s TypeError" % (fname, what))
print("survived")
