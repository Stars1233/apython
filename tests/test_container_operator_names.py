# A builtin container's operators are reachable by name.
#
# The slot was there and the name was not: hasattr(list, "__add__") was False,
# and so was every other one of these.  type_getattr is a pure tp_dict walk,
# so a slot with no dict entry is invisible -- to the operator module, to
# anything that duck-types on a dunder, and to dir().
#
# The split between the two shapes is CPython's own.  A SEQUENCE slot raises
# for an operand it refuses, so [1].__add__(5) is a TypeError; an nb_ slot
# declines with NULL and the dunder has to answer NotImplemented instead, so
# {}.__or__(5) is NotImplemented.  Getting that backwards would be a wrong
# answer rather than a missing name.

print("=== which names each type carries ===")
for t, names in (
        (list, ["__add__", "__mul__", "__rmul__", "__imul__", "__iadd__",
                "__contains__", "__getitem__"]),
        (str, ["__add__", "__mul__", "__rmul__", "__mod__", "__rmod__",
               "__contains__", "__getitem__"]),
        (bytes, ["__add__", "__mul__", "__rmul__", "__mod__", "__rmod__",
                 "__contains__", "__getitem__"]),
        (bytearray, ["__add__", "__mul__", "__rmul__", "__iadd__", "__imul__",
                     "__mod__", "__rmod__", "__contains__", "__getitem__",
                     "__setitem__", "__delitem__"]),
        (tuple, ["__add__", "__mul__", "__rmul__"]),
        (dict, ["__or__", "__ror__", "__ior__"]),
        (set, ["__sub__", "__rsub__", "__and__", "__rand__", "__xor__",
               "__rxor__", "__or__", "__ror__"]),
        (frozenset, ["__sub__", "__and__", "__xor__", "__or__"]),
        (object, ["__setattr__", "__delattr__", "__getattribute__",
                  "__getstate__", "__subclasshook__"]),
):
    print(t.__name__, " ".join(n for n in names if n in t.__dict__))


def show(label, fn):
    try:
        print("%-26s %r" % (label, fn()))
    except Exception as e:
        print("%-26s %s: %s" % (label, type(e).__name__, e))


def show_type(label, fn):
    try:
        print("%-26s %r" % (label, fn()))
    except Exception as e:
        print("%-26s %s" % (label, type(e).__name__))


print("=== and each one answers ===")
show("list.__add__", lambda: list.__add__([1], [2]))
show("list.__mul__", lambda: list.__mul__([1], 2))
show("list.__rmul__", lambda: list.__rmul__([1], 2))
show("list.__imul__", lambda: list.__imul__([1], 2))
show("str.__add__", lambda: str.__add__("a", "b"))
show("str.__mod__", lambda: str.__mod__("%s!", "x"))
show("str.__contains__", lambda: str.__contains__("abc", "b"))
show("str.__getitem__", lambda: str.__getitem__("abc", 1))
show("bytes.__add__", lambda: bytes.__add__(b"a", b"b"))
show("bytes.__mod__", lambda: bytes.__mod__(b"%d", 5))
show("bytearray.__mul__", lambda: bytearray.__mul__(bytearray(b"a"), 2))
show("dict.__or__", lambda: dict.__or__({1: 1}, {2: 2}))
show("dict.__ior__", lambda: dict.__ior__({1: 1}, {2: 2}))
show("set.__sub__", lambda: sorted(set.__sub__({1, 2}, {2})))
show("set.__rsub__", lambda: sorted(set.__rsub__({2}, {1, 2})))
show("frozenset.__and__",
     lambda: sorted(frozenset.__and__(frozenset({1, 2}), {1})))
show("tuple.__add__", lambda: tuple.__add__((1,), (2,)))

print("=== a refused operand: raise or NotImplemented, per CPython ===")
show("list.__add__(x, 5)", lambda: list.__add__([1], 5))
# Two of these compare only the exception TYPE.  Both messages come from the
# implementation the dunder reaches, not from the dunder, and both differ from
# CPython's for reasons that predate the names being reachable at all:
# list_repeat words a bad count as the OPERATOR does ("can't multiply sequence
# by non-int"), where CPython's __mul__ words it as the index protocol does;
# and dict_nb_ior's "object is not iterable" does not name the type.
show_type("list.__mul__(x, 'a')", lambda: list.__mul__([1], "a"))
show("str.__add__(x, 5)", lambda: str.__add__("a", 5))
show("str.__rmod__(x, 5)", lambda: str.__rmod__("x", 5))
show("bytes.__rmod__(x, 5)", lambda: bytes.__rmod__(b"%d", 5))
show("dict.__or__(x, 5)", lambda: dict.__or__({}, 5))
show_type("dict.__ior__(x, 5)", lambda: dict.__ior__({}, 5))
show("set.__and__(x, 5)", lambda: set.__and__({1}, 5))

print("=== object's five ===")
class Holder:
    pass

o = Holder()
object.__setattr__(o, "x", 1)
print("setattr    :", o.x)
print("getattribute:", object.__getattribute__(o, "x"))
print("getstate   :", object.__getstate__(o))
object.__delattr__(o, "x")
print("delattr    :", hasattr(o, "x"))
print("getstate bare:", object.__getstate__(object()))
print("subclasshook:", object.__subclasshook__(int))

print("=== the operators themselves are unchanged ===")
print([1] + [2], [1] * 2, 2 * [1], "a" + "b", "a" * 2, "%s!" % "x")
print(b"a" + b"b", b"%d" % 5, {1: 1} | {2: 2}, sorted({1, 2} - {2}),
      (1,) + (2,), sorted(frozenset({1, 2}) & {1}))
d = {1: 1}
d |= {2: 2}
print("dict |=:", sorted(d))
ba = bytearray(b"a")
ba += b"b"
print("bytearray +=:", bytes(ba))


# __getstate__ collects a class's __slots__, and only those.  It scanned every
# type in the MRO for member descriptors, so a BUILTIN's own -- func_type has
# one for __globals__ -- were taken for slots: `f.__getstate__()` on a
# function answered a dict holding the whole module namespace.
print("=== getstate over slots and builtins ===")


def a_function():
    pass


class Slotted:
    __slots__ = ("a", "b")


class Derived(Slotted):
    __slots__ = ("c",)


class Plain:
    pass


print(a_function.__getstate__())
print(len.__getstate__())
print((1).__getstate__(), "x".__getstate__(), [].__getstate__())
print(Slotted().__getstate__())
s = Slotted()
s.a = 1
print(s.__getstate__())
d = Derived()
d.a = 1
d.c = 3
print(d.__getstate__())
p = Plain()
print(p.__getstate__())
p.x = 2
print(p.__getstate__())
