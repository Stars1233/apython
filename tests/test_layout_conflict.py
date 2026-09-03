# Two bases whose instance layouts are unrelated cannot both be laid out in
# one instance.  `class C(MyList, MyDict)` was accepted here and laid out as
# whichever base was wider; the family flags were then OR'd from both, so the
# class claimed to be a list subclass and a dict subclass at once and
# instance_dealloc freed whichever storage its first test matched.
#
# The question is about the solid bases -- the type each base's layout really
# belongs to, ignoring the instance dict word every heaptype adds.  Unless one
# is a subtype of the other, the shapes cannot be nested.


class MyList(list):
    pass


class MyDict(dict):
    pass


class Mixin:
    pass


class Slotted:
    __slots__ = ('x',)


class SlottedToo:
    __slots__ = ('y',)


def attempt(name, bases):
    try:
        type(name, bases, {})
        print(name, "built")
    except TypeError as e:
        print(name, "TypeError:", e)


print("=== the conflicts ===")
attempt("ListDict", (MyList, MyDict))
attempt("IntStr", (int, str))
attempt("StrBytes", (str, bytes))
attempt("TupleList", (tuple, list))
attempt("ExcList", (Exception, MyList))
attempt("TwoSlotted", (Slotted, SlottedToo))

print("=== and what is still allowed ===")
# A plain class adds no layout of its own, so it mixes with anything.
attempt("ListMixin", (MyList, Mixin))
attempt("MixinList", (Mixin, MyList))
attempt("SlottedMixin", (Slotted, Mixin))
attempt("TwoMixins", (Mixin, type("Other", (), {})))
# One base a subtype of the other is a chain, not a conflict.
attempt("ListAndList", (MyList, list))
attempt("SlottedChain", (Slotted, object))

print("=== the class statement says the same thing ===")
try:
    class Bad(MyList, MyDict):
        pass
except TypeError as e:
    print("TypeError:", e)


class Good(MyList, Mixin):
    pass


g = Good()
g.append(1)
g.append(2)
g.tag = "t"
print(g, g.tag, isinstance(g, list), isinstance(g, Mixin))
