# class Sub(list) used to produce a plain list: __build_class__ copied the
# base's tp_new, type_call honoured it and returned whatever the base
# constructor made, so the subclass name was lost and its __init__ never ran.
# The same for tuple, dict, set and str.
#
# A subclass now embeds the base's layout and keeps its __dict__ after it
# (tp_dictoffset), so every base method works on it unchanged.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


class L(list):
    pass


class T(tuple):
    pass


class D(dict):
    pass


class S(set):
    pass


class Str(str):
    pass


class I(int):
    pass


# Identity, construction and contents
print(type(L([1, 2])).__name__, L([1, 2]), list(L([1, 2])), len(L([1, 2])))
print(type(T((1, 2))).__name__, T((1, 2)), T((1, 2))[0], len(T((1, 2))))
print(type(D({"a": 1})).__name__, D({"a": 1}), D({"a": 1})["a"])
print(type(S([1, 2])).__name__, sorted(S([1, 2])), len(S([1, 2])))
print(type(Str("ab")).__name__, Str("ab"), Str("ab").upper(), len(Str("ab")))
print(type(I(7)).__name__, I(7) + 1)

print(isinstance(L([]), list), isinstance(T(()), tuple), isinstance(D(), dict))
print(isinstance(S(), set), isinstance(Str(""), str), isinstance(I(0), int))

# Empty construction
print(L(), T(), D(), S(), repr(Str()), I())

# Base methods operate on the subclass
l = L([3, 1, 2])
l.append(4)
l.sort()
print(l, l.count(1), l.index(2), 3 in l, l[1:3])

d = D({"a": 1})
d["b"] = 2
print(sorted(d.items()), d.get("a"), "b" in d, len(d))

s = S([1, 2])
s.add(3)
print(sorted(s), 2 in s)

# Instance attributes, which need the dict at the right offset
l.tag = "list"
d.tag = "dict"
s.tag = "set"
print(l.tag, d.tag, s.tag)
# A str subclass has no instance dict here: str keeps its characters inline,
# so there is no fixed offset past the header to put one at.  CPython manages
# it with a negative tp_dictoffset scaled by tp_itemsize, which is a larger
# change; for now these behave like bytes and like a __slots__ class.
print(t(lambda: setattr(Str("x"), "tag", 1)) in ("None", "AttributeError"))


# __init__ and __new__ overrides
class WithInit(list):
    def __init__(self, seq, newarg=None):
        super().__init__(seq)
        self.newarg = newarg


u = WithInit([1, 2], newarg=3)
print(type(u).__name__, list(u), u.newarg)


class WithNew(list):
    def __new__(cls, seq, newarg=None):
        self = super().__new__(cls, seq)
        self.newarg = newarg
        return self


v = WithNew([1, 2], newarg=3)
print(type(v).__name__, list(v), v.newarg)

# list() itself takes no keywords, so a subclass that overrides neither
# __new__ nor __init__ must reject them
print(t(lambda: L(sequence=())))


# Subclasses are accepted wherever the base is
print([1] == L([1]), L([1]) == [1], (1,) == T((1,)), "a" == Str("a"))
print("a" + Str("b"), Str("a") + "b", "a" in Str("ab"), Str("a") in "ab")
print(",".join([Str("a"), Str("b")]), "ab".startswith(Str("a")))
print("a,b".split(Str(",")), "ab".replace(Str("a"), "c"))
print({**D({"a": 1})}, dict(D({"b": 2})), [1] + L([2]), (1,) + T((2,)))
print(sorted([L([2]), L([1])]))

e = {}
e.update(D({"c": 3}))
print(e)

# set.update takes any iterable, which is also how a set subclass fills
w = {1}
w.update([2, 3])
w.update({4})
w.update(i for i in [5])
print(sorted(w))

# repr: a subclass of set names itself, as CPython does; frozenset too
print(repr(S([1])), repr(frozenset([1])), repr(frozenset()), repr(set()))
print(repr(L([1])), repr(T((1,))), repr(D({"a": 1})))

# Cycles through a container subclass must still collect
class Holder(list):
    pass


for _ in range(30):
    a = Holder()
    b = Holder()
    a.append(b)
    b.append(a)
    del a, b
print("cycles ok")
