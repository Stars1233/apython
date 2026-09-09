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


# A subclass whose __new__ forwards to the base's, with no __init__ of its own.
# type_call hands __init__ the keywords the class was called with; it used to
# hand it whatever the LAST call had left in the pending slot instead, and the
# inner super().__new__(cls, **kw) is a call that leaves one.  dict's __init__
# then read the keyword's VALUE as a positional argument: 'int' object is not
# iterable, from `D(a=1)`.
class ForwardDict(dict):
    def __new__(cls, *a, **k):
        return super().__new__(cls, *a, **k)


class ForwardList(list):
    def __new__(cls, *a, **k):
        return super().__new__(cls, *a, **k)


print(ForwardDict(a=1), ForwardDict([("b", 2)]), ForwardDict())
print(ForwardList([1, 2]), ForwardList())

# And the keywords still reach a Python __init__ that asks for them.
class Counted(dict):
    def __new__(cls, *a, **k):
        return super().__new__(cls, *a, **k)

    def __init__(self, *a, **k):
        super().__init__(*a, **k)
        self.kw = sorted(k)


c = Counted(x=1, y=2)
print(sorted(c.items()), c.kw)

# A keyword a builtin constructor does not take is still a TypeError.
try:
    list(sequence=())
except TypeError:
    print("list keyword refused")


# `T.__new__(cls)` checks its class argument, and every builtin __new__ has to
# do it: `list.__new__(dict)` used to BUILD a dict and hand it back as the
# result of list's constructor.  container_dunder_new and scalar_dunder_new
# each serve four or five types, so neither could ask "is this a subtype of
# ME" -- the owner is the type whose dict the __new__ was reached through, and
# a shared body does not know it.  Each type names itself now.
def newcheck(expr):
    try:
        return repr(eval(expr))
    except TypeError as e:
        return "TypeError: %s" % e


for expr in ("list.__new__(dict)", "dict.__new__(list)", "tuple.__new__(list)",
             "set.__new__(frozenset)", "frozenset.__new__(set)",
             "int.__new__(str)", "str.__new__(int)", "float.__new__(int)",
             "complex.__new__(float)", "list.__new__(1)", "list.__new__(None)",
             "list.__new__()", "int.__new__(bool)", "list.__new__(list)",
             "tuple.__new__(tuple)", "set.__new__(set)"):
    print(expr, "->", newcheck(expr))


# A subclass is a subtype, and its own constructor is still the base's, so it
# passes both checks.
class L(list):
    pass


class D(dict):
    pass


class I(int):
    pass


class S(str):
    pass


print(repr(list.__new__(L)), repr(dict.__new__(D)))
print(repr(int.__new__(I, 5)), repr(str.__new__(S, "x")))
print(type(list.__new__(L)) is L, type(int.__new__(I, 5)) is I)


# Which builtins take keyword arguments, and the carve-outs CPython's
# generated constructors carry.  set and frozenset and tuple and float take
# none; list takes none either, but each draws the line in a different place
# and a subclass can move it.
def kwcheck(expr):
    try:
        return repr(eval(expr))
    except TypeError as e:
        return "TypeError: %s" % e


for expr in ("set(sequence=())", "frozenset(sequence=())", "tuple(sequence=())",
             "list(sequence=())", "float(x=1.0)", "dict(a=1)", "str(object='x')",
             "set().update(x=1)", "{1}.union(x=1)", "{1}.intersection(x=1)",
             "{1}.difference(x=1)", "{1}.symmetric_difference_update(x=1)"):
    print(expr, "->", kwcheck(expr))


# tuple, float and frozenset refuse in __new__ unless the class overrode
# __init__; set refuses in __init__, so overriding __init__ is what lets a
# subclass through; list refuses in __init__ unless the class overrode __new__.
class SetInit(set):
    def __init__(self, seq, n=None):
        super().__init__(seq)
        self.n = n


class TupleNew(tuple):
    def __new__(cls, seq, n=None):
        self = super().__new__(cls, seq)
        self.n = n
        return self


class FloatNew(float):
    def __new__(cls, v, n=None):
        return super().__new__(cls, v)


class FrozenInit(frozenset):
    def __init__(self, *a, **k):
        pass


class ListInit(list):
    def __init__(self, seq, newarg=None):
        super().__init__(seq)
        self.newarg = newarg


class ListNew(list):
    def __new__(cls, seq, newarg=None):
        self = super().__new__(cls, seq)
        self.newarg = newarg
        return self


class PlainSet(set):
    pass


class PlainTuple(tuple):
    pass


for name, thunk in (("SetInit", lambda: SetInit([1], n=2)),
                    ("TupleNew", lambda: TupleNew([1], n=2)),
                    ("FloatNew", lambda: FloatNew(1.0, n=2)),
                    ("FrozenInit", lambda: FrozenInit([1], n=2)),
                    ("ListInit", lambda: ListInit([1], newarg=3)),
                    ("ListNew", lambda: ListNew([1], newarg=3)),
                    ("PlainSet", lambda: PlainSet([1], n=2)),
                    ("PlainTuple", lambda: PlainTuple([1], n=2))):
    try:
        print(name, "->", repr(thunk()))
    except TypeError as e:
        print(name, "-> TypeError:", e)
