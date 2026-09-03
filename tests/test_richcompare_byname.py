# No builtin registered __eq__ or __ne__ of its own, so every one resolved the
# name through the MRO to object's, which compares identities.
# `int.__eq__ is object.__eq__` was True where CPython says False, and
# dict.__eq__(d, e) answered NotImplemented where CPython compares contents.
# == itself was always right -- it goes through tp_richcompare -- so this was
# the by-name half, and the stdlib asks by name: `__eq__ = dict.__eq__` in a
# mixin is ordinary.
#
# DEF_DUNDER_RICHCMP calls the DEFINING type's slot, the same rule
# DEF_DUNDER_HASH and DEF_DUNDER_STRREPR follow, so a subclass reaches the
# base's comparison rather than re-entering its own.
#
# Writing it turned up a real operator-level bug underneath: str_compare
# answered False for a non-string right operand instead of DECLINING.  False
# is the right final answer, but not that slot's to give -- declining is what
# lets the protocol ask the other operand, and only when that declines too
# does it fall back to identity.  `'a' == S()` for a class defining __eq__
# was False where CPython calls S.__eq__.

TYPES = (int, str, float, bytes, tuple, dict, list, set, frozenset)
for ty in TYPES:
    print(ty.__name__, [n for n in ("__eq__", "__ne__", "__lt__", "__le__",
                                    "__gt__", "__ge__")
                        if getattr(ty, n) is not getattr(object, n)])


def show(label, fn):
    try:
        return "%s => %r" % (label, fn())
    except BaseException as e:
        return "%s !! %s: %s" % (label, type(e).__name__, e)


PAIRS = [(int, 1, 1), (int, 1, 2), (str, "a", "a"), (str, "a", "b"),
         (float, 1.5, 1.5), (float, 1.5, 2.5), (bytes, b"a", b"a"),
         (tuple, (1,), (1,)), (tuple, (1,), (2,)), (dict, {1: 2}, {1: 2}),
         (dict, {1: 2}, {}), (list, [1], [1]), (list, [1], [2]),
         (set, {1}, {1}), (frozenset, frozenset({1}), frozenset({1}))]
for ty, a, b in PAIRS:
    print(show("%s.__eq__" % ty.__name__, lambda ty=ty, a=a, b=b: ty.__eq__(a, b)))
    print(show("%s.__ne__" % ty.__name__, lambda ty=ty, a=a, b=b: ty.__ne__(a, b)))

# A mismatched operand is NotImplemented, so the caller can try the other side.
for label, fn in (("int/str", lambda: int.__eq__(1, "a")),
                  ("str/int", lambda: str.__eq__("a", 1)),
                  ("str/None", lambda: str.__ne__("a", None)),
                  ("dict/list", lambda: dict.__eq__({}, [])),
                  ("list/tuple", lambda: list.__eq__([], ())),
                  ("set/list", lambda: set.__eq__({1}, [1])),
                  ("bytes/str", lambda: bytes.__eq__(b"a", "a")),
                  ("tuple/list", lambda: tuple.__eq__((1,), [1]))):
    print(show(label, fn))

# The receiver is still checked.
for label, fn in (("int recv", lambda: int.__eq__("a", 1)),
                  ("dict recv", lambda: dict.__eq__([], {})),
                  ("str recv", lambda: str.__eq__(1, "a"))):
    print(show(label, fn))


# A subclass reaches the base's comparison, not its own.
class D(dict):
    pass


class I(int):
    pass


class L(list):
    pass


print(show("D == D", lambda: D([("k", 1)]) == D([("k", 1)])))
print(show("dict.__eq__(D, D)",
           lambda: dict.__eq__(D([("k", 1)]), D([("k", 1)]))))
print(show("int.__eq__(I(5), 5)", lambda: int.__eq__(I(5), 5)))
print(show("list.__eq__(L([1]), [1])", lambda: list.__eq__(L([1]), [1])))


# The operator half: a declining slot must let the other operand answer.
class S:
    def __eq__(self, o):
        return "S.eq"

    __hash__ = None


print("a" == S(), S() == "a", 1 == S(), b"a" == S(), (1,) == S())
print([1] == S(), {1: 2} == S(), {1} == S(), 1.5 == S(), None == S())
print("a" != S(), 1 != S())

# And == itself is untouched.
print(1 == 1, "a" == "a", [1] == [1], {1: 2} == {1: 2}, {1} == {1})
print((1,) == (1,), 1 == 1.0, b"a" == b"a", "a" == 1, 1 == "a")
print("a" != 1, [1] != (1,), sorted([3, 1, 2]), "a" in ["a"], 1 in {1: 2})
print(repr(int.__eq__), repr(dict.__eq__), repr(list.__ne__))

# The other half of "declining is not answering": a left operand whose type
# has NO tp_richcompare at all was sent straight to the identity fallback,
# so the right operand never got asked.  None, bytearray, complex, slice and
# range were all in that state.
print("--- the left operand declines ---")
for label, v in (("int", 1), ("float", 1.5), ("str", "a"), ("bytes", b"a"),
                 ("tuple", (1,)), ("list", [1]), ("dict", {1: 2}),
                 ("set", {1}), ("frozenset", frozenset({1})),
                 ("bool", True), ("none", None),
                 ("bytearray", bytearray(b"a")), ("complex", 1j),
                 ("slice", slice(1)), ("range", range(2))):
    print(label, v == S(), v != S())

# And the identity fallback still happens when NEITHER side can answer.
class Bare:
    __hash__ = None


b1, b2 = Bare(), Bare()
print(b1 == b1, b1 == b2, b1 != b2, None == None, None == 1, 1 == None)
print(Ellipsis == Ellipsis, NotImplemented == NotImplemented)


# All six, not just the two: the orderings were object's as well, so
# str.__lt__('a', 'b') was NotImplemented where CPython says True.
print("--- the orderings ---")
ORD = [(int, 1, 2), (str, "a", "b"), (float, 1.5, 2.5), (bytes, b"a", b"b"),
       (tuple, (1,), (2,)), (list, [1], [2]), (dict, {}, {}),
       (set, frozenset({1}), frozenset({1, 2})),
       (frozenset, frozenset({1}), frozenset({1, 2}))]
for ty, a, b in ORD:
    for op in ("__lt__", "__le__", "__gt__", "__ge__"):
        print(show("%s.%s" % (ty.__name__, op),
                   lambda ty=ty, op=op, a=a, b=b: getattr(ty, op)(a, b)))

print(show("set.__lt__ subset", lambda: set.__lt__({1}, {1, 2})))
print(show("str.__lt__ mismatched", lambda: str.__lt__("a", 1)))
print(show("int.__lt__ mismatched", lambda: int.__lt__(1, "a")))
print(1 < 2, "a" < "b", [1] < [2], (1,) < (2,), {1} < {1, 2}, 1 < 2.5)
print(sorted([3, 1, 2]), sorted(["b", "a"]), max([1, 5, 3]), min("cba"))
