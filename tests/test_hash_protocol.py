# hash() refused almost everything, and __hash__ answered the wrong thing.
#
# builtin_hash_fn reimplemented obj_hash's dispatch -- int immediate, float
# immediate, else tp_hash -- and raised when tp_hash was 0.  object_type's WAS
# 0, and tp_hash is inherited, so every instance, plain class, function,
# module, iterator and object() answered TypeError.  `d[obj] = 1` worked the
# whole time, because dict goes through obj_hash, which falls back to the
# address: two dispatchers, one of them wrong.  They must agree in any case or
# a key hashes one way going in and another coming out, so hash() is obj_hash
# now and object_type has a real tp_hash.
#
# object.__hash__ handed back `args[0] + V_INT_BIAS` without unpacking.  For a
# pointer the Value IS the address and biasing makes it an int, which is the
# intended answer -- but an int immediate is already biased, so it was biased
# twice and int.__hash__(5) was -1125899906842619.
#
# Nothing but object registered __hash__, so every builtin resolved the name
# through the MRO to object's, which answers the ADDRESS: str.__hash__('a')
# was a pointer.  And the four unhashable builtins carried
# hash_not_implemented in tp_hash but nothing in tp_dict, so they advertised a
# working __hash__ -- `list.__hash__ is None` was False.
#
# Finally the two rules that are about a name being ABSENT, which no slot
# wrapper can express: `__hash__ = None` makes instances unhashable, and a
# class defining __eq__ without __hash__ is unhashable too.  Both were
# ignored, so `{Eq(): 1}` succeeded.


def show(label, fn):
    try:
        return "%s => %r" % (label, fn())
    except BaseException as e:
        return "%s !! %s: %s" % (label, type(e).__name__, e)


# --- hash() answers for everything that has a hash ---
import sys


def func():
    pass


class Plain:
    pass


for label, v in (("int", 5), ("bigint", 2 ** 70), ("negint", -3),
                 ("float", 1.5), ("str", "a"), ("bytes", b"a"),
                 ("tuple", (1, 2)), ("bool", True), ("none", None),
                 ("frozenset", frozenset({1})), ("complex", 1j),
                 ("slice", slice(1, 2))):
    print(label, "hashable:", isinstance(hash(v), int))

for label, v in (("instance", Plain()), ("object", object()),
                 ("class", Plain), ("function", func),
                 ("lambda", lambda: 1), ("module", sys),
                 ("Ellipsis", Ellipsis), ("iterator", iter([])),
                 ("type", int), ("builtin", len)):
    print(label, "hashable:", isinstance(hash(v), int))

# hash() and dict must agree -- that is the whole point of one dispatcher.
p = Plain()
print("agree:", hash(p) == hash(p), p in {p: 1}, len({p: 1, p: 2}))

# --- the by-name form answers the type's own hash, not object's ---
for label, ty, v in (("int", int, 5), ("str", str, "abc"),
                     ("float", float, 1.25), ("bytes", bytes, b"abc"),
                     ("tuple", tuple, (1, 2)), ("complex", complex, 1j)):
    print(label, "by name == hash():", ty.__hash__(v) == hash(v))

print("bool via int:", bool.__hash__(True) == hash(True))
print("frozenset:", frozenset.__hash__(frozenset({1})) == hash(frozenset({1})))
print("object.__hash__ stable:",
      object.__hash__(p) == object.__hash__(p))
print("int.__hash__ small:", int.__hash__(5), int.__hash__(0), int.__hash__(-3))

# --- the unhashable builtins say so, by name and by call ---
for v in ([1], {1: 2}, {1}, bytearray(b"a")):
    ty = type(v)
    print(ty.__name__, "is None:", ty.__hash__ is None,
          "|", show("hash", lambda v=v: hash(v)))

# --- __hash__ = None, and the __eq__ rule ---
class NoHash:
    __hash__ = None


class Eq:
    def __eq__(self, o):
        return True


class EqHash:
    def __eq__(self, o):
        return True

    def __hash__(self):
        return 7


class Ne:
    def __ne__(self, o):
        return True


class SubEq(Eq):
    pass


class ReHash(Eq):
    def __hash__(self):
        return 9


for ty in (NoHash, Eq, SubEq, EqHash, Ne, ReHash, Plain):
    print(ty.__name__, "is None:", ty.__hash__ is None)

for ty in (NoHash, Eq, SubEq, EqHash, Ne, ReHash, Plain):
    print(show("hash(%s())" % ty.__name__,
               lambda ty=ty: isinstance(hash(ty()), int)))

print(show("{Eq(): 1}", lambda: {Eq(): 1}))
print(show("set([Eq()])", lambda: set([Eq()])))
print(show("{EqHash(): 1}", lambda: len({EqHash(): 1})))
print(show("hash(ReHash())", lambda: hash(ReHash())))
print(show("hash(EqHash())", lambda: hash(EqHash())))

# --- and the message names the type ---
for v in ([1], {1: 2}, {1}, bytearray(b"a"), Eq(), NoHash()):
    print(show("hash", lambda v=v: hash(v)))
print(show("{[1]: 2}", lambda: {[1]: 2}))
print(show("set([[1]])", lambda: set([[1]])))
