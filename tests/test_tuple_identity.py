# tuple(t) is t for an exact tuple.
#
# tuple_type_call always iterated its argument into a temporary list and built
# a fresh tuple from that; the fast path was an unwritten TODO in the source.
# A tuple is immutable, so CPython returns the argument unchanged.

t = (1, 2, 3)
print(tuple(t) is t)
e = ()
print(tuple(e) is e)

# Everything else still builds a new tuple.
print(tuple([1, 2]) == (1, 2), type(tuple([1, 2])).__name__)
print(tuple("ab"), tuple(range(3)), tuple({1: "a"}))
print(tuple(x for x in range(3)))
print(tuple(iter([4, 5])))
print(tuple() == (), tuple({7}) == (7,))

# A tuple subclass is not an exact tuple in either direction: tuple(sub) must
# copy, and Sub(t) must build a Sub.
class T(tuple):
    pass
s = T((1, 2))
r = tuple(s)
print(type(r).__name__, r == (1, 2), r is s)
u = T(t)
print(type(u).__name__, u == t, u is t)

# The identity holds through the places that pass a tuple along.
def f(*args):
    return tuple(args) is args
print(f(1, 2))
print(tuple(t) is tuple(t))
d = {t: 1}
print(d[tuple(t)])
