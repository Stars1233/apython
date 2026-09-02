# PEP 604 unions: flat, deduplicated, and with __args__.
#
# union_type_or built a bare 2-tuple of its operands whatever they were, so
# int | str | float nested as ((int | str), float).  union_richcompare
# compares the two argument tuples as sets and read the inner union as one
# opaque member, so (int|str|float) == (float|str|int) was False.  The repr
# hid all of it: a member that is not a type is printed with repr(), which
# re-enters union_repr and prints flat.
#
# __args__ was an AttributeError besides -- union_type carried neither a
# tp_getattr nor a tp_dict, while the tuple sat in the object all along.

u = int | str | float
print(repr(u))
print(u.__args__)
print(len(u.__args__), type(u.__args__).__name__)

# Order does not matter to equality, at any width.
print((int | str) == (str | int))
print(u == (float | str | int), u == (str | float | int))
print(u == (int | str), u == (int | str | float | bytes))
print((int | str | float) != (int | str))

# Associativity does not matter either.
print(((int | str) | float) == (int | (str | float)))
print(repr((int | str) | (str | float)))
print(((int | str) | (str | float)).__args__)

# A repeated member collapses, and a union of one member is that member.
print(int | int, repr(int | int))
print((int | str | int).__args__)
print(((int | str) | int).__args__)

# None is NoneType inside a union, and prints as None.
print(repr(None | int), repr(int | None))
print((None | int) == (int | type(None)))
print((int | None).__args__)
print(repr(int | None | str))

# Equal unions hash equal, which is what makes them dict keys.
print(hash(u) == hash(float | int | str))
d = {u: "u"}
print(d[float | str | int])
print(len({int | str, str | int, int | str | float}))

# Not a union, and not equal to one.
print(u == 5, 5 == u, u == int)
try:
    int | 5
except TypeError:
    print("int | 5 => TypeError")
