# PEP 604 unions as dict keys.
#
# hash(int | str) raised: union_type carried no tp_hash, and builtin_hash_fn
# raises when that slot is 0.  CPython hashes a union as hash(frozenset(args))
# and compares them as frozensets; frozenset_type.tp_hash is 0 here, so the
# members are combined with XOR instead, which induces the same equivalence --
# order-insensitive and absorbing repeats.  The values differ from CPython's
# and nothing observes them; only "equal objects hash equal" is required.

u1 = int | str
u2 = str | int
print(u1 == u2, hash(u1) == hash(u2))
print(u1 == (int | float), u1 != (int | float))
print({u1: "a"}[u2])
print(u1 == 5, 5 == u1)
print(repr(u1), type(u1).__name__)

d = {}
d[int | str] = 1
d[str | int] = 2
print(len(d), d[int | str])

print(isinstance(hash(int | str), int))
