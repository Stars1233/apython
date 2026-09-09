# A frozenset's hash is computed once and kept.
#
# The fold is over the table's stored hashes, so it costs a walk of the whole
# CAPACITY -- four to six times the element count -- and every use of a
# frozenset as a dict key or a set member goes through it.  Caching it is
# only safe because a frozenset's elements cannot change after it exists, so
# what is checked here is that nothing which LOOKS like a frozenset can
# change them either: an equal frozenset built a different way must hash the
# same, and a subclass, whose instance is built by a different allocator,
# must not inherit a hash of zero from its zeroed header.

f = frozenset(range(100))
h = hash(f)
print(h == hash(f), h == hash(f), h == hash(frozenset(range(100))))
print(h == hash(frozenset(list(range(99, -1, -1)))))
print(hash(frozenset()) == hash(frozenset([])) == hash(frozenset(set())))
print(hash(frozenset([1])) == hash(frozenset([1])))
print(hash(frozenset([1, 2])) == hash(frozenset([2, 1])))

# a frozenset built from a set, and one built from a frozenset
s = {"a", "b", "c"}
print(hash(frozenset(s)) == hash(frozenset(["c", "b", "a"])))
g = frozenset(s)
print(hash(g) == hash(frozenset(g)), frozenset(g) is g)

# different contents must not collide with each other
hs = [hash(frozenset(range(n))) for n in range(20)]
print(len(set(hs)) == len(hs))

# --- as a dict key and as a set member ------------------------------------
d = {}
for n in range(30):
    d[frozenset(range(n))] = n
print(len(d), d[frozenset(range(17))], d[frozenset()], frozenset(range(30)) in d)
for n in range(30):
    if d[frozenset(range(n))] != n:
        print("MISMATCH", n)
print("dict ok")

ss = {frozenset(range(n)) for n in range(30)}
print(len(ss), frozenset(range(5)) in ss, frozenset(range(30)) in ss)

# --- a subclass, whose instance comes from the generic allocator -----------
class F(frozenset):
    pass


a = F([1, 2, 3])
b = F([4, 5, 6])
print(hash(a) == hash(frozenset([1, 2, 3])), hash(a) == hash(b))
print(hash(a) == hash(F([3, 2, 1])), hash(F([])) == hash(frozenset()))
subs = [hash(F(range(n))) for n in range(10)]
print(len(set(subs)) == len(subs))

# --- a mutable set is never hashable, cache or no cache -------------------
try:
    hash({1, 2})
except TypeError as e:
    print("TypeError", e)


class S(set):
    pass


try:
    hash(S([1, 2]))
except TypeError:
    print("TypeError")

# --- a set built, drained and refilled: the hash follows the elements ------
# (a frozenset cannot be mutated, so this asks the same question of the
# only object that can: a frozenset built from a set that has been churned)
m = set()
for i in range(200):
    m.add(i)
for i in range(200):
    if i % 3:
        m.discard(i)
print(hash(frozenset(m)) == hash(frozenset(range(0, 200, 3))))

# --- the O(1) inequality shortcut -----------------------------------------
# Two frozensets whose cached hashes differ cannot be equal, and
# set_richcompare says so without comparing an element.  What has to keep
# working is everything the shortcut must NOT decide: equal frozensets, a
# frozenset against a mutable set (which never caches a hash), and the
# orderings, none of which the hash can answer.
p = frozenset(range(50))
q = frozenset(range(50))
r = frozenset(range(1, 51))
hash(p), hash(q), hash(r)          # all three now carry a cached hash
print(p == q, p == r, p != r, p != q)
print(p == set(range(50)), set(range(50)) == p, p == frozenset(range(49)))
print(p <= q, p < q, p >= q, p > q)
print(frozenset(range(10)) < p, p < frozenset(range(10)))
print(frozenset(range(10)) <= p, p <= frozenset(range(10)))
print(p == list(range(50)), p == 5, p != 5)

# equal contents reached by different routes, hashed at different times
x = frozenset("abcdef")
y = frozenset(reversed("abcdef"))
print(hash(x) == hash(y), x == y, x != y)
z = frozenset("abcdef")
print(z == x, hash(z) == hash(x))

# one side hashed and the other not
u = frozenset(range(20))
v = frozenset(range(20))
hash(u)
print(u == v, v == u, u != v)
