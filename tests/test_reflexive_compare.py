# Comparing two structures that reach each other has to stop.
#
# dict_richcompare had no C-recursion guard, so `x = {}; x['foo'] = x` twice
# and then `x == y` recursed until the machine stack ran out.  The identity
# fast path inside only catches `x == x`.  list and tuple already wrapped
# their bodies in C_RECURSION_ENTER; dict now does too, and CPython's
# test_copy.test_deepcopy_reflexive_dict is the test for it.

def probe(name, fn):
    try:
        r = fn()
    except RecursionError:
        print(name, "RecursionError")
    except TypeError:
        print(name, "TypeError")
    else:
        print(name, "=>", r)


x = {}
x["foo"] = x
y = {}
y["foo"] = y

probe("dict ==", lambda: x == y)
probe("dict !=", lambda: x != y)
probe("dict <", lambda: x < y)
probe("dict >=", lambda: x >= y)
print("identity", x == x, y == y)

# Deeper, and through another container on the way round.
a = {}
a["l"] = [a]
b = {}
b["l"] = [b]
probe("through a list", lambda: a == b)

c = {}
c["t"] = ({"d": c},)
d = {}
d["t"] = ({"d": d},)
probe("through a tuple", lambda: c == d)

# The list and tuple guards still hold.
p = []
p.append(p)
q = []
q.append(q)
probe("list ==", lambda: p == q)
probe("list <", lambda: p < q)

t = ([],)
t[0].append(t)
u = ([],)
u[0].append(u)
probe("tuple ==", lambda: t == u)

# And the counter came back: ordinary comparisons still work afterwards.
print({1: 2} == {1: 2}, {1: 2} == {1: 3}, {} == {})
print([1, [2, [3]]] == [1, [2, [3]]], (1, 2) < (1, 3))
print(sorted([{ "b": 1 }, { "a": 2 }], key=lambda m: sorted(m))[0])

# Repeated, so a leaked counter would show as a spurious RecursionError.
for i in range(20):
    try:
        x == y
    except RecursionError:
        pass
print({"k": 1} == {"k": 1})

# repr and deepcopy of a reflexive dict are unaffected.
print(repr(x))
print(len(x), x["foo"] is x)
print("done")
