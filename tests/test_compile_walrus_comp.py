# PEP 572: a walrus inside a comprehension binds in the scope the
# comprehension appears in, not in the comprehension itself.  Ours are
# compiled as nested functions, so the target has to become a cell of the
# enclosing function -- or a global at module level -- and be declared in
# every comprehension scope in between.  Without that, y was visible only
# inside the comprehension.
r = [y := i for i in range(3)]
print(r, y)


def f():
    out = [v := n * 2 for n in range(3)]
    return out, v


print(f())


def g():
    total = 0
    vals = [(w := k) + total for k in (1, 2)]
    return vals, w


print(g())


# In a genexp, a set comprehension and a dict comprehension.
gen = list(a := q for q in (5, 6))
print(gen, a)
st = {b := q for q in (7,)}
print(sorted(st), b)
dc = {q: (c := q * 2) for q in (4,)}
print(dc, c)


# Nested comprehensions: the innermost walrus still reaches the function.
def h():
    m = [[(z := x + yy) for yy in range(2)] for x in range(2)]
    return m, z


print(h())


# The condition and the iterable can carry one too.
def k():
    out = [n for n in range(5) if (p := n) % 2 == 0]
    return out, p


print(k())


# A walrus outside a comprehension is unchanged.
def plain():
    if (n := 10) > 5:
        return n
    return 0


print(plain())

d = {}
while (item := len(d)) < 3:
    d[item] = item
print(d, item)
