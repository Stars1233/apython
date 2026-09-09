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


# A walrus target inside a comprehension binds in the ENCLOSING scope, and
# which kind of binding that is depends on what the enclosing scope has said
# about the name -- not only on what kind of scope it is.  Taking the kind as
# the whole answer declared the target nonlocal even under a `global`, and the
# classifier then went looking for a binding it had just been told not to look
# for: "no binding for nonlocal 'G' found".
G = None


def to_a_global():
    global G
    [G := 5 for _ in range(1)]
    return G


print(to_a_global(), G)


def to_a_nonlocal():
    x = 0

    def inner():
        nonlocal x
        [x := 7 for _ in range(1)]

    inner()
    return x


print(to_a_nonlocal())


def to_a_plain_local():
    y = 0
    [y := 3 for _ in range(1)]
    return y


print(to_a_plain_local())

MODULE_LEVEL = None
[MODULE_LEVEL := 9 for _ in range(1)]
print(MODULE_LEVEL)


# The global declared inside a nested function, with the comprehension two
# deep, which is where the climb out of the comprehension scopes matters.
H = None


def two_deep():
    global H
    [[H := i for i in range(3)] for _ in range(1)]
    return H


print(two_deep(), H)


# A comprehension has a scope of its own and a class body's namespace is not
# one its children can see, so a walrus inside a comprehension in a class body
# has nowhere to bind.  CPython refuses the combination outright; this used to
# accept it and bind somewhere surprising.
def refuse(source):
    try:
        compile(source, "<s>", "exec")
        return "NOT REFUSED"
    except SyntaxError as e:
        return e.msg


print(refuse("class K:\n    [A := 2 for _ in range(1)]\n"))
print(refuse("class K:\n    {A := 2 for _ in range(1)}\n"))
print(refuse("class K:\n    [[A := 2 for _ in range(1)] for _ in range(1)]\n"))

# A bare walrus in a class body is fine -- it binds in the class namespace --
# and so is one inside a method, where the enclosing scope is a function.
print(refuse("class K:\n    A = (B := 2)\n"))
print(refuse("class K:\n    def m(self):\n        [A := 2 for _ in range(1)]\n"))
print(refuse("def f():\n    [A := 2 for _ in range(1)]\n"))
print(refuse("[A := 2 for _ in range(1)]\n"))
print(refuse("class K:\n    [x for x in range(1)]\n"))


class ClassBodyWalrus:
    VALUE = (SEEN := 7)


print(ClassBodyWalrus.VALUE, ClassBodyWalrus.SEEN)
