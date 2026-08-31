# A block that declares a name `global` does not bind it for nested blocks.
#
# `global x; x = 1` leaves the name flagged DEF_GLOBAL|DEF_LOCAL, because
# sym_visit adds DEF_LOCAL for every store target without looking at the
# declarations.  sym_binds masked only the binding bits, so the declaring block
# looked like a binder, the nested block classified the name SYM_FREE, and
# sym_promote_cells then refused it a cell:
#
#   SyntaxError: internal error: free variable has no cell in the enclosing scope
#
# CPython's analyze_name puts a DEF_GLOBAL name in the `global` set and never in
# `local`, and `local` is the only thing feeding the `bound` set the children
# are told they may capture.
_cache = None


def f():
    global _cache
    _cache = {}

    def helper():
        return len(_cache)

    _cache["a"] = 1
    return helper()


print(f(), _cache)


# A comprehension is the same nested block, and failed identically.
def g():
    global _c2
    _c2 = [1, 2, 3]
    return [x * 2 for x in _c2], sum(v for v in _c2), {k: k for k in _c2}


print(g(), _c2)


# A global declaration hides an enclosing binding from everything nested inside
# it: CPython discards the name from the `bound` set it hands its children, so
# inner() reads the module's x and outer() keeps its own.
x = "module"


def outer():
    x = "outer"

    def mid():
        global x
        x = "global"

        def inner():
            return x

        return inner(), x

    return mid(), x


print(outer(), x)


y = "module-y"


def outer_comp():
    y = "outer-y"

    def mid():
        global y
        return [y for _ in range(1)]

    return mid(), y


print(outer_comp(), y)


# nonlocal is not the same: it does not discard from `bound`, so the chain
# still reaches the real binder and the middle block keeps carrying the cell.
def n_outer():
    v = 1

    def mid():
        nonlocal v
        v = 2

        def inner():
            return v

        return inner()

    r = mid()
    return r, v


print(n_outer())


def gl():
    global counter
    counter = 0

    def bump():
        global counter
        counter += 1

    bump()
    bump()
    return counter


print(gl(), counter)


def aug():
    global total
    total = 10
    add = lambda k: total + k
    return [add(i) for i in range(3)]


print(aug(), total)


def d():
    global tmp
    tmp = 5

    def read():
        return tmp

    r = read()
    del tmp
    return r


print(d(), "tmp" in globals())
