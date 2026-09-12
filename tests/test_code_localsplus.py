# co_varnames, co_cellvars and co_freevars are three filters over one tuple.
#
# This code object keeps ONE co_localsplusnames with a parallel
# co_localspluskinds saying which of local, cell and free each entry is -- the
# 3.11 layout, and what the frame's localsplus is addressed by.  CPython keeps
# three tuples.  So each of the three names is a filter over the pair here, and
# only co_varnames existed: it answered the WHOLE tuple, cells and frees
# included, while co_cellvars and co_freevars were published in the type's dict
# and then refused as "attribute is not readable".
#
# The kinds bytes had to grow a bit too.  A local a nested block CAPTURES stays
# in this layout's varnames region, and was marked CO_FAST_LOCAL alone -- so
# `def outer(): z = 1; def inner(): ... z ...` reported co_varnames
# ('z', 'inner') and co_cellvars ().  CPython's rule, which the same filter
# over a CPython .pyc's own kinds bytes already produced correctly: a captured
# local is CELL, and LOCAL as well only when it is a PARAMETER.  co_nlocals
# counts what is left, so that co_nlocals == len(co_varnames) as CPython
# guarantees.


def outer():
    z = 1

    def inner(a):
        b = 2
        return z + a + b

    return inner


def capt(a, b):
    def g():
        return a

    c = 1
    return g


def plain(x, y=1, *args, k=2, **kw):
    loc = 3
    return loc


def none_at_all():
    pass


def two_deep():
    p = 1

    def mid():
        q = 2

        def leaf():
            return p + q

        return leaf

    return mid


class K:
    def m(self):
        def g():
            return self

        return g


lam = lambda p: (lambda: p)

for fn in (outer, outer(), capt, capt(1, 2), plain, none_at_all,
           two_deep, two_deep(), two_deep()(), K.m, K.m(K()), lam, lam(1)):
    c = fn.__code__
    print(c.co_name, c.co_varnames, c.co_cellvars, c.co_freevars,
          c.co_nlocals, c.co_nlocals == len(c.co_varnames))

# And the three are all readable, which is what dir() promises.
print(sorted(n for n in dir(outer.__code__)
             if n in ("co_varnames", "co_cellvars", "co_freevars")))
