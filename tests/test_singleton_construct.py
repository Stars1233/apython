# type(None)(), type(...)() and type(NotImplemented)() answer their own
# singleton, and take no arguments at all.
#
# All three static types had a NULL tp_new, so calling one fell through to the
# ordinary class-construction path, which ALLOCATED a fresh two-word object of
# that type.  It was not the singleton, so `type(None)() is None` was False;
# it had no tp_dealloc, so nothing freed it; and its type carries no tp_flags,
# no tp_dict and no tp_basicsize past the header, so the heap fell over at
# shutdown -- CPython's test_builtin.test_construct_singletons died there.

import gc

for const in None, Ellipsis, NotImplemented:
    tp = type(const)
    print(tp.__name__, tp() is const, tp() is tp(), type(tp()) is tp)
    for args, kwargs in (((1,), {}), ((1, 2), {}), ((), {"a": 1}),
                         ((1,), {"a": 1})):
        try:
            tp(*args, **kwargs)
        except TypeError as e:
            print("  TypeError:", e)
        else:
            print("  NO ERROR", args, kwargs)

# Many of them, then a collection: nothing was allocated, so nothing is left.
for i in range(500):
    a, b, c = type(None)(), type(...)(), type(NotImplemented)()
    assert a is None and b is Ellipsis and c is NotImplemented
gc.collect()
print("repeated")

# The singletons still behave after all that.
print(None is None, bool(None), repr(None), str(None))
print(... is Ellipsis, repr(...), str(...))
print(NotImplemented is NotImplemented, repr(NotImplemented))
print({None: 1}[None], {Ellipsis: 2}[...])
print(type(None).__name__, type(...).__name__, type(NotImplemented).__name__)
print(repr(type(None)), repr(type(...)), repr(type(NotImplemented)))
gc.collect()
print("done")
