# Every builtin iterator publishes __next__ and __iter__ by NAME, not only in
# its tp_iternext / tp_iter slots.
#
# The stdlib asks by name constantly -- heapq.py does `next = it.__next__` at
# module level, inspect.py does `iter(lines).__next__` -- and a slot with no
# matching tp_dict entry answers those wrong.  `iter([1,2]).__next__` was an
# AttributeError, and so was `type(iter([1,2])).__next__`.
#
# The thunk has to call the DEFINING type's slot, not the argument's, or a
# subclass that defines __next__ re-dispatches into itself; and its exhaustion
# arm has to tell a clean stop from a raise, because a NULL from tp_iternext
# means either.

import itertools


def probe(label, it):
    t = type(it)
    print(label,
          "inst:", hasattr(it, "__next__"), hasattr(it, "__iter__"),
          "type:", hasattr(t, "__next__"), hasattr(t, "__iter__"),
          "self:", it.__iter__() is it)


probe("list_iterator     ", iter([1, 2]))
probe("tuple_iterator    ", iter((1, 2)))
probe("str_iterator      ", iter("ab"))
probe("bytes_iterator    ", iter(b"ab"))
probe("bytearray_iterator", iter(bytearray(b"ab")))
probe("range_iterator    ", iter(range(2)))
probe("longrange_iterator", iter(range(2 ** 70)))
probe("set_iterator      ", iter({1, 2}))
probe("frozenset_iterator", iter(frozenset({1, 2})))
probe("dict_keyiterator  ", iter({1: 2}))
probe("dict_valueiterator", iter({1: 2}.values()))
probe("dict_itemiterator ", iter({1: 2}.items()))
probe("dict_reversekeyit ", reversed({1: 2}))
probe("enumerate         ", enumerate([1, 2]))
probe("zip               ", zip([1], [2]))
probe("map               ", map(str, [1]))
probe("filter            ", filter(None, [1]))
probe("reversed          ", reversed([1, 2]))
probe("callable_iterator ", iter(lambda: 0, 1))
probe("memoryview_iter   ", iter(memoryview(b"ab")))
probe("generator         ", (x for x in [1]))
probe("itertools.count   ", itertools.count())
probe("itertools.chain   ", itertools.chain([1], [2]))

# --- the name is the slot: calling it drives the iterator ------------------
it = iter([10, 20, 30])
print(it.__next__(), it.__next__(), next(it))
try:
    it.__next__()
except StopIteration as e:
    print("StopIteration", e.args)

# heapq's shape, which is what actually broke: the bound method outlives the
# expression that made it.
nxt = iter([1, 2, 3]).__next__
print(nxt(), nxt(), nxt())
try:
    nxt()
except StopIteration:
    print("bound method stops")

# inspect's shape: the bound __next__ handed to something that calls it.
_n = iter(["a", "b"]).__next__
print([_n() for _ in range(2)])

# --- an exhausted iterator keeps saying so --------------------------------
e = iter([])
for _ in range(3):
    try:
        e.__next__()
    except StopIteration:
        pass
print("re-exhausted ok")

# --- a raise from inside __next__ is NOT reported as StopIteration --------
d = {1: 1, 2: 2}
di = iter(d)
di.__next__()
d[3] = 3
try:
    di.__next__()
except RuntimeError as r:
    print("RuntimeError:", r)
except StopIteration:
    print("WRONG: reported as StopIteration")

# --- the descriptor names itself the way CPython's does -------------------
t = type(iter([1]))
print(t.__next__.__name__, t.__iter__.__name__)
print(t.__next__ is not object.__init__)

# --- unbound, with the right receiver and the wrong one -------------------
print(t.__next__(iter([7])))
for wrong in ([1, 2], iter((1, 2)), 5):
    try:
        t.__next__(wrong)
    except TypeError as x:
        print("TypeError for", type(wrong).__name__)
    else:
        print("no TypeError for", type(wrong).__name__)

# --- arity is checked ------------------------------------------------------
try:
    iter([1]).__next__(1)
except TypeError:
    print("arity refused")

# --- and iteration itself is unchanged -------------------------------------
print(list(iter([1, 2, 3])), sorted(iter({3, 1, 2})), list(enumerate("ab")))
print(list(zip([1, 2], "ab")), list(map(abs, [-1, 2])), list(filter(bool, [0, 1])))
print(list(reversed(range(4))), dict(iter({1: 2}.items())))
print("done")
