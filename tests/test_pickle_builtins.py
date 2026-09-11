# The builtin types answer pickle, copy and deepcopy.
#
# CPython gives every object it can rebuild a __reduce__, and pickle, copy and
# deepcopy all go through it: one without falls back to copyreg's generic
# reduction, which refuses anything it cannot rebuild from a class and a state
# dict.  So range, slice, Ellipsis, NotImplemented, every builtin iterator,
# every bound method and every builtin function were unpicklable and
# uncopyable here -- 2,106 error lines over 28 of CPython's test modules, the
# largest single cluster in the sweep, with test_range 192 of it against 28
# tests.
#
# The shapes are CPython's, read off the running interpreter:
#
#   a sequence iterator   (iter, (seq,), index), or (iter, (empty,)) once
#                         exhausted, since next() drops its reference then
#   a range iterator      (iter, (range(current, stop, step),), None)
#   range / slice         (the type, (start, stop, step))
#   Ellipsis              'Ellipsis'          -- a bare name, which pickle
#   NotImplemented        'NotImplemented'       resolves in builtins
#   a builtin function    its own name
#   a bound method        (getattr, (receiver, name))
#
# object.__reduce_ex__ is the other half and was the reason none of it took
# effect at first: it never asked whether the type OVERRODE __reduce__, so
# every one of these went straight past to the generic reduction.

# copy and deepcopy rather than pickle: this tree's lib/pickle.py is a stub
# and the suite runs without CPython's on the path.  They exercise the same
# machinery -- copy.copy calls __reduce_ex__ exactly as pickle does -- and the
# pickling itself is checked against CPython's own test_pickle and test_range.
import copy
import operator

# --- what each answers ------------------------------------------------------
print(range(1, 10, 2).__reduce__(), "range")
print(slice(1, 9, 2).__reduce__(), "slice")
print(Ellipsis.__reduce__(), "Ellipsis")
print(NotImplemented.__reduce__(), "NotImplemented")
print(iter([1, 2, 3]).__reduce__(), "a fresh list iterator")
print(iter(range(5)).__reduce__(), "a fresh range iterator")

it = iter([10, 20, 30])
next(it)
print(it.__reduce__(), "a used list iterator")

used = iter(range(9))
next(used)
next(used)
print(used.__reduce__(), "a used range iterator")

drained = iter([1])
next(drained)
try:
    next(drained)
except StopIteration:
    pass
print(drained.__reduce__(), "an exhausted iterator names an empty")

s = iter("héllo")
next(s)
print(s.__reduce__(), "a wide str iterator counts code points")

# --- the round trips --------------------------------------------------------
for obj in (range(1, 10, 2), range(0), slice(1, 9, 2), slice(None),
            Ellipsis, NotImplemented):
    print(copy.copy(obj) == obj, "copy", repr(obj))
    print(copy.deepcopy(obj) == obj, "deepcopy", repr(obj))

for label, make in (("list", lambda: iter([1, 2, 3])),
                    ("tuple", lambda: iter((1, 2))),
                    ("str", lambda: iter("ab")),
                    ("bytes", lambda: iter(b"ab")),
                    ("bytearray", lambda: iter(bytearray(b"ab"))),
                    ("range", lambda: iter(range(4)))):
    print(list(copy.copy(make())), "copied whole:", label)
    part = make()
    next(part)
    print(list(copy.copy(part)), "copied part-way:", label)
    deep = make()
    next(deep)
    print(list(copy.deepcopy(deep)), "deepcopied part-way:", label)

# --- the state, applied by hand ---------------------------------------------
st = iter([10, 20, 30])
st.__setstate__(2)
print(next(st), "__setstate__ moves the index")
st.__setstate__(-4)
print(next(st), "a negative state is the start")
st2 = iter([10, 20, 30])
st2.__setstate__(99)
try:
    next(st2)
    print(False, "a state past the end must exhaust it")
except StopIteration:
    print(True, "a state past the end exhausts it")

# --- how many are left ------------------------------------------------------
print([operator.length_hint(x) for x in
       (iter([1, 2, 3]), iter(range(7)), iter("abcd"), iter(b"ab"),
        iter(range(10, 0, -2)), iter(range(0)))], "length_hint")

lh = iter([1, 2, 3])
next(lh)
print(lh.__length_hint__(), "after one next")

# --- the callables ----------------------------------------------------------
# A builtin reduces to its own NAME, which pickle resolves as a global; that is
# what lets an iterator's own reduce name `iter`.  A BOUND one reduces through
# getattr, as a bound method does.
print(len.__reduce__(), "a builtin function")
print(iter.__reduce__(), "and iter, which every iterator's reduce names")


class Holder:
    def __init__(self, data):
        self.data = data

    def method(self):
        return self.data


h = Holder([1, 2])
call, args = h.method.__reduce__()
print(call is getattr, args[0] is h, args[1], "a bound method")
print(call(*args)(), "and it still calls")

bound_builtin = [1, 2].append
call, args = bound_builtin.__reduce__()
print(call is getattr, args[1], "a bound builtin")
