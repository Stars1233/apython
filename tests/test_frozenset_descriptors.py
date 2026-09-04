# set and frozenset used to be registered from one shared table, so the eight
# operator dunders, __len__ and __iter__ all had one body apiece and the
# receiver check had to admit both types.  set.__and__(frozenset(...), ...)
# was therefore accepted where CPython raises: CPython gives frozenset its
# own descriptors, and each refuses the other's instances.
#
# The same change made the descriptor TypeError name the descriptor.
# dunder_require_self had only the two type names to work with -- the
# generators know their own suffix, so they pass it now and the message reads
# as CPython's.

s = {1, 2}
f = frozenset({1, 2})

OPS = ("__sub__", "__and__", "__xor__", "__or__",
       "__rsub__", "__rand__", "__rxor__", "__ror__")


def call(fn, *args):
    try:
        return repr(fn(*args))
    except TypeError as e:
        return "TypeError: " + str(e)


# Each type's own descriptor accepts its own instances and refuses the
# sibling's.  The reflected four are real on frozenset in CPython too:
# frozenset({2}).__rsub__({1}) is frozenset({1}).
for name in OPS:
    print(name,
          call(getattr(set, name), s, {1}),
          call(getattr(set, name), f, {1}),
          call(getattr(frozenset, name), f, {1}),
          call(getattr(frozenset, name), s, {1}))

for name in ("__len__", "__iter__"):
    print(name,
          call(getattr(set, name), f),
          call(getattr(frozenset, name), s))
print(set.__len__(s), frozenset.__len__(f), sorted(set.__iter__(s)))

print(sorted(n for n in OPS if hasattr(frozenset, n)) == sorted(OPS))
print(sorted(n for n in OPS if hasattr(set, n)) == sorted(OPS))

# A subclass reaches its own base's descriptor and not the sibling's.
class FS(frozenset):
    pass


class S(set):
    pass


print(call(frozenset.__and__, FS({1, 2}), {1}))
print(call(set.__and__, S({1, 2}), {1}))
print(call(set.__and__, FS({1, 2}), {1}))
print(call(frozenset.__and__, S({1, 2}), {1}))

# The operators themselves are untouched.
print(s - {1}, sorted(f - {1}), sorted(s & f), sorted(f | s), sorted(s ^ f))
print(len(f), sorted(f), 1 in f, f == {1, 2}, {1, 2} == f)
print(type(f - {1}).__name__, type(s - f).__name__, type(f - s).__name__)

# And the message names the descriptor for every other builtin that checks a
# receiver by name.  Arity is checked before the receiver here and after it in
# CPython, so each call below passes the right number of arguments.
CASES = (
    ("int.__neg__", int.__neg__, (2.5,)),
    ("int.__invert__", int.__invert__, (2.5,)),
    ("int.__bool__", int.__bool__, ("x",)),
    ("int.__add__", int.__add__, ("x", 1)),
    ("int.__radd__", int.__radd__, ("x", 1)),
    ("int.__divmod__", int.__divmod__, ("x", 1)),
    ("int.__repr__", int.__repr__, ("x",)),
    ("float.__neg__", float.__neg__, (5,)),
    ("str.__str__", str.__str__, (5,)),
    ("str.__repr__", str.__repr__, (5,)),
    ("str.__len__", str.__len__, (5,)),
    ("list.__len__", list.__len__, (5,)),
    ("tuple.__len__", tuple.__len__, (5,)),
    ("dict.__len__", dict.__len__, (5,)),
    ("dict.__setitem__", dict.__setitem__, (5, 1, 2)),
    ("list.__setitem__", list.__setitem__, (5, 0, 1)),
    ("list.__contains__", list.__contains__, (5, 1)),
    ("bytearray.__len__", bytearray.__len__, (5,)),
    ("frozenset.__hash__", frozenset.__hash__, ({1},)),
    ("set.__len__", set.__len__, (5,)),
    ("set.__iter__", set.__iter__, (5,)),
    ("list.__mul__", list.__mul__, (5, 2)),
    ("list.__rmul__", list.__rmul__, (5, 2)),
    ("str.__add__", str.__add__, (5, "a")),
)
for label, fn, args in CASES:
    print(label, call(fn, *args))

# repr() of a type with no tp_repr used to be a NULL Value with no exception
# set: print() silently skipped the argument, and repr(iter({1})) handed its
# own caller a missing argument -- one frame further on, a segfault.  It is a
# string now.  CPython prints the address too; nothing in this tree formats a
# pointer, because it could not match and every test is a diff.
for probe in (iter({1}), iter([1]), iter((1,)), iter({1: 2}), iter("a"),
              iter(b"a")):
    r = repr(probe)
    # not the type name: CPython calls a str's str_ascii_iterator, we call it
    # str_iterator, and that is a naming choice rather than a divergence.
    print(r != "", r.startswith("<"), r.endswith(">"))
print(len([repr(iter({1})), str(iter([1]))]))
