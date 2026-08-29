# Every container search -- `in`, count, index, remove, and dict/set key
# lookup -- is PyObject_RichCompareBool in Python.  Nine sites here each
# open-coded their own approximation: identity, sometimes a SmallInt compare,
# sometimes a strcmp, sometimes tp_richcompare.  All of them treated a NULL
# result as "not equal", but NULL means either NotImplemented -- in which
# case the reflected operand and then identity must be tried -- or that the
# comparison raised, in which case it must propagate.  None read
# current_exception, so a raising __eq__ inside `x in list` answered False.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


class AlwaysEq:
    def __eq__(self, other):
        return True

    def __hash__(self):
        return 1


class NeverEq:
    def __eq__(self, other):
        return False

    def __hash__(self):
        return 2


class NotImpl:
    def __eq__(self, other):
        return NotImplemented


class Raises:
    def __eq__(self, other):
        raise ValueError("cmp")


class Key:
    def __init__(self, n):
        self.n = n

    def __eq__(self, o):
        return isinstance(o, Key) and self.n == o.n

    def __hash__(self):
        return self.n


# The element's __eq__ is consulted, from either side
a = AlwaysEq()
print([1, 2, 3].count(a), (1, 2, 3).count(a), [1, 2, 3].index(a), (1, 2, 3).index(a))
print(a in [1, 2, 3], a in (1, 2, 3), a in {1: 0}, a in {1})

n = NeverEq()
print(n in [n], [n].count(n), [n].index(n))

# NotImplemented from both sides falls back to identity, and must not leak
# out as the NotImplemented object itself
x = NotImpl()
print(x == x, x != x, [x].count(x), [NotImpl()].count(NotImpl()))
print(x in [x], NotImpl() in [NotImpl()])

# A raising __eq__ propagates instead of reading as "absent"
r = Raises()
print([t(lambda: r in [1, 2]), t(lambda: [1, 2].count(r)),
       t(lambda: [1, 2].index(r)), t(lambda: [1, 2].remove(r))])

# Hash containers find a key by equality, not by identity
print({Key(1): "a"}[Key(1)], {Key(1): "a"}.get(Key(1)), Key(1) in {Key(1): "a"})
print(Key(1) in {Key(1)}, len({Key(1), Key(1), Key(2)}))

# Cross-type numeric equality, in every container
print(1 in [1.0], 1 in (1.0,), 1.0 in {1}, True in [1], True in {1})
print({1} == {1.0}, {1: "a"}[1.0], [1] == [1.0], (1,) == (1.0,))

# Ordinary cases are untouched
print([1, 2, 3].count(2), [1, 2, 3].index(3), "b" in ["a", "b"], 5 in [1, 2])
lst = [1, 2, 3, 2]
lst.remove(2)
print(lst, lst.count(2), sorted([3, 1, 2]))
print([1, 2] == [1, 2], [1, 2] == [1, 3], (1, 2) < (1, 3), [1] < [1, 2])


# bpo-38610: count/index/remove must hold a strong reference to the element
# across the comparison, because it can free the element.
class Clearing:
    def __eq__(self, other):
        lst.clear()
        return NotImplemented


lst = [Clearing()]
print(t(lambda: lst.index(lst)))

lst = [Clearing()]
print(lst.count(lst), lst)
