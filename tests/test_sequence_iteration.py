# The legacy sequence-iteration protocol, at every call site.
#
# An object with __getitem__ and __len__ but no __iter__ is iterable in
# CPython: iter() synthesises a sequence iterator that counts up from 0 until
# IndexError.  get_iterator here implements that, so list(), for, tuple(),
# iter() and unpacking all work -- but eleven call sites read tp_iter off the
# type themselves and reject anything without it: list[slice] = x, sum, min,
# max, any, all, set(), frozenset(), list.extend, list +=, the starred-unpack
# opcodes, and `yield from`.
#
# Not academic.  CPython's re parser does `self.data[i:i+1] = p` where p is a
# SubPattern, which has __getitem__ and __len__ and no __iter__ -- so every
# non-capturing group `(?:...)` failed to compile.
#
# The sum and min/max cases came in with the branch that routed them through
# the numeric and comparison protocols; that work replaced a hardcoded type
# ladder with value_type -> tp_iter, where it should have called get_iterator.


class Seq:
    """__getitem__ + __len__, and deliberately no __iter__."""

    def __init__(self, data):
        self.data = data

    def __len__(self):
        return len(self.data)

    def __getitem__(self, i):
        return self.data[i]


class NoLen:
    """__getitem__ alone is enough -- the protocol stops at IndexError."""

    def __init__(self, data):
        self.data = data

    def __getitem__(self, i):
        return self.data[i]


class Empty:
    def __getitem__(self, i):
        raise IndexError(i)


class NotIterable:
    pass


def check(label, fn):
    try:
        print("%-34s %r" % (label, fn()))
    except BaseException as e:
        print("%-34s %s" % (label, type(e).__name__))


s = Seq([1, 2, 3])

print("--- the sites that already worked ---")
check("list(seq)", lambda: list(s))
check("tuple(seq)", lambda: tuple(s))
check("for over seq", lambda: [x for x in s])
check("iter(seq) then next", lambda: next(iter(s)))
check("unpack", lambda: (lambda a, b, c: (a, b, c))(*s))
check("x in seq", lambda: 2 in s)
check("sorted(seq)", lambda: sorted(s))
check("set(seq)", lambda: set(s) == {1, 2, 3})
check("dict.fromkeys(seq)", lambda: sorted(dict.fromkeys(s)))
check("''.join over seq", lambda: "".join(Seq(["a", "b"])))
check("enumerate(seq)", lambda: list(enumerate(s)))
check("zip(seq, seq)", lambda: list(zip(s, s)))
check("map over seq", lambda: list(map(lambda v: v * 2, s)))
check("filter over seq", lambda: list(filter(None, s)))
check("reversed needs __len__", lambda: list(reversed(s)))
check("any/all", lambda: (any(s), all(s)))
check("list.extend(seq)", lambda: (lambda L: (L.extend(s), L)[1])([0]))
check("list(NoLen)", lambda: list(NoLen([4, 5])))
check("list(Empty)", lambda: list(Empty()))

print()
print("--- list slice assignment: what broke (?:...) ---")
check("l[0:1] = seq", lambda: (lambda L: (L.__setitem__(slice(0, 1), s), L)[1])([9, 9]))
check("l[:] = seq", lambda: (lambda L: (L.__setitem__(slice(None), s), L)[1])([9, 9]))
check("l[1:1] = seq", lambda: (lambda L: (L.__setitem__(slice(1, 1), s), L)[1])([9, 9]))
check("l[0:2] = NoLen", lambda: (lambda L: (L.__setitem__(slice(0, 2), NoLen([7])), L)[1])([1, 2, 3]))
check("l[0:1] = Empty", lambda: (lambda L: (L.__setitem__(slice(0, 1), Empty()), L)[1])([1, 2]))
# The extended-slice arm counts first, so a length mismatch is a ValueError.
check("l[::2] = seq (len 3)", lambda: (lambda L: (L.__setitem__(slice(None, None, 2), s), L)[1])([0] * 6))
check("l[::2] = seq (len 2)", lambda: (lambda L: (L.__setitem__(slice(None, None, 2), Seq([8, 9])), L)[1])([0] * 4))

print()
print("--- sum, min, max ---")
check("sum(seq)", lambda: sum(s))
check("sum(seq, 10)", lambda: sum(s, 10))
check("sum(NoLen)", lambda: sum(NoLen([4, 5])))
check("sum(Empty)", lambda: sum(Empty()))
check("min(seq)", lambda: min(s))
check("max(seq)", lambda: max(s))
check("min(NoLen)", lambda: min(NoLen([9, 2])))
check("max(Empty)", lambda: max(Empty()))

print()
print("--- and the refusals still refuse ---")
check("list(NotIterable())", lambda: list(NotIterable()))
check("sum(NotIterable())", lambda: sum(NotIterable()))
check("min(NotIterable())", lambda: min(NotIterable()))
check("l[0:1] = 5", lambda: (lambda L: (L.__setitem__(slice(0, 1), 5), L)[1])([1, 2]))
check("l[0:1] = None", lambda: (lambda L: (L.__setitem__(slice(0, 1), None), L)[1])([1, 2]))
check("sum(5)", lambda: sum(5))
check("min(None)", lambda: min(None))


class Raises:
    def __getitem__(self, i):
        raise RuntimeError("boom")


print()
print("--- a __getitem__ that raises something else propagates ---")
check("list(Raises)", lambda: list(Raises()))
check("sum(Raises)", lambda: sum(Raises()))
check("l[0:1] = Raises", lambda: (lambda L: (L.__setitem__(slice(0, 1), Raises()), L)[1])([1, 2]))

print()
print("--- starred unpack and yield from ---")
# These compile to LIST_EXTEND, SET_UPDATE and GET_YIELD_FROM_ITER, each of
# which read tp_iter itself.  Our own compiler emits LIST_EXTEND where the
# .pyc path did not, so `make check-source` caught three of these and the
# ordinary suite did not -- worth running both ways when touching iteration.
check("[*seq]", lambda: [*s])
check("(*seq,)", lambda: (*s,))
check("{*seq}", lambda: sorted({*s}))
check("f(*seq)", lambda: max(*s))
check("dict(*seq of pairs)", lambda: dict(Seq([(1, 2), (3, 4)])))
check("a, *rest = seq", lambda: (lambda a, *r: (a, r))(*s))


def yields_from():
    yield from s


check("yield from seq", lambda: list(yields_from()))

print()
print("--- the shape re actually uses ---")


class SubPatternish:
    """CPython's re SubPattern: __len__, __getitem__, __setitem__, no __iter__."""

    def __init__(self, data):
        self.data = list(data)

    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        return self.data[index]

    def __setitem__(self, index, code):
        self.data[index] = code


p = SubPatternish([10, 20])
outer = SubPatternish([1, 2, 3])
outer[1:2] = p
print("spliced      :", outer.data)
print("length       :", len(outer))
print("as a list    :", list(outer))

print()
print("--- churn, since the sequence iterator allocates ---")
kept = [list(Seq(list(range(i)))) for i in range(20)]
print("churn        :", len([[i, i] for i in range(3000)]))
print("intact       :", kept[5], len(kept[19]))
print("sums         :", [sum(Seq(list(range(i)))) for i in range(6)])
