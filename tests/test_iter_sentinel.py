# iter(callable, sentinel).
#
# The two-argument form did not exist -- `iter(f, 0)` was "iter() takes
# exactly one argument" -- and it is the ordinary way to read a stream until a
# marker turns up: `for chunk in iter(lambda: f.read(4096), b"")`.


def t(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


def counter(limit=None):
    n = [0]

    def f():
        n[0] += 1
        return n[0]
    return f


t("basic", lambda: list(iter(counter(), 4)))
t("type", lambda: type(iter(counter(), 4)).__name__)
t("empty", lambda: list(iter(lambda: 0, 0)))
t("one", lambda: list(iter(counter(), 2)))
t("next", lambda: (lambda it: (next(it), next(it)))(iter(counter(), 9)))
t("iter of iter", lambda: (lambda it: iter(it) is it)(iter(counter(), 3)))
t("exhausted stays", lambda: (lambda it: (list(it), list(it)))(iter(counter(), 3)))


def after_stop():
    it = iter(counter(), 2)
    list(it)
    try:
        next(it)
    except StopIteration:
        return "stopped"
    return "no stop"


t("next after stop", after_stop)

# The sentinel is compared with ==, not by identity.
t("eq not is", lambda: list(iter(lambda: [1], [1])))


class Eq:
    def __init__(self, n):
        self.n = n

    def __eq__(self, other):
        return isinstance(other, Eq) and self.n == other.n


def eq_objs():
    seq = [Eq(1), Eq(2), Eq(3)]
    i = [0]

    def f():
        v = seq[i[0]]
        i[0] += 1
        return v
    return len(list(iter(f, Eq(3))))


t("custom __eq__", eq_objs)

# A callable that raises propagates; the exception is not swallowed.
def raiser():
    n = [0]

    def f():
        n[0] += 1
        if n[0] == 3:
            raise ValueError("boom")
        return n[0]
    return list(iter(f, 99))


t("raises", raiser)


# A comparison that raises propagates too.
class BadEq:
    def __eq__(self, other):
        raise RuntimeError("cmp")


t("bad __eq__", lambda: list(iter(lambda: BadEq(), 0)))

# Argument checking.  The wording of the "must be callable" message is not
# stable across CPython patch releases -- 3.12.3 says
# "iter(object, sentinel): object must be callable" and 3.12.14 says
# "iter(v, w): v must be callable" -- so these two check the type and the
# gist rather than the text.
def tc(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, "must be callable" in str(e))


tc("not callable", lambda: iter(5, 0))
tc("none callable", lambda: iter(None, 0))
t("three args", lambda: iter(1, 2, 3))
t("no args", lambda: iter())

# The one-argument form is untouched.
t("one arg list", lambda: list(iter([1, 2, 3])))
t("one arg str", lambda: list(iter("ab")))
t("one arg noniter", lambda: iter(5))

# It works in a for loop, which is the point of it.
def in_for():
    out = []
    for v in iter(counter(), 5):
        out.append(v)
    return out


t("for loop", in_for)

# And it is a real iterator, so the itertools take it.
t("in list()", lambda: list(iter(counter(), 3)))
t("in sum()", lambda: sum(iter(counter(), 5)))
t("in zip()", lambda: list(zip(iter(counter(), 4), "abc")))
t("in enumerate", lambda: list(enumerate(iter(counter(), 3))))

print("done")
