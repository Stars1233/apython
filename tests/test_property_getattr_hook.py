# An AttributeError raised by a @property getter is not a failure: it is the
# attribute saying it is absent, and __getattr__ is Python's hook for exactly
# that.  CPython's slot_tp_getattr_hook wraps the whole of __getattribute__,
# so it cannot tell whether the error came from the search or from a
# descriptor the search ran.  Here the descriptor is run OUTSIDE
# instance_getattr -- by op_load_attr, by op_load_attr_property once the site
# has specialized, and by obj_getattr_opt for getattr()/hasattr() -- so all
# three have to join the two halves back up.
#
# Every read below goes through ONE site called repeatedly, so the specialized
# opcode sees it too; a fresh site is cold and takes the generic path.


class G:
    @property
    def v(self):
        raise AttributeError("not here")

    def __getattr__(self, name):
        return "fallback:" + name


class NoHook:
    @property
    def v(self):
        raise AttributeError("no fallback")


class Warm:
    """Succeeds until it does not, at one site, so the cache is installed and
    warm before the getter starts raising."""

    def __init__(self):
        self.n = 0

    @property
    def v(self):
        self.n += 1
        if self.n > 100:
            raise AttributeError("gone")
        return self.n

    def __getattr__(self, name):
        return "hook:" + name


class Other:
    @property
    def v(self):
        raise ValueError("a real failure")

    def __getattr__(self, name):
        return "should not be reached"


def read(o, k):
    out = []
    for _ in range(k):
        out.append(o.v)
    return out


def the_hook_answers():
    r = read(G(), 200)
    return r[0], r[-1], len(set(r))


def a_warm_site_starts_using_the_hook():
    r = read(Warm(), 200)
    return r[0], r[99], r[100], r[-1]


def no_hook_still_raises():
    o = NoHook()
    caught = 0
    for _ in range(200):
        try:
            o.v
        except AttributeError as e:
            caught += 1
            msg = str(e)
    return caught, msg


def only_an_attribute_error():
    """Anything else out of a getter is a genuine failure and keeps
    unwinding -- the hook must not swallow it."""
    o = Other()
    caught = 0
    for _ in range(200):
        try:
            o.v
        except ValueError as e:
            caught += 1
            msg = str(e)
    return caught, msg


def getattr_and_hasattr_see_it():
    g, n = G(), NoHook()
    out = []
    for _ in range(200):
        out.append(getattr(g, "v", "DEFAULT"))
    return (out[0], len(set(out)), hasattr(g, "v"),
            getattr(n, "v", "DEFAULT"), hasattr(n, "v"))


def the_hook_may_raise_in_its_turn():
    class H:
        @property
        def v(self):
            raise AttributeError("gone")

        def __getattr__(self, name):
            raise AttributeError("still gone: " + name)

    h = H()
    caught = 0
    for _ in range(200):
        try:
            h.v
        except AttributeError as e:
            caught += 1
            msg = str(e)
    return caught, msg, getattr(h, "v", "DEFAULT"), hasattr(h, "v")


def the_error_is_not_chained():
    """__getattr__ is not HANDLING the AttributeError, it is the rest of the
    lookup, so nothing it raises carries the first as its context."""
    class H:
        @property
        def v(self):
            raise AttributeError("first")

        def __getattr__(self, name):
            raise KeyError("second")

    h = H()
    for _ in range(5):
        try:
            h.v
        except KeyError as e:
            ctx = e.__context__
    return type(ctx).__name__ if ctx is not None else None


def a_getset_and_a_slot_are_unaffected():
    class S:
        __slots__ = ("a",)

        def __getattr__(self, name):
            return "slot-hook:" + name

    s = S()
    out = [getattr(s, "a", "unset")]
    s.a = 5
    for _ in range(200):
        last = s.a
    out.append(last)
    out.append(s.missing)
    return out


print(the_hook_answers())
print(a_warm_site_starts_using_the_hook())
print(no_hook_still_raises())
print(only_an_attribute_error())
print(getattr_and_hasattr_see_it())
print(the_hook_may_raise_in_its_turn())
print(the_error_is_not_chained())
print(a_getset_and_a_slot_are_unaffected())
