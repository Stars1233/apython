# A __int__ that is a DESCRIPTOR has to be bound before it is called.
#
# dunder_bind is CPython's lookup_maybe_method and every dunder call site in
# the tree goes through it -- a plain function is called unbound with self
# first, anything else that is a descriptor goes through its own __get__ and
# what that answers is called WITHOUT self.  builtin_int was the one site
# never converted: it took whatever dunder_lookup found and jumped to its
# tp_call with self prepended.  So a __int__ whose value is a descriptor had
# the DESCRIPTOR called rather than what its __get__ answers, and
#
#     int(x)   # TypeError: int() argument must be ... not 'K'
#
# while `x.__int__()`, `type(x).__int__(x)`, float(x), len(x), hash(x) and
# every other conversion on the same object worked.
#
# unittest.mock is where this shows up: MagicMock installs a MagicProxy for
# each magic method, one descriptor per name, and materialises the real mock
# in __get__.  So `int(MagicMock())` raised while `float(MagicMock())` did
# not -- and asking for `m.__int__` once, anywhere, made the next int(m) work,
# because the proxy had replaced itself by then.
import operator


class Bound:
    def __init__(self, obj, value):
        self.obj = obj
        self.value = value

    def __call__(self, *args):
        return self.value


class Proxy:
    """A non-function descriptor, the shape unittest.mock's MagicProxy is."""

    def __init__(self, value):
        self.value = value

    def __get__(self, obj, objtype=None):
        return Bound(obj, self.value)


# --- every conversion, over a descriptor dunder ------------------------
for name, fn, value in (("__int__", int, 3), ("__float__", float, 2.5),
                        ("__index__", operator.index, 4),
                        ("__len__", len, 5), ("__bool__", bool, True),
                        ("__str__", str, "S"), ("__repr__", repr, "R"),
                        ("__hash__", hash, 7), ("__complex__", complex, 1j),
                        ("__bytes__", bytes, b"B")):
    K = type("K", (), {name: Proxy(value)})
    try:
        print("%-12s %r" % (name, fn(K())))
    except Exception as exc:
        print("%-12s %s: %s" % (name, type(exc).__name__, exc))

# --- the descriptor really is consulted, and given the instance --------
seen = []


class Recording:
    def __get__(self, obj, objtype=None):
        seen.append((obj is not None, objtype is not None))
        return lambda: 11


R = type("R", (), {"__int__": Recording()})
r = R()
print("int through a recording descriptor:", int(r), seen)

# --- an already-bound method as the dunder, which is the other arm -----
class Holder:
    def value(self):
        return 12


B = type("B", (), {"__int__": Holder().value})
print("an already-bound method:", int(B()))

# --- a plain function stays the fast path ------------------------------
P = type("P", (), {"__int__": lambda self: 13})
print("a plain function:", int(P()))

# --- staticmethod and a callable instance ------------------------------
S = type("S", (), {"__int__": staticmethod(lambda: 14)})
print("a staticmethod:", int(S()))


class JustCallable:
    def __call__(self, *args):
        return 15


C = type("C", (), {"__int__": JustCallable()})
print("a callable instance:", int(C()))

# --- a __get__ that RAISES reaches the caller --------------------------
class Raising:
    def __get__(self, obj, objtype=None):
        raise ValueError("from __get__")


G = type("G", (), {"__int__": Raising()})
try:
    int(G())
    print("a raising __get__: NOT RAISED")
except ValueError as exc:
    print("a raising __get__:", exc)

# --- the fallbacks are reached in order, and each binds ---------------
IDX = type("IDX", (), {"__index__": Proxy(21)})
print("int() falls back to __index__:", int(IDX()))
TR = type("TR", (), {"__trunc__": Proxy(22)})
import warnings

with warnings.catch_warnings(record=True) as caught:
    warnings.simplefilter("always")
    value = int(TR())
print("int() falls back to __trunc__:", value,
      bool(caught) and "__trunc__" in str(caught[0].message))

# --- an int SUBCLASS whose __int__ is a descriptor --------------------
class SubInt(int):
    pass


SI = type("SI", (SubInt,), {"__int__": Proxy(23)})
print("an int subclass:", int(SI(9)))

# --- and what must still be refused -----------------------------------
NOT = type("NOT", (), {"__int__": Proxy("not a number")})
try:
    int(NOT())
    print("a non-int return: NOT REFUSED")
except TypeError as exc:
    print("a non-int return:", exc)
NONE = type("NONE", (), {})
try:
    int(NONE())
    print("no __int__ at all: NOT REFUSED")
except TypeError as exc:
    print("no __int__ at all:", exc)
print("survived")
