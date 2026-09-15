# `pickle`, CPython's own -- and copyreg, which protocol 2 and up rest on.
#
# lib/pickle.py was thirty lines that raised NotImplementedError.  That is
# the shape memory `half-implemented-is-worse` names: a name that exists and
# refuses is worse than a name that is absent, because the caller cannot fall
# back.  CPython's file works here as it is, and so does copyreg's -- ours
# omitted __newobj__ and _slotnames, which is exactly what protocol 2 reaches
# for, so the two had to move together.
#
# PickleBuffer had nowhere else to live: `from _pickle import PickleBuffer` at
# pickle.py's module scope is the only way in, so PEP 574's whole out-of-band
# path was unreachable.  It is a type, not an accelerator, and lib/_pickle.py
# is where it now is.
import copy
import copyreg
import io
import pickle

HIGHEST = pickle.HIGHEST_PROTOCOL
print("HIGHEST_PROTOCOL:", HIGHEST, "DEFAULT:", pickle.DEFAULT_PROTOCOL)


def roundtrip(obj, proto):
    return pickle.loads(pickle.dumps(obj, proto))


# --- every protocol over the shapes an ordinary program pickles --------
VALUES = [
    None, True, False, 0, 1, -1, 255, 256, -(2 ** 31), 2 ** 63, 10 ** 40,
    0.0, -0.0, 1.5, 1e300, float("inf"), "", "abc", "héllo", "\U0001f600",
    b"", b"\x00\xff", bytearray(b"ab"), (), (1,), (1, 2, 3), [], [1, [2, [3]]],
    {}, {1: "a", "b": 2}, set(), {1, 2, 3}, frozenset({1, 2}),
    complex(1, 2), range(3), slice(1, 9, 2),
]
for proto in range(HIGHEST + 1):
    bad = []
    for v in VALUES:
        got = roundtrip(v, proto)
        if got != v or type(got) is not type(v):
            bad.append(v)
    print("protocol %d: %s" % (proto, "all %d values" % len(VALUES)
                               if not bad else "FAILED %r" % (bad,)))

# -0.0 keeps its sign, which == would not have caught.
print("negative zero:", [repr(roundtrip(-0.0, p)) for p in range(HIGHEST + 1)])
# and NaN is pickled as NaN
print("nan:", [roundtrip(float("nan"), p) != roundtrip(float("nan"), p)
               for p in range(HIGHEST + 1)])


# --- classes, which is where copyreg comes in --------------------------
class Plain:
    def __init__(self, x=1):
        self.x = x

    def __eq__(self, other):
        return type(other) is type(self) and other.x == self.x


class Slotted:
    __slots__ = ("a", "b")

    def __init__(self):
        self.a = 1
        self.b = [2]

    def __eq__(self, other):
        return type(other) is type(self) and (other.a, other.b) == (self.a, self.b)


class Both(Plain):
    __slots__ = ("y",)

    def __init__(self):
        Plain.__init__(self, 7)
        self.y = 8

    def __eq__(self, other):
        return type(other) is type(self) and (other.x, other.y) == (self.x, self.y)


class WithState(Plain):
    def __getstate__(self):
        return {"x": self.x, "extra": 1}

    def __setstate__(self, state):
        self.x = state["x"]
        self.seen = state["extra"]


class WithReduce:
    def __init__(self, n):
        self.n = n

    def __reduce__(self):
        return (WithReduce, (self.n,))

    def __eq__(self, other):
        return type(other) is type(self) and other.n == self.n


class WithNewArgs(tuple):
    def __new__(cls, a, b):
        self = tuple.__new__(cls, (a, b))
        return self

    def __getnewargs__(self):
        return (self[0], self[1])


# A class with __slots__ and no __getstate__ cannot be pickled below
# protocol 2 -- copyreg._reduce_ex refuses it, because protocol 0 and 1 have
# no way to say "construct without calling __init__" -- so each class is
# tried over the protocols that can hold it.
for cls, lowest, args in ((Plain, 0, ()), (Slotted, 2, ()), (Both, 2, ()),
                          (WithReduce, 0, (5,))):
    ok = all(roundtrip(cls(*args), p) == cls(*args)
             for p in range(lowest, HIGHEST + 1))
    print("%-12s %s" % (cls.__name__,
                        "protocols %d..%d" % (lowest, HIGHEST) if ok
                        else "FAILED"))
    if lowest:
        try:
            pickle.dumps(cls(*args), lowest - 1)
            print("%-12s protocol %d NOT REFUSED" % ("", lowest - 1))
        except TypeError as exc:
            print("%-12s protocol %d: %s" % ("", lowest - 1, exc))
w = roundtrip(WithState(3), 2)
print("__getstate__/__setstate__:", w.x, w.seen)
n = roundtrip(WithNewArgs(1, 2), 2)
print("__getnewargs__:", type(n).__name__, tuple(n))

# copyreg's own two, which the protocol-2 path calls by name.
print("_slotnames(Slotted):", copyreg._slotnames(Slotted))
print("_slotnames(Both):", copyreg._slotnames(Both))
print("_slotnames(Plain):", copyreg._slotnames(Plain))
print("__newobj__:", type(copyreg.__newobj__(Plain)).__name__,
      hasattr(copyreg.__newobj__(Plain), "x"))
print("_reconstructor:", copyreg._reconstructor(Plain, object, None).__class__.__name__)
print("pickle exports copyreg's dispatch_table:", pickle.dispatch_table is
      copyreg.dispatch_table)

# --- identity and cycles, which the memo is for ------------------------
shared = [1]
a, b = roundtrip([shared, shared], 2)
print("shared list stays shared:", a is b)
cycle = [1]
cycle.append(cycle)
got = roundtrip(cycle, 2)
print("self-referential list:", got[1] is got, got[0])
d = {}
d["self"] = d
got = roundtrip(d, 2)
print("self-referential dict:", got["self"] is got)

# --- exceptions and the odd builtins ----------------------------------
e = roundtrip(ValueError("msg", 2), 2)
print("exception:", type(e).__name__, e.args)
print("a builtin function:", roundtrip(len, 2) is len)
print("a class:", roundtrip(Plain, 2) is Plain)
print("a module-level type:", roundtrip(int, 2) is int)
print("NotImplemented/Ellipsis:", roundtrip(NotImplemented, 2) is NotImplemented,
      roundtrip(Ellipsis, 2) is Ellipsis)

# --- the object interface: Pickler, Unpickler, dump, load -------------
buf = io.BytesIO()
p = pickle.Pickler(buf, 4)
p.dump([1, 2])
p.dump({"a": 3})
buf.seek(0)
u = pickle.Unpickler(buf)
print("two dumps, two loads:", u.load(), u.load())
buf = io.BytesIO()
pickle.dump("x", buf, 2)
buf.seek(0)
print("dump/load:", pickle.load(buf))
print("fast mode:", end=" ")
buf = io.BytesIO()
pk = pickle.Pickler(buf, 2)
pk.fast = 1
pk.dump([1])
print(pickle.loads(buf.getvalue()))

# --- PickleBuffer, and PEP 574's two paths ----------------------------
pb = pickle.PickleBuffer(b"abc")
r = pb.raw()
print("raw:", type(r).__name__, r.format, r.ndim, r.readonly, r.tobytes(), r.shape)
print("raw is fresh each time:", pb.raw() is not pb.raw())
with pb.raw() as m:
    pass
print("survives a with block:", pb.raw().tobytes())
print("writable source:", pickle.PickleBuffer(bytearray(b"xy")).raw().readonly)
print("in-band:", pickle.loads(pickle.dumps(pickle.PickleBuffer(b"abc"), 5)))
bufs = []
data = pickle.dumps(pickle.PickleBuffer(b"abc"), 5, buffer_callback=bufs.append)
print("out-of-band:", data, [bytes(x.raw()) for x in bufs])
print("out-of-band load:",
      bytes(pickle.loads(data, buffers=[x.raw() for x in bufs])))
try:
    pickle.dumps(pickle.PickleBuffer(b"a"), 4)
    print("protocol 4: NOT REFUSED")
except pickle.PicklingError as exc:
    # The wording is compared loosely on purpose: 3.12.3 says "can only
    # pickled" and 3.12.14 fixed the typo, and the file in lib/ is the later
    # one while the oracle on this machine may be the earlier.
    print("protocol 4:", "PickleBuffer" in str(exc) and ">= 5" in str(exc))
strided = memoryview(bytearray(b"abcdef"))[::2]
try:
    pickle.PickleBuffer(strided).raw()
    print("non-contiguous: NOT REFUSED")
except BufferError as exc:
    print("non-contiguous:", exc)
pb.release()
try:
    pb.raw()
    print("released: NOT REFUSED")
except ValueError as exc:
    print("released:", exc)
pb.release()
print("release is idempotent")

# --- what must be refused ---------------------------------------------
print("unpicklable local:", end=" ")


def outer():
    class Local:
        pass
    return Local()


try:
    pickle.dumps(outer())
    print("NOT REFUSED")
except (pickle.PicklingError, AttributeError) as exc:
    # CPython's C pickler raises AttributeError here and its own Python one
    # raises PicklingError; this is the Python one, so only the refusal is
    # compared.
    print("refused")
try:
    pickle.dumps(lambda: 1)
    print("lambda: NOT REFUSED")
except (pickle.PicklingError, AttributeError):
    print("lambda: refused")
try:
    pickle.loads(b"\x80\x06.")
    print("future protocol: NOT REFUSED")
except (pickle.UnpicklingError, ValueError) as exc:
    print("future protocol: refused")
try:
    pickle.dumps(object(), -1)
    print("negative protocol: NOT REFUSED")
except ValueError as exc:
    print("negative protocol:", exc)

# --- and deepcopy, which is copyreg's other caller --------------------
print("deepcopy over the reduction protocol:",
      copy.deepcopy({"k": [Plain(1), Slotted(), WithReduce(2)]})
      == {"k": [Plain(1), Slotted(), WithReduce(2)]})
print("copy of a slotted object:", copy.copy(Slotted()) == Slotted())
print("survived")
