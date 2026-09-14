# A slice bound's __index__ can empty the container it is slicing.
#
# CPython's issue #27863, and the same shape appeared here the moment slice
# bounds started honouring __index__: resolving a bound RUNS PYTHON CODE, and
# that code can clear, shrink or grow the very sequence being indexed.
#
# The ordering is the whole of it.  CPython splits the work in two --
# PySlice_Unpack converts the three bounds, then the CALLER reads the length,
# then PySlice_AdjustIndices clamps -- so the clamp sees the length the user
# code left behind.  slice_indices did all three in one call with the length
# passed in, read before the call, so `e[0:10:X()] = []` with an X whose
# __index__ empties e clamped against a length that no longer existed and
# wrote through a freed buffer.  CPython's own test_xml_etree has that exact
# line, and it segfaulted.
#
# The `_live` entry point takes a POINTER to the container's size field and
# reads it after the conversion; list and bytearray use it, and the immutable
# sequences do not need to.
import sys


def case(name, fn):
    try:
        print("%-22s -> %r" % (name, fn()))
    except Exception as e:
        print("%-22s %s: %s" % (name, type(e).__name__, e))
    sys.stdout.flush()


# --- the container is emptied --------------------------------------------

L = list(range(10))


class ClearsL:
    def __index__(self):
        L[:] = []
        return 1


case("list setslice", lambda: L.__setitem__(slice(0, 10, ClearsL()), []))
print("L is now:", L)

L2 = list(range(10))


class ClearsL2:
    def __index__(self):
        L2[:] = []
        return 1


case("list getslice", lambda: L2[0:10:ClearsL2()])

L3 = list(range(10))


class ClearsL3:
    def __index__(self):
        del L3[:]
        return 1


case("list delslice", lambda: L3.__delitem__(slice(0, 10, ClearsL3())))

B = bytearray(range(10))


class ClearsB:
    def __index__(self):
        B.clear()
        return 1


case("bytearray setslice", lambda: B.__setitem__(slice(0, 10, ClearsB()), b""))
case("bytearray value", lambda: bytes(B))

B2 = bytearray(range(10))


class ClearsB2:
    def __index__(self):
        B2.clear()
        return 1


case("bytearray getslice", lambda: bytes(B2[0:10:ClearsB2()]))

# --- the container GROWS, which moves its buffer --------------------------

G = list(range(4))


class GrowsG:
    def __index__(self):
        G.extend(range(100))
        return 1


case("list grows, get", lambda: len(G[0:4:GrowsG()]))

G2 = list(range(4))


class GrowsG2:
    def __index__(self):
        G2.extend(range(100))
        return 1


case("list grows, set", lambda: G2.__setitem__(slice(0, 4, GrowsG2()), []))
print("G2 length:", len(G2))

G3 = bytearray(range(4))


class GrowsG3:
    def __index__(self):
        G3.extend(range(100))
        return 1


case("bytearray grows", lambda: bytes(G3[0:4:GrowsG3()]))

# --- only ONE of the three bounds is the mutating one ----------------------

for which in ("start", "stop", "step"):
    seq = list(range(20))

    class Clears:
        def __index__(self):
            seq[:] = []
            return 2

    bounds = {"start": [Clears(), 20, 2],
              "stop": [0, Clears(), 2],
              "step": [0, 20, Clears()]}[which]
    case("clear via " + which, lambda: seq[slice(*bounds)])

# --- and the immutable sequences, where the bound still runs ---------------

print()
T = tuple(range(10))
S = "abcdefghij"
BY = bytes(range(10))


class Two:
    def __index__(self):
        return 2


case("tuple", lambda: T[0:10:Two()])
case("str", lambda: S[0:10:Two()])
case("bytes", lambda: BY[0:10:Two()])
case("range", lambda: list(range(10)[0:10:Two()]))

# --- a bound that raises leaves the container alone ------------------------

print()
R = list(range(5))


class Boom:
    def __index__(self):
        R.append(99)
        raise ZeroDivisionError("from a bound")


case("raising bound", lambda: R[0:5:Boom()])
print("R after:", R)

print("done")
