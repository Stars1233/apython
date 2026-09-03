# An int subclass wherever an index is wanted.
#
# An instance of `class N(int)` is a wrapper around an int rather than an int
# with room on the end, so reading its value means unwrapping it first.
# obj_as_index -- the one place a subscript, a repetition count, a slice bound
# and chr() all converge -- did not, and read the wrapper's own header as the
# number.  Every one of these answered as though the index were 0.


class N(int):
    pass


class E(int):
    def __new__(cls, v):
        return int.__new__(cls, v)


SEQ = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
TUP = tuple(SEQ)
TXT = "abcdefghij"

for cls in (int, N, E, bool):
    for n in (0, 1, 7):
        if cls is bool and n > 1:
            continue
        v = cls(n)
        print(cls.__name__, n, SEQ[v], TUP[v], TXT[v], bytes(TXT, "ascii")[v])

n = N(3)
print("slice list", SEQ[n:], SEQ[:n], SEQ[n:n + 2])
print("slice str", TXT[n:], TXT[:n])
print("repeat", "ab" * n, [0] * n, (1,) * n)
print("range", list(range(n)), list(range(n, N(6))), list(range(0, 6, n)))
print("chr", chr(N(65)), chr(N(0x2603)))
print("hex/oct/bin", hex(n), oct(n), bin(n))
print("round", round(N(12345), N(-2)))
print("bytes", bytes(N(3)), bytearray(N(2)))
print("to_bytes", N(258).to_bytes(N(2), "big"))
print("index", SEQ.index(N(5)), SEQ.count(N(5)))
print("insert", (lambda s: (s.insert(N(1), 99), s)[1])(list(SEQ[:3])))
print("getitem dunder", SEQ.__getitem__(N(4)))
print("operator.index", __import__("operator").index(N(9)))

# The same wrapper reaching a builtin that takes a descriptor.
import posix
fd = posix.open("/dev/null", 0)
try:
    print("dup ok", posix.dup(N(fd)) > 0)
finally:
    posix.close(fd)

# A big one, past the immediate range, and a negative one.
BIG = [0] * 3
print("negative", SEQ[N(-1)], SEQ[N(-3):])
print("bool as index", SEQ[True], SEQ[False], "ab" * True)
