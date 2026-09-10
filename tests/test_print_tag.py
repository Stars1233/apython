# A tp_repr or tp_str answers with a TAG beside the pointer, and builtin_print
# reads a zero tag as "this argument is not there" and skips it.  So a slot
# that forgets to set the tag does not crash and does not print garbage -- it
# prints NOTHING, for that one argument, leaving the separator behind.
#
# range_obj_repr forgot.  What it left in the tag register was whatever
# ap_free had put there, which for libc free was reliably non-zero; so
# `print(range(20)[3:15:4], len(s))` printed " 3" for as long as the
# allocator underneath happened to cooperate.
#
# Every value below is printed BESIDE another, because that is the path that
# reads the tag -- and every repr here is address-free, so the output is
# stable enough to diff.
import array

vals = [
    0, 1, -1, 2 ** 70, -(2 ** 70), 1.5, -0.0, float("inf"),
    True, False, None, Ellipsis, NotImplemented,
    "s", "", b"b", b"", bytearray(b"c"), bytearray(),
    (), (1,), (1, 2), [], [1], [1, 2], {1}, frozenset({1}), {}, {1: 2},
    range(5), range(0), range(10, 0, -1), range(20)[3:15:4], range(20)[::2][1:4],
    slice(1, 2), slice(None), slice(1, 10, 2),
    complex(1, 2), array.array("i", [1, 2]),
    int, str, list, dict, type, object, Exception,
    Exception("e"), ValueError("v"), KeyError("k"), StopIteration(),
    {1: 2}.keys(), {1: 2}.values(), {1: 2}.items(),
]

for i, v in enumerate(vals):
    print(i, v, "|", type(v).__name__)

# and the shape that found it: a value printed first, with more after it
s = range(20)[3:15:4]
print(s, len(s), list(s), 7 in s, 8 in s)
print(range(5), range(5)[::-1], list(range(5)[::-1]))
print(slice(1, 2), slice(1, 2).start, slice(1, 2).stop)

# a wide range, whose repr goes through the bignum path
w = range(2 ** 70, 2 ** 70 + 30, 7)
print(w, len(w), list(w) == [2 ** 70 + i * 7 for i in range(5)])
print(w[1:3], list(w[1:3]))

# every one of them again through str() and format(), which do not read a tag
# but would catch a repr that is wrong rather than absent
print(all(str(v) != "" for v in vals if v != ""))
print(all(("%s" % (v,)) == str(v) for v in vals))
print(all(f"{v}" == str(v) for v in vals))
