# Repeating a bytearray, and the two ways it can be asked for too much.
#
# In-place repetition had neither of the checks sq_repeat has, and
# bytearray_resize hands its size straight to an allocator that answers
# failure by ending the process: `b *= 2**40` printed "Fatal: out of memory"
# and took the interpreter with it, where every other repetition in the tree
# raises MemoryError.
#
# Which of the two errors is CPython's depends on what was too big.  A COUNT
# that will not fit an index is an OverflowError; a count that fits but whose
# product does not, or does but cannot be allocated, is a MemoryError.  This
# raised OverflowError for both.

print("=== in place ===")
for n in (0, 1, 2, 3, -1, -1000, 2 ** 40, 2 ** 62, 2 ** 63, 2 ** 64, 10 ** 30):
    b = bytearray(b"ab")
    try:
        b *= n
        print("%-32s %r %d" % (n, bytes(b)[:12], len(b)))
    except Exception as e:
        print("%-32s %s: %s" % (n, type(e).__name__, e))

print("=== out of place ===")
for n in (0, 1, 2, 3, -1, -1000, 2 ** 40, 2 ** 62, 2 ** 63, 2 ** 64, 10 ** 30):
    try:
        print("%-32s %r" % (n, bytes(bytearray(b"ab") * n)[:12]))
    except Exception as e:
        print("%-32s %s: %s" % (n, type(e).__name__, e))

# What is not a count at all.  The OPERATOR is what is compared here, not the
# dunder called by hand: CPython's __imul__ wrapper words it as "'str' object
# cannot be interpreted as an integer", where `b *= "3"` is "can't multiply
# sequence by non-int of type 'str'", and this implementation has one function
# for both.
print("=== and what is not a count at all ===")
for n in (None, "3", 1.5, [2]):
    b = bytearray(b"ab")
    try:
        b *= n
        print("%-12s %-8r %r" % ("in place", n, bytes(b)))
    except Exception as e:
        print("%-12s %-8r %s: %s" % ("in place", n, type(e).__name__, e))
    try:
        print("%-12s %-8r %r" % ("out of place", n, bytes(bytearray(b"ab") * n)))
    except Exception as e:
        print("%-12s %-8r %s: %s" % ("out of place", n, type(e).__name__, e))


class Index:
    def __index__(self):
        return 3


b = bytearray(b"ab")
b *= Index()
print("__index__", bytes(bytearray(b"ab") * Index()), bytes(b))
print("done")
