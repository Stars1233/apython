# A size argument is converted before the stream moves.
#
# Every size here is a C Py_ssize_t on CPython's side, so the conversion
# happens BEFORE the method touches anything.  lib/_io.py validated late, and
# `BufferedReader.read(0.0)` walked far enough in to write a FLOAT into
# _read_pos before the slice under it raised.  The TypeError the caller
# expected did arrive -- and every later read on that object then failed too,
# from a line nowhere near the mistake.
#
# CPython's test_bz2 does exactly that and then reads the file:
# `self.assertRaises(TypeError, bz2f.read, float())`, one line above
# `self.assertEqual(bz2f.read(), self.TEXT)`.
#
# CPython has two wordings and they are not interchangeable: a size that may
# be None says "argument should be integer or None, not 'float'", and one that
# may not says "'float' object cannot be interpreted as an integer".
import io

# --- the size family ---------------------------------------------------------

RAW = b"abcdefghijklmnopqrstuvwxyz"


def reader():
    return io.BufferedReader(io.BytesIO(RAW))


for name in ("read", "read1", "peek", "readline", "readlines"):
    b = reader()
    for bad in (1.5, "3", None if name in ("read", "readline") else 0.0,
                [1], object()):
        if bad is None:
            continue            # None is a legal size for those two
        try:
            getattr(b, name)(bad)
            print("%s accepted %r" % (name, bad))
        except TypeError as e:
            print("%-10s %-10s %s" % (name, type(bad).__name__, e))
    # The stream is untouched by every one of those refusals.
    print("%-10s intact: %r" % (name, b.read()))

print()
# None is accepted where CPython accepts it, and refused where it does not.
b = reader()
print("read(None):", b.read(None))
b = reader()
print("readline(None):", b.readline(None))
b = reader()
print("readlines(None):", b.readlines(None))
b = reader()
try:
    b.peek(None)
except TypeError as e:
    print("peek(None):", e)
b = reader()
try:
    b.read1(None)
except TypeError as e:
    print("read1(None):", e)

print()
# An object with __index__ is accepted wherever an int is.


class Five:
    def __index__(self):
        return 5


b = reader()
print("read(Five()):", b.read(Five()))
b = reader()
print("peek(Five()) starts:", b.peek(Five())[:5])
b = reader()
print("read1(Five()):", b.read1(Five()))
b = reader()
print("readline(Five()):", b.readline(Five()))


class Bad:
    def __index__(self):
        return "five"


b = reader()
try:
    b.read(Bad())
except TypeError as e:
    print("bad __index__:", e)
print("intact:", b.read())

print()
# The text layer takes the same arguments.
def text():
    return io.TextIOWrapper(io.BytesIO(RAW))


t = text()
try:
    t.read(1.5)
except TypeError as e:
    print("text read:", e)
print("text intact:", t.read())
t = text()
try:
    t.readline(1.5)
except TypeError as e:
    print("text readline:", e)
print("text intact:", t.readline())
t = text()
print("text read(None):", t.read(None))

# A BufferedRandom and a BufferedRWPair route through the same checks.
print()
rw = io.BufferedRandom(io.BytesIO(bytearray(RAW)))
try:
    rw.read(1.5)
except TypeError as e:
    print("random read:", e)
print("random intact:", rw.read())

pair = io.BufferedRWPair(io.BytesIO(RAW), io.BytesIO())
try:
    pair.read(1.5)
except TypeError as e:
    print("pair read:", e)
print("pair intact:", pair.read())

print("done")
