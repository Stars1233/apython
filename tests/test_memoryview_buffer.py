# A memoryview over an arbitrary exporter carries its format, not 'B'.
#
# memoryview has its own arm for bytes, bytearray and itself, and reaches
# anything else through tp_as_buffer.  That slot answered only a pointer and
# a byte count, so the view built over it was hardcoded to one-byte items,
# format 'B' and read-only:
#
#     memoryview(array('i', [1, 2, 3])).itemsize   was 1, should be 4
#     ...                              .format     was 'B', should be 'i'
#     ...                              .tolist()   was the twelve bytes
#     len(...)                                     was 12, should be 3
#
# The readonly answer was deliberate and stated -- "a buffer reached this way
# has told us where its bytes are and nothing about whether they may move" --
# but an exporter that keeps an export count, which array does, can say it is
# safe.  So the slot gains a fourth mode that answers the three things a view
# needs, and the defaults stay for an exporter that declines to.
from array import array

for code, vals in (("i", [1, 2, 3]), ("d", [1.5, 2.5]), ("h", [7]),
                   ("B", [1, 2, 3, 4]), ("q", [1 << 40]), ("f", [0.5])):
    a = array(code, vals)
    m = memoryview(a)
    print("%s itemsize=%d format=%s len=%d nbytes=%d readonly=%s %s"
          % (code, m.itemsize, m.format, len(m), m.nbytes, m.readonly,
             m.tolist()))

# Writing through the view reaches the array.
a = array("i", [1, 2, 3])
m = memoryview(a)
m[1] = 99
print("written through:", a.tolist())
m[0:2] = array("i", [7, 8])
print("slice written:", a.tolist())

# An export outstanding still blocks a resize, which is what the count is for.
try:
    a.append(4)
except BufferError as e:
    print("resize refused:", e)
del m
a.append(4)
print("resize after release:", a.tolist())

# An empty array exports an empty view of the right shape.
e = memoryview(array("i"))
print("empty:", e.itemsize, e.format, len(e), e.nbytes, e.tolist())

# cast() over such a view, which needs the itemsize to be right to start.
a2 = array("i", [1, 2])
m2 = memoryview(a2)
print("cast to B:", m2.cast("B").tolist())
print("cast back:", m2.cast("B").cast("i").tolist())

# bytes and bytearray keep their own arms, and must not have moved.
mb = memoryview(b"abc")
print("bytes:", mb.itemsize, mb.format, len(mb), mb.readonly)
ba = bytearray(b"abc")
mba = memoryview(ba)
print("bytearray:", mba.itemsize, mba.format, len(mba), mba.readonly)
mba[0] = 65
print("bytearray written:", ba)
print("survived")
