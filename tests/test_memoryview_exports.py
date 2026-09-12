# Every view that shares a buffer takes an export, and releases exactly one.
#
# memoryview_dealloc_proper releases three counts -- bytearray's own,
# BytesIO's own, and tp_as_buffer's BUF_ACQUIRE -- and the three DERIVED
# constructors took only the first two: a slice, a cast, and a view of a view.
# So the slot's count went one BELOW what was outstanding and the exporter
# allowed a resize under a live view.  `array.array` is the exporter that
# shows it, and lib/_io.py's readinto does `b = b.cast("B")`.
#
# release() and __exit__ had the opposite half of the same gap: they released
# the first two and not the slot, so an array stayed pinned for good.
import array


def pinned(make, then_release):
    b = array.array("i", range(64))
    mb = memoryview(b)
    d = make(mb)
    if then_release:
        d.release()
    del d
    try:
        for _ in range(4000):
            b.append(1)
        return "resize allowed"
    except BufferError:
        return "BufferError"


for label, make in (("slice", lambda m: m[0:4]),
                    ("cast", lambda m: m.cast("B")),
                    ("view-of-view", lambda m: memoryview(m)),
                    ("slice of a cast", lambda m: m.cast("B")[0:4])):
    print(label, pinned(make, False), "|", pinned(make, True))

# The parent still pins while it is alive, and stops when it is released.
a = array.array("i", [1, 2, 3])
m = memoryview(a)
try:
    a.append(4)
    print("live parent: allowed")
except BufferError:
    print("live parent: BufferError")
m.release()
m.release()
a.append(4)
print("after release:", a.tolist())

# A slice outliving its parent keeps the pin.
a2 = array.array("i", [1, 2])
m2 = memoryview(a2)
s2 = m2[0:1]
m2.release()
try:
    a2.append(9)
    print("slice pin: no")
except BufferError:
    print("slice pin: yes")
s2.release()
a2.append(9)
print("after both:", a2.tolist())

# with-statement form.
a3 = array.array("i", [1, 2])
with memoryview(a3) as m3:
    print("inside", bytes(m3)[:4] == a3.tobytes()[:4])
a3.append(7)
print("after with:", a3.tolist())

# bytearray and BytesIO keep their own counts, and those still work.
ba = bytearray(b"abcd")
mb = memoryview(ba)
sl = mb[1:3]
try:
    ba.append(1)
    print("bytearray: allowed")
except BufferError:
    print("bytearray: BufferError")
del sl, mb
ba.append(1)
print("bytearray after:", bytes(ba))

import io

bio = io.BytesIO(b"xyz")
bv = bio.getbuffer()
c = bv.cast("B")
del c, bv
bio.write(b"q")
print("BytesIO after:", bio.getvalue())
