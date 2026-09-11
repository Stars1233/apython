# os.write takes anything bytes-LIKE, which is what CPython's Py_buffer means.
#
# It took bytes and bytearray only, so a MEMORYVIEW was refused -- and
# subprocess's _communicate writes one: `os.write(key.fd, chunk)` where chunk
# is a slice of memoryview(input).  Every communicate() with input died there,
# and with the pipe still open the child never saw EOF and nobody exited.

import os

r, w = os.pipe()
for label, obj in (("bytes", b"abc"),
                   ("slice", b"abcdef"[1:3]),
                   ("empty", b""),
                   ("bytearray", bytearray(b"xy")),
                   ("bytearray slice", bytearray(b"xyz")[1:]),
                   ("memoryview", memoryview(b"mv")),
                   ("memoryview slice", memoryview(b"mvslice")[1:3]),
                   ("memoryview of bytearray", memoryview(bytearray(b"ba")))):
    n = os.write(w, obj)
    print(label, "->", n)
os.close(w)
print(os.read(r, 1024))
os.close(r)

# A str and a number are still refused.
r, w = os.pipe()
for bad in ("s", 5, None, [1, 2], 1.5):
    try:
        os.write(w, bad)
        print("accepted", type(bad).__name__)
    except TypeError as e:
        print(type(bad).__name__, "refused")
os.close(w)
os.close(r)

# And the shape subprocess uses.
r, w = os.pipe()
data = b"banana" * 100
view = memoryview(data)
off = 0
while off < len(view):
    off += os.write(w, view[off:off + 64])
os.close(w)
got = b""
while True:
    chunk = os.read(r, 4096)
    if not chunk:
        break
    got += chunk
os.close(r)
print(got == data, len(got))

print("done")
