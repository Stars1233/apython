# Argument validation on the codec entry points, and on the bytes-like
# constructors.  These used to accept whatever they were given: a non-str
# encoding silently fell back to utf-8, and every bad item in an iterable was
# reported as a ValueError.
import sys


def show(label, fn):
    try:
        print(label, "->", fn())
    except Exception as e:
        print(label, "->", type(e).__name__)


show("decode(123)", lambda: b"x".decode(123))
show("decode(utf-8, strict)", lambda: b"x".decode("utf-8", "strict"))
show("decode(3 args)", lambda: b"x".decode("utf-8", "strict", 1))
show("encode(123)", lambda: "x".encode(123))
show("encode(utf-8, strict)", lambda: "x".encode("utf-8", "strict"))
show("encode(3 args)", lambda: "x".encode("utf-8", "strict", 1))
show("intern(2 args)", lambda: sys.intern("a", "b"))
show("intern(int)", lambda: sys.intern(5))
show("intern(str)", lambda: sys.intern("ab"))

# A non-integer item is a TypeError; an out-of-range integer a ValueError.
# bytes() and bytearray() word the range message differently.
for label, fn in (("bytes([1.5])", lambda: bytes([1.5])),
                  ("bytes(['a'])", lambda: bytes(["a"])),
                  ("bytes([None])", lambda: bytes([None])),
                  ("bytes([256])", lambda: bytes([256])),
                  ("bytes([-1])", lambda: bytes([-1])),
                  ("bytearray([256])", lambda: bytearray([256])),
                  ("bytearray([1.5])", lambda: bytearray([1.5]))):
    try:
        print(label, "->", fn())
    except Exception as e:
        print(label, "->", type(e).__name__ + ":", e)

# The error paths must not leak; this loop would grow without bound.
for i in range(20000):
    try:
        bytes([256])
    except ValueError:
        pass
    try:
        bytes([1.5])
    except TypeError:
        pass
print("no leak")
