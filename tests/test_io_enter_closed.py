# __enter__ refuses a closed stream, and asks by NAME.
#
# CPython's _io._IOBase.__enter__ looks up `closed` as an attribute, so a
# subclass that reports it with a property of its own is answered correctly.
# Reading a flag out of the core object instead answers for the wrong half --
# and every stream in the compression stack is exactly that shape:
# bz2.BZ2File, gzip.GzipFile and lzma.LZMAFile all define `closed` themselves
# over a raw file they hold.  `with f:` on a closed one has to raise.
import io

b = io.BytesIO(b"x")
b.close()
try:
    b.__enter__()
except ValueError as e:
    print("BytesIO:", e)

f = io.BufferedReader(io.BytesIO(b"x"))
f.close()
try:
    with f:
        print("entered a closed reader")
except ValueError as e:
    print("BufferedReader:", e)

t = io.TextIOWrapper(io.BytesIO(b"x"))
t.close()
try:
    with t:
        print("entered a closed text stream")
except ValueError as e:
    print("TextIOWrapper:", e)

# The case that matters: a subclass whose `closed` is its OWN property.


class Wrapper(io.BufferedIOBase):
    def __init__(self):
        self._shut = False

    @property
    def closed(self):
        return self._shut

    def close(self):
        self._shut = True


w = Wrapper()
with w:
    print("open wrapper entered")
w2 = Wrapper()
w2.close()
try:
    with w2:
        print("entered a closed wrapper")
except ValueError as e:
    print("Wrapper:", e)

# __enter__ answers the object itself, so `with f as g` binds f.
w3 = Wrapper()
with w3 as bound:
    print("binds itself:", bound is w3)

# A stream that reports `closed` from a plain attribute rather than a property
# is treated the same, because the lookup is by name either way.


class Attr(io.RawIOBase):
    closed = False


a = Attr()
print("attribute False enters:", a.__enter__() is a)
a.closed = True
try:
    a.__enter__()
except ValueError as e:
    print("attribute True:", e)


# An object with no `closed` of its own inherits the core's, which is False
# until close() runs.


class Plain(io.BufferedIOBase):
    pass


p = Plain()
print("plain enters:", p.__enter__() is p)
p.close()
try:
    p.__enter__()
except ValueError as e:
    print("plain closed:", e)

# __exit__ closes by NAME, so an override runs.

closed_by = []


class Noted(io.BufferedIOBase):
    def close(self):
        closed_by.append("override")


with Noted():
    pass
print("exit called close:", closed_by)

print("done")
