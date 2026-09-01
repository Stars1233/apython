"""_io.BytesIO: a file whose storage is memory.

The contract is a file's, so the interesting cases are the ones where memory
and a real file could plausibly differ and do not: seeking past the end and
then writing zero-fills the gap; truncate only ever shrinks, unlike a file's;
getbuffer() hands out a view over the storage itself, so nothing may resize
it while that view is alive; and close() frees the storage, which is what
makes getvalue() afterwards an error rather than an empty answer.
"""

import _io


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(32), repr(got))


# --- reading ---
b = _io.BytesIO(b"hello world\nsecond line\nthird\n")
check("read(5)", lambda: b.read(5))
check("tell", lambda: b.tell())
check("read(0)", lambda: b.read(0))
check("readline", lambda: b.readline())
check("readline capped", lambda: b.readline(3))
check("readline rest", lambda: b.readline())
check("read to the end", lambda: b.read())
check("read at eof", lambda: b.read(5))
check("readline at eof", lambda: b.readline())
check("seek back", lambda: b.seek(0))
check("read1", lambda: b.read1(5))
check("read(None)", lambda: b.read(None))
check("read(-1)", lambda: (b.seek(6), b.read(-1)))
check("getvalue", lambda: b.getvalue()[:11])
check("predicates", lambda: (b.readable(), b.writable(), b.seekable()))

# --- iteration, which is how a file is usually read ---
check("iterating", lambda: list(_io.BytesIO(b"a\nb\nc")))
check("iterating empty", lambda: list(_io.BytesIO()))
check("iterating no newline", lambda: list(_io.BytesIO(b"abc")))
check("is its own iterator", lambda: (lambda s: s is iter(s))(_io.BytesIO()))
check("iter then read", lambda: (lambda s: (next(iter(s)), s.read()))(
    _io.BytesIO(b"a\nb\n")))

# --- seeking ---
s = _io.BytesIO(b"0123456789")
check("absolute", lambda: (s.seek(3), s.read(2)))
check("relative", lambda: (s.seek(2, 1), s.read(2)))
check("from the end", lambda: (s.seek(-3, 2), s.read()))
check("past the end", lambda: (s.seek(100), s.read()))
check("tell past the end", lambda: s.tell())
check("negative absolute", lambda: s.seek(-1))
check("negative computed", lambda: (s.seek(0), s.seek(-5, 1)))
check("a bad whence", lambda: s.seek(0, 9))

# --- writing ---
w = _io.BytesIO()
check("empty getvalue", lambda: w.getvalue())
check("write", lambda: (w.write(b"abc"), w.tell(), w.getvalue()))
check("overwrite", lambda: (w.seek(1), w.write(b"XY"), w.getvalue()))
check("append at the end", lambda: (w.seek(0, 2), w.write(b"!"), w.getvalue()))
check("gap is zero-filled", lambda: (w.seek(8), w.write(b"z"), w.getvalue()))
check("write bytearray", lambda: (w.seek(0), w.write(bytearray(b"AB")),
                                  w.getvalue()))
check("write a view", lambda: (w.seek(0), w.write(memoryview(b"CD")),
                               w.getvalue()))
check("write a str", lambda: w.write("nope"))
check("write empty", lambda: (w.write(b""), w.getvalue()))

# --- truncate ---
t = _io.BytesIO(b"0123456789")
check("truncate shrinks", lambda: (t.truncate(4), t.getvalue()))
check("truncate does not grow", lambda: (t.truncate(20), t.getvalue()))
check("truncate at the cursor", lambda: (t.seek(2), t.truncate(), t.getvalue()))
check("position survives", lambda: t.tell())
check("truncate negative", lambda: t.truncate(-1))

# --- readinto ---
r = _io.BytesIO(b"abcdefgh")
buf = bytearray(3)
check("readinto", lambda: (r.readinto(buf), bytes(buf)))
check("readinto again", lambda: (r.readinto(buf), bytes(buf)))
check("readinto a view", lambda: (r.seek(0), r.readinto(memoryview(buf)),
                                  bytes(buf)))
check("readinto at eof", lambda: (r.seek(0, 2), r.readinto(buf)))
check("readinto bytes", lambda: r.readinto(b"abc"))
check("readinto oversized", lambda: (r.seek(6), r.readinto(bytearray(10))))

# --- getbuffer shares the storage ---
g = _io.BytesIO(b"abcd")
view = g.getbuffer()
check("view contents", lambda: bytes(view))
check("write while exported", lambda: g.write(b"x"))
check("truncate while exported", lambda: g.truncate(2))
check("close while exported", lambda: g.close())
check("write through the view", lambda: (view.__setitem__(0, 90),
                                         g.getvalue()))
view.release()
check("write after release", lambda: (g.write(b"!"), g.getvalue()))

# --- close ---
c = _io.BytesIO(b"data")
check("closed before", lambda: c.closed)
c.close()
check("closed after", lambda: c.closed)
check("close is idempotent", lambda: c.close())
check("read after close", lambda: c.read())
check("write after close", lambda: c.write(b"x"))
check("getvalue after close", lambda: c.getvalue())
check("tell after close", lambda: c.tell())
check("seek after close", lambda: c.seek(0))

# --- the context manager ---
with _io.BytesIO(b"ctx") as cm:
    check("in a with", lambda: cm.read())
check("closed on exit", lambda: cm.closed)

# --- construction ---
check("from bytearray", lambda: _io.BytesIO(bytearray(b"ba")).getvalue())
check("from a memoryview", lambda: _io.BytesIO(memoryview(b"mv")).getvalue())
check("from None", lambda: _io.BytesIO(None).getvalue())
check("from a str", lambda: _io.BytesIO("nope"))
check("mro", lambda: [c.__name__ for c in _io.BytesIO.__mro__])
check("isinstance", lambda: isinstance(_io.BytesIO(),
                                       _io._BufferedIOBase))


# --- subclassing ---
class Mine(_io.BytesIO):
    def read(self, n=-1):
        return super().read(n)[::-1]


m = Mine(b"abcd")
check("subclass override", lambda: m.read())
check("subclass inherits", lambda: (m.seek(0), m.getvalue()))
m.tag = 1
check("subclass __dict__", lambda: sorted(m.__dict__))

# --- a lot of small writes, which is what the growth policy is for ---
big = _io.BytesIO()
for i in range(2000):
    big.write(b"0123456789")
check("many writes", lambda: (len(big.getvalue()), big.getvalue()[:10],
                              big.getvalue()[-10:]))
one = _io.BytesIO()
check("one big write", lambda: (one.write(b"x" * 100000), len(one.getvalue())))
