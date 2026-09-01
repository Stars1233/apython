"""Three things the io stack answered with the wrong shape.

open(path, "rb", buffering=0) hands back the raw file itself, with nothing
above it -- so everything CPython puts on _IOBase and everything inherits
has to be there, or an unbuffered file has no readline and cannot be
iterated.  Same for BytesIO, which is not built on the buffered layer
either.

detach() sets the underlying stream to None, and every forwarder then went
through None: AttributeError, where CPython raises ValueError and where code
that catches one and not the other is looking for the second.
"""

import io
import posix

TMP = "/tmp/apython_io_tail"


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(34), repr(got))


with open(TMP, "wb") as f:
    f.write(b"alpha\nbeta\ngamma\n")

# --- an unbuffered raw file has the whole IOBase surface ---
u = open(TMP, "rb", buffering=0)
check("type", lambda: type(u).__name__)
check("readline", lambda: u.readline())
check("readline again", lambda: u.readline())
check("readlines", lambda: (u.seek(0), u.readlines()))
check("iteration", lambda: (u.seek(0), [line for line in u]))
check("readlines with a hint", lambda: (u.seek(0), u.readlines(3)))
check("isatty", lambda: u.isatty())
check("flush", lambda: u.flush())
check("fileno is real", lambda: type(u.fileno()).__name__)
u.close()
check("readline when closed", lambda: u.readline())

w = open(TMP + "2", "wb", buffering=0)
check("writelines", lambda: (w.writelines([b"a\n", b"b\n"]), w.close(),
                             open(TMP + "2", "rb").read()))

# --- BytesIO has it too ---
b = io.BytesIO(b"one\ntwo\n")
check("BytesIO readlines", lambda: b.readlines())
check("BytesIO iteration", lambda: (b.seek(0), list(b)))
check("BytesIO fileno", lambda: b.fileno())
check("BytesIO isatty", lambda: b.isatty())
bw = io.BytesIO()
check("BytesIO writelines", lambda: (bw.writelines([b"x", b"y"]),
                                     bw.getvalue()))

# --- detach ---
f = open(TMP, "rb")
raw = f.detach()
check("closed after detach", lambda: f.closed)
check("read after detach", lambda: f.read())
check("tell after detach", lambda: f.tell())
check("name after detach", lambda: f.name)
check("fileno after detach", lambda: f.fileno())
check("detach twice", lambda: f.detach())
check("the raw file still works", lambda: raw.read(5))
raw.close()

t = open(TMP)
inner = t.detach()
check("text closed after detach", lambda: t.closed)
check("text read after detach", lambda: t.read())
check("the buffer still works", lambda: inner.read(5))
inner.close()

for name in (TMP, TMP + "2"):
    try:
        posix.unlink(name)
    except OSError:
        pass
print("cleaned")
