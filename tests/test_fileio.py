"""_io.FileIO: the raw layer, one syscall per call and bytes only.

Everything above it -- BufferedReader, TextIOWrapper, open() -- is built out
of read, readinto, write and seek, so what this checks is the contract those
depend on: a short read is not EOF, an empty read is EOF, readinto writes
through the caller's buffer, close is idempotent, and every operation on a
closed file raises rather than reading a descriptor the process may have
handed to something else.
"""

import posix
import _io

TMP = "/tmp/apython_fileio_test"


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(32), repr(got))


def cleanup(*names):
    for n in names:
        try:
            posix.unlink(n)
        except OSError:
            pass


cleanup(TMP, TMP + "2")

# --- writing ---
f = _io.FileIO(TMP, "wb")
check("mode flags", lambda: (f.readable(), f.writable(), f.seekable()))
check("write", lambda: f.write(b"hello world\n"))
check("tell after write", lambda: f.tell())
check("fileno is an int", lambda: type(f.fileno()).__name__)
check("isatty", lambda: f.isatty())
check("read on a write file", lambda: f.read(1))
check("name and mode", lambda: (f.name, f.mode))
check("closed before", lambda: f.closed)
f.close()
check("closed after", lambda: f.closed)
check("close is idempotent", lambda: f.close())
check("read after close", lambda: f.read(1))
check("write after close", lambda: f.write(b"x"))
check("tell after close", lambda: f.tell())
check("fileno after close", lambda: f.fileno())

# --- reading ---
g = _io.FileIO(TMP, "rb")
check("mode flags", lambda: (g.readable(), g.writable(), g.seekable()))
check("read(5)", lambda: g.read(5))
check("tell", lambda: g.tell())
check("read(0)", lambda: g.read(0))
check("read past the end", lambda: g.read(100))
check("read at eof", lambda: g.read(10))
check("readall at eof", lambda: g.readall())
check("seek absolute", lambda: g.seek(0))
check("readall", lambda: g.readall())
check("seek relative", lambda: (g.seek(0), g.seek(6, 1), g.read()))
check("seek from the end", lambda: (g.seek(-6, 2), g.read()))
check("read(-1) is readall", lambda: (g.seek(0), g.read(-1)))
check("read(None)", lambda: (g.seek(6), g.read(None)))
check("write on a read file", lambda: g.write(b"x"))
g.close()

# --- readinto, which is why the layer above holds a memoryview ---
h = _io.FileIO(TMP, "rb")
buf = bytearray(5)
check("readinto", lambda: (h.readinto(buf), bytes(buf)))
check("readinto again", lambda: (h.readinto(buf), bytes(buf)))
check("readinto a view", lambda: (h.seek(0), h.readinto(memoryview(buf)),
                                  bytes(buf)))
check("readinto empty", lambda: h.readinto(bytearray()))
check("readinto at eof", lambda: (h.seek(0, 2), h.readinto(buf)))
check("readinto bytes", lambda: h.readinto(b"12345"))
check("readinto a str", lambda: h.readinto("12345"))
check("readinto a readonly view", lambda: h.readinto(memoryview(b"12345")))
h.close()

# --- append, exclusive, and update ---
a = _io.FileIO(TMP, "ab")
check("append writes at the end", lambda: (a.write(b"tail"), a.tell()))
a.close()
check("after append", lambda: _io.FileIO(TMP, "rb").readall())

check("exclusive on an existing file", lambda: _io.FileIO(TMP, "xb"))
x = _io.FileIO(TMP + "2", "xb")
check("exclusive creates", lambda: (x.write(b"new"), x.close()))
check("its contents", lambda: _io.FileIO(TMP + "2", "rb").readall())

u = _io.FileIO(TMP + "2", "r+b")
check("update flags", lambda: (u.readable(), u.writable()))
check("update in place", lambda: (u.seek(1), u.write(b"E"), u.seek(0),
                                  u.readall()))
u.close()

# --- truncate ---
t = _io.FileIO(TMP + "2", "r+b")
check("truncate to a size", lambda: (t.truncate(2), t.seek(0), t.readall()))
check("truncate grows", lambda: (t.truncate(5), t.seek(0), t.readall()))
check("truncate at the position", lambda: (t.seek(1), t.truncate(), t.seek(0),
                                           t.readall()))
t.close()

# --- errors, and the shapes that reach here from ordinary code ---
check("a missing file", lambda: _io.FileIO(TMP + "_nope", "rb"))
check("a directory", lambda: _io.FileIO("/tmp", "rb").readall())
check("a bad mode", lambda: _io.FileIO(TMP, "q"))
check("two modes", lambda: _io.FileIO(TMP, "rw"))
check("no mode at all", lambda: _io.FileIO(TMP, "b"))
check("a mode that is not a str", lambda: _io.FileIO(TMP, 5))
check("a name that is neither", lambda: _io.FileIO(None, "rb"))
check("a negative descriptor", lambda: _io.FileIO(-1, "rb"))
check("no arguments", lambda: _io.FileIO())

# --- an adopted descriptor ---
fd = posix.open(TMP, 0, 0o666)
d = _io.FileIO(fd, "rb", False)
check("adopted reads", lambda: d.read(5))
check("adopted name", lambda: d.name)
check("adopted repr", lambda: repr(d).startswith("<_io.FileIO name="))
d.close()
check("the fd survives closefd=False", lambda: posix.read(fd, 5))
posix.close(fd)

# --- the context manager ---
with _io.FileIO(TMP, "rb") as cm:
    check("in a with", lambda: cm.read(5))
check("closed on exit", lambda: cm.closed)


def raises_inside():
    with _io.FileIO(TMP, "rb") as c:
        raise ValueError("boom")


try:
    raises_inside()
except ValueError as e:
    print("exception propagates".ljust(32), repr(str(e)))

# --- subclassing, which is what io.py and _pyio do to it ---
class MyFile(_io.FileIO):
    def read(self, n=-1):
        return super().read(n)[::-1]


m = MyFile(TMP, "rb")
check("subclass overrides", lambda: m.read(5))
check("subclass inherits", lambda: (m.seek(0), m.readall()[:5]))
check("subclass isinstance", lambda: (isinstance(m, _io.FileIO),
                                      isinstance(m, _io._RawIOBase)))
m.attr = 1
check("subclass __dict__", lambda: sorted(m.__dict__))
m.close()

cleanup(TMP, TMP + "2")
print("cleaned")
