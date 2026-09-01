"""The regressions a second review found in the first review's fixes.

Every one of these was introduced by a commit that was itself fixing
something, which is the point of the file: a fix is not finished until the
shape it introduces has been looked for too.

  * DUNDER_RAISED asks "did current_exception change", and call_iternext
    CLEARS it to swallow a StopIteration -- so list(it)/tuple(it) inside an
    except block saw a change, called it a raise, and returned NULL.
  * bytes % latin-1 decoded its arguments but not the FORMAT, so the two
    halves of the round trip disagreed and the re-encode ran off the end of
    the allocation.
  * startswith/endswith went through bytes_like_ptr_len, which declines a
    tuple -- turning the documented `data.startswith((b'PK', b'\\x1f\\x8b'))`
    into a TypeError.
  * memoryview.cast() is a fourth place a view takes a share of another's
    source, and only three of them acquired a BytesIO export.
  * BytesIO.__init__ held nargs in a caller-saved register across ap_free.
  * format() of a wide int subclass boxed a value nothing released.
  * decode() reached for PyStrObject.data on a non-str errors=.
"""

import io
import posix
import _io


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(36), repr(got))


# --- list()/tuple() of a Python iterator, inside a handler ---
class Counter:
    def __init__(self):
        self.i = 0

    def __iter__(self):
        return self

    def __next__(self):
        self.i += 1
        if self.i > 3:
            raise StopIteration
        return self.i


def in_handler(fn):
    try:
        raise ValueError("being handled")
    except ValueError:
        return fn()


check("list outside", lambda: list(Counter()))
check("list inside", lambda: in_handler(lambda: list(Counter())))
check("tuple inside", lambda: in_handler(lambda: tuple(Counter())))
check("set inside", lambda: in_handler(lambda: sorted(set(Counter()))))
check("sorted inside", lambda: in_handler(lambda: sorted(Counter())))
check("nested handler", lambda: in_handler(lambda: in_handler(
    lambda: list(Counter()))))
check("the handler still works", lambda: in_handler(lambda: "reached"))


class RaisingNext:
    def __iter__(self):
        return self

    def __next__(self):
        raise KeyError("from __next__")


check("a real raise propagates", lambda: list(RaisingNext()))
check("and inside a handler too", lambda: in_handler(
    lambda: list(RaisingNext())))


# --- bytes % with a format that has high bytes ---
check("high byte format", lambda: b"\xc3\xa9%s" % (b"a",))
check("three-byte format", lambda: b"\xe4\xb8\xad%s" % (b"a",))
check("no directives", lambda: b"\x80\x81\x82" % ())
check("0xff and an arg", lambda: b"\xff%s" % (b"a",))
check("a long one", lambda: len((b"\xe4\xb8\xad" * 40) % ()))
check("a wide str argument", lambda: b"%s" % ("中" * 3,))
check("an ascii str argument", lambda: b"%r" % ("x",))
check("the ordinary cases", lambda: (b"%d" % 5, b"%s" % (b"x",), b"%%" % ()))


# --- startswith / endswith with a tuple ---
check("startswith a tuple", lambda: b"abc".startswith((b"x", b"a")))
check("startswith no match", lambda: b"abc".startswith((b"x", b"y")))
check("startswith empty tuple", lambda: b"abc".startswith(()))
check("endswith a tuple", lambda: b"abc".endswith((b"z", b"bc")))
check("a bytearray in the tuple", lambda: b"abc".startswith((bytearray(b"a"),)))
check("on a bytearray", lambda: bytearray(b"abc").startswith((b"z", b"ab")))
check("a str in the tuple", lambda: b"abc".endswith((b"z", "c")))
check("a bare str", lambda: b"abc".startswith("a"))
check("still takes one affix", lambda: (b"abc".startswith(b"a"),
                                        b"abc".endswith(b"c")))


# --- memoryview.cast() and the BytesIO export count ---
b = _io.BytesIO(b"abcdefgh")
whole = b.getbuffer()
cast = whole.cast("B")
del whole
check("write while a cast view lives", lambda: b.write(b"x"))
check("the cast view still reads", lambda: bytes(cast))
del cast
check("write once it is gone", lambda: (b.write(b"!"), b.getvalue()[:4]))


# --- __init__ called again ---
r = io.BytesIO(b"abcdef")
check("BytesIO re-init, no args", lambda: (r.__init__(), r.getvalue()))
check("BytesIO re-init, an arg", lambda: (r.__init__(b"xy"), r.getvalue()))
TMP = "/tmp/apython_review_regressions"
open(TMP, "wb").write(b"hello")
f = _io.FileIO(TMP, "rb")
check("FileIO re-init", lambda: (f.__init__(TMP, "rb"), f.read(5)))
f.close()


# --- format() of a wide int subclass ---
class I(int):
    pass


wide = 1125899906842623 * 2
check("format a wide subclass", lambda: format(I(wide), "d"))
check("f-string of one", lambda: f"{I(wide):d}")
check("a narrow one", lambda: format(I(7), "03d"))
check("a bool", lambda: format(True, "d"))
for _ in range(20000):
    _ = format(I(wide), "d")
print("formatted 20000".ljust(36), repr(True))


# --- decode() with a bad errors= ---
for e in (5, None, [], b"x"):
    check("decode errors=%r" % (e,), lambda e=e: b"\xff".decode("utf-8", e))
check("decode errors ok", lambda: b"\xff".decode("utf-8", "replace"))
check("an unknown handler", lambda: b"\xff".decode("utf-8", "nosuch"))


# --- a truncated sequence is one subpart, not one per byte ---
for raw in (b"ab\xf0\x9f\x92cd", b"\xe2\x82x", b"\xc3(", b"\xf0\x9f"):
    check("replace %r" % raw, lambda raw=raw: raw.decode("utf-8", "replace"))
    check("strict %r" % raw, lambda raw=raw: raw.decode("utf-8"))


# --- maketrans by code point ---
check("maketrans wide", lambda: sorted(str.maketrans("ab", "áâ").items()))
check("applied", lambda: "abc".translate(str.maketrans("ab", "áâ")))
check("wide keys", lambda: "áéx".translate(str.maketrans("áé", "ab")))
check("with deletions", lambda: "abc".translate(str.maketrans("a", "x", "c")))
check("an out-of-range ordinal", lambda: "abc".translate({97: 0x110000}))


# --- posix integers are range-checked, and a PathLike is released ---
class P:
    def __init__(self, s):
        self._s = s

    def __fspath__(self):
        return "".join(self._s)


check("close a huge fd", lambda: posix.close(2 ** 64 + 3))
check("close a negative fd", lambda: posix.close(-1))
check("chmod with a bad mode", lambda: posix.chmod(P(TMP), "no"))
check("access with a bad mode", lambda: posix.access(P(TMP), "no"))
check("open with bad flags", lambda: posix.open(P(TMP), "no"))
check("and they still work", lambda: (posix.access(P(TMP), 0),
                                      posix.stat(P(TMP)).st_size))

# --- a struct sequence built from a raising iterator ---
def raises_late():
    yield 1
    yield 2
    raise ValueError("late")


def raises_early():
    raise KeyError("early")
    yield 1


check("raising after the count", lambda: posix.terminal_size(raises_late()))
check("raising before it", lambda: posix.terminal_size(raises_early()))
check("a good one", lambda: posix.terminal_size((80, 24)))

posix.unlink(TMP)
print("cleaned")
