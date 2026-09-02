"""Five wrong answers, none of which crashed.

bool() of a heap-boxed integer, an errors= handler that was accepted and
ignored, truncate(-1) read as "no size given", the legacy __getitem__
protocol inside an except block, and divmod() with a float on one side.
They have nothing in common except that each one produced a plausible answer.
"""

import _io
import posix


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(34), repr(got))


# --- bool() of a value that is not an immediate ---
# nb_bool takes (payload, tag) and obj_is_true passed only the payload, so
# int_bool read whatever the caller had left in the tag register.  When that
# was TAG_SMALLINT it tested the POINTER, which is never zero.
big_zero = (1 << 70) - (1 << 70)
check("a boxed zero", lambda: (big_zero, type(big_zero).__name__))
check("bool of it", lambda: bool(big_zero))
check("not of it", lambda: not big_zero)
check("if of it", lambda: "T" if big_zero else "F")
check("any and all", lambda: (any([big_zero]), all([big_zero])))
check("filter", lambda: list(filter(None, [big_zero, 1])))
check("bool of a boxed nonzero", lambda: bool((1 << 70) + 1))
check("a boxed remainder", lambda: bool(divmod(1 << 70, 1 << 70)[1]))
check("bool of a float zero", lambda: (bool(0.0), bool(-0.0), bool(1.5)))


# --- errors= on decode ---
# bytes.decode never validated UTF-8 at all, so "strict" never raised and
# "ignore" had nothing to ignore.
BAD = b"abc\xff\xfedef"
check("strict raises", lambda: BAD.decode("utf-8"))
check("ignore drops", lambda: BAD.decode("utf-8", "ignore"))
check("replace substitutes", lambda: BAD.decode("utf-8", "replace"))
check("a valid string", lambda: b"h\xc3\xa9llo".decode("utf-8"))
check("an unknown handler", lambda: BAD.decode("utf-8", "nosuch"))
for raw in (b"\xc3", b"\xc3\x28", b"\xe2\x82", b"\xe2\x82\xac", b"\xed\xa0\x80",
            b"\xc0\x80", b"\xf5\x80\x80\x80", b"\x80", b"\xf0\x9f\x92\xa9"):
    check("strict %r" % raw, lambda raw=raw: raw.decode("utf-8"))
    check("replace %r" % raw, lambda raw=raw: raw.decode("utf-8", "replace"))
check("through a file", lambda: (lambda p: [
    open(p, "wb").write(BAD), open(p, encoding="utf-8").read()][1])(
        "/tmp/apython_wrong_answers"))
try:
    posix.unlink("/tmp/apython_wrong_answers")
except OSError:
    pass


# --- FileIO.truncate(-1) ---
TMP = "/tmp/apython_wrong_answers2"
f = _io.FileIO(TMP, "wb")
f.write(b"0123456789")
f.close()
g = _io.FileIO(TMP, "r+b")
g.seek(4)
check("truncate(-1)", lambda: g.truncate(-1))
check("the file is intact", lambda: len(_io.FileIO(TMP, "rb").readall()))
check("truncate() at the cursor", lambda: (g.truncate(), g.tell()))
check("now it is shorter", lambda: _io.FileIO(TMP, "rb").readall())
check("truncate(0)", lambda: (g.truncate(0), _io.FileIO(TMP, "rb").readall()))
g.close()
posix.unlink(TMP)


# --- the legacy __getitem__ protocol, inside an except block ---
# current_exception is also the exception BEING HANDLED, so testing it for
# non-NULL said "__iter__ raised" for an object that has no __iter__.
class Seq:
    def __getitem__(self, i):
        if i > 2:
            raise IndexError
        return i


check("outside a handler", lambda: (list(Seq()), set(Seq()), sum(Seq())))


def inside_handler():
    try:
        raise ValueError("boom")
    except ValueError:
        return list(Seq()), tuple(Seq()), max(Seq()), min(Seq())


check("inside a handler", inside_handler)


def nested_handler():
    try:
        raise KeyError("k")
    except KeyError:
        try:
            raise TypeError("t")
        except TypeError:
            return sorted(Seq())


check("two deep", nested_handler)


class Raises:
    def __iter__(self):
        raise RuntimeError("from __iter__")


def raising_inside_handler():
    try:
        raise ValueError("boom")
    except ValueError:
        return list(Raises())


check("a real raise still propagates", raising_inside_handler)


# --- divmod with a declining slot ---
for a, b in ((7, 2), (7, 2.0), (7.0, 2), (7.5, 2.5), (-7, 2), (7, -2),
             (2 ** 70, 3), (3, 2 ** 70), (1, 2j), (1, "x"), (7, 0)):
    check("divmod(%r, %r)" % (a, b), lambda a=a, b=b: divmod(a, b))
