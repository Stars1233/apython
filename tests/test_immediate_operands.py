# A sweep that feeds every immediate kind -- int, float, bool, None -- to
# every operation that takes an object argument.  Each of these dereferenced
# the operand's payload without first establishing that it was a pointer;
# an int's payload is the integer, and a float's is raw IEEE bits.


def t(f, *a):
    try:
        return repr(f(*a))
    except Exception as e:
        return type(e).__name__


IMM = (5, 1.5, True, None)

# Unary operators: - and ~ read tp_as_number without a NULL test, and ~ had
# no float case at all.  + passed every object through unchanged.
class C:
    pass


for v in (5, -5, 1.5, True, False, None, "s", [1], (1,), {1: 2}, 2 ** 60, C()):
    # ~ on a bool is deprecated in CPython and prints a warning, so it is
    # covered by the int cases instead.
    inv = "skipped" if isinstance(v, bool) else t(lambda: ~v)
    print(type(v).__name__, t(lambda: -v), inv, t(lambda: +v))

# except with a non-class target
def catch(target):
    try:
        raise ValueError("v")
    except target:
        return "caught"


print([t(catch, v) for v in IMM + ("s",)])
print(catch(ValueError), catch(Exception), catch((TypeError, ValueError)))


class MyErr(Exception):
    pass


class Sub(MyErr):
    pass


def catch_sub(target):
    try:
        raise Sub("s")
    except target:
        return "caught"


print(catch_sub(MyErr), catch_sub((KeyError, MyErr)), catch_sub(Exception))

# bytes containment: an int searches for a byte, bytes for a subsequence
print(97 in b"ab", 99 in b"ab", 0 in b"\x00")
print(b"a" in b"xaby", b"ab" in b"xaby", b"zz" in b"xaby", b"" in b"ab")
print([t(lambda v=v: v in b"ab") for v in (1.5, None, "a", 256, -1, True)])

# range() arguments, and a zero step
print(list(range(3)), list(range(1, 4)), list(range(0, 6, 2)), list(range(5, 0, -2)))
print(list(range(True)), len(range(2 ** 60, 2 ** 60 + 2)))
print([t(range, v) for v in IMM[1:] + ("s",)])
print(t(range, 0, 5, 0), t(range, 0, 1.5))

# slice components must be integers or None, and the step must not be zero
for s in ([10, 11, 12], (1, 2, 3), "abc", b"abc"):
    print(type(s).__name__, t(lambda: s[::0]), t(lambda: s[2:0:0]),
          t(lambda: s[::1.5]), t(lambda: s[1.5:]), repr(s[::None]))
print([10, 11, 12][::-1], "abc"[::2], (1, 2, 3)[1:], b"abc"[::-1], [1, 2, 3][::True])

# slice attributes: a missing one is a NULL return, not a raise, so hasattr
# and getattr-with-default work
sl = slice(1, 2, 3)
print(sl.start, sl.stop, sl.step)
print(hasattr(sl, "start"), hasattr(sl, "nope"), getattr(sl, "nope", "dflt"))
print(t(lambda: sl.nope))

# divmod dispatches on the operand's numeric protocol, not on int
print(divmod(7, 2), divmod(1.5, 1.5), divmod(7.5, 2), divmod(-7, 2))
print([t(divmod, v, v) for v in (None, "s")])

# file.write takes a str
import sys

print([t(sys.stdout.write, v) for v in IMM])
