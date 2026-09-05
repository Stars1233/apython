# A builtin method called with too few arguments must raise TypeError, not
# read past the end of the argument array.
#
# str.center() with no width segfaulted: the handler took args[1] without
# looking at nargs, and args[1] was whatever sat past the array.  The four
# padding methods were the crash; the rest of this file is the wider question
# the crash came out of, which is whether a builtin registered with no
# argument counts refuses anything at all.


def call(f, *args):
    """What happened: the exception's class name, or 'ran'."""
    try:
        f(*args)
    except TypeError:
        return "TypeError"
    except Exception as e:
        return type(e).__name__
    return "ran"


print("-- too few arguments")
for name in ("center", "ljust", "rjust", "zfill"):
    print("str.%-8s" % name, call(getattr("x", name)))
for name in ("center", "ljust", "rjust", "zfill"):
    print("bytes.%-8s" % name, call(getattr(b"x", name)))
for name in ("count", "index", "find", "startswith", "endswith", "split"):
    print("str.%-11s" % name, call(getattr("x", name)))
print("list.index    ", call([1].index))
print("dict.get      ", call({}.get))
print("bytes.join    ", call(b"".join))

print()
print("-- too many arguments")
print("str.upper     ", call("x".upper, 1))
print("str.lower     ", call("x".lower, 1))
print("str.strip     ", call("x".strip, "a", "b"))
print("list.reverse  ", call([1].reverse, 1))
print("dict.keys     ", call({}.keys, 1))
print("int.bit_length", call((7).bit_length, 1))
print("float.is_integer", call((1.5).is_integer, 1))
print("set.copy      ", call({1}.copy, 1))
print("tuple.count   ", call((1,).count, 1, 2))

print()
print("-- the right number still works")
print("center", "x".center(5))
print("ljust", "x".ljust(3) + "|")
print("rjust", "x".rjust(3) + "|")
print("zfill", "1".zfill(3))
print("center fill", "x".center(5, "-"))
print("upper", "x".upper())
print("strip", "  x ".strip())
print("count", "aab".count("a"))
