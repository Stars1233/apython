# center, ljust and rjust: the fill, and how wide the field may be.
#
# The fill character was read as a PyStrObject whatever object arrived, so
# `"abc".center(0, 0)` read a small integer's Value as a pointer and died;
# `"abc".center(10, "xy")` padded with 'x' where CPython refuses a fill of more
# than one character; and `"abc".center(10, None)` padded with whatever byte
# sits at that offset in the None singleton.  The padding itself was a memset
# of one BYTE, so a fill outside ASCII wrote the first byte of its UTF-8ten
# times and produced a string that is not valid UTF-8 at all.
#
# And a width is an index that has to FIT one: obj_as_index truncates, so
# `"abc".center(2**70)` came back as "abc" where CPython raises.

CASES = []
for method in ("center", "ljust", "rjust"):
    for args in ((0, 0), (10, 1), (10, "xy"), (10, ""), (10, None),
                 (10, b"x"), (10, ["x"]), (2 ** 70,), (-(2 ** 70),),
                 (10, "é"), (10, "中"), (10, "-"), (10,), (3,),
                 (0,), (-1,), (4, "\U0001F600")):
        CASES.append((method, args))

for method, args in CASES:
    try:
        answer = repr(getattr("abc", method)(*args))
    except Exception as e:
        answer = "%s: %s" % (type(e).__name__, e)
    print("%-8s %-22r %s" % (method, args, answer))

print("=== and the lengths are right ===")
for method in ("center", "ljust", "rjust"):
    for fill in ("-", "é", "中", "\U0001F600"):
        s = getattr("abc", method)(9, fill)
        print("%-8s %-4r len %d %r" % (method, fill, len(s), s))
    for subject in ("", "a", "é中", "\U0001F600"):
        s = getattr(subject, method)(6, ".")
        print("%-8s %-6r len %d %r" % (method, subject, len(s), s))

print("=== bytes says it differently ===")
for subject in (b"abc", bytearray(b"abc")):
    for method in ("center", "ljust", "rjust"):
        for arg in (1, b"xy", "x", b"", None):
            try:
                answer = repr(bytes(getattr(subject, method)(10, arg)))
            except Exception as e:
                answer = "%s: %s" % (type(e).__name__, e)
            print("%-10s %-8s %-6r %s" % (type(subject).__name__, method,
                                          arg, answer))

print("=== zfill and expandtabs ===")
for args in ((2 ** 70,), (5,), (0,), (-1,)):
    try:
        print("zfill %-10r %r" % (args, "7".zfill(*args)))
    except Exception as e:
        print("zfill %-10r %s: %s" % (args, type(e).__name__, e))
for args in ((2 ** 70,), (2 ** 31,), (4,), (0,), (-1,), ("x",)):
    try:
        print("expandtabs %-10r %r" % (args, "a\tb".expandtabs(*args)))
    except Exception as e:
        print("expandtabs %-10r %s: %s" % (args, type(e).__name__, e))
print("done")
