# The '*' width and precision in %-formatting.
#
# "%*d" % (6, 42) takes the width from the argument list, and "%.*g" % (3, x)
# takes the precision.  Neither worked: '*' is not a flag, not a digit and not
# '.', so the directive scanner stopped there, the conversion went out with no
# width at all, and the number that was meant to be the width was still
# sitting in the tuple when the arity check ran.  What came out was
# "not all arguments converted during string formatting", which names neither
# the directive nor the reason.  timeit uses "%.*g".
CASES = [
 ("%*d", (6, 42)), ("%*d", (-6, 42)), ("%-*d", (6, 42)), ("%0*d", (6, 42)),
 ("%0*d", (6, -42)), ("%+*d", (6, 42)), ("% *d", (6, 42)),
 ("%.*f", (2, 3.14159)), ("%.*f", (0, 3.6)), ("%.*e", (3, 12345.6789)),
 ("%.*g", (3, 1.23456)), ("%.*g", (-1, 1.5)), ("%.*G", (2, 0.000123)),
 ("%*.*f", (10, 2, 3.14159)), ("%-*.*f|", (10, 2, 3.14159)),
 ("%*.*s", (8, 3, "abcdefg")), ("%.*s", (2, "abcdef")), ("%.*s", (0, "abc")),
 ("%*s", (5, "ab")), ("%*s", (-5, "ab")), ("%*s|", (0, "ab")),
 ("%*r", (8, [1])), ("%.*r", (3, "abcdef")),
 ("%*x", (6, 255)), ("%#*x", (8, 255)), ("%*o", (6, 8)), ("%0*X", (6, 255)),
 ("a%*db", (4, 1)), ("%*d%*d", (3, 1, 4, 2)), ("%%%*d", (3, 7)),
 ("%*c", (3, 65)), ("%*i", (5, 9)), ("%*u", (5, 9)),
 ("%*d", (True, 1)), ("%*d", (1 << 3, 5)),
 ("%.*f", (2,)), ("%*d", (6,)),
 ("%*d", ("x", 1)), ("%.*f", (1.5, 1)), ("%*d", (None, 1)),
 ("%(a)*d", {"a": 1}),
]
for fmt, args in CASES:
    try:
        print("%-14r %-22r => %r" % (fmt, args, fmt % args))
    except BaseException as e:
        print("%-14r %-22r !! %s %s" % (fmt, args, type(e).__name__, e))
# bytes % too
BCASES = [(b"%*d", (6, 42)), (b"%.*f", (2, 3.5)), (b"%*s", (5, b"ab")),
          (b"%-*d|", (6, 42)), (b"%*.*f", (9, 3, 1.5))]
for fmt, args in BCASES:
    try:
        print("%-14r %-22r => %r" % (fmt, args, fmt % args))
    except BaseException as e:
        print("%-14r %-22r !! %s %s" % (fmt, args, type(e).__name__, e))
