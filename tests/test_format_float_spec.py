# Float format specs: the uppercase conversions, and the empty one.
#
# 'F' shared 'f''s conversion, so format(float('inf'), 'F') was "inf" where
# CPython spells it INF; 'G' had no case at all in the dispatch and fell to
# the %g default, so format(1e20, 'G') was "1e+20".  C99's %F and %G already
# spell both in capitals, which is why 'E' was right by accident.
#
# The empty spec was a separate bug with the same reach: format_float_body
# synthesised a one-byte spec "r" on the strength of a comment claiming
# float_format_spec had a repr default.  It has none, so the letter was
# ignored and the %.6g defaults made format(1.0, "") into "1".

inf = float("inf")
nan = float("nan")

for t in ("f", "F", "e", "E", "g", "G", ""):
    for v in (1.5, 0.0, -0.0, 1e20, 1e-7, 12345.6789, inf, -inf, nan):
        print(repr(t), repr(v), "->", repr(format(v, t)))

# An empty spec is repr, and the width and sign machinery still runs on top.
print(repr(format(1.0, "")), repr(format(1.0, "10")), repr(format(1.0, "<10")))
print(repr(format(1.0, ">10")), repr(format(1.0, "^10")), repr(format(1.0, "=10")))
print(repr(format(1.0, "+")), repr(format(-1.0, "")), repr(format(1.0, " ")))
print(repr(format(1e100, "")), repr(format(0.1, "")), repr(format(-0.0, "")))

# An empty type with a precision is 'g', but keeps a digit after the point.
print(repr(format(1.0, ".3")), repr(format(1.23456, ".3")), repr(format(1e20, ".3")))
print(repr(format(inf, ".3")), repr(format(nan, ".3")), repr(format(100.0, ".2")))

# str(), repr() and an f-string with no spec were right all along; they must
# stay that way, and agree with format(x, "").
for v in (1.0, 1.5, 1e20, 1e-7, inf, nan, -0.0):
    print(str(v), repr(v), f"{v}", format(v, ""), str(v) == format(v, ""))

# Both halves of a complex take the same conversion.
print(repr(format(complex(inf, 1), "F")), repr(format(complex(nan, 1), "F")))
print(repr(format(complex(inf, 1), "G")), repr(format(complex(1.5, 2.5), "G")))
print(repr(format(complex(1, 2), "")), repr(format(complex(inf, 1), "")))

# The int conversions are unaffected by any of this.
print(format(255, "x"), format(255, "X"), format(255, "b"), format(255, "o"))
print(format(1234, ","), format(1234, "e"), format(1234, "E"), format(1234, "G"))


# The '%' type is 'f' applied to a hundred times the value, with a '%' on the
# end.  Neither half was done: the letter reached the renderer, matched none
# of its six, and fell to the %g default -- so format(1/3, ".2%") was "0.33",
# which is neither the right number nor carrying the sign that says what it
# is.
print("=== the percent type ===")
for v in (1.0 / 3.0, -1.0 / 3.0, 0.0, 1.0, 1e-9, 123.456, -2.5,
          float("inf"), float("nan")):
    row = []
    for spec in ("%", ".0%", ".2%", "+.2%", "10.2%", "<12.1%", "=+12.2%"):
        row.append(format(v, spec))
    print("%-8s %s" % (repr(v)[:8], " | ".join(row)))

# A precision is however many digits it takes.  Three was the most this could
# write, and rather than say so it quietly used 999 instead: format(1.0,
# ".5000f") came back one thousand places long.
print("=== a precision wider than three digits ===")
for p in (0, 1, 9, 10, 99, 100, 999, 1000, 1001, 2000, 5000):
    s = format(1.0 / 3.0, ".%df" % p)
    print("%-5d %d %s" % (p, len(s), s[:12]))
    if ("%%.%df" % p) % (1.0 / 3.0) != s:
        print("   ...but %% disagrees")

# And the limits of one.  CPython's precision is a C int and it says so; a
# width or precision so large that nothing can be allocated for it is a
# MemoryError there, and used to be "Fatal: out of memory" and a dead process
# here.  The exact threshold is not compared -- CPython will spend two
# gigabytes on a field of spaces and this refuses at a quarter of one -- only
# the shapes that are errors on both sides.
print("=== limits ===")
for label, fn in (
        ("precision > INT_MAX", lambda: format(1.0, ".2147483648f")),
        ("precision digits overflow", lambda: format(1.0, ".%df" % (10 ** 20))),
        ("width digits overflow", lambda: format(1.0, "%dg" % (10 ** 20))),
        ("%% star precision", lambda: "%.*f" % (2 ** 31, 1.0)),
        ("%% literal precision", lambda: "%.99999999999999f" % 1.0)):
    try:
        print("%-26s %r" % (label, fn()[:20]))
    except Exception as e:
        print("%-26s %s: %s" % (label, type(e).__name__, e))
