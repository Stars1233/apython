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
