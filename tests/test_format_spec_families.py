"""The format-spec families this tree got wrong, and a sweep of the grammar.

Nine differences, found by fuzzing fill/align/sign/`#`/`0`/width/separator/
precision/type over a range of values and diffing against CPython.  Each is
listed below with the case that names it; the cross-product at the end is what
found them and is what would find the next one.

  * An unknown presentation type was not refused: format(42, "Z") answered
    '42'.  A mistyped spec formatted silently, which is how one is usually
    found.
  * An explicit fill and align, then a `0`: the 0 overwrote the fill that was
    already given.  CPython supplies one only when none was written.
  * A precision on an integer presentation was not refused.
  * `n` accepted a separator, where CPython refuses it because `n` takes one
    from the locale.
  * `Invalid format specifier` named neither the spec nor the type.
  * `#` was dropped for every float type: the flag was parsed and then never
    read by the float renderer.
  * An empty type with a precision used C's %g threshold for switching to an
    exponent.  CPython lowers it by one when it is also going to add a `.0`
    (pystrtod.c, format_float_short), so format(1.5, ".0") is '2e+00' there
    and was '2.0' here.
"""

import itertools

print("--- an unknown presentation type is refused ---")
for spec in ("Z", "Q", "!", "y", "j", "$", "z"):
    for v in (42, 1.5, "s"):
        try:
            print("%-3r %-5r -> %r" % (spec, v, format(v, spec)))
        except ValueError as e:
            print("%-3r %-5r -> %s" % (spec, v, e))

print("--- the codes that ARE real still work ---")
for spec in ("b", "o", "x", "X", "d", "n", "c"):
    print(spec, "->", format(65, spec))
for spec in ("e", "E", "f", "F", "g", "G", "%"):
    print(spec, "->", format(1.5, spec))

print("--- an explicit fill survives a 0 ---")
for spec in ("*^-05d", "*<05d", "*>05d", "*=05d", "05d", "<05d", "^05d"):
    print("%-8r -> %r" % (spec, format(-7, spec)))

print("--- a precision on an integer presentation is refused ---")
for spec in ("#020.7", ".3d", ".1", "5.2d", ".0b", ".2x", ".1n"):
    try:
        print("%-8r -> %r" % (spec, format(255, spec)))
    except ValueError as e:
        print("%-8r -> %s" % (spec, e))

print("--- n refuses a separator ---")
for spec in ("0>#0,n", ",n", "_n", ",d", "_d", ",x", "_x", ",b", "_b"):
    try:
        print("%-8r -> %r" % (spec, format(10 ** 6, spec)))
    except ValueError as e:
        print("%-8r -> %s" % (spec, e))

print("--- an invalid spec names itself and the type ---")
for v in (42, 1.5, "s", [1]):
    try:
        print(type(v).__name__, "->", format(v, "*#012"))
    except (ValueError, TypeError) as e:
        print(type(v).__name__, "->", e)

print("--- # on the float types ---")
for spec in ("#.0e", "#.0E", "#.0f", "#.0F", "#g", "#G", "#.3e", "#.3f",
             "#.3g", "#12.0", "#.0", "#.1", "#.5", "#.0%"):
    print("%-7r -> %r" % (spec, format(1.5, spec)))
for spec in ("#.0e", "#.0f", "#g", "#12.0"):
    print("%-7r 0.0 -> %r" % (spec, format(0.0, spec)))

print("--- an empty type with a precision ---")
for p in range(8):
    print(p, [format(v, ".%d" % p) for v in (0.0, 1.5, 123.0, 0.00012, 1e20)])
for p in range(4):
    print("alt", p, [format(v, "#.%d" % p) for v in (0.0, 1.5, 123.0)])

print("--- and the whole grammar, crossed ---")
FILLS = ("", "*", "0")
ALIGNS = ("", "<", ">", "^", "=")
SIGNS = ("", "+", "-", " ")
ALTS = ("", "#")
ZEROS = ("", "0")
WIDTHS = ("", "6", "12")
SEPS = ("", ",", "_")
PRECS = ("", ".0", ".3")
TYPES = ("", "d", "x", "e", "f", "g", "n", "%")
VALUES = (0, 7, -7, 255, 10 ** 12, 0.0, 1.5, -1.5, 1e20, 0.00012)

seen = 0
for fill, align, sign, alt, zero, width, sep, prec, ty in itertools.product(
        FILLS, ALIGNS, SIGNS, ALTS, ZEROS, WIDTHS, SEPS, PRECS, TYPES):
    if fill and not align:
        continue                    # a fill without an align is not a fill
    spec = fill + align + sign + alt + zero + width + sep + prec + ty
    for v in VALUES:
        try:
            out = repr(format(v, spec))
        except ValueError as e:
            out = "ValueError: %s" % e
        except TypeError as e:
            out = "TypeError: %s" % e
        except OverflowError as e:
            out = "OverflowError: %s" % e
        seen += 1
        print("%-16s %-8r %s" % (spec, v, out))
print("crossed:", seen)

print("--- PEP 682's z, against the sign and the trim ---")
ZVALS = (0.0, -0.0, 0.0001, -0.0001, 0.4, -0.4, 1.5, -1.5, 1e20, -1e20,
         float("inf"), float("-inf"), 12345.6789, -12345.6789)
for _sign in ("", "+", " ", "-"):
    for _z in ("", "z"):
        for _prec in ("", ".0", ".2"):
            for _ty in ("", "f", "e", "g", "F", "E", "G", "%"):
                _spec = _sign + _z + _prec + _ty
                for _v in ZVALS:
                    try:
                        _out = repr(format(_v, _spec))
                    except ValueError as e:
                        _out = "ValueError: %s" % e
                    print("%-10s %-12r %s" % (_spec, _v, _out))

print("--- an unknown type code names itself the way CPython does ---")
for _c in ("\t", "\x01", "\x7f", "\x80", "\xe9", "\u2603", "~", "Q"):
    try:
        format(0.0, _c)
    except ValueError as e:
        print("%r -> %s" % (_c, e))
    else:
        print("%r -> no error" % (_c,))

print("done")
