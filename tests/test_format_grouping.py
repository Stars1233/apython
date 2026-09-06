# The thousands separator, and the presentation type that spells a character.
#
# `,` and `_` were parsed and then honoured only by the integer formatter, so
# format(1234567, ",") was right and format(1234567.5, ",.2f") was
# '1234567.50'.  Zero-padding did not group either -- CPython inserts the
# separators into the pad, so format(3, "08,.3f") is '0,003.000' -- and `_`
# on a hex, octal or binary presentation groups every FOUR digits from the
# right rather than every three.
#
# `c` was not implemented at all: format(65, "c") answered '65'.


def show(spec, *args):
    for v in args:
        try:
            print("%-10s %-14r %r" % (spec, v, format(v, spec)))
        except BaseException as e:
            print("%-10s %-14r %s: %s" % (spec, v, type(e).__name__, e))


print("--- integers, which already grouped ---")
for spec in (",", "_", ",d", "_d", "10,", "<12,", ">12,", "^12,", "012,"):
    show(spec, 0, 7, 1234, 1234567, -1234567, 10 ** 20, True)

print("--- floats, which did not ---")
for spec in (",.2f", "_.2f", ",f", ",e", ",g", ",", ",.0f", ",%", "015,.2f",
             "08,.3f", ">14,.2f", "=14,.2f", "+,.2f"):
    show(spec, 0.0, 1234.5, 1234567.5, -1234567.5, 1e20, 0.125)

print("--- and the same for a plain int under a float type ---")
for spec in (",.2f", ",e", ",g"):
    show(spec, 1234567, -1234567)

print("--- _ groups hex, octal and binary in FOURS ---")
for spec in ("_x", "_X", "_o", "_b", "#_x", "#_o", "#_b", "016_x", "#020_b"):
    show(spec, 0, 255, 1234567, 10 ** 20, -255)

print("--- but , is not allowed on them ---")
for spec in (",x", ",o", ",b", ",c"):
    show(spec, 255)

print("--- the c presentation type ---")
for spec in ("c", "5c", "<5c", ">5c", "^5c"):
    show(spec, 65, 0, 97, 0x10FFFF, 0x4e2d)
show("c", -1, 0x110000, 1.5, True)

print("--- and the combinations that are refused ---")
for spec in ("+c", "-c", " c", "#c", "0c", ",c", "_c", ".2c"):
    show(spec, 65)

print("--- f-strings reach the same code ---")
n, f = 1234567, 1234567.5
print(f"{n:,}", f"{f:,.2f}", f"{n:_x}", f"{65:c}", f"{n:015,}")

print("--- and %-formatting, which does not group ---")
print("%d" % 1234567, "%.2f" % 1234567.5, "%x" % 255)
