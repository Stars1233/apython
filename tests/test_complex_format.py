# format() on a complex, and the specs it refuses.
#
# Before this, a complex fell through the type dispatch in src/format.asm into
# format_float_body, which called float_to_f64 on a pointer and printed
# 0.000000 for every spec.
#
# With no type letter the body is exactly repr(z): CPython's
# format_complex_internal sets type 'r' and applies the same
# omit-a-positive-zero-real-part rule that repr already implements.  With a
# letter, both halves are rendered with the same spec and joined without
# parentheses, and the sign flag applies only to the real part.

VALUES = [1 + 2j, 1 - 2j, 2j, -2j, 0j, complex(-0.0, 2), complex(1.5, -0.25),
          complex(100, 0)]
SPECS = ['', '.2f', '+.2f', ' .2f', '.1e', '.1E', 'f', 'e', 'E', 'g', 'G',
         'n', '.3g', '.0f', '10', '^10', '<10', '>10', ',', '_']

for v in VALUES:
    for s in SPECS:
        try:
            print(repr(v), repr(s), "->", repr(format(v, s)))
        except BaseException as exc:
            print(repr(v), repr(s), "->", type(exc).__name__)

# Specs a complex refuses.  Zero padding and '=' alignment are rejected before
# anything is formatted, and only e E f F g G n are accepted as type letters.
print("--- refused ---")
for s in ['s', 'd', 'b', 'o', 'x', 'X', 'c', '%', '#x', '020.2f', '=20.2f',
          '020', '=20']:
    try:
        print(repr(s), "->", repr(format(1 + 2j, s)))
    except BaseException as exc:
        print(repr(s), "->", type(exc).__name__)

# The same function serves f-strings, str.format and %-formatting.
z = 1 + 2j
print(f"{z}", f"{z:.2f}", f"{z:>12}", "{}".format(z), "{:.3f}".format(z))
print("%s" % (z,), "%r" % (z,))

# float 'F' was falling through to %g; it is 'f' with capitalised INF/NAN.
print(format(1.5, 'F'), format(1.5, 'f'), format(2.5, '.1F'))
