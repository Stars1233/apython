# hex(), oct(), bin() and the b/o/x/X format types all went through an int64:
# hex(2**70) was "0x0", and f"{2**70:x}" raised "integer too large for this
# format".  And e/f/g with no explicit precision fell back to repr, so
# f"{1.5:f}" was "1.5" rather than "1.500000".

n = 2 ** 70
for v in (0, 1, 255, -255, 8, n, -n, 2 ** 200, True, False):
    print(hex(v), oct(v), bin(v))


class I:
    def __index__(self):
        return 26


print(hex(I()), oct(I()), bin(I()))


def err(f):
    try:
        return f()
    except Exception as e:
        return type(e).__name__


print(err(lambda: hex("a")), err(lambda: hex(1.5)), err(lambda: bin(None)))

for v in (0, 255, n, -n):
    print(f"{v:x}", f"{v:X}", f"{v:o}", f"{v:b}", f"{v:d}", f"{v:,}")
    print(f"{v:#x}", f"{v:#o}", f"{v:#b}", f"{v:>30x}|", f"{v:030x}")
    print("%x" % v, "%X" % v, "%o" % v, "%#x" % v, "%d" % v)

for v in (1.5, 1234.5678, 0.000123, 1e21, 12345678901234567890.0, -2.5, 0.0):
    print(f"{v:e}", f"{v:E}", f"{v:.3e}", f"{v:f}", f"{v:.2f}", f"{v:g}",
          f"{v:.10g}", f"{v}")
    print("%e" % v, "%f" % v, "%g" % v, "%.3f" % v)

print(f"{2**70:f}"[:30], f"{2**70:e}")
