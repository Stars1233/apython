# The int methods that read only the low 64 bits of their operand.
#
# bit_count, conjugate, to_bytes and from_bytes all went through a helper that
# truncates to an i64, so (2**70).bit_count() was 2, (2**64-1).bit_count() was
# 63, (2**70+3).conjugate() was 3, and to_bytes/from_bytes lost everything
# above the low eight bytes.  None of it raised: the answers were simply
# wrong.  to_bytes also never read its signed= argument and never checked that
# the value fit, so (-1).to_bytes(1, 'big') was b'\xff' rather than an
# OverflowError.

for n in (0, 1, 255, 2**63, 2**64 - 1, 2**70, 2**70 + 3, 2**100 + 2**50 + 1,
          -1, -255, -(2**70), -(2**64 - 1)):
    print(n, n.bit_count(), n.bit_length(), n.conjugate(), n.conjugate() == n)

class I(int):
    pass

for n in (7, 258, 2**70 + 3, -(2**70)):
    v = I(n)
    print(n, v.bit_count(), v.bit_length(), v.conjugate(), type(v.conjugate()).__name__)

# to_bytes, at widths past 64 bits and in both orders.
for n, sz in ((0, 4), (0, 0), (1, 1), (255, 1), (256, 2), (65535, 2),
              (2**64, 9), (2**70 + 3, 16), (2**100, 16), (2**127 - 1, 16)):
    print(n, sz, n.to_bytes(sz, "big").hex(), n.to_bytes(sz, "little").hex())

# signed, including the one negative value that needs the whole width.
for n, sz in ((-1, 1), (-1, 2), (-128, 1), (127, 1), (-(2**70), 16),
              (-(2**127), 16), (-1, 16)):
    print(n, sz, n.to_bytes(sz, "big", signed=True).hex(),
          n.to_bytes(sz, "little", signed=True).hex())

# The defaults, and the cases that must raise.
print((255).to_bytes().hex(), (255).to_bytes(1).hex(), (0).to_bytes().hex())
for expr in ("(-1).to_bytes(1, 'big')", "(256).to_bytes(1, 'big')",
             "(2**70).to_bytes(4, 'big')", "(128).to_bytes(1, 'big', signed=True)",
             "(-129).to_bytes(1, 'big', signed=True)", "(1).to_bytes(-1, 'big')"):
    try:
        eval(expr)
        print(expr, "=> no error")
    except (OverflowError, ValueError) as e:
        print(expr, "=>", type(e).__name__)

# from_bytes, the same widths, and the round trip that ties the two together.
for n, sz in ((0, 4), (1, 1), (255, 1), (2**64, 9), (2**70 + 3, 16), (2**100, 16)):
    b = n.to_bytes(sz, "big")
    l = n.to_bytes(sz, "little")
    print(n, int.from_bytes(b, "big"), int.from_bytes(l, "little"),
          int.from_bytes(b, "big") == n)

for n, sz in ((-1, 2), (-128, 1), (-(2**70), 16), (127, 1), (-(2**127), 16)):
    b = n.to_bytes(sz, "big", signed=True)
    l = n.to_bytes(sz, "little", signed=True)
    print(n, int.from_bytes(b, "big", signed=True),
          int.from_bytes(l, "little", signed=True),
          int.from_bytes(b, "big", signed=True) == n)

print(int.from_bytes(b"", "big"), int.from_bytes(b"\x00\x00", "big"))
print(int.from_bytes(b"\xff", "big"), int.from_bytes(b"\xff", "big", signed=True))
print(int.from_bytes([1, 2], "big"), int.from_bytes(bytearray(b"\x01\x02"), "big"))
print(int.from_bytes(b"\x01\x02"), type(int.from_bytes(b"\x01", "big")).__name__)
print(I.from_bytes(b"\x01\x02", "big"), type(I.from_bytes(b"\x01\x02", "big")).__name__)
