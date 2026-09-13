# memoryview.cast() and the native format codes.
#
# cast() accepted five formats -- B, H, I, L, Q -- and silently ALIASED 'b' to
# 'B', so `memoryview(b'\xff').cast('b')[0]` was 255 where CPython says -1 and
# `.format` afterwards answered 'B'.  A silent alias is the worst of the three
# possible answers: refusing would have been visible, and answering -1 would
# have been right.
#
# The signed codes are the point, but the float, bool and char ones come with
# them: memoryview_item_value read itemsize bytes little-endian and unsigned
# because that was all the accepted formats needed, and a format table is only
# as wide as the decoder under it.
# struct is not in the shipped lib/, so the byte patterns are written out --
# which is clearer anyway, since what is being tested is the decode.
DATA = bytes(range(16))
SIZES = {"c": 1, "b": 1, "B": 1, "?": 1, "h": 2, "H": 2, "i": 4, "I": 4,
         "l": 8, "L": 8, "q": 8, "Q": 8, "n": 8, "N": 8, "f": 4, "d": 8,
         "P": 8}
NATIVE = "cbB?hHiIlLqQnNfdP"

F_1_5 = b"\x00\x00\xc0\x3f"                       # 1.5 as a float32
D_M2_25 = b"\x00\x00\x00\x00\x00\x00\x02\xc0"     # -2.25 as a float64
F_NAN = b"\x00\x00\xc0\x7f"
D_INF = b"\x00\x00\x00\x00\x00\x00\xf0\x7f"


def fits(fmt):
    size = SIZES[fmt]
    return memoryview(DATA[: (len(DATA) // size) * size])


for fmt in NATIVE:
    c = fits(fmt).cast(fmt)
    print("%-2s itemsize=%d format=%-3r len=%-3d first=%r"
          % (fmt, c.itemsize, c.format, len(c), c[0]))

# --- signedness -------------------------------------------------------------

print()
print("b of 0xff:", memoryview(b"\xff").cast("b")[0])
print("B of 0xff:", memoryview(b"\xff").cast("B")[0])
print("b of 0x80:", memoryview(b"\x80").cast("b")[0])
print("h of 0xffff:", memoryview(b"\xff\xff").cast("h")[0])
print("H of 0xffff:", memoryview(b"\xff\xff").cast("H")[0])
print("i of -1:", memoryview(b"\xff\xff\xff\xff").cast("i")[0])
print("I of -1:", memoryview(b"\xff\xff\xff\xff").cast("I")[0])
print("q of -1:", memoryview(b"\xff" * 8).cast("q")[0])
print("Q of -1:", memoryview(b"\xff" * 8).cast("Q")[0])
print("n of -1:", memoryview(b"\xff" * 8).cast("n")[0])
print("N of -1:", memoryview(b"\xff" * 8).cast("N")[0])

# The format is reported as written, not as the one it was folded onto.
print("format of b:", memoryview(b"\xff").cast("b").format)
print("format of n:", memoryview(b"\xff" * 8).cast("n").format)

# --- floats, bools and chars -------------------------------------------------

print()
print("f:", memoryview(F_1_5).cast("f")[0])
print("d:", memoryview(D_M2_25).cast("d")[0])
nan = memoryview(F_NAN).cast("f")[0]
print("f nan is nan:", nan != nan)
print("d inf:", memoryview(D_INF).cast("d")[0])
print("bool:", memoryview(b"\x00\x01\x02\xff").cast("?").tolist())
print("char:", memoryview(b"abc").cast("c").tolist())
print("char type:", type(memoryview(b"ab").cast("c")[0]).__name__)

# --- tolist, tobytes and iteration all agree --------------------------------

print()
m = memoryview(b"\xff\xfe\x01\x02").cast("b")
print("tolist:", m.tolist())
print("iterated:", [x for x in m])
print("tobytes:", m.tobytes())
print("len, nbytes:", len(m), m.nbytes)
print("slice:", m[1:3].tolist())
print("negative index:", m[-1])

w = memoryview(b"\xff\xff\x02\x01").cast("h")
print("wide tolist:", w.tolist())
print("wide iterated:", [x for x in w])

# --- a round trip through cast ----------------------------------------------

print()
orig = memoryview(bytearray(b"\x01\x02\x03\x04"))
as_i = orig.cast("i")
print("as i:", as_i[0])
print("back to B:", as_i.cast("B").tolist())
print("via b:", orig.cast("b").tolist())

# --- what is refused ---------------------------------------------------------

print()
# 'e', the half-float, is left out: CPython accepts it and this does not,
# which is recorded rather than asserted here.
for bad in ("", "bb", "s", "x", "Z", "1b"):
    try:
        memoryview(DATA).cast(bad)
        print("accepted %r" % (bad,))
    except (TypeError, ValueError) as e:
        print("refused %-4r %s" % (bad, type(e).__name__))

# A length that does not divide evenly.
try:
    memoryview(b"abc").cast("i")
except TypeError as e:
    print("bad length:", type(e).__name__)

# A non-contiguous view has nothing to cast.
try:
    memoryview(DATA)[::2].cast("B")
except TypeError as e:
    print("strided:", type(e).__name__)

# --- writing through a cast view ---------------------------------------------

print()
buf = bytearray(b"\x00\x00\x00\x00")
mv = memoryview(buf).cast("b")
mv[0] = -1
mv[1] = 127
print("wrote b:", bytes(buf))
try:
    mv[2] = 128
except ValueError as e:
    print("b range:", type(e).__name__)
try:
    mv[2] = -129
except ValueError as e:
    print("b range low:", type(e).__name__)

buf = bytearray(4)
mv = memoryview(buf).cast("i")
mv[0] = -2
print("wrote i:", bytes(buf))

buf = bytearray(4)
mv = memoryview(buf).cast("f")
mv[0] = 1.5
print("wrote f:", bytes(buf) == F_1_5)

buf = bytearray(2)
mv = memoryview(buf).cast("?")
mv[0] = True
mv[1] = False
print("wrote bool:", bytes(buf))

buf = bytearray(2)
mv = memoryview(buf).cast("c")
mv[0] = b"x"
print("wrote char:", bytes(buf))

# A read-only view refuses.
try:
    memoryview(b"ab").cast("b")[0] = 1
except TypeError as e:
    print("read-only:", type(e).__name__)

# --- hashing is restricted to the byte-shaped formats ------------------------
#
# The hash is over the raw bytes, so two views that compare EQUAL under a
# wider format -- which compares items -- would have to hash the same, and
# over the raw bytes they need not.

print()
ro = memoryview(b"\x01\x02\x03\x04")
for fmt in "bBc?hi":
    try:
        print("hash %s: %s" % (fmt, hash(ro.cast(fmt)) == hash(b"\x01\x02\x03\x04")))
    except ValueError as e:
        print("hash %s refused: %s" % (fmt, e))

print("done")
