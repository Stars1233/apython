# zlib, over the real libz.
#
# The split is lib/_io.py's: _zlibcore owns the z_stream, the output buffer
# that has to grow while deflate writes into it, and the handle table; this
# module's surface -- the objects, the constants, the defaults, zlib.error --
# is lib/zlib.py.
#
# Every number here is checked against CPython's, which is the point: a .zip
# records the crc32 of what it holds, and a reader that computes a different
# one refuses the file.
import zlib

DATA = b"the quick brown fox jumps over the lazy dog\n" * 200
SHORT = b"hi"


def show(label, fn):
    try:
        print(label, "->", fn())
    except BaseException as e:
        print(label, "->", type(e).__name__ + ":", e)


# --- the checksums, which must agree with every other zlib ---------------
print("crc32 empty  :", zlib.crc32(b""))
print("crc32 hello  :", zlib.crc32(b"hello"))
print("crc32 chained:", zlib.crc32(b"world", zlib.crc32(b"hello")))
print("crc32 whole  :", zlib.crc32(b"helloworld"))
print("crc32 data   :", zlib.crc32(DATA))
print("crc32 bytearr:", zlib.crc32(bytearray(b"hello")))
print("adler empty  :", zlib.adler32(b""))
print("adler hello  :", zlib.adler32(b"hello"))
print("adler chained:", zlib.adler32(b"world", zlib.adler32(b"hello")))
print("crc32 signed :", zlib.crc32(b"\xff\xfe\xfd"))


# --- one-shot ------------------------------------------------------------
c = zlib.compress(DATA)
print("smaller      :", len(c) < len(DATA))
print("round trip   :", zlib.decompress(c) == DATA)
print("empty        :", zlib.decompress(zlib.compress(b"")) == b"")
print("short        :", zlib.decompress(zlib.compress(SHORT)) == SHORT)
for lvl in (0, 1, 6, 9, -1):
    out = zlib.compress(DATA, lvl)
    print("level", lvl, "  :", zlib.decompress(out) == DATA)


# --- the wbits conventions -----------------------------------------------
raw = zlib.compress(DATA, 6, -15)
print("raw deflate  :", zlib.decompress(raw, -15) == DATA)
print("raw has no hdr:", raw[:1] != c[:1])
gz = zlib.compress(DATA, 6, 31)
print("gzip wrapper :", zlib.decompress(gz, 31) == DATA)
print("gzip magic   :", gz[:2])


# --- the streaming objects -----------------------------------------------
co = zlib.compressobj()
parts = [co.compress(DATA[i:i + 997]) for i in range(0, len(DATA), 997)]
parts.append(co.flush())
print("streamed     :", zlib.decompress(b"".join(parts)) == DATA)

do = zlib.decompressobj()
out = b"".join(do.decompress(c[i:i + 101]) for i in range(0, len(c), 101))
out += do.flush()
print("destreamed   :", out == DATA, do.eof)

# max_length parks the rest of the input.
do2 = zlib.decompressobj()
first = do2.decompress(c, 100)
print("max_length   :", len(first) <= 100, len(do2.unconsumed_tail) > 0)
rest = do2.decompress(do2.unconsumed_tail)
while do2.unconsumed_tail:
    rest += do2.decompress(do2.unconsumed_tail)
print("resumed      :", first + rest + do2.flush() == DATA)

# Trailing bytes past the end of a stream are unused_data, which is how gzip
# finds the next member.
do3 = zlib.decompressobj()
print("with trailer :", do3.decompress(c + b"TRAILER") == DATA)
print("unused_data  :", do3.unused_data)


# --- the refusals ---------------------------------------------------------
show("not bytes    ", lambda: zlib.compress("text"))
show("not bytes crc", lambda: zlib.crc32("text"))
show("garbage      ", lambda: zlib.decompress(b"not compressed at all"))
show("truncated    ", lambda: zlib.decompress(c[:20]))
show("bad method   ", lambda: zlib.compressobj(6, 99))
show("after flush  ", lambda: _after_flush())


def _after_flush():
    z = zlib.compressobj()
    z.compress(b"x")
    z.flush()
    return z.compress(b"y")


show("error is a class", lambda: issubclass(zlib.error, Exception))

# --- the constants a caller reads ----------------------------------------
print("constants    :", zlib.MAX_WBITS, zlib.DEFLATED, zlib.DEF_MEM_LEVEL,
      zlib.Z_BEST_COMPRESSION, zlib.Z_DEFAULT_COMPRESSION, zlib.Z_FINISH)
print("version type :", type(zlib.ZLIB_VERSION).__name__,
      zlib.ZLIB_VERSION == zlib.ZLIB_RUNTIME_VERSION)


# --- a large body, which forces the output buffer to grow ----------------
big = bytes(range(256)) * 4000       # 1 MB, and deliberately incompressible
cb = zlib.compress(big)
print("1MB round trip:", zlib.decompress(cb) == big, len(cb) > 16384)
