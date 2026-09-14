# bz2, as a _bz2core shim with the objects in Python.
#
# `import bz2` was a ModuleNotFoundError, and so were the two names CPython's
# own bz2.py is written against.  The split is zlib's: the bz_stream, the
# output buffer that grows while libbzip2 writes into it and the handle table
# are src/modules/bz2.asm; BZ2Compressor, BZ2Decompressor, their attributes
# and the exception classes are lib/_bz2.py.
#
# Two things about libbzip2 that a rewrite gets wrong quietly, and both are
# tested here:
#
#   * BZ_RUN with no input left is not a no-op.  libbzip2 answers
#     BZ_PARAM_ERROR, because "no progress" and "bad call" are the same code
#     there, so `BZ2Compressor().compress(b"")` must not reach it at all.
#   * avail_in and avail_out are 32-bit while the buffers need not be, so the
#     loop has to top the input up rather than hand it over once.
#
# The module is also where the reduction has to be exact: bzip2 output is a
# format other programs read, so a round trip through this and a byte
# comparison against CPython's own compressor are different claims and both
# are made below.
import _bz2

TEXT = (b"When Mr. Bilbo Baggins of Bag End announced that he would shortly "
        b"be celebrating his eleventy-first birthday with a party of special "
        b"magnificence, there was much talk and excitement in Hobbiton. ")
DATA = TEXT * 40

# --- the shape of the module -------------------------------------------------

print("names:", sorted(n for n in dir(_bz2) if not n.startswith("_")))

# --- a round trip -------------------------------------------------------------

c = _bz2.BZ2Compressor()
blob = c.compress(DATA) + c.flush()
print("magic:", blob[:3], "smaller:", len(blob) < len(DATA))
d = _bz2.BZ2Decompressor()
print("round trip:", d.decompress(blob) == DATA)
print("eof:", d.eof, "needs_input:", d.needs_input, "unused:", d.unused_data)

# Every level, and each one is a valid stream.
for level in range(1, 10):
    c = _bz2.BZ2Compressor(level)
    b = c.compress(DATA) + c.flush()
    d = _bz2.BZ2Decompressor()
    ok = d.decompress(b) == DATA
    print("level %d: %-5s %s" % (level, ok, len(b)))

# --- the empty stream ---------------------------------------------------------
#
# compress(b"") must answer b"" rather than reaching libbzip2, and the flush
# alone is still a complete, decompressible stream.

print()
c = _bz2.BZ2Compressor()
first = c.compress(b"")
rest = c.flush()
print("empty compress:", first, "flush is a stream:", rest[:3])
print("empty round trip:", _bz2.BZ2Decompressor().decompress(first + rest))
c = _bz2.BZ2Compressor()
print("several empties:", c.compress(b"") + c.compress(b"") + c.compress(b"")
      == b"")
print("still usable:", _bz2.BZ2Decompressor().decompress(c.flush()) == b"")

# --- chunking, in both directions --------------------------------------------

print()
c = _bz2.BZ2Compressor(1)
parts = [c.compress(DATA[i:i + 7]) for i in range(0, len(DATA), 7)]
parts.append(c.flush())
blob = b"".join(parts)
d = _bz2.BZ2Decompressor()
out = [d.decompress(blob[i:i + 5]) for i in range(0, len(blob), 5)]
print("chunked both ways:", b"".join(out) == DATA, d.eof)

# A compressor holds its block until the flush, so most calls answer b"".
c = _bz2.BZ2Compressor(9)
held = sum(1 for i in range(0, len(DATA), 7)
           if c.compress(DATA[i:i + 7]) == b"")
print("held until flush:", held, "of", (len(DATA) + 6) // 7)
c.flush()

# --- max_length, and the input it parks --------------------------------------

print()
c = _bz2.BZ2Compressor()
blob = c.compress(DATA) + c.flush()
d = _bz2.BZ2Decompressor()
pieces = []
chunk = d.decompress(blob, 64)
rounds = 1
while not d.eof and rounds < 10000:
    if len(chunk) > 64:
        print("chunk too big:", len(chunk))
        break
    pieces.append(chunk)
    chunk = d.decompress(b"", 64)
    rounds += 1
pieces.append(chunk)
print("capped:", b"".join(pieces) == DATA, "rounds:", rounds > 1, "eof:", d.eof)

# needs_input is False while the core is holding input the caller has not
# asked for yet; that is what tells _compression.DecompressReader to send b""
# rather than read more of the file.
d = _bz2.BZ2Decompressor()
d.decompress(blob, 16)
print("needs_input while holding:", d.needs_input)

# needs_input is not simply "nothing is parked".  CPython sets it FALSE when
# the output buffer filled exactly at the cap, because the codec may still be
# holding bytes the next call will emit -- a caller told to go read more of
# the file would stall.  Sweeping the cap is what exercises both arms: the
# sizes where the two run out together are the ones that matter, and they are
# not predictable from the outside.
for cap in (1, 2, 3, 7, 15, 16, 17, 31, 64, 100, 255, 256, 1000, 4096):
    dd = _bz2.BZ2Decompressor()
    got = dd.decompress(blob, cap)
    print("cap %-5d out=%-5d needs_input=%-5s eof=%s"
          % (cap, len(got), dd.needs_input, dd.eof))
d2 = _bz2.BZ2Decompressor()
d2.decompress(blob[:20])
print("needs_input while hungry:", d2.needs_input)

# A cap of 0 asks for nothing, and a negative one for everything.
d = _bz2.BZ2Decompressor()
print("cap 0:", d.decompress(blob, 0))
print("then all:", d.decompress(b"", -1) == DATA)

# --- trailing data ------------------------------------------------------------

print()
d = _bz2.BZ2Decompressor()
print("with tail:", d.decompress(blob + b"and then some") == DATA)
print("unused_data:", d.unused_data)
d = _bz2.BZ2Decompressor()
d.decompress(blob)
print("no tail:", d.unused_data)

# Two streams back to back: the second is the first one's unused_data.
two = blob + blob
d = _bz2.BZ2Decompressor()
one = d.decompress(two)
d2 = _bz2.BZ2Decompressor()
print("two streams:", one == DATA and d2.decompress(d.unused_data) == DATA)

# --- what is refused ----------------------------------------------------------

print()
for bad in (0, 10, -1, 100):
    try:
        _bz2.BZ2Compressor(bad)
        print("accepted level", bad)
    except ValueError as e:
        print("level %-4r %s" % (bad, e))
try:
    _bz2.BZ2Compressor(1.5)
except TypeError as e:
    print("float level:", e)

try:
    _bz2.BZ2Decompressor().decompress(b"this is not a bzip2 stream at all")
except OSError as e:
    print("bad data: %s: %s" % (type(e).__name__, e))

try:
    _bz2.BZ2Decompressor().decompress(blob[:30])
    print("truncated accepted (more input expected)")
except OSError as e:
    print("truncated:", type(e).__name__)

d = _bz2.BZ2Decompressor()
d.decompress(blob)
try:
    d.decompress(b"more")
except EOFError as e:
    print("after eof: %s: %s" % (type(e).__name__, e))

c = _bz2.BZ2Compressor()
c.flush()
for fn in (lambda: c.compress(b"x"), lambda: c.flush()):
    try:
        fn()
    except ValueError as e:
        print("after flush:", e)

for bad in ("a string", 42, None, [1, 2]):
    try:
        _bz2.BZ2Compressor().compress(bad)
        print("accepted %r" % (bad,))
    except TypeError as e:
        print("%-10s %s" % (type(bad).__name__, e))

# --- the buffer protocol ------------------------------------------------------
#
# BZ2File.write hands a memoryview straight through rather than copying an
# array, so the core has to take one -- and only a contiguous one, since a
# strided view has no buffer to point at.

print()
c = _bz2.BZ2Compressor()
mv = memoryview(bytearray(DATA))
print("memoryview:", _bz2.BZ2Decompressor().decompress(
    c.compress(mv) + c.flush()) == DATA)
c = _bz2.BZ2Compressor()
print("bytearray:", _bz2.BZ2Decompressor().decompress(
    c.compress(bytearray(DATA)) + c.flush()) == DATA)
try:
    _bz2.BZ2Compressor().compress(memoryview(bytearray(DATA))[::2])
except BufferError as e:
    print("strided: %s: %s" % (type(e).__name__, e))

# --- an uninitialised decompressor --------------------------------------------
#
# CPython's is a C type whose struct is zero-filled by the allocator, so this
# answers b"" rather than raising; there is a test in CPython's suite named
# for the crash it used to be.

print()
d = _bz2.BZ2Decompressor.__new__(_bz2.BZ2Decompressor)
print("uninitialised:", d.decompress(b""), d.eof, d.needs_input, d.unused_data)

# --- neither object can be pickled --------------------------------------------

for obj in (_bz2.BZ2Compressor(), _bz2.BZ2Decompressor()):
    try:
        obj.__reduce__()
        print("reduced", type(obj).__name__)
    except TypeError as e:
        print("%s: %s" % (type(obj).__name__, e))

# --- a big one, which is where the 32-bit counters would show -----------------

print()
big = DATA * 400
c = _bz2.BZ2Compressor(9)
blob = c.compress(big) + c.flush()
d = _bz2.BZ2Decompressor()
print("big round trip:", d.decompress(blob) == big, len(big), len(blob))

print("done")
