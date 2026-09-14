# lzma, as a _lzmacore shim with the objects in Python.
#
# `import lzma` was a ModuleNotFoundError, and with it the names CPython's own
# lzma.py is written against -- 121 tests in test_lzma, and no .xz anywhere in
# tarfile, zipfile or shutil.
#
# The split is zlib's and bz2's: the lzma_stream, the filter chain's option
# structs, the output buffer and the handle table are src/modules/lzma.asm;
# the objects, the constants, LZMAError and the filter DICTS are
# lib/_lzma.py.
#
# A filter chain crosses that boundary as a list of TUPLES OF INTS, because
# the option names, the defaults and the per-filter error wordings are
# Python's business while the struct layout is the assembly's.  So the dict
# handling is what this file leans on hardest: which keys each filter takes,
# what each refusal says, and that a chain round-trips through
# _encode_filter_properties and back.
import _lzma

TEXT = (b"It is a truth universally acknowledged, that a single man in "
        b"possession of a good fortune, must be in want of a wife. ")
DATA = TEXT * 60

# --- the shape of the module -------------------------------------------------

print("formats:", _lzma.FORMAT_AUTO, _lzma.FORMAT_XZ, _lzma.FORMAT_ALONE,
      _lzma.FORMAT_RAW)
print("checks:", _lzma.CHECK_NONE, _lzma.CHECK_CRC32, _lzma.CHECK_CRC64,
      _lzma.CHECK_SHA256, _lzma.CHECK_ID_MAX, _lzma.CHECK_UNKNOWN)
print("filters:", _lzma.FILTER_LZMA1, _lzma.FILTER_LZMA2, _lzma.FILTER_DELTA,
      _lzma.FILTER_X86)
print("mf:", _lzma.MF_HC3, _lzma.MF_HC4, _lzma.MF_BT2, _lzma.MF_BT3,
      _lzma.MF_BT4)
print("modes:", _lzma.MODE_FAST, _lzma.MODE_NORMAL)
print("presets:", _lzma.PRESET_DEFAULT, _lzma.PRESET_EXTREME)
print("supported:", [_lzma.is_check_supported(i) for i in (0, 1, 4, 10, 15)])

# --- the three container formats ---------------------------------------------

print()
c = _lzma.LZMACompressor()
blob = c.compress(DATA) + c.flush()
print("xz magic:", blob[:6], "smaller:", len(blob) < len(DATA))
d = _lzma.LZMADecompressor()
print("round trip:", d.decompress(blob) == DATA, "eof:", d.eof,
      "check:", d.check)

c = _lzma.LZMACompressor(_lzma.FORMAT_ALONE)
alone = c.compress(DATA) + c.flush()
d = _lzma.LZMADecompressor(_lzma.FORMAT_ALONE)
print("alone:", d.decompress(alone) == DATA, "check:", d.check)

# FORMAT_AUTO reads either of them without being told which.
for name, b in (("xz", blob), ("alone", alone)):
    d = _lzma.LZMADecompressor(_lzma.FORMAT_AUTO)
    print("auto %-6s %s" % (name, d.decompress(b) == DATA))

# Every check the build supports, and each one is reported back.
for check in (_lzma.CHECK_NONE, _lzma.CHECK_CRC32, _lzma.CHECK_CRC64,
              _lzma.CHECK_SHA256):
    c = _lzma.LZMACompressor(check=check)
    b = c.compress(DATA) + c.flush()
    d = _lzma.LZMADecompressor()
    ok = d.decompress(b) == DATA
    print("check %-2d %-5s reported %d" % (check, ok, d.check))

# Every preset.
for preset in range(10):
    c = _lzma.LZMACompressor(preset=preset)
    b = c.compress(DATA) + c.flush()
    print("preset %d: %-5s %d"
          % (preset, _lzma.LZMADecompressor().decompress(b) == DATA, len(b)))
c = _lzma.LZMACompressor(preset=1 | _lzma.PRESET_EXTREME)
b = c.compress(DATA) + c.flush()
print("extreme:", _lzma.LZMADecompressor().decompress(b) == DATA)

# --- filter chains ------------------------------------------------------------

print()
CHAINS = [
    [{"id": _lzma.FILTER_LZMA2}],
    [{"id": _lzma.FILTER_LZMA2, "preset": 9}],
    [{"id": _lzma.FILTER_LZMA1, "preset": 6}],
    [{"id": _lzma.FILTER_DELTA, "dist": 4},
     {"id": _lzma.FILTER_LZMA2, "preset": 6}],
    [{"id": _lzma.FILTER_X86}, {"id": _lzma.FILTER_LZMA2, "preset": 1}],
    [{"id": _lzma.FILTER_X86, "start_offset": 8},
     {"id": _lzma.FILTER_LZMA2, "preset": 1}],
    [{"id": _lzma.FILTER_LZMA2, "dict_size": 1 << 20, "lc": 2, "lp": 1,
      "pb": 1, "mode": _lzma.MODE_NORMAL, "nice_len": 32,
      "mf": _lzma.MF_BT4, "depth": 8}],
]
for chain in CHAINS:
    c = _lzma.LZMACompressor(_lzma.FORMAT_RAW, filters=chain)
    b = c.compress(DATA) + c.flush()
    d = _lzma.LZMADecompressor(_lzma.FORMAT_RAW, filters=chain)
    print("raw %-4d %s" % (len(b), d.decompress(b) == DATA))

# A chain in the xz container, which is the other way liblzma takes one.
chain = [{"id": _lzma.FILTER_DELTA, "dist": 2},
         {"id": _lzma.FILTER_LZMA2, "preset": 6}]
c = _lzma.LZMACompressor(_lzma.FORMAT_XZ, filters=chain)
b = c.compress(DATA) + c.flush()
print("xz with chain:", _lzma.LZMADecompressor().decompress(b) == DATA)

# FORMAT_ALONE takes one LZMA1 filter and nothing else.
c = _lzma.LZMACompressor(_lzma.FORMAT_ALONE,
                         filters=[{"id": _lzma.FILTER_LZMA1, "preset": 6}])
b = c.compress(DATA) + c.flush()
print("alone with chain:",
      _lzma.LZMADecompressor(_lzma.FORMAT_ALONE).decompress(b) == DATA)
try:
    _lzma.LZMACompressor(_lzma.FORMAT_ALONE,
                         filters=[{"id": _lzma.FILTER_LZMA2}])
except ValueError as e:
    print("alone rejects lzma2: %s: %s" % (type(e).__name__, e))

# --- the filter properties a raw stream's header carries ----------------------

print()
for spec in ({"id": _lzma.FILTER_LZMA1, "preset": 6},
             {"id": _lzma.FILTER_LZMA2, "preset": 6},
             {"id": _lzma.FILTER_LZMA2, "dict_size": 1 << 20},
             {"id": _lzma.FILTER_DELTA, "dist": 5},
             {"id": _lzma.FILTER_X86},
             {"id": _lzma.FILTER_ARM}):
    props = _lzma._encode_filter_properties(spec)
    back = _lzma._decode_filter_properties(spec["id"], props)
    print("%-42s %-8r %s" % (str(spec)[:42], props, back))

# --- max_length, needs_input and the parked input -----------------------------

print()
c = _lzma.LZMACompressor()
blob = c.compress(DATA) + c.flush()
d = _lzma.LZMADecompressor()
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
print("capped:", b"".join(pieces) == DATA, "rounds:", rounds > 1)

d = _lzma.LZMADecompressor()
d.decompress(blob, 16)
print("needs_input while capped:", d.needs_input)

# needs_input is not simply "nothing is parked".  CPython sets it FALSE when
# the output buffer filled exactly at the cap, because the codec may still be
# holding bytes the next call will emit -- a caller told to go read more of
# the file would stall.  Sweeping the cap is what exercises both arms: the
# sizes where the two run out together are the ones that matter, and they are
# not predictable from the outside.
for cap in (1, 2, 3, 7, 15, 16, 17, 31, 64, 100, 255, 256, 1000, 4096):
    dd = _lzma.LZMADecompressor()
    got = dd.decompress(blob, cap)
    print("cap %-5d out=%-5d needs_input=%-5s eof=%s"
          % (cap, len(got), dd.needs_input, dd.eof))
d2 = _lzma.LZMADecompressor()
d2.decompress(blob[:20])
print("needs_input while hungry:", d2.needs_input)

d = _lzma.LZMADecompressor()
print("cap 0:", d.decompress(blob, 0))
print("then all:", d.decompress(b"", -1) == DATA)

# --- trailing data ------------------------------------------------------------

print()
d = _lzma.LZMADecompressor()
print("with tail:", d.decompress(blob + b"and then some") == DATA)
print("unused_data:", d.unused_data)
d = _lzma.LZMADecompressor()
d.decompress(blob)
print("no tail:", d.unused_data)

# --- what is refused ----------------------------------------------------------

print()
CHAIN = [{"id": _lzma.FILTER_LZMA2}]
for kwargs in ({"format": _lzma.FORMAT_ALONE, "check": _lzma.CHECK_CRC32},
               {"preset": 1, "filters": CHAIN},
               {"format": _lzma.FORMAT_RAW},
               {"format": 17},
               {"preset": 10},
               {"preset": -1},
               {"preset": "foo"},
               {"preset": 2 ** 33}):
    try:
        _lzma.LZMACompressor(**kwargs)
        print("accepted %r" % (kwargs,))
    except Exception as e:
        print("%-44s %s: %s" % (str(kwargs)[:44], type(e).__name__, e))

print()
for kwargs in ({"format": _lzma.FORMAT_RAW},
               {"format": _lzma.FORMAT_XZ, "filters": CHAIN},
               {"format": _lzma.FORMAT_RAW, "filters": CHAIN,
                "memlimit": 1 << 24},
               {"memlimit": b"qw"},
               {"memlimit": -1},
               {"format": 17}):
    try:
        _lzma.LZMADecompressor(**kwargs)
        print("accepted %r" % (str(kwargs)[:40],))
    except Exception as e:
        print("%-44s %s: %s" % (str(kwargs)[:44], type(e).__name__, e))

print()
for f in ({}, "abc", [], CHAIN * 5, [b"wobsite"], [{"xyzzy": 3}],
          [{"id": 98765}], 3, [{"id": _lzma.FILTER_DELTA, "dist": "x"}],
          [{"id": _lzma.FILTER_LZMA2, "nosuch": 1}]):
    try:
        _lzma.LZMACompressor(_lzma.FORMAT_RAW, filters=f)
        print("accepted %r" % (f,))
    except Exception as e:
        print("%-34s %s: %s" % (repr(f)[:34], type(e).__name__, e))

print()
try:
    _lzma.LZMADecompressor().decompress(b"this is not an xz stream")
except _lzma.LZMAError as e:
    print("bad data: %s: %s" % (type(e).__name__, e))
d = _lzma.LZMADecompressor()
d.decompress(blob)
try:
    d.decompress(b"more")
except EOFError as e:
    print("after eof: %s: %s" % (type(e).__name__, e))
c = _lzma.LZMACompressor()
c.flush()
for fn in (lambda: c.compress(b"x"), lambda: c.flush()):
    try:
        fn()
    except ValueError as e:
        print("after flush:", e)
for bad in ("a string", 42, None):
    try:
        _lzma.LZMACompressor().compress(bad)
        print("accepted %r" % (bad,))
    except TypeError as e:
        print("%-10s %s" % (type(bad).__name__, e))

# --- the buffer protocol, and an uninitialised decompressor -------------------

print()
c = _lzma.LZMACompressor()
mv = memoryview(bytearray(DATA))
print("memoryview:", _lzma.LZMADecompressor().decompress(
    c.compress(mv) + c.flush()) == DATA)
try:
    _lzma.LZMACompressor().compress(memoryview(bytearray(DATA))[::2])
except BufferError as e:
    print("strided: %s: %s" % (type(e).__name__, e))

d = _lzma.LZMADecompressor.__new__(_lzma.LZMADecompressor)
print("uninitialised:", d.decompress(b""), d.eof, d.needs_input,
      d.unused_data, d.check)

for obj in (_lzma.LZMACompressor(), _lzma.LZMADecompressor()):
    try:
        obj.__reduce__()
        print("reduced", type(obj).__name__)
    except TypeError as e:
        print("%s: %s" % (type(obj).__name__, e))

# --- a big one ----------------------------------------------------------------

print()
big = DATA * 300
c = _lzma.LZMACompressor(preset=1)
blob = c.compress(big) + c.flush()
print("big round trip:", _lzma.LZMADecompressor().decompress(blob) == big,
      len(big), len(blob))

print("done")
