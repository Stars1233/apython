# mmap, as a _mmapcore shim with the object in Python.
#
# `import mmap` was a ModuleNotFoundError, which takes test_mmap's 46 tests
# with it at collection.  The split is zlib's: the mmap/munmap/mremap/msync
# calls, the raw bytes and the search over them are src/modules/mmap.asm,
# bounds-checked there against the handle's own length; the object, the
# cursor, the ACCESS_* translation, readline and the slice protocol are
# lib/mmap.py.
#
# The one thing this cannot do is the BUFFER protocol: a Python class has no
# tp_as_buffer, so memoryview(m) refuses and so does anything -- re.search,
# say -- that wants one.  That is recorded in DIVERGENCES.md, and it is the
# single test in CPython's test_mmap that still fails.
import os
import sys

import mmap

PAGESIZE = mmap.PAGESIZE
TMP = os.path.join(os.environ.get("TMPDIR", "/tmp"),
                   "apython-mmap-%d" % os.getpid())


def written(data):
    with open(TMP, "wb") as f:
        f.write(data)
    return TMP


# --- an anonymous mapping ----------------------------------------------------

m = mmap.mmap(-1, 64)
print("len:", len(m), "tell:", m.tell(), "first:", m[0])
print("zeroed:", m[:] == b"\0" * 64)
print("written:", m.write(b"hello world"), "tell:", m.tell())
print("read back:", m[:11])
m.seek(0)
print("read 5:", m.read(5), "byte:", m.read_byte(), "line:", m.readline())
print("read to end:", m.read()[:8], "tell:", m.tell())
print("read at end:", m.read())
m.seek(0)
print("readline again:", m.readline())
m.close()
print("closed:", m.closed)
m.close()
print("close twice is fine")

# --- the operations that need a real file ------------------------------------

print()
path = written(b"0123456789" * 10)
with open(path, "r+b") as f:
    m = mmap.mmap(f.fileno(), 0)
    print("whole file:", len(m), m[:10])
    print("size():", m.size())
    m[0:3] = b"abc"
    m.flush()
    print("flush returns:", m.flush())
    m.close()
print("on disk:", open(path, "rb").read(10))

# A window into the middle, which is what an offset is for.
with open(path, "r+b") as f:
    m = mmap.mmap(f.fileno(), 10, offset=0)
    print("windowed:", len(m), m[:])
    m.close()

# resize() moves the mapping and truncates the file with it.
with open(path, "r+b") as f:
    m = mmap.mmap(f.fileno(), 0)
    m.resize(20)
    print("resized:", len(m), m[:20], m.size())
    m.close()
print("file now:", os.path.getsize(path))
os.unlink(path)

# --- what is refused ---------------------------------------------------------

print()
for length, kwargs in ((-1, {}), (0, {}), (16, {"offset": -1}),
                       (16, {"offset": 1}), (16, {"access": 4})):
    try:
        mmap.mmap(-1, length, **kwargs)
        print("accepted", length, kwargs)
    except (ValueError, OverflowError, TypeError, OSError) as e:
        # A zero-length ANONYMOUS mapping is not "cannot mmap an empty file":
        # there is no file, and CPython lets mmap(2) refuse it, so the EINVAL
        # arrives as an OSError.
        print("%-24s %s: %s" % ("%d %r" % (length, kwargs),
                                type(e).__name__, e))

# access and (flags, prot) are two spellings of one thing, and CPython refuses
# to be given both.
try:
    mmap.mmap(-1, 16, flags=mmap.MAP_PRIVATE, prot=mmap.PROT_READ,
              access=mmap.ACCESS_WRITE)
except ValueError as e:
    print("both:", e)

# An empty file has nothing to map.
path = written(b"")
with open(path, "rb") as f:
    try:
        mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)
    except ValueError as e:
        print("empty file:", e)
os.unlink(path)

# --- read-only and copy-on-write ---------------------------------------------

print()
path = written(b"a" * 32)
with open(path, "rb") as f:
    m = mmap.mmap(f.fileno(), 32, access=mmap.ACCESS_READ)
    print("read-only reads:", m[:4])
    for fn in (lambda: m.write(b"x"), lambda: m.write_byte(1),
               lambda: m.__setitem__(0, 1),
               lambda: m.__setitem__(slice(0, 2), b"xy"),
               lambda: m.resize(64), lambda: m.move(0, 1, 2)):
        try:
            fn()
            print("allowed a write")
        except TypeError as e:
            print("refused:", e)
    m.close()

with open(path, "r+b") as f:
    m = mmap.mmap(f.fileno(), 32, access=mmap.ACCESS_COPY)
    m[0:4] = b"zzzz"
    print("copy sees:", m[:4])
    m.close()
print("file unchanged:", open(path, "rb").read(4))

# prot without PROT_WRITE is read-only too, and has to be refused rather than
# attempted: the write would be a SIGSEGV, not an error.
with open(path, "rb") as f:
    m = mmap.mmap(f.fileno(), 32, prot=mmap.PROT_READ)
    try:
        m.write(b"foo")
    except TypeError as e:
        print("prot read-only:", e)
    m.close()
os.unlink(path)

# --- find and rfind ----------------------------------------------------------

print()
m = mmap.mmap(-1, 32)
m[:] = b"the quick brown fox jumps to it "
print("find:", m.find(b"quick"), m.find(b"o"), m.rfind(b"o"), m.find(b"zzz"))
print("windowed:", m.find(b"o", 20), m.find(b"o", 0, 10), m.rfind(b"o", 0, 20))
print("negative:", m.find(b"it", -8), m.rfind(b"t", -4))
print("empty needle:", m.find(b""), m.rfind(b""))
print("from pos:", (m.seek(10), m.find(b"o"))[1])
print("bytearray needle:", m.find(bytearray(b"fox")))

# --- move, which overlaps ----------------------------------------------------

print()
DATA = b"0123456789"
for dest in range(len(DATA)):
    for src in range(len(DATA)):
        for count in range(len(DATA) - max(dest, src)):
            want = DATA[:dest] + DATA[src:src + count] + DATA[dest + count:]
            mm = mmap.mmap(-1, len(DATA))
            mm[:] = DATA
            mm.move(dest, src, count)
            if mm[:] != want:
                print("move(%d,%d,%d): %r want %r"
                      % (dest, src, count, mm[:], want))
            mm.close()
print("every move agrees")

mm = mmap.mmap(-1, 16)
for args in ((-1, 0, 4), (0, -1, 4), (0, 0, -1), (0, 0, 100), (14, 0, 4)):
    try:
        mm.move(*args)
        print("accepted", args)
    except ValueError:
        print("refused", args)
mm.close()

# --- the slice protocol ------------------------------------------------------

print()
s = bytes(reversed(range(256)))
m = mmap.mmap(-1, len(s))
m[:] = s
print("round trip:", m[:] == s)
indices = (0, None, 1, 3, 19, 300, sys.maxsize, -1, -2, -31, -300)
bad = 0
for start in indices:
    for stop in indices:
        for step in indices[1:]:
            if m[start:stop:step] != s[start:stop:step]:
                bad += 1
print("extended getslice mismatches:", bad)

bad = 0
for start in indices:
    for stop in indices:
        for step in indices[1:]:
            m[:] = s
            want = list(s)
            data = bytes(reversed(want[start:stop:step]))
            want[start:stop:step] = data
            m[start:stop:step] = data
            if m[:] != bytes(want):
                bad += 1
print("extended setslice mismatches:", bad)

m[:] = s
print("index:", m[0], m[-1], m[255])
for bad_index in (256, -257, 1000):
    try:
        m[bad_index]
        print("accepted", bad_index)
    except IndexError as e:
        print("index refused:", e)
try:
    m[0:2] = b"abc"
except IndexError as e:
    print("wrong size:", e)
for fn in (lambda: m.__delitem__(0), lambda: m.__delitem__(slice(0, 2))):
    try:
        fn()
    except TypeError as e:
        print("deletion:", e)
m.close()

# --- seek, tell and the cursor ----------------------------------------------

print()
m = mmap.mmap(-1, 32)
m.seek(10)
print("abs:", m.tell())
m.seek(5, 1)
print("rel:", m.tell())
m.seek(-4, 2)
print("end:", m.tell())
for args in ((33, 0), (-1, 0), (100, 1), (1, 2), (0, 9)):
    try:
        m.seek(*args)
        print("accepted", args, m.tell())
    except ValueError as e:
        print("seek refused %r: %s" % (args, e))
m.close()

# --- madvise -----------------------------------------------------------------

print()
m = mmap.mmap(-1, 2 * PAGESIZE)
m.madvise(mmap.MADV_NORMAL)
m.madvise(mmap.MADV_WILLNEED, PAGESIZE)
m.madvise(mmap.MADV_NORMAL, 0, PAGESIZE)
print("madvise accepted")
for args, kind in (((mmap.MADV_NORMAL, 2 * PAGESIZE), ValueError),
                   ((mmap.MADV_NORMAL, -1), ValueError),
                   ((mmap.MADV_NORMAL, 0, -1), ValueError),
                   ((mmap.MADV_NORMAL, PAGESIZE, sys.maxsize), OverflowError)):
    try:
        m.madvise(*args)
        print("accepted", args)
    except kind as e:
        print("%-14s %s" % (type(e).__name__, e))
m.close()

# --- repr, the context manager, and a subclass -------------------------------

print()
with mmap.mmap(-1, 16) as m:
    print(repr(m))
    m.write(b"abc")
    print(repr(m))
print(repr(m))


class Anon(mmap.mmap):
    def __new__(cls, *args, **kwargs):
        return mmap.mmap.__new__(cls, -1, *args, **kwargs)


a = Anon(PAGESIZE)
print("subclass:", len(a), isinstance(a, mmap.mmap))
a.close()

# Using a closed mapping is a ValueError, whichever way it is reached.
m = mmap.mmap(-1, 16)
m.close()
for fn in (lambda: len(m), lambda: m[0], lambda: m[0:2], lambda: m.read(1),
           lambda: m.find(b"a"), lambda: m.seek(0), lambda: m.flush(),
           lambda: m.madvise(mmap.MADV_NORMAL), lambda: m.size()):
    try:
        fn()
        print("allowed on a closed mapping")
    except ValueError as e:
        print("closed:", e)

# An __index__ with a side effect runs where CPython's runs: before anything
# else, with the mapping re-checked after.
closing = mmap.mmap(-1, 16)


class Closes:
    def __index__(self):
        closing.close()
        return 0


try:
    closing[Closes():8]
except ValueError as e:
    print("closed mid-call:", e)

print("done")
