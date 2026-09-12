# A type can say where its bytes are, and everything that reads bytes asks.
#
# There was no buffer protocol here: bytes_like_ptr_len -- the funnel some forty
# call sites go through, from memoryview() and bytes() to FileIO.write, marshal,
# sre and the twenty-odd bytes methods -- was a hard-coded chain of bytes,
# bytearray and memoryview.  Anything else was refused, so
# `memoryview(array.array('i', [1, 2]))` was "a bytes-like object is required"
# and array.frombytes took an exact bytes and nothing else.
#
# PyTypeObject now carries a tp_as_buffer, answering the one question every one
# of those callers asks: where is the memory, and how much of it is there.  The
# funnel consults it when its own chain misses, so one filled-in slot reaches
# all forty at once.  array is the first to fill it in.
#
# Two deliberate differences, both recorded in DIVERGENCES.md, so this file
# tests only what agrees.  A view obtained through the slot is READ-ONLY, where
# CPython's over an array is writable; and it is a view of BYTES -- format 'B',
# itemsize 1 -- where CPython's keeps the array's own format, so len() and
# per-item indexing differ.  The bytes themselves are identical, which is what
# every caller of the funnel actually reads.

import array

# --- memoryview over an array -----------------------------------------------
a = array.array("i", [1, 2])
m = memoryview(a)
print(bytes(m), "the bytes of an int array")
print(m.tobytes(), "and tobytes() says the same")
print(bytes(memoryview(array.array("i"))), "an empty array")
print(bytes(memoryview(array.array("d", [1.5]))), "a double array")
print(bytes(memoryview(array.array("b", [1, -1]))), "a signed byte array")

# The view keeps its source alive.
del a
print(bytes(m), "the view outlives the name that made it")
print(len(bytes(m)), "eight bytes, whatever the view calls its items")

# --- bytes() and the rest of the funnel -------------------------------------
print(bytes(array.array("i", [1, 2])), "bytes() of an array is its raw bytes")
print(array.array("i", [258]).tobytes(), "and tobytes agrees")

# --- frombytes takes anything bytes-like ------------------------------------
# bytes, bytearray and memoryview -- and NOT another array, which CPython
# refuses because it asks for PyBUF_SIMPLE and an exporter carrying its own
# format declines that.
for src in (b"\x07\x00\x00\x00", bytearray(b"\x07\x00\x00\x00"),
            memoryview(b"\x07\x00\x00\x00")):
    got = array.array("i")
    got.frombytes(src)
    print(type(src).__name__, got.tolist(), "frombytes")

# --- and still refuses what is not a buffer ---------------------------------
for bad in (42, "text", None, [1, 2], object()):
    try:
        memoryview(bad)
        print("NO ERROR", type(bad).__name__)
    except TypeError as e:
        print(type(bad).__name__, "->", e)

try:
    array.array("i").frombytes(array.array("i", [7]))
    print(False, "another array must be refused")
except TypeError:
    print(True, "frombytes refuses another array, as CPython does")

try:
    array.array("i").frombytes("text")
    print(False, "a str must be refused")
except TypeError as e:
    print(True, "frombytes refuses a str")

try:
    array.array("i").frombytes(b"\x01")
    print(False, "a short buffer must be refused")
except ValueError as e:
    print(True, "frombytes refuses a partial item")

# --- what did not change ----------------------------------------------------
print(bytes(memoryview(b"ab")), bytes(memoryview(bytearray(b"cd"))),
      "bytes and bytearray views are untouched")
print(memoryview(b"ab").readonly, memoryview(bytearray(b"ab")).readonly,
      "and so is their readonly answer")


# --- an exported buffer pins the exporter's storage --------------------------
#
# A view through the slot holds a POINTER into the exporter's memory, and an
# array's buffer moves when it grows: array_reserve reallocs.  So the view was
# left aimed at freed memory, and reading it printed whatever the allocator had
# put there -- a use-after-free reachable from two lines of ordinary Python.
#
# CPython's answer is to refuse the resize while a view is outstanding, and to
# count the outstanding views so that the refusal lifts when the last one goes.
# bytearray already worked that way here; the slot exporter now does too.
def delete_first(arr):
    del arr[0]


a = array.array("i", [1, 2, 3, 4])
m = memoryview(a)
print(bytes(m) == a.tobytes(), "the view agrees with the array")

for label, resize in (("append", lambda: a.append(5)),
                      ("extend", lambda: a.extend([6, 7])),
                      ("fromlist", lambda: a.fromlist([8])),
                      ("frombytes", lambda: a.frombytes(b"\x09\x00\x00\x00")),
                      ("del", lambda: delete_first(a))):
    try:
        resize()
        print("NO ERROR: %s resized an array that is exporting buffers" % label)
    except BufferError as e:
        print("%-10s BufferError: %s" % (label, e))

# Everything that does not resize is still allowed.
a[0] = 99
print(a.tolist(), "assignment to an existing item is fine")
print(bytes(m)[:4] == b"\x63\x00\x00\x00", "and the view sees it")
print(a.tolist() == list(a), "iteration is fine")
print(len(a), a.buffer_info()[1], "so are len and buffer_info")

# The refusal lifts when the last view goes.
del m
a.append(5)
print(a.tolist(), "the array grows once nothing is exporting it")

# Two views, and it takes both.
m1 = memoryview(a)
m2 = memoryview(a)
del m1
try:
    a.append(6)
    print("NO ERROR: one release was enough for two views")
except BufferError:
    print("two views take two releases")
del m2
a.append(6)
print(len(a), "and then it grows")
