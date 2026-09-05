# A memoryview with a step is a view, not a refusal.
#
# `mv[::2]` and `mv[::-1]` raised NotImplementedError: the object carried no
# stride, so there was nowhere to put the step, and CPython answers with a
# non-contiguous view.  A stride has to be honoured by every reader --
# tobytes, iteration, bytes(), comparison, hex, tolist, and the write path --
# which is what made it more than a field.
#
# mv_buf points at the view's FIRST item, which for a negative step is the
# highest address in it, and mv_stride is the signed step in items.  A slice
# of a slice multiplies the two.

b = bytearray(b"abcdefghij")
m = memoryview(b)


def show(label, fn):
    try:
        print(label.ljust(24), fn())
    except Exception as exc:
        print(label.ljust(24), type(exc).__name__ + ":", exc)


print("=== reading ===")
show("every second", lambda: bytes(m[::2]))
show("reversed", lambda: bytes(m[::-1]))
show("start, stop, step", lambda: bytes(m[1:8:3]))
show("backwards, stepped", lambda: bytes(m[8:2:-2]))
show("len", lambda: len(m[::2]))
show("len, reversed", lambda: len(m[::-1]))
show("empty", lambda: bytes(m[5:5:2]))
show("step past the end", lambda: bytes(m[::100]))
show("step of one", lambda: bytes(m[::1]))
show("tolist", lambda: m[::2].tolist())
show("tobytes", lambda: m[::2].tobytes())
show("hex", lambda: m[::2].hex())
show("iteration", lambda: list(m[::-1]))
show("indexing", lambda: (m[::2][0], m[::2][4]))
show("negative index", lambda: m[::2][-1])
show("out of range", lambda: m[::2][5])
show("step of zero", lambda: m[::0])

print("=== a slice of a slice ===")
show("twice", lambda: bytes(m[::2][::2]))
show("reverse twice", lambda: bytes(m[::-1][::-1]))
show("slice of reversed", lambda: bytes(m[::-1][2:5]))
show("reversed slice", lambda: bytes(m[2:8][::-1]))

print("=== comparison, and being contained ===")
show("== bytes", lambda: m[::2] == b"acegi")
show("!= bytes", lambda: m[::2] == b"acegj")
show("bytes ==", lambda: b"acegi" == m[::2])
show("view == view", lambda: m[::2] == memoryview(b"acegi"))
show("reversed ==", lambda: m[::-1] == b"jihgfedcba")
show("in", lambda: ord("c") in m[::2])
show("not in", lambda: ord("b") in m[::2])

print("=== what a stride refuses ===")
show("contiguous?", lambda: m[::2].c_contiguous)
show("contiguous, step 1", lambda: m[1:4].c_contiguous)
show("cast", lambda: m[::2].cast("I"))
show("int()", lambda: int(memoryview(b"12345")[::2]))

print("=== the attributes come across ===")
show("nbytes", lambda: m[::2].nbytes)
show("itemsize", lambda: m[::2].itemsize)
show("format", lambda: m[::2].format)
show("readonly", lambda: m[::2].readonly)
show("obj", lambda: m[::2].obj is b)

print("=== writing ===")
v = m[::2]
v[0] = 65
show("one item", lambda: bytes(b))
v[slice(1, 4)] = b"XYZ"
show("a slice", lambda: bytes(b))
r = m[::-1]
r[0] = 90
show("through a reverse", lambda: bytes(b))
show("read-only refuses", lambda: memoryview(b"abc")[::-1].__setitem__(0, 1))

print("=== hashing ===")
show("read-only view", lambda: hash(memoryview(b"abc")[::-1]) == hash(b"cba"))
show("writable view", lambda: hash(m[::2]))
show("read-only, plain", lambda: hash(memoryview(b"abc")) == hash(b"abc"))

print("=== bytes() over a cast view, which has items wider than a byte ===")
c = memoryview(bytearray(range(16)))
show("cast to I", lambda: bytes(c.cast("I")))
show("cast to H", lambda: bytes(c.cast("H")))
show("cast then step", lambda: bytes(c.cast("I")[::2]))
show("cast len", lambda: len(c.cast("I")[::2]))
print("done")
