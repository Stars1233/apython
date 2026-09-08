# Overlapping copies at every size and in both directions.
#
# ap_memmove picks one of three arms -- forward, backward, or "the regions do
# not actually overlap, so forward is safe after all" -- from the pointers and
# the length.  Each arm has a qword body and a byte remainder, so the seams
# are at multiples of 8 and the interesting sizes are the ones either side.
# list.insert(0, x) and list.pop(0) shift by exactly one slot, which is the
# maximally-overlapping case; bytearray slice assignment shifts by more.

def shift_up(n):
    # insert at the front: every element moves one slot toward higher memory,
    # which is the backward-copy arm.
    a = list(range(n))
    a.insert(0, -1)
    return a

def shift_down(n):
    # pop from the front: every element moves one slot down, the forward arm.
    a = list(range(n))
    if n:
        a.pop(0)
    return a

def shift_mid(n):
    a = list(range(n))
    a.insert(n // 2, -1)
    a.pop(n // 3 if n else 0)
    return a

# Every size across the qword seams, plus a few page-sized ones.
sizes = list(range(0, 41)) + [63, 64, 65, 127, 128, 129, 511, 512, 513, 4095, 4096, 4097]

for n in sizes:
    u = shift_up(n)
    assert u == [-1] + list(range(n)), (n, u[:5])
    d = shift_down(n)
    assert d == list(range(1, n)), (n, d[:5])
    if n:
        shift_mid(n)
print("list shifts:", len(sizes), "sizes")

# bytearray: overlapping slice assignment, both directions, every shift
# distance across the qword seam.
for n in [0, 1, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 100]:
    for k in [1, 2, 3, 7, 8, 9, 16]:
        if n <= k:
            continue
        b = bytearray(range(n % 256)) if n < 256 else bytearray(n)
        base = bytes(b)
        # move the tail down over the head (dst < src)
        b[0:n - k] = base[k:n]
        assert bytes(b) == base[k:n] + base[n - k:n], (n, k, "down")
        # move the head up over the tail (dst > src)
        b = bytearray(base)
        b[k:n] = base[0:n - k]
        assert bytes(b) == base[0:k] + base[0:n - k], (n, k, "up")
print("bytearray slices: ok")

# del from the middle of a bytearray, which shifts the tail down by the
# deleted length -- the disjoint-after-all case when the deletion is large.
for n in [16, 32, 64, 100]:
    for cut in [1, 8, 9, n // 2]:
        b = bytearray(bytes(range(n % 256)) if n < 256 else bytes(n))
        base = bytes(b)
        del b[0:cut]
        assert bytes(b) == base[cut:], (n, cut)
print("bytearray del: ok")

# The result of every arm, printed, so run_tests.sh diffs it against CPython
# rather than only trusting the asserts above.
print(shift_up(9))
print(shift_down(17))
print(bytes(bytearray(b"abcdefghijklmnop")[3:]))
b = bytearray(b"0123456789abcdef")
b[2:14] = b"ABCDEFGHIJKL"
print(bytes(b))
