# hash(float) was the truncated integer for an integral value and an xor of
# the raw bits otherwise, so it bore no relation to CPython's and, worse,
# hash(2**61) != hash(float(2**61)) -- an int and an equal float landed in
# different dict slots.  It is _Py_HashDouble now.

vals = [0.0, -0.0, 1.0, -1.0, 1.5, -1.5, 0.1, 2.5, 3.0, 1e16, 1e17,
        2.0 ** 52, 2.0 ** 53, 2.0 ** 61, -(2.0 ** 61), 2.0 ** 62,
        1e308, 1e-308, 5e-324, 0.3333333333333333, float("inf"),
        float("-inf")]
for v in vals:
    print(v, hash(v))

ints = [0, 1, -1, 2, 7, -7, 2 ** 30, 2 ** 60, 2 ** 61 - 1, 2 ** 61,
        2 ** 61 + 1, 2 ** 62, -(2 ** 61), 2 ** 100, -(2 ** 100), True, False]
for n in ints:
    print(n, hash(n))

# An int and an equal float must hash alike, or a dict cannot find them
for n in [0, 1, -1, 2 ** 52, 2 ** 53, 2 ** 61, 2 ** 62, -(2 ** 61)]:
    print(n, hash(n) == hash(float(n)))

d = {1: "int", 2.5: "float"}
print(d[1.0], d[2.5], 1.0 in d, 1 in d)
print({1} == {1.0}, len({1, 1.0, True, 1 + 0}), sorted({1.0, 2, 3.5}))

s = set()
s.add(2 ** 61)
print(float(2 ** 61) in s, (2 ** 61) in s)

# hash(-1) is never -1
print(hash(-1), hash(-1.0), hash(-2))
