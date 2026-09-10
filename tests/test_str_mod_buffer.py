# str_mod builds its answer in a heap buffer that doubles as needed.  It used
# to start at 8192 bytes -- for every `%` operation, including one whose
# answer is eight bytes -- which meant the growth path was essentially never
# taken and the terminating NUL had a kilobyte of slack behind it.  It starts
# at 256 now, so both are live: every append has to ask for room including
# that NUL, and the doubling has to actually work.
#
# The sizes below straddle the initial capacity in both directions and land
# exactly on it, which is where an off-by-one in the terminator shows.
for n in (0, 1, 100, 200, 250, 251, 252, 253, 254, 255, 256, 257, 300, 1000, 5000):
    s = "%s" % ("x" * n)
    if len(s) != n or s != "x" * n:
        print("BAD", n)
    t = "%s-%s" % ("y" * n, "z" * n)
    if len(t) != 2 * n + 1:
        print("BAD2", n)
print("boundaries ok")
print("%s-%d" % ("abc", 7), "%d%%" % 50, "%05.2f|%x|%o|%e" % (3.14159, 255, 8, 12345.678))
print("%r %s %a" % ("q", None, "é"), "%c%c" % (65, "B"))
print("%(a)s/%(b)d" % {"a": "m", "b": 2})
print(b"%s-%d" % (b"abc", 7))
big = "%s" % ("q" * 100000)
print(len(big), big[:3], big[-3:])
