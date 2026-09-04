# repr() of a float has to be the SHORTEST decimal that reads back as it.
#
# Trying "%.*g" at rising precision finds the shortest of the forms glibc
# produces, which is not always the shortest that exists: at an exact
# half-way case glibc rounds to even, and it is the other neighbour that
# round-trips.  repr(2.0**-24) came out with seventeen digits where CPython
# prints sixteen -- "5.960464477539062e-08", glibc's correctly-rounded
# sixteen, does not read back as 2**-24, and "...063" does.
#
# About one ordinary value in a hundred hits it.


def check(vals, label):
    bad = []
    for v in vals:
        r = repr(v)
        if float(r) != v:
            bad.append(("not round-trip", v, r))
            continue
        # Nothing shorter may round-trip either.
        digits = sum(1 for c in r if c.isdigit())
        for k in range(1, 17):
            cand = "%.*g" % (k, v)
            if float(cand) == v and sum(1 for c in cand if c.isdigit()) < digits:
                bad.append(("not shortest", v, r, cand))
                break
    print("%-22s %d values, %d wrong" % (label, len(vals), len(bad)))
    for b in bad[:4]:
        print("   ", b)


# The two that were wrong, by name.
print(repr(2.0 ** -24))
print(repr(2.0 ** -44))

check([2.0 ** -n for n in range(0, 80)], "powers of two down")
check([2.0 ** n for n in range(0, 80)], "powers of two up")
check([1.0 / n for n in range(1, 300)], "reciprocals")
check([n / 7.0 for n in range(1, 300)], "sevenths")
check([n * 1.1 for n in range(1, 300)], "multiples of 1.1")
check([1e-300, 1e300, 5e-324, 1.7976931348623157e308, 0.0, -0.0,
       0.1, 0.2, 0.3, 1 / 3, 2 / 3, 1e16, 1e17, 123456789012345678.0],
      "assorted")

# The notation rule is unchanged: fixed inside [-4, 16), exponential outside.
for v in (1e-5, 1e-4, 1e15, 1e16, 100.0, 0.0001, 1234567890123456.0):
    print(repr(v))

print("done")
