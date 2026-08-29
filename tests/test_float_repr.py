# repr picked the shortest digit count that round-trips, then let %g choose
# the notation -- and %g goes exponential as soon as the exponent reaches the
# precision, so repr(100.0) was "1e+02".  CPython chooses the digits first
# and the notation second: fixed when the decimal exponent is in [-4, 16).

vals = [
    100.0, 0.1, 1.5, 1e100, 1e16, 1e15, 0.0001, 0.00001, -0.0, 0.0, 1.0,
    2.5, 1e-300, 1.7976931348623157e308, 3.14159265358979, 1 / 3,
    123456789.0, -100.0, 1e-5, 5e-324, 2.0 ** 53, 0.30000000000000004,
    -1e16, 999999999999999.0, 1e-4, 12345.6789, 2.0, 1e17,
]
for v in vals:
    print(repr(v))

print(repr(float("inf")), repr(float("-inf")), repr(float("nan")))
print(str(100.0), str(0.1), f"{100.0}", "%s" % 100.0)
print([100.0, 0.1], (1e16,), {1.5})
