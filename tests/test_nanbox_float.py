# Float edge cases for the NaN-box encoding.  Every double must survive the
# +2^48 offset encoding bit-exactly, except tag-space NaNs which are purified
# to the canonical quiet NaN (unobservable: CPython renders every NaN "nan").

inf = float("inf")
ninf = float("-inf")
nan = float("nan")

print("--- signed zero ---")
print(repr(0.0), repr(-0.0))
print(0.0 == -0.0)
print(repr(0.0 + 0.0), repr(-0.0 + -0.0), repr(0.0 * -1.0), repr(-0.0 * -1.0))
print(repr(-(0.0)), repr(-(-0.0)))
print(repr(0.0 - 0.0), repr(-0.0 - 0.0))
print(str(-0.0), str(0.0))

print("--- infinities ---")
print(repr(inf), repr(ninf), repr(-inf))
print(inf > 0, ninf < 0, inf == inf, inf != ninf)
print(repr(inf + 1.0), repr(inf * 2.0), repr(ninf * 2.0), repr(1.0 / inf))
print(repr(inf + inf), repr(ninf + ninf))
print(inf > 2**100, ninf < -(2**100))

print("--- NaN, including the x86 default QNaN from inf-inf ---")
print(repr(nan))
print(nan == nan, nan != nan, nan < 1.0, nan > 1.0)
d = inf - inf                # hardware default QNaN: 0xFFF8000000000000
print(repr(d), d != d)
e = inf + ninf
print(repr(e), e != e)
f = inf / inf
print(repr(f), f != f)
g = 0.0 * inf
print(repr(g), g != g)
print(repr(nan + 1.0), repr(nan * 0.0), repr(-nan), repr(abs(nan)))
print(repr(nan - nan), repr(d + d), repr(d * 2.0))
print((nan != nan) and (d != d) and (e != e) and (f != f) and (g != g))

print("--- subnormals and extremes ---")
tiny = 5e-324
print(repr(tiny), tiny > 0.0)
print(repr(tiny / 2.0), repr(-tiny))
big = 1.7976931348623157e308
print(repr(big), repr(-big))
print(repr(big * 2.0), repr(-big * 2.0))
print(repr(big / big), repr(tiny * 0.5))
print(repr(2.2250738585072014e-308))

print("--- ordinary arithmetic round-trips ---")
vals = [0.5, -0.5, 1.0, -1.0, 3.14159265358979, 2.718281828459045,
        1e-300, 1e300, -1e-300, -1e300, 0.1, 0.2, 1.0 / 3.0]
for v in vals:
    print(repr(v), repr(-v), repr(v * 2.0), repr(v / 2.0))
print(repr(0.1 + 0.2))
print(0.1 + 0.2 == 0.3)

print("--- hash and dict keys ---")
print(hash(3) == hash(3.0), hash(-0.0) == hash(0.0), hash(1.5) == hash(1.5))
m = {0.0: "zero", 1.5: "one-and-a-half", inf: "inf", ninf: "ninf"}
print(len(m), m[0.0], m[-0.0], m[1.5], m[inf], m[ninf])
m[2.0] = "two"
print(m[2], len(m))

print("--- mixed int/float ---")
print(1 == 1.0, 1 < 1.5, 2**50 == float(2**50))
print(repr(1 + 0.5), repr(2**50 + 0.0), repr(float(2**50)))
print(repr(float(2**50 - 1)), repr(float(-(2**50))))
print(int(2.9), int(-2.9), round(2.5), round(3.5), round(-2.5))
print(repr(7 / 2), repr(-7 / 2), 7 // 2, -7 // 2, 7 % 3, -7 % 3)
print(sorted([1.5, 1, 2**50, -0.5, inf, ninf, 0]))
