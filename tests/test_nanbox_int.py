# Integer boundary behaviour around the NaN-box immediate range [-2^50, 2^50).
# Values inside the range are immediates; outside they are heap PyIntObjects.
# Both representations must be indistinguishable from Python.

LO = -(2**50)
HI = 2**50

boundary = [
    0, 1, -1, 2, -2,
    HI - 2, HI - 1, HI, HI + 1, HI + 2,
    LO + 1, LO, LO - 1, LO - 2,
    2**62, -(2**62),
    2**63 - 1, -(2**63),
    2**64, 2**100, -(2**100),
]

print("--- repr round-trip ---")
for v in boundary:
    assert int(str(v)) == v, v
    print(v)

print("--- arithmetic across the boundary ---")
print((HI - 1) + 1)
print((HI - 1) + 2)
print(HI - 1)
print(HI + HI)
print(HI * 2)
print(HI * HI)
print((LO + 1) - 1)
print((LO + 1) - 2)
print(LO * -1)
print(-HI)
print(-(HI - 1))
print(abs(LO))
print((HI + 5) - HI)
print((HI + 5) - (HI + 5))
print((2**100) // (2**50))
print((2**100) % (2**50 + 1))
print(divmod(2**100, 3**20))

print("--- comparisons across representations ---")
print(HI == HI, HI == HI + 0, (HI - 1) == (HI - 1))
print(HI > HI - 1, LO < LO + 1, HI - 1 < HI)
print(sorted([HI, LO, 0, HI - 1, LO + 1, 2**100, -(2**100)]))
print((HI - 1) == float(HI - 1))
print(min(HI, HI - 1), max(LO, LO + 1))

print("--- hash / dict / set identity across the boundary ---")
d = {}
for v in boundary:
    d[v] = str(v)
print(len(d))
for v in boundary:
    assert d[v] == str(v), v
print(d[HI], d[HI - 1], d[LO])
print(hash(HI) == hash(HI + 0))
print(hash(HI - 1) == hash(HI - 1))
print(hash(3) == hash(3.0))
print(len(set(boundary)) == len(set(boundary)))

print("--- bit operations ---")
print(HI >> 1, HI << 1, (HI - 1) >> 1, (HI - 1) << 1)
print(HI & (HI - 1), HI | 1, HI ^ HI)
print(~HI, ~(HI - 1), ~LO)
print((2**50).bit_length(), (2**50 - 1).bit_length(), (-(2**50)).bit_length())

print("--- bool is an int ---")
print(True + (HI - 1))
print(True + HI)
print(False + LO)
print(True == 1, False == 0, hash(True) == hash(1))

print("--- int subclass ---")
class MyInt(int):
    pass

for v in (0, HI - 1, HI, LO, 2**100):
    m = MyInt(v)
    print(int(m), m == v, m + 1, isinstance(m, int), type(m) is MyInt)

print("--- conversions ---")
print(int(float(2**50)))
print(int("1125899906842624"), int("-1125899906842625"))
print(int(str(2**100)))
print(len(str(2**100)))
print(hex(HI), hex(HI - 1), oct(HI), bin(HI - 1)[:12])
