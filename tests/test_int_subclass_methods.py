# Every int method, on an int subclass instance.
#
# bit_length, bit_count, conjugate and to_bytes read PyIntObject.compact at
# +40 of a 32-byte PyIntSubclassObject without unwrapping first, so
# I(7).bit_length() was 0 and I(258).to_bytes(2, 'big') was b'\x00\x00'.
# Under valgrind it was an invalid read, and the garbage could be a limb
# pointer handed to GMP.  int_method_self_to_i64 unwraps now; this is what
# keeps it that way.

class I(int):
    pass

class J(int):
    def __init__(self, *a):
        self.tag = "j"

for cls in (int, I, J):
    for n in (0, 1, 7, 255, 256, 258, 1023, -1, -7, -256):
        v = cls(n)
        print(cls.__name__, n, v.bit_length(), v.bit_count(), v.conjugate())

for cls in (int, I):
    for n, size in ((258, 2), (0, 1), (1, 1), (65535, 2), (7, 4)):
        v = cls(n)
        print(cls.__name__, n, v.to_bytes(size, "big"), v.to_bytes(size, "little"))

# The values themselves still behave as ints.
i = I(258)
print(i, int(i), i + 1, i * 2, i == 258, hash(i) == hash(258))
print(i.real, i.imag, i.numerator, i.denominator)

# A big one, past the immediate range and into GMP.
big = I(2 ** 70 + 3)
print(big.bit_length(), big.bit_count(), big + 1 == 2 ** 70 + 4)
print(int(big) == 2 ** 70 + 3)

# bool is an int subclass of its own.
print(True.bit_length(), True.bit_count(), False.bit_length(), True.to_bytes(1, "big"))
