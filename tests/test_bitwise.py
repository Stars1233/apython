# Test bitwise operations, shifts, power, true division
print(5 & 3)     # 1
print(5 | 3)     # 7
print(5 ^ 3)     # 6
print(~5)        # -6
print(~0)        # -1
print(~(-1))     # 0

# Shifts
print(1 << 10)   # 1024
print(1024 >> 3)  # 128
print(7 >> 1)    # 3
print(-8 >> 2)   # -2

# Power
print(2 ** 10)   # 1024
print(3 ** 3)    # 27
print(10 ** 0)   # 1

# True division (int / int -> float)
print(7 / 2)     # 3.5
print(10 / 5)    # 2.0
print(1 / 3)     # 0.3333333333333333

# Inplace variants (same as regular for ints)
x = 5
x &= 3
print(x)         # 1
x = 5
x |= 3
print(x)         # 7
x = 5
x ^= 3
print(x)         # 6
x = 1
x <<= 10
print(x)         # 1024
x = 1024
x >>= 3
print(x)         # 128

# Large int power
print(2 ** 32)   # 4294967296
