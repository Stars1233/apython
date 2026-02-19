# Test float methods

# is_integer
print((1.0).is_integer())     # True
print((1.5).is_integer())     # False
print((0.0).is_integer())     # True
print((-2.0).is_integer())    # True
print((3.14).is_integer())    # False

# conjugate
print((3.14).conjugate())     # 3.14
print((-2.5).conjugate())     # -2.5

# as_integer_ratio
print((0.5).as_integer_ratio())    # (1, 2)
print((1.5).as_integer_ratio())    # (3, 2)
print((2.0).as_integer_ratio())    # (2, 1)
print((0.0).as_integer_ratio())    # (0, 1)
print((-0.5).as_integer_ratio())   # (-1, 2)

# hex
print((0.0).hex())            # 0x0.0p+0
print((1.0).hex())            # 0x1.0000000000000p+0
print((-1.0).hex())           # -0x1.0000000000000p+0
print((0.5).hex())            # 0x1.0000000000000p-1
print((2.0).hex())            # 0x1.0000000000000p+1
