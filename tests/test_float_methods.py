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
