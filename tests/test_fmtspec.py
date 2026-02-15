# Test f-string format specs

# Basic float formatting
x = 3.14159265
print(f"{x:.2f}")     # 3.14
print(f"{x:.4f}")     # 3.1416
print(f"{x:.0f}")     # 3

# Scientific notation
y = 12345.6789
print(f"{y:.2e}")     # 1.23e+04
print(f"{y:.3E}")     # 1.235E+04

# Simple float
z = 1.5
print(f"{z:.1f}")     # 1.5
print(f"{z:.3f}")     # 1.500

# Zero
print(f"{0.0:.2f}")   # 0.00

# Negative
print(f"{-3.14:.1f}") # -3.1
