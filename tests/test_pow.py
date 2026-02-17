# Test pow() builtin

# 2-arg int
print(pow(2, 10))      # 1024
print(pow(3, 0))       # 1
print(pow(5, 3))       # 125

# 2-arg negative exp -> float
print(pow(2, -1))      # 0.5

# 2-arg float
print(pow(2.0, 3))     # 8.0
print(pow(4.0, 0.5))   # 2.0 (square root, but only if integer exp path)

# 3-arg modular exponentiation
print(pow(2, 10, 1000))   # 24
print(pow(3, 4, 5))       # 1
print(pow(7, 2, 13))      # 10
