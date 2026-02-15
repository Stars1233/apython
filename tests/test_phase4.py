# Test Phase 4 fixes

# 4a: bool arithmetic
print(True + 1)       # 2
print(True + True)    # 2
print(False + 1)      # 1
print(True * 3)       # 3

# 4c: list concat and repeat
print([1, 2] + [3, 4])        # [1, 2, 3, 4]
print([1, 2] * 3)             # [1, 2, 1, 2, 1, 2]
print([] + [1])                # [1]

# 4d: tuple concat and repeat
print((1, 2) + (3, 4))        # (1, 2, 3, 4)
print((1, 2) * 3)             # (1, 2, 1, 2, 1, 2)
print(() + (1,))               # (1,)

# 4f: issubclass
class Animal:
    pass

class Dog(Animal):
    pass

print(issubclass(Dog, Animal))    # True
print(issubclass(Animal, Dog))    # False
print(issubclass(Dog, Dog))       # True

# 4g: int * str (reversed multiply)
print(3 * "ab")                # ababab
print(2 * "xyz")               # xyzxyz
