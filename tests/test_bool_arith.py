# Test bool arithmetic operations (Bool used as operand in BINARY_OP)
# Basic bool arithmetic (loaded from co_consts as TAG_PTR)
print(True + True)        # 2
print(True + 1)           # 2
print(1 + True)           # 2
print(True * 3)           # 3
print(False + 1)          # 1
print(True - False)       # 1
print(True + 1.0)         # 2.0
print(True * 2.5)         # 2.5
print(False * 10)         # 0
print(True - 1)           # 0

# TAG_BOOL from list comparison used in arithmetic
# list_richcompare returns TAG_BOOL, then used in BINARY_OP
x = ([1] < [2]) + 1      # True + 1 = 2
print(x)
y = ([2] < [1]) + 5      # False + 5 = 5
print(y)
z = ([1] == [1]) * 3     # True * 3 = 3
print(z)
