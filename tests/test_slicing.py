# Test string slicing
s = "hello"
print(s[1:4])    # ell
print(s[::-1])   # olleh
print(s[::2])    # hlo
print(s[1:])     # ello
print(s[:3])     # hel

# Test list slicing via iteration (avoids list repr)
x = [10, 20, 30, 40, 50]
y = x[1:3]
for v in y:
    print(v)
# 20
# 30

z = x[::2]
for v in z:
    print(v)
# 10
# 30
# 50

w = x[::-1]
for v in w:
    print(v)
# 50
# 40
# 30
# 20
# 10

# Test tuple slicing via iteration
t = (10, 20, 30, 40, 50)
u = t[1:4]
for v in u:
    print(v)
# 20
# 30
# 40

r = t[::-1]
for v in r:
    print(v)
# 50
# 40
# 30
# 20
# 10

# Negative indices
print(s[-3:])    # llo
print(s[:-2])    # hel

# Empty slices
e = x[3:1]
for v in e:
    print(v)
# (nothing)

# len of sliced list
print(len(x[1:4]))  # 3
print(len(s[::2]))  # 3
