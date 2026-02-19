# Test extended slice assignment

# Basic even-index assignment
a = [0, 1, 2, 3, 4, 5]
a[::2] = [10, 20, 30]
print(a)  # [10, 1, 20, 3, 30, 5]

# Odd-index assignment
b = [0, 1, 2, 3, 4, 5]
b[1::2] = [10, 20, 30]
print(b)  # [0, 10, 2, 20, 4, 30]

# Negative step
c = [0, 1, 2, 3, 4]
c[::-2] = [10, 20, 30]
print(c)  # [20, 1, 30, 3, 10]

# Step=2 with partial range
d = [0, 1, 2, 3, 4, 5, 6, 7]
d[1:6:2] = [10, 20, 30]
print(d)  # [0, 10, 2, 20, 4, 30, 6, 7]

# Step=-1 (reverse)
e = [0, 1, 2, 3, 4]
e[::-1] = [10, 20, 30, 40, 50]
print(e)  # [50, 40, 30, 20, 10]

# Step=3
f = [0, 1, 2, 3, 4, 5, 6, 7, 8]
f[::3] = [10, 20, 30]
print(f)  # [10, 1, 2, 20, 4, 5, 30, 7, 8]

# Assign from tuple
g = [0, 1, 2, 3, 4, 5]
g[::2] = (10, 20, 30)
print(g)  # [10, 1, 20, 3, 30, 5]

# Wrong size raises ValueError
try:
    h = [0, 1, 2, 3, 4, 5]
    h[::2] = [10, 20]  # slicelength=3 but assigning 2
    print("ERROR: should have raised")
except ValueError:
    print("ValueError raised")

# Step=0 raises ValueError
try:
    i = [0, 1, 2]
    i[::0] = [1]
    print("ERROR: should have raised")
except ValueError:
    print("ValueError raised")

print("done")
