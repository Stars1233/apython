# Test slice assignment on lists

# Basic slice assignment (same length)
a = [1, 2, 3, 4, 5]
a[1:3] = [20, 30]
print(a)  # [1, 20, 30, 4, 5]

# Slice assignment (shorter replacement)
b = [1, 2, 3, 4, 5]
b[1:4] = [99]
print(b)  # [1, 99, 5]

# Slice assignment (longer replacement)
c = [1, 2, 3, 4, 5]
c[1:2] = [10, 20, 30]
print(c)  # [1, 10, 20, 30, 3, 4, 5]

# Slice assignment at start
d = [1, 2, 3]
d[0:1] = [10, 20]
print(d)  # [10, 20, 2, 3]

# Slice assignment at end
e = [1, 2, 3]
e[2:3] = [30, 40]
print(e)  # [1, 2, 30, 40]

# Empty slice insertion
f = [1, 2, 3]
f[1:1] = [10, 20]
print(f)  # [1, 10, 20, 2, 3]

# Delete via empty replacement
g = [1, 2, 3, 4, 5]
g[1:4] = []
print(g)  # [1, 5]

# Tuple as replacement
h = [1, 2, 3]
h[0:2] = (10, 20, 30)
print(h)  # [10, 20, 30, 3]
