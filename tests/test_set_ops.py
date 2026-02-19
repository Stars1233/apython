# Test set query methods: union, intersection, difference,
# symmetric_difference, issubset, issuperset, isdisjoint

a = {1, 2, 3, 4}
b = {3, 4, 5, 6}

# union
u = a.union(b)
print(sorted(list(u)))  # [1, 2, 3, 4, 5, 6]

# intersection
i = a.intersection(b)
print(sorted(list(i)))  # [3, 4]

# difference
d = a.difference(b)
print(sorted(list(d)))  # [1, 2]

# symmetric_difference
sd = a.symmetric_difference(b)
print(sorted(list(sd)))  # [1, 2, 5, 6]

# issubset
print({1, 2}.issubset({1, 2, 3}))   # True
print({1, 2, 4}.issubset({1, 2, 3}))  # False

# issuperset
print({1, 2, 3}.issuperset({1, 2}))   # True
print({1, 2}.issuperset({1, 2, 3}))   # False

# isdisjoint
print({1, 2}.isdisjoint({3, 4}))   # True
print({1, 2}.isdisjoint({2, 3}))   # False

# === Operator syntax ===
# Union |
print(sorted(a | b))

# Intersection &
print(sorted(a & b))

# Difference -
print(sorted(a - b))
print(sorted(b - a))

# Symmetric difference ^
print(sorted(a ^ b))

# Empty set
e = set()
print(sorted(a | e))
print(sorted(a & e))
print(sorted(a - e))
