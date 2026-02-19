# Test map with multiple iterables

# Single iterable (backward compat)
print(list(map(str, [1, 2, 3])))
print(list(map(lambda x: x * 2, [1, 2, 3])))

# Two iterables
print(list(map(lambda x, y: x + y, [1, 2, 3], [10, 20, 30])))

# Three iterables
print(list(map(lambda x, y, z: x + y + z, [1, 2], [10, 20], [100, 200])))

# Unequal lengths (stops at shortest)
print(list(map(lambda x, y: x + y, [1, 2, 3], [10, 20])))

# With builtin function
print(list(map(max, [1, 5, 3], [4, 2, 6])))

# sorted with reverse
print(sorted([5, 2, 8, 1, 9], reverse=True))
print(sorted([5, 2, 8, 1, 9], reverse=False))

# list.sort with reverse
a = [3, 1, 4, 1, 5]
a.sort(reverse=True)
print(a)
