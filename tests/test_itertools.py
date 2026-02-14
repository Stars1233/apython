# enumerate
for i, x in enumerate(["a", "b", "c"]):
    print(i, x)

# enumerate with start
for i, x in enumerate(["x", "y"], 1):
    print(i, x)

# zip
for a, b in zip([1, 2, 3], [10, 20, 30]):
    print(a, b)

# zip different lengths
for a, b in zip([1, 2], [10, 20, 30]):
    print(a, b)

# map
for x in map(len, ["hi", "hello", "hey"]):
    print(x)

# filter
def is_positive(x):
    return x > 0
for x in filter(is_positive, [-1, 0, 1, 2, -3, 4]):
    print(x)

# reversed
for x in reversed([1, 2, 3]):
    print(x)

# sorted
for x in sorted([3, 1, 4, 1, 5]):
    print(x)

print("done")
