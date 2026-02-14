# Set literal
s = {1, 2, 3}
print(len(s))

# Membership
print(1 in s)
print(4 in s)
print(2 in s)

# Duplicate elimination
s2 = {1, 1, 2, 2, 3, 3}
print(len(s2))

# Iteration (collect and sort since set order is implementation-defined)
result = []
for x in {10, 20, 30}:
    result.append(x)
result.sort()
for x in result:
    print(x)

# Set comprehension
s3 = {x * x for x in range(5)}
print(len(s3))
result2 = []
for x in s3:
    result2.append(x)
result2.sort()
for x in result2:
    print(x)

print("done")
