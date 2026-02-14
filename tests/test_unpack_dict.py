# Test UNPACK_EX, MAP_ADD, DICT_UPDATE/MERGE, dict iteration

# UNPACK_EX: a, *b = iterable
a, *b = [1, 2, 3, 4, 5]
print(a)
for v in b:
    print(v)

# UNPACK_EX: *a, b = iterable
*a, b = [10, 20, 30]
for v in a:
    print(v)
print(b)

# UNPACK_EX: a, *b, c = iterable
a, *b, c = [1, 2, 3, 4]
print(a)
for v in b:
    print(v)
print(c)

# MAP_ADD: dict comprehension
d = {i: i * 2 for i in range(4)}
for k in d:
    print(k, d[k])

# DICT_UPDATE: {**d1, **d2}
d1 = {1: 10, 2: 20}
d2 = {3: 30, 4: 40}
d3 = {**d1, **d2}
for k in d3:
    print(k, d3[k])

print("done")
