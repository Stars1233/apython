# Test dict repr with integer keys (non-string keys)
d = {1: 'a', 2: 'b', 3: 'c'}
print(d)

# Mixed key types
d2 = {1: 'x', 2: 'y'}
print(d2)

# Single integer key
d3 = {42: 'answer'}
print(d3)

print("All dict int key tests passed")
