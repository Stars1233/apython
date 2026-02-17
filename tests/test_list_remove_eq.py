# Test list.remove() and list.count() use __eq__ not just identity
a = [1.0, 2.0, 3.0]
a.remove(1)
print(a)  # [2.0, 3.0]

b = [1, 2, 3, 2, 1]
print(b.count(2))  # 2

# Cross-type equality: int vs float
c = [1.0, 2.0, 3.0]
print(c.count(2))  # 1

# Remove int from float list
d = [1.0, 2.0, 3.0]
d.remove(3)
print(d)  # [1.0, 2.0]
