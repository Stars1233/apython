# Test dict.fromkeys
d = dict.fromkeys(["a", "b", "c"])
print(d["a"] is None)
print(d["b"] is None)
print(len(d))

d2 = dict.fromkeys(["x", "y"], 0)
print(d2["x"])
print(d2["y"])
print(len(d2))

d3 = dict.fromkeys([])
print(len(d3))
