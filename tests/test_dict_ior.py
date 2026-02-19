# Test dict |= (inplace merge)
d = {"a": 1, "b": 2}
d |= {"b": 3, "c": 4}
print(d["a"])
print(d["b"])
print(d["c"])
print(len(d))

# Test dict | (non-inplace merge)
d1 = {"x": 1}
d2 = {"y": 2}
d3 = d1 | d2
print(d3["x"])
print(d3["y"])
print(len(d3))
print(d1["x"])
print(len(d1))

# Test |= with empty dict
d = {"a": 1}
d |= {}
print(d["a"])
print(len(d))
