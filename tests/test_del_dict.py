# Test dictionary key deletion (del d[key])

# Basic del
d = {"a": 1, "b": 2, "c": 3}
del d["b"]
print(len(d))       # 2

# Del then access remaining
d2 = {"x": 10, "y": 20, "z": 30}
del d2["x"]
print(d2["y"])      # 20
print(d2["z"])      # 30

# Del all keys
d3 = {"one": 1, "two": 2}
del d3["one"]
del d3["two"]
print(len(d3))      # 0
print(d3)           # {}

# Del and re-add same key
d4 = {"k": 100}
del d4["k"]
d4["k"] = 200
print(d4["k"])      # 200
print(len(d4))      # 1

# Verify del removes the right key
d5 = {"alpha": 1, "beta": 2, "gamma": 3}
del d5["beta"]
print("alpha" in d5)   # True
print("beta" in d5)    # False
print("gamma" in d5)   # True

# KeyError on missing key
try:
    d6 = {"a": 1}
    del d6["missing"]
    print("ERROR: should have raised KeyError")
except KeyError:
    print("KeyError raised correctly")

print("All del dict tests passed")
