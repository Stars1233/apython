# Test dict operators

# dict | dict (merge)
a = {"x": 1, "y": 2}
b = {"y": 3, "z": 4}
c = a | b
print(c["x"])    # 1 (from a)
print(c["y"])    # 3 (from b, overrides a)
print(c["z"])    # 4 (from b)
print(len(c))    # 3

# dict | dict (disjoint)
d = {"a": 1} | {"b": 2}
print(d["a"])    # 1
print(d["b"])    # 2
print(len(d))    # 2

# dict == dict
print({"a": 1, "b": 2} == {"a": 1, "b": 2})  # True
print({"a": 1, "b": 2} == {"a": 1, "b": 3})  # False
print({"a": 1} == {"a": 1, "b": 2})            # False
print({} == {})                                  # True

# dict != dict
print({"a": 1} != {"a": 2})  # True
print({"a": 1} != {"a": 1})  # False
