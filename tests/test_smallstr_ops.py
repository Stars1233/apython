# Test SmallStr in containers and operations that triggered crashes
x = ["z", "a", "m", "b", "hello"]
x.sort()
print(x)

# Test SmallStr in mixed-type list operations (no sort)
y = [1, "a", 2.0, True, None, "hello"]
print(y.index("a"))
print(y.count("a"))
y.remove("a")
print(y)
print("{} {}".format("hi", "lo"))
print(type("x"))
print(isinstance("x", str))
print(abs(1))  # should work
# abs("x") should raise TypeError, not crash
try:
    abs("x")
except TypeError:
    print("abs TypeError OK")
