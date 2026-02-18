# Strings of every length 0-20
for i in range(21):
    s = "x" * i
    assert len(s) == i, f"len({i}) failed"
    assert s == "x" * i, f"eq({i}) failed"
    print(f"len {i}: ok")

# All ASCII printable chars (a-z, A-Z, 0-9)
chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
for c in chars:
    assert len(c) == 1
    assert c == c
print("ascii ok")

# Dict stress with many small keys
d = {}
for i in range(100):
    key = str(i)    # "0".."99" — all SmallStr (≤2 chars + digit)
    d[key] = i
for i in range(100):
    assert d[str(i)] == i, f"dict[{i}] failed"
print("dict stress ok")

# String as tuple elements
t = ("a", "bb", "ccc", "dddd")
for s in t:
    print(s)

# String as list elements
lst = ["x", "yy", "zzz"]
lst.append("w")
print(lst)

# Nested containers with SmallStr
d = {"key": ["a", "b", "c"]}
print(d["key"][1])  # b

# Repeated dict operations
d = {}
for i in range(50):
    d[f"k{i}"] = i
for i in range(50):
    del d[f"k{i}"]
print(len(d))  # 0
print("del stress ok")

# String identity (is)
a = "hello"
b = "hello"
print(a == b)   # True

# Long vs short comparison
print("hi" == "hi there")  # False
print("hi" < "hi there")   # True
print("hi there" > "hi")   # True

# Int * SmallStr and SmallStr * Int
print(3 * "ab")       # ababab
print("ab" * 3)       # ababab
print(0 * "xyz")      # (empty)
print("xyz" * 0)      # (empty)
print(1 * "test")     # test
print("test" * 1)     # test

# Boundary: exactly 14 chars (max SmallStr)
s14 = "a" * 14
print(len(s14))       # 14
print(s14)            # aaaaaaaaaaaaaa
print(s14 == "a" * 14)  # True

# Boundary: 15 chars (just over SmallStr limit)
s15 = "a" * 15
print(len(s15))       # 15
print(s15)            # aaaaaaaaaaaaaaa
print(s15 == "a" * 15)  # True

# Cross-boundary concat
s7 = "a" * 7
s7b = "b" * 7
s14 = s7 + s7b        # 14 chars, still SmallStr
print(len(s14))       # 14
print(s14)            # aaaaaaabbbbbbb
s15 = s7 + "b" * 8    # 15 chars, heap
print(len(s15))       # 15
print(s15)            # aaaaaaabbbbbbbb

# SmallStr in format strings
x = "hi"
print(f"{x}!")        # hi!
print(f"{'ab'}")      # ab
print("val=%s" % "ok")  # val=ok
