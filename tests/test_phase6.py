# Phase 6: Type method completeness tests

# === str methods ===
print("--- str methods ---")
print("  hello  ".lstrip())
print("  hello  ".rstrip())
print("hello world hello".count("hello"))
print("hello world".index("world"))
print("hello world hello".rfind("hello"))
print("12345".isdigit())
print("abc".isdigit())
print("abcdef".isalpha())
print("abc123".isalpha())
print("HelloWorld".removeprefix("Hello"))
print("HelloWorld".removesuffix("World"))

# === list methods ===
print("--- list methods ---")
lst = [1, 2, 3, 4, 3, 2, 1]
lst.remove(3)
print(lst)

# === dict methods ===
print("--- dict methods ---")
d = {"a": 1}
print(d.setdefault("a", 99))
print(d.setdefault("b", 2))
print(len(d))

d2 = d.copy()
print(len(d2))

# dict.popitem
d3 = {"x": 10}
item = d3.popitem()
print(item)
print(len(d3))

# === tuple methods ===
print("--- tuple methods ---")
t = (1, 2, 3, 2, 1)
print(t.index(3))
print(t.count(2))

# === globals/locals ===
print("--- globals/locals ---")
g = globals()
print(type(g).__name__)
l = locals()
print(type(l).__name__)
