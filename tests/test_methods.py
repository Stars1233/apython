# Test type methods for str, list, and dict

# Helper to print a list since list repr is not yet fully implemented
def print_list(lst):
    parts = []
    i = 0
    while i < len(lst):
        parts.append(str(lst[i]))
        i = i + 1
    print("[" + ", ".join(parts) + "]")

# --- str methods ---

# upper/lower
print("hello".upper())
print("HELLO".lower())
print("MiXeD".upper())
print("MiXeD".lower())
print("".upper())

# strip
print("  hello  ".strip())
print("hello".strip())
print("\t\nhello\n\t".strip())

# startswith/endswith
print("hello world".startswith("hello"))
print("hello world".startswith("world"))
print("hello world".endswith("world"))
print("hello world".endswith("hello"))
print("".startswith(""))
print("hello".endswith(""))

# find
print("hello world".find("world"))
print("hello world".find("xyz"))
print("abcabc".find("bc"))

# replace
print("hello world".replace("world", "python"))
print("aaa".replace("a", "bb"))
print("hello".replace("xyz", "abc"))
print("abab".replace("ab", "cd"))

# join
print(",".join(["a", "b", "c"]))
print(" ".join(["hello", "world"]))
print("-".join(["one"]))
print("".join(["a", "b", "c"]))

# split
parts = "hello world foo".split()
print_list(parts)

parts = "a,b,c".split(",")
print_list(parts)

parts = "one".split(",")
print_list(parts)

parts = "  hello  world  ".split()
print_list(parts)

# --- list methods ---

# append
x = [1, 2, 3]
x.append(4)
print_list(x)

# pop
x = [1, 2, 3, 4, 5]
print(x.pop())
print_list(x)

x = [10, 20, 30]
print(x.pop(0))
print_list(x)

# insert
x = [1, 3, 4]
x.insert(1, 2)
print_list(x)

# reverse
x = [1, 2, 3, 4, 5]
x.reverse()
print_list(x)

# sort
x = [3, 1, 4, 1, 5, 9, 2, 6]
x.sort()
print_list(x)

# index
x = [10, 20, 30, 40]
print(x.index(30))

# count
x = [1, 2, 1, 3, 1]
print(x.count(1))

# copy
x = [1, 2, 3]
y = x.copy()
y.append(4)
print_list(x)
print_list(y)

# clear
x = [1, 2, 3]
x.clear()
print(len(x))

# extend
x = [1, 2]
x.extend([3, 4, 5])
print_list(x)

# --- dict methods ---

# get
d = {"a": 1, "b": 2}
print(d.get("a"))
print(d.get("c"))
print(d.get("c", 99))

# keys
d = {"x": 10, "y": 20}
k = d.keys()
print(len(k))

# values
v = d.values()
print(len(v))

# items
items = d.items()
print(len(items))

# pop
d = {"a": 1, "b": 2, "c": 3}
print(d.pop("b"))
print(len(d))
print(d.pop("z", 42))

# clear
d = {"a": 1, "b": 2}
d.clear()
print(len(d))

# update
d1 = {"a": 1}
d2 = {"b": 2, "c": 3}
d1.update(d2)
print(len(d1))
print(d1.get("b"))
print(d1.get("c"))
