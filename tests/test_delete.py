# Test delete operations
x = 10
del x
try:
    print(x)
except NameError:
    print("x deleted")

# Delete from dict
d = {"a": 1, "b": 2, "c": 3}
del d["b"]
print(len(d))    # 2
print(d["a"])    # 1
print(d["c"])    # 3

# Delete local variable
def test_del():
    y = 42
    del y
    try:
        print(y)
    except NameError:
        print("y deleted")

test_del()

# Delete global
z = 100
del z
try:
    print(z)
except NameError:
    print("z deleted")
