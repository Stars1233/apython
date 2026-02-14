# Test that formerly-fatal errors are now catchable exceptions

# NameError
try:
    print(undefined_var)
except NameError:
    print("caught NameError")

# TypeError: not callable
try:
    x = 42
    x()
except TypeError:
    print("caught TypeError: not callable")

# IndexError: list
try:
    lst = [1, 2, 3]
    print(lst[10])
except IndexError:
    print("caught IndexError: list")

# IndexError: tuple
try:
    t = (1, 2, 3)
    print(t[10])
except IndexError:
    print("caught IndexError: tuple")

# IndexError: string
try:
    s = "abc"
    print(s[10])
except IndexError:
    print("caught IndexError: string")

# KeyError
try:
    d = {"a": 1}
    print(d["missing"])
except KeyError:
    print("caught KeyError")

# TypeError: bad subscript
try:
    x = 42
    print(x[0])
except TypeError:
    print("caught TypeError: subscript")

# AttributeError
try:
    class Foo:
        pass
    f = Foo()
    print(f.missing)
except AttributeError:
    print("caught AttributeError")

print("all exception tests passed")
