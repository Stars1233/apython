# Test new builtin functions

# abs
print(abs(5))
print(abs(-5))
print(abs(0))

# int
print(int())
print(int(42))
print(int(True))
print(int(False))

# str
print(str())
print(str(42))
print(str(True))

# ord/chr
print(ord("A"))
print(chr(65))
print(chr(97))

# hex
print(hex(255))
print(hex(0))
print(hex(16))
print(hex(-1))

# id - just check it returns something
x = 42
y = "hello"
print(id(x) == id(x))

# hash
print(hash(42))
print(hash("hello") == hash("hello"))

# callable
print(callable(print))
print(callable(42))
print(callable(len))

# iter/next
it = iter([1, 2, 3])
print(next(it))
print(next(it))
print(next(it))

# any/all
print(any([False, False, True]))
print(any([False, False, False]))
print(all([True, True, True]))
print(all([True, False, True]))
print(any([]))
print(all([]))

# sum
print(sum([1, 2, 3]))
print(sum([1, 2, 3], 10))
print(sum([]))

# min/max
print(min(3, 1, 2))
print(max(3, 1, 2))
print(min(5, 5))
print(max(-1, -2, -3))

# hasattr/getattr/setattr
class Foo:
    x = 10

f = Foo()
print(hasattr(f, "x"))
print(getattr(f, "x"))
print(getattr(f, "y", 99))
setattr(f, "z", 42)
print(f.z)

print("done")
