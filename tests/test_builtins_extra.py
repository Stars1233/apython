# Test new builtins: bin, oct, ascii, format, vars, delattr, aiter, anext, __import__

# bin()
print(bin(0))        # 0b0
print(bin(1))        # 0b1
print(bin(10))       # 0b1010
print(bin(255))      # 0b11111111
print(bin(-1))       # -0b1
print(bin(-10))      # -0b1010

# oct()
print(oct(0))        # 0o0
print(oct(1))        # 0o1
print(oct(8))        # 0o10
print(oct(255))      # 0o377
print(oct(-1))       # -0o1
print(oct(-8))       # -0o10

# ascii() - basic (all ASCII input returns same as repr)
print(ascii("hello"))     # 'hello'
print(ascii(42))          # 42
print(ascii(None))        # None
print(ascii(True))        # True

# format() - basic (returns str of value)
print(format(42))         # 42
print(format("hello"))    # hello

# delattr()
class Foo:
    pass
f = Foo()
f.x = 10
print(f.x)           # 10
delattr(f, 'x')
print(hasattr(f, 'x'))  # False

# __import__
sys = __import__('sys')
print(type(sys).__name__)  # module

# vars() with object
class Bar:
    pass
b = Bar()
b.x = 1
b.y = 2
d = vars(b)
print('x' in d)      # True
print('y' in d)      # True
