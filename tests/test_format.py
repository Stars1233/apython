# Test str.format()

# Basic placeholder
print("Hello {}!".format("world"))

# Multiple placeholders
print("{} + {} = {}".format(1, 2, 3))

# No placeholders
print("AB".format())

# Explicit index
print("{1} {0}".format("world", "hello"))

# Empty string arg
print("X{}Y".format(""))

# Mixed text and placeholder
print("ab{}cd".format("X"))

# Bound method call
s = "Hello {}!"
m = s.format
print(m("world"))

# Integer formatting
print("x = {}".format(42))

# Multiple types
print("{} is {} and {}".format("hello", 42, True))

# Repeated explicit index
print("{0} {0}".format("echo"))

# Literal braces
print("{{}}".format())
print("{{{0}}}".format("braces"))
