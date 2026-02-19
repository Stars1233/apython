# Test new string methods

# title
print("hello world".title())
print("they're bill's friends".title())
print("HELLO".title())
print("123abc".title())

# capitalize
print("hello world".capitalize())
print("HELLO WORLD".capitalize())
print("".capitalize())

# swapcase
print("Hello World".swapcase())
print("ABC".swapcase())
print("abc".swapcase())

# casefold
print("HELLO".casefold())
print("Hello".casefold())

# center
print("hi".center(10))
print("hi".center(10, '*'))
print("hi".center(1))

# ljust
print("hi".ljust(10))
print("hi".ljust(10, '-'))
print("hi".ljust(1))

# rjust
print("hi".rjust(10))
print("hi".rjust(10, '-'))
print("hi".rjust(1))

# zfill
print("42".zfill(5))
print("-42".zfill(5))
print("+42".zfill(5))
print("42".zfill(1))

# rindex
print("hello hello".rindex("hello"))
try:
    "hello".rindex("xyz")
except ValueError:
    print("OK: ValueError raised")

# istitle
print("Hello World".istitle())
print("Hello world".istitle())
print("HELLO".istitle())
print("".istitle())

# partition
print("hello world".partition(" "))
print("hello".partition(" "))

# rpartition
print("hello world hello".rpartition(" "))
print("hello".rpartition(" "))

# splitlines
print("hello\nworld\n".splitlines())
print("hello\nworld\n".splitlines(True))
print("hello\r\nworld".splitlines())

# expandtabs
print("hello\tworld".expandtabs())
print("hello\tworld".expandtabs(4))
print("01\t012\t0123\t01234".expandtabs())
