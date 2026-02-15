# Test bytes type operations

# Indexing
b = b"hello"
print(b[0])     # 104 (ord('h'))
print(b[1])     # 101
print(b[-1])    # 111 (ord('o'))

# Length
print(len(b))   # 5

# Repr
print(b)        # b'hello'

# Iteration
total = 0
for x in b"abc":
    total = total + x
print(total)    # 97 + 98 + 99 = 294

# Contains
print(104 in b"hello")   # True
print(0 in b"hello")     # False

# Slicing
s = b"hello world"
print(s[0:5])     # b'hello'
print(s[6:11])    # b'world'

# Decode
d = b"test"
print(d.decode())   # test
