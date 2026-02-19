# Test bytes methods

# hex
print(b''.hex())              # (empty string)
print(b'\xab\xcd'.hex())      # abcd
print(b'hello'.hex())         # 68656c6c6f

# startswith
print(b'hello'.startswith(b'hel'))   # True
print(b'hello'.startswith(b'xyz'))   # False
print(b'hello'.startswith(b''))      # True
print(b'hi'.startswith(b'hello'))    # False

# endswith
print(b'hello'.endswith(b'llo'))     # True
print(b'hello'.endswith(b'xyz'))     # False
print(b'hello'.endswith(b''))        # True
print(b'hi'.endswith(b'hello'))      # False

# count
print(b'hello'.count(b'l'))         # 2
print(b'hello'.count(b'x'))         # 0
print(b'aaa'.count(b'aa'))          # 1 (non-overlapping)
print(b'hello'.count(b''))          # 6 (len+1)

# find
print(b'hello'.find(b'l'))          # 2
print(b'hello'.find(b'x'))          # -1
print(b'hello'.find(b''))           # 0
print(b'hello'.find(b'lo'))         # 3
