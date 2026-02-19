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

# replace
print(b'hello world'.replace(b'world', b'there'))  # b'hello there'
print(b'hello'.replace(b'l', b'L'))                 # b'heLLo'
print(b'hello'.replace(b'x', b'y'))                 # b'hello'
print(b'hello'.replace(b'o', b''))                  # b'hell'
print(b'aaa'.replace(b'a', b'bb'))                  # b'bbbbbb'

# split
print(b'hello world foo'.split())            # [b'hello', b'world', b'foo']
print(b'a,b,c'.split(b','))                  # [b'a', b'b', b'c']
print(b'  hello  '.split())                  # [b'hello']
print(b'abc'.split(b'x'))                    # [b'abc']
print(b'a::b::c'.split(b'::'))              # [b'a', b'b', b'c']

# join
print(b', '.join([b'a', b'b', b'c']))       # b'a, b, c'
print(b''.join([b'a', b'b']))                # b'ab'
print(b'-'.join([b'hello']))                 # b'hello'
print(b' '.join([]))                         # b''
