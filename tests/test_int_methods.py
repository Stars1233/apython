# Test int methods

# bit_length
print((0).bit_length())       # 0
print((1).bit_length())       # 1
print((2).bit_length())       # 2
print((255).bit_length())     # 8
print((-1).bit_length())      # 1
print((-255).bit_length())    # 8

# bit_count
print((0).bit_count())        # 0
print((1).bit_count())        # 1
print((7).bit_count())        # 3
print((255).bit_count())      # 8
print((-7).bit_count())       # 3

# conjugate
print((42).conjugate())       # 42
print((-5).conjugate())       # -5
