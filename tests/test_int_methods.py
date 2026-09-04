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

# The names dir(int) was short of.  The stdlib classifies numbers by asking
# for them -- and (5).is_integer() was an AttributeError.
print((5).is_integer(), (0).is_integer(), (-7).is_integer(), (2 ** 70).is_integer())
print((5).as_integer_ratio(), (-7).as_integer_ratio(), (0).as_integer_ratio())
print((2 ** 70).as_integer_ratio())
print((5).__round__(), (5).__round__(2), (5).__round__(-1))
print((1234).__round__(-2), (1250).__round__(-2), (2 ** 70).__round__(-2))
print((5).__floor__(), (5).__ceil__(), (-5).__floor__(), (-5).__ceil__())
print((2 ** 70).__floor__(), (2 ** 70).__ceil__(), (2 ** 70).__trunc__())
print((5).__getnewargs__(), (2 ** 70).__getnewargs__(), (-7).__getnewargs__())

# bool and an int subclass flatten to a plain int, as CPython's do.
print(True.is_integer(), True.as_integer_ratio(), True.__round__())
print(True.__floor__(), True.__ceil__(), True.__getnewargs__())


class I(int):
    pass


print(I(5).is_integer(), I(5).as_integer_ratio(), I(5).__round__())
print(I(5).__floor__(), I(5).__getnewargs__())
print(type(I(5).__getnewargs__()[0]).__name__,
      type(I(5).as_integer_ratio()[0]).__name__)

print(sorted(set(["is_integer", "as_integer_ratio", "__round__", "__ceil__",
                  "__floor__", "__getnewargs__", "__trunc__"]) - set(dir(int))))
