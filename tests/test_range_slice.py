# A range supports slicing and gives back a range.  Only integer indexing was
# implemented, so `range(len(x))[::-1]` -- which re/_compiler.py uses to walk a
# subpattern backwards -- raised IndexError from sq_item being handed a slice.
r = range(5)
print(r[::-1], list(r[::-1]))
print(r[1:3], list(r[1:3]))
print(r[:], r[::2], r[1:], r[:3])
print(r[2], r[-1], r[0])

print(range(10)[2:8:2], list(range(10)[2:8:2]))
print(range(10)[::-2], list(range(10)[::-2]))
print(range(1, 10, 3), range(1, 10, 3)[1:], list(range(1, 10, 3)[1:]))
print(range(10, 0, -1)[2:5], list(range(10, 0, -1)[2:5]))

# Empty and degenerate results.
print(range(5)[3:1], list(range(5)[3:1]), len(range(5)[3:1]))
print(range(0)[:], list(range(0)[:]))
print(range(5)[10:20], len(range(5)[10:20]))
print(range(5)[-2:], list(range(5)[-2:]))
print(range(5)[:-2], list(range(5)[:-2]))

# Slicing a slice.
print(range(20)[::2][1:4], list(range(20)[::2][1:4]))

# len() and membership on the result.
s = range(20)[3:15:4]
print(s, len(s), list(s), 7 in s, 8 in s)

# The result really is a range.
print(type(range(5)[:]).__name__)

# Indexing still raises where it should.
try:
    range(5)[9]
except IndexError:
    print("index error")
try:
    range(5)["a"]
except TypeError:
    print("type error")
