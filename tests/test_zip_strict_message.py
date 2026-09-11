# zip(strict=True) says WHICH argument was short, and in which direction.
#
# CPython names it -- "zip() argument 2 is shorter than argument 1", and the
# plural "arguments 1-2" once there are three -- and this said "zip() has
# arguments with different lengths" whichever happened.  The index and the
# direction are both in hand at the raise, so it was wording rather than
# machinery.


def go(*args):
    try:
        return list(zip(*args, strict=True))
    except ValueError as e:
        return str(e)


print(go([1, 2, 3], [1, 2]))
print(go([1, 2], [1, 2, 3]))
print(go([1, 2, 3], [1, 2, 3], [1, 2]))
print(go([1, 2], [1, 2, 3], [1, 2, 3]))
print(go([1, 2, 3], [1, 2], [1, 2, 3]))
print(go([1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2]))
print(go([1, 2, 3], [1, 2, 3], [1, 2], [1, 2, 3]))
print(go([1], []))
print(go([], [1]))
print(go(range(3), range(2)))
print(go("abc", "ab"))
print(go(iter([1, 2, 3]), iter([1, 2])))

# Equal lengths, and the non-strict form, are unaffected.
print(go([1, 2], [3, 4]))
print(list(zip([1, 2, 3], [1, 2])))
print(list(zip()), go())
print(list(zip([1, 2, 3], strict=True)))

# A single short argument cannot be short of anything.
print(go([1, 2, 3]))

print("done")
