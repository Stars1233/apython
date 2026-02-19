# Test zip strict= kwarg (PEP 618)

# Basic zip (no strict)
print(list(zip([1,2,3], [4,5,6])))
print(list(zip([1,2], [3,4,5])))

# strict=True with equal lengths
print(list(zip([1,2,3], [4,5,6], strict=True)))

# strict=False (same as no strict)
print(list(zip([1,2,3], [4,5,6], strict=False)))

# strict=True, second shorter
try:
    list(zip([1,2,3], [4,5], strict=True))
    print("ERROR: should have raised")
except ValueError:
    print("OK: ValueError raised")

# strict=True, first shorter
try:
    list(zip([1,2], [3,4,5], strict=True))
    print("ERROR: should have raised")
except ValueError:
    print("OK: ValueError raised")

# strict=True, three iterables, unequal
try:
    list(zip([1,2,3], [4,5,6], [7,8], strict=True))
    print("ERROR: should have raised")
except ValueError:
    print("OK: ValueError raised")

# strict=True, three iterables, equal
print(list(zip([1,2], [3,4], [5,6], strict=True)))
