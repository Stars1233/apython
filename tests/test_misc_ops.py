# Test EXTENDED_ARG, LOAD_FAST_AND_CLEAR, assertions

# Test assert (uses LOAD_ASSERTION_ERROR)
assert True
assert 1 == 1

try:
    assert False
except AssertionError:
    print("assert caught")

try:
    assert 1 == 2
except AssertionError:
    print("assert expr caught")

# Test list comprehension (uses LOAD_FAST_AND_CLEAR)
x = [i for i in range(5)]
for v in x:
    print(v)

# Nested comprehension
y = [i * 2 for i in range(4)]
for v in y:
    print(v)

print("done")
