# Test CALL_FUNCTION_EX and *args functions

# Basic *args unpacking
def add(a, b):
    return a + b

args = (3, 4)
print(add(*args))

# *args in definition
def sum_all(*args):
    total = 0
    for x in args:
        total = total + x
    return total

print(sum_all(1, 2, 3))
print(sum_all(10, 20, 30, 40))

# Mixed regular + *args
def first_and_rest(first, *rest):
    print(first)
    for x in rest:
        print(x)

first_and_rest(1, 2, 3)

# Empty *args
def maybe_args(*args):
    print(len(args))

maybe_args()
maybe_args(1)
maybe_args(1, 2, 3)

# *args unpacking in call
nums = [1, 2, 3, 4, 5]
print(sum_all(*nums))

print("done")
