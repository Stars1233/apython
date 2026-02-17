# Test yield-from subgenerator return value propagation

def inner():
    yield 1
    yield 2
    return 99

def outer():
    result = yield from inner()
    print(result)  # 99
    yield 3

g = outer()
print(next(g))  # 1
print(next(g))  # 2
print(next(g))  # 3
