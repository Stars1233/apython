# Test generator return values via StopIteration.value

def gen_return_42():
    yield 1
    yield 2
    return 42

# Test 1: Catch StopIteration and check .value
g = gen_return_42()
print(next(g))  # 1
print(next(g))  # 2
try:
    next(g)
    print("ERROR: should have raised StopIteration")
except StopIteration as e:
    print(e.value)  # 42

# Test 2: Generator with no explicit return -> .value is None
def gen_no_return():
    yield 10

g2 = gen_no_return()
print(next(g2))  # 10
try:
    next(g2)
except StopIteration as e:
    print(e.value)  # None
