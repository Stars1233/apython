# Test generators: RETURN_GENERATOR, YIELD_VALUE, RESUME

# Basic generator
def count_up(n):
    i = 0
    while i < n:
        yield i
        i = i + 1

for x in count_up(5):
    print(x)

# Multiple yields
def multi():
    yield 10
    yield 20
    yield 30

for x in multi():
    print(x)

# next() builtin with generator
g = multi()
print(next(g))
print(next(g))
print(next(g))

# Generator with closures
def make_gen(start, stop):
    def gen():
        i = start
        while i < stop:
            yield i
            i = i + 1
    return gen

for x in make_gen(5, 8)():
    print(x)

# Fibonacci generator
def fib(n):
    a = 0
    b = 1
    i = 0
    while i < n:
        yield a
        c = a + b
        a = b
        b = c
        i = i + 1

for x in fib(8):
    print(x)

print("done")
