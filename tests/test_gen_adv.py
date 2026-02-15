# Test advanced generator features: send, close, yield from

# === gen.send() ===
print("--- send ---")

def accumulator():
    total = 0
    while True:
        value = yield total
        if value is None:
            break
        total = total + value

g = accumulator()
print(next(g))       # prime: yields 0
print(g.send(10))    # sends 10, yields 10
print(g.send(20))    # sends 20, yields 30
print(g.send(5))     # sends 5, yields 35

# === gen.close() ===
print("--- close ---")

def counter():
    i = 0
    while True:
        yield i
        i = i + 1

g = counter()
print(next(g))
print(next(g))
g.close()
print("closed")

# === yield from ===
print("--- yield from ---")

def inner():
    yield 1
    yield 2
    yield 3

def outer():
    yield 0
    yield from inner()
    yield 4

for x in outer():
    print(x)
