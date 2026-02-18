# Test basic coroutine type and async/await infrastructure

# Test 1: async def creates a coroutine
async def simple_coro():
    return 42

c = simple_coro()
print("coroutine type:", type(c).__name__)

# Manually step the coroutine with send(None)
try:
    c.send(None)
    print("ERROR: should have raised StopIteration")
except StopIteration as e:
    print("coroutine returned:", e.value)

# Test 2: coroutine with value
async def coro_with_value():
    return "hello"

c2 = coro_with_value()
try:
    c2.send(None)
except StopIteration as e:
    print("coro2 returned:", e.value)

# Test 3: coroutine close
async def infinite_coro():
    while True:
        return 99

c3 = infinite_coro()
c3.close()
print("close: ok")

# Test 4: multiple coroutines
async def adder(a, b):
    return a + b

c4 = adder(10, 20)
try:
    c4.send(None)
except StopIteration as e:
    print("adder:", e.value)

# Test 5: StopAsyncIteration is a builtin
print("StopAsyncIteration:", StopAsyncIteration.__name__)
print("TimeoutError:", TimeoutError.__name__)

# Test 6: StopAsyncIteration is catchable
try:
    raise StopAsyncIteration("test")
except StopAsyncIteration as e:
    print("caught StopAsyncIteration:", e.args[0])

# Test 7: TimeoutError is catchable
try:
    raise TimeoutError("timed out")
except TimeoutError as e:
    print("caught TimeoutError:", e.args[0])

# Test 8: coroutine with arguments and local vars
async def compute(x, y, z):
    result = x * y + z
    return result

c5 = compute(3, 4, 5)
try:
    c5.send(None)
except StopIteration as e:
    print("compute:", e.value)

print("all async coro tests passed")
