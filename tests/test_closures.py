# Test closures and cell variables

# Basic closure
def make_adder(n):
    def adder(x):
        return x + n
    return adder

add5 = make_adder(5)
print(add5(3))   # 8
print(add5(10))  # 15

add10 = make_adder(10)
print(add10(3))  # 13

# Closure over multiple variables
def make_pair(a, b):
    def get_sum():
        return a + b
    return get_sum

pair = make_pair(3, 4)
print(pair())    # 7

# Nested closures
def outer(x):
    def middle(y):
        def inner(z):
            return x + y + z
        return inner
    return middle

f = outer(1)(2)(3)
print(f)         # 6

# Counter pattern
def make_counter():
    count = 0
    def increment():
        nonlocal count
        count = count + 1
        return count
    return increment

counter = make_counter()
print(counter())  # 1
print(counter())  # 2
print(counter())  # 3
