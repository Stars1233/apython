# Test function default arguments

def f(x, y=2):
    print(x, y)

f(1)
f(1, 3)

def g(a, b=10, c=20):
    print(a, b, c)

g(1)
g(1, 2)
g(1, 2, 3)
