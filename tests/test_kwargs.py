# Test keyword arguments and kw-only args

# Basic keyword arg
def f(a, b):
    print(a, b)

f(1, b=2)
f(a=1, b=2)
f(b=2, a=1)

# Keyword with defaults
def g(a, b=10, c=20):
    print(a, b, c)

g(1, c=30)
g(1, b=5, c=15)

# Keyword-only args
def h(a, *, c, d=4):
    print(a, c, d)

h(1, c=3)
h(1, c=3, d=5)

# Mixed: positional defaults + kw-only defaults
def mixed(a, b=2, *, c, d=4):
    print(a, b, c, d)

mixed(1, c=3)
mixed(1, 20, c=30, d=40)

# **kwargs
def kw_func(a, **kw):
    print(a, len(kw))

kw_func(1, x=2, y=3)

# *args and **kwargs together
def varargs(a, *args, **kw):
    print(a, args, len(kw))

varargs(1, 2, 3, x=4)
