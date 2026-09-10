# A call chain that never reaches a Python frame still has to stop.
#
# `A.__call__ = A()` makes calling an A reach slot_tp_call again through the
# instance's own __call__, and again, forever: no Python frame is entered
# anywhere in the chain, so recursion_depth never moved and the machine stack
# simply ran out.  CPython raises RecursionError (Py_EnterRecursiveCall in
# slot_tp_call) and its test_class has a test for this, SF bug 532646.
#
# The counter is the C-level one that container comparison uses, so it is
# reset wholesale when the exception unwinds rather than balanced.

import sys


class A:
    pass


A.__call__ = A()
a = A()

try:
    a()
except RecursionError:
    print("RecursionError")
else:
    print("NO ERROR")

# Twice, to prove the counter came back.
for i in range(3):
    try:
        a()
    except RecursionError:
        print("again", i)

# The interpreter still works afterwards.
def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)


print(fib(15))
print(sorted({3, 1, 2}), {"a": 1} == {"a": 1}, [1, [2, [3]]])

# An ordinary Python-level runaway still reports RecursionError, and the
# limit is still what sys says it is.
def deep(n):
    return deep(n + 1)


try:
    deep(0)
except RecursionError:
    print("python-level RecursionError")
print(sys.getrecursionlimit())

# A two-object cycle: each one's __call__ is the other.
class B:
    pass


class C:
    pass


B.__call__ = C()
C.__call__ = B()
try:
    B()()
except RecursionError:
    print("mutual RecursionError")

# And the ordinary uses of __call__ are untouched.
class Callable:
    def __call__(self, *args, **kwargs):
        return ("called", args, sorted(kwargs.items()))


c = Callable()
print(c())
print(c(1, 2, x=3))
print([c(i) for i in range(3)][2])


class Chain:
    def __init__(self, inner):
        self.inner = inner

    def __call__(self, *args):
        return self.inner(*args)


chained = c
for i in range(20):
    chained = Chain(chained)
print(chained(9))
print("done")
