x = None
print(x is None)
y = 42
print(y is None)
def foo():
    pass
r = foo()
print(r is None)
if not None:
    print(1)
if not 0:
    print(1)
if 1:
    print(1)
