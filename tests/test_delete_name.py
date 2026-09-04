# `del name` in each kind of scope.
#
# DELETE_NAME deletes from the frame's locals and raises NameError when it
# misses; it used to fall through to globals, so `class C: del g` reached out
# of the class body and deleted a module global -- silently, and leaving the
# name gone for everything after it.  A frame with no locals mapping of its
# own is the module case, where locals and globals are the same dict.

# del in every scope shape
g = 1
h = 2
del h
print("module del:", "h" in dir())
class C:
    x = 1
    del x
    y = 2
    try:
        del g
    except NameError as e:
        print("class body:", type(e).__name__)
    try:
        del nosuch
    except NameError:
        print("class body missing name")
print("C.y", C.y, hasattr(C, "x"), g)
def f():
    a = 1
    del a
    try:
        print(a)
    except UnboundLocalError:
        print("func local gone")
    try:
        del g
    except NameError:
        print("func del global via NameError")
f()
def h2():
    global g
    del g
h2()
print("global deleted:", "g" in globals())
d = {}
exec("q = 1\ndel q\nprint('exec:', 'q' in dir())", d)
