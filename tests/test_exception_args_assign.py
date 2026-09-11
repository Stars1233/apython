# BaseException.args is assignable.
#
# `args` is the FIRST comparison in exc_getattr's ladder and answers out of the
# exc_args field, long before the instance dict is consulted -- but exc_setattr
# had no arm for it, so `e.args = (...)` fell through to the generic dict_set
# and landed in exc_dict where nothing would ever read it.  The assignment was
# silently dropped and e.args still read the old tuple.
#
# That is the whole of CPython's test_configparser: its exception classes end
# __init__ with `self.args = (section, source, lineno)`, which is also how a
# great deal of ordinary library code carries structured detail on an error.
# func_setattr's own comment records the identical failure for f.__name__.
#
# CPython's BaseException_set_args is PySequence_Tuple(val) then Py_XSETREF:
# any iterable becomes a tuple, an exact tuple is handed straight back, and
# deleting is refused.

# The configparser shape, which is what this is for.
class DuplicateSectionError(Exception):
    def __init__(self, section, source=None, lineno=None):
        msg = [repr(section), " already exists"]
        Exception.__init__(self, "".join(msg))
        self.section = section
        self.args = (section, source, lineno)


e = DuplicateSectionError("Foo")
print(e.args, e.section, e.__dict__)
e2 = DuplicateSectionError("Bar", "f.ini", 3)
print(e2.args, str(e2))

e = ValueError("m")
print("before:", e.args)
e.args = ("a", None, 1)
print("tuple:", e.args, "dict:", e.__dict__)
e.args = [1, 2]
print("list:", e.args, type(e.args).__name__)
e.args = "ab"
print("str:", e.args)
e.args = (x * 2 for x in "ab")
print("genexp:", e.args)
e.args = ()
print("empty:", e.args, "str:", repr(str(e)), "repr:", repr(e))

# An exact tuple is handed back unchanged, as CPython's PySequence_Tuple does.
t = (1, 2)
e.args = t
print("identity:", e.args is t)

# One element: str(e) is that element, not a one-tuple.
e.args = ("solo",)
print("one:", str(e), repr(e))

for bad in (5, None, object(), 1.5):
    try:
        e.args = bad
    except TypeError as ex:
        print("TypeError:", ex)

try:
    del e.args
except TypeError as ex:
    print("TypeError:", ex)

# Reachable through every exception type, including the group, whose own
# setattr refuses two names of its own and then delegates.
g = ExceptionGroup("g", [ValueError("v")])
g.args = (1, 2)
print("group:", g.args)

o = OSError(2, "x", "/f")
print("oserror before:", o.args, o.errno, o.filename)
o.args = (9,)
print("oserror after:", o.args, o.errno, o.filename)


class Sub(KeyError):
    pass


s = Sub("k")
s.args = ("other", 1)
print("subclass:", s.args, str(s))

# A raise carries the reassigned args through.
try:
    raise e2
except DuplicateSectionError as caught:
    print("raised:", caught.args)
