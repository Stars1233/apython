# A descriptor's __get__ is handed a VALUE, and an int, a float or a bool is
# an immediate rather than a pointer.
#
# classmethod.__get__ derived the owner from the instance by reading ob_type
# straight off it, so `classmethod(f).__get__(0)` -- which CPython's own
# test_descr does, twice -- dereferenced the NUMBER.  value_type is the one
# place that knows all three encodings.


def f(cls, arg=None):
    "f docstring"
    return (cls, arg)


class P:
    @property
    def p(self):
        return "prop"


prop = P.__dict__["p"]

probes = [
    ("classmethod.__get__(0)", lambda: classmethod(f).__get__(0)(1)),
    ("classmethod.__get__(0, int)", lambda: classmethod(f).__get__(0, int)(1)),
    ("classmethod.__get__(1.5)", lambda: classmethod(f).__get__(1.5)(1)),
    ("classmethod.__get__(True)", lambda: classmethod(f).__get__(True)(1)),
    ("classmethod.__get__('s')", lambda: classmethod(f).__get__("s")(1)),
    ("classmethod.__get__((1,))", lambda: classmethod(f).__get__((1,))(1)),
    ("classmethod.__get__(None)", lambda: classmethod(f).__get__(None)),
    ("classmethod.__get__(0, None)", lambda: classmethod(f).__get__(0, None)(1)),
    ("staticmethod.__get__(0)", lambda: staticmethod(f).__get__(0)(1)),
    ("function.__get__(0)", lambda: f.__get__(0)(1)),
    ("function.__get__(0, int)", lambda: f.__get__(0, int)(1)),
    ("property.__get__(0, int)", lambda: prop.__get__(0, int)),
    ("property.__get__(None, int) is prop", lambda: prop.__get__(None, int) is prop),
]
for name, fn in probes:
    try:
        print(name, "->", repr(fn()))
    except Exception as e:
        print(name, "!!", type(e).__name__, str(e)[:50])

# The rest of CPython's test_descr.test_classmethods around it.
ff = classmethod(f)
print("bound self:", ff.__get__(0, int).__self__)
meth = classmethod(1).__get__(1)
try:
    meth()
    print("non-callable -> ok")
except TypeError:
    print("non-callable -> TypeError")
print("done")
