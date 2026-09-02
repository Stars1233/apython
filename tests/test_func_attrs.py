# A function's __name__ is settable, and its __doc__ comes from the docstring
# slot rather than from whatever constant happens to be first.
#
# func_setattr stored every attribute in func_dict, and func_getattr answered
# __name__ from the func_name field without ever looking there -- so
# `f.__name__ = "x"` was silently lost, which is what functools.wraps does.
#
# The __doc__ half was in the compiler: CPython reserves co_consts[0] for the
# docstring and puts None there when there is none, and func_doc reads that
# slot.  Ours appended None at the end instead, so a function whose first
# constant was a string reported it as its docstring.

def documented():
    """A real docstring."""
    return 1

def undocumented():
    x = "not a docstring"
    return x

def empty():
    pass

def numeric():
    return 42

print(documented.__doc__, undocumented.__doc__, empty.__doc__, numeric.__doc__)
print(documented(), undocumented(), numeric())
print((lambda: "s")(), (lambda: "s").__doc__)

class C:
    """Class doc."""
    def m(self):
        """Method doc."""
        return 2
    def n(self):
        y = "no doc"
        return y

print(C.__doc__, C.m.__doc__, C.n.__doc__, C().m(), C().n())

# __name__ is settable, and it is stored on the function rather than in its
# __dict__, as CPython has it.
def f():
    return 1
print(f.__name__, f.__qualname__)
f.__name__ = "renamed"
print(f.__name__, f.__dict__)
try:
    f.__name__ = 5
except TypeError:
    print("__name__ must be a str")

# Other attributes still go to __dict__.
f.tag = "t"
print(f.tag, f.__dict__)

# __doc__ assignment wins over the source docstring, which is how
# collections.namedtuple writes them.
documented.__doc__ = "replaced"
print(documented.__doc__)
