"""The module docstring."""
# A leading string literal in a module or a class body binds __doc__.  Our
# compiler emitted it as an expression statement and threw the value away, so
# only functions -- whose docstring is read off co_consts[0] -- had one.
import sys


class Documented:
    """A class docstring."""

    def method(self):
        """A method docstring."""
        return 1


class Undocumented:
    x = 1


def documented():
    "one line"


def undocumented():
    pass


print(__doc__)
print(Documented.__doc__)
print(Documented.method.__doc__)
print(Undocumented.__doc__)
print(documented.__doc__, undocumented.__doc__)
print(Documented().__doc__)

# An expression that merely starts with a string is not a docstring.
class NotADoc:
    "abc" + "def"


print(NotADoc.__doc__)


class NumberFirst:
    42


print(NumberFirst.__doc__)

# A docstring under our own compiler, at run time.
ns = {}
exec('"exec doc"\nclass K:\n    "K doc"\n', ns)
print(ns["__doc__"], ns["K"].__doc__)

src = 'class J:\n    """J doc"""\n    pass\n'
ns2 = {}
exec(compile(src, "<s>", "exec"), ns2)
print(ns2["J"].__doc__)

# A class may still override it.
class Override:
    "original"
    __doc__ = "replaced"


print(Override.__doc__)
