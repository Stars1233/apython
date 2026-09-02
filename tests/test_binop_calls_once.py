# An operator dunder runs exactly as many times as it is written to.
#
# Once every heaptype overriding an operator holds the SAME wrapper in its
# nb_ slot, op_binary_op can reach that one method by three roads: the left
# type's slot, the right type's slot -- which is offered the operands in
# their ORIGINAL order, so it would call the left object's method again --
# and the by-name dunder arm.  A pure method makes that invisible; a method
# that prints, counts or mutates makes it a bug.
#
# CPython avoids the second by dropping the right slot when both types
# resolve to the same function, and the third by never asking twice.

log = []


class Decliner:
    def __add__(self, o):
        log.append("Decliner.__add__")
        return NotImplemented
    def __mul__(self, o):
        log.append("Decliner.__mul__")
        return NotImplemented


class Reflected:
    def __radd__(self, o):
        return "Reflected.__radd__"
    def __rmul__(self, o):
        return "Reflected.__rmul__"


class AlsoDeclines:
    def __add__(self, o):
        log.append("AlsoDeclines.__add__")
        return NotImplemented
    def __radd__(self, o):
        log.append("AlsoDeclines.__radd__")
        return "AlsoDeclines.__radd__"


def run(label, fn):
    del log[:]
    try:
        result = fn()
    except TypeError as e:
        result = "TypeError"
    print("%-24s %-24s %s" % (label, result, log))


run("declines then reflected", lambda: Decliner() + Reflected())
run("multiply, same shape", lambda: Decliner() * Reflected())
run("both are heaptypes", lambda: Decliner() + AlsoDeclines())
run("neither can serve", lambda: Decliner() + Decliner())
run("int on the right", lambda: Decliner() + 1)
run("int on the left", lambda: 1 + Decliner())


# The in-place forms have the same three roads, plus the __iadd__/__add__
# fallback between them.
class InplaceDeclines:
    def __iadd__(self, o):
        log.append("__iadd__")
        return NotImplemented
    def __add__(self, o):
        log.append("__add__")
        return "InplaceDeclines.__add__"


def augmented():
    x = InplaceDeclines()
    x += 1
    return x


run("iadd declines to add", augmented)


# __iadd__ = None blocks the fallback entirely, whether or not a base
# defined one.
class Base:
    def __iadd__(self, o): return "Base.__iadd__"
    def __add__(self, o): return "Base.__add__"


class Blocked(Base):
    __iadd__ = None


def blocked():
    x = Blocked()
    x += 1
    return x


def unblocked():
    x = Base()
    x += 1
    return x


run("base iadd runs", unblocked)
run("None blocks it", blocked)
