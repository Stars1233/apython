# Two more the whole-suite run found.
#
# `+x` is not the identity.  It calls __pos__, which a numeric class defines
# like any other operator; CPython routes it through CALL_INTRINSIC_1.
#
# A local that is deleted anywhere in its block has to be read with
# LOAD_FAST_CHECK, or reading it after the delete hands back whatever the slot
# still holds instead of raising.  The name an `except E as e` binds counts:
# the clause deletes it on the way out.
#
# And the symbol table's generic walk treated an except clause's bound NAME as
# a node index.  Object indices and node indices come from different arenas and
# collide freely, so `except E as e` visited whatever node sat at e's object
# index -- and where that node was a function or a lambda, it got a second
# scope that overwrote the one stamped on it by its real owner.  The owner then
# read someone else's scope and its closure came out empty, which showed up as
# a NameError from a lambda in an unrelated comprehension hundreds of lines
# away.  Only a whole file makes the two index spaces collide.
SRC = '''
class Num:
    def __init__(self, v):
        self.v = v

    def __pos__(self):
        return ("pos", self.v)

    def __neg__(self):
        return ("neg", self.v)

    def __invert__(self):
        return ("inv", self.v)


n = Num(3)
print(+n, -n, ~n, +5, -5, ~5, +-5)


def deleted():
    y = 42
    del y
    try:
        return y
    except NameError as e:
        return "unbound: " + type(e).__name__


def except_name():
    try:
        raise OSError("o")
    except OSError as e:
        pass
    try:
        return e
    except NameError:
        return "e is gone"


print(deleted(), except_name())


def guard(f):
    try:
        return f()
    except Exception as e:
        return type(e).__name__


print([guard(lambda: [0] * k) for k in (1, 2)])
print([guard(lambda: (1,) * k) for k in (1, 2)])
print([guard(lambda: ("a" * 2) * k) for k in (1, 2)])


def later(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


print(later(lambda: 1 / 0), later(lambda: 7))
'''
ns = {}
exec(compile(SRC, "<t>", "exec"), ns)
