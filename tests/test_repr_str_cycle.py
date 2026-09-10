# __repr__ and __str__ that reach each other have to stop.
#
# `Foo.__repr__ = Foo.__str__` makes the two chase each other: instance_repr
# finds __repr__, which is object.__str__, which asks for __repr__ again.  No
# Python frame is entered anywhere in that loop, so recursion_depth never
# moved and the machine stack ran out.  CPython raises RecursionError, and its
# test_descr.test_repr_as_str (issue 11603) is the test for it.
#
# The counter is the C-level one, reset wholesale when the exception unwinds.

import gc


class Foo:
    pass


Foo.__repr__ = Foo.__str__
foo = Foo()

for label, fn in (("str", lambda: str(foo)), ("repr", lambda: repr(foo))):
    try:
        fn()
    except RecursionError:
        print(label, "RecursionError")
    else:
        print(label, "NO ERROR")

# Twice more, to prove the counter came back.
for i in range(3):
    try:
        str(foo)
    except RecursionError:
        pass
print("survived")

# The other direction, written out in the class body.
class Baz:
    def __repr__(self):
        return str(self)

    def __str__(self):
        return repr(self)


try:
    repr(Baz())
except RecursionError:
    print("mutual RecursionError")
try:
    str(Baz())
except RecursionError:
    print("mutual RecursionError (str)")

# A container holding one of these reports it rather than crashing, and gives
# its buffer back on the way.
try:
    repr([Baz()])
except RecursionError:
    print("in a list")
try:
    repr({1: Baz()})
except RecursionError:
    print("in a dict")
try:
    repr((Baz(),))
except RecursionError:
    print("in a tuple")
gc.collect()

# And ordinary reprs still work afterwards.
class Fine:
    def __repr__(self):
        return "<Fine>"


print(repr(Fine()), str(Fine()), "%r" % (Fine(),), f"{Fine()!r}")
print(repr([Fine(), {1: Fine()}, (Fine(),)]))
print(repr(object()) .startswith("<object object at 0x"))


class OnlyStr:
    def __str__(self):
        return "only-str"


print(str(OnlyStr()), repr(OnlyStr()).startswith("<"))
print("done")
