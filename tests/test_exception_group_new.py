# An exception is allocated at its TYPE's size, and a group is built by the
# group's constructor.
#
# Two halves of the same crash, both exposed the moment a Python __new__ on an
# exception subclass started running.
#
# exc_new allocated a fixed PyExceptionObject_size.  That is right for the
# sixty-nine builtins and wrong for anything wider: BaseExceptionGroup carries
# eg_exceptions past the end of that layout, so a group built through
# BaseException.__new__ was eight bytes short and eg_split read the field off
# the end of the block.  A subclass with __slots__ is the same shape --
# type_from_parts puts them after tp_basicsize, which the allocation was not
# consulting.
#
# And `super().__new__(cls, msg, excs)` inside an ExceptionGroup subclass
# lands on BaseException.__new__, because BaseExceptionGroup publishes no
# __new__ of its own for the MRO walk to stop at.  It used to answer a plain
# exception whose eg_exceptions was never written, which `except*` then read
# as a tuple.
class EG(ExceptionGroup):
    def __new__(cls, message, excs):
        obj = super().__new__(cls, message, excs)
        obj.tag = "made"
        return obj

    def derive(self, excs):
        return EG(self.message, excs)


g = EG("group", [ValueError("v"), TypeError("t")])
print(type(g).__name__, g.message, g.tag)
print([type(e).__name__ for e in g.exceptions])
print(g.args[0], [type(e).__name__ for e in g.args[1]])

caught = []
try:
    raise g
except* ValueError as e:
    caught.append(("V", [type(x).__name__ for x in e.exceptions]))
except* TypeError as e:
    caught.append(("T", [type(x).__name__ for x in e.exceptions]))
print(caught)

# The plain group, unchanged.
p = ExceptionGroup("plain", [KeyError("k")])
print(type(p).__name__, p.message, [type(e).__name__ for e in p.exceptions])
try:
    raise p
except* KeyError as e:
    print("plain split:", [type(x).__name__ for x in e.exceptions])

# A subclass with __slots__: its slots live past the base layout, so the
# allocation has to be the subclass's size.
class Slotted(Exception):
    __slots__ = ("a", "b", "c", "d")


s = Slotted("m")
s.a, s.b, s.c, s.d = 1, 2, 3, 4
print(s.args, s.a, s.b, s.c, s.d)
print(Slotted.__basicsize__ > Exception.__basicsize__)

import gc
alive = []
for i in range(300):
    x = Slotted("m%d" % i)
    x.a, x.b, x.c, x.d = i, i, i, i
    alive.append(x)
    if i % 50 == 0:
        gc.collect()
print("slotted survive:", len(alive), alive[-1].a, alive[-1].args)

many = []
for i in range(300):
    e = EG("g%d" % i, [ValueError(i)])
    many.append(e)
    if i % 50 == 0:
        gc.collect()
gc.collect()
print("groups survive:", len(many), many[-1].message, many[-1].tag)
print([type(x).__name__ for x in many[-1].exceptions])
