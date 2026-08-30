# Instance layout is described by tp_dictoffset rather than a fixed +16, so
# that a subclass of a builtin container can embed the base's layout and put
# its __dict__ after it.  This commit is the plumbing; the families that need
# it come next.  The cases here are the ones the change can break: nested
# __slots__, a heaptype deriving from another heaptype (which must share the
# base's dict rather than adding a second one on top of it), int subclasses,
# and exception subclasses.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


class Plain:
    def __init__(self):
        self.x = 1


class DerivedPlain(Plain):
    pass


p = Plain()
d = DerivedPlain()
d.y = 2
print(p.x, d.x, d.y)


class Slotted:
    __slots__ = ("a", "b")


s = Slotted()
s.a, s.b = 1, 2
print(s.a, s.b, t(lambda: setattr(s, "c", 3)))


class SlottedChild(Slotted):
    __slots__ = ("c",)


sc = SlottedChild()
sc.a, sc.b, sc.c = 1, 2, 3
print(sc.a, sc.b, sc.c)


class SlottedThenDict(Slotted):
    pass


sd = SlottedThenDict()
sd.a = 1
sd.anything = "ok"
print(sd.a, sd.anything)


class MyInt(int):
    pass


mi = MyInt(7)
print(int(mi), mi + 1, type(mi).__name__)
mi.tag = "z"
print(mi.tag)


class MyErr(Exception):
    pass


e = MyErr("boom")
e.code = 42
print(e.args, e.code, isinstance(e, Exception))


class Deep(MyErr):
    pass


de = Deep("d")
de.extra = 1
print(de.args, de.extra, isinstance(de, MyErr))

# Reference cycles through the instance dict must still be collectable, which
# means traverse and clear have to find the dict at the right offset too.
class Node:
    pass


for _ in range(50):
    a = Node()
    b = Node()
    a.peer = b
    b.peer = a
    del a, b
print("cycles ok")
