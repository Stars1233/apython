# ExceptionGroup.split, .subgroup and .derive.
#
# The splitting existed only as the thing `except*` calls, so a program could
# not do it itself: hasattr(ExceptionGroup, "split") was False.  And where
# CPython's split asks the group to derive() a new one -- whose default builds a
# plain BaseExceptionGroup, letting its constructor pick the class -- eg_split
# constructed one of the group's OWN type, so a subclass of ExceptionGroup split
# into more of itself.


def show(label, value):
    print(label, "->", repr(value))


eg = ExceptionGroup("outer", [ExceptionGroup("inner", [KeyError("k")]),
                              OSError("o")])

print("--- the three are published ---")
for name in ("split", "subgroup", "derive"):
    print(name, hasattr(ExceptionGroup, name),
          hasattr(BaseExceptionGroup, name), hasattr(eg, name))

print("--- split preserves the nesting in both halves ---")
m, r = eg.split(KeyError)
show("match", m)
show("rest", r)
show("match.exceptions", m.exceptions)
print("args[1] is a list:", type(m.args[1]).__name__)

print("--- one-sided splits answer None on the other side ---")
show("all", eg.split(BaseException))
show("none", eg.split(ZeroDivisionError))

print("--- subgroup is the match half alone ---")
show("subgroup", eg.subgroup(KeyError))
show("subgroup none", eg.subgroup(ZeroDivisionError))
show("subgroup all", eg.subgroup(Exception))

print("--- a tuple of types ---")
show("pair", eg.split((KeyError, OSError))[0])

print("--- derive builds a plain group, so a subclass splits into one ---")


class MyEG(ExceptionGroup):
    pass


mine = MyEG("m", [KeyError("a"), OSError("b")])
a, b = mine.split(KeyError)
print("subclass halves:", type(a).__name__, type(b).__name__)
print("subclass derive:", type(mine.derive([KeyError("x")])).__name__)


class DerEG(ExceptionGroup):
    def derive(self, excs):
        return DerEG(self.message, excs)


der = DerEG("d", [KeyError("a"), OSError("b")])
c, d = der.split(KeyError)
print("override halves:", type(c).__name__, type(d).__name__)

print("--- derive's default is BaseExceptionGroup, which picks the class ---")
print("all Exception:", type(eg.derive([ValueError("v")])).__name__)
beg = BaseExceptionGroup("b", [KeyboardInterrupt(), ValueError("v")])
print("mixed:", type(beg.derive([KeyboardInterrupt()])).__name__)
print("mixed all-exc:", type(beg.derive([ValueError("v")])).__name__)

print("--- a BaseExceptionGroup splits into the right classes ---")
bm, br = beg.split(ValueError)
print("base halves:", type(bm).__name__, type(br).__name__)
show("base match", bm)
show("base rest", br)

print("--- the leaves are the originals ---")
leaf = KeyError("leaf")
nested = ExceptionGroup("o", [ExceptionGroup("i", [leaf]), OSError("s")])
nm, nr = nested.split(KeyError)
print("same leaf:", nm.exceptions[0].exceptions[0] is leaf)
print("new inner:", nm.exceptions[0] is not nested.exceptions[0])

print("--- __notes__ survive ---")
eg2 = ExceptionGroup("n", [KeyError("k")])
eg2.add_note("a note")
n1, _ = eg2.split(KeyError)
print("notes:", getattr(n1, "__notes__", None))

print("--- arity ---")
for call in ("split()", "split(KeyError, 1)", "subgroup()", "derive()"):
    try:
        eval("eg." + call)
        print(call, "accepted")
    except TypeError as e:
        print(call, "TypeError")

print("--- a derive that answers the wrong thing ---")


class BadDer(ExceptionGroup):
    def derive(self, excs):
        return 5


try:
    BadDer("x", [KeyError("k")]).split(KeyError)
    print("bad derive accepted")
except TypeError:
    print("bad derive refused")

print("done")
