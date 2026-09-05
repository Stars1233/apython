"""A user exception's own __init__, and the keywords it takes.

BaseException.__init__ is what stores .args and what refuses keywords, and
it only runs when the class does not define its own -- so a class that does
takes whatever ITS __init__ takes.  Here the keyword check ran first and
unconditionally, which made every user exception with a keyword parameter
unconstructible: `raise E("boom", code=7)` answered "E() takes no keyword
arguments".

And __init__ is inherited.  It was looked up in the type's OWN tp_init slot,
which type_from_parts fills only from the class's own body, so
`class F(E): pass` ran nothing at all.
"""


class WithKeyword(Exception):
    def __init__(self, msg, code=None, hint="none"):
        super().__init__(msg)
        self.code = code
        self.hint = hint


class Inherits(WithKeyword):
    pass


class Plain(Exception):
    pass


class NoSuper(Exception):
    def __init__(self, a, b=2):
        self.pair = (a, b)


print("--- a keyword parameter of its own ---")
e = WithKeyword("boom", code=7)
print(e.args, e.code, e.hint)
e = WithKeyword("boom", code=7, hint="try again")
print(e.args, e.code, e.hint)
e = WithKeyword("boom")
print(e.args, e.code, e.hint)
e = WithKeyword("boom", 3, "h")
print(e.args, e.code, e.hint)

print("--- raised and caught ---")
try:
    raise WithKeyword("raised", code=9)
except WithKeyword as caught:
    print(caught.args, caught.code, str(caught))

print("--- and it is inherited ---")
f = Inherits("sub", code=4)
print(type(f).__name__, f.args, f.code, f.hint)
try:
    raise Inherits("sub-raised", hint="h")
except WithKeyword as caught:
    print(type(caught).__name__, caught.args, caught.hint)

print("--- an __init__ that does not call super ---")
n = NoSuper(1)
print(n.pair, n.args)
n = NoSuper(1, b=5)
print(n.pair, n.args)

print("--- a class with no __init__ of its own still refuses keywords ---")
for cls in (Plain, Exception, ValueError, TypeError):
    try:
        cls("x", nope=1)
        print(cls.__name__, "accepted")
    except TypeError as t:
        print(cls.__name__, "TypeError", t)

print("--- the two builtins that do take keywords still do ---")
a = AttributeError("no attr", name="spam", obj=None)
print(a.args, a.name, a.obj)
i = ImportError("no mod", name="m", path="/p")
print(i.args, i.name, i.path)
try:
    ValueError("x", name="n")
except TypeError as t:
    print("TypeError", t)

print("--- and a wrong keyword reaches the __init__ that would take it ---")
try:
    WithKeyword("x", nonesuch=1)
except TypeError:
    print("TypeError for an unknown keyword")
print("done")
