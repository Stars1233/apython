# A str subclass instance carries a __dict__.
#
# A str keeps its characters inline, so there is no fixed offset past the
# header to put one at -- which is why the family had none, and why setting any
# attribute on such an instance raised.  The dict pointer goes at the tail
# instead, past the data and its NUL padding, and tp_dictoffset carries a
# sentinel saying so; that is the same trick CPython plays with a negative
# offset.
#
# enum needs it: a StrEnum member is a str subclass instance, and enum sets
# _value_ and _name_ on it.
class S(str):
    pass


s = S("abc")
s.tag = "t"
s.n = 1
print(s, len(s), s.upper(), s.tag, s.n)
print(sorted(s.__dict__.items()))
print(s == "abc", "b" in s, s[1], s + "d", type(s + "d").__name__)

# Empty, long, and non-ASCII strings all keep working with a dict on the end.
for text in ("", "x", "a" * 200, "héllo"):
    v = S(text)
    v.mark = len(text)
    assert v == text and v.mark == len(text) and len(v) == len(text), text
print("lengths ok")


class WithInit(str):
    def __new__(cls, v, extra):
        self = super().__new__(cls, v)
        self.extra = extra
        return self


w = WithInit("hi", 42)
print(w, w.extra, len(w), w.__dict__)


# Attributes do not leak between instances, and the base is unaffected.
a, b = S("p"), S("q")
a.x = 1
print(hasattr(b, "x"), a.x)
try:
    "plain".attr = 1
    print("plain str took an attribute")
except AttributeError:
    print("plain str still rejects one")

# Subclassing further keeps the dict.
class T(S):
    pass


t = T("z")
t.deep = True
print(t, t.deep, isinstance(t, S), isinstance(t, str))

# And the int subclass case, which already worked, still does.
class I(int):
    pass


i = I(7)
i.tag = "i"
print(i, i.tag, i + 1)
