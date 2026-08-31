# An int subclass that defines __new__ must have it called.  The construction
# path took the int shortcut before looking for one, so `NIC(7, "SEVEN")`
# reached int()'s two-argument form and complained that "SEVEN" was not a
# base.  re/_constants.py builds every one of its opcode constants that way.
class NIC(int):
    def __new__(cls, value, name):
        self = super(NIC, cls).__new__(cls, value)
        self.name = name
        return self

    def __repr__(self):
        return self.name


x = NIC(7, "SEVEN")
print(x, int(x), x.name, x + 1, x * 2, x == 7)
print(type(x).__name__, isinstance(x, int), repr(x))

ys = [NIC(i, "n%d" % i) for i in range(3)]
print(ys, [int(y) for y in ys], sorted(ys, reverse=True))


# __init__ still runs, and keyword arguments reach __new__.
class Tagged(int):
    def __new__(cls, value, tag="t"):
        self = super().__new__(cls, value)
        self.tag = tag
        return self

    def __init__(self, value, tag="t"):
        self.seen = True


t = Tagged(3, tag="z")
print(t, t.tag, t.seen)
print(Tagged(4).tag)


# A subclass with no __new__ of its own still takes the short path.
class Plain(int):
    pass


print(Plain(5) + 1, Plain("6") + 1, Plain("ff", 16), type(Plain(5)).__name__)


# The same for str, which already worked, so it stays working.
class NamedStr(str):
    def __new__(cls, value, name):
        self = super().__new__(cls, value)
        self.name = name
        return self


s = NamedStr("ab", "AB")
print(s, s.name, s.upper(), len(s))


# int.__new__ reached directly.
print(int.__new__(NIC, 9).__class__.__name__)
print(int.__new__(int, 11) + 1)
