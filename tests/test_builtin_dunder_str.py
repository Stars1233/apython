# A builtin's __str__ and __repr__ have to be reachable by name and be
# distinguishable from object's.  str.__str__ resolved to object.__str__, so
# enum's ReprEnum handling -- "if member_type.__str__ is object.__str__, use
# its __repr__ instead" -- picked the wrong one, and every StrEnum member
# printed as <Names object>.
print(str.__str__ is object.__str__, str.__repr__ is object.__repr__)
print(int.__str__ is object.__str__, float.__str__ is object.__str__)
print(bytes.__str__ is object.__str__)

print(str.__str__("ab"), str.__repr__("ab"))
print(int.__str__(7), int.__repr__(7))
print(float.__str__(1.5), float.__repr__(1.5))
print(bytes.__repr__(b"xy"))

# On subclasses, the inherited one still works and still names the base's
# behaviour, which is what ReprEnum relies on.
class S(str):
    pass


class I(int):
    pass


print(S.__str__ is str.__str__, I.__str__ is int.__str__)
print(str(S("q")), str(I(5)))

# Assigning the base's method onto a subclass gives back the base behaviour,
# which is exactly what enum does for a ReprEnum.
class T(str):
    def __repr__(self):
        return "<T>"


T.__str__ = str.__str__
t = T("zz")
print(str(t), repr(t), "%s" % t, format(t))

# object's own are unchanged.
class Plain:
    pass


p = Plain()
print(str(p) == repr(p))


# object.__str__ defers to the type's __repr__, which is what makes a class
# that defines only __repr__ print the same either way.
class OnlyRepr:
    def __repr__(self):
        return "<only>"


o = OnlyRepr()
print(str(o), object.__str__(o), repr(o))
