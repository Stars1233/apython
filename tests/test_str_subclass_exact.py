# str() of a str subclass answers an exact str, as every other builtin
# constructor already did.
#
# str_str -- which is str_type.tp_str, and therefore what both `str(x)` and
# `str.__str__(x)` reach -- increfed its argument and returned it.  For an
# exact str that is right and is what CPython does; for a SUBCLASS it is
# wrong, and it was the only builtin that got it wrong: bytes, int, float,
# tuple, list and dict all normalise a subclass argument to the exact type.
#
# It is not a cosmetic difference.  The idiom
#
#     def __getnewargs__(self): return (str(self), self.token_type)
#
# is how a str subclass tells pickle and copy how to rebuild itself, and
# every token class in CPython's email._header_value_parser is written that
# way.  With str(self) answering the subclass, copy.deepcopy recursed for
# ever -- __getnewargs__ handing back an object of the same type it was
# reducing -- and the native stack went before the recursion limit could
# fire.  A SIGSEGV, and the whole of CPython's test_email.
import copy

class S(str):
    pass


s = S("hi")
print("type(str(s)):", type(str(s)).__name__)
print("type(s.__str__()):", type(s.__str__()).__name__)
print("str(s) is s:", str(s) is s)
print("value preserved:", str(s) == "hi")

# An exact str is still handed straight back -- CPython's unicode_str does
# that and a copy per str() would be a real cost.
t = "plain"
print("exact str is identical:", str(t) is t)

# The other constructors, for the record: this is the rule str was breaking.
class B(bytes): pass
class I(int): pass
class F(float): pass
class T(tuple): pass
class L(list): pass
class D(dict): pass
print("bytes:", type(bytes(B(b"x"))).__name__)
print("int:", type(int(I(5))).__name__)
print("float:", type(float(F(1.5))).__name__)
print("tuple:", type(tuple(T((1,)))).__name__)
print("list:", type(list(L([1]))).__name__)
print("dict:", type(dict(D())).__name__)

# repr is NOT normalised, because repr of a str subclass is the subclass's
# own business and CPython answers a plain str there for a different reason.
print("repr:", repr(s))

# format() and f-strings go through __format__, not __str__, but both have to
# answer a str.
print("format:", type(format(s)).__name__, format(s))
print("fstring:", f"{s}")

# A str subclass carrying state, reduced and deep-copied: the shape that
# crashed.  __getnewargs__ must see a plain str, or the reconstructor is
# handed an object of the type it is trying to build.
class VT(str):
    def __new__(cls, value, token_type):
        self = super().__new__(cls, value)
        self.token_type = token_type
        return self

    def __getnewargs__(self):
        return (str(self), self.token_type)


v = VT("hello", "vt")
print("newargs:", v.__getnewargs__(), [type(x).__name__ for x in v.__getnewargs__()])
c = copy.deepcopy(v)
print("deepcopy:", c, c.token_type, type(c).__name__)
c2 = copy.copy(v)
print("copy:", c2, c2.token_type, type(c2).__name__)

# pickle is the other consumer of __getnewargs__ and belongs here too, but
# lib/pickle.py is still a stub that raises NotImplementedError; the round
# trip joins this file in the commit that replaces it.  copy.deepcopy above
# reaches __getnewargs__ through the same copyreg.__newobj__ path, so the
# protocol is covered either way.

# str() of an object whose __str__ answers a str subclass is left alone:
# CPython only normalises what str.__str__ itself returns.
class Weird:
    def __str__(self):
        return S("from __str__")


print("via __str__:", type(str(Weird())).__name__, str(Weird()))
print("survived")
