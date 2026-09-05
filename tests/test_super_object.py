# super is an object, not just an opcode.
#
# LOAD_SUPER_ATTR handles `super(...).attr` written out in full, and that is
# all the compiler ever emits it for.  Everything else -- storing a super,
# passing one, reaching one through getattr, or writing `super(B, B).m` at
# module level, which CPython compiles as an ordinary call -- goes through the
# type.  There used to be no type: `super_type` was a table of zeros whose
# ob_type was itself, so calling it found no tp_call and raised "object is not
# callable".
#
# The lookup is one routine now, shared by the opcode's attribute form and by
# the object's tp_getattr, so the two cannot answer differently.

class A:
    def m(self):
        return "A.m"

    @classmethod
    def c(cls):
        return "A.c " + cls.__name__

    @staticmethod
    def s():
        return "A.s"

    @property
    def p(self):
        return "A.p"

    x = "A.x"

    def __repr__(self):
        return "<A>"


class B(A):
    def stored(self):
        sup = super()
        return sup.m()

    def stored_two(self):
        sup = super(B, self)
        return sup.m()

    def through_getattr(self):
        return getattr(super(), "m")()

    def passed(self):
        return takes_a_super(super())

    def prop(self):
        return super().p

    def stat(self):
        return super().s()

    def cls(self):
        return super().c()

    def attr(self):
        return super().x


def takes_a_super(sup):
    return "passed " + sup.m()


b = B()
for name in ("stored", "stored_two", "through_getattr", "passed", "prop",
             "stat", "cls", "attr"):
    try:
        print(name.ljust(20), getattr(b, name)())
    except Exception as exc:
        print(name.ljust(20), type(exc).__name__ + ":", exc)


# --- the unbound form: __self__ IS the class -------------------------------
#
# CPython's rule is that a descriptor reached through super binds to __self__
# unless __self__ is __self_class__ itself, which is what makes super(B, B).m
# the plain function.  It used to be a bound method here.
print("super(B, B).m is A.m", super(B, B).m is A.m)
print("super(B, B).c()     ", super(B, B).c())
print("super(B, B).s()     ", super(B, B).s())
print("super(B, b).m       ", super(B, b).m)
print("super(B, b).m()     ", super(B, b).m())


# --- the object itself ------------------------------------------------------
sup = super(B, b)
print("type               ", type(sup))
print("type name          ", type(sup).__name__)
print("__self__ is b      ", sup.__self__ is b)
print("__thisclass__      ", sup.__thisclass__ is B)
print("__self_class__     ", sup.__self_class__ is B)
print("repr               ", repr(sup))
print("repr, class second ", repr(super(A, b)))

# super(B) with no object at all: legal, and unbound until it is bound.
half = super(B)
print("repr, no object    ", repr(half))
print("half __self__      ", half.__self__)
print("half __self_class__", half.__self_class__)


# --- the errors -------------------------------------------------------------
def check(label, fn):
    try:
        print(label.ljust(20), fn())
    except Exception as exc:
        print(label.ljust(20), type(exc).__name__ + ":", exc)


check("obj not an instance", lambda: super(B, 42))
check("type not a type    ", lambda: super(1, 2))
check("three arguments    ", lambda: super(B, b, b))
check("no frame           ", lambda: super())
check("no such attribute  ", lambda: super(B, b).nope)
check("unbound lookup     ", lambda: super(B).m)


# --- a diamond, which is what super is for ---------------------------------
class L:
    def who(self):
        return "L"


class M(L):
    def who(self):
        return "M " + super().who()


class N(L):
    def who(self):
        return "N " + super().who()


class D(M, N):
    def who(self):
        return "D " + super().who()

    def stored_who(self):
        sup = super()
        return "D " + sup.who()


print("diamond, opcode    ", D().who())
print("diamond, object    ", D().stored_who())
print("mro                ", [c.__name__ for c in D.__mro__])


# --- a builtin base, where the attribute is a builtin method ---------------
class MyList(list):
    def __init__(self, *args):
        super().__init__(*args)

    def stored_init(self, args):
        sup = super()
        sup.__init__(args)


ml = MyList([1, 2, 3])
print("list subclass      ", ml)
ml2 = MyList()
ml2.stored_init([4, 5])
print("stored super init  ", ml2)

# A super held across a call is still a super.
sups = [super(B, b) for _ in range(3)]
print("three of them      ", [s.m() for s in sups])
print("done")
