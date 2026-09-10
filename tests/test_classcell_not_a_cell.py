# __classcell__ has to BE a cell before the class is written into it.
#
# The compiler leaves the cell under that name for a body whose methods use
# `super()` or `__class__`, and type_from_parts fills it in.  A metaclass can
# put anything there instead -- CPython's own test_super puts None, 0, "" and
# object() there on purpose -- and this wrote the finished class straight into
# PyCellObject.ob_ref of whatever it found: into the None singleton, or
# through an int immediate as if it were an address.
#
# CPython's type_new asks the same question and words the refusal the same
# way.  __classdictcell__, PEP 695's parallel, gets the same check.


class Meta(type):
    def __new__(cls, name, bases, namespace, cell):
        namespace['__classcell__'] = cell
        return super().__new__(cls, name, bases, namespace)


for bad_cell in (None, 0, "", object(), [], 1.5, True, (1,)):
    try:
        class A(metaclass=Meta, cell=bad_cell):
            pass
        print(type(bad_cell).__name__, "-> accepted")
    except TypeError as e:
        print(type(bad_cell).__name__, "->", e)

# A real cell still works, which is the ordinary case: a body that uses
# super() leaves one, and the class has to arrive in it.
class Base:
    def who(self):
        return "Base"


class Derived(Base):
    def who(self):
        return super().who() + "->Derived"

    def cls(self):
        return __class__


d = Derived()
print("super still works:", d.who(), d.cls() is Derived)


# And through a metaclass that passes the real cell along.
class Passthrough(type):
    def __new__(cls, name, bases, namespace, **kw):
        return super().__new__(cls, name, bases, namespace)


class WithMeta(Base, metaclass=Passthrough):
    def who(self):
        return super().who() + "->WithMeta"


print("through a metaclass:", WithMeta().who())
print("done")
