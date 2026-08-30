# type(name, bases, namespace) builds a class exactly as a class statement
# does.  It used to fall through to type_call's .normal_type_call, which
# treats type_type as an ordinary class: it allocated a PyInstanceObject-sized
# block and let type fields be written into it, so the result printed as
# <class ''> and the process then aborted with "double free or corruption".
#
# The construction is now shared with __build_class__ through type_from_parts,
# so the two cannot drift apart.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


S = type("S", (), {"x": 1})
print(S.__name__, S().x, type(S()).__name__, isinstance(S(), S))

T = type("T", (object,), {"y": 2, "m": lambda self: "meth"})
print(T.__name__, T().y, T().m(), isinstance(T(), T))


class Base:
    kind = "base"

    def hello(self):
        return "base hello"


U = type("U", (Base,), {"z": 3})
u = U()
print(U.__name__, u.z, u.kind, u.hello(), isinstance(u, Base), issubclass(U, Base))

# A namespace with __init__ is honoured
V = type("V", (), {"__init__": lambda self, n: setattr(self, "n", n)})
print(V(7).n)

# The one-argument form still works
print(type(1).__name__, type("a").__name__, type([]).__name__, type(S()).__name__)

# Argument validation
print([t(lambda: type(v, (), {})) for v in (5, None, b"x")])
print(t(lambda: type("A", [], {})), t(lambda: type("A", (), [])))
# Two bases would be multiple inheritance, which __build_class__ does not
# implement either -- it reads only args[2], so a class statement silently
# drops the second base.  type() reports it instead of dropping it, but the
# exact behaviour is left out of this test until MI exists.

# A class statement still behaves the same
class Direct(Base):
    z = 3


d = Direct()
print(Direct.__name__, d.z, d.hello(), isinstance(d, Base))
