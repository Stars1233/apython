"""Three places the compiler refused an unpacking, or a comma, that Python takes.

  * `del y,` -- a trailing comma adds no target.  CPython compiles it to a
    single DELETE_NAME with no tuple; this looped back into the expression
    parser and reported `invalid syntax`.

  * `Generic[*Ts]` -- a lone starred element in a subscript.  CPython builds
    `Subscript(slice=Tuple(elts=[Starred(...)]))`, the same shape `Generic[*Ts,]`
    and `Generic[int, *Ts]` already took here; without the wrap the bare Starred
    reached the expression emitter, which has none for it.

  * `class C(*bases)` -- the bases become one tuple and the call becomes
    CALL_FUNCTION_EX, rather than a plain CALL of pushed arguments.  It was
    `unpacking is not supported in a class base list`, reported at line 0.

The cases that must keep failing are here too: `x = *Ts` is still an error, and
so is a bare `del ,`.
"""


print("--- del with a trailing comma ---")
y = 1
del y,
print("single:", "y" in dir())

a = 1
b = 2
del a, b,
print("several:", "a" in dir(), "b" in dir())

lst = [1, 2, 3]
del lst[0],
print("subscript target:", lst)


class Holder:
    pass


h = Holder()
h.attr = 1
del h.attr,
print("attribute target:", hasattr(h, "attr"))

d = {"k": 1}
del d["k"],
print("dict target:", d)

for src in ("del ,", "y = 1\ndel y, ,", "del"):
    try:
        compile(src, "<t>", "exec")
        print("compiled - wrong:", repr(src))
    except SyntaxError:
        print("still an error:", repr(src))


print("--- a lone starred subscript ---")


class Sub:
    def __class_getitem__(cls, item):
        return ("got", item)


Ts = (int, str)
print("lone:", Sub[*Ts])
print("trailing comma:", Sub[*Ts,])
print("with a sibling:", Sub[int, *Ts])
print("two stars:", Sub[*Ts, *Ts])
print("nested:", Sub[(*Ts,)])

for src in ("Ts = (int,)\nx = *Ts", "Ts = (int,)\ndef f(): return *Ts"):
    try:
        compile(src, "<t>", "exec")
        print("compiled - wrong:", repr(src))
    except SyntaxError:
        print("still an error:", repr(src))


print("--- a starred class base list ---")


class A:
    pass


class B:
    pass


bases = (A, B)


class C(*bases):
    pass


print("C:", [c.__name__ for c in C.__mro__])


class D(A, *bases[1:]):
    pass


print("D:", [c.__name__ for c in D.__mro__])


class Meta(type):
    def __new__(mcls, name, bs, ns, **kw):
        cls = super().__new__(mcls, name, bs, ns)
        cls.kw = kw
        return cls


class E(*bases, metaclass=Meta):
    pass


print("E:", type(E).__name__, E.kw, [c.__name__ for c in E.__mro__])

kw = {"metaclass": Meta}


class F(*bases, **kw):
    pass


print("F:", type(F).__name__, F.kw, [c.__name__ for c in F.__mro__])


class G(**kw):
    pass


print("G:", type(G).__name__, G.kw, [c.__name__ for c in G.__mro__])


class H(*bases, metaclass=Meta, extra=1):
    pass


print("H:", sorted(H.kw.items()), [c.__name__ for c in H.__mro__])

empty = ()


class I(*empty):
    pass


print("I:", [c.__name__ for c in I.__mro__])


# A base list that is not a tuple, and one built at run time.
class J(*[A]):
    pass


print("J:", [c.__name__ for c in J.__mro__])


def make_bases():
    return (A,)


class K(*make_bases()):
    pass


print("K:", [c.__name__ for c in K.__mro__])


# The class body still sees its own name and module.
class L(*bases):
    marker = "in body"


print("L:", L.marker, L.__name__, L.__qualname__)

print("done")
