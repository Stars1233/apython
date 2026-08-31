# A class body is not function-like, so a name it binds is never a cell.
#
# `name = "H"` has to reach the class dict through STORE_NAME.  Promoting it to
# a cell compiled the store to STORE_DEREF, so H.name did not exist at all and
# the method read the class body's value instead of the enclosing function's.
#
# CPython runs analyze_cells for a FunctionBlock only; a ClassBlock runs
# drop_class_free, and update_symbols marks the captured name DEF_FREE_CLASS so
# it lands in co_freevars while its scope stays LOCAL.  Both halves matter: the
# class body still needs the name among its free variables, because the
# MAKE_FUNCTION for the method is built inside the class body and its closure
# tuple is LOAD_CLOSURE'd from a slot there.
def make(name):
    class H:
        name = "H"

        def get(self):
            return name

    return H


H = make("param")
print(H().get(), H.name)


# A class body that BINDS the name reads it through the class dict, not the
# enclosing cell: LOAD_NAME, which falls back to globals and then raises.
def shadow(v):
    class S:
        try:
            seen = v
        except NameError:
            seen = "NameError"
        v = "class"

        def read(self):
            return v

    return S


S = shadow("outer")
print(S.seen, S.v, S().read())


# A comprehension in a class body is a nested block like any other: its free
# variable resolves PAST the class body, so the class attribute and the
# captured value are different objects.
def with_comp(n):
    class C:
        n = 9
        ys = [i + n for i in range(2)]

        def get(self):
            return n

    return C, n


C, outer_n = with_comp(3)
print(C.n, C.ys, C().get(), outer_n)


# __class__ must still get a real cell in the class body -- that is what
# LOAD_CLOSURE __class__ / STORE_NAME __classcell__ hands __build_class__ --
# alongside an ordinary captured local in the same body.
def with_super(tag):
    class Base:
        def who(self):
            return "base"

    class D(Base):
        tag = "class-tag"

        def who(self):
            return super().who() + "/" + tag

        def mytag(self):
            return self.tag

    return D


D = with_super("captured")
print(D().who(), D().mytag(), D.tag)


# A `global` in a class body governs the class body's own stores, not the
# method's read: the assignment goes to the module and the class still carries
# a free variable for the method's closure.
g = "module"


def cls_global():
    g = "func"

    class G:
        global g
        g = "set-by-class"

        def read(self):
            return g

    return G, g


G, fg = cls_global()
print(g, fg, G().read(), hasattr(G, "g"))


# Two class bodies deep: the inner one flags its own capture, and the outer one
# passes it further up even though it does not bind the name itself.
def two(a, b):
    class Outer:
        a = "outer-a"

        class Inner:
            b = "inner-b"

            def get_b(self):
                return b

        def get_a(self):
            return a

    return Outer


O = two("A", "B")
print(O().get_a(), O.a, O.Inner().get_b(), O.Inner.b)


# A lambda captures exactly as a def does.
def lam(v):
    class L:
        v = 1
        f = lambda self: v

    return L


print(lam(42)().f(), lam(42).v)


# More than one captured name in one class body, to pin the freevars order.
def multi(p, q):
    class M:
        p = "cp"
        q = "cq"

        def get(self):
            return p, q

    return M


M = multi(1, 2)
print(M().get(), M.p, M.q)
