# An argument list's order: what CPython accepts, what it refuses, and where
# it says the refusal is.  Everything here goes through exec(), so it is our
# own compiler that parses it rather than the .pyc's.


class Meta(type):
    def __new__(mcls, name, bases, ns, **kwds):
        cls = super().__new__(mcls, name, bases, ns)
        cls.kwds = dict(sorted(kwds.items()))
        return cls


class B:
    pass


class D:
    pass


def run(src):
    g = {"Meta": Meta, "B": B, "D": D, "bs": (D,), "D": D, "kw": {"a": 1}}
    try:
        exec(src, g)
    except SyntaxError as e:
        return "SyntaxError: %s | %s %s %s %s" % (
            e.msg, e.lineno, e.offset, e.end_lineno, e.end_offset)
    except Exception as e:
        return "%s: %s" % (type(e).__name__, e)
    return g.get("r", g.get("C"))


print("--- a star may follow a keyword ---")
c = run("class C(metaclass=Meta, *bs): pass")
print("bases:", c.__bases__, "kwds:", c.kwds)
c = run("class C(*bs, metaclass=Meta): pass")
print("bases:", c.__bases__, "kwds:", c.kwds)
c = run("class C(B, metaclass=Meta, *bs): pass")
print("bases:", c.__bases__, "kwds:", c.kwds)
print("call:", run("def f(*a, **k): return (a, sorted(k))\nr = f(1, x=2, *bs)"))

print("--- a keyword may follow a mapping unpack ---")
c = run("class C(**kw, extra=1, metaclass=Meta): pass")
print("kwds:", c.kwds)
c = run("class C(extra=1, **kw, metaclass=Meta): pass")
print("kwds:", c.kwds)
c = run("class C(*bs, **kw, metaclass=Meta): pass")
print("bases:", c.__bases__, "kwds:", c.kwds)
print("call:", run("def f(*a, **k): return (a, sorted(k.items()))\nr = f(**kw, b=2)"))

print("--- and the orders that are refused ---")
for src in ("f(a=1, b)",
            "f(**a, b)",
            "f(**a, *b)",
            "f(a=1, b, c)",
            "f(a=1, b, *c)",
            "g(1, a=2, b)",
            "f(**a, b, c)",
            "f(**a, *b, *c)",
            "class C(a=1, B): pass",
            "class C(**kw, *bs): pass",
            "class C(**kw, B): pass"):
    print("%-26s %s" % (src, run(src)))

print("done")
