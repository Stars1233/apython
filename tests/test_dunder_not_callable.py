# A dunder whose value is not callable reports what is wrong with it.
#
# dunder_lookup returns a Value, and dunder_call_1 classified it with
# V_TEST_PTR -- one `ja` that covers NULL and every immediate at once.  So a
# dunder set to an int or a float read as ABSENT, and the NULL with no
# exception behind it became whatever the caller makes of a missing protocol:
#
#   __len__ = 5   ->  RuntimeError: slot wrapper failed without an exception
#   __next__ = 5  ->  a clean StopIteration, so a `for` over it was EMPTY
#
# A pointer that is not callable -- None, True, "x" -- was always reported
# correctly, because a pointer is its own Value and the bind path asks its type.
# Those are the control here.


def check(label, fn):
    try:
        print(label, "->", fn())
    except Exception as e:
        print(label, "->", type(e).__name__, e)


# The immediates: an int and a float, neither of which has an object header.
# A pointer that is not callable, for contrast.
BAD = [("int", 5), ("float", 1.5), ("None", None), ("str", "x"), ("bool", True)]

for kind, value in BAD:
    C = type("C", (), {"__len__": value})
    check("len %s" % kind, lambda C=C: len(C()))

for kind, value in BAD:
    C = type("C", (), {"__bool__": value})
    check("bool %s" % kind, lambda C=C: bool(C()))

for kind, value in BAD:
    C = type("C", (), {"__hash__": value})
    check("hash %s" % kind, lambda C=C: hash(C()))

for kind, value in BAD:
    C = type("C", (), {"__getitem__": value})
    check("getitem %s" % kind, lambda C=C: C()[0])

for kind, value in BAD:
    C = type("C", (), {"__call__": value})
    check("call %s" % kind, lambda C=C: C()())

for kind, value in BAD:
    C = type("C", (), {"__iter__": value})
    check("iter %s" % kind, lambda C=C: iter(C()))

# __next__ is the one that answered WRONGLY rather than confusingly: a clean
# StopIteration made the `for` empty instead of raising.  Both forms are
# checked, because only the `for` went quiet.
for kind, value in BAD:
    C = type("C", (), {"__next__": value, "__iter__": lambda s: s})
    check("next %s" % kind, lambda C=C: next(C()))

for kind, value in BAD:
    C = type("C", (), {"__next__": value, "__iter__": lambda s: s})

    def loop(C=C):
        out = []
        for x in C():
            out.append(x)
        return out

    check("for %s" % kind, loop)

# Two-argument and three-argument dunders take the same path.
for kind, value in BAD:
    C = type("C", (), {"__add__": value})
    check("add %s" % kind, lambda C=C: C() + 1)

for kind, value in BAD:
    C = type("C", (), {"__setitem__": value})

    def setit(C=C):
        o = C()
        o[0] = 1
        return "set"

    check("setitem %s" % kind, setit)

# A dunder explicitly set to None is "not absent" in a particular way CPython
# defines: __iter__ = None makes the object non-iterable rather than
# uncallable, and __hash__ = None makes it unhashable.  Those two are the only
# slots whose wrapper interprets None, and they must keep their own wording.
check("iter None special", lambda: iter(type("C", (), {"__iter__": None})()))
check("hash None special", lambda: hash(type("C", (), {"__hash__": None})()))

# ...and a callable dunder still works, through every one of those paths.
class Good:
    def __len__(self):
        return 3

    def __getitem__(self, i):
        return i * 2

    def __add__(self, other):
        return "added"

    def __bool__(self):
        return True


g = Good()
print("good len:", len(g), "item:", g[4], "add:", g + 1, "bool:", bool(g))

print("done")
