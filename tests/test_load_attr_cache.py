# LOAD_ATTR_INSTANCE (204): the inline cache for a plain `self.x` read.
#
# LOAD_ATTR's only cache was for methods, so an ordinary attribute load went
# through op_load_attr's prologue, tp_getattr, instance_getattr,
# instance_getattr_default, LOAD_INST_DICT and dict_get -- hashing the name and
# probing the table every time.
#
# CPython caches (type version, keys version, index) and can trust the index
# because its instances share a keys object.  Ours cannot: two instances of one
# class can have completely different dict layouts, from an __init__ with a
# branch in it.  So the cached index is not trusted -- the KEY at that index is
# compared against the site's own name, which makes the read self-validating.
#
# What this file drives, one section per guard:
#
#   1. the class:            an instance of another class at the same site
#   2. the class dict:       a property or a __getattribute__ added AFTER the
#                            cache is formed, on the class or on a base
#   3. the flags:            classes that must never be cached at all
#   4. the dict and bounds:  instances whose dict is smaller, or absent
#   5. the key at the index: instances of one class with different layouts
#   6. the hole:             the attribute deleted after caching
#
# and the deopt itself, which must hand op_load_attr back the oparg it was
# given.  Getting that wrong is not a wrong answer: op_load_attr reads
# co_names at (garbage >> 1) and the wild pointer surfaces later, inside
# dict_get, with a name that is not a string.  Every section below therefore
# runs its site enough times to specialize AND then forces a deopt at it.


def read(o):
    return o.x


class A:
    def __init__(self, v):
        self.x = v
        self.y = v * 2


class B:
    def __init__(self, v):
        self.other = v
        self.x = v + 100


# --- the ordinary case, and the same site over two classes ----------------
a, b = A(1), B(2)
for _ in range(8):
    print(read(a), read(b))

# --- guard 5: one class, two layouts --------------------------------------
class Branchy:
    def __init__(self, first):
        if first:
            self.x = "x-first"
            self.z = 1
        else:
            self.z = 0
            self.x = "z-first"


p, q = Branchy(True), Branchy(False)
for _ in range(8):
    print(read(p), read(q))

# --- guard 2: a property added to the class after the cache is formed -----
class Late:
    def __init__(self):
        self.x = "instance"


lt = Late()
for _ in range(8):
    print(read(lt))
Late.x = property(lambda self: "property wins")
for _ in range(8):
    print(read(lt))
del Late.x
for _ in range(8):
    print(read(lt))


# --- guard 2 again, but the property arrives on a BASE --------------------
class Base:
    pass


class Derived(Base):
    def __init__(self):
        self.x = "derived instance"


d = Derived()
for _ in range(8):
    print(read(d))
Base.x = property(lambda self: "base property wins")
for _ in range(8):
    print(read(d))
del Base.x
for _ in range(8):
    print(read(d))


# --- guard 2: __getattribute__ arriving late ------------------------------
class Hooky:
    def __init__(self):
        self.x = "plain"


hk = Hooky()
for _ in range(8):
    print(read(hk))
Hooky.__getattribute__ = lambda self, name: "hook(%s)" % name
for _ in range(8):
    print(read(hk))
del Hooky.__getattribute__
for _ in range(8):
    print(read(hk))


# --- guard 3: classes that must never be cached ---------------------------
class HasProperty:
    def __init__(self):
        self.__dict__["x"] = "shadowed"

    x = property(lambda self: "property")


class HasGetattribute:
    def __init__(self):
        self.x = "plain"

    def __getattribute__(self, name):
        return "always(%s)" % name


for _ in range(8):
    print(read(HasProperty()), read(HasGetattribute()))


# --- guard 6: the attribute deleted after caching -------------------------
class Deletable:
    def __init__(self):
        self.x = "here"


dl = Deletable()
for _ in range(8):
    print(read(dl))
del dl.x
for _ in range(4):
    try:
        read(dl)
        print("still readable")
    except AttributeError:
        print("deleted -> AttributeError")
dl.x = "back"
for _ in range(8):
    print(read(dl))


# --- a class attribute, which is not an instance-dict read at all ---------
class ClassAttr:
    x = "on the class"


ca = ClassAttr()
for _ in range(8):
    print(read(ca))
ca.x = "now on the instance"
for _ in range(8):
    print(read(ca))
del ca.x
for _ in range(8):
    print(read(ca))


# --- __slots__, which is instance storage but not a dict ------------------
class Slotted:
    __slots__ = ("x",)

    def __init__(self):
        self.x = "slot"


sl = Slotted()
for _ in range(8):
    print(read(sl))


class SlotAndDict(Slotted):
    def __init__(self):
        Slotted.__init__(self)
        self.other = 1


sd = SlotAndDict()
for _ in range(8):
    print(read(sd))


# --- a str subclass, whose instance dict lives at the tail ----------------
class StrSub(str):
    pass


ss = StrSub("hello")
ss.x = "tail dict"
for _ in range(8):
    print(read(ss), ss.upper())


# --- __getattr__ still runs for a miss, and cannot leak into the next read -
class WithGetattr:
    def __init__(self):
        self.x = "present"

    def __getattr__(self, name):
        raise AttributeError("no %s" % name)


wg = WithGetattr()
for _ in range(8):
    print(read(wg))
for _ in range(4):
    try:
        wg.absent
    except AttributeError as exc:
        print("absent ->", exc)
    print(read(wg))


# --- the same site over many unrelated shapes -----------------------------
class C1:
    def __init__(self):
        self.x = 1


class C2:
    def __init__(self):
        self.a = 0
        self.b = 0
        self.x = 2


class C3:
    x = 3


shapes = [C1(), C2(), C3(), A(9), Branchy(True), Slotted(), ss]
for _ in range(4):
    print([read(o) for o in shapes])


# --- values of every Value kind, since the cache returns them raw ---------
class Kinds:
    pass


k = Kinds()
for v in (0, 1, -1, 2 ** 60, -(2 ** 60), 0.0, -0.0, 1.5, float("nan"),
          True, False, None, "", "s", [], [1], (), {}, Kinds):
    k.x = v
    for _ in range(4):
        got = read(k)
    print(repr(got), got is v or got != got)


# --- and enough distinct names to need EXTENDED_ARG on the oparg ----------
src = ["class Wide:", "    def __init__(self):"]
for i in range(300):
    src.append("        self.n%03d = %d" % (i, i))
src.append("def read_wide(o):")
src.append("    return (o.n000, o.n150, o.n299)")
exec("\n".join(src), globals())
w = Wide()
for _ in range(8):
    print(read_wide(w))
