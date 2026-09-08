# `class C(B, tag="t")` passes its keywords to B.__init_subclass__.  The hook
# is called from inside type_from_parts, which never saw them, so every
# __init_subclass__ was handed an empty kw dict -- which is the whole point of
# the hook.
seen = []


class Base:
    def __init_subclass__(cls, **kw):
        super().__init_subclass__()
        seen.append((cls.__name__, sorted(kw.items())))


class Plain(Base):
    pass


class Tagged(Base, tag="t", n=1):
    pass


print(seen)


# The hook can consume its keywords and record them on the class.
class Registry:
    registry = {}

    def __init_subclass__(cls, key=None, **kw):
        super().__init_subclass__(**kw)
        if key is not None:
            Registry.registry[key] = cls


class A(Registry, key="a"):
    pass


class B(Registry, key="b"):
    pass


class C(Registry):
    pass


print(sorted(Registry.registry), Registry.registry["a"] is A)


# A hook that raises stops the class being built.
class Strict:
    def __init_subclass__(cls, **kw):
        if kw:
            raise TypeError("no keywords here: %s" % sorted(kw))


try:
    class Bad(Strict, oops=1):
        pass

    print("NOT RAISED")
except TypeError as e:
    print("caught", e)

print("Bad" in globals())


# Inherited through a chain, and via the three-argument type().
seen.clear()
class Mid(Base):
    pass


class Leaf(Mid, deep=True):
    pass


print(seen)

seen.clear()
D = type("D", (Base,), {})
print(seen)


# The hook is found on the new class's MRO, not on its layout base.  A mixin
# written first is the shape unittest.TestCase subclasses use, and picking the
# widest base instead meant TestCase.__init_subclass__ never ran.
class Mixin:
    pass


class Hooked:
    def __init_subclass__(cls, **kw):
        cls.marked = cls.__name__
        super().__init_subclass__(**kw)


class MixinFirst(Mixin, Hooked):
    pass


class MixinLast(Hooked, Mixin):
    pass


print(MixinFirst.marked, MixinLast.marked)


# A wider base that defines nothing must not win over a narrower one that does.
class Wide(list):
    pass


class Narrow:
    def __init_subclass__(cls, **kw):
        cls.narrow = True
        super().__init_subclass__(**kw)


class WideFirst(Wide, Narrow):
    pass


print(getattr(WideFirst, "narrow", "MISSING"))


# Exactly one hook fires: the first after cls on the MRO.  Chaining past it is
# the author's job, through super().
order = []


class One:
    def __init_subclass__(cls, **kw):
        order.append("One")
        super().__init_subclass__(**kw)


class Two(One):
    def __init_subclass__(cls, **kw):
        order.append("Two")
        super().__init_subclass__(**kw)


order.clear()
class Three(Two):
    pass


print(order)

order.clear()
class Four(Three):
    pass


print(order)


# A class that defines its own hook does not call it on itself.
called = []


class SelfHook:
    def __init_subclass__(cls, **kw):
        called.append(cls.__name__)
        super().__init_subclass__(**kw)


print(called)
class SelfChild(SelfHook):
    pass


print(called)
