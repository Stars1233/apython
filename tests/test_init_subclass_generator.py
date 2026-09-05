"""A class built while its parent's __init_subclass__ drops a live generator.

`type_from_parts` registers the half-built class in `build_class_pending`, so
that a raise which longjmps past its C frame can still release it.  That
registration belongs to the frame that was running when the construction
started -- and a raise inside a NESTED frame unwinds only to that frame,
leaving the construction's C frame alive.

Dropping a generator that never ran to exhaustion is exactly such a raise:
closing one throws GeneratorExit into it, which runs a nested eval_frame and
unwinds.  Unscoped, that unwind found the outer construction's registration
and released a class that was still being built, and the class carried a
refcount one short for the rest of its life.  It stayed invisible because a
class is in a cycle with its own MRO tuple: one reference short still never
reaches zero.
"""

import gc


class Base:
    tally = []

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        # Created, never iterated: closing it throws GeneratorExit in.
        unfinished = (n for n in range(3))
        del unfinished
        Base.tally.append(cls.__name__)


class Direct(Base):
    value = 1


print("--- the class is built and works ---")
print(Direct.__name__, Direct.value, Direct.__mro__)
print(Direct().value, isinstance(Direct(), Base), issubclass(Direct, Base))
print(Base.tally)

print("--- and again through every shape that abandons a generator ---")


class Abandoners(Base):
    @staticmethod
    def short_circuit():
        return any(n > 0 for n in range(5))

    @staticmethod
    def partial():
        g = (n for n in range(5))
        next(g)
        return "partial"

    @staticmethod
    def closed():
        g = (n for n in range(5))
        g.close()
        return "closed"


print(Abandoners.short_circuit(), Abandoners.partial(), Abandoners.closed())


class UsesAny(Base):
    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)


class Deeper(UsesAny):
    pass


print(Deeper.__mro__)
print(Base.tally)

print("--- a class made this way is collectable, not immortal ---")
#
# The refcount being one short is invisible until something counts.  A class
# is in a cycle with its own MRO tuple, so the collector is what frees it;
# with a reference missing it was freed one collection too early instead.


class Holder(Base):
    pass


before = len(Holder.__subclasses__())
for i in range(30):
    made = type("Gone%d" % i, (Holder,), {"i": i})
    del made
gc.collect()
print(before, len(Holder.__subclasses__()))

print("--- a hundred of them, and the survivors are intact ---")
kept = []
for i in range(100):
    made = type("Kept%d" % i, (Holder,), {"i": i})
    if i % 10 == 0:
        kept.append(made)
    del made
gc.collect()
gc.collect()
print(len(kept), [c.i for c in kept])
print(all(c.__mro__[0] is c for c in kept))
print(all(type(c).__name__ == "type" for c in kept))
print(sorted(c.__name__ for c in Holder.__subclasses__()) == sorted(
    c.__name__ for c in kept))

print("--- and the same with a metaclass of its own ---")


class Meta(type):
    @property
    def tag(cls):
        return "meta-" + cls.__name__


class WithMeta(Base, metaclass=Meta):
    pass


print(WithMeta.tag, type(WithMeta).__name__)
kept2 = []
for i in range(50):
    made = Meta("M%d" % i, (WithMeta,), {})
    if i % 10 == 0:
        kept2.append(made)
    del made
gc.collect()
gc.collect()
print(len(kept2), [c.tag for c in kept2])
print(sorted(c.__name__ for c in WithMeta.__subclasses__()) == sorted(
    c.__name__ for c in kept2))
print(Base.tally[:4])
print("done")


print("--- under a collector that runs on every allocation ---")
#
# The two halves of this have to hold together: the reference has to be
# right, AND the collector has to be able to break the cycle a class makes
# with its own MRO tuple.  Either alone is wrong -- without the reference the
# collector frees a class that is still live, and without the traverse every
# class of this shape accumulates forever.
gc.set_threshold(1, 1, 1)


class StressMeta(type):
    def __init_subclass__(cls, **kwargs):
        g = (n for n in range(4))
        del g


class Root(metaclass=StressMeta):
    def __init_subclass__(cls, **kwargs):
        any(n > 100 for n in range(50))


survivors = []
for i in range(200):
    class Sub(Root):
        n = i

        def m(self):
            return self.n

    Sub.self_ref = Sub
    Sub.pair = (Sub, Root)
    if i % 25 == 0:
        survivors.append(Sub)
    del Sub

gc.collect()
gc.collect()
print(len(survivors), sorted(c.n for c in survivors))
print(all(c.__mro__[0] is c and c.self_ref is c for c in survivors))
print(all(c().m() == c.n for c in survivors))
print(len(Root.__subclasses__()) == len(survivors))
gc.set_threshold(700, 10, 10)
print("survived")
