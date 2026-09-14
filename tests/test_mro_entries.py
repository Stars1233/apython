# PEP 560's __mro_entries__ may answer any number of bases, not just one.
#
# bc_mro_entry accepted a tuple of length exactly 1 and discarded anything
# else with no exception, and bc_resolve_bases pre-sized its output tuple to
# len(bases) and advanced the write index in lockstep with the read index --
# so one written base could only ever become one real base.  Every other
# shape came back as `TypeError: bases must be types`.
#
# typing.List[T].__mro_entries__ returns TWO entries, the origin and Generic,
# so `class C(typing.List[T])`, `class C(typing.Awaitable[T])` and
# `class C(Protocol)` all failed -- and test_typing aborted at import rather
# than running.
#
# __orig_bases__ was never set at all.  CPython records the bases AS WRITTEN
# whenever the substitution changed anything, and typing reads it throughout;
# only types.new_class set it here.


# --- the shapes, hand-rolled -------------------------------------------
class One:
    def __mro_entries__(self, bases):
        return (dict,)


class Two:
    def __mro_entries__(self, bases):
        return (int, object)


class Zero:
    def __mro_entries__(self, bases):
        return ()


class NotATuple:
    def __mro_entries__(self, bases):
        return dict


class C1(One()):
    pass


print("one entry:", C1.__bases__)

class C2(Two()):
    pass


print("two entries:", C2.__bases__)

class C3(Zero()):
    pass


print("no entries:", C3.__bases__)

try:
    class Bad(NotATuple()):
        pass
except TypeError as e:
    print("NotATuple ->", e)

# __mro_entries__ is handed the WHOLE original tuple, which is what
# typing.NamedTuple asserts on.
seen = []


class Base1:
    pass


class Base2:
    pass


class Watcher:
    def __init__(self, stand_for):
        self.stand_for = stand_for

    def __mro_entries__(self, bases):
        seen.append(len(bases))
        return (self.stand_for,)


class C4(Watcher(Base1), Watcher(Base2)):
    pass


print("bases seen:", seen, [b.__name__ for b in C4.__bases__])

# A proxy in the middle keeps the order of everything around it.
class Mid:
    def __mro_entries__(self, bases):
        return (Base1,)


class A:
    pass


class C5(A, Mid(), Base2):
    pass


print("order kept:", [b.__name__ for b in C5.__bases__])

# --- __orig_bases__ ----------------------------------------------------
print("orig_bases when substituted:", C2.__orig_bases__ == (C2.__orig_bases__[0],))
print("orig_bases is the proxy:", type(C2.__orig_bases__[0]).__name__)
print("plain class has none:", "__orig_bases__" not in vars(A))


class C6(A):
    pass


print("unsubstituted has none:", "__orig_bases__" not in vars(C6))

# The reason it matters is `typing`: List[T].__mro_entries__ answers two
# entries, so `class C(typing.List[T])`, `class C(Protocol)`, NamedTuple and
# TypedDict were all "bases must be types" and test_typing aborted at import.
# None of that is exercised here, because there is no typing in lib/ -- it
# comes from a real stdlib on $PYTHONPATH, which make check does not set.
# CPython's own test_typing is what covers it, and it goes from 0 tests run
# to all of them.
print("survived")
