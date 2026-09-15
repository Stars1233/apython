# A metaclass __prepare__ has to return a mapping.
#
# bc_prepare_namespace took whatever came back and checked only that it was a
# pointer -- "Only a real object can be a namespace; anything else keeps the
# fallback".  None is a pointer.  So a __prepare__ returning None passed,
# the real namespace dict was RELEASED, and the class body executed with None
# as its locals; the first STORE_NAME handed None to dict_set, which read its
# header as a dict.  A SIGSEGV inside dict_lookup, from seven lines:
#
#     class BadMeta(type):
#         @classmethod
#         def __prepare__(*args): return None
#     class Foo(metaclass=BadMeta): pass
#
# CPython refuses, and names both the metaclass and what it got.  This is
# test_types.ClassCreationTests.test_bad___prepare__, which could not be
# reached until lib/_testinternalcapi.py let test_types run at all.
class BadMeta(type):
    @classmethod
    def __prepare__(*args):
        return None


try:
    class Foo(metaclass=BadMeta):
        pass
except TypeError as e:
    print("a type metaclass:", e)


# The metaclass need not be a type, and CPython has no name to quote then.
class BadPlain:
    @classmethod
    def __prepare__(*args):
        return None


try:
    class Bar(metaclass=BadPlain()):
        pass
except TypeError as e:
    print("a plain metaclass:", e)


# Every other non-mapping is refused the same way, including the ones that
# would not have crashed.
def maker(value):
    class M(type):
        @classmethod
        def __prepare__(mcls, name, bases, **kw):
            return value

    return M


for value in (None, 42, "text", [1, 2], (), 1.5, object()):
    try:
        class Q(metaclass=maker(value)):
            pass
        print("%-10s NOT REFUSED" % type(value).__name__)
    except TypeError as e:
        print("%-10s %s" % (type(value).__name__, str(e).split(" must ")[1]))


# And a real mapping still works -- a dict, a dict subclass, and something
# that is only a mapping by protocol.
class GoodMeta(type):
    @classmethod
    def __prepare__(mcls, name, bases, **kw):
        return {}


class OK(metaclass=GoodMeta):
    x = 1


print("dict namespace:", OK.x)


class Recording(dict):
    pass


class SubMeta(type):
    @classmethod
    def __prepare__(mcls, name, bases, **kw):
        return Recording()


class OK2(metaclass=SubMeta):
    y = 2
    def m(self):
        return 3


print("dict subclass namespace:", OK2.y, OK2().m())

# A metaclass with no __prepare__ at all keeps the fallback, which is the
# path every ordinary class takes.
class Plain(type):
    pass


class OK3(metaclass=Plain):
    z = 4


print("no __prepare__:", OK3.z)
print("ordinary class:", type("Z", (), {"w": 5}).w)


class Ordinary:
    a = 6


print("plain class:", Ordinary.a)

# A __prepare__ that RAISES is still a raise, not a missing mapping.
class Raising(type):
    @classmethod
    def __prepare__(mcls, name, bases, **kw):
        raise ValueError("from __prepare__")


try:
    class R(metaclass=Raising):
        pass
except ValueError as e:
    print("raising __prepare__:", e)

import gc

gc.collect()
print("survived")
