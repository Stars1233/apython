# A str subclass can declare __slots__.
#
# It used to be a TypeError, worded the way CPython words the ones it really
# does refuse: "nonempty __slots__ not supported for subtype of 'str'".  int,
# bytes and tuple are refused by CPython too and still are here -- int wraps
# its value and the other two keep their data inline, so a slot laid out at
# the base's basicsize lands inside that data or past the allocation.  str is
# the one CPython accepts.
#
# It works by putting the slots where the __dict__ of a str subclass already
# goes: at the TAIL, past the characters and past the word reserved for that
# dict.  A member descriptor addresses one with a negative offset, and
# tp_tailslots counts them so that the allocation, the dealloc walk and the
# collector all know they are there.

class S(str):
    __slots__ = ('tag', 'n')

    def __new__(cls, value, tag):
        obj = str.__new__(cls, value)
        obj.tag = tag
        return obj


s = S("hello world", ["a", "list"])
print("value              ", repr(s))
print("still a str        ", len(s), s.upper(), s + "!", s[3:8], "wor" in s)
print("slot               ", s.tag)
s.n = 42
print("second slot        ", s.n)
print("__slots__          ", S.__slots__)
print("descriptors        ", sorted(k for k in S.__dict__ if k in ('tag', 'n')))
print("no dict            ", S.__dictoffset__)


def check(label, fn):
    try:
        print(label.ljust(19), fn())
    except Exception as exc:
        print(label.ljust(19), type(exc).__name__ + ":", exc)


check("__dict__", lambda: s.__dict__)
check("undeclared", lambda: setattr(s, "other", 1))
check("unset slot", lambda: S("x", "t").n)

del s.n
check("after del", lambda: s.n)
s.n = "back"
print("set again          ", s.n)


# The characters are not disturbed by any of it: a long string, a slot
# written after the fact, and the string read back.
long_one = S("x" * 200, "tag")
long_one.n = "written after"
print("long string        ", len(long_one), long_one[:5], long_one[-5:],
      long_one.count("x"), long_one.n)


# Inheritance: slots accumulate, and a subclass that declares none of its own
# still has to reserve room for its base's.
class U(S):
    pass


u = U("inherited", "u-tag")
u.n = "u-n"
u.free = "in a dict"
print("subclass, no slots ", u, u.tag, u.n, u.free, sorted(u.__dict__))


class V(U):
    __slots__ = ('more',)


v = V("deep", "v-tag")
v.n = 2
v.more = 3
v.free = 4
print("three deep         ", v, v.tag, v.n, v.more, v.free, len(v))


# int, bytes and tuple are still refused, and with CPython's wording.
def refuse(name, body):
    try:
        exec(body)
        print(name.ljust(19), "accepted")
    except TypeError as exc:
        print(name.ljust(19), exc)


refuse("int", "class N(int): __slots__ = ('t',)")
refuse("bytes", "class B(bytes): __slots__ = ('t',)")
refuse("tuple", "class T(tuple): __slots__ = ('t',)")
refuse("empty on int", "class E(int): __slots__ = ()")


# A cycle through a tail slot has to be collectable, which means the
# collector has to be able to see one.
import gc


class Cyclic(str):
    __slots__ = ('ref',)


def make_cycle():
    a = Cyclic("cycle")
    a.ref = a


make_cycle()
gc.collect()
print("cycle collected    ", True)

# ...and a slot holding a container the collector walks into.
class Holder(str):
    __slots__ = ('box',)


h = Holder("holder")
h.box = []
h.box.append(h)
del h
gc.collect()
print("indirect cycle     ", True)

# A plain str subclass, with a dict and no slots, is unchanged.
class Plain(str):
    pass


p = Plain("plain")
p.z = 9
print("plain subclass     ", p, p.z, p.__dict__)
print("done")
