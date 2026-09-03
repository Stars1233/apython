# The instance-layout attributes a type carries: __basicsize__,
# __dictoffset__ and __weakrefoffset__.
#
# The absolute numbers are deliberately not printed for anything but the
# relationships below.  CPython 3.12 uses managed dicts, so a class that
# carries an instance dict reports __dictoffset__ == -1 and a negative
# __weakrefoffset__; apython stores both at real positive offsets.  What is
# the same in both is that the attributes exist, are ints, come off the
# metatype rather than the class body, and that a __slots__ entry costs one
# word.


class Slotted:
    __slots__ = ('x',)


class SlottedTwo:
    __slots__ = ('x', 'y')


class Empty:
    __slots__ = ()


class Plain:
    pass


for T in (Slotted, SlottedTwo, Empty, Plain, object, type, int, str, tuple,
          list, dict, Exception):
    for attr in ('__basicsize__', '__dictoffset__', '__weakrefoffset__'):
        v = getattr(T, attr)
        assert type(v) is int, (T, attr, v)
print('all three are ints on every type')

print('one slot costs', Slotted.__basicsize__ - Empty.__basicsize__)
print('two slots cost', SlottedTwo.__basicsize__ - Empty.__basicsize__)

# The header is never empty and never negative.
print(object.__basicsize__ > 0, type.__basicsize__ > object.__basicsize__)

# A __slots__ class cannot be weak-referenced unless it asks for it, so its
# __weakrefoffset__ is zero in both.  A plain class can, and CPython reports
# a negative offset there -- see bugs.md.
print(Slotted.__weakrefoffset__, Empty.__weakrefoffset__)


# These are getsets on the metatype, which makes them data descriptors: a
# class body that uses one of the names does not shadow the real value.
class Shadow:
    __basicsize__ = 5
    __dictoffset__ = 5


print(Shadow.__basicsize__ == 5, Shadow.__dictoffset__ == 5)

# An instance still sees the class-body value, because the metatype's
# descriptor is not on the instance's own MRO.
print(Shadow().__basicsize__)

print('type has the attributes too:', hasattr(type, '__basicsize__'))
