# A builtin method assigned by NAME into a class body is that builtin's own
# slot wearing a name, not a definition the class made.
#
# `__hash__ = ref.__hash__` -- which is how weakref.WeakMethod is written, and
# `__repr__ = list.__repr__` and the rest of the family -- put the builtin in
# the subclass's OWN dict, so type_install_slots saw a heaptype defining the
# dunder and installed the generic wrapper over it.  The wrapper looks the name
# up, finds the builtin, and the builtin dispatches on the ARGUMENT's type --
# straight back into the wrapper.  hash() on one of those recursed until the C
# stack ran out.
#
# CPython's update_one_slot recognises the same shape, an inherited wrapper
# descriptor, and installs the defining type's own function instead.  The
# "inherited" part matters: a builtin from a type this one does NOT derive
# from is left to fail on the receiver check, which is what CPython does with
# `class C: __hash__ = int.__hash__`.

import _weakref


class O:
    def m(self):
        pass


class W(_weakref.ref):
    __slots__ = ()
    __hash__ = _weakref.ref.__hash__

    def __eq__(self, other):
        return self is other


o = O()
w = W(o)
print("weakref subclass hash:", isinstance(hash(w), int), hash(w) == hash(w))


class L(list):
    __repr__ = list.__repr__
    __eq__ = list.__eq__
    __len__ = list.__len__


x = L([1, 2, 3])
print("list subclass:", repr(x), x == [1, 2, 3], len(x))


class T(tuple):
    __hash__ = tuple.__hash__


t = T((1, 2))
print("tuple subclass hash:", hash(t) == hash((1, 2)))


class D(dict):
    __getitem__ = dict.__getitem__
    __contains__ = dict.__contains__


d = D(a=1)
print("dict subclass:", d["a"], "a" in d)


class S(str):
    __str__ = str.__str__
    __hash__ = str.__hash__


s = S("hi")
print("str subclass:", str(s), hash(s) == hash("hi"))


# An INDIRECT base counts too: the method's type only has to be somewhere on
# the MRO.
class Mid(list):
    pass


class Leaf(Mid):
    __len__ = list.__len__


print("indirect base:", len(Leaf([1, 2])))


# And a builtin from a type this one does not derive from is not a definition
# it can use: CPython leaves it to the receiver check.
class C:
    __hash__ = int.__hash__


try:
    hash(C())
    print("alien __hash__ -> ok")
except TypeError:
    print("alien __hash__ -> TypeError")

print("done")
