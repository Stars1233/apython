# A __slots__ that names '__dict__' still DECLARES __slots__.
#
# CPython's type_new_set_attrs gives a heaptype a __weakref__ word unless it
# declares __slots__ without naming __weakref__ in them -- and naming
# '__dict__' does not excuse it.  weakref_referenceable asked
# TYPE_FLAG_HAS_SLOTS instead, which means "this class has NO instance dict",
# and a __slots__ naming '__dict__' keeps the dict and so leaves that flag
# clear.  The class read as slot-free and its instances were weak-referenceable
# where CPython refuses them.
#
# _weakref rather than weakref, which is not in lib/.

import _weakref


def refable(C):
    try:
        _weakref.ref(C())
        return "yes"
    except TypeError as e:
        return "no: %s" % e


class Plain:
    pass


class SlotsEmpty:
    __slots__ = ()


class SlotsNamed:
    __slots__ = ("a",)


class SlotsDict:
    __slots__ = ("__dict__",)


class SlotsWeak:
    __slots__ = ("__weakref__",)


class SlotsBoth:
    __slots__ = ("__dict__", "__weakref__")


for name, C in (("plain", Plain), ("slots ()", SlotsEmpty),
                ("slots (a,)", SlotsNamed), ("slots (__dict__,)", SlotsDict),
                ("slots (__weakref__,)", SlotsWeak),
                ("slots (both)", SlotsBoth)):
    print("%-22s %s" % (name, refable(C)))

print("--- and a subclass inherits the word once any level has it ---")


class SubOfPlain(Plain):
    pass


class SubOfPlainSlots(Plain):
    __slots__ = ()


class SubOfSlotsDict(SlotsDict):
    pass


class SubOfSlotsDictSlots(SlotsDict):
    __slots__ = ()


for name, C in (("sub of plain", SubOfPlain),
                ("sub of plain + slots", SubOfPlainSlots),
                ("sub of slots(__dict__,)", SubOfSlotsDict),
                ("sub of that + slots", SubOfSlotsDictSlots)):
    print("%-26s %s" % (name, refable(C)))

print("--- the dict a __slots__ ('__dict__',) keeps still works ---")
sd = SlotsDict()
sd.anything = 1
print("attribute:", sd.anything, sorted(sd.__dict__))

print("--- a named slot beside it still works ---")


class Mixed:
    __slots__ = ("__dict__", "s")


m = Mixed()
m.s = 2
m.other = 3
print("mixed:", m.s, m.other, sorted(m.__dict__))

print("--- and the ones that ARE referenceable still are ---")
p = Plain()
r = _weakref.ref(p)
print("alive:", r() is p)
print("done")
