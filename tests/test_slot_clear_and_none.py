"""Deleting a dunder clears its slot, and `None` reaches the wrapper.

`type_install_slots` never cleared a slot it had filled, so `del C.__iter__`
left tp_iter pointing at a wrapper that then found no dunder and raised
`RuntimeError: slot wrapper failed without an exception` where CPython says
`'A' object is not iterable`.

Clearing is `write 0`, not "re-derive from the base": CPython's
`update_one_slot` starts with NULL and ends `*ptr = specific ? specific :
generic`, and inheritance comes free because the lookup walks the whole MRO --
a base that supplies the dunder is simply found.  What the arm for a builtin
base must do is install THAT type's slot; leaving the current value is right
only at class creation, when it was just copied in, and after a delete it is a
stale wrapper.

A dunder set to `None` is the other half.  Only `tp_hash` is special-cased in
`update_one_slot`; every other None is installed and fails at call time as
`'NoneType' object is not callable`.  The disabling that does happen lives in
the wrappers -- `slot_tp_iter` tests for None itself -- so `__iter__ = None`
says "not iterable" while `__call__ = None` leaves `callable()` True.
"""


def show(label, fn):
    try:
        print(label + ":", fn())
    except Exception as e:                      # noqa: BLE001
        print(label + ":", type(e).__name__ + ":", e)


print("--- delete a dunder and the protocol goes away ---")


class It:
    def __iter__(self):
        return iter([1, 2])


print("before:", list(It()))
del It.__iter__
show("after del __iter__", lambda: list(It()))
show("iter()", lambda: iter(It()))


class Ln:
    def __len__(self):
        return 3


print("before:", len(Ln()))
del Ln.__len__
show("after del __len__", lambda: len(Ln()))
show("bool falls back", lambda: bool(Ln()))


class Gi:
    def __getitem__(self, k):
        return k


print("before:", Gi()[1])
del Gi.__getitem__
show("after del __getitem__", lambda: Gi()[1])


class Cl:
    def __call__(self):
        return "called"


print("before:", Cl()(), callable(Cl()))
del Cl.__call__
show("after del __call__", lambda: Cl()())
print("callable after del:", callable(Cl()))


print("--- del __eq__ falls back to identity ---")


class Eq:
    def __eq__(self, other):
        return True

    __hash__ = None


e1, e2 = Eq(), Eq()
print("before:", e1 == e2)
del Eq.__eq__
print("after:", e1 == e2, e1 == e1)


print("--- re-adding it works again ---")


class Re:
    def __iter__(self):
        return iter("ab")


print("before:", list(Re()))
del Re.__iter__
show("deleted", lambda: list(Re()))
Re.__iter__ = lambda self: iter("cd")
print("re-added:", list(Re()))


print("--- a builtin base still supplies its slot ---")


class E(int):
    pass


print("int arithmetic:", E(1) + 2.5, E(3) * 2, E(7) // 2)


class T(tuple):
    pass


print("tuple compares by contents:", T((1, 2)) == (1, 2), T((1, 2)) == T((1, 2)))


class L2(list):
    pass


print("list iterates:", list(L2([1, 2])))


class S2(str):
    pass


print("str works:", S2("ab").upper(), S2("ab") == "ab", len(S2("abc")))


print("--- deleting an override restores the base's ---")


class L3(list):
    def __iter__(self):
        return iter(["custom"])


print("overridden:", list(L3([1, 2])))
del L3.__iter__
print("after del:", list(L3([1, 2])))


class L4(list):
    def __len__(self):
        return 99


print("overridden len:", len(L4([1, 2])))
del L4.__len__
print("after del:", len(L4([1, 2])))


print("--- a dunder set to None ---")


class NIter:
    __iter__ = None


show("iter", lambda: iter(NIter()))


class NHash:
    __hash__ = None


show("hash", lambda: hash(NHash()))
print("still equal to itself:", NHash() == NHash())


class NCall:
    __call__ = None


print("callable:", callable(NCall()))
show("calling it", lambda: NCall()())


class NSetattr:
    __setattr__ = None


show("setattr", lambda: setattr(NSetattr(), "x", 1))


class NDelattr:
    __delattr__ = None


nd = object.__new__(NDelattr)
show("delattr", lambda: delattr(nd, "x"))


class NGetattr:
    __getattr__ = None


show("getattr", lambda: NGetattr().zzz)


class NLen:
    __len__ = None


show("len", lambda: len(NLen()))


print("--- None assigned after the class is built ---")


class Post:
    def __iter__(self):
        return iter([1])


print("before:", list(Post()))
Post.__iter__ = None
show("after = None", lambda: list(Post()))
Post.__iter__ = lambda self: iter([2])
print("restored:", list(Post()))


print("--- hasattr still sees the name ---")
print("NCall has __call__:", hasattr(NCall, "__call__"))
print("NIter has __iter__:", hasattr(NIter, "__iter__"))
print("value is None:", NCall.__call__ is None, NIter.__iter__ is None)

print("done")
