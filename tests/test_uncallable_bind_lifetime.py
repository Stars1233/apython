# When __get__ answers something that cannot be called, the refusal NAMES its
# type -- and the bound object is usually its own last reference, so the name
# has to be composed BEFORE the release.  Four paths had it the other way
# round and read a freed object: the three dunder_call_* and slot_tp_call.
#
# `with` had the same shape for a different reason: it released the manager
# and then named the manager's type.  An __exit__ bound from a descriptor
# whose __get__ answers a module-level function holds no reference to the
# manager, so that decref is the last one.


class Uncallable:
    def __get__(self, obj, objtype=None):
        return object()          # a fresh one each time: refcount 1, then 0


def probe(name, call):
    C = type("C", (), {name: Uncallable()})
    try:
        call(C())
    except TypeError as e:
        print(name, "->", e)


probe("__repr__", lambda o: repr(o))
probe("__str__", lambda o: str(o))
probe("__len__", lambda o: len(o))
probe("__iter__", lambda o: iter(o))
probe("__getitem__", lambda o: o[1])
probe("__contains__", lambda o: 1 in o)
probe("__eq__", lambda o: o == 1)
probe("__add__", lambda o: o + 1)
probe("__setitem__", lambda o: o.__setitem__(1, 2))
probe("__call__", lambda o: o())
probe("__enter__", lambda o: o.__enter__())


# __get__ answering an immediate, which has no type to read at all.
class IntGet:
    def __get__(self, obj, objtype=None):
        return 7


for name, call in (("__len__", lambda o: len(o)),
                   ("__call__", lambda o: o()),
                   ("__eq__", lambda o: o == 1)):
    C = type("C", (), {name: IntGet()})
    try:
        call(C())
    except TypeError as e:
        print("int", name, "->", e)


# The `with` statement: an __exit__ that binds to something holding no
# reference to the manager, and no __enter__ at all.
def standalone(*a):
    return False


class BoundElsewhere:
    def __get__(self, obj, objtype=None):
        return standalone


C = type("C", (), {})
C.__exit__ = BoundElsewhere()
try:
    with C():
        print("entered")
except TypeError as e:
    print("with:", e)

# ...and one with __enter__ and no __exit__, which names the type on the
# other arm.
C = type("C", (), {})
C.__enter__ = BoundElsewhere()
try:
    with C():
        print("entered")
except TypeError as e:
    print("with 2:", e)

print("done")


# ---------------------------------------------------------------------------
# The same rule in a different place: str.join names the offending item's TYPE,
# and the temporary sequence it materialised holds the ONLY reference to that
# item when the argument was a generator.  Releasing the temporary first read
# ob_type out of the block just freed.
class Weird:
    pass


for call in (lambda: "".join(Weird() for _ in range(1)),
             lambda: ",".join(Weird() for _ in range(3)),
             lambda: "".join(x for x in ["ok", Weird()]),
             lambda: "".join(iter([Weird()])),
             lambda: "-".join(x for x in (1,)),
             lambda: "".join([Weird()])):
    try:
        call()
    except TypeError as e:
        print(e)

# bytes.join takes the same road and must stay right.
for call in (lambda: b"".join(Weird() for _ in range(1)),
             lambda: b",".join(type("E2", (), {})() for _ in range(2)),
             lambda: b"".join([1])):
    try:
        call()
    except TypeError as e:
        print(e)

print("done 2")
