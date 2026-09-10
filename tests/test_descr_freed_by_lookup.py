# The descriptor an attribute lookup found has to survive the lookup.
#
# When a class has a non-data descriptor, the instance dict outranks it -- so
# instance_getattr_default consults the type first, keeps what it found, and
# then probes the instance dict.  That probe runs the instance dict's KEYS'
# __eq__, and that is arbitrary Python: `del C.attr` inside one drops the
# type's last reference to the descriptor still being held, and everything
# after reads freed memory.
#
# CPython INCREFs what _PyType_Lookup found and keeps it to the end of the
# function for exactly this; the test is its own, test_descr's
# vicious_descriptor_nonsense, "a potential segfault spotted by Thomas Wouters
# in mail to python-dev 2003-04-17".

import gc


class Evil:
    def __hash__(self):
        return hash("attr")

    def __eq__(self, other):
        try:
            del C.attr
        except AttributeError:
            pass
        return False


class Descr:
    def __get__(self, ob, type=None):
        return 1


class C:
    attr = Descr()


c = C()
c.__dict__[Evil()] = 0
print(c.attr)
gc.collect()
print(hasattr(c, "attr"))

# Repeated, because a freed descriptor is only sometimes reused in a way that
# shows.
for i in range(200):
    class Repeat:
        thing = Descr()

    class Bad:
        def __hash__(self):
            return hash("thing")

        def __eq__(self, other):
            try:
                del Repeat.thing
            except AttributeError:
                pass
            return False

    r = Repeat()
    r.__dict__[Bad()] = 0
    assert r.thing == 1, i
    if i % 50 == 0:
        gc.collect()
gc.collect()
print("repeated")

# The ordinary orders still work.
class Data:
    def __get__(self, ob, type=None):
        return "data-get"

    def __set__(self, ob, v):
        pass


class WithData:
    x = Data()


w = WithData()
print(w.x)
w.__dict__["x"] = "shadow"
print(w.x)


class NonData:
    def __get__(self, ob, type=None):
        return "nondata-get"


class WithNonData:
    y = NonData()


n = WithNonData()
print(n.y)
n.__dict__["y"] = "shadow"
print(n.y)
print("done")
