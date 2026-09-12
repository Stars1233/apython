# An instance counts as a reference TO ITS CLASS, and the collector has to be
# told so.
#
# CPython's subtype_traverse says it outright:
#
#     For a heaptype, the instances count as references to the type.
#     Traverse the type so the collector can find cycles involving this link.
#
# Ours did not.  So when an instance was freed by the CYCLE COLLECTOR rather
# than by refcounting, the reference it held to its class was never accounted
# for, and the class survived -- about 900 bytes of class object, dict, and
# method objects, every time.
#
# It needs both halves to show, which is why nothing found it earlier:
#
#   * a class whose lifetime is shorter than the program's -- one defined
#     inside a function, or built by type(), or a closure's -- because a
#     module-level class was going to live forever anyway;
#   * an instance that only the COLLECTOR can free, i.e. one in a reference
#     cycle.  `self.callback = self.method` is the everyday way to make one,
#     and it is what every callback-registering object does.
#
# Measured before the fix: 909 bytes per iteration of the loop below, growing
# without bound, where CPython grows by nothing.  A module-level class with
# the same cycle leaked 2 bytes an iteration, and a loop-defined class with no
# cycle 3, so neither half alone is visible.
#
# This test asserts the OBJECT COUNT rather than memory, because that is what
# is actually wrong: the class is still reachable from the collector's point
# of view.

import gc


def make_cyclic_class():
    """A class defined here, with an instance that only the collector frees."""

    class Holder:
        def __init__(self):
            self.callback = self.method     # self -> bound method -> self
            self.data = [1, 2, 3]

        def method(self):
            return self.data

    return Holder()


def count_classes():
    gc.collect()
    return sum(1 for o in gc.get_objects() if isinstance(o, type)
               and getattr(o, "__name__", "") == "Holder")


print("== a loop-defined class with a cyclic instance is collected ==")
print("Holder classes alive at the start:", count_classes())
for _ in range(50):
    obj = make_cyclic_class()
    del obj
print("after 50 created and dropped:", count_classes())

print()
print("== the instance itself, and what it referenced ==")
freed = []


def make_with_finalizer():
    class Tagged:
        def __init__(self, tag):
            self.tag = tag
            self.callback = self.method

        def method(self):
            pass

        def __del__(self):
            freed.append(self.tag)

    return Tagged("t")


for _ in range(10):
    o = make_with_finalizer()
    del o
gc.collect()
print("finalizers that ran:", len(freed))

print()
print("== and the same through type(), which has no class body at all ==")


def make_by_type_call():
    def method(self):
        return self.data

    cls = type("Dynamic", (), {"method": method})
    obj = cls()
    obj.callback = obj.method
    obj.data = [1]
    return obj


for _ in range(50):
    o = make_by_type_call()
    del o
gc.collect()
print("Dynamic classes alive:",
      sum(1 for o in gc.get_objects() if isinstance(o, type)
          and getattr(o, "__name__", "") == "Dynamic"))

print()
print("== a class held by nothing but a cyclic instance, one at a time ==")


def one_round():
    class Solo:
        def __init__(self):
            self.me = self          # the simplest cycle there is

    s = Solo()
    return None


for _ in range(30):
    one_round()
gc.collect()
print("Solo classes alive:",
      sum(1 for o in gc.get_objects() if isinstance(o, type)
          and getattr(o, "__name__", "") == "Solo"))

print()
print("== what must keep working: a static type is NOT a GC object ==")
# The collector must not follow ob_type for a builtin.  A static type has no
# GC head, and reading one is the shape that made an earlier crash
# layout-dependent, so the visit has to be conditional on HEAPTYPE.
print("int tracked:", gc.is_tracked(int))
print("list tracked:", gc.is_tracked(list))
print("a heaptype tracked:", gc.is_tracked(type("X", (), {})))
print("builtins still work after collecting:", gc.collect() >= 0, len([1, 2]), int("7"))

print()
print("== a subclass of a builtin, whose instances are also tracked ==")


def make_int_subclass():
    class MyInt(int):
        def m(self):
            return self

    v = MyInt(5)
    return v


for _ in range(30):
    v = make_int_subclass()
    del v
gc.collect()
print("MyInt classes alive:",
      sum(1 for o in gc.get_objects() if isinstance(o, type)
          and getattr(o, "__name__", "") == "MyInt"))

print()
print("== and a class whose instance is NOT in a cycle is still fine ==")


def make_plain():
    class Plain:
        pass

    return Plain()


for _ in range(30):
    o = make_plain()
    del o
gc.collect()
print("Plain classes alive:",
      sum(1 for o in gc.get_objects() if isinstance(o, type)
          and getattr(o, "__name__", "") == "Plain"))

print()
print("== the class is reachable for as long as an instance lives ==")


def make_kept():
    class Kept:
        def __init__(self):
            self.callback = self.method

        def method(self):
            pass

    return Kept()


kept = [make_kept() for _ in range(5)]
gc.collect()
print("Kept classes alive while instances are held:",
      sum(1 for o in gc.get_objects() if isinstance(o, type)
          and getattr(o, "__name__", "") == "Kept"))
print("and their methods still work:",
      [o.callback() for o in kept] == [None] * 5)
del kept
gc.collect()
print("after dropping them:",
      sum(1 for o in gc.get_objects() if isinstance(o, type)
          and getattr(o, "__name__", "") == "Kept"))
