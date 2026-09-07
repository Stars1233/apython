# A class holds a reference to its metatype.  It used to hold a borrowed
# pointer, which is invisible while the metaclass is one of the two static ones
# this tree ships -- they outlive everything -- and a dangling ob_type as soon
# as the metaclass is written in Python and can be collected.
#
# The shape that broke: a metaclass and its instance defined inside a function,
# so both become garbage together.  The collector reaches the metaclass's own
# MRO cycle first and frees it, and the class is then torn down through an
# ob_type that has already been freed.  Valgrind reported it as an invalid read
# in obj_dealloc from tuple_clear during the shutdown collection; nothing in
# the suite noticed, because the answers were all still right.
#
# The reference needs all three of: the count taken when ob_type is set, the
# release in user_type_dealloc -- after gc_dealloc, which still reads ob_type
# on its way through -- and the edge reported by type_traverse, without which
# the class and its metaclass are a cycle the collector cannot see all of.

import gc


def plain_metaclass():
    class M(type):
        pass

    class C(metaclass=M):
        pass

    return C.__name__, type(C).__name__


def metaclass_with_new():
    class M(type):
        def __new__(mcls, name, bases, ns):
            ns["injected"] = "yes"
            return super().__new__(mcls, name, bases, ns)

    class C(metaclass=M):
        pass

    c = C()
    return [c.injected for _ in range(3)], type(C).__name__


def metaclass_with_type_new():
    class M(type):
        def __new__(mcls, name, bases, ns):
            return type.__new__(mcls, name, bases, ns)

    class C(metaclass=M):
        pass

    return C.__name__, type(C).__name__


def inherited_metaclass():
    class M(type):
        pass

    class Base(metaclass=M):
        pass

    class Derived(Base):
        pass

    return type(Derived).__name__, Derived.__mro__[1].__name__


def many_short_lived():
    """Enough of them to force collections while earlier ones are dying."""
    names = []
    for i in range(200):
        class M(type):
            pass

        class C(metaclass=M):
            tag = i

        names.append(C.tag)
        del C, M
    gc.collect()
    return len(names), names[0], names[-1]


def metaclass_outlives_its_class():
    class M(type):
        pass

    class C(metaclass=M):
        pass

    del C
    gc.collect()
    # M must still be usable after its only instance has gone.
    class D(metaclass=M):
        pass

    return type(D).__name__


def class_outlives_nothing_special():
    holder = []

    class M(type):
        pass

    for i in range(50):
        class C(metaclass=M):
            pass
        holder.append(type(C).__name__)
    gc.collect()
    return len(set(holder)), holder[0]


print(plain_metaclass())
print(metaclass_with_new())
print(metaclass_with_type_new())
print(inherited_metaclass())
print(many_short_lived())
print(metaclass_outlives_its_class())
print(class_outlives_nothing_special())

# The static metatypes must see a balanced pair, not a slow leak and not a
# premature free: an ordinary class is created and dropped many times over.
def churn_plain_classes():
    last = None
    for i in range(500):
        class C:
            pass
        last = C.__name__
    return last


print(churn_plain_classes())
gc.collect()
print("done")
