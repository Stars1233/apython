# super() inside a classmethod must not consume a reference to the class.
#
# super_check answers two things: the class to search from, in rax, and whether
# that reference is HANDED OVER or borrowed, in edx.  Three of its four exits
# set edx; the metatype arm -- super(C, cls) where cls is a subclass, which is
# every super() written inside a classmethod -- returned with edx holding
# whatever the last call left in it.  super_construct read that as "already
# ours" and skipped its incref, so each call dropped the class's refcount by
# one.
#
# Nothing fails where the mistake is.  The class is freed while still
# reachable, and the segfault lands in dict_lookup, type_mro_next or
# tuple_clear -- code that is correct -- some thousands of instructions later.
# So the test is a loop and then a use: if the reference is being consumed, the
# class is gone by the time we ask it anything.
#
# make check, make check-cpython, both -source gates and lint.py were all green
# over this.  The only instrument that saw it was CPython's own Lib/test corpus
# and a crash count compared against the previous commit.

import gc


class Root(object):
    @classmethod
    def describe(cls):
        return "Root.describe via " + cls.__name__


class A(Root):
    @classmethod
    def unbound_super(cls):
        return super(A, cls)

    @classmethod
    def called_super(cls):
        return super(A, cls).describe()


class B(A):
    pass


# The three-argument form with a CLASS as the second argument is the shape that
# was wrong.  Two hundred is far more than enough: the class starts with a
# handful of references and each call took one.
for _ in range(200):
    A.unbound_super()
    B.unbound_super()

gc.collect()

# If the loop consumed references, B is freed memory by now and every one of
# these reads a dead object.
print("B still answers:", B.__name__, [c.__name__ for c in B.__mro__])
print("A still answers:", A.__name__, [c.__name__ for c in A.__mro__])
print("subclass check:", issubclass(B, A), isinstance(B(), A))

# The same loop through the form that CALLS through the super object, which is
# what a real classmethod does.
for _ in range(200):
    B.called_super()

gc.collect()
print("called_super:", B.called_super())
print("B still answers:", B.__name__, len(B.__mro__))

# And the attributes the super object publishes, which is what names the class
# it decided to search from.
s = B.unbound_super()
print("self_class:", s.__self_class__.__name__)
print("obj_type:", s.__self__.__name__)
print("thisclass:", s.__thisclass__.__name__)

# A cooperative chain through classmethods, which is the idiom this shape is
# for.  Each level's super() is the metatype arm.
class Base(object):
    @classmethod
    def make(cls):
        return ["Base"]


class Mid(Base):
    @classmethod
    def make(cls):
        return super(Mid, cls).make() + ["Mid"]


class Leaf(Mid):
    @classmethod
    def make(cls):
        return super(Leaf, cls).make() + ["Leaf"]


for _ in range(200):
    Leaf.make()

gc.collect()
print("chain:", Leaf.make())
print("Leaf mro:", [c.__name__ for c in Leaf.__mro__])

# The two-argument form with an INSTANCE, for contrast: this arm always set
# edx and was never wrong.  It is here so a future change that breaks the
# borrowed case is caught by the same file.
class WithInstance(object):
    def f(self):
        return "WithInstance.f"


class SubInstance(WithInstance):
    def f(self):
        return super(SubInstance, self).f() + "+Sub"


inst = SubInstance()
for _ in range(200):
    super(SubInstance, inst)

gc.collect()
print("instance form:", inst.f())
print("done")
