# PEP 695 keeps Python's scoping rule -- a class body is invisible to a
# function nested inside it -- and adds a runtime one so a generic class,
# function or type alias written in a class body can still name what that body
# binds.  The type-parameter wrapper takes the body's namespace as a free
# variable called __classdict__ and reads through it with
# LOAD_FROM_DICT_OR_GLOBALS, falling back to the globals and the builtins.
#
# Our compiler built no such cell, so every one of these raised
# `NameError: name 'B' is not defined`.  The opcode had always worked; nothing
# emitted it.
#
# Two cells come out of a class body this way, and both are plumbing rather
# than attributes: type.__new__ fills each and takes the name back out.


class Outer:
    B = list
    class Mix:
        tag = "mix"

    N = 7

    class Inner[T](B):
        pass

    class Two[T](B, metaclass=type):
        pass

    class Mixed[T](B, Mix):
        pass

    class FromBuiltins[T](Exception):     # not a class attribute at all
        pass

    def meth[T](self, x: B, n=N) -> B:
        return (x, n)

    type Alias[T] = B
    type Plain = B                        # no parameters: the value scope
                                          # alone has to see the body


def the_bases_resolve():
    return (Outer.Inner.__mro__[1].__name__, Outer.Two.__mro__[1].__name__,
            Outer.Mixed.__mro__[1].__name__, Outer.Mixed.__mro__[2].__name__,
            Outer.FromBuiltins.__mro__[1].__name__)


def annotations_and_defaults_resolve():
    return Outer().meth([1]), Outer.meth.__type_params__[0].__name__


def a_type_alias_value_resolves():
    return Outer.Alias.__value__.__name__, Outer.Plain.__value__.__name__


def the_lazy_value_is_not_a_snapshot():
    """type.__new__ repoints the cell at the finished class dict, so a value
    read after the class is built sees the class as it is now."""
    class A:
        V = 10
        type Later[T] = V

    first = A.Later.__value__
    A.V = 99
    return first, A.Later.__value__


def neither_cell_becomes_an_attribute():
    class WithBoth:
        Q = list

        class G[T](Q):
            pass

        def m(self):
            return __class__          # this is what makes __classcell__

    return (sorted(k for k in vars(WithBoth) if "class" in k),
            WithBoth().m() is WithBoth, WithBoth.G.__mro__[1].__name__)


def nesting_and_shadowing():
    class B1:
        X = "class"

        class G1[T](list):
            pass

        def read[T](self) -> X:
            return X

    return B1.G1.__mro__[1].__name__, B1().read()


X = "module"


def a_class_inside_a_function():
    L = list

    class C:
        M = L

        class I[T](M):
            pass

    return C.I.__mro__[1].__name__


def a_deleted_name_was_still_read():
    class H:
        Z = list

        class I[T](Z):
            pass

        del Z

    return H.I.__mro__[1].__name__, hasattr(H, "Z")


def two_deep():
    class P:
        Y = list

        class Q[T]:
            class R[U](list):
                pass

    return P.Q.R.__mro__[1].__name__


def a_generic_class_outside_any_class_is_unchanged():
    G = list

    class Free[T](G):
        pass

    class AlsoFree[T](list):
        pass

    return Free.__mro__[1].__name__, AlsoFree.__mro__[1].__name__


def an_ordinary_class_body_is_unchanged():
    class Plain:
        A = 1
        B = A + 1

        def m(self):
            return "m"

    return Plain.A, Plain.B, Plain().m(), sorted(
        k for k in vars(Plain) if not k.startswith("__"))


print(the_bases_resolve())
print(annotations_and_defaults_resolve())
print(a_type_alias_value_resolves())
print(the_lazy_value_is_not_a_snapshot())
print(neither_cell_becomes_an_attribute())
print(nesting_and_shadowing())
print(a_class_inside_a_function())
print(a_deleted_name_was_still_read())
print(two_deep())
print(a_generic_class_outside_any_class_is_unchanged())
print(an_ordinary_class_body_is_unchanged())
