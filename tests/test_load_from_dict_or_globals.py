# LOAD_FROM_DICT_OR_GLOBALS pushed a BORROWED reference.  dict_get answers
# with a Value and the guard was swept to `test rax, rax`, but the INCREF and
# the push below it still read rdx as a tag -- and rdx held dict_get's probe
# slot.  So the INCREF was skipped whenever that number did not look like
# TAG_PTR, and which names it happened to was a function of where they hashed.
# `class Inner[T](B)` with B a class attribute freed the base out from under
# the class being built: "TypeError: bases must be types", or an invalid free
# inside tuple_clear under valgrind.
#
# The opcode is only reached from a PEP 695 generic class whose bases name
# something in the enclosing CLASS scope.  Until our compiler learned to build
# the __classdict__ cell that needs, this file could only be run from
# CPython's .pyc; test_pep695_classdict covers the compiler half.


def a_class_body_reads_an_enclosing_name():
    """LOAD_FROM_DICT_OR_GLOBALS: the base came back BORROWED, so the class
    being built freed it.  Which names it happened to was a function of the
    probe slot the stale register held."""
    class Outer:
        B = list

        class Inner[T](B):
            pass

        class Second[T](B):
            pass

    return (Outer.Inner.__mro__[1].__name__, Outer.Second.__mro__[1].__name__,
            len(Outer.Inner([1, 2])))



print(a_class_body_reads_an_enclosing_name())
