# except* looks inside a NESTED ExceptionGroup.
#
# eg_split was a flat partition -- one isinstance per element, never asking
# whether an element was itself a group -- so a nested group landed whole in
# `rest` and `except* KeyError` over
# ExceptionGroup("outer", [ExceptionGroup("inner", [KeyError()]), OSError()])
# matched only the OSError and left the outer group unhandled.
#
# CPython recurses, and the nesting is preserved in BOTH halves.


def show(label, fn):
    try:
        fn()
        print(label, "-> no raise")
    except BaseException as e:
        print(label, "-> escaped", type(e).__name__, e)


def two_deep():
    caught = []
    try:
        raise ExceptionGroup("outer",
                             [ExceptionGroup("inner", [KeyError("k")]),
                              OSError("o")])
    except* KeyError as e:
        caught.append(("KeyError", repr(e)))
    except* OSError as e:
        caught.append(("OSError", repr(e)))
    for c in caught:
        print("  ", c)


show("two deep", two_deep)


def three_deep():
    try:
        raise ExceptionGroup("a", [ExceptionGroup("b", [
            ExceptionGroup("c", [ValueError("v")]), TypeError("t")])])
    except* ValueError as e:
        print("   V", repr(e))
    except* TypeError as e:
        print("   T", repr(e))


show("three deep", three_deep)


def all_match():
    try:
        raise ExceptionGroup("o", [ExceptionGroup("i", [KeyError("a")]),
                                   KeyError("b")])
    except* KeyError as e:
        print("   all", repr(e))


show("all match", all_match)


def none_match():
    try:
        raise ExceptionGroup("o", [ExceptionGroup("i", [KeyError("a")])])
    except* ValueError as e:
        print("   never", repr(e))


show("none match", none_match)


def tuple_of_types():
    try:
        raise ExceptionGroup("o", [ExceptionGroup("i", [KeyError("k")]),
                                   ValueError("v"), OSError("o")])
    except* (KeyError, ValueError) as e:
        print("   pair", repr(e))
    except* OSError as e:
        print("   os", repr(e))


show("tuple of types", tuple_of_types)


def deep_mixed():
    try:
        raise ExceptionGroup("top", [
            ExceptionGroup("left", [KeyError("lk"), OSError("lo")]),
            ExceptionGroup("right", [KeyError("rk")]),
            ValueError("bare"),
        ])
    except* KeyError as e:
        print("   K", repr(e))
    except* OSError as e:
        print("   O", repr(e))
    except* ValueError as e:
        print("   V", repr(e))


show("deep mixed", deep_mixed)


# A bare `except*` body that re-raises is the shape the unwinder has to get
# right when only part of the group was handled.
def partial_reraise():
    try:
        try:
            raise ExceptionGroup("o", [ExceptionGroup("i", [KeyError("k")]),
                                       OSError("o")])
        except* KeyError:
            print("   took the KeyError")
    except BaseException as e:
        print("   escaped:", type(e).__name__, repr(e))


show("partial", partial_reraise)


# BaseExceptionGroup nests the same way, and a KeyboardInterrupt inside one is
# not caught by `except* Exception`.
def base_group():
    try:
        raise BaseExceptionGroup("o", [
            BaseExceptionGroup("i", [KeyboardInterrupt()]), ValueError("v")])
    except* ValueError as e:
        print("   V", repr(e))
    except* KeyboardInterrupt as e:
        print("   KI", repr(e))


show("base group", base_group)


# The leaves' identity survives: the objects in the halves are the originals.
def identity():
    leaf = KeyError("leaf")
    inner = ExceptionGroup("i", [leaf])
    try:
        raise ExceptionGroup("o", [inner, OSError("o")])
    except* KeyError as e:
        print("   same leaf:", e.exceptions[0].exceptions[0] is leaf)
        print("   inner is a new group:", e.exceptions[0] is not inner)
        print("   types:", type(e).__name__,
              type(e.exceptions[0]).__name__)


show("identity", identity)

print("done")
