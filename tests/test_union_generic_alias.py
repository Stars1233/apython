# `int | list[int]` -- PEP 604's union over PEP 585's generic alias.
#
# `int | str` worked and `list[int]` worked, but a union could not hold one:
# union_operand_ok took a type, a union or None, and types.GenericAlias is
# none of the three.  It is the shape every modern annotation is written in --
# `dict[str, int] | None` -- and `type Tree = int | list[Tree]`, the
# self-referential alias PEP 695 exists to allow, is exactly it.


def show(label, fn):
    try:
        print("%-34s %s" % (label, fn()))
    except Exception as e:
        print("%-34s %s: %s" % (label, type(e).__name__, e))


show("int | str", lambda: int | str)
show("list[int]", lambda: list[int])
show("int | list[int]", lambda: int | list[int])
show("list[int] | int", lambda: list[int] | int)
show("list[int] | None", lambda: list[int] | None)
show("dict[str, int] | None", lambda: dict[str, int] | None)
show("list[int] | dict[str, int]", lambda: list[int] | dict[str, int])
show("int | list[int] | str", lambda: int | list[int] | str)
show("(int | list[int]).__args__", lambda: (int | list[int]).__args__)
show("union of alias equality",
     lambda: (int | list[int]) == (list[int] | int))
show("int | 1", lambda: int | 1)
show("list[int] | 1", lambda: list[int] | 1)


class Meta(type):
    pass


class C(metaclass=Meta):
    pass


show("C | None", lambda: C | None)
show("C | list[int]", lambda: C | list[int])


# isinstance and issubclass said "or a union" in their refusals and accepted
# none: `isinstance(1, int | str)` was a TypeError.  A union is its members,
# exactly as a tuple of them would be -- including inside a tuple, where this
# loop is flat and CPython's recursion is not.
#
# A parameterized generic is its own refusal in CPython, and saying only that
# it is not a type buries which of the two mistakes it was.
print("=== a union as the second argument ===")


class B:
    pass


class D(B):
    pass


def show(label, fn):
    try:
        print("%-38s %r" % (label, fn()))
    except Exception as e:
        print("%-38s %s: %s" % (label, type(e).__name__, e))


for label, fn in (
        ("isinstance(1, int|str)", lambda: isinstance(1, int | str)),
        ("isinstance('a', int|str)", lambda: isinstance("a", int | str)),
        ("isinstance(1.5, int|str)", lambda: isinstance(1.5, int | str)),
        ("isinstance(None, int|None)", lambda: isinstance(None, int | None)),
        ("isinstance(D(), B|int)", lambda: isinstance(D(), B | int)),
        ("isinstance(1, int|str|float)", lambda: isinstance(1, int | str | float)),
        ("isinstance(True, int|str)", lambda: isinstance(True, int | str)),
        ("issubclass(D, B|int)", lambda: issubclass(D, B | int)),
        ("issubclass(int, str|float)", lambda: issubclass(int, str | float)),
        ("issubclass(bool, int|str)", lambda: issubclass(bool, int | str)),
        ("nested in a tuple", lambda: isinstance(1, (int | str, bytes))),
        ("nested, no match", lambda: isinstance(1.5, (int | str, bytes))),
        ("nested, second", lambda: isinstance(b"x", (int | str, bytes))),
        ("isinstance(1, list[int])", lambda: isinstance(1, list[int])),
        ("issubclass(list, list[int])", lambda: issubclass(list, list[int])),
        ("generic in a tuple", lambda: isinstance(1, (list[int],))),
        ("generic in a union", lambda: isinstance(1, list[int] | None)),
        ("isinstance(1, 5)", lambda: isinstance(1, 5)),
        ("issubclass(int, 5)", lambda: issubclass(int, 5))):
    show(label, fn)
