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
