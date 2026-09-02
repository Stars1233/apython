"""A PEP 585 subscript that is not a pointer.

`list[int]` puts a type in the alias; `list[0]` puts an integer, and an
integer is an immediate Value, not an object.  generic_alias_new increfed it
unconditionally, so the refcount write landed on address 0 -- or wherever the
number pointed.  Nothing in the interpreter's own tests subscripts a builtin
with a number, and CPython's dis does, three imports deep.
"""


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(32), repr(got))


check("a type", lambda: repr(list[int]))
check("an int", lambda: repr(list[0]))
check("a negative int", lambda: repr(list[-5]))
check("a big int", lambda: repr(list[2 ** 70]))
check("a float", lambda: repr(list[1.5]))
check("None", lambda: repr(list[None]))
check("a bool", lambda: repr(list[True]))
check("a str", lambda: repr(list["name"]))
check("a tuple of ints", lambda: repr(dict[0, 1]))
check("mixed", lambda: repr(dict[int, 0]))
check("Ellipsis", lambda: repr(tuple[int, ...]))
check("nested", lambda: repr(list[list[0]]))

# The alias has to survive being kept, not just being printed once.
kept = [list[i] for i in range(200)]
check("kept aliases", lambda: (len(kept), repr(kept[7]), repr(kept[199])))
check("still callable", lambda: type(list[0]()).__name__)
check("origin", lambda: list[0].__origin__)
check("args", lambda: list[0].__args__)
check("args of a pair", lambda: dict[int, 0].__args__)

# And being collected: the int must not be decrefed as if it were an object.
for _ in range(500):
    _ = list[7]
print("survived the loop".ljust(32), repr(True))
