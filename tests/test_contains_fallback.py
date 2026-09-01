"""`in` over a container with no sq_contains, which falls back to iteration.

The fallback open-coded its own equality test and handled exactly two shapes:
both operands immediate, or a pointer element.  Everything else fell through
to an identity check and answered False.  Two consequences, both silent:

  * `98 in memoryview(b"abcdef")` was False while `98 in list(mv)` was True,
    because a memoryview yields freshly made ints and the list holds the
    constant the comparison names.
  * The answer depended on which side the heap value sat: `n in iter([98])`
    was True and `98 in iter([n])` was False for the same n.  Only under
    INT_STRESS=1 in the ordinary suite, where every int of size 8 or more is
    a heap object -- which is exactly what that flag is for.

A raising __eq__ inside the fallback was swallowed as "not equal" too.
"""


def mk(a, b):
    return a + b


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(34), repr(got))


# A number the compiler cannot fold, so it is a different object from the
# constant it is compared against and the identity shortcut cannot hide.
n = mk(90, 8)

# --- containers whose only search is the iteration fallback ---
check("in a memoryview", lambda: (98 in memoryview(b"abcdef"),
                                  200 in memoryview(b"abcdef")))
check("in a bytes iterator", lambda: 98 in iter(b"abcdef"))
check("in a bytearray iterator", lambda: 98 in iter(bytearray(b"abcdef")))
check("in a memoryview iterator", lambda: 98 in iter(memoryview(b"abcdef")))
check("in a list iterator", lambda: 98 in iter([97, 98, 99]))
check("in a generator", lambda: 98 in (x for x in [97, 98, 99]))
check("in a range", lambda: (98 in range(200), 98 in range(0, 200, 3)))
check("not in a range", lambda: 98 not in range(50))

# --- the same value on either side ---
check("fresh on the left", lambda: n in iter([98]))
check("fresh on the right", lambda: 98 in iter([n]))
check("fresh on both", lambda: mk(90, 8) in iter([mk(97, 1)]))
check("absent", lambda: 55 in iter([n]))

# --- other representations through the same path ---
check("str element", lambda: "ab" in iter([mk("a", "b")]))
check("tuple element", lambda: (1, 2) in iter([mk((1,), (2,))]))
check("float element", lambda: 3.5 in iter([mk(3.0, 0.5)]))
check("bool against int", lambda: True in iter([mk(0, 1)]))
check("int against float", lambda: 2 in iter([mk(1.0, 1.0)]))
check("mixed misses", lambda: "98" in iter([n]))

# --- a big int, past any immediate range on any build ---
big = mk(2 ** 70, 1)
check("big int found", lambda: 2 ** 70 + 1 in iter([big]))
check("big int absent", lambda: 2 ** 70 + 2 in iter([big]))


# --- a raising __eq__ must propagate, not read as False ---
class Boom:
    def __eq__(self, other):
        raise ValueError("boom")

    def __hash__(self):
        return 0


check("raising __eq__", lambda: Boom() in iter([1, 2, 3]))
check("identity beats __eq__", lambda: (lambda b: b in iter([b]))(Boom()))


# --- the container's own __contains__ still wins where it has one ---
class WithContains:
    def __contains__(self, item):
        return item == 98

    def __iter__(self):
        return iter([1, 2, 3])


check("__contains__ wins", lambda: (98 in WithContains(), 1 in WithContains()))


class OnlyGetitem:
    def __init__(self, items):
        self.items = items

    def __getitem__(self, i):
        return self.items[i]


check("legacy __getitem__", lambda: (98 in OnlyGetitem([97, 98]),
                                     55 in OnlyGetitem([97, 98])))
