# The `match` statement, compiled from source.
#
# One contract runs through the pattern emitters: a pattern is entered with the
# value to match on top of the stack and CONSUMES it either way -- falling
# through on a match, jumping to its failure label on a mismatch.  CPython
# instead tracks how many values a pattern has left above the subject and pops
# exactly that many on each failure path, which is tighter and a great deal
# more bookkeeping; here the caller keeps its own spare copy, so no pattern has
# to know what is beneath it.
#
# Destructuring is where that needs help: UNPACK_SEQUENCE leaves n values on
# the stack and a failure at element k has to drop the ones still there, so
# each sequence, mapping and class pattern builds a ladder of POP_TOPs, one
# rung per depth.
#
# `match` and `case` are soft keywords -- ordinary names that only this
# position treats specially.
SRC = '''
class Point:
    __match_args__ = ("x", "y")

    def __init__(self, x, y):
        self.x, self.y = x, y


def classify(s):
    match s:
        case 0 | 1 | 2:
            return "small"
        case None:
            return "none"
        case True:
            return "true"
        case "start" | "stop":
            return "verb"
        case []:
            return "empty"
        case [a]:
            return ("one", a)
        case [a, b] if a > b:
            return ("desc", a, b)
        case [a, b]:
            return ("asc", a, b)
        case [a, b, *rest]:
            return ("many", a, b, rest)
        case Point(0, 0):
            return "origin"
        case Point(x=0, y=y):
            return ("on-y", y)
        case Point(x, y):
            return ("point", x, y)
        case {"name": n, **rest}:
            return ("named", n, sorted(rest))
        case {"x": x, "y": y}:
            return ("coords", x, y)
        case {}:
            return "mapping"
        case str() as text:
            return ("text", text)
        case int() | float():
            return "number"
        case _:
            return "other"


SUBJECTS = [
    0, 1, 2, 3, None, True, False, "start", "stop", "other",
    [], [9], [2, 1], [1, 2], [1, 2, 3, 4], (5, 6),
    Point(0, 0), Point(0, 7), Point(3, 4),
    {"name": "n"}, {"name": "n", "extra": 1}, {"x": 1, "y": 2}, {},
    3.5, b"bytes", object,
]
for s in SUBJECTS:
    print(classify(s))


# match binds even when the guard then rejects the case, and falls out of the
# statement entirely when nothing matches.
def bind_and_guard(v):
    got = "unset"
    match v:
        case n if n > 10:
            got = ("big", n)
        case n if n > 5:
            got = ("mid", n)
    return got


print([bind_and_guard(v) for v in (20, 7, 1)])


# `match` and `case` are still ordinary names everywhere else.
match = 5
case = match + 1
print(match, case, [match for match in (1, 2)][-1])
d = {match: case}
print(sorted(d.items()))
'''
ns = {}
exec(compile(SRC, "<t>", "exec"), ns)
