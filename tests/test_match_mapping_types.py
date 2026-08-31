# MATCH_MAPPING decides whether a subject can be matched against `{...}`.
#
# It answered yes for anything whose type has mp_subscript, and a list has one
# -- so `case {"k": v}` against a list said "this is a mapping", MATCH_KEYS
# then looked keys up in it, and the interpreter crashed.  CPython asks a type
# flag that only mappings carry; the nearest thing here is dict and its
# subclasses, plus the explicit exclusion of the sequence types that
# MATCH_SEQUENCE already lists on its own side.
def kind(s):
    match s:
        case {"a": v}:
            return ("map-a", v)
        case {}:
            return "map"
        case [x]:
            return ("seq1", x)
        case []:
            return "seq0"
        case str():
            return "str"
        case _:
            return "other"


class MyDict(dict):
    pass


class Subscriptable:
    def __getitem__(self, k):
        return k


cases = [
    {"a": 1},
    {"b": 2},
    {},
    [1],
    [],
    (1,),
    (),
    "s",
    b"b",
    5,
    1.5,
    None,
    MyDict(a=9),
    MyDict(),
    {1: 2},
]
for c in cases:
    print(repr(c), kind(c))

# A plain object with __getitem__ is not a mapping, and matching one must not
# reach MATCH_KEYS at all.
print(kind(Subscriptable()))
