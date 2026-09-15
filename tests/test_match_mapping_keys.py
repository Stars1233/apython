# A mapping pattern's keys, when they are not literals.
#
# cg_pat_keys folded the keys into one compile-time tuple and emitted a single
# LOAD_CONST, which is why it accepted AST_CONST and nothing else: a key that
# is not a literal has no value at compile time and nowhere to go.  So
#
#     case {Color.RED: v}:
#
# was "SyntaxError: a mapping pattern's keys must be literals", and so were
# `{-1: v}` and `{1+2j: v}`, which CPython compiles.  A dotted name is the
# common spelling -- an enum member or a module constant -- and typing's own
# examples are written that way.
#
# CPython's grammar is
#     key_value_pattern: (literal_expr | attr) ':' pattern
# so what may be written there is settled by the PARSER, not by the codegen:
# a bare name, a call, a display and a parenthesised tuple are each refused
# at the point they are read.  The all-literal fold survives as the fast
# path, because it is what nearly every mapping pattern is.
import enum


class Color(enum.Enum):
    RED = "red"
    BLUE = "blue"


class Plain:
    KEY = "plain"


MODULE_KEY = "module"


class Nested:
    class Inner:
        DEEP = "deep"


def classify(subject):
    match subject:
        case {Color.RED: v}:
            return ("enum member", v)
        case {Plain.KEY: v}:
            return ("class attribute", v)
        case {Nested.Inner.DEEP: v}:
            return ("a dotted chain", v)
        case {"literal": v}:
            return ("a literal", v)
        case {1: v}:
            return ("an int", v)
        case {-1: v}:
            return ("a signed number", v)
        case {1 + 2j: v}:
            return ("a complex literal", v)
        case {None: v}:
            return ("None", v)
        case _:
            return "no match"


cases = [
    {Color.RED: "a"}, {"plain": "b"}, {"deep": "c"}, {"literal": "d"},
    {1: "e"}, {-1: "f"}, {1 + 2j: "g"}, {None: "h"}, {"other": "j"},
]
for subject in cases:
    print("%-22r %s" % (subject, classify(subject)))

# --- several keys in one pattern, mixed -------------------------------
def two(subject):
    match subject:
        case {Color.RED: a, "lit": b, 3: c}:
            return (a, b, c)
        case _:
            return None


print("three mixed keys:", two({Color.RED: 1, "lit": 2, 3: 3, "extra": 4}))
print("a missing key does not match:", two({Color.RED: 1, "lit": 2}))

# --- the all-literal fast path still works, including **rest ----------
def rest(subject):
    match subject:
        case {"a": x, **others}:
            return (x, others)
        case _:
            return None


print("**rest:", rest({"a": 1, "b": 2, "c": 3}))


def nested(subject):
    match subject:
        case {Color.RED: {"inner": v}}:
            return v
        case _:
            return None


print("a nested mapping under a dotted key:", nested({Color.RED: {"inner": 9}}))

# --- guards, and the order the keys are evaluated in -------------------
def guarded(subject):
    match subject:
        case {Color.BLUE: v} if v > 10:
            return "big"
        case {Color.BLUE: v}:
            return "small"
        case _:
            return "none"


print("with a guard:", guarded({Color.BLUE: 20}), guarded({Color.BLUE: 1}),
      guarded({}))

# --- a key whose value changes between matches ------------------------
class Mutable:
    key = "first"


def by_mutable(subject):
    match subject:
        case {Mutable.key: v}:
            return v
        case _:
            return None


print("looked up at match time:", by_mutable({"first": 1}))
Mutable.key = "second"
print("and again after it changed:", by_mutable({"second": 2}),
      by_mutable({"first": 1}))

# --- what the parser must refuse --------------------------------------
BAD = [
    ("a bare name", "case {x: v}: pass"),
    ("a call", "case {f(): v}: pass"),
    ("a subscript", "case {d[0]: v}: pass"),
    ("a list display", "case {[1]: v}: pass"),
    ("a parenthesised tuple", "case {(1, 2): v}: pass"),
    ("the empty tuple", "case {(): v}: pass"),
    ("an f-string", 'case {f"{x}": v}: pass'),
]
for what, line in BAD:
    source = "match m:\n    %s\n" % line
    try:
        compile(source, "<s>", "exec")
        print("%-24s NOT REFUSED" % what)
    except SyntaxError:
        print("%-24s SyntaxError" % what)

# --- a non-hashable key is a run-time error, not a compile-time one ----
# CPython builds the tuple and lets MATCH_KEYS raise, which is what an
# unhashable key in a dict lookup does anyway.
print("survived")
