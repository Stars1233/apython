# At module and class scope an annotation is evaluated and recorded in
# __annotations__; inside a function it is not evaluated at all.  Nothing was
# evaluated anywhere, so `x: Undefined` was silently accepted and
# __annotations__ never existed -- which dataclasses reads.
z: str
w: int = 3

print(__annotations__, w)


class C:
    y: int
    v: "str" = "s"

    def m(self):
        q: Undefined = 1        # never evaluated
        return q


print(C.__annotations__, C.v, C().m())


# A module-level bad annotation raises where CPython raises.
try:
    exec("bad: Nope", {})
except NameError as e:
    print("module", e)

try:
    exec("class K:\n    bad: Nope\n", {})
except NameError as e:
    print("class", e)

ns = {}
exec("def f():\n    bad: Nope\n    return 1\nout = f()\n", ns)
print("function", ns["out"])


# An annotated attribute or subscript evaluates the annotation but records
# nothing.
class Holder:
    pass


h = Holder()
try:
    h.attr: Nope
except NameError:
    print("attr annotation evaluated")

d = {}
try:
    d["k"]: Nope
except NameError:
    print("subscript annotation evaluated")

print(sorted(__annotations__))


# A class with no annotations has none of its own.
class Bare:
    x = 1


print("__annotations__" in Bare.__dict__)


# Annotations inside an if still create the dict.
ns2 = {}
exec("if True:\n    a: int = 1\n", ns2)
print(sorted(ns2["__annotations__"]))


# `simple` -- a bare, UNPARENTHESISED identifier -- is what decides whether an
# annotation is recorded, and `(x)` is the same Name node as `x`, so the bit
# has to come from the parser.  Without it `(y): int = 2` put y in
# __annotations__, where CPython evaluates the annotation and drops it.
print("=== a parenthesised target is not simple ===")
ns3 = {}
exec("a: int = 1\n(b): int = 2\nc: str\n(d): str\ne.f: int = 3\n"
     "g[0]: int = 4\n", {"e": type('E', (), {})(), "g": {}}, ns3)
print(sorted(k for k in ns3.get("__annotations__", {})))


class Ann:
    p: int = 1
    (q): int = 2
    r: str
    (s): str


print(sorted(Ann.__annotations__))
print(Ann.p, Ann.q)

# The annotation is still EVALUATED, whether or not it is recorded.
seen = []


def note(x):
    seen.append(x)
    return int


ns4 = {}
exec("(m): note('paren') = 1\nn: note('bare') = 2\n", {"note": note}, ns4)
print(seen, sorted(ns4["__annotations__"]))
