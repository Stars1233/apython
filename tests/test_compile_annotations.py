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
