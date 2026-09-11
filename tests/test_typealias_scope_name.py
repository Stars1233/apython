# A `type X = ...` statement's scope takes its name from the AST_NAME NODE the
# parser puts in the statement's `a` field.  sym_new read that field as an
# OBJECT index, which is what it is for a function or a class -- and object
# indices and node indices come from different arenas that overlap freely, so
# it read whatever sat at that offset in comp.objs.
#
# For any file whose node count outruns its object count by enough, that is a
# read past the end of the allocation: importing CPython's typing.py -- 3500
# lines with a type alias near the end -- segfaulted in ast_obj_at, and
# test_funcattrs with it.  The answer was never USED (it fails the pointer
# test and the scope stays unnamed), so nothing about the compiled code was
# wrong; only the read was.
#
# There is no small input that faults: whether the bogus index leaves the
# mapping depends on how the two arenas have grown.  What this file pins is
# that a type alias works in every scope one can be written in, and that
# compiling a module with many more nodes than objects still does.
#
# (__qualname__ is not among the attributes: a TypeAliasType has none, here or
# in CPython.)

type A = int
print(A.__name__, A.__value__)


def f():
    type B = str
    return B


print(f().__name__, f().__value__)


class C:
    type D = float

    def m(self):
        type E = bytes
        return E


print(C.D.__name__, C.D.__value__)
print(C().m().__name__, C().m().__value__)

type G[T] = list[T]
print(G.__name__, G[int])


class Outer:
    class Inner:
        type Deep = complex


print(Outer.Inner.Deep.__name__, Outer.Inner.Deep.__value__)

# The value is lazy, and evaluating it must still find the right scope.
type Lazy = Undefined_at_definition_time  # noqa: F821
try:
    Lazy.__value__
except NameError as e:
    print("lazy:", e)

Undefined_at_definition_time = 42
print(Lazy.__value__)

# Many more nodes than objects, with the alias last: the shape that made the
# bogus index large.
src = "\n".join(["if 1:\n    pass"] * 2000 + [
    "type Late = int",
    "out = (Late.__name__, Late.__value__)",
])
g = {}
exec(compile(src, "<generated>", "exec"), g)
print(g["out"])

# ...and the same inside a function, where the scope nests.
src = "\n".join(["def gen():"] + ["    if 1:\n        pass"] * 1000 + [
    "    type Deep2 = str",
    "    return Deep2.__name__, Deep2.__value__",
])
g = {}
exec(compile(src, "<generated2>", "exec"), g)
print(g["gen"]())

print("done")
