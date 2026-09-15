# PEP 563: `from __future__ import annotations`.
#
# There was no __future__ handling anywhere in src/compiler/.  The import
# compiled as an ordinary ImportFrom and bound a name, and annotations were
# still EVALUATED -- so `def f(x: Nope)` raised NameError at def time where
# CPython stores {'x': 'Nope'}, and every module written for the future
# import failed at IMPORT rather than at use.  That is the point of the PEP:
# a module may refer forward to a name that does not exist yet, and a
# circular import may annotate with a type it cannot yet see.
#
# What is stored is the annotation's own SOURCE TEXT, taken as a slice of the
# file between the node's start and the end its AstSpan records.  CPython
# stores what its UNPARSER produces from the same node, which differs in
# whitespace and in redundant parentheses; DIVERGENCES.md records it, and
# every case below is one where the two agree.
from __future__ import annotations

import sys


def simple(x: Nope, y: int = 3) -> Later:
    pass


print("a function:", simple.__annotations__)
print("and it is callable:", simple(1))


class WithAnnotations:
    a: Undefined
    b: int = 1
    c: 'already a string'


print("a class body:", WithAnnotations.__annotations__)

module_level: AlsoUndefined = None
print("module level:", __annotations__)


# --- the forward reference the PEP exists for -------------------------
def takes_later(node: Tree) -> Tree:
    return node


class Tree:
    left: Tree | None = None
    right: Tree | None = None


print("forward reference:", takes_later.__annotations__, Tree.__annotations__)

# --- every parameter kind ---------------------------------------------
def every_kind(a: A, /, b: B, *args: Args, c: C, d: D = 1, **kw: Kw) -> Ret:
    pass


print("all parameter kinds:", every_kind.__annotations__)

# --- nested scopes inherit the flag ------------------------------------
def outer() -> Outer:
    def inner(z: Inner) -> AlsoInner:
        pass

    class Local:
        w: LocalAnn

    return inner, Local


inner_fn, local_cls = outer()
print("a nested def:", inner_fn.__annotations__)
print("a nested class:", local_cls.__annotations__)
print("the enclosing def:", outer.__annotations__)

# --- a lambda has none, and a function-local annotation is not stored --
def local_annotation():
    q: NeverEvaluated = 1
    return q


print("inside a function:", local_annotation(),
      "__annotations__" in local_annotation.__code__.co_names)

# --- subscripts, calls and operators all survive as text ---------------
def complicated(a: dict[str, list[int]], b: Callable[..., None],
                c: int | None, d: Foo.Bar.Baz, e: Foo(1, 2)) -> 'quoted':
    pass


for name, text in complicated.__annotations__.items():
    print("  %-6s %r" % (name, text))

# --- the annotation is never evaluated, even when it would raise -------
def would_raise(x: 1 / 0) -> [][0]:
    pass


print("never evaluated:", would_raise.__annotations__)

# --- and it evaluates to the right thing when someone asks -------------
T = int


def resolvable(x: T) -> list[T]:
    pass


print("eval'd on demand:",
      [eval(v, globals()) for v in resolvable.__annotations__.values()])

# --- the name the import binds is still bound --------------------------
import __future__

print("__future__ is importable:", __future__.annotations.__class__.__name__)
print("the name is bound:", annotations.__class__.__name__)
print("the feature's flag:", hex(__future__.annotations.compiler_flag))
print("and the code object carries it:",
      bool(compile("from __future__ import annotations\nx: A\n", "<s>", "exec")
           .co_flags & __future__.annotations.compiler_flag),
      bool(compile("x: int\n", "<s>", "exec").co_flags
           & __future__.annotations.compiler_flag))

# --- a module WITHOUT the import still evaluates ------------------------
other = compile("def g(x: int) -> str: pass\n", "<other>", "exec")
namespace = {}
exec(other, namespace)
print("without the import:", namespace["g"].__annotations__)
try:
    exec("def h(x: NotDefined): pass\n", {})
    print("without the import, a bad name: NOT RAISED")
except NameError:
    print("without the import, a bad name: NameError")

# --- and compile() of a source that HAS it, from a string --------------
src = "from __future__ import annotations\ndef k(x: Missing) -> Gone: pass\n"
ns = {}
exec(compile(src, "<s>", "exec"), ns)
print("compiled from a string:", ns["k"].__annotations__)
print("survived")
