# PEP 695 type parameters and the `type` statement.
#
# The parameters are read and discarded.  A type parameter is visible only to
# annotations, and annotations are not evaluated here at all -- MAKE_FUNCTION
# pops and discards them -- so there is nothing for the names to be bound for.
# Accepting the syntax is what matters: without it the whole definition is a
# syntax error rather than a function that ignores its type parameters.
#
# `type X = V` compiles as the assignment `X = V`.  CPython makes X a
# TypeAliasType whose value is evaluated lazily and whose repr is the alias
# name; there is no such type here, so X *is* the value.  That is the one
# observable difference, and it is why this test prints the values rather than
# the aliases.
#
# `type` is a soft keyword and also a builtin, so only the statement shape --
# `type X =` or `type X[` -- is taken as one.
SRC = '''
type Alias = int
type Pair[T] = tuple


def f[T, U](a: T, b: U) -> T:
    return a


class C[T](list):
    def m[U](self, x: U):
        return x


# CPython makes these TypeAliasType wrappers; we make them the value itself.
# Either way the alias resolves to the same thing when it is used.
print(isinstance(1, Alias if isinstance(Alias, type) else int))
print(f(1, 2), C([1]).m(3), C.__name__)

# `type` keeps its ordinary meanings.
print(type(1), type("s").__name__)
type = 5
print(type)
del type
print(type([]))


class K:
    type = "attr"


print(K.type, K().type)


def g(type=3):
    return type


print(g(), g(9))
'''
ns = {}
exec(compile(SRC, "<t>", "exec"), ns)

# The brackets are a grammar now, not a bracket-depth skip that accepted
# anything between them.  These are the shapes the skipper let through.
for bad in ["def f[](): pass", "def f[1](): pass", "def f[T:](): pass",
            "def f[*](): pass", "def f[**](): pass", "class C[T,,]: pass",
            "class C[*Ts: int]: pass", "type X[] = int", "type X[T = int"]:
    try:
        compile(bad, "<t>", "exec")
        print("accepted:", bad)
    except SyntaxError:
        print("rejected:", bad)

# And the shapes it must keep taking.
for good in ["def f[T,](): pass", "class C[*Ts,]: pass", "type X[T,] = T",
             "def f[T: (int, str)](): pass", "async def f[T, **P](): pass"]:
    compile(good, "<t>", "exec")
    print("accepted:", good)


# Decorators go on OUTSIDE the type-parameter scope: `@dec def f[T]` is the
# decorator applied to what the wrapper returns, and the annotations are
# compiled inside the wrapper, where T is bound.  The decorated path built the
# function directly and skipped the wrapper, so an annotation naming a
# parameter raised NameError at definition time -- and __type_params__ came
# out empty for a decorated class.
DECORATED = '''
calls = []


def dec(x):
    calls.append(getattr(x, "__name__", x))
    return x


def twice(x):
    calls.append("twice")
    return x


@dec
def f[T](x: T) -> T:
    return x


@dec
@twice
def g[T, *Ts, **P](a: T, *rest: T) -> T:
    return a


@dec
class C[T]:
    def m(self, x: T) -> T:
        return x


@dec
class D[T](dict):
    pass


print(f(3), g(4, 5, 6))
print(f.__type_params__, g.__type_params__)
print(C.__type_params__, D.__type_params__)
print(C().m("s"), issubclass(D, dict))
print(f.__annotations__, C.m.__annotations__)
print(calls)
'''
ns = {}
exec(compile(DECORATED, "<t>", "exec"), ns)
