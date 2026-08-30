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
