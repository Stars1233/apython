# PEP 695 at run time: the objects the syntax builds.
#
# The syntax parsed and was then thrown away -- `type X = V` lowered to the
# assignment `X = V`, and a def's or a class's type parameters were discarded.
# So X was the value rather than a TypeAliasType, nothing had
# __type_params__, and a CPython .pyc holding any of it raised SystemError at
# the intrinsic that builds one.
#
# The objects are lib/_typing.py, reached from the intrinsics through the same
# bridge _iocore/_io and _socketcore/_socket use.  What matters about them is
# the laziness: a bound, a constraint tuple and an alias's value are each a
# function the compiler wrote, called the first time anything reads them.

type Simple = int
print(type(Simple).__name__, repr(Simple), Simple.__name__, Simple.__value__)
print("type_params:", Simple.__type_params__)

type Generic1[T] = list[T]
print(repr(Generic1), Generic1.__type_params__, Generic1.__value__)

# The whole reason the value is lazy: an alias may name itself, and may name
# something defined further down the file.
type Tree = int | list[Tree]
print("recursive:", Tree.__value__)

type Later = Defined
print("forward reference not yet evaluated")


class Defined:
    pass


print("forward:", Later.__value__)


def plain[T](x: T) -> T:
    return x


print("def:", plain.__type_params__, plain(3), repr(plain.__type_params__[0]))


def bounded[T: int](x):
    return x


print("bound:", bounded.__type_params__[0].__bound__)


def constrained[T: (int, str)](x):
    return x


print("constraints:", constrained.__type_params__[0].__constraints__)


def paramspec[**P](x):
    return x


print("paramspec:", paramspec.__type_params__, repr(paramspec.__type_params__[0]))


def tvtuple[*Ts](x):
    return x


print("typevartuple:", tvtuple.__type_params__, repr(tvtuple.__type_params__[0]))


def several[T, U](x, y):
    return x


print("several:", several.__type_params__)


class Klass[T]:
    pass


print("class:", Klass.__type_params__)


# A default and a keyword-only argument still work through the wrapper.
def defaults[T](a=1, *, b=2):
    return (a, b)


print("defaults:", defaults.__type_params__, defaults(), defaults(5, b=6))


# The bound is evaluated lazily too, so a parameter may name one after it.
def ordered[T: S, S](x):
    return x


print("later bound:", ordered.__type_params__[0].__bound__ is
      ordered.__type_params__[1])

print("alias subscript:", repr(Generic1[int]))
