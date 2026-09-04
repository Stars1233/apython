"""_typing - the runtime objects PEP 695's syntax builds.

CPython's _typing is a C module holding TypeVar, ParamSpec, TypeVarTuple,
TypeAliasType and the Generic base.  Nothing about them needs to be C: they
are small objects with a repr, a few read-only attributes, and -- for
TypeAliasType -- a value that is not evaluated until it is asked for.

The interpreter reaches this module from the CALL_INTRINSIC_1 and
CALL_INTRINSIC_2 handlers, which is where `type X = int` and `def f[T]()`
land.  That is the same split _iocore/_io and _socketcore/_socket use: the
bytecode is the interpreter's, the objects it builds are Python's.

The laziness is the part that matters.  A bound, a constraint tuple and an
alias's value are all written as a function by the compiler and evaluated on
first access, so `type Alias = Undefined` is not an error until something
reads Alias.__value__ -- which is what lets a pair of aliases refer to each
other.
"""


def _idfunc(x):
    """-> its argument, unchanged.

    typing.py assigns it as `_SpecialForm.__call__`, so that `Any(x)` is x.
    CPython's is a C function taking exactly one argument, and the message a
    program sees when it is called wrong is that arity's.
    """
    return x


class TypeVar:
    """A type variable: `T` in `def f[T](x: T) -> T`.

    __bound__ and __constraints__ are mutually exclusive, and either may
    arrive as a thunk the compiler built -- PEP 695 evaluates a bound lazily
    so it may name a parameter declared after it.
    """

    __slots__ = ("_name", "_bound", "_constraints", "_evaluate_bound",
                 "_evaluate_constraints", "_covariant", "_contravariant",
                 "_infer_variance")

    def __init__(self, name, *constraints, bound=None, covariant=False,
                 contravariant=False, infer_variance=False):
        if covariant and contravariant:
            raise ValueError("Bivariant type variables are not supported.")
        if constraints and bound is not None:
            raise TypeError("Constraints cannot be combined with bound=...")
        if len(constraints) == 1:
            raise TypeError("A single constraint is not allowed")
        self._name = name
        self._bound = bound
        self._constraints = tuple(constraints)
        self._evaluate_bound = None
        self._evaluate_constraints = None
        self._covariant = bool(covariant)
        self._contravariant = bool(contravariant)
        self._infer_variance = bool(infer_variance)

    @property
    def __name__(self):
        return self._name

    @property
    def __bound__(self):
        if self._evaluate_bound is not None:
            self._bound = self._evaluate_bound()
            self._evaluate_bound = None
        return self._bound

    @property
    def __constraints__(self):
        if self._evaluate_constraints is not None:
            self._constraints = tuple(self._evaluate_constraints())
            self._evaluate_constraints = None
        return self._constraints

    @property
    def __covariant__(self):
        return self._covariant

    @property
    def __contravariant__(self):
        return self._contravariant

    @property
    def __infer_variance__(self):
        return self._infer_variance

    def __repr__(self):
        if self._infer_variance:
            prefix = ""
        elif self._covariant:
            prefix = "+"
        elif self._contravariant:
            prefix = "-"
        else:
            prefix = "~"
        return prefix + self._name

    def __typing_subst__(self, arg):
        return arg

    def __reduce__(self):
        return self._name


class ParamSpecArgs:
    __slots__ = ("__origin__",)

    def __init__(self, origin):
        self.__origin__ = origin

    def __repr__(self):
        return "%s.args" % (self.__origin__.__name__,)

    def __eq__(self, other):
        if not isinstance(other, ParamSpecArgs):
            return NotImplemented
        return self.__origin__ == other.__origin__


class ParamSpecKwargs:
    __slots__ = ("__origin__",)

    def __init__(self, origin):
        self.__origin__ = origin

    def __repr__(self):
        return "%s.kwargs" % (self.__origin__.__name__,)

    def __eq__(self, other):
        if not isinstance(other, ParamSpecKwargs):
            return NotImplemented
        return self.__origin__ == other.__origin__


class ParamSpec:
    """`**P` in `def f[**P]()`: a whole parameter list as one variable."""

    __slots__ = ("_name", "_bound", "_evaluate_bound", "_covariant",
                 "_contravariant", "_infer_variance")

    def __init__(self, name, *, bound=None, covariant=False,
                 contravariant=False, infer_variance=False):
        if covariant and contravariant:
            raise ValueError("Bivariant type variables are not supported.")
        self._name = name
        self._bound = bound
        self._evaluate_bound = None
        self._covariant = bool(covariant)
        self._contravariant = bool(contravariant)
        self._infer_variance = bool(infer_variance)

    @property
    def __name__(self):
        return self._name

    @property
    def __bound__(self):
        if self._evaluate_bound is not None:
            self._bound = self._evaluate_bound()
            self._evaluate_bound = None
        return self._bound

    @property
    def __covariant__(self):
        return self._covariant

    @property
    def __contravariant__(self):
        return self._contravariant

    @property
    def __infer_variance__(self):
        return self._infer_variance

    @property
    def args(self):
        return ParamSpecArgs(self)

    @property
    def kwargs(self):
        return ParamSpecKwargs(self)

    def __repr__(self):
        if self._infer_variance:
            prefix = ""
        elif self._covariant:
            prefix = "+"
        elif self._contravariant:
            prefix = "-"
        else:
            prefix = "~"
        return prefix + self._name

    def __reduce__(self):
        return self._name


class TypeVarTuple:
    """`*Ts` in `def f[*Ts]()`: any number of types as one variable."""

    __slots__ = ("_name",)

    def __init__(self, name):
        self._name = name

    @property
    def __name__(self):
        return self._name

    def __iter__(self):
        yield Unpack(self)

    def __repr__(self):
        return self._name

    def __reduce__(self):
        return self._name


class Unpack:
    """`*Ts` where a type is wanted.  typing.Unpack, in the shape iteration
    over a TypeVarTuple produces."""

    __slots__ = ("__typing_unpacked_tuple_args__",)

    def __init__(self, arg):
        self.__typing_unpacked_tuple_args__ = arg

    def __repr__(self):
        return "*%r" % (self.__typing_unpacked_tuple_args__,)


class TypeAliasType:
    """`type X = ...`, whose value is not evaluated until it is read.

    That is the whole point of the statement form: an alias may name another
    that is defined further down the file, and a self-referential one -- `type
    Tree = int | list[Tree]` -- is only possible because nothing evaluates the
    right-hand side at definition time.
    """

    __slots__ = ("_name", "_value", "_evaluate_value", "_type_params")

    def __init__(self, name, value, *, type_params=()):
        self._name = name
        self._value = value
        self._evaluate_value = None
        self._type_params = tuple(type_params)

    @property
    def __name__(self):
        return self._name

    @property
    def __value__(self):
        if self._evaluate_value is not None:
            self._value = self._evaluate_value()
            self._evaluate_value = None
        return self._value

    @property
    def __type_params__(self):
        return self._type_params

    def __repr__(self):
        return self._name

    def __getitem__(self, args):
        return _GenericAlias(self, args if isinstance(args, tuple) else (args,))

    def __reduce__(self):
        return self._name


class _GenericAlias:
    """`Alias[int]`, kept whole rather than substituted.  Nothing in this
    interpreter inspects a subscripted alias; what matters is that the
    subscript does not raise, and that `class C[T]` -- which puts Generic[T]
    in its base list -- can say which class it stands for."""

    __slots__ = ("__origin__", "__args__")

    def __mro_entries__(self, bases):
        return (self.__origin__,)

    def __init__(self, origin, args):
        self.__origin__ = origin
        self.__args__ = args

    def __repr__(self):
        inner = ", ".join(_type_repr(a) for a in self.__args__)
        return "%r[%s]" % (self.__origin__, inner)

    def __eq__(self, other):
        if not isinstance(other, _GenericAlias):
            return NotImplemented
        return (self.__origin__ == other.__origin__
                and self.__args__ == other.__args__)


def _type_repr(obj):
    if isinstance(obj, type):
        if obj.__module__ == "builtins":
            return obj.__qualname__
        return "%s.%s" % (obj.__module__, obj.__qualname__)
    if obj is Ellipsis:
        return "..."
    return repr(obj)


class Generic:
    """The base a `class C[T]` gets.  CPython's carries the machinery for
    __class_getitem__ and parameter substitution; what is needed here is that
    it exists and that subscripting it answers something."""

    __slots__ = ()

    def __class_getitem__(cls, params):
        if not isinstance(params, tuple):
            params = (params,)
        return _GenericAlias(cls, params)


# ---------------------------------------------------------------------------
# The four entry points the interpreter's intrinsics call.  Each takes what
# the bytecode has on the stack and answers the object it should push.
# ---------------------------------------------------------------------------

def _typevar(name):
    """INTRINSIC_TYPEVAR: a bare `T`, with no bound and no constraints.

    Its variance is inferred rather than declared, which is what PEP 695
    gives every type parameter -- and what makes its repr `T` where an
    explicit typing.TypeVar("T") is `~T`.
    """
    return TypeVar(name, infer_variance=True)


def _typevar_with_bound(name, evaluate_bound):
    """INTRINSIC_TYPEVAR_WITH_BOUND: `T: int`, the bound a lazy thunk."""
    tv = TypeVar(name)
    tv._evaluate_bound = evaluate_bound
    return tv


def _typevar_with_constraints(name, evaluate_constraints):
    """INTRINSIC_TYPEVAR_WITH_CONSTRAINTS: `T: (int, str)`, likewise lazy."""
    tv = TypeVar(name)
    tv._evaluate_constraints = evaluate_constraints
    return tv


def _paramspec(name):
    """INTRINSIC_PARAMSPEC: `**P`, variance inferred as _typevar's is."""
    return ParamSpec(name, infer_variance=True)


def _typevartuple(name):
    """INTRINSIC_TYPEVARTUPLE: `*Ts`."""
    return TypeVarTuple(name)


def _typealias(args):
    """INTRINSIC_TYPEALIAS: (name, type_params, evaluate_value) as a tuple.

    The compiler pushes the three as one tuple, which is what the intrinsic
    receives.
    """
    name, type_params, evaluate_value = args
    alias = TypeAliasType(name, None, type_params=type_params or ())
    alias._evaluate_value = evaluate_value
    return alias


def _subscript_generic(params):
    """INTRINSIC_SUBSCRIPT_GENERIC: `Generic[T, ...]` for a generic class."""
    if not isinstance(params, tuple):
        params = (params,)
    return _GenericAlias(Generic, params)


def _set_function_type_params(func, params):
    """INTRINSIC_SET_FUNCTION_TYPE_PARAMS: `f.__type_params__ = params`."""
    func.__type_params__ = params
    return func
