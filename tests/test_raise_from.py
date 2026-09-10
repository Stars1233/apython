# Test raise...from
#
# The operand of `raise` and the operand of `from` are BOTH normalised: either
# may be an exception instance or an exception CLASS, and a class is
# instantiated.  The class form of the exception was the one that went wrong --
# the cause was written straight into the object on the stack, which for a
# class is the class itself, at the offset its tp_getattr slot lives at.

try:
    try:
        raise ValueError("x") from TypeError("y")
    except ValueError as e:
        print("caught:", e)
except Exception as e:
    print("outer:", e)

# The exception operand is a CLASS, and the cause is None.
try:
    raise KeyError from None
except KeyError as e:
    print("class/None:", repr(e), e.__cause__, e.__suppress_context__)

# The exception operand is a class, the cause an instance.
try:
    raise ValueError from ArithmeticError("y")
except ValueError as e:
    print("class/instance:", repr(e), repr(e.__cause__), e.__suppress_context__)

# BOTH are classes: the cause is instantiated too.
try:
    raise IndexError from KeyError
except IndexError as e:
    print("class/class:", repr(e), repr(e.__cause__), type(e.__cause__).__name__)

# An instance with a class cause.
try:
    raise ValueError("v") from KeyError
except ValueError as e:
    print("instance/class:", repr(e), repr(e.__cause__))

# A user-defined exception CLASS as the cause: its metatype is not the one a
# builtin exception carries, so it reaches the normaliser by a third path.
class Custom(Exception):
    pass

try:
    raise RuntimeError from Custom
except RuntimeError as e:
    print("custom cause:", repr(e.__cause__), type(e.__cause__) is Custom)

# Neither operand may be something that is not an exception.  The EXCEPTION is
# checked first, so `raise 5 from 5` complains about the exception.
for expr in ("raise 5 from None", "raise KeyError from 5",
             "raise KeyError from object()", "raise 5 from 5"):
    try:
        exec(expr)
    except TypeError as e:
        print(expr, "->", e)

# A `from` inside a live handler still suppresses the context it replaces.
try:
    try:
        raise ArithmeticError("ctx")
    except ArithmeticError:
        raise KeyError from None
except KeyError as e:
    print("suppressed:", e.__suppress_context__, repr(e.__cause__),
          type(e.__context__).__name__)

# And the classes themselves survive being named as an operand: writing the
# cause into a class corrupted its attribute slot, so this is what broke.
print("class intact:", KeyError.__name__, ValueError("z").args,
      isinstance(KeyError(), LookupError))
print("getattr intact:", [c.__name__ for c in KeyError.__mro__],
      getattr(KeyError, "__name__"), KeyError("k").args)

print("done")
