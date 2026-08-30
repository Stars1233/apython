# Implicit exception chaining: an exception raised while another is being
# handled records that one as its __context__.  apython set __context__ on
# nothing, so every one of these printed None.
def show(e):
    c = e.__context__
    return type(e).__name__, (type(c).__name__ if c is not None else None)

try:
    try:
        raise ValueError("a")
    except ValueError:
        raise TypeError("b")
except TypeError as e:
    print(show(e))

# A completed except block leaves nothing behind to chain onto.
try:
    raise ValueError("x")
except ValueError:
    pass
try:
    raise TypeError("y")
except TypeError as e:
    print(show(e))

# Three deep
try:
    try:
        try:
            raise KeyError("k")
        except KeyError:
            raise IndexError("i")
    except IndexError:
        raise ZeroDivisionError("z")
except ZeroDivisionError as e:
    print(show(e), show(e.__context__))

# Explicit `from` sets __cause__ and still sets __context__
try:
    try:
        raise ValueError("a")
    except ValueError as v:
        raise TypeError("b") from v
except TypeError as e:
    print(show(e), type(e.__cause__).__name__)

# Re-raising the same object must not make it its own context
try:
    try:
        raise ValueError("same")
    except ValueError as v:
        raise v
except ValueError as e:
    print(show(e))

# An exception raised outside any handler has no context
try:
    raise RuntimeError("bare")
except RuntimeError as e:
    print(show(e))

# Errors from the runtime chain too
try:
    try:
        raise ValueError("a")
    except ValueError:
        {}["missing"]
except KeyError as e:
    print(show(e))
