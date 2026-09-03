# The caret line under a traceback frame.
#
# CPython underlines the part of the line the failing instruction came from,
# using the column fields of the location table, and for a binary operation or
# a subscript it narrows the `^` to the operator itself and draws `~` under
# the operands.  Everything else in the report matched already; this line was
# simply missing.
#
# The whole report goes to stderr, which the harness compares along with
# stdout, so the shapes are chained together into one uncaught exception: the
# two `except` blocks each leave a context behind, so three full tracebacks
# are rendered and every frame below exercises a different shape.
#
# This one uses a recorded transcript rather than a diff against python3, and
# for a reason that has nothing to do with the carets: CPython absolutizes the
# path of a script it runs directly, so its report says
# "/home/.../tests/test_traceback_carets.py" where a run from the .pyc says
# "tests/test_traceback_carets.py".  Every other line of the three reports,
# caret rows included, was compared against CPython 3.12 byte for byte before
# the transcript was recorded.
#
# Note that a frame whose location covers the whole stripped line gets no
# caret row at all -- underlining a line with itself says nothing -- which is
# why the plain `helper()` calls below are bare.


def divide(a, b):
    return a / b


def floordiv(a, b):
    return a // b


def power(a, b):
    return a ** b ** 2


def bitor(a, b):
    return a | b


def shift(a, b):
    return a << b


def parenthesised(a, b):
    return (a + 1) + b


def precedence(a, b):
    return a + 1 * b


def subscript(xs, i):
    return xs[i]


def double_subscript(xs, i):
    return xs[0][i]


def attribute(x):
    return x.no_such_attribute


def call_it(f):
    return f(1, 0)


def compare(a, b):
    return a < b


def unary(a):
    return -a


def multiline(a):
    return (a
            + 1)


def nested():
    return call_it(divide)


def attempt(fn, *args):
    try:
        fn(*args)
    except BaseException as e:
        return type(e).__name__
    return "no error"


print(attempt(divide, 1, 0))
print(attempt(floordiv, 1, 0))
print(attempt(power, None, 2))
print(attempt(bitor, None, 2))
print(attempt(shift, None, 2))
print(attempt(parenthesised, None, 2))
print(attempt(precedence, None, 2))
print(attempt(subscript, [1, 2, 3], 9))
print(attempt(double_subscript, [[1]], 9))
print(attempt(attribute, object()))
print(attempt(compare, 1, "x"))
print(attempt(unary, None))
print(attempt(multiline, None))
print(attempt(nested))

# And now the same shapes uncaught, chained, so the rendered report is what
# gets compared.
try:
    subscript([1, 2, 3], 9)
except IndexError as first:
    try:
        nested()
    except ZeroDivisionError as second:
        multiline(None)
