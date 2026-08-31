# A SyntaxError's location tuple is whatever the raise supplied, and the
# traceback printer read it without checking: `SyntaxError("m", (1, 2, 3, 4))`
# printed the integer 1 as if it were a filename string and segfaulted.
# (Uncaught, CPython itself falls over on that input, so only the shapes are
# compared here; the printer is exercised by the real errors below.)
for args in (
    ("m", (1, 2, 3, 4)),
    ("m", ("f", "notaline", 1, "text")),
    ("m",),
    (),
):
    try:
        raise SyntaxError(*args)
    except SyntaxError as e:
        print(repr(e.args))

# A real one carries a real location.
try:
    compile("x = (", "<t>", "exec")
except SyntaxError as e:
    print(type(e).__name__, e.args[1][0], e.args[1][1] > 0, isinstance(e.args[0], str))

try:
    compile("def f(:\n    pass\n", "<t>", "exec")
except SyntaxError as e:
    print(e.args[1][0], e.args[1][1])

try:
    raise SyntaxError("m", ("file.py", 3, 5, "some text\n"))
except SyntaxError as e:
    print(e.args[1])

# The traceback printer runs over a real one when it reaches the top level of
# an exec, which the caller sees as the exception, not as a crash.
try:
    exec("if True:\n  x = (\n")
except SyntaxError as e:
    print("exec", type(e).__name__, e.args[1][1] > 0)
