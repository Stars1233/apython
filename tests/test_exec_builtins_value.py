# exec()/eval() accept whatever the caller put under "__builtins__" and fall
# back to the interpreter's own when it is not a dict.  The check read ob_type
# straight off it, and a NaN-boxed int or float is a non-zero Value, so the
# number was dereferenced as an object.
for junk in (1, 0, -7, 1.5, "s", (), None, True):
    ns = {"__builtins__": junk}
    exec("x = 2 + 2", ns)
    print(type(junk).__name__, ns["x"])

# A real dict under __builtins__ is honoured.
ns = {"__builtins__": {"len": len}}
exec("n = len([1, 2, 3])", ns)
print(ns["n"])

# And the same through eval.
print(eval("3 * 3", {"__builtins__": 5}))
print(eval("abs(-4)", {"__builtins__": {"abs": abs}}))

# A missing __builtins__ still gets the default injected.
ns = {}
exec("y = len('abcd')", ns)
print(ns["y"], "__builtins__" in ns)
