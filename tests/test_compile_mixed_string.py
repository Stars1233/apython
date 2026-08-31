# A bytes literal adjacent to an f-string is a SyntaxError, and reporting it
# must not free a Buf that was never initialised: the error path fell through
# into the shared cleanup, so `b"a" f"b"` corrupted the heap instead of
# reporting anything.
def try_compile(src):
    try:
        compile(src, "<t>", "exec")
    except SyntaxError:
        return "SyntaxError"
    return "ok"


print(try_compile('x = b"a" f"b"'))
print(try_compile('x = f"a" b"b"'))
print(try_compile('x = b"a" "b"'))
print(try_compile('x = "a" b"b"'))
print(try_compile('x = b"a" b"b"'))
print(try_compile('x = "a" f"b"'))
print(try_compile('x = f"a" "b"'))
print(try_compile('x = f"a" f"b"'))

# The valid adjacent-literal forms really do concatenate.
v = 1
print("a" "b", b"a" b"b", f"a{v}" f"b{v}", "a" f"b{v}", f"a{v}" "b")

# Several in a row, and across lines.
print(
    "a"
    f"{v}"
    "c"
)

# Reporting the error repeatedly must stay stable.
for i in range(4):
    print(try_compile('y = b"x" f"y"'), end=" ")
print()
