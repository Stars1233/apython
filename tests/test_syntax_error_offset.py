# A SyntaxError whose offset is None.
#
# tb_syntax_header decoded args[1][2] as an integer without checking it, the
# way it checks the filename, the line number and the text.  None is a heap
# pointer, so subtracting the integer bias from it gave about 2^50, and the
# caret loop writes one space per column with a write() each: rendering such an
# error did not finish.
#
# It is legal Python -- it is what CPython itself produces when the column is
# unknown -- and the offset is whatever the raiser put there, so the caret is
# also clamped to the length of the source line.
def show(args):
    try:
        raise SyntaxError(*args)
    except SyntaxError as e:
        print(e.args)


show(("m", ("f.py", 1, None, "  hello\n")))
show(("m", ("f.py", 1, 3, "  hello\n")))
show(("m", ("f.py", 1, 10 ** 9, "  hello\n")))
show(("m", ("f.py", 1, -5, "  hello\n")))
show(("m", ("f.py", 1, 1.5, "  hello\n")))
show(("m", ("f.py", 1, None, None)))
show(("m", ("f.py", None, None, "x\n")))

# The rendering path itself -- tb_syntax_header, reached only when such an
# error goes uncaught -- cannot be diffed here: `make check` runs CPython on
# the .py and apython on the .pyc, so the traceback's own File line differs by
# construction.  Verified by hand instead, and byte-identical to CPython:
#
#   $ ./apython -  <<< 'raise SyntaxError("m", ("f.py", 1, None, "  hello\n"))'
#     File "f.py", line 1
#       hello
#   SyntaxError: m
#
# Before the fix that call did not terminate.
