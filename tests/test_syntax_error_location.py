# A SyntaxError says where it happened.
#
# The compiler records the position of the first thing that went wrong, and the
# exception carries it in CPython's shape: args = (msg, (filename, lineno,
# offset, text, ...)).  The traceback printer reads that tuple to produce the
# File/line/source/caret block; without it a syntax error was a bare message
# with nothing to locate it by.
#
# The wording of the messages is our own and the exact column is not always
# CPython's, so this checks the structure and the line rather than the text.
# The tuple has CPython's six fields; the end of the span is the one character
# the error points at, where CPython sometimes covers a whole token, so the
# last two are checked for shape and not for value.

# (source, the line the error is on)
BAD = [
    ("a = 1\nb = = 2\n", 2),
    ("def f():\n    if x\n        pass\n", 2),
    ("for i in (1,)\n    pass\n", 1),
    ("class C\n    pass\n", 1),
    ("x = 1\ny = (2\n", None),          # unclosed: the line reported differs
]

for src, want_line in BAD:
    caught = None
    try:
        compile(src, "<t>", "exec")
    except SyntaxError as e:
        # The name an except clause binds is deleted when the clause ends.
        caught = e.args
    if caught is None:
        print("FAIL: no error for", repr(src))
        continue
    assert len(caught) == 2, caught
    msg, loc = caught
    assert isinstance(msg, str) and msg, repr(msg)
    assert isinstance(loc, tuple) and len(loc) >= 4, repr(loc)
    filename, lineno, offset, text = loc[:4]
    assert filename == "<t>", filename
    assert isinstance(lineno, int) and lineno >= 1, lineno
    assert isinstance(offset, int) and offset >= 1, offset
    if want_line is not None:
        assert lineno == want_line, (lineno, want_line)
        assert text is not None, "no source text for a line that exists"
        assert text == src.splitlines(True)[lineno - 1], repr(text)
    print("located, line", lineno if want_line is not None else "?")

# A well-formed program still compiles, and eval mode reports its own errors.
compile("a = 1\nb = a + 1\n", "<t>", "exec")
args = None
try:
    eval("1 +")
except SyntaxError as e:
    args = e.args
assert args is not None and len(args) == 2, args
assert args[1][0] == "<string>", args
print("eval mode located too")
print("good source still compiles")

# The attributes.  These were all AttributeErrors: the location was in the args
# tuple and nothing read it back out, so every tool that reports a syntax error
# -- all of which read at least .lineno -- got nothing.
try:
    compile("a = 1\nb = = 2\n", "attrs.py", "exec")
except SyntaxError as e:
    print("msg", repr(e.msg))
    print("filename", repr(e.filename))
    print("lineno", e.lineno, "offset >= 1", e.offset >= 1)
    print("text", repr(e.text))
    print("end_lineno", e.end_lineno, "end_offset > offset", e.end_offset > e.offset)
    print("str ends with the location:", str(e).endswith("(attrs.py, line 2)"))
    print("args are the same two things", len(e.args) == 2, len(e.args[1]) == 6)

# An exception built by hand answers from its own args, as CPython's does, and
# an explicit assignment wins over them.
e = SyntaxError("boom", ("f.py", 7, 3, "x = = 1\n", 7, 4))
print(e.msg, e.filename, e.lineno, e.offset, repr(e.text), e.end_lineno, e.end_offset)
print(str(e))
e.lineno = 99
print(e.lineno, e.filename)

# With no location at all they are None, not AttributeErrors.
e = SyntaxError("bare")
print(e.msg, e.filename, e.lineno, e.offset, e.text, e.end_lineno, e.end_offset)
print(str(e))
print(str(SyntaxError()))

# str() drops the half it does not have, and shows the basename of a path.
print(str(SyntaxError("m", (None, 4, 1, "t"))))
print(str(SyntaxError("m", ("/tmp/deep/dir/mod.py", None, 1, "t"))))
print(str(SyntaxError("m", ("/tmp/deep/dir/mod.py", 41, 1, "t"))))

# A subclass keeps all of it.
try:
    raise IndentationError("bad indent", ("i.py", 2, 1, "  x\n", 2, 2))
except IndentationError as e:
    print(type(e).__name__, e.msg, e.lineno, e.offset, str(e))
