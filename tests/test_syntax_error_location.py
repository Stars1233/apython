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
# CPython's tuple has two more fields (end_lineno, end_offset); only the first
# four are compared.
#
# str() of a caught SyntaxError is the bare message here, where CPython appends
# " (filename, line N)".  The traceback shows the same information in its own
# block, so that is the one place the two differ, and it is not checked here.

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
