# A multi-line string token belongs to the line it STARTED on.
#
# .sub_string advanced the lexer's line counter and line_start as it scanned
# the literal, and lex_emit stamps the token afterwards -- from the advanced
# counter, and with a column computed as start - line_start, which is NEGATIVE
# once line_start has moved past the token.  Stored into a u32 that is about
# 4.29e9, and comp_attach_location then zero-extended it and added one, so the
# offset a SyntaxError carried was 4294967285.
#
# The traceback's caret loop writes one space per column, one write() each, so
# rendering such an error did not merely look wrong -- it did not finish.
def loc(src):
    try:
        compile(src, "<t>", "exec")
        return "compiled"
    except SyntaxError as e:
        lineno, offset = e.args[1][1], e.args[1][2]
        sane = offset is None or 0 <= offset <= 200
        return (lineno, sane)


# An unterminated triple-quoted string: reported at its own line, with a
# plausible column.
print(loc('x = """abc\ndef\n'))

# A syntax error on a line after a multi-line literal.
print(loc('s = """a\nb"""\n1 = 2\n'))
print(loc('s = """a\nb\nc\nd"""\n1 = 2\n'))

# A backslash-newline inside a single-quoted string moves the counter too.
print(loc('s = "a\\\nb"\n1 = 2\n'))

# An error inside the literal's own statement.
print(loc('s = """a\nb""" +\n'))

# Two multi-line literals, so the counter has to be right twice.
print(loc('a = """1\n2"""\nb = """3\n4"""\n1 = 2\n'))

# And the ordinary case, unchanged.
print(loc('1 = 2\n'))
print(loc('x = 1\n'))


# The line numbers a traceback reports for statements anchored on a multi-line
# literal are the statements' own.
src = '''def f():
    s = """a
b
c"""
    raise ValueError("boom")
'''
ns = {}
exec(compile(src, "<t>", "exec"), ns)
try:
    ns["f"]()
except ValueError as e:
    tb = e.__traceback__
    while tb.tb_next:
        tb = tb.tb_next
    print("raise at line", tb.tb_lineno)

# The literal's own value survives all of this.
s = """a
b"""
print(repr(s), len(s))
