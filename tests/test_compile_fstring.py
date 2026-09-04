# The source compiler, phase 8: f-strings.
#
# Rather than teach the tokenizer PEP 701's FSTRING_START/MIDDLE/END, each
# replacement field's source span is tokenized on its own and appended to the
# token array; the parser's cursor is pointed at those tokens and the ordinary
# expression parser handles the rest.  Having the whole token array is what
# makes that cheap -- there is nowhere else to put them, and nothing to
# restore afterwards but an index.

CASES = [
    "f''", "f'abc'", "f'{1}'", "f'{1+2}'", "f'a{1}b'", "f'{1}{2}'",
    "f'{{literal}}'", "f'{{{1}}}'",
    "f'{x}'", "f'{x!r}'", "f'{x!s}'", "f'{x!a}'",
    "f'{x:>8}'", "f'{x:<8}'", "f'{n:5d}'", "f'{n:.3f}'",
    "f'{n:{w}}'", "f'{n:{w}.{p}f}'",
    "f'{d[\"k\"]}'", "f'{t[0]}'", "f'{o.a}'",
    "f'{ x }'", "f'{(x, n)}'", "f'{[i for i in range(3)]}'",
    "f'x' f'y'", "'a' f'{x}' 'b'",
    "f'{x if n else s}'", "f'{len(s)}'", "f'{x=}'", "f'{n=}'", "f'{n+1=}'",
    "f'{n!=1}'", "f'{n==1}'",
]
ns = {'x': 'X', 'n': 42, 's': 'hello', 'w': 6, 'p': 2, 'd': {'k': 'v'}, 't': (7,), 'o': type('O',(),{'a':'A'})()}
for c in CASES:
    print(c, '=>', repr(eval(c, dict(ns))))

# A format spec is itself an f-string, so it can nest.
print(eval("f'{n:{w}}'", dict(ns)))
print(eval("f'{n:0{w}d}'", dict(ns)))

# A lone brace is an error; a doubled one is a literal.
for bad in ["f'}'", "f'{'", "f'{x!q}'", "f'{'"]:
    try:
        eval(bad, dict(ns))
        print("no error for", bad)
    except SyntaxError:
        print("SyntaxError for", bad)

# A replacement field's tokens are lexed as a span of the same source, and the
# span's starting line has to be handed to the lexer -- an exception raised
# inside a field used to point at line 1 of the file.
print("=== a field's own line ===")
SRC = "def g():\n    return 0\n\n\nx = f'{1 / g()}'\n"
try:
    exec(compile(SRC, "<lines>", "exec"), {})
except ZeroDivisionError as e:
    tb = e.__traceback__
    seen = []
    while tb is not None:
        seen.append(tb.tb_lineno)
        tb = tb.tb_next
    print("innermost line", seen[-1])

SPEC = "w = 0\n\n\n\n\ny = f'{1:{2 / w}}'\n"
try:
    exec(compile(SPEC, "<lines>", "exec"), {})
except ZeroDivisionError as e:
    tb = e.__traceback__
    while tb.tb_next is not None:
        tb = tb.tb_next
    print("nested spec line", tb.tb_lineno)
