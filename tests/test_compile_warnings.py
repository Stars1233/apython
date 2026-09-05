# The compiler's warning channel.
#
# CPython warns while compiling -- a number that ends against a keyword, an
# `is` against a literal -- and this compiler could not: it runs before there
# is an interpreter frame, which is the same reason it may not raise.  The
# warnings are recorded and handed to warnings.warn_explicit afterwards, which
# takes a filename and a line rather than reading them off a frame.
import warnings

TOKENIZER = [
    ("1if True else 2", "eval"),
    ("1and 2", "eval"),
    ("[1for i in [1]]", "eval"),
    ("0x1if True else 2", "eval"),
    ("0b1and 2", "eval"),
    ("0o1and 2", "eval"),
    ("1or 2", "eval"),
    ("1in [1]", "eval"),
    ("1not in [1]", "eval"),
    ("x = 1\ny = 1and 2\n", "exec"),
    ("1 + 2", "eval"),
]

IS_LITERAL = [
    "1 is 2", "1 is not 2", '"a" is "b"', "x is 1", "1 is x", "() is x",
    "(1,) is x", "(x,) is x", "1.5 is x", 'b"a" is x', "None is x",
    "True is x", "[1] is x", "x is None", "x is y", "1 is x is 2",
    "x is 1 is y", "1 == 1", "x is not None", '"" is x', "x is not 1",
]

print("=== the tokenizer ===")
for src, mode in TOKENIZER:
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        try:
            compile(src, "<s>", mode)
        except SyntaxError as exc:
            print(repr(src), "-> SyntaxError", exc)
            continue
        print(repr(src), "->", [(w.category.__name__, str(w.message), w.lineno)
                                for w in caught])

print("=== is with a literal ===")
for src in IS_LITERAL:
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        try:
            compile("x=1\ny=2\n" + src, "<s>", "exec")
        except SyntaxError as exc:
            print(repr(src), "-> SyntaxError", exc)
            continue
        print(repr(src), "->", [str(w.message) for w in caught])

print("=== a filter that ignores, and one that raises ===")
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    compile("1and 2", "<s>", "eval")
    print("ignored")
with warnings.catch_warnings():
    warnings.simplefilter("error")
    try:
        compile("1and 2", "<s>", "eval")
        print("error filter: NOT RAISED")
    except SyntaxError as exc:
        # CPython replaces the SyntaxWarning a filter raised with a
        # SyntaxError carrying the same text, so what escapes compile() is a
        # compile error and not a warning class.
        print("error filter:", type(exc).__name__, exc.msg, exc.lineno,
              exc.offset)

print("=== and the code still runs ===")
print(eval(compile("1if True else 2", "<s>", "eval")))
