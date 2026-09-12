# eval's start symbol is a testlist, so a bare tuple is an expression.
#
# CPython's grammar is `eval_input: testlist NEWLINE* ENDMARKER`, and a
# testlist is comma-separated.  This compiler parsed one `test` instead, so
# eval("1, 2") was a SyntaxError -- and with it ast.literal_eval of any
# tuple, which is how dbm.dumb reads its index file back.
#
# What must NOT change: trailing junk is still an error.  eval("1 2") has to
# stay a SyntaxError, which is what the ENDMARKER check after the expression
# is for.

print(eval("1, 2"))
print(eval("1,"))
print(eval("1, 2, 3"))
print(eval("(1, 2)"))
print(eval("1"))

a, b = 10, 20
print(eval("a, b"))
print(eval("a, (b, a), b"))

# A tuple of tuples, the shape dbm.dumb stores.
print(eval("'a', (0, 7)"))

# Whitespace and newlines around it, which eval strips and skips.
print(eval("  1, 2  "))
print(eval("1, 2\n"))
print(eval("1, 2\n\n"))

# The comma binds looser than everything else in the testlist.
print(eval("1 if 0 else 2, 3"))
print(eval("[x for x in (1, 2)], 9"))
print(eval("lambda: 1, 2")[1])

# compile() in eval mode agrees.
print(eval(compile("4, 5", "<t>", "eval")))
print(type(compile("4, 5", "<t>", "eval")).__name__)

# The other two modes were never affected, but a tuple statement is the same
# construct and is worth pinning beside it.
exec(compile("q = 6, 7", "<t>", "exec"))
print(q)

# Trailing junk is still refused, and a starred element always was: eval's
# start symbol is `testlist`, where a statement's is `testlist_star_expr`.
# `(*a,)` and `[*a]` bring their own brackets and stay legal.
for src in ("1 2", "1, 2 3", "1,, 2", ", 1", "1 if 0", "x = 1",
            "*a", "*a,", "1, *a", "a, b, *c", "*[1, 2], 3"):
    try:
        eval(src, {"a": [1], "b": 2, "c": [3]})
        print("ACCEPTED", repr(src))
    except SyntaxError as e:
        print("SyntaxError", repr(src), e.msg,
              e.lineno, e.offset, e.end_lineno, e.end_offset)

print(eval("(*[1, 2],)"))
print(eval("[*[1, 2]]"))
print(eval("lambda x=1, *a: 0")(5))

# ast.literal_eval is the caller this was found through.
import ast

print(ast.literal_eval("1, 2"))
print(ast.literal_eval("'a', (0, 7)"))
print(ast.literal_eval("(1, 2)"))
print(ast.literal_eval("1"))
