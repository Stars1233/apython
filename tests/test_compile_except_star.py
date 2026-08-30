# `except*`, compiled from source.
#
# It is a different statement from `except`, not a variant of one clause.
# Where `except` picks the first clause that matches and stops, `except*` runs
# every clause that matches part of an exception group, each on its own
# subgroup, and re-raises whatever no clause claimed.
#
# So the stack carries three things across the whole chain rather than one
# exception: the original group, a list of everything the clause bodies
# themselves raised, and the part not yet matched.  A body that raises does not
# stop the chain -- what it raised joins the list and the next clause still
# sees the remainder -- which is what the inner protected region round each
# body is for.
#
# Nothing may leave a clause early.  The unwinding would have to reconstruct a
# partly-matched group, and Python does not define what that means, so CPython
# rejects `break`, `continue` and `return` there outright; so do we.
SRC = '''
def run(fn):
    try:
        try:
            fn()
        except* ValueError as e:
            print(" VE", sorted(str(x) for x in e.exceptions))
            raise KeyError("from handler")
        except* (TypeError, KeyError) as e:
            print(" TK", sorted(str(x) for x in e.exceptions))
        else:
            print(" else")
        finally:
            print(" finally")
    except BaseException as e:
        print(" escaped", type(e).__name__)


def g(exceptions):
    def go():
        raise ExceptionGroup("g", exceptions)
    return go


print("none");      run(lambda: None)
print("bare");      run(g([ValueError("v")]))
print("both");      run(g([ValueError("a"), TypeError("b")]))
print("unmatched"); run(g([OSError("o")]))


def unnamed():
    out = "unhandled"
    try:
        raise ExceptionGroup("g", [ValueError("x")])
    except* ValueError:
        out = "no name needed"
    return out


print(unnamed())
'''
ns = {}
exec(compile(SRC, "<t>", "exec"), ns)

# break, continue and return cannot leave an except* clause.
BAD = [
    "for i in (1,):\n    try:\n        f()\n    except* E:\n        break\n",
    "for i in (1,):\n    try:\n        f()\n    except* E:\n        continue\n",
    "def f():\n    try:\n        g()\n    except* E:\n        return 1\n",
]
for src in BAD:
    try:
        compile(src, "<t>", "exec")
        print("no error for", repr(src[-20:]))
    except SyntaxError:
        print("SyntaxError, as CPython does")
