# An exception raised inside an imported module's body used to return cleanly
# into the importer with the exception still pending: the importer's own
# try/except never saw it, the half-built module stayed in sys.modules and was
# handed back to every later import of that name, and three failed imports in
# one process were enough to corrupt the heap.
#
# The helper modules live in tests/importfail/.
import sys

sys.path.insert(0, "tests/importfail")


def err(fn, *a):
    try:
        return fn(*a)
    except BaseException as e:
        return type(e).__name__, str(e)[:40]


# A module body that raises: the importer's handler must see that exception,
# not ImportError and not silence.
try:
    import raises_value
except ValueError as e:
    print("caught ValueError:", e)

# ... and it must be catchable as Exception too
print(err(__import__, "raises_value"))

# The module must not be left behind in sys.modules, so a retry re-runs the
# body and fails the same way rather than handing back an empty module.
print("cached:", "raises_value" in sys.modules)
print(err(__import__, "raises_value"))

# A module that is simply absent still raises ImportError
print(err(__import__, "no_such_module_at_all"))

# The optional-accelerator idiom the whole stdlib is built on: an ImportError
# caught inside a module body, with a second import attempted from inside the
# handler.  current_exception is still set there, so the second failure used
# to re-report the first one.
import fallback
print("fallback chose:", fallback.chosen)

# A module whose body raises ImportError from a nested import reports the
# innermost cause, not the outer name
print(err(__import__, "nested_fail"))

# Repeated failures must not accumulate damage
n = 0
for i in range(60):
    try:
        __import__("raises_value")
    except ValueError:
        n += 1
    try:
        __import__("missing_%d" % i)
    except ImportError:
        n += 1
print("failures survived:", n)

# The heap is still healthy afterwards
d = {}
for i in range(2000):
    d[i] = [i] * 8
print("heap ok:", len(d), sum(len(v) for v in d.values()))

# A class body that raises propagates the same way
def make_bad_class():
    class C:
        raise KeyError("in class body")


print(err(make_bad_class))

# and a good one still works
class Good:
    v = 7


print(Good.v)
