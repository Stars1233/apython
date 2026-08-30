# sys.exit() called the exit syscall directly, so no `finally` block, no
# context-manager __exit__ and no `except SystemExit` between the call and
# the top of the program ever ran.  It raises SystemExit now.
import sys


def caught(fn):
    try:
        fn()
    except SystemExit as e:
        return repr(e.code), e.args, type(e).__name__
    return "no exit"


print(caught(lambda: sys.exit(3)))
print(caught(lambda: sys.exit()))
print(caught(lambda: sys.exit(None)))
print(caught(lambda: sys.exit("usage")))
print(caught(lambda: sys.exit(0)))
print(caught(lambda: sys.exit(2 ** 40)))

# SystemExit is a BaseException, not an Exception, so a bare `except
# Exception` must let it through.
def swallow():
    try:
        sys.exit(9)
    except Exception:
        return "swallowed"


print(caught(swallow))


# Cleanup between the call and the handler runs.
order = []


class Ctx:
    def __enter__(self):
        order.append("enter")
        return self

    def __exit__(self, *a):
        order.append("exit")
        return False


def cleanup():
    try:
        with Ctx():
            sys.exit(5)
    finally:
        order.append("finally")


print(caught(cleanup), order)

# Raising it directly behaves the same way
print(caught(lambda: (_ for _ in ()).throw(SystemExit(2))))
e = SystemExit("a", "b")
print(e.code, e.args, str(e), repr(e))
print(isinstance(SystemExit(), BaseException), isinstance(SystemExit(), Exception))
