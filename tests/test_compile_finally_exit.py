# return, break and continue that leave a finally clause.
#
# A finally body is emitted twice: once on the normal path and once on the
# exceptional one.  The exceptional copy runs with the two words PUSH_EXC_INFO
# left on the stack -- the previous exception state, then the exception -- and
# nothing on the block stack said so, so a `return` inside the clause returned
# with the exception still "being handled", reported as uncaught at exit.
#
# The normal copy runs with a pending return value underneath it, and nothing
# said that either: a `continue` inside the clause left the value on the stack,
# so the loop's back edge rejoined one word higher every iteration.  That is
# not merely a leak -- it is a stack-depth disagreement at FOR_ITER, which the
# assembler's depth pass answered by taking the larger and walking the body
# again, forever.  The compiler did not terminate.
#
# CPython records both as fblocks, FINALLY_END and POP_VALUE, and unwinds them
# the same way this does.
log = []


def ctx():
    # If an exception is still "being handled", the next raise chains onto it
    # through __context__.  That is the observable half of the bug: a `return`
    # out of a finally clause left prev_exc and exc on the stack and never ran
    # POP_EXCEPT, so the ValueError stayed current long past the return.
    try:
        raise KeyError("probe")
    except KeyError as e:
        return type(e.__context__).__name__


# --- return inside finally, exception path ---
def f1():
    try:
        raise ValueError(2)
    finally:
        return 30


print(f1(), ctx())


def f2():
    try:
        return 1
    finally:
        return 2


print(f2())


def f3():
    try:
        raise ValueError("swallowed")
    finally:
        return "from finally"


print(f3(), ctx())


# --- continue and break inside finally, inside a loop ---
def f4():
    out = []
    for i in range(3):
        try:
            return 1
        finally:
            out.append(i)
            continue
    return out


print(f4())


def f5():
    for i in range(3):
        try:
            return 1
        finally:
            break
    return 7


print(f5())


def f6():
    out = []
    for i in range(3):
        try:
            raise ValueError(i)
        finally:
            out.append(i)
            continue
    return out


print(f6(), ctx())


def f7():
    for i in range(3):
        try:
            raise ValueError(i)
        finally:
            break
    return 7


print(f7(), ctx())


# The discarded value can be a call, not a constant.
def side(n):
    log.append("side %d" % n)
    return n


def f8():
    out = []
    for i in range(3):
        try:
            return side(i)
        finally:
            out.append(i)
            continue
    return out


log.clear()
print(f8(), log)


# One more nesting level.
def f9():
    out = []
    for i in range(2):
        for j in range(2):
            try:
                return (i, j)
            finally:
                out.append((i, j))
                continue
    return out


print(f9())


# --- leaving a finally that is nested in with / try / except ---
class CM:
    def __init__(self, tag):
        self.tag = tag

    def __enter__(self):
        log.append("enter " + self.tag)
        return self

    def __exit__(self, *a):
        log.append("exit " + self.tag)
        return False


def f10():
    with CM("a"):
        try:
            raise ValueError("x")
        finally:
            return "ret"


log.clear()
print(f10(), log, ctx())


def f11():
    try:
        try:
            raise ValueError("x")
        finally:
            return "inner"
    finally:
        log.append("outer finally")


log.clear()
print(f11(), log, ctx())


def f12():
    try:
        raise ValueError("x")
    except ValueError:
        try:
            raise TypeError("y")
        finally:
            return "from nested finally"


print(f12(), ctx())


def f13():
    try:
        return 1
    finally:
        with CM("b"):
            return 2


log.clear()
print(f13(), log)


# A loop written INSIDE the finally clause is left by its own break.
def f14():
    out = []
    try:
        return 1
    finally:
        for i in range(5):
            if i == 3:
                break
            out.append(i)
        log.append(str(out))


log.clear()
print(f14(), log)


# --- the ordinary exits are unchanged ---
def f15():
    out = []
    try:
        out.append("body")
    finally:
        out.append("finally")
    return out


print(f15())
