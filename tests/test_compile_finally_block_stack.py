# The duplicated finally body writes over the caller's block stack.
#
# cg_unwind_finallys truncates the block stack while it emits a copy of a
# finally body, so a `with` or a nested try inside that body pushes at the
# indices the caller still owns -- and the restore puts the LENGTH back without
# putting the ENTRIES back.  The next unwind then read a with-mark where a
# finally node had been, and called the return value as if it were __exit__.
#
# CPython keeps its one fblock on the C stack and recurses, so the entries
# above it are safe by construction; this is a loop, so it has to keep the
# whole slice itself.
log = []


class CM:
    def __init__(self, tag):
        self.tag = tag

    def __enter__(self):
        log.append("enter " + self.tag)
        return self

    def __exit__(self, *a):
        log.append("exit " + self.tag)
        return False


def f1():
    try:
        raise KeyError(8)
        return 10
    except KeyError:
        return 18
    finally:
        with CM("a"):
            log.append("body a")


log.clear()
print(f1(), log)


# The clobbering block can be a nested try/finally rather than a with.
def f2():
    try:
        return 1
    finally:
        try:
            log.append("inner try")
        finally:
            log.append("inner finally")


log.clear()
print(f2(), log)


# ...or an except clause, which registers its own handler node.
def f3():
    try:
        raise ValueError("v")
        return 2
    except ValueError:
        return 3
    finally:
        try:
            raise TypeError("t")
        except TypeError as e:
            log.append("swallowed " + str(e))


log.clear()
print(f3(), log)


# Two returns out of the same try, so the second unwind reads what the first
# one left behind.
def f4(which):
    try:
        if which:
            return "a"
        return "b"
    finally:
        with CM("b"):
            pass


log.clear()
print(f4(True), f4(False), log)


# A with inside a finally, inside a loop, left by a break.
def f5():
    out = []
    for i in range(3):
        try:
            if i == 1:
                break
            out.append(i)
        finally:
            with CM("c"):
                pass
    return out


log.clear()
print(f5(), log)


# Three levels: the innermost with clobbers two entries at once.
def f6():
    try:
        try:
            return "inner"
        finally:
            with CM("d"):
                with CM("e"):
                    pass
    finally:
        log.append("outer finally")


log.clear()
print(f6(), log)
