# Leaving a block early has to emit its cleanup OUTSIDE that block's own
# protected region -- that is what leaving it means.  The __exit__ call a
# `return` emitted sat inside the with's own handler, so an exception from
# __exit__ re-entered PUSH_EXC_INFO with a stack the region's recorded depth
# did not describe, and the interpreter segfaulted.
#
# A `return` also leaves every enclosing loop, whose iterator is on the stack
# under the return value.  Nothing popped it, so `with: for: return v` called
# the iterator as if it were __exit__.
log = []


class CM:
    def __init__(self, tag, raise_on_exit=False):
        self.tag = tag
        self.raise_on_exit = raise_on_exit

    def __enter__(self):
        log.append("enter " + self.tag)
        return self

    def __exit__(self, *a):
        log.append("exit " + self.tag)
        if self.raise_on_exit:
            raise ValueError("from exit " + self.tag)
        return False


def f1():
    with CM("a", raise_on_exit=True):
        return 7


try:
    f1()
except ValueError as e:
    print("caught", e)
print(log)


def f2():
    with CM("b"):
        for v in [1, 2, 3]:
            return v


log.clear()
print(f2(), log)


def f3():
    with CM("c"):
        for v in [1, 2]:
            for w in [3, 4]:
                return (v, w)


log.clear()
print(f3(), log)


def f4():
    for v in [1, 2]:
        with CM("d"):
            for w in [3, 4]:
                return (v, w)


log.clear()
print(f4(), log)


def f5():
    with CM("e"), CM("f"):
        for v in [1]:
            return v


log.clear()
print(f5(), log)


# break and continue still pop only their own loop's items.
def f6():
    out = []
    for v in [1, 2, 3]:
        with CM("g"):
            if v == 2:
                break
            out.append(v)
    return out


log.clear()
print(f6(), log)


def f7():
    out = []
    for v in [1, 2, 3]:
        with CM("h"):
            if v == 2:
                continue
            out.append(v)
    return out


log.clear()
print(f7(), len(log))


# try/finally on the way out of a loop, and a return inside a finally.
def f8():
    try:
        for v in [1, 2]:
            return v
    finally:
        log.append("fin")


log.clear()
print(f8(), log)


def f9():
    for v in [1, 2]:
        try:
            return v
        finally:
            log.append("fin9")


log.clear()
print(f9(), log)


# An except clause left by a return still pops the exception state.
def f10():
    try:
        raise ValueError("x")
    except ValueError:
        for v in [1, 2]:
            return v


log.clear()
print(f10())

# The normal exits are unchanged.
log.clear()
with CM("i"):
    pass
for v in [1, 2]:
    with CM("j"):
        pass
print(log)
