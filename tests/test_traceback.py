# A raise now records every frame it passes through, so __traceback__ is a
# real chain instead of one empty entry with line 0.  The rendered report is
# on stderr; what is checkable here is the chain itself.


def raiser():
    raise ValueError("boom")


def middle():
    raiser()


def outer():
    middle()


def chain(fn):
    try:
        fn()
    except BaseException as e:
        out = []
        tb = e.__traceback__
        while tb is not None:
            out.append(tb.tb_lineno)
            tb = tb.tb_next
        return out


print(chain(outer))
print(chain(raiser))
print(chain(lambda: 1 / 0))
print(chain(lambda: {}["k"]))
print(chain(lambda: [][3]))


# A caught-and-recovered exception in the middle does not add its frames to
# the next one.
def swallow():
    try:
        raiser()
    except ValueError:
        pass
    raise TypeError("second")


print(chain(swallow))


# Deep recursion keeps a chain rather than blowing up while freeing it.
def deep(n):
    if n:
        return deep(n - 1)
    raise IndexError("bottom")


print(len(chain(lambda: deep(200))))

# A generator's frames appear too.
def gen():
    yield 1
    raise KeyError("g")


def drain():
    for _ in gen():
        pass


print(chain(drain))
