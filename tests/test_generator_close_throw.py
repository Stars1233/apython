# close() freed the generator's frame outright, so a finally block or a
# context manager inside one never ran.  throw() always called exc_new on its
# argument, so g.throw(ValueError("x")) built an exception whose *type* was
# the instance -- `except ValueError` inside the generator never matched, and
# every throw came back out as a re-raise.  Raising StopIteration over the
# top of the still-pending exception then freed it twice.
#
# close() is now what CPython does: throw GeneratorExit in, swallow it (and
# StopIteration) coming back out, and report a generator that yields instead
# of finishing.

def t(f):
    try: return repr(f())
    except Exception as e: return type(e).__name__

def g1():
    try:
        yield 1
        yield 2
    finally:
        print("cleanup")
a = g1(); print(next(a)); a.close(); print("closed")
a.close()  # idempotent

def g2():
    try:
        yield 1
    except ValueError:
        yield 99
b = g2(); next(b); print(b.throw(ValueError("x")))

def g3():
    yield 1
c = g3(); next(c)
print(t(lambda: c.throw(ValueError("x"))))

def g4():
    try:
        yield 1
    except GeneratorExit:
        print("saw exit")
        raise
d = g4(); next(d); d.close(); print("closed2")

def g5():
    with open("/dev/null") as f:
        yield 1
e = g5(); next(e); e.close(); print("closed3")

def g6():
    try:
        yield 1
    finally:
        yield 2
f6 = g6(); next(f6)
print(t(lambda: f6.close()))
print("done")
