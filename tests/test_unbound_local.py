# Reading a local before it is assigned did not raise.
#
# Our compiler chose LOAD_FAST_CHECK only when the symbol carried DEF_UNBOUND,
# which marks a name that is DELETED somewhere or bound by an `except E as e`.
# It says nothing about a name simply read before its assignment -- the
# ordinary typo -- so those read plain LOAD_FAST, which hands back whatever the
# slot holds.  An empty slot is a NULL Value with no exception set: print()
# silently skipped the argument, obj_repr called it "object has no repr",
# `x + 1` reported "'object' and 'int'", and `if x:` SEGFAULTED.
#
# The .pyc path was always right -- CPython emits LOAD_FAST_CHECK there and
# apython honours it -- so nothing that ran from a .pyc could see this, which
# is the whole of why it survived.  CPython decides with a definite-assignment
# analysis over the CFG; the sound approximation here is that a parameter is
# always bound on entry and every other local might not be.
#
# `del` on an unbound local was a silent no-op, and `del` of an undefined
# global reported the dict's KeyError rather than a NameError.


def show(label, fn):
    try:
        return "%s => %r" % (label, fn())
    except BaseException as e:
        return "%s !! %s %s" % (label, type(e).__name__, e)


# --- reading, in every shape that used to swallow it ---
def read_plain():
    print(x)
    x = 1


def read_arith():
    r = x + 1
    x = 1
    return r


def read_truth():          # this one was the segfault
    if x:
        return "yes"
    x = 1
    return "no"


def read_call():
    return len(x)
    x = 1


def read_list():
    l = [x]
    x = 1
    return l


def read_is():
    return x is None
    x = 1


def read_type():
    return type(x).__name__
    x = 1


for f in (read_plain, read_arith, read_truth, read_call, read_list, read_is,
          read_type):
    print(show(f.__name__, f))


# --- del, both halves ---
def del_unbound():
    del y
    y = 1


def del_after_del():
    z = 1
    del z
    del z


def del_then_read():
    w = 1
    del w
    return w


for f in (del_unbound, del_after_del, del_then_read):
    print(show(f.__name__, f))


# --- a parameter is always bound, and stays fast ---
def param(a, b=2, *args, **kw):
    return a, b, args, kw


print(show("param", lambda: param(1)))


def param_deleted(a):
    del a
    return a


print(show("param_deleted", lambda: param_deleted(1)))


# --- a cell variable (a local a nested scope closes over) ---
def cell():
    def inner():
        return c
    r = c
    c = 2
    return r, inner


print(show("cell", cell))


# --- a free variable, which is CPython's OTHER exception ---
def free():
    def inner():
        return v
    r = inner()
    v = 3
    return r


print(show("free", free))


# --- the ordinary cases must be untouched ---
def ordinary():
    a = 1
    b = a + 1
    for i in range(2):
        b += i
    c = [j for j in range(3)]
    with open("/dev/null") as f:
        pass
    try:
        d = 1
    except ValueError as e:
        d = e
    return a, b, c, d


print(show("ordinary", ordinary))


def closure_ok():
    n = 10

    def inner():
        return n * 2
    return inner()


print(show("closure_ok", closure_ok))


def loop_binds():
    total = 0
    for k in range(4):
        total += k
    return total, k


print(show("loop_binds", loop_binds))


# --- module-level del, which is NameError, not KeyError ---
g = 1
del g
print(show("del twice global", lambda: exec("del g")))
print(show("del never global", lambda: exec("del _never_bound_xyz_")))
print(show("read never global", lambda: exec("_never_bound_xyz_")))

# --- and an unbound local inside a comprehension and a generator ---
def comp():
    return [q for _ in range(1)] and q


print(show("comp", comp))


def gen():
    def g():
        yield m
        m = 1
    return list(g())


print(show("gen", gen))
