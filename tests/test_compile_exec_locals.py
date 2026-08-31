# eval() and exec() inside a function see the function's locals.
#
# A function frame keeps its locals in the localsplus array rather than in a
# mapping, so PyFrame.locals is NULL for one -- and everything that wanted a
# mapping substituted globals instead.  eval("lv + 1") raised NameError for a
# name sitting two words away, exec("out = ...") wrote to the module, and
# locals() listed the module's names instead of the function's.
#
# The fix is CPython's PyFrame_FastToLocalsWithError: walk co_localsplusnames
# against the slots, unwrap the cells, skip the unbound.  It is a snapshot, and
# writing to it does not write back -- which is also what CPython does outside
# a tracing hook.
def f():
    lv = 41
    return eval("lv + 1")


print(f())


def g():
    out = 1
    exec("out = 2")
    return out


print(g(), "out" in globals())


def h():
    a = 1
    b = 2
    return sorted(locals().items())


print(h())


# A closure's cells and free variables are unwrapped, not listed as cells.
def outer():
    captured = "cap"

    def inner():
        local = "loc"
        # captured has to be named here for the compiler to make it a free
        # variable at all; eval cannot conjure one that the code object has no
        # slot for, and CPython behaves the same way.
        _ = captured
        return sorted(locals().items()), eval("captured + '/' + local")

    return inner()


print(outer())


# An unbound local is absent rather than present-and-empty.
def unbound(flag):
    if flag:
        maybe = 1
    return sorted(locals().keys())


print(unbound(True), unbound(False))


# Explicit namespaces still win over the frame's.
def explicit():
    lv = 1
    return eval("2 + 3", {}), eval("lv * 10")


print(explicit())


# Module scope is unchanged: there the frame really does have a mapping.
mv = 7
print(eval("mv + 1"))
exec("mw = mv * 2")
print(mw)


# A dict subclass is a valid globals mapping, as PyDict_Check allows.
class NS(dict):
    pass


ns = NS()
ns["__builtins__"] = __builtins__
exec("z = 1 + 1", ns)
print(ns["z"], type(ns).__name__, eval("z * 3", ns))
