# Code objects are visible to the collector.
#
# A code object holds nine owned objects, and one of them -- co_consts -- can
# hold another code object.  With tp_flags 0 and a NULL tp_traverse the
# collector could not see through any of them, so the ordinary shape of a
# closure or a class body -- a function whose code is in a module's code, and
# whose __globals__ reaches the function again -- was a cycle it could not
# break.
#
# co_consts is also all that tp_clear touches.  The rest are strings and
# bytes that hold nothing, and the eval loop reads co_names and the two
# localsplus tables through r14 and the frame: clearing those from under a
# frame that is still unwinding would be worse than the cycle.

import gc


def cycle_through_a_closure():
    box = []
    def f():
        return box          # the cell holds box, box holds f
    box.append(f)


gc.collect()
cycle_through_a_closure()
print("function through a cell:", gc.collect() > 0)


def cycle_through_a_class():
    class C:
        def m(self):
            return C
    C.self = C


gc.collect()
cycle_through_a_class()
print("class through a method :", gc.collect() > 0)


def cycle_through_a_default():
    box = []
    def f(held=box):
        return held
    box.append(f)           # f.__defaults__ holds box, box holds f


gc.collect()
cycle_through_a_default()
print("function through a default:", gc.collect() > 0)

print("=== and everything that makes code still works ===")
print("eval   :", eval("1 + 2"))
ns = {}
exec("def g(a): return a * 2", ns)
print("exec   :", ns["g"](21))
c = compile("x = 5", "<s>", "exec")
print("compile:", type(c).__name__, c.co_filename)
ns2 = {}
exec(c, ns2)
print("run    :", ns2["x"])


def has_nested():
    def a():
        def b():
            return "deep"
        return b
    return a()()


print("nested :", has_nested())
print("consts :", type(has_nested.__code__.co_consts).__name__)


# A generator's frame holds its code while suspended; dropping it mid-flight
# must not disturb either.
def gen():
    yield 1
    yield 2


g = gen()
next(g)
del g
gc.collect()
print("suspended generator dropped: ok")
