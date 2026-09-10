# sys.call_tracing checks its second argument before it dereferences it.
#
# The argument is a VALUE and an int is an immediate, so
# `sys.call_tracing(type, 2)` -- which is the whole of CPython's own test for
# this function -- read ob_type off the number 2.  The refusals are CPython's
# two sentences: a wrong count is an arity error and says how many it got, a
# wrong type is an argument error and names the type.

import sys
def f(a, b): return a + b
print("ok:", sys.call_tracing(f, (1, 2)))
print("empty:", sys.call_tracing(len, ("abc",)))
for bad in ("sys.call_tracing(type, 2)", "sys.call_tracing(f, [1,2])",
            "sys.call_tracing(f)", "sys.call_tracing(f, (1,2), 3)",
            "sys.call_tracing(f, None)", "sys.call_tracing(f, 'ab')"):
    try:
        eval(bad); print(bad, "-> ok")
    except TypeError as e:
        print(bad, "->", e)
def tracer(frame, event, arg):
    return tracer
sys.settrace(tracer)
r = sys.call_tracing(f, (3, 4))
sys.settrace(None)
print("under tracing:", r)
print("done")
