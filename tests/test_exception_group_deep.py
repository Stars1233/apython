# eg_new took its return value out of rcx across the call that can collect.
#
# The group is built into rcx and tracked last, once every field the traverse
# reads is set -- and gc_track is exactly where a collection runs.  rcx is
# caller-saved, so the one allocation in N that triggered a collection came
# back with it clobbered and eg_new returned that as the new group.  Rubbish
# most of the time, and NULL often enough that eg_type_call's next line,
# `[rax + exc_args]`, faulted at address 0x38.
#
# Nesting groups is what makes it show, because each level is another
# allocation and nothing else is going on to hide the timing.  CPython's
# test_exception_group does exactly this, 1500 deep.

import gc

e = TypeError(1)
for i in range(300):
    e = ExceptionGroup("eg", [e])

print(type(e).__name__, e.message, len(e.exceptions))

# Every level is intact all the way down.
depth = 0
cur = e
while isinstance(cur, BaseExceptionGroup):
    assert cur.message == "eg", depth
    assert len(cur.exceptions) == 1, depth
    assert cur.args == ("eg", [cur.exceptions[0]]), depth
    cur = cur.exceptions[0]
    depth += 1
print("depth", depth, type(cur).__name__, cur.args)

gc.collect()
print("after collect", type(e).__name__, len(e.exceptions))

# ...and with the collector run at every single allocation, which is what
# turns the 1-in-N into 1-in-1.
gc.set_threshold(1)
try:
    g = ValueError("leaf")
    for i in range(200):
        g = ExceptionGroup("g%d" % i, [g])
    print("threshold-1", type(g).__name__, g.message, len(g.exceptions))
    print(str(g), repr(g)[:40])
finally:
    gc.set_threshold(700)

gc.collect()
del e, g, cur
gc.collect()

# A group with several children, and one built by a subclass.
class MyGroup(ExceptionGroup):
    pass


big = MyGroup("many", [ValueError(i) for i in range(50)])
print(type(big).__name__, big.message, len(big.exceptions), str(big))
gc.collect()
print(len(big.exceptions), big.exceptions[7].args)
print("done")
