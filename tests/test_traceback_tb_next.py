"""Assigning to traceback.tb_next.

unittest trims its own frames out of a failure's traceback by walking to the
last entry it wants to keep and assigning None to that entry's tb_next
(`_remove_unittest_tb_frames`, unittest/result.py).  The traceback type had no
tp_setattr at all, so every assertion failure inside CPython's own unittest
reported `AttributeError: cannot set attribute` instead of the failure it was
there to report.

Only tb_next is writable, and only a traceback or None.  CPython is not uniform
about the message for the rest -- tb_lineno has a setter that refuses,
tb_frame and tb_lasti are plain members, and a name that is not an attribute at
all is an ordinary AttributeError -- and all four are checked here, because a
wrong message is what a test asserting on one would see.
"""


def three_deep():
    def a():
        b()

    def b():
        c()

    def c():
        raise ValueError("v")

    try:
        a()
    except ValueError as e:
        return e.__traceback__


def length(tb):
    n = 0
    while tb is not None:
        n += 1
        tb = tb.tb_next
    return n


def names(tb):
    out = []
    while tb is not None:
        out.append(tb.tb_frame.f_code.co_name)
        tb = tb.tb_next
    return out


tb = three_deep()
print("entries:", length(tb))
print("names:", names(tb))

# Trim everything after the first entry.
tb.tb_next = None
print("after trim:", length(tb), names(tb))

# Re-attach a traceback from somewhere else.
other = three_deep()
tb.tb_next = other
print("re-attached:", length(tb), names(tb))
print("is that one:", tb.tb_next is other)

# Trim in the middle.
mid = three_deep()
mid.tb_next.tb_next = None
print("trimmed middle:", length(mid), names(mid))


print("--- what it refuses ---")
t = three_deep()
try:
    t.tb_next = t
except ValueError as e:
    print("self:", e)

deep = three_deep()
try:
    deep.tb_next.tb_next.tb_next = deep
except ValueError as e:
    print("cycle:", e)

for bad in (5, "x", [], (), object(), 1.5, True):
    try:
        t.tb_next = bad
        print("accepted", type(bad).__name__, "- wrong")
    except TypeError as e:
        print(type(bad).__name__, "->", type(e).__name__)


print("--- the other attributes ---")
for n in ("tb_lineno", "tb_frame", "tb_lasti", "zzz"):
    try:
        setattr(t, n, 1)
        print(n, "-> accepted, wrong")
    except AttributeError as e:
        print(n, "->", e)

# Reading them still works after all of that.
print("readable:", isinstance(t.tb_lineno, int), t.tb_frame.f_code.co_name,
      isinstance(t.tb_lasti, int))


print("--- a trimmed traceback still renders ---")
r = three_deep()
r.tb_next = None
try:
    raise IndexError("shown")
except IndexError as e:
    e.__traceback__ = r
    print("attached foreign traceback:", length(e.__traceback__))

print("--- and what tb_next refuses names itself ---")
try:
    raise ValueError("x")
except ValueError as _e:
    _tb = _e.__traceback__
for _v in (5, "x", 1.5, [], (), {}, object(), int):
    try:
        _tb.tb_next = _v
        print(type(_v).__name__, "-> accepted, wrong")
    except TypeError as _err:
        print("%-8s %s" % (type(_v).__name__, _err))

print("done")
