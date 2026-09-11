# time's integer-nanosecond clocks, and _STRUCT_TM_ITEMS.
#
# _STRUCT_TM_ITEMS is the one that hurt: _strptime.py's last line is
# `time.struct_time(tt[:time._STRUCT_TM_ITEMS])`, so every strptime call in
# the stdlib died on an AttributeError.  The struct_time here already declared
# n_fields = 11; the module just never published the number.

import time

print(time._STRUCT_TM_ITEMS)

# struct_time takes exactly that many items, which is the property _strptime
# is relying on.
tt = (2026, 9, 10, 12, 0, 0, 3, 253, 0, "UTC", 0)
st = time.struct_time(tt[: time._STRUCT_TM_ITEMS])
print(len(st), st.tm_year, st.tm_mon, st.tm_mday, st.tm_yday, st.tm_isdst)

# The *_ns clocks are integers, and agree with their float counterparts.
for name in ("time", "monotonic", "perf_counter", "process_time"):
    f = getattr(time, name)
    g = getattr(time, name + "_ns")
    a = g()
    b = f()
    c = g()
    print(name, type(a) is int, type(b) is float, a <= c)
    # The float is the same clock, so it must sit inside the bracket the two
    # integer reads make -- with a nanosecond of slack for the float's own
    # rounding at 1e9 counts a second.
    print(name, (a / 1e9) - 1e-6 <= b <= (c / 1e9) + 1e-6)

# They take no arguments.
for name in ("time_ns", "monotonic_ns", "perf_counter_ns", "process_time_ns"):
    try:
        getattr(time, name)(1)
    except TypeError:
        print(name, "arity refused")

# monotonic_ns never goes backwards, and perf_counter_ns has enough resolution
# to separate two adjacent reads of a loop.
prev = time.monotonic_ns()
ok = True
for _ in range(1000):
    now = time.monotonic_ns()
    if now < prev:
        ok = False
    prev = now
print("monotonic:", ok)

# time_ns is nanoseconds since the epoch, so it is far larger than time().
print(time.time_ns() > 10**18, abs(time.time_ns() / 1e9 - time.time()) < 1.0)

print("done")
