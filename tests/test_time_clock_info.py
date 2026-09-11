# time.get_clock_info, which is what stands between this tree and asyncio.
#
# CPython's BaseEventLoop.__init__ opens with
#     self._clock_resolution = time.get_clock_info('monotonic').resolution
# and gets no further without it, so every asyncio module -- gather, TaskGroup,
# timeout, the streams -- died on an AttributeError before running a line of
# its own.  It is five rows of a table and four fields of a namespace.
#
# The answers are the platform's, and every one of these clocks is
# clock_gettime, whose timespec counts nanoseconds -- so the resolution is
# 1e-09 across the board, which is what CPython reports too (it reports
# clock_getres' answer rather than the clock's true precision).
import time

for name in ("time", "monotonic", "perf_counter", "process_time", "thread_time"):
    ci = time.get_clock_info(name)
    print("%-13s %-42s mono=%-5s adj=%-5s res=%r"
          % (name, ci.implementation, ci.monotonic, ci.adjustable, ci.resolution))

ci = time.get_clock_info("monotonic")
print(repr(ci))
print(type(ci).__name__, type(ci).__module__)
print(ci.implementation, ci.monotonic, ci.adjustable, ci.resolution)

# The namespace is a fresh object each call and is ordinarily mutable.
a = time.get_clock_info("time")
b = time.get_clock_info("time")
print("distinct:", a is not b, "equal:", a == b)
a.resolution = 1.0
print("mutable:", a.resolution, b.resolution)

for bad in ("nope", "", "MONOTONIC", "monotonic "):
    try:
        time.get_clock_info(bad)
    except ValueError as e:
        print("ValueError:", e)

# A str SUBCLASS is a str, as PyUnicode_Check takes one...
class MyStr(str):
    pass


print("subclass:", time.get_clock_info(MyStr("perf_counter")).implementation)

# ...but a name with an embedded NUL is not a name: the table is scanned with
# a C string compare, which would stop at the NUL and match "time".
for bad in ("time\0junk", "\0", "monotonic\0"):
    try:
        time.get_clock_info(bad)
    except ValueError as e:
        print("ValueError:", e)

for bad in (5, None, b"monotonic", ("monotonic",)):
    try:
        time.get_clock_info(bad)
    except TypeError as e:
        print("TypeError:", e)

try:
    time.get_clock_info()
except TypeError as e:
    print("TypeError:", e)
try:
    time.get_clock_info("monotonic", "extra")
except TypeError as e:
    print("TypeError:", e)
