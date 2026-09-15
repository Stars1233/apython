# The clocks a caller names, tzset, and strptime.
#
# The module had five fixed clock readers -- time(), monotonic(),
# perf_counter(), process_time() and their _ns forms -- and no way to ask for
# a clock by id: clock_gettime, clock_gettime_ns and clock_getres were absent
# with all nine CLOCK_* constants, and so were thread_time, tzset and
# strptime.
#
# The one real difference between the new ones and the old: a hardcoded
# CLOCK_MONOTONIC cannot fail, so none of the five checks the syscall's
# return value.  A caller-supplied number CAN, and EINVAL for a clock this
# kernel does not have is the answer rather than a timespec of whatever was
# on the stack.
import os
import time

# CPython does not export the two _COARSE ids, so neither does this: a name
# here and not there is as much a divergence as one missing.
CLOCKS = ["CLOCK_REALTIME", "CLOCK_MONOTONIC", "CLOCK_PROCESS_CPUTIME_ID",
          "CLOCK_THREAD_CPUTIME_ID", "CLOCK_MONOTONIC_RAW",
          "CLOCK_BOOTTIME", "CLOCK_TAI"]
print("no extras:", [n for n in ("CLOCK_REALTIME_COARSE",
                                 "CLOCK_MONOTONIC_COARSE")
                     if hasattr(time, n)])
print("the constants:", [(n, getattr(time, n)) for n in CLOCKS])

# --- reading them -------------------------------------------------------
for name in CLOCKS:
    clk = getattr(time, name)
    secs = time.clock_gettime(clk)
    ns = time.clock_gettime_ns(clk)
    res = time.clock_getres(clk)
    print("%-26s float %s  ns %s  res %s"
          % (name, isinstance(secs, float) and secs >= 0.0,
             isinstance(ns, int) and ns >= 0, res > 0.0))

# The two views of one clock agree to within a millisecond of each other.
secs = time.clock_gettime(time.CLOCK_MONOTONIC)
ns = time.clock_gettime_ns(time.CLOCK_MONOTONIC)
print("seconds and nanoseconds agree:", abs(ns / 1e9 - secs) < 0.001)

# And the named readers agree with the ones taken by id.
print("monotonic agrees:",
      abs(time.monotonic() - time.clock_gettime(time.CLOCK_MONOTONIC)) < 0.1)
print("time agrees:",
      abs(time.time() - time.clock_gettime(time.CLOCK_REALTIME)) < 0.1)
print("process_time agrees:",
      abs(time.process_time()
          - time.clock_gettime(time.CLOCK_PROCESS_CPUTIME_ID)) < 0.1)

# --- it moves -----------------------------------------------------------
start = time.clock_gettime_ns(time.CLOCK_MONOTONIC)
time.sleep(0.01)
print("monotonic advances:",
      time.clock_gettime_ns(time.CLOCK_MONOTONIC) - start >= 5_000_000)

# --- thread_time --------------------------------------------------------
print("thread_time:", isinstance(time.thread_time(), float),
      time.thread_time() >= 0.0)
print("thread_time_ns:", isinstance(time.thread_time_ns(), int),
      time.thread_time_ns() >= 0)
print("get_clock_info knew about it all along:",
      time.get_clock_info("thread_time").resolution)

# --- a clock that does not exist ----------------------------------------
try:
    time.clock_gettime(9999)
    print("a bad clock id: NOT REFUSED")
except OSError as exc:
    print("a bad clock id: OSError", exc.errno)
try:
    time.clock_getres(9999)
    print("a bad clock id (getres): NOT REFUSED")
except OSError as exc:
    print("a bad clock id (getres): OSError", exc.errno)
for call, what in ((lambda: time.clock_gettime(), "no argument"),
                   (lambda: time.clock_gettime("x"), "a str")):
    try:
        call()
        print("%-12s NOT REFUSED" % what)
    except TypeError:
        print("%-12s TypeError" % what)

# --- tzset --------------------------------------------------------------
saved = os.environ.get("TZ")
try:
    os.environ["TZ"] = "UTC0"
    time.tzset()
    print("UTC:", time.tzname[0], time.timezone, time.daylight)
    print("and localtime follows it:",
          time.strftime("%Z", time.localtime(0)),
          time.localtime(0).tm_hour)
    os.environ["TZ"] = "EST5EDT"
    time.tzset()
    print("EST:", time.tzname, time.timezone, time.daylight)
    print("and localtime follows it:", time.localtime(0).tm_hour)
finally:
    if saved is None:
        del os.environ["TZ"]
    else:
        os.environ["TZ"] = saved
    time.tzset()

# --- strptime -----------------------------------------------------------
t = time.strptime("2024-01-15", "%Y-%m-%d")
print("strptime:", type(t).__name__, t.tm_year, t.tm_mon, t.tm_mday,
      t.tm_wday, t.tm_yday)
print("with a time:",
      tuple(time.strptime("2024-06-30 13:45:01", "%Y-%m-%d %H:%M:%S"))[:6])
print("the default format:",
      time.strptime("Mon Jan 15 10:00:00 2024").tm_year)
print("round trip through strftime:",
      time.strftime("%Y-%m-%d", time.strptime("1999-12-31", "%Y-%m-%d")))
for bad, what in ((("nope", "%Y"), "unparseable"),
                  (("2024", "%Q"), "a bad directive"),
                  (("2024-01-15", "%Y-%m"), "trailing data")):
    try:
        time.strptime(*bad)
        print("%-16s NOT REFUSED" % what)
    except ValueError as exc:
        print("%-16s ValueError" % what)
print("survived")
