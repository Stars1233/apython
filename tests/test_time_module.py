# time.time and time.sleep, beside the monotonic and process_time the module
# already had.  time() is the wall clock -- CLOCK_REALTIME -- where monotonic
# is the one that cannot go backwards, and sleep() is nanosleep.

import time
t = time.time()
print(type(t).__name__, t > 1600000000, t < 4000000000)
a = time.monotonic()
time.sleep(0.01)
b = time.monotonic()
print(b > a, (b - a) < 5)
print(time.sleep(0))
try:
    time.sleep(-1)
except ValueError:
    print("negative => ValueError")
try:
    time.sleep("x")
except TypeError:
    print("str => TypeError")
print(type(time.process_time()).__name__)

# A delay that cannot be represented is an error, not a silent one.
#
# The seconds were split with cvttsd2si, which answers INT64_MIN -- x86's
# "integer indefinite" -- for anything out of range, and that went straight
# into tv_sec.  time.sleep(float('inf')) returned at once, and
# time.sleep(10**10) slept for three centuries.  NaN was reported as
# "negative", which it is not.
#
# CPython reaches the same overflow by two roads and words it differently on
# each: an int is rejected converting into _PyTime_t, a float converting back
# out to the platform's timespec.  The range test also has to come before the
# negative one, or -inf reports the wrong error.
print("--- delays that do not fit ---")
for label, v in (("inf", float("inf")), ("-inf", -float("inf")),
                 ("nan", float("nan")), ("1e300", 1e300), ("-1e300", -1e300),
                 ("9.3e9", 9.3e9), ("-1.0", -1.0), ("10**10", 10 ** 10),
                 ("2**63", 2 ** 63), ("-(2**63)", -(2 ** 63)),
                 ("10**30", 10 ** 30)):
    try:
        time.sleep(v)
        print("%-9s -> returned" % label)
    except Exception as e:
        print("%-9s -> %s: %s" % (label, type(e).__name__, e))

# The short ones still sleep, and -0.0 is not negative.
time.sleep(0)
time.sleep(0.001)
time.sleep(-0.0)
time.sleep(False)
print("short delays still sleep")
