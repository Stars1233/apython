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
