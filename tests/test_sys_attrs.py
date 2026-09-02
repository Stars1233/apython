# The sys and time attributes the stdlib reads without asking first.
#
# CPython's getopt, gettext and optparse open with sys.base_prefix, timeit
# with time.perf_counter, and site.py with sys.copyright.  None of them
# guards the read, so an absent name is an AttributeError at import rather
# than a feature the program can do without -- five modules that could not be
# imported for want of four strings and a clock.
#
# The values are not compared against CPython's: prefix is an installation
# path, copyright is a licence, and perf_counter's origin is undefined by
# definition.  What has to agree is that they exist and what shape they are.

import sys
import time

print("=== the prefixes ===")
for name in ("prefix", "exec_prefix", "base_prefix", "base_exec_prefix"):
    value = getattr(sys, name)
    print(name, type(value).__name__)

# With no virtualenv, base_prefix is prefix.  CPython says so too, and this is
# the invariant getopt and gettext are really relying on.
print("base is prefix:", sys.base_prefix == sys.prefix)
print("base_exec is exec:", sys.base_exec_prefix == sys.exec_prefix)

print("=== copyright ===")
print("type:", type(sys.copyright).__name__)
print("non-empty:", len(sys.copyright) > 0)
print("names a year:", any(c.isdigit() for c in sys.copyright))

print("=== perf_counter ===")
first = time.perf_counter()
second = time.perf_counter()
print("type:", type(first).__name__)
print("does not go backwards:", second >= first)
# It measures something: a busy loop has to take a non-negative time.
start = time.perf_counter()
total = 0
for i in range(10000):
    total += i
print("elapsed is non-negative:", time.perf_counter() - start >= 0.0)
print("total:", total)
try:
    time.perf_counter(1)
except TypeError:
    print("argument => TypeError")
