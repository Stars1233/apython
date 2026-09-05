# stdout is block-buffered when it is not a terminal.
#
# CPython's is, and it is observable: a program writing to both streams
# through a pipe sees ALL of its print() output after everything on stderr,
# because only one of the two is buffered.  Here the two used to interleave as
# they were written, which is what made two of the tests in this directory
# compare against a recorded transcript rather than against python3.
#
# The order this file prints is the order a pipe shows, and `make check` runs
# it through one -- so the interleaving IS what is being tested, and it is
# compared against CPython directly.

import sys

print("stdout a")
sys.stderr.write("stderr 1\n")
print("stdout b")
sys.stderr.write("stderr 2\n")

# flush= is honoured, which is the keyword's whole point: it used to be
# accepted and ignored, which cost nothing while nothing was buffered.
print("stdout c, flushed", flush=True)
sys.stderr.write("stderr 3 (after the flush)\n")

print("stdout d")
sys.stdout.flush()
sys.stderr.write("stderr 4 (after an explicit flush)\n")

# sys.stdout.write goes through the same buffer as print.
sys.stdout.write("written directly\n")
sys.stderr.write("stderr 5\n")
sys.stdout.flush()
sys.stderr.write("stderr 6\n")

# A write longer than the buffer goes out in order with what is already
# waiting, rather than jumping ahead of it.
print("short line before a long one")
sys.stdout.write("x" * 9000 + "\n")
print("short line after")
sys.stdout.flush()
sys.stderr.write("stderr 7\n")

# isatty tells the truth, which is what the decision rests on.
print("stdout isatty", sys.stdout.isatty())
print("stderr isatty", sys.stderr.isatty())

# Everything still buffered at this point is flushed at exit, which is the
# last thing the pipe shows.
print("last line, flushed at exit")
sys.stderr.write("stderr 8, before the exit flush\n")
