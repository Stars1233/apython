# print writes to the sink it RESOLVED, and holds a reference to it.
#
# print_sink_resolve gave the reference back and kept a borrowed one, on the
# reasoning that "sys.stdout is reachable from sys the whole time".  That stops
# being true the moment user code reassigns it -- and print resolves the sink
# BEFORE converting its arguments, so a __str__ that does `sys.stdout = other`
# dropped the last reference to the sink and print then wrote through freed
# memory (valgrind: "Invalid read of size 8 at builtins.asm ... freed by
# gc_dealloc").  CPython is safe because PyFile_WriteObject fetches the bound
# `write` before it calls PyObject_Str.
#
# The sink is held in a list here, so this file can use python3 as its oracle:
# with the sink's ONLY reference in sys.stdout, CPython 3.12 itself hangs or
# segfaults on the same input -- it frees the file object while writing to it.
# What that shape proves is a valgrind run, not a diff.
import sys

real = sys.stdout
log = []
keep = []


class Sink:
    def write(self, s):
        log.append(s)
        return len(s)

    def flush(self):
        pass


class Reassigns:
    def __str__(self):
        sys.stdout = real
        return "converted"


def with_sink(*args, **kw):
    s = Sink()
    keep.append(s)
    sys.stdout = s
    try:
        print(*args, **kw)
    finally:
        sys.stdout = real


# Every argument position, and both keywords.
with_sink(Reassigns())
print("1:", "".join(log).strip())
log.clear()

with_sink("a", Reassigns(), "c", sep="-", end="!\n")
print("2:", "".join(log).strip())
log.clear()

with_sink(1, 2, Reassigns())
print("3:", "".join(log).strip())
log.clear()

with_sink(Reassigns(), flush=True)
print("4:", "".join(log).strip())
log.clear()


# A __str__ that raises after the sink was resolved.
class Raises:
    def __str__(self):
        sys.stdout = real
        raise ValueError("boom")


try:
    with_sink("before", Raises(), "after")
except ValueError:
    print("5: ValueError propagated,", repr("".join(log)))
log.clear()

# file= is borrowed from the argument array and must NOT be released.
class Counted:
    def __init__(self):
        self.n = 0

    def write(self, s):
        self.n += 1
        return len(s)

    def flush(self):
        pass


c = Counted()
for i in range(200):
    print(i, file=c)
print("6:", c.n > 0, sys.getrefcount(c) >= 2)


# And flush=True against a sink with no flush is still the error it was.
class NoFlush:
    def write(self, s):
        return len(s)


try:
    print("x", file=NoFlush(), flush=True)
except AttributeError:
    print("7: AttributeError for flush=True")

print("8:", sys.stdout is real, len(keep))
