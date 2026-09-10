# A __del__ in a cycle runs at shutdown, and `sys` is still whole when it does.
#
# The exit sequence broke the sys.modules cycle -- NULLing sys's own module
# dict -- BEFORE running the final collection.  Those finalizers are ordinary
# Python, and the first thing one does is reach for sys.stderr or
# sys.is_finalizing; module_getattr then read a dict pointer of zero and the
# process segfaulted at exit, after every line of its output had been
# produced.  CPython's asyncio does exactly this from loop.close().
#
# The other half is the flush.  The finalizers are the last Python that runs,
# so a print from one of them lands in the buffer the pre-collection flush
# just emptied; CPython flushes again after finalization and so does this.

import sys


class Cycle:
    def __init__(self, name):
        self.name = name
        self.me = self          # only the collector can free this

    def __del__(self):
        # sys must still answer, by attribute and by getattr.
        sys.stdout.write("%s: maxsize=%d\n" % (self.name, sys.maxsize))
        sys.stdout.write("%s: getattr=%s\n" % (self.name, getattr(sys, "maxsize")))
        sys.stdout.write("%s: hasattr=%s\n" % (self.name, hasattr(sys, "nosuchname")))
        # ...and so must a module reached through sys.modules.
        m = sys.modules["sys"]
        sys.stdout.write("%s: via modules=%d\n" % (self.name, m.maxsize))
        # print(), not just write(), because it is print that most finalizers
        # use and print is what the second flush is for.
        print("%s: printed" % self.name)


Cycle("first")
Cycle("second")
print("main done")
