# An attribute fetched by name is a VALUE, and a Value may be an immediate.
#
# obj_getattr_opt answers with a Value, not a PyObject*, and obj_decref writes
# through what it is handed -- `dec qword [rdi + ob_refcnt]`, with only a NULL
# test in front of it.  So every place that fetched an attribute by name, used
# it, and released it with obj_decref was a wild write waiting for someone to
# store a small int under that name:
#
#     sys.stdout = 5; print("x")          -> a core dump
#     class M: keys = 1
#     {**M()}                             -> a core dump
#
# DECREF_V is the release for a Value -- one compare and one branch, and it
# leaves an immediate alone.  The places this covers are print's sink and its
# write and flush, DICT_UPDATE's keys test, INTRINSIC_PRINT's displayhook, and
# the lazily built sys.stdout.buffer.
#
# The other half of the same mistake was print's: it treated a non-pointer
# sys.stdout as "no stream" and produced NOTHING, where CPython names the type.
# A non-pointer flows through to the write attempt now, which is what raises.

import sys


def show(label, fn):
    try:
        fn()
        print("%-42s no error" % label)
    except BaseException as e:
        print("%-42s %s: %s" % (label, type(e).__name__, str(e)[:60]))


class IntWrite:
    write = 1


class IntFlush:
    def write(self, s):
        return len(s)

    flush = 1


class IntKeys:
    keys = 1


class IntKeysWithGetItem:
    keys = 1

    def __getitem__(self, key):
        return 1


show("print(file=an int)", lambda: print("x", file=42))
show("print(file=a float)", lambda: print("x", file=1.5))
show("print(file=obj whose write is an int)", lambda: print("x", file=IntWrite()))
show("print(file=obj whose flush is an int)",
     lambda: print("x", file=IntFlush(), flush=True))
show("{**obj whose keys is an int}", lambda: {**IntKeys()})
show("{**obj with an int keys and getitem}", lambda: {**IntKeysWithGetItem()})


# sys.stdout itself, which is the one that crashed.  Restored immediately,
# because everything after it prints.
def reassign(value):
    saved = sys.stdout
    try:
        sys.stdout = value
        print("x")
    finally:
        sys.stdout = saved


show("sys.stdout = an int, then print", lambda: reassign(5))
show("sys.stdout = a float, then print", lambda: reassign(1.5))
show("sys.stdout = an int-write object", lambda: reassign(IntWrite()))


# sys.displayhook, which INTRINSIC_PRINT fetches the same way.
def display(value):
    saved = sys.displayhook
    try:
        sys.displayhook = value
        exec(compile("1", "<t>", "single"), {})
    finally:
        sys.displayhook = saved


show("sys.displayhook = an int", lambda: display(42))
show("sys.displayhook = a float", lambda: display(1.5))

# And the ordinary cases still work, which is the point of not simply refusing.
print(sys.stdout.write("a normal write survives all that\n"), "bytes written")
