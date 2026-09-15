# The types only the interpreter may build refuse to be built.
#
# A generator, a coroutine, an iterator, a dict view, a scandir iterator --
# none of these has a constructor in Python.  CPython gives each of them a
# NULL tp_new and type_call answers "cannot create 'generator' instances".
# Here tp_new was 0 and type_call fell through to the ordinary
# allocate-and-initialise path instead, so `type(iter([]))()` handed back an
# object whose fields hold whatever the allocator last left there.
#
# That is not a curiosity.  os.scandir's iterator carries a DIR* and a
# refcounted path; constructing a bare one and letting it be collected walked
# the collector's doubly-linked list through an uninitialised GC head, and
# the process died in gc_list_remove -- several hundred tests after the line
# that did it.  CPython's test_os.TestScandir.test_uninstantiable is three
# lines and does exactly this.
#
# This is the same shape CLAUDE.md records for mappingproxy: "calling it fell
# through to the ordinary class-construction path and left its fields holding
# whatever was there".
import gc
import io
import os
import sys


def gen():
    yield 1


async def agen():
    yield 1


async def coro():
    pass


c = coro()
c.close()
a = agen()

scandir_it = os.scandir("/tmp")

cases = [
    ("generator", type(gen())),
    ("async_generator", type(a)),
    ("coroutine", type(c)),
    ("list_iterator", type(iter([]))),
    ("tuple_iterator", type(iter(()))),
    ("dict_keyiterator", type(iter({}))),
    ("set_iterator", type(iter(set()))),
    ("bytes_iterator", type(iter(b"x"))),
    # str's iterator is deliberately left out of the exact comparison: CPython
    # has two of them, str_ascii_iterator and str_iterator, chosen by whether
    # the string is ASCII, and this has one.  It is checked below.
    ("range_iterator", type(iter(range(1)))),
    ("dict_keys", type({}.keys())),
    ("dict_values", type({}.values())),
    ("dict_items", type({}.items())),
    ("ScandirIterator", type(scandir_it)),
    ("DirEntry", type(next(os.scandir("/tmp")))),
]
for label, t in cases:
    try:
        t()
        print("%-18s MADE ONE" % label)
    except TypeError as e:
        print("%-18s %s" % (label, e))

# The str iterator: the refusal and the shape of the message, since the name
# inside it is one of two in CPython and one here.
try:
    type(iter("x"))()
    print("str iterator      MADE ONE")
except TypeError as e:
    s = str(e)
    print("str iterator      %s ... %s" % (s.startswith("cannot create '"),
                                           s.endswith("_iterator' instances")))

scandir_it.close()

# The ones that DO have a constructor keep it -- this must not become a
# blanket refusal.
print("int:", int())
print("list:", list())
print("dict:", dict())
print("BytesIO:", io.BytesIO().getvalue())
print("StringIO:", io.StringIO().getvalue())
print("property:", type(property()).__name__)
print("Exception:", Exception().args)
print("OSError:", OSError().args)
print("zip:", list(zip()))
print("bool:", bool())
print("complex:", complex())

# And a collection afterwards, because the crash this closes was not at the
# construction: it was the collector walking a GC head that had never been
# written.
gc.collect()
print("survived")
