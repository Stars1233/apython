# A static type is not a collectable object, and the collector must not read a
# GC head in front of one.
#
# Every metatype carries TYPE_FLAG_HAVE_GC, because the heap classes it makes
# are collected.  The visitors tested only that flag and then read a PyGC_Head
# at obj - 16, so `object`, `str` and `type` itself -- whose tables are static
# data with a string or padding in front of them -- had those bytes read as
# links.  A tp_traverse hands over ob_type, tp_base and every tp_mro entry, so
# this happened in any program that collected anything at all.
#
# When the bytes in front of a static table happened to carry the COLLECTING
# bit, the visitor moved the static type INTO a collection list, and the next
# phase traversed it and unlinked rodata.  Which bytes were there depended on
# the binary's layout, so the crash appeared and vanished with edits that had
# nothing to do with it: moving one function between two files turned a green
# suite into 712 segfaults, and the fault landed in gc_list_remove, pages away
# from anything wrong.
#
# CPython asks the question through tp_is_gc, and `type`'s implementation of
# that slot is exactly "is this a heap type".

import gc

HEAPTYPE = 1 << 9


class Plain:
    pass


class WithCycle:
    pass


class Meta(type):
    pass


class WithMeta(metaclass=Meta):
    pass


# Make real work for the collector: cycles through instances, through classes,
# and through a class built by a metaclass of its own.
for i in range(200):
    c = WithCycle()
    c.self = c
    c.cls = WithCycle
    d = {}
    d["self"] = d
    tmp = type("Temp%d" % i, (Plain,), {})
    tmp.mine = tmp

gc.collect()
gc.collect()

# The invariant: nothing the collector is holding is a static type.
tracked_statics = [o for o in gc.get_objects()
                   if isinstance(o, type) and not (o.__flags__ & HEAPTYPE)]
print(len(tracked_statics), "static types among the tracked objects")

# And the same question asked directly.
for t in (int, str, type, object, dict, tuple, BaseException, Meta.__base__):
    if gc.is_tracked(t):
        print("WRONG: gc.is_tracked(%s) is True" % t.__name__)
print("no static type reports itself tracked")

# A heap class IS tracked, which is the half that must keep working.
print(gc.is_tracked(Plain), gc.is_tracked(WithMeta), gc.is_tracked(Meta),
      "heap classes are tracked")
print(gc.is_tracked(WithCycle()), "and so are their instances")

# The collector still collects: every cycle above is gone.
before = len(gc.get_objects())
for i in range(200):
    c = WithCycle()
    c.self = c
gc.collect()
print(len(gc.get_objects()) <= before, "the cycles were collected")
