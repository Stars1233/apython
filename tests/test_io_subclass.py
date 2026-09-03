# `class F(_io.FileIO): pass` -- ordinary single inheritance, no __slots__
# anywhere -- used to SEGFAULT on deallocation.
#
# instance_dealloc and instance_traverse walk every word from the dict slot to
# tp_basicsize and treat it as a __slots__ Value.  _io.FileIO is built by
# type_from_parts and then has its tp_basicsize patched up to make room for
# the descriptor, the flags, the name, the blksize and the mode -- all above
# tp_dictoffset + 8.  So the walk found five phantom slots and XDECREF'd
# them: a file descriptor of 3 is `dec qword [3]`, a wild write.  FileIO
# itself was safe only because it had been given a tp_traverse of its own and
# a dealloc that zeroes the raw words first.
#
# The header end is floored at the LAYOUT BASE's tp_basicsize now -- tp_base
# is exactly the type whose fields sit below the subclass's own slots.
#
# The second half: type_from_parts overwrote tp_dealloc/tp_traverse/tp_clear
# unconditionally, so fileio_dealloc never ran for a subclass and every
# instance leaked its descriptor.  A heaptype base's own three slots are
# inherited now, which is CPython's rule as well.

import _io
import gc


class F(_io.FileIO):
    pass


class G(_io.FileIO):
    __slots__ = ("tag",)


class B(_io.BytesIO):
    pass


class C(_io.BytesIO):
    __slots__ = ("note",)


f = F("/dev/null", "r")
print("F readable:", f.readable(), "fd ok:", f.fileno() >= 0)
f.attr = 1
print("F dict attr:", f.attr, sorted(vars(f)))
print("F read:", f.read())
f.close()
print("F closed:", f.closed)

g = G("/dev/null", "r")
g.tag = "t"
g.other = 2
print("G slot:", g.tag, "dict:", g.other, "fd ok:", g.fileno() >= 0)
g.close()

b = B(b"xy")
print("B read:", b.read(), "seek:", b.seek(0), b.read(1))
b.attr = 3
print("B attr:", b.attr)

c = C(b"zw")
c.note = "n"
print("C slot:", c.note, "read:", c.read())

del f, g, b, c
gc.collect()
print("survived dealloc")


# The descriptor is actually closed, not leaked.  CPython reuses the same low
# fd every time; so must we.
def churn(n):
    seen = []
    for _ in range(n):
        h = F("/dev/null", "r")
        seen.append(h.fileno())
        del h
    return max(seen) - min(seen)


print("fd churn spread:", churn(200))


# Collection walks the subclass without visiting a descriptor as a pointer.
cyc = F("/dev/null", "r")
cyc.self_ref = cyc
del cyc
print("collected cycle:", gc.collect() >= 0)


# And an ordinary class is untouched by the floor.
class Plain:
    __slots__ = ("a", "b")


p = Plain()
p.a = 1
p.b = 2
print("plain slots:", p.a, p.b, p.__getstate__())


class SubList(list):
    __slots__ = ("s",)


sl = SubList([1, 2])
sl.s = "x"
print("list subclass:", list(sl), sl.s)


class SubStr(str):
    pass


ss = SubStr("hi")
ss.z = 9
print("str subclass:", ss, ss.z)
