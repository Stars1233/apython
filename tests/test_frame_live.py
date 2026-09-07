# A frame object is a VIEW onto a live frame, not a copy of one.
#
# It used to be a copy: sys._getframe() built a fresh object every time, so
# `sys._getframe() is sys._getframe()` was False, f_lineno was where the frame
# had been when the copy was taken, and there was nowhere to put f_trace.
# bdb compares frames by identity across events -- `frame is self.stopframe`,
# `frame is self.botframe` -- so a copy per event made every one of those
# comparisons false and the debugger never stopped.
#
# The frame is pooled and recycled, so the view cannot simply BE the frame.
# The two point at each other, borrowed both ways, and frame_free detaches:
# it copies out the line, the offset and the fast locals just before the pool
# takes the memory back, after which the object is the snapshot it used to be
# and every attribute still reads.
import sys


def identity():
    a = sys._getframe()
    b = sys._getframe()
    return a is b


print("same object twice:", identity())


def advancing():
    f = sys._getframe()
    first = f.f_lineno
    second = f.f_lineno        # a different line, read from the same object
    return second - first


print("f_lineno is live:", advancing())


def locals_are_live():
    x = 1
    f = sys._getframe()
    before = sorted(f.f_locals)
    y = 2                       # noqa: F841
    after = sorted(f.f_locals)
    return before, after


print("f_locals is live:", locals_are_live())


def locals_keep_extras():
    # pdb writes __return__ into f_locals and reads it back, so the dict has
    # to be the same object each time and a refresh must merge rather than
    # replace.
    f = sys._getframe()
    d1 = f.f_locals
    f.f_locals["__return__"] = 99
    zzz = 1                     # noqa: F841
    d2 = f.f_locals
    return d1 is d2, d2.get("__return__")


print("f_locals merges:", locals_keep_extras())


def survives():
    q = 7                       # noqa: F841
    return sys._getframe()


gone = survives()
print("detached f_locals:", sorted(gone.f_locals))
print("detached f_code:", gone.f_code.co_name)
print("detached f_back:", gone.f_back.f_code.co_name)


def trace_slots():
    f = sys._getframe()
    out = [f.f_trace, f.f_trace_lines, f.f_trace_opcodes]
    f.f_trace = print
    out.append(f.f_trace is print)
    f.f_trace = None            # None is a delete, as it is in CPython
    out.append(f.f_trace)
    f.f_trace = print
    del f.f_trace
    out.append(f.f_trace)
    f.f_trace_opcodes = True
    f.f_trace_lines = False
    out.append((f.f_trace_lines, f.f_trace_opcodes))
    return out


print("trace slots:", trace_slots())

try:
    sys._getframe().f_lineno = 3
except ValueError as e:
    print("f_lineno write:", e)

try:
    sys._getframe().f_code = None
except AttributeError as e:
    print("f_code write:", e)


# A generator's frame is suspended, not dead, so its view stays live across
# the suspension.
def gen():
    i = 0
    while i < 2:
        yield sys._getframe()
        i += 1


g = gen()
f1 = next(g)
f2 = next(g)
print("generator frame is one object:", f1 is f2)
print("generator locals advance:", f1.f_locals["i"])
