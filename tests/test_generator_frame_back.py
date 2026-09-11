# A suspended generator's frame has no caller.
#
# eval_frame links each frame to the one that was running when it started, so
# that sys._getframe() has an f_back to walk.  Nothing unlinks it, which is
# right for an ordinary frame -- it dies with the call that made it -- and
# wrong for a generator's, which outlives the call.  A generator suspended at
# a yield kept pointing at whatever frame happened to resume it, and that
# frame was freed and handed back out by the frame pool the moment its own
# call returned.
#
# What it looks like from Python is an f_back that should be None.  What it
# does underneath is worse: frameobj_detach follows prev_frame when the
# generator is finally torn down, to give a frame object that outlives its
# frame a working chain outward -- and by then the pointer names a recycled
# frame.  It segfaulted in frameobj_for, under gen_clear, in the shutdown
# collection.
#
# CPython clears the link at the same point, when the generator suspends.

import sys


def gen():
    frame = sys._getframe()
    yield frame
    yield "second"


def make():
    """Suspend a generator, then let this frame die."""
    g = gen()
    frame = next(g)
    return g, frame


g, frame = make()

print("f_back after the caller returned:", frame.f_back)
print("the frame is still the generator's:", frame.f_code.co_name)

# Resuming relinks it, because the generator really does have a caller again
# for the duration of the send -- and suspending must unlink it again.
print("second send:", next(g))
print("f_back after the second suspend:", frame.f_back)


# A generator that has never started has no frame to link at all.
def untouched():
    yield 1


u = untouched()
print("an unstarted generator's frame:", u.gi_frame.f_back)


# The same for a generator resumed from inside another generator: the inner
# one's f_back must not outlive the outer one's send either.
def inner():
    yield sys._getframe()


def outer():
    i = inner()
    yield next(i)


o = outer()
inner_frame = next(o)
print("nested generator f_back:", inner_frame.f_back)

# And walking outward from a running frame still works, which is what the
# link is for in the first place.
def depth_two():
    return sys._getframe().f_back.f_code.co_name


def depth_one():
    return depth_two()


print("f_back still walks for ordinary calls:", depth_one())
