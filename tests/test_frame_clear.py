"""frame.clear(), which is how a reference cycle through a traceback is broken.

traceback.clear_frames() calls it on every entry of a traceback, and unittest's
assertRaises calls that in its __exit__, so its absence stopped nearly every
test CPython ships -- an AttributeError raised from inside a __exit__ that was
already handling an exception.

clear_frames is written as

    try: tb.tb_frame.clear()
    except RuntimeError: pass

so the frames it CANNOT clear matter as much as the ones it can, and both are
here.  A frame still attached to a live frame raises RuntimeError, exactly as
CPython's does for one that is executing.
"""

import sys

f = sys._getframe()
print("has clear:", hasattr(f, "clear"))
print("callable:", callable(f.clear))


print("--- an executing frame refuses ---")
try:
    f.clear()
    print("cleared the running frame - wrong")
except RuntimeError as e:
    print("running:", e)


def inner():
    return sys._getframe()


def outer():
    marker = "outer local"
    g = inner()
    try:
        g.f_back.clear()
        return "cleared a live caller - wrong"
    except RuntimeError as e:
        return "live caller: %s" % e


print(outer())


print("--- a returned frame clears ---")
def returns_its_frame():
    local_marker = ["kept alive"]
    return sys._getframe()


fr = returns_its_frame()
print("locals before:", "local_marker" in fr.f_locals)
fr.clear()
print("locals after:", fr.f_locals)
print("code survives:", fr.f_code.co_name)
print("globals survive:", isinstance(fr.f_globals, dict))
print("lineno survives:", isinstance(fr.f_lineno, int))

# Clearing twice is fine.
fr.clear()
print("cleared twice ok")


print("--- through a traceback ---")
def raises():
    big = list(range(50))
    raise ValueError("v")


def collect_frames():
    try:
        raises()
    except ValueError as e:
        tb = e.__traceback__
        frames = []
        while tb is not None:
            frames.append(tb.tb_frame)
            tb = tb.tb_next
        return frames


frames = collect_frames()
print("traceback frames:", len(frames))
cleared = 0
refused = 0
for fo in frames:
    try:
        fo.clear()
        cleared += 1
    except RuntimeError:
        refused += 1
print("cleared:", cleared, " refused:", refused)
print("names still readable:", [fo.f_code.co_name for fo in frames])


print("--- arity ---")
for call in (lambda: fr.clear(1), lambda: fr.clear(1, 2)):
    try:
        call()
        print("no error - wrong")
    except TypeError as e:
        print("arity:", e)


print("--- f_trace is dropped too ---")
def with_trace():
    return sys._getframe()


ft = with_trace()
ft.f_trace = lambda *a: None
print("trace set:", ft.f_trace is not None)
ft.clear()
print("trace after clear:", ft.f_trace)

print("done")
