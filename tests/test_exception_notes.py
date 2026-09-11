# PEP 678: BaseException.add_note() and __notes__.
#
# A 3.11 language feature that was simply absent.  It needs no field on the
# object: CPython's BaseException_add_note is pure instance-attribute work --
# look __notes__ up, create a list if it is not there, refuse a non-list, and
# append.  What it costs is the method and the rendering.
#
# The rendering half -- one note per line, after the exception line, at every
# level of a cause/context chain -- is exercised by the CPython Lib/test sweep
# rather than here: reading an uncaught exception's report back needs a
# subprocess, and neither subprocess nor traceback is in this tree's lib/.
e = ValueError("m")
print(hasattr(e, "__notes__"))

e.add_note("first")
print(e.__notes__, e.__dict__)
e.add_note("second")
print(e.__notes__)
print(e.args, str(e))

# It is an ordinary attribute, so it can be read, replaced and deleted.
e.__notes__ = ["replaced"]
e.add_note("after")
print(e.__notes__)
del e.__notes__
print(hasattr(e, "__notes__"))
e.add_note("fresh")
print(e.__notes__)

for bad in (5, None, b"x", 1.5, ["a"]):
    try:
        e.add_note(bad)
    except TypeError as ex:
        print("TypeError:", ex)

# Spelled out rather than starred: CPython's *args path composes the message
# from the RECEIVER's class and its direct path from the defining one, so
# `e.add_note(*())` says ValueError and `e.add_note()` says BaseException.
try:
    e.add_note()
except TypeError as ex:
    print("TypeError:", ex)
try:
    e.add_note("a", "b")
except TypeError as ex:
    print("TypeError:", ex)
try:
    e.add_note("a", "b", "c")
except TypeError as ex:
    print("TypeError:", ex)

f = ValueError("x")
f.__notes__ = 5
try:
    f.add_note("a")
except TypeError as ex:
    print("TypeError:", ex)
f.__notes__ = "abc"
try:
    f.add_note("a")
except TypeError as ex:
    print("TypeError:", ex)

# Every exception type has it, including the group and a user subclass.
g = ExceptionGroup("g", [ValueError("v")])
g.add_note("group note")
print(g.__notes__)


class Sub(KeyError):
    pass


s = Sub("k")
s.add_note("sub note")
print(s.__notes__)

b = BaseException("b")
b.add_note("base note")
print(b.__notes__)

k = KeyboardInterrupt()
k.add_note("ki")
print(k.__notes__)

# A note survives being raised and caught.
try:
    raise s
except Sub as caught:
    print("raised:", caught.__notes__)

# Multi-line and empty notes are stored verbatim.
n = ValueError("n")
n.add_note("")
n.add_note("two\nlines")
print(n.__notes__)

print(BaseException.add_note)
print(callable(e.add_note))
