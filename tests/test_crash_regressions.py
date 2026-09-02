"""Eight ways to crash the interpreter, each fixed.

Every one of these segfaulted, aborted the process, or read freed memory.
They have nothing in common except that none of them is reachable from code
that only uses a type the way it was designed to be used -- a struct sequence
built rather than returned, a bytearray subclass, an argument of the wrong
kind, a memoryview outliving what it viewed.
"""

import posix
import _io


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(36), repr(got))


# --- a struct sequence built by hand, not returned by a syscall ---
# Without a tp_new of its own, type_call fell through to tuple's, which
# allocates with a GC header this family's dealloc does not expect: the free
# was sixteen bytes off and the process aborted.
check("terminal_size", lambda: posix.terminal_size((80, 24)))
check("its fields", lambda: (lambda t: (t.columns, t.lines, t[0], len(t)))(
    posix.terminal_size((80, 24))))
check("from a list", lambda: posix.terminal_size([1, 2]))
check("from a generator", lambda: posix.terminal_size(x for x in (3, 4)))
check("too short", lambda: posix.terminal_size((1,)))
check("too long", lambda: posix.terminal_size((1, 2, 3)))
check("not a sequence", lambda: posix.terminal_size(5))
check("no arguments", lambda: posix.terminal_size())
check("uname_result", lambda: posix.uname_result(("a", "b", "c", "d", "e")))
for _ in range(300):
    _ = posix.terminal_size((80, 24))
print("built and dropped 300".ljust(36), repr(True))


# --- a bytearray subclass ---
# The constructor allocated the base's size, so a subclass's dict word and
# __slots__ landed past the end of the block.
class Slotted(bytearray):
    __slots__ = ("a", "b", "c", "d")


class Plain(bytearray):
    pass


s = Slotted(b"hi")
s.a, s.b = 1, 2
check("subclass with __slots__", lambda: (s.a, s.b, bytes(s)))
p = Plain(b"xy")
p.tag = 9
check("subclass with a dict", lambda: (p.tag, bytes(p), sorted(p.__dict__)))
check("it is still a bytearray", lambda: (isinstance(s, bytearray), len(s)))
check("and still mutable", lambda: (p.append(122), bytes(p)))
for i in range(300):
    q = Slotted(b"z" * (i % 17))
    q.a = i
print("300 subclass instances".ljust(36), repr(True))


# --- sum() over something that is not iterable after all ---
# The error path released the iterator and then read it as the argument array.
class NoNext:
    def __iter__(self):
        return self


check("sum of a broken iterator", lambda: sum(NoNext()))
check("sum of an int", lambda: sum(5))
check("sum still works", lambda: sum([1, 2, 3]))


# --- __import__ with a fromlist that is not a tuple ---
# import_module reads it as one, so anything else was dereferenced.
check("fromlist 0", lambda: __import__("sys", None, None, 0).__name__)
check("fromlist None", lambda: __import__("sys", None, None, None).__name__)
check("fromlist a string", lambda: __import__("sys", None, None, "x").__name__)
check("fromlist an int", lambda: __import__("sys", None, None, 7).__name__)
check("fromlist a float", lambda: __import__("sys", None, None, 1.5).__name__)
check("fromlist a list", lambda: __import__("sys", None, None, ["*"]).__name__)
check("fromlist a tuple", lambda: __import__("sys", None, None, ()).__name__)


# --- a memoryview that outlives its buffer ---
# release() zeroes the pointer and leaves the length, and every reader here
# went through the pointer.
mv = memoryview(b"abc")
mv.release()
check("released == bytes", lambda: mv == b"abc")
check("released != bytes", lambda: mv != b"abc")
check("bytes of a live view", lambda: bytes(memoryview(b"abc")))


# --- a slice of a BytesIO getbuffer ---
# The export count was incremented once and decremented by every view that
# shared the source, so releasing the original let the storage be reallocated
# while a slice still pointed into it.
b = _io.BytesIO(b"abcdefgh")
whole = b.getbuffer()
part = whole[2:6]
copy = memoryview(whole)
del whole
check("write while a slice is live", lambda: b.write(b"x"))
check("the slice still reads", lambda: bytes(part))
check("the copy still reads", lambda: bytes(copy))
del part
check("write while a copy is live", lambda: b.write(b"x"))
del copy
check("write once all are gone", lambda: (b.write(b"!"), b.getvalue()))


# --- unary + on a complex subclass ---
# The slot declined and the caller stored the NULL it returned.
class C(complex):
    pass


c = C(1, 2)
x = +c
check("unary plus", lambda: (x, type(x).__name__))
check("in a list", lambda: [+c])
check("repr of the list", lambda: repr([+c]))
check("unary minus", lambda: -c)


# --- an fd or a mode argument of the wrong kind ---
# val_to_i64 trusts its caller: a str's length became the descriptor and
# posix.close("x") closed stdout.
for arg in ("x", 0.5, None, [1], b"z", {}):
    check("close(%r)" % (arg,), lambda arg=arg: posix.close(arg))
check("stdout survived", lambda: True)
check("access with a float mode", lambda: posix.access("/etc/hostname", 0.5))
check("dup of a str", lambda: posix.dup("2"))
check("a bool is an integer", lambda: type(posix.dup(True)).__name__)
