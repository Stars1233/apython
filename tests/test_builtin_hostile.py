# Every callable builtin, called with an argument of the wrong shape.
#
# The first sweep of this kind found eight segfaults reachable from one line
# of ordinary Python, all of the same shape: a Value that is not a pointer,
# read as one.  What is checked here is not the wording -- that is
# tests/test_argument_messages.py's job -- but that the call raises rather
# than dies, and that the cases which HAVE a right answer give it.

import builtins
import sys

BAD = [None, 0, -1, 2**70, -(2**70), 1.5, float('nan'), float('inf'), "", "x",
       b"", b"x", bytearray(b"x"), [], [1], (), (1,), {}, {"a": 1}, set(),
       object(), type, slice(None), range(3), Ellipsis, True, complex(1, 2),
       memoryview(b"ab")]

# The ones that would read stdin, exit, spawn a compiler or print -- and
# pow and round, which would be asked for 2**70 to the power of 2**70.
SKIP = {"input", "exit", "quit", "help", "eval", "exec", "compile", "open",
        "print", "breakpoint", "__import__", "copyright", "credits",
        "license", "reload", "pow", "round"}

# The exception hierarchy is a hundred names that all do the same thing --
# store their arguments -- so three of them stand for the rest and keep this
# file inside a test suite's patience.
KEEP_EXC = {"BaseException", "ValueError", "OSError", "BaseExceptionGroup",
            "ExceptionGroup", "UnicodeDecodeError", "StopIteration"}

calls = 0
for name in sorted(dir(builtins)):
    if name in SKIP:
        continue
    fn = getattr(builtins, name)
    if not callable(fn):
        continue
    if (isinstance(fn, type) and issubclass(fn, BaseException)
            and name not in KEEP_EXC):
        continue
    for arg in BAD:
        for args in ((arg,), (arg, arg)):
            calls += 1
            try:
                fn(*args)
            except BaseException:
                pass
# Not the count: the two interpreters' builtins namespaces are not
# identical, and what is being tested is that nothing here dies.
print("survived", calls > 3000)

# sorted()'s key runs even when there is nothing to sort: CPython computes
# the keys before it looks at the length.
try:
    sorted([1], key=0)
    print("sorted key: NOT RAISED")
except TypeError as e:
    print("sorted key:", e)

def boom(x):
    raise ValueError("key ran")

for n in (0, 1, 2):
    try:
        sorted([1] * n, key=boom)
        print("sorted", n, "-> no key call")
    except ValueError as e:
        print("sorted", n, "->", e)

# isinstance() and issubclass() recurse into a tuple, so an element that is
# not a class is refused rather than dereferenced.
for second in ((1,), (int,), ((str,), (int,)), (), (str, int | bytes)):
    try:
        print("isinstance", second is not None and len(second), isinstance(1, second))
    except TypeError as e:
        print("isinstance ->", e)
    try:
        print("issubclass", len(second), issubclass(bool, second))
    except TypeError as e:
        print("issubclass ->", e)

# format()'s spec is read as a string, so it has to be one.
for spec in (0, None, [], "", "d"):
    try:
        print("format", repr(format(1, spec)))
    except TypeError as e:
        print("format ->", e)
try:
    format()
except TypeError as e:
    print("format ->", e)
try:
    format(1, "", 2)
except TypeError as e:
    print("format ->", e)
