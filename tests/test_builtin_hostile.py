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

# The messages these refusals carry, which CPython words precisely and this
# tree used to word approximately.  Each line is diffed against CPython.
MESSAGES = [
 "abs()", "abs(1,2)", "chr(1,2)", "hex(1,2)", "bin(1,2)", "oct(1,2)",
 "ord(1,2)", "divmod(1)", "divmod(1,2,3)", "callable(1,2)", "next()",
 "next(1,2,3)", "any([],1)", "all([],1)", "ascii(1,2)", "globals(1)",
 "locals(1)", "dir(1,2)", "vars(1,2)", "delattr(1)", "setattr(1,2)",
 "hasattr(1)", "len()", "len(1,2)", "isinstance(1)", "issubclass(1)",
 "filter(1)", "reversed(1,2)", "map(1)", "repr(1,2)", "getattr(1)",
 "getattr(1,2,3,4)", "sum()", "sum(1,2,3)", "min()", "max()",
 "bool(1,2)", "dict(1,2)", "float(1,2)", "frozenset(1,2)", "list(1,2)",
 "set(1,2)", "tuple(1,2)", "memoryview(1,2)", "memoryview(1)",
 "type(1,2)", "type()", "slice()", "slice(1,2,3,4)", "sorted([],1)",
 "sorted([1],2,3)", "complex(1,2,3)", "property(1,2,3,4,5)",
 "bytes(1,2,3,4)", "bytearray(1,2,3,4)",
 # ...and the ones that name a type or a count rather than a rule
 "next(1)", "all(1)", "any(1.5)", "enumerate([],1.5)", "enumerate([],'x')",
 "ord(0)", "ord('')", "ord(b'')", "ord([])", "ord(b'ab')", "ord(None)",
 "int(1,'x')", "int('x',2.5)", "str(b'x',None)", "str(b'x',0)",
 "dict([1])", "dict([(1,2,3)])", "format(1,0)", "format('a','x')",
 "format(1+2j,'d')", "format(1,'','')", "format()",
 "isinstance(1,(1,))", "issubclass(int,(1,))",
 "BaseExceptionGroup('m','')", "BaseExceptionGroup('m','x')",
 "BaseExceptionGroup('m',0)", "sum([1,'a'])", "sum([{},'a'])",
 # ...and the answers that were wrong rather than badly worded
 "ord(b'x')", "ord(bytearray(b'x'))", "float(b'1.5')",
 "float(bytearray(b'2.5'))", "float(b'x')", "sorted([1],key=str)",
 "dict([iter([1,2])])", "list(enumerate([9],2))",
 "isinstance(1,((str,),(int,)))", "issubclass(bool,((str,),(int,)))",
 "isinstance(1,(str,int|bytes))", "BaseExceptionGroup('m',range(2))",
 # ...and the ones the second pass over the sweep's answers turned up
 "abs(None)", "abs([])", "len(None)", "len(1.5)", "len(object())",
 "set(None)", "set(1)", "frozenset(1.5)", "min(None,None)", "max({},{})",
 "sorted([None,1])", "bytes(None)", "bytes(1.5)", "bytes(object())",
 "bytearray(None)", "bytearray(1+2j)", "bytes(2**70)", "bytearray(2**70)",
 "bytearray(-(2**70))", "chr(2**70)", "chr(-2**70)", "chr(2**35)",
 "int(bytearray(b'x'))", "int(bytearray(b'1x'),16)",
 "issubclass((),())", "issubclass(1,(int,))", "issubclass(int,())",
 "enumerate(True,True)", "enumerate(1,2)",
 "format(True)", "format(True,'>5')", "format(True,'d')", "format(False)",
]
for expr in MESSAGES:
    try:
        print(expr, "->", repr(eval(expr))[:60])
    except BaseException as exc:
        print(expr, "->", type(exc).__name__, exc)
