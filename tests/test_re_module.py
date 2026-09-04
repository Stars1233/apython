# The `re` module itself, rather than the engine under it.
#
# apython ships `re/` now.  It used to ship only `_sre`, so `import re` found
# CPython's own if a real stdlib happened to be on PYTHONPATH and raised
# ModuleNotFoundError otherwise -- and the module needs `enum`, `types`,
# `reprlib`, `copyreg` and a `functools` with lru_cache in it, none of which
# were here either.
#
# The engine's own answers are compared over nine hundred patterns by
# tests/re_differential.py; what this file is about is that the wrapper works
# at all: compile, the module-level functions, flags, groups and the objects.

import re

print(re.match(r"a+", "aaa").span())
print(re.search(r"\d+", "abc123def").group())
print(re.fullmatch(r"[a-z]+", "abc") is not None)
print(re.findall(r"\d+", "a1b22c333"))
print(re.split(r"[,;]\s*", "a, b;c"))
print(re.sub(r"(\w+)@(\w+)", r"\2:\1", "user@host"))
print(re.subn("a", "b", "banana"))

rx = re.compile(r"(?P<word>\w+)\s+(?P<rest>.*)")
m = rx.match("hello there world")
print(m.group(0), "|", m.group("word"), "|", m.group("rest"))
print(m.groupdict())
print(m.span("word"), m.start(), m.end())
print(rx.pattern, rx.groups, rx.groupindex)

print(re.IGNORECASE == re.I, bool(re.match("ABC", "abc", re.I)))
print(re.compile("a.c", re.DOTALL).match("a\nc") is not None)
print([m.group() for m in re.finditer(r"\w+", "one two three")])
print(re.escape("a.b*c?"))

# The exception type, and that it carries what CPython's carries.
try:
    re.compile("(")
except re.error as e:
    print(type(e).__name__, e.msg, e.pos)

try:
    re.sub("a", r"\g<9>", "a")
except re.error as e:
    print(type(e).__name__, "|", e)

# The cache, and that a compiled pattern is reused.
print(re.compile("xyz") is re.compile("xyz"))

# Bytes patterns go through the same wrapper.
print(re.match(rb"\d+", b"123").group())
print(re.sub(rb"a", rb"b", b"aaa"))

# And the pieces re needs, which are shipped with it.
import enum
import types
import reprlib
import copyreg
import functools

print(issubclass(re.RegexFlag, enum.IntFlag))
print(type(types.MappingProxyType({}) ) is types.MappingProxyType)
print(reprlib.repr(list(range(100)))[:20])
print(callable(copyreg.pickle), callable(functools.lru_cache))


@functools.lru_cache(maxsize=8)
def slow(n):
    return n * n


print([slow(i) for i in range(4)], slow.cache_info().hits == 0)
print(slow(2), slow.cache_info().hits)
