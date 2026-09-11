# {**mapping} takes any mapping, not only a dict.
#
# `{**m, 'k': v}` compiles to BUILD_MAP + DICT_UPDATE, and op_dict_update
# gated on REQUIRE_DICT_TYPE and inlined its own dense walk -- so
# `{**os.environ, 'PYTHONIOENCODING': 'utf-8'}`, which is how CPython's own
# test suite builds a child environment, was "dict.update() argument must be a
# dict".
#
# The other half of the same protocol, DICT_MERGE (the `f(**m)` spelling), was
# already fixed to accept anything with keys(); dict.update itself has decided
# "a real dict takes the fast walk, anything with keys() is read through keys()
# and indexing" in one place all along.  The opcode just never asked it.

import sys


class Mapping:
    """The minimum: keys() and __getitem__, which is what CPython requires."""

    def __init__(self, pairs):
        self._pairs = dict(pairs)

    def keys(self):
        return self._pairs.keys()

    def __getitem__(self, key):
        return self._pairs[key]


print({**Mapping({"a": 1, "b": 2})}, "a mapping alone")
print({**Mapping({"a": 1}), "b": 2}, "a mapping and a literal")
print({"b": 2, **Mapping({"a": 1})}, "a literal and a mapping")
print({**Mapping({"a": 1}), **Mapping({"b": 2})}, "two mappings")
print({**{"a": 1}, **Mapping({"b": 2})}, "a dict and a mapping")

# A later key wins, whichever kind it came from.
print({**Mapping({"a": 1}), "a": 9}, "a literal overrides the mapping")
print({"a": 9, **Mapping({"a": 1})}, "the mapping overrides the literal")


# A dict subclass takes the fast path and must still work.
class SubDict(dict):
    pass


print({**SubDict(a=1), "b": 2}, "a dict subclass")

# os.environ is the shape that made this matter.
import os

env = {**os.environ, "APYTHON_PROBE": "1"}
print(env["APYTHON_PROBE"], "os.environ splats")
print(all(isinstance(k, str) for k in env), "and its keys survive")

# What is not a mapping still says so.
for bad in (5, [1, 2], None, "ab"):
    try:
        {**bad}
        print("NO ERROR", bad)
    except TypeError as e:
        print(type(bad).__name__, "->", type(e).__name__)


# A mapping whose keys() raises propagates that, rather than reporting a type.
class AngryKeys:
    def keys(self):
        raise ValueError("no keys")

    def __getitem__(self, key):
        return 1


try:
    {**AngryKeys()}
    print("NO ERROR", "AngryKeys")
except ValueError as e:
    print("keys() raising propagates:", e)


# And one that lists a key it will not hand over.
class Liar:
    def keys(self):
        return ["a"]

    def __getitem__(self, key):
        raise KeyError(key)


try:
    {**Liar()}
    print("NO ERROR", "Liar")
except KeyError as e:
    print("a missing key propagates:", type(e).__name__)
