# `{**m}` over a non-dict mapping must RELEASE the mapping.
#
# DICT_UPDATE pops its source with an owned reference.  The dense walk, which
# is what a real dict takes, decrefs it on the way out; the arm added for
# arbitrary mappings -- the one that makes `{**os.environ}` work -- never did,
# so every `{**m}` over anything but a dict pinned the mapping forever.  Five
# of them took a refcount from 2 to 7 where CPython leaves it at 2.
#
# The error paths leaked the same way: the message names the source's TYPE and
# is composed into a static buffer, so the source can be, and now is, released
# before the raise.
#
# And a `keys` whose LOOKUP raises is not "not a mapping": obj_getattr_opt
# answers 0 for both, so a property that raises was reported as TypeError.

import sys


class Mapping:
    def keys(self):
        return ("a", "b")

    def __getitem__(self, k):
        return k.upper()


class NoKeys:
    pass


class RaisingKeysAttr:
    @property
    def keys(self):
        raise ValueError("the lookup itself raised")


class KeysRaisesWhenCalled:
    def keys(self):
        raise ValueError("the call raised")


# --- the mapping is released ------------------------------------------------
m = Mapping()
before = sys.getrefcount(m)
for _ in range(5):
    d = {**m}
print(d == {"a": "A", "b": "B"}, "the merge itself is right")
print(sys.getrefcount(m) - before, "extra references after five merges")

# A real dict still works and still releases.
src = {"x": 1}
before = sys.getrefcount(src)
for _ in range(5):
    d = {**src, "y": 2}
print(d == {"x": 1, "y": 2}, sys.getrefcount(src) - before, "a dict source too")


# --- the error paths release it as well -------------------------------------
def refcount_after(fn, obj):
    before = sys.getrefcount(obj)
    for _ in range(5):
        try:
            fn()
        except BaseException:
            pass
    return sys.getrefcount(obj) - before


nk = NoKeys()
print(refcount_after(lambda: {**nk}, nk), "extra after five refusals")

# --- and each says the right thing ------------------------------------------
try:
    {**NoKeys()}
    print("NO ERROR for an object with no keys()")
except TypeError as e:
    print("TypeError:", e)

try:
    {**42}
    print("NO ERROR for an int")
except TypeError as e:
    print("TypeError:", e)

try:
    {**RaisingKeysAttr()}
    print("NO ERROR for a keys attribute that raises")
except ValueError as e:
    print("ValueError:", e)

try:
    {**KeysRaisesWhenCalled()}
    print("NO ERROR for a keys() that raises")
except ValueError as e:
    print("ValueError:", e)
