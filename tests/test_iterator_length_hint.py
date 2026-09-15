# __length_hint__ on the iterators that walk a sparse table.
#
# The list, tuple, str, bytes, bytearray and range iterators have had one for
# a while; the dict's three, the set's, the reversed-dict's and reversed()'s
# had none, so `operator.length_hint` of any of them answered the default and
# `list(it)` sized itself from nothing.
#
# CPython keeps a counter it decrements per next().  These iterators index a
# SPARSE entry table, so the answer is the occupied slots from the index to
# the end -- O(n) where CPython's is O(1), and called once, by a caller that
# is about to walk all n anyway.  That is what a length HINT is for.
import operator

D = {1: "a", 2: "b", 3: "c"}
S = {1, 2, 3, 4}
L = [10, 20, 30, 40, 50]

print("dict:", iter(D).__length_hint__())
print("keys:", iter(D.keys()).__length_hint__())
print("values:", iter(D.values()).__length_hint__())
print("items:", iter(D.items()).__length_hint__())
print("reversed dict:", iter(reversed(D)).__length_hint__())
print("set:", iter(S).__length_hint__())
print("reversed list:", reversed(L).__length_hint__())

# --- it counts DOWN as the iterator advances ---------------------------
it = iter(D)
seen = []
for _ in range(len(D) + 1):
    seen.append(it.__length_hint__())
    try:
        next(it)
    except StopIteration:
        break
print("as it drains:", seen)

rit = reversed(L)
seen = [rit.__length_hint__()]
for _ in L:
    next(rit)
    seen.append(rit.__length_hint__())
print("reversed drains:", seen)

# --- a dict with holes in its table ------------------------------------
# Deleting leaves tombstones, which are not entries and must not be counted.
holed = {i: i for i in range(20)}
for i in range(0, 20, 2):
    del holed[i]
hit = iter(holed)
print("after deletions:", hit.__length_hint__(), len(holed))
next(hit)
print("and after one step:", hit.__length_hint__())

# A set with holes, likewise.
hs = set(range(20))
for i in range(0, 20, 2):
    hs.discard(i)
print("set after discards:", iter(hs).__length_hint__(), len(hs))

# --- empty ---------------------------------------------------------------
print("empty:", iter({}).__length_hint__(), iter(set()).__length_hint__(),
      reversed([]).__length_hint__())

# --- through operator.length_hint, which is what callers use -------------
print("operator.length_hint:",
      operator.length_hint(iter(D)), operator.length_hint(iter(D.items())),
      operator.length_hint(iter(S)), operator.length_hint(reversed(L)))

# --- and list() still builds the right thing ----------------------------
print("list of each:", sorted(list(iter(D))), sorted(list(iter(D.values()))),
      sorted(list(iter(S))), list(reversed(L)))

# --- the ones that already had it are unchanged -------------------------
print("the sequence iterators:",
      iter([1, 2]).__length_hint__(), iter((1, 2, 3)).__length_hint__(),
      iter("abcd").__length_hint__(), iter(b"abcde").__length_hint__(),
      iter(bytearray(b"ab")).__length_hint__(),
      iter(range(7)).__length_hint__())
print("survived")
