# `x in obj` for a class that defines __contains__.
#
# slot_table had no row for the name, so a heaptype's sq_contains was left at
# whatever it inherited: a list subclass answered list's membership test and
# never called the method, and every answer below was the opposite of what
# the class said.  A plain class fared differently and no better -- with no
# slot at all, `in` fell back to iterating, so a __contains__ that RAISED came
# out as "argument of type is not iterable" with the real exception buried.
#
# sq_contains has no error channel, only 0 or 1, which is why the raising case
# has to leave through the unwinder rather than through a return value.

class L(list):
    def __contains__(self, x): return x == 99
class D(dict):
    def __contains__(self, x): return True
class S(str):
    def __contains__(self, x): return True
class T(tuple):
    def __contains__(self, x): return True
class By(bytes):
    def __contains__(self, x): return True
class Ba(bytearray):
    def __contains__(self, x): return True
class St(set):
    def __contains__(self, x): return False
class Fs(frozenset):
    def __contains__(self, x): return False

print("list     :", 99 in L([1]), 1 in L([1]))
print("dict     :", "k" in D(), 1 in D())
print("str      :", "z" in S("abc"))
print("tuple    :", 5 in T((1,)))
print("bytes    :", 5 in By(b"a"), 5 in Ba(b"a"))
print("set      :", 1 in St({1}), 1 in Fs({1}))

print("=== not in, which is the same slot ===")
print("not in   :", 99 not in L([1]), 1 not in L([1]))

print("=== a subclass that overrides nothing is unchanged ===")
class Lp(list): pass
class Dp(dict): pass
class Sp(str): pass
print(1 in Lp([1]), 2 in Lp([1]), "k" in Dp({"k": 1}), "a" in Sp("abc"))
print(1 in [1], 2 in [1], "k" in {"k": 1}, "a" in "abc", 1 in (1,),
      97 in b"a", 1 in {1})

print("=== truthiness, not identity with True ===")
class Truthy:
    def __contains__(self, x): return [x]      # a non-empty list
class Falsy:
    def __contains__(self, x): return []       # an empty one
print(1 in Truthy(), 1 in Falsy())

print("=== and an exception is an exception ===")
class Raises:
    def __contains__(self, x): raise ValueError("boom")
try:
    1 in Raises()
except ValueError as e:
    print("raised:", e)

class RaisesList(list):
    def __contains__(self, x): raise KeyError("k")
try:
    1 in RaisesList([1])
except KeyError as e:
    print("raised:", e)

print("=== inherited from a base that defines it ===")
class Middle(list):
    def __contains__(self, x): return "middle"
class Leaf(Middle): pass
print(1 in Leaf([1]), 0 in Leaf([]))
