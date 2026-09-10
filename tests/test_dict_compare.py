# Comparing two dicts runs user code -- the values' __eq__, and any __del__ a
# released value has -- and that user code may empty either dict.
#
# dict_richcompare read the key and both values straight out of the tables and
# held no reference to any of them, and it cached the left dict's capacity
# before the loop.  An __eq__ that clears the dict being compared therefore
# freed the entries array out from under the walk and left the comparison
# holding three dangling pointers; CPython's own test_dict has exactly that
# test, twice, because CPython segfaulted on it too (bpo-27945, bpo-38588).
#
# It also compared the values by hand rather than through the ordinary
# comparison, so two values that ARE equal but are not the same kind of number
# came out unequal.

# --- values that are equal without being the same shape -------------------
print({1: 1} == {1: 1.0})
print({1: 1.0} == {1: 1})
print({1: True} == {1: 1})
print({1: False} == {1: 0})
print({1: 2} == {1: 2})
print({1: 10**30} == {1: 10**30})
print({1: 10**30} == {1: float(10**30)})
print({"a": 1, "b": 2} == {"b": 2, "a": 1})
print({1: 1} == {1: 2})
print({1: 1} != {1: 2})
print({1: 1} == {2: 1})

# --- an __eq__ that empties the dicts being compared -----------------------
class X:
    def __del__(self):
        dict_b.clear()

    def __eq__(self, other):
        dict_a.clear()
        return True

    def __hash__(self):
        return 13


dict_a = {X(): 0}
dict_b = {X(): X()}
print("mutating eq:", dict_a == dict_b)

# --- an __eq__ that empties only the right operand -------------------------
class Y:
    def __eq__(self, other):
        dict_d.clear()
        return True


dict_c = {0: Y()}
dict_d = {0: set()}
print("clears right:", dict_c == dict_d)

# --- an __eq__ that GROWS the left operand while it is being walked --------
class Z:
    def __eq__(self, other):
        for i in range(50):
            grow[("k", i)] = i
        return True


grow = {0: Z()}
other = {0: Z()}
print("growing eq:", grow == other, len(grow))

# --- a raising __eq__ propagates rather than answering False ---------------
class Boom:
    def __eq__(self, other):
        raise RuntimeError("boom")

    def __hash__(self):
        return 3


try:
    print({1: Boom()} == {1: Boom()})
except RuntimeError as e:
    print("raised:", e)

# The identity shortcut comes first, so a broken __eq__ on the SAME object is
# never consulted.
b = Boom()
print("identity:", {1: b} == {1: b})
print("done")
