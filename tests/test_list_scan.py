# index, count, remove and list == list answer two questions about a Value
# before they call obj_richcompare_bool: identical Values are equal, and two
# int immediates that are not the same Value are not.  `x in list` has had
# those arms since it was three times CPython's speed; these four did not.
#
# Everything that is NOT settled by an arm has to fall through and be compared
# properly, and that is what this file is for: a bool is a heap singleton, a
# heap integer is outside the immediate range, a float equal to an int is
# equal across two encodings, and a class with its own __eq__ must still have
# it called.


class E:
    def __init__(self, v):
        self.v = v

    def __eq__(self, o):
        return isinstance(o, E) and self.v == o.v


class Loud:
    def __eq__(self, o):
        print("  Loud.__eq__")
        return True


BIG = 2 ** 60

ints = list(range(20))
print(ints.index(0), ints.index(19), ints.count(7), ints.count(20))
print([ints.count(i) for i in (-1, 0, 19, 20)])

# across the encodings: a bool is a singleton, a float is a different one
mixed = [1, True, 1.0, 0, False, 0.0]
for probe in (1, True, 1.0, 0, False, 0.0, 2):
    print(repr(probe), mixed.count(probe),
          mixed.index(probe) if probe in mixed else -1)

# heap integers are never immediates, so neither arm applies
heap = [BIG, BIG + 1, -BIG, 10 ** 30]
print(heap.index(BIG), heap.index(BIG + 1), heap.count(-BIG),
      heap.count(10 ** 30), heap.count(BIG + 2))

# a class with __eq__ still gets it called, and by identity too
a, b = E(1), E(1)
objs = [a, E(2), b]
print(objs.index(a), objs.index(b), objs.count(E(1)), objs.count(E(9)))

# __eq__ runs, and on the element the list holds
print([1, 2, 3].count(Loud()))
print([Loud(), 2].index(1))

# --- remove ---------------------------------------------------------------
r = list(range(10))
r.remove(0)
r.remove(9)
r.remove(5)
print(r)
r2 = [1, True, 1.0]
r2.remove(True)
print(r2)
try:
    [1, 2, 3].remove(4)
except ValueError as e:
    print("ValueError", e)
r3 = [BIG, BIG]
r3.remove(BIG)
print(len(r3), r3[0] == BIG)

# --- index with start and stop -------------------------------------------
dup = [1, 2, 1, 2, 1]
print(dup.index(1), dup.index(1, 1), dup.index(2, 2), dup.index(1, -2))
try:
    dup.index(1, 1, 2)
except ValueError as e:
    print("ValueError")
try:
    [1, 2, 3].index(9)
except ValueError as e:
    print("ValueError", e)

# --- list == list ---------------------------------------------------------
# the length shortcut for == and != must not change what any operator answers
print([1, 2] == [1, 2], [1, 2] == [1, 2, 3], [1, 2] != [1, 2, 3])
print([] == [], [] == [1], [] != [], [1] == [])
print([1, 2] < [1, 2, 3], [1, 2, 3] < [1, 2], [1, 2] <= [1, 2])
print([1, 2] > [1, 2, 3], [1, 2, 3] > [1, 2], [1, 2] >= [1, 2])
print([2] < [1, 2, 3], [1, 2, 3] < [2])
print([1, True] == [1, 1], [1, 1.0] == [1, 1], [BIG] == [BIG], [BIG] == [BIG + 1])
print([E(1)] == [E(1)], [E(1)] == [E(2)], [a] == [a])
print([1, 2] == (1, 2), [1, 2] == "12", [1, 2] == 3)

# a list compared with itself, which the identity arm settles per element
same = [E(1), E(2), object()]
print(same == same, same == list(same), same != same)

# nested lists, so the element comparison recurses
n1 = [[1, 2], [3, 4]]
n2 = [[1, 2], [3, 4]]
print(n1 == n2, n1 == [[1, 2], [3, 5]], n1 < [[1, 2], [3, 5]])

# --- the mutation cases the loops re-read ob_size for ---------------------
class Clears:
    def __init__(self, target):
        self.target = target

    def __eq__(self, o):
        del self.target[:]
        return True


la = []
lb = []
la.append(Clears(lb))
lb.append(Clears(la))
print(la == lb)

# an __eq__ that shortens the list being counted
class Pops:
    def __init__(self, target):
        self.target = target
        self.n = 0

    def __eq__(self, o):
        self.n += 1
        if self.n == 1 and self.target:
            self.target.pop()
        return False


victim = [1, 2, 3]
probe = Pops(victim)
print(victim.count(probe), len(victim))
