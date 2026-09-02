# list.extend over a generic iterable answers None, not a NULL value.
#
# The generic-iterable arm ended with EXC_RAISED_SINCE, whose whole point is
# to jump when something is pending -- and then fell straight through into the
# very label it jumps to.  So every extend from anything that is not a list or
# a tuple returned a NULL Value with no exception pending.  Nothing noticed for
# as long as every caller discarded the result: `l.extend(s)` as a statement
# pops it, and `r = l.extend(s)` simply never bound r.

l = []
print(l.extend({"a"}))
print(sorted(l))

l = [1]
r = l.extend(iter([2, 3]))
print(r, l)

l = []
l.extend(x * 2 for x in range(4))
print(l)

l = []
l.extend(range(3))
print(l)

l = ["a"]
print(l.extend("bc"), l)

l = []
print(l.extend({"k": 1}), l)

# The list and tuple fast paths kept answering None all along.
l = [0]
print(l.extend([1, 2]), l.extend((3, 4)), l)

# A __getitem__ that raises partway still propagates.
class Boom:
    def __getitem__(self, i):
        if i == 2:
            raise ValueError("boom")
        return i
l = []
try:
    l.extend(Boom())
    print("no error")
except ValueError as e:
    print("ValueError:", e, l)
