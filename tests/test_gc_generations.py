# Cross-generation references while the collector is running.
#
# The collector borrows each young object's gc_prev field to hold a refcount
# copy for the duration of a collection.  Objects in older generations keep a
# live list pointer in that same field, and a young object's traversal reaches
# them: if the two uses are not told apart, an older object's list pointer is
# decremented and the heap is quietly corrupt long before anything notices.
#
# There is no gc module here, so the collections are the automatic ones -- the
# churn below is sized to cross the gen0 threshold many times, which is what
# promotes the survivors into gen1 and gen2.

survivors = []
for i in range(60):
    survivors.append([i, (i, i + 1), {"k": i}])

for round in range(60):
    fresh = []
    for i in range(80):
        t = (round, i, tuple(range(i % 5)))
        fresh.append({"t": t, "l": [t, t]})
    # Old objects reference new ones, and new ones reference old.
    for j, o in enumerate(survivors):
        o.append(fresh[j % len(fresh)])
        fresh[j % len(fresh)]["back"] = o
    for o in survivors:
        del o[3:]

# Cycles: the part of the collector that actually frees anything.
for round in range(200):
    a = {}
    b = {"a": a}
    a["b"] = b
    a["self"] = a
    c = [None]
    c[0] = c
    del a, b, c

print(len(survivors), sum(len(o) for o in survivors))
print(sorted(survivors[0][2].items()), survivors[59][1])
print(survivors[7][0])
