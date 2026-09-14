# itertools.compress and itertools.batched.
#
# lib/itertools.py stands in for CPython's C module, and a stand-in that is
# missing a name does not degrade -- it is an AttributeError in code that has
# no reason to expect one.  These two were the only names absent.
#
# The test runs against whichever itertools is on the path: apython's own when
# there is no stdlib, and CPython's C module under python3, which is what makes
# it an oracle rather than a restatement.
import itertools

# --- compress ---------------------------------------------------------------

print("basic:", list(itertools.compress("ABCDEF", [1, 0, 1, 0, 1, 1])))
print("all off:", list(itertools.compress("ABC", [0, 0, 0])))
print("all on:", list(itertools.compress("ABC", [1, 1, 1])))
print("empty data:", list(itertools.compress("", [1, 1])))
print("empty selectors:", list(itertools.compress("ABC", [])))

# The shorter of the two ends it, in either direction.
print("short selectors:", list(itertools.compress("ABCDEF", [1, 1])))
print("short data:", list(itertools.compress("AB", [1, 1, 1, 1])))

# Truthiness, not equality to 1.
print("truthy:", list(itertools.compress("ABCDE", ["", "x", None, [0], 0])))

# Both arguments are iterators, consumed lazily and only as far as needed.
data = iter("ABCDEFGH")
sel = iter([1, 0, 1])
print("lazy:", list(itertools.compress(data, sel)))
print("data left:", list(data))

# It is an iterator, not a list: consuming it twice gives nothing the second
# time.
c = itertools.compress("ABC", [1, 1, 1])
print("first pass:", list(c), "second pass:", list(c))

# Generators on both sides.
print("generators:", list(itertools.compress((x * 2 for x in range(5)),
                                             (i % 2 for i in range(5)))))

try:
    list(itertools.compress("ABC"))
except TypeError:
    print("compress needs two arguments")

# --- batched ----------------------------------------------------------------

print("batched 3:", [list(b) for b in itertools.batched("ABCDEFG", 3)])
print("batched exact:", [list(b) for b in itertools.batched("ABCDEF", 3)])
print("batched 1:", [list(b) for b in itertools.batched("ABC", 1)])
print("batched big:", [list(b) for b in itertools.batched("ABC", 10)])
print("batched empty:", [list(b) for b in itertools.batched("", 3)])

# The batches are TUPLES, which is what makes them usable after the next one
# is taken -- unlike groupby's groups.
batches = list(itertools.batched(range(7), 3))
print("batch types:", sorted({type(b).__name__ for b in batches}))
print("batches kept:", batches)

# Lazy in its source.
src = iter(range(100))
first = next(iter(itertools.batched(src, 4)))
print("first batch:", first)

# n must be at least one.
for bad in (0, -1):
    try:
        list(itertools.batched("ABC", bad))
    except ValueError as e:
        print("batched rejects %r:" % bad, "n must be at least one" in str(e))

try:
    list(itertools.batched("ABC", "3"))
except TypeError:
    print("batched rejects a str n")

# --- both are exported ------------------------------------------------------

print("named:", hasattr(itertools, "compress"), hasattr(itertools, "batched"))

# The recipe in the documentation, which uses both and is what most callers
# actually copy.
def roundrobin(*iterables):
    return [x for x in itertools.chain.from_iterable(itertools.zip_longest(*iterables))
            if x is not None]


print("combined:", [list(b) for b in
                    itertools.batched(itertools.compress(range(20), itertools.cycle([1, 0, 0])), 3)])
print("roundrobin:", roundrobin("ABC", "D", "EF"))

print("done")
