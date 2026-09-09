# A dict write must leave the table consistent before it hands control to
# anything that can touch the dict again -- and `del d[k]` and `d[k] = v` both
# do hand control over, because releasing a value runs its __del__ and
# probing runs __hash__ and __eq__.
#
# What makes this sharp rather than theoretical is `clear()`: it hands the
# tables back and points the dict at the ONE-SLOT READ-ONLY shared table.  A
# pointer or an index read before a callback and used after it therefore does
# not merely describe a stale table -- it addresses .rodata, at an index the
# new capacity does not have.  Both cases below were a SIGSEGV.
import sys

out = []

# --- del d[k] where the value's __del__ clears the dict --------------------
for n in (1, 2, 8, 9, 20, 100):
    class D:
        def __del__(self):
            d.clear()

    d = {}
    for i in range(n):
        d['k%d' % i] = 1
    d['bomb'] = D()
    del d['bomb']
    out.append((n, len(d), 'bomb' in d))
print(out)

# the same, but the __del__ deletes another key rather than clearing
out = []
for n in (2, 9, 20):
    class D2:
        def __del__(self):
            if 'k0' in d2:
                del d2['k0']

    d2 = {}
    for i in range(n):
        d2['k%d' % i] = 1
    d2['bomb'] = D2()
    del d2['bomb']
    out.append((n, len(d2), 'k0' in d2, 'bomb' in d2))
print(out)

# and one that INSERTS during the delete, which can force a resize
out = []
for n in (2, 9, 20):
    class D3:
        def __del__(self):
            for j in range(30):
                d3['extra%d' % j] = j

    d3 = {}
    for i in range(n):
        d3['k%d' % i] = 1
    d3['bomb'] = D3()
    del d3['bomb']
    out.append((n, len(d3), d3['extra29'], 'bomb' in d3))
print(out)

# --- d[k] = v where the key's __hash__ clears the dict ---------------------
# The insert overflows the load factor, resizes, and then has to find a slot
# for a key whose __hash__ has just emptied the table under it.
out = []
for n in (0, 1, 5, 6, 20, 60):
    class K:
        def __hash__(self):
            src.clear()
            return 3

        def __eq__(self, o):
            return self is o

    src = {}
    for i in range(n):
        src['a%d' % i] = i
    src[K()] = 1
    out.append((n, len(src), list(src.values())))
print(out)

# a __eq__ that clears, reached only on a hash collision
out = []
for n in (5, 6, 20, 60):
    class C:
        def __init__(self, tag):
            self.tag = tag

        def __hash__(self):
            return 99

        def __eq__(self, o):
            victim.clear()
            return False

    victim = {}
    for i in range(n):
        victim['b%d' % i] = i
    victim[C('first')] = 'x'
    victim[C('second')] = 'y'
    out.append((n, len(victim)))
print(out)

# --- and the ordinary cases still behave ---------------------------------
d = {}
for i in range(200):
    d[i] = i * 2
for i in range(0, 200, 3):
    del d[i]
print(len(d), d[1], d[199], 0 in d, 3 in d)
for i in range(0, 200, 3):
    d[i] = -i
print(len(d), d[0], d[3], d[198], sorted(d)[:5])
d2 = dict.fromkeys(range(50), 0)
for k in list(d2):
    del d2[k]
print(len(d2), d2)
d2['after'] = 1
print(d2)

seen = []


class W:
    def __init__(self, t):
        self.t = t

    def __del__(self):
        seen.append(self.t)


def churn():
    w = {}
    for i in range(30):
        w[i] = W(i)
    for i in range(30):
        del w[i]


churn()
print(len(seen), sorted(seen) == list(range(30)))
