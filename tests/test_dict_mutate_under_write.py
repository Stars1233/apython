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

# --- a SOURCE that empties itself while it is being copied out -------------
# update(), `**mapping` and `dict(pairs)` all walk a source while calling
# dict_set on the destination, and dict_set runs the DESTINATION's __hash__
# and __eq__.  That user code can clear the source, which frees its table and
# points it at the one-slot read-only shared one -- so a bound or an entry
# pointer read once and used across the call walks freed or read-only memory,
# and the key and value already loaded out of it can be released underneath.
class Clears:
    def __init__(self, target, tag):
        self.target = target
        self.tag = tag

    def __hash__(self):
        return 7

    def __eq__(self, o):
        self.target.clear()
        return False


class Same:
    def __hash__(self):
        return 7

    def __eq__(self, o):
        return False


for n in (1, 5, 6, 20, 60):
    src = {}
    for i in range(n):
        src['s%d' % i] = i
    src[Same()] = 'x'
    for i in range(n):
        src['t%d' % i] = i
    dst = {Clears(None, 'd'): 0}
    for k in dst:
        k.target = src
    # CPython notices the source changing size and says so; what matters
    # either way is that nothing outside the two operands ends up in dst.
    try:
        dst.update(src)
        note = "ok"
    except RuntimeError as e:
        note = str(e)
    print(n, note, len(dst) <= 2 * n + 2,
          all(type(k).__name__ in ('Clears', 'Same', 'str') for k in dst))

# the same through `**`
def take(**kw):
    return len(kw)


for n in (5, 20):
    src = {}
    for i in range(n):
        src['u%d' % i] = i
    print(n, take(**src))

# --- a pair SEQUENCE that shortens itself while it is being consumed -------
# An exact list source is read where it lies rather than snapshotted, so the
# bound has to come from the list each turn.
lst = []


class K:
    def __hash__(self):
        del lst[1:]
        return 1

    def __eq__(self, o):
        return self is o


lst.append((K(), 1))
for i in range(200):
    lst.append(("key%d" % i, i))
d = {}
d.update(lst)
print(len(d))

# and one where the PAIR itself is a list that gets emptied
inner = []


class K2:
    def __hash__(self):
        del inner[:]
        return 2

    def __eq__(self, o):
        return self is o


pair = [K2(), "val"]
inner.append(pair)
holder = [pair]
d2 = {}
try:
    d2.update(holder)
except (ValueError, TypeError) as e:
    print(type(e).__name__)
print(len(d2))

# --- and the ordinary bulk forms still work -------------------------------
a = {'x': 1, 'y': 2}
b = dict(a)
b.update({'z': 3})
b.update([('w', 4)])
b.update((('v', 5),))
b.update(w=40)
print(sorted(b.items()))
big = {i: i * i for i in range(300)}
c = {}
c.update(big)
print(len(c), c[299], c == big)
print(len(dict(list(big.items()))), len(dict(tuple(big.items()))))
print(take(**{'p': 1, 'q': 2}))

# --- the same source, reached through the two opcodes ---------------------
# DICT_UPDATE (`{**a, **b}`) and DICT_MERGE (`f(**mapping)`) each walk a
# source dict from an opcode handler rather than from dict.update, and each
# had its own cached bound and entry pointer.
def build(n):
    s = {}
    for i in range(n):
        s['s%d' % i] = i
    s[Same()] = 'x'
    for i in range(n):
        s['t%d' % i] = i
    return s


def taker(**kw):
    return len(kw)


for n in (1, 5, 20):
    src = build(n)
    holder = {Clears(src, 'c'): 0}
    try:
        r = {**holder, **src}
        note = ("ok", len(r) <= 2 * n + 3)
    except RuntimeError as e:
        note = str(e)
    print("update", n, note)

for n in (1, 5, 20):
    src = build(n)
    try:
        r = taker(**src)
        note = ("ok", r <= 2 * n + 1)
    except (RuntimeError, TypeError) as e:
        note = type(e).__name__
    print("merge", n, note)

# a merge whose duplicate-key error still names the key
def two(a=None, b=None):
    return (a, b)


try:
    two(**{'a': 1}, a=2)
except TypeError as e:
    print("TypeError", e)
print(two(**{'a': 1}, b=2), two(**{'a': 1, 'b': 2}))
print({**{'m': 1}, **{'n': 2}}, {**{}, **{}})
