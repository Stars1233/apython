"""A non-ASCII subject is decoded once per scan, not once per match.

The engine indexes code points, so a non-ASCII subject is decoded to u32
before matching -- O(len).  A scanner holds that decode and finditer pays it
once; a hand-written `while m := p.search(s, pos)` loop had nothing to hold it
and paid per call, which is quadratic over the subject.  json.decoder is
written exactly that way.

The correctness half matters more than the speed half: the cache is keyed on
the subject's ADDRESS, so anything that lets a freed string's reused address
pass for a hit would index a stale decode and answer from the wrong text.
Every case below alternates subjects, drops them, and rebuilds them at what is
very likely the same address.

The growth check is a ratio, not a threshold: what was wrong was the SHAPE of
the curve, and a wall-clock limit would only measure this machine.
"""

import re
import time

ACCENT = "é"


def scan(p, s):
    """The loop that had no scanner to cache anything."""
    out, pos = [], 0
    while True:
        m = p.search(s, pos)
        if m is None:
            break
        out.append(m.span())
        pos = m.end() if m.end() > m.start() else m.end() + 1
    return out


print("--- the same answers as finditer ---")
p = re.compile("a")
subj = (ACCENT * 4 + "a") * 20
print("scan == finditer:", scan(p, subj) == [m.span() for m in p.finditer(subj)])
print("count:", len(scan(p, subj)))

print("--- alternating between two subjects ---")
s1 = (ACCENT * 3 + "x") * 10
s2 = (ACCENT * 5 + "x") * 10
px = re.compile("x")
a = scan(px, s1)
b = scan(px, s2)
for _ in range(5):
    if scan(px, s1) != a or scan(px, s2) != b:
        print("ALTERNATION WRONG")
        break
else:
    print("alternation stable:", len(a), len(b))

print("--- a subject freed and another built at the same address ---")
results = []
for i in range(6):
    tmp = (ACCENT * (i + 1) + "z") * 8
    results.append((len(tmp), scan(re.compile("z"), tmp)))
    del tmp
ok = all(len(r) == 8 for _, r in results)
print("each answered for itself:", ok)
print("spans differ by length:", len({tuple(r) for _, r in results}) == 6)

print("--- equal strings that are different objects ---")
u = ACCENT + "q" + ACCENT
v = ACCENT + "q" + ACCENT
pq = re.compile("q")
print("both found:", scan(pq, u), scan(pq, v), u == v)

print("--- ASCII is unaffected ---")
print("ascii:", scan(re.compile("b"), "abcabc"))

print("--- pos and endpos still narrow ---")
s = ACCENT * 3 + "mmm" + ACCENT * 3
pm = re.compile("m")
print("whole:", [m.span() for m in pm.finditer(s)])
print("from 4:", pm.search(s, 4).span())
print("to 5:", pm.search(s, 0, 5).span())
print("empty window:", pm.search(s, 4, 4))
print("match at pos:", pm.match(s, 3).span(), pm.match(s, 0))

print("--- the other entry points agree ---")
big = (ACCENT * 2 + "k") * 30
print("findall:", len(re.findall("k", big)))
print("split:", len(re.split("k", big)))
print("sub:", re.sub("k", "K", big).count("K"))
print("subn:", re.subn("k", "K", big)[1])
print("finditer:", sum(1 for _ in re.finditer("k", big)))

print("--- groups still carry the right text ---")
g = re.compile(r"(" + ACCENT + r")(\d+)")
subject = ACCENT + "12 " + ACCENT + "345"
print("groups:", [(m.group(1), m.group(2)) for m in g.finditer(subject)])
print("via scan:", [g.search(subject, i).group(2)
                   for i in (0, 4) if g.search(subject, i)])

print("--- the cost is linear in the subject ---")


def timed(n):
    s = (ACCENT * 4 + "a") * (n // 5)
    t = time.time()
    scan(re.compile("a"), s)
    return time.time() - t


base = timed(20000)
big_t = timed(80000)
# Quadratic would be about sixteen times; linear about four.  Eight is a
# generous ceiling that still fails the shape this test is about.
ratio = big_t / base if base > 0 else 0
print("four times the subject costs less than eight times:", ratio < 8)

print("done")
