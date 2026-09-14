# decompressobj(max_length=...) and the unconsumed_tail loop.
#
# The core parks whatever libz did not consume and prepends it to the next
# feed.  That is right for _ZlibDecompressor, which holds its input and
# reports `needs_input`; it is wrong for zlib.Decompress, where CPython hands
# the leftover back as `unconsumed_tail` and the CALLER feeds it in again.
# Doing both meant the input doubled every round -- 315 bytes, then 630, then
# 1260, from a 356-byte stream -- until the process died with "Fatal: out of
# memory".  CPython's own test_decompimax is that loop.
#
# It is a shape a small test misses twice over: it needs a stream long enough
# that max_length actually stops libz short, and it needs more than two
# rounds, since the first is correct and the second only looks wasteful.
import zlib

SCENE = (b"LAERTES\n   O, fear me not.\n   I stay too long: but here my "
         b"father comes.\nA double blessing is a double grace,\n" * 64)


def stream(data, chunk=256):
    co = zlib.compressobj()
    out = [co.compress(data[i:i + chunk]) for i in range(0, len(data), chunk)]
    out.append(co.flush())
    return b"".join(out)


comp = stream(SCENE)
print("compressed smaller:", len(comp) < len(SCENE))
print("one shot:", zlib.decompress(comp) == SCENE)

# --- the loop CPython documents ---------------------------------------------

dco = zlib.decompressobj()
pieces = []
tails = []
cb = comp
rounds = 0
while cb:
    chunk = dco.decompress(cb, 64)
    if len(chunk) > 64:
        print("chunk too big:", len(chunk))
        break
    pieces.append(chunk)
    cb = dco.unconsumed_tail
    tails.append(len(cb))
    rounds += 1
    if rounds > 20000:
        print("not terminating; tail is", len(cb))
        break
pieces.append(dco.flush())
print("rounds:", rounds)
print("round trip:", b"".join(pieces) == SCENE)

# The tail never grows: that is the whole defect, stated directly.
print("tail never grows:", all(b <= a for a, b in zip(tails, tails[1:])))
print("tail bounded by input:", max(tails) <= len(comp))
print("tail ends empty:", tails[-1] == 0)

# --- max_length is a cap, not a promise -------------------------------------

print()
for cap in (1, 2, 7, 64, 1000, len(SCENE) * 2):
    dco = zlib.decompressobj()
    first = dco.decompress(comp, cap)
    print("cap %-7d got %-7d within %s" % (cap, len(first), len(first) <= cap))

# A max_length of 0 means no cap at all.
dco = zlib.decompressobj()
print("no cap:", dco.decompress(comp, 0) == SCENE)
print("no cap tail:", dco.unconsumed_tail == b"")

# --- feeding nothing ---------------------------------------------------------

print()
dco = zlib.decompressobj()
dco.decompress(comp, 64)
held = dco.unconsumed_tail
print("held:", len(held) > 0)
print("empty feed:", dco.decompress(b"", 64))
print("tail after empty feed:", dco.unconsumed_tail)
# The leftover is the caller's now, and feeding it back still works.
rest = [dco.decompress(held)]
rest.append(dco.flush())
print("resumed:", len(b"".join(rest)) > 0)

# --- flush() does inflate what is left ---------------------------------------
#
# CPython's Decompress.flush() uses unconsumed_tail as its input, so a caller
# that stops mid-loop and flushes gets the remainder rather than b"".

print()
dco = zlib.decompressobj()
head = dco.decompress(comp, 64)
tail = dco.flush()
print("flush finished it:", head + tail == SCENE)

# --- the same stream through _ZlibDecompressor, which holds its input --------

print()
zd = zlib._ZlibDecompressor()
got = []
got.append(zd.decompress(comp, 64))
print("needs_input after a capped read:", zd.needs_input)
while not zd.eof:
    got.append(zd.decompress(b"", 64))
print("holder round trip:", b"".join(got) == SCENE)
print("eof:", zd.eof)

print("done")
