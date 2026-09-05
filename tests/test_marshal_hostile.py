# marshal.loads() is handed bytes the program chose, and they need not be a
# marshal stream at all.
#
# Every refusal in the reader used to call fatal_error, which prints and
# exits: marshal.loads(b"") did not raise EOFError, it ended the process.
# That is right for a .pyc, which is read before there is an interpreter
# frame to raise into, and wrong for a call inside a running program.  And
# the reader recurses once per level of nesting, with nothing watching the
# machine stack, so a stream of two hundred thousand open parentheses walked
# it off the end.
#
# What follows is the shape of the refusals, not their timing: CPython and
# this reader stop at slightly different points in a corrupt stream, so the
# fuzz section counts survivors rather than comparing verdicts.

import marshal

# A stream that CPython's own marshal.dumps produced, so the good path is
# pinned too.  It cannot be built here: this implementation reads marshal and
# does not write it.
GOOD = (b')\x08\xe9\x01\x00\x00\x00\xda\x03two\xe7\x00\x00\x00\x00\x00\x00\x08@'
        b'[\x01\x00\x00\x00\xe9\x04\x00\x00\x00{\xe9\x05\x00\x00\x00'
        b'\xf3\x03\x00\x00\x00six0NT\xa9\x00')

print("roundtrip", marshal.loads(GOOD))

# Truncated where an object was due, and truncated in the middle of one.
for b in (b"", b"\xe9", b"(", b"\xdb", b"c", b"c\x01\x00\x00\x00"):
    try:
        print("%-22r -> %r" % (b, marshal.loads(b)))
    except Exception as e:
        print("%-22r -> %s: %s" % (b, type(e).__name__, e))

# Every prefix of a valid stream, which is every way it can stop early.
short = 0
for i in range(len(GOOD)):
    try:
        marshal.loads(GOOD[:i])
    except EOFError:
        short += 1
    except Exception as e:
        print("prefix %d -> %s" % (i, type(e).__name__))
print("prefixes refused as short:", short, "of", len(GOOD))

# Nesting is what the recursion spends, and a stream may ask for as much of
# it as it likes.
deep = b"(\x01\x00\x00\x00" * 200000 + b"N"
try:
    print("deeply nested ->", type(marshal.loads(deep)).__name__)
except Exception as e:
    print("deeply nested ->", type(e).__name__)

# Random bytes, from a generator written here so that both interpreters see
# the same ones.  What is compared is that there is a line to print at the
# end: the two readers give up at different points, so their verdicts are not
# the same and are not counted.
state = 7


def rand(n):
    global state
    state = (state * 6364136223846793005 + 1442695040888963407) % (1 << 64)
    return (state >> 33) % n


survived = 0
for _ in range(400):
    b = bytes(rand(256) for _ in range(rand(39) + 1))
    try:
        marshal.loads(b)
    except Exception:
        pass
    survived += 1
print("fuzz: still here after", survived)

# ...and it still reads what it should.
print("roundtrip again", marshal.loads(GOOD))
print("done")
