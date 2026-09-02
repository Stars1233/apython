# A specialized opcode that deopts must not lose a preceding EXTENDED_ARG.
#
# FOR_ITER rewrites itself to FOR_ITER_LIST the first time it sees a list
# iterator.  When a later pass hands it something else, the specialized
# handler has to hand back to the generic one -- and it used to do that by
# rewinding the instruction pointer two bytes and re-dispatching.  Two bytes
# is one instruction word, so a FOR_ITER whose jump offset needs an
# EXTENDED_ARG prefix came back without it: the offset was read as its own low
# byte, and the exhausted branch jumped into the middle of its own loop body
# instead of past it.
#
# It needs a loop body over 255 instruction words for the prefix to exist at
# all, which is why this generates the function rather than writing it out.

BODY = "\n".join("        x%d = i" % k for k in range(200))
SRC = (
    "def walk(seq):\n"
    "    seen = []\n"
    "    for i in seq:\n"
    "%s\n"
    "        seen.append(i)\n"
    "    return seen\n"
) % BODY

ns = {}
exec(SRC, ns)
walk = ns["walk"]


class Seq:
    """__getitem__ and no __iter__: the sequence-protocol iterator."""

    def __init__(self, data):
        self.data = data

    def __len__(self):
        return len(self.data)

    def __getitem__(self, i):
        return self.data[i]


# A list first, so the loop specializes; then everything that has to deopt.
print(walk([1, 2, 3]))
print(walk(Seq([4, 5])))
print(walk(Seq([])))
print(walk([]))
print(walk((6, 7)))
print(walk(iter([8, 9])))
print(walk({10: None}))
print(walk(range(3)))
print(walk([11]))

# The other order: a range first, so it specializes to the range form.
ns2 = {}
exec(SRC, ns2)
walk2 = ns2["walk"]
print(walk2(range(2)))
print(walk2(Seq([])))
print(walk2([12, 13]))
print(walk2(Seq([14])))

# And a generator, whose exhaustion arrives as a NULL from tp_iternext.
def gen(n):
    for k in range(n):
        yield k * 2

ns3 = {}
exec(SRC, ns3)
walk3 = ns3["walk"]
print(walk3([0]))
print(walk3(gen(3)))
print(walk3(gen(0)))
