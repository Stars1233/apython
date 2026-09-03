# A failing UNPACK_SEQUENCE released the sequence twice.
#
# The handler pops the sequence off the value stack with VPOP_VAL, which
# lowers the stack pointer but leaves the slot holding it -- and the unwinder
# restores the stack pointer from eval_saved_r13, the value it had *before*
# the instruction, so that slot is inside the range the unwind releases.  The
# count-mismatch path released it as well.  A sequence still named by a live
# variable was freed under it, and the next use of that variable read freed
# memory: `print(xs)` after the except block was a segfault.
#
# The iterable path already knew this and said so in a comment; the count
# mismatch, which is the ordinary way an unpack fails, did not.

xs = [1, 2, 3]
try:
    p, q = xs
except ValueError as e:
    print(e)
print(xs)

ys = "xyz"
try:
    p, q = ys
except ValueError as e:
    print(e)
print(ys)

ts = (1, 2, 3)
try:
    p, q = ts
except ValueError as e:
    print(e)
print(ts)

# Too few, as well as too many.
short = [1]
try:
    p, q = short
except ValueError as e:
    print(e)
print(short)

# A set and a range go through the materialising path, which was already
# right; they are here so the two stay together.
try:
    p, q = {1, 2, 3}
except ValueError as e:
    print(e)

r = range(5)
try:
    p, q = r
except ValueError as e:
    print(e)
print(list(r))


# The constants survive being unpacked badly in a loop, which is what drove
# a co_consts entry's refcount to zero.
def unpack_badly():
    try:
        a, b = "abc"
    except ValueError:
        pass
    return "abc"


for _ in range(5):
    print(unpack_badly())

# And the sequence is still released when the unpack succeeds.
ok = [1, 2]
p, q = ok
print(p, q, ok)
