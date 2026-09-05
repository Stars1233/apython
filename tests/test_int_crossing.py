# Integers crossing the two boundaries this representation has:
#
#   +-2**50   an immediate stops fitting and becomes a heap PyIntObject
#   +-2**63   the heap object's compact int64 stops fitting and needs GMP
#
# Nothing in the suite drove an accumulator through the second one, and
# int_add and int_sub segfaulted when it happened:
#
#     s = 0
#     for _ in range(20000):
#         s = s + 2**49          # SIGSEGV once s passed 2**63
#
# .gmp_path is reached both from the tag checks, where ecx is the right
# operand's tag, and from the overflow `jo`, where rcx had just been clobbered
# with its payload.  The payload was then saved as if it were the tag, the
# operand was never promoted, and its raw integer value was dereferenced as a
# pointer.  int_mul already guarded against this; add and sub did not.
#
# TAG_SMALLINT is 1, so a step whose low 32 bits happened to equal 1 passed the
# broken comparison -- `s += 1` worked and everything else crashed.  The step
# values below are chosen to cover that: 2**49, 2**49+1, 2**49+2, 2**49+3.

STEPS = [
    2 ** 49,
    2 ** 49 + 1,
    2 ** 49 + 2,
    2 ** 49 + 3,
    2 ** 50 - 1,
    2 ** 50,
    2 ** 50 + 1,
    (2 ** 62) // 3,
    1,
    7,
    -(2 ** 49),
    -(2 ** 49) - 3,
]


def climb(step, n, op):
    s = 0
    i = 0
    while i < n:
        if op == "add":
            s = s + step
        elif op == "sub":
            s = s - step
        elif op == "iadd":
            s += step
        elif op == "isub":
            s -= step
        i += 1
    return s


# --- drive an accumulator through both boundaries, each way ----------------
for step in STEPS:
    for op in ("add", "sub", "iadd", "isub"):
        print(op, step, climb(step, 40000, op))


# --- multiplication climbing out of int64 ----------------------------------
def climb_mul(base, n):
    s = 1
    for _ in range(n):
        s = s * base
    return s


for base in (2, 3, 7, -2, -3, 10):
    for n in (10, 20, 40, 60, 80):
        print("mul", base, n, climb_mul(base, n))


# --- the exact crossing values, computed rather than folded ----------------
def add(a, b):
    return a + b


def sub(a, b):
    return a - b


def mul(a, b):
    return a * b


EDGES = [
    2 ** 49, 2 ** 50 - 1, 2 ** 50, 2 ** 50 + 1,
    2 ** 62, 2 ** 63 - 1, 2 ** 63, 2 ** 63 + 1, 2 ** 64,
    -(2 ** 50), -(2 ** 50) - 1, -(2 ** 63), -(2 ** 63) + 1, -(2 ** 63) - 1,
    0, 1, -1,
]

for a in EDGES:
    for b in (1, -1, 2, -2, 2 ** 49, -(2 ** 49), 2 ** 62, -(2 ** 62)):
        print(add(a, b), sub(a, b), mul(a, b))

# both operands already heap objects
for a in EDGES:
    for b in EDGES:
        print(add(a, b), sub(a, b))

# --- the results must still be usable as ints ------------------------------
vals = []
for a in EDGES:
    for b in (3, -3, 2 ** 49):
        v = add(a, b)
        vals.append(v)
        print(v, v == a + b, v > 0, bool(v), hash(v) == hash(a + b))

print(sum(vals))
print(sorted(set(vals))[:5])
print(len({v: i for i, v in enumerate(vals)}))
print(repr(max(vals)), repr(min(vals)))
print(str(max(vals)) == repr(max(vals)))
print(int(str(max(vals))) == max(vals))
print(-max(vals), abs(min(vals)))
print(max(vals) // 7, max(vals) % 7, divmod(max(vals), 7))
print(float(2 ** 62) == float(2 ** 62))
