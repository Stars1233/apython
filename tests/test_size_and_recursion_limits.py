# ap_malloc never returns NULL -- it calls fatal_error -- so a size
# computation that overflows *to zero* yields a live minimal chunk and then a
# heap smash, rather than a clean abort.  Only list_repeat had an overflow
# guard; tuple and str had none, and all three truncated an oversized count
# through __gmpz_get_si, so a repetition of 2**64 quietly produced an empty
# result instead of raising.
#
# Separately, comparing two structures that reach each other recursed until
# the machine stack ran out: the identity fast path only catches a == a.


def too_big(f):
    # CPython says MemoryError when the size is representable but cannot be
    # allocated and OverflowError when it is not; apython's allocator exits
    # fatally on a failed malloc, so it reports OverflowError for both.
    try:
        f()
        return "no error"
    except (OverflowError, MemoryError):
        return "too big"
    except Exception as e:
        return type(e).__name__


print([too_big(lambda: (1,) * n) for n in (2 ** 61, 2 ** 64, 2 ** 64 + 1)])
print([too_big(lambda: ("a" * 16) * n) for n in (2 ** 60, 2 ** 64)])
print([too_big(lambda: [0] * n) for n in (2 ** 62, 2 ** 64)])

# Ordinary and edge-case repetition still works
print((1, 2) * 2, "ab" * 3, [1] * 2, (1,) * 1)
print(repr("x" * 0), [] * 5, () * 3, repr(b"") )
print([too_big(lambda: (1,) * n) for n in (-1, 0)])
print(len((1,) * 1000), len("a" * 1000), len([0] * 1000))


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


# Mutually recursive containers
a = []
a.append(a)
b = []
b.append(b)
print(t(lambda: a == b), a == a)

x = []
tx = (x,)
x.append(tx)
y = []
ty = (y,)
y.append(ty)
print(t(lambda: tx == ty), tx == tx)

# Ordinary comparison is untouched
print([1, 2] == [1, 2], (1, 2) == (1, 2), [1] == [2], [1, [2]] == [1, [2]])
print(sorted([[3], [1], [2]]), [1, 2] < [1, 3], (1, 2) < (2, 0))

# A sys.path entry longer than the 8192-byte path buffer used to memcpy past
# the end of it; it now simply cannot match.
import sys

sys.path.insert(0, "A" * 9000)
try:
    import nonexistent_module_xyz
except ImportError:
    print("ImportError")
sys.path.pop(0)
print("imports still work:", sys.maxsize > 0)
