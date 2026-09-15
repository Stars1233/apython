"""A power too wide to build is refused, not aborted.

GMP calls `__gmp_overflow_in_mpz` when an mpz would exceed what it can
represent, and that raises SIGFPE and kills the process -- there is no error
return for `__gmpz_pow_ui` to hand back.  So the size has to be refused before
the call.  `Decimal(10**19).sqrt()` at decimal's MAX_PREC asks for exactly such
a power.

CPython reaches MemoryError on the same expressions by failing a multi-gigabyte
allocation, which takes minutes and the memory to try; it is not asked to here.
What is asserted below is the boundary in both directions: everything that
SHOULD still be computed is computed and matches CPython exactly, and the four
that cannot be come back as MemoryError rather than as a dead process.
"""

import sys

HUGE = [
    "10 ** (10**19)",
    "2 ** (2**40)",
    "(10**30) ** (10**10)",
    "(-7) ** (2**35)",
]

# Only this interpreter is asked to evaluate them; see the module docstring.
REFUSES = sys.implementation.cache_tag.startswith("apython")


def main():
    # The int64 fast path, and the first exponent past it.
    for e in range(0, 5):
        print("2 **", e, "=", 2 ** e)
    print("2 ** 62 =", 2 ** 62)
    print("2 ** 63 =", 2 ** 63)
    print("2 ** 64 =", 2 ** 64)
    print("3 ** 40 =", 3 ** 40)

    # Wide, but perfectly buildable.
    print("bits:", (10 ** 5000).bit_length(), (2 ** 100000).bit_length())
    print("big tail:", str(7 ** 3000)[-12:])
    print("negative base:", (-3) ** 101 == -(3 ** 101), (-3) ** 100 > 0)

    # A magnitude of one answers in one bit however large the exponent is, so
    # these are not refusals -- the size guard has to let them past.
    print("1 **:", 1 ** (10**19), 1 ** (2**40))
    print("0 **:", 0 ** (10**19))
    print("-1 **:", (-1) ** (10**19), (-1) ** (10**19 + 1))

    # A negative exponent goes to float and never reaches the guard.
    print("negative exponent:", 2 ** -3, 10 ** -30)

    for expr in HUGE:
        if REFUSES:
            try:
                eval(expr)
                verdict = "built"
            except MemoryError:
                verdict = "refused"
        else:
            verdict = "refused"
        print(expr, verdict)

    # And the interpreter is still alive and arithmetic still works.
    print("alive:", 2 ** 10, pow(3, 5), pow(3, 5, 7))


main()
