# int(str) reached GMP for every input, however short.  int("5") allocated a
# cleaned copy of the string, then a PyIntObject, then an mpz, called
# __gmpz_set_str, and finally asked __gmpz_get_si and __gmpz_cmp_si whether the
# answer would have fitted an int64 all along -- before freeing all three.
# malloc and free were 23% of an int(str) loop.
#
# An ordinary run of ASCII digits that fits an int64 is now read in place, with
# no allocation.  Everything else declines to the old path, so the error
# wording and the awkward cases stay written down in exactly one place.  What
# has to keep declining:
#
#   - an underscore anywhere, including leading, trailing and doubled
#   - a Unicode digit, a leading or trailing space, a sign in the middle
#   - a digit out of range for the base ('9' in base 8, 'g' in base 16)
#   - an empty string, or one that is only a sign
#   - more than 64 digits, where sys.set_int_max_str_digits could matter
#   - an int64 overflow part way through the accumulation
#
# and the last of those is the one with a boundary worth naming: the fast path
# gives up at 2**63, so -(2**63) itself takes the slow path even though the
# answer fits, because the magnitude does not.

CASES = [
    "0", "1", "-1", "+1", "42", "-42", "007", "1234567", "-1234567",
    "9223372036854775807", "-9223372036854775808", "9223372036854775808",
    "-9223372036854775809", "10000000000000000000",
    "1" * 18, "1" * 19, "1" * 20, "1" * 64, "1" * 65, "0" * 70 + "5",
    "  42  ", "\t42\n", "1_000", "1_0_0", "_1", "1_", "__1", "1__0",
    "", "  ", "+", "-", "4 2", "0x1f", "0X1F", "0b101", "0o17", "0_0",
    "00", "0_", "abc", "1e5", "1.5", "0x", "0b", "0o", "-0", "+0",
    "-0x10", "  -0x1f  ", "z", "Z", "0xZ", "    42   ",
]
BASES = [0, 2, 8, 10, 16, 36, 1, 37, -1]

for b in BASES:
    for c in CASES:
        try:
            r = int(c, b)
        except ValueError:
            r = "ValueError"
        except TypeError:
            r = "TypeError"
        print(repr(c), b, r)

for c in CASES:
    try:
        r = int(c)
    except ValueError:
        r = "ValueError"
    print("d", repr(c), r)

# --- letters, in every base that has them ----------------------------------
for b in (11, 16, 20, 36):
    for c in ("a", "A", "z", "Z", "ff", "FF", "zz", "10", "1a", "g", "G",
              "-ff", "+ff", "  ff  ", "f_f", "0xff", "0Xff"):
        try:
            r = int(c, b)
        except ValueError:
            r = "ValueError"
        print("b", b, repr(c), r)

# --- the int64 boundary, from both sides -----------------------------------
for n in (2 ** 62, 2 ** 63 - 1, 2 ** 63, 2 ** 63 + 1, 2 ** 64, 2 ** 64 + 1,
          10 ** 18, 10 ** 19, 10 ** 20):
    for v in (n, -n):
        s = str(v)
        got = int(s)
        print(s, got, got == v, type(got).__name__)

# --- long inputs, which must still go to GMP -------------------------------
print(int("1" * 30), int("-" + "9" * 25), int("0x" + "f" * 20, 16))
print(int("1" * 100) % 1000003, int("-" + "7" * 200) % 1000003)
print(int("0" * 100 + "1"), int("0" * 630 + "9"))

# --- and the digit limit, which the fast path is short enough never to hit --
import sys
print(sys.get_int_max_str_digits())
try:
    sys.set_int_max_str_digits(100)
except ValueError:
    print("set_int_max_str_digits(100) -> ValueError")
sys.set_int_max_str_digits(640)
print(int("1" * 64), int("1" * 63))
try:
    int("1" * 700)
except ValueError:
    print("700 digits over a 640 limit -> ValueError")
print(int("0x" + "1" * 700, 16) % 1000003)   # base 16 is exempt
sys.set_int_max_str_digits(4300)

# --- a bytes and bytearray argument goes through the same parser -----------
for v in (b"42", b"  -7 ", b"1_0", bytearray(b"255")):
    try:
        print(repr(v), int(v), int(v, 16))
    except ValueError:
        print(repr(v), "ValueError")
