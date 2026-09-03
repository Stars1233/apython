# int(), float() and complex() all run their argument through
# _PyUnicode_TransformDecimalAndSpaceToASCII first, so a Unicode decimal
# digit is a digit and a Unicode space is a space.  None of the three did:
# any byte past ASCII was a malformed string.
#
# The transform maps a decimal digit to the ASCII digit of the same value and
# a space character to an ASCII space, and leaves everything else alone --
# so a string that was never going to parse still does not, and says so with
# the original text in the message.

FULLWIDTH = "１２３"          # １２３
ARABIC = "١٢٣"             # ١٢٣
DEVANAGARI = "१२३"         # १२३
MATH_BOLD = "\U0001d7d9\U0001d7da"        # the mathematical bold digits
IDEO_SPACE = "　"                     # IDEOGRAPHIC SPACE
NBSP_LIKE = " "                      # EM SPACE

print(int(FULLWIDTH), int(ARABIC), int(DEVANAGARI), int(MATH_BOLD))
print(int(IDEO_SPACE + FULLWIDTH + IDEO_SPACE))
print(int(NBSP_LIKE + "42"))
print(int("-" + FULLWIDTH))
print(int("12"), int(" 12 "), int("+7"))

print(float("１.5"), float("　-2.5　"), float(ARABIC))
print(float("1e3"), float(" 2.5 "))

print(complex("　1+2j"), complex("１+２j"))
print(complex(" 1+2j "), complex("(1+2j)"), complex("1_0+2j"))
print(complex(ARABIC + "j"))

# What the transform does not fix still fails, and the message names the
# string as it was written.
for bad in ("１a", "１１x", "abc"):
    try:
        int(bad)
    except ValueError as e:
        print(e)
for bad in ("１x", "1 2"):
    try:
        float(bad)
    except ValueError as e:
        print(e)
for bad in ("1 + 2j", "１+j2"):
    try:
        complex(bad)
    except ValueError as e:
        print(e)

# An embedded NUL is not the end of the string, in any of the three.  float()
# used to think it was, because strtod does: float("1\x002") answered 1.0.
for bad in ("123\x00", "1\x002", "\uff11\x00"):
    for fn in (int, float, complex):
        try:
            fn(bad)
            print(fn.__name__, repr(bad), "NO ERROR")
        except ValueError:
            print(fn.__name__, repr(bad), "ValueError")

# isdigit/isdecimal already agreed with CPython; this is the value behind them.
print(FULLWIDTH.isdecimal(), ARABIC.isdigit(), MATH_BOLD.isdecimal())
print(int(FULLWIDTH) == 123, int(ARABIC) == 123, int(MATH_BOLD) == 12)

# Every decimal block in the table, one digit from each of the first few.
for zero in ("0", "٠", "۰", "०", "০", "੦",
             "௦", "๐", "０", "\U0001d7ce"):
    n = "".join(chr(ord(zero) + d) for d in range(10))
    assert int(n) == 123456789, (zero, int(n))
print("every decimal block reads as its own digits")
