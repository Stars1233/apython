# Where a numeric literal ends, and what is wrong with one that does not.
#
# CPython's tokenizer lets a number end against a keyword: `1if True else 2`
# is 1, `[1for i in x]` is a comprehension, and `0x1if True else 2` is 1 --
# with a SyntaxWarning, because the shape is going away, but they compile.
# This refused every one of them, and they turn up in real code because a
# formatter that strips spaces produces them.
#
# The warning is the one part not reproduced here: the compiler runs before
# there is an interpreter frame to warn from, which is the same reason it may
# not raise.  So the warnings are silenced on both sides and only the answer
# is compared.
#
# The refusals were wrong in their own way.  The scan takes hex digits
# whatever the radix, so `0b1and 2` arrived as `0b1a` and was refused for a
# digit that was not part of the literal at all; a letter after a radix
# literal was reported as a bad digit where CPython names the base; `0x` with
# no digits reached the parser, which called it an "invalid numeric literal",
# naming neither the base nor the reason; and an underscore that separated
# nothing went the same way.

import warnings


def show(src, mode="eval"):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            answer = repr(eval(compile(src, "<t>", mode)))
        except SyntaxError as e:
            answer = "SyntaxError: %s" % (e.msg,)
        except Exception as e:
            answer = "%s: %s" % (type(e).__name__, e)
    print("%-26r %s" % (src, answer))


print("=== a number against a keyword ===")
for src in ("1if True else 2", "1and 2", "1or 2", "1in [1]", "1is 1",
            "[1for i in (1,)]", "1if 0else 2", "1not in [2]",
            "0x1if True else 2", "1.5if True else 2", "1jif True else 2",
            "0b1and 2", "0o7or 2", "1.2and 3", "1_0if True else 2",
            "1e5if True else 2", "[x for x in(1,)]", "1 if True else 2"):
    show(src)

print("=== and what is not one ===")
for src in ("1x", "1foo", "1e", "1while", "1None", "1True", "1import", "1as",
            "1ifx", "1andx", "1isx", "1orx", "1notx", "1elsex", "1forx",
            "1if", "1in", "1else"):
    show(src)

print("=== radix literals ===")
for src in ("0b101", "0o17", "0xff", "0b1_0", "0x1_f", "0b_1", "0x_f",
            "0b12", "0o18", "0x1g", "0xg", "0b1x", "0o7x", "0x1z",
            "0x1andx", "0b", "0x", "0o", "0b_", "0x_", "0o_", "0b1_", "0x1_"):
    show(src)

print("=== underscores ===")
for src in ("1_000", "1_0", "1_000.5", "1_", "1__2", "12__", "0b1__0",
            "0x1__f", "1_.5", "1_e5", "123"):
    show(src)

print("=== and the numbers themselves still work ===")
print(1_000, 0b1010, 0o777, 0xdeadBEEF, 1e5, 1.5e-3, 3j, 0.0, 1_0.2_5)
print("done")
