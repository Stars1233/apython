# format() ignored its spec entirely and fell through to str(); the f-string
# opcode had a partial float path that understood only a precision and a type
# letter.  So format(255, "08b") was "255" and f"{5:>5}" was "5" -- and both
# are among the most common formatting there is.
#
# The grammar is CPython's:
#   [[fill]align][sign][#][0][width][grouping][.precision][type]


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


# Integer bases, with and without the alternate-form prefix
print(format(255, "b"), format(255, "o"), format(255, "x"), format(255, "X"))
print(format(255, "#b"), format(255, "#o"), format(255, "#x"), format(255, "#X"))
print(format(0, "b"), format(0, "x"), format(-255, "x"), format(255, "d"))

# Grouping
print(format(1234, "_"), format(1234567, ","), format(1234567, "_"))
print(format(-1234567, ","), format(12, ","), format(123, ","))

# Sign
print(format(42, "+"), format(42, "-"), format(42, " "), format(-42, "+"))

# Width, fill and alignment
print(format(5, ">5") + "|", format(5, "<5") + "|", format(5, "^5") + "|")
print(format("ab", ">5") + "|", format("ab", "<5") + "|", format("ab", "^5") + "|")
print(format("x", "*^5"), format("x", "-<5"), format(7, "0>4"))
print(format(-42, "05"), format(42, "05"), format(-42, "=6"))

# Precision
print(format(3.14159, ".2f"), format(3.14159, ".3f"), format(1.5, ".3f"))
print(format("abcdef", ".3"), format("ab", ".5"))

# The same through f-strings, which use the same engine
print(f"{255:08b}", f"{1234:_}", f"{5:>5}|", f"{1.5:.2f}", f"{255:#x}")
print(f"{42:+}", f"{42:^7}|", f"{1234567:,}", f"{-42:05}", f"{3.14159:.3f}")
print(f"{'ab':*^6}|", f"{'abcdef':.3}", f"{7}", f"{7!r}", f"{7:d}")
print(f"{True:d}", f"{False:d}", f"{2**40:,}")

# No spec at all still works
print(format(42), format("x"), format(1.5), f"{42}{'x'}{1.5}")


# A class with __format__ keeps control of its own spec
class Custom:
    def __format__(self, spec):
        return "custom[%s]" % spec


print(format(Custom(), ">5"), format(Custom()), f"{Custom():^9}")
