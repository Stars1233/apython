# The str predicates the stdlib checks names with.  ASCII-only, like the rest
# of them here: a str is still a byte string.
cases = ["", "a", "_", "1", "a1", "_a_1", "a b", "class", "Ab_9", "1a", " ",
         "-x", "x-", "__init__", "9lives", "A", "z", "0", "99", "a\tb"]
for c in cases:
    print(repr(c), c.isidentifier(), c.isprintable(), c.isascii(),
          c.isdecimal(), c.isnumeric())
print("".isprintable(), "".isascii(), "".isdecimal(), "".isnumeric())
print([n for n in ("isidentifier", "isprintable", "isascii", "isdecimal",
                   "isnumeric") if not hasattr("x", n)])
