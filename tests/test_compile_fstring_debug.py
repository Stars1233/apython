# `{x=}` implies !r only when there is no format spec.
#
# The guard tested the conversion slot and never the spec slot, so the implied
# repr went in anyway and the spec was then handed a str: f"{x=:.2f}" raised
#
#   ValueError: Unknown format code for object of type 'str'
#
# With a spec, CPython formats the value itself.  Note this only shows with a
# value whose repr is not its str -- for an int, format(repr(5), ">8") and
# format(5, ">8") are the same string, which is why the existing coverage
# missed it.
x = 1.5
s = "ab"
n = 5

print(f"{x=}")
print(f"{x=:.2f}")
print(f"{x = :.2f}")
print(f"{x=:>10}")
print(f"{x = :>10}")
print(f"{s=}")
print(f"{s=:>6}")
print(f"{n=:>8}")

# An explicit conversion still wins over both.
print(f"{x=!s}")
print(f"{x=!s:>10}")
print(f"{x=!r:>12}")
print(f"{s=!r:>8}")

# An expression, not just a name.
print(f"{x + 1 = :.3f}")
print(f"{x + 1 = }")

# Nested spec, which builds the spec as its own f-string.
w = 8
print(f"{x=:>{w}.2f}")

# A plain field with a spec, unaffected.
print(f"{x:.2f}")
print(f"{s:>6}")


# A newline inside a replacement field is a continuation, not a statement end.
# The field is lexed as its own span, and the span started at bracket depth 0,
# so the newline emitted NEWLINE and the next line's indent became INDENT --
# in the middle of an expression.  PEP 701 allows this.
a = 1
print(f"""{a +
 1}""")
print(f"""{
    a
}""")
print(f"""{[a,
            a + 1]}""")
print(f"""{a if a else
           0}""")
