# Imaginary literals, through our own compiler and through marshal.
#
# The lexer already flagged the `j` suffix; src/compiler/parse.asm raised
# "complex literals are not supported" for it.  The suffix is counted in
# Token.len, so it has to be stripped before strtod sees the text -- and the
# magnitude strtod returns is the IMAGINARY part, so it goes in xmm1 with a
# +0.0 real part in xmm0.  Swapping the two yields (2+0j) for 2j, which reprs
# differently and is what this file would catch.
#
# tests/source_probe.sh globs tests/test_*.py, so this file also runs through
# make check-source, where our compiler produces the bytecode rather than
# CPython's.  Read from a .pyc instead, the same constants arrive through
# marshal's TYPE_BINARY_COMPLEX -- which used to be an unknown type code and
# a fatal_error, not an exception.

print(2j, 2J, 1_0j, .5j, 1e3j, 1E3j, 0j, 0.0j)
print(-2j, 1 + 2j, 1 - 2j, (3 - 4j), 2j * 2j, 1j * 1j)
print([2j, 3j], (1j,), {1: 2j}, {2j})
print(type(2j).__name__, (2j).real, (2j).imag)

# A constant folded at compile time versus one built at run time.
FOLDED = 1 + 2j
built = complex(1, 2)
print(FOLDED, built, FOLDED == built)

# Negation is UNARY_NEGATIVE over a positive literal, not a signed constant.
print(-(1 + 2j), -0j, -(0j))

# Deduplication of equal constants is not required; equality is.
print(2j == 2j, 2j == complex(0, 2))
