# Every method of every builtin type, called with an argument of the wrong
# shape, and what it ANSWERS diffed against CPython.
#
# tests/arity_probe.sh already asks whether a call is refused; this asks what
# it says and what it returns.  The difference found eight wrong answers that
# a refusal test cannot see: hex() ignoring its separator, `7 << (1 << 70)`
# answering 7, `set() in {1, 2}` raising where CPython answers False, and a
# refusal that named no type at all.

CASES = [
    # bytes.hex's separator and grouping, which were accepted and ignored
    'b"abcd".hex(":")', 'b"abcd".hex(":", 2)', 'b"abcd".hex(":", -2)',
    'b"abcde".hex("-", 1)', 'b"abcde".hex("-", 2)', 'b"abcde".hex("-", 3)',
    'b"abcde".hex("-", -1)', 'b"abcde".hex("-", -2)', 'b"abcde".hex("-", -3)',
    'b"abcd".hex("-", 0)', 'b"abcd".hex("")', 'b"abcd".hex("ab")',
    'b"abcd".hex(1)', 'b"abcd".hex(None)', 'b"".hex(":")', 'b"abcd".hex()',
    'bytearray(b"abcd").hex(":")', 'memoryview(b"abcd").hex(":")',
    'memoryview(b"abcde").hex("-", 2)', 'memoryview(b"abcd").hex()',

    # a shift count wider than an int64
    '7 << (2**70)', '7 >> (2**70)', '-7 >> (2**70)', '(2**70) >> (2**70)',
    '(2**70) >> 60', '7 >> 100', '7 >> -1', '7 << 3',

    # a set is unhashable, and `set() in {...}` is still False
    'set() in {1, 2}', '{1, 2}.__contains__(set())', 'frozenset() in {1, 2}',
    '{1} in {frozenset({1})}', '[] in {1, 2}', '1 in {1, 2}',

    # the refusal names the operand's type
    '[1].__iadd__(0)', '[1].__iadd__(None)', '[1].__iadd__(object())',
    '[1].__iadd__([2])', '"a" in "abc"', 'None in "abc"', '1 in "abc"',
    'None in b"abc"', '1.5 in b"abc"', 'b"a" in b"abc"', '[] in b"ab"',

    # a byte value that cannot be one
    '(2**70) in b"abc"', 'b"abc".count(2**70)', 'b"abc".find(2**70)',
    '(2**70) in bytearray(b"abc")', '300 in b"abc"', '97 in b"abc"',

    # strip's None is the DEFAULT, not a refusal, and each names itself
    '"  ab ".strip(None)', '" ab ".lstrip(None)', '" ab ".rstrip(None)',
    '"ab".strip(0)', '"ab".lstrip(0)', '"ab".rstrip(0)',
    '"xaby".strip("xy")',

    # an empty encoding name is unknown, not the default
    '"abc".encode("")', 'b"abc".decode("")', '"abc".encode("nope")',
    'b"abc".decode("nope")', '"abc".encode()', 'b"abc".decode()',
]
for expr in CASES:
    try:
        print(expr, "->", repr(eval(expr))[:60])
    except BaseException as exc:
        print(expr, "->", type(exc).__name__, exc)
