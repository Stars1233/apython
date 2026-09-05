# Every method of every builtin type, called with an argument of the wrong
# shape, and what it ANSWERS diffed against CPython.
#
# tests/arity_probe.sh already asks whether a call is refused; this asks what
# it says and what it returns.  The difference found eleven wrong answers that
# a refusal test cannot see: hex() ignoring its separator, `7 << (1 << 70)`
# answering 7, `set() in {1, 2}` raising where CPython answers False,
# `b"".decode("nope")` raising where CPython short-circuits on empty input,
# and a refusal that named no type at all.
#
# What is left after it is CPython's own inconsistency between its
# clinic-generated and hand-written wordings -- `str.replace` says "replace
# expected at least 2 arguments, got 1" where `str.upper` says
# "str.upper() takes no arguments (1 given)" -- plus set iteration order,
# which is nobody's to match.

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

    # ...and what the second pass over the sweep turned up
    'b"abc".hex(b"x")', 'b"abc".hex(bytearray(b"x"))',
    'b"abc".count(None)', 'b"abc".find(1.5)', 'b"abc".index([])',
    'b"abc".rfind({})', 'b"abc".rindex(set())', 'b"abc".count(b"a")',
    'b"abc".strip(1)', 'b"abc".lstrip(None)', 'b"abc".rstrip([])',
    'b"abc".partition(None)', 'b"abc".rpartition(1)', 'b"abc".strip(b"a")',
    'b"abc".split(1)', 'b"abc".rsplit(None)', 'b"a b".split(None)',
    'b"abc".replace(1, b"x")', 'b"abc".replace(b"a", 1)',
    'b"abc".removeprefix(None)', 'b"abc".removesuffix(1)',
    'b"abc".translate(1)', 'b"abc".translate(None)',
    'b"abc".startswith(1.5)', 'b"abc".endswith(1.5)',
    'b"-".join(None)', 'b"-".join(1)', 'b"-".join([b"a", b"b"])',
    'b"abc".maketrans(1)', 'b"abc".maketrans(1, 2, 3)',
    '[1].index(None)', '[1, 2].index(3)', '["a"].index("b")',
    'bytearray(b"a") + None', 'bytearray(b"a").__iadd__(None)',
    'bytearray(b"a") + b"b"', 'None in bytearray(b"a")',
    'b"a" in bytearray(b"ab")', '[1] in bytearray(b"a")',
    'set().copy(1)', 'frozenset().copy(1)', '{1}.copy()',
    '[1].__setitem__(1)', '{1: 2}.__setitem__(1)',
    'bytearray(b"a").__setitem__(1)', '(1).__eq__()', '"a".__len__(1)',
    'set().__len__(1)', 'set().__iter__(1)',
    'b"abc".hex([1])', 'b"abc".hex([1, 2])', 'b"abc".hex({})',
    'b"abc".hex(bytearray(b"xy"))', 'b"abc".hex("\u00e9")',
    'b"abc".hex(memoryview(b"x"))',
    'b"".decode("")', 'b"".decode("nope")', 'bytearray(b"").decode("nope")',
    'b"".decode()', 'b"abc".decode("latin-1")',
]
for expr in CASES:
    try:
        print(expr, "->", repr(eval(expr))[:60])
    except BaseException as exc:
        print(expr, "->", type(exc).__name__, exc)
