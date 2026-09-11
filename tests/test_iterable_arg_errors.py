# An exception raised WHILE an iterable argument is being materialised is the
# exception the caller sees.
#
# bytes(), bytearray() and dict() each build their argument into a list or a
# dict first, and each read a NULL from that step as "the argument was the
# wrong type" -- raising their own TypeError over whatever had actually gone
# wrong.  So a generator or a map that raises part way through reported
# "cannot convert 'map' object to bytes" instead of the error it raised, and
# the real one was lost.
#
# It is the same defect str.join had: materialise, then check the step for
# failure rather than assuming the type was wrong.  list(), tuple(), set() and
# str.join() were already right, which is what makes the three stand out.
#
# ipaddress is where it shows in real code: _ip_int_from_string does
#
#     int.from_bytes(map(cls._parse_octet, octets))
#
# and catches ValueError to re-raise AddressValueError.  With the ValueError
# turned into a TypeError, a bad address came out as the wrong exception
# entirely.


def boom(value):
    raise ValueError("boom %s" % value)


def gen_raises():
    yield 1
    raise KeyError("half way")


def gen_pairs_raises():
    # Valid pairs first, so that dict() gets past its own element check and
    # the raise is what it meets.  With a bad element first, CPython reports
    # the element and this tree reports the raise -- it materialises the whole
    # iterable before looking, which is a separate difference from this one.
    yield ("a", 1)
    raise KeyError("half way")


calls = [
    ("bytes(map)", lambda: bytes(map(boom, [1, 2]))),
    ("bytearray(map)", lambda: bytearray(map(boom, [1, 2]))),
    ("dict(map)", lambda: dict(map(boom, [1, 2]))),
    ("bytes(genexp)", lambda: bytes(boom(i) for i in [1])),
    ("bytearray(genexp)", lambda: bytearray(boom(i) for i in [1])),
    ("bytes(generator)", lambda: bytes(gen_raises())),
    ("dict(generator)", lambda: dict(gen_pairs_raises())),
    ("int.from_bytes(map)", lambda: int.from_bytes(map(boom, [1, 2]), "big")),
    # The ones that were already right, so a regression in them shows here too.
    ("list(map)", lambda: list(map(boom, [1, 2]))),
    ("tuple(map)", lambda: tuple(map(boom, [1, 2]))),
    ("set(map)", lambda: set(map(boom, [1, 2]))),
    ("''.join(map)", lambda: "".join(map(boom, [1, 2]))),
]

for label, call in calls:
    try:
        call()
        print("NO ERROR", label)
    except BaseException as e:
        print("%-22s %s: %s" % (label, type(e).__name__, e))

# And a genuinely wrong type still gets the type error, with its own wording.
for label, call in (("bytes(object)", lambda: bytes(object())),
                    ("bytearray(object)", lambda: bytearray(object())),
                    ("dict(object)", lambda: dict(object()))):
    try:
        call()
        print("NO ERROR", label)
    except TypeError as e:
        print("%-22s TypeError: %s" % (label, e))

# ipaddress is not exercised here -- this tree's lib/ does not ship it, and
# the suite runs without CPython's on the path.  Checked by hand against
# CPython's: IPv4Address("42..42.42") is an AddressValueError on both sides
# now, where it was a TypeError before, because _ip_int_from_string catches
# ValueError from int.from_bytes(map(cls._parse_octet, octets)).
