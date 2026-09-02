# bytes %-formatting, with an operand of every Value shape.
#
# `b"%d" % 5` used to segfault.  bytes_mod unpacked its right operand and
# handed the raw *payload* to str_mod -- which is itself an nb_remainder slot
# and unpacks its arguments again.  The bare 5 has a zero high16, so the second
# unpack classified it as a pointer and dereferenced address 0x5.  A heap int
# came through unharmed, a pointer being its own Value, which is exactly why
# the one `b'%d' %` in tests/cpython/test_int.py never caught it.
#
# So the operand shapes are the point of this file: an int immediate, a heap
# int, a bool singleton, a float immediate, a tuple, and a bytes.
#
# tests/test_binop_matrix.py covers the operator's type protocol but cannot
# reach this: its format string is b"ab", which has no directives, so the
# argument is never converted.

CASES = [
    'b"%d" % 5',                  # int immediate -- the segfault
    'b"%d" % 0',
    'b"%d" % -7',
    'b"%d" % True',               # bool singleton, a pointer
    'b"%d" % (1 << 70)',          # heap int, the shape that always worked
    'b"%d%d" % (1, 2)',
    'b"%f" % 1.5',                # float immediate
    'b"%x" % 255',
    'b"%o" % 8',
    'b"%r" % 5',
    'b"%%" % ()',
    'b"" % ()',
    'b"ab" % 3',                  # no directives: TypeError, not a crash
    'b"%d" % (1, 2)',             # too many arguments
    'b"%d %d" % (1,)',            # too few
]

# eval() over CASES is safe here and deliberate: every string is a literal in
# this file, nothing is read from input, and evaluating the source text is what
# keeps the printed label and the expression that produced it from drifting
# apart.  exec/eval are themselves part of what the suite exercises.
for expr in CASES:
    try:
        print(expr, "=", repr(eval(expr)))
    except BaseException as exc:
        print(expr, "->", type(exc).__name__)

print()
print("=== the conversions a bytes format spells differently ===")
# bytes % used to reach str_mod by latin-1 decoding the format AND every
# bytes-like argument up front, which cannot express the difference: %s on a
# bytes REQUIRES bytes-like where str's takes anything, %r has to answer
# b'x' and not 'x', %b exists at all, and %c takes a byte.
GOOD = [
    (b"%s", b"x"), (b"%b", b"y"), (b"%s", bytearray(b"z")),
(b"%r", b"x"), (b"%a", b"x"),
    (b"%r", 5), (b"%r", "s"), (b"%a", "é"), (b"%c", 65), (b"%c", b"A"),
    (b"%c", 0), (b"%c", 255), (b"%d", 5), (b"%x", 255), (b"%o", 8),
    (b"%f", 1.5), (b"%e", 1.5), (b"%g", 1.5), (b"%5s", b"q"),
    (b"%-5s|", b"q"), (b"%5d", 42), (b"%05.1f", 3.14159),
    (b"[%s][%b]", (b"1", b"2")),
]
for fmt, arg in GOOD:
    a = arg if isinstance(arg, tuple) else (arg,)
    print(repr(fmt), repr(arg), "->", repr(fmt % a))
print(repr(b"%%" % ()), repr(b"100%%" % ()))

print("=== and the ones that must raise ===")
BAD = [
    (b"%s", "x"), (b"%b", "x"), (b"%s", 5), (b"%b", 5), (b"%s", None),
    (b"%d", "x"), (b"%d", b"x"), (b"%c", "A"), (b"%c", b"AB"), (b"%c", 256),
    (b"%c", -1), (b"%x", b"x"),
]
for fmt, arg in BAD:
    try:
        print(repr(fmt), repr(arg), "-> NO ERROR", repr(fmt % (arg,)))
    except (TypeError, ValueError, OverflowError) as e:
        print(repr(fmt), repr(arg), "->", type(e).__name__, e)

print("=== bytes-keyed mappings ===")
print(b"%(a)s/%(b)d" % {b"a": b"x", b"b": 2})
print(b"%(k)s" % {b"k": bytearray(b"v")})
try:
    print(b"%(missing)s" % {b"a": b"x"})
except KeyError as e:
    print("KeyError", e)

print("=== bytearray inherits all of it ===")
print(repr(bytearray(b"%s-%d") % (b"q", 7)))
print(repr(bytearray(b"%r") % (b"x",)))
print(repr(bytearray(b"%(a)s") % {b"a": b"z"}))

print("=== high bytes survive the round trip ===")
print(repr(b"%s" % (b"\xff\x00\xfe",)))
print(repr(b"\xe4\xb8\xad%s" % (b"\xff",)))
print(repr(b"%c%c" % (0xff, 0x80)))
