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
