# The operator an unsupported pair is reported against.
#
# op_binary_op kept the op index in r9d and built its TypeError from it, but
# r9 is caller-saved and the reflected-dunder call clobbers it -- and that
# call is on the path every unsupported pair takes.  So the index read back as
# zero and binary_op_symbols[0] is "+": EVERY binary operator in the
# interpreter reported itself as a failed addition, including the augmented
# ones, which have their own thirteen spellings.


class Plain:
    pass


class Index:
    def __index__(self):
        return 3


n = Plain()

print("=== binary ===")
for name, fn in (("+", lambda: [1] + n), ("-", lambda: 1 - n),
                 ("*", lambda: 1 * n), ("/", lambda: 1 / n),
                 ("//", lambda: 1 // n), ("%", lambda: 1 % n),
                 ("**", lambda: 1 ** n), ("<<", lambda: 1 << n),
                 (">>", lambda: 1 >> n), ("&", lambda: 1 & n),
                 ("|", lambda: 1 | n), ("^", lambda: 1 ^ n),
                 ("@", lambda: (1).__matmul__(n))):
    try:
        print("%-4s %r" % (name, fn()))
    except Exception as e:
        print("%-4s %s: %s" % (name, type(e).__name__, e))

print("=== augmented ===")
for name, code in (("+=", "a = 1\na += n"), ("-=", "a = 1\na -= n"),
                   ("*=", "a = 1\na *= n"), ("/=", "a = 1\na /= n"),
                   ("//=", "a = 1\na //= n"), ("%=", "a = 1\na %= n"),
                   ("**=", "a = 1\na **= n"), ("<<=", "a = 1\na <<= n"),
                   (">>=", "a = 1\na >>= n"), ("&=", "a = 1\na &= n"),
                   ("|=", "a = 1\na |= n"), ("^=", "a = 1\na ^= n"),
                   ("@=", "a = 1\na @= n"),
                   ("list *=", "a = [1]\na *= n"),
                   ("list +=", "a = [1]\na += n")):
    try:
        exec(code, {"n": n})
        print("%-8s ok" % name)
    except Exception as e:
        print("%-8s %s: %s" % (name, type(e).__name__, e))

# A sequence multiplied by something that is not an index gets a message of
# its own, naming only the count.  It goes through PyNumber_AsSsize_t, which
# means __index__ counts and a value too big for an index is refused rather
# than truncated -- `"a" * 2**64` used to answer "" on the way past.
# bytearray is deliberately absent from both loops: tests/
# test_bytearray_repeat.py covers it, and CPython's own finalizer prints an
# unraisable SystemError about exported buffers when a bytearray repetition
# that fails shares a file with the rest of this.
print("=== repetition ===")
for label, seq in (("list", [1]), ("tuple", (1,)), ("str", "ab"),
                   ("bytes", b"ab")):
    for arg, an in ((Index(), "Index"), (n, "plain"), (True, "True"),
                    (2 ** 40, "2**40"), (2 ** 63, "2**63"), (2 ** 64, "2**64"),
                    (-2, "-2")):
        try:
            print("%-10s %-6s %r" % (label, an, (seq * arg)))
        except Exception as e:
            print("%-10s %-6s %s: %s" % (label, an, type(e).__name__, e))

print("=== repetition in place ===")
for label, mk in (("list", lambda: [1]),):
    for arg, an in ((Index(), "Index"), (n, "plain"), (True, "True"),
                    (2 ** 40, "2**40"), (2 ** 63, "2**63"), (2 ** 64, "2**64"),
                    (-2, "-2")):
        v = mk()
        try:
            v *= arg
            print("%-10s %-6s %r" % (label, an, v))
        except Exception as e:
            print("%-10s %-6s %s: %s" % (label, an, type(e).__name__, e))

# And the right operand still gets its turn: declining is not the same as
# refusing, and a type with an __rmul__ of its own must still be asked.
class RMul:
    def __rmul__(self, other):
        return "rmul(%r)" % (other,)

    def __rtruediv__(self, other):
        return "rtruediv"


r = RMul()
print("__rmul__", [1] * r, "ab" * r, (1,) * r, b"ab" * r, 3 * r)
for label, code in (("list", "a = [1]"), ("str", "a = 'ab'"),
                    ("tuple", "a = (1,)"), ("bytes", "a = b'ab'"),
                    ("bytearray", "a = bytearray(b'ab')")):
    ns = {"r": r}
    exec(code + "\na *= r", ns)
    print("%-10s in place %r" % (label, ns["a"]))
print("__rtruediv__", 1 / r)

# ...and called by name it is not the operator, so it says what the count had
# to be rather than what the multiplication needed.  CPython draws the same
# line, because the two go through different code.
# Which operand the message names, when both are sequences.  CPython asks the
# LEFT first -- whichever side has sq_repeat is the sequence, and the other is
# the count -- and asking the right first named the wrong one.
print("=== which operand is the count ===")
for a, b in (("[1]", "'x'"), ("'x'", "[1]"), ("(1,)", "b'a'"), ("b'a'", "(1,)"),
             ("[1]", "None"), ("None", "[1]"), ("1.5", "'x'"), ("'x'", "1.5"),
             ("bytearray(b'a')", "'x'"), ("'x'", "bytearray(b'a')"),
             ("{}", "[1]"), ("[1]", "{}")):
    try:
        answer = repr(eval("%s * %s" % (a, b)))
    except Exception as e:
        answer = "%s: %s" % (type(e).__name__, e)
    print("%-18s * %-18s %s" % (a, b, answer))

print("=== the dunder, by name ===")
for label, fn in (("list", lambda: [1].__mul__("x")),
                  ("list r", lambda: [1].__rmul__("x")),
                  ("tuple", lambda: (1,).__mul__("x")),
                  ("tuple r", lambda: (1,).__rmul__("x")),
                  ("str", lambda: "a".__mul__("x")),
                  ("bytes", lambda: b"a".__mul__("x")),
                  ("bytearray", lambda: bytearray(b"a").__mul__("x"))):
    try:
        print("%-10s %r" % (label, fn()))
    except Exception as e:
        print("%-10s %s: %s" % (label, type(e).__name__, e))
print("done")
