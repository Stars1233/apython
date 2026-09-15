"""marshal.dumps / dump / load: the write side.

The singletons have one encoding each, so those are compared byte for byte.
Everything else is compared by round-trip: whether a writer sets FLAG_REF on
a given object is CPython's refcount talking, not the format, so two correct
writers legitimately emit different bytes for the same value.
"""


def rt(x):
    import marshal
    return marshal.loads(marshal.dumps(x))


def main():
    import io
    import marshal

    print("version:", marshal.version)

    # The scalars, byte for byte.
    print("none:", marshal.dumps(None))
    print("true:", marshal.dumps(True))
    print("false:", marshal.dumps(False))
    print("ellipsis:", marshal.dumps(Ellipsis))
    print("stopiter:", marshal.dumps(StopIteration))
    # The scalars that the memo may or may not flag, by value.
    for v in (0, 1, -1, 2 ** 31 - 1, -2 ** 31, 2 ** 40, 1.5, 1 + 2j,
              b"", b"\x00\xff"):
        r = rt(v)
        print("scalar:", repr(v), r == v, type(r) is type(v))

    # The integers that need the digit form, both signs, across a limb.
    for n in (2 ** 15, 2 ** 30 - 1, 2 ** 64, 2 ** 100 + 7, 10 ** 40):
        print("bigint:", n, rt(n) == n, rt(-n) == -n)

    # Floats that are not representable as short decimals, plus the edges.
    for f in (0.1, -0.0, 1e308, 5e-324, float("inf"), -float("inf")):
        r = rt(f)
        print("float:", repr(f), r == f or (f != f and r != r),
              repr(r) == repr(f))

    # str: ASCII, non-ASCII, and the empty string.
    for s in ("", "abc", "h\xe9llo", "\U0001f600", "a" * 300):
        print("str:", len(s), rt(s) == s, type(rt(s)) is str)

    # The containers.
    for c in ([], [1, [2, [3]]], (), (1, 2, 3), {}, {1: "a", "b": 2},
              set(), {1, 2, 3}, frozenset(), frozenset({4, 5})):
        r = rt(c)
        print("container:", type(c).__name__, r == c, type(r) is type(c))

    # The memo: one object named twice comes back as one object.
    shared = ("x", "y")
    pair = rt((shared, shared))
    print("memo shares:", pair[0] is pair[1])

    # A self-referential list survives as a cycle rather than as a tower.
    cyc = [1]
    cyc.append(cyc)
    out = rt(cyc)
    print("cycle:", out[0], out[1] is out)

    # Inexact types are refused, the way CPython refuses them.
    class MyList(list):
        pass

    for bad in (MyList([1]), object(), lambda: 1 if 0 else 2):
        try:
            marshal.dumps(bad)
        except ValueError as e:
            print("refused:", e)

    # A code object round-trips and still runs.
    src = "def f(a, b=2):\n    return a * b + len('hi')\nresult = f(3)\n"
    code = compile(src, "<rt>", "exec")
    ns = {}
    exec(rt(code), ns)
    print("code runs:", ns["result"])
    print("code fields:", rt(code).co_name, rt(code).co_filename,
          rt(code).co_argcount)

    # Every nested code object keeps its OWN bytecode.  A writer that
    # memoises a temporary bytes built for co_code hands the next one a back
    # reference to the address the first was freed from, and the inner code
    # comes back running the outer's instructions.
    nested = compile(
        "class A(property):\n"
        "    'doc'\n"
        "    def m(self):\n"
        "        return 1\n"
        "def g():\n"
        "    return 2\n",
        "<nest>", "exec")

    def walk(c):
        yield c
        for k in c.co_consts:
            if hasattr(k, "co_code"):
                yield from walk(k)

    before = [c.co_code for c in walk(nested)]
    after = [c.co_code for c in walk(rt(nested))]
    print("nested codes:", len(before), before == after,
          len(set(before)) == len(before))

    # dump/load over a file object.
    buf = io.BytesIO()
    marshal.dump({"a": [1, 2], "b": (3.5,)}, buf)
    buf.seek(0)
    print("dump/load:", marshal.load(buf))

    # dump accepts the version argument and ignores it.
    buf = io.BytesIO()
    marshal.dump([1], buf, 2)
    print("dump version arg:", marshal.loads(buf.getvalue()))

    # Anything with .write will do.
    class Sink:
        def __init__(self):
            self.chunks = []

        def write(self, data):
            self.chunks.append(bytes(data))
            return len(data)

    sink = Sink()
    marshal.dump("hello", sink)
    print("duck write:", marshal.loads(b"".join(sink.chunks)))

    # A file with neither method fails the way the plain getattr behind it
    # fails -- an AttributeError naming the type -- and not as a crash.
    try:
        marshal.dump(1, object())
    except AttributeError as e:
        print("no write:", e)
    try:
        marshal.load(object())
    except AttributeError as e:
        print("no read:", e)


main()
