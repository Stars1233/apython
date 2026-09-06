# str comparison: identity, then length, then bytes.
#
# The point of the file is that these are COUNTED strings, not C strings.  A
# NUL is an ordinary byte in Python, so `"a\0b" == "a\0c"` must be False and
# `"a\0b" < "a\0c"` must be True; a comparison that stops at the first NUL
# answers both wrong and answers them silently.  str_contains was moved off
# ap_strstr for exactly this reason; str_compare was missed.
#
# The ordering cases over non-ASCII are the other half: UTF-8's byte order IS
# code-point order, so comparing bytes is not an approximation of comparing
# characters, it is the same answer.  If it ever stops being so, the sorted()
# rows here diverge from CPython's.


def compare_all(a, b):
    return (a == b, a != b, a < b, a <= b, a > b, a >= b)


def show(a, b):
    print(" ".join("%d" % v for v in compare_all(a, b)))


def nul_cases():
    # Equal length, differing after an embedded NUL.
    show("a\0b", "a\0c")
    show("a\0c", "a\0b")
    show("a\0b", "a\0b")
    # Differing length, equal up to the NUL.
    show("ab\0", "ab\0z")
    show("ab\0z", "ab\0")
    # A NUL against an ordinary byte at the same offset.
    show("a\0b", "aXb")
    show("a\0", "a")
    show("", "\0")
    # Built at run time, so neither side is a shared constant.
    left = "a" + chr(0) + "b"
    right = "a" + chr(0) + "c"
    show(left, right)
    print(left == "a\0b", len(left))


def length_cases():
    # A prefix is less than what extends it, at every length that crosses the
    # 8-byte stride ap_memcmp walks in.
    base = "abcdefghijklmnopqrstuvwxyz0123456789"
    for n in (0, 1, 7, 8, 9, 15, 16, 17, 23, 24, 25, 31, 32, 33, 35, 36):
        a = base[:n]
        show(a, base)
        show(base, a)
        show(a, a)
        # Differ in the LAST byte only, so every earlier word matches and the
        # answer comes from the tail.
        if n:
            b = a[:-1] + chr(ord(a[-1]) + 1)
            show(a, b)


def identity_cases():
    # The same object on both sides must answer from identity alone.
    s = "x" * 40
    t = s
    show(s, t)
    u = "x" * 40
    print(s == u, compare_all(s, u))
    # An empty string against itself and against another empty one.
    e = ""
    show(e, e)
    show(e, "".join([]))


def wide_cases():
    # Byte-lexicographic order is code-point order under UTF-8.
    words = ["été", "eta", "éta", "zebra", "中文",
             "\U0001f600", "é", "e", "", "\0"]
    print(sorted(words))
    print(sorted(words, reverse=True))
    for a in words:
        for b in words:
            print(a.encode(), b.encode(),
                  "".join("%d" % v for v in compare_all(a, b)))
    # Same first byte, differing continuation byte.
    show("é", "ê")
    show("中", "丮")
    # A one-byte string against the first byte of a two-byte sequence: the
    # shorter is a prefix in neither direction, so this is a byte compare.
    show("é", "éx")
    # Non-ASCII with an embedded NUL.
    show("é\0a", "é\0b")


def sort_cases():
    print(sorted(["a\0z", "a\0b", "a\0m"]))
    print(sorted(["ab", "a", "abc", "", "aa", "a\0"]))
    print(min(["pear", "apple", "fig"]), max(["pear", "apple", "fig"]))
    print(sorted(set(["b", "a", "b\0", "a\0"])))


def nonstring_cases():
    # A non-string right operand must DECLINE, not answer, so the protocol can
    # ask the other side and only then fall back to identity.
    class S:
        def __eq__(self, other):
            return True

        def __lt__(self, other):
            return True

    print("a" == S(), "a" != S())
    # `>` finds S.__lt__ reflected; `>=` finds nothing and is a TypeError.
    print("a" > S())
    try:
        print("a" >= S())
    except TypeError:
        print("TypeError from >=")
    print("a" == 1, "a" != 1, "a" == None, "a" == b"a")
    try:
        print("a" < 1)
    except TypeError as e:
        print("TypeError", e)
    try:
        print("a" < b"a")
    except TypeError as e:
        print("TypeError", e)


def subclass_cases():
    class S(str):
        pass

    a = S("a\0b")
    b = "a\0b"
    print(a == b, b == a, a == "a\0c", a < "a\0c")
    print(sorted([S("b"), "a", S("c")]))
    print(a == a, S("") == "")


nul_cases()
length_cases()
identity_cases()
wide_cases()
sort_cases()
nonstring_cases()
subclass_cases()
