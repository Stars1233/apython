# The methods that build a new string of a size they can work out in advance:
# join, and the six case mappings.
#
# Both used to build their answer twice -- fill a scratch buffer, hand it to
# str_new_heap, which allocated again, copied again, and rescanned the result
# for its code-point count.  They now allocate the result itself and write into
# it once, which means the code-point count is arithmetic rather than counted,
# and THAT is what most of this file is checking: len() of the result, not just
# its contents.  A byte length used where a code-point length belongs looks
# perfectly correct until something asks how long the string is.
#
# The ASCII case loops process eight bytes at a time with no branch per byte,
# so the cases here walk a changing byte through every offset of an eight-byte
# window and across the 7/8/9 boundary, and include every character on either
# side of the four range edges ('A'-1, 'Z'+1, 'a'-1, 'z'+1) where an
# off-by-one in the arithmetic would show.


def join_contents():
    print("-".join([]))
    print("-".join(["a"]))
    print("-".join(["a", "b"]))
    print("".join(["a", "b", "c"]))
    print("--".join(["a"] * 10))
    print("|".join(("t", "u", "p")))
    print(",".join(str(i) for i in range(5)))
    print("".join(iter(["x", "y"])))
    # Embedded NULs on both sides.
    print(repr("\0".join(["a\0b", "c"])))
    # Empty pieces.
    print(repr("".join(["", "", ""])), repr("x".join(["", ""])))


def join_lengths():
    # len() must be code points.  A separator that is wide, pieces that are
    # wide, and both.
    print(len("-".join(["é"] * 10)), len("é".join(["a"] * 10)))
    print(len("é".join(["é"] * 10)))
    print(len("".join(["中文", "日本", "한국"])))
    print(len("\U0001f600".join(["a", "b", "c"])))
    a = "-".join(["é", "中", "\U0001f600"])
    print(a, len(a), len(a.encode()))
    # A long one, so the sum is not confusable with any single piece.
    b = ",".join(["éé"] * 500)
    print(len(b), len(b.encode()))


def join_single():
    # One exact str is handed back as it is; a subclass is not, and neither is
    # a one-item list with a separator that would never appear anyway.
    class S(str):
        pass

    item = "the only one"
    print("".join([item]) == item, "-".join([item]) == item)
    r = "".join([S("sub")])
    print(r, type(r) is str, len(r))
    # A wide single item keeps its code-point count.
    w = "".join(["éé中"])
    print(w, len(w), len(w.encode()))
    # A generator of one still goes through the general path.
    print("".join(x for x in ["solo"]))


def join_errors():
    try:
        "".join([1])
    except TypeError:
        print("TypeError item")
    try:
        "".join(1)
    except TypeError as e:
        print("TypeError arg", e)
    try:
        "".join(["a", None])
    except TypeError:
        print("TypeError None")


def case_ascii():
    window = "abcdefgh"
    edges = [0x00, 0x20, 0x2f, 0x30, 0x39, 0x3a, 0x40, 0x41, 0x5a, 0x5b,
             0x5f, 0x60, 0x61, 0x7a, 0x7b, 0x7e, 0x7f]
    for i in range(8):
        for c in edges:
            t = window[:i] + chr(c) + window[i + 1:]
            print(t.upper().encode(), t.lower().encode(),
                  t.swapcase().encode())
    # Lengths either side of the eight-byte stride.
    for n in (0, 1, 7, 8, 9, 15, 16, 17, 31, 32, 33):
        s = ("aBcDeFgH" * 5)[:n]
        print(n, s.upper(), s.lower(), s.swapcase(), s.title(),
              s.capitalize(), s.casefold())
    # Every ASCII character at once.
    every = "".join(chr(c) for c in range(1, 128))
    print(every.upper().encode())
    print(every.lower().encode())
    print(every.swapcase().encode())
    print(every.casefold().encode())


def case_wide():
    for s in ("é", "É", "ß", "straße", "ΟΔΟΣ", "ΣΣ", "σ", "ς", "ǅ", "ǆ", "Ǆ",
              "ﬁ", "ﬄ", "İ", "ı", "中文", "日本語abc", "\U0001f600a",
              "éÉèÈ", "ᾈ", "µ"):
        print(repr(s.upper()), repr(s.lower()), repr(s.swapcase()),
              repr(s.title()), repr(s.capitalize()), repr(s.casefold()))
        print(len(s.upper()), len(s.lower()), len(s.swapcase()),
              len(s.title()), len(s.capitalize()), len(s.casefold()))
    # A mapping that lengthens: len() must be the code-point count of the
    # RESULT, which is not the input's.
    for s in ("ß", "ßß", "ﬄ", "aßb"):
        u = s.upper()
        print(s, len(s), u, len(u), len(u.encode()))


def case_mixed_width():
    # ASCII and non-ASCII in one string forces the general path even though
    # most of the bytes are ASCII.
    s = "abc" + "é" + "DEF"
    print(s.upper(), len(s.upper()), s.lower(), len(s.lower()))
    print(s.swapcase(), len(s.swapcase()))
    t = "é" + "a" * 40
    print(t.upper(), len(t.upper()), len(t.upper().encode()))


def case_title_words():
    for s in ("hello world", "a1b", "  two  words  ", "don't", "a-b-c",
              "HELLO", "h", "", "1a", "ǅetva"):
        print(repr(s.title()), repr(s.capitalize()))


join_contents()
join_lengths()
join_single()
join_errors()
case_ascii()
case_wide()
case_mixed_width()
case_title_words()
