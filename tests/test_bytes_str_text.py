"""Four places where a byte was taken for a character, or a character for a byte.

A bytearray's extended-slice delete walked forward while the step ran
backward; bytes formatting handed its arguments to the STR formatter, so %s
applied str() to them; str.translate indexed its subject by byte, so a
non-ASCII character was looked up by the first byte of its UTF-8; and
OSError's message cut its fields at a fixed byte count, mid-character.
"""


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(32), repr(got))


# --- deleting an extended slice of a bytearray ---
def deleted(sl):
    b = bytearray(b"abcdefgh")
    del b[sl]
    return bytes(b)


check("del [::2]", lambda: deleted(slice(None, None, 2)))
check("del [::3]", lambda: deleted(slice(None, None, 3)))
check("del [::-1]", lambda: deleted(slice(None, None, -1)))
check("del [::-2]", lambda: deleted(slice(None, None, -2)))
check("del [::-3]", lambda: deleted(slice(None, None, -3)))
check("del [1:6:2]", lambda: deleted(slice(1, 6, 2)))
check("del [6:1:-2]", lambda: deleted(slice(6, 1, -2)))
check("del [1:1:2]", lambda: deleted(slice(1, 1, 2)))
check("del [:]", lambda: deleted(slice(None, None, None)))
check("del [2:4]", lambda: deleted(slice(2, 4)))


# --- bytes formatting inserts bytes, not their repr ---
check("one bytes", lambda: b"%s" % (b"abc",))
check("two of them", lambda: b"%s and %s" % (b"x", bytearray(b"y")))
check("a memoryview", lambda: b"%s" % (memoryview(b"mv"),))
check("not a tuple", lambda: b"%s" % b"solo")
check("an int", lambda: b"%d" % (5,))
check("mixed", lambda: b"%d %s" % (7, b"z"))
check("high bytes", lambda: b"%s" % (b"\xff\xfe",))
check("a percent", lambda: b"%%" % ())
check("padding", lambda: b"[%5s]" % (b"ab",))
check("the str version", lambda: "%s" % ("abc",))


# --- translate by code point ---
check("a two-byte character", lambda: "é".translate({233: "X"}))
check("ascii", lambda: "abc".translate({97: "Z"}))
check("delete one", lambda: "héllo".translate({104: None, 233: "e"}))
check("maketrans", lambda: "abc".translate(str.maketrans("ab", "xy")))
check("a three-byte one", lambda: "日本".translate({26085: "J"}))
check("to a high ordinal", lambda: "abc".translate({97: 233}))
check("to an astral one", lambda: "abc".translate({97: 0x1F600}))
check("an empty table", lambda: "abc".translate({}))
check("an empty subject", lambda: "".translate({97: "z"}))
check("untouched non-ascii", lambda: "aéb".translate({97: "1"}))
check("a list table", lambda: "ab".translate(["X", "Y"]))
check("length is code points", lambda: len("日本".translate({26085: "ab"})))


# --- OSError's message stays valid UTF-8 ---
# The buffer is a fixed size here and CPython's is not, so what is checked is
# that the cut lands on a character boundary rather than where it lands.
def message(n):
    return str(OSError(2, "x", "é" * n))


check("short filename", lambda: message(3))
check("plain fields", lambda: str(OSError(2, "msg", "plain")))
# The trailing quote is CPython's; ours is cut short by the fixed buffer.
# What must hold either way is that the result is a valid string.
check("long is still a str", lambda: message(200).encode().decode()
      == message(200))
check("long, no filename", lambda: (lambda s: s.encode().decode() == s)(
    str(OSError(2, "é" * 200))))
check("a three-byte filename", lambda: (lambda s: s.encode().decode() == s)(
    str(OSError(2, "x", "日" * 200))))
