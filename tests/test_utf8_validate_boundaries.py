# UTF-8 validation now runs ASCII eight bytes at a time and drops back to the
# byte ladder the moment a high bit appears.
#
# So what matters is exactly where the non-ASCII byte sits relative to the
# eight-byte stride: a multi-byte character that starts in the middle of a
# word, one that starts on a word boundary, one whose continuation bytes span
# a boundary, and a malformed byte at each of those places -- because the two
# loops have to agree about the index, and a validator that resumes at the
# wrong offset either accepts garbage or rejects good input.
#
# Every malformed case checks the REPORTED POSITION, not just that it raised.
# The position is what says the word loop handed the right index back.

CHARS = ["a", "é", "߿", "ࠀ", "中", "￿", "\U00010000",
         "\U0001f600", "\U0010ffff"]

BAD = [
    b"\x80",                    # a bare continuation byte
    b"\xbf",
    b"\xc0\x80",                # overlong two-byte
    b"\xc1\xbf",
    b"\xc2",                    # truncated two-byte
    b"\xc2\x41",                # bad continuation
    b"\xe0\x80\x80",            # overlong three-byte
    b"\xe0\x9f\xbf",
    b"\xed\xa0\x80",            # a surrogate
    b"\xed\xbf\xbf",
    b"\xe1\x80",                # truncated three-byte
    b"\xf0\x80\x80\x80",        # overlong four-byte
    b"\xf4\x90\x80\x80",        # past U+10FFFF
    b"\xf5\x80\x80\x80",
    b"\xf0\x9f\x98",            # truncated four-byte
    b"\xff",
    b"\xfe",
]


def valid_at_every_offset():
    # A non-ASCII character starting at every offset from 0 to 17, so it
    # begins before, on and after each eight-byte boundary, and so its
    # continuation bytes straddle one.
    for ch in CHARS:
        enc = ch.encode()
        for n in range(18):
            b = b"a" * n + enc + b"z" * 3
            s = b.decode()
            print(len(enc), n, len(s), len(b), s == "a" * n + ch + "zzz")


def pure_ascii_lengths():
    # Every length across and past the stride, all ASCII, so the word loop
    # runs to the end and the tail is every possible size.
    for n in range(0, 40):
        b = bytes((0x61 + (i % 26)) for i in range(n))
        s = b.decode()
        print(n, len(s), len(b), s == b.decode("latin-1"))
    # Every ASCII byte value, including NUL.
    b = bytes(range(128))
    s = b.decode()
    print(len(s), len(b), s.encode() == b)


def bad_at_every_offset():
    # The same malformed sequences, at every offset around a word boundary.
    for bad in BAD:
        for n in (0, 1, 7, 8, 9, 15, 16, 17):
            b = b"a" * n + bad
            try:
                b.decode()
                print("NO ERROR", n, bad)
            except UnicodeDecodeError as e:
                print(n, len(bad), e.start, e.end, e.reason)


def bad_after_wide():
    # A malformed byte after a valid multi-byte character, so the ladder is
    # already running when it is reached rather than the word loop.
    for bad in BAD[:8]:
        for pre in ("é", "中", "\U0001f600", "aé", "aaaaaaa中"):
            b = pre.encode() + bad + b"tail"
            try:
                b.decode()
                print("NO ERROR", pre, bad)
            except UnicodeDecodeError as e:
                print(len(pre.encode()), e.start, e.end, e.reason)


def truncation_at_the_end():
    # A character cut short exactly at the end of the input, at every offset.
    for ch in CHARS[1:]:
        enc = ch.encode()
        for cut in range(1, len(enc)):
            for n in (0, 6, 7, 8, 9, 16):
                b = b"a" * n + enc[:cut]
                try:
                    b.decode()
                    print("NO ERROR", n, cut, ch)
                except UnicodeDecodeError as e:
                    print(n, cut, len(enc), e.start, e.end, e.reason)


def error_handlers():
    for bad in BAD:
        for n in (0, 8, 9):
            b = b"a" * n + bad + b"end"
            print(repr(b.decode("utf-8", "replace")),
                  repr(b.decode("utf-8", "ignore")),
                  repr(b.decode("latin-1")))


def other_sources():
    # bytearray and memoryview reach the same validator.
    for s in ("", "a", "abcdefgh", "abcdefghi", "aaaaaaa中", "é" * 20):
        b = s.encode()
        print(bytearray(b).decode() == s, memoryview(b).tobytes().decode() == s,
              len(b.decode()), len(b))
    try:
        bytearray(b"abcdefgh\x80").decode()
    except UnicodeDecodeError as e:
        print("bytearray", e.start, e.end)


def long_mixed():
    # Long enough that the word loop runs many times between characters.
    s = ("x" * 40 + "é" + "y" * 40 + "中" + "z" * 40 + "\U0001f600") * 5
    b = s.encode()
    print(len(s), len(b), b.decode() == s)
    # And one where every eighth byte is non-ASCII, so the word loop almost
    # never completes.
    t = "".join("aaaaaaa" + "é" for _ in range(50))
    print(len(t), len(t.encode()), t.encode().decode() == t)
    # All non-ASCII: the word loop bails on its first word every time.
    u = "中" * 200
    print(len(u), len(u.encode()), u.encode().decode() == u)


valid_at_every_offset()
pure_ascii_lengths()
bad_at_every_offset()
bad_after_wide()
truncation_at_the_end()
error_handlers()
other_sources()
long_mixed()
