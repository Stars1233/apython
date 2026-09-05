# UTF-16 and UTF-32, and what they do with a malformed stream.
#
# The decoder detected exactly one thing -- a byte count that was not a
# multiple of the code-unit width -- and it RAISED rather than consulting
# `errors`, so `b"\x00".decode("utf-16", "replace")` was a
# UnicodeDecodeError where CPython answers a replacement character.
# Everything else it accepted: a lone surrogate came back as a character no
# str should hold, and a UTF-32 code point past 0x10FFFF was passed to chr()
# as if it were one.
#
# What is compared here is the exception's five fields as much as the answer:
# the positions are of the whole input, BOM included, because that is what the
# handler is handed and what it indexes.

CASES = [
    ("utf-16-le", b"\x00"),
    ("utf-16-le", b"a\x00b"),
    ("utf-16-le", b"\x00\xd8"),
    ("utf-16-le", b"\x00\xdc"),
    ("utf-16-le", b"\x00\xd8\x00\xdc"),
    ("utf-16-le", b"\x00\xd8a\x00"),
    ("utf-16-le", b"a\x00\x00\xd8a\x00b\x00"),
    ("utf-16-be", b"\xd8\x00"),
    ("utf-16-be", b"\xdc\x00"),
    ("utf-16", b"\x00"),
    ("utf-16", b"\xff\xfea\x00"),
    ("utf-16", b"\xff\xfea"),
    ("utf-16", b"\xfe\xff\x00a"),
    ("utf-16", b"\xfe\xff\xd8\x00"),
    ("utf-32-le", b"\x00"),
    ("utf-32-le", b"\x00\x00\x11\x00"),
    ("utf-32-le", b"\x00\xd8\x00\x00"),
    ("utf-32-le", b"a\x00\x00\x00"),
    ("utf-32-le", b"\xff\xff\xff\xff"),
    ("utf-32-be", b"\x00\x00\xd8\x00"),
    ("utf-32", b"\x00\x00"),
    ("utf-32", b"\xff\xfe\x00\x00a\x00\x00\x00"),
    ("utf-32", b"\xff\xfe\x00\x00\x00\xd8\x00\x00"),
]

for enc, data in CASES:
    for err in ("strict", "replace", "ignore", "backslashreplace"):
        try:
            answer = repr(data.decode(enc, err))
        except Exception as e:
            answer = "%s: %s" % (type(e).__name__, e)
        print("%-10s %-24r %-16s %s" % (enc, data, err, answer))

print("=== the exception's fields ===")
for enc, data in CASES:
    try:
        data.decode(enc)
    except UnicodeDecodeError as e:
        print("%-10s %-24r %-12s %r %d %d %s"
              % (enc, data, e.encoding, e.object, e.start, e.end, e.reason))
    except Exception as e:
        print("%-10s %-24r %s" % (enc, data, type(e).__name__))

print("=== round trips ===")
for enc in ("utf-16", "utf-16-le", "utf-16-be",
            "utf-32", "utf-32-le", "utf-32-be"):
    for s in ("", "a", "hello", "\xe9中", "\U0001F600", "a\U0001F600b",
              "\U0010FFFF", "퟿"):
        blob = s.encode(enc)
        print("%-10s %-14r %-4d %s" % (enc, s, len(blob), blob.decode(enc) == s))

print("=== what the decoder says it consumed ===")
import _codecs

print(_codecs.utf_16_decode(b"\xff\xfea\x00"))
print(_codecs.utf_16_decode(b"\xfe\xff\x00a"))
print(_codecs.utf_32_decode(b"\xff\xfe\x00\x00a\x00\x00\x00"))
print(_codecs.utf_16_le_decode(b"a\x00"))
print(_codecs.utf_32_le_decode(b"a\x00\x00\x00"))
print("done")
