# The codecs the interpreter can do without help: utf-8, ascii and latin-1.
# Anything else is the codecs module's business, and is a LookupError here.
s = "héllo"

print(s.encode(), s.encode("utf-8"), s.encode("UTF-8"), s.encode("utf8"))
print(s.encode("latin-1"), s.encode("latin1"), s.encode("iso-8859-1"))
print(b"h\xe9llo".decode("latin-1"), b"h\xe9llo".decode("latin1"))
print("abc".encode("ascii"), b"abc".decode("ascii"), b"abc".decode())
print(s.encode("latin-1").decode("latin-1") == s, s.encode().decode() == s)
print(len(s.encode()), len(s.encode("latin-1")), len(s))

# Round trips through the whole byte range.
raw = bytes(range(256))
print(raw.decode("latin-1").encode("latin-1") == raw, len(raw.decode("latin-1")))

for enc, data in (("ascii", s), ("latin-1", "日本")):
    try:
        data.encode(enc)
    except UnicodeEncodeError:
        print("UnicodeEncodeError", enc)
try:
    b"\xff".decode("ascii")
except UnicodeDecodeError:
    print("UnicodeDecodeError")

# utf-8 is validated too: a stray continuation byte, a truncated sequence and
# an over-long lead byte are all errors, not bytes that come through as they
# are.
for bad in (b"\xff", b"\xc3", b"\xc3\x28", b"\xe2\x82", b"\x80"):
    try:
        bad.decode("utf-8")
        print("decoded", bad)
    except UnicodeDecodeError:
        print("UnicodeDecodeError utf-8", bad)
for good in (b"abc", b"\xc3\xa9", b"\xf0\x9f\x98\x80"):
    print("decoded", good.decode("utf-8"), len(good.decode("utf-8")))
try:
    s.encode("no-such-codec-here")
except LookupError:
    print("LookupError")
