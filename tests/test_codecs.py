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

# --- The errors= handler, on the ascii paths ---------------------------------
#
# bytes.decode read errors= through codec_error_id and acted on it in the utf-8
# fixup loop, but its ascii arm jumped straight to the raise; str.encode parked
# the argument in a frame slot and never passed it anywhere at all.  So
# b"a\xffb".decode("ascii", "ignore") and "a\u1234b".encode("ascii", "ignore")
# both raised where CPython answers 'ab' and b'ab', and an unknown handler name
# was never reported as a LookupError on either path.

for errors in ("strict", "ignore", "replace"):
    for data in (b"a\xffb", b"abc", b"\xff", b"", b"\xff\xfe"):
        try:
            print(errors, data, repr(data.decode("ascii", errors)))
        except UnicodeDecodeError:
            print(errors, data, "UnicodeDecodeError")
    for text in ("a\u1234b", "abc", "\xe9", "", "\U0001F600x"):
        try:
            print(errors, repr(text), repr(text.encode("ascii", errors)))
        except UnicodeEncodeError:
            print(errors, repr(text), "UnicodeEncodeError")

# The handler is looked up only when something fails, which is also when
# CPython reports an unknown name.
print(repr("ab".encode("ascii", "bogus")), repr(b"ab".decode("ascii", "bogus")))
for label, fn in (("encode", lambda: "a\u1234b".encode("ascii", "bogus")),
                  ("decode", lambda: b"a\xffb".decode("ascii", "bogus"))):
    try:
        fn()
        print(label, "=> no error")
    except LookupError:
        print(label, "=> LookupError")

# utf-8 and latin-1 are unchanged.
print(repr(b"a\xffb".decode("utf-8", "ignore")), repr(b"a\xffb".decode("utf-8", "replace")))
print(repr("a\u00e9b".encode("latin-1")), repr("a\u1234b".encode("utf-8")))
