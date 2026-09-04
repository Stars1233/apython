# UnicodeDecodeError: the message, and the five fields.
#
# The ascii arm raised "byte not in range for this encoding", which names
# neither the codec, nor the byte, nor where it was.  And no decode error
# carried any of the attributes CPython puts on one -- encoding, object,
# start, end, reason -- which are how an error handler knows what to replace.


def t(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


def fields(b, enc):
    try:
        b.decode(enc)
    except UnicodeDecodeError as e:
        return (e.encoding, e.object, e.start, e.end, e.reason)
    return None


# --- ascii
t("ascii 0xff", lambda: b"\xff".decode("ascii"))
t("ascii mid", lambda: b"ab\xffcd".decode("ascii"))
t("ascii end", lambda: b"abc\x80".decode("ascii"))
t("ascii fields", lambda: fields(b"a\xffb", "ascii"))
t("ascii fields mid", lambda: fields(b"abc\x99", "ascii"))
t("ascii clean", lambda: b"abc".decode("ascii"))
t("ascii ignore", lambda: b"a\xffb".decode("ascii", "ignore"))
t("ascii replace", lambda: b"a\xffb".decode("ascii", "replace"))

# --- utf-8
t("utf8 start", lambda: b"\xff".decode("utf-8"))
t("utf8 mid", lambda: b"ab\xffcd".decode("utf-8"))
t("utf8 cont", lambda: b"\xc3\x28".decode("utf-8"))
t("utf8 truncated", lambda: b"ab\xc3".decode("utf-8"))
t("utf8 fields", lambda: fields(b"a\xffb", "utf-8"))
t("utf8 fields cont", lambda: fields(b"\xc3\x28", "utf-8"))
t("utf8 clean", lambda: "héllo".encode().decode("utf-8"))
t("utf8 default", lambda: b"\xff".decode())
t("utf8 ignore", lambda: b"a\xffb".decode("utf-8", "ignore"))
t("utf8 replace", lambda: b"a\xffb".decode("utf-8", "replace"))

# --- latin-1 never fails
t("latin1", lambda: b"\xff".decode("latin-1"))
t("latin1 fields", lambda: fields(b"\xff", "latin-1"))

# --- the exception is an ordinary object, and its fields survive
def stash():
    try:
        b"x\xffy".decode("ascii")
    except UnicodeDecodeError as e:
        saved = e
    return (saved.encoding, saved.start, saved.end, saved.reason,
            isinstance(saved, ValueError), isinstance(saved, UnicodeError))


t("stashed", stash)

# --- bytearray and memoryview go through the same path
t("bytearray", lambda: bytearray(b"a\xffb").decode("ascii"))
t("bytearray fields",
  lambda: fields(bytes(bytearray(b"a\xffb")), "ascii"))

# --- an unknown codec is a LookupError, and names the codec
t("unknown codec", lambda: b"a".decode("nosuchcodec"))
t("unknown handler", lambda: b"a\xff".decode("ascii", "nosuchhandler"))

print("done")
