"""The encodings package: the single-byte codecs, and their error handlers.

CPython ships one module per codec and this ships one table per codec, so the
thing worth testing is that a lookup answers the same CodecInfo name and that
encode and decode agree byte for byte over the whole 256-entry range -- not a
spot check, which would pass on a table that is right in its ASCII half and
wrong above it.

Everything printed here is compared against python3's own output, so a table
that disagrees with CPython's in one entry shows up as one differing line.
"""

CODECS = ("cp1252", "cp1251", "cp437", "cp850", "cp866", "cp1250", "cp1253",
          "cp1254", "cp1255", "cp1256", "cp1257", "cp1258", "cp037", "cp424",
          "cp500", "cp720", "cp737", "cp775", "cp852", "cp855", "cp856",
          "cp857", "cp858", "cp860", "cp861", "cp862", "cp863", "cp864",
          "cp865", "cp869", "cp874", "cp875", "cp1006", "cp1026", "cp1125",
          "cp1140", "iso8859_2", "iso8859_3", "iso8859_4", "iso8859_5",
          "iso8859_6", "iso8859_7", "iso8859_8", "iso8859_9", "iso8859_10",
          "iso8859_11", "iso8859_13", "iso8859_14", "iso8859_15",
          "iso8859_16", "koi8_r", "koi8_t", "koi8_u", "kz1048", "mac_cyrillic",
          "mac_greek", "mac_iceland", "mac_latin2", "mac_roman", "mac_turkish",
          "mac_croatian", "mac_farsi", "mac_romanian", "ptcp154", "tis_620",
          "hp_roman8", "palmos")


def summarise(name):
    """-> a short digest of a codec's whole table, as one printable line."""
    parts = []
    holes = 0
    for i in range(256):
        try:
            ch = bytes([i]).decode(name)
        except UnicodeDecodeError:
            holes += 1
            continue
        parts.append(ord(ch))
    total = 0
    for k, v in enumerate(parts):
        total = (total * 31 + v) & 0xFFFFFFFF
    return "%s decodable=%d holes=%d digest=%08x" % (name, len(parts), holes,
                                                     total)


print("--- every table, whole ---")
for name in CODECS:
    print(summarise(name))

print("--- the name each answers to ---")
import _codecs
for name in ("utf-8", "UTF_8", "utf8", "u8", "ascii", "646", "latin-1",
             "latin1", "L1", "iso8859-15", "ISO 8859 15", "cp1252",
             "windows-1252", "windows-1251", "koi8-r", "mac_roman",
             "cp037", "IBM037", "tis-620", "utf-16", "utf-8-sig",
             "unicode_escape", "hp-roman8", "cp1140", "greek8"):
    print(name, "->", _codecs.lookup(name).name)

print("--- round trip ---")
for name in ("cp1252", "koi8-r", "iso8859-15", "mac-roman", "cp037", "cp437"):
    src = bytes(i for i in range(256))
    try:
        text = src.decode(name)
    except UnicodeDecodeError:
        text = src.decode(name, "ignore")
    print(name, len(text), text.encode(name, "backslashreplace")[:32])

print("--- the error handlers, over a table with holes ---")
for handler in ("strict", "ignore", "replace", "backslashreplace",
                "xmlcharrefreplace"):
    try:
        print(handler, "☃éx".encode("cp1252", handler))
    except UnicodeEncodeError as e:
        print(handler, "UnicodeEncodeError", e.encoding, e.start, e.end,
              e.reason)
for handler in ("strict", "ignore", "replace", "backslashreplace"):
    try:
        print(handler, b"a\x81\x8dz".decode("cp1252", handler))
    except UnicodeDecodeError as e:
        print(handler, "UnicodeDecodeError", e.encoding, e.start, e.end,
              e.reason)

print("--- a run of holes reaches the handler as one run ---")
print(b"\x81\x8d\x8f".decode("cp1252", "backslashreplace"))
print("☃☄★".encode("cp1252", "xmlcharrefreplace"))

print("--- a name nobody ships is a LookupError, not something worse ---")
#
# The multi-byte CJK codecs and the transform codecs (base64_codec, rot_13)
# are not tables and are not here; DIVERGENCES.md says so.  What is tested is
# that an unknown name fails the same way in both, since that is the shape a
# program catches.
for name in ("no-such-codec", "cp9999", "cp1252x", "iso8859_99"):
    try:
        "a".encode(name)
        print(name, "found")
    except LookupError as e:
        print(name, "LookupError", e)

print("--- normalize_encoding ---")
import encodings
for name in ("utf-8", "UTF 8", "iso-8859-15", "ISO_8859-15", "cp1252",
             "..utf..8..", "", "-", "u.t.f.8"):
    print(repr(name), "->", repr(encodings.normalize_encoding(name)))

print("--- utf-7, which is a state machine and not a table ---")
for text in ("Hi Mom -<WBWV>-!", "日本語", "Item 3 is \xa31.", "a+b", "+",
             "~\\", "a\xe9\xe9b", "\U0001F600", "+-+-", "\u2603]",
             "\u2603-", "\u2603A", "\u2603+"):
    print(repr(text), text.encode("utf-7"))
for raw in (b"+AGE-", b"+-", b"+AGEAYQ-x", b"a+ImIDkQ.", b"+", b"+A", b"++",
            b"a\x80b", b"+JgM]", b"+2D3eAA-", b"+2D0.", b"+2D3-", b"+.",
            b"+AAA-", b"+////-", b"x+.y"):
    try:
        print(raw, repr(raw.decode("utf-7")))
    except UnicodeDecodeError as e:
        print(raw, "UnicodeDecodeError", e.start, e.end, e.reason)
for handler in ("replace", "ignore", "backslashreplace"):
    print(handler, [b.decode("utf-7", handler)
                    for b in (b"+.", b"x+.y", b"+A.", b"+2D3-", b"a\x80b")])

print("--- str.encode and bytes.decode reach it, not just the registry ---")
print("héllo".encode("cp1252"))
print(b"h\xe9llo".decode("cp1252"))
print("héllo".encode("iso8859-15").decode("iso8859-15"))
print(bytearray(b"\xc0\xc1").decode("koi8-r"))
