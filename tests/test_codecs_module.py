# `codecs`, the module -- and the four CodecInfo fields that were never there.
#
# lib/_codecs.py is the registry and the stateless codecs; `codecs` itself,
# which is 1,100 lines of pure Python in CPython, was missing entirely.  So
# `import codecs` was ModuleNotFoundError, and the AttributeError under it was
# worse: a CodecInfo has SEVEN fields and the registry here supplied three.
# In CPython the other four come from the per-codec module -- encodings/utf_16.py
# defines its own IncrementalEncoder, IncrementalDecoder, StreamReader and
# StreamWriter -- and there are no per-codec modules here.
#
# The one that mattered: lib/_io.py handles utf-8, ascii and latin-1 itself and
# hands every other encoding to codecs.getincrementaldecoder, so a standalone
# apython could not open a file in any other encoding at all.
import codecs
import io

# --- the stateless pair, which did work --------------------------------
print("encode/decode:", codecs.encode("abc", "utf-8"), codecs.decode(b"abc", "utf-8"))
print("lookup:", codecs.lookup("utf-8").name, codecs.lookup("cp1252").name,
      codecs.lookup("UTF_16_LE").name)

# `encodings.aliases` is the MODULE and the table inside it is
# `aliases.aliases`, as in CPython.  This package bound the name to the dict,
# which was invisible here and wrong for everyone else -- locale.py reaches
# for encodings.aliases.aliases and got AttributeError on a dict.
import encodings.aliases

print("encodings.aliases is a module:",
      type(encodings.aliases).__name__,
      encodings.aliases.aliases["latin"],
      encodings.aliases.aliases["u8"])
print("BOMs:", codecs.BOM_UTF8, codecs.BOM_UTF16_LE, codecs.BOM_UTF32_BE)

# --- the incremental encoders ------------------------------------------
# A stateless codec encodes each chunk on its own; the three that write a BOM
# write it once.
for enc in ("utf-8", "utf-16", "utf-16-le", "utf-32", "utf-8-sig", "latin-1",
            "cp1252", "utf-7"):
    e = codecs.getincrementalencoder(enc)()
    parts = [e.encode("ab"), e.encode("cd"), e.encode("", True)]
    joined = b"".join(parts)
    print("%-10s %-34r whole=%s" % (enc, joined, joined == "abcd".encode(enc)))

# reset() puts the BOM back in front, which is what a rewound stream needs.
e = codecs.getincrementalencoder("utf-16")()
first = e.encode("a")
e.reset()
print("reset re-emits the BOM:", e.encode("a") == first)

# --- the incremental decoders -----------------------------------------
# One byte at a time is the shape that finds every buffering bug: a multi-byte
# character, and for the BOM'd forms the byte order latched from chunk one.
# latin-1 and cp1252 are single-byte codecs, so they get text they can hold.
for enc in ("utf-8", "utf-16", "utf-16-le", "utf-16-be", "utf-32", "utf-32-be",
            "utf-8-sig", "latin-1", "cp1252", "utf-7"):
    text = "aé" if enc in ("latin-1", "cp1252") else "aé中"
    data = text.encode(enc)
    d = codecs.getincrementaldecoder(enc)()
    out = "".join(d.decode(bytes([b])) for b in data) + d.decode(b"", True)
    print("%-10s %-10r %s" % (enc, out, "ok" if out == text else "WRONG"))

# The latching case stated on its own.  A big-endian UTF-16 stream carries its
# BOM only in the first chunk; a decoder that re-sniffs on every chunk reads
# the rest of the file in the native order and says nothing.
data = "hello world".encode("utf-16-be")
d = codecs.getincrementaldecoder("utf-16")()
halves = [d.decode(codecs.BOM_UTF16_BE + data[:8]), d.decode(data[8:], True)]
print("byte order latches:", "".join(halves) == "hello world", halves)

# A partial unit at a non-final call is held back, not refused.
d = codecs.getincrementaldecoder("utf-16-le")()
print("partial unit held:", repr(d.decode(b"a")), repr(d.decode(b"\x00")))

# --- the stream pair ---------------------------------------------------
w = codecs.getwriter("utf-16-le")(io.BytesIO())
w.write("hi")
w.writelines(["!", "?"])
print("StreamWriter:", w.stream.getvalue())
r = codecs.getreader("utf-16-le")(io.BytesIO("one\ntwo\n".encode("utf-16-le")))
print("StreamReader.readline:", repr(r.readline()))
print("StreamReader.read:", repr(r.read()))
r = codecs.getreader("cp1252")(io.BytesIO(b"\x80\x93"))
print("StreamReader charmap:", repr(r.read()))
sr = codecs.StreamReaderWriter(io.BytesIO(), codecs.getreader("utf-8"),
                               codecs.getwriter("utf-8"))
sr.write("x")
print("StreamReaderWriter:", sr.stream.getvalue())

# --- what the module itself offers ------------------------------------
print("iterencode:", b"".join(codecs.iterencode(["a", "b"], "utf-8")))
print("iterdecode:", "".join(codecs.iterdecode([b"a", b"b"], "utf-8")))
print("register_error/lookup_error:", callable(codecs.lookup_error("strict")))
print("_is_text_encoding:", codecs.lookup("utf-8")._is_text_encoding)
# And what a lookup answers is a REAL codecs.CodecInfo, not a lookalike of
# its own: `type()` and `isinstance()` are what CPython's test_codecs asks,
# and all seven fields are filled rather than three.
info = codecs.lookup("utf-8")
print("type:", type(info).__name__, isinstance(info, codecs.CodecInfo))
print("seven fields:", [n for n in ("encode", "decode", "streamreader",
                                    "streamwriter", "incrementalencoder",
                                    "incrementaldecoder", "name")
                        if getattr(info, n, None) is None])
print("as a 4-tuple:", len(info), info[0] is info.encode,
      info[2] is info.streamreader)

# An unknown codec is a LookupError from every entry point.
for call in (lambda: codecs.lookup("no-such-codec"),
             lambda: codecs.getincrementaldecoder("no-such-codec"),
             lambda: codecs.getreader("no-such-codec")):
    try:
        call()
        print("unknown codec: NOT REFUSED")
    except LookupError:
        print("unknown codec: LookupError")

# --- and the reason this matters: open() in a non-native encoding ------
import os
path = "@test_codecs_module_tmp"
for enc in ("utf-16", "utf-16-be", "utf-32", "cp1252", "utf-8-sig", "latin-1"):
    text = "line one\nline two\né"
    with open(path, "w", encoding=enc) as f:
        f.write(text)
    with open(path, "r", encoding=enc) as f:
        got = f.read()
    # And a one-character read at a time, which is the buffering path.
    with open(path, "r", encoding=enc) as f:
        chunked = "".join(iter(lambda: f.read(1), ""))
    print("%-10s %s %s" % (enc, "ok" if got == text else "WRONG",
                           "chunked ok" if chunked == text else "CHUNKED WRONG"))
os.unlink(path)
print("survived")
