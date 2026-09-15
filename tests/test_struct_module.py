# `struct`, the module -- as opposed to `_struct`, the C module it stands on.
#
# lib/_struct.py has been here all along: the format parser, the packers, the
# Struct object and the cache.  `struct` itself was missing, and CPython's is
# nine lines of re-export, so every program that spells the import the
# ordinary way -- `import struct` -- got ModuleNotFoundError from a complete
# implementation sitting one underscore away.  pickle.py is one such program.
import struct

# --- the functions -----------------------------------------------------
print("pack/unpack:", struct.pack(">i", 7), struct.unpack(">i", b"\x00\x00\x00\x07"))
print("calcsize:", struct.calcsize("<2sHd"), struct.calcsize(">4x"))
print("byte orders:", struct.pack("<H", 1), struct.pack(">H", 1))
print("iter_unpack:", list(struct.iter_unpack("<H", b"\x01\x00\x02\x00")))

buf = bytearray(4)
struct.pack_into(">H", buf, 1, 513)
print("pack_into:", bytes(buf))
print("unpack_from:", struct.unpack_from(">H", buf, 1))

# --- the Struct object -------------------------------------------------
s = struct.Struct("!fH")
print("Struct:", s.format, s.size, s.pack(1.5, 2))
print("Struct.unpack:", s.unpack(s.pack(1.5, 2)))

# --- the error, and that it is the one raised --------------------------
print("error is an exception:", issubclass(struct.error, Exception))
for bad, what in ((("Q", -1), "negative into unsigned"),
                  (("B", 256), "out of range"),
                  (("i",), "too few arguments")):
    try:
        struct.pack(*bad)
        print("%-24s NOT REFUSED" % what)
    except struct.error:
        print("%-24s struct.error" % what)
try:
    struct.unpack(">i", b"ab")
    print("short buffer            NOT REFUSED")
except struct.error:
    print("short buffer            struct.error")

# --- __all__ is CPython's, and every name in it is there ---------------
missing = [n for n in struct.__all__ if not hasattr(struct, n)]
print("__all__ complete:", not missing, sorted(struct.__all__))
# _clearcache is imported by name, beside __all__, and the cache is real.
struct.Struct("<i")
print("_clearcache:", struct._clearcache())
print("__doc__ is a string:", isinstance(struct.__doc__, str))

# --- the round trip over every code, which is what callers rely on -----
# n and N are native-size-only, so they take the native order and every
# other code takes an explicit one -- which is also the distinction our
# _parse has to get right, since "<n" is a format error in CPython too.
cases = [("<", "b", -1), ("<", "B", 255), ("<", "h", -300), ("<", "H", 60000),
         ("<", "i", -70000), ("<", "I", 4000000000), ("<", "q", -(2 ** 40)),
         ("<", "Q", 2 ** 63), ("<", "f", 0.5), ("<", "d", 1e300),
         ("<", "?", True), ("<", "c", b"z"), ("<", "3s", b"abc"),
         ("<", "e", 1.5), ("<", "5p", b"ab"), (">", "i", -70000),
         (">", "d", -0.25), ("@", "n", -5), ("@", "N", 5), ("@", "l", -9),
         ("@", "P", 4096)]
for order, code, value in cases:
    got = struct.unpack(order + code, struct.pack(order + code, value))[0]
    print("%-3s %-4s %-14r %s"
          % (order, code, value, "ok" if got == value else "GOT %r" % (got,)))

# And "<n" really is refused, on both sides.
try:
    struct.pack("<n", 1)
    print("native-only code under an explicit order: NOT REFUSED")
except struct.error:
    print("native-only code under an explicit order: struct.error")
print("survived")
