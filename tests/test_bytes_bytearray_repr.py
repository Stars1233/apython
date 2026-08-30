# print(bytearray(b"ab")) used to segfault: bytearray had no tp_repr, and
# obj_repr's no-repr exit returned a zero payload while leaving the tag
# register stale, so print() dereferenced the NULL.
#
# bytes and bytearray have identical layouts, so one repr serves both -- but
# CPython's two implementations differ in one respect, and the tests below
# pin that down: both switch to a double-quote delimiter when the data holds
# a single quote and no double quote, yet bytearray still escapes the single
# quote inside it and bytes does not.

cases = [
    b"", b"ab", b"a\x00b", b"a\tb\nc", b"\\", b"\x7f\x80\xff",
    b"a'b", b'a"b', b"""a'b"c""", b"'", b'"', b"'\"",
]
for c in cases:
    print(repr(c), repr(bytearray(c)))

# str() and print() go through tp_str, which must be wired too
print(bytearray(b"xy"), str(bytearray(b"xy")), str(b"xy"))
print(bytearray(b""), repr(bytearray(b"")), len(bytearray(b"")))

# Round-trip through a container, which reprs its elements
print([bytearray(b"a"), b"b"], (bytearray(b"c"),), {b"k": bytearray(b"v")})
