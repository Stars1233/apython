# Methods that existed and lied: each accepted its argument and then ignored
# it, so the answer was wrong rather than absent.


def t(f):
    try:
        return repr(f())
    except Exception as e:
        return type(e).__name__


# strip/lstrip/rstrip took a chars argument and dropped it on the floor
print("xxaxx".strip("x"), "xxaxx".lstrip("x"), "xxaxx".rstrip("x"))
print(repr("  a  ".strip()), repr("  a  ".lstrip()), repr("  a  ".rstrip()))
print("abcba".strip("ab"), repr("aaa".strip("a")), repr("".strip("a")))
print(repr("abc".strip("")), "\tx\n".strip(), "xyx".strip("xy"))
print("mississippi".strip("mip"), t(lambda: "a".strip(5)))

# split ignored maxsplit, and rsplit was a plain jump to split
print("a-b-c".split("-", 1), "a-b-c".rsplit("-", 1))
print("a-b-c".split("-"), "a-b-c".rsplit("-"))
print("a b  c".split(), "a b  c".rsplit())
print("a b  c".split(None, 1), "a b  c".rsplit(None, 1))
print("".split("-"), "".split(), "a".split("-"), "a,,b".split(","))
print("  x  ".split(), " x ".rsplit(None, 1), "aXXbXXc".rsplit("XX", 1))
print("a-b-c".split("-", 0), "a-b-c".rsplit("-", 0), "a-b-c".split("-", 9))
print(t(lambda: "a".split("")), t(lambda: "a".split(5)))

# startswith/endswith took only a single str, and ignored start/end
print("He".startswith(("X", "He")), "He".startswith(("X", "Y")), "He".startswith("H"))
print("abc".endswith(("x", "bc")), "abc".endswith(("x",)), "abc".endswith("c"))
print("abc".startswith("b", 1), "abc".startswith(("b", "z"), 1))
print("abcdef".startswith("cd", 2, 4), "abcdef".startswith("cde", 2, 4))
print("abc".endswith("b", 0, 2), "abc".endswith(("b", "z"), 0, 2))
print("abc".startswith("a", -3), "abc".endswith("c", -1))
print("".startswith(()), "a".startswith(()), "abc".startswith("", 1))

# bit_length went through int64, so anything wider truncated
print([n.bit_length() for n in (0, 1, 255, 256, -255, 2 ** 50)])
print([n.bit_length() for n in (2 ** 63, 2 ** 63 - 1, 2 ** 200, -(2 ** 200))])

# repr picks its delimiter so the quote inside needs no backslash
print(repr("a'b"), repr('a"b'), repr('a\'b"c'), repr("plain"), repr(""))
print(repr("\n\t\\"), repr("'"), repr('"'), repr('\'"'))
